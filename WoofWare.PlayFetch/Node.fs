namespace WoofWare.PlayFetch

open System
open TypeEquality

[<RequireQualifiedAccess>]
module Node =

    let isValue (n : 'a Node) : bool =
        match n.Kind with
        | Kind.Invalid -> false
        | _ -> true

    let isNecessary (n : 'a Node) : bool =
        n.NumParents > 0
        || n.Observers.IsSome
        || (
            match n.Kind with
            | Kind.Freeze _ -> true
            | _ -> false
        )
        || n.ForceNecessary

    let nodeIsInjective<'a, 'b> (t : Teq<'a Node, 'b Node>) : Teq<'a, 'b> = Teq.Cong.believeMe t

    let typeEqualIfPhysSame<'a, 'b> (t1 : Node<'a>) (t2 : Node<'b>) : Teq<'a, 'b> option =
        (* This is type-safe assuming no one can give the same incremental node two different
           types.  This is true because the field [mutable old_value_opt : 'a option] prevents
           both subtyping and parameteric polymorphism.  But this allows to break
           abstractions, as in someone could write:

           {[
             type t
             type u (* = t underneath *)
             val create : unit -> t Incr.t * u Incr.t (* the two incrementals are phys_equal *)
           ]}

           and we would figure out that type t = u.  However, we could add a Type_equal.Id to
           nodes and do the same, so it seems to be a more general issue. *)
        if Object.ReferenceEquals (t1, t2) then
            Teq.tryRefl<'a, 'b>
        else
            None

    let same (t1 : Node<'a>) (t2 : Node<'b>) = Object.ReferenceEquals (t1, t2)
    let packedSame (t1 : NodeCrate) (t2 : NodeCrate) =
        { new NodeEval<_> with
            member _.Eval n =
                { new NodeEval<_> with
                    member _.Eval m =
                        same m n
                }
                |> t2.Apply
        }
        |> t1.Apply

    let initialNumChildren (n : Node<_>) : int = Kind.initialNumChildren n.Kind
    let iteriChildren (t: Node<'a>) (f: int -> NodeCrate -> unit) : unit = Kind.iteriChildren t.Kind f

    let userInfo (t: Node<'a>) : string option =
        match t.UserInfo with
        | None -> None
        | Some (DotUserInfo.Info i) -> Some i
        | Some other -> Some (DotUserInfo.createS (DotUserInfo.sexpOfT other))

    let setUserInfo (t: Node<'a>) (info: string option) : unit =
        let desired =
            match info with
            | None -> None
            | Some i -> Some (DotUserInfo.Info i)
        t.UserInfo <- desired

let append_user_info_graphviz t ~label ~attrs =
  let new_ = Dot_user_info.dot ~label ~attributes:attrs in
  t.user_info
  <- (match t.user_info with
      | None -> Some new_
      | Some other -> Some (Dot_user_info.append other new_))
;;

let edge_is_stale ~child ~parent =
  Stabilization_num.compare child.changed_at parent.recomputed_at > 0
;;

let is_stale_with_respect_to_a_child t =
  let is_stale = ref false in
  iteri_children t ~f:(fun _ (T child) ->
    if edge_is_stale ~child ~parent:t then is_stale := true);
  !is_stale
;;

let is_stale : type a. a t -> bool =
  fun (t : a t) ->
  match t.kind with
  | Uninitialized -> assert false
  (* A const node is stale only at initialization. *)
  | Const _ -> Stabilization_num.is_none t.recomputed_at
  (* Time-based nodes are considered stale when [t.recomputed_at] is none, which happens
     at initialization and when the alarm mechanism makes a node stale (it sets the
     [t.recomputed_at] to [Stabilization_num.none]). *)
  | At _ -> Stabilization_num.is_none t.recomputed_at
  | At_intervals _ -> Stabilization_num.is_none t.recomputed_at
  | Snapshot _ -> Stabilization_num.is_none t.recomputed_at
  (* We never consider an invalidated node to be stale -- when we invalidate a node, we
     immediately propagate invalidity to its ancestors. *)
  | Invalid -> false
  (* A [Var] node is stale if it was set since it was recomputed. *)
  | Var { set_at; _ } -> Stabilization_num.compare set_at t.recomputed_at > 0
  (* Nodes that have children. *)
  | Bind_lhs_change _ ->
    Stabilization_num.is_none t.recomputed_at || is_stale_with_respect_to_a_child t
  | If_test_change _ ->
    Stabilization_num.is_none t.recomputed_at || is_stale_with_respect_to_a_child t
  | Join_lhs_change _ ->
    Stabilization_num.is_none t.recomputed_at || is_stale_with_respect_to_a_child t
  | Array_fold _
  | Bind_main _
  | Freeze _
  | If_then_else _
  | Join_main _
  | Map _
  | Map2 _
  | Step_function _
  | Unordered_array_fold _ ->
    Stabilization_num.is_none t.recomputed_at || is_stale_with_respect_to_a_child t
  | Expert { force_stale; _ } ->
    force_stale
    || Stabilization_num.is_none t.recomputed_at
    || is_stale_with_respect_to_a_child t
;;

let needs_to_be_computed t = is_necessary t && is_stale t
let is_in_recompute_heap t = t.height_in_recompute_heap >= 0
let is_in_adjust_heights_heap t = t.height_in_adjust_heights_heap >= 0

let get_parent t ~index =
  Uopt.value_exn
    (if index = 0 then t.parent0 else Uniform_array.get t.parent1_and_beyond (index - 1))
;;

let iteri_parents t ~f =
  if t.num_parents > 0
  then (
    f 0 (Uopt.value_exn t.parent0);
    for index = 1 to t.num_parents - 1 do
      f index (Uopt.value_exn (Uniform_array.get t.parent1_and_beyond (index - 1)))
    done)
;;

let has_child t ~child =
  let has = ref false in
  iteri_children t ~f:(fun _ (T child') -> has := !has || same child child');
  !has
;;

let has_invalid_child t =
  let has = ref false in
  iteri_children t ~f:(fun _ (T child) -> has := !has || not (is_valid child));
  !has
;;

let has_parent (t : _ t) ~parent =
  let has = ref false in
  iteri_parents t ~f:(fun _ (T parent') -> has := !has || same parent parent');
  !has
;;

let should_be_invalidated : type a. a t -> bool =
  fun t ->
  match t.kind with
  (* nodes with no children *)
  | Uninitialized -> assert false
  | At _ -> false
  | At_intervals _ -> false
  | Const _ | Snapshot _ | Var _ -> false
  | Invalid -> false
  (* Nodes with a fixed set of children are invalid if any child is invalid. *)
  | Array_fold _
  | Freeze _
  | Map _
  | Map2 _
  | Step_function _
  | Unordered_array_fold _ -> has_invalid_child t
  (* A *_change node is invalid if the node it is watching for changes is invalid (same
     reason as above).  This is equivalent to [has_invalid_child t]. *)
  | Bind_lhs_change { lhs; _ } -> not (is_valid lhs)
  | If_test_change { test; _ } -> not (is_valid test)
  | Join_lhs_change { lhs; _ } -> not (is_valid lhs)
  (* [Bind_main], [If_then_else], and [Join_main] are invalid if their *_change child is,
     but not necessarily if their other children are -- the graph may be restructured to
     avoid the invalidity of those. *)
  | Bind_main { lhs_change; _ } -> not (is_valid lhs_change)
  | If_then_else { test_change; _ } -> not (is_valid test_change)
  | Join_main { lhs_change; _ } -> not (is_valid lhs_change)
  | Expert _ ->
    (* This is similar to what we do for bind above, except that any invalid child can be
       removed, so we can only tell if an expert node becomes invalid when all its
       dependencies have fired (which in practice means when we are about to run it). *)
    false
;;

let fold_observers (t : _ t) ~init ~f =
  let r = ref t.observers in
  let ac = ref init in
  while Uopt.is_some !r do
    let observer = Uopt.value_exn !r in
    r := observer.next_in_observing;
    ac := f !ac observer
  done;
  !ac
;;

let iter_observers t ~f = fold_observers t ~init:() ~f:(fun () observer -> f observer)

let invariant (type a) (invariant_a : a -> unit) (t : a t) =
  Invariant.invariant t [%sexp_of: _ t] (fun () ->
    [%test_eq: bool] (needs_to_be_computed t) (is_in_recompute_heap t);
    if is_necessary t
    then (
      assert (t.height > Scope.height t.created_in);
      iteri_children t ~f:(fun _ (T child) ->
        assert (t.height > child.height);
        assert (has_parent child ~parent:t));
      assert (not (should_be_invalidated t)));
    iteri_parents t ~f:(fun _ (T parent) ->
      assert (has_child parent ~child:t);
      assert (is_necessary parent);
      assert (t.height < parent.height));
    let check f = Invariant.check_field t f in
    Fields.iter
      ~id:(check Node_id.invariant)
      ~state:ignore
      ~recomputed_at:(check Stabilization_num.invariant)
      ~value_opt:
        (check (fun value_opt ->
           if is_valid t && not (is_stale t) then assert (Uopt.is_some value_opt);
           Uopt.invariant invariant_a value_opt))
      ~kind:
        (check (fun kind ->
           Kind.invariant invariant_a kind;
           match kind with
           | Expert e ->
             Expert.invariant_about_num_invalid_children e ~is_necessary:(is_necessary t)
           | _ -> ()))
      ~cutoff:(check (Cutoff.invariant invariant_a))
      ~changed_at:
        (check (fun changed_at ->
           Stabilization_num.invariant changed_at;
           if Stabilization_num.is_some t.recomputed_at
           then assert (Stabilization_num.compare changed_at t.recomputed_at <= 0)))
      ~num_on_update_handlers:
        (check
           ([%test_result: int]
              ~expect:
                (List.length t.on_update_handlers
                 + fold_observers t ~init:0 ~f:(fun n { on_update_handlers; _ } ->
                   n + List.length on_update_handlers))))
      ~num_parents:
        (check (fun num_parents ->
           assert (num_parents >= 0);
           assert (num_parents <= 1 + Uniform_array.length t.parent1_and_beyond)))
      ~parent1_and_beyond:
        (check (fun parent1_and_beyond ->
           for parent_index = 1 to Uniform_array.length parent1_and_beyond do
             [%test_eq: bool]
               (parent_index < t.num_parents)
               (Uopt.is_some (Uniform_array.get parent1_and_beyond (parent_index - 1)))
           done))
      ~parent0:
        (check (fun parent0 ->
           [%test_eq: bool] (t.num_parents > 0) (Uopt.is_some parent0)))
      ~created_in:(check Scope.invariant)
      ~next_node_in_same_scope:
        (check (fun next_node_in_same_scope ->
           if Scope.is_top t.created_in || not (is_valid t)
           then assert (Uopt.is_none next_node_in_same_scope)))
      ~height:
        (check (fun height ->
           if is_necessary t then assert (height >= 0) else assert (height = -1)))
      ~height_in_recompute_heap:
        (check (fun height_in_recompute_heap ->
           assert (height_in_recompute_heap >= -1);
           assert (height_in_recompute_heap <= t.height)))
      ~prev_in_recompute_heap:
        (check (fun (prev_in_recompute_heap : Packed.t Uopt.t) ->
           if not (is_in_recompute_heap t)
           then assert (Uopt.is_none prev_in_recompute_heap);
           if Uopt.is_some prev_in_recompute_heap
           then (
             let (T prev) = Uopt.value_exn prev_in_recompute_heap in
             assert (packed_same (T t) (Uopt.value_exn prev.next_in_recompute_heap));
             assert (t.height_in_recompute_heap = prev.height_in_recompute_heap))))
      ~next_in_recompute_heap:
        (check (fun (next_in_recompute_heap : Packed.t Uopt.t) ->
           if not (is_in_recompute_heap t)
           then assert (Uopt.is_none next_in_recompute_heap);
           if Uopt.is_some next_in_recompute_heap
           then (
             let (T next) = Uopt.value_exn next_in_recompute_heap in
             assert (packed_same (T t) (Uopt.value_exn next.prev_in_recompute_heap));
             assert (t.height_in_recompute_heap = next.height_in_recompute_heap))))
      ~height_in_adjust_heights_heap:
        (check (fun height_in_adjust_heights_heap ->
           if height_in_adjust_heights_heap >= 0
           then assert (height_in_adjust_heights_heap < t.height)))
      ~next_in_adjust_heights_heap:
        (check (fun (next_in_adjust_heights_heap : Packed.t Uopt.t) ->
           if not (is_in_adjust_heights_heap t)
           then assert (Uopt.is_none next_in_adjust_heights_heap)
           else if Uopt.is_some next_in_adjust_heights_heap
           then (
             let (T next) = Uopt.value_exn next_in_adjust_heights_heap in
             assert (is_in_adjust_heights_heap next);
             assert (t.height_in_adjust_heights_heap = next.height_in_adjust_heights_heap))))
      ~old_value_opt:(check (Uopt.invariant invariant_a))
      ~observers:
        (check (fun _ ->
           iter_observers t ~f:(fun { state; observing; _ } ->
             assert (phys_equal t observing);
             match state with
             | In_use | Disallowed -> ()
             | Created | Unlinked -> assert false)))
      ~is_in_handle_after_stabilization:ignore
      ~on_update_handlers:ignore
      ~user_info:ignore
      ~my_parent_index_in_child_at_index:
        (check (fun my_parent_index_in_child_at_index ->
           (match t.kind with
            | Expert _ -> ()
            | _ ->
              [%test_result: int]
                (Array.length my_parent_index_in_child_at_index)
                ~expect:(initial_num_children t));
           if is_necessary t
           then
             iteri_children t ~f:(fun child_index (T child) ->
               assert (
                 packed_same
                   (T t)
                   (get_parent
                      child
                      ~index:my_parent_index_in_child_at_index.(child_index))));
           if debug && not (is_necessary t)
           then Array.iter my_parent_index_in_child_at_index ~f:(fun x -> assert (x = -1))))
      ~my_child_index_in_parent_at_index:
        (check (fun my_child_index_in_parent_at_index ->
           [%test_result: int]
             (Array.length my_child_index_in_parent_at_index)
             ~expect:(Uniform_array.length t.parent1_and_beyond + 1);
           iteri_parents t ~f:(fun parent_index (T parent) ->
             assert (
               packed_same
                 (T t)
                 (Kind.slow_get_child
                    parent.kind
                    ~index:my_child_index_in_parent_at_index.(parent_index))));
           if debug && not (is_necessary t)
           then Array.iter my_child_index_in_parent_at_index ~f:(fun x -> assert (x = -1))))
      ~force_necessary:ignore
      ~creation_backtrace:ignore)
;;

let unsafe_value t = Uopt.unsafe_value t.value_opt

let value_exn t =
  if Uopt.is_some t.value_opt
  then Uopt.unsafe_value t.value_opt
  else failwiths "attempt to get value of an invalid node" t [%sexp_of: _ t]
;;

let get_cutoff t = t.cutoff
let set_cutoff t cutoff = t.cutoff <- cutoff

let is_const t =
  match t.kind with
  | Const _ -> true
  | _ -> false
;;

let on_update t on_update_handler =
  t.on_update_handlers <- on_update_handler :: t.on_update_handlers;
  t.num_on_update_handlers <- t.num_on_update_handlers + 1
;;

let run_on_update_handlers t node_update ~now =
  let r = ref t.on_update_handlers in
  while not (List.is_empty !r) do
    match !r with
    | [] -> assert false
    | on_update_handler :: rest ->
      r := rest;
      On_update_handler.run on_update_handler node_update ~now
  done;
  let r = ref t.observers in
  while Uopt.is_some !r do
    let observer = Uopt.value_exn !r in
    r := observer.next_in_observing;
    let r = ref observer.on_update_handlers in
    while not (List.is_empty !r) do
      match !r with
      | [] -> assert false
      | on_update_handler :: rest ->
        r := rest;
        (* We have to test [state] before each on-update handler, because an on-update
           handler might disable its own observer, which should prevent other on-update
           handlers in the same observer from running. *)
        (match observer.state with
         | Created | Unlinked -> assert false
         | Disallowed -> ()
         | In_use -> On_update_handler.run on_update_handler node_update ~now)
    done
  done
;;

let set_kind t kind =
  t.kind <- kind;
  t.my_parent_index_in_child_at_index
  <- Array.create ~len:(Kind.initial_num_children kind) (-1)
;;

let create state created_in kind =
  let t =
    { id = Node_id.next ()
    ; state
    ; recomputed_at = Stabilization_num.none
    ; value_opt = Uopt.none
    ; kind
    ; cutoff = Cutoff.phys_equal
    ; changed_at = Stabilization_num.none
    ; num_on_update_handlers = 0
    ; num_parents = 0
    ; parent1_and_beyond = Uniform_array.empty
    ; parent0 = Uopt.none
    ; created_in
    ; next_node_in_same_scope = Uopt.none
    ; height = -1
    ; height_in_recompute_heap = -1
    ; prev_in_recompute_heap = Uopt.none
    ; next_in_recompute_heap = Uopt.none
    ; height_in_adjust_heights_heap = -1
    ; next_in_adjust_heights_heap = Uopt.none
    ; old_value_opt = Uopt.none
    ; observers = Uopt.none
    ; is_in_handle_after_stabilization = false
    ; on_update_handlers = []
    ; my_parent_index_in_child_at_index =
        Array.create ~len:(Kind.initial_num_children kind) (-1)
        (* [my_child_index_in_parent_at_index] has one element because it may need to hold
       the child index of [parent0]. *)
    ; my_child_index_in_parent_at_index = [| -1 |]
    ; force_necessary = false
    ; user_info = None
    ; creation_backtrace =
        (if state.keep_node_creation_backtrace then Some (Backtrace.get ()) else None)
    }
  in
  Scope.add_node created_in t;
  (* [invariant] does not yet hold here because many uses of [Node.create] use [kind =
     Uninitialized], and then mutate [t.kind] later. *)
  t
;;

let max_num_parents t = 1 + Uniform_array.length t.parent1_and_beyond

let make_space_for_parent_if_necessary t =
  if t.num_parents = max_num_parents t
  then (
    let new_max_num_parents = 2 * max_num_parents t in
    t.parent1_and_beyond
    <- Uniform_array.realloc t.parent1_and_beyond ~len:(new_max_num_parents - 1);
    t.my_child_index_in_parent_at_index
    <- Array.realloc t.my_child_index_in_parent_at_index ~len:new_max_num_parents (-1));
  if debug then assert (t.num_parents < max_num_parents t)
;;

let make_space_for_child_if_necessary t ~child_index =
  let max_num_children = Array.length t.my_parent_index_in_child_at_index in
  if child_index >= max_num_children
  then (
    if debug then assert (child_index = max_num_children);
    let new_max_num_children = Int.max 2 (2 * max_num_children) in
    t.my_parent_index_in_child_at_index
    <- Array.realloc t.my_parent_index_in_child_at_index ~len:new_max_num_children (-1));
  if debug then assert (child_index < Array.length t.my_parent_index_in_child_at_index)
;;

let set_parent : type a. child:a t -> parent:Packed.t Uopt.t -> parent_index:int -> unit =
  fun ~child ~parent ~parent_index ->
  if parent_index = 0
  then child.parent0 <- parent
  else Uniform_array.set child.parent1_and_beyond (parent_index - 1) parent
;;

let link
  : type a b. child:a t -> child_index:int -> parent:b t -> parent_index:int -> unit
  =
  fun ~child ~child_index ~parent ~parent_index ->
  set_parent ~child ~parent:(Uopt.some (Packed.T parent)) ~parent_index;
  child.my_child_index_in_parent_at_index.(parent_index) <- child_index;
  parent.my_parent_index_in_child_at_index.(child_index) <- parent_index
;;

let unlink
  : type a b. child:a t -> child_index:int -> parent:b t -> parent_index:int -> unit
  =
  fun ~child ~child_index ~parent ~parent_index ->
  set_parent ~child ~parent:Uopt.none ~parent_index;
  if debug
  then (
    child.my_child_index_in_parent_at_index.(parent_index) <- -1;
    parent.my_parent_index_in_child_at_index.(child_index) <- -1)
;;

let add_parent : type a b. child:a t -> parent:b t -> child_index:int -> unit =
  fun ~child ~parent ~child_index ->
  make_space_for_parent_if_necessary child;
  make_space_for_child_if_necessary parent ~child_index;
  link ~child ~child_index ~parent ~parent_index:child.num_parents;
  child.num_parents <- child.num_parents + 1
;;

let remove_parent : type a b. child:a t -> parent:b t -> child_index:int -> unit =
  fun ~child ~parent ~child_index ->
  if debug then assert (child.num_parents >= 1);
  let parent_index = parent.my_parent_index_in_child_at_index.(child_index) in
  if debug then assert (packed_same (T parent) (get_parent child ~index:parent_index));
  let last_parent_index = child.num_parents - 1 in
  if parent_index < last_parent_index
  then (
    let (T parent) =
      Uopt.value_exn (Uniform_array.get child.parent1_and_beyond (last_parent_index - 1))
    in
    link
      ~child
      ~child_index:child.my_child_index_in_parent_at_index.(last_parent_index)
      ~parent
      ~parent_index);
  unlink ~child ~child_index ~parent ~parent_index:last_parent_index;
  child.num_parents <- child.num_parents - 1
;;

let swap_children_except_in_kind parent ~child1 ~child_index1 ~child2 ~child_index2 =
  if debug
  then (
    assert (packed_same (T child1) (Kind.slow_get_child parent.kind ~index:child_index1));
    assert (packed_same (T child2) (Kind.slow_get_child parent.kind ~index:child_index2)));
  let index_of_parent_in_child1 =
    parent.my_parent_index_in_child_at_index.(child_index1)
  in
  let index_of_parent_in_child2 =
    parent.my_parent_index_in_child_at_index.(child_index2)
  in
  if debug
  then (
    assert (
      child1.my_child_index_in_parent_at_index.(index_of_parent_in_child1) = child_index1);
    assert (
      child2.my_child_index_in_parent_at_index.(index_of_parent_in_child2) = child_index2));
  (* now start swapping *)
  child1.my_child_index_in_parent_at_index.(index_of_parent_in_child1) <- child_index2;
  child2.my_child_index_in_parent_at_index.(index_of_parent_in_child2) <- child_index1;
  parent.my_parent_index_in_child_at_index.(child_index1) <- index_of_parent_in_child2;
  parent.my_parent_index_in_child_at_index.(child_index2) <- index_of_parent_in_child1
;;

module Packed = struct
  type t = Packed.t = T : _ Types.Node.t -> t [@@unboxed]

  let sexp_of_t (T t) = t |> [%sexp_of: _ t]
  let invariant (T t) = invariant ignore t

  module As_list (M : sig
      val next : Packed.t -> Packed.t Uopt.t
    end) =
  struct
    type t = Packed.t Uopt.t

    let fold t ~init ~f =
      let ac = ref init in
      let r = ref t in
      while Uopt.is_some !r do
        let packed_node = Uopt.unsafe_value !r in
        r := M.next packed_node;
        ac := f !ac packed_node
      done;
      !ac
    ;;

    let iter t ~f = fold t ~init:() ~f:(fun () n -> f n)
    let invariant t = iter t ~f:invariant
    let length t = fold t ~init:0 ~f:(fun n _ -> n + 1)
    let to_list t = List.rev (fold t ~init:[] ~f:(fun ac n -> n :: ac))
    let sexp_of_t t = to_list t |> [%sexp_of: Packed.t list]
  end

  let iter_descendants_internal ts ~f =
    let seen = Node_id.Hash_set.create () in
    let rec iter_descendants (T t) =
      if not (Hash_set.mem seen t.id)
      then (
        Hash_set.add seen t.id;
        f (T t);
        iteri_children t ~f:(fun _ t -> iter_descendants t))
    in
    List.iter ts ~f:iter_descendants;
    seen
  ;;

  let iter_descendants ts ~f = ignore (iter_descendants_internal ts ~f : _ Hash_set.t)
  let append_user_info_graphviz (T t) = append_user_info_graphviz t
end
