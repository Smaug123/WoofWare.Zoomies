namespace WoofWare.PlayFetch

open System
open System.Collections.Concurrent
open System.Runtime.ExceptionServices
open TypeEquality

type Status =
    | Stabilizing
    | Running_on_update_handlers
    | Not_stabilizing
    | Stabilize_previously_raised of RaisedException

type AdjustHeightsHeap =
    { mutable Length: int
      mutable HeightLowerBound: int
      mutable MaxHeightSeen: int
      mutable NodesByHeight: PackedNode option array }

and Alarm = TimingWheel<AlarmValue>

and AlarmValueAction =
    | At of At
    | AtIntervals of AtIntervals
    | Snapshot of SnapshotCrate
    | StepFunction of StepFunctionNodeCrate

and AlarmValue =
    { Action: AlarmValueAction
      mutable NextFired: Option<AlarmValue> }

and ArrayFold<'a, 'acc> =
    { Init: 'acc
      F: 'acc -> 'a -> 'acc
      Children: 'a Node[] }

and At =
    { Main: Node<BeforeOrAfter>
      At: TimeNs
      mutable Alarm: Alarm
      Clock: Clock }

and AtIntervals =
    { Main: Node<unit>
      Base: TimeNs
      Interval: TimeSpan
      Alarm: Alarm
      Clock: Clock }

and Bind<'a, 'b> =
    { Main: Node<'b>
      mutable F: 'a -> Node<'b>
      Lhs: 'a Node
      LhsChange: unit Node
      mutable Rhs: 'b Node option
      RhsScope: Scope
      AllNodesCreatedOnRhs: PackedNode option }

and Clock =
    { TimingWheel: TimingWheel<AlarmValue>
      Now: Var<TimeNs>
      HandleFired: Alarm -> unit
      mutable FiredAlarmValues: AlarmValue option }

and ExpertEdge<'a> =
    { Child: 'a Node
      OnChange: 'a -> unit
      Index: int option }

and ExpertPackedEdgeEval<'ret> =
    abstract Eval<'a> : 'a ExpertEdge -> 'ret

and ExpertPackedEdgeCrate =
    abstract Apply: ExpertPackedEdgeEval<'ret> -> 'ret

and Expert<'a> =
    {
        F: unit -> 'a
        /// bool is `is_now_observable`
        OnObservabilityChange: bool -> unit
        mutable children: ExpertPackedEdgeCrate option[]
        mutable NumChildren: int
        mutable ForceStale: bool
        mutable NumInvalidChildren: int
        mutable WillFireAllCallbacks: bool
    }

and Freeze<'a> =
    { Main: Node<'a>
      Child: Node<'a>
      OnlyFreezeWhen: 'a -> bool }

and IfThenElse<'a> =
    { Main: Node<'a>
      Test: Node<bool>
      TestChange: Node<unit>
      mutable CurrentBranch: Node<'a> option
      Then: Node<'a>
      Else: Node<'a> }

and InternalObserverState =
    | Created
    | InUse
    | Disallowed
    | Unlinked

and InternalObserver<'a> =
    { mutable State: State
      Observing: Node<'a>
      mutable OnUpdateHandlers: 'a OnUpdateHandler list
      mutable PrevInAll: PackedInternalObserver option
      mutable NextInAll: PackedInternalObserver option
      mutable PrevInObserving: InternalObserver<'a> option
      mutable NextInObserving: InternalObserver<'a> option }

and PackedInternalObserverEval<'ret> =
    abstract Eval<'a> : ('a InternalObserver -> PackedInternalObserver) -> 'ret

and PackedInternalObserver =
    abstract Apply: PackedInternalObserverEval<'ret> -> 'ret

and Join<'a> =
    { Main: 'a Node
      Lhs: 'a Node Node
      LhsChange: unit Node
      mutable Rhs: 'a Node option }

and ArrayFoldEval<'a, 'ret> =
    abstract Apply<'b> : ArrayFold<'b, 'a> -> 'ret

and ArrayFoldCrate<'a> =
    abstract Apply<'ret> : ArrayFoldEval<'a, 'ret> -> 'ret

and BindEval<'ret> =
    abstract Eval<'a, 'b> : Bind<'a, 'b> -> 'ret

and BindCrate =
    abstract Apply<'ret> : BindEval<'ret> -> 'ret

and BindMainEval<'a, 'ret> =
    abstract Eval<'b> : Bind<'a, 'b> -> 'ret

and BindMainCrate<'a> =
    abstract Apply<'ret> : BindMainEval<'a, 'ret> -> 'ret

and IfThenElseEval<'ret> =
    abstract Eval<'a> : IfThenElse<'a> -> 'ret

and IfThenElseCrate =
    abstract Apply<'ret> : IfThenElseEval<'ret> -> 'ret

and JoinEval<'ret> =
    abstract Eval<'a> : Join<'a> -> 'ret

and JoinCrate =
    abstract Apply<'ret> : JoinEval<'ret> -> 'ret

and MapEval<'a, 'ret> =
    abstract Apply<'a1> : ('a1 -> 'a) * 'a1 Node -> 'ret

and MapCrate<'a> =
    abstract Apply<'ret> : MapEval<'a, 'ret> -> 'ret

and UnorderedArrayFoldEval<'a, 'ret> =
    abstract Eval<'b> : UnorderedArrayFold<'b, 'a> -> 'ret

and UnorderedArrayFoldCrate<'a> =
    abstract Apply<'ret> : UnorderedArrayFoldEval<'a, 'ret> -> 'ret

and Map2Eval<'a, 'ret> =
    abstract Eval<'a1, 'a2> : ('a1 -> 'a2 -> 'a) * 'a1 Node * 'a2 Node -> 'ret

and Map2Crate<'a> =
    abstract Apply<'ret> : Map2Eval<'a, 'ret> -> 'ret

and Kind<'a> =
    | ArrayFold of ArrayFoldCrate<'a>
    | At of At * Teq<'a, BeforeOrAfter>
    | AtIntervals of AtIntervals * Teq<'a, unit>
    | BindLhsChange of BindCrate * Teq<'a, unit>
    | BindMain of BindMainCrate<'a>
    | Const of 'a
    | Expert of Expert<'a>
    | Freeze of Freeze<'a>
    | IfTestChange of IfThenElseCrate * Teq<'a, unit>
    | IfThenElse of IfThenElse<'a>
    | Invalid
    | JoinLhsChange of JoinCrate * Teq<'a, unit>
    | JoinMain of Join<'a>
    | Map of MapCrate<'a>
    | Snapshot of 'a Snapshot
    | StepFunction of 'a StepFunctionNode
    | Uninitialized
    | UnorderedArrayFold of UnorderedArrayFoldCrate<'a>
    | Var of Var<'a>
    | Map2 of Map2Crate<'a>

and Node<'a> =
    { Id: NodeId
      State: State
      mutable RecomputedAt: StabilizationNum
      mutable ValueOpt: 'a voption
      mutable Kind: 'a Kind
      mutable Cutoff: 'a Cutoff
      mutable ChangedAt: StabilizationNum
      mutable NumOnUpdateHandlers: int
      mutable NumParents: int
      mutable Parent1AndBeyond: PackedNode option[]
      mutable Parent0: PackedNode option
      mutable CreatedIn: Scope
      mutable NextNodeInSameScope: PackedNode option
      mutable Height: int
      mutable HeightInRecomputeHeap: int
      mutable PrevInRecomputeHeap: PackedNode option
      mutable NextInRecomputeHeap: PackedNode option
      mutable HeightInAdjustHeightsHeap: int
      mutable NextInAdjustHeightsHeap: PackedNode option
      mutable OldValueOpt: 'a option
      mutable Observers: 'a InternalObserver voption
      mutable IsInHandleAfterStabilization: bool
      mutable OnUpdateHandlers: 'a OnUpdateHandler list
      mutable MyParentIndexInChildAtIndex: int array
      mutable MyChildIndexInParentAtIndex: int array
      mutable ForceNecessary: bool
      mutable UserInfo: DotUserInfo option
      mutable CreationBacktrace: ExceptionDispatchInfo option }

and PackedNodeEval<'ret> =
    abstract Eval<'a> : ('a Node -> PackedNode) -> 'ret

and PackedNode =
    abstract Apply<'ret> : PackedNodeEval<'ret> -> 'ret

and Observer<'a> = 'a InternalObserver ref

and OnlyInDebug =
    { mutable CurrentlyRunningNode: PackedNode option
      mutable ExpertNodesCreatedByCurrentNode: PackedNode list }

and PackedWeakHashTableEval<'ret> =
    abstract Eval<'a, 'b when 'a: equality and 'b: not struct> : WeakHashTable<'a, 'b> -> 'ret

and PackedWeakHashTable =
    abstract Apply<'ret> : PackedWeakHashTableEval<'ret> -> 'ret

and RecomputeHeap =
    { mutable Length: int
      mutable HeightLowerBound: int
      mutable NodesByHeight: PackedNode option[] }

and RunOnUpdateHandlersEval<'ret> =
    abstract Eval<'a> : 'a Node -> 'a NodeUpdate -> 'ret

and RunOnUpdateHandlers =
    abstract Apply<'ret> : RunOnUpdateHandlersEval<'ret> -> 'ret

and Scope =
    | Top
    | Bind of BindCrate

and Snapshot<'a> =
    { Main: 'a Node
      At: TimeNs
      Before: 'a
      ValueAt: 'a Node
      Clock: Clock }

and SnapshotEval<'ret> =
    abstract Eval<'a> : Snapshot<'a> -> 'ret

and SnapshotCrate =
    abstract Apply<'ret> : SnapshotEval<'ret> -> 'ret

and State =
    { mutable Status: Status
      BindLhsChangeShouldInvalidateRhs: bool
      mutable StabilizationNum: StabilizationNum
      mutable CurrentScope: Scope
      RecomputeHeap: RecomputeHeap
      AdjustHeightsHeap: AdjustHeightsHeap
      PropagateInvalidity: PackedNode list
      mutable NumActiveObservers: int
      mutable AllObservers: PackedInternalObserver option
      FinalizedObservers: PackedInternalObserver ConcurrentQueue
      NewObservers: PackedInternalObserver list
      DisallowedObservers: PackedInternalObserver list
      SetDuringStabilization: PackedVar list
      HandleAfterStabilization: PackedNode list
      RunOnUpdateHandlers: RunOnUpdateHandlers list
      mutable OnlyInDebug: OnlyInDebug
      WeakHashTables: PackedWeakHashTable ConcurrentQueue
      mutable KeepNodeCreationBacktrace: bool
      mutable NumNodesBecameNecessary: int
      mutable NumNodesBecameUnnecessary: int
      mutable NumNodesChanged: int
      mutable NumNodesCreated: int
      mutable NumNodesInvalidated: int
      mutable NumNodesRecomputed: int
      mutable NumNodesRecomputedDirectlyBecauseOneChild: int
      mutable NumNodesRecomputedDirectlyBecauseMinHeight: int
      mutable NumVarSets: int }

and StepFunctionNode<'a> =
    { Main: 'a Node
      mutable Child: 'a StepFunction Node option
      mutable ExtractedStepFunctionFromChildAt: StabilizationNum
      mutable Value: 'a option
      mutable UpcomingSteps: (TimeNs * 'a) seq
      mutable Alarm: Alarm
      mutable AlarmValue: AlarmValue
      Clock: Clock }

and StepFunctionNodeEval<'ret> =
    abstract Eval<'a> : StepFunctionNode<'a> -> 'ret

and StepFunctionNodeCrate =
    abstract Apply<'ret> : StepFunctionNodeEval<'ret> -> 'ret

and UnorderedArrayFold<'a, 'acc> =
    {
        Main: 'acc Node
        Init: 'acc
        F: 'acc -> 'a -> 'acc
        /// old value first, then new value
        Update: 'acc -> 'a -> 'a -> 'acc
        FullComputeEveryNChanges: int
        Children: 'a Node[]
        mutable FoldValue: 'acc voption
        mutable NumChangesSinceLastFullCompute: int
    }

and Var<'a> =
    { mutable Value: 'a
      mutable ValueSetDuringStabilization: 'a option
      mutable SetAt: StabilizationNum
      Watch: 'a Node }

and PackedVarEval<'ret> =
    abstract Eval<'a> : Var<'a> -> 'ret

and PackedVar =
    abstract Apply<'ret> : PackedVarEval<'ret> -> 'ret
