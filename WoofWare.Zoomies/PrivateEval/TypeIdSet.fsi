namespace WoofWare.Zoomies

/// <summary>
/// A type for containing a set of
/// </summary>
type TypeIdSet

/// <summary>
/// A polymorphic function for folding over the set
/// </summary>
type TypeIdFolder<'acc> =
    /// A function to apply to some TypeId.
    abstract f<'a> : 'acc -> 'a TypeId -> 'acc

[<RequireQualifiedAccess>]
module TypeIdSet =
    /// <summary>
    /// The empty set.
    /// </summary>
    val empty : TypeIdSet

    /// <summary>
    /// A singleton set.
    /// </summary>
    val singleton<'a> : 'a TypeId -> TypeIdSet

    /// <summary>
    /// Returns <c>true</c> if the set contains no elements.
    /// </summary>
    val isEmpty : TypeIdSet -> bool

    val length : TypeIdSet -> int

    /// <summary>
    /// Adds a type-id to the set.
    /// </summary>
    /// <remarks>Nothing happens if the set already contains the item.</remarks>
    val add<'a> : TypeIdSet -> 'a TypeId -> TypeIdSet

    /// <summary>
    /// Removes a type-id from the set.
    /// </summary>
    /// <remarks>
    /// Nothing happens if the set doesn't already contain it.
    /// </remarks>
    val remove<'a> : TypeIdSet -> 'a TypeId -> TypeIdSet

    /// <summary>
    /// Computes the union of two sets
    /// </summary>
    val union : TypeIdSet -> TypeIdSet -> TypeIdSet

    /// <summary>
    /// Folds over the elements in the set
    /// </summary>
    val fold<'acc> : TypeIdSet -> init : 'acc -> 'acc TypeIdFolder -> 'acc

    /// <summary>
    /// Maps over the set and sticks the return values in a list
    /// </summary>
    val mapToList<'a> : TypeIdSet -> 'a TypeIdEval -> 'a list

    /// <summary>
    /// iterates over the elements in the set
    /// </summary>
    val iter : TypeIdSet -> unit TypeIdEval -> unit
