// From https://github.com/janestreet/core_kernel/tree/774a6821b14cbcdcde02cbbca1984ea32bf06184
namespace WoofWare.PlayFetch

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Runtime.CompilerServices

/// Represents a weak hash table that automatically reclaims entries when values are garbage collected
type WeakHashTable<'Key, 'Value when 'Key: equality and 'Value: not struct> =
    private
        { EntryByKey: Dictionary<'Key, WeakReference>
          KeysWithUnusedData: ConcurrentQueue<'Key>
          mutable ThreadSafeRunWhenUnusedData: unit -> unit }

[<RequireQualifiedAccess>]
module internal WeakHashTable =
    /// Creates a new weak hash table
    let create<'Key, 'Value when 'Key: equality and 'Value: not struct> () : WeakHashTable<'Key, 'Value> =
        { EntryByKey = Dictionary<'Key, WeakReference>()
          KeysWithUnusedData = ConcurrentQueue<'Key>()
          ThreadSafeRunWhenUnusedData = ignore }

    /// Sets the callback to run when unused data is detected
    let setRunWhenUnusedData (t: WeakHashTable<'Key, 'Value>) (threadSafeF: unit -> unit) : unit =
        t.ThreadSafeRunWhenUnusedData <- threadSafeF

    /// Removes a key from the table
    let remove (t: WeakHashTable<'Key, 'Value>) (key: 'Key) : unit = t.EntryByKey.Remove(key) |> ignore

    /// Clears all entries from the table
    let clear (t: WeakHashTable<'Key, 'Value>) : unit = t.EntryByKey.Clear()

    /// Reclaims space for keys whose values have been garbage collected
    let reclaimSpaceForKeysWithUnusedData (t: WeakHashTable<'Key, 'Value>) : unit =
        let rec processQueue () =
            match t.KeysWithUnusedData.TryDequeue() with
            | true, key ->
                match t.EntryByKey.TryGetValue(key) with
                | true, entry when not entry.IsAlive -> remove t key
                | _ -> ()

                processQueue ()
            | false, _ -> ()

        processQueue ()

    /// Gets or creates a weak reference entry for a key
    let private getEntry (t: WeakHashTable<'Key, 'Value>) (key: 'Key) : WeakReference =
        match t.EntryByKey.TryGetValue(key) with
        | true, entry -> entry
        | false, _ ->
            let entry = WeakReference(null)
            t.EntryByKey.[key] <- entry
            entry

    /// Checks if a key exists with a live value
    let mem (t: WeakHashTable<'Key, 'Value>) (key: 'Key) : bool =
        match t.EntryByKey.TryGetValue(key) with
        | true, entry -> entry.IsAlive
        | false, _ -> false

    /// Checks if a key is using space in the table (regardless of whether value is alive)
    let keyIsUsingSpace (t: WeakHashTable<'Key, 'Value>) (key: 'Key) : bool = t.EntryByKey.ContainsKey(key)

    /// Sets data for an entry and registers finalizer
    let private setData (t: WeakHashTable<'Key, 'Value>) (key: 'Key) (entry: WeakReference) (data: 'Value) : unit =
        entry.Target <- data

        // Register a finalizer to enqueue the key when the value is collected
        let cleanup = ConditionalWeakTable<'Value, Object>()

        let callbackObj =
            { new Object() with
                override _.Finalize() =
                    try
                        t.KeysWithUnusedData.Enqueue(key)
                        t.ThreadSafeRunWhenUnusedData()
                    with _ ->
                        () }

        cleanup.Add(data, callbackObj)

    /// Replaces the value for a key
    let replace (t: WeakHashTable<'Key, 'Value>) (key: 'Key) (data: 'Value) : unit = setData t key (getEntry t key) data

    /// Adds a new key-value pair, raising an exception if key already exists with live value
    let addExn (t: WeakHashTable<'Key, 'Value>) (key: 'Key) (data: 'Value) : unit =
        let entry = getEntry t key

        if entry.IsAlive then
            failwithf "Weak_hashtbl.add_exn: key already in use"

        setData t key entry data

    /// Finds the value associated with a key
    let find (t: WeakHashTable<'Key, 'Value>) (key: 'Key) : 'Value option =
        match t.EntryByKey.TryGetValue(key) with
        | true, entry ->
            match entry.Target with
            | :? 'Value as value -> Some value
            | _ -> None
        | false, _ -> None

    /// Finds the value for a key or adds a new one using the default function
    let findOrAdd (t: WeakHashTable<'Key, 'Value>) (key: 'Key) (defaultF: unit -> 'Value) : 'Value =
        let entry = getEntry t key

        match entry.Target with
        | :? 'Value as value -> value
        | _ ->
            let data = defaultF ()
            setData t key entry data
            data
