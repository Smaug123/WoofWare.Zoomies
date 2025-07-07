// From https://github.com/janestreet/core_kernel/tree/774a6821b14cbcdcde02cbbca1984ea32bf06184
module WoofWare.PlayFetch.Test.TestWeakHashTable

open System
open WoofWare.PlayFetch
open Expecto

let data (i : int) = ref i

let testBasicOperations () =
    let t = WeakHashTable.create<int, int ref> ()
    let key = 13

    // Initially not present
    Expect.isFalse (WeakHashTable.mem t key) "Key should not be present initially"

    // Add key
    WeakHashTable.addExn t key (data key)
    Expect.isTrue (WeakHashTable.mem t key) "Key should be present after add"

    // Find should return the value
    match WeakHashTable.find t key with
    | Some v -> Expect.equal 13 v.Value "Should find value 13"
    | None -> failwith "Should find value"

    // Replace with new value
    WeakHashTable.replace t key (data 14)

    match WeakHashTable.find t key with
    | Some v -> Expect.equal 14 !v "Should find value 14 after replace"
    | None -> failwith "Should find value after replace"

    // Remove key
    WeakHashTable.remove t key
    Expect.equal None (WeakHashTable.find t key) "Should not find after remove"

    // Add again
    WeakHashTable.addExn t key (data key)

    match WeakHashTable.find t key with
    | Some v -> Expect.equal 13 !v "Should find value 13 after re-add"
    | None -> failwith "Should find value after re-add"

    // Clear
    WeakHashTable.clear t
    Expect.equal None (WeakHashTable.find t key) "Should not find after clear"

// Test: keyIsUsingSpace and reclaimSpaceForKeysWithUnusedData
let testSpaceReclamation () =
    let t = WeakHashTable.create<int, int ref> ()
    let key = 13

    // Initially no space used
    Expect.isFalse (WeakHashTable.keyIsUsingSpace t key) "Key should not use space initially"
    Expect.isFalse (WeakHashTable.mem t key) "Key should not be present initially"

    // Add key - now using space
    WeakHashTable.addExn t key (data 0)
    Expect.isTrue (WeakHashTable.keyIsUsingSpace t key) "Key should use space after add"
    Expect.isTrue (WeakHashTable.mem t key) "Key should be present after add"

    // Force GC to collect the value
    GC.Collect ()
    GC.WaitForPendingFinalizers ()
    GC.Collect ()

    // Key still uses space but mem returns false (weak ref cleared)
    Expect.isTrue (WeakHashTable.keyIsUsingSpace t key) "Key should still use space after GC"
    Expect.isFalse (WeakHashTable.mem t key) "Key should not be present after GC"

    // Reclaim space
    WeakHashTable.reclaimSpaceForKeysWithUnusedData t
    Expect.isFalse (WeakHashTable.keyIsUsingSpace t key) "Key should not use space after reclaim"
    Expect.isFalse (WeakHashTable.mem t key) "Key should not be present after reclaim"

// Test: setRunWhenUnusedData callback
let testRunWhenUnusedData () =
    let t = WeakHashTable.create<int, int ref> ()
    let key = 13
    let ran = ref false

    WeakHashTable.setRunWhenUnusedData t (fun () -> ran := true)

    // GC before adding data - callback shouldn't run
    GC.Collect ()
    GC.WaitForPendingFinalizers ()
    Expect.isFalse !ran "Callback should not run before data added"

    // Add data and keep reference
    let data = data key
    WeakHashTable.addExn t key data

    // GC with live reference - callback shouldn't run
    GC.Collect ()
    GC.WaitForPendingFinalizers ()
    Expect.isFalse !ran "Callback should not run with live reference"

    // Ensure data is still accessible (prevents optimization)
    Expect.equal 13 !data "Data should still be 13"

    // Now let data go out of scope by setting to null (simulating going out of scope)
    // In real F# this would happen naturally when data goes out of scope
    GC.KeepAlive (data) // Ensure it's alive until here
// data goes out of scope after the function

// Test: findOrAdd and complex scenarios
type Struct =
    {
        mutable Foo : int
        Bar : int
        Baz : string
    }

let testComplexScenarios () =
    let t = WeakHashTable.create<int, Struct ref> ()

    let createData foo =
        ref
            {
                Foo = foo
                Bar = 0
                Baz = "hello"
            }

    let stabilize () =
        GC.Collect ()
        GC.WaitForPendingFinalizers ()
        GC.Collect ()
        WeakHashTable.reclaimSpaceForKeysWithUnusedData t

    // Use mutable cells to control lifetime
    let b1 = ref (Some (createData 1))
    let b2 = ref (Some (createData 2))
    let b3 = ref (Some (createData 3))
    let b4 = ref (Some (createData 4))

    let k1, k2, k3 = 1, 2, 3

    // Add using findOrAdd
    let add k bref =
        match !bref with
        | Some b ->
            let result = WeakHashTable.findOrAdd t k (fun () -> b)
            result
        | None -> failwith "Reference already cleared"

    let v1 = add k1 b1
    let v2 = add k2 b2
    let v3 = add k3 b3

    let isAbsent k = not (WeakHashTable.keyIsUsingSpace t k)

    let isBlock k (expected : 'a ref option) =
        match WeakHashTable.find t k, expected with
        | Some found, Some exp -> Object.ReferenceEquals (found, exp)
        | _ -> false

    // All should be present
    Expect.isTrue (isBlock k1 !b1) "k1 should have b1"
    Expect.isTrue (isBlock k2 !b2) "k2 should have b2"
    Expect.isTrue (isBlock k3 !b3) "k3 should have b3"

    // Clear b1 and stabilize
    b1 := None
    stabilize ()

    Expect.isTrue (isAbsent k1) "k1 should be absent after GC"
    Expect.isTrue (isBlock k2 !b2) "k2 should still have b2"
    Expect.isTrue (isBlock k3 !b3) "k3 should still have b3"

    // Clear b2 and stabilize
    b2 := None
    stabilize ()

    Expect.isTrue (isAbsent k1) "k1 should still be absent"
    Expect.isTrue (isAbsent k2) "k2 should be absent after GC"
    Expect.isTrue (isBlock k3 !b3) "k3 should still have b3"

    // Replace k3 with b4
    match !b4 with
    | Some v4 -> WeakHashTable.replace t k3 v4
    | None -> failwith "b4 shouldn't be cleared"

    b3 := None
    stabilize ()

    Expect.isTrue (isBlock k3 !b4) "k3 should now have b4"

    // Clear b4 and stabilize
    b4 := None
    stabilize ()

    Expect.isTrue (isAbsent k3) "k3 should be absent after GC"

[<Tests>]
let tests =
    testList
        "TestWeakHashTable"
        [
            test "basic operations" { testBasicOperations () }
            test "space reclamation" { testSpaceReclamation () }
            test "callback" { testRunWhenUnusedData () }
            test "complex" { testComplexScenarios () }
        ]
