namespace WoofWare.Zoomies

[<RequireQualifiedAccess>]
module internal Array =

    let maxOf<'k, 'v when 'v : comparison> (ifEmpty : 'v) (f : 'k -> 'v) (arr : 'k[]) : 'v =
        if arr.Length = 0 then
            ifEmpty
        else

        let mutable bestMax = f arr.[0]

        for i = 1 to arr.Length - 1 do
            let candidate = f arr.[i]
            bestMax <- max bestMax candidate

        bestMax

    let max2Of<'k1, 'k2, 'v when 'v : comparison>
        (ifEmpty : 'v)
        (f : 'k1 -> 'k2 -> 'v)
        (arr1 : 'k1[])
        (arr2 : 'k2[])
        : 'v
        =
        assert (arr1.Length = arr2.Length)

        if arr1.Length = 0 then
            ifEmpty
        else

        let mutable bestMax = f arr1.[0] arr2.[0]

        for i = 1 to arr1.Length - 1 do
            let candidate = f arr1.[i] arr2.[i]
            bestMax <- max bestMax candidate

        bestMax

    /// Extract a column from a 2D array as a 1D array
    let getColumn<'a> (colIdx : int) (arr : 'a[,]) : 'a[] =
        Array.init (arr.GetLength 0) (fun rowIdx -> arr.[rowIdx, colIdx])

    /// Extract a row from a 2D array as a 1D array
    let getRow<'a> (rowIdx : int) (arr : 'a[,]) : 'a[] =
        Array.init (arr.GetLength 1) (fun colIdx -> arr.[rowIdx, colIdx])

    /// Map over the rows of a 2D array, producing a 1D array of results
    let mapiRows<'a, 'b> (f : int -> 'a[] -> 'b) (arr : 'a[,]) : 'b[] =
        Array.init (arr.GetLength 0) (fun rowIdx -> f rowIdx (getRow rowIdx arr))
