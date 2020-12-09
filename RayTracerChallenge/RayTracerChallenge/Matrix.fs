module RayTracer.Matrix

open Utilities

let create2DarrayByRow n m sequence =
    array2D
        [| for i in 0..n-1 do
            [| for j in 0..m-1 -> Seq.item (i*m + j) sequence |] |]
            //array.[i*m .. i*m+n]

let create2DarrayByColumn n m sequence =
    array2D
        [| for i in 0..n-1 do
             [| for j in 0..m-1 -> Seq.item (i + j*n) sequence |] |]

let inline dotArray (array1 : 'T[]) (array2 : 'T[]) : 'T =
    Array.map2 (fun x y -> x * y) array1 array2 |> Array.sum

type MatrixCreationOrder = ByRow | ByColumn

type MatrixType =
    | Constant of float
    | Diagonal of float
    | Identity

exception InvalidDimension of {| Row: int; Column: int; Message: string|}

[<StructuredFormatDisplay("{AsString}")>]
type Matrix(n: int, m: int, elements: float[,]) =
    new(n, m) = Matrix(n, m, Array2D.zeroCreate<float> n m)

    new(n, m, elements: seq<float>, order) =
        match order with
        | ByRow    -> Matrix(n, m, create2DarrayByRow n m elements)
        | ByColumn -> Matrix(n, m, create2DarrayByColumn n m elements)

    new(n, m, elements: seq<int>, order) =
        let elements = elements |> Seq.map float // Convert the seq<int> to seq<float>
        match order with
        | ByRow    -> Matrix(n, m, create2DarrayByRow n m elements)
        | ByColumn -> Matrix(n, m, create2DarrayByColumn n m elements)

    new(n, m, matrixType) =
        match matrixType with
        | Constant value -> Matrix(n, m, Array2D.init n m (fun _ _ -> value))
        | Diagonal value -> Matrix(n, m, Array2D.init n m (fun i j -> if i = j then value else 0.0))
        | Identity       -> Matrix(n, m, Array2D.init n m (fun i j -> if i = j then 1.0   else 0.0))

    (* Old implementation when the Matrix type was defined with a generic type parameter and not float.
       However, that implementation proved to be difficult to implement in a generic way, but this is
       left for reference.
    new(n, m, matrixType, value) =
        match matrixType with
        | Constant -> Matrixx(n, m, Array2D.init n m (fun _ _ -> value))
        | Diagonal -> Matrixx(n, m, Array2D.init n m (fun i j -> if i = j
                                                                 then value
                                                                 else Unchecked.defaultof<float>))
    *)
    
    /// Private member to use with the StructuredFormatDisplay attribute for custom display in FSI
    member private _.AsString = sprintf "\n%A" elements

    /// Private property to allow methods access to the internal elements array of another matrix
    member private _.GetElements = elements

    /// Number of rows in the matrix
    member _.N = Array2D.length1 elements
    
    /// Number of columns in the matrix
    member _.M = Array2D.length2 elements

    /// Number of rows in the matrix
    member this.NumOfRows    = this.N

    /// Number of columns in the matrix
    member this.NumOfColumns = this.M

    /// Allows indexing notation. If m is a Matrix, then m.[i,j] retrieves the (i,j)-element of the matrix
    member _.Item
        with get(i, j) = elements.[i, j]
        //and set(a: int, b: int) (value:'T) = internalArray.[a, b] <- value

    member this.GetSlice(rowStart: int option, rowFinish: int option, columnStart: int option, columnFinish: int option) =
        let rowStart =
            match rowStart with
            | Some v  -> v
            | None    -> 0
        let rowFinish =
            match rowFinish with
            | Some v  -> v
            | None    -> this.N - 1
        let colStart =
            match columnStart with
            | Some v  -> v
            | None    -> 0
        let columnFinish =
            match columnFinish with
            | Some v  -> v
            | None    -> this.M - 1
        elements.[rowStart..rowFinish, colStart..columnFinish]

    member this.GetSlice(row: int, columnStart: int option, columnFinish: int option) =
        let columnStart =
            match columnStart with
            | Some v  -> v
            | None    -> 0
        let columnFinish =
            match columnFinish with
            | Some v  -> v
            | None    -> this.M - 1
        elements.[row, columnStart..columnFinish]

    member this.GetSlice(rowStart: int option, rowFinish: int option, column: int) =
        let rowStart =
            match rowStart with
            | Some v  -> v
            | None    -> 0
        let rowFinish =
            match rowFinish with
            | Some v  -> v
            | None    -> this.N - 1
        elements.[rowStart..rowFinish, column]

    /// Multiplies two matrices and returns a new matrix
    static member (*) (m1: Matrix, m2: Matrix) =
        let newElements = array2D [| for i in 0..m1.N-1 ->
                                        [| for j in 0..m2.M-1 -> dotArray m1.[i,*] m2.[*,j] |] |]
        Matrix(m1.N, m2.M, newElements)

    /// Multiplies a matrix by a constant element-wise and returns a new matrix
    static member (*) (c: float, m: Matrix) =
        Matrix(m.N, m.M, Array2D.map (fun x -> c * x) m.GetElements)

    /// Adds two matrices together and returns a new matrix
    static member (+) (m1: Matrix, m2: Matrix) =
        let newElements = array2D [| for i in 0..m1.N-1 ->
                                      Array.map2 (fun x y -> x + y) m1.[i,*] m2.[i,*] |]
        Matrix(m1.N, m2.M, newElements)

    /// Adds a constant element-wise to a matrix and returns a new matrix
    static member (+) (c: float, m: Matrix) =
        Matrix(m.N, m.M, Array2D.map (fun x -> c + x) m.GetElements)

    /// Overrides the Object.Equals method to provide a custom equality compare for matrices
    override this.Equals object =
        // Note that Seq.forall2 will short circuit once it encounters a false
        // Seq.cast is used to cast the 2D arrays to sequences to use higher-order functions
        match object with
        | :? Matrix as m -> Seq.forall2 (fun x y -> compareFloat x y)
                                         (this.GetElements |> Seq.cast<float>)
                                         (m.GetElements |> Seq.cast<float>)
        | _ -> false
    
    /// Overrides the Object.GetHashCode method, which is recommended if overriding Object.Equals
    override _.GetHashCode() = elements.GetHashCode() // Simply re-use the hash code for the 2d array

    /// Transposes the matrix and returns the transpose as a new matrix
    member this.Transpose () =
        // Cast to a sequence so that we can cleverly use the ByColumn sequence constructor
        // since 2d arrays are not sequences
        let array = this.GetElements |> Seq.cast<float>
        Matrix(this.M, this.N, array, ByColumn)

    /// Calculates the determinant of the matrix
    member this.Determinant () =
        match this.N, this.M with
        | 2, 2          -> this.[0,0] * this.[1,1] - this.[0,1] * this.[1,0]
        | n, m when n=m -> this.[0,*] |> Array.mapi (fun i x -> x * this.Cofactor(0,i)) |> Array.sum
        | _             -> raise (InvalidDimension {| Row = n; Column = m;
                                                      Message = "Determinants are only supported for square matrices." |})
    
    /// Returns the submatrix, as a new matrix, found by removing the given row and column
    member this.GetSubmatrix(rowToRemove: int, columnToRemove: int) =
        let newElements = this.GetElements
                          |> Array2D.mapi (fun i j element -> (i, j, element))
                          |> Seq.cast<int*int*float>
                          |> Seq.filter (fun (i,j,_) -> (i <> rowToRemove) && (j <> columnToRemove))
                          |> Seq.map (fun (i,j,element) -> element)
        Matrix(this.N - 1, this.M - 1, newElements, ByRow)

    member this.ReplaceSubmatrix(rowToLeave: int, columnToLeave: int, subMatrix: Matrix) =
        let mutable subMatrixList = subMatrix.GetElements |> Seq.cast<float> |> Seq.toList
        let getNext () =
            match subMatrixList with
            | head :: tail -> subMatrixList <- tail
                              head
            | []           -> 0.0
        let newElements = this.GetElements
                          |> Array2D.mapi (fun i j element -> if (i = rowToLeave || j = columnToLeave)
                                                              then element
                                                              else getNext())
        Matrix(this.N, this.M, newElements)

    /// Calculates the minor of the matrix at the given row and column
    member this.Minor(row, column) = this.GetSubmatrix(row, column).Determinant()

    /// Calculates the cofactor of the matrix at the given row and column
    member this.Cofactor(row, column) =
        match row + column with
        | Odd  -> -this.Minor(row, column)
        | Even ->  this.Minor(row, column)

    /// Property that returns if the matrix is invertible or not
    member this.Invertible = this.Determinant() <> 0.0

    /// Inverts the matrix
    member this.Invert() =
        let determinant = this.Determinant()
        let elements = this.GetElements |> Array2D.mapi (fun i j element -> this.Cofactor(i,j) / determinant)
        Matrix(this.N, this.M, elements |> Seq.cast<float>, ByColumn)