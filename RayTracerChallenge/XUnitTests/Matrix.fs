module RayTracer.Tests.Matrix

open Xunit
open FsUnit.Xunit
open RayTracer.Matrix

[<Fact>]
let ``Constructing and inspecting a 4x4 matrix`` () =
    let m = Matrix(4, 4, [1.;2.;3.;4.;5.5;6.5;7.5;8.5;9.;10.;11.;12.;13.5;14.5;15.5;16.5], ByRow)
    [m.[0,0]; m.[0,3]; m.[1,0]; m.[1,2]; m.[2,2]; m.[3,0]; m.[3,2]]
    |> should equal [1.; 4.; 5.5; 7.5; 11.; 13.5; 15.5]

[<Fact>]
let ``A 2x2 matrix ought to be representable`` () =
    let m = Matrix(2, 2, array2D [[-3.; 5.]; [1.; -2.]])
    [m.[0,0]; m.[0,1]; m.[1,0]; m.[1,1]] |> should equal [-3.; 5.; 1.; -2.]

[<Fact>]
let ``A 3x3 matrix ought to be representable`` () =
    let m = Matrix(3, 3, [-3; 5; 0; 1; -2; -7; 0; 1; 1], ByRow)
    [m.[0,0]; m.[1,1]; m.[2,2]] |> should equal [-3.; -2.; 1.]

[<Fact>]
let ``Matrix equality with identical matrices`` () =
    // Note that this also notionally tests different construction methods
    let m1 = Matrix(4, 4, [1.000001;2.;3.;4.;5.;6.;7.;8.;9.;8.;7.;6.;5.;4.;3.;2.], ByRow)
    let m2 = Matrix(4, 4, array2D [[1.;2.;3.;4.]; [5.;6.;7.;8.]; [9.;8.;7.;6.]; [5.;4.;3.;2.]])
    m1 |> should equal m2
    // Since the Matrix class overrides Object.Equals, this uses the custom compare defined
    // by the override. This is tested by the addition of 1.000001 in the [0,0] position of m1.

[<Fact>]
let ``Matrix equality with different matrices`` () =
    // Note that this also notionally tests different construction methods
    let m1 = Matrix(4, 4, [|1.;2.;3.;4.;5.;6.;7.;8.;9.;8.;7.;6.;5.;4.;3.;2.|], ByRow)
    let m2 = Matrix(4, 4, array2D [[2.;3.;4.;5.]; [6.;7.;8.;9.]; [8.;7.;6.;5.]; [4.;3.;2.;1.]])
    m1 |> should not' (equal m2)

[<Fact>]
let ``Multiplying two matrices`` () =
    let m1 = Matrix(4, 4, [1.;2.;3.;4.;5.;6.;7.;8.;9.;8.;7.;6.;5.;4.;3.;2.], ByRow)
    let m2 = Matrix(4, 4, [-2.;1.;2.;3.;3.;2.;1.;-1.;4.;3.;6.;5.;1.;2.;7.;8.], ByRow)
    let expectedResult = Matrix(4, 4, [20.;22.;50.;48.;44.;54.;114.;108.;40.;58.;110.;102.;16.;26.;46.;42.], ByRow)
    m1 * m2 |> should equal expectedResult

[<Fact>]
let ``Multiplying a matrix by the identity matrix`` () =
    let m = Matrix(4, 4, [0.;1.;2.;4.;1.;2.;4.;8.;2.;4.;8.;16.;4.;8.;16.;32.], ByRow)
    let id = Matrix(4, 4, Identity)
    m * id |> should equal m

[<Fact>]
let ``Transposing a matrix`` () =
    let m = Matrix(4, 4, [0.;9.;3.;0.;9.;8.;0.;8.;1.;8.;5.;3.;0.;0.;5.;8.], ByRow)
    let transpose = Matrix(4, 4, [0.;9.;1.;0.;9.;8.;8.;0.;3.;0.;5.;5.;0.;8.;3.;8.], ByRow)
    m.Transpose() |> should equal transpose

[<Fact>]
let ``Transposing the identity matrix`` () =
    // Note that this also notionally tests different construction methods
    let id1 = Matrix(4, 4, Identity)
    let id2 = Matrix(4, 4, Diagonal 1.0)
    id1.Transpose() |> should equal id2

[<Fact>]
let ``Calculating the determinant of a 2x2 matrix`` () =
    let m = Matrix(2, 2, [1; 5; -3; 2], ByRow)
    m.Determinant() |> should equal 17.0

[<Fact>]
let ``A submatrix of a 3x3 matrix is a 2x2 matrix`` () =
    let m = Matrix(3, 3, [1; 5; 0; -3; 2; 7; 0; 6; -3], ByRow)
    let expectedResult = Matrix(2, 2, [-3; 2; 0; 6], ByRow)
    m.GetSubmatrix(0,2) |> should equal expectedResult

[<Fact>]
let ``A submatrix of a 4x4 matrix is a 3x3 matrix`` () =
    let m = Matrix(4, 4, [-6;1;1;6;-8;5;8;6;-1;0;8;2;-7;1;-1;1], ByRow)
    let expectedResult = Matrix(3, 3, [-6;1;6;-8;8;6;-7;-1;1], ByRow)
    m.GetSubmatrix(2,1) |> should equal expectedResult

[<Fact>]
let ``Calculating a minor of a 3x3 matrix`` () =
    let A = Matrix(3, 3, [3;5;0;2;-1;-7;6;-1;5], ByRow)
    let B = A.GetSubmatrix(1,0)
    [A.Minor(1,0); B.Determinant()] |> should equal [25.0; 25.0]

[<Fact>]
let ``Calculating a cofactor of a 3x3 matrix`` () =
    let A = Matrix(3, 3, [3;5;0;2;-1;-7;6;-1;5], ByRow)
    [A.Minor(0,0); A.Cofactor(0,0); A.Minor(1,0); A.Cofactor(1,0)]
    |> should equal [-12.0; -12.0; 25.0; -25.0]

[<Fact>]
let ``Calculating the determinant of a 3x3 matrix`` () =
    let A = Matrix(3, 3, [1;2;6;-5;8;-4;2;6;4], ByRow)
    [A.Cofactor(0,0); A.Cofactor(0,1); A.Cofactor(0,2); A.Determinant()]
    |> should equal [56.; 12.; -46.; -196.]

[<Fact>]
let ``Calculating the determinant of a 4x4 matrix`` () =
    let A = Matrix(4, 4, [-2;-8;3;5;-3;1;7;3;1;2;-9;6;-6;7;7;-9], ByRow)
    [A.Cofactor(0,0); A.Cofactor(0,1); A.Cofactor(0,2); A.Cofactor(0,3); A.Determinant()]
    |> should equal [690.; 447.; 210.; 51.; -4071.]

[<Fact>]
let ``Testing an invertible matrix for invertibility`` () =
    let A = Matrix(4, 4, [6;4;4;4;5;5;7;6;4;-9;3;-7;9;1;7;-6], ByRow)
    (A.Determinant(), A.Invertible) |> should equal (-2120.0, true)

[<Fact>]
let ``Testing a noninvertible matrix for invertibility`` () =
    let A = Matrix(4, 4, [-4;2;-2;-3;9;6;2;6;0;-5;1;-5;0;0;0;0], ByRow)
    (A.Determinant(), A.Invertible) |> should equal (0.0, false)

[<Fact>]
let ``Calculating the inverse of a matrix`` () =
    let A = Matrix(4, 4, [-5;2;6;-8;1;-5;1;8;7;7;-6;-7;1;-3;7;4], ByRow)
    let B = A.Invert()
    let inverseOfB = Matrix(4, 4, array2D [[ 0.21805;  0.45113;  0.24060; -0.04511];
                                           [-0.80827; -1.45677; -0.44361;  0.52068];
                                           [-0.07895; -0.22368; -0.05263;  0.19737];
                                           [-0.52256; -0.81391; -0.30075;  0.30639]])
    (A.Determinant(), A.Cofactor(2,3), B.[3,2], A.Cofactor(3,2), B.[2,3], B)
    |> should equal (532., -160., -160./532., 105., 105./532., inverseOfB)

[<Fact>]
let ``Calculating the inverse of another matrix`` () =
    let A = Matrix(4, 4, [8;-5;9;2;7;5;6;1;-6;0;9;6;-3;0;-9;-4], ByRow)
    let inverseOfA = Matrix(4, 4, array2D [[-0.15385; -0.15385; -0.28205; -0.53846];
                                           [-0.07692;  0.12308;  0.02564;  0.03077];
                                           [ 0.35897;  0.35897;  0.43590;  0.92308];
                                           [-0.69231; -0.69231; -0.76923; -1.92308]])
    A.Invert() |> should equal inverseOfA

[<Fact>]
let ``Calculating the inverse of a third matrix`` () =
    let A = Matrix(4, 4, [9;3;0;9;-5;-2;-6;-3;-4;9;6;4;-7;6;6;2], ByRow)
    let inverseOfA = Matrix(4, 4, array2D [[-0.04074; -0.07778;  0.14444; -0.22222];
                                           [-0.07778;  0.03333;  0.36667; -0.33333];
                                           [-0.02901; -0.14630; -0.10926;  0.12963];
                                           [ 0.17778;  0.06667; -0.26667;  0.33333]])
    A.Invert() |> should equal inverseOfA

[<Fact>]
let ``Multiplying a product by its inverse`` () =
    let A = Matrix(4, 4, [3;-9;7;3;3;-8;2;-9;-4;4;4;1;-6;5;-1;1], ByRow)
    let B = Matrix(4, 4, [8;2;2;2;3;-1;7;0;7;0;5;4;6;-2;0;5], ByRow)
    let C = A * B
    C * B.Invert() |> should equal A

// Additional tests

[<Fact>]
let ``Replacing a submatrix at (1,1)`` () =
    let matrix = Matrix(4, 4, [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16], ByRow)
    let unchangedMatrix = Matrix(4, 4, [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16], ByRow)
    let subMatrix = Matrix(3, 3, [-1;-2;-3;-4;-5;-6;-7;-8;-9;], ByRow)
    let resultMatrix = Matrix(4, 4, array2D [[-1.0;  2.0; -2.0; -3.0];
                                             [ 5.0;  6.0;  7.0;  8.0];
                                             [-4.0; 10.0; -5.0; -6.0];
                                             [-7.0; 14.0; -8.0; -9.0]])
    (matrix.ReplaceSubmatrix(1, 1, subMatrix), matrix) |> should equal (resultMatrix, unchangedMatrix)

[<Fact>]
let ``Replacing a submatrix at (0,0)`` () =
    let matrix = Matrix(4, 4, [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16], ByRow)
    let unchangedMatrix = Matrix(4, 4, [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16], ByRow)
    let subMatrix = Matrix(3, 3, [-1;-2;-3;-4;-5;-6;-7;-8;-9;], ByRow)
    let resultMatrix = Matrix(4, 4, array2D [[ 1.0;  2.0;  3.0;  4.0];
                                             [ 5.0; -1.0; -2.0; -3.0];
                                             [ 9.0; -4.0; -5.0; -6.0];
                                             [13.0; -7.0; -8.0; -9.0]])
    (matrix.ReplaceSubmatrix(0, 0, subMatrix), matrix) |> should equal (resultMatrix, unchangedMatrix)

[<Fact>]
let ``Replacing a submatrix at (3,3)`` () =
    let matrix = Matrix(4, 4, [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16], ByRow)
    let unchangedMatrix = Matrix(4, 4, [1;2;3;4;5;6;7;8;9;10;11;12;13;14;15;16], ByRow)
    let subMatrix = Matrix(3, 3, [-1;-2;-3;-4;-5;-6;-7;-8;-9;], ByRow)
    let resultMatrix = Matrix(4, 4, array2D [[-1.0; -2.0; -3.0;  4.0];
                                             [-4.0; -5.0; -6.0;  8.0];
                                             [-7.0; -8.0; -9.0; 12.0];
                                             [13.0; 14.0; 15.0; 16.0]])
    (matrix.ReplaceSubmatrix(3, 3, subMatrix), matrix) |> should equal (resultMatrix, unchangedMatrix)