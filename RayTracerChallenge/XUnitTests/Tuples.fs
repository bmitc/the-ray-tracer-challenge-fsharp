module RayTracer.Tests.Tuples

open Xunit
open FsUnit.Xunit
open RayTracer.Tuples

// "A tuple with w=1.0 is a point"
// "A tuple with w=0 is a vector"
// These tests are not implemented because points and vectors are two distinct types in
// this implementation.

[<Fact>]
let ``point() creates tuples`` () = // original test name: point() creates tuples with w=1
    point(4.0, -4.0, 3.0) |> should equal { X = 4.0; Y = -4.0; Z = 3.0 }

[<Fact>]
let ``vector() creates tuples`` () = // original test name: vector() creates tuples with w=0
    vector(4.0, -4.0, 3.0) |> should equal { I = 4.0; J = -4.0; K = 3.0 }

[<Fact>]
let ``Adding a vector to a point`` () = // original test name: "Adding two tuples"
    point(3.0, -2.0, 5.0) + vector(-2.0, 3.0, 1.0) |> should equal (point(1.0, 1.0, 6.0))

[<Fact>]
let ``Adding a vector to a vector`` () = // original test name: "Adding two tuples"
    vector(1.0, 2.0, 3.0) + vector(4.0, 5.5, 6.5) |> should equal (vector(5.0, 7.5, 9.5))

[<Fact>]
let ``Subtracting two points`` () =
    point(3.0, 2.0, 1.0) - point(5.0, 6.0, 7.0) |> should equal (vector(-2.0, -4.0, -6.0))

[<Fact>]
let ``Subtracting a vector from a point`` () =
    point(3.0, 2.0, 1.0) - vector(5.0, 6.0, 7.0) |> should equal (point(-2.0, -4.0, -6.0))

[<Fact>]
let ``Subtracting two vectors`` () =
    vector(3.0, 2.0, 1.0) - vector(5.0, 6.0, 7.0) |> should equal (vector(-2.0, -4.0, -6.0))

[<Fact>]
let ``Subtracting a vector from the zero vector`` () =
    vector(0.0, 0.0, 0.0) - vector(1.0, -2.0, 3.0) |> should equal (vector(-1.0, 2.0, -3.0))

[<Fact>]
let ``Negating a vector`` () = // original test name: "Negating a tuple"
    -vector(1.0, -2.0, 3.0) |> should equal (vector(-1.0, 2.0, -3.0))

[<Fact>]
let ``Negating a point`` () = // original test name: "Negating a tuple"
    -point(1.0, -2.0, 3.0) |> should equal (point(-1.0, 2.0, -3.0))

[<Fact>]
let ``Multiplying a point by a scalar`` () = // original test name: "Multiplying a tuple by a scalar"
    3.5 * point(1.0, -2.0, 3.0) |> should equal (point(3.5, -7.0, 10.5))

[<Fact>]
let ``Multiplying a vector by a scalar`` () = // original test name: "Multiplying a tuple by a scalar"
    3.5 * vector(1.0, -2.0, 3.0) |> should equal (vector(3.5, -7.0, 10.5))

[<Fact>]
let ``Multiplying a point by a fraction`` () = // original test name: "Multiplying a tuple by a fraction"
    0.5 * point(1.0, -2.0, 3.0) |> should equal (point(0.5, -1.0, 1.5))

[<Fact>]
let ``Multiplying a vector by a fraction`` () = // original test name: "Multiplying a tuple by a fraction"
    0.5 * vector(1.0, -2.0, 3.0) |> should equal (vector(0.5, -1.0, 1.5))

[<Fact>]
let ``Dividing a point by a scalar`` () = // original test name: "Dividing a tuple by a scalar"
    point(1.0, -2.0, 3.0) / 2.0 |> should equal (point(0.5, -1.0, 1.5))

[<Fact>]
let ``Dividing a vector by a scalar`` () = // original test name: "Dividing a tuple by a scalar"
    vector(1.0, -2.0, 3.0) / 2.0 |> should equal (vector(0.5, -1.0, 1.5))

[<Fact>]
let ``Computing the magnitude of vector(1.0, 0.0, 0.0)`` () =
    magnitude (vector(1.0, 0.0, 0.0)) |> should equal 1.0

[<Fact>]
let ``Computing the magnitude of vector(0.0, 1.0, 0.0)`` () =
    magnitude (vector(0.0, 1.0, 0.0)) |> should equal 1.0

[<Fact>]
let ``Computing the magnitude of vector(0.0, 0.0, 1.0)`` () =
    magnitude (vector(0.0, 0.0, 1.0)) |> should equal 1.0

[<Fact>]
let ``Computing the magnitude of vector(1.0, 2.0, 3.0)`` () =
    magnitude (vector(1.0, 2.0, 3.0)) |> should equal (sqrt 14.0)

[<Fact>]
let ``Computing the magnitude of vector(-1.0, -2.0, -3.0)`` () =
    magnitude (vector(-1.0, -2.0, -3.0)) |> should equal (sqrt 14.0)

[<Fact>]
let ``Normalizing vector(4.0, 0.0, 0.0) gives (1.0, 0.0, 0.0)`` () =
    normalize (vector(4.0, 0.0, 0.0)) |> should equal (vector(1.0, 0.0, 0.0))

[<Fact>]
let ``Normalizing vector(1.0, 2.0, 3.0)`` () =
    normalize (vector(1.0, 2.0, 3.0)) |> should equal (vector(1.0/(sqrt 14.0), 2.0/(sqrt 14.0), 3.0/(sqrt 14.0)))

[<Fact>]
let ``The magnitude of a normalized vector`` () =
    magnitude (normalize (vector(1.0, 2.0, 3.0))) |> should equal 1.0

[<Fact>]
let ``The dot product of two vectors`` () =
    dot (vector(1.0, 2.0, 3.0)) (vector(2.0, 3.0, 4.0)) |> should equal 20.0

[<Fact>]
let ``The cross product of two vectors`` () =
    let a = vector(1.0, 2.0, 3.0)
    let b = vector(2.0, 3.0, 4.0)
    cross a b |> should equal (vector(-1.0, 2.0, -1.0))
    cross b a |> should equal (vector(1.0, -2.0, 1.0))
