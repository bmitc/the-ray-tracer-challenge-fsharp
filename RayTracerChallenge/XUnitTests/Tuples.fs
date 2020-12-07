module RayTracer.Tests.Tuples

open Xunit
open FsUnit.Xunit
open RayTracer.Tuples

[<Fact>]
let ``Adding a vector to a point, p + v`` () =
    point(3.0, -2.0, 5.0) + vector(-2.0, 3.0, 1.0) |> should equal (point(1.0, 1.0, 6.0))

[<Fact>]
let ``Adding a vector to a vector, u + v`` () =
    vector(1.0, 2.0, 3.0) + vector(4.0, 5.5, 6.5) |> should equal (vector(5.0, 7.5, 9.5))

[<Fact>]
let ``Subtracting a point from a point, p - q = v`` () =
    point(3.0, 2.0, 1.0) - point(5.0, 6.0, 7.0) |> should equal (vector(-2.0, -4.0, -6.0))

[<Fact>]
let ``Subtracting a vector from a point, p - v = q`` () =
    point(3.0, 2.0, 1.0) - vector(5.0, 6.0, 7.0) |> should equal (point(-2.0, -4.0, -6.))

[<Fact>]
let ``Subtracting two vectors, u - v`` () =
    vector(3.0, 2.0, 1.0) - vector(5.0, 6.0, 7.0) |> should equal (vector(-2.0, -4.0, -6.0))

[<Fact>]
let ``Subtracting a vector from the zero vector, 0 - v`` () =
    vector(0.0, 0.0, 0.0) - vector(1.0, -2.0, 3.0) |> should equal (vector(-1.0, 2.0, -3.0))

[<Fact>]
let ``Negating a tuple, -v`` () =
    -vector(1.0, -2.0, 3.0) |> should equal (vector(-1.0, 2.0, -3.0))

[<Fact>]
let ``Multiplying a point by a scalar, a * p`` () =
    3.5 * point(1.0, -2.0, 3.0) |> should equal (point(3.5, -7.0, 10.5))

[<Fact>]
let ``Multiplying a vector by a scalar, a * v`` () =
    3.5 * vector(1.0, -2.0, 3.0) |> should equal (vector(3.5, -7.0, 10.5))

[<Fact>]
let ``Dividing a point by a scalar, p / a`` () =
    point(1.0, -2.0, 3.0) / 2.0 |> should equal (point(0.5, -1.0, 1.5))

[<Fact>]
let ``Dividing a vector by a scalar, v / a`` () =
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
let ``Normalizing vector(4.0, 0.0, 0.0)`` () =
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