module RayTracer.Tests.Transformation

open Xunit
open FsUnit.Xunit
open RayTracer.Utilities
open RayTracer.Tuples
open RayTracer.Transformation

// Note that a slightly different approach was taken than in the book.
// In the book, transforms are represented purely by matrices. Here, we represent
// transforms as their own type which can then be represented by matrices and
// applied to various objects.

[<Fact>]
let ``Multiplying by a translation matrix`` () =
    let p = point(-3.0, 4.0, 5.0)
    translate (5.0, -3.0, 2.0) p |> should equal (point(2.0, 1.0, 7.0))

[<Fact>]
let ``Multiplying by the inverse of a translation matrix`` () =
    let transform = Translation(5.0, -3.0, 2.0)
    let inv = inverse transform
    let p = point(-3.0, 4.0, 5.0)
    applyTransform inv p |> should equal (point(-8.0, 7.0, 3.0))

[<Fact>]
let ``Translation does not affect vectors`` () =
    let p = vector(-3.0, 4.0, 5.0)
    translate (5.0, -3.0, 2.0) p |> should equal (vector(-3.0, 4.0, 5.0))

[<Fact>]
let ``A scaling matrix applied to a point`` () =
    let p = point(-4.0, 6.0, 8.0)
    scale (2.0, 3.0, 4.0) p |> should equal (point(-8.0, 18.0, 32.0))

[<Fact>]
let ``A scaling matrix applied to a vector`` () =
    let p = vector(-4.0, 6.0, 8.0)
    scale (2.0, 3.0, 4.0) p |> should equal (vector(-8.0, 18.0, 32.0))

[<Fact>]
let ``Multiplying by the inverse of a scaling matrix`` () =
    let transform = Scaling(2.0, 3.0, 4.0)
    let inv = inverse transform
    let v = vector(-4.0, 6.0, 8.0)
    applyTransform inv v |> should equal (vector(-2.0, 2.0, 2.0))

[<Fact>]
let ``Reflection is scaling by a negative value`` () =
    let p = point(2.0, 3.0, 4.0)
    reflect X p |> should equal (point(-2.0, 3.0, 4.0))

[<Fact>]
let ``Rotating a point half-quarter around the x axis`` () = // original test name: "Rotating a point around the x axis"
    let p = point(0.0, 1.0, 0.0)
    rotate (X, pi/4.0) p |> should equal (point(0.0, sqrt(2.0)/2.0, sqrt(2.0)/2.0))

[<Fact>]
let ``Rotating a point full-quarter around the x axis`` () = // original test name: "Rotating a point around the x axis"
    let p = point(0.0, 1.0, 0.0)
    rotate (X, pi/2.0) p |> should equal (point(0.0, 0.0, 1.0))
    
[<Fact>]
let ``The inverse of an x-rotation rotates in the opposite direction`` () =
    let p = point(0.0, 1.0, 0.0)
    let halfQuarter = Rotation (X, pi/4.0)
    let inv = inverse halfQuarter
    applyTransform inv p |> should equal (point(0.0, sqrt(2.0)/2.0, -sqrt(2.0)/2.0))

[<Fact>]
let ``Rotating a point half-quarter around the y axis`` () = // original test name: "Rotating a point around the y axis"
    let p = point(0.0, 0.0, 1.0)
    rotate (Y, pi/4.0) p |> should equal (point(sqrt(2.0)/2.0, 0.0, sqrt(2.0)/2.0))

[<Fact>]
let ``Rotating a point full-quarter around the y axis`` () = // original test name: "Rotating a point around the y axis"
    let p = point(0.0, 0.0, 1.0)
    rotate (Y, pi/2.0) p |> should equal (point(1.0, 0.0, 0.0))

[<Fact>]
let ``Rotating a point half-quarter around the z axis`` () = // original test name: "Rotating a point around the z axis"
    let p = point(0.0, 1.0, 0.0)
    rotate (Z, pi/4.0) p |> should equal (point(-sqrt(2.0)/2.0, sqrt(2.0)/2.0, 0.0))

[<Fact>]
let ``Rotating a point full-quarter around the z axis`` () = // original test name: "Rotating a point around the z axis"
    let p = point(0.0, 1.0, 0.0)
    rotate (Z, pi/2.0) p |> should equal (point(-1.0, 0.0, 0.0))

[<Fact>]
let ``A shearing transformation moves x in proportion to y`` () =
    let p = point(2.0, 3.0, 4.0)
    shear Xy 1.0 p |> should equal (point(5.0, 3.0, 4.0))

[<Fact>]
let ``A shearing transformation moves x in proportion to z`` () =
    let p = point(2.0, 3.0, 4.0)
    shear Xz 1.0 p |> should equal (point(6.0, 3.0, 4.0))

[<Fact>]
let ``A shearing transformation moves y in proportion to x`` () =
    let p = point(2.0, 3.0, 4.0)
    shear Yx 1.0 p |> should equal (point(2.0, 5.0, 4.0))

[<Fact>]
let ``A shearing transformation moves y in proportion to z`` () =
    let p = point(2.0, 3.0, 4.0)
    shear Yz 1.0 p |> should equal (point(2.0, 7.0, 4.0))

[<Fact>]
let ``A shearing transformation moves z in proportion to x`` () =
    let p = point(2.0, 3.0, 4.0)
    shear Zx 1.0 p |> should equal (point(2.0, 3.0, 6.0))

[<Fact>]
let ``A shearing transformation moves z in proportion to y`` () =
    let p = point(2.0, 3.0, 4.0)
    shear Zy 1.0 p |> should equal (point(2.0, 3.0, 7.0))

[<Fact>]
let ``Individual transformations are applied in sequence`` () =
    let p = point(1.0, 0.0, 1.0)
    p |> rotate (X, pi/2.0) |> scale (5.0, 5.0, 5.0) |> translate (10.0, 5.0, 7.0)
    |> should equal (point(15.0, 0.0, 7.0))

[<Fact>]
let ``Chained transformations must be applied in reverse order`` () =
    let p = point(1.0, 0.0, 1.0)
    let A = Rotation(X, pi/2.0)
    let B = Scaling(5.0, 5.0, 5.0)
    let C = Translation(10.0, 5.0, 7.0)
    let transforms = Combination [C; B; A]
    applyTransform transforms p |> should equal (point(15.0, 0.0, 7.0))
