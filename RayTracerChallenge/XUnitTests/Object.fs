module RayTracer.Tests.Object

open Xunit
open FsUnit.Xunit
open RayTracer.Utilities
open RayTracer.Tuples
open RayTracer.Color
open RayTracer.Transformation
open RayTracer.Pattern
open RayTracer.Object

[<Fact>]
let ``The normal on a sphere at a point on the x axis`` () =
    normalAt sphere (pointu<world>(1.0, 0.0, 0.0)) |> should equal (vector (1.0, 0.0, 0.0))

[<Fact>]
let ``The normal on a sphere at a point on the y axis`` () =
    normalAt sphere (pointu<world>(0.0, 1.0, 0.0)) |> should equal (vector (0.0, 1.0, 0.0))

[<Fact>]
let ``The normal on a sphere at a point on the z axis`` () =
    normalAt sphere (pointu<world>(0.0, 0.0, 1.0)) |> should equal (vector (0.0, 0.0, 1.0))

[<Fact>]
let ``The normal on a sphere at a nonaxial point`` () =
    let x = sqrt(3.0)/3.0
    normalAt sphere (pointu<world>(x, x, x)) |> should equal (vector (x, x, x))

[<Fact>]
let ``The normal is a normalized vector`` () =
    let x = sqrt(3.0)/3.0
    let n = normalAt sphere (pointu<world>(x, x, x))
    n |> should equal (normalize n)

[<Fact>]
let ``Computing the normal on a translated sphere`` () =
    normalAt {sphere with Transform = Some (Translation (0.0, 1.0, 0.0))} (pointu<world>(0.0, 1.70711, -0.70711))
    |> should equal (vector(0.0, 0.70711, -0.70711))

[<Fact>]
let ``Computing the normal on a transformed sphere`` () =
    let transforms = Combination [Scaling(1.0, 0.5, 1.0); Rotation(Z, pi/5.0)]
    normalAt {sphere with Transform = Some transforms} (pointu<world>(0.0, sqrt(2.0)/2.0, -sqrt(2.0)/2.0))
    |> should equal (vector(0.0, 0.97014, -0.24254))

[<Fact>]
let ``The default material`` () =
    let m = Material.Default
    (m.Color, m.Ambient, m.Diffuse, m.Specular, m.Shininess) |> should equal (color(1.0, 1.0, 1.0), 0.1, 0.9, 0.9, 200.0)

// "A sphere has a default material"
// "A sphere may be assigned a material"
// These tests are not implemented because we do not assign materials directly to shapes as the book does.
// Instead, an Object contains a shape along with a transform and material, both optional.

// "The default transformation"
// "The default material"
// These tests, appearing later in the book, are also not implemented for similar reasons

[<Fact>]
let ``Assigning a transformation`` () =
    let s = sphere
    let object = { s with Transform = Some (Translation (2.0, 3.0, 4.0)) }
    object.Transform |> should equal (Some (Translation (2.0, 3.0, 4.0)))

[<Fact>]
let ``Assigning a material`` () =
    let s = sphere
    let object = { s with Material = Some Material.Default }
    object.Material |> should equal (Some Material.Default)

[<Fact>]
let ``Computing the normal on a translated shape`` () =
    let s = { sphere with Transform = Some (Translation (0.0, 1.0, 0.0)) }
    normalAt s (pointu<world>(0.0, 1.70711, -0.70711)) |> should equal (vector(0.0, 0.70711, -0.70711))

[<Fact>]
let ``Computing the normal on a transformed shape`` () =
    let s = { sphere with Transform = Some (Combination[Scaling(1.0, 0.5, 1.0); Rotation (Z, pi/5.0)]) }
    normalAt s (pointu<world>(0.0, sqrt(2.0)/2.0, -sqrt(2.0)/2.0)) |> should equal (vector(0.0, 0.97014, -0.24254))

[<Fact>]
let ``The normal of a plane is constant everywhere`` () =
    let p = plane
    localNormal p (pointu<object>(0.0, 0.0, 0.0)) |> should equal (vector(0.0, 1.0, 0.0))
    localNormal p (pointu<object>(10.0, 0.0, -10.0)) |> should equal (vector(0.0, 1.0, 0.0))
    localNormal p (pointu<object>(-5.0, 0.0, 150.0)) |> should equal (vector(0.0, 1.0, 0.0))

[<Fact>]
let ``Stripes with an object transformation`` () =
    let object = { sphere with Transform = Some (Scaling (2, 2, 2)) }
    let pattern = stripe white black None
    patternAtObject pattern object (pointu<world>(1.5, 0, 0)) |> should equal white

[<Fact>]
let ``Stripes with a pattern transformation`` () =
    let object = sphere
    let pattern = stripe white black (Some (Scaling (2, 2, 2)))
    patternAtObject pattern object (pointu<world>(1.5, 0, 0)) |> should equal white

[<Fact>]
let ``Stripes with both an object and a pattern transformation`` () =
    let object = { sphere with Transform = Some (Scaling (2, 2, 2)) }
    let pattern = stripe white black (Some (Translation (0.5, 0, 0)))
    patternAtObject pattern object (pointu<world>(2.5, 0, 0)) |> should equal white

// "A pattern with object transformation"
// "A pattern with a pattern transformation"
// "A pattern with both an object and a pattern transformation"
// These tests are not implemented because the book takes an object-oriented approach and
// creates a test pattern, which is not needed here. It would dirty up the more functional
// Pattern record defined here and would only test basic creation and destructuring of
// records and discriminated unions.

//*******************************
// Additional tests
//*******************************

[<Fact>]
let ``The normal of a plane rotated about the X axis`` () =
    let point = pointu<world>(0.0, 0.0, 0.0)
    normalAt {plane with Transform = Some( Rotation (X, pi/2.0))} point |> should equal (vector(0.0, 0.0, 1.0))

[<Fact>]
let ``The normal of a plane rotated about the Y axis`` () =
    let point = pointu<world>(0.0, 0.0, 0.0)
    normalAt {plane with Transform = Some( Rotation (Y, pi/2.0))} point |> should equal (vector(0.0, 1.0, 0.0))

[<Fact>]
let ``The normal of a plane rotated about the Z axis`` () =
    let point = pointu<world>(0.0, 0.0, 0.0)
    normalAt {plane with Transform = Some( Rotation (Z, -pi/2.0))} point |> should equal (vector(1.0, 0.0, 0.0))

[<Fact>]
let ``The normal of a plane partially rotated about the Z axis`` () =
    let point = pointu<world>(0.0, 0.0, 1.0)
    normalAt {plane with Transform = Some( Rotation (Z, pi/4.0))} point |> should equal (normalize (vector(-1.0, 1.0, 0.0)))

[<Fact>]
let ``The normal of a scaled plane remains unchanged`` () =
    let point = pointu<world>(0.0, 0.0, 0.0)
    normalAt {plane with Transform = Some(Scaling (1.0, 2.0, 3.0))} point |> should equal (vector(0.0, 1.0, 0.0))

[<Fact>]
let ``The normal of a translated plane remains unchanged`` () =
    let point = pointu<world>(0.0, 0.0, 0.0)
    normalAt {plane with Transform = Some(Translation (0.0, 2.0, 0.0))} point |> should equal (vector(0.0, 1.0, 0.0))

[<Fact>]
let ``Reflectivity for the default material`` () =
    Material.Default.Reflective |> should equal 0.0
