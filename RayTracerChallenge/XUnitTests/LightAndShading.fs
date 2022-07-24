module RayTracer.Tests.LightAndShading

open Xunit
open FsUnit.Xunit
open RayTracer.Utilities
open RayTracer.Tuples
open RayTracer.Color
open RayTracer.Transformation
open RayTracer.Ray
open RayTracer.LightAndShading

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
let ``Reflecting a vector approaching at 45 degrees`` () =
    reflect (vector(1.0, -1.0, 0.0)) (vector(0.0, 1.0, 0.0)) |> should equal (vector(1.0, 1.0, 0.0))

[<Fact>]
let ``Reflecting a vector off a slanted surface`` () =
    reflect (vector(0.0, -1.0, 0.0)) (vector(sqrt(2.0)/2.0, sqrt(2.0)/2.0, 0.0)) |> should equal (vector(1.0, 0.0, 0.0))

[<Fact>]
let ``A point light has a position and intensity`` () =
    let position = point(0.0, 0.0, 0.0)
    let intensity = color(1.0, 1.0, 1.0)
    let light = {Position = position; Intensity = intensity}
    (light.Position, light.Intensity) |> should equal (position, intensity)

[<Fact>]
let ``The default material`` () =
    let m = material()
    (m.Color, m.Ambient, m.Diffuse, m.Specular, m.Shininess) |> should equal (color(1.0, 1.0, 1.0), 0.1, 0.9, 0.9, 200.0)

// "A sphere has a default material"
// "A sphere may be assigned a material"
// These tests are not implemented because we do not assign materials directly to shapes as the book does.
// Instead, an Object contains a shape along with a transform and material, both optional.

let m = material()
let position = point(0.0, 0.0, 0.0)

[<Fact>]
let ``Lighting with the eye between the light and the surface`` () =
    let eyev = vector(0.0, 0.0, -1.0)
    let normalv = vector(0.0, 0.0, -1.0)
    let light = {Position = point(0.0, 0.0, -10.0); Intensity = color(1.0, 1.0, 1.0)}
    lighting m light position eyev normalv false |> should equal (color(1.9, 1.9, 1.9))

[<Fact>]
let ``Lighting with the eye between light and surface, eye offset 45 degrees`` () =
    let eyev = vector(0.0, sqrt(2.0)/2.0, -sqrt(2.0)/2.0)
    let normalv = vector(0.0, 0.0, -1.0)
    let light = {Position = point(0.0, 0.0, -10.0); Intensity = color(1.0, 1.0, 1.0)}
    lighting m light position eyev normalv false |> should equal (color(1.0, 1.0, 1.0))

[<Fact>]
let ``Lighting with the eye opposite surface, light offset 45 degrees`` () =
    let eyev = vector(0.0, 0.0, -1.0)
    let normalv = vector(0.0, 0.0, -1.0)
    let light = {Position = point(0.0, 10.0, -10.0); Intensity = color(1.0, 1.0, 1.0)}
    lighting m light position eyev normalv false |> should equal (color(0.7364, 0.7364, 0.7364))

[<Fact>]
let ``Lighting with the eye in the path of the reflection vector`` () =
    
    let eyev = vector(0.0, -sqrt(2.0)/2.0, -sqrt(2.0)/2.0)
    let normalv = vector(0.0, 0.0, -1.0)
    let light = {Position = point(0.0, 10.0, -10.0); Intensity = color(1.0, 1.0, 1.0)}
    lighting m light position eyev normalv false |> should equal (color(1.6364, 1.6364, 1.6364))

[<Fact>]
let ``Lighting with the light behind the surface`` () =
    let eyev = vector(0.0, 0.0, -1.0)
    let normalv = vector(0.0, 0.0, -1.0)
    let light = {Position = point(0.0, 0.0, 10.0); Intensity = color(1.0, 1.0, 1.0)}
    lighting m light position eyev normalv false |> should equal (color(0.1, 0.1, 0.1))

[<Fact>]
let ``Lighting with the surface in shadow`` () =
    let eyev = vector(0.0, 0.0, -1.0)
    let normalv = vector(0.0, 0.0, -1.0)
    let light = {Position = point(0.0, 0.0, -10.0); Intensity = color(1.0, 1.0, 1.0)}
    let inShadow = true
    lighting m light position eyev normalv inShadow |> should equal (color(0.1, 0.1, 0.1))