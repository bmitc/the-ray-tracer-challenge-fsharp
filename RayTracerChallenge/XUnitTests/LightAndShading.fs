module RayTracer.Tests.LightAndShading

open Xunit
open FsUnit.Xunit
open RayTracer.Tuples
open RayTracer.Color
open RayTracer.Object
open RayTracer.LightAndShading

[<Fact>]
let ``A point light has a position and intensity`` () =
    let position = point(0.0, 0.0, 0.0)
    let intensity = color(1.0, 1.0, 1.0)
    let light = {Position = position; Intensity = intensity}
    (light.Position, light.Intensity) |> should equal (position, intensity)

[<Fact>]
let ``The default material`` () =
    let m = Material.Default
    (m.Color, m.Ambient, m.Diffuse, m.Specular, m.Shininess) |> should equal (color(1.0, 1.0, 1.0), 0.1, 0.9, 0.9, 200.0)

// "A sphere has a default material"
// "A sphere may be assigned a material"
// These tests are not implemented because we do not assign materials directly to shapes as the book does.
// Instead, an Object contains a shape along with a transform and material, both optional.

let m = Material.Default
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
