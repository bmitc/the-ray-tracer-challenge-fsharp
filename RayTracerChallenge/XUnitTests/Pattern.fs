module RayTracer.Tests.Pattern

open Xunit
open FsUnit.Xunit
open RayTracer.Utilities
open RayTracer.Tuples
open RayTracer.Color
open RayTracer.Transformation
open RayTracer.Pattern
open RayTracer.Object
open RayTracer.LightAndShading

[<Fact>]
let ``Creating a stripe pattern`` () =
    let pattern = stripe white black None
    pattern.ColorA |> should equal white
    pattern.ColorB |> should equal black

[<Fact>]
let ``A stripe pattern is constant in y`` () =
    let pattern = stripe white black None
    patternAt pattern (point(0.0, 0, 0)) |> should equal white
    patternAt pattern (point(0.0, 1, 0)) |> should equal white
    patternAt pattern (point(0.0, 2, 0)) |> should equal white

[<Fact>]
let ``A stripe pattern is constant in z`` () =
    let pattern = stripe white black None
    patternAt pattern (point(0.0, 0, 0)) |> should equal white
    patternAt pattern (point(0.0, 0, 1)) |> should equal white
    patternAt pattern (point(0.0, 0, 2)) |> should equal white

[<Fact>]
let ``A stripe pattern alternates in x`` () =
    let pattern = stripe white black None
    patternAt pattern (point(0.0, 0, 0))    |> should equal white
    patternAt pattern (point(0.9, 0, 0))  |> should equal white
    patternAt pattern (point(1.0, 0, 0))    |> should equal black
    patternAt pattern (point(-0.1, 0, 0)) |> should equal black
    patternAt pattern (point(-1.0, 0, 0))   |> should equal black
    patternAt pattern (point(-1.1, 0, 0)) |> should equal white

[<Fact>]
let ``Lighting with a pattern applied`` () =
    let material = { Material.Default with Pattern = Some(stripe white black None)
                                           Ambient = 1.0
                                           Diffuse = 0.0
                                           Specular = 0.0 }
    let eyev = vector(0, 0, -1)
    let normalv = vector(0, 0, -1)
    let light = {Position = pointu<world>(0.0, 0, -10); Intensity = white}
    let c1 = lighting material sphere light (pointu<world>(0.9, 0, 0)) eyev normalv false
    let c2 = lighting material sphere light (pointu<world>(1.1, 0, 0)) eyev normalv false
    c1 |> should equal white
    c2 |> should equal black

[<Fact>]
let ``The default pattern transformation`` () =
    let pattern = stripe white black None
    pattern.Transform |> should equal None

[<Fact>]
let ``Assigning a transformation`` () =
    let pattern = stripe white black (Some (Translation (1, 2, 3)))
    pattern.Transform |> should equal (Some (Translation (1, 2, 3)))

[<Fact>]
let ``A gradient linearly interpolates between colors`` () =
    let pattern = gradient white black None
    patternAt pattern (pointu<world>(0, 0, 0))    |> should equal white
    patternAt pattern (pointu<world>(0.25, 0, 0)) |> should equal (color(0.75, 0.75, 0.75))
    patternAt pattern (pointu<world>(0.75, 0, 0)) |> should equal (color(0.25, 0.25, 0.25))

[<Fact>]
let ``A ring should extend in both x and z`` () =
    let pattern = ring white black None
    patternAt pattern (pointu<world>(0, 0, 0)) |> should equal white
    patternAt pattern (pointu<world>(1, 0, 0)) |> should equal black
    patternAt pattern (pointu<world>(0, 0, 1)) |> should equal black
    // 0.708 = just slightly more than sqrt(2)/2
    patternAt pattern (pointu<world>(0.708, 0, 0.708)) |> should equal black

[<Fact>]
let ``Checkers should repeat in x`` () =
    let pattern = checker white black None
    patternAt pattern (pointu<world>(0, 0, 0)) |> should equal white
    patternAt pattern (pointu<world>(0.99, 0, 0)) |> should equal white
    patternAt pattern (pointu<world>(1.01, 0, 0)) |> should equal black

[<Fact>]
let ``Checkers should repeat in y`` () =
    let pattern = checker white black None
    patternAt pattern (pointu<world>(0, 0, 0)) |> should equal white
    patternAt pattern (pointu<world>(0, 0.99, 0)) |> should equal white
    patternAt pattern (pointu<world>(0, 1.01, 0)) |> should equal black

[<Fact>]
let ``Checkers should repeat in z`` () =
    let pattern = checker white black None
    patternAt pattern (pointu<world>(0, 0, 0)) |> should equal white
    patternAt pattern (pointu<world>(0, 0, 0.99)) |> should equal white
    patternAt pattern (pointu<world>(0, 0, 1.01)) |> should equal black
