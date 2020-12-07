module RayTracer.Tests.Color

open Xunit
open FsUnit.Xunit
open RayTracer.Color

[<Fact>]
let ``Adding colors`` () =
    color(0.9, 0.6, 0.75) + color(0.7, 0.1, 0.25) |> should equal (color(1.6, 0.7, 1.0))

[<Fact>]
let ``Subtracting colors`` () =
    color(0.9, 0.6, 0.75) - color(0.7, 0.1, 0.25) |> should equal (color(0.2, 0.5, 0.5))

[<Fact>]
let ``Multiplying a color by a scalar`` () =
    2.0 * color(0.2, 0.3, 0.4) |> should equal (color(0.4, 0.6, 0.8))

[<Fact>]
let ``Multiplying colors`` () =
    color(1.0, 0.2, 0.4) * color(0.9, 1.0, 0.1) |> should equal (color(0.9, 0.2, 0.04))