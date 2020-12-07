module RayTracer.Tests.Canvas

open Xunit
open FsUnit.Xunit
open RayTracer.Canvas
open RayTracer.Color
open RayTracer.PPM

[<Fact>]
let ``Creating a canvas`` () =
    let c = canvas (10, 20)
    (c.width, c.height) |> should equal (10, 20)

[<Fact>]
let ``Writing pixels to a canvas`` () =
    let c = canvas (10, 20)
    let red = color(1.0, 0.0, 0.0)
    let newCanvas = writePixel (2,3) red c
    readPixel newCanvas (2,3) |> should equal red

[<Fact>]
let ``Constructing the PPM header`` () =
    let c = canvas (5,3)
    createHeader 5 3 255.0 |> should equal "P3\n5 3\n255"

[<Fact>]
let ``Constructing the PPM pixel data`` () =
    let ppm = "P3\n\
               5 3\n\
               255\n\
               255 0 0 0 0 0 0 0 0 0 0 0 0 0 0\n\
               0 0 0 0 0 0 0 128 0 0 0 0 0 0 0\n\
               0 0 0 0 0 0 0 0 0 0 0 0 0 0 255\n"
    let c = canvas (5,3)
    let c1 = color (1.5, 0., 0.)
    let c2 = color (0., 0.5, 0.)
    let c3 = color (-0.5, 0., 1.)
    let newCanvas = c |> writePixel (0,0) c1 |> writePixel (2,1) c2 |> writePixel (4,2) c3
    canvasToPPM newCanvas |> should equal ppm

[<Fact>]
let ``Splitting long lines in PPM files`` () =
    let ppm = "P3\n\
               10 2\n\
               255\n\
               255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204\n\
               153 255 204 153 255 204 153 255 204 153 255 204 153\n\
               255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204\n\
               153 255 204 153 255 204 153 255 204 153 255 204 153\n"
    let c = canvas (10,2)
    let newCanvas = map (fun _ -> color(1.0, 0.8, 0.6)) c
    canvasToPPM newCanvas |> should equal ppm

[<Fact>]
let ``PPM files are terminated by a newline character`` () =
    let c = canvas (5,3)
    let ppm = canvasToPPM c
    ppm.Chars(ppm.Length - 1) |> should equal '\n'