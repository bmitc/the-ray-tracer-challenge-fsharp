module RayTracer.Tests.World

open Xunit
open FsUnit.Xunit
open RayTracer.Utilities
open RayTracer.Tuples
open RayTracer.Color
open RayTracer.Matrix
open RayTracer.Transformation
open RayTracer.Ray
open RayTracer.LightAndShading
open RayTracer.Scene

// "Creating a world"
// This test is not implemented.

[<Fact>]
let ``The default world`` () =
    let light = {Position = point(-10.0, 10.0, -10.0); Intensity = color(1.0, 1.0, 1.0)}
    let m = {material() with Color = color(0.8, 1.0, 0.6); Diffuse = 0.7; Specular = 0.2}
    let t = Scaling(0.5, 0.5, 0.5)
    let s1 = {sphere with Material = Some m}
    let s2 = {sphere with Transform = Some t}
    let w = {Objects = [s1; s2]; LightSource = light}
    (w.LightSource, List.contains s1 w.Objects, List.contains s2 w.Objects) |> should equal (light, true, true)

[<Fact>]
let ``Intersect a world with a ray`` () =
    let light = {Position = point(-10.0, 10.0, -10.0); Intensity = color(1.0, 1.0, 1.0)}
    let m = {material() with Color = color(0.8, 1.0, 0.6); Diffuse = 0.7; Specular = 0.2}
    let t = Scaling(0.5, 0.5, 0.5)
    let s1 = {sphere with Material = Some m}
    let s2 = {sphere with Transform = Some t}
    let w = {Objects = [s1; s2]; LightSource = light}
    let r = ray (point(0.0, 0.0, -5.0)) (vector(0.0, 0.0, 1.0))
    let xs = intersectWorld w r
    xs |> List.map (fun x -> x.Time) |> should equal [4.0; 4.5; 5.5; 6.0]

[<Fact>]
let ``Precomputing the state of an intersection`` () =
    let r = ray (point(0.0, 0.0, -5.0)) (vector(0.0, 0.0, 1.0))
    let i = { Object = sphere; Time = 4.0}
    let comps = prepareComputation i r
    (comps.Time, comps.Object, comps.Point, comps.Eye, comps.Normal)
    |> should equal (i.Time, i.Object, point(0.0,0.0,-1.0), vector(0.0,0.0,-1.0), vector(0.0,0.0,-1.0))

[<Fact>]
let ``The hit, when an intersection occurs on the outside`` () =
    let r = ray (point(0.0, 0.0, -5.0)) (vector(0.0, 0.0, 1.0))
    let i = { Object = sphere; Time = 4.0}
    let comps = prepareComputation i r
    comps.Inside |> should equal false

[<Fact>]
let ``The hit, when an intersection occurs on the inside`` () =
    let r = ray (point(0.0, 0.0, 0.0)) (vector(0.0, 0.0, 1.0))
    let i = { Object = sphere; Time = 1.0}
    let comps = prepareComputation i r
    (comps.Point, comps.Eye, comps.Inside, comps.Normal)
    |> should equal (point(0.0, 0.0, 1.0), vector(0.0, 0.0, -1.0), true, vector(0.0, 0.0, -1.0))

[<Fact>]
let ``Shading an intersection`` () =
    let light = {Position = point(-10.0, 10.0, -10.0); Intensity = color(1.0, 1.0, 1.0)}
    let m = {material() with Color = color(0.8, 1.0, 0.6); Diffuse = 0.7; Specular = 0.2}
    let t = Scaling(0.5, 0.5, 0.5)
    let s1 = {sphere with Material = Some m}
    let s2 = {sphere with Transform = Some t}
    let w = {Objects = [s1; s2]; LightSource = light}
    let r = ray (point(0.0, 0.0, -5.0)) (vector(0.0, 0.0, 1.0))
    let shape = s1
    let i = { Object = shape; Time = 4.0}
    let comps = prepareComputation i r
    shadeHit w comps |> should equal (color(0.38066, 0.47583, 0.2855))

[<Fact>]
let ``Shading an intersection from the inside`` () =
    let light = {Position = point(0.0, 0.25, 0.0); Intensity = color(1.0, 1.0, 1.0)}
    let m = {material() with Color = color(0.8, 1.0, 0.6); Diffuse = 0.7; Specular = 0.2}
    let t = Scaling(0.5, 0.5, 0.5)
    let s1 = {sphere with Material = Some m}
    let s2 = {sphere with Transform = Some t}
    let w = {Objects = [s1; s2]; LightSource = light}
    let r = ray (point(0.0, 0.0, 0.0)) (vector(0.0, 0.0, 1.0))
    let shape = s2
    let i = { Object = shape; Time = 0.5}
    let comps = prepareComputation i r
    shadeHit w comps |> should equal (color(0.90498, 0.90498, 0.90498))

[<Fact>]
let ``The color when a ray misses`` () =
    let light = {Position = point(-10.0, 10.0, -10.0); Intensity = color(1.0, 1.0, 1.0)}
    let m = {material() with Color = color(0.8, 1.0, 0.6); Diffuse = 0.7; Specular = 0.2}
    let t = Scaling(0.5, 0.5, 0.5)
    let s1 = {sphere with Material = Some m}
    let s2 = {sphere with Transform = Some t}
    let w = {Objects = [s1; s2]; LightSource = light}
    let r = ray (point(0.0, 0.0, -5.0)) (vector(0.0, 1.0, 0.0))
    colorAt w r |> should equal black

[<Fact>]
let ``The color when a ray hits`` () =
    let light = {Position = point(-10.0, 10.0, -10.0); Intensity = color(1.0, 1.0, 1.0)}
    let m = {material() with Color = color(0.8, 1.0, 0.6); Diffuse = 0.7; Specular = 0.2}
    let t = Scaling(0.5, 0.5, 0.5)
    let s1 = {sphere with Material = Some m}
    let s2 = {sphere with Transform = Some t}
    let w = {Objects = [s1; s2]; LightSource = light}
    let r = ray (point(0.0, 0.0, -5.0)) (vector(0.0, 0.0, 1.0))
    colorAt w r |> should equal (color(0.38066, 0.47583, 0.2855))

[<Fact>]
let ``The color with an intersection behind the ray`` () =
    let light = {Position = point(-10.0, 10.0, -10.0); Intensity = color(1.0, 1.0, 1.0)}
    let m = {material() with Color = color(0.8, 1.0, 0.6); Diffuse = 0.7; Specular = 0.2}
    let t = Scaling(0.5, 0.5, 0.5)
    let s1 = {sphere with Material = Some m}
    let s2 = {sphere with Transform = Some t}
    let outer = {s1 with Material = Some {m with Ambient = 1.0}}
    let inner = {s2 with Material = Some {material() with Ambient = 1.0}}
    let w = {Objects = [outer; inner]; LightSource = light}
    let r = ray (point(0.0, 0.0, 0.75)) (vector(0.0, 0.0, -1.0))
    colorAt w r |> should equal inner.Material.Value.Color

[<Fact>]
let ``The transformation matrix for the default orientation`` () =
    let from = point(0.0, 0.0, 0.0)
    let toward = point(0.0, 0.0, -1.0)
    let up = vector(0.0, 1.0, 0.0)
    viewTransform from toward up |> should equal (Matrix(4, 4, Identity))

[<Fact>]
let ``A view transformation matrix looking in positive z direction`` () =
    let from = point(0.0, 0.0, 0.0)
    let toward = point(0.0, 0.0, 1.0)
    let up = vector(0.0, 1.0, 0.0)
    viewTransform from toward up |> should equal (getTransformMatrix (Scaling(-1.0, 1.0, -1.0)))

[<Fact>]
let ``The view transformation moves the world`` () =
    let from = point(0.0, 0.0, 8.0)
    let toward = point(0.0, 0.0, 0.0)
    let up = vector(0.0, 1.0, 0.0)
    viewTransform from toward up |> should equal (getTransformMatrix (Translation(0.0, 0.0, -8.0)))

[<Fact>]
let ``An arbitrary view transformation`` () =
    let from = point(1.0, 3.0, 2.0)
    let toward = point(4.0, -2.0, 8.0)
    let up = vector(1.0, 1.0, 0.0)
    viewTransform from toward up |> should equal (Matrix(4, 4, array2D [[-0.50709; 0.50709;  0.67612; -2.36643];
                                                                        [ 0.76772; 0.60609;  0.12122; -2.82843];
                                                                        [-0.35857; 0.59761; -0.71714;  0.00000];
                                                                        [ 0.00000; 0.00000;  0.00000;  1.00000]]))

[<Fact>]
let ``Constructing a camera`` () =
    let c = camera(160.0<pixels>, 120.0<pixels>, pi/2.0)
    (c.HorizontalSize, c.VerticalSize, c.FieldOfView, c.Transform)
    |> should equal (160.0<pixels>, 120.0<pixels>, pi/2.0, Matrix(4, 4, Identity))

[<Fact>]
let ``The pixel size for a horizontal canvas`` () =
    let c = camera(200.0<pixels>, 125.0<pixels>, pi/2.0)
    c.PixelSize |> should (equalWithin epsilon) 0.01<world>

[<Fact>]
let ``The pixel size for a vertical canvas`` () =
    let c = camera(125.0<pixels>, 200.0<pixels>, pi/2.0)
    c.PixelSize |> should (equalWithin epsilon) 0.01<world>

[<Fact>]
let ``Constructing a ray through the center of the canvas`` () =
    let c = camera(201.0<pixels>, 101.0<pixels>, pi/2.0)
    let r = rayForPixel c 100.0<pixels> 50.0<pixels>
    (r.Origin, r.Direction) |> should equal (point(0.0, 0.0, 0.0), vector(0.0, 0.0, -1.0))

[<Fact>]
let ``Constructing a ray through a corner of the canvas`` () =
    let c = camera(201.0<pixels>, 101.0<pixels>, pi/2.0)
    let r = rayForPixel c 0.0<pixels> 0.0<pixels>
    (r.Origin, r.Direction) |> should equal (point(0.0, 0.0, 0.0), vector(0.66519, 0.33259, -0.66851))

[<Fact>]
let ``Constructing a ray when the camera is transformed`` () =
    let c = {camera(201.0<pixels>, 101.0<pixels>, pi/2.0)
             with Transform = getTransformMatrix (Combination [Rotation(Y, pi/4.0); Translation(0.0, -2.0, 5.0)])}
    let r = rayForPixel c 100.0<pixels> 50.0<pixels>
    (r.Origin, r.Direction) |> should equal (point(0.0, 2.0, -5.0), vector(sqrt(2.0)/2.0, 0.0, -sqrt(2.0)/2.0))

[<Fact>]
let ``Rendering a world with a camera`` () =
    let light = {Position = point(-10.0, 10.0, -10.0); Intensity = color(1.0, 1.0, 1.0)}
    let m = {material() with Color = color(0.8, 1.0, 0.6); Diffuse = 0.7; Specular = 0.2}
    let t = Scaling(0.5, 0.5, 0.5)
    let s1 = {sphere with Material = Some m}
    let s2 = {sphere with Transform = Some t}
    let w = {Objects = [s1; s2]; LightSource = light}
    let from = point(0.0, 0.0, -5.0)
    let toward = point(0.0, 0.0, 0.0)
    let up = vector(0.0, 1.0, 0.0)
    let c = {camera(11.0<pixels>, 11.0<pixels>, pi/2.0)
             with Transform = viewTransform from toward up}
    let image = render c w
    image.[5, 5] |> should equal (color(0.38066, 0.47583, 0.2855))