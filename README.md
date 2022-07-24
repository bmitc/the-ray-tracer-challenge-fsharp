[![build and test](https://github.com/bmitc/the-ray-tracer-challenge-fsharp/actions/workflows/build-and-test.yml/badge.svg?branch=main)](https://github.com/bmitc/the-ray-tracer-challenge-fsharp/actions/workflows/build-and-test.yml)

# The Ray Tracer Challenge in F#
This repository is an F# implementations of the ray tracer found in The Ray Tracer Challenge book by Jamis Buck. [Tests](https://github.com/bmitc/the-ray-tracer-challenge-fsharp/tree/main/RayTracerChallenge/XUnitTests) are fully implemented, and due to F#'s clean syntax and domain modeling, the tests are nearly identical at moments to the psuedocode used to specify the tests in the book. It's been a pleasure to write this code in F#.

Below you'll find code exerpts that generate the displayed image, and these are the examples found in the various chapters of the book. The ray tracer implementation is not yet complete but is fully working through chapter 7 (scenes). Some time is being spent documenting the code and doing some low-hanging performance characterization and enhancements.

## [Chapter 2: Drawing on a Canvas: Putting it Together](https://github.com/bmitc/the-ray-tracer-challenge-fsharp/blob/main/RayTracerChallenge/RayTracerChallenge/scripts/projectile.fsx)

<details>
<summary>F# script to generate below scene</summary>

```fsharp
type Projectile = { Position: Point<1>; Velocity: Vector }

type Environment = { Gravity: Vector; Wind: Vector }

let tick environment projectile =
    { Position = projectile.Position + projectile.Velocity;
      Velocity = projectile.Velocity + environment.Gravity + environment.Wind }

let initialPosition =
    { Position = point(0.0, 1.0, 0.0);
      Velocity = 11.25 * (normalize (vector(1.0, 1.8, 0.0))) }

let initialEnvironment =
    { Gravity = vector(0.0, -0.1, 0.0);
      Wind    = vector(-0.01, 0.0, 0.0) }

let run environment initialPosition (canvas: Canvas<pixels>) filePath =
    let mutable position = initialPosition
    while (tick environment position).Position.Y >= 0.0 do
        position <- (tick environment position)
        canvas.[roundToInt position.Position.X, canvas.Height - (roundToInt position.Position.Y)] <- green
    writeToPPM canvas filePath

run initialEnvironment
    initialPosition
    (Canvas(900<pixels>, 550<pixels>))
    (System.IO.Path.Combine(__SOURCE_DIRECTORY__, "../../../images/projectile.ppm"))
```
</details>

![projectile](https://github.com/bmitc/the-ray-tracer-challenge-fsharp/blob/main/images/projectile.png)

## [Chapter 4: Matrix Transformations: Putting it Together](https://github.com/bmitc/the-ray-tracer-challenge-fsharp/blob/main/RayTracerChallenge/RayTracerChallenge/scripts/clock.fsx)

<details>
<summary>F# script to generate below scene</summary>

```fsharp
let twelveOClock = point(0.0, 1.0, 0.0)
let angle = pi/6.0
let canvasSize = 200<pixels>
let center = point((float canvasSize)/2.0, (float canvasSize)/2.0, 0.0)

let writeHour h =
    let scaleFactor = (float canvasSize) * 3.0 / 8.0
    twelveOClock
    |> rotate (Z, float(h) * angle)             // move the 12 o'clock position to the hour position
    |> scale (scaleFactor, scaleFactor, 0.0)    // scale the clock radius to 3/8 of canvas size
    |> translate (center.X, center.Y, center.Z) // translate the clock to the middle of the canvas
    |> (fun p -> (roundToInt p.X, roundToInt p.Y))

let canvas = Canvas(canvasSize)

List.iter (fun hour -> let (x,y) = writeHour hour
                       canvas.[x,y] <- white)
          [1..12]

writeToPPM canvas (System.IO.Path.Combine(__SOURCE_DIRECTORY__, "../../../Images/clock.ppm"))
```
</details>

![clock](https://github.com/bmitc/the-ray-tracer-challenge-fsharp/blob/main/images/clock.png)

## [Chapter 5: Ray-Sphere Intersections: Putting it Together](https://github.com/bmitc/the-ray-tracer-challenge-fsharp/blob/main/RayTracerChallenge/RayTracerChallenge/scripts/circle.fsx)

<details>
<summary>F# script to generate below scene</summary>

```fsharp
let rayOrigin = pointu<world>(0.0, 0.0, -5.0)
let wallZ = 10.0<world>
let wallSize = 7.0<world>
let canvasSize = 200.0<pixels>
let pixelSize = wallSize / canvasSize
let halfSize = wallSize / 2.0
let canvas = Canvas(canvasSize)

let compute x y =
    let worldX = -halfSize + pixelSize * x
    let worldY =  halfSize - pixelSize * y
    let position = point(worldX, worldY, wallZ)
    let r = ray rayOrigin (normalize (position - rayOrigin))
    let xs = intersect r sphere
    match hit xs with
    | Some _ -> color(0.0, 0.5, 1.0)
    | None   -> color(0.0, 0.0, 0.0)

#time
canvas.UpdatePixels(fun x y _ -> compute (floatUnits<pixels> x) (floatUnits<pixels> y))
#time

writeToPPM canvas (System.IO.Path.Combine(__SOURCE_DIRECTORY__, "../../../images/circle.ppm"))
```
</details>

![circle](https://github.com/bmitc/the-ray-tracer-challenge-fsharp/blob/main/images/circle.png)

## [Chapter 6: Light and Shading: Putting it Together](https://github.com/bmitc/the-ray-tracer-challenge-fsharp/blob/main/RayTracerChallenge/RayTracerChallenge/scripts/lightAndShading.fsx)

<details>
<summary>F# script to generate below scene</summary>

```fsharp
let rayOrigin = pointu<world>(0.0, 0.0, -5.0)
let wallZ = 10.0<world>
let wallSize = 7.0<world>
let canvasSize = 1000.0<pixels>
let pixelSize = wallSize / canvasSize
let halfSize = wallSize / 2.0
let canvas = Canvas(canvasSize)

let m = {material() with Color = color(0.0, 0.5, 1.0)}
let light = {Position = pointu<world>(-10.0, 10.0, -10.0); Intensity = color(1.0, 1.0, 1.0)}

let compute x y =
    let worldX = -halfSize + pixelSize * x
    let worldY =  halfSize - pixelSize * y
    let pos = point(worldX, worldY, wallZ)
    let r = ray rayOrigin (normalize (pos - rayOrigin))
    let xs = intersect r sphere
    match hit xs with
    | Some i -> let point = position r i.Time
                let normal = normalAt i.Object point
                let eye = -r.Direction
                lighting m light point eye normal false
    | None   -> black

#time
canvas.UpdatePixels(fun x y _ -> compute (floatUnits<pixels> x) (floatUnits<pixels> y))
#time

writeToPPM canvas (System.IO.Path.Combine(__SOURCE_DIRECTORY__, "../../../images/lightAndShading.ppm"))
```
</details>

![lightAndShading](https://github.com/bmitc/the-ray-tracer-challenge-fsharp/blob/main/images/lightAndShading.png)

## [Chapter 7: Making a Scene: Putting it Together](https://github.com/bmitc/the-ray-tracer-challenge-fsharp/blob/main/RayTracerChallenge/RayTracerChallenge/scripts/scene.fsx)

<details>
<summary>F# script to generate below scene</summary>

```fsharp
//******************************************
// Scene objects
//******************************************

let floor = {sphere with Transform = Some (Scaling(10.0, 0.01, 10.0));
                         Material = Some {material() with Color = color(1.0, 0.9, 0.9);
                                                          Specular = 0.0}}

let leftWall = {sphere with Transform = Some (Combination [Translation(0.0,0.0,5.0);
                                                           Rotation(Y,-pi/4.0);
                                                           Rotation(X,pi/2.0);
                                                           Scaling(10.0, 0.01, 10.0)]);
                            Material = floor.Material}

let rightWall = {sphere with Transform = Some (Combination [Translation(0.0,0.0,5.0);
                                                            Rotation(Y,pi/4.0);
                                                            Rotation(X,pi/2.0);
                                                            Scaling(10.0, 0.01, 10.0)]);
                             Material = floor.Material}

let middle = {sphere with Transform = Some (Translation(-0.5, 1.0, 0.5));
                          Material = Some {material() with Color = color(0.1, 1.0, 0.5);
                                                           Diffuse = 0.7;
                                                           Specular = 0.3}}

let right = {sphere with Transform = Some (Combination [Translation(1.5, 0.5, -0.5); Scaling(0.5, 0.5, 0.5)]);
                         Material = Some {material() with Color = color(0.5, 1.0, 0.1);
                                                          Diffuse = 0.7;
                                                          Specular = 0.3}}

let left = {sphere with Transform = Some (Combination [Translation(-1.5, 0.33, -0.75); Scaling(0.33, 0.33, 0.33)]);
                        Material = Some {material() with Color = color(0.0, 0.5, 0.8);
                                                         Diffuse = 0.7;
                                                         Specular = 0.3}}

//******************************************
// World
//******************************************

let light = {Position = pointu<world>(-10.0, 10.0, -10.0); Intensity = color(1.0, 1.0, 1.0)}
let world = {Objects = [floor; leftWall; rightWall; middle; left; right]; LightSource = light}

let camera = {camera(2000.0<pixels>, 1000.0<pixels>, pi/3.0)
              with Transform = viewTransform (point(0.0, 1.5, -5.0)) (point(0.0, 1.0, 0.0)) (vector(0.0, 1.0, 0.0)) }

#time
let image = render camera world
#time

writeToPPM image (System.IO.Path.Combine(__SOURCE_DIRECTORY__, "../../../images/scene.ppm"))
```
</details>

![scene](https://github.com/bmitc/the-ray-tracer-challenge-fsharp/blob/main/images/scene.png)

## [Chapter 8: Shadows: Putting it Together](https://github.com/bmitc/the-ray-tracer-challenge-fsharp/blob/main/RayTracerChallenge/RayTracerChallenge/scripts/shadows.fsx)

<details>
<summary>F# script to generate below scene</summary>

```fsharp
//******************************************
// Scene objects
//******************************************

let floor = {sphere with Transform = Some (Scaling(10.0, 0.01, 10.0));
                         Material = Some {material() with Color = color(1.0, 0.9, 0.9);
                                                          Specular = 0.0}}

let leftWall = {sphere with Transform = Some (Combination [Translation(0.0,0.0,5.0);
                                                           Rotation(Y,-pi/4.0);
                                                           Rotation(X,pi/2.0);
                                                           Scaling(10.0, 0.01, 10.0)]);
                            Material = floor.Material}

let rightWall = {sphere with Transform = Some (Combination [Translation(0.0,0.0,5.0);
                                                            Rotation(Y,pi/4.0);
                                                            Rotation(X,pi/2.0);
                                                            Scaling(10.0, 0.01, 10.0)]);
                             Material = floor.Material}

let middle = {sphere with Transform = Some (Translation(-0.5, 1.0, 0.5));
                          Material = Some {material() with Color = color(0.1, 1.0, 0.5);
                                                           Diffuse = 0.7;
                                                           Specular = 0.3}}

let right = {sphere with Transform = Some (Combination [Translation(1.5, 0.5, -0.5); Scaling(0.5, 0.5, 0.5)]);
                         Material = Some {material() with Color = color(0.5, 1.0, 0.1);
                                                          Diffuse = 0.7;
                                                          Specular = 0.3}}

let left = {sphere with Transform = Some (Combination [Translation(-1.5, 0.33, -0.75); Scaling(0.33, 0.33, 0.33)]);
                        Material = Some {material() with Color = color(0.0, 0.5, 0.8);
                                                         Diffuse = 0.7;
                                                         Specular = 0.3}}

//******************************************
// World
//******************************************

let light = {Position = pointu<world>(-10.0, 10.0, -10.0); Intensity = color(1.0, 1.0, 1.0)}
let world = {Objects = [floor; leftWall; rightWall; middle; left; right]; LightSource = light}

let camera = {camera(2000.0<pixels>, 1000.0<pixels>, pi/3.0)
              with Transform = viewTransform (point(0.0, 1.5, -5.0)) (point(0.0, 1.0, 0.0)) (vector(0.0, 1.0, 0.0)) }

#time
let image = render camera world
#time

writeToPPM image (System.IO.Path.Combine(__SOURCE_DIRECTORY__, "../../../images/shadows.ppm"))
```
</details>

![scene](https://github.com/bmitc/the-ray-tracer-challenge-fsharp/blob/main/images/shadows.png)
