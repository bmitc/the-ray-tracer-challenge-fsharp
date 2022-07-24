[![build and test](https://github.com/bmitc/the-ray-tracer-challenge-fsharp/actions/workflows/build-and-test.yml/badge.svg?branch=main)](https://github.com/bmitc/the-ray-tracer-challenge-fsharp/actions/workflows/build-and-test.yml)

# The Ray Tracer Challenge in F#
This repository is an F# implementation of the ray tracer found in [*The Ray Tracer Challenge: A Test-Driven Guide to Your First 3D Renderer*](https://pragprog.com/titles/jbtracer/the-ray-tracer-challenge/) by Jamis Buck. The [tests](https://github.com/bmitc/the-ray-tracer-challenge-fsharp/tree/main/RayTracerChallenge/XUnitTests) described in the book and more are fully implemented, and due to F#'s clean syntax and domain modeling, the tests are nearly identical at moments to the psuedocode used to specify the tests in the book. It has been a pleasure and a lot of fun to write this code in F#.

The book is written from an agnostic point of view regarding programming languages, although the suggested tests and implementations do lean towards mutability and OOP type of languages. This implementation has been made to be as idiomatic to F# as possible, making full use of F#'s functional-first but multiparadigm nature. Functional programming is mixed with OOP naturally, although immutability is still highly preferred.

## A note on implementation

Domain-driven design is heavily used in this implementation, where F# types of discriminated unions, records, and sometimes classes are used to model all of the various components of the ray tracer. Probably the largest departure from the book, and an excellent example of functional programming, is defining 3D transformations using a discriminated union:

```fsharp
/// Represents a 3D transform
type Transform =
    | Translation of x: float * y: float * z: float
    | Scaling     of x: float * y: float * z: float
    | Reflection  of Axis
    | Rotation    of Axis * angle: float<radians>
    | Shearing    of ShearComponent * proportion: float
    | Combination of Transform list // transforms will be listed left to right but applied right to left
```

The book does this differently and thus requires the computation of matrix inverses to compute inverses of these transformations. However, this is actually an example where the functional approach improves performance because creating inverses is as simple as pattern matching on the transformations:

```fsharp
/// Invert the transform
let rec inverse transform =
    match transform with
    | Translation (x, y, z)  -> Translation (-x, -y, -z)
    | Scaling (x, y, z)      -> Scaling (reciprocal x, reciprocal y, reciprocal z)
    | Reflection axis        -> Reflection axis
    | Rotation (axis, r)     -> Rotation (axis, -r)
    | Shearing (c, p)        -> Shearing (c, -p)
    | Combination transforms -> Combination (transforms |> List.rev |> List.map (fun t -> inverse t))
```
Note that the definition of the inverse of each transform essentially follows and highlights the intuitive notion of the inverse of a transformation.

The matrix representing any given transform can be directly returned by again pattern matching on the transformations. An abbreviated implementation is:

```fsharp
/// Get the matrix that represents the transform
let rec getTransformMatrix transform =
    match transform with
    | Translation (x,y,z) -> Matrix(4, 4, array2D [[1.0; 0.0; 0.0;  x ];
                                                   [0.0; 1.0; 0.0;  y ];
                                                   [0.0; 0.0; 1.0;  z ];
                                                   [0.0; 0.0; 0.0; 1.0]])
    | Scaling (x,y,z)     -> Matrix(4, 4, array2D [[ x ; 0.0; 0.0; 0.0];
                                                   [0.0;  y ; 0.0; 0.0];
                                                   [0.0; 0.0;  z ; 0.0];
                                                   [0.0; 0.0; 0.0; 1.0]])
    // and so on ...
```

Lastly, one of the tests exhibits the nicety of this approach, in that transforms can simply be strung together in pipe operations:

```fsharp
[<Fact>]
let ``Individual transformations are applied in sequence`` () =
    let p = point(1.0, 0.0, 1.0)
    
    p
    |> rotate (X, pi/2.0)
    |> scale (5.0, 5.0, 5.0)
    |> translate (10.0, 5.0, 7.0)
    |> should equal (point(15.0, 0.0, 7.0))
```
Each one of the transformation functions simply creates a transformation of the same name and applies it to the point by computing the matrix product of the transform's matrix and the point.

## Example renders

Below you'll find code exerpts that generate the displayed image, and these are the examples found in the various chapters of the book. The ray tracer implementation is not yet complete but is fully working through Chapter 8: Shadows.

### [Chapter 2: Drawing on a Canvas: Putting it Together](https://github.com/bmitc/the-ray-tracer-challenge-fsharp/blob/main/RayTracerChallenge/RayTracerChallenge/scripts/projectile.fsx)

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

### [Chapter 4: Matrix Transformations: Putting it Together](https://github.com/bmitc/the-ray-tracer-challenge-fsharp/blob/main/RayTracerChallenge/RayTracerChallenge/scripts/clock.fsx)

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

### [Chapter 5: Ray-Sphere Intersections: Putting it Together](https://github.com/bmitc/the-ray-tracer-challenge-fsharp/blob/main/RayTracerChallenge/RayTracerChallenge/scripts/circle.fsx)

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

### [Chapter 6: Light and Shading: Putting it Together](https://github.com/bmitc/the-ray-tracer-challenge-fsharp/blob/main/RayTracerChallenge/RayTracerChallenge/scripts/lightAndShading.fsx)

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

### [Chapter 7: Making a Scene: Putting it Together](https://github.com/bmitc/the-ray-tracer-challenge-fsharp/blob/main/RayTracerChallenge/RayTracerChallenge/scripts/scene.fsx)

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

### [Chapter 8: Shadows: Putting it Together](https://github.com/bmitc/the-ray-tracer-challenge-fsharp/blob/main/RayTracerChallenge/RayTracerChallenge/scripts/shadows.fsx)

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
