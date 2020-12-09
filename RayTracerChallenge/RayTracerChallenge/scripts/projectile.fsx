// Implementation of the putting it together at the end of Chapter 2

#load "load.fsx"

open RayTracer.Utilities
open RayTracer.Tuples
open RayTracer.Color
open RayTracer.Canvas
open RayTracer.PPM

type Projectile = { Position: Point; Velocity: Vector }

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

let run environment initialPosition (canvas: Canvas) filePath =
    let mutable position = initialPosition
    while (tick environment position).Position.Y >= 0.0 do
        position <- (tick environment position)
        canvas.[roundToInt position.Position.X, canvas.Height - (roundToInt position.Position.Y)] <- green
    writeToPPM canvas filePath

run initialEnvironment
    initialPosition
    (Canvas(900, 550))
    (System.IO.Path.Combine(__SOURCE_DIRECTORY__, "../../../images/projectile.ppm"))