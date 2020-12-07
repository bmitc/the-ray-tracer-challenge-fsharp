module RayTracer.Projectile

open RayTracer.Tuples
open RayTracer.Canvas
open RayTracer.PPM
open RayTracer.Color

type Projectile = {Position : Point; Velocity : Vector}

type Environment = {Gravity : Vector; Wind : Vector}

let tick environment projectile =
    {Position = projectile.Position + projectile.Velocity;
     Velocity = projectile.Velocity + environment.Gravity + environment.Wind}

let initialPosition1 = {Position = point(0.0, 1.0, 0.0); Velocity = normalize (vector(1.0, 10.0, 0.0))}

let initialPosition2 = {Position = point(0.0, 1.0, 0.0); Velocity = 11.25 * (normalize (vector(1.0, 1.8, 0.0)))}

let initialEnvironment = {Gravity = vector(0.0, -0.1, 0.0); Wind = vector(-0.01, 0.0, 0.0)}

let c = canvas(900, 550)

let run environment initialPosition (initialCanvas : Canvas) filePath =
    let mutable position = initialPosition
    let mutable canvas = initialCanvas
    while (tick environment position).Position.Y >= 0.0 do
        position <- (tick environment position)
        canvas <- writePixel (int(position.Position.X), canvas.height - int(position.Position.Y)) (color(0.0, 1.0, 0.0)) canvas
    writeToPPM canvas filePath