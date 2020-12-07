module RayTracer.Utilities

[<Measure>] type radians
[<Measure>] type degrees
[<Measure>] type pixels
[<Measure>] type world

let removeRadians (x: float<radians>) = x/1.0<radians>

let pi = System.Math.PI * 1.0<radians>

let epsilon = 0.00001

let compareFloat x y = abs(x-y) <= epsilon

let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd

let roundToInt x = int (round x)

let reciprocal x =
    match x with
    | x when (compareFloat x 0.0) -> 0.0
    | _                           -> 1.0/x