module RayTracer.Utilities

[<Measure>] type radians
[<Measure>] type degrees
[<Measure>] type pixels
[<Measure>] type world

/// Removes the radians unit of measure from the float
let removeRadians (x: float<radians>) = x/1.0<radians>

/// Constant for pi in radians
let pi = System.Math.PI * 1.0<radians>

/// Constant for use in comparing floats within the ray tracer
let epsilon = 0.00001

/// Compares the two floats to see if they are epsilon away from each other.
/// See the definition of epsilon to see the resolution of the compare.
let compareFloat x y = abs(x-y) <= epsilon

/// Rounds the given float to the nearest integer and converts to an int
let roundToInt x = int (round x)

/// Calculates the reciprocal of the float. If the input is close, to within
/// epsilon, of 0.0, then this function returns 0.0. See the definition of epsilon.
let reciprocal x =
    match x with
    | x when (compareFloat x 0.0) -> 0.0
    | _                           -> 1.0/x

/// An active pattern for matching an integer to know when it's odd or even
let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd