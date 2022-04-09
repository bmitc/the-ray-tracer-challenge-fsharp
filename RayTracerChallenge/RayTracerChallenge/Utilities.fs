/// Utility units of measure, constants, and functions
module RayTracer.Utilities

[<Measure>] type radians
[<Measure>] type degrees
[<Measure>] type pixels
[<Measure>] type world

let inline removeUnits (x: float<_>) = float x

/// Removes the radians unit of measure from the float
let inline removeRadians (x: float<radians>) = x/1.0<radians>

/// Constant for pi in radians
let pi = System.Math.PI * 1.0<radians>

/// Constant for use in comparing floats within the ray tracer
[<Literal>]
let epsilon = 0.00001

/// Compares the two floats to see if they are epsilon away from each other.
/// See the definition of epsilon to see the resolution of the compare.
let compareFloat x y = abs(x-y) <= epsilon

/// Rounds the given float to the nearest integer and converts to an int
let inline roundToInt x = int (round x)

/// Calculates the reciprocal of the float. If the input is close, to within
/// epsilon, of 0.0, then this function returns 0.0. See the definition of epsilon.
let reciprocal x =
    match x with
    | x when (compareFloat x 0.0) -> 0.0
    | _                           -> 1.0/x

/// An active pattern for matching an integer to know when it's odd or even
let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd

/// Given a 2D array's width (number of columns), converts a 1D array index to a 2D array (x,y) index
let inline convert1DIndexTo2DIndex index width = (index % width, index / width)

/// Given a 2D array's width (number of columns), converts a 2D array (x,y) index to a 1D array index
let inline convert2DIndexTo1DIndex x y width = x + y * width