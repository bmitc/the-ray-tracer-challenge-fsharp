/// Utility units of measure, constants, and functions
module RayTracer.Utilities

/// Represents angles in radians
[<Measure>] type radians

/// Represents angles in degrees
[<Measure>] type degrees

/// Represents a point in pixels
[<Measure>] type pixels

/// Represents a point in world units
[<Measure>] type world

/// Removes any units of measure
let inline removeUnits (x: float<_>) = float x

/// Removes the radians units of measure from the float
let inline removeRadians (x: float<radians>) = x/1.0<radians>

/// Cast the given float to have the given units of measure
let inline castFloatUnit<[<Measure>] 'Unit> (x: float) = LanguagePrimitives.FloatWithMeasure<'Unit> x

/// Cast the given int to have the given units of measure
let inline castIntUnit<[<Measure>] 'Unit> (x: int) = LanguagePrimitives.Int32WithMeasure<'Unit> x

/// Converts an integer to a float and then casts to a float with the given units
let inline floatUnits<[<Measure>] 'Unit> (x: int) = x |> float |> castFloatUnit<'Unit>

/// Converts a float to an integer and then casts to an integer with the given units
let inline intUnits<[<Measure>] 'Unit> (x: float) = x |> int |> castIntUnit<'Unit>

/// Constant for pi in radians
let pi = System.Math.PI * 1.0<radians>

/// Constant for use in comparing floats within the ray tracer
[<Literal>]
let epsilon = 0.00001

/// Constant, in world units, for use in comparing floats within the ray tracer
let epsilonWorld = epsilon * 1.0<world>

/// Compares the two floats to see if they are epsilon away from each other.
/// See the definition of epsilon to see the resolution of the compare.
/// Units of measure are ignored.
let compareFloat (x: float<'u>) (y: float<'u>) =
    abs(removeUnits(x) - removeUnits(y)) <= epsilon

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
