/// Utility units of measure, constants, and functions
module RayTracer.Utilities

/// Represents angles in radians
[<Measure>] type radians

/// Represents angles in degrees
[<Measure>] type degrees

/// Represents a point in pixels
[<Measure>] type pixels

/// Represents a point in world space
[<Measure>] type world

/// Represents a point in object space
[<Measure>] type object

/// Represents a point in pattern space
[<Measure>] type pattern

/// Removes any units of measure
let inline removeUnits (x: float<'Unit>) = float x

/// Removes the radians units of measure from the float
let inline removeRadians (x: float<radians>) = x/1.0<radians>

/// Cast the float to have the given units of measure
let inline castFloatUnit<[<Measure>] 'Unit> (x: float) = LanguagePrimitives.FloatWithMeasure<'Unit> x

/// Cast the integer to have the given units of measure
let inline castIntUnit<[<Measure>] 'Unit> (x: int) = LanguagePrimitives.Int32WithMeasure<'Unit> x

/// Converts the float with units to a float with new units
let inline convertUnits<[<Measure>] 'OldUnit, [<Measure>] 'NewUnit>
    (x: float<'OldUnit>) : float<'NewUnit> =
        x |> removeUnits |> castFloatUnit<'NewUnit>

/// Converts an integer to a float and then casts to a float with the given units
let inline floatUnits<[<Measure>] 'Unit> (x: int) = x |> float |> castFloatUnit<'Unit>

/// Converts a float to an integer and then casts to an integer with the given units
let inline intUnits<[<Measure>] 'Unit> (x: float) = x |> int |> castIntUnit<'Unit>

/// Converts an integer to a float while preserving the integer's units of measure
let inline floatPreserveUnits (x: int<'Unit>) = x |> float |> castFloatUnit<'Unit>

/// Converts a float to an integer while preserving the float's units of measure
let inline intPreserveUnits (x: float<'Unit>) = x |> int |> castIntUnit<'Unit>

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

/// Returns the floor of the given float converted to an integer
let floorInt (x: float<'Unit>) =
    x
    |> removeUnits
    |> floor
    |> int

//**********************************************************************
//***** Perlin noise ***************************************************
//**********************************************************************

let permutation =
   [| 151; 160; 137;  91;  90;  15; 131;  13; 201;  95;  96;  53; 194; 233;   7; 225;
      140;  36; 103;  30;  69; 142;   8;  99;  37; 240;  21;  10;  23; 190;   6; 148;
      247; 120; 234;  75;   0;  26; 197;  62;  94; 252; 219; 203; 117;  35;  11;  32;
       57; 177;  33;  88; 237; 149;  56;  87; 174;  20; 125; 136; 171; 168;  68; 175;
       74; 165;  71; 134; 139;  48;  27; 166;  77; 146; 158; 231;  83; 111; 229; 122;
       60; 211; 133; 230; 220; 105;  92;  41;  55;  46; 245;  40; 244; 102; 143;  54;
       65;  25;  63; 161;   1; 216;  80;  73; 209;  76; 132; 187; 208;  89;  18; 169;
      200; 196; 135; 130; 116; 188; 159;  86; 164; 100; 109; 198; 173; 186;   3;  64;
       52; 217; 226; 250; 124; 123;   5; 202;  38; 147; 118; 126; 255;  82;  85; 212;
      207; 206;  59; 227;  47;  16;  58;  17; 182; 189;  28;  42; 223; 183; 170; 213;
      119; 248; 152;   2;  44; 154; 163;  70; 221; 153; 101; 155; 167;  43; 172;   9;
      129;  22;  39; 253;  19;  98; 108; 110;  79; 113; 224; 232; 178; 185; 112; 104;
      218; 246;  97; 228; 251;  34; 242; 193; 238; 210; 144;  12; 191; 179; 162; 241;
       81;  51; 145; 235; 249;  14; 239; 107;  49; 192; 214;  31; 181; 199; 106; 157;
      184;  84; 204; 176; 115; 121;  50;  45; 127;   4; 150; 254; 138; 236; 205;  93;
      222; 114;  67;  29;  24;  72; 243; 141; 128; 195;  78;  66; 215;  61; 156; 180 |]

let p = Array.append permutation permutation

let fade t = t * t * t * (t * (t * 6.0 - 15.0) + 10.0)

let lerp (t, a, b) : float = a + t * (b - a)

let grad (hash, x, y, z) : float =
    // Convert low 4 bits of hash code into 12 gradient directions
    let h = hash &&& 15
    let u = if h < 8 then x else y
    let v = if h < 4 then y else
                if h = 12 || h = 14 then x else z
    (if h &&& 1 = 0 then u else -u) + (if h &&& 2 = 0 then v else -v)

let noise (x: float, y: float, z: float) =
    /// Find unit cube that contains point
    let X = floorInt(x) &&& 255
    let Y = floorInt(y) &&& 255
    let Z = floorInt(z) &&& 255

    // Find relative x, y, z of point in cube
    let x = x - floor(x)
    let y = y - floor(y)
    let z = z - floor(z)

    // Compute fade curves for each of x, y, z
    let u = fade x
    let v = fade y
    let w = fade z

    // Hash coordinates of the 8 cube corners
    let A  = p[ X ] + Y
    let B  = p[X+1] + Y
    let AA = p[ A ] + Z
    let BA = p[ B ] + Z
    let AB = p[A+1] + Z
    let BB = p[B+1] + Z
    
    // And add blended results from 8 corners of cube
    lerp (w, lerp (v, lerp (u, grad (p[ AA ], x,     y,     z     ),
                               grad (p[ BA ], x-1.0, y,     z     )),
                      lerp (u, grad (p[ AB ], x,     y-1.0, z     ),
                               grad (p[ BB ], x-1.0, y-1.0, z     ))),
             lerp (v, lerp (u, grad (p[AA+1], x,     y,     z-1.0 ),
                               grad (p[BA+1], x-1.0, y,     z-1.0 )),
                      lerp (u, grad (p[AB+1], x,     y-1.0, z-1.0 ),
                               grad (p[BB+1], x-1.0, y-1.0, z-1.0 ))))
