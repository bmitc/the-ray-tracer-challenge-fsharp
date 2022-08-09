﻿/// Provides ability to perturb a point via Perlin noise
module RayTracer.Perlin

open Utilities
open Tuples

// Reference: https://mrl.cs.nyu.edu/~perlin/noise/

/// A permutation array of size 256 that is uniquely seeded to be used
/// as the permutation array for the Perlin noise algorithm
let private permutation() =
    let seed = System.DateTime.Now.Ticks |> int
    let random = System.Random(seed)
    Array.init 256 (fun _ -> random.Next(0, 255))
    
///// The static permutation array of size 256 defined by the Perlin noise algorithm
//let permutation =
//   [| 151; 160; 137;  91;  90;  15; 131;  13; 201;  95;  96;  53; 194; 233;   7; 225;
//      140;  36; 103;  30;  69; 142;   8;  99;  37; 240;  21;  10;  23; 190;   6; 148;
//      247; 120; 234;  75;   0;  26; 197;  62;  94; 252; 219; 203; 117;  35;  11;  32;
//       57; 177;  33;  88; 237; 149;  56;  87; 174;  20; 125; 136; 171; 168;  68; 175;
//       74; 165;  71; 134; 139;  48;  27; 166;  77; 146; 158; 231;  83; 111; 229; 122;
//       60; 211; 133; 230; 220; 105;  92;  41;  55;  46; 245;  40; 244; 102; 143;  54;
//       65;  25;  63; 161;   1; 216;  80;  73; 209;  76; 132; 187; 208;  89;  18; 169;
//      200; 196; 135; 130; 116; 188; 159;  86; 164; 100; 109; 198; 173; 186;   3;  64;
//       52; 217; 226; 250; 124; 123;   5; 202;  38; 147; 118; 126; 255;  82;  85; 212;
//      207; 206;  59; 227;  47;  16;  58;  17; 182; 189;  28;  42; 223; 183; 170; 213;
//      119; 248; 152;   2;  44; 154; 163;  70; 221; 153; 101; 155; 167;  43; 172;   9;
//      129;  22;  39; 253;  19;  98; 108; 110;  79; 113; 224; 232; 178; 185; 112; 104;
//      218; 246;  97; 228; 251;  34; 242; 193; 238; 210; 144;  12; 191; 179; 162; 241;
//       81;  51; 145; 235; 249;  14; 239; 107;  49; 192; 214;  31; 181; 199; 106; 157;
//      184;  84; 204; 176; 115; 121;  50;  45; 127;   4; 150; 254; 138; 236; 205;  93;
//      222; 114;  67;  29;  24;  72; 243; 141; 128; 195;  78;  66; 215;  61; 156; 180 |]

/// The duplicate permutation array of size 512 as defined in the Perlin noise algorithm
let private p =
    let perm = permutation()
    Array.append perm perm

let private fade t = t * t * t * (t * (t * 6.0 - 15.0) + 10.0)

let private lerp (t, a, b) : float = a + t * (b - a)

let private grad (hash, x, y, z) : float =
    // Convert low 4 bits of hash code into 12 gradient directions
    let h = hash &&& 15
    let u = if h < 8 then x else y
    let v = if h < 4 then y else
                if h = 12 || h = 14 then x else z
    (if h &&& 1 = 0 then u else -u) + (if h &&& 2 = 0 then v else -v)

/// Perlin noise at a 3D point
let private noise (x: float, y: float, z: float) =
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

/// Perturbs a point in any unit space with Perlin noise
let perturbPoint (p: Point<'Unit>) =
    //let random = System.Random()
    //let point = (convertPointUnits<'Unit, 1> p) + point(random.NextDouble() - 0.5, random.NextDouble() - 0.5, random.NextDouble() - 0.5)
    let point = (convertPointUnits<'Unit, 1> p)
    let noise = noise(point.X, point.Y, point.Z) - 0.5
    let noiseVector = vector(noise, noise, noise) //|> normalize
    let perturbedPoint = point + 1.0 * noiseVector
    convertPointUnits<1, 'Unit> perturbedPoint
