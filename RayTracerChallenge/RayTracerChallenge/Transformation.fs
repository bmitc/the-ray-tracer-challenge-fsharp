module RayTracer.Transformation

open RayTracer.Utilities
open RayTracer.Tuples
open RayTracer.Matrix

type Axis = X | Y | Z

type ShearComponent = Xy | Xz | Yx | Yz | Zx | Zy

type Transform =
    | Translation of x: float * y: float * z: float
    | Scaling     of x: float * y: float * z: float
    | Reflection  of Axis
    | Rotation    of Axis * angle: float<radians>
    | Shearing    of ShearComponent * proportion: float
    | Combination of Transform list

let rec getTransformMatrix transform =
    match transform with
    | Translation (x,y,z) -> Matrix(4, 4, array2D [[1.0; 0.0; 0.0; x  ];
                                                   [0.0; 1.0; 0.0; y  ];
                                                   [0.0; 0.0; 1.0; z  ];
                                                   [0.0; 0.0; 0.0; 1.0]])
    | Scaling (x,y,z)     -> Matrix(4, 4, array2D [[x  ; 0.0; 0.0; 0.0];
                                                   [0.0; y  ; 0.0; 0.0];
                                                   [0.0; 0.0; z  ; 0.0];
                                                   [0.0; 0.0; 0.0; 1.0]])
    | Reflection X        -> Matrix(4, 4, array2D [[-1.0; 0.0; 0.0; 0.0];
                                                   [ 0.0; 1.0; 0.0; 0.0];
                                                   [ 0.0; 0.0; 1.0; 0.0];
                                                   [ 0.0; 0.0; 0.0; 1.0]])
    | Reflection Y        -> Matrix(4, 4, array2D [[ 1.0;  0.0;  0.0; 0.0];
                                                   [ 0.0; -1.0;  0.0; 0.0];
                                                   [ 0.0;  0.0;  1.0; 0.0];
                                                   [ 0.0;  0.0;  0.0; 1.0]])
    | Reflection Z        -> Matrix(4, 4, array2D [[ 1.0;  0.0;  0.0; 0.0];
                                                   [ 0.0;  1.0;  0.0; 0.0];
                                                   [ 0.0;  0.0; -1.0; 0.0];
                                                   [ 0.0;  0.0;  0.0; 1.0]])
    | Rotation (X, r)     -> let r = r/(1.0<radians>)
                             Matrix(4, 4, array2D [[   1.0;    0.0;    0.0; 0.0];
                                                   [   0.0;  cos r; -sin r; 0.0];
                                                   [   0.0;  sin r;  cos r; 0.0];
                                                   [   0.0;    0.0;    0.0; 1.0]])
    | Rotation (Y, r)     -> let r = r/(1.0<radians>)
                             Matrix(4, 4, array2D [[ cos r;    0.0;  sin r; 0.0];
                                                   [   0.0;    1.0;    0.0; 0.0];
                                                   [-sin r;    0.0;  cos r; 0.0];
                                                   [   0.0;    0.0;    0.0; 1.0]])
    | Rotation (Z, r)     -> let r = r/(1.0<radians>)
                             Matrix(4, 4, array2D [[ cos r; -sin r;    0.0; 0.0];
                                                   [ sin r;  cos r;    0.0; 0.0];
                                                   [   0.0;    0.0;    1.0; 0.0];
                                                   [   0.0;    0.0;    0.0; 1.0]])
    | Shearing (c, p)     -> let (x_y, x_z, y_x, y_z, z_x, z_y) =
                                 match c with
                                 | Xy -> (p, 0.0, 0.0, 0.0, 0.0, 0.0)
                                 | Xz -> (0.0, p, 0.0, 0.0, 0.0, 0.0)
                                 | Yx -> (0.0, 0.0, p, 0.0, 0.0, 0.0)
                                 | Yz -> (0.0, 0.0, 0.0, p, 0.0, 0.0)
                                 | Zx -> (0.0, 0.0, 0.0, 0.0, p, 0.0)
                                 | Zy -> (0.0, 0.0, 0.0, 0.0, 0.0, p)
                             Matrix(4, 4, array2D [[1.0; x_y; x_z; 0.0];
                                                   [y_x; 1.0; y_z; 0.0];
                                                   [z_x; z_y; 1.0; 0.0];
                                                   [0.0; 0.0; 0.0; 1.0]])
    | Combination transforms -> List.fold (fun acc transform -> (getTransformMatrix transform) * acc)
                                          (Matrix(4, 4, Identity))
                                          (List.rev transforms)

let rec inverse transform =
    match transform with
    | Translation (x, y, z)  -> Translation (-x, -y, -z)
    | Scaling (x, y, z)      -> Scaling (reciprocal x, reciprocal y, reciprocal z)
    | Reflection axis        -> Reflection axis
    | Rotation (axis, r)     -> Rotation (axis, -r)
    | Shearing (c, p)        -> Shearing (c, -p)
    | Combination transforms -> Combination (transforms |> List.rev |> List.map (fun t -> inverse t))
    
let getInverseTransformMatrix transform = (getTransformMatrix transform).Invert()

let applyTransformMatrix (transformMatrix: Matrix) (tuple: ITuple) =
    let t = Matrix(4, 1, tuple.ToTupleArray(), ByColumn)
    let result = transformMatrix * t
    match tuple with
    | :? Vector -> vector(result.[0,0], result.[1,0], result.[2,0]) :> ITuple
    | :? Point  -> point(result.[0,0], result.[1,0], result.[2,0]) :> ITuple
    | _         -> tuple

/// Applies the given transform to a vector or point. All other ITuple implementations do not transform the tuple.
let applyTransform transform (tuple : ITuple) =
    let matrix = getTransformMatrix transform
    applyTransformMatrix matrix tuple

let applyTransposedTransform transform (tuple : ITuple) =
    let t = Matrix(4, 1, tuple.ToTupleArray(), ByColumn)
    let matrix = getTransformMatrix transform
    let transposedMatrix = matrix.ReplaceSubmatrix(3, 3, matrix.GetSubmatrix(3, 3).Transpose())
    let result = transposedMatrix * t
    match tuple with
    | :? Vector -> vector(result.[0,0], result.[1,0], result.[2,0]) :> ITuple
    | :? Point  -> point(result.[0,0], result.[1,0], result.[2,0]) :> ITuple
    | _         -> tuple

let translate (x,y,z) = applyTransform (Translation (x,y,z))

let scale (x,y,z) = applyTransform (Scaling (x,y,z))

let reflect axis = applyTransform (Reflection axis)

let rotate (axis, angle: float<radians>) = applyTransform (Rotation (axis, angle))

let shear shearComponent proportion = applyTransform (Shearing (shearComponent, proportion))