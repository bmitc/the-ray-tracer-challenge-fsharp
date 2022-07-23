/// 3D transformations that operate on tuple types, such as vectors and points
module RayTracer.Transformation

open Utilities
open Tuples
open Matrix

/// Represents one of the axis in 3D space
type Axis = X | Y | Z

/// Represents a shear component that can be applied in a shear transformation
type ShearComponent = Xy | Xz | Yx | Yz | Zx | Zy

/// Represents a 3D transform
type Transform =
    | Translation of x: float * y: float * z: float
    | Scaling     of x: float * y: float * z: float
    | Reflection  of Axis
    | Rotation    of Axis * angle: float<radians>
    | Shearing    of ShearComponent * proportion: float
    | Combination of Transform list // transforms will be listed left to right but applied right to left

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
    | Rotation (X, r)     -> let r = removeRadians r
                             Matrix(4, 4, array2D [[   1.0;    0.0;    0.0; 0.0];
                                                   [   0.0;  cos r; -sin r; 0.0];
                                                   [   0.0;  sin r;  cos r; 0.0];
                                                   [   0.0;    0.0;    0.0; 1.0]])
    | Rotation (Y, r)     -> let r = removeRadians r
                             Matrix(4, 4, array2D [[ cos r;    0.0;  sin r; 0.0];
                                                   [   0.0;    1.0;    0.0; 0.0];
                                                   [-sin r;    0.0;  cos r; 0.0];
                                                   [   0.0;    0.0;    0.0; 1.0]])
    | Rotation (Z, r)     -> let r = removeRadians r
                             Matrix(4, 4, array2D [[ cos r; -sin r;    0.0; 0.0];
                                                   [ sin r;  cos r;    0.0; 0.0];
                                                   [   0.0;    0.0;    1.0; 0.0];
                                                   [   0.0;    0.0;    0.0; 1.0]])
    | Shearing (c, p)     -> let (x_y, x_z, y_x, y_z, z_x, z_y) = // only support a single component being sheared at a time
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

/// Invert the transform
let rec inverse transform =
    match transform with
    | Translation (x, y, z)  -> Translation (-x, -y, -z)
    | Scaling (x, y, z)      -> Scaling (reciprocal x, reciprocal y, reciprocal z)
    | Reflection axis        -> Reflection axis
    | Rotation (axis, r)     -> Rotation (axis, -r)
    | Shearing (c, p)        -> Shearing (c, -p)
    | Combination transforms -> Combination (transforms |> List.rev |> List.map (fun t -> inverse t))
    
/// Applies the transform matrix to a vector or point
let applyTransformMatrix (transformMatrix: Matrix) (tuple: ITuple<'T, _>) =
    matrixTimesTuple transformMatrix tuple

/// Applies the transform to a vector or point
let applyTransform transform (tuple : ITuple<'T, _>) =
    let matrix = getTransformMatrix transform
    applyTransformMatrix matrix tuple
    
/// Applies the transpose of the transform to a vector or point
let applyTransposedTransform transform (tuple : ITuple<'T, _>) =
    let matrix = getTransformMatrix transform
    let transposedMatrix = matrix.ReplaceSubmatrix(3, 3, matrix.GetSubmatrix(3, 3).Transpose())
    applyTransformMatrix transposedMatrix tuple

/// Translates a vector or point by (x,y,z)
let translate (x,y,z) = Translation (x,y,z) |> applyTransform

/// Scales a vector or point by (x,y,z)
let scale (x,y,z) = Scaling (x,y,z) |> applyTransform

/// Reflects a vector or point across the axis
let reflect axis = Reflection axis |> applyTransform

/// Rotates a vector or point around the axis by the given angle
let rotate (axis, angle: float<radians>) = Rotation (axis, angle) |> applyTransform

/// Shears a vector or point via the shear component by the given proportion
let shear shearComponent proportion = Shearing (shearComponent, proportion) |> applyTransform