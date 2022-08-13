/// 3D transformations that operate on tuple types, such as vectors and points
module RayTracer.Transformation

open Utilities
open Tuples
open Matrix

(* The ray tracer in the book is oriented via the left-hand rule, rather than the more typical right-hand rule.
   Thus, images are displayed on an x-y plane, oriented normally, and the positive z direction goes into the image.
   This affects the transformations. For example, rotations also follow the left-hand rule. If you put your left
   thumb along the positive direction, then your fingers curl in the positive direction of rotation.

                                                   y      z
                                                   ^      ^
                                                   |     /
                                                   |    /
                                                   |   /
                                                   |  /
                                                   | /
                                                   |/
                               --------------------/--------------------> x
                                                  /|
                                                 / |
                                                /  |
                                               /   |
                                              /    |
                                             /     |
*)

/// Represents an axis in 3D space
[<Struct>]
type Axis =
    /// X-axis
    | X
    /// Y-axis
    | Y
    /// Z-axis
    | Z

/// Represents a shear component that can be applied in a shear transformation
[<Struct>]
type ShearComponent =
    /// X in proportion to Y
    | Xy
    /// X in proportion to Z
    | Xz
    /// Y in proportion to X
    | Yx
    /// Y in proportion to Z
    | Yz
    /// Z in proportion to X
    | Zx
    /// Z in proportion to Y
    | Zy

/// Represents a 3D transform
type Transform =
    /// Translation moves a point via addition of the components to the point's components
    | Translation  of x: float * y: float * z: float
    /// Scaling moves a point via multiplication of the components with the point's components
    | Scaling      of x: float * y: float * z: float
    /// Scaling moves a point via multiplication of the components with the point's components.
    /// This scales the point's coordinates equally using the single scale factor.
    | ScalingEqual of scaleFactor: float
    /// Reflection moves a point to the other side of the given access, keeping the distance from the axis
    | Reflection   of Axis
    /// Rotation moves a point by rotating by an angle, in radians, relative to an axis
    | Rotation     of Axis * angle: float<radians>
    /// Shearing,  or skew, makes straight lines slanted by moving a component in proportion to another component
    | Shearing     of ShearComponent * proportion: float
    /// Combination chains together several transforms. Transforms listed left to right but will be applied
    /// right to left.
    | Combination  of Transform list

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
    | ScalingEqual factor -> getTransformMatrix (Scaling (factor, factor, factor))
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
    | ScalingEqual factor    -> ScalingEqual (reciprocal factor)
    | Reflection axis        -> Reflection axis
    | Rotation (axis, r)     -> Rotation (axis, -r)
    | Shearing (c, p)        -> Shearing (c, -p)
    | Combination transforms -> Combination (transforms |> List.rev |> List.map inverse)
    
/// Applies the transform matrix to a vector or point
let applyTransformMatrix (transformMatrix: Matrix) (tuple: ITuple<'T, _>) =
    matrixTimesTuple transformMatrix tuple

/// Applies the transform to a vector or point
let applyTransform transform (tuple: ITuple<'T, 'Unit>) =
    let matrix = getTransformMatrix transform
    applyTransformMatrix matrix tuple
    
/// Applies the transpose of the transform to a vector or point
let applyTransposedTransform transform (tuple: ITuple<'T, 'Unit>) =
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
