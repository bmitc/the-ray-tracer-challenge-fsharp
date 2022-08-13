/// Vector and point types and operations and a tuple interface
module RayTracer.Tuples

// This compiler directive is to get rid of the FS1240 warning when type testing
// types with units of measure
#nowarn "1240"

open Utilities

// Tuples are intentionally separated out into separate types of Vector and Point.
// This is because they are distinctilly two different types with enough variation
// that they don't belong as a single type. The ITuple interface provides shared
// behavior, that primarily allows Vector and Point to be transformed with matrices.
// There is a small hit in duplicated code, but this duplication is small and contained
// whereas the correct type design pays off in the overall design of the ray tracer.

/// Interface used to convert 3D tuple-like elements to and from a 4D tuple
type ITuple<'Tuple, [<Measure>] 'Unit> =

    /// The first element of the tupe-like element
    abstract member X1 : float<'Unit>

    /// The second element of the tupe-like element
    abstract member X2 : float<'Unit>

    /// The third element of the tupe-like element
    abstract member X3 : float<'Unit>

    /// Converts a 3D tuple-like element to an array of length four consisting of
    /// the three components plus a fourth component. The array is essentially the
    /// homogeneous coordinate representation of the tuple.
    abstract member ToTupleArray : unit -> float<'Unit>[]

    /// Creates a 3D tuple-like element from a tuple array
    abstract member FromTupleArray : float<'Unit>[] -> 'Tuple

/// Represents a 3D vector
[<Struct; CustomEquality; NoComparison>]
type Vector = { I: float; J: float; K: float } with

    /// Maps the operation to each element of the vector
    static member mapElementwise op v = {I = op v.I; J = op v.J; K = op v.K}

    /// Maps the operation pairwise across two vectors.
    /// For example, mapPaiwise (+) u v = {I = u.I + v.I; J = u.J + v.J; K = u.K + v.K},
    /// where the infix notation is only used for convenience here.
    static member mapPairwise op u v = { I = op u.I v.I; J = op u.J v.J; K = op u.K v.K }
    
    /// Add a constant to each element of a vector
    static member ( + ) (a: float, v) = Vector.mapElementwise ((+) a) v

    /// Multiply each element of a vector by a constant
    static member ( * ) (a: float, v) = Vector.mapElementwise ((*) a) v

    /// Divide each element of the vector by a constant
    static member ( / ) (v, a: float) = Vector.mapElementwise (fun x -> x / a) v

    /// Add two vectors
    static member ( + ) (u, v) = Vector.mapPairwise (+) u v

    /// Subtract two vectors
    static member ( - ) (u, v) = Vector.mapPairwise (-) u v

    /// Multiply two vectors, multiplying each element in one vector by the corresponding
    /// positional element in the other vector
    static member ( * ) (u, v) = Vector.mapPairwise (*) u v

    /// Negate each element of a vector
    static member ( ~- ) (u: Vector) = -1.0 * u

    /// Overrides the Object.Equals method to provide a custom equality compare for Vector records
    override x.Equals object =
        match object with
        | :? Vector as v -> compareFloat x.I v.I &&
                            compareFloat x.J v.J &&
                            compareFloat x.K v.K
        | _ -> false

    /// Overrides the Object.GetHashCode method, which is recommended when overriding Object.Equals 
    override x.GetHashCode () = hash x // Re-use the built-in hash for records

    // Implements the ITuple interface to be treated like a tuple-like element.
    // Vectors are treated as dimensionless, so the units of measure are <1>.
    interface ITuple<Vector, 1> with
        member this.X1 = this.I
        member this.X2 = this.J
        member this.X3 = this.K
        member this.ToTupleArray () = [| this.I; this.J; this.K; 0.0 |]
        member _.FromTupleArray array = { I = array.[0]; J = array.[1]; K = array.[2] }

/// Convenience function for creating a Vector record
let inline vector (i,j,k) = { I = i; J = j; K = k }

/// Represents a 3D point
[<Struct; CustomEquality; NoComparison>]
type Point<[<Measure>] 'Unit> = { X: float<'Unit>; Y: float<'Unit>; Z: float<'Unit> } with

    /// Maps the operation to each element of the vector
    static member mapElementwise op p = {X = op p.X; Y = op p.Y; Z = op p.Z}

    /// Maps the operation pairwise across two points.
    /// For example, mapPaiwise (+) p q = {X = p.X + q.X; Y = p.Y + q.Y; Z = p.Z + q.Z},
    /// where the infix notation is only used for convenience here.
    static member mapPairwise op p q = {X = op p.X q.X; Y = op p.Y q.Y; Z = op p.Z q.Z}
    
    /// Add a constant to each element of a point
    static member ( + ) (a: float<'Unit>, p) = Point<'Unit>.mapElementwise ((+) a) p

    /// Multiply each element of a point by a constant
    static member ( * ) (a: float, p) = Point<'Unit>.mapElementwise ((*) a) p

    /// Divide each element of the vector by a constant
    static member ( / ) (p, a: float) = Point<'Unit>.mapElementwise (fun x -> x / a) p

    /// Adds a point to a vector
    static member ( + ) (p, v: Vector) = { X = p.X + castFloatUnit<'Unit> v.I; Y = p.Y + castFloatUnit<'Unit> v.J; Z = p.Z + castFloatUnit<'Unit> v.K }

    /// Subtracts two points to get a vector that points from q to p
    static member ( - ) (p, q) = vector( removeUnits(p.X - q.X), removeUnits(p.Y - q.Y), removeUnits(p.Z - q.Z))

    /// Subtracts a vector from a point
    static member ( - ) (p, v: Vector) = { X = p.X - castFloatUnit<'Unit> v.I; Y = p.Y - castFloatUnit<'Unit> v.J; Z = p.Z - castFloatUnit<'Unit> v.K }

    /// Multiply two points, multiplying each element in one point by the corresponding
    /// positional element in the other point
    static member ( * ) (p, q) = Point<'Unit>.mapPairwise (*) p q

    /// Divide two points, dividing each element in one point by the corresponding
    /// positional element in the other point
    static member ( / ) (p, q) = Point<'Unit>.mapPairwise (/) p q

    /// Negate each element of a point
    static member ( ~- ) (p: Point<'Unit>) = -1.0 * p

    /// Overrides the Object.Equals method to provide a custom equality compare for Point records
    override this.Equals object =
        match object with
        // The #nowarn "1240" is ignore warning FS1240 for the snippet `:? Point<_>`
        | :? Point<_> as p -> compareFloat this.X p.X &&
                              compareFloat this.Y p.Y &&
                              compareFloat this.Z p.Z
        | _ -> false

    /// Overrides the Object.GetHashCode method, which is recommended when overriding Object.Equals
    override x.GetHashCode () = hash x // Re-use the built-in hash for records

    // Implements the ITuple interface to be treated like a tuple-like element
    interface ITuple<Point<'Unit>, 'Unit> with
        member this.X1 = this.X
        member this.X2 = this.Y
        member this.X3 = this.Z
        member this.ToTupleArray () = [| this.X; this.Y; this.Z; LanguagePrimitives.FloatWithMeasure 1.0 |]
        member _.FromTupleArray array = { X = array.[0]; Y = array.[1]; Z = array.[2] }

/// Convenience function for creating a Point record from floats with units of measure
let pointu<[<Measure>] 'Unit> (x, y, z) = Point<'Unit>.mapElementwise castFloatUnit<'Unit> { X = x; Y = y; Z = z }

/// Convenience function for creating a Point record from floats with no units of measure
let inline point ((x, y, z): float<'Unit> * float<'Unit> * float<'Unit>) = { X = x; Y = y; Z = z }

/// Converts a point from the old units to the new units
let convertPointUnits<[<Measure>] 'OldUnit, [<Measure>] 'NewUnit>
    (point: Point<'OldUnit>) : Point<'NewUnit> =
        Point<'OldUnit>.mapElementwise convertUnits<'OldUnit, 'NewUnit> point

/// Sum all elements of a vector to a single number
let sumVector v = v.I + v.J + v.K

/// Compute the dot product of two vectors
let dotProduct (u : Vector) (v : Vector) = sumVector (u * v)

/// Compute the dot product of two vectors
let dot = dotProduct

// Note: the dot function is for convienience to maintain capability with
// The Ray Tracer Challange book's naming.

/// Compute the cross product of two vectors
let crossProduct u v =
    { I = u.J*v.K - v.J*u.K
      J = v.I*u.K - u.I*v.K
      K = u.I*v.J - v.I*u.J }

/// Compute the cross product of two vectors
let cross = crossProduct

// Note: the cross function is for convienience to maintain capability with
// The Ray Tracer Challange book's naming.

/// Compute the norm, that is the square of the dot product, of a vector
let norm v = sqrt(dotProduct v v)

/// Compute the magnitude of a vector, which is equivalent to the norma of the vector
let magnitude = norm

/// Compute the square of the norm or magnitude of a vector
let normSquared v = dotProduct v v

/// Normalize a vector by dividing it by its norm or magnitude
let normalize v = v / (norm v)

/// Calculates the reflection of the vector across the normal vector
let reflect vector normal = vector - 2.0 * (dot vector normal) * normal
