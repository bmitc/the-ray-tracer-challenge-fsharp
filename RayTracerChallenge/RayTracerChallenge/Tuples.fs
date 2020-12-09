module RayTracer.Tuples

open Utilities

// Tuples are intentionally separated out into separate types of Vector and Point.
// This is because they are distinctilly two different types with enough variation
// that they don't belong as a single type. The ITuple interface provides shared
// behavior, that primarily allows Vector and Point to be transformed with matrices.
// There is a small hit in duplicated code, but this duplication is small and contained
// whereas the correct type design pays off in the overall design of the ray tracer.

/// Interface used to convert 3D tuple-like elements to and from a 4D tuple
type ITuple<'T> =

    /// The first element of the tupe-like element
    abstract member X1 : float

    /// The second element of the tupe-like element
    abstract member X2 : float

    /// The third element of the tupe-like element
    abstract member X3 : float

    /// Converts a 3D tuple-like element to an array of length four consisting of
    /// the three components plus a fourth component. The array is essentially the
    /// homogeneous coordinate representation of the tuple.
    abstract member ToTupleArray : unit -> float[]

    /// Creates a 3D tuple-like element from a tuple array
    abstract member FromTupleArray : float[] -> 'T

/// Represents a 3D vector
[<CustomEquality; CustomComparison>]
type Vector = { I: float; J: float; K: float } with

    /// Maps the operation to each element of the vector
    static member mapElementwise op u = {I = op u.I; J = op u.J; K = op u.K}

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

    interface System.IComparable with
        
        /// Implements a custom comparison method, using the Utilities.compareFloat function
        /// for the Vector floats
        member x.CompareTo y =
            match y with
            | :? Vector as v -> [x.I.CompareTo v.I; x.J.CompareTo v.J; x.K.CompareTo v.K]
                                |> List.tryFind (fun result -> result <> 0)
                                |> function
                                    | Some x -> x
                                    | None   -> 0
            | _ -> 0

    /// Implements the ITuple interface to be treated like a tuple-like element
    interface ITuple<Vector> with
        member this.X1 = this.I
        member this.X2 = this.J
        member this.X3 = this.K
        member this.ToTupleArray () = [| this.I; this.J; this.K; 0.0 |]
        member _.FromTupleArray array = { I = array.[0]; J = array.[1]; K = array.[2] }

/// Convenience function for creating a Vector record
let vector (i,j,k) = { I = i; J = j; K = k }

/// Represents a 3D point
[<CustomEquality; CustomComparison>]
type Point = { X: float; Y: float; Z: float } with

    /// Maps the operation to each element of the vector
    static member mapElementwise op p = {X = op p.X; Y = op p.Y; Z = op p.Z}

    /// Maps the operation pairwise across two points.
    /// For example, mapPaiwise (+) p q = {X = p.X + q.X; Y = p.Y + q.Y; Z = p.Z + q.Z},
    /// where the infix notation is only used for convenience here.
    static member mapPairwise op p q = {X = op p.X q.X; Y = op p.Y q.Y; Z = op p.Z q.Z}
    
    /// Add a constant to each element of a point
    static member ( + ) (a: float, p) = Point.mapElementwise ((+) a) p

    /// Multiply each element of a point by a constant
    static member ( * ) (a: float, p) = Point.mapElementwise ((*) a) p

    /// Divide each element of the vector by a constant
    static member ( / ) (p, a: float) = Point.mapElementwise (fun x -> x / a) p

    /// Adds a point to a vector
    static member ( + ) (p, v: Vector) = { X = p.X + v.I; Y = p.Y + v.J; Z = p.Z + v.K }

    /// Subtracts two points to get a vector that points from q to p
    static member ( - ) (p, q) = vector( p.X - q.X, p.Y - q.Y, p.Z - q.Z)

    /// Subtracts a vector from a point
    static member ( - ) (p, v: Vector) = { X = p.X - v.I; Y = p.Y - v.J; Z = p.Z - v.K }

    /// Multiply two points, multiplying each element in one point by the corresponding
    /// positional element in the other point
    static member ( * ) (p, q) = Point.mapPairwise (*) p q

    /// Divide two points, dividing each element in one point by the corresponding
    /// positional element in the other point
    static member ( / ) (p, q) = Point.mapPairwise (/) p q

    /// Negate each element of a point
    static member ( ~- ) (p: Point) = -1.0 * p

    /// Overrides the Object.Equals method to provide a custom equality compare for Point records
    override x.Equals object =
        match object with
        | :? Point as p -> compareFloat x.X p.X &&
                           compareFloat x.Y p.Y &&
                           compareFloat x.Z p.Z
        | _ -> false

    /// Overrides the Object.GetHashCode method, which is recommended when overriding Object.Equals
    override x.GetHashCode () = hash x // Re-use the built-in hash for records

    /// Implements a custom comparison method, using the Utilities.compareFloat function
    /// for the Point floats
    interface System.IComparable with
        member x.CompareTo y =
            match y with
            | :? Point as p -> [x.X.CompareTo p.X; x.Y.CompareTo p.Y; x.Z.CompareTo p.Z]
                               |> List.tryFind (fun result -> result <> 0)
                               |> function
                                   | Some x -> x
                                   | None   -> 0
            | _ -> 0

    /// Implements the ITuple interface to be treated like a tuple-like element
    interface ITuple<Point> with
        member this.X1 = this.X
        member this.X2 = this.Y
        member this.X3 = this.Z
        member this.ToTupleArray () = [| this.X; this.Y; this.Z; 1.0 |]
        member _.FromTupleArray array = { X = array.[0]; Y = array.[1]; Z = array.[2] }

/// Convenience function for creating a Point record
let point (x, y, z) = { X = x; Y = y; Z = z }

/// Sum all elements of a vector to a single number
let sumVector v = v.I + v.J + v.K

/// Compute the dot product of two vectors
let dotProduct (u : Vector) (v : Vector) = sumVector (u * v)

/// Compute the dot product of two vectors
let dot = dotProduct

// Note: the dot function is there for convienience to maintain capability with
// The Ray Tracer Challange book's naming.

/// Compute the cross product of two vectors
let crossProduct u v =
    { I = u.J*v.K - v.J*u.K
      J = v.I*u.K - u.I*v.K
      K = u.I*v.J - v.I*u.J }

/// Compute the cross product of two vectors
let cross = crossProduct

// Note: the cross function is there for convienience to maintain capability with
// The Ray Tracer Challange book's naming.

/// Compute the norm, that is the square of the dot product, of a vector
let norm v = sqrt(dotProduct v v)

/// Compute the magnitude of a vector, which is equivalent to the norma of the vector
let magnitude = norm

/// Compute the square of the norm or magnitude of a vector
let normSquared v = dotProduct v v

/// Normalize a vector by dividing it by its norm or magnitude
let normalize v = v / (norm v)