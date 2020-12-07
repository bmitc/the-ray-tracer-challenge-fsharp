module RayTracer.Tuples

open RayTracer.Utilities

/// Interface used to convert 3D tuple-like elements to a 4D tuple
type ITuple =
    abstract member X1 : float
    abstract member X2 : float
    abstract member X3 : float
    abstract member ToTupleArray: unit -> float[]

/// Represents a 3D vector
[<CustomEquality; CustomComparison>]
type Vector = {I : float; J : float; K : float} with

    static member mapPointwise op p = {I = op p.I; J = op p.J; K = op p.K}
    static member mapElementwise op p q = {I = op p.I q.I; J = op p.J q.J; K = op p.K q.K}
    
    static member (+) (a:float, q) = Vector.mapPointwise ((+) a) q
    static member (*) (a:float, q) = Vector.mapPointwise ((*) a) q
    static member (/) (q, a:float) = Vector.mapPointwise (fun x -> x / a) q

    static member (+) (p, q) = Vector.mapElementwise (+) p q
    static member (-) (p, q) = Vector.mapElementwise (-) p q
    static member (*) (p, q) = Vector.mapElementwise (*) p q

    static member (~-) (u : Vector) = -1.0 * u

    /// Overrides the Object.Equals method to provide a custom equality compare for Point records
    override x.Equals object =
        match object with
        | :? Vector as v -> compareFloat x.I v.I &&
                            compareFloat x.J v.J &&
                            compareFloat x.K v.K
        | _ -> false

    override x.GetHashCode() = hash x

    interface System.IComparable with
        member x.CompareTo y =
            match y with
            | :? Vector as v -> [x.I.CompareTo v.I; x.J.CompareTo v.J; x.K.CompareTo v.K]
                                |> List.tryFind (fun result -> result <> 0)
                                |> function
                                    | Some x -> x
                                    | None   -> 0
            | _ -> 0

    interface ITuple with
        member this.ToTupleArray () = [| this.I; this.J; this.K; 0.0 |]
        member this.X1 = this.I
        member this.X2 = this.J
        member this.X3 = this.K

let vector (i,j,k) = {I=i; J=j; K=k}

/// Represents a 3D point
[<CustomEquality; CustomComparison>]
type Point = {X : float; Y : float; Z : float} with

    static member mapPointwise op p = {X = op p.X; Y = op p.Y; Z = op p.Z}
    static member mapElementwise op p q = {X = op p.X q.X; Y = op p.Y q.Y; Z = op p.Z q.Z}
    
    static member (+) (a:float, q) = Point.mapPointwise ((+) a) q
    static member (*) (a:float, q) = Point.mapPointwise ((*) a) q
    static member (/) (q, a:float) = Point.mapPointwise (fun x -> x / a) q

    static member (+) (p, q : Vector) = {X = p.X + q.I; Y = p.Y + q.J; Z = p.Z + q.K}
    static member (-) (p, q) = vector( p.X - q.X, p.Y - q.Y, p.Z - q.Z)
    static member (-) (p, q : Vector) = {X = p.X - q.I; Y = p.Y - q.J; Z = p.Z - q.K}
    static member (*) (p, q) = Point.mapElementwise (*) p q
    static member (/) (p, q) = Point.mapElementwise (/) p q

    /// Overrides the Object.Equals method to provide a custom equality compare for Point records
    override x.Equals object =
        match object with
        | :? Point as p -> compareFloat x.X p.X &&
                           compareFloat x.Y p.Y &&
                           compareFloat x.Z p.Z
        | _ -> false

    override x.GetHashCode() = hash x

    interface System.IComparable with
        member x.CompareTo y =
            match y with
            | :? Point as p -> [x.X.CompareTo p.X; x.Y.CompareTo p.Y; x.Z.CompareTo p.Z]
                               |> List.tryFind (fun result -> result <> 0)
                               |> function
                                   | Some x -> x
                                   | None   -> 0
            | _ -> 0

    interface ITuple with
        member this.ToTupleArray () = [| this.X; this.Y; this.Z; 1.0 |]
        member this.X1 = this.X
        member this.X2 = this.Y
        member this.X3 = this.Z

let point (x,y,z) = {X=x; Y=y; Z=z}

type Tuple = Vector of Vector | Point of Point

let sumVector v = v.I + v.J + v.K

let dotProduct (u : Vector) (v : Vector) = sumVector (u * v)

let dot = dotProduct

let crossProduct u v =
    {I = u.J*v.K - v.J*u.K
     J = v.I*u.K - u.I*v.K
     K = u.I*v.J - v.I*u.J}

let cross = crossProduct

let norm v = sqrt(dotProduct v v)

let magnitude = norm

let normSquared v = dotProduct v v

let normalize v = v / (norm v)