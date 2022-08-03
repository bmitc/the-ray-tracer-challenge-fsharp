module RayTracer.Tests.Ray

open Xunit
open FsUnit.Xunit
open RayTracer.Utilities
open RayTracer.Tuples
open RayTracer.Transformation
open RayTracer.Object
open RayTracer.Ray

[<Fact>]
let ``Creating and querying a ray`` () =
    let p = point(1.0, 2.0, 3.0)
    let d = vector(4.0, 5.0, 6.0)
    let r = ray p d
    (r.Origin, r.Direction) |> should equal (p, d)

[<Fact>]
let ``Computing a point from a distance`` () =
    let r = ray (pointu<world>(2.0, 3.0, 4.0)) (vector(1.0, 0.0, 0.0))
    [position r 0.0; position r 1.0; position r -1.0; position r 2.5]
    |> should equal [point(2.0,3.0,4.0); point(3.0,3.0,4.0); point(1.0,3.0,4.0); point(4.5,3.0,4.0)]

[<Fact>]
let ``A ray interesects a sphere at two points`` () =
    let r = ray (pointu<world>(0.0, 0.0, -5.0)) (vector(0.0, 0.0, 1.0))
    let s = sphere
    intersect r s |> should equal [{Object = s; Time = 4.0}; {Object = s; Time = 6.0}]

[<Fact>]
let ``A ray interesects a sphere at a tangent`` () =
    let r = ray (pointu<world>(0.0, 1.0, -5.0)) (vector(0.0, 0.0, 1.0))
    let s = sphere
    intersect r s |> should equal [{Object = s; Time = 5.0}]

[<Fact>]
let ``A ray misses a sphere`` () =
    let r = ray (pointu<world>(0.0, 2.0, -5.0)) (vector(0.0, 0.0, 1.0))
    let s = sphere
    intersect r s |> should equal List<Intersection>.Empty // could also use "should be Empty"

[<Fact>]
let ``A ray originates inside a sphere`` () =
    let r = ray (pointu<world>(0.0, 0.0, 0.0)) (vector(0.0, 0.0, 1.0))
    let s = sphere
    intersect r s |> should equal [{Object = s; Time = -1.0}; {Object = s; Time = 1.0}]

[<Fact>]
let ``A sphere is behind a ray`` () =
    let r = ray (pointu<world>(0.0, 0.0, 5.0)) (vector(0.0, 0.0, 1.0))
    let s = sphere
    intersect r s |> should equal [{Object = s; Time = -6.0}; {Object = s; Time = -4.0}]

[<Fact>]
let ``An intersection encapsulates t and object`` () =
    let s = sphere
    let i = {Object = s; Time = 3.5}
    (i.Object, i.Time) |> should equal (s, 3.5)

// "Aggregating intersections"
// This is not implemented since aggregations of intersections are treated as lists of intersections.

[<Fact>]
let ``Intersect sets the object on the intersection`` () =
    let r = ray (pointu<world>(0.0, 0.0, -5.0)) (vector(0.0, 0.0, 1.0))
    let s = sphere
    let xs = intersect r s
    [(xs.Item 0).Object; (xs.Item 1).Object] |> should equal [s; s]

[<Fact>]
let ``The hit, when all intersections have positive t`` () =
    let s = sphere
    let i1 = {Object = s; Time = 1.0}
    let i2 = {Object = s; Time = 2.0}
    hit [i2; i1] |> should equal (Some i1)

[<Fact>]
let ``The hit, when some intersections have negative t`` () =
    let s = sphere
    let i1 = {Object = s; Time = -1.0}
    let i2 = {Object = s; Time = 1.0}
    hit [i2; i1] |> should equal (Some i2)

[<Fact>]
let ``The hit, when all intersections have negative t`` () =
    let s = sphere
    let i1 = {Object = s; Time = -2.0}
    let i2 = {Object = s; Time = -1.0}
    hit [i2; i1] |> should equal Option<Intersection>.None

[<Fact>]
let ``The hit is always the lowest nonnegative intersection`` () =
    let s = sphere
    let i1 = {Object = s; Time = 5.0}
    let i2 = {Object = s; Time = 7.0}
    let i3 = {Object = s; Time = -3.0}
    let i4 = {Object = s; Time = 2.0}
    hit [i1; i2; i3; i4] |> should equal (Some i4)

[<Fact>]
let ``Translating a ray`` () =
    let r = ray (pointu<world>(1.0, 2.0, 3.0)) (vector(0.0, 1.0, 0.0))
    transform (Translation (3.0, 4.0, 5.0)) r
    |> should equal {Origin = point(4.0, 6.0, 8.0); Direction = vector(0.0, 1.0, 0.0)}

[<Fact>]
let ``Scaling a ray`` () =
    let r = ray (pointu<world>(1.0, 2.0, 3.0)) (vector(0.0, 1.0, 0.0))
    transform (Scaling (2.0, 3.0, 4.0)) r
    |> should equal {Origin = point(2.0, 6.0, 12.0); Direction = vector(0.0, 3.0, 0.0)}

[<Fact>]
let ``Intersecting a scaled sphere with a ray`` () =
    let r = ray (pointu<world>(0.0, 0.0, -5.0)) (vector(0.0, 0.0, 1.0))
    let s = {sphere with Transform = Some (Scaling(2.0, 2.0, 2.0))}
    let xs = intersect r s
    List.map (fun x -> x.Time) xs |> should equal [3.0; 7.0]

[<Fact>]
let ``Intersecting a translated sphere with a ray`` () =
    let r = ray (pointu<world>(0.0, 0.0, -5.0)) (vector(0.0, 0.0, 1.0))
    let s = {sphere with Transform = Some (Translation(5.0, 0.0, 0.0))}
    let xs = intersect r s
    xs |> should be Empty

// The following tests depart from the book. This is because the book describes an OOP approach,
// whereas here, shapes are described as a discriminated union and then an object is defined to
// contain a shape and optional material and optional transform. Due to this existing abstraction,
// the refactor described in the book doesn't need to take place. New shapes will simply need to
// add a pattern match case to the intersect function.

[<Fact>]
let ``Intersecting a scaled shape with a ray`` () =
    let r = ray (pointu<world>(0.0, 0.0, -5.0)) (vector(0.0, 0.0, 1.0))
    let s = { sphere with Transform = Some (Scaling (2.0, 2.0, 2.0)) }
    localRay r s |> should equal (ray (point(0.0, 0.0, -2.5)) (vector(0.0, 0.0, 0.5)))

[<Fact>]
let ``Intersecting a translated shape with a ray`` () =
    let r = ray (pointu<world>(0.0, 0.0, -5.0)) (vector(0.0, 0.0, 1.0))
    let s = { sphere with Transform = Some (Translation (5.0, 0.0, 0.0)) }
    localRay r s |> should equal (ray (point(-5.0, 0.0, -5.0)) (vector(0.0, 0.0, 1.0)))

[<Fact>]
let ``Intersect with a ray parallel to the plane`` () =
    let p = plane
    let r = ray (pointu<world>(0.0, 10.0, 0.0)) (vector(0.0, 0.0, 1.0))
    intersect r p |> should equal List<Intersection>.Empty

[<Fact>]
let ``Intersect with a coplanar ray`` () =
    let p = plane
    let r = ray (pointu<world>(0.0, 0.0, 0.0)) (vector(0.0, 0.0, 1.0))
    intersect r p |> should equal List<Intersection>.Empty

[<Fact>]
let ``A ray intersecting a plane from above`` () =
    let p = plane
    let r = ray (pointu<world>(0.0, 1.0, 0.0)) (vector(0.0, -1.0, 0.0))
    intersect r p |> should equal [{Object = p; Time = 1.0}]

[<Fact>]
let ``A ray intersecting a plane from below`` () =
    let p = plane
    let r = ray (pointu<world>(0.0, -1.0, 0.0)) (vector(0.0, 1.0, 0.0))
    intersect r p |> should equal [{Object = p; Time = 1.0}]

//*******************************
// Additional tests
//*******************************

// The book does not cover this case, which was found while developing.
[<Fact>]
let ``Intersecting a translated sphere with a zero ray`` () =
    let r = ray (pointu<world>(0.0, 0.0, 0.0)) (vector(0.0, 0.0, 0.0))
    let s = sphere
    let xs = intersect r s
    xs |> should be Empty

[<Fact>]
let ``A ray intersecting a rotated plane from above`` () =
    let p = { plane with Transform = Some (Rotation (Z, pi/4.0)) }
    let r = ray (pointu<world>(1.0, 0.0, 1.0)) (vector(-1.0, 0.0, 0.0))
    intersect r p |> should equal [{Object = p; Time = 1.0}]
