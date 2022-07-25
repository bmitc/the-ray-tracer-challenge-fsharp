module RayTracer.Tests.Utilities

open Xunit
open FsUnit.Xunit
open RayTracer.Utilities

[<Fact>]
let ``Convert an integer without units to a float with units`` () =
    floatUnits<world> 3 |> should equal 3.0<world>
    
[<Fact>]
let ``Convert an integer with units to a float with the same units`` () =
    floatPreserveUnits 5<pixels> |> should equal 5.0<pixels>
