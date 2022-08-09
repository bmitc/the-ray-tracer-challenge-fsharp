// Implementation of the putting it together at the end of Chapter 10

#load "load.fsx"

open RayTracer.Utilities
open RayTracer.Tuples
open RayTracer.Color
open RayTracer.PPM
open RayTracer.Transformation
open RayTracer.Pattern
open RayTracer.Object
open RayTracer.LightAndShading
open RayTracer.Scene

//******************************************
// Scene objects
//******************************************

let ground = {plane  with Transform = Some (Rotation (Z, 0.0<radians>));
                          Material  = Some {Material.Default with Specular  = Material.Default.Specular
                                                                  Pattern   = Some (checker skyBlue gray (Some (ScalingEqual 0.9)))}}

let wall   = {plane  with Transform = Some (Combination [Translation (0.0, 0.0, 5.0); Rotation (Y, -pi/4.0); Rotation (Z, pi/2.0)])
                          Material  = Some {Material.Default with Specular  = Material.Default.Specular
                                                                  Pattern   = Some (Perturb (Blend (stripe white hotPink (Some (Combination [ScalingEqual 0.7;
                                                                                                                                             Rotation (Y, pi/4.0)])),
                                                                                                    stripe white hotPink (Some (Combination [ScalingEqual 0.7;
                                                                                                                                             Rotation (Y, 3.0*pi/4.0)])))))}}

let middle = {sphere with Transform = Some (Translation(-0.5, 1.0, 0.5));
                          Material  = Some {Material.Default with Diffuse   = 0.7
                                                                  Specular  = 0.3
                                                                  Shininess = 30
                                                                  Pattern   = Some (Perturb (ring paleGreen purple (Some (Combination [Rotation (Z, pi/4.0);
                                                                                                                                       Rotation (X, pi/4.0);
                                                                                                                                       ScalingEqual 0.09]))))}}

let right  = {sphere with Transform = Some (Combination [Translation(1.5, 0.5, -0.5); ScalingEqual 0.5])
                          Material  = Some {Material.Default with Diffuse   = 0.7
                                                                  Specular  = 0.3
                                                                  Shininess = 40
                                                                  Pattern   = Some (gradient deepPink blue (Some (Combination [Rotation (Y, pi/6.0);
                                                                                                                               Scaling (2, 0, 0);
                                                                                                                               Translation (1.5, 0, 0)])))}}

let left   = {sphere with Transform = Some (Combination [Translation(-1.5, 0.33, -0.75); ScalingEqual 0.33])
                          Material  = Some {Material.Default with Diffuse   = 1.0
                                                                  Specular  = 0.3
                                                                  Shininess = 10
                                                                  Pattern   = Some (checker powderBlue yellow (Some (ScalingEqual 0.4)))}}

//******************************************
// World
//******************************************

let light = {Position = pointu<world>(-10.0, 10.0, -10.0); Intensity = color(1.0, 1.0, 1.0)}
let world = {Objects = [ground; wall; middle; left; right]; LightSource = light}

let camera = { camera(2000<pixels>, 1000<pixels>, pi/3.0)
               with Transform = viewTransform (point(0.0, 1.5, -5.0)) (point(0.0, 1.0, 0.0)) (vector(0.0, 1.0, 0.0)) }

#time
let image = render camera world
#time

writeToPPM image (System.IO.Path.Combine(__SOURCE_DIRECTORY__, "../../../images/patterns.ppm"))
