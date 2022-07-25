// Implementation of the putting it together at the end of Chapter 7

#load "load.fsx"

open RayTracer.Utilities
open RayTracer.Tuples
open RayTracer.Color
open RayTracer.PPM
open RayTracer.Transformation
open RayTracer.Ray
open RayTracer.LightAndShading
open RayTracer.Scene

//******************************************
// Scene objects
//******************************************

let floor = {sphere with Transform = Some (Scaling(10.0, 0.01, 10.0));
                         Material = Some {material() with Color = color(1.0, 0.9, 0.9);
                                                          Specular = 0.0}}

let leftWall = {sphere with Transform = Some (Combination [Translation(0.0,0.0,5.0);
                                                           Rotation(Y,-pi/4.0);
                                                           Rotation(X,pi/2.0);
                                                           Scaling(10.0, 0.01, 10.0)]);
                            Material = floor.Material}

let rightWall = {sphere with Transform = Some (Combination [Translation(0.0,0.0,5.0);
                                                            Rotation(Y,pi/4.0);
                                                            Rotation(X,pi/2.0);
                                                            Scaling(10.0, 0.01, 10.0)]);
                             Material = floor.Material}

let middle = {sphere with Transform = Some (Translation(-0.5, 1.0, 0.5));
                          Material = Some {material() with Color = color(0.1, 1.0, 0.5);
                                                           Diffuse = 0.7;
                                                           Specular = 0.3}}

let right = {sphere with Transform = Some (Combination [Translation(1.5, 0.5, -0.5); Scaling(0.5, 0.5, 0.5)]);
                         Material = Some {material() with Color = color(0.5, 1.0, 0.1);
                                                          Diffuse = 0.7;
                                                          Specular = 0.3}}

let left = {sphere with Transform = Some (Combination [Translation(-1.5, 0.33, -0.75); Scaling(0.33, 0.33, 0.33)]);
                        Material = Some {material() with Color = color(0.0, 0.5, 0.8);
                                                         Diffuse = 0.7;
                                                         Specular = 0.3}}

//******************************************
// World
//******************************************

let light = {Position = pointu<world>(-10.0, 10.0, -10.0); Intensity = color(1.0, 1.0, 1.0)}
let world = {Objects = [floor; leftWall; rightWall; middle; left; right]; LightSource = light}

let camera = {camera(2000<pixels>, 1000<pixels>, pi/3.0)
              with Transform = viewTransform (point(0.0, 1.5, -5.0)) (point(0.0, 1.0, 0.0)) (vector(0.0, 1.0, 0.0)) }

#time
let image = render camera world
#time

writeToPPM image (System.IO.Path.Combine(__SOURCE_DIRECTORY__, "../../../images/scene.ppm"))
