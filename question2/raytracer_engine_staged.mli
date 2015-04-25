open Raytracer_types

val trace_ray :
  specular:float ->
  camera:float3 ->
  ambient:float ->
  light_colour:float3 ->
  light_position:float3 ->
  scene:obj list ->
  float3 -> float3 -> (obj * float3 * float3 * float3) option
