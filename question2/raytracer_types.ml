(* Basic types and operations *)
type float3 = {x:float;y:float;z:float}

(* vector-vector arithmetic *)
let (+|) l r = {x=l.x+.r.x; y=l.y+.r.y; z=l.z+.r.z}
let (-|) l r = {x=l.x-.r.x; y=l.y-.r.y; z=l.z-.r.z}
let ( *|) l r = {x=l.x*.r.x; y=l.y*.r.y; z=l.z*.r.z}
let ( /|) l r = {x=l.x/.r.x; y=l.y/.r.y; z=l.z/.r.z}

(* vector-scalar arithmetic *)
let ( +|. ) l r = {x=l.x+.r; y=l.y+.r; z=l.z+.r}
let ( -|. ) l r = {x=l.x-.r; y=l.y-.r; z=l.z-.r}
let ( *|. ) l r = {x=l.x*.r; y=l.y*.r; z=l.z*.r}
let ( /|. ) l r = {x=l.x/.r; y=l.y/.r; z=l.z/.r}

let dot l r = l.x *. r.x  +.  l.y *. r.y  +.  l.z *. r.z
let normalize v = v /|. sqrt (dot v v)

let minimum = function
    [] -> invalid_arg "minimum"
  | x::xs -> List.fold_left min x xs

type plane_properties = {
  diffuse_c : float;
  specular_c : float;
  position : float3;
  normal : float3;
  reflection : float;
  colour : (float3 -> float3);
}
type sphere_properties = {
  position : float3;
  radius : float;
  colour : float3;
  reflection : float;
}
type obj =
    Plane : plane_properties -> obj
  | Sphere : sphere_properties -> obj

let diffuse_c = function
    Plane {diffuse_c} -> diffuse_c
  | Sphere _          -> 1.0

let specular_c = function
    Plane {specular_c} -> specular_c
  | Sphere _           -> 1.0

let reflection = function
    Plane {reflection}
  | Sphere {reflection} -> reflection

let obj_colour obj m = match obj with
    Sphere {colour} -> colour
  | Plane {colour} -> colour m

let obj_normal obj m =
  match obj with
    Sphere {position; radius} ->
    normalize (m -| position)
  | Plane {normal} ->
    normal

let specular_k = 50.0
let colour_plane0 = {x = 1.; y = 1.; z = 1.}
let colour_plane1 = {x = 0.; y = 0.; z = 0.}

let sphere ~position ~radius ~colour =
  Sphere {position; radius; colour; reflection=0.5}

let plane ~position ~normal =
  let colour m =
    if (int_of_float (m.x *. 2.0) mod 2)
       =
       (int_of_float (m.z *. 2.0) mod 2)
    then colour_plane0
    else colour_plane1 in
  Plane {diffuse_c=0.75; specular_c=0.5; position; normal; reflection=0.25; colour}
