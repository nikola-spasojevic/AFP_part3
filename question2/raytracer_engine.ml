open Raytracer_types

(** Return the distance from o to the intersection of the ray (o, d) with the 
    plane (p, n), or +inf if there is no intersection. *)
let intersect_plane o d position normal =
  let denom = dot d normal in
  if abs_float denom < 1e-6
  then infinity
  else
    let d = dot (position -| o) normal /. denom in
    if d < 0.0 then
      infinity
    else d

(** Return the distance from o to the intersection of the ray (o, d) with the 
    sphere (position, radius), or +inf if there is no intersection. *)
let intersect_sphere o d position radius =
  let a = dot d d in
  let os = o -| position in
  let b = 2. *. dot d os in
  let c = dot os os -. radius *. radius in
  let disc = b *. b -. 4. *. a *. c in
  if disc > 0. then
    let distSqrt = sqrt disc in
    let q =
      if b < 0.
      then (-.b -. distSqrt) /. 2.0
      else (-.b +. distSqrt) /. 2.0
    in
    let t0 = q /. a in
    let t1 = c /. q in
    let t0 = min t0 t1
    and t1 = max t0 t1 in
    if t1 >= 0. then
      if t0 < 0. then t1 else t0
    else infinity
  else infinity

let intersect o d = function
    Plane {position; normal} -> intersect_plane o d position normal
  | Sphere {position; radius} -> intersect_sphere o d position radius

let trace_ray
    ~specular ~camera ~ambient ~light_colour ~light_position ~scene rayO rayD =
  (* Find the first point of intersection with the scene. *)
  let t = infinity in
  let rec find_first_intersection i (t, obj_index) = function
      [] -> (t, obj_index)
    | obj :: objs ->
      let t_obj = intersect rayO rayD obj in
      find_first_intersection (succ i)
        (if t_obj < t then (t_obj, i)
         else (t, obj_index))
        objs
  in
  let t, obj_index = find_first_intersection 0 (t, 0) scene in
  (* Return None if the ray does not intersect any object. *)
  if t = infinity then None
  else
     (* Find the object. *)
    let obj = List.nth scene obj_index in
     (* Find the point of intersection on the object. *)
    let m = rayO +| (rayD *|. t) in
     (* Find properties of the object. *)
    let n = obj_normal obj m in
    let colour = obj_colour obj m in
    let toL = normalize (light_position -| m) in
    let toO = normalize (camera -| m) in
    (* Shadow: find if the point is shadowed or not. *)
    let rec find_shadowed k l = function
        [] -> List.rev l
      | obj_sh :: obj_shs ->
        find_shadowed (succ k)
          (if k = obj_index then l else
             intersect  (m +| (n *|. 0.0001)) toL obj_sh :: l)
          obj_shs
    in
    let l = find_shadowed 0 [] scene in
    if List.length l <> 0 && minimum l < infinity
    then None
    else
      (* Start computing the colour. *)
      let col_ray = ambient in
      (* Lambert shading (diffuse). *)
      let col_ray = 
         (colour *|. (diffuse_c obj *. max 0.0 (dot n toL))) +|. col_ray in
      (* Blinn-Phong shading (specular). *)
      let col_ray = 
        (light_colour *|.
         (specular_c obj *. max 0.0 (dot n (normalize (toL +| toO))) ** specular))
        +| col_ray
      in
      Some (obj, m, n, col_ray)
