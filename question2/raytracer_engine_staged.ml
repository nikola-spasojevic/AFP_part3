open Raytracer_types

type _ sd =
    Sta : 'a -> 'a sd
  | Dyn : 'a code -> 'a sd

let unsd : 'a. 'a sd -> 'a code = function
    Sta v -> .< v >.
  | Dyn v -> v

let sqr (x : float sd) : float sd = match x with
    Sta t  -> Sta (t *. t)
  | Dyn t -> Dyn .<let y = .~t in y *. y>.

let ( +~) (l : float sd) (r : float sd) : float sd = match l, r with
    Sta 0., x 
  | x, Sta 0. -> x
  | Sta x, Sta y -> Sta (x +. y)
  | Sta r, Dyn v
  | Dyn v, Sta r -> Dyn .<.~v +. r>.
  | Dyn l, Dyn r -> Dyn .<.~l +. .~r>.

let ( -~) (l : float sd) (r : float sd) : float sd = match l, r with
    x, Sta 0. -> x
  | Sta 0., Sta y -> Sta (-.y)    
  | Sta x, Sta y -> Sta (x -. y)  
  | Sta r, Dyn v -> Dyn .<r -. .~v>.
  | Dyn v, Sta r -> Dyn .<.~v -. r>.
  | Dyn l, Dyn r -> Dyn .<.~l -. .~r>.

let ( *~) (l : float sd) (r : float sd) : float sd = match l, r with
    Sta 0., _ 
  | _, Sta 0. -> Sta 0.
  | Sta 1., x 
  | x, Sta 1. -> x
  | Sta x, Sta y -> Sta (x *. y)
  | Sta r, Dyn v
  | Dyn v, Sta r -> Dyn .<.~v *. r>.
  | Dyn l, Dyn r -> Dyn .<.~l *. .~r>.

let ( /~) (l : float sd) (r : float sd) : float sd = match l, r with
     Sta 0., _ -> Sta 0.
  |  _, Sta 0. -> assert false
  |  x, Sta 1. -> x
  | Sta l, Sta v -> Sta (l /. v)
  | Sta l, Dyn v -> Dyn .<l /. .~v>.
  | Dyn v, Sta r -> Dyn .<.~v /. r>.
  | Dyn l, Dyn r -> Dyn .<.~l /. .~r>.

(* Question 2(a) *)
let ( +|@) (x : float3 sd) (y : float3 sd) : float3 sd = let nil = {x = 0.; y = 0.;z = 0.} in match x, y with 
    Sta nil, x 
  | x, Sta nil -> x
  | Sta l, Sta r -> Sta (l +| r)
  | Sta r, Dyn v
  | Dyn v, Sta r ->  Dyn  .< let a = .~v in { Raytracer_types.x = .~(unsd (Sta r.x +~ Dyn .<a.x>.)); 
                                              Raytracer_types.y = .~(unsd (Sta r.y +~ Dyn .<a.y>.)); 
                                              Raytracer_types.z = .~(unsd (Sta r.z +~ Dyn .<a.z>.)); 
                                            }
                          >.
  | Dyn l, Dyn r -> Dyn .<.~l +| .~r>.

let ( -|@) (x : float3 sd) (y : float3 sd) : float3 sd = let nil = {x = 0.; y = 0.;z = 0.} in match x, y with 
    Sta l, Sta r -> Sta (l -| r)  
  | Dyn v, Sta nil -> Dyn v
  | Sta l, Dyn v -> Dyn  .< let a = .~v in {  Raytracer_types.x = .~(unsd (Sta l.x -~ Dyn .<a.x>.)); 
                                              Raytracer_types.y = .~(unsd (Sta l.y -~ Dyn .<a.y>.)); 
                                              Raytracer_types.z = .~(unsd (Sta l.z -~ Dyn .<a.z>.)); 
                                            } >.


  | Dyn v, Sta r -> Dyn  .< let a = .~v in {  Raytracer_types.x = .~(unsd (Dyn .<a.x>. -~ Sta r.x));
                                              Raytracer_types.y = .~(unsd (Dyn .<a.y>. -~ Sta r.y));
                                              Raytracer_types.z = .~(unsd (Dyn .<a.z>. -~ Sta r.z));
                                            } >.
  | Dyn l, Dyn r -> Dyn .< .~l -| .~r >.

let ( *|@) (x : float3 sd) (y : float3 sd) : float3 sd = let nil = {x = 0.; y = 0.;z = 0.} and one = {x = 1.; y = 1.;z = 1.} in match x, y with 
    Sta l, Sta r -> Sta (l *| r)
  | Sta nil, Dyn v
  | Dyn v, Sta nil -> Dyn .<nil>.
  | Sta one, Dyn v
  | Dyn v, Sta one -> Dyn v
  | Sta l, Dyn v
  | Dyn v, Sta l -> Dyn .< let a = .~v in {   Raytracer_types.x = .~(unsd (Dyn .<a.x>. *~ Sta l.x));
                                              Raytracer_types.y = .~(unsd (Dyn .<a.y>. *~ Sta l.y));
                                              Raytracer_types.z = .~(unsd (Dyn .<a.z>. *~ Sta l.z));
                                            } >.
  | Dyn l, Dyn r -> Dyn .< .~l *| .~r >.

let ( /|@) (x : float3 sd) (y : float3 sd) : float3 sd = let nil = {x = 0.; y = 0.;z = 0.} and one = {x = 1.; y = 1.;z = 1.} in match x, y with 
    Sta l, Sta r -> Sta (l /| r)
  | Sta nil, Dyn v -> Dyn .<nil>.
  | Dyn v, Sta nil -> assert false
  | Dyn v, Sta one -> Dyn v
  | Sta l, Dyn v -> Dyn .< let a = .~v in {   Raytracer_types.x = .~(unsd (Sta l.x /~ Dyn .<a.x>.));
                                              Raytracer_types.y = .~(unsd (Sta l.y /~ Dyn .<a.y>.));
                                              Raytracer_types.z = .~(unsd (Sta l.z /~ Dyn .<a.z>.));
                                            } >.
  | Dyn v, Sta r -> Dyn .< let a = .~v in {   Raytracer_types.x = .~(unsd (Dyn .<a.x>. /~ Sta r.x));
                                              Raytracer_types.y = .~(unsd (Dyn .<a.y>. /~ Sta r.y));
                                              Raytracer_types.z = .~(unsd (Dyn .<a.z>. /~ Sta r.z));
                                            } >.
  | Dyn l, Dyn r -> Dyn .< .~l /| .~r >.

(* Question 2(b) *)
let dot_ (l : float3 sd) (r : float3 sd) : float sd = let nil = {x = 0.; y = 0.;z = 0.} in match l, r with
    Sta nil, _
  | _, Sta nil -> Sta 0.  
  | Sta l, Sta r -> Sta (dot l r)
  | Sta l, Dyn v
  | Dyn v, Sta l -> Dyn .< dot l .~v >.
  | Dyn l, Dyn r -> Dyn .< dot .~l .~r >.
  
(* Question 2(c) *)
let intersect_ (o : float3 code) (d : float3 code) : obj -> float code = function
    
    Plane {position; normal} -> begin 
                                  let q = dot_ (Sta position -|@ Dyn o) (Sta normal) in
                                  match dot_ (Dyn d) (Sta normal) with
                                  | Sta denom -> begin
                                    if abs_float denom < 1e-6
                                    then .<infinity>.
                                    else match q /~ (Sta denom) with
                                      | Dyn x -> .<if .~x < 0.0 then infinity else .~x>.
                                      | Sta x -> .<x>.
                                    end
                                  | Dyn denom -> begin
                                    let x = unsd (q /~ (Dyn denom))in
                                    .<
                                      if abs_float .~denom < 1e-6
                                      then infinity
                                      else
                                        if .~x < 0.0
                                        then infinity
                                        else .~x
                                    >.
                                  end
                                end

  | Sphere {position; radius} ->  begin 
                                    let a = dot_ (Dyn d) (Dyn d) in
                                    let os = Dyn o -|@ Sta position in
                                    let b = Sta 2. *~ dot_ (Dyn d) os in
                                    let c = dot_ os os -~ Sta (radius *. radius) in
                                    let disc = unsd((b *~ b) -~ ((Sta 4.) *~ a *~ c)) in
                                    let a' = unsd a and b' = unsd b and c' = unsd c in
                                    .<
                                    if .~disc > 0. then
                                        let distSqrt = sqrt .~disc in
                                        let q =
                                          if .~b' < 0.
                                          then (-.(.~b') -. distSqrt) /. 2.0
                                          else (-.(.~b') +. distSqrt) /. 2.0
                                        in
                                        let t0 = q /. .~a' in
                                        let t1 = .~c' /. q in
                                        let t0 = min t0 t1
                                        and t1 = max t0 t1 in
                                        if t1 >= 0. then
                                          if t0 < 0. then t1 else t0
                                        else infinity
                                      else infinity
                                    >.
                                  end
let vec x = {x = x; y = x; z = x}
let normalise_ (x : float3 sd) = 
  let mag = dot_ x x in
  match mag with
    Sta y -> x /|@ Sta (vec (sqrt y))
  | Dyn y -> x /|@ Dyn .<vec (sqrt .~y)>.
                                  
(* Question 2(d) *)
let trace_ray_
    ~specular ~camera ~ambient ~light_colour ~light_position ~scene rayO rayD =
 (* Find the first point of intersection with the scene. *)
  let t = infinity in
  let find_first_intersection i col objs = 
    let rec find_first_intersection' i = function
      [] -> .<col>.
    | obj :: objs ->
    .<
        let t_obj = .~(intersect_ rayO rayD obj) in
        let x, x_i = .~(find_first_intersection' (succ i) objs) in
        if t_obj < x then (t_obj, i)
         else (x, x_i)
    >.
     in 
     find_first_intersection' i objs
  in

  (* Shadow: find if the point is shadowed or not. *)
  let rec find_shadowed k o_idx b toL = 
    let rec find_shadowed' k = function
        [] -> .<[]>.
      | obj_sh :: obj_shs ->
        .<
          let next = .~(find_shadowed' (succ k) obj_shs) in
              if k = .~o_idx then next
              else 
                let col = .~(intersect_ b toL obj_sh) in
                col :: next
        >.
    in find_shadowed' k
  in

  .<
      let t, o_idx = .~(find_first_intersection 0 (t,0) scene) in
      (* Return none if the ray does not intersect *)
      if t = infinity then None
      else
        (* Find the object. *) 
        let obj = List.nth scene o_idx in
        (* Find the point of intersection on the object. *)
        let m = .~(unsd (Dyn rayO +|@ (Dyn rayD *|@ Dyn .<vec t>.))) in
        (* Find properties of the object. *)
        let n = obj_normal obj m in
        let colour = obj_colour obj m in
        let toL = normalize .~(unsd(Sta light_position -|@ Dyn .<m>.)) in
        let toO = normalize .~(unsd(Sta camera -|@ Dyn .<m>.)) in
        let b = .~(unsd(Dyn .<m>. +|@ (Dyn .<n>. *|@ Sta (vec 0.0001)))) in
        let l = .~(find_shadowed 0 .<o_idx>. .<b>. .<toL>. scene) in
        if List.length l <> 0 && minimum l < infinity
        then None
        else
          let col_ray = vec ambient in
          let lambert =
            .~(unsd((Dyn .<colour>. *|@
                     Dyn .<vec ((diffuse_c obj) *.
                           max 0. .~(unsd(dot_ (Dyn .<n>.) (Dyn .<toL>.))))
                         >.
                     )
                   )
                  )
          in
          let specular = (
            max 0. .~(unsd(dot_ (Dyn .<n>.) 
                          (normalise_ (Dyn .<toL>. +|@ Dyn .<toO>.))))
            ) ** specular
          in
          let phong = .~(unsd (Sta light_colour *|@ Dyn .<vec specular>.)) in
          Some(obj, m, n, .~(unsd(Dyn .<col_ray>. +|@ Dyn .<lambert>. +|@ Dyn .<phong>.)))
    >.

let trace_ray ~specular ~camera ~ambient ~light_colour ~light_position ~scene =
  let code =
    .< fun ro rd ->
      .~(trace_ray_ ~specular ~camera ~ambient ~light_colour ~light_position ~scene
           .< ro >. .< rd >.) >.
  in Runcode.run code

(* Question 2(e)(i) is a written question, not a programming question. *)
(* Question 2(e)(ii) is a written question, not a programming question. 


#use "raytracer_types.ml"
#use "raytracer_engine_staged.ml"
*)
