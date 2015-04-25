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
    Sta x, Sta y -> Sta (x +. y)
  | Sta 0., Dyn v 
  | Dyn v, Sta 0. -> Dyn v
  | Sta r, Dyn v
  | Dyn v, Sta r -> Dyn .<.~v +. r>.
  | Dyn l, Dyn r -> Dyn .<.~l +. .~r>.

let ( -~) (l : float sd) (r : float sd) : float sd = match l, r with
    Sta x, Sta y -> Sta (x -. y)  
  | Dyn v, Sta 0. -> Dyn v
  | Sta r, Dyn v -> Dyn .<r -. .~v>.
  | Dyn v, Sta r -> Dyn .<.~v -. r>.
  | Dyn l, Dyn r -> Dyn .<.~l -. .~r>.

let ( *~) (l : float sd) (r : float sd) : float sd = match l, r with
    Sta x, Sta y -> Sta (x *. y)
  | Sta 0., Dyn v 
  | Dyn v, Sta 0. -> Dyn .<0.>.
  | Sta r, Dyn v
  | Dyn v, Sta r -> Dyn .<.~v *. r>.
  | Dyn l, Dyn r -> Dyn .<.~l *. .~r>.

let ( /~) (l : float sd) (r : float sd) : float sd = match l, r with
    Sta x, Sta y -> Sta (x /. y)
  | Sta 0., Dyn v -> Dyn .<0.>.
  | Dyn v, Sta 0. -> assert false
  | Sta l, Dyn v -> Dyn .<l /. .~v>.
  | Dyn v, Sta r -> Dyn .<.~v /. r>.
  | Dyn l, Dyn r -> Dyn .<.~l /. .~r>.

(* Question 2(a) *)
let ( +|@) (x : float3 sd) (y : float3 sd) : float3 sd = let nil = {x = 0.; y = 0.;z = 0.} in match x, y with 
    Sta l, Sta r -> Sta (l +| r)
  | Sta nil, Dyn v  
  | Dyn v, Sta nil -> Dyn v
  | Sta r, Dyn v
  | Dyn v, Sta r ->  Dyn  .< let a = .~v in { Raytracer_types.x = .~(unsd (Sta r.x +~ Dyn .<a.x>.)); 
                                              y = .~(unsd (Sta r.y +~ Dyn .<a.y>.)); 
                                              z = .~(unsd (Sta r.z +~ Dyn .<a.z>.)); 
                                            }
                          >.
  | Dyn l, Dyn r -> Dyn .<.~l +| .~r>.

let ( -|@) (x : float3 sd) (y : float3 sd) : float3 sd = let nil = (0., 0., 0.) in match x, y with 
    Sta l, Sta r -> Sta (l -| r)
  | Sta nil, Dyn v  
  | Dyn v, Sta nil -> Dyn v
  | Sta l, Dyn v -> Dyn .<l -| .~v>. 
  | Dyn v, Sta r -> Dyn .<.~v -| r>.
  | Dyn l, Dyn r -> Dyn .< .~l -| .~r >.

let ( *|@) (x : float3 sd) (y : float3 sd) : float3 sd = let nil = (0., 0., 0.) and one = (1.0, 1.0, 1.0) in match x, y with 
    Sta l, Sta r -> Sta (l *| r)
  | Sta nil, Dyn v
  | Dyn v, Sta nil -> Dyn .<nil>.
  | Sta one, Dyn v
  | Dyn v, Sta one -> Dyn v
  | Sta l, Dyn v
  | Dyn v, Sta l -> Dyn .< l *| .~v >.
  | Dyn l, Dyn r -> Dyn .< .~l *| .~r >.

let ( /|@) (x : float3 sd) (y : float3 sd) : float3 sd = let nil = (0., 0., 0.) and one = (1.0, 1.0, 1.0) in match x, y with 
    Sta l, Sta r -> Sta (l /| r)
  | Sta nil, Dyn v -> Dyn .<nil>.
  | Dyn v, Sta nil -> assert false
  | Dyn v, Sta one -> Dyn v
  | Sta l, Dyn v -> Dyn .< l /| .~v >.
  | Dyn v, Sta r -> Dyn .< .~v /| r >.
  | Dyn l, Dyn r -> Dyn .< .~l /| .~r >.

(* Question 2(b) *)
let dot_ (l : float3 sd) (r : float3 sd) : float sd = let nil = (0., 0., 0.) in match l, r with
    Sta l, Sta r -> Sta (dot l r)
  | Sta nil, Dyn v
  | Dyn v, Sta nil -> Dyn .<0.>.
  | Sta l, Dyn v
  | Dyn v, Sta l -> Dyn .< dot l .~v >.
  | Dyn l, Dyn r -> Dyn .< dot .~l .~r >.
  
(* Question 2(c) *)(*
let intersect_ (o : float3 code) (d : float3 code) : obj -> float code = function
    
    Plane {position; normal} ->  .< let p = dot_ .~d normal and nil = (0., 0., 0.) in
                                    match p with 
                                    | nil -> assert false (*parallel*)
                                    | _ -> let x = dot_ (.~o -|@ position) normal in
                                      x /|@ p
                                  >.
  | Sphere {position; radius} -> .< let v = position -|@ o in
                                    let b = dot_ v d in
                                    let disc = sqr (b -.)


                                  >.



let trace_ray_
    ~specular ~camera ~ambient ~light_colour ~light_position ~scene rayO rayD =
  assert false (* Question 2(d) *)

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
*)
