
open Raytracer_types
open Raytracer_engine
open Raytracer_engine_staged

let scene = [
  sphere
    ~position:{x=0.75; y=0.1; z=1.}
    ~radius:0.6
    ~colour:{x=0.; y=0.; z=1.};
  sphere
    ~position:{x= -0.75; y=0.1; z=2.25}
    ~radius:0.6
    ~colour:{x=0.5; y=0.223; z=0.5};
  sphere
    ~position:{x= -2.75; y=0.1; z=3.5}
    ~radius:0.6
    ~colour:{x=1.0; y=0.572; z=0.184};
  plane
    ~position:{x=0.0; y= -0.5; z=0.}
    ~normal:{x=0.0; y=1.0; z=0.0};
]

let light_position = {x = 5.; y = 5.; z = -10. }
let light_colour = {x=1.0; y=1.0; z=1.0}
let camera_position = {x= 0.; y = 0.35; z = -1. }
let ambient = 0.05

let width = 1200
let height = 900
let ratio = float width /. float height

let x0 = -1.
and y0 = -1. /. ratio +. 0.25
and x1 = 1.
and y1 = 1. /. ratio +. 0.25

let filename = "fig.png"
let max_reflections = 5

let clip v below above =
  if v < below then below
  else if v > above then above
  else v
 
let rgb_colour {x;y;z} =
  let f v =  int_of_float (clip v 0.0 1.0 *. 255.) in
  Color.({r = f x; g = f y; b = f z})

let trace_ray = ref (fun ~specular ->
    failwith "Run with either [-staged] or [-unstaged]")

let arg_spec = Arg.[
    "-staged", Unit (fun () -> trace_ray := Raytracer_engine_staged.trace_ray),
    "Run the staged ray tracer";
    "-unstaged", Unit (fun () -> trace_ray := Raytracer_engine.trace_ray),
    "Run the unstaged ray tracer";
  ]

let () = Arg.parse arg_spec (fun _ -> ()) "Usage: raytrace (-staged|-unstaged)"

let trace_ray_scene =
  !trace_ray ~specular:specular_k ~camera:camera_position ~ambient ~light_colour
    ~light_position ~scene

let () = begin
  let img = Rgb24.create width height in
  let xstep = (x1 -. x0) /. float (pred width)
  and ystep = (y1 -. y0) /. float (pred height) in
  let rec xloop x i =
    if x -. x1 < 1e-6 then begin
      if i mod 30 = 0 then begin
        Printf.fprintf stderr "%.1f%%\n" ((float i /. float width) *. 100.0);
        flush stderr;
      end;
      let rec yloop y j =
        if y -. y1 < 1e-6 then begin
          let camera_target = {x; y; z=0.0} in
          let d = normalize (camera_target -| camera_position) in
          let rec loop ~reflection_ ~rayO ~rayD ~depth ~colour =
            if depth < max_reflections then
              match trace_ray_scene rayO rayD with
              | Some (obj, m, n, col_ray) ->
                loop ~reflection_:(reflection obj) ~depth:(succ depth)
                  ~colour:(colour +| (col_ray *|. reflection_))
                  ~rayO:(m +| (n *|. 0.0001))
                  ~rayD:(normalize (rayD -| (n *|. (2.0 *. (dot rayD n)))))
              | None -> colour
            else colour
          in
          let col = loop ~reflection_:1.0 ~depth:0 ~rayO:camera_position ~rayD:d
              ~colour:{x=0.0; y=0.0; z=0.0}
          in
          Rgb24.set img i  (height - j - 1) (rgb_colour col);
          yloop (y +. ystep) (succ j)
        end
      in yloop y0 0;
      xloop (x +. xstep) (succ i)
    end
  in xloop x0 0;
  Png.save filename [] (Images.Rgb24 img)
end

