(** See the OCaml documentation for details about Bigarrays:
    http://caml.inria.fr/pub/docs/manual-ocaml/libref/Bigarray.html *)


type int64_array =
  (int64, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t

module type BIT_PARSER =
sig
  type _ t
  (** [int64 ~bits:n] builds a parser which reads [n]-bit integers. *)
  val int64 : bits:int -> int64 t

  (** [parse p arr] uses the parser [p] to read values from the array [arr]. *)
  val parse : 'a t -> int64_array -> 'a
end

(* Question 1(a)(i) *)
let extract_bits : int * int -> int64_array -> int64 = 
  fun (lower, upper) arr -> let dim = Bigarray.Array1.dim in
  if lower > upper || (dim arr) * 64 < lower || (dim arr) * 64 < upper || upper - lower > 64 then
    assert false 
  else
    let mod_l = lower mod 64 and mod_u = upper mod 64 in  (* modulus of the upper and lower bounds *)
    let idx_l = lower/64 and idx_u = upper/64 in          (* corresponding indexes of bounds *)
    let first = arr.{idx_l} in 
      if idx_l = idx_u then
        let out = Int64.shift_left first mod_l in
        Int64.shift_right_logical out (63 - (mod_u - mod_l)) 
      else
        let second = arr.{idx_u} in
        let x1 = Int64.shift_right_logical second (63 - mod_u) in
        let y1 = Int64.shift_left first mod_l in
        let y2 = Int64.shift_right_logical y1 (63 - (mod_u + mod_l)) in
        Int64.logxor x1 y2
    
module type MONAD =
sig
  type _ t
  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
end

module type BIT_PARSERM =
sig
  include MONAD
  include BIT_PARSER with type 'a t := 'a t
end

(* Question 1(a)(ii)*)
module Bit_parserM : BIT_PARSERM =
struct
  
  (* takes in index of position (state), array and returns pair of the new state and the number of bits of the parser *)
  type 'a t = int -> int64_array -> int * 'a 
  let return x    = fun s arr -> (s, x) 
  let (>>=) par f = fun s arr -> let (s', v) = par s arr in (f v) s' arr
  let int64 ~bits = fun s arr -> (s + bits, extract_bits (bits, s + bits) arr)
  let parse m arr = snd (m 0 arr)

end

module type APPLICATIVE =
sig
  type _ t
  val pure : 'a -> 'a t
  val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
end

module type BIT_PARSERA =
sig
  include APPLICATIVE
  include BIT_PARSER with type 'a t := 'a t
end

(* Question 1(a)(iii) *)
module Bit_parserA : BIT_PARSERA =
struct

  type 'a t = int -> int64_array -> int * 'a
  let int64 ~bits = fun s arr -> (s + bits, extract_bits (bits, s + bits) arr)
  let parse m arr = snd (m 0 arr)
  let pure x = fun s arr -> (s, x) 
  let (<*>) f m = fun s arr ->  let (s', v) = f s arr in 
                                let (y, z) = m s' arr in (y, v z)

end

(* Question 1(a)(iv) is a written question, not a programming question. *)

(* Question 1(b)(i) *)
let extract_bits_staged : int * int -> int64_array code -> int64 code =
    fun (from, upto) word -> 

    let mod_l = from mod 64 and mod_u = upto mod 64 in        (* modulus of the upper and lower bounds *)
    let idx_l = from/64 and idx_u = upto/64 in                (* corresponding indexes of bounds *)
    .<   
      let arr = .~word and dim = Bigarray.Array1.dim in 
      if from > upto || (dim arr) * 64 < from || (dim arr) * 64 < upto || upto - from > 64 then
        assert false 
      else
        let first = arr.{idx_l} in 
        if idx_l = idx_u then
          let out = Int64.shift_left first mod_l in
            Int64.shift_right_logical out (63 - (mod_u - mod_l)) 
        else
          let second = arr.{idx_u} in
          let x1 = Int64.shift_right_logical second (63 - mod_u) in
          let y1 = Int64.shift_left first mod_l in
          let y2 = Int64.shift_right_logical y1 (63 - (mod_u + mod_l)) in
            Int64.logxor x1 y2
    >.

module type BIT_PARSERA_staged =
sig
  include BIT_PARSERA

  val compile : 'a t -> (int64_array -> 'a) code
end

(* Question 1(b)(ii) *)
module Bit_parserA_staged : BIT_PARSERA_staged =
struct
  type 'a t = int -> int64_array code -> int * 'a code

  let int64 ~bits = fun s arr_code -> (s + bits, extract_bits_staged (bits, s + bits) arr_code )
  let parse m arr = Runcode.(!.(snd (m 0 .<arr>.)))
  let pure x = fun s arr_code -> (s, .<x>.)
  let (<*>) f m  = fun s arr_code -> let (s', v) = f s arr_code in 
                                     let (y, z) = m s' arr_code in (y, .< .~v .~z >.)
  let compile m  = .< fun arr -> .~(snd (m 0 .<arr>.)) >.
end


(* Question 1(b)(iii) *)
module Bit_parserA_staged' : BIT_PARSERA_staged =
struct
  type 'a t = int -> int64_array code -> int * 'a code



  


   (* (This version should generate parsers that read each element of
      the input array at most once) *)
end


(* Question 1(b)(iv) is a written question, not a programming question. *)
