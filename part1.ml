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

let extract_bits : int * int -> int64_array -> int64 = (* Question 1(a)(i) *)
  fun (lower, upper) arr -> let dim = Bigarray.Array1.dim in
  if lower > upper || (dim arr) * 64 < lower || (dim arr) * 64 < upper || upper - lower > 64 then
    assert false 
  else
    let ml = lower mod 64 and mu = upper mod 64 in
    let first = arr.{ml} in
      if ml < mu then (* If the mod of the lower bound is still less than the mod of the upper bound, one number from array*)
        Int64.shift_right first (63 - mu) 
        (* zero padding from 0 - (ml + 63 - mu) *)
      else
        let second = arr.{mu} in
        Int64.shift_left second mu
        (* zero padding from 0 - (ml - mu) *)

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

(* Question 1(a)(ii)
module Bit_parserM : BIT_PARSERM =
struct

end
*)


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

(* Question 1(a)(iii)
module Bit_parserA : BIT_PARSERA =
struct

end
*)

(* Question 1(a)(iv) is a written question, not a programming question. *)

let extract_bits_staged : int * int -> int64_array code -> int64 code =
    fun (from, upto) word -> assert false (* Question 1(b)(i) *)

module type BIT_PARSERA_staged =
sig
  include BIT_PARSERA

  val compile : 'a t -> (int64_array -> 'a) code
end

(* Question 1(b)(ii)
module Bit_parserA_staged : BIT_PARSERA_staged =
struct

end
*)


(* Question 1(b)(iii)
module Bit_parserA_staged' : BIT_PARSERA_staged =
struct
   (* (This version should generate parsers that read each element of
      the input array at most once) *)
end
*)

(* Question 1(b)(iv) is a written question, not a programming question. *)
