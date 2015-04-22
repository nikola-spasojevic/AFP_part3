module type BIT_PARSER =
sig
	type _ t
	val int64 : bits : int → int64 t
	val parse : ✬a t → int64_array → ✬a
end