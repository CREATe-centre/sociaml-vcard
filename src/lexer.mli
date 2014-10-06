type error =
	| Lexing of (int * int * string)
	| Parsing of (int * int * string)

val parse : char Stream.t -> (Syntax.vcard list, error) Core.Result.t