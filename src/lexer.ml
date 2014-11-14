open Parser
open Ulexing

module R = Core_kernel.Result

let regexp newline = "\r\n"

let regexp version = "VERSION:" ['0' - '9']* '.'? ['0' - '9']* newline

let regexp name = ['0' - '9' 'a' - 'z' 'A' - 'Z' '-']+

let regexp whitespace = [' ' '\t']

let regexp non_ascii = [0x80 - 0x10FFFF]

let regexp safe_chars = (whitespace | '!' | [0x23 - 0x39] | [0x3C - 0x7E] | non_ascii )+

let regexp quote_safe_chars = (whitespace | '!' | [0x23 - 0x7E] | non_ascii )+

let common_value buf cfunc = lexer
	| "\\," -> 
		Buffer.add_char buf ',';
		cfunc buf lexbuf
	| "\\\\" -> 
		Buffer.add_char buf '\\';
		cfunc buf lexbuf
	| ("\\n" | "\\N") -> 
		Buffer.add_char buf '\n';
		cfunc buf lexbuf
	| "\\" -> 
		raise Error
  | _ ->
    utf8_lexeme lexbuf |> Buffer.add_string buf;
    cfunc buf lexbuf

let rec value buf = lexer
  | newline -> VALUE(Buffer.contents buf)
	| "\\;" -> 
		Buffer.add_string buf "\\;";
		value buf lexbuf
	| _ -> 
		rollback lexbuf;
		common_value buf value lexbuf 
		
let rec parameter_value buf = lexer
	| [',' ';' ':']   -> 
		let pv = PARAMETER_VALUE (Buffer.contents buf) in
		rollback lexbuf;
		pv
	| safe_chars -> 
		rollback lexbuf;
		common_value buf parameter_value lexbuf 
		
let rec quoted_parameter_value buf = lexer
  | ['"']   -> PARAMETER_VALUE (Buffer.contents buf) 
	| quote_safe_chars -> 
		rollback lexbuf;
		common_value buf quoted_parameter_value lexbuf

let rec token =
  let buf = Buffer.create 75 in 
	let read_into_buf lexbuf rfunc = 
		let v = rfunc buf lexbuf in
		Buffer.clear buf;
		v
	in
  lexer
  | eof -> EOF
	| newline -> token lexbuf
  | "BEGIN:VCARD" newline -> BEGIN
  | version ->
    let s = String.length "VERSION:" in
    let l = lexeme_length lexbuf - s - 2 in 
    VERSION(utf8_sub_lexeme lexbuf s l)
  | "END:VCARD" newline -> END
	| ';' -> SEMICOLON
	| "=\"" -> read_into_buf lexbuf quoted_parameter_value 
	| ['=' ','] -> read_into_buf lexbuf parameter_value 
	| name '.' -> 
		GROUP (utf8_sub_lexeme lexbuf 0 (lexeme_length lexbuf - 1))
	| name -> NAME (utf8_lexeme lexbuf)
  | ':' -> read_into_buf lexbuf value

let unwrap_stream stream = 
	Stream.from (fun _ ->
		match Stream.peek stream with
		| Some v when v = '\r' -> 
			(match Stream.npeek 3 stream with
			| ['\r'; '\n'; ' ']
			| ['\r'; '\n'; '\t'] -> 
				Stream.junk stream;
				Stream.junk stream;
				Stream.junk stream;
				Some (Stream.next stream)
			| _  -> Some (Stream.next stream))
		| Some _ -> Some (Stream.next stream)
		| None -> None) 

type error =
	| Lexing of (int * int * string)
	| Parsing of (int * int * string)

let parse stream =  
	let open MenhirLib.Convert.Simplified in
	let lexbuf = stream |> unwrap_stream |> from_utf8_stream in 
  let revised_parser = traditional2revised Parser.vcards in
	try
  	R.Ok (revised_parser (fun () -> (token lexbuf, Lexing.dummy_pos, Lexing.dummy_pos)))
	with
	| Error ->
    R.Error (Lexing (lexeme_start lexbuf, lexeme_end lexbuf, utf8_lexeme lexbuf))
	| Parser.Error -> 
    R.Error (Parsing (lexeme_start lexbuf, lexeme_end lexbuf, utf8_lexeme lexbuf))
	  