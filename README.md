## vCard library for OCaml

Library for parsing and creating contact information in the vCard format.
Currently only version 4.0 of the vCard specification is supported.

## Example Application

    open Vcard
    open Ulexing
    open Syntax
              
    module R = Core.Result
                    
    let () =  
      match open_in_bin "contacts.vcf" |> Stream.of_channel |> Lexer.parse with 
      | R.Ok vcards ->
        List.iter (fun vcard ->
          Vcard_4_0.of_parsed vcard |> function
            | R.Ok vc -> Printer.print stdout vc; flush stdout;
            | _ -> print_endline "Error"
        ) vcards
      | R.Error (Lexer.Lexing (start, end', buf)) -> 
        Printf.printf "Lexing error, start: %i, end: %i, buffer: %s\n" start end' buf
      | R.Error (Lexer.Parsing (start, end', buf)) -> 
        Printf.printf "Parsing error, start: %i, end: %i, buffer: %s\n" start end' buf