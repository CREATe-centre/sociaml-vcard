# sociaml-vcard
> An OCaml library for parsing and creating vCard formatted data.

**Development on this project has ceased.**

OCaml library for parsing and creating contact information in the vCard format. Only version 4.0 of the vCard specification is supported.

## Installation

This library is distributed through [OPAM](https://opam.ocaml.org/), simply run `opam install social-vcard`. The [Findlib](http://projects.camlcity.org/projects/findlib.html) package name is `sociaml_vcard`.

## Example Application

```ocaml
open Vcard
open Ulexing
open Syntax
          
module R = Core_kernel.Result
                
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
```

## Development setup

Development requires a working OCaml installation and [OASIS](http://oasis.forge.ocamlcore.org/) installed. It is recommended that you use [OPAM](https://opam.ocaml.org/) to install and configure Ocaml and OASIS. Please refer to the `Makefile` for build targets.

## Meta

Distributed under the ISC license. See ``LICENSE`` for more information.

<https://github.com/CREATe-centre/sociaml-vcard>
