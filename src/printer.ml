open Vcard_4_0

let escape_value value =
  let open Core_kernel.Std in
  value |> String.substr_replace_all ~pattern:"\\" ~with_:"\\\\" |> 
  String.substr_replace_all ~pattern:"\n" ~with_:"\\n" |> 
  String.substr_replace_all ~pattern:"," ~with_:"\\,"
  
let rec split s out =
  let open CamomileLibrary in
  match String.length s with
  | l when l <= 75 -> output_string out s
  | l -> 
    let e = UTF8.prev s 76 in
    String.sub s 0 e |> output_string out;
    output_string out "\r\n ";
    split (String.sub s e (l - e)) out

let print out vcard =
  let open Content_line in
  output_string out "BEGIN:VCARD\r\nVERSION:4.0\r\n";
  vcard.content_lines |> List.iter (fun cl ->
    let buf = Buffer.create 75 in
    (match Group.to_string cl.group with 
    | Some g -> Buffer.add_string buf g; Buffer.add_char buf '.'
    | None -> ());
    Name.to_string cl.name |> Buffer.add_string buf;
    cl.parameters |> List.iter (fun p -> 
      Buffer.add_char buf ';';
      Buffer.add_string buf p.Parameter.name;
      Buffer.add_char buf '=';
      List.hd p.Parameter.values |> Buffer.add_string buf;
      List.tl p.Parameter.values |> List.iter (fun v ->
        Buffer.add_char buf ',';
        Buffer.add_string buf v)
      );
    Buffer.add_char buf ':';
    Value.to_string cl.value |> escape_value |> Buffer.add_string buf;
    split (Buffer.contents buf) out;
    output_string out "\r\n");
  output_string out "END:VCARD\r\n"