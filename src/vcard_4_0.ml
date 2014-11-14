module R = Core_kernel.Result

type error =
  | InvalidCharacters

module Group : sig
  type t 
  val of_string : string option -> (t, error) R.t
  val to_string : t -> string option
end = struct
  
  let allowed_chars = Re2.Regex.create_exn "[0-9a-zA-Z\\-]+"
  
  type t = string option
  
  let of_string = function
    | None -> R.Ok None
    | Some s -> (match Re2.Regex.matches allowed_chars s with
      | true -> R.Ok (Some s)
      | false -> R.Error InvalidCharacters)
  
  let to_string g = g

end

module Name : sig
  type t =
    | SOURCE | KIND | FN | N | NICKNAME | PHOTO | BDAY | ANNIVERSARY | GENDER | ADR 
    | TEL | EMAIL | IMPP | LANG | TZ | GEO | TITLE | ROLE | LOGO | ORG | MEMBER 
    | RELATED | CATEGORIES | NOTE | PRODID | REV | SOUND | UID | CLIENTPIDMAP
    | URL | KEY | FBURL | CALADRURI | CALURI | XML | BIRTHPLACE | DEATHPLACE
    | DEATHDATE | EXPERTISE | HOBBY | INTEREST | ORG_DIRECTORY | X_NAME of string
  val of_string : string -> (t, error) R.t
  val to_string : t -> string
end = struct
  
  type t =
    | SOURCE | KIND | FN | N | NICKNAME | PHOTO | BDAY | ANNIVERSARY | GENDER | ADR 
    | TEL | EMAIL | IMPP | LANG | TZ | GEO | TITLE | ROLE | LOGO | ORG | MEMBER 
    | RELATED | CATEGORIES | NOTE | PRODID | REV | SOUND | UID | CLIENTPIDMAP
    | URL | KEY | FBURL | CALADRURI | CALURI | XML | BIRTHPLACE | DEATHPLACE
    | DEATHDATE | EXPERTISE | HOBBY | INTEREST | ORG_DIRECTORY | X_NAME of string
  
  let of_string t = 
    R.Ok (match String.uppercase t with
    | "SOURCE" -> SOURCE | "KIND" -> KIND | "FN" -> FN | "N" -> N | "NICKNAME" -> NICKNAME
    | "PHOTO" -> PHOTO | "BDAY" -> BDAY | "ANNIVERSARY" -> ANNIVERSARY | "GENDER" -> GENDER
    | "ADR" -> ADR | "TEL" -> TEL | "EMAIL" -> EMAIL | "IMPP" -> IMPP | "LANG" -> LANG
    | "TZ" -> TZ | "GEO" -> GEO | "TITLE" -> TITLE | "ROLE" -> ROLE | "LOGO" -> LOGO 
    | "ORG" -> ORG | "MEMBER" -> MEMBER | "RELATED" -> RELATED | "CATEGORIES" -> CATEGORIES
    | "NOTE" -> NOTE | "PRODID" -> PRODID | "REV" -> REV | "SOUND" -> SOUND | "UID" -> UID
    | "CLIENTPIDMAP" -> CLIENTPIDMAP | "URL" -> URL | "KEY" -> KEY | "FBURL" -> FBURL 
    | "CALADRURI" -> CALADRURI | "CALURI" -> CALURI | "XML" -> XML | "BIRTHPLACE" -> BIRTHPLACE
    | "DEATHPLACE" -> DEATHPLACE | "DEATHDATE" -> DEATHDATE | "EXPERTISE" -> EXPERTISE
    | "HOBBY" -> HOBBY | "INTEREST" -> INTEREST | "ORG_DIRECTORY" -> ORG_DIRECTORY
    | name -> X_NAME(name))

  let to_string = function
    |SOURCE -> "SOURCE" | KIND -> "KIND" | FN -> "FN" | N -> "N" | NICKNAME -> "NICKNAME" 
    | PHOTO -> "PHOTO" | BDAY -> "BDAY" | ANNIVERSARY -> "ANNIVERSARY" | GENDER -> "GENDER" 
    | ADR  -> "ADR" | TEL -> "TEL" | EMAIL -> "EMAIL" | IMPP -> "IMPP" | LANG -> "LANG" 
    | TZ -> "TZ" | GEO -> "GEO" | TITLE -> "TITLE" | ROLE -> "ROLE" | LOGO -> "LOGO" | ORG -> "ORG" 
    | MEMBER -> "MEMBER" | RELATED -> "RELATED" | CATEGORIES -> "CATEGORIES" | NOTE -> "NOTE"
    | PRODID -> "PRODID" | REV -> "REV" | SOUND -> "SOUND" | UID -> "UID" 
    | CLIENTPIDMAP -> "CLIENTPIDMAP" | URL -> "URL" | KEY -> "KEY" | FBURL -> "FBURL" 
    | CALADRURI -> "CALADRURI" | CALURI -> "CALURI" | XML -> "XML" | BIRTHPLACE -> "BIRTHPLACE"
    | DEATHPLACE -> "DEATHPLACE" | DEATHDATE -> "DEATHDATE" | EXPERTISE -> "EXPERTISE" 
    | HOBBY -> "HOBBY" | INTEREST -> "INTEREST" | ORG_DIRECTORY -> "ORG_DIRECTORY" | X_NAME xname -> xname
  
end

module Parameter : sig
  type t = { 
    name : string;
    values : string list;
  }
  val of_parsed : Syntax.parameter -> (t, error) R.t
end = struct
  
  type t = { 
    name : string;
    values : string list;
  }
  
  let of_parsed (parameter : Syntax.parameter ) = R.Ok {
    name = parameter.Syntax.name;
    values = parameter.Syntax.values;
  }
  
end

module Value : sig
  type t
  val of_string : string -> (t, error) R.t
  val to_string : t -> string
end = struct
  
  type t = string
  
  let of_string t = R.Ok t
  
  let to_string t = t 
  
end

module Content_line = struct

  type t = {
    group : Group.t;
    name : Name.t;
    parameters : Parameter.t list;
    value : Value.t;
  }

  let of_parsed parsed = 
    let (>>=) = R.(>>=) in
    Group.of_string parsed.Syntax.group >>= fun group ->
    Name.of_string parsed.Syntax.name >>= fun name ->
      
    parsed.Syntax.parameters |> List.map Parameter.of_parsed |> R.all >>= fun parameters -> 
      
    Value.of_string parsed.Syntax.value >>= fun value ->      
    R.Ok {group = group; name = name; parameters = parameters; value = value;}

end

type t = {
  content_lines : Content_line.t list;
}

let of_parsed parsed = 
  let (>>=) = R.(>>=) in
  parsed.Syntax.content_lines |> List.map Content_line.of_parsed |> R.all >>= fun cls ->
  R.Ok { content_lines = cls; }
  