type version = string

type group = string

type name = string

type value = string

type parameter_name = string

type parameter_value = string

type parameter = {
	name : parameter_name;
	values : parameter_value list;
} 

type content_line = {
	group : group option;
	name : name;
	parameters : parameter list;
	value : value;
}

type vcard = {
	version : version;
	content_lines : content_line list;
}
