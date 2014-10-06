%token <string> VERSION GROUP NAME VALUE PARAMETER_VALUE
%token BEGIN SEMICOLON END EOF

%start <Syntax.vcard list> vcards

%%

vcards:
  | vcards = vcard*
		EOF
		{ vcards }

vcard: 
  | BEGIN 
	  version = VERSION 
		content_lines = content_line*
		END 
		{ { 
			Syntax.version = version;
			content_lines = content_lines
		} }

content_line:
  | group = option(GROUP)
	  name = NAME
		parameters = parameter*
		value = VALUE 
		{ {
			Syntax.group = group;
			name = name;
			parameters = parameters;
			value = value;
		} }
		
parameter:
	| SEMICOLON
		name = NAME
		values = PARAMETER_VALUE+ 
		{ {
			Syntax.name = name;
			values = values
		} }
