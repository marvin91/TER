(* Analyse lexicale *)
{

  open Lexing
  open Parser
  open Ast

  (* Erreurs lexicales *)

  exception Lexical_error of string                                                                                                                            
  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum;
        pos_cnum=0 }

  let char_error s = raise (Lexical_error ("illegal character sequence: " ^ s))

  let keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [ "if",    IF;
	"else",  ELSE;
	"while", WHILE;
	"char",   CHAR;
	"double", DOUBLE;
	"extern", EXTERN;
	"for", FOR; 
	"int", INT;
	"return", RETURN;
	"long", LONG;
	"short", SHORT;
	"unsigned", UNSIGNED;
	"struct", STRUCT;
	"void", VOID;
	"sizeof", SIZEOF;
      ]	;
    fun s ->
      try  Hashtbl.find h s
      with Not_found -> IDENT(s)
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let s_char = ([' '-'~']#['\'' '\\' '\"'])
let char = s_char | "\\" ("\\"|'n'|'t'| ('x'['0'-'9' 'a'-'f' 'A'-'F']))
let integer = digit+
let ident = ( alpha | '_')( alpha | '_' |digit)*

rule token = parse
  | '\n'
      { newline lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
      { token lexbuf }
  | "/*"
      { comment lexbuf }
  | ';'
      { SEMI }
  | ','
      { COMMA }
  | '('
      { LPAREN }
  | ')'
      { RPAREN }
  | '['
      { LBRACKET }
  | ']'
      { RBRACKET }
  | '*'
      { STAR }
  | '.'
      { POINT }
  | "->"
      { ARROW }
  | "++"
      { INCR }
  | "--"
      { DECR }
  | '+'
      { PLUS }
  | '-'
      { MINUS }
  | "++"
      { INCR }
  | "--"
      { DECR }
  | "&&"
      { AND }
  | '&'
      { ADDR }
  | '!'
      { NOT }
  | '%'
      { MODULO }
  | "||"
      { OR }
  | "=="
      { EQ }
  | '='
      { ASSIGN }
  | "!="
      { NEQ }
  | "<="
      { LE }
  | '<'
      { LT }
  | ">="
      { GE }
  | '>'
      { GT }
  | '*'
      { STAR }
  | '/'
      { DIV }
  | '{'
      { LBRACE }
  | '}'
      { RBRACE }
  | ident
      { keyword (lexeme lexbuf) }
  | (integer as i)(['U''u']? as u)(['L''l']? as l)
      {
	let s = if u = "" then
            Ast.Signed
          else Ast.Unsigned in
        let t = if l = "" then
            Ast.Int
          else Ast.Long in
        try
          CONST_INT(s, t, int_of_string i)
        with
          _ ->
            raise (Lexical_error ("illegal integer constant: " ^ i))
      }
  | alpha+
      { keyword (lexeme lexbuf) }
  | s_char?
      { CONST_INT (Signed, Char, ( int_of_string (lexeme lexbuf) )) }
  | digit+('u'|'U')?('l'|'L')?
      { CONST_INT (Signed, Long, (int_of_string (lexeme lexbuf))) }
  | (digit+ '.'digit* | '.'digit+)(('e'|'E')'-'?digit+)?
      { CONST_DOUBLE (float_of_string( lexeme lexbuf)) }
  | eof
      { EOF }
  | _
      { raise (Lexical_error ("illegal character: " ^ lexeme lexbuf)) }

and comment = parse
    | "//"
	{ newline lexbuf; token lexbuf }
    | "/*"
	{ comment lexbuf }
    | "*/"
	{ token lexbuf }
    | _
	{ comment lexbuf }
    | eof
	{ raise (Lexical_error "unterminated comment") }
