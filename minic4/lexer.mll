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
      { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1 }

  let char_error s = raise (Lexical_error ("illegal character sequence: " ^ s))

  let explode s =
    let rec expl i l =
      if i < 0 then l else
        expl (i - 1) (s.[i] :: l) in
    expl (String.length s - 1) []


  let implode l =
    let result = Bytes.create (List.length l) in
    let rec imp i = function
      | [] -> result
      | c :: l -> Bytes.set result i c; imp (i + 1) l in
    imp 0 l

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
      ] ;
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
  | "//"
      { commentline lexbuf }
  | ';'
      { SEMI }
  | ','
      { COMMA }
  | '('
      { LPAREN }
  | ')'
      { RPAREN }
  | '['
      { LBRACE }
  | ']'
      { RBRACE }
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
      { LBRACKET }
  | '}'
      { RBRACKET }

  | "#include <stdlib.h>"
      { commentline lexbuf }

  | "#include <stdio.h>"
      { commentline lexbuf }

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
(*
  | s_char?
      { CAR (lexeme_char lexbuf 1) }

  | (char* as str)
      {
        let chl =
          try  
            explode str
          with
            _ -> raise (Lexical_error ("Not implemented " ^ str))
        in
        CHAINE chl
  }    *)

  | ((digit+ '.'digit* | '.'digit+) as i)(((('e'|'E')('-')?)? as e)((digit+)? as p))?
      {
        let x =
          try
            match e with
            | None -> begin
                match p with
                | Some pt -> raise (Lexical_error ("illegal float precision" ^ pt))
                | None -> 1.
            end
            | Some bt ->
               match (explode bt), p with
               | ['e';'-'],Some d -> -(10.) ** (float_of_string d)
               | ['e'],Some d -> 10. ** (float_of_string d)
               | ['E';'-'],Some d -> -10. ** (float_of_string d)
               | ['E'],Some d -> 10. ** (float_of_string d)
               | ['e'],None -> 1.
               | ['E'],None -> 1.
               | _,_ -> raise (Lexical_error ("illegal float precision" ^ bt))
          with
            _ -> raise (Lexical_error ("illegal float precision " ^ i))
        in
        try
          CONST_DOUBLE ((float_of_string i)*. x)
        with
          _ ->
            raise (Lexical_error ("illegal float constant: " ^ i))
      }

  | eof
      { EOF }
  | _
      { raise (Lexical_error ("illegal character: " ^ lexeme lexbuf)) }
      
and comment = parse
    | "*/"
        { token lexbuf }
    | "\n"
	{ newline lexbuf; comment lexbuf }
    | eof
        { raise (Lexical_error "unterminated comment") }
    | _
        { comment lexbuf }

and commentline = parse
    | "\n"
	{ newline lexbuf; token lexbuf }
    | eof
	{ EOF }
    | _
	{ commentline lexbuf }
