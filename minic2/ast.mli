(* Arbres de syntaxe abstraite *)

type ('info, 'node) node = { info : 'info;
			     node : 'node }

type loc = Lexing.position * Lexing.position

type ident = (loc, string) node

type signedness = Unsigned | Signed
type num = Char | Short | Int | Long
type binop = | Plus | Minus | Mult | Div | Modulo
	     | And  | Or
	     | Eq   | Neq   | Lt   | Le  | Gt | Ge
type unop  =
  | Uminus | Not | PreIncr | PostIncr | PreDecr | PostDecr | Adr | Deref | Posit

type c_type =
  | Tnull (* pour typer null *)
  | Tvoid
  | Tnum of (signedness * num)
  | Tdouble
  | Tstruct of ident
  | Tpointer of c_type
  (* à completer *)

type constant =
  (*| Cint of (signedness * num * int)*)
  | Cint of signedness * num * int
  | Cshort of int32
  | Clong of int64
  | Cdouble of float
  | Cchar of char

type 'info expr = ('info, 'info expr_node) node
and 'info expr_node =
  | Enull (* Inséré automatiquement à partir de 0 *)
  | Econst of constant
  | Eident of ident
  | Esizeof of c_type
  | Eassign of ('info expr) * ('info expr)
  | Ebinop of ('info expr) * binop * ('info expr)
  | Eunop of unop * ('info expr)
  | Eacces of ('info expr) * ident
  (* | Earray of ident * ('info expr) *)
  | Ecall of ident * ('info expr)list
  | Etype of c_type * ('info expr)


type var_decl =  c_type * ident

type 'info statement = ('info, 'info statement_node) node

(* Instructions *)
and 'info statement_node =
  | Sskip
  | Sexpr of ('info expr)
  | Sif of ('info expr) * ('info statement)
  | Sifels of ('info expr) * ('info statement) * ('info statement)
  | Swhile of ('info expr) * ('info statement)
  | Sfor of ('info expr)list * ('info expr) * ('info expr)list * ('info statement)
  | Sbloc of ('info block)
  | Sreturn of ('info expr)

and 'info block =
    var_decl list * 'info statement list

type 'info decl =
   | Dfun of c_type * ident * var_decl list * (('info block) option)
   | Dvar of var_decl
   | Dstruct of ident * var_decl list
  (* à compléter *)

type 'info file =  'info decl list
