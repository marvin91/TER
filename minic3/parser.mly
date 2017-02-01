/* Parseur pour le compilateur C */

%{
  open Ast


  let mk_loc e l = { info = l; node = e }

  let loc e =
    mk_loc e (Parsing.symbol_start_pos (),Parsing.symbol_end_pos())

  let loc_i i e =
    mk_loc e (Parsing.rhs_start_pos i, Parsing.rhs_end_pos i)

  let loc_dummy e =
    mk_loc e (Lexing.dummy_pos, Lexing.dummy_pos)

  type pvar =
    | I of ident
    | P of pvar

  let rec unvar t v =
    match v with
      | I l -> (t, l)
      | P vv -> unvar (Tpointer t) vv

%}

%token < Ast.signedness * Ast.num * int> CONST_INT
%token <string> IDENT
%token <float> CONST_DOUBLE
%token CHAR DOUBLE INT LONG SHORT VOID
%token UNSIGNED
%token ASSIGN EXTERN STRUCT SIZEOF
%token OR
%token AND
%token EQ NEQ
%token GE GT LE LT
%token PLUS MINUS
%token STAR DIV MODULO
%token NOT INCR DECR ADDR
%token LPAREN RPAREN LBRACKET RBRACKET POINT COMMA ARROW
%token LBRACE RBRACE
%token IF ELSE FOR WHILE
%token SEMI
%token RETURN
%token EOF

%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left GE GT LE LT
%left PLUS MINUS
%left DIV MODULO STAR
%right INCR DECR ADDR
%right ustar uminus uplus
%left RPAREN LBRACKET ARROW POINT


/* Point d'entrée */

%start file
%type <Ast.loc Ast.file> file

%%

file:
| l=list(decl) EOF    { l }
;

typ:
| s=sign; n=num		{ Tnum (s,n)	}
| DOUBLE		{ Tdouble	}
| VOID			{ Tvoid		}
| STRUCT; i=ident	{ Tstruct i	}
;

sign:
|		{ Signed   }
| UNSIGNED 	{ Unsigned }

num:
| CHAR 		{ Char	}
| SHORT 	{ Short }
| INT 		{ Int	}
| LONG		{ Long	}

/* déclarations */

decl:
| d=decl_var; SEMI	{ Dvar (d) 			}
| STRUCT ; id=ident; LBRACKET; l=list(terminated(decl_var,SEMI));
  RBRACKET; SEMI	{ Dstruct (id, l)		}
| EXTERN; t = typ; v = var; LPAREN;
  l = separated_list(COMMA, decl_var) ; RPAREN; SEMI 
    	{  	let t, i = unvar t v in
      		Dfun(t, i, l, None)   	
	}
| t=typ; v=var; LPAREN;
  l=separated_list(COMMA,decl_var); RPAREN; b=block;
    	{	let t, i = unvar t v in
		Dfun (t, i, l, Some b)	
	}
;

decl_var:
| t=typ; v=var		{ unvar t v	 	}
;

block:
| LBRACKET; lv=list(terminated(decl_var, SEMI)); li=list(instr); RBRACKET
			{ (lv, li)		}

ident:
| id=IDENT		{ mk_loc id ($startpos, $endpos)}

var:
| id=ident		{ I(id)	}
| STAR; v=var		{ P(v)	}

l_expr:
| l=separated_list(COMMA, expr)	{ l	}

expr_:
| c=const				{  c			}
| id=ident				{ (Eident (id))		}
| LPAREN; e=expr_; RPAREN		{ e			}
| e1=expr; op=binop; e2=expr		{ (Ebinop (e1, op, e2))	}
| e=expr; POINT; id=ident		{ (Eacces (e, id))	}
| e=expr; ARROW; id=ident		{ (Eacces (e, id))	}
| e1=expr; ASSIGN; e2=expr		{ (Eassign (e1,e2))	}
| id=ident; LPAREN; le=l_expr; RPAREN	{ (Ecall (id, le))	}
// | id=ident; LBRACE; e=expr; RBRACE	{ (Earray (id, e))	}
| e1=expr; LBRACE; e2=expr; RBRACE	{ (Ebinop (e1, Plus, e2))} 
| MINUS; e=expr    %prec uminus 	{ (Eunop (Uminus, e))	}
| STAR; e=expr	   %prec ustar		{ (Eunop (Deref, e))	}
| INCR; e=expr 				{ (Eunop (PreIncr, e))	}
| e=expr; INCR				{ (Eunop (PostIncr, e))	}
| DECR; e=expr 				{ (Eunop (PreDecr, e))	}
| e=expr; DECR				{ (Eunop (PostDecr, e))	}
| NOT; e=expr				{ (Eunop (Not, e))	}
| ADDR; e=expr				{ (Eunop (Adr, e))	}
| PLUS; e=expr    %prec uplus		{ (Eunop (Posit, e))	}
| SIZEOF; LPAREN; t=typ; RPAREN		{ (Esizeof t)		}
| LPAREN; t=typ; RPAREN; e=expr		{ (Etype (t, e))	}
;

expr:
| e=expr_		{ mk_loc e ($startpos, $endpos) }
;

/* e1 -> l  ==> ( *e1 ).l 
   e1[e2] ==> (e1+e2)       */

instr_:
| SEMI  				{ Sskip			}
| e=expr; SEMI				{ (Sexpr e)		}
| IF; LPAREN; e=expr; RPAREN; i=instr	{ (Sif (e, i))		}
| IF; LPAREN; e=expr; RPAREN; i1=instr;
  ELSE; i2=instr			{ (Sifels (e, i1, i2))	}
| WHILE; LPAREN; e=expr; RPAREN; i=instr{ (Swhile (e, i))	}
| FOR; LPAREN; l1=l_expr; SEMI; e=expr;
  SEMI; l2=l_expr; RPAREN; i=instr	{ (Sfor (l1, e, l2, i))	}
| b=block				{ (Sbloc b)		}
| RETURN; e=expr; SEMI			{ (Sreturn e)		}
;

instr:
| i=instr_		{ mk_loc i ($startpos, $endpos) }
;

const:

| i=CONST_INT		{ let s,t,c = i in Econst (Cint(s,t,c))	}
| d=CONST_DOUBLE	{ Econst (Cdouble d)		}
;

%inline binop:
| PLUS  { Plus  }
| MINUS { Minus }
| STAR  { Mult  }
| DIV   { Div   }
| MODULO{ Modulo}
| AND   { And   }
| OR    { Or    }
| EQ    { Eq    }
| NEQ   { Neq   }
| LT    { Lt    }
| LE    { Le    }
| GT    { Gt    }
| GE    { Ge    }
;

