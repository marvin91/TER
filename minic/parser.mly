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

// %token < Ast.signedless * Ast.num * int> CONST_INT
%token <int> CONST_INT
%token <string> IDENT
%token <int64> CONST_LONG
%token <int32> CONST_SHORT
%token <char> CONST_CHAR
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
| d=decl_var; SEMI	{ Dvar (d) 		}
| STRUCT ; id=ident; LBRACKET; l=list(terminated(decl_var,SEMI));
  RBRACKET; SEMI	{ Dstruct (id, l)	}
| EXTERN; t = typ; v = var; LPAREN;
  l = separated_list(COMMA, decl_var) ; RPAREN; SEMI 
    	{
      		let t, i = unvar t v in
      		Dfun(t, i, l, None)
    	}
| t=typ; v=var; LPAREN;
  l=separated_list(COMMA,decl_var); RPAREN; b=block;
    	{
		let t, i = unvar t v in
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
| id=IDENT		{ loc id		}
/*
arguments:
| t=typ; v=var
| t=typ; v=var; COMMA; a=arguments  */

var:
| id=ident		{ I(id)			}
| STAR; v=var		{ P(v)			}

l_expr:
| l=separated_list(COMMA, expr)	{ l	}

expr:
| c=const				{ loc c				}
| id=ident				{ loc (Eident (id))		}
| LPAREN; e=expr; RPAREN		{ e				}
| e1=expr; op=binop; e2=expr		{ loc (Ebinop (e1, op, e2))	}
| e=expr; POINT; id=ident		{ loc (Eacces (e, id))		}
| e=expr; ARROW; id=ident		{ loc (Eacces (e, id))		}
| e1=expr; ASSIGN; e2=expr		{ loc (Eassign (e1,e2))		}
| id=ident; LPAREN; le=l_expr; RPAREN	{ loc (Earray (id, le))		}
| e1=expr; LBRACE; e2=expr; RBRACE	{ loc (Ebinop (e1, Plus, e2))	} 
| MINUS; e=expr    %prec uminus 	{ loc (Eunop (Uminus, e))	}
| STAR; e=expr	   %prec ustar		{ loc (Eunop (Deref, e))	}
| INCR; e=expr | e=expr; INCR		{ loc (Eunop (Incr, e))		}
| DECR; e=expr | e=expr; DECR		{ loc (Eunop (Decr, e))		}
| NOT; e=expr				{ loc (Eunop (Not, e))		}
| ADDR; e=expr				{ loc (Eunop (Adr, e))		}
| PLUS; e=expr    %prec uplus		{ loc (Eunop (Posit, e))	}
| SIZEOF; LPAREN; t=typ; RPAREN		{ loc (Esizeof t)		}
| LPAREN; t=typ; RPAREN; e=expr		{ loc (Etype (t, e))		}
;

/* e1 -> l  ==> ( *e1 ).l 
   e1[e2] ==> (e1+e2)       */

instr:
| SEMI  				{ loc Sskip			}
| e=expr; SEMI				{ loc (Sexpr e)			}
| IF; LPAREN; e=expr; RPAREN; i=instr	{ loc (Sif (e, i))		}
| IF; LPAREN; e=expr; RPAREN; i1=instr;
  ELSE; i2=instr			{ loc (Sifels (e, i1, i2))	}
| WHILE; LPAREN; e=expr; RPAREN; i=instr{ loc (Swhile (e, i))		}
| FOR; LPAREN; l1=l_expr; SEMI; e=expr;
  SEMI; l2=l_expr; RPAREN; i=instr	{ loc (Sfor (l1, e, l2, i))	}
| b=block				{ loc (Sbloc b)			}
| RETURN; e=expr; SEMI			{ loc (Sreturn e)		}
;

const:
/*
| i=CONST_INT		{ Econst (Cint (let s, t, i = c in
				s,t,i))	}	*/
| i=CONST_INT		{ Econst (Cint i)		}
| l=CONST_LONG		{ Econst (Clong l)		}
| c=CONST_CHAR		{ Econst (Cchar c)		}
| d=CONST_DOUBLE	{ Econst (Cdouble d)		}
| s=CONST_SHORT		{ Econst (Cshort s)		}
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

