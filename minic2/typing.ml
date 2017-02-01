open Ast

module Env = Map.Make(String)

let global_env = Hashtbl.create 17
let struct_env : (string, var_decl list) Hashtbl.t = Hashtbl.create 17
let fun_env = Hashtbl.create 17

let compatible t1 t2 =
  let rec compat_aux t1 t2 =
    match t1, t2 with
    | Tstruct id1, Tstruct id2 -> id1.node = id2.node
    | Tpointer (Tvoid), Tpointer _ -> true
    | Tpointer tt1, Tpointer tt2 -> compat_aux tt1 tt2
    | Tnull, Tpointer _ -> true
    | (Tdouble | Tnull | Tnum(_) | Tpointer _),
      (Tdouble | Tnull | Tnum(_)) -> true
    | _ -> false
  in
  if t1 <> t2 then
    compat_aux t1 t2 || compat_aux t2 t1
  else false
    
let num t =
  match t with
  | Tstruct _
  | Tvoid -> false
  | _ -> true

let arith t =
  match t with
  | Tpointer _ -> false
  | _ -> if num t then true else false
     
let rank t =
  let rank_aux n =
    match n with
      Char -> 7
    | Short -> 15
    | Int -> 31
    | Long -> 63
  in
  match t with
  | Tnum(Signed, n) -> rank_aux n
  | Tnum(Unsigned, n) -> 1 + rank_aux n
  | Tdouble -> 100
  | Tnull -> 0
  | _ -> assert false

let inf_type t1 t2 =
  rank t1 < rank t2

let max_type t1 t2 =
  if inf_type t1 t2 then t2
  else t1

exception TypeError of loc * string
let error loc msg = raise (TypeError (loc, msg))
    
let rec type_bf t =
  match t with
    Tpointer tt -> type_bf tt
  | Tstruct id -> Hashtbl.mem struct_env id.node
  | _ -> true

let type_var_decl vd =
  let _ =
  List.fold_left (fun seen (t, id) ->
    if type_bf t && t <> Tvoid && not (List.mem id.node seen) then
      id.node :: seen
    else error id.info "Champ ou variable incorrect")
    [] vd
  in
  ()

let add_env env vd =
  List.fold_left (fun acc (t, id) -> Env.add id.node t acc) env

let rec type_expr e =
  match e.node with
  | Econst c ->
    begin
      match c with
	Cint (s,n,i) -> Tnum (s,n)
      | Cshort s -> Tnum (Signed, Short)
      | Clong l -> Tnum (Signed, Long)
      | Cdouble d -> Tdouble
      | Cchar c -> Tnum (Signed, Char)
    end

  | Enull -> Tnull

  | Etype (t, e1) ->
     if inf_type (type_expr e1) t then t
     else error e1.info ("Le type de cette expression est de taille inférieur au type indiqué ")

  | Eident id ->
     if Hashtbl.mem global_env id.node then
       Hashtbl.find global_env id.node
     else error id.info ("Variable non déclarée " ^ id.node)
      
  | Esizeof t ->
     if type_bf t && (t <> Tvoid) then Tnum(Unsigned, Long)
     else error e.info ("Le type est mal formé ou de type void ")
       
  | Eassign (e1,e2) ->
     let t1 = type_expr e1
     and t2 = type_expr e2 in
     if compatible (t1) (t2) && type_bf t1 && type_bf t2 then max_type t1 t2
     else error e1.info ("L'assignation nécéssite 2 types compatibles ")

  | Eunop (unop, e1) ->
     let typ = type_expr e1 in
     if type_bf typ then
       begin
	 match unop with
	 | PreIncr | PreDecr | PostIncr | PostDecr ->
	    if num typ then typ
	    else error e1.info ("La valeur à incrémenter ou décrémenter doit être de type numérique ")
	 | Not ->
	    if num typ then
	      Tnum(Signed, Int)
	    else error e1.info ("La valeur à inverser doit être de type numérique ")
	 | Posit | Uminus ->
	    if arith typ then typ
	    else error e1.info ("Une valeur positive ou négative doit être de type arithmétique ")
	 | Adr -> Tpointer typ
	 | Deref ->
	    match typ with
	      Tpointer tt -> tt
	    | _ -> error e1.info ("Cette expression n'est pas un pointeur ")
       end
     else error e1.info ("Type de l'expression mal formé ")

  | Ebinop (e1, binop, e2) ->
     let t1 = type_expr e1
     and t2 = type_expr e2 in
     if type_bf t1 && type_bf t2 then
	 begin
	   match binop with
	   | Eq | Neq | Lt | Le | Gt | Ge ->
	      if num t1 && compatible t1 t2 then Tnum(Signed, Int)
	      else error e1.info ("La comparaison nécéssite 2 types compatibles ")
	   | Plus ->
	      begin
		match t1 with
		| Tpointer _ ->
		   if rank (max_type t2 (Tnum(Signed, Int))) < 65 && compatible t1 t2 then t1
		   else error e1.info ("L'addition nécéssite 2 types compatibles " ^
				   "et de longueur inférieur à un unsigned long")
		| _ ->
		   if compatible t1 Tdouble && compatible t1 t2 then max_type t1 t2
		   else error e1.info ("L'opération nécéssite 2 types compatibles ")
	      end
	   | Minus ->
	      begin
		match t1, t2 with
		| Tpointer _, _ ->
		   if t1 = t2 then Tnum(Signed, Long) 
		   else
		     if rank (max_type t2 (Tnum(Signed, Int))) < 65 then t1
		     else error e1.info ("L'opération nécéssite 2 types compatibles " ^
				   "et de longueur inférieur à un unsigned long ")
		| _ ->
		   if compatible t1 Tdouble && compatible t1 t2 then max_type t1 t2
		   else error e1.info ("L'opération nécéssite 2 types compatibles ")
	      end
	   | Mult | Div ->
	      if compatible t1 Tdouble && compatible t1 t2 then max_type t1 t2
	      else error e1.info ("L'opération nécéssite 2 types compatibles ")
	   | And | Or ->
	      if compatible t1 Tdouble && compatible t1 t2 then Tnum(Signed, Int)
	      else error e1.info ("L'opération logique nécéssite 2 types compatibles ")
	   | Modulo ->
	      if compatible t1 t2 && rank (max_type t1 t2) < 65 then max_type t1 t2
	      else error e1.info ("L'opération modulo nécéssite 2 types compatibles "
			      ^ "et de longueurs inférieures à celle du type unsigned long ")
	 end
     else error e1.info ("Type de l'expression mal formé ")

  | Ecall (f, l) ->
     if Hashtbl.mem fun_env f.node then
       let t, params = Hashtbl.find fun_env f.node in
       let rec plist par arg test =
	 match par, arg with
	 | (parhd::partl),(arghd::argtl) ->
	    let t1, id = parhd in
	    if t1 = type_expr arghd then plist partl argtl true
	    else false
	 | _ -> test
       in
       if plist params l true then t
       else error f.info ("Le type d'un argument de la fonction ne correspond pas " ^ f.node)
     else error f.info ("Cette fonction n'est pas déclarée " ^ f.node)

  | Eacces (e1, id) ->
     let typ = type_expr e1 in
     if type_bf typ then
       match typ with
       | Tstruct id1 ->
	  if Hashtbl.mem struct_env id1.node then
	    let l = Hashtbl.find struct_env id1.node in
	    let rec plist sl sid =
	      match sl with
	      | (t, var)::tl ->
		 if sid = var then t
		 else plist tl sid
	      | _ -> error sid.info ("Cette variable n'existe pas dans la structure " ^ sid.node)
	    in plist l id
	  else error id1.info ("Cette structure n'est pas déclarée " ^ id1.node)
       | _ -> error e1.info ("Cette expression n'est pas une structure ")
     else error e1.info ("Type de l'expression mal formé ")
       (*
let type_instr ins t =
  match instr.node with
    Sexpr e ->
      if type_bf (type_expr e) then t
      else error ins.info ("Le type de cette instruction est mal formé " ^ ins.node)
  | Sreturn e ->
     *)     

let type_block t env block = failwith "Not implemented"
	
    
let type_decl d =
  match d with
  | Dvar (t, i) ->
    if type_bf t && t <> Tvoid
      && not (Hashtbl.mem global_env i.node) then
      begin
	Hashtbl.add global_env i.node t
      end
    else error i.info "Déclaration globale invalide"
      
  | Dstruct (id, var_decl) ->
    if not (Hashtbl.mem struct_env id.node) then
      begin
	Hashtbl.add struct_env id.node var_decl;
        type_var_decl var_decl;
      end
    else error id.info ("Structure déjà déclarée " ^ id.node)
      
  | Dfun (t, f, params, b) ->
    if not (type_bf t) then error f.info ("Type de retour invalide pour " ^f.node);
    if Hashtbl.mem fun_env f.node then
      error f.info ("Redéfinition de la fonction " ^ f.node);
    type_var_decl params;
    Hashtbl.add fun_env f.node (t, params);
    match b with
      None -> ()
    | Some block -> let env = add_env Env.empty params in
		    type_block t env block
 

let type_prog l =
  List.iter (type_decl) l
