open Ast

module Env = Map.Make(String)

let global_env = Hashtbl.create 17
let struct_env : (string, var_decl list) Hashtbl.t = Hashtbl.create 17
let fun_env = Hashtbl.create 17

let mk_node t e = { info = t; node = e }

let type_int = Tnum(Signed, Int)

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

let check_var_decl vd =
  let _ =
    List.fold_left (fun seen (t, id) ->
      if not (type_bf t) then
	error id.info "Champ ou variable incorrect"
      else
	if t = Tvoid then
	  error id.info "Champ ou variable incorrect"
	else
	  if List.mem id.node seen then
	    error id.info "Champ ou variable incorrect"
	  else
	    id.node :: seen
    ) [] vd
  in
  vd

let add_env env vd =
  List.fold_left (fun acc (t, id) -> Env.add id.node t acc) env

let type_const c =
  match c with
  | Cstring _ -> Tpointer (Tnum (Signed, Char))
  | Cdouble _ -> Tdouble
  | Cint (Signed, Int, 0) -> Tnull
  | Cint (s, i, _) -> Tnum(s, i)

let rec type_expr env e =
  match e.node with
  | Econst c ->
    let tc = type_const c in mk_node tc (Econst c)
 
  | Etype (t, e1) ->
    let te1 = type_expr env e1 in
    if type_bf te1.info then
      if num t && num te1.info then mk_node t (Etype (t, te1))
      else error e1.info ("Le type de cette expression est de taille inférieur au type indiqué ")
    else error e1.info ("Le type de cette expression est mal formé ")
      
  | Eident id ->
     if Hashtbl.mem global_env id.node then
       Hashtbl.find global_env id.node
     else error id.info ("Variable non déclarée " ^ id.node)
      
  | Esizeof t ->
    if type_bf t && (t <> Tvoid) then mk_node (Tnum(Unsigned, Long)) (Esizeof t)
     else error e.info ("Le type est mal formé ou de type void ")
       
  | Eassign (e1,e2) ->
     let t1 = type_expr env e1
     and t2 = type_expr env e2 in
     if compatible (t1.info) (t2.info)
       && type_bf t1.info && type_bf t2.info then
       mk_node (max_type t1.info t2.info) (Eassign (t1, t2))
     else error e1.info ("L'assignation nécéssite 2 types compatibles ")

  | Eunop (unop, e1) ->
    let te1 = type_expr env e1 in
     if type_bf te1.info then
       begin
	 match unop with
	 | PreIncr | PreDecr | PostIncr | PostDecr ->
	    if num te1.info then mk_node te1.info (Eunop (unop, te1))
	    else error e1.info ("La valeur à incrémenter ou décrémenter doit être de type numérique ")
	      
	 | Not ->
	    if num te1.info then
	      mk_node type_int (Eunop (unop, te1))
	    else error e1.info ("La valeur à inverser doit être de type numérique ")
	      
	 | Posit | Uminus ->
	    if arith te1.info then mk_node te1.info (Eunop (unop, te1))
	    else error e1.info ("Une valeur positive ou négative doit être de type arithmétique ")
	      
	 | Adr -> mk_node (Tpointer te1.info) (Eunop (unop, te1))
	   
	 | Deref ->
	    match te1.info with
	      Tpointer tt -> mk_node tt (Eunop (unop, te1))
	    | _ -> error e1.info ("Cette expression n'est pas un pointeur ")
       end
     else error e1.info ("Type de l'expression mal formé ")

  | Ebinop (e1, binop, e2) ->
    let te1 = type_expr env e1
    and te2 = type_expr env e2 in
     if type_bf te1.info && type_bf te2.info then
	 begin
	   match binop with
	   | Eq | Neq | Lt | Le | Gt | Ge ->
	     if num te1.info && compatible te1.info te2.info then
	       mk_node type_int (Ebinop (te1, binop, te2))
	      else error e1.info ("La comparaison nécéssite 2 types compatibles ")
		
	   | Plus ->
	      begin
		match te1.info with
		| Tpointer _ ->
		  if rank (max_type te2.info type_int) < 65
		    && compatible te1.info te2.info then mk_node te1.info (Ebinop (te1, binop, te2))
		   else error e1.info ("L'addition nécéssite 2 types compatibles " ^
				   "et de longueur inférieur à un unsigned long")
		| _ ->
		  if compatible te1.info Tdouble && compatible te1.info te2.info then
		    mk_node (max_type te1.info te2.info) (Ebinop (te1, binop, te2))
		   else error e1.info ("L'opération nécéssite 2 types compatibles ")
	      end
		
	   | Minus ->
	      begin
		match te1.info, te2.info with
		| Tpointer _, _ ->
		   if te1.info = te2.info then mk_node (Tnum(Signed, Long)) (Ebinop (te1, binop, te2))
		   else
		     if rank (max_type te2.info type_int) < 65 then
		       mk_node te1.info (Ebinop (te1, binop, te2))
		     else error e1.info ("L'opération nécéssite 2 types compatibles " ^
				   "et de longueur inférieur à un unsigned long ")
		| _ ->
		  if compatible te1.info Tdouble && compatible te1.info te2.info then
		    mk_node (max_type te1.info te2.info) (Ebinop (te1, binop, te2))
		   else error e1.info ("L'opération nécéssite 2 types compatibles ")
	      end
		
	   | Mult | Div ->
	     if compatible te1.info Tdouble && compatible te1.info te2.info then
	       mk_node (max_type te1.info te2.info) (Ebinop (te1, binop, te2))
	     else error e1.info ("L'opération nécéssite 2 types compatibles ")
	       
	   | And | Or ->
	     if compatible te1.info Tdouble && compatible te1.info te2.info then
	       mk_node type_int (Ebinop (te1, binop, te2))
	     else error e1.info ("L'opération logique nécéssite 2 types compatibles ")
	       
	   | Modulo ->
	     if compatible te1.info te2.info && rank (max_type te1.info te2.info) < 65 then
	       mk_node (max_type te1.info te2.info) (Ebinop (te1, binop, te2))
	      else error e1.info ("L'opération modulo nécéssite 2 types compatibles "
			      ^ "et de longueurs inférieures à celle du type unsigned long ")
	 end
     else error e1.info ("Type de l'expression mal formé ")

  | Ecall (f, l) ->
     if Hashtbl.mem fun_env f.node then
       let t, params = Hashtbl.find fun_env f.node
       and t_l = List.map (type_expr env) l in
       try
	 begin
	   List.iter2 ( fun (t, param) e->
	     if not (compatible t e.info) then
	       error f.info ("Un argument n'est pas compatible avec la fonction déclarée " ^ f.node))
	     params t_l;
	   mk_node t (Ecall (f, t_l));
	 end
       with
	 Invalid_argument _ -> error f.info ("Il manque un ou plusieurs arguments" ^ f.node)
     else error f.info ("Cette fonction n'est pas déclarée " ^ f.node)

  | Eacces (e1, id) ->
    let te1 = type_expr env e1 in
    if type_bf te1.info then
      match te1.info with
      | Tstruct id1 ->
	if Hashtbl.mem struct_env id1.node then
	  let l = Hashtbl.find struct_env id1.node in
	  let rec plist sl sid =
	    match sl with
	    | (t, var)::tl ->
	      if sid.node = var.node then mk_node t (Eacces (te1, id))
	      else plist tl sid
	    | _ -> error sid.info ("Cette variable n'existe pas dans la structure " ^ sid.node)
	  in plist l id
	else error id1.info ("Cette structure n'est pas déclarée " ^ id1.node)
      | _ -> error e1.info ("Cette expression n'est pas une structure ")
    else error e1.info ("Type de l'expression mal formé ")

let type_lvalue env e =
  match e.node with
  | Eident id ->
    let t =
      try
	try
	  Env.find id.node env
	with
	  Not_found ->
	    Hashtbl.find global_env id.node
      with
	Not_found -> error id.info ("Variable non déclarée " ^ id.node)
    in
    mk_node t (Eident id)
      
  | _ -> error e.info "Valeur gauche attendue"
	
let rec type_instr t env ins =
  match ins.node with
  | Sskip -> mk_node Tvoid Sskip
  | Sexpr e -> let te = type_expr env e in
	       mk_node te.info (Sexpr te)
  | _ -> assert false
    

and type_block t env (var_decl, instrs) =
  (check_var_decl var_decl,
   let new_env = add_env env var_decl in
   List.map (type_instr t new_env) instrs)
    
    
let type_decl d =
  match d with
  | Dvar (t, id) ->
    if type_bf t && t <> Tvoid
      && not (Hashtbl.mem global_env id.node) then
      begin
	Hashtbl.add global_env (id.node) t;
	Dvar(t, id);
      end
    else error i.info "Déclaration globale invalide"
      
  | Dstruct (id, var_decl) ->
    if not (Hashtbl.mem struct_env id.node) then
      begin
	Hashtbl.add struct_env id.node var_decl;
        let t_var_decl = check_var_decl var_decl in
	Dstruct (id, t_var_decl);
      end
    else error id.info ("Structure déjà déclarée " ^ id.node)
      
  | Dfun (t, f, params, b) ->
    if not (type_bf t) then error f.info ("Type de retour invalide pour " ^f.node)
    else
      begin
	if Hashtbl.mem fun_env f.node then
	  error f.info ("Redéfinition de la fonction " ^ f.node);
	let t_params = check_var_decl params in
	Hashtbl.add fun_env f.node (t, t_params);
	let t_block =
	  match b with
	    None -> None
	  | Some block -> let env = add_env Env.empty params in
			  let tblock = type_block t env block in
			  Some tblock
	in
	Dfun(t, f, t_params, t_block)
      end

let type_prog prog =
  List.map (type_decl) prog
