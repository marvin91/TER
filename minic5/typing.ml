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
  else true

let rec type_eq t1 t2 =
  match t1, t2 with
  | Tnull, Tnull
  | Tvoid, Tvoid
  | Tdouble, Tdouble -> true
  | Tnum(s1, k1), Tnum(s2, k2) -> s1 = s2 && k1 = k2
  | Tstruct id1, Tstruct id2 -> id1.node = id2.node
  | Tpointer p1, Tpointer p2 -> type_eq p1 p2
  | _ -> false

let mk_cast t e =
  if not (compatible t e.info) then assert false
  else
    mk_node t (Ecast(t, e))

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
        error id.info "Champ ou variable mal typé"
      else
        if t = Tvoid then
          error id.info "Champ ou variable ne peut etre de type void"
        else
          if List.mem id.node seen then
            error id.info "Champ ou variable existe deja"
          else
            id.node :: seen
    ) [] vd
  in
  vd

let add_env env vd =
  List.fold_left (fun acc (t, id) -> Env.add id.node t acc) env vd

let type_const c =
  match c with
  | Cdouble _ -> Tdouble
  | Cint (Signed, Int, 0) -> Tnull
  | Cint (s, i, _) -> Tnum(s, i)
(*
  | Ccar c -> Tnum (Signed, Char)
  | Cchaine lc -> Tnum (Signed, Char) *)

let rec type_expr env e =
  match e.node with
  | Econst c ->
    let tc = type_const c in
    mk_node tc (Econst c)

  | Ecast (t, e1) ->
    let te1 = type_expr env e1 in
    if type_bf t then
      if num t && num te1.info then
	mk_node t (Ecast (t, te1))
      else error e1.info
	("Le type indiqué et le type de l'expression doivent être numériques ")
    else error e1.info ("Le type indiqué est mal formé ")

  | Esizeof t ->
    if type_bf t then
      if t <> Tvoid then
	mk_node (Tnum(Unsigned, Long)) (Esizeof t)
      else error e.info ("Sizeof ne prend pas le type void ")
    else error e.info ("Type indiqué mal formé ")

  | Eassign (e1,e2) ->
    let t1 = type_lvalue env e1
    and t2 = type_expr env e2 in
    if compatible (t1.info) (t2.info) then
      mk_node t1.info (Eassign (t1, mk_cast t1.info t2))
    else error e.info ("L'assignation necessite 2 types compatibles ")
      
  | Eunop (unop, e1) ->
    begin
      match unop with
      | PreIncr | PreDecr | PostIncr | PostDecr ->
        let te1 = type_lvalue env e1 in
        if num te1.info then
          mk_node te1.info (Eunop (unop, te1))
        else error e1.info
          ("L'expression doit être de type numérique ")


      | Not ->
        let te1 = type_expr env e1 in
        if num te1.info then
          mk_node type_int (Eunop (unop, te1))
        else error e1.info ("L'expression doit être de type numérique ")


      | Posit | Uminus ->
        let te1 = type_expr env e1 in
        if arith te1.info then
          mk_node te1.info (Eunop (unop, te1))
        else error e1.info
          ("L'expression doit être de type arithmétique ")


      | Adr ->
        let te1 = type_lvalue env e1 in
        if type_bf te1.info then
          mk_node (Tpointer te1.info) (Eunop (unop, te1))
        else error e1.info ("Type de l'expression mal formé ")

      | Deref -> type_lvalue env e
    end

  | Ebinop (e1, binop, e2) ->
    let te1 = type_expr env e1 in
    let te2 = type_expr env e2 in
    let t1 = te1.info in
    let t2 = te2.info in
    let nte1, nte2 =
      if arith t1 && arith t2 && not (type_eq t1 t2) then
	if t1 = Tdouble then te1, mk_cast Tdouble te2
	else if t2 = Tdouble then mk_cast Tdouble te1, te2
	else
	  let te1 = if inf_type t1 type_int then mk_cast type_int te1 else te1 in
	  let te2 = if inf_type t2 type_int then mk_cast type_int te2 else te2 in
	  let t1 = te1.info in
	  let t2 = te2.info in
	  if type_eq t1 (Tnum(Unsigned, Long)) then te1, mk_cast (Tnum(Unsigned, Long)) te2
	  else
	    if type_eq t2 (Tnum(Unsigned, Long)) then mk_cast (Tnum(Unsigned, Long)) te1, te2
	    else
	      if type_eq t1 (Tnum(Signed, Long)) then te1, mk_cast (Tnum(Signed, Long)) te2
	      else
		if type_eq t2 (Tnum(Signed, Long)) then mk_cast (Tnum(Signed, Long)) te1, te2
		else
		  if type_eq t1 (Tnum(Unsigned, Int)) then te1, mk_cast (Tnum(Unsigned, Int)) te2
		  else
		    if type_eq t2 (Tnum(Unsigned, Int)) then mk_cast (Tnum(Unsigned, Int)) te1, te2
		    else te1, te2
	else te1, te2
    in
    let te1 = nte1 in
    let te2 = nte2 in
    begin
      match binop with
      | Eq | Neq | Lt | Le | Gt | Ge ->
        if num te1.info then
          if compatible te1.info te2.info then
            mk_node type_int (Ebinop (te1, binop, te2))
          else error e1.info
            ("La comparaison nécéssite 2 types compatibles ")
        else error e1.info
          ("Le type de cette expression doit être numérique ")
      | Plus | Minus | Mult | Div ->
        begin
          match binop, te1.info, te2.info with
          | (Plus | Minus), Tpointer tt1, tp2 ->
            if binop = Minus && tp2 = Tpointer tt1 then
              mk_node (Tnum(Signed, Long)) (Ebinop (te1, binop, te2))
            else
              if rank (max_type te2.info type_int) < 65 then
                mk_node (Tpointer tt1) (Ebinop (te1, binop, te2))
              else error e2.info
                ("Taille du type de l'expression trop grande (>unsigned long)")
          | _ ->
            if compatible te1.info Tdouble
              && compatible te1.info te2.info then
              mk_node (max_type te1.info te2.info) (Ebinop (te1, binop, te2))
            else error e1.info ("L'opération nécéssite 2 types compatibles ")
        end


      | And | Or ->
        if compatible te1.info Tdouble
          && compatible te1.info te2.info then
          mk_node type_int (Ebinop (te1, binop, te2))
        else error e1.info
          ("L'opération logique nécéssite 2 types compatibles ")


      | Modulo ->
        if rank (max_type te1.info te2.info) < 65 then
          mk_node (max_type te1.info te2.info) (Ebinop (te1, binop, te2))
        else error e1.info
          ("Types des expressions trop longs (>unsigned long) ")
    end


  | Ecall (f, l) ->
    if Hashtbl.mem fun_env f.node then
      let t, params, _ = Hashtbl.find fun_env f.node
      and t_l = List.map (type_expr env) l in
      let rec checkpl parl lex count acc  =
        match parl, lex with
        | [], [] -> acc
        | [], _ ->
          error e.info ("Trop d'arguments pour cette fonction " ^ f.node)
        | _, [] ->
          error e.info ("Il manque des arguments à cet fonction " ^ f.node) 
        | (h1::t1, h2::t2) ->
          let (tpar, ipar) = h1 in
          if compatible tpar h2.info then
	    checkpl t1 t2 (count+1) (acc @ [(mk_cast tpar h2)])
          else error e.info
            ("Le " ^ string_of_int count
	     ^ "ème argument n'est pas compatible avec l'argument de la fonction "
	     ^ f.node)
      in
      mk_node t (Ecall (f, checkpl params t_l 1 []))
	
    else error f.info ("La fonction \"" ^ f.node ^ "\" n'est pas déclarée")

  | Eacces (e1, id) ->
    type_eaccess type_expr env e1 id 

  | _ -> type_lvalue env e


and type_lvalue env e =
  match e.node with
  | Eident id ->
    let t =
      try
        try
          Env.find id.node env
        with
          Not_found ->
	    try
              Hashtbl.find global_env id.node
	    with
	      Not_found ->
		if Hashtbl.mem struct_env id.node then
		  Tstruct id
		else raise Not_found
      with
        Not_found ->
          error id.info ("Variable " ^ id.node ^ " non declaree")
    in
    if type_bf t then
      mk_node t (Eident id)
    else
      error id.info ("Type de variable mal formé " ^ id.node)

  | Eunop (Deref, e1) ->
    let te1 = type_expr env e1 in
    begin
      match te1.info with
        Tpointer tt -> mk_node tt (Eunop (Deref, te1))
      | _ -> error e1.info ("Cette expression n'est pas un pointeur ")
    end

  | Eacces (e1, id) ->
    type_eaccess type_lvalue env e1 id

  | _ -> error e.info "Valeur gauche attendue"

and type_eaccess f_type env e1 id =
  let te1 = f_type env e1 in
  begin
    match te1.info with
    | Tstruct id1 ->
      if Hashtbl.mem struct_env id1.node then
        let l = Hashtbl.find struct_env id1.node in
        let rec plist sl sid =
          match sl with
          | (t, var)::tl ->
            if sid.node = var.node then
              mk_node t (Eacces (te1, id))
            else plist tl sid
          | [] -> error id.info
            ("La variable \"" ^ id.node ^ "\" n'existe pas dans la structure \""
	     ^ id1.node ^ "\"")
        in plist l id
      else error id1.info ("Cette structure \"" ^ id1.node ^ "\" n'est pas déclarée : ")
    | _ -> error e1.info ("Cette expression n'est pas une structure ")
  end    

let rec type_instr t env ins =
  match ins.node with
  | Sskip -> mk_node Tvoid Sskip


  | Sexpr e -> let te = type_expr env e in
               begin
                 match e.node with
                 | Ebinop (e1, Plus, e2) ->
                   let tinv = type_expr env {info = e.info; node = (Ebinop(e2, Plus, e1))} in
                   if tinv.info = te.info then
                     mk_node te.info (Sexpr te)
                   else error e.info ("L'addition doit être reversible")
                 | _ -> mk_node te.info (Sexpr te)
               end

  | Sreturn e ->
    begin
      match e with
      | None ->
        if t <> Tvoid then
          error ins.info ("Le type de retour ne peut être void")
        else mk_node t (Sreturn None)

      | Some e ->
        let te = type_expr env e in
        if compatible te.info t then
          mk_node te.info (Sreturn (Some te))
        else error e.info ("Le type de retour est incorrect")
    end

  | Sif (e, i1, i2) ->
    let te = type_expr env e in
    if num te.info then
      let ti1 = type_instr t env i1
      and ti2 = type_instr t env i2 in
      mk_node ti1.info (Sif (te, ti1, ti2))
    else error e.info ("L'expression doit être de type numérique ")

  | Sfor (l1, e, l2, ins) ->
    let te = type_expr env e
    and tl1 = if (l1 <> []) then List.map (type_expr env) l1
      else []
    and tl2 = if (l2 <> []) then List.map (type_expr env) l2
      else []
    and tins = type_instr t env ins in
    if num te.info then
      mk_node tins.info (Sfor (tl1, te, tl2, tins))
    else error e.info ("L'expression doit être de type numérique ")

  | Sbloc b ->
    let tb = type_block t env b in
    mk_node t (Sbloc tb)
      

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
        Hashtbl.add global_env id.node t;
        Dvar(t, id);
      end
    else error id.info ("Déclaration globale invalide" ^ id.node)


  | Dstruct (id, var_decl) ->
    if not (Hashtbl.mem struct_env id.node) then
      begin
        Hashtbl.add struct_env id.node var_decl;
        let t_var_decl = check_var_decl var_decl in
        Dstruct (id, t_var_decl);
      end
    else error id.info ("Structure déjà déclarée " ^ id.node)


  | Dfun (t, f, params, b) ->
    if not (type_bf t) then
      error f.info ("Le type de \""^ f.node ^ "\" est mal formé")
    else
      begin
        if Hashtbl.mem fun_env f.node then
          error f.info ("Redéfinition de la fonction " ^ f.node);
        let t_params = check_var_decl params in
        Hashtbl.add fun_env f.node (t, t_params, b = None);
        let t_block =
          match b with
            None -> None
          | Some block ->
            let benv = add_env Env.empty params in
            let tblock = type_block t benv block in
            Some tblock
        in
        Dfun(t, f, t_params, t_block)
      end


let type_prog prog =
  let rec intmain pl =
    match pl with
    | hd::tl ->
      begin
        match hd with
        | Dfun (Tnum (Signed, Int), id, _, _) ->
          if id.node = "main" then true
          else intmain tl
        | _ -> intmain tl
      end
    | [] -> false
  in
  if intmain prog then
    List.map (type_decl) prog
  else
    let progloc = Lexing.dummy_pos in
    error (progloc, progloc) ("Le fichier doit comporter une fonction \"int main()\"")
