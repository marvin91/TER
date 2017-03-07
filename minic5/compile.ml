open Ast
open Amd64
module Env = Map.Make(String)


let size_of t =
  match t with
  | Tvoid | Tnull -> assert false
  | Tnum (_, Char) -> 1
  | Tnum (_, Short) -> 2
  | Tnum (_, Int) -> 4
  | Tnum (_, Long) | Tdouble | Tpointer _ -> 8
  | Tstruct _ -> assert false

let align_of t =
  match t with
  | Tstruct _ -> assert false
  | _ -> size_of t

let compile_expr env e =
  match e.node with
  | Econst c -> assert false
    (* let n = size_of e.info in
       match e.info with *)
       
  | _ -> assert false
     

let rec compile_instr env ins =
  match ins.node with
  | Sif (e, i1, i2) ->
     let ce = compile_expr env e in
     let ci1 = compile_instr env i1 in
     let ci2 = compile_instr env i2 in
     ce ++
       jns "else" ++
       ci1 ++
       jmp "endif" ++
       label "else" ++
       ci2 ++
       label "endif"

  | Sreturn eopt ->
     begin
       match eopt with
       | None -> nop
       | Some e -> compile_expr env e
     end
       
  | _ -> assert false

and compile_block lab_fln rsp env (var_decl, instrs) =
  (List.length var_decl * rsp, List.fold_left ( fun acc i ->
    acc ++ compile_instr env i) nop instrs )
    
let compile_decl (atext, adata) d =
  match d with
  | Dstruct _ -> atext, adata
  | Dvar (t, id) ->
    atext,
    let n = size_of t in
    let a = align_of t in
    adata ++
      label id.node ++ 
      align a ++
      space n

  | Dfun (_, _, _, None) -> atext, adata

  | Dfun (tret, f, params, Some body) ->
    let env = Env.empty in
    let lab_fln = f.node ^ "_fln" in
    (* max_rbp_offset est negatif *)
    let max_rbp_offset, body_code = compile_block lab_fln (-8) env body in
    let code =
      glabel f.node ++
	comment ("On rentre dans la fonction " ^ f.node) ++
	pushq ~%rbp ++
	mov ~%rsp ~%rbp ++
	addq ~$max_rbp_offset ~%rsp ++
	
	body_code ++
	subq ~$max_rbp_offset ~%rsp ++
	
	mov ~%rbp ~%rsp ++
	popq ~%rbp ++
	ret		
    in
    atext ++ code, adata
      
    
let compile_prog p =
  let text, data =
    List.fold_left compile_decl (nop, nop) p
  in
  
  {
    text = text;

    data = data
      
  }
