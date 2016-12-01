
open Ast

exception TODO (* to be used for actions remaining to be done *)
exception Error of string (* to be used for semantic errors *)

(* global context, main module, and builder for generating code *)

let context = Llvm.global_context ()
let the_module = Llvm.create_module context "main"
let builder = Llvm.builder context

(* LLVM types for VSL+ *)

let int_type = Llvm.i32_type context
let void_type = Llvm.void_type context
let char_type = Llvm.i8_type context
let string_type = Llvm.pointer_type char_type
let int_array_type = Llvm.array_type int_type 0

(* generation of constant integer LLVM values *)

let const_int n = Llvm.const_int int_type n

let zero_int = const_int 0

(* generation of constant string LLVM values *)

let const_string =
  let string_gep_indices = [|zero_int; zero_int|] in
  fun s ->
    let const_s = Llvm.const_stringz context s in
    let global_s = Llvm.define_global s const_s the_module in
    Llvm.const_gep global_s string_gep_indices

(* the printf function as a LLVM value *)

let func_printf =
  let tf = Llvm.var_arg_function_type int_type [|string_type|] in
  let f = Llvm.declare_function "printf" tf the_module in
  Llvm.add_function_attr f Llvm.Attribute.Nounwind;
  Llvm.add_param_attr (Llvm.param f 0) Llvm.Attribute.Nocapture;
  f

(* the scanf function as a LLVM value *)

let func_scanf =
  let tf = Llvm.var_arg_function_type int_type [|string_type|] in
  let f = Llvm.declare_function "scanf" tf the_module in
  Llvm.add_function_attr f Llvm.Attribute.Nounwind;
  Llvm.add_param_attr (Llvm.param f 0) Llvm.Attribute.Nocapture;
  f

(* Create an alloca instruction in the entry block of the
function. This is used for mutable local variables. *)

let create_entry_block_alloca the_function var_name typ =
  let builder = Llvm.builder_at context (Llvm.instr_begin (Llvm.entry_block the_function)) in
  Llvm.build_alloca typ var_name builder

let create_entry_block_array_alloca the_function var_name typ size =
  let builder = Llvm.builder_at context (Llvm.instr_begin (Llvm.entry_block the_function)) in
  let vsize = Llvm.const_int int_type size in
  Llvm.build_array_alloca typ vsize var_name builder

(* generation of code for each VSL+ construct *)

let rec gen_expression : expression -> Llvm.llvalue = function
  | Const n ->
     const_int n
  (* returns a constant llvalue for that integer *)
  | Plus (e1,e2) ->
     let t1 = gen_expression e1 in
        (* generates the code for [e1] and returns the result llvalue *)
     let t2 = gen_expression e2 in
        (* the same for e2 *)
     Llvm.build_add t1 t2 "plus" builder
  (* appends an 'add' instruction and returns the result llvalue *)
  | Minus (e1,e2) -> 
     let t1 = gen_expression e1 in
     let t2 = gen_expression e2 in
     Llvm.build_sub t1 t2 "minus" builder
  | Mul (e1,e2) ->
     let t1 = gen_expression e1 in
      let t2 = gen_expression e2 in
      Llvm.build_mul t1 t2 "mul" builder
  | Div (e1,e2) -> 
     let t1 = gen_expression e1 in
     let t2 = gen_expression e2 in
     Llvm.build_sdiv t1 t2 "div" builder
  | Expr_Ident(id) ->  SymbolTableList.lookup id
  | ArrayElem (id,e) -> raise TODO
  | ECall (id,array) -> let fn = SymbolTableList.lookup id in
			let args = Array.map (gen_expression) array in
			Llvm.build_call fn args  id builder

let gen_decl_item di the_function: unit =
  match di with
  | Dec_Ident (id) -> SymbolTableList.add id (create_entry_block_alloca the_function id int_type);
  | Dec_Array (id, n) -> raise TODO
					
let rec gen_decl decl f: unit =
  match decl with
  | [] -> ()
  | di :: decl' -> gen_decl_item di f; gen_decl decl' f
					
let rec gen_statement (f:Llvm.llvalue) (s:statement): unit  = 
    match s with 
    | Assign (l,e) ->  begin match l with	      
			     | LHS_Ident (id) -> ignore (Llvm.build_store (gen_expression e) (SymbolTableList.lookup id) builder)
			     | LHS_ArrayElem (id,expr) -> raise TODO
		       end
    | Return (expr) ->
       let l = gen_expression expr in
       (* store dans le registre return *)
       ignore (Llvm.build_ret l builder)
    | SCall (id, exprarray) -> raise TODO
    | Print (itemlist) -> raise TODO
    | Read (itemlist)  -> raise TODO
    | Block (decl, statementlist) ->
     SymbolTableList.open_scope();
     gen_decl decl f;
     List.iter (gen_statement f) statementlist;
     SymbolTableList.close_scope()
    | If (expr, stmt, stmtoption) ->
       let l = Llvm.build_icmp (Llvm.Icmp.Ne) (gen_expression expr) (const_int 0) "icmp" builder in
       begin match stmtoption with
	     | None ->
		       let tbb = Llvm.append_block context "then" f in
		       let endbb = Llvm.append_block context "fi" f in
		       ignore ( Llvm.build_cond_br (l) tbb endbb builder );
		       Llvm.position_at_end tbb builder;
		       gen_statement f stmt;
		       ignore (Llvm.build_br endbb builder);
		       Llvm.position_at_end endbb builder
	     | Some s -> 
		let tbb = Llvm.append_block context "then" f in
		let fbb = Llvm.append_block context "else" f in
		let endbb = Llvm.append_block context "fi" f in
		ignore ( Llvm.build_cond_br (l) tbb fbb builder );
		Llvm.position_at_end tbb builder;
		gen_statement f stmt;
		ignore (Llvm.build_br endbb builder);
		Llvm.position_at_end fbb builder;  gen_statement f s; ignore (Llvm.build_br endbb builder);
		Llvm.position_at_end endbb builder
       end  
       
    | While (expr,stmt) ->
       let l = Llvm.build_icmp (Llvm.Icmp.Ne) (gen_expression expr) (const_int 0) "icmp" builder in

       let loop  = Llvm.append_block context "loop" f in
       let body  = Llvm.append_block context "body" f in
       let after = Llvm.append_block context "after" f in

       ignore ( Llvm.build_br loop builder);

       (* test to go in the body or after *)
       Llvm.position_at_end loop builder;
       ignore ( Llvm.build_cond_br (l) body after builder );

       (* body of the while loop *)
       Llvm.position_at_end body builder;
       gen_statement f stmt;
       ignore ( Llvm.build_br loop builder);

       Llvm.position_at_end after builder

let gen_proto (p:proto) (is_def:bool) : Llvm.llvalue =
  match p with
  | (p_typ, p_ident, p_paramarray) ->
     let return_type = begin match p_typ with
			   | Type_Int -> int_type
			   | Type_Void -> void_type
		       end in
     let arg_type = Array.make (Array.length p_paramarray) (int_type) in (* eventually, we should be able to have other types as arguments *)
     let fun_type = Llvm.function_type return_type arg_type in
     let f = begin match Llvm.lookup_function p_ident the_module with
	   | None -> Llvm.declare_function p_ident fun_type the_module 
	   | Some f ->
	      (* verification : no re-definition *)
	      if (Array.length (Llvm.basic_blocks f) != 0) then raise (Error "re-definition of function");
	      if (Array.length (Llvm.params f) != Array.length p_paramarray) then raise (Error "redefinition with wrong number of arguments"); f
     end in
     if (is_def) then 
	 Array.iteri  (fun i a -> let n = p_paramarray.(i) in
				  Llvm.set_value_name n a;
				  SymbolTableList.add n a)
		      (Llvm.params f);
     f
       

let gen_program_unit (u : program_unit) =
  match u with
  | Proto(p) -> ignore(gen_proto p false)
  | Function(p,s) -> SymbolTableList.open_scope();
		     (* créer registre return *)
		     let f = gen_proto p true in
		     let entrybb = Llvm.append_block context "entry" f in
		     Llvm.position_at_end entrybb builder;
		     gen_statement f s ;
		     (* return : build_ret du registre return ou return void*)
		     SymbolTableList.close_scope()

let rec gen_program (p : program) =
  List.iter (gen_program_unit) p

(* function that turns the code generated for an expression into a valid LLVM code *)
(*let gen (s : statement) : unit =
  let the_function = Llvm.declare_function "main" (Llvm.function_type int_type [||]) the_module in
  let bb = Llvm.append_block context "entry" the_function in
  Llvm.position_at_end bb builder;
  SymbolTableList.open_scope();
  (* SymbolTableList.add "i" (create_entry_block_alloca the_function "i" int_type);*)
  gen_statement the_function s;
  (* ignore(Llvm.build_ret (const_int 0) builder) (* returns 0 *) *)
  (* let x = gen_statement s in
  ignore (Llvm.build_ret x builder) *) (* for expressions that returned llvalues *)
 *)
let gen (p : program) = gen_program p
				    
