
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


let rec expression_type (e:expression) : SymbolTableList.symbType =
  match e with
  | Plus(e1,e2) | Minus(e1,e2) | Mul(e1,e2) | Div(e1,e2) ->
      if (expression_type e1 = SymbolTableList.SymbArray ||
	    expression_type e2 = SymbolTableList.SymbArray)
      then
	raise (Error "bad use of Array in expression")
      else SymbolTableList.SymbInt
  | Const(_) -> SymbolTableList.SymbInt
  | Expr_Ident(i) -> SymbolTableList.lookup_type i
  | ArrayElem(i,e) -> if (SymbolTableList.lookup_type i = SymbolTableList.SymbInt ||
			    expression_type e = SymbolTableList.SymbArray)
		      then raise (Error "bad types in Array element access")
		      else SymbolTableList.SymbInt
  | ECall(i,expr_array) -> SymbolTableList.SymbInt (* TODO : see if it's void *)

			  
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
  | Expr_Ident(id) ->  Llvm.build_load (SymbolTableList.lookup id) (id^"_loaded") builder
  | ArrayElem (id,e) -> let arrayptr = SymbolTableList.lookup id in
			let index = Array.make 1 (gen_expression e) in
			let elem_ptr = Llvm.build_gep arrayptr index "array_elem" builder in
			Llvm.build_load elem_ptr id builder
  | ECall (id,array) -> let fn = Llvm.lookup_function id the_module in
			let args = Array.map (gen_expression) array in
			begin match fn with
			      | None -> raise (Error ("Unknown function "^id))
			      | Some f -> Llvm.build_call f args "ecall" builder
			end
		
let gen_decl_item di the_function: unit =
  match di with
  | Dec_Ident (id) -> SymbolTableList.add id (create_entry_block_alloca the_function id int_type) SymbolTableList.SymbInt
  | Dec_Array (id, n) ->  SymbolTableList.add id (create_entry_block_array_alloca the_function id int_type n) SymbolTableList.SymbArray
					
let rec gen_decl decl f: unit =
  match decl with
  | [] -> ()
  | di :: decl' -> gen_decl_item di f; gen_decl decl' f

type ret_type = INT | VOID
						
let rec gen_statement (f:Llvm.llvalue) (s:statement) (ret:ret_type): unit  = 
    match s with 
    | Assign (l,e) ->  begin match l with	      
			     | LHS_Ident (id) -> ignore (Llvm.build_store (gen_expression e) (SymbolTableList.lookup id) builder)
			     | LHS_ArrayElem (id,expr) -> 
				let array_ptr = SymbolTableList.lookup id in
				let index = Array.make 1 (gen_expression expr) in
				let array_elem_ptr = Llvm.build_gep array_ptr index "array_elem" builder in
				ignore (Llvm.build_store (gen_expression e) array_elem_ptr builder) 
		       end
    | Return (expr) ->
       begin match ret with
	     | VOID -> ignore(Llvm.build_ret_void builder)
	     | INT -> ignore(Llvm.build_ret (gen_expression expr) builder)
       end
    | SCall (id, array) -> let fn = Llvm.lookup_function id the_module in
			       let args = Array.map (gen_expression) array in
			       begin match fn with
				     | None -> raise (Error ("Unknown function "^id))
				     | Some f -> ignore(Llvm.build_call f args "" builder) 
			       end
    | Print (itemlist) ->
       List.iter
	 ( fun i -> let args =
		      begin match i with
			    | Print_Expr e -> [|const_string "%d"; (gen_expression e)|]
			    | Print_Text s -> Array.make 1 (const_string s)
		      end in
		    ignore (Llvm.build_call func_printf args "print" builder ) )
	 itemlist
	 
    | Read (itemlist)  ->
       List.iter
	 ( fun i -> let args =
		      begin match i with
			    | LHS_Ident i -> [|const_string "%d"; (SymbolTableList.lookup i)|]
			    | LHS_ArrayElem (i,e) ->
			       let array = SymbolTableList.lookup i in
			       let index = gen_expression e in
			       let elem_ptr = Llvm.build_gep array [|index|] i builder
			       in
			       [|const_string "%d"; elem_ptr|] 
		      end in
		    ignore (Llvm.build_call func_scanf args "read" builder ) )
	 itemlist
			   
    | Block (decl, statementlist) ->
     SymbolTableList.open_scope();
     gen_decl decl f;
     List.iter (fun s -> gen_statement f s ret) statementlist;
     SymbolTableList.close_scope()
    | If (expr, stmt, stmtoption) ->
       let l = Llvm.build_icmp (Llvm.Icmp.Ne) (gen_expression expr) (const_int 0) "icmp" builder in
       begin match stmtoption with
	     | None ->
		let tbb = Llvm.append_block context "then" f in
		let endbb = Llvm.append_block context "fi" f in
		ignore ( Llvm.build_cond_br (l) tbb endbb builder );
		Llvm.position_at_end tbb builder;
		gen_statement f stmt ret;
		ignore (Llvm.build_br endbb builder);
		Llvm.position_at_end endbb builder
	     | Some s -> 
		let tbb = Llvm.append_block context "then" f in
		let fbb = Llvm.append_block context "else" f in
		let endbb = Llvm.append_block context "fi" f in
		ignore ( Llvm.build_cond_br (l) tbb fbb builder );
		Llvm.position_at_end tbb builder;
		gen_statement f stmt ret;
		ignore (Llvm.build_br endbb builder);
		Llvm.position_at_end fbb builder;  gen_statement f s ret; ignore (Llvm.build_br endbb builder);
		Llvm.position_at_end endbb builder
       end  
       
    | While (expr,stmt) ->
       
       let loop  = Llvm.append_block context "loop" f in
       let body  = Llvm.append_block context "body" f in
       let after = Llvm.append_block context "after" f in

       ignore ( Llvm.build_br loop builder);

       (* test to go in the body or after *)
       Llvm.position_at_end loop builder;
       let l = Llvm.build_icmp (Llvm.Icmp.Ne) (gen_expression expr) (const_int 0) "icmp" builder in
       ignore ( Llvm.build_cond_br (l) body after builder );

       (* body of the while loop *)
       Llvm.position_at_end body builder;
       gen_statement f stmt ret;
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
     end in f       

       
let gen_program_unit (u : program_unit) =
  match u with
  | Proto(p) -> ignore(gen_proto p false)
  | Function(p,s) -> SymbolTableList.open_scope();
		     let f = gen_proto p true in
		     let entrybb = Llvm.append_block context "entry" f in
		     Llvm.position_at_end entrybb builder;
		     let paramarray = begin match p with (_,_,a) -> a end in
		     (* copying parameters *)
		     Array.iteri  (fun i a -> let n = paramarray.(i) in
					      Llvm.set_value_name n a;
					      (* allocate mirror variable *)
					      let mirror = create_entry_block_alloca f (n^"_mirror") int_type in
					      (* store parameter value in mirror variable *)
					      SymbolTableList.add n mirror SymbolTableList.SymbInt;
					      ignore(Llvm.build_store a mirror builder))
				  (Llvm.params f);
		     begin match p with
			   |(p_typ, _, _) ->
			     begin match p_typ with
				   | Type_Int ->
				      gen_statement f s INT;
				      ignore (Llvm.build_ret (const_int 0) builder)
       				   | Type_Void ->
				      gen_statement f s VOID;
				      ignore(Llvm.build_ret_void builder);
			     end
		     end;
		     SymbolTableList.close_scope()

let rec gen_program (p : program) =
  List.iter (gen_program_unit) p

(* function that turns the code generated for an expression into a valid LLVM code *)
let gen (p : program) = gen_program p
				    


(* TODO :
invalid assign does not fail
 *)
