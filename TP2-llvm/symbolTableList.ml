(* type in the symbol table list *)
type symbType = SymbInt | SymbArray


(* global variable for the symbol table *)
let st : (string * (Llvm.llvalue * symbType)) list list ref = ref []

(* opens a new scope *)
let open_scope () =
  st := []::!st

(* closes the current scope *)
let close_scope () =
  match !st with
  | [] -> failwith "SymbolTable.leave_scope: no scope to close"
  | _::l -> st := l

(* adds a symbol from its id and 'llvalue' in the current scope *)
let add id v (s:symbType)=
  match !st with
  | [] -> failwith "SymbolTable.add: no open scope"
  | scope::l ->
      if List.mem_assoc id scope
      then failwith ("SymbolTable.add: " ^ id ^ " already defined in the current scope")
      else st := ((id,(v,s))::scope)::l

(* lookup the 'llvalue' of a symbol from its id in the innermost scope *)
let lookup id =
  let rec aux st =
    match st with
    | [] -> failwith ("SymbolTable.lookup: unknown variable " ^ id ) 
    | scope::l ->
	try fst(List.assoc id scope)
	with Not_found -> aux l
  in
  aux !st

(* lookup the type *)
let lookup_type id =
    let rec aux st =
    match st with
    | [] -> failwith ("SymbolTable.lookup: unknown variable " ^ id ) 
    | scope::l ->
	try snd(List.assoc id scope)
	with Not_found -> aux l
  in
  aux !st

