(* Top-level of the MicroC compiler: scan & parse the input,
   check the resulting AST, generate LLVM IR, and dump the module *)
open Ast
open Parser
open Scanner

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_unop = function
    Neg -> "-"
  | Not -> "!"
  | Addone -> "++"
  | Subone -> "--" 

let string_of_typ_a = function
    Int -> "int"
  | Float -> "float"
  | Complex -> "complex"

let string_of_typ = function
	 Typ_a(a) -> string_of_typ_a a
  | Bool -> "bool"
  | String -> "string"
  | Poly -> "poly"
  | Void -> "void"

let string_of_primary_c = function
    Intlit(l) -> string_of_int l
  | Floatlit(f) -> string_of_float f

let string_of_extra_asn_value = function
    Id(s) -> s
  | Polyextr(s, i) -> s ^ "[[" ^ string_of_int i ^ "]]"
  | Arrextr(s, i) -> s ^ "[" ^ string_of_int i ^ "]"

let string_of_primary_ap = function
    Prim_c(c) -> string_of_primary_c c
  | Comp(a,b) ->  "<" ^ string_of_primary_c a ^ "," ^ string_of_primary_c b  ^ ">"
  | Extr(e) ->  string_of_extra_asn_value e

let string_of_primary = function
    Boollit(true) -> "true"
  | Boollit(false) -> "false"
  | Polylit(p) -> "{" ^ String.concat "," (List.map string_of_primary_ap p) ^ "}"
  | Strlit(p) -> p
  | Prim_ap(p_ap)-> string_of_primary_ap p_ap

let rec string_of_expr = function
    Primary(prim) -> string_of_primary prim
  | Arrlit (arr) -> "[" ^ String.concat "," (List.map string_of_primary_ap arr) ^ "]"
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_unop o ^ string_of_expr e
  | Asn(v, e) -> string_of_extra_asn_value v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  (*| If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ String.concat ")\n" (List.map string_of_stmt s)^ "\n"*) (* ********* *)
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ String.concat ")\n" (List.map string_of_stmt s1)
       ^  String.concat "else\n" (List.map string_of_stmt s2) ^ "\n"
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") {\n" ^ "{" ^ String.concat "" (List.map string_of_stmt s) ^ "\n}\n" 
  | Foreach(e1, s) -> "foreach (" ^ String.concat "" (List.map string_of_expr e1) ^ ") {" ^
      " \n " ^ String.concat "" (List.map string_of_stmt s) ^ "\n}\n"
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") {\n" ^ String.concat "" (List.map string_of_stmt s) ^ "\n}\n"

let string_of_formaldecl = function
	Prim_f_decl(t, s) -> string_of_typ t ^ " " ^ s
  | Arr_f_decl(t, s) ->  string_of_typ_a t ^ " " ^ s ^ "[]"

let rec string_of_variabledecl = function
    Primdecl(a,b) -> string_of_typ a ^ " " ^ b ^ ";\n"
  | Primdecl_i (a,b,c) -> string_of_typ a ^ " " ^ b ^ " = " ^ string_of_primary c ^ ";\n"
  | Arrdecl (a,b,c) -> string_of_typ_a a ^ " [" ^ string_of_int c ^ "]" ^ b ^ ";\n"
  | Arrdecl_i (a,b,c,d) -> string_of_typ_a a ^ " [" ^ string_of_int c ^ "]" ^ b ^ " = " ^ "[" ^ String.concat "," (List.map string_of_primary_ap d) ^ "]" ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.ftyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_formaldecl fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_variabledecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_variabledecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let ast = Parser.program Scanner.token lexbuf in
  print_string (string_of_program ast);;


