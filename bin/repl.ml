open Common;;




let rec string_val e =
    let rec string_list l =
        match l with
        | Pair (a, Nil) -> string_val a
        | Pair (a, b) -> string_val a ^ " " ^ string_list b
        | _ -> raise Unreachable 
    in
    let string_pair p =
        match p with
        | Pair (a, b) -> string_val a ^ " . " ^ string_val b
        | _ -> raise Unreachable 
    in
    match e with
    | Fixnum v -> string_of_int v
    | Boolean b -> if b then "#t" else "#f"
    | Symbol s -> s
    | Nil -> "nil"
    | Pair (_a, _b) ->
            "(" ^ (if is_list e then string_list e else string_pair e) ^ ")"
    | Quote v -> "'" ^ string_val v
    | Primitive (name, _) -> "#<primitive:" ^ name ^ ">"
    | Closure (_ns,_e, _) -> "#<closure>"
(*        "[lambda (" ^ spacesep ns ^ ") " ^ string_exp e ^ "]"  *)


let get_ic () =
  try  open_in Sys.argv.(1)
  with Invalid_argument _s -> stdin


let rec repl stm env= 
  if stm.chan=stdin then ( print_string "> "; flush stdout; );
  let ast = build_ast (read_sexp stm) in
  let (result, env') = eval ast env in
  if stm.chan=stdin then print_endline (string_val result);
  repl stm env';;

let main =
  let ic = get_ic () in
  let stm = { chr=[]; line_num=1; chan=ic } in
  try  repl stm basis
  with End_of_file -> if ic <> stdin then close_in ic;;


main;;