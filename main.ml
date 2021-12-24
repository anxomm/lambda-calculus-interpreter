open Parsing;;
open Lexing;;

open Lambda;;
open Parser;;
open Lexer;;

(*
Return the expression and the number of lines read. To read, it uses a 
function 'f' with its argument 'a' (typical function and channel)
*)
let get_expr f a =

  (* Read the whole expression *)
  let rec aux sub_expr i =

    let line = f a in

      (* If the expression ends in this line, return the expression and line *)
      if String.contains line ';' then
        let expr = sub_expr ^ (List.hd (String.split_on_char ';' line)) in
        expr, i

      (* If the expression does not end in this line, read the next one and concatenate *)
      else aux (sub_expr ^ line ^ " ") (i+1)

  in aux "" 1
;;

(* User interactive loop *)
let interactive_loop () = 
  print_endline "Evaluator of lambda expressions...";
  let rec loop (vctx, tctx) =
    print_string ">> ";
    flush stdout;
    try
      let expr, _ = get_expr read_line () in
      let tm = s token (from_string expr) in
      loop (execute (vctx, tctx) tm)
    with
      Lexical_error ->
        print_endline "lexical error";
        loop (vctx, tctx)
    | Parse_error ->
        print_endline "syntax error";
        loop (vctx, tctx)
    | Type_error e ->
        print_endline ("type error: " ^ e);
        loop (vctx, tctx)
    | End_of_file ->
        print_endline "...bye!!!"
  in
    let initialvctx = addvbinding emptyvctx "debug" TmFalse in
    let initialtctx = addtbinding emptytctx "debug" TyBool in
    loop (initialvctx, initialtctx)
;;

(*
Evaluate expressions in the whole file, until an error occurs (line error is 
the line where the expression begins)
*)
let file_loop filename = 
  print_endline ("Reading file \"" ^ filename ^ "\"...");
  let ic = open_in filename in
    let rec loop (vctx, tctx) i =
      try
        let expr, n = get_expr input_line ic in
        print_endline (">> " ^ expr ^ String.make 1 ';');
        let tm = s token (from_string expr) in
        loop (execute (vctx, tctx) tm) (i+n)
      with
        Lexical_error ->
          print_endline ("[" ^ (string_of_int i) ^ "] lexical error")
      | Parse_error ->
          print_endline ("[" ^ (string_of_int i) ^ "] syntax error")
      | Type_error e ->
          print_endline ("[" ^ (string_of_int i) ^ "] type error: " ^ e)
      | End_of_file ->
          close_in ic
    in    
      let initialvctx = addvbinding emptyvctx "debug" TmFalse in
      let initialtctx = addtbinding emptytctx "debug" TyBool in
      loop (initialvctx, initialtctx) 1
;;

(* If a file is given, execute the file_loop, otherwise the interactive_loop *)
let top_level_loop () = 
  if (Array.length Sys.argv = 1) then interactive_loop ()
  else file_loop Sys.argv.(1)
;;


top_level_loop ()
;;

