
(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyString
  | TyPair of ty * ty
  | TyRecord of (string * ty) list
  | TyArr of ty * ty
  | TyTop
;;

type tcontext =
  (string * ty) list
;;

type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmFix of term
  | TmString of string
  | TmConcat of term * term
  | TmPair of term * term
  | TmProj1 of term
  | TmProj2 of term
  | TmRecord of (string * term) list
  | TmProj of term * string
;;

(* List of 'global variables' *)
type vcontext =
  (string * term) list
;;

(*
A command may be the evaluation of a term (Eval) or the assignation of a new
variable to the vcontext (Bind)
*)
type command =
    Eval of term
  | Bind of string * term
;;

(* CONTEXT MANAGEMENT *)

let emptytctx =
  []
;;

let addtbinding ctx x bind =
  (x, bind) :: ctx
;;

let gettbinding ctx x =
  List.assoc x ctx
;;

let emptyvctx =
  []
;;

let addvbinding ctx x bind =
  (x, bind) :: ctx
;;

let getvbinding ctx x =
  List.assoc x ctx
;;

(* TYPE MANAGEMENT (SUBTYPING) *)

let rec subtypeof tyS tyT = match (tyS, tyT) with

    (* S-Top *)
    (_, TyTop) -> true

    (* S-Refl *)
  | (ty1, ty2) when ty1 = ty2 -> true

    (* S-Arrow *)
  | (TyArr (tyS1, tyS2), TyArr (tyT1, tyT2)) ->
      subtypeof tyT1 tyS1 && subtypeof tyS2 tyT2

    (* S-Rcd: every field with id 's' in ty2 must coincide, or be a supertype,
       of the type with id 's' in ty1 *)
  | (TyRecord ty1, TyRecord ty2) ->
      let f (s, ty2') = 
        (try 
          let ty1' = List.assoc s ty1 in
          subtypeof ty1' ty2'
        with Not_found -> false) in 
      List.for_all f ty2

  | _ -> false
;;

(* TYPE MANAGEMENT (TYPING) *)

let rec string_of_ty ty = match ty with
    TyBool ->
      "Bool"
  | TyNat ->
      "Nat"
  | TyString ->
      "String"
  | TyPair (ty1, ty2) ->
      "{" ^ string_of_ty ty1 ^ ", " ^ string_of_ty ty2 ^ "}"
  | TyRecord l ->
      let f (li, tyi) = li ^ " : " ^ string_of_ty tyi ^ ", " in
      let s = List.fold_left (^) "" (List.map f l) in
      let s' = if s <> "" then String.sub s 0 (String.length s - 2) else "" in
      "{ " ^ s' ^ " }"
  | TyArr (ty1, ty2) ->
      "(" ^ string_of_ty ty1 ^ ")" ^ " -> " ^ "(" ^ string_of_ty ty2 ^ ")"
  | TyTop ->
      "Top"
;;

exception Type_error of string
;;

let rec typeof ctx tm = match tm with

    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof ctx t1 = TyBool then
        let tyT2 = typeof ctx t2 in
        if typeof ctx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")
      
    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof ctx t1 = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

    (* T-Var *)
  | TmVar x ->
      (try gettbinding ctx x with
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let ctx' = addtbinding ctx x tyT1 in
      let tyT2 = typeof ctx' t2 in
      TyArr (tyT1, tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if subtypeof tyT2 tyT11 then tyT12
             else raise (Type_error "parameter type mismatch")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addtbinding ctx x tyT1 in
      typeof ctx' t2

    (* T-Fix *)
  | TmFix t1 ->
      let tyT1 = typeof ctx t1 in
      (match tyT1 with
          TyArr (tyT11, tyT12) ->
            if tyT11 = tyT12 then tyT12
            else raise (Type_error "result of body not compatible with domain")
        | _ -> raise (Type_error "arrow type expected"))

    (* T-String *)
  | TmString _ ->
      TyString

    (* T-Concat *)
  | TmConcat (t1, t2) ->
      if typeof ctx t1 <> TyString then raise (Type_error "first argument of concat is not a string")
      else if typeof ctx t2 <> TyString then raise (Type_error "second argument of concat is not a string")
      else TyString

    (* T-Pair *)
  | TmPair (t1, t2) ->
      let ty1 = typeof ctx t1 in
      let ty2 = typeof ctx t2 in
      TyPair (ty1, ty2)

    (* T-Proj1 *)
  | TmProj1 t1 ->
      let tyT1 = typeof ctx t1 in
      (match tyT1 with
          TyPair (tyT11, _) ->
            tyT11
        | _ -> raise (Type_error "pair type expected"))

    (* T-Proj2 *)
  | TmProj2 t1 ->
      let tyT1 = typeof ctx t1 in
      (match tyT1 with
          TyPair (_, tyT12) ->
            tyT12
        | _ -> raise (Type_error "pair type expected"))

    (* T-Record *)
  | TmRecord l ->
      let f (li, ti) = (li, typeof ctx ti) in
      TyRecord (List.map f l)

    (* T-Proj *)
  | TmProj (t1, lj) ->
      let tyT1 = typeof ctx t1 in
      (match tyT1 with 
          TyRecord l ->
            (try List.assoc lj l
            with Not_found -> raise (Type_error ("no field '" ^ lj ^ "' found")))
        | _ -> raise (Type_error "record type expected"))
;;


(* TERMS MANAGEMENT (EVALUATION) *)

let rec string_of_term = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmIf (t1,t2,t3) ->
      "if " ^ "(" ^ string_of_term t1 ^ ")" ^
      " then " ^ "(" ^ string_of_term t2 ^ ")" ^
      " else " ^ "(" ^ string_of_term t3 ^ ")"
  | TmZero ->
      "0"
  | TmSucc t ->
     let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
      in f 1 t
  | TmPred t ->
      "pred " ^ "(" ^ string_of_term t ^ ")"
  | TmIsZero t ->
      "iszero " ^ "(" ^ string_of_term t ^ ")"
  | TmVar s ->
      s
  | TmAbs (s, tyS, t) ->
      "(lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ ")"
  | TmApp (t1, t2) ->
      "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmFix t -> 
      "(fix " ^ string_of_term t ^ ")"
  | TmString s ->
      "\"" ^ s ^ "\""
  | TmConcat (t1, t2) ->
      string_of_term t1 ^ " ^ " ^ string_of_term t2
  | TmPair (t1, t2) ->
      "{" ^ string_of_term t1 ^ ", " ^ string_of_term t2 ^ "}"
  | TmProj1 t ->
      "first " ^ string_of_term t
  | TmProj2 t ->
      "second " ^ string_of_term t
  | TmRecord l ->
      let f (li, ti) = li ^ " = " ^ string_of_term ti ^ ", " in
      let s = List.fold_left (^) "" (List.map f l) in
      let s' = if s <> "" then String.sub s 0 (String.length s - 2) else "" in
      "{ " ^ s' ^ " }"
  | TmProj (t, lj) ->
      string_of_term t ^ "." ^ lj
;;

let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmFix t ->
      free_vars t
  | TmString _ ->
      []
  | TmConcat (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmPair (t1, t2) -> 
      lunion (free_vars t1) (free_vars t2)
  | TmProj1 t ->
      free_vars t
  | TmProj2 t ->
      free_vars t
  | TmRecord t ->
      let f (li, ti) = free_vars ti in
      List.fold_left lunion [] (List.map f t)
  | TmProj (t, _) ->
      free_vars t
;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;
    
let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) -> 
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))  
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
  | TmFix t ->
      TmFix (subst x s t)
  | TmString s ->
      TmString s
  | TmConcat (t1, t2) ->
      TmConcat (subst x s t1, subst x s t2)
  | TmPair (t1, t2) -> 
      TmPair (subst x s t1, subst x s t2)
  | TmProj1 t ->
      TmProj1 (subst x s t)
  | TmProj2 t ->
      TmProj2 (subst x s t)
  | TmRecord t ->
      let f (li, ti) = (li, subst x s ti) in
      TmRecord (List.map f t)
  | TmProj (t, lj) ->
      TmProj (subst x s t, lj)
;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmString _ -> true
  | TmAbs _ -> true
  | TmPair (v1, v2) when isval v1 && isval v2 -> true
  | TmRecord t ->
      let f (li, vi) = isval vi in
      List.for_all f t 
  | t when isnumericval t -> true
  | _ -> false
;;

exception NoRuleApplies
;;

let rec eval1 vctx tm = match tm with

    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 vctx t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 vctx t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 vctx t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 vctx t1 in
      TmIsZero t1'

    (* E-Var *)
  | TmVar s ->
      getvbinding vctx s

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 vctx t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn (x, t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmLetIn (x, t1', t2) 

    (* E-FixBeta *)
  | TmFix (TmAbs (x, _, t2)) ->
      subst x tm t2

    (* E-Fix *)
  | TmFix t1 ->
      let t1' = eval1 vctx t1 in
      TmFix t1'

    (* E-Concat *)
  | TmConcat (TmString s1, TmString s2) ->
      TmString (s1 ^ s2)

  | TmConcat (v1, t2) when isval v1 ->
      let t2' = eval1 vctx t2 in
      TmConcat (v1, t2')

  | TmConcat (t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmConcat(t1', t2)

    (* E-PairBeta1 *)
  | TmProj1 (TmPair (v1, v2)) when isval v1 && isval v2 ->
      v1

    (* E-PairBeta2 *)
  | TmProj2 (TmPair (v1, v2)) when isval v1 && isval v2 ->
      v2

    (* E-Proj1 *)
  | TmProj1 t1 ->
      let t1' = eval1 vctx t1 in
      TmProj1 t1'

    (* E-Proj2 *)
  | TmProj2 t1 ->
      let t1' = eval1 vctx t1 in
      TmProj2 t1'

    (* E-Pair2 *)
  | TmPair (v1, t2) when isval v1 ->
      let t2' = eval1 vctx t2 in
      TmPair (v1, t2')

    (* E-Pair1 *)
  | TmPair (t1, t2) ->
      let t1' = eval1 vctx t1 in
      TmPair (t1', t2)

    (* E-ProjRcd *)
  | TmProj (TmRecord l, lj) when isval (TmRecord l) ->
        List.assoc lj l

    (* E-Proj *)
  | TmProj (t1, l) ->
      let t1' = eval1 vctx t1 in 
      TmProj (t1', l)

    (* E-EmptyRecord *)
  | TmRecord [] ->
      raise NoRuleApplies

    (* E-Record *)
  | TmRecord l -> 
      let f (li, ti) = (li, eval1 vctx ti) in
      TmRecord (List.map f l)
      
  | _ ->
      raise NoRuleApplies
;;

(*
Substitute in the term the 'global variables' by their value in the vcontext
*)
let apply_context vctx tm =

  let rec aux vl tm = match tm with

      TmTrue ->
        TmTrue

    | TmFalse ->
        TmFalse

    | TmIf (t1, t2, t3) ->
        TmIf (aux vl t1, aux vl t2, aux vl t3)

    | TmZero -> 
        TmZero

    | TmSucc t ->
        TmSucc (aux vl t)

    | TmPred t ->
        TmPred (aux vl t)
        
    | TmIsZero t ->
        TmIsZero (aux vl t)

    | TmVar s ->
        if List.mem s vl then TmVar s else getvbinding vctx s

    | TmAbs (s, ty, t1) ->
        TmAbs(s, ty, aux (s::vl) t1)

    | TmApp (t1, t2) ->
        TmApp (aux vl t1, aux vl t2)

    | TmLetIn (s, t1, t2) ->
        TmLetIn (s, aux vl t1, aux (s::vl) t2)

    | TmFix t ->
        TmFix (aux vl t)

    | TmString s ->
        TmString s 

    | TmConcat (t1, t2) ->
        TmConcat (aux vl t1, aux vl t2)

    | TmPair (t1, t2) ->
        TmPair (aux vl t1, aux vl t2)

    | TmProj1 t ->
        TmProj1 (aux vl t)

    | TmProj2 t ->
        TmProj2 (aux vl t)

    | TmRecord l ->
        let f (li, ti) = (li, aux vl ti)
        in TmRecord (List.map f l) 

    | TmProj (t, l) ->
        TmProj (aux vl t, l)

  in aux [] tm
;;

let rec eval vctx tm =
  try
    let tm' = eval1 vctx tm in
    let debug = getvbinding vctx "debug" in
    if debug = TmTrue then print_endline ("   " ^ string_of_term tm');
    eval vctx tm'
  with
    NoRuleApplies -> apply_context vctx tm
;;

(*
Execute a command. When it is the simple evaluation of a term, it behaves the same.
When it is the assignation of a 'global variable', evaluate the term and add the 
association (name,value) to the vcontext
*)
let execute (vctx, tctx) command = match command with
    Eval tm ->
      let tyTm = typeof tctx tm in
      let tm' = eval vctx tm in
      print_endline ("- : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
      (vctx, tctx)
  | Bind (s, tm) ->
      let tyTm = typeof tctx tm in
      let tm' = eval vctx tm in
      print_endline (s ^ " : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
      (addvbinding vctx s tm', addtbinding tctx s tyTm)
;;


