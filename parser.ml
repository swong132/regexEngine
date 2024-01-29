open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* PASTE YOUR PARSERS FROM P4A HERE *)

let rec parse_expr toks =
  let (t, e) = parse_E toks in (t, e)

  and parse_E toks =
    match lookahead toks with 
    | Some Tok_Let -> parse_L toks
    | Some Tok_If -> parse_I toks
    | Some Tok_Fun -> parse_F toks
    | _ -> parse_Or toks

  and parse_L toks =
    match lookahead toks with
    | Some Tok_Let -> let t = match_token toks Tok_Let in
        let (t1, r) = parse_Rec t in 
        (match lookahead t1 with
        | Some Tok_ID id -> let toks = match_token t1 (Tok_ID id) in
          let toks' = match_token toks Tok_Equal in
          let (t1, e1) = parse_E toks' in
          let toks'' = match_token t1 Tok_In in
          let (t2, e2) = parse_E toks'' in
          (t2, Let (id, r, e1, e2))
        | _ -> raise (InvalidInputException "Invalid1"))
    | _ -> raise (InvalidInputException "Invalid3")

  and parse_Rec toks = 
    match lookahead toks with
    | Some Tok_Rec -> let toks = match_token toks Tok_Rec in (toks, true)
    | _ -> (toks, false)

  and parse_F toks = 
    match lookahead toks with
    | Some Tok_Fun -> let toks = match_token toks Tok_Fun in
      (match lookahead toks with
      | Some Tok_ID id -> let toks = match_token toks (Tok_ID id) in
        let toks = match_token toks Tok_Arrow in
        let (t, e1) = parse_E toks in
        (t, Fun (id, e1))
      | _ -> raise (InvalidInputException "Invalid4"))
    | _ -> raise (InvalidInputException "Invalid5")

  and parse_I toks =
    match lookahead toks with
    | Some Tok_If -> let t = match_token toks Tok_If in
      let (t1, e1) = parse_E t in
      let toks = match_token t1 Tok_Then in
      let (t2, e2) = parse_E toks in
      let toks = match_token t2 Tok_Else in
      let (t3, e3) = parse_E toks in
      (t3, If (e1, e2, e3))
    | _ -> raise (InvalidInputException "Invalid6")

  and parse_Or toks =
    let (t, e1) = parse_And toks in
    match lookahead t with
    | Some Tok_Or -> let t1 = match_token t Tok_Or in let (t2, e2) = parse_Or t1 in (t2, Binop (Or, e1, e2))
    | _ -> (t, e1)

  and parse_And toks =
    let (t, e1) = parse_Eq toks in
    match lookahead t with
    | Some Tok_And -> let t1 = match_token t Tok_And in let (t2, e2) = parse_And t1 in (t2, Binop (And, e1, e2))
    | _ -> (t, e1)

  and parse_Eq toks =
    let (t, e1) = parse_Rel toks in
    match lookahead t with
    | Some Tok_Equal -> let t1 = match_token t Tok_Equal in
        let (t2, e2) = parse_Eq t1 in (t2, Binop (Equal, e1, e2))
    | Some Tok_NotEqual -> let t1 = match_token t Tok_NotEqual in
        let (t2, e2) = parse_Eq t1 in (t2, Binop (NotEqual, e1, e2))
    | _ -> (t, e1)
    
  and parse_Rel toks =
    let (t, e1) = parse_Add toks in
    match lookahead t with
    | Some Tok_Less -> let t1 = match_token toks Tok_Less in
        let (t2, e2) = parse_Rel t1 in (t2, Binop (Less, e1, e2))
    | Some Tok_Greater -> let t1 = match_token toks Tok_Greater in
        let (t2, e2) = parse_Rel t1 in (t2, Binop (Greater, e1, e2))
    | Some Tok_LessEqual -> let t1 = match_token toks Tok_LessEqual in 
        let (t2, e2) = parse_Rel t1 in (t2, Binop (LessEqual, e1, e2))
    | Some Tok_GreaterEqual -> let t1 = match_token toks Tok_GreaterEqual in
        let (t2, e2) = parse_Rel t1 in (t2, Binop (GreaterEqual, e1, e2))
    | _ -> (t, e1)
   
  and parse_Add toks =
    let (t, e1) = parse_Mul toks in
    match lookahead t with
    | Some Tok_Add -> let t1 = match_token t Tok_Add in 
        let (t2, e2) = parse_Add t1 in (t2, Binop (Add, e1, e2))
    | Some Tok_Sub -> let t1 = match_token t Tok_Sub in 
        let (t2, e2) = parse_Add t1 in (t2, Binop (Sub, e1, e2))
    | _ -> (t, e1)

  and parse_Mul toks =
    let (t, e1) = parse_Con toks in
    match lookahead t with
    | Some Tok_Mult -> let t1 = match_token t Tok_Mult in 
        let (t2, e2) = parse_Mul t1 in (t2, Binop (Mult, e1, e2))
    | Some Tok_Div -> let t1 = match_token t Tok_Div in 
        let (t2, e2) = parse_Mul t1 in (t2, Binop (Div, e1, e2))
    | _ -> (t, e1)

  and parse_Con toks =
    let (t1, e1) = parse_U toks in
    match lookahead t1 with
    | Some Tok_Concat -> let t2 = match_token t1 Tok_Concat in 
      let (t3, e2) = parse_Con t2 in (t3, Binop (Concat, e1, e2))
    | _ -> (t1, e1)

  and parse_U toks =
    match lookahead toks with
    | Some Tok_Not -> let t1 = match_token toks Tok_Not in
      let (t2, e1) = parse_U t1 in (t2, Not (e1))
    | _ -> parse_Fun toks

  and parse_Fun toks =
    let (t1, e1) = parse_P toks in
    match lookahead t1 with
    | Some Tok_Int _ | Some Tok_Bool _ | Some Tok_String _ | Some Tok_ID _ | Some Tok_LParen -> 
      let (t2, e2) = parse_P t1 in (t2, FunctionCall (e1, e2))
    | _ -> (t1, e1)

  and parse_P toks =
    match lookahead toks with
    | Some Tok_Int i -> let t = match_token toks (Tok_Int i) in (t, Value (Int (i)))
    | Some Tok_Bool b -> let t = match_token toks (Tok_Bool b) in (t, Value (Bool (b)))
    | Some Tok_String s -> let t = match_token toks (Tok_String s) in (t, Value (String (s)))
    | Some Tok_ID id -> let t = match_token toks (Tok_ID id) in (t, ID (id))
    | Some Tok_LParen -> let t = match_token toks Tok_LParen in 
          let (t1, e1) = parse_E t in
          let t2 = match_token t1 Tok_RParen in (t2, e1)
    | _ -> raise (InvalidInputException "Invalid7")

let rec parse_mutop toks = 
  match lookahead toks with
  | Some Tok_Def -> parse_Def toks
  | Some Tok_DoubleSemi -> ([], NoOp)
  | _ -> parse_Expr toks

  and parse_Def toks =
    let toks = match_token toks Tok_Def in
    match lookahead toks with
    | Some Tok_ID id -> let t1 = match_token toks (Tok_ID id) in
      let t2 = match_token t1 Tok_Equal in
      let (t3, e) = parse_expr t2 in 
      let t4 = match_token t3 Tok_DoubleSemi in (t4, Def (id, e))
    | _ -> raise (InvalidInputException "Invalid8")

  and parse_Expr toks =
    let (t, e) = parse_expr toks in
    match lookahead t with
    | Some Tok_DoubleSemi -> let t' = match_token t Tok_DoubleSemi in
      (t', Expr (e))
    | _ -> raise (InvalidInputException "Invalid9")
