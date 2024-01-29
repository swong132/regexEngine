open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions
  Helpful for creating an environment. You can do this with references 
  (not taught) or without. 
  You do not have to use these and you can modify them if you want. 
  If you do not use references, you will need the following data type:
*)
(* type values = Int of int|Bool of bool|String of string *)

(* Adds mapping [x:v] to environment [env] *)
let ref_extend env x v = (x, ref v)::env

(* let extend env x v = (x,v)::env *)

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec ref_lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else ref_lookup t x

(* let rec lookup env x = 
  match env with
  [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then value else lookup t x *)

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let ref_extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec ref_update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else ref_update t x v
        
(* Removes the most recent variable,value binding from the environment *)
(* let rec remove env x = match env with
  [] -> []
  | (var,value)::t -> if x = var then t else (var,value)::(remove t x) *)

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)
let rec eval_expr env e = 
  match e with
  | Value v -> v
  | ID id -> ref_lookup env id
  | Not n -> let holder = eval_expr env n in  
    (match holder with
    | Bool b -> Bool (not b)
    | _ -> raise (TypeError "Expected bool"))
  | Binop (oper, n1, n2) ->
    (match oper with 
    | Add -> let (n3, n4) = (eval_expr env n1, eval_expr env n2) in
      (match (n3, n4) with 
      | Int n5, Int n6 -> Int (n5 + n6) 
      | _ -> raise (TypeError "Expected int"))
    | Sub -> let (n3, n4) = (eval_expr env n1, eval_expr env n2) in
      (match (n3, n4) with 
      | Int n5, Int n6 -> Int (n5 - n6) 
      | _ -> raise (TypeError "Expected int"))
    | Mult -> let (n3, n4) = (eval_expr env n1, eval_expr env n2) in
      (match (n3, n4) with 
      | Int n5, Int n6 -> Int (n5 * n6) 
      | _ -> raise (TypeError "Expected int"))
    | Div -> let (n3, n4) = (eval_expr env n1, eval_expr env n2) in
      (match (n3, n4) with 
      | Int n5, Int n6 -> if n6 = 0 then raise DivByZeroError else Int (n5/n6) 
      | _ -> raise (TypeError "Expected int"))
    | Greater -> let (n3, n4) = (eval_expr env n1, eval_expr env n2) in
      (match (n3, n4) with 
      | Int n5, Int n6 -> Bool (n5 > n6) 
      | _ -> raise (TypeError "Expected int"))
    | Less -> let (n3, n4) = (eval_expr env n1, eval_expr env n2) in
      (match (n3, n4) with 
      | Int n5, Int n6 -> Bool (n5 < n6) 
      | _ -> raise (TypeError "Expected int"))
    | GreaterEqual -> let (n3, n4) = (eval_expr env n1, eval_expr env n2) in
      (match (n3, n4) with 
      | Int n5, Int n6 -> Bool (n5 >= n6) 
      | _ -> raise (TypeError "Expected int"))
    | LessEqual -> let (n3, n4) = (eval_expr env n1, eval_expr env n2) in
      (match (n3, n4) with 
      | Int n5, Int n6 -> Bool (n5 <= n6) 
      | _ -> raise (TypeError "Expected int"))
    | Concat -> let (n3, n4) = (eval_expr env n1, eval_expr env n2) in
      (match (n3, n4) with 
      | String n5, String n6 -> String (n5 ^ n6) 
      | _ -> raise (TypeError "Expected string"))
    | Equal -> let (n3, n4) = (eval_expr env n1, eval_expr env n2) in
      (match (n3, n4) with 
      | Int n5, Int n6 -> Bool (n5 = n6) 
      | Bool n5, Bool n6 -> Bool (n5 = n6)
      | String n5, String n6 -> Bool (n5 = n6)
      | _ -> raise (TypeError "Cannot compare types"))
    | NotEqual -> let (n3, n4) = (eval_expr env n1, eval_expr env n2) in
      (match (n3, n4) with 
      | Int n5, Int n6 -> Bool (n5 <> n6) 
      | Bool n5, Bool n6 -> Bool (n5 <> n6)
      | String n5, String n6 -> Bool (n5 <> n6)
      | _ -> raise (TypeError "Cannot compare types"))
    | Or -> let (n3, n4) = (eval_expr env n1, eval_expr env n2) in
      (match (n3, n4) with 
      | Bool n5, Bool n6 -> Bool (n5 || n6) 
      | _ -> raise (TypeError "Expected bool"))
    | And -> let (n3, n4) = (eval_expr env n1, eval_expr env n2) in
      (match (n3, n4) with 
      | Bool n5, Bool n6 -> Bool (n5 && n6) 
      | _ -> raise (TypeError "Expected bool")))
  | If (a1, a2, a3) -> let a1 = eval_expr env a1 in
    (match a1 with
    | Bool b -> if b then eval_expr env a2 else eval_expr env a3
    | _ -> raise (TypeError "Expected bool"))
  | Let (id, r, e1, e2) ->
    (match r with
    | false -> let e1' = eval_expr env e1 in eval_expr (ref_extend env id e1') e2
    | true -> let temp = ref_extend_tmp env id in let e1' = eval_expr temp e1 in 
      let () = ref_update temp id e1' in eval_expr temp e2)
  | Fun (id, e1) -> Closure(env, id, e1)
  | FunctionCall (e1, e2) -> let (e1', e2') = (eval_expr env e1, eval_expr env e2) in 
    (match (e1', e2') with
    | (Closure(a, b, c), v) -> let new_env = ref_extend a b v in eval_expr new_env c
    | _ -> raise (TypeError "Wrong types"))

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
  match m with
  | Def (id, e) -> let new_env = ref_extend_tmp env id in 
    let v = eval_expr new_env e in
    let () = ref_update new_env id v in (new_env,Some v)
  | Expr (e) -> (env, Some (eval_expr env e))
  | NoOp -> (env, None)
