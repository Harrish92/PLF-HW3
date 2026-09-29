(* CS 4110 Homework 3
   This is the file where you'll do your work. Your job is to take an AST
   (which has already been parsed for you) and execute it. Exceptional
   conditions will arise in some cases, see `errors.ml` for the exceptions to
   `raise` when these happen. *)

open Ast
open Errors

(* A type for stores. *)
type store = (var, int) Hashtbl.t

type continuation = {
  break_cont : com option;     (* command to run after break *)
  continue_cont : com option;  (* command to run after continue *)
}

(* A type for configurations. *)
type configuration = {
  store : store;
  cmd : com;
  cont : com;
  k : continuation list;
}

(* Create an initial configuration from a command. *)
let make_configuration (c:com) : configuration =
  {
    store = Hashtbl.create 64;
    cmd = c;
    cont = Skip;
    k = [];
  }

(* Look up a variable in the store; raise if unbound. *)
let lookup (s:store) (x:var) : int =
  match Hashtbl.find_opt s x with
  | Some v -> v
  | None -> raise (UnboundVariable x)

let rec eval_aexp (s:store) (a:aexp) : int =
  match a with
  | Int n -> n
  | Var x -> lookup s x
  | Plus (a1, a2) -> eval_aexp s a1 + eval_aexp s a2
  | Minus (a1, a2) -> eval_aexp s a1 - eval_aexp s a2
  | Times (a1, a2) -> eval_aexp s a1 * eval_aexp s a2
  | Input ->
      print_string "> ";
      flush stdout;
      (try int_of_string (read_line ())
       with _ -> 0)

let rec eval_bexp (s:store) (b:bexp) : bool =
  match b with
  | True -> true
  | False -> false
  | Equals (a1, a2) -> eval_aexp s a1 = eval_aexp s a2
  | NotEquals (a1, a2) -> eval_aexp s a1 <> eval_aexp s a2
  | Less (a1, a2) -> eval_aexp s a1 < eval_aexp s a2
  | LessEq (a1, a2) -> eval_aexp s a1 <= eval_aexp s a2
  | Greater (a1, a2) -> eval_aexp s a1 > eval_aexp s a2
  | GreaterEq (a1, a2) -> eval_aexp s a1 >= eval_aexp s a2
  | Not b1 -> not (eval_bexp s b1)
  | And (b1, b2) ->
      (* Evaluate both sides; no short-circuit. *)
      let v1 = eval_bexp s b1 in
      let v2 = eval_bexp s b2 in
      v1 && v2
  | Or (b1, b2) ->
      (* Evaluate both sides; no short-circuit. *)
      let v1 = eval_bexp s b1 in
      let v2 = eval_bexp s b2 in
      v1 || v2

(* Evaluate a command. *)
let rec evalc (conf:configuration) : store =
  let s = conf.store in
  match conf.cmd with
  | Skip ->
      (* execute the continuation *)
      if conf.cont = Skip then s
      else evalc { conf with cmd = conf.cont; cont = Skip }
  | Assign (x, a) ->
      Hashtbl.replace s x (eval_aexp s a);
      evalc { conf with cmd = conf.cont; cont = Skip }
  | Print a ->
      print_int (eval_aexp s a);
      print_newline ();
      evalc { conf with cmd = conf.cont; cont = Skip }
  | Seq (c1, c2) ->
      if conf.cont = Skip
      then evalc { conf with cmd = c1; cont = c2 }
      else evalc { conf with cmd = c1; cont = Seq (c2, conf.cont) }
  | If (b, c1, c2) ->
      let branch = if eval_bexp s b then c1 else c2 in
      evalc { conf with cmd = branch; cont = conf.cont }
  | While (b, c) ->
      if eval_bexp s b then
        let loop = { break_cont = Some conf.cont;
                     continue_cont = Some (While (b, c)) } in
        evalc { store = s;
                cmd = c;
                cont = Seq (While (b, c), conf.cont);
                k = loop :: conf.k }
      else
        evalc { conf with cmd = conf.cont; cont = Skip }
  | Test (info, b) ->
      if eval_bexp s b then
        evalc { conf with cmd = conf.cont; cont = Skip }
      else begin
        Printf.printf "TestFailed at %s\n" (Pprint.strInfo info);
        exit 1
      end
  | Break ->
      (match conf.k with
       | [] -> raise IllegalBreak
       | loop :: _ ->
           (match loop.break_cont with
            | Some k -> evalc { conf with cmd = k; cont = Skip;
                                k = List.tl conf.k }
            | None -> raise IllegalBreak))
  | Continue ->
      (match conf.k with
       | [] -> raise IllegalContinue
       | loop :: _ ->
           (match loop.continue_cont with
            | Some k -> evalc { conf with cmd = k; cont = Skip;
                                k = conf.k }
            | None -> raise IllegalContinue))
