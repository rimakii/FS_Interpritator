module Environment

open Ast

type Env = Map<string, LispVal ref>

let emptyEnv : Env = Map.empty

let extendEnv (name: string) (value: LispVal ref) (env: Env) : Env =
    env.Add(name, value)

let lookup (name: string) (env: Env) : LispVal ref =
    match env.TryFind name with
    | Some v -> v
    | None -> failwithf "Unbound variable: %s" name
