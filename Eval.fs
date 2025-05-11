module Eval

open Ast
open Environment
open Primitives

let rec eval env expr =
    match expr with
    | Number _ | String _ | Bool _ -> expr

    | Atom id -> 
        match lookup id env with
        | { contents = v } -> v

    | List [Atom "quote"; v] -> v

    | List [Atom "if"; cond; tru; fls] -> 
        match eval env cond with
        | Bool true -> eval env tru
        | Bool false -> eval env fls
        | _ -> failwith "if: condition must be boolean"

    | List (Atom "let" :: Atom id :: value :: body) ->
        let newVal = eval env value
        let newEnv = extendEnv id (ref newVal) env
        evalBody newEnv body

    | List (Atom "letrec" :: Atom id :: value :: body) ->
        let dummy = ref (Number 0)
        let tempEnv = extendEnv id dummy env
        dummy := eval tempEnv value
        evalBody tempEnv body

    | List [Atom "lambda"; List args; body] ->
        let argNames = args |> List.map (function 
            | Atom n -> n 
            | _ -> failwith "lambda: expected atoms as parameters")
        Lambda(argNames, body, env)

    | List (Atom "begin" :: exprs) ->
        evalBody env exprs

    | List (func::args) ->
        let f = eval env func
        let argVals = args |> List.map (fun arg -> Lazy.CreateFromValue(eval env arg))
        apply f (argVals |> List.map (fun l -> l.Value))


    | _ -> failwithf "Invalid expression: %A" expr

and evalBody env = function
    | [] -> failwith "Empty body"
    | [x] -> eval env x
    | x::xs -> eval env x |> ignore; evalBody env xs

and apply func args =
    match func with
    | PrimitiveFunc f -> f args
    | Lambda(argNames, body, closure) ->
        if argNames.Length <> args.Length then 
            failwithf "Expected %d arguments, got %d" argNames.Length args.Length
        let newEnv = 
            (argNames, args) 
            ||> List.zip 
            |> List.fold (fun e (k, v) -> extendEnv k (ref v) e) closure
        eval newEnv body
    | _ -> failwith "Not a function"

let globalEnv = 
    allPrimitives 
    |> List.fold (fun env (name, fn) -> extendEnv name (ref fn) env) emptyEnv
