module REPL

open System
open Parser
open Eval

let rec repl env =
    Console.Write("> ")
    let input = Console.ReadLine()
    if input = "exit" then ()
    else
        try
            let parsed = parse input
            let result = eval env parsed
            printfn "%A" result
        with ex -> printfn "Error: %s" ex.Message
        repl env

let start () = repl globalEnv