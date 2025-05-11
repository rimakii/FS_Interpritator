module Program

open System
open REPL
open Examples

[<EntryPoint>]
let main argv =
    printfn "Functional Language REPL"
    printfn "Available modes:"
    printfn "1. REPL (interactive)"
    printfn "2. Run all examples"
    printfn "Choose mode (1/2): "
    
    match Console.ReadLine() with
    | "1" -> 
        printfn "Starting REPL. Type 'exit' to quit."
        REPL.start()
    | "2" ->
        printfn "Running all examples..."
        Examples.runAllExamples()
    | _ -> 
        printfn "Invalid choice. Starting REPL by default."
        REPL.start()
    
    0