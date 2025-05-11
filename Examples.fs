module Examples

open Parser
open Eval
open System

let runAllExamples () =
    let examples = [
        "(+ 1 2)", "Number 3"
        "(- 10 5)", "Number 5"
        "(* 3 4)", "Number 12"
        "(/ 20 5)", "Number 4"

        "(and true false)", "Bool false"
        "(or true false)", "Bool true"
        "(not true)", "Bool false"
        "(= 5 5)", "Bool true"
        "(< 3 5)", "Bool true"
        "(> 10 5)", "Bool true"

        "(if true 42 0)", "Number 42"
        "(if false 1 0)", "Number 0"
        "(if (> 3 2) \"yes\" \"no\")", "String \"yes\""

        "(let x 5 (+ x x))", "Number 10"
        "(let x 10 (let y 20 (+ x y)))", "Number 30"
        "(let fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))) (fact 5))", "Number 120"

        """(letrec fact 
              (lambda (n) 
                (if (= n 0) 
                    1 
                    (* n (fact (- n 1)))))
           (fact 5))""", "Number 120"

        """(letrec fib
              (lambda (n)
                (if (< n 2)
                    1
                    (+ (fib (- n 1)) (fib (- n 2)))))
           (fib 6))""", "Number 13"

        "((lambda (x) (+ x 1)) 5)", "Number 6"
        "(let add5 (lambda (x) (+ x 5)) (add5 10))", "Number 15"

        "(force (delay (+ 1 2)))", "Number 3"
        
        """(let lazy-value 
              (delay 
                (begin 
                  (print "Calculating...") 
                  (* 3 7)))
           (force lazy-value))""", "Number 21"

        "(car (cons 1 (cons 2 ())))", "Number 1"
        "(cdr (cons 1 (cons 2 ())))", "List [Number 2]"
        "(null? ())", "Bool true"
        "(null? (cons 1 ()))", "Bool false"

        """(let lst (cons 1 (cons 2 (cons 3 ())))
           (car (cdr lst)))""", "Number 2"

        """(let fh (open-input-file "test.txt")
           (read-line fh))""", "String \"Test file content\""
    ]

    printfn "=== Start testing ==="
    printfn "All: %d\n" (List.length examples)

    examples |> List.iteri (fun i (code, expected) ->
        try
            printfn "Test #%d: %s" (i+1) code
            let result = eval globalEnv (parse code)
            printfn "Expected: %s\nResived: %A\n" expected result
        with ex ->
            printfn "Test #%d failed: %s\nError: %s\n" (i+1) code ex.Message)

    printfn "=== End testing ==="

let runExample code =
    try
        let parsed = parse code
        let result = eval globalEnv parsed
        printfn "Result: %A" result
    with ex ->
        printfn "Error: %s" ex.Message