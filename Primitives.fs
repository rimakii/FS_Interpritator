module Primitives

open Ast
open System.IO

let numericBinop op args =
    match args with
    | [Number x; Number y] -> Number(op x y)
    | _ -> failwith "Expected two numbers"

let comparisonOp op args =
    match args with
    | [Number x; Number y] -> Bool(op x y)
    | _ -> failwith "Expected two numbers"

let equal args =
    match args with
    | [Number x; Number y] -> Bool (x = y)
    | [Bool x; Bool y] -> Bool (x = y)
    | [String x; String y] -> Bool (x = y)
    | [List []; List []] -> Bool true
    | [List _; List _] -> Bool false  // Simple list comparison - doesn't recurse
    | [Atom x; Atom y] -> Bool (x = y)
    | _ -> Bool false  // Different types or complex values are not equal

let listPrimitives = [
    ("car", PrimitiveFunc(fun args ->
        match args with 
        | [List(x::_)] -> x 
        | _ -> failwith "car: expected non-empty list"))
    
    ("cdr", PrimitiveFunc(fun args ->
        match args with 
        | [List(_::xs)] -> List xs 
        | _ -> failwith "cdr: expected non-empty list"))
    
    ("cons", PrimitiveFunc(fun args ->
        match args with 
        | [x; List xs] -> List(x::xs) 
        | [x; y] -> List [x; y]
        | _ -> failwith "cons: expected two arguments"))
    
    ("null?", PrimitiveFunc(fun args ->
        match args with
        | [List []] -> Bool true
        | [List _] -> Bool false
        | _ -> failwith "null?: expected list"))
]

let ioPrimitives = [
    ("open-input-file", PrimitiveFunc(fun args ->
        match args with
        | [String path] -> FileHandle(File.OpenRead path)
        | _ -> failwith "open-input-file: expected string path"))
    
    ("read-line", PrimitiveFunc(fun args ->
        match args with
        | [FileHandle stream] -> 
            use reader = new StreamReader(stream)
            String(reader.ReadLine())
        | _ -> failwith "read-line: expected file handle"))
]

let primitives = [
    ("+", PrimitiveFunc(numericBinop (+)))
    ("-", PrimitiveFunc(numericBinop (-)))
    ("*", PrimitiveFunc(numericBinop (*)))
    ("/", PrimitiveFunc(numericBinop (/)))

    ("=", PrimitiveFunc(equal))
    ("<", PrimitiveFunc(comparisonOp (<)))
    (">", PrimitiveFunc(comparisonOp (>)))

    ("and", PrimitiveFunc(fun args ->
        match args with
        | [Bool a; Bool b] -> Bool(a && b)
        | _ -> failwith "and: expected two booleans"))

    ("or", PrimitiveFunc(fun args ->
        match args with
        | [Bool a; Bool b] -> Bool(a || b)
        | _ -> failwith "or: expected two booleans"))

    ("not", PrimitiveFunc(fun args ->
        match args with
        | [Bool a] -> Bool(not a)
        | _ -> failwith "not: expected boolean"))
]

let rec factorial n =
    if n = 0 then 1
    else n * factorial (n - 1)

let factorialFunc =
    ("factorial", PrimitiveFunc(fun args ->
        match args with
        | [Number n] when n >= 0 -> Number(factorial n)
        | _ -> failwith "factorial: expected a non-negative integer"))

let allPrimitives = 
    primitives @ listPrimitives @ ioPrimitives @ [factorialFunc]
