module Parser

open Ast

let tokenize (input: string) =
    let rec loop acc = function
        | ' '::xs -> loop acc xs
        | '('::xs -> loop ("("::acc) xs
        | ')'::xs -> loop (")"::acc) xs
        | '"'::xs ->
            let str, rest = parseString ['"'] xs
            loop ((sprintf "\"%s\"" (new string (List.toArray str)))::acc) rest
        | x::xs when System.Char.IsWhiteSpace(x) -> loop acc xs
        | x::xs ->
            let token, rest = parseToken [] (x::xs)
            loop ((new string (List.toArray token))::acc) rest
        | [] -> List.rev acc
    
    and parseString acc = function
        | '"'::xs -> (List.rev acc, xs)
        | '\\'::x::xs -> parseString (x::acc) xs
        | x::xs -> parseString (x::acc) xs
        | [] -> failwith "Unclosed string literal"
    
    and parseToken acc = function
        | c::xs when System.Char.IsWhiteSpace(c) || c = '(' || c = ')' -> 
            (List.rev acc, c::xs)
        | c::xs -> parseToken (c::acc) xs
        | [] -> (List.rev acc, [])
    
    input.ToCharArray() 
    |> List.ofArray 
    |> loop []
    |> List.filter (fun s -> s <> "")

let rec parseExpr = function
    | "("::rest -> 
        let list, remaining = parseList [] rest
        list, remaining
    | ")"::_ -> failwith "Unexpected )"
    | token::rest -> parseAtom token, rest
    | [] -> failwith "Unexpected EOF"

and parseList acc = function
    | ")"::rest -> 
        if List.isEmpty acc then List [], rest
        else List(List.rev acc), rest
    | [] -> failwith "Unclosed list"
    | tokens ->
        let expr, remaining = parseExpr tokens
        parseList (expr::acc) remaining

and parseAtom = function
    | "true" -> Bool true
    | "false" -> Bool false
    | token when token.StartsWith("\"") -> 
        String(token.Trim('"').Replace("\\\"", "\""))
    | token ->
        match System.Int32.TryParse(token) with
        | true, n -> Number n
        | _ -> Atom token

let parse input =
    input |> tokenize |> parseExpr |> fst