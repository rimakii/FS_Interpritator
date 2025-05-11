module Ast

[<NoEquality; NoComparison>]
type LispVal =
    | Atom of string
    | Number of int
    | String of string
    | Bool of bool
    | List of LispVal list
    | Lambda of string list * LispVal * Env
    | PrimitiveFunc of (LispVal list -> LispVal)
    | FileHandle of System.IO.FileStream

and Env = Map<string, LispVal ref>  // ???????? Env ?????
