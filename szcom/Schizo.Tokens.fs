//
// Schizo F# Reference Compiler
// Copyright (C) 2014-2016  Wael El Oraiby
// 
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
// 
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

module Schizo.Tokens

open System

type Token =
    | TokUnit         of DebugInfo
    | TokBoolean      of DebugInfo * bool       // true | false
    | TokChar         of DebugInfo * char       // 'c' | '\n' | ...
    | TokInt64        of DebugInfo * int64      // 1234567890
    | TokReal64       of DebugInfo * double     // 450.5 | 567e10
    | TokIdentifier   of DebugInfo * string     // abcdedf
    | TokType         of DebugInfo * string     // Type
    | TokGeneric      of DebugInfo * string     // #fg
    | TokOperator     of DebugInfo * string     // + | ++ | $$ | == | ...
    | TokList         of DebugInfo * Token list // [ ... ; ... ]
    | TokScope        of DebugInfo * Token list // { ... ; ... ; }
    | TokTuple        of DebugInfo * Token list // ( ... , ... )
    | TokExpression   of DebugInfo * Token list // (sym | app) exp exp ... | (sym | app | ident) op exp ...
with
    member x.TokenTypeName =
        match x with
        | TokUnit         di      -> "unit"
        | TokBoolean      (di, y) -> "boolean"
        | TokChar         (di, y) -> "char"
        | TokInt64        (di, y) -> "int"
        | TokReal64       (di, y) -> "real"
        | TokIdentifier   (di, y) -> "identifier"
        | TokType         (di, y) -> "type"
        | TokGeneric      (di, y) -> "generic"
        | TokOperator     (di, y) -> "operator"
        | TokList         (di, y) -> "list"
        | TokScope        (di, y) -> "scope"
        | TokTuple        (di, y) -> "tuple"
        | TokExpression   (di, y) -> "expression"

    override x.ToString() =
        let printList chLeft (y: Token list) chRight sep =
            let str =
                y
                |> List.fold (fun state tok ->
                    match state with
                    | "" -> sprintf "%O" tok
                    | _  -> sprintf "%s%c %O" state sep tok) ""
            sprintf "%c%s%c" chLeft str chRight

        match x with
        | TokUnit         (_)    -> "()"
        | TokBoolean      (_, y) -> y.ToString()
        | TokChar         (_, y) -> sprintf "'%c'" y
        | TokInt64        (_, y) -> y.ToString()
        | TokReal64       (_, y) -> y.ToString()
        | TokIdentifier   (_, y) -> y
        | TokType         (_, y) -> sprintf "*%s*" y
        | TokGeneric      (_, y) -> sprintf "`%s`" y
        | TokOperator     (_, y) -> y
        | TokList         (_, y) -> printList '[' y ']' ';'
        | TokScope        (_, y) -> printList '{' y '}' ';'
        | TokTuple        (_, y) -> printList '(' y ')' ','
        | TokExpression   (_, y) -> printList '~' y '~' ' '

    member x.DebugInfo =
        match x with
        | TokUnit         di      -> di
        | TokBoolean      (di, _) -> di
        | TokChar         (di, _) -> di
        | TokInt64        (di, _) -> di
        | TokReal64       (di, _) -> di
        | TokIdentifier   (di, _) -> di
        | TokType         (di, _) -> di
        | TokGeneric      (di, _) -> di
        | TokOperator     (di, _) -> di
        | TokList         (di, _) -> di
        | TokScope        (di, _) -> di
        | TokTuple        (di, _) -> di
        | TokExpression   (di, _) -> di

and DebugInfo = {
    StreamId        : string
    Line            : int
    Offset          : int
} with
    static member empty streamId = { StreamId = streamId; Line = 1; Offset = 0 }

exception TokenException of (DebugInfo * string) list

let tokExcept (di: DebugInfo, message: string) = TokenException ((di, message) :: [])

let private isAlpha ch = (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch = '_')

let private isDigit ch = ch >= '0' && ch <= '9'

let private isUpper ch = ch >= 'A' && ch <= 'Z'

let private isSpecial =
    function
    | '`'   | '!'   | '@'    
    | '$'   | '%'   | '^'   | '&' 
    | '*'   | '-'   | '+'   | '='
    | '/'   | '?'   | '<'   | '>'
    | '|'   | '~'   | '.'   | ':' -> true
    | _   -> false

let private readChar (str: Token list) : Token * Token list =
    match str with
    | TokChar (di, '\\') :: TokChar (_, ch) :: TokChar (_, '\'') :: t ->
        match ch with
        | 'n'  -> TokChar (di, '\n'), t
        | 'r'  -> TokChar (di, '\r'), t
        | 'a'  -> TokChar (di, '\a'), t
        | 't'  -> TokChar (di, '\t'), t
        | '\'' -> TokChar (di, '\''), t
        | _    -> raise (tokExcept (di, sprintf "unknown escape character %c after \\ in char" ch))
    | TokChar (di, ch) :: TokChar (_, '\'') :: t       -> TokChar (di, ch), t
    | h :: t        -> raise (tokExcept (h.DebugInfo, "readChar: unexpected token"))
    | []            -> failwith "reading a char: not a well formed char"

let rec private readString (acc: Token list) (str: Token list) : Token * Token list =
    match str with
    | TokChar (di, '"')  :: t -> TokList (di, (acc |> List.rev)), t // "
    | TokChar (di, '\\') :: TokChar (_, ch) :: t ->
        let newCh =
            match ch with
            | 'n'  -> '\n'
            | 'r'  -> '\r'
            | 'a'  -> '\a'
            | 't'  -> '\t'
            | '\'' -> '\''
            | _    -> raise (tokExcept (di, sprintf "unknown escape character %c after \\ in string" ch))
        readString (TokChar (di, newCh) :: acc) t
    | ch :: t -> readString (ch :: acc) t
    | [] -> failwith "ill formated string"

let rec private readSymbolAlphaNum di (acc: Token list) (str: Token list) : Token * Token list =
    match str with
    | TokChar (_di, ch) :: t when isAlpha ch || isDigit ch || (acc = [] && ch = '#') -> readSymbolAlphaNum di (TokChar (_di, ch) :: acc) t
    | _ ->
        let sym = System.String(acc |> List.rev |> Array.ofList |> Array.map (function | TokChar (_, ch) -> ch | _ -> failwith "unreachable"))
        let sym =
            match sym with
            | ""      -> raise (tokExcept (di, "invalid identifier (null)"))
            | "true"  -> TokBoolean (di, true)
            | "false" -> TokBoolean (di, false)
            | str when str.[0] |> isUpper -> TokType    (di, sym)
            | str when str.[0] = '#'      -> TokGeneric (di, sym)
            | str     -> TokIdentifier (di, sym)
        sym, str    // anything else, just bail

let rec private readSymbolSpecial di (acc: Token list) (str: Token list) : Token * Token list =
    match str with
    | TokChar (_di, ch) :: t when isSpecial ch -> readSymbolSpecial di (TokChar (_di, ch) :: acc) t
    | TokChar (_di, ch) :: _                     ->
        let sym = System.String(acc |> List.rev |> Array.ofList |> Array.map (function | TokChar (_, ch) -> ch | _ -> failwith "readSymbolSpecial: unreachable"))
        TokOperator (di, sym), str    // anything else, just bail
    | _ -> failwith "readSymbolSpecial: unreachable"
//
// floating point grammar
// D   [0-9]
// E   ([Ee][+-]?{D}+)
//
// {D}+"."{D}+{E}?
//333
let private readNumber di (str: Token list) : Token * Token list=
    let rec readInt (acc: Token list) (str: Token list) : Token list * Token list =
        match str with
        | TokChar (di, ch) :: t when isDigit ch ->  readInt (TokChar (di, ch) :: acc) t
        | _ -> List.rev acc, str

    let integral, nextList =
        match str with
        | TokChar (di, '+') :: t -> let i, nextList = readInt [] t in TokChar (di, '+') :: i, nextList
        | TokChar (di, '-') :: t -> let i, nextList = readInt [] t in TokChar (di, '-') :: i, nextList
        | _ -> readInt [] str

    let decimal, nextList =
        match nextList with
        | TokChar (di, '.') :: t -> let d, nextList = readInt [] t in TokChar (di, '.') :: d, nextList
        | _ -> [], nextList

    let expo, nextList =
        match decimal, nextList with
        | [], _ -> [], nextList
        | _, TokChar (di, 'E') :: t
        | _, TokChar (di, 'e') :: t ->
            match t with
            | TokChar (di2, '+') :: t -> let i, nextList = readInt [] t in (TokChar (di, 'E')) :: (TokChar (di2, '+')) :: i, nextList
            | TokChar (di2, '-') :: t -> let i, nextList = readInt [] t in (TokChar (di, 'E')) :: (TokChar (di2, '-')) :: i, nextList
            | t -> let i, nextList = readInt [] t in (TokChar (di, 'E')) :: (TokChar (di, '+')) :: i, nextList
        | _, _ -> [], nextList
        

    let token, nextList =
        match integral, decimal, expo with
        | integral, [], _ ->
            let intstr = System.String (integral |> List.toArray |> Array.map (function | TokChar (_, ch) -> ch | _ -> failwith "unreachable"))

            if intstr.Length <> 0
            then TokInt64 (di, Convert.ToInt64 intstr), nextList
            else raise (tokExcept (di, "invalid integer number"))

        | integral, decimal, [] ->
            let number =  List.append integral decimal 
            let number = System.String (number |> List.toArray |> Array.map (function | TokChar (_, ch) -> ch | _ -> failwith "unreachable"))

            if number.Length <> 0
            then TokReal64 (di, Convert.ToDouble number), nextList
            else raise (tokExcept (di, "invalid floating number"))

        | integral, decimal, expo ->
            let number = List.append  (List.append integral decimal) expo
            let number = System.String (number |> List.toArray |> Array.map (function | TokChar (_, ch) -> ch | _ -> failwith "unreachable"))

            if number.Length <> 0
            then TokReal64 (di, Convert.ToDouble number), nextList
            else raise (tokExcept (di, "invalid floating number"))

    match nextList with
    | TokChar (di, ch) :: l when isAlpha ch -> raise (tokExcept (di, "invalid number: followed by a alpha or special character, separator or space is needed"))
    | _ -> token, nextList
   
let rec private skipWS (str: Token list) : Token list =
    let isWS = function
        | ' '
        | '\n'
        | '\r'
        | '\t' -> true
        | _    -> false

    match str with
    | TokChar (_, ch) :: t when isWS ch -> skipWS t
    | _ -> str


let rec private reduceTuple (e: Token) =
    match e with
    | TokTuple (di, []) -> TokUnit di
    | TokTuple (di, (h :: [])) -> reduceTuple h
    | _ -> e

let reduceExpression di (toks: Token list) =
    match toks with
    | h :: [] -> h
    | _       -> TokExpression (di, toks)

let curr2Tup f x y = f(x, y)

let rec private readListOfTokens di splitterChar endingChar (allowSplitterAtEnd: bool) (boxFunc: DebugInfo -> Token list -> Token) (acc: Token list) (str: Token list) : Token * Token list =
    let str = skipWS str
    match str with
    | []                                 -> raise (tokExcept (di, sprintf "%s doesn't have an end '%c'" ((boxFunc di []).TokenTypeName) endingChar))
    | TokChar (_, ch) :: t when ch = endingChar -> let exp = (boxFunc di (acc |> List.rev)) in exp, t
    | _ ->
        let rec readExpression (acc: Token list) (str: Token list) : Token * Token list =
            let str = skipWS str
            match str with
            | []                                                        -> raise (tokExcept (di, sprintf "%s doesn't have an end '%c'" ((boxFunc di []).TokenTypeName) endingChar))
            | TokChar (_, ch) :: t when ch = endingChar                 -> let exp = acc |> List.rev |> reduceExpression di in exp, str
            | TokChar (_, ch) :: t when ch = splitterChar               ->
                match skipWS t with
                | TokChar (di, ch) :: t when ch = endingChar && allowSplitterAtEnd = false -> raise (tokExcept (di, sprintf "%s ends with splitter '%c'" ((boxFunc di []).TokenTypeName) splitterChar))
                | _     -> let exp = acc |> List.rev |> reduceExpression di in exp, t
            | _                                                         -> let tok, nextList = nextToken str in readExpression (tok :: acc) nextList

        // this call will reduce expression of the form "a b c ..."
        let tok, nextList = readExpression [] str
        // and this one will close the list
        readListOfTokens di splitterChar endingChar allowSplitterAtEnd boxFunc (tok :: acc) nextList

and private readScope di = readListOfTokens di ';' '}' true (curr2Tup TokScope)

and private readTuple di = readListOfTokens di ',' ')' false (curr2Tup (TokTuple >> reduceTuple))

and private readList  di = readListOfTokens di ';' ']' true (curr2Tup TokList)

and private nextToken (str: Token list) : Token * Token list =
    let str = skipWS str
    match str with
    | TokChar (di, '\'') :: t -> readChar        t
    | TokChar (di, '"' ) :: t -> readString   [] t // "
    | TokChar (di, '(' ) :: t -> readTuple    di [] t
    | TokChar (di, '{' ) :: t -> readScope    di [] t
    | TokChar (di, '[' ) :: t -> readList     di [] t
    | TokChar (di, sign) :: TokChar (_, ch) :: t when (sign = '+' || sign = '-') && isDigit ch -> readNumber di str
    | TokChar (di, ch  ) :: t when isDigit ch   -> readNumber di str
    | TokChar (di, ch  ) :: t when isAlpha ch || ch = '#' -> readSymbolAlphaNum di [] str
    | TokChar (di, ch  ) :: t when isSpecial ch -> readSymbolSpecial di [] str
    | h :: t -> raise (tokExcept (h.DebugInfo, "ill formed Schizo Token"))
    | [] -> failwith "reached the end of the stream unexpectdly"

type Token
with
    static member private tokenize_ (str: Token list) : Token list =
        let rec loop (acc: Token list) (str: Token list) : Token list =
            match skipWS str with
            | [] -> List.rev acc
            | _ ->
                let tok, nextList = nextToken str
                loop (tok :: acc) nextList
        
        loop [] str

    static member tokenize (streamId: string) (str: string) : Token list =
        if str.Length <> 0
        then
            str
            |> Seq.toList
            |> List.fold
                (fun (di: DebugInfo, acc) ch ->
                    let di =
                        { di with
                            Offset  = di.Offset + 1
                            Line    = if ch = '\n' then di.Line + 1 else di.Line
                        }
                    (di, TokChar (di, ch) :: acc))
                (DebugInfo.empty streamId, [])
            |> snd
            |> List.rev
            |> Token.tokenize_
        else []

let test =
    [|
        "()"
        "'c'"
        "123"
        "0"
        "123.4"
        "123.4e+10"
        "123.4e10"
        "symbol"
        "symbol 1 2 a 3.5 b"
        "\"string\""
        "( )"
        "'\\n'"
        "a b c d"
        "{a b c}"
        "(abc def gfhi)"
        "(abc123 d045ef gf.hi)"
        "(s 123) () 456 a b 123.0 hh !hello $bla%bla()aha:something"
        "(a b (c 10 12 de) (f 10 11 12))"
        "false"
        "true"
        "d true(false)"
        "hello { a b c d; e f g; 1; 2; 3 }"
        "{ a b c d }"
        "d (1, 2, 3) (1) (ab cd, defg)"
        "e (1, 2, 3) (4, (6, 7)) (ab cd, defg)"
        "[1]"
        "[1; 2; 3; ab cd; (1, 2); (1) ]"
        "[1; 2; 3; ab cd; (1, 2); (1); []]"
        "[1; 2; 3 ; ab cd ; (1, 2 ); (1) ]"
        "module A { funA : a -> b -> c; funB : d }"
        "1 + 2"
        "1 + 2 * 5"
        "1 - 10 / 30 * 50"
        "hello world + 10 / 20 - 20 * 50 | abc d e fg hi 12 --> right"
        "[1; 2; 3; 4]"
        "{ a b; c }"
        "module Type {}"
        "datatype Type = Type #a #b #c#"
    |]
    |> Array.map (Token.tokenize "")
    |> Array.iter (printfn "%A")
