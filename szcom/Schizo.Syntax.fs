//
// SchizoCOM F# Reference Compiler
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

module Schizo.Syntax

open System

open Schizo.Tokens
open Schizo.AST

type State = {
    CurrentFile : string
    Types       : Map<string, Ty>    // no recursive types yet
    Modules     : TyModule list
    Streams     : string list
} with
    static member empty() = {
        CurrentFile = ""
        Types   = 
            [| "()",    Ty.Unit
               "Bool",  Ty.Boolean
               "Char",  Ty.Char
               "Int",   Ty.Int64
               "Real",  Ty.Real64 |]
            |> Map.ofArray
        Modules = []
        Streams = []
    }
    static member resolveType (s: State) (tyName: string) : Ty = 
        match s.Types.TryFind tyName with
        | Some (Ty.Alias tyA) -> State.resolveType s tyA.Ty.Name
        | Some ty -> ty
        | None -> failwith (sprintf "type %s not found" tyName)
    
    member x.resolve (tyName: string) =
        State.resolveType x tyName

    member x.addQualifiedType(modName, ty : Ty) = { x with Types = x.Types.Add(modName + "." + ty.Name, ty) }
    member x.addUnqualifiedAlias(name, tyName) = { x with Types = x.Types.Add(name, x.resolve tyName) }
    member x.addType(modName, ty) = x.addQualifiedType(modName, ty).addUnqualifiedAlias(ty.Name, modName + "." + ty.Name)

let parseEnum (modName: string) (tl: Token list) : TyEnum =
    let parseEnumCase (tl: Token list) : TyEnumCase * Token list =
        match tl with
        | Token.TokExpression (_, tl) :: t ->
            match tl with
            | Token.TokType (di, ident) :: Token.TokOperator (_, "=") :: Token.TokInt64 (_, value) :: [] ->
                { TyEnumCase.Name   = ident
                  TyEnumCase.Info   = di
                  TyEnumCase.Value  = value
                }, t
            | _ -> failwith "Enum Case Syntax: invalid enum case syntax, expected enum identifier and a value got something else"
        | _ -> failwith "Enum Case Expression: invalid enum case syntax, expected enum identifier and a value got something else"


    match tl with
    | Token.TokType (di, enumName) :: Token.TokScope(_, enumScope) :: [] ->
        let rec loop (cases: TyEnumCase list) (tl: Token list) =
            match tl with
            | [] -> cases
            | _  ->
                let case, tl = parseEnumCase tl
                loop (case :: cases) tl
        let cases = loop [] enumScope
        { TyEnum.Name   = modName + "." + enumName
          TyEnum.Info   = di
          TyEnum.Cases  = cases |> List.rev |> List.toArray
        }
    | xs -> failwith "enum expects a name and a scope, got something else"

let rec parseType (state: State) (tl: Token list) : Ty =
    let rec parseTypePartial (tl: Token list) : Ty * Token list =
        match tl with
        | Token.TokUnit di :: [] -> Ty.Unit, []

        | Token.TokUnit di :: tl ->
            let retTy, tl = parseTypePartial tl
            Ty.Function { TyFunction.Info   = di
                          TyFunction.Param  = Ty.Unit
                          TyFunction.RetVal = retTy
                        }, tl

        | Token.TokType (di, modName) :: Token.TokOperator(_, ".") :: Token.TokType(_, typeName) :: Token.TokOperator (_, "->") :: tl ->
            let retTy, tl = parseTypePartial tl
            Ty.Function { TyFunction.Info = di
                          TyFunction.Param  = state.resolve (modName + "." + typeName)
                          TyFunction.RetVal = retTy
                        }, tl

        | Token.TokType (di, modName) :: Token.TokOperator(_, ".") :: Token.TokType(_, typeName) :: [] ->
             state.resolve (modName + "." + typeName), []

        | Token.TokType (di, typeName) :: Token.TokOperator (_, "->") :: tl -> 
            let retTy, tl = parseTypePartial tl
            Ty.Function { TyFunction.Info = di
                          TyFunction.Param  = state.resolve typeName
                          TyFunction.RetVal = retTy
                        }, tl
        | Token.TokType (di, typeName) :: [] -> state.resolve typeName, []
        | Token.TokTuple (di, tuple) :: Token.TokOperator (_, "->") :: tl ->
            let retTy, tl = parseTypePartial tl
            Ty.Function { TyFunction.Info = di
                          TyFunction.Param  = parseTuple di state tuple
                          TyFunction.RetVal = retTy
                        }, tl
        | Token.TokTuple (di, tuple) :: tl -> parseTuple di state tuple, tl

        | _ -> failwith "type can either be a type, a tuple or a function type"

    match parseTypePartial tl with
    | ty, [] -> ty
    | _ -> failwith "parseType: expected the type to be complete, got more"

and parseTuple (di: DebugInfo) (state: State) (tl: Token list) : Ty =
    let rec loop (types: Ty list) (tl: Token list) =
        match tl with
        | [] -> types |> List.rev
        | h :: t  ->
            let ty =
                match h with
                | Token.TokType (_, ident) -> state.resolve ident
                | Token.TokIdentifier (_, ident) -> failwith "parseTuple: implement named field"
                | Token.TokExpression (_, exp) -> parseType state exp
                | _ -> failwith "parseTuple: unhandled case"
            loop (ty :: types) t

    let tupleTys = loop [] tl
    { TyTuple.Info = di
      TyTuple.Params    = tupleTys |> List.toArray |> Array.map(fun ty -> None, ty)
    } |> Ty.Tuple

let parseField (state: State) (tl: Token list) : TyField =
    match tl with
    | Token.TokIdentifier (di, fieldName) :: Token.TokOperator (_, ":") :: tl ->
        let ty = parseType state tl
        { TyField.Name  = fieldName
          TyField.Info  = di
          TyField.Type  = ty }

    | _ -> failwith "invalid field syntax"

let parseRecord (modName: string) (state: State) (tl: Token list) : TyRecord =       
    match tl with
    | Token.TokType (di, recordName) :: Token.TokScope (_, recordScope) :: [] ->
        let fields =
            recordScope
            |> List.map(fun tok ->
                match tok with
                | Token.TokExpression (di, tl) -> parseField state tl
                | _ -> failwith "parseRecord: expected \"field : Type\" got something else")
        { TyRecord.Name = modName + "." + recordName
          TyRecord.Info = di
          TyRecord.Fields   = fields |> List.toArray
        }
    | _ -> failwith "parseRecord: invalid recod syntax"

let parseCase (state: State) (tl: Token list) : TyUnionCase =
    match tl with
    | Token.TokType (di, caseName) :: tl ->
        let ty = parseType state tl
        { TyUnionCase.Name  = caseName
          TyUnionCase.Info  = di
          TyUnionCase.Type  = ty }

    | _ -> failwith "invalid field syntax"

let parseUnion (modName: string) (state: State) (tl: Token list) : TyUnion =
    match tl with
    | Token.TokType (di, unionName) :: Token.TokScope (_, unionScope) :: [] ->
        let cases =
            unionScope
            |> List.map (fun tok ->
                match tok with
                | Token.TokExpression (di, tl) -> parseCase state tl
                | _ -> failwith "parseUnion: expected \"Case Type\" got something else")
        { TyUnion.Name  = modName + "." + unionName
          TyUnion.Info = di
          TyUnion.Cases = cases |> List.toArray
        }
    | _ -> failwith "parseUnion: invalid union syntax"

let parseInterface (modName: string) (state: State) (tl: Token list) : TyInterface =
    match tl with
    | Token.TokType (di, ifaceName) :: Token.TokScope(_, ifaceScope) :: [] ->
        let methods =
            ifaceScope
            |> List.map (fun tok ->
                match tok with
                | Token.TokExpression (di, tl) -> parseField state tl
                | _ -> failwith "parseInterface: expected \"method: function type\" got something else")
        { TyInterface.Name = modName + "." + ifaceName
          TyInterface.Info = di
          TyInterface.Methods   = methods |> List.toArray
        }
    | _ -> failwith "parseInterface: invalid interface syntax"

let rec parseUse (state: State) (tl: Token list) : State =
    match tl with
    | Token.TokType (di, modName) :: [] -> 
        let m  = parseModuleFile state (modName + ".szm")

        { state with
            State.Types =
                m.Types
                |> Array.fold (fun (mt: Map<string, Ty>) (ty : Ty) ->
                    let name = ty.Name
                    if name.Contains(modName)
                    then
                        let shortName = name.Substring(0, modName.Length + 1)
                        mt.Add(ty.Name, ty)
                          .Add(shortName, ty)
                    else mt) state.Types
        }
    | _ -> failwith "Error: expecting use ModuleName"

and parseModule (state: State) (tl: Token list) : TyModule =
    let parseModEntry (innerState: State) (modName: string) (tl: Token list) : State * Ty option =
        match tl with
        | Token.TokIdentifier (di, "enum")      :: t ->
            let ty = parseEnum modName t |> Ty.Enum
            innerState.addType (modName, ty), Some ty

        | Token.TokIdentifier (di, "record")    :: t ->
            let ty = parseRecord modName innerState t |> Ty.Record
            innerState.addType (modName, ty), Some ty

        | Token.TokIdentifier (di, "union")     :: t ->
            let ty = parseUnion modName innerState t |> Ty.Union
            innerState.addType (modName, ty), Some ty

        | Token.TokIdentifier (di, "interface") :: t ->
            let ty = parseInterface modName innerState t |> Ty.Interface
            innerState.addType (modName, ty), Some ty

        | Token.TokIdentifier (di, "object")    :: t -> failwith "implement"
        | Token.TokIdentifier (di, "alias")     :: t -> failwith "implement"

        | Token.TokIdentifier (di, "use")       :: t -> parseUse innerState t, None

        | xs -> failwith (sprintf "invalid token %O at this level in module expected one of {enum, record, union, interface, object, alias, use}" xs)

    match tl with
    | Token.TokIdentifier(_, "module") :: Token.TokType (di, modName) :: TokScope (_, modScope) :: [] ->
        let state, entries =
            modScope
            |> List.fold(fun (state: State, entries: Ty list) (t: Token) ->
                match t with
                | Token.TokExpression (_, tl) ->
                    let state, entry = parseModEntry state modName tl
                    match entry with
                    | Some entry -> state, entry :: entries
                    | None       -> state, entries
                | _ -> failwith "module entry expected, got something else") (state, [])

        { TyModule.Name = modName
          TyModule.Info = di
          TyModule.Types = entries |> List.rev |> List.toArray
        }
    | _ -> failwith "error parsing module"


and parseModuleFile (state: State) (fName: string) : TyModule =
    try
        let stream = IO.File.ReadAllText fName
        let tl = Token.tokenize 0 stream
        parseModule state tl
    with e ->
        failwith (sprintf "Error reading file: %s with %s" fName e.Message)

         