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

module Schizo.Parser

open System

open Schizo.Tokens
open Schizo.AST

type RetVal<'T> =
    | Value of 'T
    | Error of string

type State = {
    CurrentFile : string
    Types       : Map<string, Ty>    // no recursive types yet
    Modules     : Module list
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
    static member resolveType (s: State) (tyName: string) : RetVal<Ty> = 
        match s.Types.TryFind tyName with
        | Some (Ty.Alias tyA) -> State.resolveType s tyA.Ty.Name
        | Some ty -> Value ty
        | None -> Error (sprintf "type %s not found" tyName)
    
    member x.resolve (tyName: string) =
        State.resolveType x tyName

    member x.addQualifiedType(ty : Ty) = { x with Types = x.Types.Add(ty.Name, ty) }
    member x.addUnqualifiedAlias (name, tyName) =
        match x.resolve tyName with
        | Value ty -> Value { x with Types = x.Types.Add(name, ty) }
        | Error e  -> Error e
    member x.addType(modName, ty) = x.addQualifiedType(ty).addUnqualifiedAlias(stripModName ty.Name, ty.Name)

let parseEnum di (modName: string) (tl: Token list) : TyEnum =
    let parseEnumCase di (tl: Token list) : TyEnumCase * Token list =
        match tl with
        | Token.TokExpression (di, tl) :: t ->
            match tl with
            | Token.TokType (di, ident) :: Token.TokOperator (_, "=") :: Token.TokInt64 (_, value) :: [] ->
                { TyEnumCase.Name   = ident
                  TyEnumCase.Info   = di
                  TyEnumCase.Value  = value
                }, t
            | _ -> raise (tokExcept (di, "Enum Case Syntax: invalid enum case syntax, expected enum identifier and a value got something else"))
        | _ -> raise (tokExcept (di, "Enum Case Expression: invalid enum case syntax, expected enum identifier and a value got something else"))


    match tl with
    | Token.TokType (di, enumName) :: Token.TokScope(_, enumScope) :: [] ->
        let rec loop (cases: TyEnumCase list) (tl: Token list) =
            match tl with
            | [] -> cases
            | h :: _  ->
                let case, tl = parseEnumCase h.DebugInfo tl
                loop (case :: cases) tl

        let cases = loop [] enumScope

        let validate (cases: TyEnumCase list) =

            let caseMap = // no two cases should share the same name
                cases
                |> List.fold(fun (s: Set<string>) m ->
                    match s.Contains m.Name with
                    | true  -> raise (tokExcept(di, sprintf "in enum %s.%s case %s is defined more than once" modName enumName m.Name))
                    | false -> s.Add m.Name) Set.empty
            cases
        
        let cases = validate cases

        { TyEnum.Name   = modName + "." + enumName
          TyEnum.Info   = di
          TyEnum.Cases  = cases |> List.rev |> List.toArray
        }
    | xs -> raise (tokExcept(di, "enum expects a name and a scope, got something else"))

let rec parseType (state: State) (tl: Token list) : Ty =
    let parseTypePartial (tl: Token list) : DebugInfo * Ty * Token list =
        match tl with
        | Token.TokUnit di :: tl    ->
            di, Ty.Unit, tl

        | Token.TokType (di, modName) :: Token.TokOperator(_, ".") :: Token.TokType(_, typeName) :: tl  ->
            let ty = state.resolve (modName + "." + typeName)
            match ty with
            | Value ty -> di, ty, tl
            | Error e -> raise (tokExcept (di, e))

        | Token.TokType (di, typeName) :: tl    ->
            let ty = state.resolve typeName
            match ty with
            | Value ty -> di, ty, tl
            | Error e -> raise (tokExcept (di, e))

        | Token.TokTuple (di, tuple) :: tl  ->
            di, parseTuple di state tuple, tl

        | h :: _ -> raise (tokExcept(h.DebugInfo, "while parsing type: invalid type syntax"))
        | [] -> failwith "parseType: unreachable"

    let parseArrayType (tl: Token list) : DebugInfo * Ty * Token list =
        let di, ty, tl = parseTypePartial tl
        match tl with
        | Token.TokList (_, []) :: tl ->
            di
            , Ty.Array { TyArray.Info = di
                         TyArray.Type = ty
                       }, tl
        | _ -> di, ty, tl

    let rec parseTypeComplete (tl: Token list) : Ty * Token list =
        let di, ty, tl = parseArrayType tl
        match tl with
        | [] -> ty, []
        | Token.TokOperator (_, "->") :: tl -> 
            let retTy, tl = parseTypeComplete tl
            Ty.Function { TyFunction.Info = di
                          TyFunction.Param  = ty
                          TyFunction.RetVal = retTy
                        }, tl

        | h :: _ -> raise (tokExcept (h.DebugInfo, "type can either be one of the formats: Type, (Type, Type, ...) or a Type -> Type"))

    let di =
        match tl with
        | h :: _ -> h.DebugInfo
        | [] -> failwith "parseType: unhandled case, this shouldn't happen"

    match parseTypeComplete tl with
    | ty, [] -> ty
    | _ -> raise (tokExcept (di, "parseType: expected the type to be complete, got more"))

and parseTuple (di: DebugInfo) (state: State) (tl: Token list) : Ty =
    let rec loop (types: Ty list) (tl: Token list) =
        match tl with
        | [] -> types |> List.rev
        | h :: t  ->
            let ty =
                match h with
                | Token.TokType (di, ident) ->
                    let ty = state.resolve ident
                    match ty with
                    | Value ty -> ty
                    | Error e  -> raise (tokExcept (di, e))
                | Token.TokIdentifier (_, ident) -> failwith "parseTuple: implement named field"
                | Token.TokExpression (di, exp) -> parseType state exp
                | _ -> raise (tokExcept (h.DebugInfo, "parseTuple: unhandled case"))
            loop (ty :: types) t

    let tupleTys = loop [] tl
    { TyTuple.Info = di
      TyTuple.Params    = tupleTys |> List.toArray |> Array.map(fun ty -> None, ty)
    } |> Ty.Tuple

let parseField di (state: State) (tl: Token list) : TyField =
    match tl with
    | Token.TokIdentifier (di, fieldName) :: Token.TokOperator (_, ":") :: tl ->
        let ty = parseType state tl
        { TyField.Name  = fieldName
          TyField.Info  = di
          TyField.Type  = ty }

    | h :: _ -> raise (tokExcept (h.DebugInfo, "invalid field syntax"))
    | [] -> raise (tokExcept (di, "there is no field!!!"))

let parseFieldScope (di: DebugInfo) (state: State) (modName: string, tyName: string) (scope: Token list) : TyField [] =
    let fields =
        scope
        |> List.map(fun tok ->
            match tok with
            | Token.TokExpression (di, tl) -> parseField di state tl
            | _ -> raise (tokExcept(di, "parseFieldScope: expected \"field : Type\" got something else")))
    
    let validate (fields: TyField list) =
        let fieldMap = // no two fieldss should share the same name
            fields
            |> List.fold(fun (s: Set<string>) m ->
                match s.Contains m.Name with
                | true  -> raise (tokExcept(di, sprintf "in %s.%s field %s is defined more than once" modName tyName m.Name))
                | false -> s.Add m.Name) Set.empty
        fields
    
    validate fields
    |> List.toArray

let parseRecord di (modName: string) (state: State) (tl: Token list) : TyRecord =       
    match tl with
    | Token.TokType (di, recordName) :: Token.TokScope (_, recordScope) :: [] ->

        let fields = parseFieldScope di state (modName, recordName) recordScope

        { TyRecord.Name = modName + "." + recordName
          TyRecord.Info = di
          TyRecord.Fields   = fields
        }
    | _ -> raise (tokExcept (di, "parseRecord: invalid record syntax"))

let parseCase di (state: State) (tl: Token list) : TyUnionCase =
    match tl with
    | Token.TokType (di, caseName) :: tl ->
        let ty = parseType state tl
        { TyUnionCase.Name  = caseName
          TyUnionCase.Info  = di
          TyUnionCase.Type  = ty }

    | _ -> raise (tokExcept (di, "invalid field syntax"))

let parseUnion di (modName: string) (state: State) (tl: Token list) : TyUnion =
    match tl with
    | Token.TokType (di, unionName) :: Token.TokScope (_, unionScope) :: [] ->
        let cases =
            unionScope
            |> List.map (fun tok ->
                match tok with
                | Token.TokExpression (di, tl) -> parseCase di state tl
                | _ -> raise (tokExcept (di, "expected \"Case Type\" got something else")))

        let validate (cases: TyUnionCase list) =
            let caseMap = // no two cases should share the same name
                cases
                |> List.fold(fun (s: Set<string>) m ->
                    match s.Contains m.Name with
                    | true  -> raise (tokExcept(di, sprintf "in union %s.%s case %s is defined more than once" modName unionName m.Name))
                    | false -> s.Add m.Name) Set.empty
            cases
        
        let cases = validate cases

        { TyUnion.Name  = modName + "." + unionName
          TyUnion.Info = di
          TyUnion.Cases = cases |> List.toArray
        }
    | _ -> raise (tokExcept (di, "invalid union syntax"))

let parseInterface di (modName: string) (state: State) (tl: Token list) : TyInterface =
    match tl with
    | Token.TokType (di, ifaceName) :: Token.TokScope(_, ifaceScope) :: [] ->
        let methods = parseFieldScope di state (modName, ifaceName) ifaceScope

        let validate (methods: TyField []) =
            let methods =   // all fields should be methods
                methods
                |> Array.map (fun m ->
                    match m.Type with
                    | Ty.Function _ -> m
                    | _ -> raise (tokExcept (di, sprintf "validateInterface: interface %s has members that are not methods" ifaceName)))
            methods
        
        let methods = validate methods

        { TyInterface.Name = modName + "." + ifaceName
          TyInterface.Info = di
          TyInterface.Methods   = methods
        }
    | _ -> raise (tokExcept (di, "parseInterface: invalid interface syntax"))

let parseObject di (modName: string) (state: State) (tl: Token list) : TyObject =
    let getIfaceList (di, objName) (ifaceList: Token list) =
        let ifaces =
            ifaceList
            |> List.map (fun t ->
                match t with
                | Token.TokExpression (di, ifaceList) -> parseType state ifaceList
                | Token.TokType (di, typeName) ->
                    match state.resolve typeName with
                    | Value ty -> ty
                    | Error e  -> raise (tokExcept (di, sprintf "type %s not found" typeName))
                | _ -> raise (tokExcept (di, "invalid type syntax")))
            |> List.map(fun ty ->
                match ty with
                | Ty.Interface iface -> iface
                | _ -> raise (tokExcept (di, sprintf "expected an interface, type %O is not" ty.Name)))

        let validate (ifaces: TyInterface list) =

            let ifaceMap = // no two cases should share the same name
                ifaces
                |> List.fold(fun (s: Set<string>) m ->
                    match s.Contains m.Name with
                    | true  -> raise (tokExcept(di, sprintf "in object %s.%s interface %s is defined more than once" modName objName m.Name))
                    | false -> s.Add m.Name) Set.empty
            ifaces
        
        validate ifaces

    match tl with
    | Token.TokType (di, objName) :: Token.TokIdentifier(_, "implements") :: Token.TokTuple(_, ifaceList) :: [] ->
        let ifaces = getIfaceList (di, objName) ifaceList
        
        { TyObject.Info = di
          TyObject.Interfaces   = ifaces |> List.toArray
          TyObject.Name = objName
          TyObject.PrivMembers  = [||]
        }

    | Token.TokType (di, objName) :: Token.TokIdentifier(_, "implements") :: Token.TokTuple(_, ifaceList) :: Token.TokIdentifier(_, "with") :: Token.TokScope (_, scope) :: [] ->
        let ifaces = getIfaceList (di, objName) ifaceList
        let members = parseFieldScope di state (modName, objName) scope
        
        { TyObject.Info = di
          TyObject.Interfaces   = ifaces |> List.toArray
          TyObject.Name = objName
          TyObject.PrivMembers  = members
        }

    | _ -> raise (tokExcept (di, "invalid object syntax"))

let parseAlias di (modName: string) (state: State) (tl: Token list) : TyAlias =
    match tl with
    | Token.TokType (di, tyName) :: Token.TokIdentifier (_, "for") :: tl ->
        let ty = parseType state tl
        { TyAlias.Name  = tyName
          TyAlias.Ty    = ty
          TyAlias.Info  = di
        }
    | _ -> raise (tokExcept (di, "invalid alias syntax"))

let rec parseUse di (modFolder: string) (state: State) (tl: Token list) : string * State =
    match tl with
    | Token.TokType (di, modName) :: [] -> 
        let m  = parseModuleFile modFolder modName state 

        modName,
        { state with
            State.Types =
                m.Decls
                |> Array.fold (fun (mt: Map<string, Ty>) (decl : Declaration) ->
                    match decl with
                    | Declaration.Use _ -> mt
                    | _ ->
                        let ty =
                            match decl with
                            | Declaration.Interface ty -> ty |> Ty.Interface
                            | Declaration.Record    ty -> ty |> Ty.Record
                            | Declaration.Union     ty -> ty |> Ty.Union
                            | Declaration.Enum      ty -> ty |> Ty.Enum
                            | Declaration.Object    ty -> ty |> Ty.Object
                            | Declaration.Alias     ty -> ty |> Ty.Alias
                            | _ -> failwith "unreachable"
                        let name = ty.Name
                        if name.Contains(modName)
                        then
                            let shortName = name.Substring(0, modName.Length + 1)
                            mt.Add(name, ty)
                              .Add(shortName, ty)
                        else mt) state.Types
        }
    | _ -> raise (tokExcept (di, "Error: expecting use ModuleName"))

and parseModule (modFolder: string) (state: State) (tl: Token list) : Module =
    let parseModEntry di (innerState: State) (modName: string) (tl: Token list) : State * Declaration =
        match tl with
        | Token.TokIdentifier (di, "enum")      :: t ->
            let decl = parseEnum di modName t 
            let ty = decl |> Ty.Enum
            let state = innerState.addType (modName, ty)
            match state with
            | Value state -> state, decl |> Declaration.Enum
            | Error e     -> raise (tokExcept (di, e))

        | Token.TokIdentifier (di, "record")    :: t ->
            let decl = parseRecord di modName innerState t
            let ty = decl |> Ty.Record
            let state = innerState.addType (modName, ty)
            match state with
            | Value state -> state, decl |> Declaration.Record
            | Error e     -> raise (tokExcept (di, e))

        | Token.TokIdentifier (di, "union")     :: t ->
            let decl = parseUnion di modName innerState t
            let ty = decl |> Ty.Union
            let state = innerState.addType (modName, ty)
            match state with
            | Value state -> state, decl |> Declaration.Union
            | Error e     -> raise (tokExcept (di, e))

        | Token.TokIdentifier (di, "interface") :: t ->
            let decl = parseInterface di modName innerState t
            let ty = decl |> Ty.Interface
            let state = innerState.addType (modName, ty)
            match state with
            | Value state -> state, decl |> Declaration.Interface
            | Error e     -> raise (tokExcept (di, e))

        | Token.TokIdentifier (di, "object")    :: t ->
            let decl = parseObject di modName innerState t
            let ty = decl |> Ty.Object
            let state = innerState.addType (modName, ty)
            match state with
            | Value state -> state, decl |> Declaration.Object
            | Error e     -> raise (tokExcept (di, e))

        | Token.TokIdentifier (di, "alias")     :: t ->
            let decl = parseAlias di modName innerState t
            let ty = decl |> Ty.Alias
            let state = innerState.addType (modName, ty)
            match state with
            | Value state -> state, decl |> Declaration.Alias
            | Error e     -> raise (tokExcept (di, e))

        | Token.TokIdentifier (di, "use")       :: t ->
            let modName, state = parseUse di modFolder innerState t
            state, Declaration.Use (di, modName)

        | xs -> raise (tokExcept (di, sprintf "invalid token %O at this level in module. Expected one of {enum, record, union, interface, object, alias, use}" xs))

    match tl with
    | Token.TokIdentifier(_, "module") :: Token.TokType (di, modName) :: TokScope (_, modScope) :: [] ->
        let state, entries =
            modScope
            |> List.fold(fun (state: State, entries: Declaration list) (t: Token) ->
                match t with
                | Token.TokExpression (di, tl) ->
                    let state, entry = parseModEntry di state modName tl
                    state, entry :: entries

                | t -> raise (tokExcept (t.DebugInfo, "module entry expected, got something else"))) (state, [])

        { Module.Name   = modName
          Module.Info   = di
          Module.Decls  = entries |> List.rev |> List.toArray
        }
    | _ -> failwith "error parsing module"


and parseModuleFile (modFolder: string) (fName: string) (state: State) : Module =
    try
        let moduleFileName = modFolder + "/" + fName + ".szm"
        let stream = IO.File.ReadAllText moduleFileName
        let tl = Token.tokenize moduleFileName stream
        parseModule modFolder state tl
    with
        | TokenException diMsg ->
            diMsg
            |> List.map(fun (di, msg) ->
                printfn "%s:%d - %s" di.StreamId di.Line msg)
            |> ignore

            failwith (sprintf "%A" diMsg)
        | e ->
            failwith (sprintf "Error reading file: %s with %s" fName e.Message)

         