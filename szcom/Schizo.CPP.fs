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
module Schizo.CPP

open System
open Schizo.Tokens
open Schizo.AST

let indent (n: int) =
    let sb = Text.StringBuilder()
    let sb = [| 0..n - 1 |]
             |> Array.fold(fun (sb : Text.StringBuilder) _ -> sb.Append "\t") sb
    sb.ToString()

let makeFirstLetterUpper (s: string) =
    (sprintf "%c" (Char.ToUpper s.[0])) + s.[1..]

let rec typeName (tupleAsArgs: bool) (ty: Ty) =
    let replaceDot (s: string) = s.Replace (".", "::")

    match ty with
    | Ty.Unit        _ -> "void"
    | Ty.Boolean     _ -> "Bool"
    | Ty.Char        _ -> "Char"
    | Ty.Int64       _ -> "Int"
    | Ty.Real64      _ -> "Real"
    | Ty.Interface   v -> replaceDot v.Name
    | Ty.Record      v -> replaceDot v.Name
    | Ty.Union       v -> replaceDot v.Name
    | Ty.Enum        v -> replaceDot v.Name
    | Ty.Function    v ->
        let args = typeName true v.Param
        let retVal  = typeName false v.RetVal
        sprintf "Core::Function<%s(%s)>" retVal args
    | Ty.Object      v -> replaceDot v.Name
    | Ty.Tuple       v ->
        let types =
            v.Params
            |> Array.map(fun (_, t) -> typeName false t)
        let txt =
            types
            |> Array.fold(fun s t ->
                match s with
                | "" -> sprintf "%s" t
                | _  -> sprintf "%s, %s" s t) ""
        match tupleAsArgs with
        | false -> sprintf "Core::Tuple%d<%s>" types.Length txt
        | true  -> txt
    | Ty.Array       v -> sprintf "Core::Array<%s>" (replaceDot v.Type.Name)
    | Ty.Alias       v -> replaceDot v.Name

let constTypeName (tupleAsArgs: bool) (ty: Ty) =
    match ty with
    | Ty.Unit        _ 
    | Ty.Boolean     _ 
    | Ty.Char        _ 
    | Ty.Int64       _ 
    | Ty.Real64      _ -> typeName tupleAsArgs ty
    | _                -> sprintf "const %s&" (typeName tupleAsArgs ty)

let dumpMethodType (f: TyField) =
    match f.Type with
    | Ty.Function { TyFunction.Param = param; TyFunction.RetVal = retVal } ->
        sprintf "%s %s(%s)" (typeName false retVal) f.Name (typeName true param)
    | _ -> typeName false f.Type

let dumpInterface (sb: Text.StringBuilder) (tyIFace : TyInterface) =
    let sb = sb.Append (sprintf "class %s {\npublic:\n" (stripModName tyIFace.Name))
    let sb =
        tyIFace.Methods
        |> Array.fold(fun (sb: Text.StringBuilder) f ->
            sb.Append (sprintf "virtual %s%s = 0;\n" (indent 1) (dumpMethodType f))) sb
    sb.Append "};\n"

let dumpRecord (sb: Text.StringBuilder) (tyRecord : TyRecord) =
    let recordConstructorArgs =
        tyRecord.Fields
        |> Array.fold (fun (s: string) f ->
            match s with
            | "" -> sprintf "%s %s" (typeName false f.Type) f.Name
            | _  -> sprintf "%s, %s %s" s (constTypeName false f.Type) f.Name) ""

    let recordConstructorArgInits =
        tyRecord.Fields
        |> Array.fold (fun (s: string) f ->
            match s with
            | "" -> sprintf "%s_(%s)" f.Name f.Name
            | _  -> sprintf "%s, %s_(%s)" s f.Name f.Name) ""

    let recordWiths n =
        tyRecord.Fields
        |> Array.fold (fun (s: string) f ->
            let parameters =
                tyRecord.Fields
                |> Array.fold(fun (s: string) f2 ->
                    let name =
                        if f.Name = f2.Name
                        then f.Name
                        else f.Name + "_"
                    match s with
                    | "" -> sprintf "%s" name
                    | _  -> sprintf "%s, %s" s name) ""

            sprintf "%s%s%s with%s(%s %s) const { return %s(%s); }\n" s (indent n) (stripModName tyRecord.Name) (makeFirstLetterUpper f.Name) (typeName false f.Type) f.Name (stripModName tyRecord.Name) parameters) ""

    let copyConstructorFields =
        tyRecord.Fields
        |> Array.fold (fun (s: string) f ->
            match s with
            | "" -> sprintf "%s_(other.%s_)" f.Name f.Name
            | _  -> sprintf "%s, %s_(other.%s_)" s f.Name f.Name) ""

    let sb = sb.Append (sprintf "struct %s {\n" (stripModName tyRecord.Name))

    let sb =
        (sb.Append (sprintf "%s// constructor\n" (indent 1)))
           .Append (sprintf "%s%s(%s) : %s {}\n\n" (indent 1) (stripModName tyRecord.Name) recordConstructorArgs recordConstructorArgInits)
    let sb =
        (sb.Append (sprintf "%s// copy constructor\n" (indent 1)))
           .Append (sprintf "%s%s(const %s& other) : %s {}\n\n" (indent 1) (stripModName tyRecord.Name) (stripModName tyRecord.Name) copyConstructorFields)

    let sb =
        (sb.Append (sprintf "%s// With copy constructor\n" (indent 1)))
           .Append (sprintf "%s\n\n" (recordWiths 1))

    let sb =
        tyRecord.Fields
        |> Array.fold(fun (sb: Text.StringBuilder) f ->
            sb.Append (sprintf "%s%s %s() const { return %s_; }\n" (indent 1) (constTypeName false f.Type) f.Name f.Name)) sb

    let sb = sb.Append "\nprivate:\n"
    let sb =
        tyRecord.Fields
        |> Array.fold(fun (sb: Text.StringBuilder) f ->
            sb.Append (sprintf "%s%s %s_;\n" (indent 1) (typeName false f.Type) f.Name)) sb
    sb.Append "};\n"

let dumpUnion (sb: Text.StringBuilder) (tyUnion : TyUnion) =
    let sb = sb.Append (sprintf "struct %s {\n" (stripModName tyUnion.Name))

    // forward struct declaration
    let sb =
        tyUnion.Cases
        |> Array.fold (fun (sb: Text.StringBuilder) c ->
            sb.Append (sprintf "%sstruct %s;\n" (indent 1) c.Name)) sb

    // tags
    let sb = sb.Append (sprintf "protected:\n%senum class TAG {\n" (indent 1))
    let sb =
        tyUnion.Cases
        |> Array.fold (fun (sb: Text.StringBuilder) c ->
            sb.Append (sprintf "%s%s,\n" (indent 2) c.Name)) sb
    let sb = sb.Append (sprintf "%s};\n" (indent 1))

    // default constructor
    let sb = sb.Append (sprintf "%s%s(TAG tag) : tag_(tag) {}\n" (indent 1) (stripModName tyUnion.Name))


    (*let sb =
        let dumpCase (tyc: TyUnionCase) =
            sprintf "struct %s::%s" (striptyc.Name
        tyUnion.Cases
        |> Array.fold (fun (sb: Text.StringBuilder) c ->
            sb.Append (dumpCase c)) sb
    *)
    let sb = sb.Append (sprintf "private:\n%sTAG tag_;\n" (indent 1))
    (*
    let sb =
        tyUnion.Cases
        |> Array.fold(fun (sb: Text.StringBuilder) f ->
            sb.Append (sprintf "%s%s;\n" (indent 1) (dumpField f))) sb
    *)
    sb.Append "};\n"

let dumpEnum (sb: Text.StringBuilder) (tyEnum: TyEnum) =
    let sb = sb.Append (sprintf "enum class %s {\n" (stripModName tyEnum.Name)) 
    let sb =
        tyEnum.Cases
        |> Array.fold(fun (sb: Text.StringBuilder) e ->
            sb.Append (sprintf "%s%s = %d,\n" (indent 1) e.Name e.Value)) sb
    sb.Append "};\n"

let dumpObject (sb: Text.StringBuilder) (tyObject: TyObject) =
    failwith "implement"

let dumpAlias (sb: Text.StringBuilder) (tyObject: TyAlias) =
    sb.Append (sprintf "typedef ::%s %s;\n" (tyObject.Ty.Name.Replace (".", "::")) (stripModName tyObject.Name))

let rec dumpModule (loadModule: Set<string> -> string -> Set<string> * Module option) (mods: Set<string>, sb: Text.StringBuilder) (tyMod: Module) : Text.StringBuilder =
    let mods, sb =
        // handle use
        tyMod.Decls
        |> Array.filter(fun d -> match d with Declaration.Use _ -> true | _ -> false)
        |> Array.map(fun d -> match d with Declaration.Use (_, name) -> name | _ -> failwith "unreachable")
        |> Array.fold (fun (mods: Set<string>, sb: Text.StringBuilder) n -> 
            let mods, m = loadModule mods n
            match m with
            | Some m -> mods, (dumpModule loadModule (mods, sb) m).Append "\n"
            | None   -> mods, sb) (mods, sb)

    let sb = sb.Append (sprintf "namespace %s {\n" tyMod.Name) 

    let sb =
        tyMod.Decls
        |> Array.fold(fun (sb: Text.StringBuilder) d ->
            let sb =
                match d with
                | Declaration.Use   (di, m) -> sb.Append (sprintf "using namespace %s;\n" m)
                | Declaration.Interface  ty -> dumpInterface sb ty
                | Declaration.Record     ty -> dumpRecord sb ty
                | Declaration.Union      ty -> dumpUnion sb ty
                | Declaration.Enum       ty -> dumpEnum sb ty
                | Declaration.Object     ty -> dumpObject sb ty
                | Declaration.Alias      ty -> dumpAlias sb ty
            sb.Append "\n") sb

    sb.Append "}\n"

