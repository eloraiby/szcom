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

module Schizo.AST

open System
open Schizo.Tokens

type TyField = {
    Name    : string
    Info    : DebugInfo
    Type    : Ty
}

and TyInterface = {
    Name    : string
    Info    : DebugInfo
    Methods : TyField []
}

and TyRecord = {
    Name    : string
    Info    : DebugInfo
    Fields  : TyField []
}

and TyUnionCase = {
    Name    : string
    Info    : DebugInfo
    Type    : Ty
}

and TyUnion = {
    Name    : string
    Info    : DebugInfo
    Cases   : TyUnionCase []
}

and TyEnumCase = {
    Name    : string
    Info    : DebugInfo
    Value   : int64
}

and TyEnum = {
    Name    : string
    Info    : DebugInfo
    Cases   : TyEnumCase []
}

// Only concrete types, No generics for now
//
//and TyInstance = {
//    GenMap  : Map<string, Ty>
//    Type    : Ty
//}

and TyFunction = {
    Info    : DebugInfo
    Param   : Ty
    RetVal  : Ty
} with
    member x.Name =
        sprintf "%s->%s" x.Param.Name x.RetVal.Name

and TyObject = {
    Name    : string
    Info    : DebugInfo
    Interfaces  : TyInterface []
    PrivMembers : TyField []
}

and TyArray = {
    Info    : DebugInfo
    Type    : Ty
}

and TyAlias = {
    Name    : string
    Info    : DebugInfo
    Ty      : Ty
}

and TyTuple = {
    Info    : DebugInfo
    Params  : (string option * Ty) []
} with
    member x.Name =
        let paramString =
            x.Params
            |> Array.fold(fun s (_, t) ->
                if s = ""
                then sprintf "%s" t.Name
                else sprintf "%s,%s" s t.Name) ""
        sprintf "(%s)" paramString

and [<RequireQualifiedAccess>]
    Ty =
    | Unit          
    | Boolean       
    | Char          
    | Int64         
    | Real64        
    | Interface     of TyInterface
    | Record        of TyRecord
    | Union         of TyUnion
    | Enum          of TyEnum
    | Function      of TyFunction
    | Object        of TyObject
    | Tuple         of TyTuple
    | Array         of TyArray
    | Alias         of TyAlias
with
    member x.Name  =
        match x with
        | Ty.Unit        _ -> "()"
        | Ty.Boolean     _ -> "Bool"
        | Ty.Char        _ -> "Char"
        | Ty.Int64       _ -> "Int"
        | Ty.Real64      _ -> "Real"
        | Ty.Interface   v -> v.Name
        | Ty.Record      v -> v.Name
        | Ty.Union       v -> v.Name
        | Ty.Enum        v -> v.Name
        | Ty.Function    v -> v.Name
        | Ty.Object      v -> v.Name
        | Ty.Tuple       v -> v.Name
        | Ty.Array       v -> v.Type.Name
        | Ty.Alias       v -> v.Name

    member x.DebugInfo  =
        match x with
        | Unit          -> { DebugInfo.StreamId = "Core"; DebugInfo.Line = 0; DebugInfo.Offset = 0 }
        | Boolean       -> { DebugInfo.StreamId = "Core"; DebugInfo.Line = 0; DebugInfo.Offset = 0 }
        | Char          -> { DebugInfo.StreamId = "Core"; DebugInfo.Line = 0; DebugInfo.Offset = 0 }
        | Int64         -> { DebugInfo.StreamId = "Core"; DebugInfo.Line = 0; DebugInfo.Offset = 0 }
        | Real64        -> { DebugInfo.StreamId = "Core"; DebugInfo.Line = 0; DebugInfo.Offset = 0 }
        | Interface  ty -> ty.Info
        | Record     ty -> ty.Info
        | Union      ty -> ty.Info
        | Enum       ty -> ty.Info
        | Function   ty -> ty.Info
        | Object     ty -> ty.Info
        | Tuple      ty -> ty.Info
        | Array      ty -> ty.Info
        | Alias      ty -> ty.Info
 
[<RequireQualifiedAccess>]       
type Declaration =
    | Use           of DebugInfo * string
    | Interface     of TyInterface
    | Record        of TyRecord
    | Union         of TyUnion
    | Enum          of TyEnum
    | Object        of TyObject
    | Alias         of TyAlias

type Module = {
    Name    : string
    Info    : DebugInfo
    Decls   : Declaration []
}

let stripModName (tyName: string) =
    let pos = tyName.LastIndexOfAny [| '.' |]
    tyName.Substring (pos + 1)

type Ty
with
    member x.ShortName = stripModName x.Name
    member x.ModuleName =
        let e = x.Name.IndexOfAny [| '.' |]
        x.Name.Substring(0, e)