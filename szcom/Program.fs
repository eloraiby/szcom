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
open Schizo.AST
open Schizo.Syntax

[<EntryPoint>]
let main argv = 
    if argv.Length = 1
    then
        let m = parseModuleFile (State.empty()) argv.[0]
        printfn "%A" m
        ()
    else printfn "invalid option: schizocom file.szm"
    0 // return an integer exit code
