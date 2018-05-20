// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace Microsoft.FSharp.Compiler.SourceCodeServices

open System
open System.Diagnostics
open System.Collections.Generic
open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.AbstractIL.Internal.Library 


module internal RecordStubGenerator =
    
    type RecordExpr = {
        Expr : SynExpr
        CopyExprOption : option<SynExpr * BlockSeparator>
        FieldExprList : (RecordFieldName * SynExpr option * BlockSeparator option) list
    }
        

    [<RequireQualifiedAccess>]
    type PositionKind =
        /// let record = {<insert-here>}
        | AfterLeftBrace
        /// let y = { x with<insert-here> }
        | AfterCopyExpression
        /// let x = { Field1 = expr<insert-here> }
        | AfterLastField

        
    type RecordStubsInsertionParams = {
        Kind: PositionKind
        InsertionPos: pos
        IndentColumn: int
    } with
        static member TryCreateFromRecordExpression : expr: RecordExpr -> RecordStubsInsertionParams option

    
    val tryFindRecordBindingInParsedInput : pos:pos -> parsedInput:ParsedInput -> RecordExpr option