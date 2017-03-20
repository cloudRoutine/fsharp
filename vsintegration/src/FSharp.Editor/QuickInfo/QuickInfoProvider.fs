﻿// Copyright (c) Microsoft Corporation.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.Editor

open System
open System.Composition
open System.Threading
open System.Threading.Tasks

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Editor
open Microsoft.CodeAnalysis.Editor.Implementation.IntelliSense.QuickInfo
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.Editor.Shared.Utilities

open Microsoft.VisualStudio.FSharp.LanguageService
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio.Language.Intellisense

open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices


module internal FSharpQuickInfo = 

    /// Get tooltip combined from doccom of Signature and definition
    let getCompoundTooltipInfo 
        (checker: FSharpChecker, position:int, document:Document, projectInfoManager: ProjectInfoManager, cancellationToken) = asyncMaybe {

        let solution = document.Project.Solution

        // when a construct has been declared in a signature file the documentation comments that are
        // written in that file are the ones that go into the generated xml when the project is compiled
        // therefore we should include these doccoms in our design time tooltips
        let getTooltipFromRange (declRange:range) = asyncMaybe {
            // ascertain the location of the target declaration in the signature file
            let! extDocId = solution.GetDocumentIdsWithFilePath declRange.FileName |> Seq.tryHead
            let extDocument = solution.GetProject(extDocId.ProjectId).GetDocument extDocId
            let! extSourceText = extDocument.GetTextAsync cancellationToken

            let extSpan = CommonRoslynHelpers.FSharpRangeToTextSpan (extSourceText, declRange)
            let extLineText = (extSourceText.Lines.GetLineFromPosition extSpan.Start).ToString()
            
            // project options need to be retrieved because the signature file could be in another project 
            let extProjectOptions = projectInfoManager.TryGetOptionsForProject extDocId.ProjectId |>  Option.get
            let extDefines = 
                CompilerEnvironment.GetCompilationDefinesForEditing
                    (extDocument.FilePath, extProjectOptions.OtherOptions |> Seq.toList)
                
            let! extLexerSymbol = 
                CommonHelpers.getSymbolAtPosition 
                    (extDocId, extSourceText, extSpan.Start, declRange.FileName, extDefines, SymbolLookupKind.Greedy)

            let! _, _, extCheckFileResults = 
                checker.ParseAndCheckDocument (extDocument,extProjectOptions,allowStaleResults=true,sourceText=extSourceText)

            let! extTooltipText = 
                extCheckFileResults.GetStructuredToolTipTextAlternate
                    (declRange.StartLine, extLexerSymbol.Ident.idRange.EndColumn, extLineText, extLexerSymbol.FullIsland, FSharpTokenTag.IDENT) |> liftAsync

            match extTooltipText with
            | FSharpToolTipText [] 
            | FSharpToolTipText [FSharpStructuredToolTipElement.None] -> 
                return! None
            | extTooltipText  -> 
                let! extSymbolUse = 
                    extCheckFileResults.GetSymbolUseAtLocation 
                        (declRange.StartLine, extLexerSymbol.Ident.idRange.EndColumn, extLineText, extLexerSymbol.FullIsland)
                let extTextSpan = CommonRoslynHelpers.FSharpRangeToTextSpan (extSourceText, extLexerSymbol.Range)
                return! Some (extTooltipText, extTextSpan, extSymbolUse.Symbol, extLexerSymbol.Kind)
        }

        let! sourceText = document.GetTextAsync cancellationToken
        let! projectOptions = projectInfoManager.TryGetOptionsForEditingDocumentOrProject document
        let defines = CompilerEnvironment.GetCompilationDefinesForEditing(document.FilePath, projectOptions.OtherOptions |> Seq.toList)
        let! lexerSymbol = 
            CommonHelpers.getSymbolAtPosition 
                (document.Id, sourceText, position, document.FilePath, defines, SymbolLookupKind.Greedy)
        let idRange = lexerSymbol.Ident.idRange  

        let! _, _, checkFileResults = 
            checker.ParseAndCheckDocument 
                (document, projectOptions, allowStaleResults = true,sourceText=sourceText)

        let textLinePos = sourceText.Lines.GetLinePosition position
        let fcsTextLineNumber = Line.fromZ textLinePos.Line
        let lineText = (sourceText.Lines.GetLineFromPosition position).ToString()        

        /// Gets the tooltip information for the orignal target
        let getTargetSymbolTooltip () = asyncMaybe {
            let! targetTooltip = 
                checkFileResults.GetStructuredToolTipTextAlternate
                    (fcsTextLineNumber, idRange.EndColumn, lineText, lexerSymbol.FullIsland, FSharpTokenTag.IDENT) |> liftAsync
            match targetTooltip with
            | FSharpToolTipText [] 
            | FSharpToolTipText [FSharpStructuredToolTipElement.None] -> return! None
            | _ -> 
                let! lexerSymbolUse = checkFileResults.GetSymbolUseAtLocation (fcsTextLineNumber, idRange.EndColumn, lineText, lexerSymbol.FullIsland)
                let targetTextSpan = CommonRoslynHelpers.FSharpRangeToTextSpan (sourceText, lexerSymbol.Range)
                return! Some (targetTooltip, targetTextSpan, lexerSymbolUse.Symbol, lexerSymbol.Kind)
        } 
        // if the target is in a signature file, adjusting the tooltip info is unnecessary
        if isSignatureFile document.FilePath then
            let! targetTooltipInfo = getTargetSymbolTooltip()
            return (None ,Some targetTooltipInfo)
        else
        // find the declaration location of the target symbol, with a preference for signature files
        let! findSigDeclarationResult = 
            checkFileResults.GetDeclarationLocationAlternate
                (idRange.StartLine, idRange.EndColumn, lineText, lexerSymbol.FullIsland, preferFlag=true)  |> liftAsync
            
        // it is necessary to retrieve the backup tooltip info because this acquires
        // the textSpan designating where we want the tooltip to appear.
        let! backupTooltipInfo 
            & (_, targetTextSpan, _, _) = getTargetSymbolTooltip()
   
        let tooltipInfoPair = asyncMaybe {
            match findSigDeclarationResult with 
            | FSharpFindDeclResult.DeclNotFound _failReason -> 
                return (None, Some backupTooltipInfo)

            | FSharpFindDeclResult.DeclFound declRange -> 
                if isSignatureFile declRange.FileName then 
                    let! sigTooltipInfo = getTooltipFromRange declRange
                    // if the target was declared in a signature file, and the current file
                    // is not the corresponding module implementation file for that signature,
                    // the doccoms from the signature will overwrite any doccoms that might be 
                    // present on the definition/implementation

                    let! findImplDefinitionResult = 
                        checkFileResults.GetDeclarationLocationAlternate
                            (idRange.StartLine, idRange.EndColumn, lineText, lexerSymbol.FullIsland, preferFlag=false) |> liftAsync   

                    match findImplDefinitionResult  with 
                    | FSharpFindDeclResult.DeclNotFound _failReason -> 
                        return (Some sigTooltipInfo , None)
                    | FSharpFindDeclResult.DeclFound declRange -> 
                        let! (implTooltip,_,implSymbol,implLex) = getTooltipFromRange declRange
                        
                        return (Some sigTooltipInfo ,Some (implTooltip,targetTextSpan,implSymbol,implLex))
                else 
                return (None, Some backupTooltipInfo)
        }
        return! tooltipInfoPair
    }



open FSharpQuickInfo

[<Shared>]
[<ExportQuickInfoProvider (PredefinedQuickInfoProviderNames.Semantic, FSharpCommonConstants.FSharpLanguageName)>]
type internal FSharpQuickInfoProvider [<ComponentModel.Composition.ImportingConstructor>] 
    (   [<ComponentModel.Composition.Import (typeof<SVsServiceProvider>)>] 
        serviceProvider: IServiceProvider, checkerProvider: FSharpCheckerProvider, projectInfoManager: ProjectInfoManager, typeMap: Shared.Utilities.ClassificationTypeMap, glyphService: IGlyphService) =

    let xmlMemberIndexService = serviceProvider.GetService<SVsXMLMemberIndexService,IVsXMLMemberIndexService>()
    let documentationBuilder = XmlDocumentation.CreateDocumentationBuilder (xmlMemberIndexService, serviceProvider.DTE)


    static member ProvideQuickInfo(checker: FSharpChecker, documentId: DocumentId, sourceText: SourceText, filePath: string, position: int, options: FSharpProjectOptions, textVersionHash: int) =
        asyncMaybe {
            let! _, _, checkFileResults = checker.ParseAndCheckDocument(filePath, textVersionHash, sourceText.ToString(), options, allowStaleResults = true)
            let textLine = sourceText.Lines.GetLineFromPosition(position)
            let textLineNumber = textLine.LineNumber + 1 // Roslyn line numbers are zero-based
            let defines = CompilerEnvironment.GetCompilationDefinesForEditing(filePath, options.OtherOptions |> Seq.toList)
            let! symbol = CommonHelpers.getSymbolAtPosition(documentId, sourceText, position, filePath, defines, SymbolLookupKind.Precise)
            let! res = checkFileResults.GetStructuredToolTipTextAlternate(textLineNumber, symbol.Ident.idRange.EndColumn, textLine.ToString(), symbol.FullIsland, FSharpTokenTag.IDENT) |> liftAsync
            match res with
            | FSharpToolTipText [] 
            | FSharpToolTipText [FSharpStructuredToolTipElement.None] -> return! None
            | _ -> 
                let! symbolUse = checkFileResults.GetSymbolUseAtLocation(textLineNumber, symbol.Ident.idRange.EndColumn, textLine.ToString(), symbol.FullIsland)
                return! Some(res, CommonRoslynHelpers.FSharpRangeToTextSpan(sourceText, symbol.Range), symbolUse.Symbol, symbol.Kind)
        }

    interface IQuickInfoProvider with
        override this.GetItemAsync (document: Document, position: int, cancellationToken: CancellationToken): Task<QuickInfoItem> =
            asyncMaybe {
                let! (sigTooltipInfo,targetTooltipInfo) = 
                    FSharpQuickInfo.getCompoundTooltipInfo
                        (checkerProvider.Checker, position, document, projectInfoManager, cancellationToken)

                match sigTooltipInfo, targetTooltipInfo with 
                | None, None -> return null
                | Some (toolTipElement, textSpan, symbol, symbolKind), None  
                | None, Some (toolTipElement, textSpan, symbol, symbolKind) -> 
                    let mainDescription = Collections.Generic.List()
                    let documentation = Collections.Generic.List()
                    XmlDocumentation.BuildDataTipText( 
                        documentationBuilder,   CommonRoslynHelpers.CollectTaggedText mainDescription,
                        CommonRoslynHelpers.CollectTaggedText documentation,
                        toolTipElement
                    )
                    let empty = ClassifiableDeferredContent (Array.Empty<TaggedText>(), typeMap)
                    let glyph = SymbolGlyphDeferredContent (CommonRoslynHelpers.GetGlyphForSymbol(symbol, symbolKind), glyphService)
                    let content = 
                        QuickInfoDisplayDeferredContent(
                            symbolGlyph = glyph,
                            warningGlyph = null,
                            mainDescription = ClassifiableDeferredContent (mainDescription, typeMap),
                            documentation = ClassifiableDeferredContent (documentation, typeMap),
                            typeParameterMap = empty,
                            anonymousTypes = empty,
                            usageText = empty,
                            exceptionText = empty
                        )
                    return QuickInfoItem (textSpan, content)

                |( Some (sigToolTipElement, _sigTextSpan, _sigSymbol, _sigSymbolKind)
                 , Some (targetToolTipElement, targetTextSpan, targetSymbol, targetSymbolKind) ) ->                   
                    let description, targetDocumentation, sigDocumentation = ResizeArray(), ResizeArray(), ResizeArray()
                    XmlDocumentation.BuildDataTipText(
                        documentationBuilder,
                        CommonRoslynHelpers.CollectTaggedText (ResizeArray()),
                        CommonRoslynHelpers.CollectTaggedText sigDocumentation,
                        sigToolTipElement
                    )
                    XmlDocumentation.BuildDataTipText(
                        documentationBuilder,
                        CommonRoslynHelpers.CollectTaggedText description,
                        CommonRoslynHelpers.CollectTaggedText targetDocumentation,
                        targetToolTipElement
                    )

                    let width = 
                        (0, Seq.concat [description; targetDocumentation; sigDocumentation]) 
                        ||> Seq.fold (fun acc elm -> max acc elm.Text.Length)

                    let seperator = TaggedText (TextTags.Text, String.replicate width "-")  
                    let lineBreak = TaggedText (TextTags.LineBreak, "\n") 
                    let bridge = [|  lineBreak; seperator; lineBreak |]
                    
                    let taggedTextIsEmpty (tts:#seq<TaggedText>) =
                        tts |> Seq.forall (fun tt -> String.IsNullOrWhiteSpace tt.Text)
                    
                    let documentation = 
                        match taggedTextIsEmpty sigDocumentation, taggedTextIsEmpty targetDocumentation with 
                        | true, true -> targetDocumentation 
                        | false, true -> sigDocumentation
                        | true, false -> targetDocumentation
                        | false, false -> 
                            ResizeArray (Array.concat[sigDocumentation.ToArray();bridge;targetDocumentation.ToArray()])

                    let empty = ClassifiableDeferredContent (Array.Empty<TaggedText>(), typeMap)
                    let glyph = SymbolGlyphDeferredContent (CommonRoslynHelpers.GetGlyphForSymbol (targetSymbol, targetSymbolKind), glyphService)
                    let content = 
                        QuickInfoDisplayDeferredContent(
                            symbolGlyph = glyph,
                            warningGlyph = null,
                            mainDescription = ClassifiableDeferredContent (description , typeMap),
                            documentation = ClassifiableDeferredContent (documentation, typeMap),
                            typeParameterMap = empty,
                            anonymousTypes = empty,
                            usageText = empty,
                            exceptionText = empty
                        )
                    return QuickInfoItem (targetTextSpan, content)
            }   |> Async.map Option.toObj
                |> CommonRoslynHelpers.StartAsyncAsTask cancellationToken 
