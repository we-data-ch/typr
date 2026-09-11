" syntax/typr.vim — GENERATED FILE, do not edit.
" Produced by `typr syntax --target vim` from the syntax manifest in
" crates/typr-core/src/components/syntax/mod.rs, the single source of truth
" for TypR's lexemes. Edit the manifest and re-run `typr syntax --write`;
" CI runs `typr syntax --check` and fails on a hand-edited grammar.

if exists("b:current_syntax")
  finish
endif

let s:save_cpo = &cpo
set cpo&vim

syntax case match
syn sync fromstart
syn iskeyword @,48-57,_

syn match   typrEscape "\\." contained

syn match   typrPunctuationBrackets "\v[][(){}]"
syn match   typrPunctuationSeparator "\v\,|\:"
syn match   typrPunctuationTerminator "\v\;"
syn match   typrVariablesOther "\v<[a-z_][A-Za-z0-9_]*>"
syn match   typrVariablesParameter "\v<[a-z_][A-Za-z0-9_]*\s*%(:%(:@!))@="
syn match   typrFunctionsCall "\v<[a-z_][A-Za-z0-9_]*\s*\(@="
syn match   typrOperatorsLambda "\v\\[({:]@="
syn match   typrOperatorsAssign "\v\="
syn match   typrOperatorsAccess "\v\:\:|\$|\."
syn match   typrOperatorsSpread "\v\.\.\.|\.\."
syn match   typrOperatorsVectorialBlock "\v\@\{|\}\@"
syn match   typrOperatorsArithmetic "\v\%|\*|\+|\-|\/"
syn match   typrOperatorsCustom "\v\%[^%\s]*\%"
syn match   typrOperatorsTypeUnion "\v\|\>@!"
syn match   typrOperatorsPipe "\v\|\>"
syn match   typrOperatorsLogical "\v\&\&|\|\||\!|\&"
syn match   typrOperatorsComparison "\v\!\=|\<\=|\=\=|\>\=|\<|\>"
syn match   typrOperatorsBind "\v\<\-"
syn match   typrOperatorsArrow "\v\-\>|\=\>"
syn match   typrTypesAlias "\v<[A-Z][A-Za-z0-9_]+>"
syn match   typrTypesGeneric "\v<[A-Z][A-Za-z0-9_]@!"
syn match   typrTypesSigilGeneric "\v[#%@^?$]%(Self|[A-Z])[A-Za-z0-9_]@!"
syn match   typrTypesVariant "\v\.[A-Z][A-Za-z0-9_]*>"
syn match   typrKeywordsBlock "\v<%(Test|JS|R)%(\s*[\[{])@="
syn match   typrTypesConstructor "\v<%(library|Class|seq|c)%(\s*[\[(])@="
syn match   typrTypesBuiltinIndexed "\v<%(df)%(\s*\[)@="
syn match   typrTypesBuiltin "\v<%(UnknownFunction|data__frame|data\.frame|dataframe|Record|Array|Tuple|tuple|list|Vec)[A-Za-z0-9_]@!"
syn keyword typrTypesPrimitive int num char bool logic Any Empty Self
syn keyword typrConstants true TRUE false FALSE null NULL na NA
syn keyword typrKeywordsOperatorWord and or in
syn match   typrKeywordsCast "\v<%(as\!|as)[A-Za-z0-9_]@!"
syn keyword typrKeywordsDeclaration let fn function type opaque typeconstructor recursive interface record object module mod import use extern embed
syn keyword typrKeywordsControl if else match for while loop break next return
syn match   typrAnnotations "\v%(\@importFrom|\@testable|\@export|\@extern|\@pub)[A-Za-z0-9_]@!"
syn match   typrNumbersInteger "\v<[0-9]+>"
syn match   typrNumbersFloat "\v<[0-9]+\.[0-9]+>"
syn region  typrStringsBacktick start=+`+ end=+`+
syn region  typrStringsSingle start=+'+ skip=+\\.+ end=+'+ contains=typrEscape
syn region  typrStringsDouble start=+"+ skip=+\\.+ end=+"+ contains=typrEscape
syn match   typrComments "\v#%(%(Self|[A-Z])[A-Za-z0-9_]@!)@!.*$"
syn region  typrStringsRawR start=+r#"+ end=+"#+

hi def link typrEscape SpecialChar
hi def link typrStringsRawR String
hi def link typrComments Comment
hi def link typrStringsDouble String
hi def link typrStringsSingle String
hi def link typrStringsBacktick String
hi def link typrNumbersFloat Float
hi def link typrNumbersInteger Number
hi def link typrAnnotations PreProc
hi def link typrKeywordsControl Statement
hi def link typrKeywordsDeclaration Keyword
hi def link typrKeywordsCast Keyword
hi def link typrKeywordsOperatorWord Keyword
hi def link typrConstants Constant
hi def link typrTypesPrimitive Type
hi def link typrTypesBuiltin Type
hi def link typrTypesBuiltinIndexed Type
hi def link typrTypesConstructor Function
hi def link typrKeywordsBlock Keyword
hi def link typrTypesVariant Identifier
hi def link typrTypesSigilGeneric Type
hi def link typrTypesGeneric Type
hi def link typrTypesAlias Type
hi def link typrOperatorsArrow Operator
hi def link typrOperatorsBind Operator
hi def link typrOperatorsComparison Operator
hi def link typrOperatorsLogical Operator
hi def link typrOperatorsPipe Operator
hi def link typrOperatorsTypeUnion Operator
hi def link typrOperatorsCustom Operator
hi def link typrOperatorsArithmetic Operator
hi def link typrOperatorsVectorialBlock Operator
hi def link typrOperatorsSpread Operator
hi def link typrOperatorsAccess Operator
hi def link typrOperatorsAssign Operator
hi def link typrOperatorsLambda Operator
hi def link typrFunctionsCall Function
hi def link typrVariablesParameter Identifier
hi def link typrVariablesOther Identifier
hi def link typrPunctuationTerminator Delimiter
hi def link typrPunctuationSeparator Delimiter
hi def link typrPunctuationBrackets Delimiter

let b:current_syntax = "typr"

let &cpo = s:save_cpo
unlet s:save_cpo
