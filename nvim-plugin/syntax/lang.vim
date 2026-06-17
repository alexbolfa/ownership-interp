" Vim syntax file
" Language: ownership-interp .lang files

if exists("b:current_syntax")
  finish
endif

syntax keyword ownershipKeyword let mut fn return if then else while
syntax keyword ownershipType int bool unit
syntax keyword ownershipBoolean True False

syntax match ownershipNumber "\v<\d+>"
syntax match ownershipFunction "\v<\k+>\ze\s*\("
syntax match ownershipOperator "[-+*/=!&|<>]\+"

syntax region ownershipLineComment start="//" end="$"
syntax region ownershipBlockComment start="/\*" end="\*/"

highlight default link ownershipKeyword Keyword
highlight default link ownershipType Type
highlight default link ownershipBoolean Boolean
highlight default link ownershipNumber Number
highlight default link ownershipFunction Function
highlight default link ownershipOperator Operator
highlight default link ownershipLineComment Comment
highlight default link ownershipBlockComment Comment

let b:current_syntax = "lang"
