" worldpeace.vim --- syntax highlighting for World Peace

if exists('b:current_syntax')
  finish
endif

syntax case match

syntax keyword worldpeaceKeyword dec end ret do by case num load if else
syntax keyword worldpeaceBuiltin len push pop print read syscall errno argv

syntax match worldpeaceFunction "\<dec\>\s\+\zs[A-Za-z_][A-Za-z0-9_]*"
syntax match worldpeaceVariable "\<num\>\s*\%(\[[^]\n]*\]\s*\)\?\zs[A-Za-z_][A-Za-z0-9_]*"
syntax match worldpeaceLoad "\<load\>\s\+\zs[^;\n]\+"

syntax match worldpeaceNumber "\<0[xX][0-9A-Fa-f][0-9A-Fa-f_]*\>"
syntax match worldpeaceNumber "\<0[bB][01][01_]*\>"
syntax match worldpeaceNumber "\<0[oO][0-7][0-7_]*\>"
syntax match worldpeaceNumber "\<[0-9][0-9_]*\>"
syntax match worldpeaceWildcard "\<_\>"

syntax match worldpeaceBodyMarker "^\s*---"
syntax match worldpeaceOperator "\%(>>=\|<<=\|-->\|---\|<-\|==\|!=\|<=\|>=\|<<\|>>\|&&\|||\|+=\|-=\|\*=\|/=\|%=\|&=\||=\|\^=\|\.\.\)"
syntax match worldpeaceOperator "[][(){}.,:;=+\-*/%!^<>&|]"

syntax match worldpeaceLineComment "//.*$" contains=@Spell
syntax region worldpeaceBlockComment start="/\*" end="\*/" contains=worldpeaceBlockComment,@Spell

highlight default link worldpeaceKeyword Keyword
highlight default link worldpeaceBuiltin Function
highlight default link worldpeaceFunction Function
highlight default link worldpeaceVariable Identifier
highlight default link worldpeaceLoad Include
highlight default link worldpeaceNumber Number
highlight default link worldpeaceWildcard Special
highlight default link worldpeaceBodyMarker PreProc
highlight default link worldpeaceOperator Operator
highlight default link worldpeaceLineComment Comment
highlight default link worldpeaceBlockComment Comment

let b:current_syntax = 'worldpeace'
