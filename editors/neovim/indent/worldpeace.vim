" worldpeace.vim --- indentation for World Peace

if exists('b:did_indent')
  finish
endif
let b:did_indent = 1

setlocal autoindent
setlocal expandtab
setlocal shiftwidth=3
setlocal softtabstop=3
setlocal indentexpr=worldpeace#indent(v:lnum)
setlocal indentkeys=0=end,0=},0=---,:

let b:undo_indent =
      \ 'setlocal autoindent< expandtab< shiftwidth< softtabstop< indentexpr< indentkeys<'
