" worldpeace.vim --- buffer settings for World Peace

if exists('b:did_ftplugin')
  finish
endif
let b:did_ftplugin = 1

setlocal commentstring=//\ %s
setlocal comments=s1:/*,mb:*,ex:*/,://
setlocal formatoptions-=t
setlocal formatoptions+=croql
setlocal suffixesadd=.wp
setlocal include=^\\s*load\\s\\+

if !exists('g:worldpeace_no_mappings')
  inoremap <buffer><expr> <CR> worldpeace#newline()
endif

let b:undo_ftplugin =
      \ 'setlocal commentstring< comments< formatoptions< suffixesadd< include<'
      \ . '|silent! iunmap <buffer> <CR>'
