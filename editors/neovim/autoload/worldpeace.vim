" worldpeace.vim --- helpers for World Peace files

function! worldpeace#newline() abort
  let l:line = getline('.')
  let l:indentation = matchstr(l:line, '^\s*')

  if l:line =~# '^\s*---'
    return "\<CR>" . l:indentation . '--- '
  endif

  if l:line =~# '^\s*dec\>.*:\s*$'
    return "\<CR>--- "
  endif

  return "\<CR>"
endfunction

function! worldpeace#previous_code_lnum(lnum) abort
  let l:lnum = a:lnum - 1

  while l:lnum > 0 && getline(l:lnum) =~# '^\s*$'
    let l:lnum -= 1
  endwhile

  return l:lnum
endfunction

function! worldpeace#opens_block(line) abort
  return a:line =~# '{\s*\(//.*\)\=$'
        \ || a:line =~# '^\s*dec\>.*:\s*$'
endfunction

function! worldpeace#closes_block(line) abort
  return a:line =~# '^\s*\(}\|end\>\)'
endfunction

function! worldpeace#indent(lnum) abort
  let l:line = getline(a:lnum)

  if l:line =~# '^\s*---'
    return 0
  endif

  let l:previous_lnum = worldpeace#previous_code_lnum(a:lnum)
  if l:previous_lnum == 0
    return 0
  endif

  let l:indentation = indent(l:previous_lnum)
  let l:previous = getline(l:previous_lnum)

  if worldpeace#opens_block(l:previous)
    let l:indentation += shiftwidth()
  endif

  if worldpeace#closes_block(l:line)
    let l:indentation -= shiftwidth()
  endif

  return max([0, l:indentation])
endfunction
