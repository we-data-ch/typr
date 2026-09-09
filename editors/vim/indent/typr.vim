" indent/typr.vim
" Language: TypR (Typed R with Rust-inspired syntax)
"
" Simple heuristic indentation mirroring editors/vscode/language-configuration.json:
"   increase on lines ending with an unclosed { ( [ or a few keyword forms
"   decrease on lines starting with } ) ]

if exists("b:did_indent")
  finish
endif
let b:did_indent = 1

setlocal indentexpr=TyprIndent()
setlocal indentkeys+=0=},0=),0=],0=end
setlocal nosmartindent

if exists("*TyprIndent")
  finish
endif

function! TyprIndent()
  let l:lnum = prevnonblank(v:lnum - 1)
  if l:lnum == 0
    return 0
  endif

  let l:line = getline(l:lnum)
  let l:cline = getline(v:lnum)
  let l:indent = indent(l:lnum)

  " Decrease indent if current line opens with a closing bracket
  if l:cline =~ '^\s*[}\])]'
    return l:indent - shiftwidth()
  endif

  " Increase indent if the previous non-blank line ends with an unclosed
  " opening bracket, or with a control keyword such as match / else / =>
  if l:line =~ '[{\[(]\s*$'
        \ || l:line =~ '^\s*\(match\|else\)\s*$'
        \ || l:line =~ '=>\s*$'
    return l:indent + shiftwidth()
  endif

  return l:indent
endfunction
