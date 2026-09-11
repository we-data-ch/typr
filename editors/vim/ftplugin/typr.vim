" ftplugin/typr.vim
" Language: TypR (Typed R with Rust-inspired syntax)

if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1

let s:save_cpo = &cpo
set cpo&vim

" Comment string for ", comment" and folding
setlocal commentstring=#\ %s

" Indentation settings
setlocal expandtab
setlocal shiftwidth=4
setlocal softtabstop=4
setlocal tabstop=4

" Use # region / # endregion for folding (mirrors vscode language-configuration)
setlocal foldmethod=marker
setlocal foldmarker=#\ region,#\ endregion

setlocal formatoptions-=t

let b:undo_ftplugin = "setlocal commentstring< expandtab< shiftwidth< softtabstop< tabstop< foldmethod< foldmarker< formatoptions<"

let &cpo = s:save_cpo
unlet s:save_cpo
