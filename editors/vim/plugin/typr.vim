" plugin/typr.vim
" Language: TypR (Typed R with Rust-inspired syntax)
"
" Loaded once at startup. Registers helper commands and, on Neovim with
" Lua support, wires the language server for .ty buffers.

if exists("g:loaded_typr")
  finish
endif
let g:loaded_typr = 1

let s:save_cpo = &cpo
set cpo&vim

" --------------------------------------------------------------------------
" Language server command (both Vim and Neovim)
" --------------------------------------------------------------------------
" :TyprLspStatus - report whether the typr binary / lsp subcommand is usable
function! s:TyprLspStatus()
  if !executable("typr")
    echohl WarningMsg
    echomsg "typr binary not found in PATH. Install it first (see doc/typr.txt)."
    echohl None
    return
  endif
  let l:out = system("typr lsp --help")
  if v:shell_error != 0
    echohl WarningMsg
    echomsg "typr binary found but it does not expose an 'lsp' subcommand."
    echohl None
    return
  endif
  echomsg "typr lsp detected. Configure your client to run: typr lsp"
endfunction

command! -bar TyprLspStatus call s:TyprLspStatus()

" --------------------------------------------------------------------------
" :Typr* tooling commands — run the typr CLI from inside Vim/Neovim.
" Works in both editors. Async output goes to a :terminal; use the bang
" form for a synchronous run.
" --------------------------------------------------------------------------
command! -bang -nargs=* -complete=customlist,typr#complete TyprCheck  call typr#run('check', '', <q-args>, <bang>0)
command! -bang -nargs=* -complete=customlist,typr#complete TyprBuild   call typr#run('build', '', <q-args>, <bang>0)
command! -bang -nargs=* -complete=customlist,typr#complete TyprRun     call typr#run('run', '', <q-args>, <bang>0)
command! -bang -nargs=* -complete=customlist,typr#complete TyprTest    call typr#run('test', '', <q-args>, <bang>0)
command! -bang -nargs=* -complete=customlist,typr#complete TyprRepl    call typr#run('repl', '', <q-args>, <bang>0)

command! -bang -nargs=* -complete=customlist,typr#complete TyprCheckFile call typr#run('check', expand('%:p'), <q-args>, <bang>0)
command! -bang -nargs=* -complete=customlist,typr#complete TyprBuildFile  call typr#run('build', expand('%:p'), <q-args>, <bang>0)
command! -bang -nargs=* -complete=customlist,typr#complete TyprRunFile    call typr#run('run', expand('%:p'), <q-args>, <bang>0)

" --------------------------------------------------------------------------
" Neovim-only: auto-start the language server on .ty files
" --------------------------------------------------------------------------
if has("nvim")
  lua require("typr").setup()
endif

let &cpo = s:save_cpo
unlet s:save_cpo
