" autoload/typr.vim
" Language: TypR (Typed R with Rust-inspired syntax)
"
" Implementation behind the :Typr* commands. Loaded lazily by Vim on first
" use, works identically in Vim and Neovim. The language server itself is a
" separate binary (typr lsp) shared with every other editor — these commands
" only launch the CLI tooling from inside the editor.

let s:save_cpo = &cpo
set cpo&vim

" --------------------------------------------------------------------------
" Binary resolution
" --------------------------------------------------------------------------
" Order of precedence:
"   1. g:typr_path           (global, e.g. set in vimrc)
"   2. "typr" resolved from PATH
" Returns: path string, or "" if unavailable.
function! typr#bin() abort
  if exists("g:typr_path") && !empty(g:typr_path)
    return g:typr_path
  endif
  return executable("typr") ? "typr" : ""
endfunction

" --------------------------------------------------------------------------
" CLI completion for :Typr* commands
" --------------------------------------------------------------------------
let s:typr_flags = {
      \ 'check' : ['--help'],
      \ 'build' : ['--help', '--test', '--no-incremental', '--checked', '--strict'],
      \ 'run'   : ['--help', '--profile', '--checked', '--strict'],
      \ 'test'  : ['--help', '--profile'],
      \ 'repl'  : ['--help'],
      \ }

function! typr#complete(arglead, cmdline, cursorpos) abort
  " Identify the subcommand from the command name:
  "   "TyprBuild[File] ..." -> "build", "TyprCheckFile ..." -> "check"
  let l:name = matchstr(a:cmdline, '^\a*')
  let l:sub = tolower(substitute(substitute(l:name, '^Typr', '', ''), 'File$', '', ''))
  let l:flags = get(s:typr_flags, l:sub, [])
  if a:arglead =~ '^--'
    return filter(copy(l:flags), 'v:val =~ "^" . a:arglead')
  endif
  return copy(l:flags)
endfunction

" --------------------------------------------------------------------------
" Launch core
" --------------------------------------------------------------------------
" Run a typr subcommand in a :terminal. Falls back to synchronous execution
" on Vim builds without the +terminal feature. The bang version skips the
" terminal and runs synchronously (useful for scripting / quickfix-style use).
"   subcmd : "check" | "build" | "run" | "test" | "repl"
"   file   : "" or path to operate on a single file
"   args   : extra CLI arguments already escaped for the shell
"   bang   : bool — synchronous run with output echoed
function! typr#run(subcmd, file, args, bang) abort
  let l:bin = typr#bin()
  if empty(l:bin)
    echohl WarningMsg
    echomsg "typr binary not found. Install it (see :help typr-requirements) or set g:typr_path."
    echohl None
    return
  endif

  let l:cmd = l:bin . " " . a:subcmd
  if !empty(a:file)
    let l:cmd .= " " . shellescape(a:file)
  endif
  if !empty(a:args)
    let l:cmd .= " " . a:args
  endif

  " Early bail-out: only file commands and run/check accept a file.
  if !empty(a:file) && a:subcmd ==# "test"
    echohl WarningMsg
    echomsg "typr test does not take a file — it runs the project suite."
    echohl None
    return
  endif

  if a:bang || !has("nvim") && !has("terminal")
    " Synchronous: capture output in the messages area. Simplest portable
    " path, also the only one available on older Vim.
    exec "silent !" . l:cmd
    redraw!
    return
  endif

  if has("nvim")
    " Open a terminal split below, attach the job.
    belowright new
    call termopen(l:cmd)
    startinsert
  else
    " Vim 8.1+ : term_start opens its own window. The terminal is left open
    " when the job exits so the (possibly colored) typR output stays visible;
    " close it with :q. Note: has("terminal") is a Vim-only feature flag —
    " Neovim is handled by the branch above.
    call term_start(l:cmd)
  endif
endfunction

" --------------------------------------------------------------------------
" :Typr* user commands — defined in plugin/typr.vim, routed here.
" Project-level: TyprCheck, TyprBuild, TyprRun, TyprTest, TyprRepl
" File-level  : TyprCheckFile, TyprBuildFile, TyprRunFile
" --------------------------------------------------------------------------

let &cpo = s:save_cpo
unlet s:save_cpo