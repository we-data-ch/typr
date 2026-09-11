" ftdetect/typr.vim
" Language: TypR (Typed R with Rust-inspired syntax)
" Detect .ty files (canonical extension) with .typr/.tyr as legacy aliases.

au BufRead,BufNewFile *.ty   setfiletype typr
au BufRead,BufNewFile *.typr setfiletype typr
au BufRead,BufNewFile *.tyr  setfiletype typr
