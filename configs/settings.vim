command! Debug Cmd
command! Adt !tmux new-window nvim adt.pl
command! Prolog !tmux new-window prolog -s adt.pl -g "consult('adt.pl'), trace" 
command! Nostd !rm std.ty; touch std.ty
nnoremap 00 $r.V"+y
