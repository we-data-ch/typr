
def main [] {
	open adt.pl | split row "\n" | get 4 | str replace ", T)," ", T)." | xclip -sel clip  
	tmux new-window prolog -s adt.pl -g "consult('adt.pl')"
}
