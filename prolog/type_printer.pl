:- module(type_printer, [pretty_print/1]).
:- use_module(type_module).

format(tarray(N1, T1), Res) :-
	format(N1, N2),
	format(T1, T2),
	type_module:list_concat(['[', N2, ', ', T2, ']'], Res).

% alias with empty parameters
format(var(Name, Path, Permission, Mutable, params([])), Name).

format(var(Name, Path, Permission, Mutable, params(Params1)), Res) :-
	maplist(format, Params1, Params2),
	join(Params2, ", ", Params3),
	type_module:list_concat([Name, '<', Params3,'>'], Res).

format(var(Name, Path, Permission, Mutable, Type), Name).


format(tfn(Kinds1, Params1, Type1), Res) :-
	maplist(format, Kinds1, Kinds2),
	maplist(format, Params1, Params2),
	format(Type1, Type2),
	join(Kinds2, ", ", Kinds3),
	join(Params2, ", ", Params3),
	type_module:list_concat(['fn<', Kinds3, '>(', Params3, ') -> ', Type2], Res).


format(ttag(Name, Param), Res) :-
	format(Param, Param2),
	type_module:list_concat([Name, '(', Param2, ')'], Res).
	
format(var(Label), Label).

format([Label1, Element1], Res) :-
	format(Label1, Label2),
	format(Element1, Element2),
	type_module:list_concat([Label2, ': ', Element2], Res).


% trecord([[var(x),num],[var(y),num]])
format(trecord(Fields1), Res) :-
	maplist(format, Fields1, Fields2),
	join(Fields2, ", ", Fields3),
	type_module:list_concat(['{', Fields3, '}'], Res).

format(gen(a), 'A').
format(gen(b), 'B').
format(gen(c), 'C').
format(gen(d), 'D').
format(gen(e), 'E').
format(gen(f), 'F').
format(gen(g), 'G').
format(gen(h), 'H').
format(gen(i), 'I').
format(gen(j), 'J').
format(gen(k), 'K').
format(gen(l), 'L').
format(gen(m), 'M').
format(gen(n), 'N').
format(gen(o), 'O').
format(gen(p), 'P').
format(gen(q), 'Q').
format(gen(r), 'R').
format(gen(s), 'S').
format(gen(t), 'T').
format(gen(u), 'U').
format(gen(v), 'V').
format(gen(w), 'W').
format(gen(x), 'X').
format(gen(y), 'Y').
format(gen(z), 'Z').

format(ind(Gen), Res) :-
	upcase_atom(Gen, Gen2),
	type_module:join(['#', Gen2], '', Res).

format(N, N).

pretty_print(N1) :-
	format(N1, N2),
	write(N2).

pretty_print(N) :-
	write("Wasn't able to print:"),
	write(N).
