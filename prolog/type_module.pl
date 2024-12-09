:- module(type_module, [base_name_from/3, get_base_name/2, list_atom_concat/2, list_concat/2, join/3, add_path/4]).

% ========================================

list_atom_concat([Element], Element).
list_atom_concat([gen(H) | T], Res) :-
	list_atom_concat(T, T2),
	atom_concat(H, ', ', Res1),
	atom_concat(Res1, T2, Res).
list_atom_concat([H | T], Res) :-
	list_atom_concat(T, T2),
	atom_concat(H, ', ', Res1),
	atom_concat(Res1, T2, Res).

% ========================================

list_concat([Element], Element).
list_concat([H | T], Res) :-
	list_concat(T, T2),
	atom_concat(H, T2, Res).

% ========================================

join([], Sep, '').
join([Element], Sep, Element).
join([H | T], Sep, Res) :-
	list_atom_concat(T, T2),
	atom_concat(H, Sep, Res1),
	atom_concat(Res1, T2, Res).

% ========================================

flatten([], []).
flatten([Head|Tail], FlatList) :-
    flatten(Head, FlatHead),
    flatten(Tail, FlatTail),
    append(FlatHead, FlatTail, FlatList).
flatten(Element, [Element]) :-
    \+ is_list(Element). 

% ========================================

% deprecated, old
get_base_name(gen(a), 'A').
get_base_name(gen(b), 'B').
get_base_name(gen(c), 'C').
get_base_name(gen(d), 'D').
get_base_name(gen(e), 'E').
get_base_name(gen(f), 'F').
get_base_name(gen(g), 'G').
get_base_name(gen(h), 'H').
get_base_name(gen(i), 'I').
get_base_name(gen(j), 'J').
get_base_name(gen(k), 'K').
get_base_name(gen(l), 'L').
get_base_name(gen(m), 'M').
get_base_name(gen(n), 'N').
get_base_name(gen(o), 'O').
get_base_name(gen(p), 'P').
get_base_name(gen(q), 'Q').
get_base_name(gen(r), 'R').
get_base_name(gen(s), 'S').
get_base_name(gen(t), 'T').
get_base_name(gen(u), 'U').
get_base_name(gen(v), 'V').
get_base_name(gen(w), 'W').
get_base_name(gen(x), 'X').
get_base_name(gen(y), 'Y').
get_base_name(gen(z), 'Z').

get_base_name(var(X), X).
get_base_name(num, 'Num').
get_base_name(bool, 'Bool').
get_base_name(tarray(Dim, Type), Res) :-
	get_base_name(Dim, Dim2),
	get_base_name(Type, Type2),
	list_atom_concat(['array', Dim2, Type2], Res).
get_base_name(trecord(Params), Res) :-
	flatten(Params, List1),
	maplist(get_base_name, List1, List2),
	list_atom_concat(List2, Concatenated),
	atom_concat('record_', Concatenated, Res).
get_base_name(tfn(Kinds, Types, Type), Res) :-
	maplist(get_base_name, Types, Bases),
	get_base_name(Type, Base),
	list_atom_concat(Bases, Conc1),
	list_atom_concat(['function', Conc1, Base], Res).
get_base_name(tag(Name, Value), Res) :-
	list_atom_concat(['tag', Name, Value], Res).
get_base_name(Index, Res) :-
	number(Index),
	number_chars(Index, ListeChars), % Convertit le nombre en une liste de caractères
    atom_chars(Res, ListeChars). 

% ========================================

base_name_from(Name, Type, NewName) :-
	get_base_name(Type, Base),
	list_atom_concat([Base, Name], NewName).

% ========================================

substitute_opaque(opaques(O), var(N, _, _, _, _), opaque(N)) :-
	member(N, O).

substitute_opaque(opaques(O), [H1 | T1], [H2 | T2]) :-
	substitute_opaque(opaques(O), H1, H2),
	substitute_opaque(opaques(O), T1, T2).

substitute_opaque(opaques(O), [], []).
substitute_opaque(opaques(O), N, N).

% ========================================


% if private declaration
add_path(Name, [var(N, Pa, private, _, _), _], opaques(O1), [priv(N, Name), empty], opaques(O2)).

% if opaque type
add_path(Name, [var(N, Pa1, Pe, true, params(P)), Element], opaques(O1), [var(N, Pa2, Pe, true, params(P)), opaque(N)], opaques(O2)) :-
	concat_path(Name, Pa1, Pa2),
	append(O1, [N], O2).

% if reference to an opaque type previousely
add_path(Name, [var(N1, Pa1, Pe, MutOp, Ty), var(N2, _, _, _, _)], opaques(O1), [var(N1, Pa2, Pe, MutOp, Ty), opaque(N2)], opaques(O1)) :-
	member(N2, O1),
	concat_path(Name, Pa1, Pa2).

add_path(Name, [var(N1, Pa1, Pe, MutOp, Ty1), tfn(Kinds, Types1, Type1)], opaques(O1), [var(N1, Pa2, Pe, MutOp, Ty2), tfn(Kinds, Types2, Type2)], opaques(O1)) :-
	substitute_opaque(opaques(O1), Ty1, Ty2),
	substitute_opaque(opaques(O1), Types1, Types2),
	substitute_opaque(opaques(O1), Type1, Type2),
	concat_path(Name, Pa1, Pa2).

add_path(Name, [var(N, Pa1, Pe, MutOp, Ty), Element], opaques(O1), [var(N, Pa2, Pe, MutOp, Ty), Element], opaques(O1)) :-
	concat_path(Name, Pa1, Pa2).

add_path(Name, [], opaques(O), [], opaques(O)).

add_path(Name, [H1 | T1], opaques(O1), [H2 | T2], opaques(O3)) :-
	add_path(Name, H1, opaques(O1), H2, opaques(O2)),
	add_path(Name, T1, opaques(O2), T2, opaques(O3)).

add_path(Name, [], _, []).

% ========================================

concat_path(Name, empty, Name).
concat_path(Name, Path, Name2) :- join([Name, Path], '/', Name2).

% ========================================

%get_permission(H, Perm).
%set_permission(H, Perm, H).
set_permission(Context, Perm, Context).
