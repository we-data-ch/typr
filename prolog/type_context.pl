:- module(type_context, [get_right_elements/2, get_right_element/3, concat_context/3, get_from_context/4, get_type/3, get_kind/3, add_type/3, add_kind/3, get_left_elements/2, intersection/3]).

% ========================================

% OK
% subtyping or same type FAIL
% get_from_context(Context, var(X, P, Y, M1, Base1), [[var(X, P, _, _M2, Base2), Type] | T], Type2) :-
% \+ type_comparison:is_matching(Context, Base1, Base2),
% get_from_context(Context, var(X, P, Y, M1, Base1), T, Type2).

% subtyping or same type
get_from_context(Context, var(X, P, _, _M1, Base1), [[var(X, P, _, _M2, Base2), Type] | T], Type) :-
	type_comparison:is_matching(Context, Base1, Base2).


% for mutable assignement
get_from_context(Context, var(X, Path, _, true, _), [[var(X, Path, public, true, _), Typ] | T], mutable).
get_from_context(Context, var(X, Path, _, true, _), [[var(X, Path, _, false, _), Typ] | T], _) :-
	writeln("Error: "),
	write(X), writeln(" is immutable."),
	write("Tips: "), writeln("try changing it to mutable with the 'mut' keyword instead of 'let'"),
	halt.

% PASS
% if the variable is private: pass 
get_from_context(Context, var(X, Path, Pe, M, Base), [[var(_, _, private, _, _), Type2] | T], Type3) :-
	get_from_context(Context, var(X, Path, Pe, M, Base), T, Type3).

% ignore different variable 
get_from_context(Context, var(X1, P1, Pe1, M1, B1), [[var(X2, P2, Pe2, M2, B2), Type2] | T], Type3) :-
	get_from_context(Context, var(X1, P1, Pe1, M1, B1), T, Type3).

% private field error
get_from_context(Context, var(X1, P1, Pe1, M1, B1), [[priv(X1, P1), Type2] | T], Type3) :-
	writeln("Error: "),
	write("The symbol '"), write(X1), write("' from the module '"), write(P1), writeln("' is private"),
	write("tips: Try to add a 'pub' before the declaration."),
	halt.

% ignore private field with different path
get_from_context(Context, var(X1, P1, Pe1, M1, B1), [[priv(_, _), Type2] | T], Type3) :-
	get_from_context(Context, var(X1, P1, Pe1, M1, B1), T, Type3).

get_from_context(Context, var(Name, A, B, C, D), [], _) :-
	writeln("Error: "),
	writeln(var(Name, A, B, C, D)),
	write(Name), writeln(" wasn't found."),
	write("if you declared it in a module, be sure you mention it: '[Module Name]::"), write(Name), writeln("'"), writeln(""),
	!.
% halt.

% ========================================

get_type(var(X, P, Pe, M, B), context(Kinds, Types), T) :-
	get_right_element(var(X, P, Pe, M, B), Types, T).

get_kind(var(X, P, Pe, M, B), context(Kinds, Types), T) :-
	get_right_element(var(X, P, Pe, M, B), Kinds, T).

% ========================================

get_right_element(var(X, P, Pe, M, B), [[var(X, P, Pe, M, B), Type] | T], Type).
% for alias the params don't need to match since there are generics
get_right_element(var(X, P, Pe, M, params(B1)), [[var(X, P, Pe, M, params(B2)), Type] | T], Type).
get_right_element(var(X, P, Pe1, M1, B1), [[var(Y, P, Pe2, M2, B2), V] | T], Res) :-
	get_right_element(var(X, P, Pe1, M1, B), T, Res).

get_right_element(var(X, P, Pe, M, B), [], any).

get_right_element(gen(X), [[gen(X), Type] | T], Type).
get_right_element(gen(X), [[gen(Y), V] | T], Res) :-
	get_right_element(gen(X), T, Res).


get_right_element(gen(X), [], any).

get_right_element(X, [[X, Y] | T], Y).
get_right_element(X, [[Z, Y] | T], W) :-
	get_right_element(X, T, W).

get_right_element(E, [], E).

% prend une liste de paramètre [nom, type] puis retourne le nom seulement
get_right_elements([[V, T] | R], Res) :-
	get_right_elements(R, Res1), Res = [T | Res1].
get_right_elements([], []).

% prend une liste de paramètre [nom, type] puis retourne le nom seulement
get_left_elements([[V, T] | R], Res) :-
	get_left_elements(R, Res1), Res = [V | Res1].
get_left_elements([], []).

% ========================================

merge([H1 | T1], [H2 | T2], [[H1, H2] | Res]) :-
	merge(T1, T2, Res).
merge([], [], []).

% ========================================

concat([], L, L).
concat([Tête|Queue1], L2, [Tête|Queue3]) :-
    concat(Queue1, L2, Queue3).

concat_context(context(Kinds1, Types1), context(Kinds2, Types2), context(Kinds3, Types3)) :-
	concat(Kinds1, Kinds2, Kinds3),
	concat(Types1, Types2, Types3).
	
% ========================================

add_type(context(Kinds, Types), [Var, Type], context(Kinds, [[Var, Type] | Types])).
add_kind(context(Kinds, Types), [Var, Kind], context(Kinds, [[Var, Kind] | Types])).

% ========================================

intersection(_, [], []).
intersection(L2, [X|L1], [X|L3]) :-
    member(X, L2),
    intersection(L2, L1, L3).
intersection(L2, [X|L1], L3) :-
    \+ member(X, L2), 
    intersection(L2, L1, L3).
