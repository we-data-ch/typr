:- module(unification, [unification/4, type_substitution/3]).
:- use_module(type_context).
:- use_module(type_comparison).

set_append(M1, M2, M3) :-
	append(M1, M2, M0),
	sort(M0, M3).

% ========================================

unification_helper(V, T, T, []).

unification_helper(V, T, gen(G), [[gen(G), T]]).

unification_helper(V, Num, ind(G), [[ind(G), Num]]) :- number(Num).

unification_helper(V, int, ind(G), [[ind(G), V]]).

unification_helper(values([H0 | T0]), [H1 | T1], [H2 | T2], Match3) :-
	unification_helper(H0, H1, H2, Match1),
	unification_helper(values(T0), T1, T2, Match2),
	set_append(Match1, Match2, Match3).

unification_helper(V, tfn(Kinds1, Array1, Type1), tfn(Kinds2, Array2, Type2), Match3) :-
	unification_helper(V, Type1, Type2, Match1),
	unification_helper(V, Array1, Array2, Match2),
	set_append(Match1, Match2, Match3).

unification_helper(V, tarray(N1, T1), tarray(N2, T2), Match3) :-
	unification_helper(V, N1, N2, Match1),
	unification_helper(V, T1, T2, Match2),
	set_append(Match1, Match2, Match3).

unification_helper(V, ttag(Name, Type1), ttag(Name, Type2), Match) :-
	unification_helper(V, Type1, Type2, Match).

unification_helper(V, trecord(A1), trecord(A2), Match3) :-
	record_intersection(trecord(A1), trecord(A2), trecord(A3), trecrod(A4)),
	get_right_elements(A3, Types1),
	get_right_elements(A4, Types2),
	unification_helper(V, Types1, Types2, Match3).

unification_helper(V, A, B, []).

% ========================================

values_from_keys(_, [], []).
values_from_keys(L1, [Key|L2], [Val|Vals]) :-
    member([Key, Val], L1),
    extraire_valeurs(L1, L2, Vals).

% ========================================

record_intersection(trecord(A1), trecord(A2), trecord(A3), trecord(A4)) :-
	context:get_left_elements(A1, A1p),
	context:get_left_elements(A2, A2p),
	context:intersection(A1p, A2p, A),
	values_from_keys(A, A1, V1),
	values_from_keys(A, A2, V2),
	context:merge(A, V1, A3),
	context:merge(A, V2, A4).

% ========================================

is_unifiable(T1, T2) :-
	unification_helper(values([]), T1, T2, T3).

is_coherent(A).

% ========================================

unification(Context, values(V), A, B, C) :-
	unification_helper(values(V), A, B, C),
	is_coherent(C).

unification(Context, A, B, C) :-
	type_comparison:get_common_type_denominator(Context, A, B, C),
	is_coherent(C).

% ========================================

type_substitution(A, [], A).

% Simple Generics
type_substitution(gen(T), [[gen(T), M]], M).
type_substitution(gen(T), [[gen(T), M] | Rest], M).
type_substitution(gen(T), [[N, M] | Rest], Result) :-
	type_substitution(gen(T), Rest, Result).

% Indexes generics
type_substitution(ind(T), [[ind(T), M]], M).
type_substitution(ind(T), [[ind(T), M] | Rest], M).
type_substitution(ind(T), [[N, M] | Rest], Result) :-
	type_substitution(ind(T), Rest, Result).

% Sum generics
type_substitution(add(A, B), Tab, C) :-
	type_substitution(A, Tab, Ap),
	type_substitution(B, Tab, Bp),
	C is Ap + Bp.

% Minus generics
type_substitution(minus(A, B), Tab, C) :-
	type_substitution(A, Tab, Ap),
	type_substitution(B, Tab, Bp),
	C is Ap - Bp.

% Mul generics
type_substitution(mul(A, B), Tab, C) :-
	type_substitution(A, Tab, Ap),
	type_substitution(B, Tab, Bp),
	C is Ap * Bp.

% Div generics
type_substitution(division(A, B), Tab, C) :-
	type_substitution(A, Tab, Ap),
	type_substitution(B, Tab, Bp),
	C is round(Ap / Bp).

% List of term
type_substitution([], Tab, []).
type_substitution([gen(H) | T], Tab, [H2 | T2]) :-
	type_substitution(gen(H), Tab, H2),
	type_substitution(T, Tab, T2).
type_substitution([H | T], Tab, [H | T2]) :-
	type_substitution(T, Tab, T2).


% Records
type_substitution(trecord(Params), Tab, trecord(Params2)) :-
	type_context:get_right_elements(Params, Types),
	type_substitution(Types, Tab, Types2),
	type_context:get_left_elements(Params, Labels),
	type_context:merge(Labels, Types2, Params2).

% Functions
type_substitution(tfn(Kinds, Elements, Type), Tab, tfn(Kinds, Ress, Res)) :-
	type_substitution(Elements, Tab, Ress),
	type_substitution(Type, Tab, Res).

% Arrays
type_substitution(tarray(N, T), Tab, tarray(N2, T2)) :-
	type_substitution(N, Tab, N2),
	type_substitution(T, Tab, T2).
	
% Type constructor
type_substitution(talias(Kinds, Name, Params1), Tab, talias(Kinds, Name, Params2)) :-
	type_substitution(Params1, Tab, Params2).

% other type
type_substitution(A, Tab, A).

% ========================================

:- begin_tests(test_unification).
	test(t1) :- unification_helper(int, int, []).
	test(t2) :- unification_helper(int, gen(t), [[gen(t), int]]).
	test(t3) :- unification_helper([int, bool], [gen(t), gen(u)], [[gen(t), int], [gen(u), bool]]).
	test(t4) :- unification_helper(tfn([], [int, int], int), tfn([], [gen(t), gen(t)], gen(t)), [[gen(t), int]]).
	test(t5) :- unification_helper(tfn([], [str, int], str), tfn([], [gen(t), gen(u)], gen(t)), [[gen(t), str], [gen(u), int]]).
	test(t6) :- unification_helper(tarray(3, int), tarray(gen(n), gen(t)), [[gen(n), 3], [gen(t), int]]).
	test(t7) :- unification_helper(trecord([[x, int], [y, int]]), trecord([[x, gen(t)], [y, gen(t)]]), [[gen(t), int]]).
	test(t8) :- is_unifiable(int, int).
:- end_tests(test_unification).

