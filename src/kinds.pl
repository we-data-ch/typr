:- module(kinds, [get_gen_kind/2, type_to_kind/2]).
:- use_module(type_context).

% ========================================

set_append(M1, M2, M3) :-
	append(M1, M2, M0),
	sort(M0, M3).

% ========================================

get_gen_index(gen(N), [[gen(N), k_index]]).
get_gen_index(_, []).

% ========================================

% gen_gen_kind
get_gen_kind(gen(G), [[gen(G), k_type]]).
get_gen_kind(ind(G), [[ind(G), k_index]]).

get_gen_kind([], []).

get_gen_kind([H | T], Kinds2) :-
	get_gen_kind(H, Kind),
	get_gen_kind(T, Kinds),
	set_append(Kinds, Kind, Kinds2).

get_gen_kind(talias(_Kinds, Params, _Type), Kinds3) :-
	get_gen_kind(Params, Kinds2),
	set_append(kinds1, Kinds2, Kinds3).

get_gen_kind(tfn(Kinds, _Array1, _Type1), Kinds).

get_gen_kind(tarray(N1, T1), Kinds) :-
	get_gen_index(N1, Kinds1),
	get_gen_kind(T1, Kinds2),
	set_append(Kinds1, Kinds2, Kinds).

get_gen_kind(trecord(A), Kinds) :-
	type_context:get_right_elements(A, Types),
	get_gen_kind(Types, Kinds).

get_gen_kind(_, []).

% ========================================

type_to_kind([], []).
type_to_kind([H1 | T1], [H2 | T2]) :-
	type_to_kind(H1, H2),
	type_to_kind(T1, T2).
type_to_kind(Type, k_index) :- integer(Type).
type_to_kind(_Type, k_type).

% ========================================

contains_indices(ind(G)).

contains_indices([[H1, H2] | T]) :-
	contains_indices(H1);
	contains_indices(T).

% ========================================

:- begin_tests(test_kinds).
	test(t1) :- get_gen_kind(num, []).
	test(t2) :- get_gen_kind(gen(t), [[gen(t), k_type]]).
	test(t3) :- get_gen_kind(tarray(gen(n), gen(t)), [[gen(n), k_index], [gen(t), k_type]]).
	test(t4) :- get_gen_kind(trecord([[var(x), gen(t)], [var(y), gen(t)]]), [[gen(t), k_type]]).
	test(t5) :- get_gen_kind(trecord([[var(x), tarray(gen(n), gen(t))], [var(y), num]]), [[gen(n), k_index], [gen(t), k_type]]).
:- end_tests(test_kinds).
