:- module(type_comparison, [is_matching/3, is_subtype/3, get_common_type_denominator/4, reduce_types/3, reduce_type/3, get_best_type/4]).
:- use_module(type_context).
:- use_module(kinds).

% ========================================

all_in([], _).
all_in([H|T], L2) :-
    member(H, L2),
    all_in(T, L2).

% ========================================

% interface subtyping
% check_interface_function([var(X, P, Pe, M, B), FnType1], [[gen('self'), Type]], context(Kinds, Types)) :-
% unification:type_substitution(FnType1, [[gen('self'), Type]], FnType2),
% type_context:get_from_context(context(Kinds, Types), var(X, P, Pe, M, Type), Types, FnType2).
	% todo: check if it works with reduce type
%
% interface subtyping

check_interface_function(var(X), FnType1, [[gen('self'), Type]], context(Kinds, Types)) :-
	unification:type_substitution(FnType1, [[gen('self'), Type]], FnType2),
	type_context:get_from_context(context(Kinds, Types), var(X, P, Pe, M, Type), Types, FnType2).
	% todo: check if it works with reduce type

check_interface_functions([], [[gen('self'), Type]], Context).
check_interface_functions([[H1, H2] | T], [[gen('self'), Type]], Context) :-
	check_interface_function(H1, H2, [[gen('self'), Type]], Context),
	check_interface_functions(T, [[gen('self'), Type]], Context).

% ========================================

match_one_of_them(Context, Type, [H | T]) :-
	is_matching(Context, Type, H);
	match_one_of_them(Context, Type, T).

% ========================================

% tarray subtyping
is_subtype(Context, tarray(N1, T1), tarray(N2, T2)) :-
	is_subtype(Context, N1, N2),
	is_subtype(Context, T1, T2).

% interface subtyping
is_subtype(Context, Type, interface(Args)) :-
	check_interface_functions(Args, [[gen('self'), Type]], Context).

% record subtyping
is_subtype(Context, trecord(R1), trecord(R2)) :-
	all_in(R2, R1).

% union subtyping
is_subtype(Context, Type, union(Tab)) :-
	match_one_of_them(Context, Type, Tab).

is_subtype(Context, union([]), union(Tab)).
is_subtype(Context, union([H | T]), union(Tab)) :-
	is_subtype(Context, H, union(Tab)),
	is_subtype(Context, union(T), union(Tab)).

% generic subtyping
is_subtype(Context, Type, gen(X)).

% generic subtyping index
is_subtype(Context, Type, ind(X)).

% for alias params
is_subtype(Context, params([]), params([])).
is_subtype(Context, params([H1 | T1]), params([H2 | T2])) :-
	is_subtype(Context, H1, H2),
	is_subtype(Context, params(T1), params(T2)).


% ========================================

is_non_empty_list([_|_]).

% ========================================

is_same_type(Context, X, X).

% perhaps remove it
is_same_type(context(Kinds1, Types1), Type1, talias([], var(X, P, Pe, M, B), Params1)) :-
	is_same_type(context(Kinds1, Types1), Type1, Type3).

is_same_type(Context, T1, T2) :-
	unification:unification(Context, T1, T2, Tab),
	is_non_empty_list(Tab).

is_same_type(Context, X, alias(Params)).

% ========================================

reduce_param(context(Kinds1, Types1), [], []).
reduce_param(context(Kinds1, Types1), [[X, Y1] | T1], [[X, Y2] | T2]) :-
	reduce_type(context(Kinds1, Types1), Y1, Y2),
	reduce_param(context(Kinds1, Types1), T1, T2).

% ========================================

reduce_type(context(Kinds1, Types1), trecord(R1), trecord(R2)) :-
	reduce_param(context(Kinds1, Types1), R1, R2).

% alias
reduce_type(context(Kinds1, Types1), var(N, P, Pe, M, params(Params1)), Type3) :-
	type_context:get_from_context(context(Kinds1, Types1), var(N, P, Pe, M, params(Params1)), Types1, talias(Kinds, Params2, Type1)),
	type_context:merge(Params2, Params1, Params3),
	unification:type_substitution(Type1, Params3, Type2),
	reduce_type(context(Kinds1, Types1), Type2, Type3).

% alias opaque
reduce_type(context(Kinds1, Types1), var(N, P, Pe, M, params(Params1)), opaque(Name)) :-
	type_context:get_from_context(context(Kinds1, Types1), var(N, P, Pe, M, params(Params1)), Types1, opaque(Name)).

% var (classic)
reduce_type(context(Kinds1, Types1), var(X, P, Pe, M, B), Type3) :-
	type_context:get_from_context(context(Kinds1, Types1), var(X, P, Pe, M, B), Types1, Type2),
	reduce_type(context(Kinds1, Types1), Type2, Type3).

% union
reduce_type(Context, union(U1), union(U2)) :-
	reduce_type(Context, U1, U2).

% tag
reduce_type(Context, ttag(Type1), ttag(Type2)) :-
	reduce_type(Context, Type1, Type2).
	
reduce_type(Context, var(N, P, Pe, M, B), ttag(N, empty)).

reduce_type(Context, empty, any).
reduce_type(Context, X, X).

reduce_types(Context, [], []).
reduce_types(Context, [H1 | T1], [H2 | T2]) :-
	reduce_type(Context, H1, H2),
	reduce_types(Context, T1, T2).

% ========================================

is_matching(Context, N, N).
is_matching(Context, X, any).
is_matching(Context, any, Y).
is_matching(Context, X, empty).
is_matching(Context, empty, Y).

is_matching(Context, X1, Y1) :-
	reduce_type(Context, X1, X2),
	reduce_type(Context, Y1, Y2),
	(is_same_type(Context, X2, Y2); is_subtype(Context, X2, Y2); is_subtype(Context, Y2, X2)).

is_matching(Context, X, Y) :-
		X =@= Y,
		writeln("Real matching error:"),
		write_canonical(X), write(" and "), write_canonical(Y), writeln(""),
		writeln(""),
		fail.

% ========================================

get_common_type_denominator(Context, trecord(R1), trecord(R2), trecord(R1)) :-
	all_in(R2, R1).

get_common_type_denominator(Context, trecord(R1), trecord(R2), trecord(R2)) :-
	all_in(R1, R2).

get_common_type_denominator(Context, X, X, X).

% ========================================

get_best_type(Context, any, Type2, Type2).
get_best_type(Context, Type2, any, Type2).
get_best_type(Context, Type, Type, Type).
get_best_type(Context, Type1, Type2, Type) :-
	reduce_type(Context, Type1, Type3),
	reduce_type(Context, Type2, Type4),
	unification:unification(Context, Type4, Type3, Tab),
	unification:type_substitution(Type1, Tab, Type).
