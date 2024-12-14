:- module(type_checker, [typing/3]).
:- use_module(type_context).
:- use_module(unification).
:- use_module(kinds).
:- use_module(type_comparison).
:- use_module(type_module).

% ========================================

get_tag_names([], []).
get_tag_names([ttag(Name, Type) | T1], [Name, T2]) :-
	get_tag_names(T1, T2).

get_tag_names([tag(Name, Type) | T1], [Name, T2]) :-
	get_tag_names(T1, T2).

get_tag_names([N], _) :-
	format("~w is not a tag"),
	halt.

get_tag_names(A, _) :-
	format("The element ~w shouldn't be there, get_tag_names can only work with array of tags", [A]), halt.

% ========================================

subset([], _).
subset([X | Rest], List) :-
	member(X, List),
	subset(Rest, List).

% ========================================

same_values(L1, L2) :-
	length(L1, N), length(L2, N),
	subset(L1, L2),
	subset(L2, L1).

% ========================================

% for function application 
split_at_n(0, List, [], List) :- !.
split_at_n(_, [], [], []). 
split_at_n(N, [H|T], [H|FirstN], Rest) :-
    N > 0,                           
    N1 is N - 1,                     
    split_at_n(N1, T, FirstN, Rest). 

% ========================================

unify_types([], any).
unify_types([Type], Type).
unify_types([H1, H2 | T], Type2) :-
	unify_type(H1, H2, Type1),
	unify_types([Type1 | T], Type2).

% ========================================

% Vérifie si une lettre existe déjà dans la liste
contains_key(Key, [[Key, _] | _]). % Cas où la lettre est en tête de la liste
contains_key(Key, [[_, V] | Tail]) :- 
    contains_key(Key, Tail). % Recherche dans le reste de la liste

% ========================================

% Ajoute une paire si la clé n'existe pas déjà dans la liste
add_if_not_present([Key, Value], List, List) :- 
    contains_key(Key, List). % Si la lettre existe, ne rien ajouter
add_if_not_present([Key, Value], List, [[Key, Value]|List]). % Sinon, ajoute la paire

% ========================================

% Combine deux listes sans doublons de lettres
record_union([], List, List). % Si la première liste est vide, retourne la seconde
record_union([[Key, Value]|Rest], List2, Result) :-
    add_if_not_present([Key, Value], List2, TempList), % Ajoute la paire si elle n'existe pas
    record_union(Rest, TempList, Result). % Continue avec le reste de la première liste

% ========================================

eval(Context, comment, Context).

%type alias declaration
eval(Context, decl(var(X, P, Pe, M, params(Params)), params(Params), Type), Context2) :-
	kinds:get_gen_kind(Type, Kinds),
	type_context:concat_context(Context, context([], [[var(X, P, Pe, M, params(Params)), talias(Kinds, Params, Type)]]), Context2).

% let 
eval(Context, decl(var(X, P, Pe, M, B), Type0, Exp), Context2) :-
	type_comparison:reduce_type(Context, Type0, Type1),
	typing(Context, Exp, Type2),
	type_comparison:is_matching(Context, Type2, Type1),
	type_comparison:get_best_type(Context, Type0, Type2, Type),
	type_context:concat_context(Context, context([], [[var(X, P, Pe, M, B), Type]]), Context2).

eval(Context, decl(_, Type, Exp), Context) :-
	typing(Context, Exp, Type2),
	writeln("Error:"),
	write("The expression '"), write(Exp), writeln("'"),
	write("evaluate to '"), write(Type2), writeln("'"),
	write("but dont evaluate to '"), write(Type), write("'"),
	writeln(""), halt.

eval(Context, mod(Name, Exps), context(Kinds, Decl2)) :-
	typing(context([], []), sequence2(Exps), context(Kinds, Decl1)),
	type_module:add_path(Name, Decl1, opaques([]), Decl2, _).

eval(context(Kinds, Types), assign(var(Name, Path, Permission, MutOpa, Type), Exp), context(Kinds, Types)) :-
	type_context:get_from_context(context(Kinds, Types), var(Name, Path, Permission, true, Type), Types, mutable),
	% type_comparison:reduce_type(context(Kinds, Types), var(Name, Path, Permission, true, Type), mutable),
	typing(context(Kinds, Types), var(Name, Path, Permission, true, Type), Type2),
	typing(context(Kinds, Types), Exp, Type3),
	type_comparison:is_matching(Context, Type2, Type3).

eval(context(Kinds, Types), assign(var(Name, Path, Permission, MutOpa, Type), Exp), context(Kinds, Types)) :- 
	type_context:get_from_context(context(Kinds, Types), var(Name, Path, Permission, true, Type), Types, mutable),
	% type_comparison:reduce_type(context(Kinds, Types), var(Name, Path, Permission, true, Type), mutable),
	typing(context(Kinds, Types), var(Name, Path, Permission, flase, Type), Type2),
	typing(context(Kinds, Types), Exp, Type3),
	write("Error: '"), write(Name), write("' is of type: "), writeln(Type2),
	write("You can't assign a element of type: "), writeln(Type3),
	writeln(""), halt.

% ========================================

union_append(Unions1, Element, Unions1) :-
	member(Element, Unions1).
union_append(Unions1, Element, Unions2) :-
	append(Unions1, [Element], Unions2).

% ========================================

%var(Name, Path, Permission, Mutability, Base)
typing(Context, sequence([]), empty_sequence_failed_parsing).

% typing values
typing(Context, true, bool).
typing(Context, false, bool).

typing(Context, chars(Chr), chars).

typing(Context, empty, any).

typing(Context, and(X, Y), bool) :- 
		typing(Context, X, bool),
		typing(Context, Y, bool).

typing(Context, or(X, Y), bool) :- 
		typing(Context, X, bool),
		typing(Context, Y, bool).

typing(Context, rec_union(X, Y), trecord(R3)) :- 
		typing(Context, X, Type1), 
		type_comparison:reduce_type(Context, Type1, trecord(R1)),
		typing(Context, Y, Type2),
		type_comparison:reduce_type(Context, Type2, trecord(R2)),
		record_union(R2, R1, R3). % because the sens is inversed

typing(Context, eq(X, Y), bool) :- 
		typing(Context, X, T),
		typing(Context, Y, T).

typing(Context, lesser_or_equal(X, Y), bool) :- 
		typing(Context, X, T),
		typing(Context, Y, T).

typing(Context, greater_or_equal(X, Y), bool) :- 
		typing(Context, X, T),
		typing(Context, Y, T).

typing(Context, greater_than(X, Y), bool) :- 
		typing(Context, X, T),
		typing(Context, Y, T).

typing(Context, lesser_than(X, Y), bool) :- 
		typing(Context, X, T),
		typing(Context, Y, T).

% dot
typing(Context, dot(0, Val), T) :-
	typing(Context, Val, ttag(X, T)).

typing(Context, dot(Num, Val), T) :-
	number(Num),
	typing(Context, Val, trecord(R1)),
	type_context:get_right_element(var(Num), R1, T).

typing(context(Kinds, Types), dot(var(N1, _, _, _, _), var(Name2, Path2, Perm2, MutOpa, Typ)), Type2) :-
	type_comparision:reduce_type(Context, var(Name2, Path2, Perm2, MutOpa, Typ), trecord(R1)),
	type_context:get_right_element(var(N1), R1, Type2).

typing(context(Kinds, Types), dot(var(N1, _, _, _, _), Rest), Type2) :- 
	typing(context(Kinds, Types), Rest, trecord(R1)),
	type_context:get_right_element(var(N1), R1, Type2).

typing(Context, dot(fn_app(Var, values(Val)), Element), Type) :-
	append([Element], Val, Val2),
	typing(Context, fn_app(Var, values(Val2)), Type).

% pipe
typing(context(Kinds, Types), pipe(var(N1, _, _, _, _), var(Name2, Path2, Perm2, MutOpa, Typ)), Type2) :-
	type_comparison:reduce_type(Context, var(Name2, Path2, Perm2, MutOpa, Typ), trecord(R1)),
	type_context:get_right_element(var(N1), R1, Type2).

typing(context(Kinds, Types), pipe(var(N1, _, _, _, _), Rest), Type2) :-
	typing(context(Kinds, Types), Rest, trecord(R1)),
	type_context:get_right_element(var(N1), R1, Type2).

typing(Context, pipe(fn_app(Var, values(Val)), Element), Type) :- 
	append([Element], Val, Val2),
	typing(Context, fn_app(Var, values(Val2)), Type).

typing(Context, fn(Kinds, Params, Type, Body), tfn(Kinds2, Types, Type)) :-
	type_context:get_right_elements(Params, Types),
	kinds:get_gen_kind(Types, Kinds2).

typing(context(Kinds, Types), var(X, Y, Z, V, W), T) :- 
	type_comparison:reduce_type(context(Kinds, Types), var(X, Y, Z, V, W), T).

% for values
typing(Context, values([]), []).
typing(Context, values([H | T]), [Type | Types]) :-
	typing(Context, H, Type),
	typing(Context, values(T), Types).

% for values2
typing(Context, values2([]), []).
typing(Context, values2([H | T]), [Type | Types]) :-
	(number(H) -> 
		Type = H 
		;
		typing(Context, H, Type)
	),
	typing(Context, values2(T), Types).

typing(Context, fn_app(var(X, P, Pe, M, B), values(V)), Type2) :-
	typing(Context, values(V), [FirstType | Rest]),
	typing(Context, var(X, P, Pe, M, FirstType), tfn(Kinds, Types, Type)),
	unification:unification(Context, values(V), [FirstType | Rest], Types, Match),
	unification:type_substitution(Type, Match, Type2).

typing(Context, fn_app(var(X, P, Pe, M, B), values([])), Type2) :-
	typing(Context, var(X, P, Pe, M, any), tfn(Kinds, Types, Type2)).

typing(Context, tag(Name, empty), ttag(Name, empty)).

typing(Context, tag(Name, Value), ttag(Name, Type)) :-
	typing(Contex, Value, Type).

typing(Context, sequence2([]), Context).
typing(Context, sequence2([H | T]), Context3) :- 
	% get permission
	% set permission public
	eval(Context, H, Context2),
	% context set H original permission
	typing(Context2, sequence2(T), Context3).

typing(Context, sequence([X]), Type) :- typing(Context, X, Type).

typing(Context, sequence([H | T]), Type) :- 
	eval(Context, H, Context2),
	typing(Context2, sequence(T), Type).

typing(Context, if(Cond, True, empty), T) :-
	typing(Context, Cond, bool),
	typing(Context, True, T).

typing(Context, if(Cond, True, Else), T3) :-
	typing(Context, Cond, bool),
	typing(Context, True, T1),
	typing(Context, Else, T2),
	unify_type(T1, T2, T3).

% IF ERRORS
% bad condition
typing(Context, if(Cond, True, Else), TERROR) :- 
	typing(Context, Cond, T),
	writeln("Type error"),
	write("The condition '"),
	write(Cond),
	writeln("' "),
	writeln("Doesn't evaluate to a boolean value."),
	writeln(""), halt.

typing(Context, array(V), tarray(N, Type)) :- 
	typing(Context, values(V), Types),
	same(Types),
	first(Types, Type),
	length(V, N).

typing(Context, match(Val, Branches), T) :-
	type_comparison:reduce_type(Context, Val, union(U1)),
	get_tag_names(U1, TagNames1),
	type_context:get_left_elements(Branches, RightValues),
	get_tag_names(RightValues, TagNames2),
	same_values(TagNames1, TagNames2),
	type_context:get_right_elements(Branches, LeftValues),
	typing(Context, values(LeftValues), Types),
	unify_types(Types, T).

typing(Context, record([]), trecord([])).
typing(Context, record([[Label, Value] | Rest]), trecord([[Label, Type] | Rest2])) :-
	typing(Context, Value, Type),
	typing(Context, record(Rest), trecord(Rest2)).

% numbers
typing(Context, N, int) :- integer(N).
typing(Context, N, num) :- float(N).

typing(Context, array_indexing(Exp, values(Num)), Type) :-
	typing(Context, Exp, tarray(N, Type)),
	(Num < N -> 
		number(Num)
	;
		writeln("Error: "),
		pretty_print(Exp), write(" is of length "), writeln(N),
		write("But the given index is of size "), writeln(Num),
		writeln("tips: The indexing of a array always start with 0"),
		writeln("The max indexing size shoue(T), Type)ld always be strictly inferior to it's length"),
		halt
	).

% ========================================

unify_type(ttag(Name1, Params1), ttag(Name2, Params2), union([ttag(Name1, Params1), ttag(Name2, Params2)])).
unify_type(ttag(Name1, Params1), union(U1), union(U2)) :-
	union_append(U1, ttag(Name1, Params1), U2).

unify_type(union(U1), ttag(Name1, Params1), union(U2)) :-
	union_append(U1, ttag(Name1, Params1), U2).

unify_type(T, any, any).
unify_type(any, T, any).
unify_type(T, T, T).

unify_type(union(U1), T, any) :- 
	write("Error: '"), write(T), writeln("' is not a tag type."),
	write("It can't be associated with the union type "), writeln(union(U1)),
	write("tips: Try wrapping the resulting type in a tag"),
	writeln(""), halt.

unify_type(T, union(U1), any) :- 
	write("Error: '"), write(T), writeln("' is not a tag type."),
	write("It can't be associated with the union type "), writeln(union(U1)),
	writeln("Tips: Try wrapping the resulting type in a tag"),
	writeln(""), halt.

unify_type(T1, T2, any) :- 
	write("Error: '"), write(T1), write("' and '"), write(T2), writeln("' don't match."),
	writeln("Tips: each type returned by the if statement must be the same or all wrapped around tags"),
	writeln(""), halt.

% ========================================

same([]).
same([_]).
same([X, X| Rest]) :- same([X | Rest]). 

same([X, Y | Rest]) :- 
	writeln("Error"),
	writeln("The type:"),
	writeln(X),
	writeln("doesn't match the type:"),
	writeln(Y),
	writeln(""), halt.

% ========================================

first([H | _], H).
