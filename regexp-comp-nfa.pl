is_regexp(RE) :-
    nonvar(RE),
    is_regexp_helper(RE),
    !.

is_regexp_helper(RE) :-
    atomic(RE),
    !.

is_regexp_helper(RE) :-
    compound(RE),
    functor(RE, Name, _),
    Name \= '+',
    Name \= '*',
    Name \= '/',
    Name \= '[|]',
    !.

is_regexp_helper(RE) :-
    RE =.. Rs,
    regexp_split_case(Rs),
    !.

regexp_split_case(['+'|[Re1]]) :-
    is_regexp(Re1),
    !.

regexp_split_case(['*'|[Re1]]) :-
    is_regexp(Re1),
    !.

regexp_split_case(['/'|Rs]) :-
    is_regexp_check_all(Rs),
    !.

regexp_split_case(['[|]'|Rs]) :-
    is_regexp_check_all(Rs),
    !.

is_regexp_check_all([]).

is_regexp_check_all([Head|Tail]) :-
    is_regexp(Head),
    is_regexp_check_all(Tail),
    !.

%%%%%%%%%%%%%%%%%%%%%%%%%

/*
%OR 2 STATES
%CONCAT n+1 STATES
%STAR 4 STATES
%PLUS 3 STATES

a) 1 -a-> 2

nfa_delta(FA_Id, State1, Transition, State2)
nfa_delta(xxx, 1, a, 2).
*/

nfa_regexp_comp(FA_Id, RE) :-
    nonvar(FA_Id),
    is_regexp(RE),
    RE =.. Rs,
    nfa_regexp_comp_helper(FA_Id, Rs, Start, Final),
    assert(nfa_start(FA_Id, Start)),
    assert(nfa_final(FA_Id, Final)),
    !.

/*
BASE
*/
nfa_regexp_comp_base(FA_Id, Base, S, F) :-
    atomic(Base),
    nfa_regexp_comp_helper(FA_Id, Base, S, F),
    !.

nfa_regexp_comp_base(FA_Id, Base, S, F) :-
    Base =.. Bx,
    nfa_regexp_comp_helper(FA_Id, Bx, S, F),
    !.

nfa_regexp_comp_helper(FA_Id, Rs, Start, Final) :-
    atomic(Rs),
    gensym(q_, Start),
    gensym(q_, Final),
    assert(nfa_delta(FA_Id, Rs, Start, Final)),
    !.

/*
STAR
*/
nfa_regexp_comp_helper(FA_Id, ['*'|Rs], Start, Final) :-
    gensym(q_, Start),
    gensym(q_, Final),
    Rs =.. ['[|]',Rx|_],
    nfa_regexp_comp_base(FA_Id, Rx, S, F),
    assert(nfa_delta(FA_Id, epsilon, Start, Final)),
    assert(nfa_delta(FA_Id, epsilon, Start, S)),
    assert(nfa_delta(FA_Id, epsilon, F, Final)),
    assert(nfa_delta(FA_Id, epsilon, F, S)),
    !.

/*
PLUS
*/
nfa_regexp_comp_helper(FA_Id, ['+'|Rs], Start, Final) :-
    gensym(q_, Start),
    gensym(q_, Final),
    Rs =.. ['[|]',Rx|_],
    nfa_regexp_comp_base(FA_Id, Rx, S, F),
    assert(nfa_delta(FA_Id, epsilon, Start, S)),
    assert(nfa_delta(FA_Id, epsilon, F, Final)),
    assert(nfa_delta(FA_Id, epsilon, F, S)),
    !.

/*
SEQUENCE
*/
nfa_regexp_comp_helper(FA_Id, ['[|]'|Rs], Start, Final) :-
    gensym(q_, Start),
    plain(Rs, Rx),
    nfa_regexp_comp_helper_sequence(FA_Id, Rx, Start, Final),
    !.

nfa_regexp_comp_helper_sequence(FA_Id, [], Final, Final).

nfa_regexp_comp_helper_sequence(FA_Id, [First], Start, Final) :-
    nfa_regexp_comp_base(FA_Id, First, S, F),
    gensym(q_, Final),
    assert(nfa_delta(FA_Id, epsilon, Start, S)),
    assert(nfa_delta(FA_Id, epsilon, F, Final)),
    !.

nfa_regexp_comp_helper_sequence(FA_Id, [First|Others], Start, Final) :-
    nfa_regexp_comp_base(FA_Id, First, S, F),
    assert(nfa_delta(FA_Id, epsilon, Start, S)),
    nfa_regexp_comp_helper_sequence(FA_Id, Others, F, Final),
    !.

/*
OR
*/
nfa_regexp_comp_helper(FA_Id, ['/'|Rs], Start, Final) :-
    gensym(q_, Start),
    gensym(q_, Final),
    nfa_regexp_comp_helper_or(FA_Id, Rs, Start, Final),
    !.

nfa_regexp_comp_helper_or(FA_Id, [], _, _).

nfa_regexp_comp_helper_or(FA_Id, [First|Others], Start, Final) :-
    nfa_regexp_comp_base(FA_Id, First, S, F),
    assert(nfa_delta(FA_Id, epsilon, Start, S)),
    assert(nfa_delta(FA_Id, epsilon, F, Final)),
    nfa_regexp_comp_helper_or(FA_Id, Others, Start, Final),
    !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


nfa_rec(FA_Id, Input) :-
    nfa_start(FA_Id, Start),
    nfa_rec(FA_Id, Input, Start, Final),
    !,
    nfa_final(FA_Id, Final).

nfa_rec(FA_Id, [], Final, Final) :-
    nfa_final(FA_Id, Final),
    !.

nfa_rec(FA_Id, [], Start, Final) :-
    nfa_delta(FA_Id, epsilon, Start, X),
    nfa_rec(FA_Id, [], X, Final),
    !.

nfa_rec(FA_Id, Input, Start, Final) :-
    nfa_delta(FA_Id, epsilon, Start, X),
    nfa_rec(FA_Id, Input, X, Final),
    !.

nfa_rec(FA_Id, [First|Others], Start, Final) :-
    nfa_delta(FA_Id, First, Start, X),
    nfa_rec(FA_Id, Others, X, Final),
    !.

%%%%%%%%%%%%%%%%%%%%%

nfa_clear() :-
    retractall(nfa_delta(_, _, _, _)),
    retractall(nfa_start(_, _)),
    retractall(nfa_final(_, _)).

nfa_clear(FA_Id) :-
    retractall(nfa_delta(FA_Id, _, _, _)),
    retractall(nfa_start(FA_Id, _)),
    retractall(nfa_final(FA_Id, _)).

%%%%%%%%%%%%%%%%%%%%%%%%%
/* PREDICATI MAGGGICI*/
plain(L1,L2) :- plain(L1,[],L2).

plain([], ACC, ACC).

plain([H|REST], ACC, L2) :-
    H = [_|_],
    plain(H,ACC,ACC1),
    plain(REST,ACC1,L2),
    !.

plain([H|REST],ACC,L2) :-
    append(ACC,[H],ACC1),
    plain(REST,ACC1,L2),
    !.

plain(X,ACC,L2) :-
    append(ACC,[X],L2),
    !.
