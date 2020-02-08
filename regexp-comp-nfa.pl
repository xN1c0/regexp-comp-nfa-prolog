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
    assert(nfa_start(Fa_Id, Start)),
    assert(nfa_final(Fa_Id, Final)),
    !.

nfa_regexp_comp_helper(FA_Id, [Rs], Start, Final) :-
    atomic(Rs),
    gensym(q_, Start),
    gensym(q_, Final),
    assert(nfa_delta(Fa_Id, Rs, Start, Final)),
    !.

/*
STAR
*/
nfa_regexp_comp_helper(Fa_Id, ['*'|Rs], Start, Final) :-
    gensym(q_, Start),
    gensym(q_, Final),
    nfa_regexp_comp_helper(Fa_Id, Rs, S, F),
    assert(nfa_delta(Fa_Id, epsilon, Start, Final)),
    assert(nfa_delta(Fa_Id, epsilon, Start, S)),
    assert(nfa_delta(Fa_Id, epsilon, F, Final)),
    assert(nfa_delta(Fa_Id, epsilon, F, S)),
    !.

/*
PLUS
*/
nfa_regexp_comp_helper(Fa_Id, ['+'|Rs], Start, Final) :-
    nfa_regexp_comp_helper(Fa_Id, [Rs,*(Rs)]),
    !.

/*
SEQUENCE
*/
nfa_regexp_comp_helper(Fa_Id, ['[|]'|Rs], Start, Final) :-
    nfa_regexp_comp_helper_sequence(Fa_Id, Rs, Start, Final),
    !.

nfa_regexp_comp_helper_sequence(Fa_Id, [], Final, Final).

nfa_regexp_comp_helper_sequence(Fa_Id, [First|Others], Start, Final) :-
    nfa_regexp_comp_helper(Fa_Id, First, S, F),
    assert(nfa_delta(Fa_Id, epsilon, Start, S)),
    nfa_regexp_comp_helper_sequence(Fa_Id, Others, F, Final),
    !.

/*
OR
*/
nfa_regexp_comp_helper(Fa_Id, ['/'|Rs], Start, Final) :-
    gensym(q_, Start),
    gensym(q_,Final),
    nfa_regexp_comp_helper_or(Fa_Id, Rs, S, F),
    !.

nfa_regexp_comp_helper_or(Fa_Id, [], Final, Final).

nfa_regexp_comp_helper_or(Fa_Id, [First|Others], Start, Final) :-
    nfa_regexp_comp_helper(Fa_Id, First, S, F),
    assert(nfa_delta(Fa_Id, epsilon, Start, S)),
    assert(nfa_delta(Fa_Id, epsilon, F, Final)),
    nfa_regexp_comp_helper_or(Fa_Id, Others, Start, Final),
    !.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


nfa_rec(Fa_Id, Input) :-
    nfa_start(Fa_Id, Start),
    nfa_rec(Fa_Id, Input, Start, Final),
    !,
    nfa_final(Fa_Id, Final).

nfa_rec(Fa_Id, [], Final, Final) :-
    !.

nfa_rec(Fa_Id, [First|Others], Start, Final) :-
    nfa_delta(Fa_Id, First, Start, X),
    nfa_rec(Fa_Id, Others, X, Final),
    !.

nfa_rec(Fa_Id, Input, Start, Final) :-
    nfa_delta(Fa_Id, epsilon, Start, X),
    nfa_rec(Fa_Id, Input, X, Final),
    !.





















