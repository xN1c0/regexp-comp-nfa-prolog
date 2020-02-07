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
    nfa_regexp_comp_helper(FA_Id, RE, Start, Final),
    !.

nfa_regexp_comp_helper(FA_Id, RE, Start, Final) :-
    atomic(RE),
    gensym(q_, Start),
    gensym(q_, Final),
    assert(nfa_delta(Fa_Id, RE, Start, Final)),
    !.

nfa_regexp_comp_helper(Fa_Id, ['[|]'|Rs], Start, Final) :-
    nfa_regexp_comp_helper_sequence(Fa_Id, Rs, Start, Final),
    !.

nfa_regexp_comp_helper_sequence(Fa_Id, [], Final, Final).

nfa_regexp_comp_helper_sequence(Fa_Id, [L], Start, Final) :-
    nfa_recompile_helper(Fa_Id, L, S, F),
    assert(nfa_delta(Fa_Id, L, epsilon, S, F)),
    nfa_regexp_comp_helper_sequence(Fa_Id, Rs, Start, Final).

nfa_regexp_comp_helper_sequence(Fa_Id, [First|Others], Start, Final) :-
    nfa_recompile_helper(Fa_Id, First, S, F),
    assert(nfa_delta(Fa_Id, epsilon, Start, S)),
    nfa_regexp_comp_helper_sequence(Fa_Id, Others, F, Final),
    !.


nfa_regexp_comp_split_case(Fa_Id, ['+'|Rs], Start, Final) :-
    nfa_regexp_comp_case_plus(Fa_Id, Rs, Start, Final),
    !.

nfa_regexp_comp_case_plus(Fa_Id, [], State1, State2) :-
    State2 = gensym(q_, Unique).

nfa_regexp_comp_case_plus(Fa_Id, Rs, Sate1, State2) :-
    nfa_delta(FA_Id, State1, epsilon, State2),
    nfa_delta(Fa_Id, State2).

nfa_regexp_comp_split_case(Fa_Id, ['*'|Rs], Start, Final) :-
    !.

nfa_regexp_comp_split_case(Fa_Id, ['/'|Rs], Start, Final) :-
    !.

nfa_regexp_comp_split_case(Fa_Id, ['[|]'|Rs], Start, Final) :-
    !.




























