/*
 *checks that the input is not a varible
 *and calls an auxiliar predicate
 */
is_regexp(RE) :-
    nonvar(RE),
    is_regexp_helper(RE),
    !.

/*
 * base case for atoms
 */
is_regexp_helper(RE) :-
    atomic(RE),
    !.

/*
 * base case for functors
 */
is_regexp_helper(RE) :-
    compound(RE),
    functor(RE, Name, _),
    Name \= '+',
    Name \= '*',
    Name \= '/',
    Name \= '[|]',
    !.

/*
 * calls a second auxiliar predicate
 */
is_regexp_helper(RE) :-
    RE =.. Rs,
    regexp_split_case(Rs),
    !.

/*
 * recursive case for the '+' (PLUS) functor
 */
regexp_split_case(['+' | [Re1]]) :-
    is_regexp(Re1),
    !.

/*
 * recursive case for the '*' (STAR) functor
 */
regexp_split_case(['*' | [Re1]]) :-
    is_regexp(Re1),
    !.

/*
 * predicate for the '/' (OR) functor
 * calls an auxiliar predicate for the recursion
 */
regexp_split_case(['/' | Rs]) :-
    is_regexp_check_all(Rs),
    !.

/*
 * predicate for the '[]' (SEQUENCE) functor
 * calls an auxiliar predicate for the recursion
 */
regexp_split_case(['[|]' | Rs]) :-
    is_regexp_check_all(Rs),
    !.

/*
 * base case for empty list
 */
is_regexp_check_all([]).

/*
 * recursive case to verify
 * the regularity of an expression
 */
is_regexp_check_all([Head | Tail]) :-
    is_regexp(Head),
    is_regexp_check_all(Tail),
    !.

%%%%%%%%%%%%%%%%%%%%%%%%%

/*
 * checks the validity of the inputs
 * calls a second predicate to create the nfa
 * saves in memory the initial and the final state
 * of the complete nfa
 */
nfa_regexp_comp(FA_Id, RE) :-
    nonvar(FA_Id),
    is_regexp(RE),
    nfa_regexp_comp_base(FA_Id, RE, Start, Final),
    assert(nfa_start(FA_Id, Start)),
    assert(nfa_final(FA_Id, Final)),
    !.

/*
BASE
*/

/*
 * case for atoms
 * calls recursion without modifying the re in input
 */
nfa_regexp_comp_base(FA_Id, Base, S, F) :-
    atomic(Base),
    nfa_regexp_comp_helper(FA_Id, Base, S, F),
    !.

/*
 * general case
 * calls recursion appending the list with the functor of the list
 */
nfa_regexp_comp_base(FA_Id, Base, S, F) :-
    Base =.. Sub_base,
    nfa_regexp_comp_helper(FA_Id, Sub_base, S, F),
    !.


/*
 * base case for atoms
 * which generates two states
 * and saves in memory the delta between the two
 */
nfa_regexp_comp_helper(FA_Id, Sub_re, Start, Final) :-
    atomic(Sub_re),
    gensym(q_, Start),
    gensym(q_, Final),
    assert(nfa_delta(FA_Id, Sub_re, Start, Final)),
    !.

/*
STAR
*/

/*
 * recursive case for '*' (STAR) functor
 * which generates two states
 * extracts the re
 * calls recursion with an auxiliar procedure
 * saves in memory the epsilon deltas
 * to connect previous initial and final states
 * with the new ones and to accept the empty list
 */
nfa_regexp_comp_helper(FA_Id, ['*' | Sub_re], Start, Final) :-
    gensym(q_, Start),
    gensym(q_, Final),
    Sub_re =.. ['[|]', Rx | _],
    nfa_regexp_comp_base(FA_Id, Rx, S, F),
    assert(nfa_delta(FA_Id, epsilon, Start, Final)),
    assert(nfa_delta(FA_Id, epsilon, Start, S)),
    assert(nfa_delta(FA_Id, epsilon, F, Final)),
    assert(nfa_delta(FA_Id, epsilon, F, S)),
    !.

/*
PLUS
*/

/*
 * recursive case for '+' (PLUS) functor
 * which generates two states
 * extracts the re
 * calls recursion with an auxiliar procedure
 * saves in memory the epsilon deltas
 * to connect previous initial and final states
 * with the new ones
 * (without accepting the empty list)
 */
nfa_regexp_comp_helper(FA_Id, ['+' | Sub_re_list], Start, Final) :-
    gensym(q_, Start),
    gensym(q_, Final),
    Sub_re_list =.. ['[|]', Sub_re | _],
    nfa_regexp_comp_base(FA_Id, Sub_re, S, F),
    assert(nfa_delta(FA_Id, epsilon, Start, S)),
    assert(nfa_delta(FA_Id, epsilon, F, Final)),
    assert(nfa_delta(FA_Id, epsilon, F, S)),
    !.

/*
SEQUENCE
*/

/*
 * case for '[]' (SEQUENCE) functor
 * generates the initial state
 * calls a procedure to make a multy-leveled list
 * into an one-level list
 * calls an auxiliar procedure
 * to recursively examine all the elements in the list
 */
nfa_regexp_comp_helper(FA_Id, ['[|]' | Sub_re_list], Start, Final) :-
    gensym(q_, Start),
    plain(Sub_re_list, Sub_re_plain),
    nfa_regexp_comp_helper_sequence(FA_Id, Sub_re_plain, Start, Final),
    !.

/*
 * base case for empty list
 */
nfa_regexp_comp_helper_sequence(_, [], Final, Final).

/*
 * case for one-element list
 * calls recursion with an auxiliar procedure
 * to create the nfa of the last re of the sequence
 * generates the final state
 * saves in memory the epsilon deltas
 * to connect previous initial and final states with the new ones
 */
nfa_regexp_comp_helper_sequence(FA_Id, [First], Start, Final) :-
    nfa_regexp_comp_base(FA_Id, First, S, F),
    gensym(q_, Final),
    assert(nfa_delta(FA_Id, epsilon, Start, S)),
    assert(nfa_delta(FA_Id, epsilon, F, Final)),
    !.

/*
 * recursive case
 * calls recursion with an auxiliar procedure
 * to create the nfa of the first re of the sequence
 * saves in memory the epsilon delta
 * to connect the old initial state with the new one
 * calls recursion for the rest of the list
 */
nfa_regexp_comp_helper_sequence(FA_Id, [First | Others], Start, Final) :-
    nfa_regexp_comp_base(FA_Id, First, S, F),
    assert(nfa_delta(FA_Id, epsilon, Start, S)),
    nfa_regexp_comp_helper_sequence(FA_Id, Others, F, Final),
    !.

/*
OR
*/

/*
 * case for '/' (OR) functor
 * generates the initial and the final states
 * calls an auxiliar procedure
 * to recursively examine all the elements in the list
 */
nfa_regexp_comp_helper(FA_Id, ['/' | Sub_re_list], Start, Final) :-
    gensym(q_, Start),
    gensym(q_, Final),
    nfa_regexp_comp_helper_or(FA_Id, Sub_re_list, Start, Final),
    !.

/*
 * base case for empty list
 */
nfa_regexp_comp_helper_or(_, [], _, _).

/*
 * recursive case
 * calls recursion with an auxiliar procedure
 * to create the nfa of the first re of the sequence
 * saves in memory the epsilon deltas
 * to connect the old initial and final states with the new ones
 * calls recursion for the rest of the list
 */
nfa_regexp_comp_helper_or(FA_Id, [First | Others], Start, Final) :-
    nfa_regexp_comp_base(FA_Id, First, S, F),
    assert(nfa_delta(FA_Id, epsilon, Start, S)),
    assert(nfa_delta(FA_Id, epsilon, F, Final)),
    nfa_regexp_comp_helper_or(FA_Id, Others, Start, Final),
    !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
 * calls a second procedure (with ariety 4)
 * with added arguments the initial and the final states
 * of the nfa with a specific ID
 */
nfa_rec(FA_Id, Input) :-
    nfa_start(FA_Id, Start),
    nfa_rec(FA_Id, Input, Start, Final),
    !,
    nfa_final(FA_Id, Final).

/*
 * base case for empty list in final state
 */
nfa_rec(FA_Id, [], Final, Final) :-
    nfa_final(FA_Id, Final),
    !.

/*
 * recursive case
 * looks for epsilon deltas that could lead to a final state
 * even if the list is already empty
 * calls recursion with the next state
 */
nfa_rec(FA_Id, [], Start, Final) :-
    nfa_delta(FA_Id, epsilon, Start, X),
    nfa_rec(FA_Id, [], X, Final),
    !.

/*
 * recursive case
 * looks for epsilon deltas but the list is not empty
 * calls recursion with the next state
 */
nfa_rec(FA_Id, Input, Start, Final) :-
    nfa_delta(FA_Id, epsilon, Start, X),
    nfa_rec(FA_Id, Input, X, Final),
    !.

/*
 * recursive case
 * looks for specific delta with a given atom
 * (first element of the list in input)
 * calls recursion with the next state
 */
nfa_rec(FA_Id, [First | Others], Start, Final) :-
    nfa_delta(FA_Id, First, Start, X),
    nfa_rec(FA_Id, Others, X, Final),
    !.

%%%%%%%%%%%%%%%%%%%%%

/*
 * deletes all the nfa in memory
 */
nfa_clear() :-
    retractall(nfa_delta(_, _, _, _)),
    retractall(nfa_start(_, _)),
    retractall(nfa_final(_, _)).

/*
 * deletes the nfa with a specific ID
 */
nfa_clear(FA_Id) :-
    retractall(nfa_delta(FA_Id, _, _, _)),
    retractall(nfa_start(FA_Id, _)),
    retractall(nfa_final(FA_Id, _)).

%%%%%%%%%%%%%%%%%%%%%

/*
 * calls an auxiliar procedure that has ariety 3
 */
plain(List, Plain_List) :-
    plain(List, [], Plain_List),
    !.

/*
 * base case for empty list
 */
plain([], Acc, Acc).

/*
 * recursive case
 * for the elements that are list
 */
plain([Head | Rest], Acc, Plain_List) :-
    Head = [_ | _],
    plain(Head, Acc, Acc1),
    plain(Rest, Acc1, Plain_List),
    !.

/*
 * recursive case
 * for elements that are not lists
 */
plain([Head | Rest], Acc, Plain_List) :-
    append(Acc, [Head], Acc1),
    plain(Rest, Acc1, Plain_List),
    !.

/*
 * recursive case
 * for single elements that are not in a list
 */
plain(X, Acc, Plain_List) :-
    append(Acc, [X], Plain_List),
    !.
