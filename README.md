README.txt of the PROJECT IN PROLOG JANUARY 2020
Compilation of regular expressions in non-deterministic automata

The purpose of this project is to implement in Prolog language a compiler from regexps to NFA.

OPERATION
      There is no need for explanation since prolog is a logical language.

THE OPERATORS
SEQUENCE --> [<re1>, <re2>, ... , <ren>]
OR --> /(<re1>, <re2>, ... , <ren>)
STAR --> *(<re>)
PLUS --> +(<re>)

THE PREDICATES

is_regexp(RE)		Checks that the input is not a variable and calls the
			is_regexp_helper predicate.

is_regexp_helper(RE)	Returns true if the regexp is atomic or if it is compound and
			has the functor different from the four operators. (base cases)
			Recursive case: applies the unit predicate to the regexp and
			calls the regexp_split_case predicate.

regexp_split_case([OP|[RE]])
			In case the functor is the operator STAR or PLUS the predicate
			calls is_regexp with the rest of the regexp as argument.
			In case the functor is the operator SEQUENCE or OR the predicate
			calls is_regexp_check_all because the two operator could have
			more than one element.

is_regexp_check_all(RE)
			Returns true if the list is empty. (base case)
			Calls is regexp_for the first element of the regexp and
			calls recursively itself for the rest of the regexp.

nfa_regexp_comp(FA_Id, RE)
			Checks the validity of the NFA's ID and checks that the second
			argument is a regexp.
			Calls the nfa_regexp_comp_helper with ariety 4 predicate
			giving as arguments the NFA's ID, the regexp with previously
			applied the univ predicate, NFA's initial and final states.

nfa_regexp_comp_base(FA_Id, Base, S, F)
			Auxiliar step for the recursion of nfa_regexp_comp_helper to assure
			that in case the regexp is already in the atomic form it will not be
			extracted and considered as a sequence of a single atom.
			In case of a non-atomic regexp the predicate applies the univ predicate
			to the regexp and calls the recursion of nfa_regexp_comp_helper.

nfa_regexp_comp_helper(FA_Id, RE, Start, Final)
			In base case, when the regexp is an atom, the initial and final states
			are generated and the delta between the two is saved in memory with the
			value of the atom.
			Every next recursive case changes its procedure depending on the
			operator that will be recognized.
			In case of the STAR operator, the predicate generates two new states
			and calls the nfa_regexp_comp_base procedure. Saves all the epsilon
			deltas needed in memory.
			In case of the PLUS operator, the procedure is the same of the STAR
			operator, but the epsilon delta that allows to accept the empty list
			is missing.
			In case of the SEQUENCE operator, the predicate generates an initial
			state and flattens the regexp using the plain predicate to assure not
			to have any further complications with nesting of lists. Calls the
			nfa_regexp_comp_helper_sequence predicate to create the NFA.
			In case of the OR operator, the predicate generates both an initial
			and a final new states and calls the nfa_regexp_comp_helper_or that
			will call the recursion to create the NFA.

nfa_regexp_comp_helper_sequence(FA_Id, RE, Start, Final)
			The first base case returns true if the list is empty.
			The first recursive case calls nfa_regexp_comp_base if there's just
			one element in the list, which corresponds to the last element of the
			list. The final state is genereted and the epsilon deltas needed are
			saved in memory.
			The second recursive case calls nfa_regexp_comp_base on the first
			element of the list, saves in memory the epsilon delta to link the
			first element of the sequence with its next and recursively calls
			itself for the rest of the list.

nfa_regexp_comp_helper_or(Fa_Id, RE, Start, Final)
			The base case returns true with the empty list.
			The recursive case calls nfa_regexp_comp_base on the first element of
			the list, saves in memory the epsilon deltas to link the previous
			initial and final states with the new ones and recursively calls itself
			for the rest of the list.

nfa_rec(FA_Id, Input)
			Predicate with ariety 2 that calls another predicate with the same
			arguments and two more that are the NFA's initial and final states.

nfa_rec(FA_Id, Input, Start, Final)
			The base case returns true if the input is empty and the NFA is in
			a final state.
			First recursive case, the input is empty, but there are epsilon deltas
			that could lead to the final state.
			Second recursive case, the input is not empty, but there are epsilon
			deltas that could lead to the next atom.
			Third recursive case, the input is divided into head and rest and
			the predicate looks for a delta with the value of the head and then
			calls recursion.

nfa_clear()
			Deletes all the datas of NFAs saved in memory.

nfa_clear(FA_Id)
			Deletes all the data of the NFA with a specific ID.

plain(List, Plain_List)
			This predicate flattens the list given.
