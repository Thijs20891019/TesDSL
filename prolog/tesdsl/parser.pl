:- module(parser, [
    parse//1
]).


space --> [C],
    {char_type(C,space)}, !,
    space.

space --> [].


find_label([C|Code]) --> [C],
    {char_type(C,csym); C=40; C=41; C=43; C=44; C=45; C=46; C=47; C=62; C=91; C=93},
    find_label(Code), !.

find_label([C]) --> [C],
    {char_type(C,csym); C=40; C=41; C=43; C=44; C=45; C=46; C=47; C=62; C=91; C=93}.

label(Label) --> space,
    find_label(Code),
    {atom_codes(Label,Code)}.

multi_label([Label|Rest]) -->
    label(Label),
    multi_label(Rest), !.

multi_label([]) --> [].


find_action([Label,Actions]) -->
    label(Label),
    list(Actions).

find_action([>,Label]) --> 
    space, [C], {C=62},
    label(Label).

find_action(Labels) -->
    multi_label(Labels).

action(Action) -->
    find_action(Action),
    space, ";".


find_list([Action|Rest]) -->
    action(Action),
    find_list(Rest), !.
    
find_list([]) --> [].

list(Actions) -->
    space, "{", space,
    find_list(Actions),
    space, "}".


actionlist([Label,Actions]) --> 
    label(Label),
    list(Actions).


find_actionlists([ActionList|Rest]) -->
    actionlist(ActionList),
    find_actionlists(Rest), !.

find_actionlists([]) --> [].


parse(ActionLists) -->
    find_actionlists(ActionLists).