:- module(display, [
    tree_display/1,
    tree_display/2,
    display_output/3
]).

:- use_module(tools).


tree_symbol_prefix_2([],    "    ").
tree_symbol_prefix_2([_|_], "│   ").
tree_symbol_prefix_1([_|_], "├── ").
tree_symbol_prefix_1([],    "└── ").


create_tree_branch(Prefix,I,L_i,Out):-
    tree_symbol_prefix_1(L_i,P_1),
    format(string(Out),"~w~w~w~n",[Prefix,P_1,I]).

prefix_tree_branch(Prefix,L_i,P_n):-
    tree_symbol_prefix_2(L_i,P_2),
    string_concat(Prefix,P_2,P_n).

tree_display_create([],[],_):-!.
tree_display_create([''],[],_):-!.
tree_display_create([""],[],_):-!.

tree_display_create([[I,L]|L_i],[Out|L_o],Prefix):-!,
    create_tree_branch(Prefix,I,L_i,O),
    prefix_tree_branch(Prefix,L_i,P_n),
    tree_display_create(L,Out_l,P_n),
    atomics_to_string([O|Out_l],"",Out),
    tree_display_create(L_i,L_o,Prefix).

tree_display_create([I|L_i],[Out|L_o],Prefix):-
    create_tree_branch(Prefix,I,L_i,Out),
    tree_display_create(L_i,L_o,Prefix).

tree_display(List,Text):-
    tree_display_create(List,Out_l,""),
    atomics_to_string(Out_l,"",Text).

tree_display(List):-
    tree_display(List,Text),
    write(Text).



display_output(Goals,Sols,Options):-
    findall(Out,create_output(Goals,Sols,Out,Options),[L|List]),
    atomics_to_string([L|List],"\n",Text),
    write_output(Text,Options).


create_output(_,[I|L],Out,Options):-
    \+member(show_true(false),Options),
    convert_list([I|L],List_T,_),
    create_display(List_T,"Succeeded",Out).

create_output([I|L],U,Out,Options):-
    member(show_false(true),Options),
    convert_list(U,_,List_S),
    subtract([I|L],List_S,List_F),
    create_display(List_F,"Failed",Out).

create_display([L|List],Prefix,Out):-
    tree_display_create([L|List],Output_list,""),
    atomics_to_string(Output_list,"│\n",Text),
    format(string(Out),"~w:~n~w",[Prefix,Text]).


write_output(Text,Options):-
    member(write(F),Options),
    atomic(F), write_file(F,Text), !.

write_output(Text,Options):-
    member(write(),Options),
    write_file('output.tesdsl',Text), !.

write_output(Text,_):-
    write(Text).