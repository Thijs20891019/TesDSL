:- module(dsl, [
    dev_help/0,
    runf/0,
    run/0,
    run/1,
    ls/0
]).

:- use_module(scan).
:- use_module(display).
:- use_module(tools).


ls:-
    folder_contents(U,_,''),
    tree_display(U).

dev_help:-
    pack_file_path(data,'dev_help.txt',File),
    read_file(File,Text), write(Text).


user:load(U):-
    folder_contents(_,Unloading,''),
    unload_files(Unloading),
    folder_contents(Display,Prolog_files,U),
    user:load_files(Prolog_files),
    tree_display(Display).

unload_files([]).
unload_files([File|Files]):-
    unload_file(File),!,
    unload_files(Files).
unload_files([_|Files]):-
    unload_files(Files).


input_data(Input,Title,Terms):-
    user:input(Input), Input=..List,
    complex_to_atom(List,Title),
    filter_terms(List,Terms).

output_data(Title,Title,[]):-!.
output_data([Title,Output],Title,[T|Terms]):-
    terms_to_atoms([T|Terms],Atoms),
    atomics_to_string(Atoms," ",Output).

output(Output):-
    input_data(Input,Title,Terms),
    user:Input,
    output_data(Output,Title,Terms).

all_goals(Title):-
    input_data(_,Title,_).


run(Options):-
    make, findall(Title,all_goals(Title),Goals),
    findall(Output,output(Output),Sols),
    display_output(Goals,Sols,Options).

runf:- run([show_true(true),show_false(true),write()]).
run:- run([]).