:- module(tes_tools, [
    parse_tes_file/2,
    cycle_listnames/2,
    tesdsl_compiler/2,
    tesdsl_executor/1,
    set_files/1,
    get_files/1
]).

:- use_module(exe_tools).
:- use_module(display).
:- use_module(parser).
:- use_module(tools).


/* parse *.tes file */
parse_tes_file(File,[ActionLists,_,_]):-
    absolute_file_name(File,Path,[extensions([tes])]),
    read_file(Path,Text),
    string_codes(Text,Codes),
    phrase(parse(ActionLists),Codes,_).

/* cycle listnames */
cycle_listnames([_,ListName,_],ListNames):-
    member(ListName,ListNames).    

/* tesdsl compiler */
tesdsl_compiler([Tes|Data],Actions):-
    findall(Data,member(Data,Tes),[Data]),
    findall(Action,get_action([Tes|Data],Action),Actions).

get_action([Tes,_,Actions],Action):-
    member(Acti0n,Actions),
    get_action(Tes,Acti0n,Action).

get_action(Tes,[>,Listname],Action):- !,
    findall([Listname,A],member([Listname,A],Tes),[Data]),
    get_action([Tes|Data],Action).
get_action(_,Action,Action).

/* tesdsl executor */
tesdsl_executor(Actions):-
    action_load(Actions,Files),
    findall(Output, (
        action_input(Actions,Data),
        action_inspect(Actions,Data),
        action_output(Output,Data)
    ), OutputData),
    action_compile(OutputData,Text),
    action_write(Actions,Text),
    action_display(Actions,Text),
    action_unload(Actions,Files).

/* actions */
action_load(Actions,Files):-
    findall(F_list,(
        member([load|U],Actions),
        files_to_load(U,F_list)
    ),[L|List]) ->
    append([L|List],File_list),
    list_to_set(File_list,Files),
    get_files(Unload), unload_files(Unload),
    user:load_files(Files); true.

action_unload(Actions,Files):-
    member([load,_],Actions) ->
    get_files(Loaded),
    user:load_files(Loaded),
    subtract(Files,Loaded,Unload),
    unload_files(Unload); true.

action_input(Actions,[_,Term|TermData]):-
    member([input|U],Actions),
    input_term(U,Term,TermData).

action_inspect(Actions,[U|Data]):-
    member([inspect|U],Actions),
    inspect_data(U,Data).

action_output([Title,[[R,Statis]],[Title,R|Mode]],[Mode,_,Title,[],[R|Stats]]):-!,
    atomics_to_string(Stats,", ",Statis).
action_output([Title,[[R,Statis]],[Title,R|Mode]],[Mode,_,Title,_,[R|Stats]]):-
    atomics_to_string(Stats,", ",Statis), member(Mode,[[only],[loss]]), !.
action_output([Title,[[R,Statis],Output],[Title,R|Mode]],[Mode,_,Title,[V|Vars],[R|Stats]]):-
    atomics_to_string(Stats,", ",Statis),
    terms_to_atoms([V|Vars],Atoms),
    atomics_to_string(Atoms," ",Output).

action_compile(OutputData,Text):-
    sort_outcome(OutputData,TrueList,FalseList),
    findall(Out,(
        create_display(TrueList,"Succeeded",Out);
        create_display(FalseList,"Failed",Out)
    ),[L|List]),
    atomics_to_string([L|List],"\n",Text).

action_write(Actions,Text):-
    findall(U,(
        member([write,U],Actions),
        write_file(U,Text)
    ),[F|Files]) -> 
    ansi_format([fg(green)],"% output to: ~w~n",[[F|Files]]);
    true.

action_display(Actions,Text):-
    member([display],Actions) ->
    write(Text); true.


set_files(Files):-
    recorded(loaded_files,_,U), erase(U), fail;
    recorda(loaded_files,Files).

get_files(Files):-
    recorded(loaded_files,Files).