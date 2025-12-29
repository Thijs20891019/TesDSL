:- module(exe_tools, [
    files_to_load/2,
    input_term/3,
    terms_to_atoms/2,
    inspect_data/2,
    sort_outcome/3
]).

:- use_module(tes_tools).
:- use_module(scan).


/* action load helper */
files_to_load([],Files):-
    get_files(Files).

files_to_load([U],Files):-
    atom(U), read_term_from_atom(U,Term,[]),
    folder_contents(_,Files,Term).

files_to_load([[L|List]],Files):-
    findall(File_path,(
        member([File],[L|List]),
        absolute_file_name(File,File_path,[extensions([pl,pro,prolog,plt,ecl]),access(exist),file_errors(fail)])
    ),Files).




/* action input helper */
input_term([],Term,TermData):-
    user:input(Term), 
    term_data(Term,TermData).

input_term([[L|List]],Term,TermData):-
    member([Atom],[L|List]),
    read_term_from_atom(Atom,Term,[]),
    term_data(Term,TermData).

term_data(Term,[Title,Vars,_]):-
    Term =.. List,
    complex_to_atom(List,Title),
    filter_terms(List,Vars).


terms_to_atoms([],[]).

terms_to_atoms([Item_in|List_in],[Item_out|List_out]):-
    var(Item_in), !, Item_out = '_',
    terms_to_atoms(List_in,List_out).

terms_to_atoms([Item_in|List_in],[Item_out|List_out]):-
    term_to_atom(Item_in, Item_out),
    terms_to_atoms(List_in,List_out).


complex_to_atom([Pred|List_in],Atom):-!,
    terms_to_atoms(List_in,List_out),
    atomic_list_concat(List_out,',',Mid),
    atomic_list_concat([Pred,'(',Mid,')'],'',Atom).


filter_terms([],[]).

filter_terms([Item_in|List_in],[Item_out|List_out]):-
    var(Item_in), !, Item_out = Item_in,
    filter_terms(List_in,List_out).

filter_terms([_|List_in],List_out):-!,
    filter_terms(List_in,List_out).



/* action inspect helper */
inspect_data([],[Term,_,_,[Result]]):-
    user:Term *-> Result=true; Result=false.

inspect_data([loss],[Term,_,_,[true|Stats]]):-
    findall([R,D],(
        user:call_time(Term,D_o,R),
        Infs is D_o.inferences + 1,
        D=D_o.put([inferences=Infs])
    ),[[true,_]|List]), 
    append(_,[[false,Dict]],List), !,
    inspect_stats([[inferences],[cpu],[wall]],Dict,Stats).

inspect_data([Check],[Term,_,_,[Result|Stats]]):-
    member(Check,[all,only]),
    user:call_time((Term *-> Result=true; Result=false),Dict),
    inspect_stats([[inferences],[cpu],[wall]],Dict,Stats).

inspect_data([[L|List]],[Term,_,_,[Result|Stats]]):-
    user:call_time((Term *-> Result=true; Result=false),Dict),
    member([Result],[L|List]),
    inspect_stats([L|List],Dict,Stats).


inspect_stats(List,Dict,Stats):-
    findall(Stat,find_stat(List,Dict,Stat),Stats).
find_stat(List,Dict,Statistic):-
    member([Item],List), stat_text(Item,Dict,Statistic).

stat_text(Stat,Dict,Statistic):-
    member(Stat,[infs,inferences]),
    get_dict(inferences,Dict,Statis),
    atom_concat(Statis,' inferences',Statistic).
stat_text(Stat,Dict,Statistic):-
    member(Stat,[cpu,'CPU',time]),
    get_dict(cpu,Dict,Statis),
    time_convert(Statis,Time),
    atom_concat(Time,' cpu-time',Statistic).
stat_text(Stat,Dict,Statistic):-
    member(Stat,[wall,clock,time]),
    get_dict(wall,Dict,Statis),
    time_convert(Statis,Time),
    atom_concat(Time,' clock-time',Statistic).

time_convert(0.0,'too small'):-!.
time_convert(Time,Text):-
    time_convert(Time,[' s',' ms',' μs',' ns'],Text).
time_convert(Time,[_|Rest],Text):-
    Time < 1, !, Zoom is Time*1000,
    time_convert(Zoom,Rest,Text).
time_convert(Time,[Unit|_],Text):-
    atom_concat(Time,Unit,Text).



/* action compile helper */
sort_outcome([],[],[]):-!.

sort_outcome([[T,[[true,""],S],Mode]|OutputData],[[T,[S|Next_S]]|Next_T],FalseList):-
    sort_outcome(OutputData,Mode,Next_S,Next_Data), !,
    sort_outcome(Next_Data,Next_T,FalseList).
sort_outcome([[T,[[true,""]],Mode]|OutputData],[[T,Next_S]|Next_T],FalseList):-
    sort_outcome(OutputData,Mode,Next_S,Next_Data), !,
    sort_outcome(Next_Data,Next_T,FalseList).
sort_outcome([[T,[[false,""]|_],Mode]|OutputData],TrueList,[[T,Next_S]|Next_T]):-
    sort_outcome(OutputData,Mode,Next_S,Next_Data), !,
    sort_outcome(Next_Data,TrueList,Next_T).

sort_outcome([[T,[[true,Statis],S],Mode]|OutputData],[[T,[[S,[Statis]]|Next_S]]|Next_T],FalseList):-
    sort_outcome(OutputData,Mode,Next_S,Next_Data), !,
    sort_outcome(Next_Data,Next_T,FalseList).
sort_outcome([[T,[[true,Statis]],Mode]|OutputData],[[T,[Statis|Next_S]]|Next_T],FalseList):-
    sort_outcome(OutputData,Mode,Next_S,Next_Data), !,
    sort_outcome(Next_Data,Next_T,FalseList).
sort_outcome([[T,[[false,Statis]|_],Mode]|OutputData],TrueList,[[T,[Statis|Next_S]]|Next_T]):-
    sort_outcome(OutputData,Mode,Next_S,Next_Data),
    sort_outcome(Next_Data,TrueList,Next_T).


sort_outcome([[_,[[true,""],S],Mode]|OutputData],Mode,[S|Next_S],Next_Data):-
    sort_outcome(OutputData,Mode,Next_S,Next_Data), !.
sort_outcome([[_,[[_,""]|_],Mode]|OutputData],Mode,Next_S,Next_Data):-
    sort_outcome(OutputData,Mode,Next_S,Next_Data), !.

sort_outcome([[_,[[true,Statis],S],Mode]|OutputData],Mode,[[S,[Statis]]|Next_S],Next_Data):-
    sort_outcome(OutputData,Mode,Next_S,Next_Data), !.
sort_outcome([[_,[[_,Statis]|_],Mode]|OutputData],Mode,[Statis|Next_S],Next_Data):-
    sort_outcome(OutputData,Mode,Next_S,Next_Data), !.

sort_outcome(Next_Data,_,[],Next_Data).