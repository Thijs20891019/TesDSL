:- module(tools, [
    check_directory/1,
    check_file/1,
    check_file_dir/1,
    read_file/2,
    write_file/2,
    ask_input/2,
    terms_to_atoms/2,
    complex_to_atom/2,
    filter_terms/2,
    convert_list/3,
    pack_file_path/3
]).


check_directory(Path):- exists_directory(Path), !.
check_directory(Path):-
    ansi_format([fg(red)],"Warning: directory does not exist: ~w~n",[Path]), false.

check_file(Path):- exists_file(Path), !.
check_file(Path):-
    ansi_format([fg(red)],"Warning: file does not exist: ~w~n",[Path]), false.

check_file_dir(Path):-
    absolute_file_name(Path, C_Path), file_directory_name(C_Path,Dir_Path),
    check_directory(Dir_Path).


read_file(File,Text):-
    check_file(File),
    open(File,read,Stream),
    read_string(Stream,_,Text),
    close(Stream).

write_file(File,Text):-
    check_file_dir(File),
    open(File,write,Stream),
    write(Stream,Text),
    close(Stream).


ask_input(Ask, Input):-
    format("~w ",[Ask]), flush_output,
    read_line_to_string(user_input, String),
    normalize_space(string(Input), String).


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


convert_list([],[],[]).
convert_list([[Title,Solution]|Rest],[[Title,[Solution|Next_S]]|Next_T],[Title|Next]):-
    convert_list(Rest,Title,Next_S,New_Rest),
    convert_list(New_Rest,Next_T,Next), !.
convert_list([Title|Rest],[Title|Next_T],[Title|Next]):-
    convert_list(Rest,Next_T,Next).

convert_list([[Title,Solution]|Rest],Title,[Solution|Next_S],Rest_N):-
    convert_list(Rest,Title,Next_S,Rest_N), !.
convert_list(Rest_N,_,[],Rest_N).


pack_file_path(Folder,File,Path):-
    pack_property(tesdsl,directory(Dir)),
    directory_file_path(Dir,Folder,Dir_f),
    directory_file_path(Dir_f,File,Path).