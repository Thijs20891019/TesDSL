:- module(tools, [
    check_directory/1,
    check_file/1,
    check_file_dir/1,
    read_file/2,
    write_file/2,
    ask_input/2,
    unload_files/1
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


unload_files([]).
unload_files([File|Files]):-
    unload_file(File),!,
    unload_files(Files).
unload_files([_|Files]):-
    unload_files(Files).