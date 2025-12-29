:- module(path, [
    read_project_path/0,
    write_project_path/1,
    change_folder/1
]).

:- use_module(tools).


read_project_path:-
    absolute_file_name(user_app_config('project_path.tesdsl'),File),
    read_file(File,S_path), atom_string(Path,S_path), check_directory(Path),!,
    set_prolog_flag(project_path,Path), load_folder.

read_project_path:-
    ask_input("Enter PATH:",Path), write_project_path(Path),!.

read_project_path:-
    working_directory(CWD, CWD), 
    writeln("\nRedirecting to a default path..."),
    writeln("Use ?- write_project_path(+Path_to_Directory). to change the path.\n"),
    write_project_path(CWD).


write_project_path(Path):-
    absolute_file_name(user_app_config('project_path.tesdsl'),File),
    check_directory(Path), set_prolog_flag(project_path,Path),
    write_file(File,Path), load_folder.


project_help("For dev-built-in help, use ?- dev_help.").

load_folder:-
    current_prolog_flag(project_path, Path),
    project_help(Help),
    working_directory(_,Path),
    format("PATH: ~w~n~w~n~n",[Path,Help]).

change_folder(Path):-
    check_directory(Path),
    set_prolog_flag(project_path,Path),
    load_folder.