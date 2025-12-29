:- module(dsl, [
    dev_help/0,
    run/0,
    ls/0
]).

:- use_module(tes_tools).
:- use_module(display).
:- use_module(tools).
:- use_module(scan).


ls:-
    folder_contents(U,_,''),
    tree_display(U).

dev_help:-
    directory_file_path('data','dev_help.txt',DataFile),
    absolute_file_name(pack(tesdsl),Dir),
    directory_file_path(Dir,DataFile,File),
    read_file(File,Text), write(Text).


user:load(U):-
    folder_contents(_,Unloading,''),
    unload_files(Unloading),
    folder_contents(Display,Prolog_files,U),
    user:load_files(Prolog_files),
    set_files(Prolog_files),
    tree_display(Display).


user:tes(File_ListNames):-
    File_ListNames =.. [File|ListNames],
    parse_tes_file(File,TesData),
    cycle_listnames(TesData,ListNames),
    tesdsl_compiler(TesData,Actions),
    tesdsl_executor(Actions).


run:-
    directory_file_path('data','default.tes',DataFile),
    absolute_file_name(pack(tesdsl),Dir),
    directory_file_path(Dir,DataFile,File),
    File_Listnames =.. [File,default],
    tes(File_Listnames).


