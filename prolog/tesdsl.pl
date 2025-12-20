:- module(tesdsl, [
    write_project_path/1,
    read_project_path/0,
    change_folder/1,
    tree_display/1,
    tree_display/2,
    write_file/2,
    read_file/2,
    dev_help/0,
    runf/0,
    run/0,
    run/1,
    ls/0
]).

:- user:consult(tes/setup).

:- use_module(tes/display).
:- use_module(tes/tools).
:- use_module(tes/path).
:- use_module(tes/dsl).

:- read_project_path.