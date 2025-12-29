:- module(tesdsl, [
    write_project_path/1,
    read_project_path/0,
    change_folder/1,
    tree_display/1,
    tree_display/2,
    write_file/2,
    read_file/2,
    dev_help/0,
    run/0,
    ls/0
]).

:- set_prolog_flag(encoding,utf8).
:- multifile user:input/1.
:- op(950, fx, user:input).
:- op(950, fx, user:load).
:- op(950, fx, user:tes).
:- op(800, xfy, user:(+>)).

:- use_module(tesdsl/display).
:- use_module(tesdsl/tools).
:- use_module(tesdsl/path).
:- use_module(tesdsl/dsl).

:- read_project_path.