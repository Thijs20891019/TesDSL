:- module(scan, [
    folder_contents/3
]).


folder_contents(Display_list,Files_list,Validate):-
    working_directory(CWD,CWD),
    folder_contents(CWD,Display_list,Files_list,CWD-Validate).

folder_contents(Dir,Display_list,Files_list,Validate):-
    directory_files(Dir,Dir_content),
    folder_contents(Dir,Dir_content,Display_list,Files_list,Validate).

folder_contents(_,[],[],[],_):-!.

folder_contents(Dir,[Folder|Dir_content],[[Folder,[L|List]]|Display_list],Files_list,Validate):-
    absolute_file_name(Folder,New_dir,[file_type(directory),relative_to(Dir),file_errors(fail)]),
    Folder \= '.', Folder \= '..', % If necessary, block certain folders here that can be ignored.
    folder_contents(New_dir,[L|List],Content,Validate), !,
    append(Content,More_files,Files_list),
    folder_contents(Dir,Dir_content,Display_list,More_files,Validate).

folder_contents(Dir,[File|Dir_content],[File|Display_list],[File_path|Files_list],CWD-Validate):-
    absolute_file_name(File,File_path,[extensions([pl,pro,prolog,plt,ecl]),relative_to(Dir),access(exist),file_errors(fail)]),
    directory_file_path(CWD,CWD_file_path,File_path), 
    validation_check(Validate,CWD_file_path), !,
    folder_contents(Dir,Dir_content,Display_list,Files_list,CWD-Validate).

folder_contents(Dir,[_|Dir_content],Display_list,Files_list,Validate):-
    folder_contents(Dir,Dir_content,Display_list,Files_list,Validate).


validation_check(+>(_,U),F):-
    validation_check(U,F).
validation_check(+>(U,_),F):-
    validation_check(U,F).

validation_check(U-C,F):-!,
    validation_check(-C,F),
    validation_check(U,F).

validation_check(U+C,F):-!,
    validation_check(C,F),
    validation_check(U,F).

validation_check(_/C,F):-
    validation_check(C,F).
validation_check(C/_,F):-
    validation_check(C,F).

validation_check(-C,F):-!,
    \+validation_check(C,F).

validation_check(+C,F):-!,
    validation_check(C,F).

validation_check(C,F):-
    atomic(C), atom_length(C,N),
    sub_atom(F,_,N,_,C),!.