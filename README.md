# TesDSL library

This library provides predicates and operators that make Prolog a more enjoyable experience.

TesDSL introduces a small domain-specific language for selecting files and declaring executable goals. 

It is intended for users who want to quickly load, run, and test Prolog code across project directories.


### Installation

```
?- pack_install(tesdsl,[url('https://github.com/Thijs20891019/TesDSL.git')]).
?- use_module(library(tesdsl)).
```


### Predicates

```
ls/0                    --> Shows all Prolog files in the working directory.
load/1                  --> Loads Prolog files that validate the condition set.
input/1                 --> Declares a goal within a file.
run/1                   --> Runs all goals declared by input/1.
run/0                   --> Uses run/1 with the default options.
runf/0                  --> Uses run/1 and writes to a file.

read_project_path/0     --> Reads the project path, changes working directory.
write_project_path/1    --> Changes the project path, changes working directory.
change_folder/1         --> Changes working directory for the remainder of the session.

tree_display/1          --> Displays a list in a tree structure.
tree_display/2          --> Outputs a string of a list in a tree structure.
read_file/2             --> Outputs the contents of the file.
write_file/2            --> Overwrites the contents of a file.
```



## TesDSL commands

### Workflow

To see all the Prolog files in the project folder, use `?- ls.`.
```
?- ls.
├── A1
│   └── n3.pl
├── A2
│   ├── n1.pl
│   ├── n2.pl
│   ├── n3.pl
│   └── n5.pl
├── A3
│   ├── n1.pl
│   ├── n2.pl
│   ├── n3.pl
│   └── n5.pl
└── test.pl
```

Load files that meet the condition with `?- load Condition.`.
```
?- load a1+x +> a2+n1/n2 +> a3-n1/n2 +> e.
├── A2
│   ├── n1.pl
│   └── n2.pl
├── A3
│   ├── n3.pl
│   └── n5.pl
└── test.pl
```

Make sure to include `input Goal.` in the file for the goals.
> **File:** ***test.pl***
> ```
> testing_n(1,'2').
> testing_n("3",[4]).
> testing_a(a,'b').
> testing_a("c",[d]).
> 
> input testing_n(_,_).
> input testing_n(1,2).
> input testing_a(_,_).
> input testing_a(a,b).
> ```

Then use `?- run(Options).`, `?- run.` to display the output or `?- runf.` to output to a file.
```
?- run([show_false(true)]).
Succeeded:
├── testing_n(_,_)
│   ├── 1 '2'
│   └── "3" [4]
│
├── testing_a(_,_)
│   ├── a b
│   └── "c" [d]
│
└── testing_a(a,b)

Failed:
└── testing_n(1,2)
```


### Predicates

```
ls
    Shows all Prolog files (including those within folders) in the working directory.

load :Condition
    Loads Prolog files in the working directory that validate the condition set.
    Conditions can be added continuously:
        Atom        -> all file names that include Atom.
        +Atom       -> all file names that include Atom.
        -Atom       -> all file names that don't include Atom.
        A1 + A2     -> A1 and file names that include A2.
        A1 - A2     -> A1 and file names that don't include A2.
        A1 / A2     -> A1 or A2.
        C1 +> C2    -> all files from condition 1 and 2.

input :Goal
    Declares a goal within a file to be executed later by run/1, it isn't executed at load time.

run(+Options)
    Shows the outcomes from the goals within input/1. The options:
        show_true(Bool)     -> show succeeded input(s), default true.
        show_false(Bool)    -> show failed input(s), default false.
        write(+File)        -> output to file, default to terminal.
        write()             -> output to output.tesdsl, default to terminal.

runf
    Uses run/1 with the options: show_true(true), show_false(true), write().

run
    Uses run/1 with the default options.
```



## Project configuration

### Workflow

When using the library for the first time, it will ask for the path to the project folder with `Enter PATH: `.
```
Warning: file does not exist: c:/users/~/appdata/roaming/swi-prolog/project_path.tesdsl
Enter PATH: ~/project_folder/
```
If the folder does not exist, it will redirect to the default Prolog folder.

To change the path to a project folder, use:
```
?- write_project_path(Path_to_folder).
```
This will change the contents in the file: `project_path.tesdsl`.

To change to a different folder that will revert to the project folder once the session ends, use:
```
?- change_folder(Path_to_folder).
```
> [!CAUTION]
> The predicates `write_project_path/1` and `change_folder/1` change the loaded folder immediately.
> Files that are currently loaded stay loaded and won't unload when using `load/1` in a folder not containing these files.


### Predicates

```
read_project_path
    From the file ~/swi-prolog/project_path.tesdsl, it changes the working directory to the path it read.

write_project_path(+Dir)
    Changes the path written in the file ~/swi-prolog/project_path.tesdsl and of the working directory.

change_folder(+Dir)
    Changes the path of the working directory.
```



## Utility predicates

### Workflow

Here is an example of `tree_display/2`, the predicate does the same as `tree_display/1`, but `tree_display/1` outputs immediately.
```
?- tree_display([ [ a , [ 1 , 2 ] ] , [ b , [ [ 3 , [ ? ] ] ] ] , c ], Output), format(Output).
├── a
│   ├── 1
│   └── 2
├── b
│   └── 3
│       └── ?
└── c
Output = "├── a\n│   ├── 1\n│   └── 2\n├── b\n│   └── 3\n│       └── ?\n└── c\n".
```


### Predicates

```
tree_display(+List)
    Displays a list in a tree structure.

tree_display(+List, -Text)
    Outputs a string of a list in a tree structure.

read_file(+File, -Text)
    Outputs the contents of the file.

write_file(+File, +Text)
    Overwrites the contents of a file.
```