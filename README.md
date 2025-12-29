# TesDSL library

This library provides predicates and operators that make Prolog a more enjoyable experience.

TesDSL introduces a small domain-specific language for selecting files and declaring executable goals. 

It is intended for users who want to quickly load, run, and test Prolog code across project directories.


### Installation

```
?- pack_install(tesdsl).
?- use_module(library(tesdsl)).
```


### Predicates

```
tes/1                   --> An operator that executes DSL code.

ls/0                    --> Shows all Prolog files in the working directory.
load/1                  --> Loads Prolog files that validate the condition set.
input/1                 --> Declares a goal within a file.
run/0                   --> Runs all goals declared by input/1.

read_project_path/0     --> Reads the project path, changes working directory.
write_project_path/1    --> Changes the project path, changes working directory.
change_folder/1         --> Changes working directory for the remainder of the session.

tree_display/1          --> Displays a list in a tree structure.
tree_display/2          --> Outputs a string of a list in a tree structure.
read_file/2             --> Outputs the contents of the file.
write_file/2            --> Overwrites the contents of a file.
```



## The domain-specific language

### Operator

```
tes :Activate
    This is an operator that executes DSL code. Activate is defined by:
    <*.tes-filename>(<listname>)
```

An example would be:
```
?- tes file(name).
```


### DSL documentation

The `tes` operator looks for a file with the extension `.tes`.

In the DSL, each named block inside a `.tes` file is called an _actionlist_:
```
listname {
    ...
}
```

Within _actionlists_ are _actions_, an _action_ is always ended with `;`.

This is a list of different _actions_ with their explanation:
```
> :Listname;            --> Includes all actions from the specified actionlist at this point.

load;                   --> Reloads the loaded files from load/1.
load :Condition;        --> Behaves the same as load/1.
load {...};             --> Loads specific Prolog files in the list.

input;                  --> Finds all goals declared by input/1.
input {...};            --> Finds the goals declared inside the list.

inspect;                --> Gathers the succeeded and failed goals from input.
inspect all;            --> Gathers all the goals outcomes and their statistics.
inspect only;           --> Gathers only the statistics from the goals.
inspect loss;           --> Gathers the succeeded goals statistics that backtrack without solution.
inspect {...};          --> Gathers specific statistics from the goals.

display;                --> Shows the results in the terminal.

write +Filename;        --> Writes the results to a file.
```

Certain _actions_ can contain _actionitems_, an _actionitem_ is always ended with `;`.

The _action_ `load {...};`, can contain relative or absolute paths to files.
```
load {
    folder/file.pl;
    ~/file.pl;
};
```

The _action_ `input {...};`, can contain Prolog goals.
```
input {
    member(X,[a,b,c]);
};
```

The _action_ `inspect {...};`, can contain items that limit the range that is inspected.
```
inspect {
    true;               --> Searches for predicates that succeed.
    false;              --> Searches for predicates that fail.
    inferences; infs;   --> Shows the number of inferences.
    cpu; CPU; time;     --> Shows the amount of time the CPU takes.
    wall; clock; time;  --> Shows the amount of run time it takes.
};
```

A default `.tes` file is provided within the pack, this file is used by run/0.
> **File:** ***default.tes***
> ```
> default {
>     load;
>     input;
>     inspect;
>     display;
> }
> ```

The predicate run/0 internally calls:
```
?- tes default(default).
```

Note that multiple _actionlists_ can be specified when invoking the DSL:
```
?- tes default(default,listname,otherlist,etc).
```
Each _actionlist_ is expanded and executed in the order provided.
Internally, the arguments are treated as a sequence of _actionlist_ names.



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

Then use `?- run.` to display the output.
```
?- run.
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
    Declares a goal within a file to be executed later by the dsl, it isn't executed at load time.

run
    Shows the outcomes from the goals within input/1.
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

Folders or files that should be ignored by `ls/0` and `load/1` can be listed in a file named `.tesignore` located in the project root.
> **File:** ***.tesignore***
> ```
> a2
> A3
> ```
As a test, here is the outcome from `?- ls.`.
```
?- ls.
├── A1
│   └── n3.pl
└── test.pl
```

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