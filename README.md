# fmake
a build tool for (free) pascal

Fmake is a minimalistic build tool for (free) pascal. It mimics to some extent the commands from cmake yet only targets pascal compilers. The inner working of fmake are very simple as the compiler does all the hard work. The developer can define with simple commands which targets are present in the project and what the dependencies between these targets are. A target can either be a library (no executable, only object files) or an exectable.

The task of fmake is to create the needed output folders and provide the correct search paths, based on the dependency list, to the compiler.

The build commands are defined in textfiles named FMake.txt, similar to CMakeLists.txt. By invoking the commandline application fmake, all the FMake.txt files are searched recursively starting from the folder in which fmake is invoked. Fmake will merge all FMake.txt files into a single source file and compile it into a "make" executable. No validation of the FMake.txt files is done (yet).

After make is compiled, one can invoke make and all build definitions are processed, creating a structure that can be used to determine the order in which libraries and executables are compiled and linked. First all targets that have no dependencies are processed. Once done the dependency is marked as satisfied and then the next target is searchd for that has no unsatisfied dependencies.

Simple but effective! Please comment, use and/or supply patches if you like.

Darius Blaszyk, April 30th, 2016


## Build commands
Currently the following build functions are implemented. Here's a short description;

    procedure project(Name: string);
This procedure will assign a name to the project. Without a name fmake will not work, however not much is done with this sofar.

    procedure add_library(Name: string; files: array of const);
This procedure will define a pascal library. In fact it is nothing more than a collection of source files that are compiled and can be a dependency for another library or executable.

    procedure add_executable(Name: string; files: array of const);
This procedure will define an executable. The name of the compiled executable is given first. What follows is a list of units that belong to this executable. However it is also possible to only specify the "program" source file and define the remainder of the source files as a library.
    
    procedure target_link_libraries(Name: string; files: array of const);
This procedure defines how dependencies are related to each other. One can interlink libraries and executables any way one chooses. The first parameter is the name of the library or executable that the dependencies belong to. The second parameter is a list of dependencies.


##Using fmake
Once the FMake.txt files are present in your source tree, then using fmake is very simple. You should compile the fmake generator only once and put it somewhere accessible from the environment. Then it's only a matter of invoking fmake to create the make file itself.

    ./fmake 
    ./make

The make file is not yet capable of detecting changes in the FMake.txt files and regenerate itself. So please invoke fmake each time you have worked on the build scripts or consider using a shell script that does this for you.

##TODO
The idea is to add more functionality when needed. Especially execting external programs, generating files, validating FMake.txt files and adding a configure step in the fmake generator is on the todo list. Also autogenerating make is something that should be considered.

Please note that this is only a proof of concept for now. The fmake sources contain parts of code from a number of different projects, most prominently fpmkunit, that is part of the freepascal project. This means that fmake is far from efficient, however it get's the job done. A refactor and recode is needed to simplify fmake and make it more versatile and efficient. Any help is greatly appreciated!

A rather incomplete list, but with most important items mentioned:

 - [x] add getopts to fmake so one can assign commandline parameters for FPC location, get/display help message and control verbosity of fmake
 - [x] keep track of FMake.txt locations and display a proper message when failing to compile via fmake
 - [ ] add clean target to make
 - [ ] add install target to make
 - [ ] implement executing of external tools to make
 - [ ] implement autogenerating of make
 - [ ] refactor ufmake.pas. Drop dependency on fpmkunit??
