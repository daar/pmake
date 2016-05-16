Introduction
------------

Fmake is a minimalistic yet powerfull build tool for (free) pascal. It mimics to some extent the commands from cmake yet only targets pascal compilers. The inner working of fmake are very simple as the compiler does all the hard work. The developer can define with simple commands which targets are present in the project and what the dependencies between these targets are. A target can either be a library (no executable, only object files) or an exectable.

The task of fmake is to create the needed output folders and provide the correct search paths, based on the dependency list, to the compiler.

The build commands are defined in textfiles named FMake.txt, similar to CMakeLists.txt. By invoking the commandline application fmake, all the FMake.txt files are searched recursively starting from the folder in which fmake is invoked. Fmake will merge all FMake.txt files into a single source file and compile it into a "make" executable. No validation of the FMake.txt files is done (yet).

After make is compiled, one can invoke make and all build definitions are processed, creating a structure that can be used to determine the order in which libraries and executables are compiled and linked. First all targets that have no dependencies are processed. Once done the dependency is marked as satisfied and then the next target is searchd for that has no unsatisfied dependencies.

Simple but effective! Please comment, use and/or supply patches if you like.

Darius Blaszyk, April 30th, 2016