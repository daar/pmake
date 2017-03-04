Introduction
------------

PMake is a minimalistic yet powerful build tool for (free) pascal. It mimics to some extent the commands from CMake but targets pascal compilers specifically. The inner working of PMake are very simple as the compiler does all the hard work. The developer can define with simple commands which targets are present in the project and what the dependencies between these targets exist. A target can either be a library (no executable, only object files) or an executable.

The task of PMake is to create the needed output folders and provide the correct search paths, based on the dependency list, to the compiler.

The build commands are defined in text files named PMake.txt, similar to CMakeLists.txt, not to confuse them with the source files of the project. Also the functions naming is kept similar to have a shallow learning curve. By invoking the command line application PMake, all the PMake.txt files are searched recursively starting from the folder in which PMake is invoked. PMake will merge all PMake.txt files into a single source file and compile it into a "make" executable. No validation of the PMake.txt files is done (yet).

After a makefile is created in the root folder, one can invoke *make* and all build definitions are processed, creating a structure that can be used to determine the order in which libraries and executables are compiled and linked. First all targets that have no dependencies are processed. Once done the dependency is marked as satisfied and then the next target is searched for that has no unsatisfied dependencies.

Simple but effective! Please comment, use and/or supply patches if you like.

Darius Blaszyk, April 30th, 2016
