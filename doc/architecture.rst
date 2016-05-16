Architecture
------------

When using *fmake* it is important to understand the architecture of the software in order to be able to create a proper buildscript. The basic concepts of the *fmake* build tool are targets and packages. A package is a collection of commands that belong to each other. In the FMake.txt build scripts one can define how these packages are linked to each other (dependencies) and of which commands they exist. There are a number of different commands like *executable*, *unit*, *install* and *custom*. A target is a specific overall command that is going to be executed. This target is specified from the commandline by the developer. The predefined targets for *fmake* are;
  
Build
=====
The build target will compile and link all source code that is defined by the build scripts in the source tree. It will create if needed an output folder for the object (*.o) and compiled pascal unit files (*.ppu). The default output folder for compiled units is *.\units\$(TargetCPU)-$(TargetOS)*, while for binary executables the output folder is: *.\bin\$(TargetCPU)-$(TargetOS)*. These folders will be created in the locations where the FMake.txt files are placed. Build is also the default tagret. This means that it does not need to be supplied, but by default it will assume the build target is called for.

Clean
=====
The clean target will delete all the create output folders, including the contents. Please keep this in mind and do not copy any files in there yourself or they will get lost.

Install
=======
The install target will copy the designated files into a directory. One can use this target to install the compiled files to any folder, either relative to the source folder (for instance to create a release folder) or to an absolute folder to install the program, including any additional files on the desired location according to the platform specifications. Please note that install will not automatically call for build. This means that build needs to be invoked separately.

The packages are defined in the FMake.txt files which are located in the source tree. In each FMake.txt the commands for each package are defined. A package can have a combination of the commands *executable*, *unit*, *install* and *custom*. The dependency resolving algorithm will try to interpret the package commands and try to determine the order in which the packages are processed. Please note that withing a package, thus the package command order is the responsibility of the developer. In case a command has a dependency with another command within the same package and this needs to be resolved automatically, it is advised to create a new package within the same FMake.txt. The dependencies for units within a package are handled by the compiler. So it is perfectly possible to add unit A before unit B, while unit B depends on unit A. In that case unit A will be compiled two times as the first time unit A will be compiled because the compiler requires it for unit B and the second time unit A will be compiled because it was added via FMake.txt. File references within FMake.txt files can be references relatively or absolute, although the latter is not recommended.

Please go to the reference page for a complete description of the functions.

executable command
==================
The executable command will define an executbale to be added to a package. It is possible to add one or more executables to a single package. In case there are dependencies between packages the user will need to specify them.


unit command
============
The unit command is used to add units to a package. Multiple units can be added to a single package. For now *fmake* will not work out the unit interdependencies. This is left over to the compiler. But please be aware that the compiler will not be able to find interdependencies between packages. This is work for the developer instead.


install command 
===============
The install command is a special command as it will copy files from one location to another location. This command can be used to install the files to a location relative to the source folder (for instance to create a release folder) or to an absolute folder to install the program, including any additional files on the desired location according to the platform specifications. The install command is initiated by using the following procedure prototype in FMake.txt


custom command
==============
The custom command is a special command that invokes an executable with parameters and adds a dependency to this invokation. This command is used for instance to generate source files (for example lex and yacc) or other files needed for the release of the project. A custom command is always located in a separate package so it can be a dependency for other packages. The custom command is initiated by using the following procedure prototype in FMake.txt


Inner workings
==============
When running *fmake* the whole source folder is recursively searched for FMake.txt files. In principle FMake.txt files are executing parts of the *make* program. Upon collection, *fmake* will merge all FMake.txt files into a single program file which is then compiled into the *make* executable.

Each FMake.txt file defines one or more packages
