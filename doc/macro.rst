Macro's
-------

When using PMake it is important to understand the architecture of the software in order to be able to create a proper build script. A few basic concepts of the PMake build tool are targets and packages. A package is a collection of one or more source files that belong to each other. In the PMake.txt build scripts one can define how these packages are linked to each other (dependencies) and of which commands they exist. There are a number of different commands like *executable*, *unit*, *install* and *custom*. A target is a specific overall command that is going to be executed. This target is specified form the command line by the developer. The predefined targets for PMake are;

$(TargetOS)
  Expands to the target OS that PMake is invoked on. Example is win64 on a 64bit windows machine.

$(TargetCPU)
  Expands to the target CPU that PMake is invoked on. Example is x86_64 on a 64bit machine.

$(UNITSOUTPUTDIR)
  Expands to the unit output directory that is defined for the specific package. The value is equivalent to .\\units\\$(TargetCPU)-$(TargetOS)

$(BINOUTPUTDIR)
  Expands to the binary output directory that is defined for the specific package. The value is equivalent to .\\bin\\$(TargetCPU)-$(TargetOS)

$(EXE)
  Expands to the executable file extension defined for the platform PMake is invoked on. On UNIX systems executbles have no extension by default, on the other platforms this is .exe.

$(DLL)
  Expands to the shared library file extension defined for the platform PMake is invoked on.

+-------------------+----------------+-------------------+
| Operating systems | Library prefix | Library extension |
+===================+================+===================+
| Apple             | lib            | .so               |
+-------------------+----------------+-------------------+
| BeOS              | lib            | .so               |
+-------------------+----------------+-------------------+
| FreeBSD           | lib            | .so               |
+-------------------+----------------+-------------------+
| Linux             | lib            | .so               |
+-------------------+----------------+-------------------+
| NetBSD            | lib            | .so               |
+-------------------+----------------+-------------------+
| Windows           | none           | .dll              |
+-------------------+----------------+-------------------+
