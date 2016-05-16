FMake reference
------------------

.. code:: pascal

  add_executable(name, executable, srcfile: string; depends: array of const)
  
This procedure will add a package called **name** to the build system and link to it a source file via the **srcfile** variable. The **depends** variable, if needed, can contain one or more dependencies to other packages. An example of how to use the executable command is shown below;

.. code:: pascal

  add_executable('pkg_helloworld', 'helloworld$(EXE)', 'hello.pp', ['lib_hello']);

This command indicates that the executabale *helloworld$(EXE)* that belongs to the package *pkg_helloworld* is compiled from the source file hello.pp. In turn this source file depends on the package *lib_hello*. Please note that the helloworld executable makes use of the macro $(EXE) that resolves to .exe for platforms other than unix platforms. Please go to the macro page to get an overview of the supported macro's.

With this command it is also possible to compile shared libraries. Instead of the $(EXE) macro then please use the $(DLL) macro.

.. code:: pascal

  add_library(name: string; srcfiles, depends: array of const)
  
This procedure will add a package called **name** to the build system and link to it one or more source files via the **srcfiles** variable. The **depends** variable, if needed, can contain one or more dependencies to other packages. An example of how to use the executable command is shown below;

.. code:: pascal

  add_executable('lib_hello', 'uhello.pas', ['']);

This command indicates that the package *lib_hello* contains one source file uhello.pas. This package does not have a dependency to any other package.

.. code:: pascal

  install(directory, destination, pattern: string)

An example of how to use the executable command is shown below;

.. code:: pascal

  install('$(BINOUTPUTDIR)', '.\\release\\$(TargetCPU)-$(TargetOS)', 'helloworld$(EXE)');

This command will install the helloworld executable (helloworld.exe on windows) from the binary output folder of the package to the .\\release\\$(TargetCPU)-$(TargetOS) folder, relative to the folder from which fmake was invoked from and where make.exe is generated. For the file pattern it is also possible to use wildcard characters such as;

\* 
  match zero or more characters
?
  match one character

So using \*.exe as search pattern would copy all .exe files from the binary output folder to the release folder.

.. code:: pascal

  add_custom_command(name, executable, parameters: string; depends: array of const)
  
An example of how to use the install command is shown below;

.. code:: pascal

  add_custom_command('exec_helloworld', '.\\bin\\$(TargetCPU)-$(TargetOS)\\helloworld$(EXE)', '', ['pkg_helloworld']);
  
This command will execute the hello world executable located in the bin folder with no additional parameters. This command is part of the exec_helloworld package and has a dependency on *pkg_helloworld*. This means that this package needs to be built and linked first before the custom command is executed.