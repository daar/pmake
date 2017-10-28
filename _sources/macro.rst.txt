Macro's
-------

PMake allows for custom defined variables and options that can be set and read at runtime from the PMake.tx file. The following functions are defined for this purpose;

.. code:: pascal

  procedure set_(name: string; value: boolean);
  procedure set_(name: string; value: integer);
  procedure set_(name: string; value: string);
  procedure set_(name: string; value: double);

  function val_(name: string): string;
  function val_(name: string): boolean;
  function val_(name: string): integer;
  function val_(name: string): double;

You can use `set_` to create a custom variable and use `val_` in any PMake.txt file.

All API functions will do a macro expand internally. The following ;

$(PMAKE_HOST_SYSTEM_NAME)
  Expands to the target OS that PMake is invoked on. Example is win64 on a 64bit windows machine.

$(PMAKE_HOST_SYSTEM_PROCESSOR)
  Expands to the target CPU that PMake is invoked on. Example is x86_64 on a 64bit machine.

$(PMAKE_PAS_COMPILER)
  The full path to the pascal compiler

$(PMAKE_SOURCE_DIR)
  The path to the top level of the source tree.

$(PMAKE_CURRENT_SOURCE_DIR)
  The path to the source directory currently being processed.

$(PMAKE_BINARY_DIR)
  The path to the top level of the build tree.

$(PMAKE_CURRENT_BINARY_DIR)
  The path to the binary directory currently being processed.

$(PMAKE_PAS_COMPILER_VERSION)
  Compiler version string.

$(UNITSOUTPUTDIR)
  Expands to the unit output directory that is defined for the specific package. The value is equivalent to:

  .. code:: bash

    .\units\$(PMAKE_HOST_SYSTEM_PROCESSOR)-$(PMAKE_HOST_SYSTEM_NAME)

$(BINOUTPUTDIR)
  Expands to the binary output directory that is defined for the specific package. The value is equivalent to:


  .. code:: bash

    .\bin\$(PMAKE_HOST_SYSTEM_PROCESSOR)-$(PMAKE_HOST_SYSTEM_NAME)

$(EXE)
  Expands to the executable file extension defined for the platform PMake is invoked on. On UNIX systems executables have no extension by default, on the other platforms this is .exe.

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
