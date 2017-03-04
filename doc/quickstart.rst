Quick start
-----------

Here's a quick start on how to install PMake and create a simple working project, compilable and installable by PMake.

Install PMake
=============
There are two ways to get and install PMake on your machine. One way is to download and install the binary from GitHub (https://github.com/daar/pmake/releases). The release files are zip files and need to be copied to a location on your machine that is accessible from the command line. So either copy the folder contents to such a location or add the folder location which contains the binary and unit files to the PATH environment.

.. NOTE::
  Make sure you copy the location of the upmake folder relative to the PMake executable, so PMake can find them when compiling the build scripts.

.. NOTE::
  It's more convenient to use the install target of PMake to install the build tool to the correct location on your machine. See below for a detailed explanation on how to do it.


The other way to download the sources from https://github.com/daar/pmake/ via either GIT or SVN.

.. code:: bash

  svn co https://github.com/daar/pmake.git

or

.. code:: bash

  git clone https://github.com/daar/pmake

Then you can compile PMake by doing

.. code:: bash

  cd pmake
  cd pmake
  fpc pmake

To install PMake you need to invoke PMake to create *make* and then do a *make install*;

.. code:: bash

  ./pmake
  sudo ./make install

This command will allow you to install PMake to the correct platform specific location. On \*nix platforms you will need to provide the administrator password to have rights to install the files, hence *sudo*.

Simple example
==============
Once PMake is installed you can start using it in your projects.

The following example demonstrates some concept ideas of PMake. For this example you need two source files and one PMake.txt. The executable *hellodemo* is built by linking to the library hello_pkg that is built first.

The first, top-level directory contains the following PMake.txt file.

.. code:: pascal

  //root folder PMake.txt
  project('HELLO');

  add_library('hello_pkg', ['uhello.pas'])
  add_executable('hellodemo_pkg', 'hellodemo$(EXE)', 'demo.pp', ['hello_pkg']);

The program source file in the root folder is:

.. code:: pascal

  program hello;

  uses
    uhello;

  begin
    WriteHello;
  end.

The unit source file is shown below:

.. code:: pascal

  unit uhello;

  interface

  procedure WriteHello;

  implementation

  procedure WriteHello;
  begin
    writeln('Hello World!');
  end;

  end.

Now the project can be compiled by using the following commands;

.. code:: bash

  >./pmake
  >./make install

If all went well there will be two folders created in the root folder ./bin/x86_64-win64 and ./units/x86_64-win64 (in case you are working on a 64bit windows machine, using the 64bit FPC compiler). In the bin folder you will find an executable called hellodemo.exe.
