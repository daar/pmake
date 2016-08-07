Quick start
----------

Here's a quick start on how to install *fmake* and create a simple working project, compilable and installable by *fmake*.

Install fmake
=============
There are two ways to get and install fmake on your machine. One way is to download and install the binary from GitHub (https://github.com/daar/fmake/releases). The release files are zip files and need to be copied to a location on your machine that is accessible from the command line. So either copy the folder contents to such a location or add the folder location which contains the binary and unit files to the PATH environment. 

.. NOTE::
  Make sure you copy the location of the ufmake folder relative to the *fmake* executable, so *fmake* can find them when compiling the build scripts.
  
.. NOTE::
  It's more convenient to use the install target of *fmake* to install the build tool to the correct location on your machine. See below for a detailed explanation on how to do it.
  

The other way to download the sources from https://github.com/daar/fmake/ via either GIT or SVN.

.. code:: bash

  svn co https://github.com/daar/fmake/
  
or

.. code:: bash

  git pull https://github.com/daar/fmake/
  
Then you can compile fmake by doing

.. code:: bash

  cd fmake.git
  cd trunk
  cd fmake
  fpc fmake
  
To install *fmake* you need to invoke *fmake* to create *make* and then do a *make install*;

.. code:: bash

  ./fmake
  sudo ./make install
  
This command will allow you to install *fmake* to the correct platform specific location. On \*nix platforms you will need to provide the administrator password to have rights to install the files, hence *sudo*.

Simple example
==============
Once *fmake* is installed you can start using it in your projects. 

The following example demonstrates some concept ideas of *fmake*. For this example you need two source files and one FMake.txt. The executable *hellodemo* is built by linking to the library hello_pkg that is built first.

The first, top-level directory contains the following FMake.txt file.

.. code:: pascal

  //root folder FMake.txt
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

  >./fmake
  >./make install
  
If all went well there will be two folders created in the root folder ./bin/x86_64-win64 and ./units/x86_64-win64 (in case you are working on a 64bit windows machine, using the 64bit FPC compiler). In the bin folder you will find an executable called hellodemo.exe.