[![license](https://img.shields.io/badge/license-%20GPL--2-blue.svg)](../master/LICENSE)
[![Build Status](https://travis-ci.org/daar/pmake.svg?branch=v0.02)](https://travis-ci.org/daar/pmake)

<img src="https://github.com/daar/pmake/blob/master/logo/pmake_logo.png" alt="PMake" width="50%" height="50%"/>

*a cross-platform build tool for (free)pascal*

Introduction
============
PMake is a minimalistic build tool for (free) pascal. It mimics to some extent the commands from CMake yet only targets pascal compilers. The inner working of pmake are relatively simple as the compiler does all the hard work. Once invoked pmake will generate a build system for the project. The developer can define with simple commands which targets are present in the project and what the dependencies between these targets are. A target can either be a library (no executable, only object files) or an executable.

> Please be aware that for the time being PMake is under development and that some features might break. However due to the simplicity and the forgivingness of the PMake script it should be easy to adjust.

Building PMake
==============

Supported Platforms
-------------------

* Linux
* Macintosh
* Windows

Other operating systems will probably work too out of the box, if not it should not be a major problem to make PMake work on this platform.

Building PMake from Scratch
---------------------------
You need to have the latest stable freepascal compiler installed. PMake can bootstrap itself with the provided PMake.txt script. You will first need to build pmake and then install it. In short:

`$ fpc pmake`

`$ ./pmake`

`$ ./make install`


Reporting Bugs
==============
If you have found a bug:

1. If you have a patch, please make a pull request.

2. Otherwise, please create an issue on the GitHub page.

Contributing
============
You can for PMake and make contributions by making pull request.

