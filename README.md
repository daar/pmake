[![license](https://img.shields.io/badge/license-%20GPL--2-blue.svg)](../master/LICENSE)

<img src="https://github.com/daar/pmake/blob/master/logo/pmake_logo.png" alt="PMake" width="50%" height="50%"/>

a build tool for (free) pascal

PMake is a minimalistic build tool for (free) pascal. It mimics to some extent the commands from CMake yet only targets pascal compilers. The inner working of fmake are relatively simple as the compiler does all the hard work. The developer can define with simple commands which targets are present in the project and what the dependencies between these targets are. A target can either be a library (no executable, only object files) or an exectable.

> Please be aware that for the time being PMake is under development and that some features might break. However due to the simplicity and the forgivingness of the PMake script it should be easy to adjust.
