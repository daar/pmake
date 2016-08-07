[![license](https://img.shields.io/badge/license-%20GPL--2-blue.svg)](../master/LICENSE)

# fmake
a build tool for (free) pascal

Fmake is a minimalistic build tool for (free) pascal. It mimics to some extent the commands from cmake yet only targets pascal compilers. The inner working of fmake are relatively simple as the compiler does all the hard work. The developer can define with simple commands which targets are present in the project and what the dependencies between these targets are. A target can either be a library (no executable, only object files) or an exectable.
