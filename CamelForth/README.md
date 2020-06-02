### VecForth, CamelForth/6809 and Multicomp6809

**VecForth** is based on CamelForth/6809 by Brad Rodriguez, which is an ANSI compliant Forth system for the Motorola 6809 microprocessor. It includes the Forth kernel, interpreter, and cross compiler. It was released in 1995. The CamelForth compiler was written to run on an early PC DOS Forth called F83 by Laxen & Perry, which produces a binary firmware image for burning to an EPROM.
The **F83 cam09-10.zip** folder contains a copy of the original source code from camelforth.com, see [http://www.camelforth.com/](http://www.camelforth.com/page.php?6) for more information.

**CamelForth/6809** was ported to the Grant Searle's FPGA-based multicomp 6809 computer by  Neal Crook. Neal added several extensions to CamelForth to take advantage of the FPGA computer features e.g.  SD Card storage. Additionally, he then modified the original cross compiler source code to to be ANS Forth-compliant, so it could run using Gforth instead of F83, allowing the compilation to run on a modern Windows or Linux OS instead of requiring a DOSBox setup.
The **Gforth multicomp6809** folder contains a copy of the original code from Neal's GitHub repository, see https://github.com/nealcrook/multicomp6809 for more information.

**VecForth** was cloned from the multicomp6809 Forth source code and modified to use plain text file source code instead of traditional 1kb source code blocks (.scr files). Most of the extensions were stripped out (sorry Neal) to aid understanding of the underlying Forth, maybe they'll go back in one day. The F83, Multicomp+Gforth and initial VecForth+Gforth compiled target images were binary compared and found to be identical, proving the conversion from the origianal source code, to Multicomp to VecForth was clean and nothing was broken in the process. (Note, Multicomp was compiled with the 'vanilla' switch i.e. no extensions for SD card etc.)




