HEX

\ Block 02 -----------------------------------------------------
\   gforth extensions & metacompiler vocabularies     31jan16nac
\
\ Version 2 improvements:
\ 1. Generalized the forward-reference mechanism so that it can be
\    used within the metacompiler, eliminating 'DOVAR 'LIT etc.
\ 2. Added ability to predefine host actions of target words.
\ 3. Added DUAL mechanism to build host analogs of target words.
\ 4. Factored ITC/DTC/STC dependencies into a small word set.
\
\ LIMITATIONS of the Chromium metacompiler, release 2 (2 Apr 95)
\ 1. The target Forth word size must be the same as or smaller
\    than the host's.  Thus only 16-bit Forths can be produced.
\ 2. When metacompiling, the same WIDTH is used in the host and
\    target dictionaries.  This really isn't a limitation.
\ 3. Word-aligned machines not yet supported.
\ 4. T['] cannot do forward references.
\ 5. Target colon definitions cannot span multiple blocks.
\ 6. T: and T; must be rewritten if STC is to be used.
\
ROOT DEFINITIONS \ Expand ROOT to include..
   ' DEFINITIONS ALIAS DEFINITIONS
   ' ALSO        ALIAS ALSO
   ' ONLY        ALIAS ONLY
   ' BYE         ALIAS BYE
   ' INCLUDE     ALIAS INCLUDE
   ' +           ALIAS +
   ' -           ALIAS -

\ OFFSET  defines simple data-structure words.
\ AKA  defines a synonym word.  Usage:  AKA oldword newname
\ THRU  is redefined to print screen number & stack contents.
\ :NONAME  begins a headerless Forth definition in the host, and
\    returns the address (cfa) of the new definition.
\ ;NONAME  ends a headerless Forth definition in the host.
\ COMPILE, appends an execution token to a host Forth definition.
\
\ META  holds the words which comprise the metacompiler.
\ ASSEMBLER  holds the cross-assembler.
\ TARGET  holds the "symbol" words for all target definitions,
\    plus target compiler directives.  TARGET is selected during
\    metacompilation and metainterpretation.  Within TARGET is a
\    vocabulary tree exactly paralleling that of the target Forth.
\
ONLY FORTH DEFINITIONS     \ extensions
1 CELLS CONSTANT CELL      \ host computer's cell size
: OFFSET ( n - ) CREATE , DOES> @ + ;
: AKA    ' CREATE , DOES> @ EXECUTE ;     \ AKA oldname newname
: THRU   1+ SWAP DO  CR I .  .S I LOAD LOOP ;   \ lo hi THRU
: DEFINED ( -- here 0 | cfa [ -1 | 1 ] ) BL WORD FIND ;
: 2+ 2 + ;
: .XTID ( xt n -- ) >R >NAME DUP 0= IF DROP S" <noname>" ELSE
  NAME>STRING THEN DUP >R TYPE R> R> SWAP - 1 MAX SPACES ;

VOCABULARY META  ALSO META DEFINITIONS    \ metacompiler
VOCABULARY ASSEMBLER                      \ cross-assembler
VOCABULARY TARGET                         \ target words
