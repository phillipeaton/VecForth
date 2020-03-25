HEX

\ Block 04 -----------------------------------------------------
\   target memory size & alignment    6809 DTC   (c) 16apr95 bjr
\
\ These words define the target memory model.
\
\ CELL size and CHAR size are specified in target address units.
\ Any needed conversion to host address units will be done by
\ the memory reference words.
\
2 CONSTANT TCELL   \ target cell size in target address units
: TCELLS  2* ;
: TCELL+  2+ ;
1 CONSTANT TCHAR   \ target character size in target adrs units
: TCHARS     ;
: TCHAR+  1+ ;

\ Block 05 -----------------------------------------------------
\   Multiple target dictionaries, 16 bit         (c) 16apr95 bjr
\ ( 16 bit addresses)
\
\ These words manage multiple dictionaries in the target image.
\ 'TDP  holds the address of the "current" dictionary.
\ DICTIONARY  given origin and limit addresses, defines a dict.
\    When executed it will become the "current" dictionary.
\ TDP THERE TALLOT  are analogous to their host counterparts
\    except that they work in 'target addresses'.
\ ?FULL  aborts if the current DP exceeds its limit.
\ T, TC,  store words/bytes into the image.
\ ORG  sets the current target DP to a new value.
\
\ The T prefix signifies "target", thus THERE should be read as
\ "target HERE".  Normally I hate T-prefix names; these will be
\ imported into the TARGET vocabulary without the T prefix.
\
VARIABLE 'TDP  \ dictionary pointer
: DICTIONARY ( org limit)   CREATE SWAP , , DOES> 'TDP ! ;
: TDP   ( - a) 'TDP @ ;
: THERE ( - a) TDP @ ;
: ?FULL ( a - a) TDP CELL + @ OVER U<
    ABORT" Dictionary overflowed!" ;
: TALLOT ( n)  TDP +! ;
: T, ( n)      THERE ?FULL T!  TCELL   TALLOT ;
: TC, ( n)     THERE ?FULL TC! TCHAR   TALLOT ;
: ORG ( n)     TDP !  ;


\ Block 06 -----------------------------------------------------
\   target threading model            6809 DTC   (c) 17apr95 bjr
\
\ These words embody the target threading model and alignment.
\ TALIGN  aligns the target dictionary ptr; TALIGNED aligns any
\   target address.  (These are no-ops on the byte-aligned 6809).
\
\ *Note* Chromium 2 assumes all word references in the target use
\ Absolute addresses, with an optional prefix:   STC  DTC  ITC
\ ,!XT  begins a Forth thread reference.         JSR  ---  ---
\ ,!CF  begins the code field of a Forth word.   ---  JSR  ---
\ ,CODE  lays the code field of a CODE word.     ---  ---  Here+2
\
\ T!CF  changes the code field of the target Forth word at 'tadr'.
\    This should also store the JSR/JMP instruction, if used.
\ T>BODY  converts a target cfa (technically, xt) to a target pfa.
\
: TALIGNED ( a - a)  ;      \ align given target address
: TALIGN     ;              \ align target HERE

HEX 0BD CONSTANT (JSR) \ 6809 JSR-extended opcode
: ,!VAL ( value tadr - n)  T!  2 ;             \ 16-bit value
: ,!REL ( value tadr - n)  TUCK 1+ - SWAP TC!  1 ;  \ 8-bit offs
: ,!JSR ( value tadr - n)  (JSR) OVER TC!  1+ T!  3 ;  \ JSR
: ,!XT ( value tadr - n)   ,!VAL ;             \ Forth thread
: ,!CF ( value tadr - n)   ,!JSR ;             \ code field

: T!CF ( value tadr - )    ,!CF DROP ;
: ,CODE ;              \ append code field for CODE word
: T>BODY   3 + ;   \ tcfa -- tpfa   address of parameter field

\ Block 07 -----------------------------------------------------
\   target header structure           CamelForth (c)  31mar15nac
\
\ These words define the target header structure.  (CamelForth)
\    These are model-specific but usually not CPU-specific.
\    If n-way hashed lists and peculiar vocabulary schemes are
\    used in the target, this screen can become much more complex!
\ HEADS  if 0, will prevent headers from being generated.
\ TLATEST  holds the target nfa of the last word defined in the
\    target.  Note that the linking scheme is model-specific.
\ (THEADER)  builds a header in the target, except for the code
\    field, linking it into the image dictionary.  It parses a
\    WORD non-destructively.  Header layout is model-specific.
\ (TIMMEDIATE)  makes the LATEST target word "immediate".
\    This is also model-specific.
\
\ Once this screen is loaded, we have all the support words
\ needed for the assembler.
\
VARIABLE HEADS    -1 HEADS !     \ true = generate headers
VARIABLE TLATEST  0 TLATEST !    \ addr of latest target def'n

: (THEADER)               \ lay header except for code field
    HEADS @ IF   >IN @
        TLATEST @ T,            \ target link
        THERE TLATEST !         \ update target link
        BL WORD THERE
        OVER C@         \ src(host), dst(target), len
        WIDTH @ MIN 1+          \ F83 limit length, +1 for count
        DUP TALLOT >TCMOVE      \ allot & copy to target
    >IN !  THEN ;               \ no ALIGN needed
HEX
: (TIMMEDIATE)            \ change latest header to immediate
    HEADS @ IF  TLATEST @ DUP TC@ 40 OR SWAP TC!   THEN ;































