\ 4\   target memory size & alignment    6809 DTC   (c) 16apr95 bjr
2 CONSTANT TCELL   \ target cell size in target address units
: TCELLS  2* ;
: TCELL+  2+ ;
1 CONSTANT TCHAR   \ target character size in target adrs units
: TCHARS     ;
: TCHAR+  1+ ;

\ 5\   Multiple target dictionaries, 16 bit         (c) 16apr95 bjr
( 16 bit addresses)

VARIABLE 'TDP  \ dictionary pointer
: DICTIONARY ( org limit)   CREATE SWAP , , DOES> 'TDP ! ;
: TDP   ( - a) 'TDP @ ;
: THERE ( - a) TDP @ ;
: ?FULL ( a - a) TDP CELL + @ OVER U<
    ABORT" Dictionary overflowed!" ;
: TALLOT ( n)  TDP +! ;
: T, ( n)      THERE ?FULL T!  TCELL TALLOT ;
: TC, ( n)     THERE ?FULL TC! TCHAR TALLOT ;
: ORG ( n)     TDP !  ;

\ 6\   target threading model            6809 DTC   (c) 17apr95 bjr
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

\ 7\   target header structure           CamelForth (c)  31mar15nac
VARIABLE HEADS    -1 HEADS !     \ true = generate headers
VARIABLE TLATEST  0 TLATEST !    \ addr of latest target def'n

: (THEADER)               \ lay header except for code field
    HEADS @ IF   >IN @
        TLATEST @ T,            \ target link
        THERE TLATEST !         \ update target link
        BL WORD THERE OVER C@   \ src(host), dst(target), len
        WIDTH @ MIN 1+          \ F83 limit length, +1 for count
        DUP TALLOT >TCMOVE      \ allot & copy to target
    >IN !  THEN ;               \ no ALIGN needed
HEX
: (TIMMEDIATE)            \ change latest header to immediate
    HEADS @ IF  TLATEST @ DUP TC@ 40 OR SWAP TC!   THEN ;

