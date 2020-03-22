\ 8\   Generic MIRROR word                          (c) 18apr95 bjr
0 CELL * OFFSET TARG-VALUE    1 CELL * OFFSET HOST-COMP
2 CELL * OFFSET HOST-EXEC     3 CELL * OFFSET FWD-LIST
4 CELL * OFFSET MIRR-LINK     5 CELL * OFFSET PUT-CODE
6 CELL * OFFSET IMM-FLAG
VARIABLE TSTATE   0 TSTATE !   \ metacompile STATE
VARIABLE MLATEST  0 MLATEST !  \ mirror adrs of latest targ defn
VARIABLE 'MIRROR  0 'MIRROR !  \ adrs of latest mirror defined
VARIABLE 'MA                   \ pfa of mirror word being exec'd
: MA ( - a)   'MA @ ;          \ get address of mirror data

: MIRROR ( host-exec host-comp targ-val -- )
    CREATE  HERE >R  , , ,  0 ,  'MIRROR @ , R> 'MIRROR !
       ['] ,!XT ,  0 ,         \ default "put" code
    DOES> DUP 'MA !  DUP IMM-FLAG @ TSTATE @ 0= OR
       IF HOST-EXEC ELSE HOST-COMP THEN @ EXECUTE ;
\ \\   Forward referencing                          (c) 17apr95 bjr
0 CELL * OFFSET REF-LINK      1 CELL * OFFSET REF-ADRS
: PUT-REF ( value tadr - n)  MA PUT-CODE @ EXECUTE ;
: !REF ( value tadr - )      PUT-REF DROP ;
: ,REF ( value - )           THERE PUT-REF TALLOT ;
: +FWDREF ( - )           \ add target HERE to forward ref list
    MA FWD-LIST DUP @        \ get adrs of list head  -- la head
    HERE ROT !               \ this becomes new list head
    , THERE ,                \ store link,targ-adrs in host
    0 ,REF ;                 \ store empty cell in target
: (RESOLVE) ( value -- )   \ expects mirror adrs in 'MA
    DUP MA TARG-VALUE !         \ store value in mirror word
    MA FWD-LIST DUP @  0 ROT !  \ adrs of list element, zap head
    BEGIN DUP WHILE             \ -- value el-adrs
        2DUP REF-ADRS @ !REF    \ patch cell in target image
    REF-LINK @ REPEAT  2DROP ;
\ \\   Host actions for mirror words                (c) 18apr95 bjr
: .UNDEF        ." Error: no host action defined. " ABORT ;
: (PRESUME)     ['] .UNDEF ['] +FWDREF 0  MIRROR ;
: PRESUME       (PRESUME) 'MIRROR @ MLATEST ! ;  \ for EMULATES
: IS-CF         ['] ,!CF  MLATEST @ PUT-CODE ! ;
: IS-JSR        ['] ,!JSR MLATEST @ PUT-CODE ! ;
: MIMMEDIATE    -1 MLATEST @ IMM-FLAG ! ;
: EMULATES      ALSO META '  MLATEST @ HOST-EXEC !  PREVIOUS ;
: EMULATE: ALSO META :NONAME >R >R >R >R MLATEST @ HOST-EXEC !
R> R> R> R> ; : ;EMULATE  POSTPONE ;  PREVIOUS ;  IMMEDIATE
: M,REF ( - )   MA TARG-VALUE @ ,REF ;   \ usual compile action
: DOCOMP ( - )  MA HOST-COMP @ EXECUTE ;
: MCOMPILE, ( mcfa - )  >BODY 'MA ! DOCOMP ;
: [TCOMPILE]    ' MCOMPILE, ;  \ force compile of following word
: T' ( - tcfa)  ' >BODY TARG-VALUE @ ;
: TPFA ( - tpfa)  MA TARG-VALUE @ T>BODY ;   \ get pfa of target
\ \\   Create/resolve mirror word                   (c) 18apr95 bjr
: ?MIRROR  \ hexec hcomp tval --  build/resolve mirror word
    >IN @ >R  DEFINED  IF >BODY   \ -- ma    if exists,
        DUP HOST-COMP @ ['] +FWDREF = IF   \ and it's fwd-ref,
            DUP MLATEST !              \ save ma for DOES>
            'MA ! (RESOLVE)            \ resolve it
            MA HOST-COMP !  DROP       \ store compile action
            R> DROP EXIT               \ we're done
    THEN THEN                     \ -- textadr   otherwise,
    DROP R> >IN ! MIRROR            \ build the needed mirror
    'MIRROR @ MLATEST ! ;           \ save ma for DOES>

: RESOLVES  \ value RESOLVES name   resolve a fwd ref
    ' >BODY 'MA ! (RESOLVE)    \ get mirror word adrs, resolve
    ['] M,REF MA HOST-COMP ! ; \ change compile action to append

\ \\   Meta CREATE                                  (c) 18apr95 bjr
ALSO TARGET DEFINITIONS  PRESUME DOCREATE IS-CF
PREVIOUS DEFINITIONS

: M+HEADER                \ common factor of TCREATE etc.
    (THEADER)                      \ compile header in target
    ['] .UNDEF ['] M,REF THERE ?MIRROR ;  \ build mirror word

: TCREATE  M+HEADER
    [ TARGET ] ['] DOCREATE [ META ] MCOMPILE, ;
: ;C     [ ASSEMBLER ] EXITCODE [ META ]   PREVIOUS ;
: ASM:   [ ASSEMBLER ] ENTERCODE [ META ]  ALSO ASSEMBLER ;
: CODE   M+HEADER  ,CODE  ASM: ;  \ build CODE word's code fld

\ \\   Literals, equates, duals                     (c) 17apr95 bjr
ALSO TARGET DEFINITIONS  PRESUME LIT  PREVIOUS DEFINITIONS
: TLITERAL ( n - )
    [ TARGET ] ['] LIT [ META ] MCOMPILE,  T, ;
: T['] ( - )   T' TLITERAL ;
: M['] ( - )   T' [COMPILE] LITERAL ; IMMEDIATE  \ lit in host!

: @EQU ( - n )   MA TARG-VALUE @ ;       \ fetch TARG-VALUE
: ,EQU ( - )    @EQU TLITERAL ;
: EQU  ( n - )   ['] @EQU ['] ,EQU ROT MIRROR ;

VARIABLE DUAL   0 DUAL !            \ true=generate host "dual"
: DUAL?   DUAL @ TSTATE @ AND ;     \ true if dual & compiling
TARGET ' DOCREATE ." ->" .s META @ CONSTANT MIRROR-CF    \ F83
: DUAL, ( xt -- )  DUP @ MIRROR-CF =                     \ F83
    IF >BODY HOST-EXEC @ THEN  ( xt) COMPILE, ;          \ F83
\ \\   Meta compiler engine                    F83  (c) 18apr95 bjr
: T[   0 TSTATE ! ;
: T]  -1 TSTATE !
   BEGIN  ?STACK  >IN @
           SOURCE SWAP DROP OVER = IF ( ." BUFFER EMPTY! " ) REFILL 2DROP 0 THEN    \  <--- LINE ADDED
      DEFINED \ F83
      IF  ( found) NIP DUAL? IF DUP DUAL, THEN
                        EXECUTE
      ELSE NUMBER?
         IF ( number)      DUAL? IF DUP [COMPILE] LITERAL THEN
                        TLITERAL  DROP
         ELSE ( undef)  DUAL? IF .UNDEF THEN
                         DROP  >IN ! (PRESUME)  \ build fwd ref
                        'MIRROR @ 'MA ! +FWDREF
      THEN THEN
   TSTATE @ 0=  END? @ OR  UNTIL  END? OFF ;   \ F83



\ \\   Target interpretation                             31mar15nac
: D>T ( d)   DPL @ 1+ IF SWAP >T ELSE DROP THEN  >T ;

: TINTERPRET   BEGIN ?STACK DEFINED
      IF ( found)  EXECUTE
      ELSE ( number?)  NUMBER D>T  THEN
   FALSE DONE? UNTIL ;

: TQUIT   BLK OFF STATE OFF
   BEGIN  RP0 @ RP!  CR QUERY TINTERPRET ." Tok"  AGAIN ;

: HOT    CR ."  Tok" QUIT ;
: COOL   CR ."  ok"  QUIT ;

\ \\   Target colon definition                      (c) 17apr95 bjr
ALSO TARGET DEFINITIONS  PRESUME DOCOLON IS-CF
                         PRESUME EXIT     PREVIOUS DEFINITIONS

: T:   M+HEADER                       \ targ.header & mirror
    [ TARGET ] ['] DOCOLON [ META ] MCOMPILE,   \ build colondef
    DUAL? IF
        :NONAME                        \ F83
        'MIRROR @ HOST-EXEC !          \ start dual definition
    THEN T] ;

: T;  [ TARGET ] ['] EXIT [ META ] MCOMPILE,   \ compile EXIT
    DUAL? IF  POSTPONE ; THEN                   \ F83??
     T[ ;

: TIMMEDIATE    (TIMMEDIATE) MIMMEDIATE ;
\ \\   Meta DOES>                              F83  (c) 19apr95 bjr
ALSO TARGET DEFINITIONS  PRESUME DODOES IS-JSR
                         PRESUME (DOES>)  PREVIOUS DEFINITIONS
VARIABLE TARG-DOES       \ to remember adrs of target DOES code
: (MDOES>)  ( adrs.targ.code -- )       \ *** not valid ANSI ***
    MLATEST @ TARG-VALUE @ ( tcfa) T!CF  \ change tgt code field
    R> MLATEST @ HOST-EXEC ! ;     \ host-exec -> following code
: MDOES>   TARG-DOES @ [COMPILE] LITERAL \ compile tgt DOES adr
    POSTPONE (MDOES>)                    \ compile host (MDOES>)
    NEST , POSTPONE TPFA ; IMMEDIATE     \ begin host's DOES def

: T;CODE   [ TARGET ] ['] (DOES>) [ META ] MCOMPILE,
    THERE TARG-DOES !  T[ ALSO ASSEMBLER ;
: TDOES>   [ TARGET ] ['] (DOES>) [ META ] MCOMPILE,
    THERE TARG-DOES !   [ TARGET ] DODOES [ META ]
    DUAL? IF [COMPILE] MDOES> THEN  ;
\ \\   Initial target vocabulary: directives        (c) 18apr95 bjr
ALSO TARGET DEFINITIONS  META
AKA META META    AKA TARGET TARGET    AKA ASSEMBLER ASSEMBLER
AKA LOAD LOAD    AKA \ \              AKA \S \S
AKA CODE CODE    AKA EQU EQU          AKA ;C ;C
AKA T;CODE ;CODE     AKA BYE BYE
AKA RESOLVES RESOLVES   AKA ORG ORG   AKA THRU THRU
AKA [TCOMPILE] [COMPILE]   AKA ( (    AKA ASM: ASM:
AKA EMULATES EMULATES      AKA EMULATE: EMULATE:
AKA ;EMULATE ;EMULATE IMMEDIATE   AKA DICTIONARY DICTIONARY
AKA PRESUME PRESUME        AKA IS-CF IS-CF   AKA AKA AKA

: +DUAL  DUAL ON ;    : -DUAL  DUAL OFF ;
: +HEADS HEADS ON ;   : -HEADS HEADS OFF ;
META

\ \\   Initial target vocabulary: synonyms          (c) 17apr95 bjr
PRESUME :         EMULATES T:
PRESUME ;         EMULATES T;          MIMMEDIATE
PRESUME IMMEDIATE EMULATES TIMMEDIATE
PRESUME DOES>     EMULATES TDOES>      MIMMEDIATE
PRESUME [         EMULATES T[          MIMMEDIATE
PRESUME ]         EMULATES T]
PRESUME @         EMULATES T@
PRESUME !         EMULATES T!
PRESUME C@        EMULATES TC@     PRESUME LOAD   EMULATES LOAD
PRESUME C!        EMULATES TC!     PRESUME THRU   EMULATES THRU
PRESUME ,         EMULATES T,
PRESUME C,        EMULATES TC,
PRESUME HERE      EMULATES THERE
PRESUME ALLOT     EMULATES TALLOT
PRESUME CREATE    EMULATES TCREATE
\ \\   Initial target vocabulary: synonyms          (c) 1
PRESUME '         EMULATES T'
PRESUME [']       EMULATES T[']       MIMMEDIATE
PRESUME CELL      EMULATES TCELL
PRESUME CELLS     EMULATES TCELLS
PRESUME CELL+     EMULATES TCELL+
PRESUME CHARS     EMULATES TCHARS
PRESUME CHAR+     EMULATES TCHAR+
PRESUME ALIGN     EMULATES TALIGN
PRESUME ALIGNED   EMULATES TALIGNED
PRESUME HEX       EMULATES HEX
PRESUME DECIMAL   EMULATES DECIMAL
PRESUME (         EMULATES (        MIMMEDIATE
PRESUME \         EMULATES \        MIMMEDIATE
PREVIOUS DEFINITIONS

\ \\ Support for CamelForth DO loops                (c) 25apr95 bjr
CR ORDER
CREATE LOOPSTACK  20 CELL * ALLOT
VARIABLE TLP      LOOPSTACK TLP !       \ target loop stack ptr

: T>L  CELL TLP +!  TLP @ ! ;         \ x --   put on loop stack
: TL>  TLP @ @   CELL NEGATE TLP +! ; \ -- x   get from loop stk

\ Forth words which need to be in META vocabulary
AKA DUP DUP  AKA SWAP SWAP
: .S CR .S ;

\ Forth words which need to be in META ASSEMBLER
ALSO ASSEMBLER DEFINITIONS
AKA + +
PREVIOUS DEFINITIONS

\ 23\ Show mirror words                              (c) 18apr95 bjr
VARIABLE EVERYONE  1 EVERYONE !  DECIMAL

: .MIRRORS   'MIRROR @
   BEGIN DUP WHILE  ( - ma )
    DUP FWD-LIST @  EVERYONE @ OR  IF
      DUP           CR BODY>           10 .XTID
      DUP           @   ." : value="    5 U.R
      DUP FWD-LIST  @   ."  fwdlist="   5 U.R
      DUP PUT-CODE  @   ."  put="       6 .XTID
      DUP HOST-COMP @   ."  comp="      7 .XTID
      DUP HOST-EXEC @   ."  exec="      6 .XTID
      KEY? IF KEY DROP KEY 27 = IF DROP EXIT THEN THEN
    THEN
   MIRR-LINK @ REPEAT DROP ;

