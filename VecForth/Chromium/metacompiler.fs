HEX

\ Block 08 -----------------------------------------------------
\   Generic MIRROR word                          (c) 18apr95 bjr
\
\ Generic definition for the 'mirror' word.  These words reside in
\   the TARGET vocabulary and are executed during metacompilation.
\ TARG-VALUE  is usually the address of the corresponding target
\    word.  It may also be a numeric value (for an EQUATE).
\ HOST-COMP  is what the host executes in compile state.
\ HOST-EXEC  is what the host executes in interpret state.
\ FWD-LIST  is the head of this word's forward-reference list.
\ MIRR-LINK  maintains a linked list of all mirror words.
\ TSTATE  is -1 when metacompiling, 0 when metainterpreting.
\ MLATEST  is the mirror word most recently CREATEd.
\ 'MIRROR  holds the pfa of the last-defined mirror word.
\ MIRROR  creates a mirror word in the host, initializing its
\    forward-ref list to empty.  The action of a mirror word is
\    to execute either the HOST-COMP or HOST-EXEC vector, with
\    the address of this data structure in the variable 'MA.
\ MA  returns the data address of the mirror word being exec'd.
\
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

\ Block 09 -----------------------------------------------------
\   Forward referencing                          (c) 17apr95 bjr
\
\ Forward references are accumulated in a linked list in the
\ HOST's memory space.  Each element in the list contains a link
\ (0 for end of list), followed by the address in the target which
\ has to be "patched" later.  The mirror word holds the list head.
\
\ !REF  uses the mirror's PUT-CODE to patch a reference.
\ ,REF  uses the mirror's PUT-CODE to append a reference.
\ +FWDREF  is the basic compile action for a forward-reference.
\   It adds the target HERE to the list of patch addresses.
\   'MA' is the pfa of the mirror word ('mirror address').
\ (RESOLVE)  given 'adrs' within the target, and pfa of a mirror
\   word 'ma', patches all forward references, then zaps the fwd
\   list head.  Sets value; does not change comp/exec actions.
\   NB: all references assumed to be one-cell absolute addresses!
\
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

\ Block 10 -----------------------------------------------------
\   Host actions for mirror words                (c) 18apr95 bjr
\
\ Each target word has associated with it (in the "mirror" word)
\ a "compiling" action and an "executing" action.  For most words
\ compiling is "compile my address" and executing is "error".
\ .UNDEF  is the execute action for a forward-referenced word.
\   Since the value is not yet known, print an error message.
\ PRESUME name   predefines a forward reference to a target word,
\   so it can be used within the metacompiler -- e.g. LIT, EXIT.
\   PRESUME-CF defines a code field, PRESUME-ADRS a simple adrs.
\ EMULATES name   changes the action of the MLATEST mirror to the
\    host word 'name'.  Works w/PRESUME, eg, PRESUME @ EMULATES T@
\ EMULATE: ..code.. ;EMULATE   changes the action of the MLATEST
\    mirror word to the given Forth code.
\ TIMMEDIATE  makes the latest target word immediate, by setting
\    (TIMMEDIATE) and MIMMEDIATE.
\ T' name   gets the cfa (xt) of a target word, by fetching it
\   from the mirror word 'name'.
\
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

\ Block 11 -----------------------------------------------------
\   Create/resolve mirror word                   (c) 18apr95 bjr
\
\ ?MIRROR  must recognize three cases.  (Note: may change 'MA)
\ 1. Target word has been forward-referenced (or PRESUMEd):
\    Resolve value and set compile action in existing mirror word.
\    Leave execute action alone (may have been pre-defined).
\ 2. Mirror word exists but is NOT fwd ref (duplicate definition):
\    Leave old mirror word alone; build new mirror word.
\ 3. Mirror does not exist: build new mirror word.
\
\ RESOLVES  is a convenience, e.g., HERE RESOLVES DOCREATE.
\   The word being resolved had better be a mirror word!
\
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

\ Block 12 -----------------------------------------------------
\   Meta CREATE                                  (c) 18apr95 bjr
\
\ This is the metacompiler's CREATE.
\ DOCREATE  is a mirror word for the target's DOCREATE code.  We
\   define this now so the metacompiler can forward-reference it.
\
\ TCREATE  builds an (optional) header in the target, then builds
\   (or resolves) a mirror word whose value=target cfa, and whose
\   action is to compile that target word into a target thread.
\   Finally it builds the default target code field (DOCREATE).
\ CODE  is similar, but does not compile a reference to DOCREATE,
\   and adds the meta assembler to the search order.
\ END-CODE  drops the meta assembler from the search order.
\
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

\ Block 13 -----------------------------------------------------
\   Literals, equates, duals                     (c) 17apr95 bjr
\
\ These words handle EQUATEs and compile literal values.
\ LIT  is a mirror word for the target's LIT.  We define this now
\    so that TLITERAL can forward-reference it.
\ TLITERAL  compiles a single cell literal into the image.
\    NOTE: this implementation won't handle fwd STC references!
\ @EQU  is the execute action for an Equate (fetch TARG-VALUE).
\ ,EQU  is the compile action for an Equate (append a TLITERAL).
\ EQU  defines an equate with value n.
\
\ DUAL  when true, causes mirror words to be compiled in the host
\   while target words are compiled in the target.  This builds an
\   analog ("dual") of the target colon definition in the host.
\
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

\ Block 14 -----------------------------------------------------
\   Meta compiler engine                    F83  (c) 18apr95 bjr
\
\ T[  sets metainterpreting state.
\ T]  sets metacompiling state and enters the meta compiling loop.
\    Words from the input stream are searched (in the TARGET
\    vocabulary) and executed.  The execution action of a defined
\    target word is to compile itself.  Other words, such as
\    compiler directives, perform their programmed action.
\    This loop will end upon ; or [.  F83 note: colon definitions
\    cannot cross screen boundaries.
\
\ Note that while metacompiling, all words are actually executed!
\ The metacompiler works like an assembler in this regard.
\
: T[   0 TSTATE ! ;

: T]-EVAL \ offset c-addr -- ;
   FIND                     \ -- offset [ xt +/-1 | c-addr 0 ];
   0<> IF                   \ -- offset xt ; Word found?
      NIP                   \ -- xt ;
      EXECUTE               \ -- ; ( i*x xt — j*x )
   ELSE                     \ -- offset c-addr ; Word not found
      NUMBER?               \ -- offset [ c-addr 0 | n -1 | d 0> ] ; [ string -- string 0 / n -1 / d 0> ]
      0<> IF                \ -- offset n ; Number found?
         NIP TLITERAL       \ -- n ; Drop offset, compile literal
      ELSE                  \ -- n c-addr ; Number not found
         DROP               \ -- offset ; Drop string
         >IN ! (PRESUME)    \ build fwd ref
         'MIRROR @ 'MA ! +FWDREF
      THEN
   THEN
;

: T]
   -1 TSTATE !
   BEGIN                         \ -- ;
      ?STACK                     \ -- ;
      >IN @                      \ -- offset ;
      SOURCE                     \ -- offset addr len ;
      NIP OVER = IF              \ -- offset ; Offset = buffer length? i.e. is buffer empty
         DROP                    \ -- ; dump offset, will get reset
         REFILL 0=               \ -- f ; ff = refill successful
      ELSE                       \
         BL WORD                 \ -- offset c-addr ; String len=0 when WORD fails
         T]-EVAL                 \ -- ;
         TSTATE @ 0=             \ -- f ; tf = have returned to interpret state
      THEN
  UNTIL
;

\ Block 15 -----------------------------------------------------
\   Target interpretation                             31mar15nac
\
\ Target interpretation is only valid when the target CPU is
\ actually connected "live" to the host CPU.
\
\ D>T  pushes a single or double number on the target's stack.
\    This uses the talker word >T , put a number on target stack.
\
\ TINTERPRET and TQUIT  are a modified interpreter loop to run
\    Forth words remotely on the target machine.  Numbers are
\    pushed on the target stack.  Target words are automatically
\    INVOKEd when their mirror words are executed on the host.
\
\ HOT  starts the target interpreter.  This will run until COOL
\    is executed, or UNTIL ANY ERROR CONDITION IS ENCOUNTERED.
\    (F83 restarts its own QUIT after any error!)
\
: D>T ( d)   DPL @ 1+ IF SWAP >T ELSE DROP THEN  >T ;

: TINTERPRET   BEGIN ?STACK DEFINED
      IF ( found)  EXECUTE
      ELSE ( number?)  NUMBER D>T  THEN
   FALSE DONE? UNTIL ;

: TQUIT   BLK OFF STATE OFF
   BEGIN  RP0 @ RP!  CR QUERY TINTERPRET ." Tok"  AGAIN ;

: HOT    CR ."  Tok" QUIT ;
: COOL   CR ."  ok"  QUIT ;


\ Block 16 -----------------------------------------------------
\   Target colon definition                      (c) 17apr95 bjr
\
\ DOCOLON  is a mirror word for the target's DOCOLON code.
\ EXIT  is a mirror word for the target's EXIT.  We define these
\   now so that T: and T; can forward-reference them.
\
\ T:  starts a target colon definition, building the target header
\   and code field, and entering metacompile state.  If DUAL is
\   true, builds the code field for the host analog, and stores
\   this as the execute action of the mirror word.
\
\ T;  ends a target colon definition and leaves metacompile state.
\   If DUAL, ends the analog definition in the host.
\   NOTE: this version does not correctly handle STC.
\
\ Normally I hate "T-prefix" names, but when these are imported
\ into the TARGET vocabulary, they'll be known as : and ;.
\
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

\ Block 17 -----------------------------------------------------
\   Meta DOES>                              F83  (c) 19apr95 bjr
\
\ These words allow the host machine to correctly build "defining"
\ and "defined" words in the target.  The target's DODOES code and
\ (DOES>) word need to be forward-referenced.
\ TARG-DOES  holds the address of the target's DOES> code.
\ TPFA  as a mirror action, returns the pfa of the target word.
\ (MDOES>)  when executed by the host machine, changes the execute
\    action of the most recently defined target word, in the image
\    AND in the host's mirror word.  The host's "execute" vector
\    is set to the address immediately following the (DOES>) .
\ MDOES>  a) adds the target DOES address and meta (MDOES>) to the
\    host's colon definition; b) builds a headerless colon def
\    for the host's DOES> action, and starts it with TPFA.
\ TDOES>  a) compiles target (DOES>) and JSR DODOES, and saves the
\    target DOES address; b) performs MDOES> if dual is enabled.
\ You really need to see a picture to understand this.
\ Refer also to the target's source code for DOES> and ;CODE .
\
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

\ Block 18 -----------------------------------------------------
\   Initial target vocabulary: directives        (c) 18apr95 bjr
\
\ These are words which must be recognized while metacompiling
\ (i.e., while in the TARGET vocabulary), but which have no
\ equivalent in the target Forth.
\
\ AKA xxx xxx  simply makes a synonym in the TARGET vocabulary.
\
\ TARGET  is the root of the "mirrored" dictionary tree, which
\    will be built in the host.  This tree will hold all of the
\    "mirror" words and will exactly duplicate the search order
\    of the dictionary being built in the image. (scr # 2)
\
\ Once the TARGET vocabulary is sealed, the only exits are
\ META  to select the META vocabulary
\ ASSEMBLER  to select the META ASSEMBLER vocabulary
\ CODE  to create a code header and select META ASSEMBLER
\
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

\ Block 19 -----------------------------------------------------
\   Initial target vocabulary: synonyms          (c) 17apr95 bjr
\
\ These are words which will be defined in the target Forth, but
\ which also have equivalent actions in the host.  Each is
\ predefined here as a forward reference with a specified execute
\ action (PRESUME targ-name EMULATES host-word).  When the target
\ definition for the word is given (CODE, colon, or CREATE),
\ the execute action will remain unchanged AS LONG AS "DUAL" IS
\ SWITCHED OFF!
\
\ These are the words which can be used in "dual" definitions.
\ (Note that AKA words cannot be used in duals!)
\
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

\ Block 20 -----------------------------------------------------
\   Initial target vocabulary: synonyms          (c) 1
\
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

\ Block 21 -----------------------------------------------------
\ Support for CamelForth DO loops                (c) 25apr95 bjr
\
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

\ Block 22 -----------------------------------------------------
\ (Spare)
\

\ Block 23 -----------------------------------------------------
\ Show mirror words                              (c) 18apr95 bjr
\
\ Normally, EVERYONE is 0 and .MIRRORS - executed at the end of
\ the build process - only reports unresolved words (which
\ represent errors).
\
\ With EVERYONE set to 1, .MIRRORS reports all of the mirror
\ words. This can be useful for debug of the metacompiler itself
\ or of errors arising from the addition of new code to CamelForth
\ itself.
\
VARIABLE EVERYONE  1 EVERYONE !  DECIMAL

: .MIRRORS   'MIRROR @
   BEGIN DUP WHILE  ( - ma )
    DUP FWD-LIST @  EVERYONE @ OR  IF
      DUP           CR BODY>           20 .XTID
      DUP           @   ." : value="    5 U.R
      DUP FWD-LIST  @   ."  fwdlist="   5 U.R
      DUP PUT-CODE  @   ."  put="       6 .XTID
      DUP HOST-COMP @   ."  comp="      7 .XTID
      DUP HOST-EXEC @   ."  exec="      6 .XTID
      KEY? IF KEY DROP KEY 27 = IF DROP EXIT THEN THEN
    THEN
   MIRR-LINK @ REPEAT DROP ;
