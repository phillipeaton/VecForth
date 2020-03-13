\ \\ Load screen for Multicomp port                      29apr15nac
DECIMAL     2 LOAD   ( metacompiler vocabularies)
DECIMAL     3 LOAD   ( image to disk, bin endian)
DECIMAL 24 26 THRU   ( DUMP and Intel hex file output)
DECIMAL  4  7 THRU   ( target model - needed for assembler)
DECIMAL 30 44 THRU   ( 6809 assembler)
DECIMAL  8 23 THRU   ( rest of metacompiler)

ONLY FORTH META TARGET DEFINITIONS
DECIMAL 46 48   THRU ( MULTICOMP VER. OF 61-63)
DECIMAL 64 118  THRU ( 6809 SOURCE CODE)
DECIMAL 50 59   THRU DECIMAL 121 127 THRU ( MULTICOMP EXTRAS)
DECIMAL 49      LOAD ( MULTICOMP VER. OF 119) DECIMAL 120 LOAD
ONLY FORTH ALSO META
HEX E000 2000 HEXFILE 6809M.HEX  ( make hex file )
.MIRRORS \ BYE                   ( print undef'd references )
\ \\ like scr61 but for multicomp                        14apr15nac
HEX E000 FFFF DICTIONARY ROM  ROM
   0600 EQU UP-INIT      \ UP must be page aligned.  Stacks,
   06   EQU UP-INIT-HI   \   TIB, etc. init'd relative to UP.

   0000 EQU DP-INIT      \ starting RAM adrs for dictionary
   \ Multicomp map: 8K ROM from E000, RAM from 0, I/O FFD0-FFDF
   \ .. UP-INIT sized for 2k RAM!! UP-INIT is RAMTOP-0200

\ Harvard synonyms - these must all be PRESUMEd
AKA , I,     AKA @ I@     AKA ! I!
AKA C, IC,   AKA C@ IC@   AKA C! IC!
AKA HERE IHERE     AKA ALLOT IALLOT
PRESUME WORD       AKA WORD IWORD

\ \\ like scr62 but for multicomp                        01apr15nac

\ \\ like scr63 but for multicomp                        01apr15nac
HEX FFD0 EQU VDUCMD   FFD1 EQU VDUDAT

CODE KEY    \ -- c    get char from serial port
   6 # ( D) PSHS,   BEGIN,   VDUCMD LDB,   1 # ANDB,  NE UNTIL,
   VDUDAT LDB,   CLRA,   NEXT ;C

CODE KEY?   \ -- f    return true if char waiting
   6 # ( D) PSHS,   CLRA,   VDUCMD LDB,   1 # ANDB,
   NE IF,   -1 # LDB,   THEN,   NEXT ;C

CODE EMIT   \ c --    output character to serial port
   BEGIN,   VDUCMD LDA,   2 # ANDA,   NE UNTIL,
   VDUDAT STB,   6 # ( D) PULS,   NEXT ;C

\ \\ like scr119 but for multicomp                       01apr15nac
ASM: HERE EQU ENTRY   HEX

   UP-INIT-HI # LDA,   A DPR TFR,   \ initial UP
   UP-INIT 100 + # LDS,             \ initial SP
   UP-INIT 200 + # LDU,             \ initial RP

   ' COLD JMP,   ;C           \ enter top-level Forth word
ASM: HERE EQU IRET   RTI,  ;C
HERE  0FFF0 ORG    \ 6809 hardware vectors
  IRET ,  IRET ,  IRET ,  IRET ,    \ tbd, SWI3, SWI2, FIRQ
  IRET ,  IRET ,  IRET ,  ENTRY ,   \ IRQ, SWI, NMI, RESET
ORG
\ \\ Multicomp extras: SDCARD access                     29aug15nac
HEX
: SDLBA2  ( n -- ) FFDC C! ;
: SDLBA10 ( n -- ) DUP FF AND FFDA C! 8 RSHIFT FFDB C! ;

: SDIDLE FFD9 BEGIN DUP C@ 80 = UNTIL DROP ;

: SDRD ( block c-addr-dest -- ) \ read 512 bytes
  SWAP SDLBA10 SDIDLE 0 FFD9 C! ( read)  DUP 200 + SWAP DO
    FFD9 BEGIN DUP C@ E0 = UNTIL DROP ( byte available)
    FFD8 C@ I C! LOOP ;

: SDWR ( block c-addr-src -- ) \ write 512 bytes
  SWAP SDLBA10 SDIDLE 1 FFD9 C! ( write) DUP 200 + SWAP DO
    FFD9 BEGIN DUP C@ A0 = UNTIL DROP ( space available)
    I C@ FFD8 C! LOOP ;
\ \\ Multicomp extras: SDCARD access                     29aug15nac
: SDRDn ( block c-addr-dest #blocks --)
  0 DO 2DUP SDRD 200 + SWAP 1+ SWAP
    LOOP 2DROP ;

: SDRD256 ( block c-addr-dest -- ) \ first 256bytes of block
  SWAP SDLBA10 SDIDLE 0 FFD9 C! ( read)  DUP 100 + SWAP DO
    FFD9 BEGIN DUP C@ E0 = UNTIL DROP ( byte available)
    FFD8 C@ I C! LOOP
  100 0 DO ( 256 bytes)
    FFD9 BEGIN DUP C@ E0 = UNTIL DROP ( byte available)
    FFD8 C@ DROP LOOP ;

: SDRD256n ( block c-addr-dest #blocks --)
  0 DO 2DUP SDRD256 100 + SWAP 1+ SWAP
    LOOP 2DROP ;
\ \\ Multicomp extras: SDCARD access                     17jan16nac
: SDWRn ( block c-addr-dest #blocks --)
  0 DO 2DUP SDWR 200 + SWAP 1+ SWAP
    LOOP 2DROP ;

: SDWRZ ( block -- ) \ write 512 bytes of 0
  SDLBA10 SDIDLE 1 FFD9 C! ( write) 200 0 DO
    FFD9 BEGIN DUP C@ A0 = UNTIL DROP ( space available)
    0 FFD8 C! LOOP ;

\ \\ Multicomp extras: helpers for bootloaders           22jun15nac
: piv ( --) \ helper for PIVOT, PIVOTRST
  4F   1000 C!                     ( CLRA)
  1F8B 1001 !                      ( TFR A,DP)
  86A0 1003 !                      ( LDA #A0)
  B7   1005 C! FFDE 1006 !         ( STA FFDE ; page ROM out)
  1000 EXECUTE
;

: PIVOT ( addr --) \ copy to RAM, init DP, disable ROM, jump
  7E   1008 C!      1009 !         ( JMP addr ; go there)
  piv ;

: PIVOTRST ( - ) \ copy to RAM, init DP, disable ROM, jump rst
  6E9F 1008  ! FFFE 100A !         ( JMP [FFFE] ; go there)
  piv ;
\\: MMUMAP \ enable MMU with 1-1 mapping                29aug15nac
  10 0 DO I DUP 8 LSHIFT OR FFDE ! LOOP 20 FFDE C! ;

: EFTOCD ( --) \ map top 8K to C000-DFFF so it can be loaded
  2607 FFDE ! ;

: CDTOCD ( --) \ restore 1-1 map
  2606 FFDE ! ;

\ \\ Multicomp extras: start CUBIX/BASIC/BUGGY/FORTH     29aug15nac
: CUBIX \ 4KB from SD at 0x2.0000
  MMUMAP EFTOCD
  2 SDLBA2 0 D800 4 SDRDn 0 SDLBA2 \ load bootloader to F800
  CDTOCD PIVOTRST ;                \ restore map, go thru RST

: SDBOOT ( n - ) \ 8KB from SD at 0x2.nnnn and jmp thru RST
  MMUMAP EFTOCD 2 SDLBA2
  ( n ) C000 10 SDRDn 0 SDLBA2  \ load 8Kb from n to E000
  CDTOCD 2787 FFDE ! PIVOTRST ; \ restore map, protect, jmp
: BASIC   1000 SDBOOT ; \ on SD at 0x2.1000
: SDFORTH 0FF0 SDBOOT ; \ on SD at 0x2.0FF0 - blk file with src
: BUGGY \ 7KB from SD at 0x2.1E00
  MMUMAP EFTOCD
  2 SDLBA2 1E00 C400 E SDRDn 0 SDLBA2 \ load to E400
  CDTOCD PIVOTRST ; \ restore map, go thru RST vector
\ \\ Multicomp extras: start FLEX/NITROS9/FUZIX          10may16nac
: FLEX \ 512 bytes (256 used) from SD at 0x2.2000
  MMUMAP
  2 SDLBA2 2000 C100 SDRD 0 SDLBA2
  C100 PIVOT ;                  ( entry point of loader)

: NITROS9 \ load track34 and start it
  \ disk is on SD at 0x2.8000 so track34 is at 0x2.84C8
  MMUMAP EFTOCD
  2 SDLBA2 84C8 2600   \ load from 0x2.84C8 to 2600
  12 SDRD256n 0 SDLBA2 \ 18 (hex 12) 256-byte sectors
  \ vectors to RAM expected by krn
  0100 DFF2 ! 0103 DFF4 ! 010F DFF6 ! \ SWI3 SWI2 FIRQ
  010C DFF8 ! 0106 DFFA ! 0109 DFFC ! \ IRQ  SWI  NMI
  CDTOCD 2602 PIVOT ;  \ restore map, start at 2602
: FUZIX MMUMAP 3 SDLBA2 0 D000 SDRD D000 PIVOT ;
\ \\ RSVD FOR MULTICOMP                                  29apr15nac

\ \\ RSVD FOR MULTICOMP                                  29apr15nac

\ \\ RSVD FOR MULTICOMP - LAST ONE                       29apr15nac


\ \\ Multicomp: SDcard binary save/load to blk file      17jan16nac
HEX
: BADBLOB ." ERROR BAD BLOB " TYPE CR ABORT ;
\ Abort if CFA does not contain magic number
: BADMAG DUP 6 + S" CF09" S= IF S" MAGIC NUMBER" BADBLOB THEN ;
\ Abort if CFA has load-addr == end-addr
: BADSIZ DUP @ OVER CELL+ @ = IF S" ZERO BYTES" BADBLOB THEN ;
\ Abort if CFA has load-addr that differs from HERE
: BADLD  DUP @ HERE <> IF S" LOAD ADDR" BADBLOB THEN ;

: BLOB ( "foo" --) \ create foo as marker for loadable binary
  LATEST @ HERE DUP CREATE , , , \ start end link
  4346 , 3039 , DOES>       \ "CF09" magic number
    CELL+ DUP HERE SWAP !   \ run-time: fill in end-addr
    CELL+ LATEST @ SWAP ! ; \           fill in link-addr

\ \\ Multicomp: SDcard binary save/load to blk file      17jan16nac
: RMBLOB ( "foo" --) \ delete definitions delimited by foo
  ' >BODY BADMAG DUP @ DP ! @ @ LATEST ! ;

\ A word created by BLOB contains code field: BD xx yy where
\ xxyy is an address in BLOB. That address is a fixed offset
\ from ' BLOB. Find the offset (currently 21) and code it below
: FIXBLOB ( "foo" --) \ patch foo to allow later extension
  ['] BLOB 21 +  \ current addr for BLOB DOES>
  ' >BODY BADMAG 2 - ! ; \ patch addr in defined blob

\ \\ Multicomp: SDcard binary save/load to blk file      17jan16nac
: WRBLOB ( n "foo" --) \ write blob foo starting at block n
  1 LSHIFT BLKADRS CELL+ @ +
  ' >BODY BADMAG BADSIZ
  DUP CELL+ @ OVER @ - \ size in bytes
  9 RSHIFT 1 + \ size in 512b sd sectors
  SWAP @ SWAP BLKADRS @ SDLBA2 SDWRn ;

\ \\ Multicomp: SDcard binary save/load to blk file      17jan16nac
: RDBLOB ( n -- ) \ read blob starting at block n
  BLKADRS @ SDLBA2 1 LSHIFT BLKADRS CELL+ @ + HERE
  2DUP SDRD HERE CELL+ NFA>CFA 3 + BADMAG BADLD
  DUP CELL+ @ OVER @ - 9 RSHIFT SWAP >R
  BEGIN DUP 0 <> WHILE \ load remaining sectors
    >R \ stash current count
    200 + SWAP 1+ SWAP \ next addr sector
    2DUP SDRD \ load next sector
    R> 1-
  REPEAT DROP 2DROP R>
  \ stitch the blob into place
  LATEST    @ HERE !     \ Update ..oldest link in loaded blob
  CELL+ DUP @ DP !       \ ..HERE from blob's end-addr
  CELL+     @ LATEST ! ; \ ..LATEST from value stored in blob

\ \\ Multicomp: helpers for ANS BLOCK/BLOCK EXT words    17jan16nac
D000 EQU BBBASE \ 4 blk buffers each 1Kbyte in size
: nxtbuf ( -- buf) \ return the next buf to be (re)-assigned
  BSTATE @ DUP 300 AND DUP 8 RSHIFT >R
  100 + 300 AND SWAP FCFF AND OR BSTATE ! R> ;
\ make the specified buffer the Current buffer
: curbuf ( buf --) A LSHIFT BSTATE @ F3FF AND OR BSTATE ! ;
\ given a buf number return the start address of that buffer
: buf2a  ( buf -- a-addr) A LSHIFT BBBASE + ;
\ given a buffer number set up SDLBA2 and end with parameters
\ ready for SD operation -- remember, 1 SDcard block=512bytes.
: buf2sd ( buf -- sdblock c-addr-src 2)
  DUP buf2a SWAP BLKADRS 2@ SDLBA2
  SWAP 2* CELL+ BSTATE + @ 2* + SWAP 2 ;
: bufrd ( buf --) \ read the assigned block into buf
  1 OVER LSHIFT BSTATE @ AND IF DROP ELSE buf2sd SDRDn THEN ;
\ \\ Multicomp: helpers for ANS BLOCK/BLOCK EXT words    17jan16nac
: bufwr ( buf -- ) \ if buf is Modified, save it, mark !Modified
  1 OVER LSHIFT BSTATE @ AND DUP IF
    ( buf bitmask) BSTATE @ XOR BSTATE ! ( clear Dirty)
    ( buf) buf2sd SDWRn
  ELSE 2DROP THEN ;
\ associate a buf with a blk: search for it. If not found,
\ free up a buffer and assign it. Current undefined after this!
: asnblk ( blk -- buf)
  4 0 DO BSTATE @ 1 I 4 + LSHIFT AND IF
      ( Assigned) I CELLS CELL+ BSTATE + @ OVER =
      IF ( Match) DROP I UNLOOP EXIT THEN
  THEN LOOP ( free up and assign a buf) nxtbuf DUP bufwr
  DUP >R CELLS CELL+ BSTATE + ! ( save blk)
  R> 1 OVER 4 + LSHIFT BSTATE @ OR BSTATE ! ( mark Assigned) ;

\ \\ Multicomp: ANS BLOCK/BLOCK EXT words                17jan16nac
: BLOCK ( blk -- a-addr)  asnblk DUP curbuf DUP bufrd buf2a ;
: BUFFER ( blk -- a-addr) asnblk DUP curbuf           buf2a ;
: LOAD ( blk --)  'SOURCE 2@ >R >R BLK @ >R >IN @ >R ( save)
  DUP BLK ! BLOCK 0 >IN ! 400 INTERPRET      ( 400 = 1kbyte)
  R> >IN ! R> DUP 0= IF DROP 0 ELSE DUP BLK ! BLOCK THEN
  R> 3FF AND OR R> 'SOURCE 2! ;                      ( restore)
: THRU ( blk1 blk2 -- ) 1+ SWAP DO I LOAD  LOOP ;
: SAVE-BUFFERS ( --)        4 0 DO I bufwr LOOP ;
: FLUSH ( --)  SAVE-BUFFERS 0 BSTATE ! ;
: UPDATE ( --) \ mark the Current buffer as Modified
  BSTATE DUP @ DUP A RSHIFT 3 AND 1 SWAP LSHIFT OR SWAP ! ;
: EMPTY-BUFFERS ( --) 0 BSTATE ! ; \ unassign all
: ltop 40 0 DO 2D EMIT LOOP CR ;
: LIST ( blk --) CR ltop DUP SCR ! BLOCK 10 0 DO
       40 0 DO .A      LOOP CR    LOOP DROP ltop ;
\\Start of shadow screens. This is the shadow for scr 0.

* Type 1  LOAD to load the compiler & generate CamelForth in
  file 6809.HEX for the ScroungeMaster II 6809 board.
* Type 45 LOAD  to load the compiler & generate CamelForth in
  file 6809M.HEX for the Multicomp 6809 board.

CHROMIUM.SCR screens index
  0- 29  Chromium 2 metacompiler         (scr 22 spare)
 30- 44  6809 assembler
 45- 59  Multicomp-specific extra stuff  (scr 57-59 spare)
 60-120  6809 CamelForth source code
121-127  Multicomp-specific extra stuff
128-247  Shadow screens
248-255  8KByte binary image (output)

\\Version 2 improvements:
1. Generalized the forward-reference mechanism so that it can be
   used within the metacompiler, eliminating 'DOVAR 'LIT etc.
2. Added ability to predefine host actions of target words.
3. Added DUAL mechanism to build host analogs of target words.
4. Factored ITC/DTC/STC dependencies into a small word set.

LIMITATIONS of the Chromium metacompiler, release 2 (2 Apr 95)
1. The target Forth word size must be the same as or smaller
   than the host's.  Thus only 16-bit Forths can be produced.
2. When metacompiling, the same WIDTH is used in the host and
   target dictionaries.  This really isn't a limitation.
3. Word-aligned machines not yet supported.
4. T['] cannot do forward references.
5. Target colon definitions cannot span multiple blocks.
6. T: and T; must be rewritten if STC is to be used.
\\OFFSET  defines simple data-structure words.
AKA  defines a synonym word.  Usage:  AKA oldword newname
THRU  is redefined to print screen number & stack contents.
:NONAME  begins a headerless Forth definition in the host, and
   returns the address (cfa) of the new definition.
;NONAME  ends a headerless Forth definition in the host.
COMPILE, appends an execution token to a host Forth definition.

META  holds the words which comprise the metacompiler.
ASSEMBLER  holds the cross-assembler.
TARGET  holds the "symbol" words for all target definitions,
   plus target compiler directives.  TARGET is selected during
   metacompilation and metainterpretation.  Within TARGET is a
   vocabulary tree exactly paralleling that of the target Forth.

\\These words store the target image in a range of Forth screens.
   BIG ENDIAN: Most Significant byte is at LOWER adrs, e.g.6809
BASE-SCREEN  is the starting screen # of this range.  There is
   no ending screen #; up to 64 screens (64K) can be used.
   You may change this to suit your disk layout.
><  swaps the hi and lo bytes of the top stack item. <16 BIT!>
VIRTUAL  converts a target image address to an address within
   a block buffer.  Adrs 0000 is the first byte of BASE-SCREEN.
TC@ TC!  fetch and store bytes in the target image.
T@ T!  fetch and store 16-bit cells in the target image.  Since
   a cell may cross a block boundary, these must be done with
   byte operations.
>TCMOVE  copies a string from the host memory to the image.
TEXECUTE  executes the target word at address a.  Since we are
   compiling to disk, this simply prints an error message.
>T T>  move data to and from the target stack (future use).
\\These words define the target memory model.

CELL size and CHAR size are specified in target address units.
Any needed conversion to host address units will be done by
the memory reference words.

\\These words manage multiple dictionaries in the target image.
'TDP  holds the address of the "current" dictionary.
DICTIONARY  given origin and limit addresses, defines a dict.
   When executed it will become the "current" dictionary.
TDP THERE TALLOT  are analogous to their host counterparts
   except that they work in 'target addresses'.
?FULL  aborts if the current DP exceeds its limit.
T, TC,  store words/bytes into the image.
ORG  sets the current target DP to a new value.

The T prefix signifies "target", thus THERE should be read as
"target HERE".  Normally I hate T-prefix names; these will be
imported into the TARGET vocabulary without the T prefix.

\\These words embody the target threading model and alignment.
TALIGN  aligns the target dictionary ptr; TALIGNED aligns any
  target address.  (These are no-ops on the byte-aligned 6809).

*Note* Chromium 2 assumes all word references in the target use
Absolute addresses, with an optional prefix:   STC  DTC  ITC
,!XT  begins a Forth thread reference.         JSR  ---  ---
,!CF  begins the code field of a Forth word.   ---  JSR  ---
,CODE  lays the code field of a CODE word.     ---  ---  Here+2

T!CF  changes the code field of the target Forth word at 'tadr'.
   This should also store the JSR/JMP instruction, if used.
T>BODY  converts a target cfa (technically, xt) to a target pfa.

\\These words define the target header structure.  (CamelForth)
   These are model-specific but usually not CPU-specific.
   If n-way hashed lists and peculiar vocabulary schemes are
   used in the target, this screen can become much more complex!
HEADS  if 0, will prevent headers from being generated.
TLATEST  holds the target nfa of the last word defined in the
   target.  Note that the linking scheme is model-specific.
(THEADER)  builds a header in the target, except for the code
   field, linking it into the image dictionary.  It parses a
   WORD non-destructively.  Header layout is model-specific.
(TIMMEDIATE)  makes the LATEST target word "immediate".
   This is also model-specific.

Once this screen is loaded, we have all the support words
needed for the assembler.

\\Generic definition for the 'mirror' word.  These words reside in
  the TARGET vocabulary and are executed during metacompilation.
TARG-VALUE  is usually the address of the corresponding target
   word.  It may also be a numeric value (for an EQUATE).
HOST-COMP  is what the host executes in compile state.
HOST-EXEC  is what the host executes in interpret state.
FWD-LIST  is the head of this word's forward-reference list.
MIRR-LINK  maintains a linked list of all mirror words.
TSTATE  is -1 when metacompiling, 0 when metainterpreting.
MLATEST  is the mirror word most recently CREATEd.
'MIRROR  holds the pfa of the last-defined mirror word.
MIRROR  creates a mirror word in the host, initializing its
   forward-ref list to empty.  The action of a mirror word is
   to execute either the HOST-COMP or HOST-EXEC vector, with
   the address of this data structure in the variable 'MA.
MA  returns the data address of the mirror word being exec'd.
\\Forward references are accumulated in a linked list in the
HOST's memory space.  Each element in the list contains a link
(0 for end of list), followed by the address in the target which
has to be "patched" later.  The mirror word holds the list head.

!REF  uses the mirror's PUT-CODE to patch a reference.
,REF  uses the mirror's PUT-CODE to append a reference.
+FWDREF  is the basic compile action for a forward-reference.
  It adds the target HERE to the list of patch addresses.
  'MA' is the pfa of the mirror word ('mirror address').
(RESOLVE)  given 'adrs' within the target, and pfa of a mirror
  word 'ma', patches all forward references, then zaps the fwd
  list head.  Sets value; does not change comp/exec actions.
  NB: all references assumed to be one-cell absolute addresses!

\\Each target word has associated with it (in the "mirror" word)
a "compiling" action and an "executing" action.  For most words
compiling is "compile my address" and executing is "error".
.UNDEF  is the execute action for a forward-referenced word.
  Since the value is not yet known, print an error message.
PRESUME name   predefines a forward reference to a target word,
  so it can be used within the metacompiler -- e.g. LIT, EXIT.
  PRESUME-CF defines a code field, PRESUME-ADRS a simple adrs.
EMULATES name   changes the action of the MLATEST mirror to the
   host word 'name'.  Works w/PRESUME, eg, PRESUME @ EMULATES T@
EMULATE: ..code.. ;EMULATE   changes the action of the MLATEST
   mirror word to the given Forth code.
TIMMEDIATE  makes the latest target word immediate, by setting
   (TIMMEDIATE) and MIMMEDIATE.
T' name   gets the cfa (xt) of a target word, by fetching it
  from the mirror word 'name'.
\\?MIRROR  must recognize three cases.  (Note: may change 'MA)
1. Target word has been forward-referenced (or PRESUMEd):
   Resolve value and set compile action in existing mirror word.
   Leave execute action alone (may have been pre-defined).
2. Mirror word exists but is NOT fwd ref (duplicate definition):
   Leave old mirror word alone; build new mirror word.
3. Mirror does not exist: build new mirror word.

RESOLVES  is a convenience, e.g., HERE RESOLVES DOCREATE.
  The word being resolved had better be a mirror word!

\\This is the metacompiler's CREATE.
DOCREATE  is a mirror word for the target's DOCREATE code.  We
  define this now so the metacompiler can forward-reference it.

TCREATE  builds an (optional) header in the target, then builds
  (or resolves) a mirror word whose value=target cfa, and whose
  action is to compile that target word into a target thread.
  Finally it builds the default target code field (DOCREATE).
CODE  is similar, but does not compile a reference to DOCREATE,
  and adds the meta assembler to the search order.
END-CODE  drops the meta assembler from the search order.

\\These words handle EQUATEs and compile literal values.
LIT  is a mirror word for the target's LIT.  We define this now
   so that TLITERAL can forward-reference it.
TLITERAL  compiles a single cell literal into the image.
   NOTE: this implementation won't handle fwd STC references!
@EQU  is the execute action for an Equate (fetch TARG-VALUE).
,EQU  is the compile action for an Equate (append a TLITERAL).
EQU  defines an equate with value n.

DUAL  when true, causes mirror words to be compiled in the host
  while target words are compiled in the target.  This builds an
  analog ("dual") of the target colon definition in the host.

\\T[  sets metainterpreting state.
T]  sets metacompiling state and enters the meta compiling loop.
   Words from the input stream are searched (in the TARGET
   vocabulary) and executed.  The execution action of a defined
   target word is to compile itself.  Other words, such as
   compiler directives, perform their programmed action.
   This loop will end upon ; or [.  F83 note: colon definitions
   cannot cross screen boundaries.

Note that while metacompiling, all words are actually executed!
The metacompiler works like an assembler in this regard.

\\Target interpretation is only valid when the target CPU is
actually connected "live" to the host CPU.

D>T  pushes a single or double number on the target's stack.
   This uses the talker word >T , put a number on target stack.

TINTERPRET and TQUIT  are a modified interpreter loop to run
   Forth words remotely on the target machine.  Numbers are
   pushed on the target stack.  Target words are automatically
   INVOKEd when their mirror words are executed on the host.

HOT  starts the target interpreter.  This will run until COOL
   is executed, or UNTIL ANY ERROR CONDITION IS ENCOUNTERED.
   (F83 restarts its own QUIT after any error!)

\\DOCOLON  is a mirror word for the target's DOCOLON code.
EXIT  is a mirror word for the target's EXIT.  We define these
  now so that T: and T; can forward-reference them.

T:  starts a target colon definition, building the target header
  and code field, and entering metacompile state.  If DUAL is
  true, builds the code field for the host analog, and stores
  this as the execute action of the mirror word.

T;  ends a target colon definition and leaves metacompile state.
  If DUAL, ends the analog definition in the host.
  NOTE: this version does not correctly handle STC.

Normally I hate "T-prefix" names, but when these are imported
into the TARGET vocabulary, they'll be known as : and ;.

\\These words allow the host machine to correctly build "defining"
and "defined" words in the target.  The target's DODOES code and
(DOES>) word need to be forward-referenced.
TARG-DOES  holds the address of the target's DOES> code.
TPFA  as a mirror action, returns the pfa of the target word.
(MDOES>)  when executed by the host machine, changes the execute
   action of the most recently defined target word, in the image
   AND in the host's mirror word.  The host's "execute" vector
   is set to the address immediately following the (DOES>) .
MDOES>  a) adds the target DOES address and meta (MDOES>) to the
   host's colon definition; b) builds a headerless colon def
   for the host's DOES> action, and starts it with TPFA.
TDOES>  a) compiles target (DOES>) and JSR DODOES, and saves the
   target DOES address; b) performs MDOES> if dual is enabled.
You really need to see a picture to understand this.
Refer also to the target's source code for DOES> and ;CODE .
\\These are words which must be recognized while metacompiling
(i.e., while in the TARGET vocabulary), but which have no
equivalent in the target Forth.

AKA xxx xxx  simply makes a synonym in the TARGET vocabulary.

TARGET  is the root of the "mirrored" dictionary tree, which
   will be built in the host.  This tree will hold all of the
   "mirror" words and will exactly duplicate the search order
   of the dictionary being built in the image. (scr # 2)

Once the TARGET vocabulary is sealed, the only exits are
META  to select the META vocabulary
ASSEMBLER  to select the META ASSEMBLER vocabulary
CODE  to create a code header and select META ASSEMBLER

\\These are words which will be defined in the target Forth, but
which also have equivalent actions in the host.  Each is
predefined here as a forward reference with a specified execute
action (PRESUME targ-name EMULATES host-word).  When the target
definition for the word is given (CODE, colon, or CREATE),
the execute action will remain unchanged AS LONG AS "DUAL" IS
SWITCHED OFF!

These are the words which can be used in "dual" definitions.
(Note that AKA words cannot be used in duals!)

\ \\\ \\Normally, EVERYONE is 0 and .MIRRORS - executed at the end of
the build process - only reports unresolved words (which
represent errors).

With EVERYONE set to 0, .MIRRORS reports all of the mirror
words. This can be useful for debug of the metacompiler itself
or of errors arising from the addition of new code to CamelForth
itself.

\ \\\These words implement Charles Curley's DUMP as part of the
image compiler.  Use META DUMP to look at the target image.
Use NATIVE DUMP for the "original" dump of F83's memory.

\\Usage:  HOST  address length HEXFILE filename.ext

\ \\\These words store the target image in a range of Forth screens.
   LITTLE ENDIAN: Most Significant byte is at HIGHER address,
   e.g. 8086 or Z80

Only T@ and T! are changed from the previous screen.

\\These words store the target image directly to target memory,
working through a resident "talker" program running on the
target CPU.  The talker provides the following functions:
   XADR  set address in target
   X!+   store byte and advance address
   X@+   fetch byte and advance address
   GO    jump to machine language subroutine at given address
   AWAIT wait for subroutine to complete (talker re-entered)

INVOKE  executes the target Forth word at address a.
   This issues a GO command to the talker, then an AWAIT to
   wait for the talker program to regain control.

\\This is the same as the previous screen except for the byte
order of T@ and T!.

\\This is the start of the 6809 assembler, which runs on the host
in order to build CODE words in the target image.

\ \\\ \\\ \\\ \\The load screen for the Multicomp target. This target supports
SDcard access so additional words are provided for low-level
sector read-write and for high-level BLOCK access.

Multicomp has 8KByte ROM at $E000-FFFF which is overlayed by
an I/O region at $FFD0-$FFDF. Therefore, those 16 bytes of the
ROM cannot be accessed: the image must not grow big enough to
reach that region.
The mininal Multicomp has 2KBytes of RAM starting at $0. This
image is built for that setup: the dictionary grows up from $0
and the user space and stacks grow down from $800.
If more RAM is present go HEX 800 ALLOT to jump over the top
and start the dictionary above the stacks.
Screens 46-48 are  Multicomp versions of screens 61-63.
Screen     49 is a Multicomp versions of screen  119.
Screens 50-59 and 121-127 provide Multicomp-specific extras.
\\Lines 1-7 describe the memory map of the Multicomp target.

\\The Multicomp equivalent of scr62 - empty because the Multicomp
I/O does not require any initialisation.

\\Simple I/O for the more-or-less 6850-compatible real/virtual
UART on Multicomp.

\\The Multicomp hardware requires no initialisation after reset.
It does contain a memory mapper, but this is disabled at reset
and initialised when needed.

\\The SDcard is structured as 512-byte blocks with a linear 24-bit
address (LBA=linear block address)
SDLBA2 - set the upper 8 bits of the address for the next access
SDLBA10 - set the lower 16-bits
*NOTE* the words that access sequential blocks assume that
no access will cross the bit15-bit16 boundary!!

SDIDLE - make sure the HW is ready to receive a command

SDRD - read 1 512byte block from SDcard to memory.
SDWR - write 512bytes from memory to 1 SDcard block.

To do low-level SD read/write use SDLBA2, SDBLA10, SDRD, SDWR
from here; you should never need to use SDIDLE directly.

\\SDRDn   - read multiple successive blocks from SD and load to
          successive 512byte regions of memory.

SDRD256 - to support ports of old OS whcih often used 256byte
          sectors on floppy disks, the simplest thing to do is
          to map each 256byte sector to a 512byte SDblock. It's
          wasteful, but the space available on the SDcard
          compared with a floppy means that the waste is not a
          problem. SDRD256 reads a 512byte block, copies the
          first 256bytes to memory and discards the remainder.

SDRD256n -read multiple successive blocks from SD and load the
          first 256bytes of each block to successive 256byte
          regions of memory.

\\SDWRn   - write successive 512byte regions of memory to
          successive blocks on the SD

SDWRZ   - write 0 to each of the 512bytes of an SD block

There is no SDWR256 because CamelForth is only used to boot
those old floppy images. Once the target OS is loaded, it uses
its own disk drivers for read/write (and those drivers implement
the equivalent of SDRD256 and SDWR256).

\\                                                      04may15nac

We're executing from ROM but we want to disable the ROM so that
the underlying RAM appears. To do this, we need to be NOT
executing from ROM, so we load a fragment of code to RAM,
including the ROM disable, and branch to it and thence to the
new ROM replacement. By inspection, both CUBIX and BASIC rely
on the reset value of DP so initialise that, too.

\\The Multicomp memory mapper breaks the address space into 8,
8Kbyte blocks. It is disabled at reset. Before enabling it,
map each logical block to the physical block at the same
address - what I call a "1-1 mapping"

CamelForth is in ROM at address $E000. The ROM can be disabled,
allowing RAM to be mapped there (but the RAM cannot be accessed
while the ROM is enabled). Therefore, to execute (eg) MS BASIC
from $E000 we have to do a little dance:
Map a RAM block from address $E000 to address $C000 (EFTOCD)
Load the image into RAM at address $C000
Map the RAM back to address $E000 (CDTOEF)
Create a blob of code in RAM (eg at address $1000). Pass control
(ie jump) from ROM to that blob of code
From the blob of code, disable the ROM so that the RAM appears
From the blob of code, (write-protect then) jump to RAM
\\CUBIX loads the 2kbyte ROM image from SD, puts it into RAM
pages it in and jumps to it. Cannot protect it because the
protect resolution is 8KByte and, by deduction, the boot process
expects RAM in at least some of that region. Anyway, it does not
need write-protect. The ROM image includes the vectors at the
end (eg, for the SWIs) and these vector to locations DExx

BASIC loads the 8kbyte BASIC ROM image from SD, puts it in RAM,
pages it in, protects it and then jumps into it. From working
with the ROM image in the emulator I already know that its
memory sizing algorithm does not corrupt memory directly but
will set RAMTOP to the top of the writeable space and so the
BASIC ROM must be write protected to avoid being seen as RAM and
so overwritten.
FORTH loads like BASIC.
BUGGY runs from RAM - so no write-protect is needed.
\\
FLEX boots from a virtual floppy image and needs RAM from $C100
In theory, the ROM doesn't need to be mapped out.

NITROS9 needs all the RAM it can get. Also, the boot image
expects some vectors to be set up in low-order memory. Maybe
it would have been better to do that set-up in the boot image
but at the time it seemed cleaner and easier to do it here.

\ \\\ \\\\D7       D0                                           01apr15nac
+----------+                                 CamelForth
|  link    | <- lfa (link field address)     Header
+          +                                 Format
|          |
+----------+
|S|P| len  | <- nfa (name field address)
+-+-+------+
|          |    P is the "precedence" bit - set for IMMEDIATE
+          +    words.
|          |    S is the "smudge" bit - set during word defn.
+  name    +    to prevent FIND from locating the definition
|          |    that is in progress; this allows a word to be
+-..-..-..-+    redefined (use RECURSE for deliberate recursion)
|          | <- cfa (code field address)

\\                                                      13mar15nac
The link field points to the NFA of the previous definition
The link field is 0000 for the oldest definition
The variable LATEST points to the NFA of the youngest definition

CamelForth used to use a separate byte between link and len to
store P and S bits - this design decision costs 1 byte per
definition. The author wrote that he chose to do this because
it allowed the use of "normal" string operators (ie that work
on counted strings) like S= in the definition of FIND and TYPE
in the definition of WORDS.

In this version of CamelForth, the P and S bits are combined
with the length field (just as Fig-Forth, Pygmy and F83 all do)

\ \\\ \\\ \\\\                                                      01apr15nac

The core of FIND is a string match. The incoming string is
a normal counted string. The string to search against in the
dictionary might have bit 6 (Immediate) set in its length byte.
Therefore, we fetch that length byte, mask out the Immediate
bit and compare against the length of the incoming string. Only
on length match do we do a string compare (with S=) of the rest
of the string. On mismatch we move to the next entry in the
dictionary.. until we succeed or reach the end.
\\
\ \\\                                                      13mar15 na
INTERPRET is an almost verbatim translation of the algorithm
given in section 3.4 of the ANS document. It parses one
space-delimited string from the input and tries to FIND the
Forth word of that name. If the word is found, it will either be
executed (if it is an IMMEDIATE word, or if in the "interpret"
state, STATE=0) or compiled into the dictionary (if in the
"compile" state, STATE=1). If not a Forth word and not a valid
number, the string is typed, an error message is displayed
and the intepreter ABORTs. This process is repeated, string by
string, until the end of the input line is reached.

\\                                                      28apr15nac
COLD initializes the user variables from a startup table and
then does ABORT

ABORT resets the parameter stack pointer and does QUIT

QUIT resets the return stack pointer, loop stack pointer and
interpret state (STATE) and then begins to interpret Forth
commands. The name is apt, because QUIT can be used to abort an
application and get back to the "top-level" of Forth. Unlike
ABORT, QUIT will leave the parameter stack alone. QUIT is an
infinite loop that will ACCEPT a line from the keyboard and then
INTERPRET it as Forth commands. When not compiling, QUIT will
prompt "ok" after each line.

\ \\\ \\\\PASS (start-addr end-addr --) will do a simple memory (RAM) test
with incrementing and decrementing address sequences.

PASSES (start-addr end-addr n --) will do n passes of the memory
test.

\\Assuming an 8KByte image, the following 8 screens are used to
build up the binary image as the metacompiler runs. Code
generation starts at the first byte of scr 248 - representing
address $D000 in the target - and continues linearly apart from
the vectors at address $FFF0-$FFFF which are written to the last
16 bytes of scr255.

Any space between the end of the dictionary and the start of the
vectors is "spare" (unused ROM space) except for Multicomp
where the dictionary must not extend into the region $FFD0-$FFDF
(marked "IO" below)

ie, the final line of scr255 is laid out like this:

--DICT/SPARE---><---- IO ------><--- UNUSED ---><--- VECTORS -->

