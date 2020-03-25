HEX

\ Block 03 -----------------------------------------------------
\   Image to disk: big-endian, 16-bit host           17apr95 bjr
\
\   Target address unit = Host address unit; cell size=2
\
\ These words store the target image in a range of Forth screens.
\    BIG ENDIAN: Most Significant byte is at LOWER adrs, e.g.6809
\
\ BASE-SCREEN  is the starting screen # of this range.  There is
\    no ending screen #; up to 64 screens (64K) can be used.
\    You may change this to suit your disk layout.
\
\ ><  swaps the hi and lo bytes of the top stack item. <16 BIT!>
\ VIRTUAL  converts a target image address to an address within
\    a block buffer.  Adrs 0000 is the first byte of BASE-SCREEN.
\ TC@ TC!  fetch and store bytes in the target image.
\ T@ T!  fetch and store 16-bit cells in the target image.  Since
\    a cell may cross a block boundary, these must be done with
\    byte operations.
\ >TCMOVE  copies a string from the host memory to the image.
\ TEXECUTE  executes the target word at address a.  Since we are
\    compiling to disk, this simply prints an error message.
\ >T T>  move data to and from the target stack (future use).
\
DECIMAL 192 CONSTANT BASE-SCREEN   ( last 64 screens)  HEX
: >< ( n - n)   0 100 UM/MOD  SWAP 100 * + ;
: VIRTUAL ( a - a)   0 400 UM/MOD BASE-SCREEN + BLOCK + ;
: TC@ ( a - c)  VIRTUAL C@ ;
: TC! ( c a)    VIRTUAL C! UPDATE ;
: T@  ( a - n)  DUP TC@ 100 * ( hi)  SWAP 1+ TC@ ( lo)  + ;
: T!  ( n a)   >R 0 100 UM/MOD  R@ TC! ( hi)  R> 1+ TC! ( lo) ;
: >TCMOVE ( s d n) OVER + SWAP DO  DUP C@ I TC!  1+ LOOP DROP ;
: OOPS   DECIMAL BLK @ . >IN @ 40 /MOD . . ABORT ;
: TEXECUTE ( a)   U. ." Attempt to execute Target word " OOPS ;
: >T  ( n)   ." Attempt to access Target stack " OOPS ;
: T>  ( - n )   >T ;

\ Block 27 -----------------------------------------------------
\   Image to disk: little-endian, 16-bit host        31jan93 bjr
\
\ These words store the target image in a range of Forth screens.
\  LITTLE ENDIAN: Most Significant byte is at HIGHER address,
\  e.g. 8086 or Z80
\
\ Only T@ and T! are changed from the previous screen.
\
\ These words store the target image directly to target memory,
\ working through a resident "talker" program running on the
\ target CPU.  The talker provides the following functions:
\    XADR  set address in target
\    X!+   store byte and advance address
\    X@+   fetch byte and advance address
\    GO    jump to machine language subroutine at given address
\    AWAIT wait for subroutine to complete (talker re-entered)
\
\ INVOKE  executes the target Forth word at address a.
\    This issues a GO command to the talker, then an AWAIT to
\    wait for the talker program to regain control.
\
\ DECIMAL 192 CONSTANT BASE-SCREEN   ( last 64 screens)
\ HEX
\ : >< ( n - n)   0 100 UM/MOD  SWAP 100 * + ;
\ : VIRTUAL ( a - a)   0 400 UM/MOD BASE-SCREEN + BLOCK + ;
\ : TC@ ( a - b)  VIRTUAL C@ ;
\ : TC! ( b a)    VIRTUAL C! UPDATE ;
\ : T@  ( a - n)  DUP TC@ ( lo)  SWAP 1+ TC@ 100 * ( hi)  + ;
\ : T!  ( n a)   >R 0 100 Un27M/MOD  R@ 1+ TC! ( hi)  R> TC! ( lo) ;
\ : >TCMOVE ( s d n) OVER + SWAP DO  DUP C@ I TC!  1+ LOOP DROP ;
\ : INVOKE  ( a)   U. 1 ABORT" Attempt to execute Target word!" ;
\
\ --------------------------------------------------------------
\   Image to target machine: big-endian, 16 bit      31jan93 bjr
\
\ HEX
\ : >< ( n - n)   0 100 UM/MOD  SWAP 100 * + ;
\ : TC@ ( a - b)   XADR X@+ ;
\ : TC! ( b a)     XADR X!+ ;
\ : T@  ( a - n)   XADR X@+ 100 * ( hi)  X@+ ( lo) + ;
\ : T!  ( n a)     XADR DUP 0 100 UM/MOD  X!+ ( hi) X!+ ( lo) ;
\ : >TCMOVE ( s d n)  SWAP XADR  OVER + SWAP DO  I C@ X!+  LOOP ;
\ : INVOKE ( cfa)   GO AWAIT ;
\
\ --------------------------------------------------------------
\   Image to target machine: little-endian, 16 bit   31jan93 bjr
\
\ HEX
\ : >< ( n - n)   0 100 UM/MOD  SWAP 100 * + ;
\ : TC@ ( a - b)   XADR X@+ ;
\ : TC! ( b a)     XADR X!+ ;
\ : T@  ( a - n)   XADR X@+ ( lo)  X@+ 100 * ( hi) + ;
\ : T!  ( n a)     XADR DUP 0 100 UM/MOD  SWAP X!+ X!+ ( lo,hi) ;
\ : >TCMOVE ( s d n)  SWAP XADR  OVER + SWAP DO  I C@ X!+  LOOP ;
\ : INVOKE ( cfa)   GO AWAIT ;































