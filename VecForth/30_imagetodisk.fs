\ 3\   Image to disk: big-endian, 16-bit host           17apr95 bjr
\   Target address unit = Host address unit; cell size=2
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


\ \27\\   Image to disk: little-endian, 16-bit host        31jan93 bjr
\
\ DECIMAL 192 CONSTANT BASE-SCREEN   ( last 64 screens)
\ HEX
\ : >< ( n - n)   0 100 UM/MOD  SWAP 100 * + ;
\ : VIRTUAL ( a - a)   0 400 UM/MOD BASE-SCREEN + BLOCK + ;
\ : TC@ ( a - b)  VIRTUAL C@ ;
\ : TC! ( b a)    VIRTUAL C! UPDATE ;
\ : T@  ( a - n)  DUP TC@ ( lo)  SWAP 1+ TC@ 100 * ( hi)  + ;
\ : T!  ( n a)   >R 0 100 UM/MOD  R@ 1+ TC! ( hi)  R> TC! ( lo) ;
\ : >TCMOVE ( s d n) OVER + SWAP DO  DUP C@ I TC!  1+ LOOP DROP ;
\ : INVOKE  ( a)   U. 1 ABORT" Attempt to execute Target word!" ;
\
\ \ \\   Image to target machine: big-endian, 16 bit      31jan93 bjr
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
\ \ \\   Image to target machine: little-endian, 16 bit   31jan93 bjr
\
\ HEX
\ : >< ( n - n)   0 100 UM/MOD  SWAP 100 * + ;
\ : TC@ ( a - b)   XADR X@+ ;
\ : TC! ( b a)     XADR X!+ ;
\ : T@  ( a - n)   XADR X@+ ( lo)  X@+ 100 * ( hi) + ;
\ : T!  ( n a)     XADR DUP 0 100 UM/MOD  SWAP X!+ X!+ ( lo,hi) ;
\ : >TCMOVE ( s d n)  SWAP XADR  OVER + SWAP DO  I C@ X!+  LOOP ;
\ : INVOKE ( cfa)   GO AWAIT ;