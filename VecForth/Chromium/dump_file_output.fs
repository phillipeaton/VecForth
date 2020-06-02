HEX

\ Block 24 -----------------------------------------------------
\ ( Image dump)   HEX                           ( 31 jan 93 bjr)
\
\ These words implement Charles Curley's DUMP as part of the
\ image compiler.  Use META DUMP to look at the target image.
\ Use NATIVE DUMP for the "original" dump of F83's memory.
\
: (DUMP)  \ addr ct ---  | dump as pointed to by reloc
  SPACE  OVER + SWAP DO  I TC@ 3 .R  LOOP ;

: LASCI   \ addr ct ---  | asci type as pointed to by reloc
  SPACE  OVER + SWAP DO  I TC@  7F AND  DUP
    20 7F WITHIN 0= IF DROP 2E ( .) THEN  EMIT  LOOP ;

: HEAD  \ addr --- | header for dump display
  10 0 DO I OVER + 0F AND 3 .R LOOP DROP ;

  \ N. B: Not responsible for negative counts! -- the MGT.
: DUMP   \  addr ct ---  | dump as pointed to by reloc
  OVER CR 6 SPACES HEAD  BEGIN  DUP  WHILE  CR OVER 5 U.R
     2DUP 10 MIN >R  R@ 2DUP (DUMP)  36 #OUT @ - SPACES LASCI
     R@ R> NEGATE D+  KEY? IF DROP 0 THEN  REPEAT 2DROP ;

\ Block 25 -----------------------------------------------------
\ ( Intel hex files, ANS FILE words)  HEX       ( 07 feb 93 bjr)
\
VARIABLE IHANDLE   IHANDLE OFF

\ CREATE HEXTBL  30 C, 31 C, 32 C, 33 C, 34 C, 35 C, 36 C, 37 C,
\                38 C, 39 C, 41 C, 42 C, 43 C, 44 C, 45 C, 46 C,

\ Block 26 -----------------------------------------------------
\ ( Intel hex files, ANS FILE words)  HEX       ( 07 feb 93 bjr)
\
\ : ,H  ( n - c)  HEXTBL + C@ C, ;
\ : ,HH ( u c - u)  DUP  DUP 0F0 AND 10 / ,H  0F AND ,H  + ;
\
\ : WREC ( type a n)   HERE >R  SWAP >R
\    3A C,  0 OVER ,HH   R@ >< ,HH  R@ ,HH  ROT ,HH  R> ROT
\    ?DUP IF  OVER + SWAP DO  I TC@ ,HH  LOOP
\       ELSE  DROP  THEN
\    NEGATE DUP ,HH DROP   0D C, 0A C,
\    R> HERE OVER DP ! OVER -
\    IHANDLE @ .s WRITE-FILE 0<> ABORT" Write err" ;
\
\ \ Usage:  HOST  address length HEXFILE filename.ext
\ : HEXFILE ( a n)   BL WORD COUNT W/O CREATE-FILE
\    0<> ABORT" Open err"  IHANDLE !
\    OVER + SWAP DO  0 I 10 WREC  10 +LOOP
\    1 0 0 WREC  IHANDLE @ CLOSE-FILE DROP ;

\ Block --------------------------------------------------------
\ ( Binary files words)  HEX                    ( 26 dec 18 pje)
\
: BINFILE ( a n -- )
   BL WORD COUNT W/O CREATE-FILE
   0<> ABORT" Open err"  IHANDLE !
   OVER + SWAP DO
      I TC@ IHANDLE @ EMIT-FILE 0<> ABORT" Write err"
   LOOP
   IHANDLE @ CLOSE-FILE DROP ;
