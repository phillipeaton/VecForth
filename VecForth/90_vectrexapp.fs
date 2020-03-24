\ Vectrex Forth Application

HEX

F192 EQU Wait_Recal
F2A5 EQU Intensity_5F
F37A EQU Print_Str_d \ A, B, U

HERE EQU HELLO-WORLD-STRING
S" HELLO WORLD" 80 C,

\ CODE DPR!   B DPR TFR,   6 # ( D) PULS, NEXT ;C \ DPR -- ;

CODE _Wait_Recal    Wait_Recal     JSR, NEXT ;C
CODE _Intensity_5F  Intensity_5F   JSR, NEXT ;C
CODE _Print_Str_d   \ x y c-addr -- ; Print single string to screen
  3 #   ADDD,                   \ Skip Forth string header
  D U   EXG,                    \ -- x y U-addr ; String addr to U, save U to D
  S 2 , LDX,   S 2 , STD,       \ -- U-addr x y ; Stack -ROT
  S 0,  LDD,   S 0,  STX,       \ "
  A B   EXG,   S ,++ ADDD,      \ -- U-addr yx ; Combine x and y
  Print_Str_d JSR,              \ Call Vectrex BIOS subroutine
  6 # ( D) PULS,                \ -- U-addr ; Drop TOS
  D U   TFR,   6 # ( D) PULS,   \ -- ; Restore U, drop TOS
NEXT ;C

: HELLO \ -- ;
    BEGIN
      _Wait_Recal
      _Intensity_5F
      31 -30
      DO ." Hello World! "
        I 7 MOD 10 * v4eLEDReg C!
        51 -50
        DO J 40 - I HELLO-WORLD-STRING _Print_Str_d
        LOOP 20
      +LOOP
." 1"
      KEY 89 <>
." 2"
    UNTIL
.S
;

DECIMAL

: RAND ( X -- Y ) RND @ 31421 * 6927 + DUP RND ! UM* SWAP DROP ;

: MY-GO ( -- )
    BEGIN 10 RAND 1+ MT +!
      ." MY SCORE:" MT @ DUP . CR 21 >
      IF ." I'VE BUST, YOU WIN" 1
      ELSE MT @ YT @ SWAP <
        IF ." I WIN" 1
        ELSE 0
        THEN
      THEN
    UNTIL  ;

: PONT ( -- )
    CR ." **PHIL'S PONTOON**" CR
    BEGIN 0 YT ! 0 MT !
      BEGIN 10 RAND 1+ YT +!
        ." YOUR SCORE :" YT @ DUP . 22 <
        IF ." TWIST? (Y/N)" CR KEY 89 <> DUP
          IF MY-GO
          THEN
        ELSE CR ." YOU'VE BUST, I WIN" 14 YT ! 1
        THEN
      UNTIL
      CR ." AGAIN ?" CR KEY 89 <>
    UNTIL ;
