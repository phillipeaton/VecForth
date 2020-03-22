\ Vectrex Forth Application

HEX

F192 EQU Wait_Recal
F2A5 EQU Intensity_5F
F37A EQU Print_Str_d \ A, B, U

HERE EQU HELLO-WORLD-STRING
S" HELLO=WORLD" 80 C,

CODE DPR!   B DPR TFR,   6 # ( D) PULS, NEXT ;C \ DPR -- ;

CODE _Wait_Recal    Wait_Recal     JSR, NEXT ;C
CODE _Intensity_5F  Intensity_5F   JSR, NEXT ;C
CODE _Print_Str_d   \ y x s-addr -- ; Print single string to screen
   3 # ADDD,                          \ Skip Forth string header
   D U EXG,                           \ y x U-addr ; String addr to U, save U to D
   S 0, LDX,   S 0, STD,   S 2 ,      \ U-addr y x ; Stack ROT
   LDD,   S 2 , STX,                  \ "
   A B EXG,   S ,++ ADDD,             \ -- U-addr xy ; Combine xy
   Print_Str_d JSR,   6 # ( D) PULS,  \ -- U-addr ; Print the string, drop TOS
   D U TFR,   6 # ( D) PULS,          \ -- ; Restore U, drop TOS
NEXT ;C

: COLD        \ -- ; cold start Forth system
  UINIT U0 #INIT CMOVE
    BEGIN
      _Wait_Recal
      _Intensity_5F
      -40 -40 HELLO-WORLD-STRING _Print_Str_d
       00 -40 HELLO-WORLD-STRING _Print_Str_d
       40 -40 HELLO-WORLD-STRING _Print_Str_d
    AGAIN
  ." VecForth v0.1 2018-03-18"                  CR
  ." based on 6809 CamelForth v1.1 2016-03-20"  CR
  ABORT ;
