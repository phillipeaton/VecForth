\ Vectrex Forth Application

HEX

INCLUDE VECTREX_EQU.FS \ Vectrex BIOS functions and variables

HERE EQU HELLO-WORLD-STRING
S" HELLO WORLD" 80 C,

DECIMAL

HERE EQU DOT-LIST
  30 C, -70 C, \ seven dots, relative
 -40 C,  10 C, \ position, Y, X
   0 C,  30 C,
  40 C,  10 C,
  10 C,  30 C,
   5 C,  30 C,
 -10 C,  40 C,

HEX

CODE -ROT     \ x1 x2 x3 -- x3 x1 x2   per stack diagram
    S 2 , LDX,   S 2 , STD,   S 0, LDD,   S 0, STX, NEXT ;C

\ CODE DPR!   B DPR TFR,   6 # ( D) PULS, NEXT ;C \ DPR -- ;

CODE _Random \ -- n ; n is a random number between 0 and 255
             \      ; Just a test, quicker to copy function from Vectrex BIOS
     6 # ( D) PSHS,   Random JSR,   CLRB,   A B EXG,   NEXT ;C

CODE _Wait_Recal \ -- ;
    1E # ( X DP D) PSHS,   Wait_Recal JSR,   1E # ( X DP D) PULS,   NEXT ;C
\
CODE _Intensity_5F \ -- ; SHOULD BE SETTING DP-$D0!!!!!!!!!!
     8 # ( DP) PSHU,           \ -- ; Save DP
     D0 # LDX,   X DPR TFR,    \ -- ; DP to D0
     6 # ( D) PSHS,   Intensity_5F JSR,   6 # ( D) PULS,
     8 # ( DP) PULU,           \ -- ; Restore DP
    NEXT ;C

CODE _Intensity_7F \ -- ; SHOULD BE SETTING DP-$D0!!!!!!!!!!
     8 # ( DP) PSHU,           \ -- ; Save DP
     D0 # LDX,   X DPR TFR,    \ -- ; DP to D0
     6 # ( D) PSHS,   Intensity_7F JSR,   6 # ( D) PULS,
     8 # ( DP) PULU,           \ -- ; Restore DP
    NEXT ;C

CODE _Intensity_a \ n -- ;
    8 # ( DP) PSHU,           \ -- TOS ; Save DP
    D0 # LDX,   X DPR TFR,    \ -- TOS ; DP to D0
    Intensity_a JSR,
    6 # ( D) PULS,            \ -- ; Drop TOS
    8 # ( DP) PULU,           \ -- ; Restore DP
    NEXT ;C

CODE _Dot_here \ -- ;
    6 # ( D) PSHS,            \ -- TOS ;
    8 # ( DP) PSHU,           \ -- TOS ; Save DP
    D0 # LDX,   X DPR TFR,    \ -- TOS ; DP to D0
    Dot_here JSR,             \ Call Vectrex BIOS subroutine
    8 # ( DP) PULU,           \ -- TOS ; Restore DP
    6 # ( D) PULS,            \ -- ; Drop TOS
    NEXT ;C

CODE _Dot_List \ dl_addr -- ;
    8 # ( DP) PSHU,           \ -- dl_addr ; Save DP
    D0 # LDX,   X DPR TFR,    \ -- dl_addr ; DP to D0
    D X TFR,                  \ -- dl_addr ; Load X with address of dot list
    Dot_here JSR,             \ Call Vectrex BIOS subroutine
    6 # ( D) PULS,            \ -- ; Drop TOS
    8 # ( DP) PULU,           \ -- ; Restore DP
    NEXT ;C

CODE _Print_Str_d \ x y c-addr -- ; Print single string to screen
    8 # ( DP) PSHU,           \ -- x y c-addr ; Save DP
    D0 # LDX,   X DPR TFR,    \ -- x y c-addr ; DP to D0
    D U EXG,                  \ -- x y U-addr ; String addr to U, save U to D
    S 2 , LDX,   S 2 , STD,   \               ; Stack -ROT (2 lines)
    S 0, LDD,   S 0, STX,     \ -- U-addr x y ;
    A B EXG,   S ,++ ADDD,    \ -- U-addr yx  ; Combine x and y
    Print_Str_d JSR,          \ Call Vectrex BIOS subroutine
    6 # ( D) PULS,            \ -- U-addr     ; Drop TOS
    D U TFR,   6 # ( D) PULS, \ -- ; Restore U, drop TOS
    8 # ( DP) PULU,           \ -- ; Restore DP
    NEXT ;C

CODE _Moveto_d \ x y -- ;
    8 # ( DP) PSHU,           \ -- x y c-addr ; Save DP
    D0 # LDX,   X DPR TFR,    \ -- x y c-addr ; DP to D0
    A B EXG,   S ,++ ADDD,    \ -- yx  ; Combine x and y
    Moveto_d JSR,             \ Call Vectrex BIOS subroutine
    6 # ( D) PULS,            \ -- ; Drop TOS
    8 # ( DP) PULU,           \ -- ; Restore DP
    NEXT ;C

CODE _Draw_Line_d \ x y ;
    8 # ( DP) PSHU,           \ -- x y c-addr ; Save DP
    D0 # LDX,   X DPR TFR,    \ -- x y c-addr ; DP to D0
    A B EXG,   S ,++ ADDD,    \ -- yx  ; Combine x and y
    Draw_Line_d JSR,          \ Call Vectrex BIOS subroutine
    6 # ( D) PULS,            \ -- ; Drop TOS
    8 # ( DP) PULU,           \ -- ; Restore DP
    NEXT ;C


: DL \ -- ;
  0
  BEGIN
    1 + DUP U.          \ Send a frame counter to display on the terminal
    _Wait_Recal         \
    _Intensity_7F       \
    FF VIA_t1_cnt_lo C! \ set scaling factor
    -3F -3F _Moveto_d   \ move beam to origin
    0 Vec_Misc_Count C! \ must be set to 0 to draw one vector
    0 7F _Draw_Line_d   \ draw the line
    0 Vec_Misc_Count C! \ must be set to 0 to draw one vector
    7F 0 _Draw_Line_d   \ draw the line
    0 Vec_Misc_Count C! \ must be set to 0 to draw one vector
    0 80 _Draw_Line_d   \ draw the line
    0 Vec_Misc_Count C! \ must be set to 0 to draw one vector
    80 0 _Draw_Line_d   \ draw the line
    KEY?                \ terminal key press exits loop
  UNTIL
  DROP
;


: TURNKEY \ -- ;
    BEGIN

\       _Dot_here
\
\       32 VIA_t1_cnt_lo C! \ #50 scaling
\       6 Vec_Misc_Count C! \ # dots-1 to counter
\       DOT_LIST _Dot_List  \ Display the dots

        _Wait_Recal
        80 VIA_t1_cnt_lo C!
        0 0 _Moveto_d
        _Intensity_5F
        0 Vec_Misc_Count C!
        50 50 _Draw_Line_d


\      5F _Intensity_a
\       _Wait_Recal
\       _Intensity_5F
\
\       31 -30
\       DO
\         51 -50
\         DO J 40 - I HELLO-WORLD-STRING 3 + _Print_Str_d
\         LOOP 20
\       +LOOP
    AGAIN ;


: HELLO \ -- ;
    BEGIN
      _Wait_Recal
      _Intensity_5F
      31 -30
      DO ." Hello World! "
        I 7 MOD 10 * v4eLEDReg C!
        51 -50
        DO J 40 - I HELLO-WORLD-STRING 3 + _Print_Str_d
        LOOP 20
      +LOOP
      CR ." Escape to end, any other to continue" KEY 1B =
    UNTIL ;

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
