\ Vectrex Forth Application

HEX

\ \\\\\\\\\\ NOT TESTED

\ CODE DPR!   B DPR TFR,   6 # ( D) PULS, NEXT ;C \ DPR -- ;

CODE _Intensity_a \ n -- ;
    8 # ( DP) PSHU,           \ -- TOS ; Save DP
    D0 # LDX,   X DPR TFR,    \ -- TOS ; DP to D0
    Intensity_a JSR,
    6 # ( D) PULS,            \ -- ; Drop TOS
    8 # ( DP) PULU,           \ -- ; Restore DP
    NEXT ;C

CODE _Dot_List \ dl_addr -- ;
    8 # ( DP) PSHU,           \ -- dl_addr ; Save DP
    D0 # LDX,   X DPR TFR,    \ -- dl_addr ; DP to D0
    D X TFR,                  \ -- dl_addr ; Load X with address of dot list
    Dot_here JSR,             \ Call Vectrex BIOS subroutine
    6 # ( D) PULS,            \ -- ; Drop TOS
    8 # ( DP) PULU,           \ -- ; Restore DP
    NEXT ;C

 CODE _Draw_VLc \ addr -- ;  addr = vector list in format: count, rel y, rel x, rel y, rel x, ...
                \            current scaling factor is used
    18 # ( X DP) PSHU,        \ -- addr ; Save X, DP
    D0 # LDX,   X DPR TFR,    \ -- addr ; DP to D0
    D X TFR,                  \ -- addr ; addr to X
    Draw_VLc JSR,            \ Call Vectrex BIOS subroutine
    6 # ( D) PULS,            \ -- addr ; Drop TOS
    18 # ( X DP) PULU,        \ -- ; Restore X, DP
    NEXT ;C


\ \\\\\\\\\ WORKING

CODE _Random \ -- n ; n is a random number between 0 and 255
     6 # ( D) PSHS,   Random JSR,   CLRB,   A B EXG,   NEXT ;C

CODE _Wait_Recal \ -- ;
    1E # ( X DP D) PSHS,   Wait_Recal JSR,   1E # ( X DP D) PULS,   NEXT ;C

CODE _Recalibrate \ -- ;
    1E # ( X DP D) PSHS,
    D0 # LDX,   X DPR TFR,    \ -- ; DP to D0
    Recalibrate JSR,
    1E # ( X DP D) PULS,
    NEXT ;C

CODE _Reset0Ref \ -- ; MOSTLY A COPY OF THE RECALIBRATE INTERFACE, SO SHOULD JUST WORK
    1E # ( X DP D) PSHS,      \ Save Registers
    D0 # LDX,   X DPR TFR,    \ DP to D0
    Reset0Ref JSR,            \ Call Vectrex BIOS subroutine
    1E # ( X DP D) PULS,      \ Restore Registers
    NEXT ;C

CODE _Reset_Pen \ -- ; MOSTLY A COPY OF THE RECALIBRATE INTERFACE, SO SHOULD JUST WORK
    1E # ( X DP D) PSHS,      \ Save Registers
    D0 # LDX,   X DPR TFR,    \ DP to D0
    Reset_Pen JSR,            \ Call Vectrex BIOS subroutine
    1E # ( X DP D) PULS,      \ Restore Registers
    NEXT ;C

CODE _Intensity_5F \ -- ;
     8 # ( DP) PSHU,           \ -- ; Save DP
     D0 # LDX,   X DPR TFR,    \ -- ; DP to D0
     6 # ( D) PSHS,   Intensity_5F JSR,   6 # ( D) PULS,
     8 # ( DP) PULU,           \ -- ; Restore DP
    NEXT ;C

CODE _Intensity_7F \ -- ;
     8 # ( DP) PSHU,           \ -- ; Save DP
     D0 # LDX,   X DPR TFR,    \ -- ; DP to D0
     6 # ( D) PSHS,   Intensity_7F JSR,   6 # ( D) PULS,
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
    8 # ( DP) PSHU,           \ -- x y ; Save DP
    D0 # LDX,   X DPR TFR,    \ -- x y ; DP to D0
    A B EXG,   S ,++ ADDD,    \ -- yx  ; Combine x and y
    Moveto_d JSR,             \ Call Vectrex BIOS subroutine
    6 # ( D) PULS,            \ -- ; Drop TOS
    8 # ( DP) PULU,           \ -- ; Restore DP
    NEXT ;C

CODE _Moveto_d_7F \ x y -- ; Identical to _Moveto_d except for BIOS subroutine name
    8 # ( DP) PSHU,           \ -- x y ; Save DP
    D0 # LDX,   X DPR TFR,    \ -- x y ; DP to D0
    A B EXG,   S ,++ ADDD,    \ -- yx  ; Combine x and y
    Moveto_d_7F JSR,          \ Call Vectrex BIOS subroutine
    6 # ( D) PULS,            \ -- ; Drop TOS
    8 # ( DP) PULU,           \ -- ; Restore DP
    NEXT ;C

CODE _Draw_Line_d \ x y -- ;
    8 # ( DP) PSHU,           \ -- x y c-addr ; Save DP
    D0 # LDX,   X DPR TFR,    \ -- x y c-addr ; DP to D0
    A B EXG,   S ,++ ADDD,    \ -- yx  ; Combine x and y
    Draw_Line_d JSR,          \ Call Vectrex BIOS subroutine
    6 # ( D) PULS,            \ -- ; Drop TOS
    8 # ( DP) PULU,           \ -- ; Restore DP
    NEXT ;C

CODE _Draw_VLcs \ addr -- ;  addr = vector list in format: count, scale, rel y, rel x, rel y, rel x, ...
    18 # ( X DP) PSHU,        \ -- addr ; Save X, DP
    D0 # LDX,   X DPR TFR,    \ -- addr ; DP to D0
    D X TFR,                  \ -- addr ; addr to X
    Draw_VLcs JSR,            \ Call Vectrex BIOS subroutine
    6 # ( D) PULS,            \ -- addr ; Drop TOS
    18 # ( X DP) PULU,        \ -- ; Restore X, DP
    NEXT ;C

CODE _Draw_VL_ab \ sf #v addr -- ;  addr = vector list in format: rel y, rel x, rel y, rel x, ...
    18 # ( X DP) PSHU,        \ -- sf #v addr ; Save X, DP
    D0 # LDX,   X DPR TFR,    \ -- sf #v addr ; DP to D0
    D X TFR,   6 # ( D) PULS, \ -- sf #v      ; addr to X and drop TOS
    A B EXG,   S ,++ ADDD,    \ -- #v+sf      ; Combine sf and #v
    Draw_VL_ab JSR,           \ -- ??         ; Call Vectrex BIOS subroutine
    6 # ( D) PULS,            \ --            ; Drop TOS
    18 # ( X DP) PULU,        \ --            ; Restore X, DP
    NEXT ;C

CODE _Dot_here \ -- ;
    1E # ( X DP D) PSHS,
    D0 # LDX,   X DPR TFR,    \ -- ; DP to D0
    Dot_here JSR,
    1E # ( X DP D) PULS,
    NEXT ;C

CODE _Read_Btns \ -- b ; Button Transition State (Same as $C811)
    6 # ( D) PSHS,
    18 # ( X DP) PSHS,
    D0 # LDX,   X DPR TFR,    \ -- ; DP to D0
    Read_Btns JSR,
    A B TFR,  CLRA,
    18 # ( X DP) PULS,
    NEXT ;C

CODE _Joy_Digital \ -- ;
    1E # ( X DP D) PSHS,
    D0 # LDX,   X DPR TFR,    \ -- ; DP to D0
    Joy_Digital JSR,
    1E # ( X DP D) PULS,
    NEXT ;C
