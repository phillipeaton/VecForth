\ Vectrex Forth Application

HEX

\ Calibration/vector reset

CODE _Wait_Recal \ -- ;
    1E # ( X DP D) PSHS,   Wait_Recal JSR,   1E # ( X DP D) PULS,   NEXT ;C

CODE _Set_Refresh   NEXT ;C

CODE _Recalibrate \ -- ;
    1E # ( X DP D) PSHS,
    D0 # LDX,   X DPR TFR,    \ -- ; DP to D0
    Recalibrate JSR,
    1E # ( X DP D) PULS,
    NEXT ;C

CODE _Reset0Ref_D0  NEXT ;C
CODE _Check0Ref     NEXT ;C

CODE _Reset0Ref \ -- ;
    1E # ( X DP D) PSHS,      \ Save Registers
    D0 # LDX,   X DPR TFR,    \ DP to D0
    Reset0Ref JSR,            \ Call Vectrex BIOS subroutine
    1E # ( X DP D) PULS,      \ Restore Registers
    NEXT ;C

CODE _Reset_Pen \ -- ;
    1E # ( X DP D) PSHS,      \ Save Registers
    D0 # LDX,   X DPR TFR,    \ DP to D0
    Reset_Pen JSR,            \ Call Vectrex BIOS subroutine
    1E # ( X DP D) PULS,      \ Restore Registers
    NEXT ;C

CODE _Reset0Int NEXT ;C

\ Day to Day

\ CODE DPR!   B DPR TFR,   6 # ( D) PULS, NEXT ;C \ DPR -- ; Quicker to set DP directly
CODE _DP_to_D0          NEXT ;C
CODE _DP_to_C8          NEXT ;C
CODE _Print_Ships_x     NEXT ;C
CODE _Print_Ships       NEXT ;C
CODE _Random_3          NEXT ;C

CODE _Random \ -- n ; n is a random number between 0 and 255
     6 # ( D) PSHS,   Random JSR,   CLRB,   A B EXG,   NEXT ;C

CODE _Dec_3_Counters    NEXT ;C
CODE _Dec_6_Counters    NEXT ;C
CODE _Dec_Counters      NEXT ;C
CODE _Bitmask_a         NEXT ;C

\ Delay

CODE _Delay_3           NEXT ;C
CODE _Delay_2           NEXT ;C
CODE _Delay_1           NEXT ;C
CODE _Delay_0           NEXT ;C
CODE _Delay_b           NEXT ;C
CODE _Delay_RTS         NEXT ;C

\ Drawing / Dot

CODE _Dot_ix_b          NEXT ;C
CODE _Dot_ix            NEXT ;C
CODE _Dot_d             NEXT ;C

CODE _Dot_here \ -- ;
    1E # ( X DP D) PSHS,
    D0 # LDX,   X DPR TFR,    \ -- ; DP to D0
    Dot_here JSR,
    1E # ( X DP D) PULS,
    NEXT ;C

CODE _Dot_List \ dl_addr -- ; *** NOT TESTED ***
    8 # ( DP) PSHU,           \ -- dl_addr ; Save DP
    D0 # LDX,   X DPR TFR,    \ -- dl_addr ; DP to D0
    D X TFR,                  \ -- dl_addr ; Load X with address of dot list
    Dot_here JSR,             \ Call Vectrex BIOS subroutine
    6 # ( D) PULS,            \ -- ; Drop TOS
    8 # ( DP) PULU,           \ -- ; Restore DP
    NEXT ;C

CODE _Dot_List          NEXT ;C
CODE _Dot_List_Reset    NEXT ;C

\ Drawing / String

CODE _Print_Str_hwyx    NEXT ;C
CODE _Print_Str_yx      NEXT ;C

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

CODE _Print_List_hw     NEXT ;C
CODE _Print_List        NEXT ;C
CODE _Print_List_chk    NEXT ;C
CODE _Print_Str         NEXT ;C

\ drawing / Vector / Drawing and moving

CODE _Mov_Draw_VLc_a    NEXT ;C
CODE _Mov_Draw_VLc_b    NEXT ;C
CODE _Mov_Draw_VLcs     NEXT ;C
CODE _Mov_Draw_VL_ab    NEXT ;C
CODE _Mov_Draw_VL_a     NEXT ;C
CODE _Mov_Draw_VL       NEXT ;C
CODE _Mov_Draw_VL_d     NEXT ;C

\ Drawing / Vector / Drawing only

\ *** NOT TESTED ***
CODE _Draw_VLc \ addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...
                \            current scaling factor is used
    18 # ( X DP) PSHU,        \ -- addr ; Save X, DP
    D0 # LDX,   X DPR TFR,    \ -- addr ; DP to D0
    D X TFR,                  \ -- addr ; addr to X
    Draw_VLc JSR,            \ Call Vectrex BIOS subroutine
    6 # ( D) PULS,            \ -- addr ; Drop TOS
    18 # ( X DP) PULU,        \ -- ; Restore X, DP
    NEXT ;C

CODE _Draw_VL_b         NEXT ;C

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

CODE _Draw_VL_a         NEXT ;C
CODE _Draw_VL           NEXT ;C

CODE _Draw_Line_d \ x y -- ;
    8 # ( DP) PSHU,           \ -- x y c-addr ; Save DP
    D0 # LDX,   X DPR TFR,    \ -- x y c-addr ; DP to D0
    A B EXG,   S ,++ ADDD,    \ -- yx  ; Combine x and y
    Draw_Line_d JSR,          \ Call Vectrex BIOS subroutine
    6 # ( D) PULS,            \ -- ; Drop TOS
    8 # ( DP) PULU,           \ -- ; Restore DP
    NEXT ;C

CODE _Draw_VLp_FF       NEXT ;C
CODE _Draw_VLp_7F       NEXT ;C
CODE _Draw_VLp_scale    NEXT ;C
CODE _Draw_VLp_b        NEXT ;C
CODE _Draw_VLp          NEXT ;C
CODE _Draw_Pat_VL_a     NEXT ;C
CODE _Draw_Pat_VL       NEXT ;C
CODE _Draw_Pat_VL_d     NEXT ;C
CODE _Draw_VL_mode      NEXT ;C
CODE _Draw_Grid_VL      NEXT ;C

\ Joystick handling

CODE _Read_Btns_Mask    NEXT ;C

CODE _Read_Btns \ -- b ; Button Transition State (Same as $C811)
    6 # ( D) PSHS,
    18 # ( X DP) PSHS,
    D0 # LDX,   X DPR TFR,    \ -- ; DP to D0
    Read_Btns JSR,
    A B TFR,  CLRA,
    18 # ( X DP) PULS,
    NEXT ;C

CODE _Joy_Analog        NEXT ;C

CODE _Joy_Digital \ -- ;
    1E # ( X DP D) PSHS,
    D0 # LDX,   X DPR TFR,    \ -- ; DP to D0
    Joy_Digital JSR,
    1E # ( X DP D) PULS,
    NEXT ;C

\ Mathematical

CODE _Abs_a_b           NEXT ;C
CODE _Abs_b             NEXT ;C

\ Memory Management / Memory clear

CODE _Clear_x_b         NEXT ;C
CODE _Clear_C8_RAM      NEXT ;C
CODE _Clear_x_256       NEXT ;C
CODE _Clear_x_d         NEXT ;C

\ Memory management / Memory copy

CODE _Move_Mem_a_1      NEXT ;C
CODE _Move_Mem_a        NEXT ;C

\ Memory Management / Memory fill

CODE _Clear_x_b_80      NEXT ;C
CODE _Clear_x_b_a       NEXT ;C

\ Player option

CODE _Select_Game       NEXT ;C
CODE _Display_Option    NEXT ;C

\ Reset and initialization

CODE _Cold_Start Cold_Start JMP, ;C \ -- ; Jump here to restart the Vectrex and re-initialize the OS.
CODE _Warm_Start Warm_Start JMP, ;C \ -- ; Jump here to restart the Vectrex without re-initializing the OS.

CODE _Init_VIA          NEXT ;C
CODE _Init_OS_RAM       NEXT ;C
CODE _Init_OS           NEXT ;C

\ Score

CODE _Clear_Score       NEXT ;C
CODE _Add_Score_a       NEXT ;C
CODE _Add_Score_d       NEXT ;C
CODE _Strip_Zeros       NEXT ;C
CODE _Compare_Score     NEXT ;C
CODE _New_High_Score    NEXT ;C

\ Sound

CODE _Sound_Byte        NEXT ;C
CODE _Sound_Byte_x      NEXT ;C
CODE _Sound_Byte_raw    NEXT ;C
CODE _Clear_Sound       NEXT ;C
CODE _Sound_Bytes       NEXT ;C
CODE _Sound_Bytes_x     NEXT ;C
CODE _Do_Sound          NEXT ;C
CODE _Do_Sound_x        NEXT ;C
CODE _Init_Music_Buf    NEXT ;C
CODE _Init_Music_chk    NEXT ;C
CODE _Init_Music        NEXT ;C
CODE _Init_Music_dft    NEXT ;C
CODE _Explosion_Snd     NEXT ;C

\ Vector beam positioning

CODE _Moveto_x_7F       NEXT ;C

CODE _Moveto_d_7F \ x y -- ;
    8 # ( DP) PSHU,           \ -- x y ; Save DP
    D0 # LDX,   X DPR TFR,    \ -- x y ; DP to D0
    A B EXG,   S ,++ ADDD,    \ -- yx  ; Combine x and y
    Moveto_d_7F JSR,          \ Call Vectrex BIOS subroutine
    6 # ( D) PULS,            \ -- ; Drop TOS
    8 # ( DP) PULU,           \ -- ; Restore DP
    NEXT ;C

CODE _Moveto_ix_FF      NEXT ;C
CODE _Moveto_ix_7F      NEXT ;C
CODE _Moveto_ix_b       NEXT ;C
CODE _Moveto_ix         NEXT ;C

CODE _Moveto_d \ x y -- ;
    8 # ( DP) PSHU,           \ -- x y ; Save DP
    D0 # LDX,   X DPR TFR,    \ -- x y ; DP to D0
    A B EXG,   S ,++ ADDD,    \ -- yx  ; Combine x and y
    Moveto_d JSR,             \ Call Vectrex BIOS subroutine
    6 # ( D) PULS,            \ -- ; Drop TOS
    8 # ( DP) PULU,           \ -- ; Restore DP
    NEXT ;C

\ Vector brightness

CODE _Intensity_1F \ -- ;
     8 # ( DP) PSHU,           \ -- ; Save DP
     D0 # LDX,   X DPR TFR,    \ -- ; DP to D0
     6 # ( D) PSHS,   Intensity_1F JSR,   6 # ( D) PULS,
     8 # ( DP) PULU,           \ -- ; Restore DP
    NEXT ;C

CODE _Intensity_3F \ -- ;
     8 # ( DP) PSHU,           \ -- ; Save DP
     D0 # LDX,   X DPR TFR,    \ -- ; DP to D0
     6 # ( D) PSHS,   Intensity_3F JSR,   6 # ( D) PULS,
     8 # ( DP) PULU,           \ -- ; Restore DP
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

CODE _Intensity_a \ n -- ;
    8 # ( DP) PSHU,           \ -- TOS ; Save DP
    D0 # LDX,   X DPR TFR,    \ -- TOS ; DP to D0
    Intensity_a JSR,
    6 # ( D) PULS,            \ -- ; Drop TOS
    8 # ( DP) PULU,           \ -- ; Restore DP
    NEXT ;C

\ Vector object handling / Object collision detection

CODE _Obj_Will_Hit_u    NEXT ;C
CODE _Obj_Will_Hit      NEXT ;C
CODE _Obj_Hit           NEXT ;C

\ Vector object handling / Rotating

CODE _Rise_Run_Angle    NEXT ;C
CODE _Get_Rise_Idx      NEXT ;C
CODE _Get_Run_Idx       NEXT ;C
CODE _Rise_Run_Idx      NEXT ;C
CODE _Rise_Run_X        NEXT ;C
CODE _Rise_Run_Y        NEXT ;C
CODE _Rise_Run_Len      NEXT ;C
CODE _Rot_VL_ab         NEXT ;C
CODE _Rot_VL            NEXT ;C
CODE _Rot_VL_Mode       NEXT ;C
CODE _Rot_VL_M_dft      NEXT ;C
CODE _Xform_Run_a       NEXT ;C
CODE _Xform_Run         NEXT ;C
CODE _Xform_Rise_a      NEXT ;C
CODE _Xform_Rise        NEXT ;C
