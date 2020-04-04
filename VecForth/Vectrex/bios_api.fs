\ Vectrex Forth Application

HEX

\ Forth doesn't care about the X register, it's short term work space only, much like the vectrex.

\ Calibration/vector reset

CODE _Reset0Ref_D0      E # ( DP D) PSHU,                           Reset0Ref_D0 JSR,   E # ( DP D) PULU,   NEXT ;C \ -- ; DP=D0
CODE _Wait_Recal        E # ( DP D) PSHU,                             Wait_Recal JSR,   E # ( DP D) PULU,   NEXT ;C \ -- ; DP=D0
CODE _Recalibrate       E # ( DP D) PSHU,   D0 # LDA,   A DPR TFR,   Recalibrate JSR,   E # ( DP D) PULU,   NEXT ;C \ -- ;
CODE _Set_Refresh       E # ( DP D) PSHU,   D0 # LDA,   A DPR TFR,   Set_Refresh JSR,   E # ( DP D) PULU,   NEXT ;C \ -- ;
CODE _Check0Ref         E # ( DP D) PSHU,   D0 # LDA,   A DPR TFR,     Check0Ref JSR,   E # ( DP D) PULU,   NEXT ;C \ -- ;
CODE _Reset0Ref         E # ( DP D) PSHU,   D0 # LDA,   A DPR TFR,     Reset0Ref JSR,   E # ( DP D) PULU,   NEXT ;C \ -- ;
CODE _Reset_Pen         E # ( DP D) PSHU,   D0 # LDA,   A DPR TFR,     Reset_Pen JSR,   E # ( DP D) PULU,   NEXT ;C \ -- ;
CODE _Reset0Int         E # ( DP D) PSHU,   D0 # LDA,   A DPR TFR,     Reset0Int JSR,   E # ( DP D) PULU,   NEXT ;C \ -- ;


\ Day to Day

CODE _DP_to_D0          NEXT ;C \ Not needed for Forth, DP managed in API calls
CODE _DP_to_C8          NEXT ;C \ Not needed for Forth, DP managed in API calls

CODE _Print_Ships_x     8 # ( DP) PSHU,   D0 # LDX,   X DPR TFR,                           D X TFR,   6 # ( D) PULS,   A B EXG,   S ,++ ADDD,   Print_Ships_x JSR,   6 # ( D) PULS,   8 # ( DP) PULU,   NEXT ;C \ #ships ship_char addr -- ; Utilises Stack underflow?
CODE _Print_Ships       8 # ( DP) PSHU,   D0 # LDX,   X DPR TFR,  A B EXG,   S ,++ ADDD,   D X TFR,   6 # ( D) PULS,   A B EXG,   S ,++ ADDD,   Print_Ships   JSR,   6 # ( D) PULS,   8 # ( DP) PULU,   NEXT ;C \ #ships ship_char x y -- ; Utilises Stack underflow?

CODE _Random            6 # (  D) PSHS,                 Random   JSR,  CLRB,   A B EXG,   NEXT ;C \ -- n ; n is a random number between 0 and 255
CODE _Random_3          6 # (  D) PSHS,                 Random_3 JSR,  CLRB,   A B EXG,   NEXT ;C \ -- n ; n is a random number between 0 and 255

CODE _Dec_3_Counters    6 # (  D) PSHU,           Dec_3_Counters JSR,    6 # ( D) PULU,   NEXT ;C \ -- ;
CODE _Dec_6_Counters    6 # (  D) PSHU,           Dec_6_Counters JSR,    6 # ( D) PULU,   NEXT ;C \ -- ;
CODE _Dec_Counters      D X TFR,   6 # ( D) PULS,   Dec_Counters JSR,    6 # ( D) PULS,   NEXT ;C \ counter_bytes #counters-1 -- ;

CODE _Bitmask_a         A B EXG,                       Bitmask_a JSR,          A B EXG,   NEXT ;C \ bit_number -- bit_mask ;

\ Delay

CODE _Delay_3           6 # ( D) PSHS,   Delay_3 JSR,   NEXT ;C \   -- n  ; n is xxFF, where xx is undefined
CODE _Delay_2           6 # ( D) PSHS,   Delay_2 JSR,   NEXT ;C \   -- n  ; n is xxFF, where xx is undefined
CODE _Delay_1           6 # ( D) PSHS,   Delay_1 JSR,   NEXT ;C \   -- n  ; n is xxFF, where xx is undefined
CODE _Delay_0           6 # ( D) PSHS,   Delay_0 JSR,   NEXT ;C \   -- n  ; n is xxFF, where xx is undefined
CODE _Delay_b                            Delay_b JSR,   NEXT ;C \ n -- n' ; n is xxnn, where xx is undefined and nn is length to delay. n' is xxFF, where xx is undefined.
CODE _Delay_RTS                          Delay_b JSR,   NEXT ;C \   --    ;

\ Drawing / Dot

CODE _Dot_ix_b          8 # ( DP  ) PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   6 # ( D) PULS,   Dot_ix_b JSR,         X D TFR,   8 # ( DP  ) PULU,   NEXT ;C \ intensity coord-pair-addr -- coord-pair-addr+2 ;
CODE _Dot_ix            8 # ( DP  ) PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                    Dot_ix   JSR,         X D TFR,   8 # ( DP  ) PULU,   NEXT ;C \           coord-pair-addr -- coord-pair-addr+2 ;
CODE _Dot_d             8 # ( DP  ) PSHU,   D0 # LDX,   X DPR TFR,   A B EXG,      S ,++ ADDD,   Dot_d    JSR,   6 # ( D) PULS,   8 # ( DP  ) PULU,   NEXT ;C \     x y -- ;
CODE _Dot_here          E # ( DP D) PSHU,   D0 # LDA,   A DPR TFR,                               Dot_here JSR,                    E # ( DP D) PULU,   NEXT ;C \         -- ;
CODE _Dot_List          8 # ( DP  ) PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                    Dot_List JSR,         X D TFR,   8 # ( DP  ) PULU,   NEXT ;C \ dl_addr -- dl_addr' ;
CODE _Dot_List_Reset    8 # ( DP  ) PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,              Dot_List_Reset JSR,         X D TFR,   8 # ( DP  ) PULU,   NEXT ;C \ dl_addr -- dl_addr' ;

\ Drawing / String

CODE _Print_Str_d      28 # ( Y DP) PSHU,   D0 # LDX,   X DPR TFR,   U Y TFR,   D U TFR,   6 # ( D) PULS,   A B EXG,   S ,++ ADDD,   Print_Str_d    JSR,   6 # ( D) PULS,  Y U TFR,          28 # ( Y DP) PULU,   NEXT ;C \ x y c-addr -- ; Print single string to screen
CODE _Print_Str_hwyx    8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,   U D EXG,   6 # ( D) PSHS,   Print_Str_hwyx JSR,   6 # ( D) PULS,  D U TFR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \ c-addr -- ;
CODE _Print_Str_yx      8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,   U D EXG,   6 # ( D) PSHS,   Print_Str_yx   JSR,   6 # ( D) PULS,  D U TFR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \ c-addr -- ;
CODE _Print_List_hw     8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,   U D EXG,   6 # ( D) PSHS,   Print_List_hw  JSR,   6 # ( D) PULS,  D U TFR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \ c-addr -- ;
CODE _Print_List        8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,   U D EXG,   6 # ( D) PSHS,   Print_List     JSR,   6 # ( D) PULS,  D U TFR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \ c-addr -- ;
CODE _Print_List_chk    8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,   U D EXG,   6 # ( D) PSHS,   Print_List_chk JSR,   6 # ( D) PULS,  D U TFR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \ c-addr -- ;
CODE _Print_Str         8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,   U D EXG,   6 # ( D) PSHS,   Print_Str      JSR,   6 # ( D) PULS,  D U TFR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \ c-addr -- ;

\ drawing / Vector / Drawing and moving

CODE _Mov_Draw_VLc_a    8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,    D X TFR,                                              Mov_Draw_VLc_a JSR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Mov_Draw_VL_b     8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,    D X TFR,   6 # ( D) PULS,                             Mov_Draw_VL_b  JSR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \ sf    addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Mov_Draw_VLcs     8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,    D X TFR,                                              Mov_Draw_VLcs  JSR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Mov_Draw_VL_ab    8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,    D X TFR,   6 # ( D) PULS,   A B EXG,   S ,++ ADDD,    Mov_Draw_VL_ab JSR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \ sf #v addr -- ; addr = vector list in format: rel y, rel x, rel y, rel x, ...
CODE _Mov_Draw_VL_a     8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,    D X TFR,   6 # ( D) PULS,   A B EXG,                  Mov_Draw_VL_a  JSR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \    #v addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Mov_Draw_VL       8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,    D X TFR,                                              Mov_Draw_VL    JSR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Mov_Draw_VL_d     8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,    D X TFR,   6 # ( D) PULS,   A B EXG,   S ,++ ADDD,    Mov_Draw_VL_d  JSR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \ sf #v addr -- ; addr = vector list in format: rel y, rel x, rel y, rel x, ...

\ Drawing / Vector / Drawing only

CODE _Draw_VLc          8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,    D X TFR,                                              Draw_VLc       JSR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Draw_VL_b         8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,    D X TFR,   6 # ( D) PULS,                             Draw_VL_b      JSR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \ sf    addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Draw_VLcs         8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,    D X TFR,                                              Draw_VLcs      JSR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, scale, rel y, rel x, rel y, rel x, ...
CODE _Draw_VL_ab        8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,    D X TFR,   6 # ( D) PULS,   A B EXG,   S ,++ ADDD,    Draw_VL_ab     JSR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \ sf #v addr -- ; addr = vector list in format: rel y, rel x, rel y, rel x, ...
CODE _Draw_VL_a         8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,    D X TFR,   6 # ( D) PULS,   A B EXG,                  Draw_VL_a      JSR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \    #v addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Draw_VL           8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,    D X TFR,                                              Draw_VL        JSR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Draw_Line_d       8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,                                A B EXG,   S ,++ ADDD,    Draw_Line_d    JSR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \        x y -- ;
CODE _Draw_VLp_FF       8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,    D X TFR,                                              Draw_VLp_FF    JSR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Draw_VLp_7F       8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,    D X TFR,                                              Draw_VLp_7F    JSR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Draw_VLp_scale    8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,    D X TFR,                                              Draw_VLp_scale JSR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Draw_VLp_b        8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,    D X TFR,   6 # ( D) PULS,                             Draw_VLp_b     JSR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \ sf    addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Draw_VLp          8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,    D X TFR,                                              Draw_VLp       JSR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Draw_Pat_VL_a     8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,    D X TFR,   6 # ( D) PULS,   A B EXG,                  Draw_Pat_VL_a  JSR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \    #v addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Draw_Pat_VL       8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,    D X TFR,                                              Draw_Pat_VL    JSR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Draw_Pat_VL_d     8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,    D X TFR,   6 # ( D) PULS,   A B EXG,   S ,++ ADDD,    Draw_Pat_VL_d  JSR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \ sf #v addr -- ; addr = vector list in format: rel y, rel x, rel y, rel x, ...
CODE _Draw_VL_mode      8 # (   DP) PSHU,   D0 # LDX,   X DPR TFR,    D X TFR,                                              Draw_VL_mode   JSR,   6 # ( D) PULS,   8 # (   DP) PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Draw_Grid_VL     28 # ( Y DP) PSHU,   D0 # LDX,   X DPR TFR,    D X TFR,   6 # ( D) PULS,   D Y TFR,                  Draw_Grid_VL   JSR,   6 # ( D) PULS,   8 # ( Y DP) PULU,   NEXT ;C \ addrY addrX -- ;

\ Joystick handling

CODE _Read_Btns_Mask    E # ( DP  ) PSHU,   D0 # LDB,   B DPR TFR,    Read_Btns_Mask JSR,   A B TFR,   CLRA,   8 # ( DP  ) PULU,   NEXT ;C \ maskA -- b ; Button Transition State (Same as $C811)
CODE _Read_Btns         E # ( DP D) PSHU,   D0 # LDA,   A DPR TFR,    Read_Btns      JSR,   A B TFR,   CLRA,   8 # ( DP  ) PULU,   NEXT ;C \       -- b ; Button Transition State (Same as $C811)

CODE _Joy_Analog        E # ( DP D) PSHU,   D0 # LDA,   A DPR TFR,    Joy_Analog     JSR,                      E # ( DP D) PULU,   NEXT ;C \ -- ;
CODE _Joy_Digital       E # ( DP D) PSHU,   D0 # LDA,   A DPR TFR,    Joy_Digital    JSR,                      E # ( DP D) PULU,   NEXT ;C \ -- ;

\ Mathematical

CODE _Abs_a_b           Abs_a_b JSR,   NEXT ;C \ n -- n' ; Returns abs value of two 8 bit numbers
CODE _Abs_b             Abs_b   JSR,   NEXT ;C \ n -- n' ; Returns abs value of ONE 8 bit number (bottom 8 bits)

\ Memory Management / Memory clear

CODE _Clear_x_b         D X TFR,   6 # ( D) PULS,   Clear_x_b    JSR,   6 # ( D) PULS,   NEXT ;C \ addr #bytes-1 -- ; #bytes stored in bottom 8 bits only
CODE _Clear_C8_RAM      6 # ( D) PSHU,              Clear_C8_RAM JSR,   6 # ( D) PULS,   NEXT ;C \               -- ;
CODE _Clear_x_256       D X TFR,                    Clear_x_b    JSR,   6 # ( D) PULS,   NEXT ;C \ addr          -- ; addr = start of RAM to be cleared
CODE _Clear_x_d         D X TFR,   6 # ( D) PULS,   Clear_x_b    JSR,   6 # ( D) PULS,   NEXT ;C \ addr #bytes-1 -- ; #bytes stored in 16 bits only

\ Memory management / Memory copy

CODE _Move_Mem_a_1      NEXT ;C \ Not need for Forth, use CMOVE or CMOVE> instead
CODE _Move_Mem_a        NEXT ;C \ Not need for Forth, use CMOVE or CMOVE> instead

\ Memory Management / Memory fill

CODE _Clear_x_b_80      NEXT ;C \ Not need for Forth, use FILL instead
CODE _Clear_x_b_a       NEXT ;C \ Not need for Forth, use FILL instead

\ Player option

CODE _Select_Game      28 # ( Y DP) PSHU,                            A B EXG,   S ,++ ADDD,     Select_Game    JSR,   6 # ( D) PULS,   28 # ( Y DP) PULU, NEXT ;C \ #game_versions #players_max -- ;
CODE _Display_Option   48 # ( U DP) PSHU,   D0 # LDX,   X DPR TFR,   D Y TFR,   6 # ( D) PULS,  Display_Option JSR,   6 # ( D) PULS,   48 # ( U DP) PULU, NEXT ;C \ option_val addr -- ;

\ Reset and initialization

CODE _Cold_Start                            Cold_Start  JMP,                            ;C \ -- ; Jump here to restart the Vectrex and re-initialize the OS.
CODE _Warm_Start                            Warm_Start  JMP,                            ;C \ -- ; Jump here to restart the Vectrex without re-initializing the OS.
CODE _Init_VIA          E # ( DP D) PSHU,   Init_VIA    JSR,   E # ( DP D) PULU,   NEXT ;C \ -- ;
CODE _Init_OS_RAM       E # ( DP D) PSHU,   Init_OS_RAM JSR,   E # ( DP D) PULU,   NEXT ;C \ -- ;
CODE _Init_OS           E # ( DP D) PSHU,   Init_OS     JSR,   E # ( DP D) PULU,   NEXT ;C \ -- ;

\ Score

CODE _Clear_Score       D X TFR,   Clear_Score   JSR,   6 # ( D) PULS,   NEXT ;C \    addr -- ;
CODE _Add_Score_a       40 # ( U) PSHU,   D X TFR,   6 # ( D) PULS,   D U TFR,   6 # ( D) PULS,   Add_Score_a    JSR,   6 # ( D) PULS,   40 # ( U) PULU,   NEXT ;C \ binary# BCD ptr -- ;
CODE _Add_Score_d                         D X TFR,   6 # ( D) PULS,                               Add_Score_d    JSR,   6 # ( D) PULS,                     NEXT ;C \         BCD ptr -- ;
CODE _Strip_Zeros                         D X TFR,   6 # ( D) PULS,                               Strip_Zeros    JSR,   6 # ( D) PULS,                     NEXT ;C \      digit# ptr -- ; digit# 8 bit only
CODE _Compare_Score     40 # ( U) PSHU,   D X TFR,   6 # ( D) PULS,   D U TFR,                    Compare_Score  JSR,   A B EXG,         40 # ( U) PULU,   NEXT ;C \       ptr2 ptr1 -- 0|1|2; Same|1>2|2>1
CODE _New_High_Score    40 # ( U) PSHU,   D X TFR,   6 # ( D) PULS,   D U TFR,                    New_High_Score JSR,   6 # ( D) PULS,   40 # ( U) PULU,   NEXT ;C \     addr2 addr1 -- ;

\ Sound

CODE _Sound_Byte        NEXT ;C
CODE _Sound_Byte_x      NEXT ;C
CODE _Sound_Byte_raw    NEXT ;C
CODE _Clear_Sound       NEXT ;C
CODE _Sound_Bytes       NEXT ;C
CODE _Sound_Bytes_x     NEXT ;C
CODE _Do_Sound                     4E # ( U   DP D) PSHS,               D0 # LDA,   A DPR TFR,   Do_Sound       JSR,   4E # ( U   DP D) PULS,                                     NEXT ;C \      -- ;
CODE _Do_Sound_x                   48 # ( U   DP  ) PSHS,    D X TFR,   D0 # LDA,   A DPR TFR,   Do_Sound       JSR,   48 # ( U   DP  ) PULS,                                     NEXT ;C \  ptr -- ; TBD
CODE _Init_Music_Buf                E # (     DP D) PSHS,                                        Init_Music_Buf JSR,   1E # (     DP D) PULS,                               NEXT ;C \      -- ;
CODE _Init_Music_chk    D U EXG,   6E # ( U Y DP D) PSHS,               C8 # LDX,   X DPR TFR,   Init_Music_chk JSR,   7E # ( U Y DP D) PULS,   D U TFR,   6 # ( D) PULS,   NEXT ;C \ addr -- ;
CODE _Init_Music        D U EXG,   6E # ( U Y DP D) PSHS,               C8 # LDX,   X DPR TFR,   Init_Music     JSR,   7E # ( U Y DP D) PULS,   D U TFR,   6 # ( D) PULS,   NEXT ;C \ addr -- ;
CODE _Init_Music_dft    D U EXG,   6E # ( U Y DP D) PSHS,               C8 # LDX,   X DPR TFR,   Init_Music_x   JSR,   7E # ( U Y DP D) PULS,   D U TFR,   6 # ( D) PULS,   NEXT ;C \ addr -- ; Note Init_Music_dft/Music_x
CODE _Explosion_Snd     D U EXG,    E # (     DP D) PSHS,               C8 # LDX,   X DPR TFR,   Explosion_Snd  JSR,   1E # (     DP D) PULS,   D U TFR,   6 # ( D) PULS,   NEXT ;C \ addr -- ;

\ Vector beam positioning

CODE _Moveto_x_7F       8 # ( DP) PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                    Moveto_x_7F   JSR,   6 # ( D) PULS,   8 # ( DP) PULU,   NEXT ;C \    addr -- ;
CODE _Moveto_d_7F       8 # ( DP) PSHU,   D0 # LDX,   X DPR TFR,   A B EXG,      S ,++ ADDD,   Moveto_d_7F   JSR,   6 # ( D) PULS,   8 # ( DP) PULU,   NEXT ;C \ x  y    -- ;
CODE _Moveto_ix_FF      8 # ( DP) PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                    Moveto_ix_FF  JSR,   6 # ( D) PULS,   8 # ( DP) PULU,   NEXT ;C \    addr -- ;
CODE _Moveto_ix_7F      8 # ( DP) PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                    Moveto_ix_7F  JSR,   6 # ( D) PULS,   8 # ( DP) PULU,   NEXT ;C \    addr -- ;
CODE _Moveto_ix_b       8 # ( DP) PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   6 # ( D) PULS,   Moveto_ix_7F  JSR,   6 # ( D) PULS,   8 # ( DP) PULU,   NEXT ;C \ sf addr -- ;
CODE _Moveto_ix         8 # ( DP) PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                    Moveto_ix     JSR,   6 # ( D) PULS,   8 # ( DP) PULU,   NEXT ;C \    addr -- ;
CODE _Moveto_d          8 # ( DP) PSHU,   D0 # LDX,   X DPR TFR,   A B EXG,      S ,++ ADDD,   Moveto_d      JSR,   6 # ( D) PULS,   8 # ( DP) PULU,   NEXT ;C \ x  y    -- ;

\ Vector brightness

CODE _Intensity_1F      8 # ( DP) PSHU,   D0 # LDX,   X DPR TFR,   6 # ( D) PSHS,   Intensity_1F JSR,   6 # ( D) PULS,   8 # ( DP) PULU,   NEXT ;C \ -- ;
CODE _Intensity_3F      8 # ( DP) PSHU,   D0 # LDX,   X DPR TFR,   6 # ( D) PSHS,   Intensity_3F JSR,   6 # ( D) PULS,   8 # ( DP) PULU,   NEXT ;C \ -- ;
CODE _Intensity_5F      8 # ( DP) PSHU,   D0 # LDX,   X DPR TFR,   6 # ( D) PSHS,   Intensity_5F JSR,   6 # ( D) PULS,   8 # ( DP) PULU,   NEXT ;C \ -- ;
CODE _Intensity_7F      8 # ( DP) PSHU,   D0 # LDX,   X DPR TFR,   6 # ( D) PSHS,   Intensity_7F JSR,   6 # ( D) PULS,   8 # ( DP) PULU,   NEXT ;C \ -- ;
CODE _Intensity_a       8 # ( DP) PSHU,   D0 # LDX,   X DPR TFR,                     Intensity_a JSR,   6 # ( D) PULS,   8 # ( DP) PULU,   NEXT ;C \ n -- ;

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
