\ Vectrex Forth Application

HEX

\ Forth doesn't care about the X register, it's short term work space only, much like the vectrex.
\ ********** Code saving to U can't store it on teh U stack, needs to go on Y or S stack. E.g. Display_Option, Add_Score etc.
\ ***** Might be better to use some of these with S stack only, but used U due to parametnes. Some do only use S, where no paramenters.
\ X is a scratchpad for Vectrex and forth, doesn't need saving
\ general case ,save registers, set Do, arrange registers, combine where necessary, call routine, pull stack etc, note extra pulls to drop stack items at end
\ Routines rougly in ROM address order as several run inte each other.
\ Some of these routine entry points don't make sense from Forth
\ Need to swap over the x and ys on stack

\ Registers
$06 equ ____D
$08 equ __dp_
$0E equ __dpD
$26 equ _Y__D
$28 equ _Ydp_
$2E equ _YdpD
$40 equ U____    $40 equ S____
$48 equ U_dp_    $48 equ S_dp_
$4E equ U_dpD    $4E equ S_dpD
$68 equ UYdp_    $68 equ SYdp_
$6E equ UYdpD    $6E equ SYdpD




\ Reset and initialization

CODE _Cold_Start        Cold_Start  JMP,   ;C \ -- ; Jump here to restart the Vectrex and re-initialize the OS.
CODE _Warm_Start        Warm_Start  JMP,   ;C \ -- ; Jump here to restart the Vectrex without re-initializing the OS.
CODE _Init_VIA          __dpD # PSHU,   Init_VIA    JSR,   __dpD # PULU,   NEXT ;C \ -- ;
CODE _Init_OS_RAM       __dpD # PSHU,   Init_OS_RAM JSR,   __dpD # PULU,   NEXT ;C \ -- ;
CODE _Init_OS           __dpD # PSHU,   Init_OS     JSR,   __dpD # PULU,   NEXT ;C \ -- ;

\ Calibration/Vector reset

CODE _Wait_Recal        __dpD # PSHU,                            Wait_Recal   JSR,   __dpD # PULU,   NEXT ;C \ -- ;
CODE _Set_Refresh       __dpD # PSHU,   D0 # LDA,   A DPR TFR,   Set_Refresh  JSR,   __dpD # PULU,   NEXT ;C \ -- ;
\
CODE _Recalibrate       __dpD # PSHU,   D0 # LDA,   A DPR TFR,   Recalibrate  JSR,   __dpD # PULU,   NEXT ;C \ -- ;
\
CODE _Reset0Ref_D0      __dpD # PSHU,                            Reset0Ref_D0 JSR,   __dpD # PULU,   NEXT ;C \ -- ;
CODE _Check0Ref         __dpD # PSHU,   D0 # LDA,   A DPR TFR,   Check0Ref    JSR,   __dpD # PULU,   NEXT ;C \ -- ;
CODE _Reset0Ref         __dpD # PSHU,   D0 # LDA,   A DPR TFR,   Reset0Ref    JSR,   __dpD # PULU,   NEXT ;C \ -- ;
CODE _Reset_Pen         __dpD # PSHU,   D0 # LDA,   A DPR TFR,   Reset_Pen    JSR,   __dpD # PULU,   NEXT ;C \ -- ;
CODE _Reset0Int         __dpD # PSHU,   D0 # LDA,   A DPR TFR,   Reset0Int    JSR,   __dpD # PULU,   NEXT ;C \ -- ;

\ Set Direct Pointer

CODE _DP_to_D0          NEXT ;C \ Not needed for Forth, DP managed in API calls
CODE _DP_to_C8          NEXT ;C \ Not needed for Forth, DP managed in API calls

\ Joystick handling

CODE _Read_Btns_Mask    __dp_ # PSHU,   D0 # LDA,   A DPR TFR,        B A TFR,   Read_Btns_Mask JSR,   A B TFR,   CLRA,   __dp_ # PULU,   NEXT ;C \ maskA -- b ; Button Transition State (Same as $C811)
CODE _Read_Btns         __dp_ # PSHU,   D0 # LDA,   A DPR TFR,   ____D # PSHS,   Read_Btns      JSR,   A B TFR,   CLRA,   __dp_ # PULU,   NEXT ;C \       -- b ; Button Transition State (Same as $C811)
CODE _Joy_Analog        __dpD # PSHU,   D0 # LDX,   X DPR TFR,                   Joy_Analog     JSR,                      __dpD # PULU,   NEXT ;C \       -- ;
CODE _Joy_Digital       __dpD # PSHU,   D0 # LDX,   X DPR TFR,                   Joy_Digital    JSR,                      __dpD # PULU,   NEXT ;C \       -- ;

\ Sound

CODE _Sound_Byte        __dp_ # PSHU,   D0 # LDX,   X DPR TFR,                              A B EXG,   S ,++ ADDD,   Sound_Byte       JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ sound_byte_data reg# -- ;
CODE _Sound_Byte_x      __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D # PULS,   A B EXG,   S ,++ ADDD,   Sound_Byte_x     JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ sound_byte_data reg# shadow-addr -- ;
CODE _Sound_Byte_raw    __dp_ # PSHU,   D0 # LDX,   X DPR TFR,                              A B EXG,   S ,++ ADDD,   Sound_Byte_raw   JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ sound_byte_data reg# -- ;
CODE _Clear_Sound       __dpD # PSHU,   D0 # LDA,   A DPR TFR,                           Clear_Sound   JSR,                              __dpD # PULU,   NEXT ;C \     -- ;
CODE _Sound_Bytes       _Ydp_ # PSHU,   D0 # LDX,   X DPR TFR,   U Y TFR,   D U   TFR,   Sound_Bytes   JSR,   Y U TFR,   ____D # PULS,   _Ydp_ # PULU,   NEXT ;C \ ptr -- ;
CODE _Sound_Bytes_x     _Ydp_ # PSHU,   D0 # LDX,   X DPR TFR,   U Y TFR,   D U   TFR,   Sound_Bytes_x JSR,   Y U TFR,   ____D # PULS,   _Ydp_ # PULU,   NEXT ;C \ ptr -- ;
CODE _Do_Sound          U_dpD # PSHS,              D0 # LDA,   A DPR TFR,   Do_Sound       JSR,   U_dpD # PULS,                   NEXT ;C \      -- ;
CODE _Do_Sound_x        U_dp_ # PSHS,   D X TFR,   D0 # LDA,   A DPR TFR,   Do_Sound       JSR,   U_dp_ # PULS,   ____D # PULS,   NEXT ;C \  ptr -- ;
\
CODE _Init_Music_Buf    ____D # PSHS,                                       Init_Music_Buf JSR,                   ____D # PULS,   NEXT ;C \      -- ;
\
CODE _Init_Music_chk    UYdp_ # PSHS,   D U TFR,   C8 # LDA,   A DPR TFR,   Init_Music_chk JSR,   UYdp_ # PULS,   ____D # PULS,   NEXT ;C \ addr -- ;
CODE _Init_Music        UYdp_ # PSHS,   D U TFR,   C8 # LDA,   A DPR TFR,   Init_Music     JSR,   UYdp_ # PULS,   ____D # PULS,   NEXT ;C \ addr -- ;
CODE _Init_Music_dft    UYdp_ # PSHS,   D U TFR,   C8 # LDA,   A DPR TFR,   Init_Music_x   JSR,   UYdp_ # PULS,   ____D # PULS,   NEXT ;C \ addr -- ; Note Init_Music_dft/Music_x
CODE _Explosion_Snd     U_dp_ # PSHS,   D U TFR,   C8 # LDA,   A DPR TFR,   Explosion_Snd  JSR,   U_dp_ # PULS,   ____D # PULS,   NEXT ;C \ addr -- ;

\ Vector brightness

CODE _Intensity_1F      __dpD # PSHU,   D0 # LDA,   A DPR TFR,              Intensity_1F JSR,                   __dpD # PULU,   NEXT ;C \   -- ;
CODE _Intensity_3F      __dpD # PSHU,   D0 # LDA,   A DPR TFR,              Intensity_3F JSR,                   __dpD # PULU,   NEXT ;C \   -- ;
CODE _Intensity_5F      __dpD # PSHU,   D0 # LDA,   A DPR TFR,              Intensity_5F JSR,                   __dpD # PULU,   NEXT ;C \   -- ;
CODE _Intensity_7F      __dpD # PSHU,   D0 # LDA,   A DPR TFR,              Intensity_7F JSR,                   __dpD # PULU,   NEXT ;C \   -- ;
CODE _Intensity_a       __dp_ # PSHU,   D0 # LDA,   A DPR TFR,   A B EXG,   Intensity_a  JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ n -- ; n is intensity between 0 and $7F

\ Drawing / Dot

CODE _Dot_ix_b          __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D #  PULS,   Dot_ix_b JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ intensity coord-pair-addr -- ;
CODE _Dot_ix            __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                    Dot_ix   JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \           coord-pair-addr -- ;
CODE _Dot_d             __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   A B EXG,   S ,++ ADDD,      Dot_d    JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \     x y -- ;
CODE _Dot_here          __dpD # PSHU,   D0 # LDA,   A DPR TFR,                               Dot_here JSR,                   __dpD # PULU,   NEXT ;C \         -- ;
CODE _Dot_List          __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                    Dot_List JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ dl_addr -- ;
CODE _Dot_List_Reset    __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,              Dot_List_Reset JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ dl_addr -- ;

\ Vector beam positioning

CODE _Moveto_x_7F       __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                    Moveto_x_7F  JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \    addr -- ;
CODE _Moveto_d_7F       __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   A B EXG,   S ,++ ADDD,      Moveto_d_7F  JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ x  y    -- ;
CODE _Moveto_ix_FF      __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                    Moveto_ix_FF JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \    addr -- ;
CODE _Moveto_ix_7F      __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                    Moveto_ix_7F JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \    addr -- ;
CODE _Moveto_ix_b       __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D #  PULS,   Moveto_ix_7F JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ sf addr -- ;
CODE _Moveto_ix         __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                    Moveto_ix    JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \    addr -- ;
CODE _Moveto_d          __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   A B EXG,   S ,++ ADDD,      Moveto_d     JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ x  y    -- ;

\ Drawing / String

CODE _Print_Str_hwyx    _Ydp_ # PSHU,   D0 # LDX,   X DPR TFR,   U Y TFR,   D U TFR,   Print_Str_hwyx JSR,   ____D # PULS,   Y U TFR,   _Ydp_ # PULU,   NEXT ;C \ c-addr -- ;
CODE _Print_Str_yx      _Ydp_ # PSHU,   D0 # LDX,   X DPR TFR,   U Y TFR,   D U TFR,   Print_Str_yx   JSR,   ____D # PULS,   Y U TFR,   _Ydp_ # PULU,   NEXT ;C \ c-addr -- ;
CODE _Print_Str_d       _Ydp_ # PSHU,   D0 # LDX,   X DPR TFR,   U Y TFR,   D U TFR,   ____D # PULS,   A B EXG,   S ,++ ADDD,   Print_Str_d JSR,   ____D # PULS,   Y U TFR,   _Ydp_ # PULU,   NEXT ;C \ x y c-addr -- ; Print single string to screen
CODE _Print_List_hw     _Ydp_ # PSHU,   D0 # LDX,   X DPR TFR,   U Y TFR,   D U TFR,   Print_List_hw  JSR,   ____D # PULS,   Y U TFR,   _Ydp_ # PULU,   NEXT ;C \ c-addr -- ;
CODE _Print_List        _Ydp_ # PSHU,   D0 # LDX,   X DPR TFR,   U Y TFR,   D U TFR,   Print_List     JSR,   ____D # PULS,   Y U TFR,   _Ydp_ # PULU,   NEXT ;C \ c-addr -- ;
CODE _Print_List_chk    _Ydp_ # PSHU,   D0 # LDX,   X DPR TFR,   U Y TFR,   D U TFR,   Print_List_chk JSR,   ____D # PULS,   Y U TFR,   _Ydp_ # PULU,   NEXT ;C \ c-addr -- ;
CODE _Print_Str         _Ydp_ # PSHU,   D0 # LDX,   X DPR TFR,   U Y TFR,   D U TFR,   Print_Str      JSR,   ____D # PULS,   Y U TFR,   _Ydp_ # PULU,   NEXT ;C \ c-addr -- ;

\ Print Ships

CODE _Print_Ships_x     _Ydp_ # PSHU,   D0 # LDX,   X DPR TFR,  U Y TFR,                            D X TFR,   ____D # PULS,   A B EXG,   S ,++ ADDD,   Print_Ships_x JSR,   ____D # PULS,   Y U TFR,   _Ydp_ # PULU,   NEXT ;C \ #ships ship_char addr -- ; Underflows stack?
CODE _Print_Ships       _Ydp_ # PSHU,   D0 # LDX,   X DPR TFR,  U Y TFR,   A B EXG,   S ,++ ADDD,   D X TFR,   ____D # PULS,   A B EXG,   S ,++ ADDD,   Print_Ships   JSR,   ____D # PULS,   Y U TFR,   _Ydp_ # PULU,   NEXT ;C \ #ships ship_char x y  -- ; Underflows stack?

\ Drawing / Vector / Move and Draw

CODE _Mov_Draw_VLc_a    __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                                            Mov_Draw_VLc_a JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Mov_Draw_VL_b     __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D # PULS,                            Mov_Draw_VL_b  JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ sf    addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Mov_Draw_VLcs     __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                                            Mov_Draw_VLcs  JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Mov_Draw_VL_ab    __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D # PULS,   A B EXG,   S ,++ ADDD,   Mov_Draw_VL_ab JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ sf #v addr -- ; addr = vector list in format: rel y, rel x, rel y, rel x, ...
CODE _Mov_Draw_VL_a     __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D # PULS,   A B EXG,                 Mov_Draw_VL_a  JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \    #v addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Mov_Draw_VL       __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                                            Mov_Draw_VL    JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Mov_Draw_VL_d     __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D # PULS,   A B EXG,   S ,++ ADDD,   Mov_Draw_VL_d  JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \   x y addr -- ; addr = vector list in format: rel y, rel x, rel y, rel x, ...

\ Drawing / Vector / Draw only

CODE _Draw_VLc          __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                                            Draw_VLc       JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Draw_VL_b         __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D # PULS,                            Draw_VL_b      JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ sf    addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Draw_VLcs         __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                                            Draw_VLcs      JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, scale, rel y, rel x, rel y, rel x, ...
CODE _Draw_VL_ab        __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D # PULS,   A B EXG,   S ,++ ADDD,   Draw_VL_ab     JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ sf #v addr -- ; addr = vector list in format: rel y, rel x, rel y, rel x, ...
CODE _Draw_VL_a         __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D # PULS,   A B EXG,                 Draw_VL_a      JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \    #v addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Draw_VL           __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                                            Draw_VL        JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Draw_Line_d       __dp_ # PSHU,   D0 # LDX,   X DPR TFR,                              A B EXG,   S ,++ ADDD,   Draw_Line_d    JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \        x y -- ;
CODE _Draw_VLp_FF       __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                                            Draw_VLp_FF    JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Draw_VLp_7F       __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                                            Draw_VLp_7F    JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Draw_VLp_scale    __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                                            Draw_VLp_scale JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Draw_VLp_b        __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D # PULS,                            Draw_VLp_b     JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ sf    addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Draw_VLp          __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                                            Draw_VLp       JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Draw_Pat_VL_a     __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D # PULS,   A B EXG,                 Draw_Pat_VL_a  JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \    #v addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Draw_Pat_VL       __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                                            Draw_Pat_VL    JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Draw_Pat_VL_d     __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D # PULS,   A B EXG,   S ,++ ADDD,   Draw_Pat_VL_d  JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \   x y addr -- ; addr = vector list in format: rel y, rel x, rel y, rel x, ...
CODE _Draw_VL_mode      __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                                            Draw_VL_mode   JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
CODE _Draw_Grid_VL      _Ydp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D # PULS,   D Y TFR,                 Draw_Grid_VL   JSR,   ____D # PULS,   _Ydp_ # PULU,   NEXT ;C \ addrY addrX -- ;

\ Random number generator

CODE _Random_3          ____D # PSHS,   Random_3 JSR,   CLRB,   A B EXG,   NEXT ;C \ -- n ; n is a random number between 0 and 255
CODE _Random            ____D # PSHS,   Random   JSR,   CLRB,   A B EXG,   NEXT ;C \ -- n ; n is a random number between 0 and 255

\ Memory Management / Memory clear

CODE _Clear_x_b         D X TFR,   ____D # PULS,   Clear_x_b    JSR,   ____D # PULS,   NEXT ;C \ #bytes-1 addr -- ; #bytes stored in bottom 8 bits only
CODE _Clear_C8_RAM      ____D # PSHU,              Clear_C8_RAM JSR,   ____D # PULS,   NEXT ;C \               -- ;
CODE _Clear_x_256       D X TFR,                   Clear_x_256  JSR,   ____D # PULS,   NEXT ;C \          addr -- ; addr = start of RAM to be cleared
CODE _Clear_x_d         D X TFR,   ____D # PULS,   Clear_x_d    JSR,   ____D # PULS,   NEXT ;C \ #bytes-1 addr -- ; #bytes stored in 16 bits only

\ Memory Management / Memory fill

CODE _Clear_x_b_80      D X TFR,   ____D # PULS,   Clear_x_b_80 JSR,   ____D # PULS,   NEXT ;C \ #bytes-1 addr -- ; #bytes = $0-$FF, $00=$100. Fills with $80
CODE _Clear_x_b_a       D X TFR,   ____D # PULS,   Clear_x_b_a  JSR,   ____D # PULS,   NEXT ;C \ #bytes-1 addr -- ; #bytes = $0-$FF, $00=$100. Fills with byte=in #bytes top 8 bits

\ Counters

CODE _Dec_3_Counters               ____D # PSHU,   Dec_3_Counters JSR,   ____D # PULU,   NEXT ;C \ -- ;
CODE _Dec_6_Counters               ____D # PSHU,   Dec_6_Counters JSR,   ____D # PULU,   NEXT ;C \ -- ;
CODE _Dec_Counters      D X TFR,   ____D # PULS,   Dec_Counters   JSR,   ____D # PULS,   NEXT ;C \ #counters-1 ptr_counter_bytes -- ;

\ Delay

CODE _Delay_3           ____D # PSHU,   Delay_3   JSR,   ____D # PULU,   NEXT ;C \   -- ;
CODE _Delay_2           ____D # PSHU,   Delay_2   JSR,   ____D # PULU,   NEXT ;C \   -- ;
CODE _Delay_1           ____D # PSHU,   Delay_1   JSR,   ____D # PULU,   NEXT ;C \   -- ;
CODE _Delay_0           ____D # PSHU,   Delay_0   JSR,   ____D # PULU,   NEXT ;C \   -- ;
CODE _Delay_b                           Delay_b   JSR,   ____D # PULS,   NEXT ;C \ n -- ; n is xxnn, where xx is undefined and nn is length to delay. n' is xxFF, where xx is undefined.
CODE _Delay_RTS                         Delay_RTS JSR,                   NEXT ;C \   -- ;

\ Day to Day / Bitmask

CODE _Bitmask_a         A B EXG,   Bitmask_a JSR,   A B EXG,   NEXT ;C \ bit_number -- bit_mask ;

\ Mathematical

CODE _Abs_a_b           Abs_a_b JSR,   NEXT ;C \ n -- n' ; Returns abs value of two 8 bit numbers
CODE _Abs_b             Abs_b   JSR,   NEXT ;C \ n -- n' ; Returns abs value of one 8 bit number (bottom 8 bits)

\ Vector object handling / Rotating

\ // COMPAS   0xF593  CMPASS  Return angle for given delta 'Y:X'
\ // COSGET   0xF5D9  COSINE  Calculate the cosine of 'A'
\ // SINGET   0xF5DB  SINE    Calculate the sine of 'A'
\ // SINCOS   0xF5EF  ---     Calculate the sine and cosine of 'ANGLE'
\
\ // RSINA    0xF65B  MSINE   Multiply 'A' by previous sine value
\ // RSIN     0xF65D  LSINE   Multiply 'LEG' by previous sine value
\ // RCOSA    0xF661  MCSINE  Multiply 'A' by previous cosine value
\ // RCOS     0xF663  LCSINE  Multiply 'LEG' by previous cosine value

CODE _Rise_Run_Angle    __dp_ # PSHU,   C8 # LDX,   X DPR TFR,   A B EXG,   S ,++ ADDD,   Rise_Run_Angle JSR,   __dp_ # PULU,   NEXT ;C \ run  rise -- angle ; angle is from X axis
CODE _Get_Rise_Idx                                               A B EXG,                 Get_Rise_Idx   JSR,                   NEXT ;C \     angle -- value ; value  A=VALUE,B=SIGN/OVERFLOW
CODE _Get_Run_Idx                                                A B EXG,                 Get_Run_Idx    JSR,                   NEXT ;C \     angle -- value ; value  A=VALUE,B=SIGN/OVERFLOW
CODE _Rise_Run_Idx      __dpD # PSHU,   C8 # LDX,   X DPR TFR,                            Get_Rise_Run   JSR,   __dpD # PULU,   NEXT ;C \           --       ; Entry:Vec_Angle, Exit:Vec_Run_Index/Vec_Rise_Index(2 bytes each) Note: Rise_Run_Idx/Get_Rise_Run
CODE _Rise_Run_X        __dp_ # PSHU,   C8 # LDX,   X DPR TFR,   A B EXG,   S ,++ ADDD,   Rise_Run_X     JSR,   __dp_ # PULU,   NEXT ;C \ svv angle -- rise/run ; scaler_velocity_value
CODE _Rise_Run_Y        __dp_ # PSHU,   C8 # LDX,   X DPR TFR,   A B EXG,   S ,++ ADDD,   Rise_Run_Y     JSR,   __dp_ # PULU,   NEXT ;C \ svv angle -- rise/run ; scaler_velocity_value
CODE _Rise_Run_Len      __dp_ # PSHU,   C8 # LDX,   X DPR TFR,                            Rise_Run_Len   JSR,   __dp_ # PULU,   NEXT ;C \     angle -- rise/run ;
CODE _Rot_VL_ab         NEXT ;C
CODE _Rot_VL            NEXT ;C
CODE _Rot_VL_Mode       NEXT ;C
CODE _Rot_VL_M_dft      NEXT ;C
CODE _Xform_Run_a       NEXT ;C
CODE _Xform_Run         NEXT ;C
CODE _Xform_Rise_a      NEXT ;C
CODE _Xform_Rise        NEXT ;C

\ Memory management / Memory copy

CODE _Move_Mem_a_1      NEXT ;C \ Not need for Forth, use CMOVE or CMOVE> instead
CODE _Move_Mem_a        NEXT ;C \ Not need for Forth, use CMOVE or CMOVE> instead

\ Player option

CODE _Select_Game       _Ydp_ # PSHU,                            A B EXG,   S ,++ ADDD,     Select_Game    JSR,   ____D # PULS,   _Ydp_ # PULU, NEXT ;C \ #game_versions #players_max -- ;
CODE _Display_Option    U_dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D Y TFR,   ____D # PULS,   Display_Option JSR,   ____D # PULS,   U_dp_ # PULU, NEXT ;C \ option_val addr -- ;

\ Score

CODE _Clear_Score       D X TFR,   Clear_Score   JSR,   ____D # PULS,   NEXT ;C \    addr -- ;
CODE _Add_Score_a       U____ # PSHU,   D X TFR,   ____D # PULS,   D U TFR,   ____D # PULS,   Add_Score_a    JSR,   ____D # PULS,   U____ # PULU,   NEXT ;C \ binary# BCD ptr -- ;
CODE _Add_Score_d                       D X TFR,   ____D # PULS,                              Add_Score_d    JSR,   ____D # PULS,                   NEXT ;C \         BCD ptr -- ;
CODE _Strip_Zeros                       D X TFR,   ____D # PULS,                              Strip_Zeros    JSR,   ____D # PULS,                   NEXT ;C \      digit# ptr -- ; digit# 8 bit only
CODE _Compare_Score     U____ # PSHU,   D X TFR,   ____D # PULS,   D U TFR,                   Compare_Score  JSR,   A B EXG,        U____ # PULU,   NEXT ;C \       ptr2 ptr1 -- 0|1|2; Same|1>2|2>1
CODE _New_High_Score    U____ # PSHU,   D X TFR,   ____D # PULS,   D U TFR,                   New_High_Score JSR,   ____D # PULS,   U____ # PULU,   NEXT ;C \     addr2 addr1 -- ;

\ Vector object handling / Object collision detection

CODE _Obj_Will_Hit_u    _Y__D # PSHU,  A B EXG,   S ,++ ADDD,   D Y TFR,   ____D # PSHS,   S ,++ ADDD,   D X TFR,   ____D # PSHS,   NEXT ;C \ height/2 width/2 ptr>movement x_mis y_mis x_obj y_obj -- f=collided ;
CODE _Obj_Will_Hit      NEXT ;C \ ********** NOT DONE YET
CODE _Obj_Hit           NEXT ;C \ ********** NOT DONE YET

