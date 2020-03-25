\ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
\ ; this file contains includes for vectrex BIOS functions and variables      ;
\ ; it was written by Bruce Tomlin, slightly changed by Malban                ;
\ ; then converted to Forth by Phillip Eaton                                  ;
\ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

HEX

C800   EQU   Vec_Snd_Shadow  \ Shadow of sound chip registers (15 bytes)
C80F   EQU   Vec_Btn_State   \ Current state of all joystick buttons
C810   EQU   Vec_Prev_Btns   \ Previous state of all joystick buttons
C811   EQU   Vec_Buttons     \ Current toggle state of all buttons
C812   EQU   Vec_Button_1_1  \ Current toggle state of stick 1 button 1
C813   EQU   Vec_Button_1_2  \ Current toggle state of stick 1 button 2
C814   EQU   Vec_Button_1_3  \ Current toggle state of stick 1 button 3
C815   EQU   Vec_Button_1_4  \ Current toggle state of stick 1 button 4
C816   EQU   Vec_Button_2_1  \ Current toggle state of stick 2 button 1
C817   EQU   Vec_Button_2_2  \ Current toggle state of stick 2 button 2
C818   EQU   Vec_Button_2_3  \ Current toggle state of stick 2 button 3
C819   EQU   Vec_Button_2_4  \ Current toggle state of stick 2 button 4
C81A   EQU   Vec_Joy_Resltn  \ Joystick A/D resolution ($80=min $00=max)
C81B   EQU   Vec_Joy_1_X     \ Joystick 1 left/right
C81C   EQU   Vec_Joy_1_Y     \ Joystick 1 up/down
C81D   EQU   Vec_Joy_2_X     \ Joystick 2 left/right
C81E   EQU   Vec_Joy_2_Y     \ Joystick 2 up/down
C81F   EQU   Vec_Joy_Mux     \ Joystick enable/mux flags (4 bytes)
C81F   EQU   Vec_Joy_Mux_1_X \ Joystick 1 X enable/mux flag (=1)
C820   EQU   Vec_Joy_Mux_1_Y \ Joystick 1 Y enable/mux flag (=3)
C821   EQU   Vec_Joy_Mux_2_X \ Joystick 2 X enable/mux flag (=5)
C822   EQU   Vec_Joy_Mux_2_Y \ Joystick 2 Y enable/mux flag (=7)
C823   EQU   Vec_Misc_Count  \ Misc counter/flag byte, zero when not in use
C824   EQU   Vec_0Ref_Enable \ Check0Ref enable flag
C825   EQU   Vec_Loop_Count  \ Loop counter word (incremented in Wait_Recal)
C827   EQU   Vec_Brightness  \ Default brightness
C828   EQU   Vec_Dot_Dwell   \ Dot dwell time?
C829   EQU   Vec_Pattern     \ Dot pattern (bits)
C82A   EQU   Vec_Text_HW     \ Default text height and width
C82A   EQU   Vec_Text_Height \ Default text height
C82B   EQU   Vec_Text_Width  \ Default text width
C82C   EQU   Vec_Str_Ptr     \ Temporary string pointer for Print_Str
C82E   EQU   Vec_Counters    \ Six bytes of counters
C82E   EQU   Vec_Counter_1   \ First  counter byte
C82F   EQU   Vec_Counter_2   \ Second counter byte
C830   EQU   Vec_Counter_3   \ Third  counter byte
C831   EQU   Vec_Counter_4   \ Fourth counter byte
C832   EQU   Vec_Counter_5   \ Fifth  counter byte
C833   EQU   Vec_Counter_6   \ Sixth  counter byte
C834   EQU   Vec_RiseRun_Tmp \ Temp storage word for rise/run
C836   EQU   Vec_Angle       \ Angle for rise/run and rotation calculations
C837   EQU   Vec_Run_Index   \ Index pair for run
\ C839         *               \ Pointer to copyright string during startup
C839   EQU   Vec_Rise_Index  \ Index pair for rise
\ C83B         *               \ High score cold-start flag (=0 if valid)
C83B   EQU   Vec_RiseRun_Len \ length for rise/run
\ C83C         *               \ temp byte
C83D   EQU   Vec_Rfrsh       \ Refresh time (divided by 1.5MHz)
C83D   EQU   Vec_Rfrsh_lo    \ Refresh time low byte
C83E   EQU   Vec_Rfrsh_hi    \ Refresh time high byte
C83F   EQU   Vec_Music_Work  \ Music work buffer (14 bytes, backwards?)
C842   EQU   Vec_Music_Wk_A  \         register 10
\ C843         *               \         register 9
\ C844         *               \         register 8
C845   EQU   Vec_Music_Wk_7  \         register 7
C846   EQU   Vec_Music_Wk_6  \         register 6
C847   EQU   Vec_Music_Wk_5  \         register 5
\ C848         *               \         register 4
\ C849         *               \         register 3
\ C84A         *               \         register 2
C84B   EQU   Vec_Music_Wk_1  \         register 1
\ C84C         *               \         register 0
C84D   EQU   Vec_Freq_Table  \ Pointer to note-to-frequency table (normally $FC8D)
C84F   EQU   Vec_Max_Players \ Maximum number of players for Select_Game
C850   EQU   Vec_Max_Games   \ Maximum number of games for Select_Game
C84F   EQU   Vec_ADSR_Table  \ Storage for first music header word (ADSR table)
C851   EQU   Vec_Twang_Table \ Storage for second music header word ('twang' table)
C853   EQU   Vec_Music_Ptr   \ Music data pointer
C853   EQU   Vec_Expl_ChanA  \ Used by Explosion_Snd - bit for first channel used?
C854   EQU   Vec_Expl_Chans  \ Used by Explosion_Snd - bits for all channels used?
C855   EQU   Vec_Music_Chan  \ Current sound channel number for Init_Music
C856   EQU   Vec_Music_Flag  \ Music active flag ($00=off $01=start $80=on)
C857   EQU   Vec_Duration    \ Duration counter for Init_Music
C858   EQU   Vec_Music_Twang \ 3 word 'twang' table used by Init_Music
C858   EQU   Vec_Expl_1      \ Four bytes copied from Explosion_Snd's U-reg parameters
C859   EQU   Vec_Expl_2      \
C85A   EQU   Vec_Expl_3      \
C85B   EQU   Vec_Expl_4      \
C85C   EQU   Vec_Expl_Chan   \ Used by Explosion_Snd - channel number in use?
C85D   EQU   Vec_Expl_ChanB  \ Used by Explosion_Snd - bit for second channel used?
C85E   EQU   Vec_ADSR_Timers \ ADSR timers for each sound channel (3 bytes)
C861   EQU   Vec_Music_Freq  \ Storage for base frequency of each channel (3 words)
\ C85E         *               \ Scratch 'score' storage for Display_Option (7 bytes)
C867   EQU   Vec_Expl_Flag   \ Explosion_Snd initialization flag?
\ C876   C868...*             \ Unused?
C877   EQU   Vec_Expl_Timer  \ Used by Explosion_Snd
\ C878         *               \ Unused?
C879   EQU   Vec_Num_Players \ Number of players selected in Select_Game
C87A   EQU   Vec_Num_Game    \ Game number selected in Select_Game
C87B   EQU   Vec_Seed_Ptr    \ Pointer to 3-byte random number seed (=$C87D)
C87D   EQU   Vec_Random_Seed \ Default 3-byte random number seed
\
\ *    $C880 - $CBEA is user RAM
\
CBEA   EQU   Vec_Default_Stk \ Default top-of-stack
CBEB   EQU   Vec_High_Score  \ High score storage (7 bytes)
CBF2   EQU   Vec_SWI3_Vector \ SWI2/SWI3 interrupt vector (3 bytes)
CBF2   EQU   Vec_SWI2_Vector \ SWI2/SWI3 interrupt vector (3 bytes)
CBF5   EQU   Vec_FIRQ_Vector \ FIRQ interrupt vector (3 bytes)
CBF8   EQU   Vec_IRQ_Vector  \ IRQ interrupt vector (3 bytes)
CBFB   EQU   Vec_SWI_Vector  \ SWI/NMI interrupt vector (3 bytes)
CBFB   EQU   Vec_NMI_Vector  \ SWI/NMI interrupt vector (3 bytes)
CBFE   EQU   Vec_Cold_Flag   \ Cold start flag (warm start if = $7321)

D000   EQU   VIA_port_b      \ VIA port B data I/O register
\ *       0 sample/hold (0=enable  mux 1=disable mux)
\ *       1 mux sel 0
\ *       2 mux sel 1
\ *       3 sound BC1
\ *       4 sound BDIR
\ *       5 comparator input
\ *       6 external device (slot pin 35) initialized to input
\ *       7 /RAMP
D001   EQU   VIA_port_a      \ VIA port A data I/O register (handshaking)
D002   EQU   VIA_DDR_b       \ VIA port B data direction register (0=input 1=output)
D003   EQU   VIA_DDR_a       \ VIA port A data direction register (0=input 1=output)
D004   EQU   VIA_t1_cnt_lo   \ VIA timer 1 count register lo (scale factor)
D005   EQU   VIA_t1_cnt_hi   \ VIA timer 1 count register hi
D006   EQU   VIA_t1_lch_lo   \ VIA timer 1 latch register lo
D007   EQU   VIA_t1_lch_hi   \ VIA timer 1 latch register hi
D008   EQU   VIA_t2_lo       \ VIA timer 2 count/latch register lo (refresh)
D009   EQU   VIA_t2_hi       \ VIA timer 2 count/latch register hi
D00A   EQU   VIA_shift_reg   \ VIA shift register
D00B   EQU   VIA_aux_cntl    \ VIA auxiliary control register
\ *       0 PA latch enable
\ *       1 PB latch enable
\ *       2 \                     110=output to CB2 under control of phase 2 clock
\ *       3  > shift register control     (110 is the only mode used by the Vectrex ROM)
\ *       4 /
\ *       5 0=t2 one shot                 1=t2 free running
\ *       6 0=t1 one shot                 1=t1 free running
\ *       7 0=t1 disable PB7 output       1=t1 enable PB7 output
D00C   EQU   VIA_cntl        \ VIA control register
\ *       0 CA1 control     CA1 -> SW7    0=IRQ on low 1=IRQ on high
\ *       1 \
\ *       2  > CA2 control  CA2 -> /ZERO  110=low 111=high
\ *       3 /
\ *       4 CB1 control     CB1 -> NC     0=IRQ on low 1=IRQ on high
\ *       5 \
\ *       6  > CB2 control  CB2 -> /BLANK 110=low 111=high
\ *       7 /
D00D   EQU   VIA_int_flags   \ VIA interrupt flags register
\ *               bit                             cleared by
\ *       0 CA2 interrupt flag            reading or writing port A I/O
\ *       1 CA1 interrupt flag            reading or writing port A I/O
\ *       2 shift register interrupt flag reading or writing shift register
\ *       3 CB2 interrupt flag            reading or writing port B I/O
\ *       4 CB1 interrupt flag            reading or writing port A I/O
\ *       5 timer 2 interrupt flag        read t2 low or write t2 high
\ *       6 timer 1 interrupt flag        read t1 count low or write t1 high
\ *       7 IRQ status flag               write logic 0 to IER or IFR bit
D00E   EQU   VIA_int_enable  \ VIA interrupt enable register
\ *       0 CA2 interrupt enable
\ *       1 CA1 interrupt enable
\ *       2 shift register interrupt enable
\ *       3 CB2 interrupt enable
\ *       4 CB1 interrupt enable
\ *       5 timer 2 interrupt enable
\ *       6 timer 1 interrupt enable
\ *       7 IER set/clear control
D00F   EQU   VIA_port_a_nohs \ VIA port A data I/O register (no handshaking)

                             \ DP    Inreg   Outreg  Trashreg
                             \ ----- ------- ------- --------
F000   EQU   Cold_Start      \ --            --      --
F06C   EQU   Warm_Start      \ --            --      --
F14C   EQU   Init_VIA        \ =D0           DP      D,X
F164   EQU   Init_OS_RAM     \ =C8           DP      D,X
F18B   EQU   Init_OS         \ =D0           DP      D,X
F192   EQU   Wait_Recal      \ =D0           DP      D,X
F1A2   EQU   Set_Refresh     \ D0                    D
F1AA   EQU   DP_to_D0        \ =D0           A,DP
F1AF   EQU   DP_to_C8        \ =C8           A,DP
F1B4   EQU   Read_Btns_Mask  \ D0    A       A       B,X
F1BA   EQU   Read_Btns       \ D0            A       B,X
F1F5   EQU   Joy_Analog      \ D0                    D,X
F1F8   EQU   Joy_Digital     \ D0                    D,X
F256   EQU   Sound_Byte      \ D0            X       D
F259   EQU   Sound_Byte_x    \ D0    A,B,X           D
F25B   EQU   Sound_Byte_raw  \ D0
F272   EQU   Clear_Sound     \ D0                    D,X
F27D   EQU   Sound_Bytes     \ D0    U               D,X,U
F284   EQU   Sound_Bytes_x   \ D0    X,U             D,X,U
F289   EQU   Do_Sound        \ D0                    D,X,U
F28C   EQU   Do_Sound_x      \ D0                    D,X,U
F29D   EQU   Intensity_1F    \ D0                    D
F2A1   EQU   Intensity_3F    \ D0                    D
F2A5   EQU   Intensity_5F    \ D0                    D
F2A9   EQU   Intensity_7F    \ D0                    D
F2AB   EQU   Intensity_a     \ D0    A               D
F2BE   EQU   Dot_ix_b        \ D0    B,X             D
F2C1   EQU   Dot_ix          \ D0    X               D
F2C3   EQU   Dot_d           \ D0    A,B             D
F2C5   EQU   Dot_here        \ D0                    D
F2D5   EQU   Dot_List        \ D0    X       X       D
F2DE   EQU   Dot_List_Reset  \ D0    X       X       D
F2E6   EQU   Recalibrate     \ D0                    D,X
F2F2   EQU   Moveto_x_7F     \ D0    X               D
F2FC   EQU   Moveto_d_7F     \ D0    A,B             D
F308   EQU   Moveto_ix_FF    \ D0    X       X       D
F30C   EQU   Moveto_ix_7F    \ D0    X       X       D
F30E   EQU   Moveto_ix_b     \ D0    X,A     X       D
F310   EQU   Moveto_ix       \ D0    X       X       D
F312   EQU   Moveto_d        \ D0            A,B     D
F34A   EQU   Reset0Ref_D0    \ =D0           DP      D
F34F   EQU   Check0Ref       \ D0                    D
F354   EQU   Reset0Ref       \ D0                    D
F35B   EQU   Reset_Pen       \ D0                    D
F36B   EQU   Reset0Int       \ D0                    D
F373   EQU   Print_Str_hwyx  \ D0    U       U       D,X
F378   EQU   Print_Str_yx    \ D0    U       U       D,X
F37A   EQU   Print_Str_d     \ D0    A,B,U   U       D,X
F385   EQU   Print_List_hw   \ D0    U       U       D,X
F38A   EQU   Print_List      \ D0    U       U       D,X
F38C   EQU   Print_List_chk  \ D0    U       U       D,X
F391   EQU   Print_Ships_x   \ D0    A,B,X           D,X,U
F393   EQU   Print_Ships     \ D0    A,B,X           D,X,U
F3AD   EQU   Mov_Draw_VLc_a  \ D0    X       X       D       count y x y x ...
F3B1   EQU   Mov_Draw_VL_b   \ D0    B,X     X       D       y x y x ...
F3B5   EQU   Mov_Draw_VLcs   \ D0    X       X       D       count scale y x ...
F3B7   EQU   Mov_Draw_VL_ab  \ D0    A,B,X   X       D       y x y x ...
F3B9   EQU   Mov_Draw_VL_a   \ D0    A,X     X       D       y x y x ...
F3BC   EQU   Mov_Draw_VL     \ D0    X       X       D       y x y x ...
F3BE   EQU   Mov_Draw_VL_d   \ D0    D,X     X       D       y x y x ...
F3CE   EQU   Draw_VLc        \ D0    X       X       D       count y x y x ...
F3D2   EQU   Draw_VL_b       \ D0    B,X     X       D       y x y x ...
F3D6   EQU   Draw_VLcs       \ D0    X       X       D       count scale y x ...
F3D8   EQU   Draw_VL_ab      \ D0    A,B,X   X       D       y x y x ...
F3DA   EQU   Draw_VL_a       \ D0    A,X     X       D       y x y x ...
F3DD   EQU   Draw_VL         \ D0    X       X       D       y x y x ...
F3DF   EQU   Draw_Line_d     \ D0    A,B     X       D       y x y x ...
F404   EQU   Draw_VLp_FF     \ D0    X       X       D       pat y x ... $01
F408   EQU   Draw_VLp_7F     \ D0    X       X       D       pat y x ... $01
F40C   EQU   Draw_VLp_scale  \ D0    X       X       D       scale pat y x ... $01
F40E   EQU   Draw_VLp_b      \ D0    B,X     X       D       pat y x ... $01
F410   EQU   Draw_VLp        \ D0    X       X       D       pat y x ... $01
F434   EQU   Draw_Pat_VL_a   \ D0    A,X     X       D       y x y x ...
F437   EQU   Draw_Pat_VL     \ D0    X       X       D       y x y x ...
F439   EQU   Draw_Pat_VL_d   \ D0    D,X     X       D       y x y x ...
F46E   EQU   Draw_VL_mode    \ D0    X       X       D       mode y x ... $01
F495   EQU   Print_Str       \ D0    U       U       D,X
F511   EQU   Random_3        \ --            A
F517   EQU   Random          \ --            A
F533   EQU   Init_Music_Buf  \ --                    D,X
F53F   EQU   Clear_x_b       \ --    B,X     D
F542   EQU   Clear_C8_RAM    \ --            D,X
F545   EQU   Clear_x_256     \ --    X       D
F548   EQU   Clear_x_d       \ --    D,X     D
F550   EQU   Clear_x_b_80    \ --    B,X     A,B
F552   EQU   Clear_x_b_a     \ --    A,B,X   B
F55A   EQU   Dec_3_Counters  \ --            X,B
F55E   EQU   Dec_6_Counters  \ --            X,B
F563   EQU   Dec_Counters    \ --    B,X     B
F56D   EQU   Delay_3         \ --            B               30 cycles
F571   EQU   Delay_2         \ --            B               25 cycles
F575   EQU   Delay_1         \ --            B               20 cycles
F579   EQU   Delay_0         \ --            B               12 cycles
F57A   EQU   Delay_b         \ --    B       B               5*B + 10 cycles
F57D   EQU   Delay_RTS       \ --                            5 cycles
F57E   EQU   Bitmask_a       \ --    A       A       X
F584   EQU   Abs_a_b         \ --    A,B     A,B
F58B   EQU   Abs_b           \ --    B       B
F593   EQU   Rise_Run_Angle  \ C8    A,B     A,B
F5D9   EQU   Get_Rise_Idx    \ --    A       A,B     X
F5DB   EQU   Get_Run_Idx     \ --    A       A,B     X
F5EF   EQU   Get_Rise_Run    \ C8                    D
F5FF   EQU   Rise_Run_X      \ C8    A,B
F601   EQU   Rise_Run_Y      \ C8    A,B
F603   EQU   Rise_Run_Len    \ C8    A,B     A,B

F610   EQU   Rot_VL_ab       \ =C8   A,B,X,U DP,X,U  D       y x y x ...
F616   EQU   Rot_VL          \ =C8   X,U     DP,X,U  D       y x y x ...
F61F   EQU   Rot_VL_Mode     \ C8    A,X,U   X,U     D       mode y x ... $01
F62B   EQU   Rot_VL_M_dft    \ C8    X,U     X,U     D       mode y x ... $01
F637   EQU   Rot_VL_dft      \ (Needs chceking)

\ F61F   EQU   Rot_VL_Mode_a   \ (Needs chceking)
\ F62B   EQU   Rot_VL_Mode     \ (Needs chceking)

F65B   EQU   Xform_Run_a     \ C8    A       A       B
F65D   EQU   Xform_Run       \ C8            A       B
F661   EQU   Xform_Rise_a    \ C8    A       A       B
F663   EQU   Xform_Rise      \ C8            A       B
F67F   EQU   Move_Mem_a_1    \ --    A,X,U   A,B
F683   EQU   Move_Mem_a      \ --    A,X,U   A,B
F687   EQU   Init_Music_chk  \ C8    U               D,X,Y,U
F68D   EQU   Init_Music      \ C8    U               D,X,Y,U
F692   EQU   Init_Music_x    \ C8    U               D,X,Y,U
F7A9   EQU   Select_Game     \ =C8   A,B     DP      D,X,Y
F835   EQU   Display_Option  \ D0    A,Y             D,X,U
F84F   EQU   Clear_Score     \ --    X               D
F85E   EQU   Add_Score_a     \ --    A,X,U           D
F87C   EQU   Add_Score_d     \ --    D,X,U           D
F8B7   EQU   Strip_Zeros     \ --    B,X             D
F8C7   EQU   Compare_Score   \ --    X,U     A       B
F8D8   EQU   New_High_Score  \ --    X,U             X,U,D
F8E5   EQU   Obj_Will_Hit_u  \ --    D,X,Y,U C-flag
F8F3   EQU   Obj_Will_Hit    \ --    D,X,Y,U C-flag
F8FF   EQU   Obj_Hit         \ --    D,X,Y   C-flag
F92E   EQU   Explosion_Snd   \ C8    U               D,X
FF9F   EQU   Draw_Grid_VL    \ D0    X,Y     X,Y     D

FD0D   EQU   music1          \
FD1D   EQU   music2          \
FD81   EQU   music3          \
FDD3   EQU   music4          \
FE38   EQU   music5          \
FE76   EQU   music6          \
FEC6   EQU   music7          \
FEF8   EQU   music8          \
FF26   EQU   music9          \
FF44   EQU   musica          \
FF62   EQU   musicb          \
FF7A   EQU   musicc          \
FF8F   EQU   musicd          \
F9F4   EQU   Char_Table      \
FBD4   EQU   Char_Table_End  \

\ END OF FILE