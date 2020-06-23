\ This file contains includes for vectrex BIOS functions and variables
\ it was written by Bruce Tomlin, slightly changed by Malban
\ then converted to Forth by Phillip Eaton

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



F000   EQU   Cold_Start
F06C   EQU   Warm_Start
F14C   EQU   Init_VIA
F164   EQU   Init_OS_RAM
F18B   EQU   Init_OS
F192   EQU   Wait_Recal
F1A2   EQU   Set_Refresh
F1AA   EQU   DP_to_D0
F1AF   EQU   DP_to_C8
F1B4   EQU   Read_Btns_Mask
F1BA   EQU   Read_Btns
F1F5   EQU   Joy_Analog
F1F8   EQU   Joy_Digital
F256   EQU   Sound_Byte
F259   EQU   Sound_Byte_x
F25B   EQU   Sound_Byte_raw
F272   EQU   Clear_Sound
F27D   EQU   Sound_Bytes
F284   EQU   Sound_Bytes_x
F289   EQU   Do_Sound
F28C   EQU   Do_Sound_x
F29D   EQU   Intensity_1F
F2A1   EQU   Intensity_3F
F2A5   EQU   Intensity_5F
F2A9   EQU   Intensity_7F
F2AB   EQU   Intensity_a
F2BE   EQU   Dot_ix_b
F2C1   EQU   Dot_ix
F2C3   EQU   Dot_d
F2C5   EQU   Dot_here
F2D5   EQU   Dot_List
F2DE   EQU   Dot_List_Reset
F2E6   EQU   Recalibrate
F2F2   EQU   Moveto_x_7F
F2FC   EQU   Moveto_d_7F
F308   EQU   Moveto_ix_FF
F30C   EQU   Moveto_ix_7F
F30E   EQU   Moveto_ix_b
F310   EQU   Moveto_ix
F312   EQU   Moveto_d
F34A   EQU   Reset0Ref_D0
F34F   EQU   Check0Ref
F354   EQU   Reset0Ref
F35B   EQU   Reset_Pen
F36B   EQU   Reset0Int
F373   EQU   Print_Str_hwyx
F378   EQU   Print_Str_yx
F37A   EQU   Print_Str_d
F385   EQU   Print_List_hw
F38A   EQU   Print_List
F38C   EQU   Print_List_chk
F391   EQU   Print_Ships_x
F393   EQU   Print_Ships
F3AD   EQU   Mov_Draw_VLc_a
F3B1   EQU   Mov_Draw_VL_b
F3B5   EQU   Mov_Draw_VLcs
F3B7   EQU   Mov_Draw_VL_ab
F3B9   EQU   Mov_Draw_VL_a
F3BC   EQU   Mov_Draw_VL
F3BE   EQU   Mov_Draw_VL_d
F3CE   EQU   Draw_VLc
F3D2   EQU   Draw_VL_b
F3D6   EQU   Draw_VLcs
F3D8   EQU   Draw_VL_ab
F3DA   EQU   Draw_VL_a
F3DD   EQU   Draw_VL
F3DF   EQU   Draw_Line_d
F404   EQU   Draw_VLp_FF
F408   EQU   Draw_VLp_7F
F40C   EQU   Draw_VLp_scale
F40E   EQU   Draw_VLp_b
F410   EQU   Draw_VLp
F434   EQU   Draw_Pat_VL_a
F437   EQU   Draw_Pat_VL
F439   EQU   Draw_Pat_VL_d
F46E   EQU   Draw_VL_mode
F495   EQU   Print_Str
F511   EQU   Random_3
F517   EQU   Random
F533   EQU   Init_Music_Buf
F53F   EQU   Clear_x_b
F542   EQU   Clear_C8_RAM
F545   EQU   Clear_x_256
F548   EQU   Clear_x_d
F550   EQU   Clear_x_b_80
F552   EQU   Clear_x_b_a
F55A   EQU   Dec_3_Counters
F55E   EQU   Dec_6_Counters
F563   EQU   Dec_Counters
F56D   EQU   Delay_3
F571   EQU   Delay_2
F575   EQU   Delay_1
F579   EQU   Delay_0
F57A   EQU   Delay_b
F57D   EQU   Delay_RTS
F57E   EQU   Bitmask_a
F584   EQU   Abs_a_b
F58B   EQU   Abs_b
F593   EQU   Rise_Run_Angle
F5D9   EQU   Get_Rise_Idx
F5DB   EQU   Get_Run_Idx
F5EF   EQU   Get_Rise_Run
F5FF   EQU   Rise_Run_X
F601   EQU   Rise_Run_Y
F603   EQU   Rise_Run_Len

F610   EQU   Rot_VL_ab
F616   EQU   Rot_VL
F61F   EQU   Rot_VL_Mode
F62B   EQU   Rot_VL_M_dft
F637   EQU   Rot_VL_dft

\ F61F   EQU   Rot_VL_Mode_a
\ F62B   EQU   Rot_VL_Mode

F65B   EQU   Xform_Run_a
F65D   EQU   Xform_Run
F661   EQU   Xform_Rise_a
F663   EQU   Xform_Rise
F67F   EQU   Move_Mem_a_1
F683   EQU   Move_Mem_a
F687   EQU   Init_Music_chk
F68D   EQU   Init_Music
F692   EQU   Init_Music_x
F7A9   EQU   Select_Game
F835   EQU   Display_Option
F84F   EQU   Clear_Score
F85E   EQU   Add_Score_a
F87C   EQU   Add_Score_d
F8B7   EQU   Strip_Zeros
F8C7   EQU   Compare_Score
F8D8   EQU   New_High_Score
F8E5   EQU   Obj_Will_Hit_u
F8F3   EQU   Obj_Will_Hit
F8FF   EQU   Obj_Hit
F92E   EQU   Explosion_Snd
FF9F   EQU   Draw_Grid_VL

FD0D   EQU   music1
FD1D   EQU   music2
FD81   EQU   music3
FDD3   EQU   music4
FE38   EQU   music5
FE76   EQU   music6
FEC6   EQU   music7
FEF8   EQU   music8
FF26   EQU   music9
FF44   EQU   musica
FF62   EQU   musicb
FF7A   EQU   musicc
FF8F   EQU   musicd
F9F4   EQU   Char_Table
FBD4   EQU   Char_Table_End

\ END OF FILE