\ Vectrex Forth Application

HEX

\ Uncomment all these for turnkey application i.e. without serial port
\ : KEY? 0 ;
\ : KEY 1 ;
\ : CR ;
\ : EMIT DROP ;
\ : U. DROP ;
\ : .S ;

HERE EQU HELLO-WORLD-STRING
S" HELLO WORLD" 80 C,

\ Reset and initialization
\ No test words for cold and warm start, as no params passed just run direct
: inits \ -- ;
  _Init_VIA  _Init_OS_RAM  _Init_OS
;

\ Calibration/Vector Reset
: c/vr \ -- ;
   _Wait_Recal
   _Set_Refresh
   _Recalibrate
   _Reset0Ref_D0
   _Check0Ref
   _Reset0Ref
   _Reset_Pen
   _Reset0Int
;

\ Read Buttons/mask. Display all the bytes that are set by the read.
\ The u. is displayed on the console at the end of the line as dump includes a CR.
\ The $0033 sets 3 & 4 to NOT be momentary read for _mask, default setting is $FF.
: rbm begin $33 _Read_Btns_mask u. Vec_Btn_State $10 dump key? until key drop ;
: rb  begin     _Read_Btns      u. Vec_Btn_State $10 dump key? until key drop ;

\ Joystick read routines
: jsetup \ -- ; Joystick setup
   $00   Vec_Joy_Resltn c! \ Set resolution $00=highest, $80=lowest
   $0103 Vec_Joy_Mux_1_X ! \ Set joystick read routines
   $0507 Vec_Joy_Mux_2_X ! \   to do X & Y for Joy 1 & 2
;

: jd \ -- ; \ Read Joystick Digital
   jsetup
   begin
      _Joy_Digital
      Vec_Joy_1_X $10 dump
      key?
   until
   key drop
;

: ja \ -- ; \ Read Joystick Analogue
   jsetup
   begin
      _Joy_Analog
      Vec_Joy_1_X $10 dump
      key?
   until
   key drop
;

\ Sound_Byte/_x/_raw and Clear_Sound test, i.e. a small music player
\ ymlen/data/regs loaded from external YM music file
: sb \ -- ;
   ymlen @ 0 do         \ for each line of the YM file (j loop)
      _Wait_Recal       \ one line per 20ms i.e. 50Hz
      $2A emit           \ show feedback asterisk at terminal
      $B 0 do           \ store a line of music data to PSG registers (i loop)
         ymdata j $B * + i + c@   \ -- data ; Get the data byte
         ymregs i + c@            \ -- data reg# ; Get the register#
\         _Sound_Byte              \ -- ; comment out _Sound_byte, _byte_x or
         Vec_Snd_Shadow _Sound_Byte_x \  or _byte_raw to test each of them
\         _Sound_Byte_raw          \  individually
      loop
      key? if key drop leave then \ exit loop if key pressed
   loop
   _Clear_Sound         \ set all registers to 0
;

\ Do_Sound/_x and Init_music_chk test word, i.e. built-in music and player
\ Not sure whath Init_Music and Init_Music_dft do, but interface same as _chk
: ds \ -- ;
   1 Vec_Music_Flag c!
   begin
\      yankee _Init_Music_chk
      music4 _Init_Music_chk \ music4 = scramble tune
      _Wait_Recal
\      _Do_Sound             \ comment out on of the Do_Sound/_x lines to test
       Vec_Snd_Shadow _Do_Sound_x
     key?
   until
   key drop
   _Clear_Sound             \ set all registers to 0
;

: imb \ -- ; Init_Music_Buf test word
   Vec_Music_Work 10 2A fill    \ Fill buffer so "It's full of stars"
   Vec_Music_Work 10 dump       \ Display buffer full of stars
   _Init_Music_Buf              \ Init buffer
   Vec_Music_Work 10 dump       \ Buffer now clear, except for reg#6=$3f
;

here equ ES_DATA
  %00111111 c, $01 c, $EF c, $02 c,

: es \ -- ; Explosion Sound test word
   ." Space to (re-)play explosion, any other key end"
   begin
      ES_DATA _Explosion_Snd
      _Wait_Recal
      _Do_Sound
      key? dup if                \ -- flag ;
         key $20 = if            \ -- flag ; key pressed was space?
            drop 0               \ -- ff ;
            -1 Vec_Expl_Flag c!  \ -- ff ; Start (re-)playing sound
         then
      then
   until
;

: box \ len_x len_y start_x start_y -- ;
   _Reset0Ref
   _Moveto_d  \ -- lx ly ;
   dup 0         _Draw_Line_d
   over 0 swap   _Draw_Line_d
   negate 0      _Draw_Line_d
   negate 0 swap _Draw_Line_d
;

: intensity \ -- ; Intensity words test
   begin
      _Wait_Recal
      _Intensity_7F 7f 7f -$40 -$40 box
      _Intensity_5F 5f 5f -$30 -$30 box
      _Intensity_3F 3f 3f -$20 -$20 box
      _Reset0Ref
      $3f -$30 do \ Display a list of 7 HELLO WORLDS with decreasing intensity
         i $40 + _Intensity_a
         -$45 i HELLO-WORLD-STRING 3 + _Print_Str_d
      $11 +loop   \ $11 spaces out the text nicely
      key?
   until
   key drop
;

here equ dot_list
       -50 c, -70 c,  \ seven dots, relative
       -40 c,  10 c,  \ position, Y, X
        0  c,  30 c,
        40 c,  10 c,
        10 c,  30 c,
         5 c,  30 c,
       -10 c,  40 c,

here equ dot_list_packet
 ff c,  70 c, -70 c,  \ seven dots, relative
 ff c, -40 c,  10 c,  \ position, Y, X
 ff c,  0  c,  30 c,
 ff c,  40 c,  10 c,
 ff c,  10 c,  30 c,
 ff c,   5 c,  30 c,
 ff c, -10 c,  40 c,
 01 c,                \ list terminator

\ Dot drawing tests. Displays an approximate mirrored version of Ursa Major.
\  Pad is a Forth temporary store area, mainly used for building strings.
: dots \ -- ;
   begin
      _Wait_Recal
      $40 VIA_t1_cnt_lo c!              \ set scaling factor
      _Intensity_5F                     \ set intensity, works with dwell

      _Dot_here                         \ dot in centre of screen

      $10 8 _Dot_d                      \ dot $10 right, $8 up from previous dot

      $1020 pad !                       \ setup yx co-ord parameter
      pad _Dot_ix                       \ dot $20 right, $10 up from previous dot

      $2040 pad !                       \ setup yx co-ord parameter
      5 pad _Dot_ix_b                   \ dot $40 right, $20 up, with dwell 5

      _Reset0Ref
      dot_list_packet _Dot_List_Reset   \ display dot list, using terminator

      _Reset0Ref
      6 Vec_Misc_Count c!               \ 7 dots total, starting from 0
      dot_list _Dot_List                \ display dot list, using counter

      key? \ hit a key to exit
   until
   key drop
;

: mt \ -- ; Moveto tests. Display a dot at each moveto position.
   begin
      _Wait_Recal
      _Intensity_5F

      \ Use the seven different moveto's to position and then draw seven dots
      \  in a horizontal line across display.
      $4000 pad !  -$4000 pad cell+ !
      pad _Moveto_x_7F _Dot_here

      _Reset0Ref
      -$30 $40 _Moveto_d_7F _Dot_here   \ x y on stack

      _Reset0Ref
      $20F0 pad !
      pad _Moveto_ix_ff _Dot_here       \ y x in pad

      _Reset0Ref
      $40F0 pad !
      pad _Moveto_ix_7F _Dot_here       \ y x in pad

      _Reset0Ref
      $4000 pad !
      $7F pad _Moveto_ix_b _Dot_here    \ y x in pad

      _Reset0Ref
      $4010 pad !
      pad _Moveto_ix _Dot_here          \ y x in pad

      _Reset0Ref
      $20 $40 _Moveto_d _Dot_here       \ x y on stack

      key? \ hit a key to exit
   until
   key drop
;

\                 width  height  rel x, rel y   Char 1 2      3      Terminator
here equ pstr_hwyx F8 c, 50 c,   70 c, -40 c,   41 c,  42 c,  43 c,  80 c, \ ABC
here equ pstr_yx                 50 c, -40 c,   44 c,  45 c,  46 c,  80 c, \ DEF
here equ pstr_d                                 47 c,  48 c,  49 c,  80 c, \ GHI
here equ pstr                                   4A c,  4B c,  4C c,  80 c, \ JKL
here equ plist_hw  F8 c, 50 c,  -10 c, -40 c,   4D c,  4E c,  4F c,  80 c, \ MNO
                   F8 c, 50 c,  -10 c, -00 c,   50 c,  51 c,  52 c,  80 c, \ PQR
                                                                     00 c, \ List end
here equ plist                  -30 c, -40 c,   53 c,  54 c,  55 c,  80 c, \ STU
                                -30 c, -00 c,   56 c,  57 c,  58 c,  80 c, \ VWX
                                                                     00 c, \ List end
here equ plist_chk              -50 c, -40 c,   59 c,  5A c,  5B c,  80 c, \ YZ[
                                -50 c, -00 c,   5C c,  5D c,  5E c,  80 c, \ \]^
                                                                     00 c, \ List end

: ps \ -- ; Print tests. Display string for each subroutine call type.
   begin
      _Wait_Recal
      _Intensity_7F
                                              pstr_hwyx _Print_Str_hwyx \ ABC
      _Reset0Ref                              pstr_yx   _Print_Str_yx   \ DEF
      _Reset0Ref                      -$40 30 pstr_d    _Print_Str_d    \ GHI
      _Reset0Ref   -$40 10 _Moveto_d          pstr      _Print_Str      \ JKL
      _Reset0Ref                              plist_hw  _Print_List_hw  \ MNO PQR
      _Reset0Ref                              plist     _Print_List     \ STU VWX
      _Reset0Ref                              plist_chk _Print_List_chk \ YZ[ \]^
      key?  \ press a key to end
   until
   key drop
;

\ Print Ships(_x) test word
\ Includes a dump of $10 bytes before and after the stack as I saw some
\ comments that Print_Ships BIOS/RUM routine underflows the stack, but it
\ looks OK to me
: psh \ -- ;
   cr s0 $10 - $20 dump     \ dump the stack, s0 is base of stack
   $30a0 pad !              \ store y x coords for _x variant

   begin
      _Wait_Recal
      _Intensity_7F
      #5  $68      pad _Print_Ships_x  \ #ships ship_char=spaceship ptr_to_xy_coords
      #10 $69 $a0 -$30 _Print_Ships    \ #ships ship_char=spaceman x y
      key?  \ press a key to end
   until
   key drop

   cr s0 $10 - $20 dump     \ redump the stack, check under stack is same
;



here equ plane1
-7f c, -7f c,   \ rel y, rel x, move to start
 00 c,  6E c,   \ rel y, rel x
 14 c, -1E c,
 00 c, -32 c,
 14 c, -1E c,
-28 c,  00 c,

here equ plane2
 05 c,  20 c,   \ count, scale
-7f c, -7f c,   \ rel y, rel x, move to start
 00 c,  6E c,   \ rel y, rel x
 14 c, -1E c,
 00 c, -32 c,
 14 c, -1E c,
-28 c,  00 c,

here equ plane3
 06 c,          \ count
-7f c, -7f c,   \ rel y, rel x, move to start
 00 c,  6E c,   \ rel y, rel x
 14 c, -1E c,
 00 c, -32 c,
 14 c, -1E c,
-28 c,  00 c,



: md_reset/move \ y -- ;
   _Reset0Ref 0 swap _Moveto_d
;

: md \ -- ; Move Draw tests.
   begin
      _Wait_Recal
      _Intensity_7F

       $20 VIA_t1_cnt_lo c! \ Set scaling factor
       $70 md_reset/move                                 plane3 _Mov_Draw_VLc_a
       $50 md_reset/move  5 Vec_Misc_Count c!   $20      plane1 _Mov_Draw_VL_b
       $30 md_reset/move                                 plane2 _Mov_Draw_VLcs
       $10 md_reset/move                        $20    5 plane1 _Mov_Draw_VL_ab
      -$10 md_reset/move                               5 plane1 _Mov_Draw_VL_a
      -$30 md_reset/move  5 Vec_Misc_Count c!            plane1 _Mov_Draw_VL
      -$50 md_reset/move  5 Vec_Misc_Count c!  -$7f -$7f plane1 _Mov_Draw_VL_d

      key?  \ press a key to end
   until
   key drop
;

here equ planeA
 00 c,  6E c,   \ rel y, rel x
 14 c, -1E c,
 00 c, -32 c,
 14 c, -1E c,
-28 c,  00 c,

here equ planeB
 04 c,  20 c,   \ count, scale
 00 c,  6E c,   \ rel y, rel x
 14 c, -1E c,
 00 c, -32 c,
 14 c, -1E c,
-28 c,  00 c,

here equ planeC
 04 c,          \ count
 00 c,  6E c,   \ rel y, rel x
 14 c, -1E c,
 00 c, -32 c,
 14 c, -1E c,
-28 c,  00 c,

here equ planeD
 02 c,  00 c,  6E c,   \ rel y, rel x
 FF c,  14 c, -1E c,
 00 c,  00 c, -32 c,
 FF c,  14 c, -1E c,
 FF c, -28 c,  00 c,
 01 c,

: md_reset/move2 \ y -- ;
   _Reset0Ref -$7F swap _Moveto_d
;

: draw \ -- ; Draw tests.
   begin
      _Wait_Recal
      _Intensity_7F

       $20 VIA_t1_cnt_lo c! \ Set scaling factor
       $F0 Vec_Pattern c!
       $70 md_reset/move                                 planeC _Draw_VLc
       $50 md_reset/move  4 Vec_Misc_Count c!   $20      planeA _Draw_VL_b
       $30 md_reset/move                                 planeB _Draw_VLcs
       $10 md_reset/move                        $20    4 planeA _Draw_VL_ab
      -$10 md_reset/move                               4 planeA _Draw_VL_a
      -$30 md_reset/move  4 Vec_Misc_Count c!            planeA _Draw_VL


\ _Draw_VLp_FF
\      -$50 md_reset/move                                 planeD _Draw_VLp_7F
\ _Draw_VLp_scale
\ _Draw_VLp_b
\ _Draw_VLp
       $70 md_reset/move2                                4 planeA _Draw_Pat_VL_a
       $50 md_reset/move2 4 Vec_Misc_Count c!              planeA _Draw_Pat_VL
       $30 md_reset/move2 5 Vec_Misc_Count c!     0      0 planeA _Draw_Pat_VL_d
\ _Draw_VL_mode
\ _Draw_Grid_VL

\ _Draw_Line_d

\ ;$FF ENABLES DOTTED LINE
\ ;$00 REQUESTS BLANK LINE
\ ;$02 IS SOLID LINE
\ ;$01 DELIMIT

           key?  \ press a key to end
   until
   key drop
;









\ Random(_3) test words. Visually, rand3 looks more random
: rand  begin cr _random   8 / 1+ 0 do ." *" loop key? until ;
: rand3 begin cr _random_3 8 / 1+ 0 do ." *" loop key? until ;

\ Dec_3/6_Counters test. Counters are 8 bit.
: dec36c \ -- ;
   6 0 do                         \ for 6 counters
      _random i Vec_Counters + c! \ store in a random number
   loop
   $ff 0 do                   \ decrement all counters to zero
      _Dec_3_Counters         \ dec first three
      _Dec_6_Counters         \ dec all six, thus first three dec'd twice
      Vec_Counters $10 dump   \ dump the counters to terminal
      key? if leave then      \ exit on key press
   loop
   key drop
;

\ Decrement Counters test. Decrements 1, then 2, 3, through to 6 counters.
: decc \ -- ;
   Vec_Counters 6 $ff fill           \ initialise counters to $ff
   Vec_Counters $10 dump             \ Display counters
   6 0 do                            \ Decrement 1 counter, 2 ,3...6
      i Vec_Counters _Dec_Counters   \
   loop
   Vec_Counters $10 dump             \ Display counters again
;

\ Delay_n etc test. Probably unlikey to be used by Forth directly.
: delays \ -- ;
   _Delay_3 _Delay_2 _Delay_1 _Delay_0 _Delay_RTS
   $ff _Delay_b \ Parameterised general delay
;

\ Bitmask_a test. Provide a number 1 to 8, get back the power of 2
\ Forth alternative is LSHIFT e.g. 1 5 lshift = 32
: bma \ bit_number -- bit_mask ; \ output gives 1 2 4 8 16 32 64 128
   8 0 do i _Bitmask_a u. loop
;

\ -----------------------------------------------------------------------------

: DT \ -- ; Display turtles using vector list with two different BIOS calls
   BEGIN
      1+ DUP U.
      _Wait_Recal
      _Intensity_7F

      -1f 1f _Moveto_d
      20 17 TURTLE _Draw_VL_ab    \ scale_factor count_of_vectors VL_addr

      0 -7f _Moveto_d
      0 -7f _Moveto_d
      TURTLEcs _Draw_VLcs         \ draw vector list using BIOS call

      KEY?
  UNTIL
  key drop
  DROP
;

: DG \ -- ; Draw grid line by line
  FF VIA_t1_cnt_lo C!   \ set scaling factor
  0                     \ initial frame counter
  BEGIN
    1 + DUP U.          \ Send a frame counter to display on the terminal
    _Wait_Recal         \
    _Intensity_7F       \

    0 Vec_Misc_Count C! \ Vec_Misc_Count must be set to 0 to draw one vector
    \ start by moving beam to bottom left corner of grid, then zig-zag the lines
    -3F -3F _Moveto_d   0 7F _Draw_Line_d
     20  80 _Moveto_d   0 7F _Draw_Line_d
     20  80 _Moveto_d   0 7F _Draw_Line_d
     20  80 _Moveto_d   0 7F _Draw_Line_d
     20  80 _Moveto_d   0 7F _Draw_Line_d
     80  80 _Moveto_d   7F 0 _Draw_Line_d
     80  20 _Moveto_d   7F 0 _Draw_Line_d
     80  20 _Moveto_d   7F 0 _Draw_Line_d
     80  20 _Moveto_d   7F 0 _Draw_Line_d
     80  20 _Moveto_d   7F 0 _Draw_Line_d

    KEY?                \ terminal key press exits loop
  UNTIL
  key drop
  DROP
;

: (dg2) \ -- ;
    0 Vec_Misc_Count C! \ Vec_Misc_Count must be set to 0 to draw one vector
    _Recalibrate   -3F -3F _Moveto_d   0  7F _Draw_Line_d
    _Recalibrate   -1F -3F _Moveto_d   0  7F _Draw_Line_d
    _Recalibrate     0 -3F _Moveto_d   0  7F _Draw_Line_d
    _Recalibrate    1F -3F _Moveto_d   0  7F _Draw_Line_d
    _Recalibrate    3F -3F _Moveto_d   0  7F _Draw_Line_d
    _Recalibrate    3F  3F _Moveto_d   -7F 0 _Draw_Line_d
    _Recalibrate    3F  1F _Moveto_d   -7F 0 _Draw_Line_d
    _Recalibrate    3F   0 _Moveto_d   -7F 0 _Draw_Line_d
    _Recalibrate    3F -1F _Moveto_d   -7F 0 _Draw_Line_d
    _Recalibrate    3F -3F _Moveto_d   -7F 0 _Draw_Line_d
;

: DG2 \ -- ; Draw grid line by line, but no zig zag, more optimal drawing
  FF VIA_t1_cnt_lo C!   \ set scaling factor
  0                     \ initial frame counter
  BEGIN
    1 + DUP U.          \ Send a frame counter to display on the terminal
    _Wait_Recal         \
    _Intensity_7F       \
    (dg2)
    KEY?                \ terminal key press exits loop
  UNTIL
  key drop
  DROP
;

: DG3 \ -- ; Draw grid with lops, no hardcoding of co-ordinates
  FF VIA_t1_cnt_lo C! \ set scaling factor
  0
  BEGIN
     1 + DUP U.          \ Send a frame counter to display on the terminal
     _Wait_Recal         \
     _Intensity_7F       \

     \ Draw vertical lines
     0 Vec_Misc_Count C! \ must be set to 0 to draw one vector
     -3F -3F _Moveto_d   \ move beam to bottom left corner
     5 0 DO
\        0 I 2 MOD 0= IF 7F ELSE 80 THEN _Draw_Line_d
        0 I 2 MOD -FF * 7F + FF AND _Draw_Line_d \ SLOWER
        I 4 < IF 20 0 _Moveto_d THEN
     LOOP

     \ Draw vertical lines
     0 Vec_Misc_Count C! \ must be set to 0 to draw one vector
     80 80 _Moveto_d   \ move beam to bottom left corner
     5 0 DO
\        I 2 MOD 0= IF 7F ELSE 80 THEN 0 _Draw_Line_d
        I 2 MOD -FF * 7F + FF AND 0 _Draw_Line_d \ SLOWER
        I 4 < IF 0 20 _Moveto_d THEN
     LOOP

     KEY?                \ terminal key press exits loop
  UNTIL
  key drop
  DROP
;

: HELLO \ -- ; Hello World, to vector display, to interactive
             \ terminal and VecFever LED
  BEGIN
     _Wait_Recal
     _Intensity_5F
     CR
     31 -30 DO
        ." Hello World! "
        I 7 MOD 10 * v4eLEDReg C!
        51 -50 DO
           J 40 - I HELLO-WORLD-STRING 3 + _Print_Str_d
        LOOP
     20 +LOOP
     CR ." Escape to end, any other to continue"
     KEY 1B =
   UNTIL
;


