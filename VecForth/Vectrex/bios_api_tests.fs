\ Vectrex Forth Application

HEX

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

: ds \ -- ; Do_Sound test word
   1 Vec_Music_Flag c!
   begin
\      yankee _Init_Music_chk
      music1 _Init_Music_chk
      _Wait_Recal
      _Do_Sound
     key?
   until
   key drop
;

here equ ES_DATA
$3F c, $01 c, $EF c, $02 c,

: es \ -- ; Explosion Sound test word
   ." Space to (re-)play explosion, any other key end"
   Begin
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

\ Sound_Byte test, i.e. a small music player
\ ymlen/data/regs loaded from external YM music file
: sb \ -- ;
   ymlen @ 0 do         \ for each line of the YM file (j loop)
      _Wait_Recal       \ one line per 20ms i.e. 50Hz
      ." *"             \ show feedback at terminal
      $B 0 do           \ store a line of music data to PSG registers (i loop)
         ymdata j $B * + i + c@   \ -- data ; Get the data byte
         ymregs i + c@            \ -- data reg# ; Get the register#
         _Sound_Byte              \ -- ;
      loop
      key? if key drop leave then \ exit loop if key pressed
   loop
   $B 8 do 0 i _Sound_Byte loop   \ set volume 0 for channels 8,9,10
;


\ Print Ships(_x) test word
\ Includes a dump of $10 bytes before and after the stack as I saw some
\ comments that Print_Ships BIOS/RUM routine underflows the stack, but it
\ looks OK to me
: ps \ -- ;
   cr s0 $10 - $20 dump     \ dump the stack, s0 is base of stack
   $30a0 pad !              \ store y x coords for _x variant

   begin
       _Wait_Recal
       _Intensity_7F
       #5  $68 pad _Print_Ships_x \ #ships ship_char=spaceship ptr_to_xy_coords
       #10 $69 0 0 _Print_Ships   \ #ships ship_char=spaceman x y
       key?                       \ press a key to end
   until
   cr s0 $10 - $20 dump     \ redump the stack, check under stack is same
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

here equ dot_list
        60 c, -70 c,  \ seven dots, relative
       -40 c,  10 c,  \ position, Y, X
        0  c,  30 c,
        40 c,  10 c,
        10 c,  30 c,
         5 c,  30 c,
       -10 c,  40 c,

here equ dot_list_packet
 ff c, -60 c, -70 c,  \ seven dots, relative
 00 c, -40 c,  10 c,  \ position, Y, X
 ff c,  0  c,  30 c,
 00 c,  40 c,  10 c,
 ff c,  10 c,  30 c,
 ff c,   5 c,  30 c,
 ff c, -10 c,  40 c,
 01 c,                \ list terminator

\ Dot_List test. Displays an approximate mirrored version of Ursa Major
: dl \ -- ;
     begin
        _Wait_Recal
        _Intensity_5F
        #50 VIA_t1_cnt_lo c!   \ set scaling to 50 decimal

        _Dot_here                        \ dot in centre of screen

        $10 8 _Dot_d                     \ dot 10 right, 5 up from previous dot

        $1020 pad !                      \ setup yx co-ord parameter
        pad _Dot_ix                      \ dot 20 right, 10 up from previous dot

        $1020 pad !                      \ setup yx co-ord parameter
        $1f pad _Dot_ix_b                \ dot 20 right, 10 up, intensity 1f

        dot_list_packet _Dot_List_Reset  \ display dot list, using terminator

        #6 Vec_Misc_Count c!             \ 7 dots total, starting from 0
        dot_list _Dot_List               \ display dot list, using counter

        key? \ hit a key to exit
     until
;

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

: box \ len_x len_y start_x start_y -- ;
   _Reset0Ref
   _Moveto_d  \ -- lx ly ;
   dup 0         _Draw_Line_d
   over 0 swap   _Draw_Line_d
   negate 0      _Draw_Line_d
   negate 0 swap _Draw_Line_d
;

: intensity \ -- ;
  BEGIN
    _Wait_Recal
    _Intensity_3F (dg2)
    _Intensity_5F 50 50 -27 -28 box
    80 0 do
        I 100 * _Intensity_a
        -44 i 30 - HELLO-WORLD-STRING 3 + _Print_Str_d
    10 +loop
    KEY?
  UNTIL
;

