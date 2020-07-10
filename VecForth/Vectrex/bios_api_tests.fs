\ Vectrex Forth BIOS Application Programming Interface Test Words
\ Copyright (C) 2020 Phillip Eaton <inbox at phillipeaton.com>

HEX

\ FORTH WORD NAMES
\
\ Forth typically uses dash in word names, not underscore like e.g. C,
\ which is handy for Vectrex Forth programming because anything with an
\ underscore is easily identifiable as either a Vectrex BIOS call or memory
\ address. Typically I use upper case name for constants e.g. memory addresses.
\
\ CODE COMMENTS
\
\ I've commented the first handful of words fully to give you an idea of what's
\ going on. I don't comment the remaining words to the same degree as it's just
\ repeating the same. See the CamelForth source files for more info or Google it.
\ The \ -- ; at the start of a word is a stack comment. Anything before the --
\ is consumed by the word, anything after the -- is returned by the word.
\ Where you see a stack comment in the rest of the code, it's showing the stack
\ contents once that line has completed.
\
\ COMMON FORTH WORDS
\
\ The test words in this file use a few common Forth words quite often. Note,
\ all the words below consume parameters from or return results to the S stack:
\ dump      - Displays memory locations from given address and number of bytes
\ key?      - Checks terminal input buffer for a key and returns a flag
\ key       - Key a key from the terminal input buffer and put it on the stack
\ drop      - Discard the top item off the S stack
\ do..loop  - Like BASIC for/next loop, refer to the index with 'i'. +loop=step
\ if        - Not like other languages! If decides based on top of stack boolean
\ then      - Not like other languages! It's just like Endif in other languages
\
\ PAD USAGE
\
\ The Forth PAD is an area of RAM that Forth uses for short term storage,
\ typically for building formatted strings for display. It's handy for stashing
\ some variables for passing to the Vectrex BIOS routines, e.g. a vector list.
\ The 'pad' Forth word returns the address of the start of the Pad.


\ Reset and initialization ----------------------------------------------------

\ No test words needed for Cold_Start and Warm_Start, as no params passed. You
\ can just run them directly from the command line.
: inits \ -- ;
  _Init_VIA  _Init_OS_RAM  _Init_OS
;

\ Calibration/Vector Reset ----------------------------------------------------

\ Test the various entry points, several of these are a supersets of the other
\ routines, e.g. the Wait_Recal continues into the Set_Refresh code, which
\ calls Recalibrate, which calls Reset0Int and Reset0Ref, which continues into
\ Reset_Pen...you get the idea.
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

\ Set Direct Pointer ----------------------------------------------------------

\ No tests needed, as not needed for Forth.

\ Joystick handling -----------------------------------------------------------

\ Read Buttons/mask.
\ Display all the bytes that are set by the read.
\ The dump command starts by sending a CR to the terminal, so the u. is shown at
\ the end of the previous line.
\ The $0033 sets 3 & 4 to NOT be momentary read for _mask, default setting is $FF.
: rbm begin $33 _Read_Btns_mask u. Vec_Btn_State $10 dump key? until key drop ;
: rb  begin     _Read_Btns      u. Vec_Btn_State $10 dump key? until key drop ;

\ Joystick read routines
: jsetup \ -- ; Joystick setup, used by analog and digital
   $00   Vec_Joy_Resltn c! \ Set resolution. $00=highest, $80=lowest
   $0103 Vec_Joy_Mux_1_X ! \ Set joystick read routines
   $0507 Vec_Joy_Mux_2_X ! \   to do X & Y for Joystick 1 & 2
;

: jd \ -- ; \ Read Joystick Digital
   jsetup           \ Setup which joystick resolution and directions to read
   begin
      _Joy_Digital          \ Call the Vectrex BIOS routine
      Vec_Joy_1_X $10 dump  \ Show the memory locations updated by Joy_Digital
      key?                  \ Returns a true flag if key input at terminal
   until                    \ Until true i.e. in this case if a key at terminal
   key drop                 \ Get key from terminal buffer and they ignore it
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

\ Sound -----------------------------------------------------------------------

\ Sound_Byte/_x/_raw and Clear_Sound test, i.e. a small music player
\ ymlen/data/regs loaded from external YM music file
: sb \ -- ;
   YMLEN @ 0 do         \ For each line of the YM file (j loop)
      _Wait_Recal       \ Call Vectrex BIOS routine. One line per 20ms i.e. 50Hz
      $2A emit          \ Show feedback asterisk at terminal
      $B 0 do           \ Store a line of music data to PSG registers (i loop)
         YMDATA j $B * + i + c@       \ -- data ; Get the data byte
         YMREGS i + c@                \ -- data reg# ; Get the register#

         \ Uncomment out one of these below lines to store the byte to register
         \   using one of the three different Vectrex BIOS calls
         \              _Sound_Byte     \ -- ;
         Vec_Snd_Shadow _Sound_Byte_x   \ -- ;
         \              _Sound_Byte_raw \ -- ;

      loop
      key? if key drop leave then \ Exit loop if key pressed
   loop
   _Clear_Sound         \ Vectrex BIOS call to set all registers to 0
;

: sbs \ -- ; Sound_Bytes test, modified version of Sound_Byte test
   YMLEN2 @ 0 do         \ For each line of the YM file (j loop)
      _Wait_Recal        \ One line per 20ms i.e. 50Hz
      $2A emit           \ Show feedback asterisk at terminal
      YMDATA2 i $17 * +  \ -- addr ; Get  next register/data pair data block

      \ Uncomment one of these two lines out to use either Vectrex BIOS call
                       _Sound_Bytes   \ -- ;
      \ Vec_Snd_Shadow _Sound_Bytes_x \ -- ;

      key? if key drop leave then   \ Exit loop if key pressed
   loop
   _Clear_Sound          \ Set all registers to 0
;

\ Do_Sound/_x and Init_music_chk test word, i.e. built-in music player
\ Not sure what Init_Music and Init_Music_dft do, but interface same as _chk
: ds \ -- ;
   1 Vec_Music_Flag c!          \ Required to make it work
   begin
      YANKEE _Init_Music_chk    \ Comment one of these two music lines out
\     music4 _Init_Music_chk    \ music4 = Scramble intro tune in BIOS ROM
      _Wait_Recal
\     _Do_Sound                 \ Comment out one of the Do_Sound/_x lines out
      Vec_Snd_Shadow _Do_Sound_x
      key?
   until
   key drop
   _Clear_Sound                 \ Set all registers to 0
;

: imb \ -- ; Init_Music_Buf test word
   Vec_Music_Work 10 2A fill    \ Fill buffer so "It's full of stars"
   Vec_Music_Work 10 dump       \ Display buffer full of stars
   _Init_Music_Buf              \ Init buffer
   Vec_Music_Work 10 dump       \ Buffer now clear, except for reg#6=$3F
;

here equ ES_DATA
  %00111111 c, $01 c, $EF c, $02 c,

: es \ -- ; Explosion Sound test word
   ." Space to (re-)play explosion, any other key end"
   begin
      ES_DATA _Explosion_Snd
      _Wait_Recal
      _Do_Sound
      key? dup if                \ -- flag ; Key pressed?
         key $20 = if            \ -- flag ; Key pressed was a space?
            drop 0               \ -- f ; False flag
            -1 Vec_Expl_Flag c!  \ -- f ; Start (re-)playing sound
         then
      then
   until
;

\ Vector brightness -----------------------------------------------------------

: box \ len_x len_y start_x start_y -- ; Simple box drawing word
   _Reset0Ref
   _Moveto_d  \ -- len_x len_y ; Consumes start_x start_y
   dup 0         _Draw_Line_d   \ -- len_x len_y ;
   over 0 swap   _Draw_Line_d   \ -- len_x len_y ;
   negate 0      _Draw_Line_d   \ -- len_x ; Negate makes it draw backwards
   negate 0 swap _Draw_Line_d   \ -- ;
;

here equ HELLO-WORLD-STRING
str" HELLO WORLD€"  \ str" is custom word to compile chars up to " to ROM, €=$80

: intensity \ -- ; Intensity words test
   begin
      _Wait_Recal
      _Intensity_7F 7f 7f -$40 -$40 box \ Draw concentric boxes with decreasing
      _Intensity_5F 5f 5f -$30 -$30 box \   intensities
      _Intensity_3F 3f 3f -$20 -$20 box
      _Reset0Ref
      $3f -$30 do \ Display a list of 7 HELLO WORLDS with decreasing intensity
         i $40 + _Intensity_a
         -$45 i HELLO-WORLD-STRING _Print_Str_d
      $11 +loop   \ $11 spaces-out the text nicely
      key?
   until
   key drop
;

\ Drawing / Dot ---------------------------------------------------------------

\ Dot drawing tests.
: dots \ -- ;
   begin
      _Wait_Recal
      $40 VIA_t1_cnt_lo c!   \ Set scaling factor
      _Intensity_5F          \ Set intensity, works with default dwell value

      \ Display four dots, starting from centre, moving in a line right and up
      _Dot_here              \ Dot in centre of screen

      $10 8 _Dot_d           \ Dot $10 right, $8 up from previous dot

      $1020 pad !            \ Setup yx co-ord parameter in pad
      pad _Dot_ix            \ Dot $20 right, $10 up from previous dot

      $2040 pad !            \ Setup yx co-ord parameter in pad
      5 pad _Dot_ix_b        \ Dot $40 right, $20 up, with dwell 5

      \ Displays an approximate mirrored version of Ursa Major
      _Reset0Ref
      DOTS_LIST_PACKET _Dot_List_Reset \ Display dot list, using terminator

      \ Displays an approximate mirrored version of Ursa Major, 2nd time
      _Reset0Ref
      6 Vec_Misc_Count c!    \ 7 dots total, starting from 0
      DOTS_LIST _Dot_List    \ Display dot list, using counter

      key?
   until
   key drop
;

\ Vector beam positioning -----------------------------------------------------

: mt \ -- ; Moveto tests. Display a dot at each moveto position.
   begin
      _Wait_Recal
      _Intensity_5F

      \ Use the seven different moveto's to position and then draw seven dots
      \  in a horizontal line across display. _7F/_FF sets the scale factor
      $4000 pad !  -$4000 pad cell+ !
      pad _Moveto_x_7F _Dot_here

      _Reset0Ref
      -$30 $40 _Moveto_d_7F _Dot_here   \ x y on stack

      _Reset0Ref   $20F0 pad !
      pad _Moveto_ix_FF _Dot_here       \ y x in pad. _FF so move 20

      _Reset0Ref   $40F0 pad !
      pad _Moveto_ix_7F _Dot_here       \ y x in pad. _7F so move 40

      _Reset0Ref   $4000 pad !
      $7F pad _Moveto_ix_b _Dot_here    \ y x in pad

      _Reset0Ref   $4010 pad !
      pad _Moveto_ix _Dot_here          \ y x in pad

      _Reset0Ref
      $20 $40 _Moveto_d _Dot_here       \ x y on stack

      key?
   until
   key drop
;

\ Drawing / String ------------------------------------------------------------

\ Print_Str/_List tests, character reference in John Hall RUM disassembly listing
\                 width  height  rel x, rel y   Chars+Terminator
here equ PSTR_HWYX F8 c, 50 c,   70 c, -40 c,   str" ABC€"
here equ PSTR_YX                 50 c, -40 c,   str" DEF€"
here equ PSTR_D                                 str" GHI€"
here equ PSTR                                   str" JKL€"

here equ PLIST_HW  F8 c, 50 c,  -10 c, -40 c,   str" MNO€"
                   F8 c, 50 c,  -10 c, -00 c,   str" PQR€"   00 c,  \ List end
here equ PLIST                  -30 c, -40 c,   str" STU€"
                                -30 c, -00 c,   str" VWX€"   00 c,  \ List end
here equ PLIST_CHK              -50 c, -40 c,   str" YZ[€"
                                -50 c, -00 c,   str" \]^€"   00 c,  \ List end

: ps \ -- ; Print tests. Display string for each subroutine call type
   begin
      _Wait_Recal
      _Intensity_7F

      \ Display some text using the seven different Print_Str BIOS calls
                                        PSTR_HWYX _Print_Str_hwyx \ ABC
      _Reset0Ref                        PSTR_YX   _Print_Str_yx   \ DEF
      _Reset0Ref   -$40 $30             PSTR_D    _Print_Str_d    \ GHI
      _Reset0Ref   -$40 $10 _Moveto_d   PSTR      _Print_Str      \ JKL

      _Reset0Ref                        PLIST_HW  _Print_List_hw  \ MNO PQR
      _Reset0Ref                        PLIST     _Print_List     \ STU VWX
      _Reset0Ref                        PLIST_CHK _Print_List_chk \ YZ[ \]^
      key?
   until
   key drop
;

\ Print Ships -----------------------------------------------------------------

\ Print Ships(_x) test word
\ Includes a dump of $10 bytes before and after the stack as I saw some
\ comments that Print_Ships BIOS/RUM routine underflows the stack, but it
\ looks OK to me from my test.
: psh \ -- ;
   cr s0 $10 - $20 dump     \ Dump the stack, s0 is base address of stack
   $30a0 pad !              \ Store y x coords for _x variant

   begin
      _Wait_Recal
      _Intensity_7F
      #5  $68      pad _Print_Ships_x  \ #ships ship_char=spaceship xy_coords_addr
      #10 $69 $a0 -$30 _Print_Ships    \ #ships ship_char=spaceman x y
      key?
   until
   key drop

   cr s0 $10 - $20 dump     \ Redump the stack, check under stack is same
;

\ Drawing / Vector / Move and Draw --------------------------------------------

: reset/move  \ y-coordinate -- ;
   _Reset0Ref    0 swap _Moveto_d
;
: reset/move2 \ y-coordinate -- ;
   _Reset0Ref -$7F swap _Moveto_d
;

: md \ -- ; Move_Draw tests.
   begin
      _Wait_Recal
      _Intensity_7F

       $20 VIA_t1_cnt_lo c! \ Set scaling factor

       \ Display a plane VL using the seven different Mov_Draw BIOS calls
       $70 reset/move                                 PLANE3 _Mov_Draw_VLc_a
       $50 reset/move  5 Vec_Misc_Count c!   $20      PLANE1 _Mov_Draw_VL_b
       $30 reset/move                                 PLANE2 _Mov_Draw_VLcs
       $10 reset/move                        $20    5 PLANE1 _Mov_Draw_VL_ab
      -$10 reset/move                               5 PLANE1 _Mov_Draw_VL_a
      -$30 reset/move  5 Vec_Misc_Count c!            PLANE1 _Mov_Draw_VL
      -$50 reset/move  5 Vec_Misc_Count c!  -$7F -$7F PLANE1 _Mov_Draw_VL_d

      key?
   until
   key drop
;

\ Drawing / Vector / Draw only ------------------------------------------------

: draw \ -- ; Draw tests.
   begin
      _Wait_Recal
      _Intensity_7F

       $20 VIA_t1_cnt_lo c! \ Set scaling factor

      \ Display plane VL using the seven different standard Mov_Draw BIOS calls
       $70 reset/move                            PLANEC _Draw_VLc
       $50 reset/move  4 Vec_Misc_Count c! $20   PLANEA _Draw_VL_b
       $30 reset/move                            PLANEB _Draw_VLcs
       $10 reset/move                      $20 4 PLANEA _Draw_VL_ab
      -$10 reset/move                          4 PLANEA _Draw_VL_a
      -$30 reset/move  4 Vec_Misc_Count c!       PLANEA _Draw_VL
      _Reset0Ref 0 -$7F _Moveto_d          $7F 0        _Draw_Line_d

      \ Display plane VL using the seven different patterned Mov_Draw BIOS calls
       $F0 Vec_Pattern c!

       $70 reset/move2                           PLANEE _Draw_VLp_scale
       $50 reset/move2                     $20   PLANED _Draw_VLp_b
       $30 reset/move2                           PLANED _Draw_VLp

       $10 reset/move2                         4 PLANEA _Draw_Pat_VL_a
      -$10 reset/move2 4 Vec_Misc_Count c!       PLANEA _Draw_Pat_VL
      -$30 reset/move2 4 Vec_Misc_Count c!   0 0 PLANEB _Draw_Pat_VL_d
      -$50 reset/move2                           PLANED _Draw_VL_mode

       $5F VIA_t1_cnt_lo c! \ Set scaling factor for move
       _Reset0Ref -$7F  $5F _Moveto_d            PLANED _Draw_VLp_7F
       $5F VIA_t1_cnt_lo c! \ Set scaling factor for move
       _Reset0Ref -$7F -$7F _Moveto_d            PLANED _Draw_VLp_FF

      \ _Draw_Grid_VL \ No API call implementede. Appears to be a prototype VL
                      \ routine using 4 bits each for x and y coordinates.
      key?
   until
   key drop
;

\ Random number generator -----------------------------------------------------

\ Random(_3) test words. Visually, rand3 looks more random
: rand  begin cr _random   8 / 1+ 0 do ." *" loop key? until ;
: rand3 begin cr _random_3 8 / 1+ 0 do ." *" loop key? until ;

\ Memory Management / Memory clear --------------------------------------------

: clearxb \ -- ; Test word to fill memory black with 0's, up to 2^8 bytes
   $1234 pad !  $5678 pad cell+ ! \ Add some data
   pad $10 dump
   1 pad _clear_x_b         \ 1=2 chars to fill with 0
   pad $10 dump
;

: clearc8 \ -- ; CRASHES FORTH, use FILL instead
\   $C800 $100 dump  _Clear_C8_RAM  cr $C800 $100 dump
;

: clearx256 \ -- ; CRASHES FORTH, use FILL instead
\   $CB00 $100 dump  $CB00 _Clear_x_256  $CB00 $100 dump
;

: clearxd \ -- ; Test word to fill memory block with 0's, up to 2^16 bytes
   $1234 pad !  $5678 pad cell+ ! \ Add some data
   pad $10 dump
   1 pad _clear_x_d         \ 1=2 chars to fill with 0
   pad $10 dump
;

\ Memory Management / Memory fill ---------------------------------------------

: clearxb80 \ -- ; Test word to fill memory block with $80's.
   $1234 pad !  $5678 pad cell+ ! \ Add some data
   pad $10 dump
   1 pad _clear_x_b_80      \ 1=2 chars to fill with $80
   pad $10 dump
;

: clearxba \ -- ; Test work to fill memory block with bytes.
   $1234 pad !  $5678 pad cell+ ! \ Add some data
   pad $10 dump
   $AA01 pad _clear_x_b_a   \ $AA=byte to fill with, $01=2 chars
   pad $10 dump
;

\ Counters --------------------------------------------------------------------

\ Dec_3/6_Counters test. Counters are 8 bit.
: dec36c \ -- ;
   6 0 do                          \ For 6 counters:
      _random i Vec_Counters + c!  \ Store in a random number
   loop
   $FF 0 do                        \ Decrement all counters to zero
      _Dec_3_Counters              \ Dec first three
      _Dec_6_Counters              \ Dec all six, thus first three dec'd twice
      Vec_Counters $10 dump        \ Dump the counters to terminal
      key? if leave then           \ Exit on key press
   loop
   key? if key drop then
;

\ Decrement Counters test. Decrements 1, then 2, 3, through to 6 counters.
: decc \ -- ;
   Vec_Counters 6 $FF fill         \ Initialise counters to $FF
   Vec_Counters $10 dump           \ Display counters
   6 0 do                          \ Decrement 1 counter, 2 ,3...6
      i Vec_Counters _Dec_Counters \
   loop
   Vec_Counters $10 dump           \ Display counters again
;

\ Delay -----------------------------------------------------------------------

\ Delay_n etc test. Probably unlikey to be used by Forth directly.
: delays \ -- ;
   _Delay_3 _Delay_2 _Delay_1 _Delay_0 _Delay_RTS
   $FF _Delay_b \ Parameterised general delay
;

\ Day to Day / Bitmask --------------------------------------------------------

\ Bitmask_a test. Provide a number 1 to 8, get back the power of 2
\ Forth alternative is LSHIFT e.g. 1 5 lshift = 32
: bma \ bit_number -- bit_mask ; \ output gives 1 2 4 8 16 32 64 128
   8 0 do i _Bitmask_a u. loop
;

\ Mathematical ----------------------------------------------------------------

: absab \ 2_8-bit_numbers -- 2_8-bit_numbers_absolute ;
   $8080 _abs_a_b u. ;

: absb \ 8-bit_number -- 8-bit_number_absolute ;
   $80 _abs_b u. ;

\ Vector object handling / Rotating -------------------------------------------

\ Rise_Run_Angle test. Given a Rise length and Run length, Rise_Run_Angle
\ calculates the angle of the slope to make a triangle in Vectrex degrees.
\ This test word runs the calculation with $30 pairs of Rise/Run values.
\ Note 360 degrees = $40 steps for Vectrex
: rra \ -- ;
   $30 1 do
      $30 i - i 2dup cr ."  Run " . ."  Rise " .     \ -- run rise ;
      _Rise_Run_Angle $FF and dup ."  Degrees64 " .  \ -- degrees64 ; Vectrex degrees
      #360 swap $40 */ ."  Degrees360 " .            \ -- ; Real world degrees
   loop
;

: sine-wave \ -- ; Display a sine wave to the terminal. Run=Sine, Rise=Cosine
   $80 0 do                             \ 360 degrees = $40 steps for Vectrex

      \ Uncomment either rise or run
      i _get_rise_idx dup cr 4 u.r      \ -- 180sine-val&negative-flags ;
\     i _get_run_idx dup cr 4 u.r       \ -- 180sine-val&negative-flags ;

      >< dup 0< if                      \ -- 180sine-val ;
         $FF and negate
      else
         $FF and
      then                              \ -- 360sine-val ;
      $10 / $10 + dup space 2 u.r space \ -- 360sine-val-low-res ; display val
      ?dup if 0 do $2A emit loop then   \ -- ; Print stars to form sine wave
   4 +loop                              \ Make super coarse
;

\ C836   EQU   Vec_Angle       \ Angle for rise/run and rotation calculations
\ C837   EQU   Vec_Run_Index   \ Index pair for run
\ C839   EQU   Vec_Rise_Index  \ Index pair for rise
: sincos \ -- ; Puts Sine and Cosine into memory for given angle in memory
                                      Vec_Angle 5 dump
   $00 Vec_Angle c!   _Rise_Run_Idx   Vec_Angle 5 dump
   $08 Vec_Angle c!   _Rise_Run_Idx   Vec_Angle 5 dump
   $10 Vec_Angle c!   _Rise_Run_Idx   Vec_Angle 5 dump
;

\ Rise_Run_Y. Not sure what this is for, the Malban tutorial text I don't get.
\ Seems to be explained better in the Vectrex Programmer's Manual Vol II.
\ Needs some more thinking about before fixing this word to be a bit more useful.
\ This below Forth word just makes sure the BIOS routine returns some values.
: rrx \ -- ;
   $40 0 do
      cr i .
      i 1 _Rise_Run_X 5 u.r
   loop
;

: rry \ -- ; Rise_Run_Y. See 'rrx'.
   $40 0 do
      cr i .
      i 1 _Rise_Run_Y 5 u.r
   loop
;

: rrl \ -- ; Rise_Run_Y. See 'rrx'.
   $40 0 do
      cr i .
      i Vec_Angle c!
      $100 _Rise_Run_Len 5 u.r
   loop
;

: rvl \ -- ; Rotate Vector List. Test out two rotation routines.
   begin
      $40 0 do
         _Wait_Recal
         _Intensity_7F
         $20 VIA_t1_cnt_lo c!  \ Set scaling factor

         \ Setup and perform VL rotation using _Rot_VL BIOS routine
         $17 Vec_Misc_Count c! \ Number of vectors - 1
         i Vec_Angle c!        \ Angle to rotate by
         TURTLE pad _Rot_VL    \ Rotate vector list

         0 -$7f _Moveto_d      \ Move and draw VL
         $17 pad _Draw_VL_a

         _Reset0Ref

         \ Setup and perform VL rotation using _Rot_VL_ab BIOS routine
         $17 i TURTLE pad      \ -- #vectors angle VL_source VL_rotated ;
         _Rot_VL_ab            \ Rotate vector list

         0 $7F _Moveto_d       \ Move and draw VL
         $17 pad _Draw_VL_a

      loop
      key?
   until
   key drop
;

: rvlm \ -- ; Rot_VL_Mode. Rotate and display VL with mode information.
   begin
      $40 0 do
         _Wait_Recal
         _Intensity_7F
         $20 VIA_t1_cnt_lo c!  \ Set scaling factor
         $F0 Vec_Pattern c!    \ Patterned vector lists

         i planeD pad          \ -- vectors angle VL_source VL_rotated ;
         _Rot_VL_Mode          \ Rotate vector list

         0 $7F _Moveto_d
         pad _Draw_VL_mode

      loop
      key?
   until
   key drop
;

\ Rot_VL_M_dft not tested as it'll never be used from Forth, because it uses
\ a pre-existing value is the A register, which is already used by Forth.

: xfruna \ -- ;
   $0000 Vec_Run_Index !
   $20 _Xform_Run_a u.
;

\ Not tested yet, need to check what uses them, can't find any ASM that does
\ _Xform_Run_a
\ _Xform_Run
\ _Xform_Rise_a
\ _Xform_Rise

\ Memory management / Memory copy ---------------------------------------------

: mma1 \ -- ; Test Move_Mem_a(_1)
   $10 0 do i pad i + c! loop            \ Fill start of pad with sequence of numbers
   pad $10 dump                          \ Display start of pad i.e. initial sequence
   1 pad     pad  8 + _Move_Mem_a_1      \ Copy bytes 0 & 1 of pad to pad bytes 8 & 9
   2 pad 4 + pad $C + _Move_Mem_a        \ Copy bytes 4 & 5 of pad to pad bytes $c & $d
   pad $10 dump                          \ Display start of pad i.e. after moves
;

\ Player option ---------------------------------------------------------------

: sg \ -- ; Select_Game, use buttons 1,2 and 3 to choose, 4 to end, 5 second timeout
   3 4                      \ -- #game_versions #players_max ;
   _select_game             \ -- ;
   Vec_Num_Players 10 dump  \ $C879 = Vec_Num_Players, $C87A = Vec_Num_Game
;

: dopt \ -- ; Display_Option
   $0000 pad !      \ y x relative to display option value
   $0000 pad 2 + !  \ y x relative to display option string
   $5859 pad 4 + !  \ "XY"
   $5A80 pad 6 + !  \ "Z" + string terminator
   begin
      _Wait_Recal
      _Intensity_7F
      1 pad         \ -- option_value option_string ;
      _Display_Option
      key?
   until
   key drop
;

\ Score -----------------------------------------------------------------------

\ No test word needed for Clear_Score, as no params passed. You
\ can just run directly from the command line.

: .score \ string_addr -- ; Print a score
   begin
      dup c@ dup $80 <>
   while
      emit 1 +
   repeat
   2drop
;

: dump-pad/score pad 10 dump pad .score ;

: addd-dump pad _Add_Score_d dump-pad/score ;
: adda-dump pad _Add_Score_a dump-pad/score ;

\ Add decimal score to 6 digit ascii text score field.
\ BIOS converts decimal score to BCD and then runs _Add_Score_d.
: scorea \ -- ;
   pad 10 dump
   pad _clear_score dump-pad/score
   #003 adda-dump  \ Add & display various numbers to test boundries
   #020 adda-dump
   #100 adda-dump
   #255 adda-dump
;

\ Add BCD score to 6 digit ascii text score field.
: scored \ -- ;
   pad 10 dump
   pad _clear_score dump-pad/score
   $0004 addd-dump  \ Add & display various numbers to test boundries
   $0030 addd-dump
   $0200 addd-dump
   $1000 addd-dump
   $1234 addd-dump
;

: sz \ -- ; Strip Zeros. Strips leading zeros from score for display.
   pad 5 $30 fill       \ Set first 5 bytes of pad with ASCII $30 i.e. '0'
   $3080 pad 5 + !      \ Set sixth byte of pad to '0' and terminate with $80
   dump-pad/score
   0 pad _Strip_Zeros   \ First digit to start with and address of score to use
   dump-pad/score
;

\ Compare_Scores. Compare two BCD score strings, to determine highest.
: cs \ -- ;
   pad     _clear_score \ Clear up score area 1
   pad 8 + _clear_score \ Clear up score area 2

   pad $10 dump
   pad pad 8 + _Compare_Score 4 u.r     \ No scores, result is a 0

   $31 pad $d + c!                      \ Increment second score area
   pad $10 dump
   pad pad 8 + _Compare_Score 4 u.r     \ 2nd bigger than 1st, result is a 1

   $32 pad 5 + c!                       \ Increment first score area
   pad $10 dump
   pad pad 8 + _Compare_Score 4 u.r     \ 2nd bigger than 1st, result is a 2
;

: nhs \ -- ; New_High_Score. Sets high score to score if score is higher.
   pad     _clear_score     \ Score 1
   pad 8 + _clear_score     \ Score 2

   $1000 pad      _Add_Score_d dump-pad/score \ Set score 1 = 1000 decimal
   $1234 pad 8 +  _Add_Score_d dump-pad/score \ Set score 2 = 1234 decimal

   pad 8 + pad     _New_High_Score pad 10 dump \ New high score? No
   pad     pad 8 + _New_High_Score pad 10 dump \ New high score? Yes, set high score = score
;

\ Vector object handling / Object collision detection -------------------------

\ Obj_Hit test. Use three hit check routines to produce hit and no-hit. Hard to explain how
\ these work without some diagrams. Malban has some on his VIDE blog.
: oh \ -- ;
   $0404 pad !              \ 0404=no_hit
   $0505 pad $1010 $1A1A    \ -- box_yx  movement_xy_addr  pos_obj_yx  pos_missile_yx
   _Obj_Will_Hit_u          \ -- flag ; t = hit, f=no-hit
   cr ." Obj_Will_Hit_u " . \ Display result on a newline

   $0505 pad !              \ 0505=hit
   $0505 pad $1010 $1A1A
   _Obj_Will_Hit_u
   cr ." Obj_Will_Hit_u " .


   $0505 $0404 $1010 $1A1A  \ -- box_yx  movement_xy       pos_obj_yx  pos_missile_yx \ 0404=no_hit
   _Obj_Will_Hit
   cr ." Obj_Will_Hit " .

   $0505 $0505 $1010 $1A1A  \ 0505=hit
   _Obj_Will_Hit
   cr ." Obj_Will_Hit " .


   $0505 $1010 $1616        \ -- box_yx                    pos_obj_yx  pos_missile_yx \ 1616=no_hit
   _Obj_Hit
   cr ." Obj_Hit " .

   $0505 $1010 $1515        \ 1515=hit
   _Obj_Hit
   cr ." Obj_Hit " .
;
