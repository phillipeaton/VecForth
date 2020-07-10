\ Vectrex Forth BIOS Application Programming Interface
\ Copyright (C) 2020 Phillip Eaton <inbox at phillipeaton.com>

HEX

\ Test words in here are all pretty junky, they were the first I did once I
\ managed to get the compiler to cross-compile to the target and threw
\ together some BIOS API words. Nonetheless, they should still all work.

: dt \ -- ; Display turtles using vector list with two different BIOS calls
   begin
      _Wait_Recal
      _Intensity_7F

      -1f 1f _Moveto_d
      20 17 TURTLE _Draw_VL_ab    \ Scale_factor count_of_vectors VL_addr

      0 -7f _Moveto_d
      0 -7f _Moveto_d
      TURTLEcs _Draw_VLcs         \ Draw vector list using BIOS call

      key?
   until
   key drop
;

: dg \ -- ; Draw grid line by line
  FF VIA_t1_cnt_lo C!   \ Set scaling factor
  0                     \ Initial frame counter
  begin
    1 + dup U.          \ Send a frame counter to display on the terminal
    _Wait_Recal         \
    _Intensity_7F       \

    0 Vec_Misc_Count C! \ Vec_Misc_Count must be set to 0 to draw one vector
    \ Start by moving beam to bottom left corner of grid, then zig-zag the lines
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

    key?  \ Terminal key press exits loop
  until
  key drop
  drop
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

: dg2 \ -- ; Draw grid line by line, but no zig zag, more optimal drawing
  $FF VIA_t1_cnt_lo c!  \ set scaling factor
  0                     \ initial frame counter
  begin
    1 + dup u.          \ Send a frame counter to display on the terminal
    _Wait_Recal         \
    _Intensity_7F       \
    (dg2)
    key?                \ terminal key press exits loop
  until
  key drop
  drop
;

: DG3 \ -- ; Draw grid with lops, no hardcoding of co-ordinates
  FF VIA_t1_cnt_lo C! \ set scaling factor
  0
  begin
     1 + dup u.          \ Send a frame counter to display on the terminal
     _Wait_Recal         \
     _Intensity_7F       \

     \ Draw vertical lines
     0 Vec_Misc_Count C! \ must be set to 0 to draw one vector
     -3F -3F _Moveto_d   \ move beam to bottom left corner
     5 0 do
\        0 I 2 MOD 0= IF 7F ELSE 80 THEN _Draw_Line_d
        0 I 2 mod -FF * 7F + FF and _Draw_Line_d \ SLOWER
        I 4 < if 20 0 _Moveto_d then
     loop

     \ Draw vertical lines
     0 Vec_Misc_Count C! \ must be set to 0 to draw one vector
     80 80 _Moveto_d   \ move beam to bottom left corner
     5 0 DO
\        I 2 mod 0= IF 7F else 80 then 0 _Draw_Line_d
        I 2 mod -FF * 7F + FF and 0 _Draw_Line_d \ SLOWER
        I 4 < if 0 20 _Moveto_d then
     loop

     key?                \ terminal key press exits loop
  until
  key drop
  drop
;

: HELLO \ -- ; Hello World, to vector display, to interactive
             \ terminal and VecFever LED
  begin
     _Wait_Recal
     _Intensity_5F
     CR
     31 -30 do
        ." Hello World! "
        I 7 mod 10 * v4eLEDReg C!
        51 -50 do
           J 40 - I HELLO-WORLD-STRING _Print_Str_d
        loop
     20 +loop
     cr ." Escape to end, any other to continue"
     key 1B =
   until
;
