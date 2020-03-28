\ Vectrex Forth Application

HEX

: KEY? 0 ;
: KEY 1 ;
: CR ;
: EMIT DROP ;
: U. DROP ;
: .S ;

HERE EQU HELLO-WORLD-STRING
S" HELLO WORLD" 80 C,


: DT \ -- ; Display turtles using vector list with two different BIOS calls
  0
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
\     _Intensity_3F (dg2)
\     _Intensity_5F 50 50 -27 -28 box
    80 0 do
        I 100 * _Intensity_a
        -44 i 30 - HELLO-WORLD-STRING 3 + _Print_Str_d
    10 +loop
    KEY?
  UNTIL
;

