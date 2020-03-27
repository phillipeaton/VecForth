\ Vectrex Forth Application

HEX

: DT \ -- ; Display turtle using vector list
  0
  BEGIN
    1+ DUP U.
    _Wait_Recal
    _Intensity_7F
    0 0 _Moveto_d
    20 17 TURTLE _Draw_VL_ab \ scale factor and count of vectors
    KEY?
  UNTIL
  DROP
;

: DTcs  \ -- ; Display turtle using vector list with count and scale
  0                     \ initial frame counter
  BEGIN
    1+ DUP U.           \ display frame counter on the terminal
    _Wait_Recal         \ BIOS recalibration
    _Intensity_7F       \ set beam intensity
    0 0 _Moveto_d       \ move beam to origin
    TURTLEcs _Draw_VLcs \ draw vector list using BIOS call
    KEY?                \ was a key pressed?
  UNTIL
  DROP                  \ drop frame counter
;

: DG \ -- ; Draw grid line by line
  FF VIA_t1_cnt_lo C!   \ set scaling factor
  0                     \ initial frame counter
  BEGIN
    1 + DUP U.          \ Send a frame counter to display on the terminal
    _Wait_Recal         \
    _Intensity_7F       \

    \ start by moving beam to bottom left corner of grid
    0 Vec_Misc_Count C! \ Vec_Misc_Count must be set to 0 to draw one vector
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

: DG2 \ -- ; Dram grid l by l, but 2nd 5 lines drawn from opposite origin
  FF VIA_t1_cnt_lo C!   \ set scaling factor
  0                     \ initial frame counter
  BEGIN
    1 + DUP U.          \ Send a frame counter to display on the terminal
    _Wait_Recal         \
    _Intensity_7F       \

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

    KEY?                \ terminal key press exits loop
  UNTIL
  DROP
;

: DG3 \ -- ;
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

HERE EQU HELLO-WORLD-STRING
S" HELLO WORLD" 80 C,

: HELLO \ -- ;
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
