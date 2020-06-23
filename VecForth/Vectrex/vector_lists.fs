\ Vector Lists for example programs

HEX

here equ DOT-LIST  \ Seven dots
 30 c, -70 c,      \ rel y, rel x
-40 c,  10 c,
  0 c,  30 c,
 40 c,  10 c,
 10 c,  30 c,
  5 c,  30 c,
-10 c,  40 c,

here equ TURTLEcs
 17 c,          \ count of vectors - 1
 20 c,          \ scale factor
here equ TURTLE
 20 c,  20 c,   \ rel y, rel x
-10 c,  20 c,
 20 c,  10 c,
 20 c, -20 c,
 00 c,  20 c,
-10 c,  10 c,
 10 c,  30 c,
-10 c,  40 c,
 10 c,  00 c,
-10 c,  10 c,
-10 c,  00 c,
-30 c,  20 c,
-30 c, -20 c,
-10 c,  00 c,
-10 c, -10 c,
 10 c,  00 c,
-10 c, -40 c,
 10 c, -30 c,
-10 c, -10 c,
 00 c, -20 c,
 20 c,  20 c,
 20 c, -10 c,
-10 c, -20 c,
 20 c, -20 c,

here equ PLANE
 00 c,  6E c,   \ rel y, rel x
 14 c, -1E c,
 00 c, -32 c,
 14 c, -1E c,
-28 c,  00 c,

here equ DOTS_LIST
       -50 c, -70 c,  \ seven dots, relative
       -40 c,  10 c,  \ position, Y, X
        0  c,  30 c,
        40 c,  10 c,
        10 c,  30 c,
         5 c,  30 c,
       -10 c,  40 c,

here equ DOTS_LIST_PACKET \ Similar to DOTS_LIST
 ff c,  70 c, -70 c,  \ seven dots, relative
 ff c, -40 c,  10 c,  \ position, Y, X
 ff c,  0  c,  30 c,
 ff c,  40 c,  10 c,
 ff c,  10 c,  30 c,
 ff c,   5 c,  30 c,
 ff c, -10 c,  40 c,
 01 c,                \ list terminator

here equ PLANE1
-7f c, -7f c,   \ rel y, rel x, move to start
 00 c,  6E c,   \ rel y, rel x
 14 c, -1E c,
 00 c, -32 c,
 14 c, -1E c,
-28 c,  00 c,

here equ PLANE2
 05 c,  20 c,   \ count, scale
-7f c, -7f c,   \ rel y, rel x, move to start
 00 c,  6E c,   \ rel y, rel x
 14 c, -1E c,
 00 c, -32 c,
 14 c, -1E c,
-28 c,  00 c,

here equ PLANE3
 06 c,          \ count
-7f c, -7f c,   \ rel y, rel x, move to start
 00 c,  6E c,   \ rel y, rel x
 14 c, -1E c,
 00 c, -32 c,
 14 c, -1E c,
-28 c,  00 c,

here equ PLANEA
 00 c,  6E c,   \ rel y, rel x
 14 c, -1E c,
 00 c, -32 c,
 14 c, -1E c,
-28 c,  00 c,

here equ PLANEB
 04 c,  20 c,   \ count, scale
 00 c,  6E c,   \ rel y, rel x
 14 c, -1E c,
 00 c, -32 c,
 14 c, -1E c,
-28 c,  00 c,

here equ PLANEC
 04 c,          \ count
 00 c,  6E c,   \ rel y, rel x
 14 c, -1E c,
 00 c, -32 c,
 14 c, -1E c,
-28 c,  00 c,

here equ PLANED
 FF c,  00 c,  6E c,   \ rel y, rel x
 FF c,  14 c, -1E c,
 FF c,  00 c, -32 c,
 FF c,  14 c, -1E c,
 FF c, -28 c,  00 c,
 01 c,

here equ PLANEE
 20 c,          \ scale
 FF c,  00 c,  6E c,   \ rel y, rel x
 FF c,  14 c, -1E c,
 FF c,  00 c, -32 c,
 FF c,  14 c, -1E c,
 FF c, -28 c,  00 c,
 01 c,
