\ VecFever Exit to menu

C8A0 equ ramfunction \ mem for persistent or modified functions

\ the function below does the magic handshake with the cart,
\ then waits for the new cart data to appear in the cart address
\ space and jumps back to the menu
asm:
here equ ramfunctiondata
   $7FF0     LDB,    \ notify the cart uProc
   X 0,      LDB,    \ put command on the bus
   0 #       LDX,
   $6720 #   LDD,    \ ASCII "g "
here equ ramloop
   X 0,      CMPD,   \ while the cart is setting up itself there is only one data byte
   ramloop   BNE,    \ available, so check for .two. known and different ones
   X $D ,    LEAX,   \ 0-A: "GCE xxxx",$80 / B+C: music pointer (could contain a zero..)
here equ ramloop2
   X ,+      LDA,    \ look for end of menu cart header  ( ,x+)
   ramloop2  BNE,
   X PC      TFR,    \ return to menu code data
here equ ramfuncend
next, ;c

\ ramfuncend ramfunctiondata - 1 + EQU #bytes
\ + and - don't compile, not sure why, probably a vocabulary issue.
\ Slimey hack, hardcode $19 bytes, you can use more for safety e.g. $30

code exit \ -- ; Exit back to VecFever menu system
   ramfunctiondata # LDU,      \ source
   ramfunction     # LDX,      \ destination
   $19             # LDA,      \ #bytes = 1+ramfuncend-ramfunctiondata
   Move_Mem_a        JSR,      \ copy the vec4ever switching function into place
   $1000           # LDX,      \ the 'switch back to menu' command
   ramfunction       JMP,      \ up up and away
next ;c
