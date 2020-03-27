HEX

\ \\   6809 DTC: user area initialization                24feb16nac
DECIMAL 26 CONSTANT #INIT   \ # bytes of user area init data

CREATE UINIT  HEX
   0 , 0A ,                 \ reserved,BASE
   DP-INIT ,                \ DP
   0 , 0 ,                  \ BLK,SCR
   02 , 0800 ,              \ SDcard address of block file
   0 , 0 , 0 , 0 , 0 ,      \ BSTATE flags and buffers

META ALSO FORTH TLATEST @ T, PREVIOUS TARGET    \ LATEST

\ This must be the *last* compiled line in the kernel, in
\ order to set the initial LATEST as shown above. It will
\ lay down the address of the previously definition for
\ the dictionary search.