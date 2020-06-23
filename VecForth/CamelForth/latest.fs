HEX

\   6809 DTC: user area initialization                24feb16nac
DECIMAL 18 CONSTANT #INIT   \ # bytes of user area init data

CREATE UINIT  HEX
   0 , 0 , 0A , 0 ,         \ reserved, >IN, BASE, STATE
   DP-INIT ,                \ Direct Pointer (DP)
   0 , 0 ,                  \ 'SOURCE length, address
   0 ,                      \ BLK
META ALSO FORTH TLATEST @ T, PREVIOUS TARGET    \ LATEST

\ This must be the *last* compiled line in the kernel, in
\ order to set the initial LATEST as shown above. It will
\ lay down the address of the previously definition for
\ the dictionary search.