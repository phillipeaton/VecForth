\ start of vectrex memory with cartridge name
\ ***************************************************************************
\ VECTREX CART HEADER SECTION
\ ***************************************************************************

HEX
\ g         G     C     E           2     0     2     0
67 C, 20 C, 47 C, 43 C, 45 C, 20 C, 32 C, 30 C, 32 C, 30 C, 80 C,
FD C, 0D C,             \ Intro Music
F8 C, 50 C, 20 C, 80 C, \ Height Width etc.
\ V   E     C     F     O     R     T     H
56 C, 45 C, 43 C, 46 C, 4F C, 52 C, 54 C, 48 C, 80 C,
00 C,   \ End of Header

ASM: HERE EQU VFSTART
HERE 1 + EQU ENTRY-ADDR
9999 JMP, ;C \ Vectrex code to jump to VecForth ENTRY word, 9999 gets overwritten
