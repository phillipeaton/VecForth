HEX

\ Block 60 -------------------------------------------------------
\
\ CamelForth for the Motorola 6809  (c) 1995 Bradford J. Rodriguez
\ *   Permission is granted to freely copy, modify, and          *
\ *   distribute this program for personal or educational use.   *
\ *   Commercial inquiries should be directed to the author at   *
\ *   221 King St. E., #32, Hamilton, Ontario L8N 1B5 Canada     *
\
\ Direct-Threaded Forth model for Motorola 6809
\ 16 bit cell, 8 bit char, 8 bit (byte) adrs unit
\  X = Forth W   temporary address register
\  Y =       IP  Interpreter Pointer
\  U =       RSP Return Stack Pointer
\  S =       PSP Parameter Stack Pointer
\  D =       TOS top parameter stack item
\ DP =       UP  User Pointer (high byte)
\
\ v1.0  alpha test version, 7 May 95

\ Block 61 -----------------------------------------------------
\ 6809 Source Code: boot parameters              (c) 28apr95 bjr
\
ONLY FORTH META TARGET DEFINITIONS

HEX 0000 FFFF DICTIONARY ROM  ROM
    CA00 EQU UP-INIT      \ UP must be page aligned.  Stacks,
    CA   EQU UP-INIT-HI   \   TIB, etc. init'd relative to UP.

    C880 EQU DP-INIT      \ starting RAM adrs for dictionary
   \ Vectrex memory map with 1K RAM: C800-CBFF RAM, Dxx0-DxxF I/O

\  0000 +--------------------+
\       |                    |
\       | Forth kernel ROM   |
\       |                    |
\  8000 +--------------------+
\       |*     UNMAPPED     *|
\  C800 +--------------------+
\       |*RUM RAM (TO C87F) *|
\  C900 +--------------------+
\       |Forth RAM dictionary|
\  C980 +--------------------+
\       |        TIB         |
\  CA00 +--------------------+
\       |     user area      |
\  CA80 +--------------------+
\       |  parameter stack |S|
\  CB00 +--------------------+ Initial SP
\       |   HOLD,PAD areas   |
\  CB80 +--------------------+
\       |    return stack  |U|
\  CBEA +--------------------+
\       |*BIOS HOUSEKEEPING *|
\  CC00 +--------------------+
\       |*   RAM MIRROR     *|
\  D000 +--------------------+
\       |*   6522VIAx128    *|
\  D800 +--------------------+
\       |*    DO NOT USE    *|
\  E000 +--------------------+
\       |*    MINESTORM     *|
\  F000 +--------------------+
\       |*       RUM        *|
\  FFF0 +--------------------+
\       |*6809 reset vectors*|
\  FFFF +--------------------+
