HEX

\ Block 61 -----------------------------------------------------
\ 6809 Source Code: boot parameters              (c) 28apr95 bjr
\
ONLY FORTH META TARGET DEFINITIONS

HEX 0000 FFFF DICTIONARY ROM  ROM
    CA00 EQU UP-INIT      \ UP must be page aligned.  Stacks,
    CA   EQU UP-INIT-HI   \   TIB, etc. init'd relative to UP.

    C880 EQU DP-INIT      \ starting RAM adrs for dictionary

\ Vectrex memory map has 1K RAM: C800-CBFF RAM, Dxx0-DxxF I/O
\ Lines enclosed by * denote VECTREX memory map
\ Without * denotes Forth-related memory map, i.e. ROM and RAM
\ Stacks grow downwards in memory

\  0000 +---------------------+
\       |                     |
\       |  Forth kernel ROM   |
\       |                     |
\  8000 +---------------------+
\       |                     |
\       |*     UNMAPPED      *|
\       |                     |
\  C800 +=====================+
\       |*   RUM/BIOS RAM    *| Sound reg's, controllers etc
\  C880 +---------------------+
\       |Forth RAM dictionary |
\  C980 +---------------------+
\       |         TIB         | Text Input Buffer
\  CA00 +---------------------+
\       |      user area      |
\  CA80 +---------------------+
\       |   parameter stack |S|
\  CB00 +---------------------+ Initial SP
\       |   HOLD,PAD areas    | Forth working RAM
\  CB80 +---------------------+
\       |    return stack   |U|
\  CBEA +---------------------+ Initial SP
\       |* BIOS HOUSEKEEPING *| Interrupt vectors
\  CC00 +=====================+
\       |*    RAM MIRROR     *|
\  D000 +=====================+
\       |*    6522VIAx128    *|
\  D800 +=====================+
\       |*    DO NOT USE     *|
\  E000 +---------------------+
\       |*     MINESTORM     *|
\  F000 +---------------------+
\       |*     RUM/BIOS      *|
\  FFF0 +---------------------+
\       |*6809 reset vectors *|
\  FFFF +---------------------+
