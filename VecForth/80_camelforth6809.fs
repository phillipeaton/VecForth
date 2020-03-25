\ \\CamelForth for the Motorola 6809  (c) 1995 Bradford J. Rodriguez
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

\ \\ 6809 Source Code: boot parameters              (c) 28apr95 bjr

ONLY FORTH META TARGET DEFINITIONS

HEX 0000 FFFF DICTIONARY ROM  ROM
    CA00 EQU UP-INIT      \ UP must be page aligned.  Stacks,
    CA   EQU UP-INIT-HI   \   TIB, etc. init'd relative to UP.

    C900 EQU DP-INIT      \ starting RAM adrs for dictionary
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
\       |  parameter stack   |
\  CB00 +--------------------+ Initial SP
\       |   HOLD,PAD areas   |
\  CB80 +--------------------+
\       |    return stack    |
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


\ Harvard synonyms - these must all be PRESUMEd
AKA , I,     AKA @ I@     AKA ! I!
AKA C, IC,   AKA C@ IC@   AKA C! IC!
AKA HERE IHERE     AKA ALLOT IALLOT
PRESUME WORD       AKA WORD IWORD

\ start of vectrex memory with cartridge name...
\ ***************************************************************************
\ VECTREX CART HEADER SECTION
\ ***************************************************************************

HEX
\ g         G     C     E           2     0     1     8
67 C, 20 C, 47 C, 43 C, 45 C, 20 C, 32 C, 30 C, 31 C, 38 C, 80 C,
FD C, 0D C,             \ Intro Music
F8 C, 50 C, 20 C, 80 C, \ Height Width etc.
\ V   E     C     F     O     R     T     H
56 C, 45 C, 43 C, 46 C, 4F C, 52 C, 54 C, 48 C, 80 C,
00 C,   \ End of Header

ASM: HERE EQU VFSTART
HERE 1 + EQU ENTRY-ADDR
9999 JMP, ;C \ Vectrex code to jump to VecForth ENTRY word, 9999 gets overwritten

10 equ v4eRED
20 equ v4eGREEN
40 equ v4eBLUE
30 equ v4eYELLOW
50 equ v4eMAGENTA
60 equ v4eCYAN
70 equ v4eWHITE

\ ***************************************************************************
\  MAGIC CARTHEADER SECTION
\       DO NOT CHANGE THIS STRUCT
\ ***************************************************************************
HEX
30 ORG

\ T   h     G     S
54 C, 68 C, 47 C, 53 C,             \ magic handshake marker
HERE EQU v4ecartversion     0001 ,  \ I always have a version
                                    \ in comm. structs
\ $800 KILLS TEXT \ HERE EQU v4ecartflags       10F0 ,  \ v4e flags:
HERE EQU v4ecartflags       18F0 ,  \ v4e flags:
                                    \ $8000 + always set by v4e
                                    \ $4000 - hiscore entry supported
                                    \ $2000 - enable cart as ram
                                    \ $1000 - supply default font
                                    \ $0800 - fast menu switch supported
                                    \       + set to 0 if hiscore entry
                                    \ $0700 + v4e timing bits:
                                    \          0 - heuristic
                                    \          1 - zero
                                    \          2 - one
                                    \          3 - two
                                    \          4 - three
                                    \ $0080 - populate storage upon start
                                    \ $0040 - extension calls used
                                    \ $0020 - gpios used
                                    \ $0010 - serial regs and dma
                                    \       + set to 0 again if serial regs active
                                    \ $0008 + screensaver enabled
                                    \ $0003 - font size
\
\ first the variables for the v4e font system
\
HERE EQU v4efontptr         0000 ,  \ supplied by app:
                                    \ if != 0: the cart uses this
                                    \ ptr to supply a font and to
                                    \ optimize strings;
HERE EQU v4efontwidth       5F C,   \ supplied by app:
                                    \ the cart stores a system
                                    \ font at v4efontptr+0x20 and
                                    \ adds v4efontwidth per line
                                    \ ..must be at least $3f
HERE EQU v4efontlastchar    7E C,   \ supplied by cart:($5e or $7e)
                                    \ last char supplied by v4e
                                    \ (first one is always 0x20)
HERE EQU v4estringlists     0000 ,  \ if !=0 a ptr to a list of ptrs
                                    \ containing lists of constant
                                    \ strings that can be optimized
                                    \ for a given font (0 == end
                                    \ of lists)
\
\ now the variables for the v4e store/load area
\
HERE EQU v4eStorageArea     0000 ,  \ pointer to the area - 0: unused
HERE EQU v4eStorageSize     0000 ,  \ and its size
HERE EQU v4eStorageLoaded   0000 ,  \ set by v4e: return size for a load
                                    \ e.g. if set to zero in compile an != 0
                                    \ at the start shows whether something was
                                    \ loaded via 'populate storage upon start'
HERE EQU v4eStorageID
\ T   E     S     T
54 C, 45 C, 53 C, 54 C,             \ 4 bytes storage identifier
\
\ variables for the v4e bank switching, only populated for 'V4EB' carts
\ or for carts using serial communication
\
HERE EQU v4eMultiBank        0000 , \ during init: no. of banks (0x48)
HERE EQU v4eMultiBankAddress 0000 , \ e.g. 0: PB6 bank - $8000- normal bank
HERE EQU v4eMultiBankFlag    00 C,  \ 0: ok
\
\ end of v4e cart header
\

\ PJE the below matches up mostly
\ https://hackaday.io/project/5233-z80-computer/log/19836-serial-io
\ See also AM8530.pdf, big file, circuitry, C and assembler at the end,
\ like below.
\ $7C02 then $25 = #bytes = 3 * 6 + 1
\ #define sccSetsCount 38
\ const unsigned char sccSets[] = {
\     0x00,0x00,  //pointer reset
\     0x09,0xC0,  //hardware reset
\     0x04,0x04,  //1x clock, async, 1 stop, no par
\     0x01,0x00,  //no dma, no interrupts
\     0x02,0x00,  //clear int vector
\     0x03,0xC0,  //rx 8 bits, disabled
\     0x05,0x60,  //tx 8 bits, disabled
\     0x09,0x01,  //status low, no interrupts
\     0x0A,0x00,  //nrz encoding
\     0x0B,0xD6,  //xtal, BRG for rxc, trxc output
\     0x0C,0xfe,  //time constant low byte (1200)
\     0x0D,0x05,  //time constant high byte(1200)
\     0x0E,0x00,  //BRG source RTxC
\     0x0E,0x80,  //clock source BRG
\     0x0E,0x01,  //enable BRG
\     0x0F,0x00,  //no ints
\     0x10,0x10,  //reset interrupts
\     0x03,0xC1,  //enable Rx
\     0x05,0x68,  //enable Tx
\     0x00,0x00   //overflow

\ \\   6809 DTC: SCC initialization                 (c) 17apr95 bjr
\ HERE EQU SCCATBL HEX
\    7C02 ,  2500 ,   \ port address, #bytes, reset reg ptr
\    09C0 ,  0444 ,  0100 ,  0200 ,  03C0 ,  0560 ,
\    0901 ,  0A00 ,  0B50 ,  0C18 ,  0D00 ,  0E02 ,
\    0E03 ,  03C1 ,  0568 ,  0F00 ,  1010 ,  0100 ,
\
\ HERE EQU SCCBTBL
\    7C00 ,  1F00 ,   \ port address, #bytes, reset reg ptr
\    0444 ,  0100 ,  03C0 ,  0560 ,  0A00 ,  0B50 ,
\    0C18 ,  0D00 ,  0E02 ,  0E03 ,  03C1 ,  0568 ,
\    0F00 ,  1010 ,  0100 ,  \ 0909 ,
\
\ ASM: HERE EQU SCCINIT   \ set up on-board i/o
\    X ,++ LDY,   X ,+ LDB,
\    BEGIN,   X ,+ LDA,   Y 0, STA,   DECB,   EQ UNTIL,   RTS, ;C
\ \ \\   6809 DTC: serial I/O                         (c) 31mar95 bjr
\ HEX 7C02 EQU SCCACMD   7C03 EQU SCCADTA
\
\ CODE KEY    \ -- c    get char from serial port
\    6 # ( D) PSHS,   BEGIN,   SCCACMD LDB,   1 # ANDB,  NE UNTIL,
\    SCCADTA LDB,   CLRA,   NEXT ;C
\
\ CODE KEY?   \ -- f    return true if char waiting
\    6 # ( D) PSHS,   CLRA,   SCCACMD LDB,   1 # ANDB,
\    NE IF,   -1 # LDB,   THEN,   NEXT ;C
\
\ CODE EMIT   \ c --    output character to serial port
\    BEGIN,   SCCACMD LDA,   4 # ANDA,   NE UNTIL,
\    SCCADTA STB,   6 # ( D) PULS,   NEXT ;C

7FFB EQU v4eTxStatReg \ Read, negative if transmit buffer is in use, positive otherwise
7FFB EQU v4eTxByteReg \ Write
7FFC EQU v4eRxStatReg \ Read, zero if no data received, otherwise != 0
7FFD EQU v4eRxByteReg \ Read, upon reading, 7FFC is automatically cleared
7FFE EQU v4eLEDReg    \ LED Register, probably read/write?


CODE KEY?   \ -- f    return true if char waiting
  6 # ( D) PSHS,   CLRA,   v4eRxStatReg LDB,
  NE IF,   -1 # LDB,   THEN,   NEXT ;C

CODE KEY    \ -- c    get char from serial port
   6 # ( D) PSHS,   BEGIN,   v4eRxStatReg LDB,   NE UNTIL,
   v4eRxByteReg  LDB,   CLRA,   NEXT ;C

CODE EMIT   \ c --    output character to serial port
   BEGIN,   v4eTxStatReg  LDA,  MI UNTIL,
   v4eTxByteReg STB,   6 # ( D) PULS,   NEXT ;C




\ \\   6809 DTC: interpreter logic                       01apr15nac
ASM:  HERE RESOLVES DOCOLON   HERE EQU <DOCOLON>
    HEX 20 # ( Y) PSHU,   20 # PULS,   NEXT, ;C

ASM:  HERE RESOLVES DOCREATE   HERE EQU <DOCREATE>
    HEX 10 # ( X) PULS,   6 # ( D) PSHS,   X D TFR,   NEXT, ;C

CODE EXIT    \ --     exit a colon definition
    HEX 20 # ( Y) PULU,   NEXT ;C

CODE LIT     \ -- x   fetch inline literal to stack
    6 # ( D) PSHS,   Y ,++ LDD,   NEXT ;C

CODE EXECUTE  \ i*x xt -- j*x   execute Forth word at 'xt'
    D X TFR,   6 # ( D) PULS,   X 0, JMP, ;C

\ \\   6809 DTC: stack operations                   (c) 31mar95 bjr
CODE DUP    \ x -- x x       duplicate top of stack
    6 # ( D) PSHS,   NEXT ;C

CODE ?DUP   \ x -- 0 | x x   DUP if nonzero
    0 # CMPD,   NE IF,   6 # ( D) PSHS,   THEN,   NEXT ;C

CODE DROP   \ x --           drop top of stack
    6 # ( D) PULS,   NEXT ;C

CODE SWAP   \ x1 x2 -- x2 x1  swap top two items
    S 0, LDX,   S 0, STD,   X D TFR,   NEXT ;C

CODE OVER   \ x1 x2 -- x1 x2 x1   per stack diagram
    6 # ( D) PSHS,   S 2 , LDD,   NEXT ;C

\ \\   6809 DTC: stack operations                   (c) 314apr15nac
CODE ROT     \ x1 x2 x3 -- x2 x3 x1   per stack diagram
    S 0, LDX,   S 0, STD,   S 2 , LDD,   S 2 , STX,   NEXT ;C

CODE NIP     \ x1 x2 -- x2            per stack diagram
    S 2 , LEAS,   NEXT ;C

CODE TUCK    \ x1 x2 -- x2 x1 x2      per stack diagram
    S 0, LDX,   S 0, STD,   HEX 10 # ( X) PSHS,   NEXT ;C
CODE PICK    \ x2 x1 x0 n -- x2 x1 x0 xn
    ASLB, ROLA,   S D, LDD,           NEXT ;C

CODE >R      \ x --   R: -- x        push to return stack
    6 # ( D) PSHU,   6 # ( D) PULS,   NEXT ;C
CODE R>      \ -- x   R: x --        pop from return stack
    6 # ( D) PSHS,   6 # ( D) PULU,   NEXT ;C
\ \\   6809 DTC: stack operations                   (c) 31mar95 bjr
CODE R@     \ -- x   R: x -- x   fetch from return stack
    6 # ( D) PSHS,   U 0, LDD,   NEXT ;C

CODE SP@    \ -- a-addr         get data stack pointer
    6 # ( D) PSHS,   S D TFR,   NEXT ;C

CODE SP!    \ a-addr --         set data stack pointer
    D S TFR,   6 # ( D) PULS,   NEXT ;C

CODE RP@    \ -- a-addr         get return stack pointer
    6 # ( D) PSHS,   U D TFR,   NEXT ;C

CODE RP!    \ a-addr --         set return stack pointer
    D U TFR,   6 # ( D) PULS,   NEXT ;C

\ \\   6809 DTC: memory operations                  (c) 31mar95 bjr
CODE !      \ x a-addr --     store cell in memory
    D X TFR,   6 # ( D) PULS,   X 0, STD,   6 # ( D) PULS,
    NEXT ;C

CODE C!     \ char c-addr --     store char in memory
    D X TFR,   6 # ( D) PULS,   X 0, STB,   6 # ( D) PULS,
    NEXT ;C

CODE @      \ a-addr -- x        fetch cell from memory
    D X TFR,   X 0, LDD,   NEXT ;C

CODE C@     \ c-addr -- char     fetch char from memory
    D X TFR,   X 0, LDB,   CLRA,   NEXT ;C

\ \\   6809 DTC: arithmetic operations              (c) 26apr95 bjr
CODE +      \ n1/u1 n2/u2 -- n3/u3   add n1+n2
    S ,++ ADDD,   NEXT ;C

CODE M+     \ d n -- d            add single to double
    S 2 , ADDD,   S 2 , STD,
    6 # ( D) PULS,   0 # ADCB,   0 # ADCA,   NEXT ;C

CODE -      \ n1/u1 n2/u2 -- n3/u3   subtract n1-n2
    S ,++ SUBD,   COMA,   COMB,   1 # ADDD,   NEXT ;C

CODE NEGATE   \ x1 -- x2     two's complement
    COMA,   COMB,   1 # ADDD,   NEXT ;C

\ \\   6809 DTC: logical operations                 (c) 31mar95 bjr
CODE AND    \ x1 x2 -- x3     logical AND
    S ,+ ANDA,   S ,+ ANDB,   NEXT ;C

CODE OR     \ x1 x2 -- x3     logical OR
    S ,+ ORA,    S ,+ ORB,    NEXT ;C

CODE XOR    \ x1 x2 -- c3     logical XOR
    S ,+ EORA,   S ,+ EORB,   NEXT ;C

CODE INVERT   \ x1 -- x2       bitwise inversion
    COMA,   COMB,   NEXT ;C

CODE ><       \ x1 -- x2      swap bytes
    A B EXG,   NEXT ;C

\ \\   6809 DTC: arithmetic operations              (c) 31mar95 bjr
CODE 1+     \ n1/u1 -- n2/u2         add 1 to TOS
    1 # ADDD,   NEXT ;C

CODE 1-     \ n1/u1 -- n2/u2         subtract 1 from TOS
    1 # SUBD,   NEXT ;C

CODE 2*     \ x1 -- x2              arithmetic left shift
    ASLB,   ROLA,   NEXT ;C

CODE 2/     \ x1 -- x2              arithmetic right shift
    ASRA,   RORB,   NEXT ;C

CODE +!     \ n/u a-addr --         add cell to memory
    D X TFR,   6 # ( D) PULS,   X 0, ADDD,   X 0, STD,
    6 # ( D) PULS,   NEXT ;C
\ \\   6809 DTC: arithmetic operations              (c) 31mar95 bjr
CODE LSHIFT    \ x1 u -- x2      logical shift left u places
    D X TFR,   6 # ( D) PULS,   X 0, LEAX,   NE IF,
        BEGIN,   LSLB,   ROLA,   X -1 , LEAX,   EQ UNTIL,
    THEN,   NEXT ;C

CODE RSHIFT    \ x1 u -- x2      logical shift right u places
    D X TFR,   6 # ( D) PULS,   X 0, LEAX,   NE IF,
        BEGIN,   LSRA,   RORB,   X -1 , LEAX,   EQ UNTIL,
    THEN,   NEXT ;C

\ \\   6809 DTC: comparison operations              (c) 31mar95 bjr
CODE 0=     \ n/u -- flag     return true if TOS=0
    0 # CMPD,   EQ IF,
        HERE EQU TOSTRUE   -1 # LDD,  NEXT
    THEN,   CLRA,   CLRB,   NEXT ;C

CODE 0<     \ n/u -- flag     true if TOS negative
    TSTA,   TOSTRUE BMI,   CLRA,   CLRB,   NEXT ;C

CODE =      \ x1 x2 -- flag   test x1=x2
    S ,++ SUBD,   TOSTRUE BEQ,   CLRA,   CLRB,   NEXT ;C

CODE <>     \ x1 x2 -- flag   test not equal
    S ,++ SUBD,   TOSTRUE BNE,   CLRA,   CLRB,   NEXT ;C

\ \\   6809 DTC: comparison operations              (c) 31mar95 bjr
CODE <      \ n1 n2 -- flag     test n1<n2, signed
    S ,++ SUBD,   TOSTRUE BGT,   CLRA,   CLRB,   NEXT ;C

CODE >      \ n1 n2 -- flag     test n1>n2, signed
    S ,++ SUBD,   TOSTRUE BLT,   CLRA,   CLRB,   NEXT ;C

CODE U<     \ n1 n2 -- flag     test n1<n2, unsigned
    S ,++ SUBD,   TOSTRUE BHI,   CLRA,   CLRB,   NEXT ;C

CODE U>     \ n1 n2 -- flag     test n1>n2, unsigned
    S ,++ SUBD,   TOSTRUE BLO,   CLRA,   CLRB,   NEXT ;C

\ \\   6809 DTC: branch and loop operations         (c) 31mar95 bjr
CODE BRANCH   \ --         branch always
    Y 0, LDY,   NEXT ;C

CODE ?BRANCH  \ x --      branch if TOS zero
    0 # CMPD,   EQ IF,   6 # ( D) PULS,   Y 0, LDY,   NEXT
                THEN,   6 # ( D) PULS,   Y 2 , LEAY,   NEXT ;C

CODE (DO)     \ n1|u1 n2|u2 --    R: -- sys1 sys2
    D X TFR,   HEX 8000 # LDD,   S ,++ SUBD,   \ fudg=8000-limit
    6 # ( D) PSHU,   X D, LEAX,   10 # ( X) PSHU,   \ start+fudg
    6 # ( D) PULS,   NEXT ;C

CODE UNLOOP   \ --    R: sys1 sys2 --     drop loop parameters
    U 4 , LEAU,   NEXT ;C

\ \\   6809 DTC: branch and loop operations         (c) 31mar95 bjr
CODE (LOOP)   \ R: sys1 sys2 -- | sys1 sys2   run-time for LOOP
    6 # ( D) PSHS,   U 0, LDD,   1 # ADDD,   VC IF,
        HERE EQU TAKELOOP   U 0, STD,   Y 0, LDY,
        6 # PULS,   NEXT
    THEN,   Y 2 , LEAY,   U 4 , LEAU,   6 # PULS,   NEXT ;C

CODE (+LOOP)  \ n --   R: sys1 sys2 -- | sys1 sys2    for +LOOP
    U 0, ADDD,   TAKELOOP BVC,
    Y 2 , LEAY,   U 4 , LEAU,   6 # PULS,   NEXT ;C

CODE I        \ -- n   R: sys1 sys2 -- sys1 sys2     loop index
    6 # ( D) PSHS,   U 0, LDD,   U 2 , SUBD,   NEXT ;C

CODE J        \ -- n   R: 4*sys -- 4*sys         2nd loop index
    6 # ( D) PSHS,   U 4 , LDD,   U 6 , SUBD,   NEXT ;C
\ \\   6809 DTC: multiply                                24mar15nac
CODE UM*      \ u1 u2 -- ud   16*16->32 unsigned multiply
   HEX 16 # ( X,D) PSHS,                    \ push temporary, u2
   S 5 , LDA,  S 1 , LDB,  MUL,  S 2 , STD,   \ 1lo*2lo
   S 4 , LDA,  S 1 , LDB,  MUL,               \ 1hi*2lo
     S 2 , ADDB,  0 # ADCA,  S 1 , STD,
   S 5 , LDA,  S 0, LDB,  MUL,                \ 1lo*2hi
     S 1 , ADDD,  S 1 , STD,  CLRA,  ROLA,    \ cy in A
   S 0, LDB,  S 0, STA,  S 4 , LDA,  MUL,     \ 2hi*1hi
     S 0, ADDD,                               \ hi result in D
   S 2 , LDX,  S 4 , LEAS,  S 0, STX,  NEXT ;C   \ lo result

\ \\   6809 DTC: divide                             (c) 25apr95 bjr
CODE UM/MOD      \ ud u1 -- rem quot   32/16->16 divide
   HEX 6 # PSHS,   10 # LDX,      \ save u1 in mem
   S 5 , ASL,  S 4 , ROL,         \ initial shift (lo 16)
   BEGIN,
      S 3 , ROL,  S 2 , ROL,   S 2 , LDD,   \ shift left hi 16
      CS IF,                  \ 1xxxx: 17 bits, subtract is ok
         S 0, SUBD,  S 2 , STD,  0FE # ANDCC,   \ clear cy
      ELSE,                   \ 0xxxx: 16 bits, test subtract
         S 0, SUBD,  CC IF,  S 2 , STD,  THEN,  \ cs=can't subtr
      THEN,                   \ cy=0 if sub ok, 1 if no subtract
      S 5 , ROL,  S 4 , ROL,  \ rotate cy into result
   X -1 , LEAX,  EQ UNTIL,    \ loop 16 times
   S 4 , LDD,  COMA,  COMB,   \ invert to get true quot in D
   S 2 , LDX,  S 4 , STX,  S 4 , LEAS,   \ save rem, clean stack
   NEXT ;C
\ \\   6809 DTC: block and string operations        (c) 31mar95 bjr
CODE FILL   \ c-addr u char --    fill mem with char
    HEX 20 # ( Y) PSHU,   30 # ( X,Y) PULS,   \ D=char X=u Y=adr
    0 # CMPX,  NE IF,
        BEGIN,   Y ,+ STB,   X -1 , LEAX,   EQ UNTIL,
    THEN,   6 # ( D) PULS,   20 # ( Y) PULU,  NEXT, ;C

CODE S=    \ c-addr1 c-addr2 u -- n    string compare 1:2
    S 2 , ADDD,   S 2 , LDX,   S 2 , STY,     \ X=src D=end
    S 0, LDY,   S 0, STD,   CLRB,             \ Y=dst B=0
    BEGIN,   S 0, CMPX,   NE WHILE,   X ,+ LDA,   Y ,+ SUBA,
        NE IF,   0 # SBCB,   B A TFR,   1 # ORB,
                 HEX 30 # ( X,Y) PULS,   NEXT,   THEN,
    REPEAT,   B A TFR,   HEX 30 # ( X,Y) PULS,   NEXT, ;C

\ \\   6809 DTC: block and string operations        (c) 31mar95 bjr
CODE CMOVE  \ c-addr1 c-addr2 u --   move from bottom 1->2
    S 2 , ADDD,   S 2 , LDX,   S 2 , STY,     \ X=src D=end
    S 0, LDY,   S 0, STD,                     \ Y=dst
    BEGIN,   S 0, CMPX,   NE WHILE,   X ,+ LDB,   Y ,+ STB,
    REPEAT,   HEX 30 # ( X,Y) PULS,   6 # ( D) PULS,   NEXT ;C

CODE CMOVE>  \ c-addr1 c-addr2 u --   move from top 1->2
    S 2 , LDX,  X D, LEAX,   S 2 , STY,       \ X=src D=u
    S 0, LDY,   Y D, LEAY,                    \ Y=dst
    BEGIN,   S 0, CMPY,   NE WHILE,   X -, LDB,   Y -, STB,
    REPEAT,   HEX 30 # ( X,Y) PULS,   6 # ( D) PULS,   NEXT ;C

\ \\   6809 DTC: block and string operations        (c) 31mar95 bjr
ASM: HERE EQU SKIPEXIT   Y -1 , LEAY,
HERE EQU SKIPDONE  HEX 20 # PSHS,  X D TFR,  20 # PULU, NEXT ;C

CODE SKIP   \ c-addr u c -- c-addr' u'   skip matching chars
    HEX 20 # ( Y) PSHU,   30 # ( X,Y) PULS,   \ D=char X=u Y=adr
    0 # CMPX,   NE IF,
        BEGIN,   Y ,+ CMPB,   SKIPEXIT BNE,   X -1 , LEAX,
    EQ UNTIL,   THEN,   SKIPDONE BRA,  ;C

CODE SCAN   \ c-addr u c -- c-addr' u'   find matching char
    HEX 20 # ( Y) PSHU,   30 # ( X,Y) PULS,   \ D=char X=u Y=adr
    0 # CMPX,   NE IF,
        BEGIN,   Y ,+ CMPB,   SKIPEXIT BEQ,   X -1 , LEAX,
    EQ UNTIL,   THEN,   SKIPDONE BRA,  ;C

\ \\   6809 DTC: system dependencies                (c) 21apr95 bjr


\ These words are shorter in CODE than as colon definitions!
CODE ALIGNED  NEXT ;C               \ a1 -- a2  align address
CODE ALIGN   NEXT ;C                \ --        align HERE
CODE CELL+   2 # ADDD,  NEXT ;C     \ a1 -- a2  add cell size
CODE CELLS   ASLB,  ROLA,  NEXT ;C  \ n1 -- n2  cells->adr units
CODE CHAR+   1 # ADDD,  NEXT ;C     \ a1 -- a2  add char size
CODE CHARS   NEXT ;C                \ n1 -- n2  chars->adr units
CODE >BODY   3 # ADDD,  NEXT ;C     \ xt -- a-addr    cfa->pfa

AKA 1- CHAR-
\ Note: CELL, a constant, must be defined after CONSTANT.

\ \\   6809 DTC: system dependencies                (c) 21apr95 bjr
HEX
: COMPILE,   , ;                 \ xt --   append execution tokn
: !CF       0BD OVER C! 1+ ! ;   \ adrs cfa --   set code field
: ,CF       HERE !CF 3 ALLOT ;   \ adrs --     append code field
: !COLON    -3 ALLOT <DOCOLON> ,CF ;   \ --  changes last c.f.
: ,EXIT     ['] EXIT COMPILE, ;  \ --      append EXIT action
: ,BRANCH   , ;                  \ xt --   append branch instr.
: ,DEST     , ;                  \ dest --  append dest'n adrs
: !DEST     ! ;                  \ dest adr --   change dest'n

\ \\   6809 DTC: dodoes (does>) does>               (c) 18apr95 bjr

ASM:  HERE RESOLVES DODOES   HERE EQU <DODOES>
   HEX 20 # ( Y) PSHU,   20 # ( Y) PULS,   \ adrs of DODOES code
   10 # ( X) PULS,   6 # ( D) PSHS,   X D TFR,  \ adrs of data
   NEXT, ;C
DECIMAL   \ to keep ,CF from compiling as a hex number

: (DOES>)   R>  LATEST @ NFA>CFA  !CF ;

: DOES>    ['] (DOES>) COMPILE,   <DODOES> ,CF ;   IMMEDIATE

\ \\   6809 DTC: defining words                     (c) 21apr95 bjr
: :   CREATE HIDE ] !COLON ;

: ;   REVEAL ,EXIT  [COMPILE] [  ;   IMMEDIATE

: CONSTANT   CREATE , ;CODE
    HEX 10 # ( X) PULS,   6 # ( D) PSHS,   X 0, LDD,   NEXT, ;C
EMULATE:  TCREATE T, MDOES> T@  ;EMULATE

: VARIABLE   CREATE CELL ALLOT ;
EMULATE:  TCREATE 0 T, MDOES>   ;EMULATE

: USER   CREATE , ;CODE
    HEX 10 # ( X) PULS,   6 # ( D) PSHS,       \ get pfa in X
    DPR A TFR,  CLRB,   X 0, ADDD,   NEXT, ;C  \ UP+offset -> D
EMULATE:  TCREATE T, MDOES> .UNDEF  ;EMULATE

\ \\   High level: control structures               (c) 21apr95 bjr

\ -- adrs        conditional forward branch
: IF  ['] ?BRANCH ,BRANCH  HERE DUP ,DEST ;
EMULATE:  M['] ?BRANCH T,  THERE DUP T, ;EMULATE  IMMEDIATE

\ adrs --        resolve forward branch
: THEN  HERE SWAP !DEST ;
EMULATE:  THERE SWAP T! ;EMULATE          IMMEDIATE

\ adrs1 -- adrs2   branch for IF..ELSE
: ELSE ['] BRANCH ,BRANCH  HERE DUP ,DEST SWAP [COMPILE] THEN ;
EMULATE:  M['] BRANCH T,  THERE DUP T,
    SWAP  THERE SWAP T! ;EMULATE         IMMEDIATE

\ \\   High level: control structures               (c) 21apr95 bjr
: BEGIN   HERE ;  \ -- adrs     target for backward branch
EMULATE: THERE   ;EMULATE        IMMEDIATE

\ adrs --     conditional backward branch
: UNTIL               ['] ?BRANCH ,BRANCH  ,DEST ;
EMULATE:  M['] ?BRANCH T, T, ;EMULATE  IMMEDIATE

\ adrs --     unconditional backward branch
: AGAIN    ['] BRANCH ,BRANCH  ,DEST ;
EMULATE:  M['] BRANCH T,  T, ;EMULATE   IMMEDIATE

\ -- adrs     branch for WHILE loop
: WHILE [COMPILE] IF ;
EMULATE:  M['] ?BRANCH T,  THERE DUP T, ;EMULATE  IMMEDIATE

\ \\   High level: control structures               (c) 21apr95 bjr
\ adrs1 adrs2 ---   resolve WHILE loop
: REPEAT              SWAP [COMPILE] AGAIN [COMPILE] THEN ;
EMULATE:  SWAP  M['] BRANCH T,  T,
    THERE SWAP T! ;EMULATE          IMMEDIATE

: >L   CELL LP +!  LP @ ! ;
: L>   LP @ @  CELL NEGATE LP +! ;

\ -- adrs  L: -- 0
: DO                  ['] (DO) ,BRANCH  HERE  0 >L ;
EMULATE: M['] (DO) T,  THERE  0 T>L  ;EMULATE  IMMEDIATE

: LEAVE  ['] UNLOOP COMPILE,    ['] BRANCH ,BRANCH   HERE DUP ,DEST  >L ;
EMULATE:  M['] UNLOOP T,
    M['] BRANCH T,  THERE DUP T, T>L  ;EMULATE   IMMEDIATE
\ \\   High level: control structures               (c) 21apr95 bjr
 \ adrs xt --  L: 0 a1 a2 .. aN --
: ENDLOOP   ,BRANCH ,DEST       BEGIN L> ?DUP WHILE [COMPILE] THEN REPEAT ;

ALSO FORTH ALSO META DEFINITIONS
: TENDLOOP   T, T, BEGIN TL> ?DUP WHILE THERE SWAP T! REPEAT ;
PREVIOUS PREVIOUS DEFINITIONS

: LOOP   ['] (LOOP) ENDLOOP ;
EMULATE:  M['] (LOOP) TENDLOOP  ;EMULATE  IMMEDIATE

: +LOOP  ['] (+LOOP) ENDLOOP ;
EMULATE:  M['] (+LOOP) TENDLOOP  ;EMULATE  IMMEDIATE

\ \\   High level: system variables and constants        24feb16nac
HEX  2 CONSTANT CELL     \ system dependent constant
    20 CONSTANT BL
    7E CONSTANT TIBSIZE

HEX -80 USER TIB      \ -- a-addr   Terminal Input Buffer
      0 USER U0       \ -- a-addr   current user area adrs
      2 USER BASE     \ -- a-addr   holds conversion radix
      4 USER DP       \ -- a-addr   holds dictionary pointer
      6 USER BLK      \ -- a-add    holds 0 or blk being int'd
      8 USER SCR      \ -- a-add    blk most recently LISTed
      A USER BLKADRS  \ -- a-addr   two cells: lba2 lba10
      E USER BSTATE   \ -- a-addr   5 cells: f b0 b1 b2 b3
     18 USER LATEST   \ -- a-addr   last word in dictionary

\ ^-variables from U0 to here initialised by UINIT in scr120 -^

     1A USER >IN      \ -- a-addr   holds offset into TIB
     1C USER STATE    \ -- a-addr   holds compiler state
     1E USER 'SOURCE  \ -- a-addr   two cells: length, address
     22 USER HP       \ -- a-addr   HOLD pointer
     24 USER LP       \ -- a-addr   leave-stack pointer

     26 USER YT       \ -- a-addr   User application variable
     28 USER MT       \ -- a-addr   User application variable
     2A USER RND      \ -- a-addr   User application variable

    100 USER S0       \ -- a-addr   end of parameter stack
    128 USER PAD      \ -- a-addr   user PAD buffer/end of hold
    180 USER L0       \ -- a-addr   bottom of leave stack
    200 USER R0       \ -- a-addr   end of return stack

\ \\   High level: arithmetic operators             (c) 31mar95 bjr

\ n -- d   single -> double precision
: S>D             DUP 0< ;

\ n1 n2 -- n3   negate n1 if n2 negative
: ?NEGATE         0< IF NEGATE THEN ;

\ n1 -- n2      absolute value
: ABS    DUP ?NEGATE ;

\ d1 -- d2      negate, double precision
: DNEGATE    SWAP INVERT SWAP INVERT 1 M+ ;

\ d1 n -- d2    negate d1 if n negative
: ?DNEGATE    0< IF DNEGATE THEN ;

\ d1 -- d2      absolute value, double precision
: DABS    DUP ?DNEGATE ;

\ \\   High level: arithmetic operators             (c) 31mar95 bjr

\ n1 n2 -- d       signed 16*16->32 multiply
: M*              2DUP XOR >R    SWAP ABS SWAP ABS UM*    R> ?DNEGATE ;


\ d1 n1 -- n2 n3   symmetric signed division
: SM/REM
    2DUP XOR >R
    OVER >R
    ABS >R DABS R> UM/MOD
    SWAP R> ?NEGATE
    SWAP R> ?NEGATE ;

\ \\   High level: arithmetic operators             (c) 31mar95 bjr
: FM/MOD      \ d1 n1 -- n2 n3   floored signed division
    DUP >R
    SM/REM
    DUP 0< IF
        SWAP R> +
        SWAP 1-
    ELSE  R> DROP  THEN ;

: *          \ n1 n2 -- n3        signed multiply
    M* DROP ;
: /MOD       \ n1 n2 -- n3 n4     signed divide/remainder
    >R S>D R> FM/MOD ;
: /          \ n1 n2 -- n3        signed divide
    /MOD NIP ;

\ \\   High level: arithmetic operators             (c) 31mar95 bjr
: MOD         \ n1 n2 -- n3       signed remainder
    /MOD DROP ;
: */MOD       \ n1 n2 n3 -- n4 n5   n1*n2/n3, remainder&quotient
    >R M* R> FM/MOD ;
: */          \ n1 n2 n3 -- n4      n1*n2/n3
    */MOD NIP ;

: MAX         \ n1 n2 -- n3         signed maximum
    2DUP < IF SWAP THEN DROP ;
: MIN         \ n1 n2 -- n3         signed minimum
    2DUP > IF SWAP THEN DROP ;

\ \\   High level: double operators                 (c) 31mar95 bjr
: 2@          \ a-addr -- x1 x2     fetch 2 cells
    DUP CELL+ @ SWAP @ ;
: 2!          \ x1 x2 a-addr --     store 2 cells
    SWAP OVER ! CELL+ ! ;
: 2DROP       \ x1 x2 --            drop 2 cells
    DROP DROP ;
: 2DUP        \ x1 x2 -- x1 x2 x1 x2   dup top 2 cells
    OVER OVER ;
: 2SWAP       \ x1 x2 x3 x4 -- x3 x4 x1 x2    per diagram
    ROT >R ROT R> ;
: 2OVER       \ x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2   per diagram
    >R >R 2DUP R> R> 2SWAP ;

\ \\   High level: input/output                     (c) 31mar95 bjr
HEX
: COUNT       \ c-addr1 -- c-addr2 u    counted->addr/length
    DUP CHAR+ SWAP C@ ;
: CR          \ --                      output newline
    0D EMIT 0A EMIT ;
: SPACE       \ --                      output a space
    BL EMIT ;
: SPACES      \ u --                    output u spaces
    BEGIN DUP WHILE SPACE 1- REPEAT DROP ;
: UMIN        \ u1 u2 -- u              unsigned minimum
    2DUP U> IF SWAP THEN DROP ;
: UMAX        \ u1 u2 -- u              unsigned maximum
    2DUP U< IF SWAP THEN DROP ;

\ \\   High level: input/output                     (c) 31mar95 bjr
: ACCEPT      \ c-addr +n -- +n'   get line from terminal
    OVER + 1- OVER
    BEGIN KEY
    DUP 0D <> WHILE
        DUP EMIT
        DUP 8 = IF  DROP 1-  >R OVER R> UMAX
              ELSE  OVER C!  1+ OVER UMIN
        THEN
    REPEAT
    DROP NIP SWAP - ;

: TYPE        \ c-addr +n --        type line to terminal
    ?DUP IF
        OVER + SWAP DO I C@ EMIT LOOP
    ELSE DROP THEN ;
\ \\   High level: input/output                     (c) 31mar95 bjr
: (S")        \ -- c-addr u        run-time code for S"
    R> COUNT 2DUP + ALIGN >R ;

ALSO FORTH ALSO META DEFINITIONS
: TS"   22 WORD DUP C@ 1+ THERE OVER TALLOT SWAP >TCMOVE ;
PREVIOUS PREVIOUS DEFINITIONS

: S"          \ --             compile in-line string
    ['] (S") COMPILE,
    22 WORD   C@ 1+ ALIGNED  ALLOT ;
EMULATE:  M['] (S") T,  TS"  ;EMULATE   IMMEDIATE

: ."          \ --             compile string to print
    [COMPILE] S" ['] TYPE COMPILE, ;
EMULATE:  M['] (S") T,  TS"  M['] TYPE T,  ;EMULATE  IMMEDIATE
\ \\   High level: numeric output                   (c) 31mar95 bjr
: UD/MOD      \ ud1 u2 -- u3 ud4     32/16->32 divide
    >R 0 R@ UM/MOD  ROT ROT R> UM/MOD ROT ;
: UD*         \ ud1 u2 -- ud3        32*16->32 multiply
    DUP >R UM* DROP  SWAP R> UM* ROT + ;
: HOLD        \ char --             add char to output string
    -1 HP +!  HP @ C! ;
: <#          \ --                  begin numeric conversion
    PAD HP ! ;
: >DIGIT      \ n -- c              convert to 0..9A..Z
    DUP 9 > 7 AND + 30 + ;
: #           \ ud1 -- ud2          convert 1 digit of output
    BASE @ UD/MOD ROT >DIGIT HOLD ;
: #S          \ ud1 -- ud2          convert remaining digits
    BEGIN # 2DUP OR 0= UNTIL ;

\ \\   High level: numeric output                   (c) 31mar95 bjr
: #>          \ ud1 -- c-addr u      end conversion, get string
    2DROP HP @ PAD OVER - ;
: SIGN        \ n --                 add minus sign if n<0
    0< IF 2D HOLD THEN ;
: U.          \ u --                 display u unsigned
    <# 0 #S #> TYPE SPACE ;
: .           \ n --                 display n signed
    <# DUP ABS 0 #S ROT SIGN #> TYPE SPACE ;
: DECIMAL     \ --                   set number base to decimal
    0A BASE ! ;
: HEX         \ --                   set number base to hex
    10 BASE ! ;

\ \\   High level: dictionary management            (c) 31mar95 bjr
: HERE        \ -- addr              returns dictionary ptr
    DP @ ;
: ALLOT       \ n --             allocate n adr units in dict
    DP +! ;
: ,           \ x --             append cell to dict
    HERE !  1 CELLS ALLOT ;
: C,          \ char --          append char to dict
    HERE C!  1 CHARS ALLOT ;

\ \\   High level: interpreter                      (c) 31mar95 bjr
: SOURCE      \ -- adr n         current input buffer
    'SOURCE 2@ ;
: /STRING     \ a u n -- a+n u-n           trim string
    ROT OVER + ROT ROT - ;
: >COUNTED    \ src n dst --        copy to counted string
    2DUP C! CHAR+ SWAP CMOVE ;
: WORD        \ char -- c-addr      word delim'd by char
    DUP  SOURCE >IN @ /STRING
    DUP >R   ROT SKIP
    OVER >R  ROT SCAN
    DUP IF CHAR- THEN
    R> R> ROT -   >IN +!
    TUCK -
    HERE >COUNTED   HERE
    BL OVER COUNT + C! ;
\ \\   High level: interpreter                           01apr15nac
: NFA>LFA     \ nfa -- lfa      name adr -> link field
    2 - ;
: NFA>CFA     \ nfa -- cfa      name adr -> code field
    COUNT 3F AND + ;
: IMMED?      \ nfa -- f        fetch immediate flag
    C@ 40 AND ;
: FIND        \ c-addr -- c-addr 0/1/-1   not found/immed/normal
    LATEST @ BEGIN              \ -- a nfa
        2DUP C@ BF AND SWAP C@  \ -- a nfa n1 n2
        = IF 2DUP CHAR+ SWAP DUP CHAR+ SWAP C@ S= ELSE 1 THEN
        DUP IF DROP NFA>LFA @ DUP THEN
    0= UNTIL                    \ -- a nfa  OR  a 0
    DUP IF                      \ if found, check immed status
        NIP DUP NFA>CFA         \ -- nfa xt
        SWAP IMMED?  0= 1 OR  THEN ;
\ \\   High level: interpreter                      (c) 31mar95 bjr
: LITERAL     \ x --       append numeric literal
    STATE @ IF  ['] LIT COMPILE,  I, THEN ;
EMULATES TLITERAL                       IMMEDIATE

HEX
: DIGIT?      \ c -- n -1 | x 0    true if c is a valid digit
   DUP 39 > 100 AND +              \ silly looking,
   DUP 140 > 107 AND -  30 -       \ but it works!
   DUP BASE @ U< ;
: ?SIGN       \ adr n -- adr' n' f   get optional sign
   OVER C@                 \ -- adr n c
   2C - DUP ABS 1 = AND    \ -- +=-1, -=+1, else 0
   DUP IF 1+               \ +=0, -=+2        NZ=negative
       >R 1 /STRING R>     \ adr' n' f
   THEN ;
\ \\   High level: interpreter                      (c) 31mar95 bjr
: >NUMBER     \ ud adr u -- ud' adr' u'  conv. string to number
    BEGIN DUP WHILE
        OVER C@ DIGIT?
        0= IF DROP EXIT THEN
        >R 2SWAP BASE @ UD*
        R> M+ 2SWAP   1 /STRING
    REPEAT ;

: ?NUMBER     \ c-addr -- n -1 | c-addr 0     string->number
    DUP  0 0 ROT COUNT     \ -- ca ud adr n
    ?SIGN >R  >NUMBER      \ -- ca ud adr' n'
    IF  R> 2DROP 2DROP 0   \ -- ca 0   (error)
    ELSE  2DROP NIP R>
        IF NEGATE THEN -1  \ -- n -1   (ok)
    THEN ;
\ \\   High level: interpreter                      (c) 31mar95 bjr
: INTERPRET   \ i*x c-addr u -- j*x   interpret given buffer
    'SOURCE 2!  0 >IN !
    BEGIN  BL WORD  DUP C@ WHILE        \ -- textadr
        FIND  ?DUP IF                   \ -- xt 1/-1
            1+ STATE @ 0= OR            \ immed or interp?
            IF EXECUTE ELSE COMPILE, THEN
        ELSE                            \ -- textadr
            ?NUMBER IF  [COMPILE] LITERAL  \ converted ok
            ELSE  COUNT TYPE 3F EMIT CR ABORT  THEN  \ error
        THEN
    REPEAT DROP ;

: EVALUATE   \ i*x c-addr u -- j*x   interpret string
    0 BLK !  'SOURCE 2@ >R >R  >IN @ >R   INTERPRET
    R> >IN !  R> R> 'SOURCE 2!  ;
\ \\   High level: interpreter                      (c) 07may95 bjr
: QUIT        \ --   R: i*x --    interpret from keyboard
    L0 LP !  R0 RP!  0 STATE !    \ reset stacks, state
    BEGIN
        TIB DUP TIBSIZE ACCEPT SPACE
        INTERPRET
        CR STATE @ 0= IF ." OK-" DEPTH . THEN
    AGAIN ;

: ABORT       \ i*x --  R: j*x --   clear stack and QUIT
    S0 SP!  QUIT ;
: ?ABORT      \ f c-addr u --       abort and print message
    ROT IF TYPE ABORT THEN 2DROP ;
: ABORT"      \ i*x 0 -- i*x        abort, print inline msg
    [COMPILE] S" ['] ?ABORT COMPILE, ;
EMULATE:  M['] (S") T,  TS"  M['] ?ABORT T,  ;EMULATE IMMEDIATE
\ \\   High level: interpreter                           24feb16nac
: '           \ -- xt        find word in dictionary
    BL WORD FIND   0= ABORT" ?" ;
: CHAR        \ -- char      parse ASCII character
    BL WORD 1+ C@ ;
: [CHAR]      \ --           compile character literal
    CHAR  ['] LIT COMPILE,  I, ;  IMMEDIATE
( *host action?* )

: (           \ --           skip input until )
    29 WORD DROP ;           IMMEDIATE

: \           \ --           skip input until end of buffer
    BLK @ IF ( if blk file skip to next 64-char boundary)
        >IN @ DUP 40 MOD 40 SWAP - + >IN ! EXIT
    THEN SOURCE >IN ! DROP ; IMMEDIATE
\ \\   High level: compiler                              01apr15nac
: CREATE      \ --        create an empty definition
    LATEST @ I,             \ link field
    IHERE LATEST !          \ new "latest" link
    BL IWORD IC@ 1+ IALLOT  \ name field
    <DOCREATE> ,CF ;        \ code field

: RECURSE     \ --        recurse current definition
    LATEST @ NFA>CFA COMPILE, ;  IMMEDIATE
( *host action?* )

: [           \ --        enter interpretive state
    0 STATE ! ;  IMMEDIATE
: ]           \ --        enter compiling state
    -1 STATE ! ;

\ \\   High level: compiler                         (c) 331mar15nac
HEX
: HIDE        \ --        "hide" latest definition
    LATEST @ DUP  IC@ 80 OR SWAP IC! ;
: REVEAL      \ --        "reveal" latest definition
    LATEST @ DUP  IC@ 7F AND SWAP IC! ;
: IMMEDIATE   \ --        make last definition immediate
    LATEST @ DUP  IC@ 40 OR  SWAP IC! ;
: [']         \ --        find word and compile as literal
    '  ['] LIT COMPILE,  I, ;  IMMEDIATE

\ \\   High level: compiler                         (c) 31mar95 bjr
: POSTPONE    \ --    postpone compile action of word
    BL WORD FIND  DUP 0= ABORT" ?"  \ find word
    0< IF  ['] LIT COMPILE,  I,    \ non-immed: compiles later
           ['] COMPILE, COMPILE,   \ add "LIT xt COMPILE," to df
    ELSE  COMPILE,  THEN ;  IMMEDIATE   \ immed: compile into df
( *host action?* )

\ \\   High level: other operations                 (c) 25apr95 bjr
: WITHIN   \ n1|u1 n2|u2 n3|u3 -- f   n2<=n1<n3?
    OVER - >R - R> U< ;

: MOVE     \ addr1 addr2 u --    smart move
    >R 2DUP SWAP DUP R@ +
    WITHIN IF  R> CMOVE>  ELSE  R> CMOVE  THEN ;

: DEPTH    \ -- n
    SP@ S0 SWAP - 2/ ;      \ 16 BIT VERSION!

: ENVIRONMENT?   \ c-addr u -- i*x true    system query
    2DROP 0 ;    \          -- false

\ \\   High level: utility words                    (c)  31mar15nac
: WORDS         \ --     list all words in dictionary
    LATEST @ BEGIN
        DUP COUNT 3F AND TYPE SPACE
        NFA>LFA @
    DUP 0= UNTIL
    DROP ;
EMULATES WORDS

: .S            \ --     print contents of stack
    DEPTH .
    SP@ S0 - IF
        SP@ S0 2 - DO  I @ h.  -2 +LOOP
    THEN ;
EMULATES .S

\ \\   High level: utility words                    (c)  17jan16nac
: MARKER ( "foo" --) \ run-time: foo deletes definitions
  HERE LATEST @ CREATE , , \ prev-nfa, prev-here
  DOES> DUP @ SWAP CELL+ @ \ latest here
        DP ! LATEST ! ;
( *host action?* )