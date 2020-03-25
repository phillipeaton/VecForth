HEX

ALSO META ASSEMBLER DEFINITIONS

\ Block 30 -----------------------------------------------------
\   6809 assembler: utilities                         01apr15nac
\
: WITHIN   ROT SWAP OVER   \ n lo hi -- f | test within limits
   < ROT ROT > OR 0= ;
: 8BIT?   -80 7F WITHIN ;
: 5BIT?   -10 0F WITHIN ;

\ : ALIGN ;  ( for 8-bit versions)
: N,       , ;
: NCREATE  CREATE ;

\ Block 31 -----------------------------------------------------
\   6809 assembler: addressing modes            ( 01 feb 93 bjr)
\
: OPCODE,                  \ store opcode with prefix (if any)
   DUP 0FF00 AND  IF T, ELSE TC, THEN  ;

VARIABLE MODE   \ 0=immed, 10=direct, 20=indexed, 30=extended
: #   0 MODE ! ;    : <>   10 MODE ! ;

: ?ADRERR   IF ." Illegal addressing mode " DECIMAL BLK @ .
            >IN @ 40 /MOD . .  ABORT THEN ;
: INDEXREG   20 MODE !   \ rval postbyte -- postbyte |
   SWAP  1-  DUP 0 3 WITHIN 0= ?ADRERR   \ must be x,y,u, or s
   20 * OR ;                             \ put reg # in postbyte
: XMODE   NCREATE N,    \ postbyte --  | Simple Indexed Modes
   DOES>  @ INDEXREG ;  \ rval -- postbyte
84 XMODE 0,    86 XMODE A,    85 XMODE B,    8B XMODE D,
80 XMODE ,+    81 XMODE ,++   82 XMODE -,    83 XMODE --,

\ Block 32 -----------------------------------------------------
\   6809 assembler: addressing modes            ( 01 feb 93 bjr)
\
: ,      SWAP 89 INDEXREG ;   \ rval n -- n postbyte |
: ,PCR   20 MODE !  8D ;      \ n -- n postbyte |

: []   MODE @ 20 =    \ Indexed:  postbyte -- postbyte
                      \ Extended:        n -- n postbyte
   IF  DUP 9D AND 80 = ?ADRERR   10 +    \ Indexed Indirect
   ELSE  20 MODE !  9F THEN ;            \ Extended Indirect

: RESET   30 MODE ! ;      RESET

\ register definitions
0 CONSTANT D    1 CONSTANT X    2 CONSTANT Y    3 CONSTANT U
4 CONSTANT S    5 CONSTANT PC   8 CONSTANT A    9 CONSTANT B
0A CONSTANT CCR    0B CONSTANT DPR

\ Block 33 -----------------------------------------------------
\   6809 assembler: inherent instructions       ( 01 feb 93 bjr)
\
: INHOP   NCREATE  N,      \ opcode -- | Inherent Addressing
   DOES>   @ OPCODE, RESET ; \ -- | lay one or two bytes

3A INHOP ABX,   48 INHOP ASLA,  58 INHOP ASLB,  47 INHOP ASRA,
57 INHOP ASRB,  4F INHOP CLRA,  5F INHOP CLRB,  43 INHOP COMA,
53 INHOP COMB,  19 INHOP DAA,   4A INHOP DECA,  5A INHOP DECB,
4C INHOP INCA,  5C INHOP INCB,  48 INHOP LSLA,  58 INHOP LSLB,
44 INHOP LSRA,  54 INHOP LSRB,  3D INHOP MUL,   40 INHOP NEGA,
50 INHOP NEGB,  12 INHOP NOP,   49 INHOP ROLA,  59 INHOP ROLB,
46 INHOP RORA,  56 INHOP RORB,  3B INHOP RTI,   39 INHOP RTS,
1D INHOP SEX,   3F INHOP SWI, 103F INHOP SWI2, 113F INHOP SWI3,
13 INHOP SYNC,  4D INHOP TSTA,  5D INHOP TSTB,

\ Block 34 -----------------------------------------------------
\   6809 assembler: immediate instructions      ( 01 feb 93 bjr)
\
: IMMOP   NCREATE  N,      \ opcode -- | Immediate Only (8-bit)
   DOES>   MODE @ ?ADRERR   @ TC, TC, RESET ;  \ operand --

3C IMMOP CWAI,  34 IMMOP PSHS,  36 IMMOP PSHU,  35 IMMOP PULS,
37 IMMOP PULU,  1C IMMOP ANDCC, 1A IMMOP ORCC,

: RROP   NCREATE  N,       \ opcode -- | Register-Register
   DOES>   @ TC,  SWAP 10 * + TC, RESET ;  \ srcrval dstrval --

1E RROP EXG,    1F RROP TFR,

\ Block 35 -----------------------------------------------------
\   6809 assembler: +mode                       ( 01 feb 93 bjr)
\
: +MODE    \ operand -- operand | modify operand per mode
   MODE @ +  DUP 0F0 AND 50 = IF 0F AND THEN ; \ change 5x to 0x

\ Block 36 -----------------------------------------------------
\   6809 assembler: pcrel, cofset                     01apr15nac
\
: PCREL   \ operand postbyte -- | lay PC relative
   SWAP  THERE 2 + -  DUP 8BIT?  \ try 8 bit relative offset
   IF  SWAP 0FE AND TC, TC,      \ it fits...lay postbyte,offset
   ELSE  1-  SWAP TC, T,  THEN ; \ no good...use 16 bit relative

: NOTINDIR?  10 AND 0= ;  \ postbyte -- f | test for indirection

: COFSET   \ operand postbyte -- | lay constant offset
   OVER 0= IF  0F0 AND 4 OR TC, DROP     \ no offset
   ELSE OVER 5BIT? OVER NOTINDIR? AND IF
      60 AND SWAP 1F AND OR TC,          \ 5 bit offset
   ELSE OVER 8BIT? IF  0FE AND TC, TC,   \ 8 bit offset
   ELSE  TC, T,  THEN THEN THEN ;        \ 16 bit offset

\ Block 37 -----------------------------------------------------
\   6809 assembler: indexed, immed                    01apr15nac
\
: EXTIND    \ operand postbyte -- | lay extended indirect
   TC, T, ;      \ lay postbyte and operand

: INDEXED   \ operand? postbyte -- | lay indexed poststuff
   DUP 8F AND           \ check postbyte for modes w/ operands
      DUP 89 = IF DROP  COFSET  ELSE      \ const.offset
      DUP 8D = IF DROP  PCREL   ELSE      \ PC relative
      DUP 8F = IF DROP  EXTIND  ELSE      \ extended indir
                  DROP  TC,  \ "simple" modes, postbyte only
   THEN THEN THEN ;

: IMMED   \ operand opcode-pfa -- | lay immediate poststuff
   CELL + @  DUP 0= ?ADRERR \ test immedsize
   1- IF T, ELSE TC, THEN ; \ lay immediate operand in reqd.size

\ Block 38 -----------------------------------------------------
\   6809 assembler: general addr instr          ( 01 feb 93 bjr)
\
: GENOP   NCREATE  N, N,   \ immedsize opcode -- | Gen'l Addr
   DOES>   DUP @ +MODE OPCODE,   \ [see below] | lay opcode
      MODE @ DUP  0 = IF DROP  IMMED         ELSE   \ immediate
             DUP 10 = IF DROP  DROP TC,      ELSE   \ direct
             DUP 20 = IF DROP  DROP INDEXED  ELSE   \ indexed
             DUP 30 = IF DROP  DROP T,       ELSE   \ extended
                         DROP
      THEN THEN THEN THEN  RESET ;

: INXOP   NCREATE  N,       \ opcode -- | Indexed Only
   DOES>   MODE @ 20 - ?ADRERR   @ OPCODE, INDEXED RESET ;
\ Stack action of general addressing instructions
\ (1) immediate, direct, extended:                 operand --
\ (2) all indexed except (3):                     postbyte --
\ (3) const.offset, PCR, extended indir:  operand postbyte --

\ Block 39 -----------------------------------------------------
\   6809 assembler: general addr instr          ( 01 feb 93 bjr)
\
1   89 GENOP ADCA,   1  0C9 GENOP ADCB,   1   8B GENOP ADDA,
1  0CB GENOP ADDB,   2  0C3 GENOP ADDD,   1   84 GENOP ANDA,
1  0C4 GENOP ANDB,   1   85 GENOP BITA,   1  0C5 GENOP BITB,
0   48 GENOP ASL,    0   47 GENOP ASR,    0   4F GENOP CLR,
1   81 GENOP CMPA,   1  0C1 GENOP CMPB,   2 1083 GENOP CMPD,
2 118C GENOP CMPS,   2 1183 GENOP CMPU,   2   8C GENOP CMPX,
2 108C GENOP CMPY,   1   88 GENOP EORA,   1  0C8 GENOP EORB,
0   43 GENOP COM,    0   4A GENOP DEC,    0   4C GENOP INC,
1   86 GENOP LDA,    1  0C6 GENOP LDB,    2  0CC GENOP LDD,
2 10CE GENOP LDS,    2  0CE GENOP LDU,    2   8E GENOP LDX,
2 108E GENOP LDY,    0   4E GENOP JMP,    0   8D GENOP JSR,
0   48 GENOP LSL,    0   44 GENOP LSR,    0   40 GENOP NEG,
1   8A GENOP ORA,    1  0CA GENOP ORB,    0   49 GENOP ROL,

\ Block 40 -----------------------------------------------------
\   6809 assembler: general addr instr          ( 01 feb 93 bjr)
\
0   46 GENOP ROR,    1   82 GENOP SBCA,   1  0C2 GENOP SBCB,
0   87 GENOP STA,    0  0C7 GENOP STB,    0  0CD GENOP STD,
0 10CF GENOP STS,    0  0CF GENOP STU,    0   8F GENOP STX,
0 108F GENOP STY,    1   80 GENOP SUBA,   1  0C0 GENOP SUBB,
2   83 GENOP SUBD,   0   4D GENOP TST,

32 INXOP LEAS,  33 INXOP LEAU,  30 INXOP LEAX,  31 INXOP LEAY,

\ Block 41 -----------------------------------------------------
\   6809 assembler: branches                          01apr15nac
\
: CONDBR   NCREATE  N,      \ opcode -- | Conditional Branch
   DOES>   @  SWAP THERE 2 + -     \ addr --
   DUP 8BIT? IF  SWAP TC, TC,                   \ 8 bit
   ELSE  10 TC, SWAP TC, 2 - T,  THEN RESET ;   \ 16 bit

: UNCBR   NCREATE  N,       \ short:long -- | Unconditional Br.
   DOES>   @  SWAP THERE 2 + -         \ addr --
   DUP 8BIT? IF  SWAP >< TC, TC,       \ 8 bit: use short opcode
   ELSE  SWAP TC, 1- T,  THEN RESET ;  \ 16 bit: use long opcode

\ Block 41 -----------------------------------------------------
\   6809 assembler: branch instructions         ( 01 feb 93 bjr)
\
24 CONDBR BCC,  25 CONDBR BCS,  27 CONDBR BEQ,  2C CONDBR BGE,
2E CONDBR BGT,  22 CONDBR BHI,  24 CONDBR BHS,  2F CONDBR BLE,
25 CONDBR BLO,  23 CONDBR BLS,  2D CONDBR BLT,  2B CONDBR BMI,
26 CONDBR BNE,  2A CONDBR BPL,  21 CONDBR BRN,  28 CONDBR BVC,
29 CONDBR BVS,  2016 UNCBR BRA,  8D17 UNCBR BSR,

\ Block 42 -----------------------------------------------------
\   6809 assembler: conditions                  ( 01 feb 93 bjr)
\
24 CONSTANT CS  25 CONSTANT CC  27 CONSTANT NE  2C CONSTANT LT
2E CONSTANT LE  22 CONSTANT LS  24 CONSTANT LO  2F CONSTANT GT
25 CONSTANT HS  23 CONSTANT HI  2D CONSTANT GE  2B CONSTANT PL
26 CONSTANT EQ  2A CONSTANT MI  21 CONSTANT ALW 28 CONSTANT VS
29 CONSTANT VC  20 CONSTANT NVR

\ Block 43 -----------------------------------------------------
\   6809 assembler: structured conditionals      (c) 17apr95 bjr
\
: IF,     \ br.opcode -- adr.next.instr  | reserve space
   TC, 0 TC, THERE ;
: ENDIF,  \ adr.instr.after.br -- | patch the forward ref.
   THERE OVER -  DUP 8BIT? 0= ?ADRERR  SWAP 1- TC! ;
: ELSE,   \ adr.after.br -- adr.after.this.br
   NVR IF,  SWAP ENDIF, ;
: BEGIN,  \ -- dest.adr
   THERE ;
: UNTIL,  \ dest.adr br.opcode --
   TC,  THERE 1+ -  DUP 8BIT? 0= ?ADRERR  TC, ;
: WHILE,  \ dest.adr br.opcode -- adr.after.this dest.adr
   IF, SWAP ;
: REPEAT, \ adr.after.while dest.adr.of.begin --
   NVR UNTIL,  ENDIF, ;
: THEN,  ENDIF, ;   : END,  UNTIL, ;

\ Block 44 -----------------------------------------------------
\   6809 assembler: next, code  6809 DTC model   (c) 01apr95 bjr
\
: ENTERCODE   RESET ;        \ used by metacompiler
: EXITCODE    ;              \ used by metacompiler

: NEXT,  Y ,++ [] JMP, ;     \ 6809 DTC
AKA NEXT, NEXT
Y CONSTANT IP   S CONSTANT SP   U CONSTANT RP   X CONSTANT W

PREVIOUS DEFINITIONS \ end of assembler!
\ : CODE        THEAD ENTERCODE ;   ...defined in metacompiler
\ : ;C          ;              \ aka END-CODE on some systems
