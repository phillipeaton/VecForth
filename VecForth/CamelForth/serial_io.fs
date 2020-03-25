HEX

\ VecFever UART serial port interface. VecFever provides buffering (256 bytes each way?)

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

\ Block 62 -----------------------------------------------------
\   6809 DTC: SCC initialization                 (c) 28oct18 bjr
\
\ Documentation and config of the SM2 8530 UART
\
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
\
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

\ Block 63 -----------------------------------------------------
\   6809 DTC: serial I/O                         (c) 31mar95 bjr
\
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
