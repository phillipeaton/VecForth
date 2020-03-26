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

HEX

: COLD        \ -- ; cold start Forth system
    UINIT U0 #INIT CMOVE
    CR ." VecForth v0.04 2020-03-26"
    CR ." based on 6809 CamelForth v1.1 2016-03-20"
    CR ." OK-0 " ABORT ;

\ \\   Testing words: memory dump                        24mar15nac
HEX
: .H  ( n - )   0F AND 30 + DUP 39 > IF 7 + THEN EMIT ;
: .HH ( n - )   DUP 2/ 2/ 2/ 2/ .H .H ;
: .HHHH ( n - )   DUP 2/ 2/ 2/ 2/ 2/ 2/ 2/ 2/ .HH .HH ;
: H.  ( n - )   .HHHH SPACE ;
: .B  ( a - a+1 )   DUP C@ .HH SPACE 1+ ;
: .A  ( a - a+1 )   DUP C@ DUP 20 7F WITHIN 0= IF
  DROP 2E ( . ) THEN EMIT 1+ ;
: DUMP ( a n - )  0 DO CR DUP H. SPACE DUP
    .B .B .B .B .B .B .B .B SPACE .B .B .B .B .B .B .B .B
    DROP SPACE
    .A .A .A .A .A .A .A .A       .A .A .A .A .A .A .A .A
    10 +LOOP DROP ;

\ \\   6809 DTC: reset initialization               (c) 25apr95 bjr
ASM: HERE EQU ENTRY   HEX
\   CLRA,  F000 STA,  INCA,  E000 STA,  INCA,  D000 STA,
\   INCA,  C000 STA,  INCA,  B000 STA,  INCA,  A000 STA,
\   INCA,  9000 STA,  INCA,  8000 STA,  \ init mem mapping
   UP-INIT-HI # LDA,   A DPR TFR,   \ initial UP
   UP-INIT 100 + # LDS,             \ initial SP
   UP-INIT 1E9 + # LDU,             \ initial RP - Avoid Vectrex BIOS area
\   UP-INIT 200 + # LDU,             \ initial RP
\   SCCATBL # LDX,  SCCINIT JSR,     \ init serial ports
\   SCCBTBL # LDX,  SCCINIT JSR,
   ' COLD JMP,   ;C           \ enter top-level Forth word


ENTRY ENTRY-ADDR ! \ Insert entry address into Vectrex boot code ********


ASM: HERE EQU IRET   RTI,  ;C
HERE  0FFF0 ORG    \ 6809 hardware vectors
  IRET ,  IRET ,  IRET ,  IRET ,    \ tbd, SWI3, SWI2, FIRQ
  IRET ,  IRET ,  IRET ,  ENTRY ,   \ IRQ, SWI, NMI, RESET
ORG

\ \\   6809 DTC: user area initialization                24feb16nac
DECIMAL 26 CONSTANT #INIT   \ # bytes of user area init data

CREATE UINIT  HEX
   0 , 0A ,                 \ reserved,BASE
   DP-INIT ,                \ DP
   0 , 0 ,                  \ BLK,SCR
   02 , 0800 ,              \ SDcard address of block file
   0 , 0 , 0 , 0 , 0 ,      \ BSTATE flags and buffers
META ALSO FORTH TLATEST @ T, PREVIOUS TARGET    \ LATEST



\ Note that UINIT must be the *last* word in the kernel, in
\ order to set the initial LATEST as shown above.  If this is
\ not the last word, be sure to patch the LATEST value above.