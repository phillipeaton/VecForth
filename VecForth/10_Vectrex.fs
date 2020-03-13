\ *************** CHROMIUM 2 Metacompiler for F83 ****************
\              (c) 1993,1995 Bradford J. Rodriguez
\
\ "I don't know why you people seem to think this is magic;
\ it's just this little chromium switch here."
\     - The Firesign Theatre
\
\ based on Image Compiler for real-Forth, (c) 1988 B.J.Rodriguez
\  described in Forth Dimensions XIV:3, XIV:4, XIV:5
\ 6809 assembler (c) 1985 B.J.Rodriguez
\  described in The Computer Journal #52, #54
\ 6809 CamelForth (c) 1995 B.J.Rodriguez
\  described in The Computer Journal, #59,60,62,64,67,69,72


\ 1\ Metacompiler & 6809 target LOAD SCREEN          12 feb 93 bjr
\ DECIMAL     2 LOAD   ( metacompiler vocabularies)
\ DECIMAL     3 LOAD   ( image to disk, big endian)
\ DECIMAL 24 26 THRU   ( DUMP & Intel hex file output)
\ DECIMAL  4  7 THRU   ( target model - needed for assembler)
\ DECIMAL 30 44 THRU   ( 6809 assembler)
\ DECIMAL  8 23 THRU   ( rest of metacompiler)
\ ONLY FORTH META TARGET DEFINITIONS
\ DECIMAL 61 120 THRU  ( 6809 source code)

                                        \ Vocabualary compiled into:
INCLUDE 20_METACOMPVOCAB.FS             \ ROOT & FORTH
INCLUDE 30_IMAGETODISK.FS               \ META
INCLUDE 40_DUMP&HEXFILEOUTPUT.FS        \ META
INCLUDE 50_TARGETMODELFORASSEMBLER.FS   \ META
INCLUDE 60_6809ASSEMBLER.FS             \ ASSEMBLER
INCLUDE 70_RESTOFMETACOMPILER.FS        \ META but a few jumps to TARGET and one ASSEMBLER
INCLUDE 80_CAMELFORTH6809.FS            \ TARGET but one META

ONLY FORTH ALSO META
HEX E000 2000 HEXFILE 6809.HEX   ( make hex file )
\ HEX 0000 2000 BINFILE 6809.BIN   ( make binary file )
.MIRRORS                         ( print undef'd references )
BYE


