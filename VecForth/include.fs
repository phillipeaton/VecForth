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

\ ****************************************************************
\ To compile target image:
\ from command prompt: gforth include.fs
\ from within gforth: include include.fs
\ ****************************************************************

use image.scr                             \ Target image gets built in this file

                                          \ Compiled into vocabualary:
include   Chromium\gforth_shim.fs         \ FORTH

include   Chromium\extensions_vocab.fs    \ ROOT, FORTH, META
include   Chromium\target_model.fs        \ META
include   Chromium\dump_file_output.fs    \ META
include   Chromium\assembler.fs           \ ASSEMBLER
include   Chromium\metacompiler.fs        \ META, TARGET, ASSEMBLER

include CamelForth\memory_map.fs          \ TARGET
include CamelForth\synonyms.fs
include    Vectrex\vectrex_equ.fs
include    Vectrex\cartheader.fs
include    Vectrex\vecfeverheader.fs
include    Vectrex\ym_music_data.fs
include    Vectrex\vector_lists.fs
include CamelForth\serial_io.fs
include CamelForth\camelforth.fs
include    Vectrex\bios_api.fs
include    Vectrex\bios_api_tests.fs
include    Vectrex\bios_api_tests_old.fs
include    Vectrex\vecfever_exit.fs
include CamelForth\latest.fs

entry entry-addr !                 \ Insert entry address into VECTREX boot code

only forth also meta
hex 0000 5000 binfile 6809.bin     \ make binary file: start, length, filename )
show-mirror off .mirrors           \ print undef'd references
bye                                \ exit back to command prompt