\ Vectrex Forth BIOS Application Programming Interface

HEX

\ GENERAL NOTES
\
\ This Vectrex Forth to Vectrex BIOS API was created by looking at two different
\ BIOS/RUM disassemblies, primarily the Fred Taft/Bruce Tomlin BIOS version used
\ in Malban's VIDE, but I also cross-referenced the original John Hall RUM
\ disassembly that is also referenced in the Vectrex Programmer's Manual Vol I and
\ II and the 'RoadsideThoughts' website. All the API calls are named as per the
\ Fred Taft/Bruce Tomlin version dissaembly.
\
\ API ROUTINES LISTED BY ROM ADDRESS (MOSTLY)
\
\ The API Routines in this file are in the order that they appear in the BIOS ROM
\ address space, except for a few that are seemingly randomly placed. The order is
\ important, because many related routines actually call the following routines.
\ E.g. at the end of Wait_Recal, the code then continues into Set_Refresh, which
\ jumps to Recalibrate, which call various other routines, the order of the
\ routines can give some clues to this flow and this which routine to call.
\
\ MALBAN TUTORIALS
\
\ Whilst part of the reason for writing this Forth to BIOS API was to enable
\ people to make Vectrex programs without having to write assembly, the tutorials
\ written by Malban that cover mostly assembly programming are still very
\ applicable and highly recommended. Several of the API test words were translated
\ from these tutorials.
\
\ FORTH AND VECRTEX BIOS USAGE OF X REGISTER
\
\ The CamelForth that underlies VecForth uses the CPU X register as a working
\ register, just like the Vectrex BIOS. For this reason, when the API routines are
\ called, unlike all the other registers, the X register is never saved, it's
\ short term work space only.
\
\ FORTH & 6809 STACK USAGE
\
\ Forth is a stack-oriented language, using two stacks. The 6809 S stack is used
\ for parameters e.g. for passing between routines. The 6809 U stack is used for
\ holding the return address for the Forth inner interpreter.
\
\ The CamelForth implementation uses the D register as the top of stack, which
\ simplifies some operations, but makes others more complex (as opposed to having
\ the top of stack in memory). On balance, this is probably the best method, but
\ has to be considered when looking at the API code in this file.
\
\ THE ASSEMBLER
\
\ Forth assembly code looks a bit strange as everything is backwards! This is
\ because the assembler is simply a Forth program that parses the input, looking
\ for numbers to put on the stack or operaters to do something with the numbers on
\ the stack.
\
\ FORTH SOURCE CODE LAYOUT INCLUDING ASSEMBLY
\
\ Forth code is laid out in 2D! Intstead of simply having long lists of single
\ instructions, Forth source code often shows those instructions horizontally and
\ vertically. In Forth's early days, source code was entered into 1k blocks of 16
\ x64 bytes each and so instructions were written horizontally and vertically to
\ save space, and this is still the norm today, even though source code in 1k
\ blocks have long since been replaced by plain text files.
\
\ SOME BIOS ROUTINES NOT REALLY NEEDED FROM FORTH
\
\ Some of the Vectrex BIOS routine entry points don't make sense from Forth, for
\ example, the memory fill operations are not really needed, Forth has flexible
\ native code calls already, which is is probably faster than using the BIOS
\ routine via an API layer. Additionally, some entry points assume certain values
\ are in certain registers, but in Forth you don't have control of the registers,
\ that's handled by the API.
\
\ RETURNING FROM BIOS ROUTINES
\
\ Several of the BIOS routines exit with specific values in registers e.g. a
\ pointer to the next address after a vector list. Potentially this could be
\ useful but I view this as unlikely, especially from a Forth program, so the API
\ calls simply delete these return values, either by dropping them from the S
\ stack or restoring registers by pulling from the U stack. If you found you
\ needed any of these values, you could tweak the BIOS API routine to suit.
\
\ API CALLS
\
\ The general case for the API calls is:
\
\ 1. Save registers that the Vectrex BIOS routine will destroy on the U stack
\ 2. Set the Direct Pointer to D0 or C8 where necessary
\ 3. Pull any parameters from the S stack and transfer into designated
\    registers, combining stack values where necessary
\ 4. Call the Vectrex BIOS routine
\ 5. Pull any remaining S stack items to drop them and put BIOS return values
\    onto the S stack where necessary
\ 6. Restore other saved registers from the U stack, including the Direct
\    Pointer
\ 7. Call the next Forth word
\
\ There is a lot of replicated code in the below BIOS API calls, e.g. to save
\ the Direct Pointer before calling the BIOS routine. This could be replaced with
\ a call to a single BIOS routine or replaced with an inline macro. I haven't used
\ a single BIOS routine, because it would slow things down and I think the
\ assembler might need a tweak to do macros, which I haven't really had the need
\ to review. Maybe you could do it?
\
\ At the end of each line are the stack comments, which show what parameters are
\ expected on the S stack when the routine is called and what will be returned
\ e.g. Bitmask_a expects a the number of a bit an the stack and it will return a
\ mask calculated by the number on the stack i.e. \ bit_number -- bit_mask ;
\
\
\ BUGS/TO-DO
\
\ Not really a bug in my Forth code, but if you are using a VecFever as an EPROM
\ emulater, the VecFever "fast boot" switch breaks vector screen text rendering.
\
\ FORTH STACK COMMENTS
\
\ Forth is a stack-based language. For each of the Forth "words" (roughly
\ analageous to a subroutine), parameters are generally passed via the main stack,
\ for entry and exit. However, unlike for example "C", you can return as many
\ stack items as you like and there are no checks made! Thus you need a way of
\ documenting your "word" entry and exit stack parameters. Typically this is
\ done using delimeted comments, either encased with ( ) or \ and CR, see below:
\ ( entry_parameter1 -- exit_parameter1 ) or
\ \ entry_parameter1 -- exit_parameter1 ; or

\ Fix stack comments e.g. addr, ptr, flag etc.

\ Registers - Simple set, doesn't cover all combinations

$06 equ ____D
$08 equ __dp_
$0E equ __dpD
$20 equ _Y___
$26 equ _Y__D
$28 equ _Ydp_
$2E equ _YdpD
$40 equ U____    $40 equ S____ \ If push to S, it's U____. If push to U, it's S_____.
$48 equ U_dp_    $48 equ S_dp_
$4E equ U_dpD    $4E equ S_dpD
$68 equ UYdp_    $68 equ SYdp_
$6E equ UYdpD    $6E equ SYdpD


\ Name and $ADDR in parenthesis for each call are the alternate RUM subroutine name
\ and address in the BIOS code.
\ ------- denotes BIOS subroutine not recognised as a subroutine in RUM.
\ Name and $ADDR shown with no CODE following denotes RUM subroutine not in BIOS.


\ Reset and initialization

( POWER   $F000 ) CODE _Cold_Start        Cold_Start  JMP,   ;C \ -- ; Jump here to restart the Vectrex and re-initialize the OS.
( ------- $F06C ) CODE _Warm_Start        Warm_Start  JMP,   ;C \ -- ; Jump here to restart the Vectrex without re-initializing the OS.
( INITPIA $F14C ) CODE _Init_VIA          __dpD # PSHU,   Init_VIA    JSR,   __dpD # PULU,   NEXT ;C \ -- ;
( INITMSC $F164 ) CODE _Init_OS_RAM       __dpD # PSHU,   Init_OS_RAM JSR,   __dpD # PULU,   NEXT ;C \ -- ;
( INITALL $F18B ) CODE _Init_OS           __dpD # PSHU,   Init_OS     JSR,   __dpD # PULU,   NEXT ;C \ -- ;

\ Calibration/Vector reset

( FRAM20  $F192 ) CODE _Wait_Recal        __dpD # PSHU,                            Wait_Recal   JSR,   __dpD # PULU,   NEXT ;C \ -- ;
( ------- $F1A2 ) CODE _Set_Refresh       __dpD # PSHU,   D0 # LDA,   A DPR TFR,   Set_Refresh  JSR,   __dpD # PULU,   NEXT ;C \ -- ;
\
( DEFLOK  $F2E6 ) CODE _Recalibrate       __dpD # PSHU,   D0 # LDA,   A DPR TFR,   Recalibrate  JSR,   __dpD # PULU,   NEXT ;C \ -- ;
\
( ZERO.DP $F34A ) CODE _Reset0Ref_D0      __dpD # PSHU,                            Reset0Ref_D0 JSR,   __dpD # PULU,   NEXT ;C \ -- ;
( ZEGO    $F34F ) CODE _Check0Ref         __dpD # PSHU,   D0 # LDA,   A DPR TFR,   Check0Ref    JSR,   __dpD # PULU,   NEXT ;C \ -- ;
( ZEROIT  $F354 ) CODE _Reset0Ref         __dpD # PSHU,   D0 # LDA,   A DPR TFR,   Reset0Ref    JSR,   __dpD # PULU,   NEXT ;C \ -- ;
( ZEREF   $F35B ) CODE _Reset_Pen         __dpD # PSHU,   D0 # LDA,   A DPR TFR,   Reset_Pen    JSR,   __dpD # PULU,   NEXT ;C \ -- ;
( ZERO.   $F36B ) CODE _Reset0Int         __dpD # PSHU,   D0 # LDA,   A DPR TFR,   Reset0Int    JSR,   __dpD # PULU,   NEXT ;C \ -- ;

\ Set Direct Pointer

( DPIO    $F1AA ) CODE _DP_to_D0          NEXT ;C \ -- ; Not needed for Forth, DP managed in API calls
( DPRAM   $F1AF ) CODE _DP_to_C8          NEXT ;C \ -- ; Not needed for Forth, DP managed in API calls

\ Joystick handling

( ENPUT   $F1B4 ) CODE _Read_Btns_Mask    __dp_ # PSHU,   D0 # LDA,   A DPR TFR,        B A TFR,   Read_Btns_Mask JSR,   A B TFR,   CLRA,   __dp_ # PULU,   NEXT ;C \ maskA -- b ; Button Transition State (Same as $C811)
( INPUT   $F1BA ) CODE _Read_Btns         __dp_ # PSHU,   D0 # LDA,   A DPR TFR,   ____D # PSHS,   Read_Btns      JSR,   A B TFR,   CLRA,   __dp_ # PULU,   NEXT ;C \       -- b ; Button Transition State (Same as $C811)
( PBANG4  $F1F5 ) CODE _Joy_Analog        __dpD # PSHU,   D0 # LDX,   X DPR TFR,                   Joy_Analog     JSR,                      __dpD # PULU,   NEXT ;C \       -- ;
( PANG    $F1F8 ) CODE _Joy_Digital       __dpD # PSHU,   D0 # LDX,   X DPR TFR,                   Joy_Digital    JSR,                      __dpD # PULU,   NEXT ;C \       -- ;

\ Sound

( PSGX    $F256 ) CODE _Sound_Byte        __dp_ # PSHU,   D0 # LDX,   X DPR TFR,                              A B EXG,   S ,++ ADDD,     Sound_Byte     JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ sound_data_byte reg# -- ;
( PSG     $F259 ) CODE _Sound_Byte_x      __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D # PULS,   A B EXG,   S ,++ ADDD,     Sound_Byte_x   JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ sound_data_byte reg# shadow-addr -- ;
( ------- $F52B ) CODE _Sound_Byte_raw    __dp_ # PSHU,   D0 # LDX,   X DPR TFR,                              A B EXG,   S ,++ ADDD,     Sound_Byte_raw JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ sound_data_byte reg# -- ;
( INITPSG $F272 ) CODE _Clear_Sound       __dpD # PSHU,   D0 # LDA,   A DPR TFR,                                                         Clear_Sound    JSR,                   __dpD # PULU,   NEXT ;C \ -- ;
( PSGLUP  $F27D ) CODE _Sound_Bytes       __dp_ # PSHU,   D0 # LDX,   X DPR TFR,                              U____ # PSHS,   D U TFR,   Sound_Bytes    JSR,   U____ # PULS,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ sound_data_byte_addr             -- ;
( PSGLPU  $F284 ) CODE _Sound_Bytes_x     __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D # PULS,   U____ # PSHS,   D U TFR,   Sound_Bytes_x  JSR,   U____ # PULS,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ sound_data_byte_addr AY_reg_addr -- ;
( REQOUT  $F289 ) CODE _Do_Sound          U_dpD # PSHS,              D0 # LDA,   A DPR TFR,   Do_Sound       JSR,   U_dpD # PULS,                   NEXT ;C \          -- ;
( REQOUT  $F28C ) CODE _Do_Sound_x        U_dp_ # PSHS,   D X TFR,   D0 # LDA,   A DPR TFR,   Do_Sound_x     JSR,   U_dp_ # PULS,   ____D # PULS,   NEXT ;C \ shadow_registers_addr -- ;
\
( IREQ    $F533 ) CODE _Init_Music_Buf    ____D # PSHS,                                       Init_Music_Buf JSR,                   ____D # PULS,   NEXT ;C \          -- ;
\
( REPLAY  $F687 ) CODE _Init_Music_chk    UYdp_ # PSHS,   D U TFR,   C8 # LDA,   A DPR TFR,   Init_Music_chk JSR,   UYdp_ # PULS,   ____D # PULS,   NEXT ;C \ addr -- ;
( SPLAY   $F68D ) CODE _Init_Music        UYdp_ # PSHS,   D U TFR,   C8 # LDA,   A DPR TFR,   Init_Music     JSR,   UYdp_ # PULS,   ____D # PULS,   NEXT ;C \ addr -- ;
( SOPLAY  $F690 )
( YOPLAY  $F692 ) CODE _Init_Music_dft    UYdp_ # PSHS,   D U TFR,   C8 # LDA,   A DPR TFR,   Init_Music_x   JSR,   UYdp_ # PULS,   ____D # PULS,   NEXT ;C \ addr -- ; Note Init_Music_dft/Music_x
( XPLAY   $F742 )
( AXE     $F92E ) CODE _Explosion_Snd     U_dp_ # PSHS,   D U TFR,   C8 # LDA,   A DPR TFR,   Explosion_Snd  JSR,   U_dp_ # PULS,   ____D # PULS,   NEXT ;C \ addr -- ;

\ Vector brightness

( INT1Q   $F29D ) CODE _Intensity_1F      __dpD # PSHU,   D0 # LDA,   A DPR TFR,              Intensity_1F JSR,                   __dpD # PULU,   NEXT ;C \   -- ;
( INTMID  $F2A1 ) CODE _Intensity_3F      __dpD # PSHU,   D0 # LDA,   A DPR TFR,              Intensity_3F JSR,                   __dpD # PULU,   NEXT ;C \   -- ;
( INT3Q   $F2A5 ) CODE _Intensity_5F      __dpD # PSHU,   D0 # LDA,   A DPR TFR,              Intensity_5F JSR,                   __dpD # PULU,   NEXT ;C \   -- ;
( INTMAX  $F2A9 ) CODE _Intensity_7F      __dpD # PSHU,   D0 # LDA,   A DPR TFR,              Intensity_7F JSR,                   __dpD # PULU,   NEXT ;C \   -- ;
( INTENS  $F2AB ) CODE _Intensity_a       __dp_ # PSHU,   D0 # LDA,   A DPR TFR,   A B EXG,   Intensity_a  JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ n -- ; n is intensity between 0 and $7F

\ Drawing / Dot

( DOTTIM  $F2BE ) CODE _Dot_ix_b          __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D #  PULS,   Dot_ix_b JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ intensity coord-pair-addr -- ;
( DOTX    $F2C1 ) CODE _Dot_ix            __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                    Dot_ix   JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \           coord-pair-addr -- ;
( DOTAB   $F2C3 ) CODE _Dot_d             __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   A B EXG,   S ,++ ADDD,      Dot_d    JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \     x y -- ;
( DOT     $F2C5 ) CODE _Dot_here          __dpD # PSHU,   D0 # LDA,   A DPR TFR,                               Dot_here JSR,                   __dpD # PULU,   NEXT ;C \         -- ;
( DIFDOT  $F2D5 ) CODE _Dot_List          __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                    Dot_List JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ dl_addr -- ;
( DOTPAK  $F2DE ) CODE _Dot_List_Reset    __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,              Dot_List_Reset JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ dl_addr -- ;

\ Vector beam positioning

( POSWID  $F2F2 ) CODE _Moveto_x_7F       __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                    Moveto_x_7F  JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \    addr -- ;
( POSITD  $F2FC ) CODE _Moveto_d_7F       __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   A B EXG,   S ,++ ADDD,      Moveto_d_7F  JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ x  y    -- ;
( POSIT2  $F308 ) CODE _Moveto_ix_FF      __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                    Moveto_ix_FF JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \    addr -- ;
( POSIT1  $F30C ) CODE _Moveto_ix_7F      __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                    Moveto_ix_7F JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \    addr -- ;
( POSITB  $F30E ) CODE _Moveto_ix_b       __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D #  PULS,   Moveto_ix_7F JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ sf addr -- ;
( POSITX  $F310 ) CODE _Moveto_ix         __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                    Moveto_ix    JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \    addr -- ;
( POSITN  $F312 ) CODE _Moveto_d          __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   A B EXG,   S ,++ ADDD,      Moveto_d     JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ x  y    -- ;

\ Drawing / String

( SIZPRAS $F373 ) CODE _Print_Str_hwyx    _Ydp_ # PSHU,   D0 # LDX,   X DPR TFR,   U Y TFR,   D U TFR,   Print_Str_hwyx JSR,   ____D # PULS,   Y U TFR,   _Ydp_ # PULU,   NEXT ;C \ c-addr -- ;
( POSNRAS $F378 ) CODE _Print_Str_yx      _Ydp_ # PSHU,   D0 # LDX,   X DPR TFR,   U Y TFR,   D U TFR,   Print_Str_yx   JSR,   ____D # PULS,   Y U TFR,   _Ydp_ # PULU,   NEXT ;C \ c-addr -- ;
( POSDRAS $F37A ) CODE _Print_Str_d       _Ydp_ # PSHU,   D0 # LDX,   X DPR TFR,   U Y TFR,   D U TFR,   ____D # PULS,   A B EXG,   S ,++ ADDD,   Print_Str_d JSR,   ____D # PULS,   Y U TFR,   _Ydp_ # PULU,   NEXT ;C \ x y c-addr -- ; Print single string to screen
( TEXSIZ  $F385 ) CODE _Print_List_hw     _Ydp_ # PSHU,   D0 # LDX,   X DPR TFR,   U Y TFR,   D U TFR,   Print_List_hw  JSR,   ____D # PULS,   Y U TFR,   _Ydp_ # PULU,   NEXT ;C \ c-addr -- ;
( ------- $F38A ) CODE _Print_List        _Ydp_ # PSHU,   D0 # LDX,   X DPR TFR,   U Y TFR,   D U TFR,   Print_List     JSR,   ____D # PULS,   Y U TFR,   _Ydp_ # PULU,   NEXT ;C \ c-addr -- ;
( TEXPOS  $F38C ) CODE _Print_List_chk    _Ydp_ # PSHU,   D0 # LDX,   X DPR TFR,   U Y TFR,   D U TFR,   Print_List_chk JSR,   ____D # PULS,   Y U TFR,   _Ydp_ # PULU,   NEXT ;C \ c-addr -- ;
( RASTUR  $F495 ) CODE _Print_Str         _Ydp_ # PSHU,   D0 # LDX,   X DPR TFR,   U Y TFR,   D U TFR,   Print_Str      JSR,   ____D # PULS,   Y U TFR,   _Ydp_ # PULU,   NEXT ;C \ c-addr -- ;
( RASTER  $F498 )

\ Print Ships

( SHIPSAT $F391 ) CODE _Print_Ships_x     _Ydp_ # PSHU,   D0 # LDX,   X DPR TFR,  U Y TFR,                            D X TFR,   ____D # PULS,   A B EXG,   S ,++ ADDD,   Print_Ships_x JSR,   ____D # PULS,   Y U TFR,   _Ydp_ # PULU,   NEXT ;C \ #ships ship_char addr -- ;
( SHIPSHO $F393 ) CODE _Print_Ships       _Ydp_ # PSHU,   D0 # LDX,   X DPR TFR,  U Y TFR,   A B EXG,   S ,++ ADDD,   D X TFR,   ____D # PULS,   A B EXG,   S ,++ ADDD,   Print_Ships   JSR,   ____D # PULS,   Y U TFR,   _Ydp_ # PULU,   NEXT ;C \ #ships ship_char x y  -- ;

\ Drawing / Vector / Move and Draw

( DUFFAX  $F3AD ) CODE _Mov_Draw_VLc_a    __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                                            Mov_Draw_VLc_a JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
( DUFTIM  $F3B1 ) CODE _Mov_Draw_VL_b     __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D # PULS,                            Mov_Draw_VL_b  JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ sf    addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
( DUFLST  $F3B5 ) CODE _Mov_Draw_VLcs     __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                                            Mov_Draw_VLcs  JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
( ------- $F3B7 ) CODE _Mov_Draw_VL_ab    __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D # PULS,   A B EXG,   S ,++ ADDD,   Mov_Draw_VL_ab JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ sf #v addr -- ; addr = vector list in format: rel y, rel x, rel y, rel x, ...
( ------- $F3B9 ) CODE _Mov_Draw_VL_a     __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D # PULS,   A B EXG,                 Mov_Draw_VL_a  JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \    #v addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
( DUFFY   $F3BC ) CODE _Mov_Draw_VL       __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                                            Mov_Draw_VL    JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
( DUFFAB  $F3BE ) CODE _Mov_Draw_VL_d     __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D # PULS,   A B EXG,   S ,++ ADDD,   Mov_Draw_VL_d  JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \   x y addr -- ; addr = vector list in format: rel y, rel x, rel y, rel x, ...

\ Drawing / Vector / Draw only

( DIFFAX  $F3CE ) CODE _Draw_VLc          __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                                            Draw_VLc       JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
( DIFTIM  $F3D2 ) CODE _Draw_VL_b         __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D # PULS,                            Draw_VL_b      JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ sf    addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
( DIFLST  $F3D6 ) CODE _Draw_VLcs         __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                                            Draw_VLcs      JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, scale, rel y, rel x, rel y, rel x, ...
( DIFFX   $F3D8 ) CODE _Draw_VL_ab        __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D # PULS,   A B EXG,   S ,++ ADDD,   Draw_VL_ab     JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ sf #v addr -- ; addr = vector list in format: rel y, rel x, rel y, rel x, ...
( DIFTLS  $F3DA ) CODE _Draw_VL_a         __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D # PULS,   A B EXG,                 Draw_VL_a      JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \    #v addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
( DIFFY   $F3DD ) CODE _Draw_VL           __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                                            Draw_VL        JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
( DIFFAB  $F3DF ) CODE _Draw_Line_d       __dp_ # PSHU,   D0 # LDX,   X DPR TFR,                              A B EXG,   S ,++ ADDD,   Draw_Line_d    JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \   x y      -- ;
( PAC2X   $F404 ) CODE _Draw_VLp_FF       __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                                            Draw_VLp_FF    JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
( PAC1X   $F408 ) CODE _Draw_VLp_7F       __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                                            Draw_VLp_7F    JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
( PACXX   $F40C ) CODE _Draw_VLp_scale    __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                                            Draw_VLp_scale JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
( PACB    $F40E ) CODE _Draw_VLp_b        __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D # PULS,                            Draw_VLp_b     JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \ sf    addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
( PACKET  $F410 ) CODE _Draw_VLp          __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                                            Draw_VLp       JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
( DASHE   $F433 )
( DASHEL  $F434 ) CODE _Draw_Pat_VL_a     __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D # PULS,   A B EXG,                 Draw_Pat_VL_a  JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \    #v addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
( DASHY   $F437 ) CODE _Draw_Pat_VL       __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                                            Draw_Pat_VL    JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
( ------- $F439 ) CODE _Draw_Pat_VL_d     __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D # PULS,   A B EXG,   S ,++ ADDD,   Draw_Pat_VL_d  JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \   x y addr -- ; addr = vector list in format: rel y, rel x, rel y, rel x, ...
( DASHY3  $F46E ) CODE _Draw_VL_mode      __dp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,                                            Draw_VL_mode   JSR,   ____D # PULS,   __dp_ # PULU,   NEXT ;C \       addr -- ; addr = vector list in format: count, rel y, rel x, rel y, rel x, ...  current scaling factor is used
( NIBBY   $FF9F ) CODE _Draw_Grid_VL      _Ydp_ # PSHU,   D0 # LDX,   X DPR TFR,   D X TFR,   ____D # PULS,   D Y TFR,                 Draw_Grid_VL   JSR,   ____D # PULS,   _Ydp_ # PULU,   NEXT ;C \ addrY addrX -- ;

\ Random number generator

( RAND3   $F511 ) CODE _Random_3          ____D # PSHS,   Random_3 JSR,   CLRB,   A B EXG,   NEXT ;C \ -- n ; n is a random number between 0 and 255
( RANDOM  $F517 ) CODE _Random            ____D # PSHS,   Random   JSR,   CLRB,   A B EXG,   NEXT ;C \ -- n ; n is a random number between 0 and 255

\ Memory Management / Memory clear - Not really needed for Forth, you can use the native Forth FILL word instead

( CLRSON  $F53F ) CODE _Clear_x_b         D X TFR,   ____D # PULS,   Clear_x_b    JSR,   ____D # PULS,   NEXT ;C \ #bytes-1 addr -- ; #bytes stored in bottom 8 bits only
( CLRMEM  $F542 ) CODE _Clear_C8_RAM      ____D # PSHU,              Clear_C8_RAM JSR,   ____D # PULS,   NEXT ;C \               -- ;
( CLR256  $F545 ) CODE _Clear_x_256       D X TFR,                   Clear_x_256  JSR,   ____D # PULS,   NEXT ;C \          addr -- ; addr = start of RAM to be cleared
( GILL    $F548 ) CODE _Clear_x_d         D X TFR,   ____D # PULS,   Clear_x_d    JSR,   ____D # PULS,   NEXT ;C \ #bytes-1 addr -- ; #bytes stored in 16 bits only

\ Memory Management / Memory fill - Not really needed for Forth, you can use the native Forth FILL word instead

( NEGSOM  $F550 ) CODE _Clear_x_b_80      D X TFR,   ____D # PULS,   Clear_x_b_80 JSR,   ____D # PULS,   NEXT ;C \ #bytes-1 addr -- ; #bytes = $0-$FF, $00=$100. Fills with $80
( FILL    $F552 ) CODE _Clear_x_b_a       D X TFR,   ____D # PULS,   Clear_x_b_a  JSR,   ____D # PULS,   NEXT ;C \ #bytes-1 addr -- ; #bytes = $0-$FF, $00=$100. Fills with byte=in #bytes top 8 bits

\ Counters

( DEKR3   $F55A ) CODE _Dec_3_Counters               ____D # PSHU,   Dec_3_Counters JSR,   ____D # PULU,   NEXT ;C \ -- ;
( DEKR    $F55E ) CODE _Dec_6_Counters               ____D # PSHU,   Dec_6_Counters JSR,   ____D # PULU,   NEXT ;C \ -- ;
( ------- $F563 ) CODE _Dec_Counters      D X TFR,   ____D # PULS,   Dec_Counters   JSR,   ____D # PULS,   NEXT ;C \ #counters-1 ptr_counter_bytes -- ;

\ Delay

( DEL38   $F56D ) CODE _Delay_3           ____D # PSHU,   Delay_3   JSR,   ____D # PULU,   NEXT ;C \   -- ;
( DEL33   $F571 ) CODE _Delay_2           ____D # PSHU,   Delay_2   JSR,   ____D # PULU,   NEXT ;C \   -- ;
( DEL28   $F575 ) CODE _Delay_1           ____D # PSHU,   Delay_1   JSR,   ____D # PULU,   NEXT ;C \   -- ;
( DEL20   $F579 ) CODE _Delay_0           ____D # PSHU,   Delay_0   JSR,   ____D # PULU,   NEXT ;C \   -- ;
( DEL     $F57A ) CODE _Delay_b                           Delay_b   JSR,   ____D # PULS,   NEXT ;C \ n -- ; n is xxnn, where xx is undefined and nn is length to delay. n' is xxFF, where xx is undefined.
( DEL13   $F57D ) CODE _Delay_RTS                         Delay_RTS JSR,                   NEXT ;C \   -- ;

\ Day to Day / Bitmask

( BITE    $F57E ) CODE _Bitmask_a         A B EXG,   Bitmask_a JSR,   A B EXG,   NEXT ;C \ bit_number -- bit_mask ;

\ Mathematical

( ABSVAL  $F584 ) CODE _Abs_a_b           Abs_a_b JSR,   NEXT ;C \ n -- n' ; Returns abs value of two 8 bit numbers
( AOK     $F58B ) CODE _Abs_b             Abs_b   JSR,   NEXT ;C \ n -- n' ; Returns abs value of one 8 bit number (bottom 8 bits)

\ Vector object handling / Rotating

\ COMPAS  $F593   CMPASS  Return angle for given delta 'Y:X'
\ COSGET  $F5D9   COSINE  Calculate the cosine of 'A'
\ SINGET  $F5DB   SINE    Calculate the sine of 'A'
\ SINCOS  $F5EF   ---     Calculate the sine and cosine of 'ANGLE'
\
\ RSINA   $F65B   MSINE   Multiply 'A' by previous sine value
\ RSIN    $F65D   LSINE   Multiply 'LEG' by previous sine value
\ RCOSA   $F661   MCSINE  Multiply 'A' by previous cosine value
\ RCOS    $F663   LCSINE  Multiply 'LEG' by previous cosine value

( COMPAS  $F593 ) CODE _Rise_Run_Angle    __dp_ # PSHU,   C8 # LDX,   X DPR TFR,   A B EXG,   S ,++ ADDD,   Rise_Run_Angle JSR,   __dp_ # PULU,   NEXT ;C \ run  rise -- angle ; angle is from X axis
( COSGET  $F5D9 ) CODE _Get_Rise_Idx                                               A B EXG,                 Get_Rise_Idx   JSR,                   NEXT ;C \     angle -- value ; value  A=VALUE,B=SIGN/OVERFLOW
( SINGET  $F5DB ) CODE _Get_Run_Idx                                                A B EXG,                 Get_Run_Idx    JSR,                   NEXT ;C \     angle -- value ; value  A=VALUE,B=SIGN/OVERFLOW
( SINCOS  $F5EF ) CODE _Rise_Run_Idx      __dpD # PSHU,   C8 # LDX,   X DPR TFR,                            Get_Rise_Run   JSR,   __dpD # PULU,   NEXT ;C \           --       ; Entry:Vec_Angle, Exit:Vec_Run_Index/Vec_Rise_Index(2 bytes each) Note: Rise_Run_Idx/Get_Rise_Run

( RATOT   $F5FF ) CODE _Rise_Run_X        __dp_ # PSHU,   C8 # LDX,   X DPR TFR,   A B EXG,   S ,++ ADDD,   Rise_Run_X     JSR,   __dp_ # PULU,   NEXT ;C \ angle svv -- rise/run ; scaler_velocity_value
( ROTOR   $F601 ) CODE _Rise_Run_Y        __dp_ # PSHU,   C8 # LDX,   X DPR TFR,   A B EXG,   S ,++ ADDD,   Rise_Run_Y     JSR,   __dp_ # PULU,   NEXT ;C \ angle svv -- rise/run ; scaler_velocity_value
( ROTAR   $F603 ) CODE _Rise_Run_Len      __dp_ # PSHU,   C8 # LDX,   X DPR TFR,                            Rise_Run_Len   JSR,   __dp_ # PULU,   NEXT ;C \ angle     -- rise/run ;

( DANROT  $F610 ) CODE _Rot_VL_ab         _Ydp_ # PSHU,   U Y TFR,   D U TFR,   ____D # PULS,   D X TFR,   ____D # PULS,   A B EXG,   S ,++ ADDD,   Rot_VL_ab    JSR,   Y U TFR,   ____D # PULS,   _Ydp_ # PULU,   NEXT ;C \ #vectors angle vl_addr_before vl_addr_after -- ;
( DISROT  $F613 )
( DIFROT  $F616 ) CODE _Rot_VL            _Ydp_ # PSHU,   U Y TFR,   D U TFR,   ____D # PULS,   D X TFR,                                            Rot_VL       JSR,   Y U TFR,   ____D # PULS,   _Ydp_ # PULU,   NEXT ;C \                vl_addr_before vl_addr_after -- ;
( POTATA  $F61F ) CODE _Rot_VL_Mode       _Ydp_ # PSHU,   U Y TFR,   D U TFR,   ____D # PULS,   D X TFR,   ____D # PULS,   A B EXG,                 Rot_VL_Mode  JSR,   Y U TFR,   ____D # PULS,   _Ydp_ # PULU,   NEXT ;C \          angle vl_addr_before vl_addr_after -- ;
( POTATE  $F622 )
( ------- $F62B ) CODE _Rot_VL_M_dft      _Ydp_ # PSHU,   U Y TFR,   D U TFR,   ____D # PULS,   D X TFR,                                            Rot_VL_M_dft JSR,   Y U TFR,   ____D # PULS,   _Ydp_ # PULU,   NEXT ;C \                vl_addr_before vl_addr_after -- ;

( RSINA   $F65B ) CODE _Xform_Run_a       __dp_ # PSHU,   C8 # LDX,   X DPR TFR,                   Xform_Run_a  JSR,   CLRB,   A B EXG,   __dp_ # PULU,   NEXT ;C \ length --  run_value ; Length for run
( RSIN    $F65D ) CODE _Xform_Run         __dp_ # PSHU,   C8 # LDX,   X DPR TFR,   ____D # PSHS,   Xform_Run    JSR,   CLRB,   A B EXG,   __dp_ # PULU,   NEXT ;C \        --  run_value ; Length for run already in $C83B Vec_RiseRun_Len
( RCOSA   $F661 ) CODE _Xform_Rise_a      __dp_ # PSHU,   C8 # LDX,   X DPR TFR,                   Xform_Rise_a JSR,   CLRB,   A B EXG,   __dp_ # PULU,   NEXT ;C \ length -- rise_value ; Length for rise
( RCOS    $F663 ) CODE _Xform_Rise        __dp_ # PSHU,   C8 # LDX,   X DPR TFR,   ____D # PSHS,   Xform_Rise   JSR,   CLRB,   A B EXG,   __dp_ # PULU,   NEXT ;C \        -- rise_value ; Length for rise already in $C83B Vec_RiseRun_Len

\ Memory management / Memory copy - Not really needed for Forth, you can use the native Forth MOVE/CMOVE/CMOVE> words instead

( BAGAUX  $F67F ) CODE _Move_Mem_a_1      _Y___ # PSHU,   U Y TFR,   D X TFR,   ____D # PULS,   D U TFR,   ____D # PULS,   A B EXG,   Move_Mem_a_1 JSR,   Y U TFR,   ____D # PULS,   _Y___ # PULU,   NEXT ;C \ byte_count source destination -- ; Not need for Forth, use CMOVE or CMOVE> instead
( STFAUX  $F683 ) CODE _Move_Mem_a        _Y___ # PSHU,   U Y TFR,   D X TFR,   ____D # PULS,   D U TFR,   ____D # PULS,   A B EXG,   Move_Mem_a   JSR,   Y U TFR,   ____D # PULS,   _Y___ # PULU,   NEXT ;C \ byte_count source destination -- ; Not need for Forth, use CMOVE or CMOVE> instead

\ Player option

( OPTION  $F7A9 ) CODE _Select_Game       _Ydp_ # PSHU,   A B EXG,   S ,++ ADDD,   U____ # PSHS,   Select_Game    JSR,   U____ # PULS,   ____D # PULS,   _Ydp_ # PULU,   NEXT ;C \ #game_versions #players_max -- ;
( ------- $F835 ) CODE _Display_Option    _Ydp_ # PSHU,   D0 # LDX,   X DPR TFR,   D Y TFR,   ____D # PULS,   B A EXG,   U____ # PSHS,   Display_Option JSR,   U____ # PULS,   ____D # PULS,   _Ydp_ # PULU, NEXT ;C \ option_val addr -- ;

\ Score

( SCLR    $F84F ) CODE _Clear_Score       D X TFR,                                              Clear_Score    JSR,                   ____D # PULS,      NEXT ;C \    addr -- ;
( SHADD   $F85E ) CODE _Add_Score_a       D X TFR,   ____D # PULS,   B A EXG,   U____ # PSHS,   Add_Score_a    JSR,   U____ # PULS,   ____D # PULS,      NEXT ;C \ decimal# ptr -- ; decimal# 8 bit only
( SADD    $F87C ) CODE _Add_Score_d       D X TFR,   ____D # PULS,              U____ # PSHS,   Add_Score_d    JSR,   U____ # PULS,   ____D # PULS,      NEXT ;C \      BCD ptr -- ;
( SADD2   $F880 )
( ------- $F8B7 ) CODE _Strip_Zeros       D X TFR,   ____D # PULS,                              Strip_Zeros    JSR,                   ____D # PULS,      NEXT ;C \   digit# ptr -- ; digit# 8 bit only
( WINNER  $F8C7 ) CODE _Compare_Score     D X TFR,   ____D # PULS,   U____ # PSHS,   D U TFR,   Compare_Score  JSR,   U____ # PULS,   A B TFR,   CLRA,   NEXT ;C \    ptr2 ptr1 -- 0|1|2; Same|1>2|2>1
( HIGHSCR $F8D8 ) CODE _New_High_Score    D X TFR,   ____D # PULS,   U____ # PSHS,   D U TFR,   New_High_Score JSR,   U____ # PULS,   ____D # PULS,      NEXT ;C \  addr2 addr1 -- ; addr1=NewScore addr2=HighScore

\ Vector object handling / Object collision detection

( OFF1BOX $F8E5 ) CODE _Obj_Will_Hit_u    _Y___ # PSHU,   D X TFR,   ____D # PULS,   D Y TFR,   ____D # PULS,   D U EXG,   ____D # PSHS,   S 2 , LDD,   Obj_Will_Hit_u JSR,   ____D # PULS,   D U TFR,   ____D # PULS,   0 # LDD,   0 # ADCB,   _Y___ # PULU,   NEXT ;C \ box_yx movement_yx_addr pos_obj_yx pos_missile_yx -- f ; f = hit
( OFF2BOX $F8F3 ) CODE _Obj_Will_Hit      _Y___ # PSHU,   D X TFR,   ____D # PULS,   D Y TFR,   ____D # PULS,   D U EXG,   ____D # PSHS,   S 2 , LDD,   Obj_Will_Hit   JSR,   ____D # PULS,   D U TFR,   ____D # PULS,   0 # LDD,   0 # ADCB,   _Y___ # PULU,   NEXT ;C \ box_yx movement_yx      pos_obj_yx pos_missile_yx -- f ; f = hit
( FINDBOX $F8FF ) CODE _Obj_Hit           _Y___ # PSHU,   D X TFR,   ____D # PULS,   D Y TFR,   ____D # PULS,                                           Obj_Hit        JSR,                                              0 # LDD,   0 # ADCB,   _Y___ # PULU,   NEXT ;C \ box_yx                  pos_obj_yx pos_missile_yx -- f ; f = hit
( LOUDIN  $F9CA )

\ End of File