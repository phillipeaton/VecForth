; VERSION 4/17/82  5:00 pm  SMN
;
;
;
; CRYPITIC THOUGHT "ANY PROGRAMMING IS A CHANGE" PAUL NEWELL
;    (crypitic update: "Second rule of management--They don't believe the
;        first rule" JS/ES)
;    another cryptic thought : "The spanish inqistion is in the hands of
;        its enemys!!!"  EAP
;
;    "AND TELL A FRIEND"  GK
;
;    "They shoot horses, don't they?"
;
;    "A 'K' a day keeps the Smith's away." MI
;
;    "When in dought run about scream and shout!!" LC
;
;    "Nuts!" Brigider General McAlphy
;
;
;
;
;  *******************************************************************
;
;  THIS RUM IS FOR USE WITH THE FINAL COPY OF THE PROGRAM THIS IS THE ONE
;  THAT IS ON THE FINAL CHIP AND SHOULD BE USED THUSLY.
;
;  *******************************************************************
;
;
;
;* * * HERE WE GO ON INTERSYSTEMS! * * *
;
;* * *  RUN-TIME ROM for HOME-ARCADE
;
;         AUTHOR UNKNOWN, TRADITIONAL
;
         ORG       $C800           ;RAM
REG0     DS        1               ;MIRROR SOUND CHIP REGS FINE TUNE -A- 8 BIT
REG1     DS        1               ;COARSE TUNE -A- 4 BIT
REG2     DS        1               ;FINE TUNE -B- 8 BIT
REG3     DS        1               ;COARSE TUNE -B- 4 BIT
REG4     DS        1               ;FINE TUNE -C-
REG5     DS        1               ;COARSE TUNE -C-
REG6     DS        1               ;NOISE PER, 5 BIT
REG7     DS        1               ;ENABLE BAR- IN BAR/OUT: IOB,IOA
;                                  ;NOISE BAR: C,B,A TONE BAR: C,B,A
REG8     DS        1               ;AMPL -A-, 4 BIT , 5TH BIT OVERRIDE IF=1
REG9     DS        1               ;AMPLITUDE -B-  " "
REGA     DS        1               ;AMPL -C-
REGB     DS        1               ;FINE TUNE ENVELOPE PER, 8 BITS
REGC     DS        1               ;COARSE TUNE ENVELOPE PER, 8 BIT
REGD     DS        1               ;ENVELOPE TYPE, 4 BIT- CONT,ATT,ALT,HOLD
REGE     DS        1               ;PORT A I/O
;
;
TRIGGR   DS        2               ;BUTTONS INPUT TO PSG- NEW, LAST
HEDGES   DS        1               ;POS EDGE, 1 BIT PER BUTTON
KEY0     DS        1               ;POS EDGE, 1 BYTE PER BUTTON, POS LOG
KEY1     DS        1
KEY2     DS        1
KEY3     DS        1
KEY4     DS        1
KEY5     DS        1
KEY6     DS        1
KEY7     DS        1
;
POTRES   DS        1               ;ACCURACY REQ'D FOR POT READ, LESS IS FASTER
                                   ;$40=1 BIT,$20=2 BITS,$01=7 BITS,$00=8 BITS
POT0     DS        1               ;VALUE READ FROM POTS
POT1     DS        1
POT2     DS        1
POT3     DS        1
DPOT0    DS        1               ;DELTA FROM LAST TEST
DPOT1    DS        1
DPOT2    DS        1
DPOT3    DS        1
;
LIST     DS        1               ;# OF VECTORS IN PRESENT CHART
ZGO      DS        1               ;IF NONZERO, CAUSES 'ZEROIT' CALL AFTER VECTOR ROUTINES
FRAME    DS        2               ;INCRS EVERY TIME FRAM20 OR FRAM30 CALLED
TENSTY   DS        1               ;WAST INTENSITY WEVEL
;
DWELL    DS        1               ;DOT DWELL TIME, SET IN INITMSC OR AFTER
DASH     DS        1               ;PATTERN FOR DASHED VECTORS
;
;
SIZRAS   DS        2               ;FOR RASTER - CONTROLS SLEW RATE
                                   ;SIZE,SIZE+1=Y RATE,X RATE
MESAGE   DS        2               ;START OF STRING
;
;
X0       DS        1
X1       DS        1               ;INTERVAL TIMERS
X2       DS        1               ;COUNT DOWN TO 0 PER FRAME IF 'DEKR' CALLED
X3       DS        1
X4       DS        1
X5       DS        1
;
ABSY     DS        1               ;FOR COMPAS ROUTINE
ABSX     DS        1               ;"
ANGLE    DS        1               ;"
SINE     DS        2               ;VALUE,OVERFLOW
COSINE   DS        2
LEG      DS        1
LAG      DS        1
;
XMSEC    DS        2               ;REFRESH VALUE, INITMSC SETS TO MSEC20
;
MSEC20   EQU       $3075           ;LO BYTE, HI
MSEC25   EQU       $7C92
MSEC30   EQU       $C8AF
;
; -   - - REGS FOR TUNE PLAYING AND SOUND EFFECTS - - - - -
;
REQ0     DS        1               ;ENVELOPE TYPE - FOR REQOUT ROUTINE
REQ1     DS        1               ;TUNE ENVELOPE- MSB (INVERTED ORDER OF REG0-D)
REQ2     DS        1               ; "    "        LSB
REQ3     DS        1               ;LEVEL- C
REQ4     DS        1               ;LEVEL- B
REQ5     DS        1               ;LEVEL- A
REQ6     DS        1               ;ENABLES- TONES / NOISE / I-O
REQ7     DS        1               ;TUNE NOISE - 5 BIT
REQ8     DS        1               ;TUNE C- MSB
REQ9     DS        1               ; "   "  LSB
REQA     DS        1               ;TUNE B- MSB
REQB     DS        1               ; "   "  LSB
REQC     DS        1               ;TUNE A- MSB
REQD     DS        1               ; "   "  LSB
;
CHCF     EQU       REQ8            ;CHANNEL C FREQUENCY
CHBF     EQU       REQA
CHAF     EQU       REQC
NOISEF   EQU       REQ7            ;NOISE FREQ
ENVELF   EQU       REQ1            ;ENVELOPE FREQ
;
CHCV     EQU       REQ3            ;CHANNEL C VOLUME
CHBV     EQU       REQ4
CHAV     EQU       REQ5
;
SENABL   EQU       REQ6            ;SOUND ENABLE
;
DOREMI   DS        2               ;BASE TABLE FOR NOTES USUALLY=NOTES
FADE     DS        2               ;FOR TUNE PLAYER USE
VIBE     DS        2
TUNE     DS        2
NEWGEN   DS        1
TSTAT    DS        1
RESTC    DS        1
RATEA    DS        1
VIBA     DS        1
RATEB    DS        1               ;VIBRATO
VIBB     DS        1
RATEC    DS        1
VIBC     DS        1
FADEA    DS        1
FADEB    DS        1
FADEC    DS        1
TONEA    DS        2               ;FREQ BEFORE VIBE
TONEB    DS        2
TONEC    DS        2
;
; - - - - - REGS FOR SOUND EFFECTS - - - - -
;
SATUS    DS        1               ;GAME-SOUNDS INTERFACE
LATUS    DS        1               ;LAST 'SATUS' VALUE
XATUS    DS        1               ; ", EXPLOSION TYPE
GAP      DS        1               ;TIMES HOLD IN BACKGROUND SOUND
B1FREQ   DS        2               ;
B2FREQ   DS        2
F1FREQ   DS        2
FEAST    DS        1               ;FIRING STATE COUNTER
PEDGE    DS        1               ;POSITIVE EDGE OF 'SATUS'
NEDGE    DS        1               ;NEGATIVE EDGE OF 'SATUS'
K1FREQ   DS        2               ;BACKGROUND FREQ 1
BACON    DS        1               ;" SEQUENCER
XACON    DS        1               ;EXPLO SEQ
SPEKT    DS        1               ;EXPL SPECTR
;
PLAYRZ   DS        1               ;# PLAYERS IN GAME
GAMZ     DS        1               ;GAME # OPTION
;
SEED     DS        2               ;POINTER TO 3 BYTE RANDOM #, USUALLY IS RANCID
RANCID   DS        3
;
;
LASRAM   EQU       $
;
;
RAMMES   EQU       $CA00
;
         ORG       $CBEA
;
STACK    DS        1
HEIGH    DS        7               ;TITLE PAGE HIGH SCORE
INMI
ISWI3
ISWI2    DS        3
IFIRQ    DS        3
IIRQ     DS        3
ISWI     DS        3
JRES     DS        2
;
         ORG       $D000  ;I/O
;
BPORT    DS        1
APORT    DS        1
BPORTD   DS        1
APORTD   DS        1
T1LOLC   DS        1
T1HOC    DS        1
T1LOL    DS        1
T1HOL    DS        1
T2LOLC   DS        1
T2HOC    DS        1
SR       DS        1
ACR      DS        1
PPCR     DS        1
IFR      DS        1
IER      DS        1
ORAIRA   DS        1
;
;
DAC      EQU       APORT           ;FOR HUMAN INTERFACE
PORT     EQU       BPORT           ;MAIN CONTROL PORT
ZERO     EQU       $CC             ;CODE TO PCR ZERO INTEGRATORS
UNZERO   EQU       $CE             ;CODE FOR RELEASE ZERO HOLD
SCAL1X   EQU       $7F             ;TIMING PARMAMETERS FOR VECTOR WRITE
SCAL2X   EQU       $FF
CARTS    EQU       $0              ;ADDRESS OF EXTERNAL ROM PACK
ROMS     EQU       $F000           ;ADDRESS OF ON-BOARD ROM
INHOUS   EQU       $E000
;
; - - - - - - RESET VECTORS - - - - - - -
;
         ORG       $FFF2
         DW        ISWI3           ;RAM INDIRECT VECTORS
         DW        ISWI2           ;NMI AND SWI HANDLED BY -MAID-
         DW        IFIRQ
         DW        IIRQ
         DW        ISWI            ;NOT IN MAID
         DW        ISWI            ;NOT IN MAID
         DW        POWER
;
; - -- - - - PROGRAM MATERIAL - - - - - -
;
         ORG       ROMS
WARM
POWER    LDS       #STACK          ;AT RESET
         JSR       INITALL         ;BRING UP HWARE, BRANCH IF WARM <----
         LDD       #$7321          ;CHECK WARM OR CLOD BOOT
         CMPD       JRES
         BEQ       WARME
;
         STD       JRES           ;START COLD BOOT. CONSISTS OF:
         INC       LEG            ; 1) PLAY STARTUP TUNE 'VCTRX'
                                  ; DISPLAY START MESSAGE
                                  ; 2) TIMEOUT 13 SEC INTO WARM BOOT
         LDX       #HEIGH
         JSR       SCLR           ;CLR HI SKOR IF COLD
;
CLOD     JSR       DPRAM          ;SOUND
         SETDP     $C8
         LDD       FRAME
         CMPD      #$101          ;WAIT FOR START OF TUNE
         BNE       MFIZZ
         STB       TSTAT          ;(B MUST BE POS)
MFIZZ    ASRB 
         ANDB      #3
         LDX       #DABLE
         LDB       B,X
         STB       DASH
         LDB       #2
         STB       ZGO            ;PREPARE DASH LOOP
         LDU       #VCTRX         ;WILL START IF TSTAT=POS NZRO
         JSR       REPLAY
         SETDP     $D0
;
DONO     JSR       FRAM20
         JSR       REQOUT
         JSR       INTMAX
         LDA       FRAME+1        ;BLINK TITLE
         LDU       #MCOLD
         BITA      #$20
         BEQ       ONBABY
         LEAU      12,U
ONBABY   JSR       TEXSIZ
         LDX       #DOSH          ;DISPLAY TITLE BLOCK
RETIRE   JSR       POSIT2
         LDA       #3
         JSR       DASHEL
         DEC       ZGO
         BNE       RETIRE
         LDA       FRAME
         CMPA      #1             ;FOR 10 SEC TIMEOUT
         BLS       CLOD
;
WARME    SETDP     $C8
         JSR       DPRAM
         LDA       #$CC
         STA       DASH
         LDD       #SECRET
         STD       COSINE         ;FOR LEGAL
         CLR       FRAME
         CLR       FRAME+1
         LDU       #CARTS
         LDX       #SECRET
         LDB       #$0B           ;IGNORE DATE, WHERE B=5,4,3,2
ESCAPE   LDA       0,U+           ;TEST FOR CART PRESENT
         CMPA      0,X+
         BEQ       WARMER
         CMPB      #1
         BEQ       REKKED
         CMPB      #5
         BLS       WARMER
REKKED   LDU       #INHOUS
         BRA       WARMEST
;
;
WARMER   DECB 
         BNE       ESCAPE
         STB       COSINE         ;INDICATE CART SELECTED
         STB       COSINE+1
WARMEST  INC       TSTAT          ;PICK UP TUNE START
         STU       SINE
         LDU       0,U
WARMLY   JSR       DPRAM          ;GAME TITLE LOOP
         LDD       #$F848         ;SET LEAGL SIZ
         STD       SIZRAS
         JSR       REPLAY
         JSR       FRAM20
         JSR       REQOUT
         JSR       INTMAX
         LDD       #$C0C0
         LDU       >COSINE
         JSR       POSDRAS        ;DISPLAY LEGAL MESS
         LDA       >LEG
         BNE       NEIGH          ;NOT ON COLD BOOT
         DECA 
         LDU       #HEIGH
         STA       6,U            ;BE SURE DELIM IN PLACE
         LDD       #$68D0         ;START HIGH SCORE DISPLAY
         JSR       POSDRAS
NEIGH    LDU       >SINE          ;GAME TEXT
         LEAU      2,U
         JSR       TEXSIZ
         LDA       >TSTAT         ;WAIT FOR TUNE END
         BNE       WARMLY
         LDX       >FRAME         ;WAIT AT LEAST 2.5 SEC
         CMPX      #125
         BLS       WARMLY
         JMP       1,U            ;OFF TO CART OR SCHWEEPEH
;
DOSH     DW        $40D6          ;POSITION OF BLOCK
         DW        $0056,$8100,$00A9,$7E00   ;(DASHED LINES)
;
         DW        $39DC          ;SMALLER DASHES, OPPOSITEE DIR
         DW        $8E00,$004A,$7200,$00B6
;
DABLE    DB        $E0,$38,$0E,$03   ;FOR MOVIE PATTERN
;
;
SECRET   DB        $67,' GCE 1982',$80
;
MCOLD    DW        $F160,$27CF    ;DUBL INTENSITY VECTREX
         DB       'VECTREX',$80
MBOLD    DW        $F360,$26CF
         DB       'VECTREX',$80
;
SACRET   DW        $FC60
         DW        $DFE9      ;SECND
         DB       'GCE',$80
;
         DW        $FC38
         DW        $CCD1
         DB        'ENTERTAINING',$80
;
         DW        $F338
         DW        $BCDC
         DB        'NEW IDEAS',$80
         DB        0              ;END OF TEXT PACKET
;
INITPIA  SETDP     $D0            ;DO ONCE <----
         BSR       DPIO
         LDD       #$9FFF         ;INIT PIA
         STD       BPORTD
         LDD       #$0100
         STD       BPORT
         LDD       #$9800+SCAL1X  ;SR=SHIFT OUT FAST,T1=ONE SHOT,T2=TIMED INTERRUPT
         STA       ACR            ;SETUP TIMERS
         STB       T1LOLC         ;VEC TIM
         JSR       ZEROIT
         BRA       STAR20         ;RETURNS FROM THERE
;
;
         SETDP     $C8
;
INITMSC  BSR       DPRAM          ;OTHER STUFF TO INIT IF YOU WANT <----
         LDB       # GAMZ-REG0
         LDX       #REG0
         JSR       CLRSOM
         LDD       #RANCID
         STD       SEED
LAHK     INC       RANCID         ;UNLOCK RANDOM
         BEQ       LAHK
         LDA       #$05
         STA       DWELL
         LDD       #MSEC20
         STD       XMSEC          ;SETUP FOR 20 MSEC FRAME RATE
         LDD       #$0103
         STD       DPOT0
         LDD       #$0507         ;SETUP FOR PBANG
         STD       DPOT2
         RTS
;
;
;
         SETDP     $D0
;
INITALL  BSR       INITMSC        ;FOR LAZY INIT <----
         BSR       INITPIA
         JMP       INITPSG
;
FRAM20   LDX       FRAME          ;INCR FRAME (16 BIT), WAIT FOR 'XMSEC' FRAME EDGE <----
         LEAX       1,X
         STX       FRAME
         BSR       DPIO
         LDA       #$20
OCCULT   BITA      IFR
         BEQ       OCCULT
STAR20   LDD       XMSEC          ;LOW BYTE,HI- INIT TIMER <----
         STD       T2LOLC         ;RESET IFR BIT 5,START FRAME TIMER
         JMP       DEFLOK         ;KEEP POWER UP, RETURN FROM THERE
;
;
DPIO     LDA       #$D0           ;SET DP REG TO $D0 <----
         TFR       A,DP
         RTS
;
;
;
DPRAM    LDA       #$C8           ;SET DP REG TO $C8
         TFR       A,DP
         RTS
;
;
;
ENPUT    ANDA      TRIGGR         ;ENTRY FOR 'INPUTT' WITH UN-BOUNCE MASK <----
         STA       TRIGGR
;
INPUT    LDX       #KEY0          ;READ KEYS PROM PSG <----
         LDA       -3,X           ;SAVE IN 'TRIGGR' NEG LOG
         STA       -2,X           ;SAVE LAST READING IN 'TRIGGR'+1
         LDA       #$0E           ;EDGE DETECT, POS LOG IN 'HEDGES'
         STA       APORT          ;FOR EASY TEST OF KEY EDGE, TST KEY0 THRU KEY7
         LDD       #$1901         ;NON ZERO IS EDGE VALID
         STA       BPORT
         NOP 
         STB       BPORT          ;SETUP ADDR
         CLR       APORTD         ;INPUT TO PIA
         LDD       #$0901
         STA       BPORT          ;READ FROM PSG
         NOP 
         LDA       APORT
         COMA                     ;MAKE POS LOG
         STA       -3,X           ;PUT IN MEMRY
         STB       BPORT          ;COMPLETE READ CYCLE
         LDB       #$FF
         STB       APORTD         ;BACK TO OUTPUT
         COMA                     ;0= KEY PRESSED AFTER COMA
         ORA       -2,X           ;BEGIN POS EDGE DEBOUNCE
         COMA 
         STA       -1,X           ;DEEDGED VALUE
         PSHS      A
         LDB       #1             ;LOAD 8 BYTES FOR EASY LOGIC
IENPUT   TFR       B,A
         ANDA      0,S
         STA       0,X+
         ASLB 
         BNE       IENPUT
         PULS      A,PC           ;FINISH KEY INPUTz
;
;
;
POTS4    DEC       LIST           ;READ UP TO 4 POTS, SAR STYLE <----
;
PBANG4   LDX       #DPOT0         ;QUICK READ UP TO 4 POTS <----
PANG     LDA       0,X+           ;ENABLE EACH READ WITH NONZERO VALUE IN DPOT0-3
         BNE       PBONG
PBRONG   CMPX      #DPOT3+1
         BNE       PANG
         CLR       0,X            ;'LIST' WAS USED FOR BANG/SAR FLAG
         LDA       #1
         STA       PORT           ;FOR CLEAN S/H LATER
         RTS 
;
;
;
PBONG    STA       PORT           ;MUX CODE FROM DPOT0-3
         CLR       DAC
         DEC       PORT
         LDB       #$60
PBEST    INCB 
         BPL       PBEST
         LDA       LIST           ;USE TO FLAG BANG OR SAR
         BMI       PITS           ;GO TO SAR
         LDA       #$20
         INC       PORT
         BITA      PORT           ;FIRST TEST > OR < 0
         BEQ       PMIN
         LDB       #$40           ;PLUS: CHECK MID-RANGE
         STB       DAC
         BITA      PORT
         BNE       PSTIF          ;VALID IF ABOVE MIDDLE POS VALUE
         BRA       PZRO
;
;
PMIN     LDB       #$C0           ;MINUS
         STB       DAC
         BITA      PORT
         BEQ       PSTIF
PZRO     CLRB
PSTIF    STB       -5,X           ;PUTS IN POT0-3
         BRA       PBRONG         ;END OF ROUTINE - - - - --
;
;
PAST     TFR       B,A            ;FIRST LINE OF SAR, ENTER BELOW
         ORA       DAC
         STA       DAC
PITS     LDA       #$20           ;BEGIN      SAR - - - - - -
         BITA      PORT           ;TEST COMP
         BNE       PARTS
         TFR       B,A
         EORA      DAC
         STA       DAC
PARTS    LSRB 
         CMPB      POTRES
         BNE       PAST
         LDB       DAC            ;SAVE AND GO ON
         BRA       PSTIF          ;END OF SAR FOR 'POTS4'
;
;
                                  ;WRITES B DATA TO A ADDRESS OF AY3-ETC <----
PSGX     LDX       #REG0          ;USES X REG. A AND X REGS DO NOT CHANGE
;
                                  ;USE THIS ENTRY IF X ALREADY LOADED <----
PSG      STB       A,X            ;STORE IN MIRROR RAM
         STA       APORT
         LDA       #$19           ;CODE FOR ADDR LATCH
         STA       BPORT
         LDA       #1
         STA       BPORT          ;LATCH ADDRESS TO PSG
         LDA       APORT          ;GET A REG BACK
         STB       APORT
         LDB       #$11
         STB       BPORT           ;LATCH DATA
         LDB       #1
         STB       BPORT
         RTS                      ;EXITS WITH ADDRESS STILL IN 'A'
;
;
;
INITPSG  LDD       #$0E00         ;INIT   SOUND CHIP ALL REGS=0 <----
SINIT    BSR       PSGX           ;     A=ADDRESS, B=DATA, WILL MIRROR
         DECA
         BPL       SINIT
         JMP       IREQ
;
PSGLUP   LDX       #REG0          ;SENDS STREAM TO PSG, U REG POINTER <----
         BRA       PSGLPU         ;FORMAT- PSG ADRS, PSG DATA ETC.
;
;
PSGULP   BSR       PSG            ;DELIMITER IS PSG ADRS= NEGATIVE
PSGLPU   LDD       0,U++          ;ENTER IF X REG=#REG0 <----
         BPL       PSGULP
         RTS
;
;
;
REQOUT   LDX       #REG0          ;SENDS REQ DATA TO PSG <----
         LDU       #REQ0          ;COMPARES FIRST WITH REG
         LDA       #$0D           ;SENDS OUT 14 REGS
REQOUR   LDB       0,U+           ;STORE INVERTED FOR SANITY
         CMPB      A,X            ;CALL AFTER ALL PSG FIDDLING
         BEQ       REQNOT
         BSR       PSG
REQNOT   DECA
         BPL       REQOUR
         RTS
;
;
;
; - - - - - - - VECTOR WRITING ROUTINES - - - - - - - - - - - --
;
;
INT1Q    LDA       #$1F           ;SET INTENSITY TO QTR LEVEL <----
         BRA       INTENS
;
;
INTMID   LDA       #$3F           ;SET INTENSITY TO HALF LEVEL <----
         BRA       INTENS
;
;
INT3Q    LDA       #$5F           ;SET TO 3/4 LEVEL <----
         BRA       INTENS
;
;
INTMAX   LDA       #$7F           ;SET INTENS TO MAX LEVEL <----
INTENS   STA       DAC            ;SET INTENS TO 'A' LEVEL- USE 0-7F <----
         STA       TENSTY
         LDD       #$0504
         STA       PORT
         STB       PORT
         STB       PORT           ;NOP
         LDB       #$01
         STB       PORT           ;LEAVE 4052 AT ADDRESS 0
         RTS
;
;
;
DOTTIM   STB       DWELL          ;ONE DOT, IF B HAS HOLD TIME <----
DOTX     LDD       0,X++          ;DRAWS ONE DOT INDEXED BY X <----
DOTAB    BSR       POSITN         ;DRAWS ONE DOT ACCORDING TO A,B <----
DOT      LDA       #$FF           ;TURNS VIDEO ON FOR DOT <----
         STA       SR
         LDB       DWELL
DADOT    DECB
         BNE       DADOT          ;HOLD FOR INTENSITY
         CLR       SR
         RTS
;
;
;
         DEC       LIST
DIFDOT   BSR       DOTX           ;2 BYTE/ENTRY DOT PACKAGES, USES 'LIST' <----
         LDA       LIST
         BNE       DIFDOT-3
         BRA       ZEROIT
;
;
DOTPAK   LDA       0,X+           ;DRAWS DOTS, USES PACKET FORMAT <----
         BGT       ZEROIT         ;ENDS THERE
DOKPAT   BSR       DOTX
         BRA       DOTPAK         ;FINISHES WHEN CONTROL BYTE=01
;
;
DEFLOK   LDX       #FALWEL        ;CALL TO KEEP POWER ON
         BSR       POSIT2         ;SUPER LONG SATURATED KEEP ALIVE PULSE
         JSR       ZERO.
         BSR       POSITX
         BRA       ZEROIT         ;ENDING
;
;
POSWID   LDB       #SCAL1X        ;REL POS IF X REG POINTS TO 16 BIT DATA (WIDE) <----
         STB       T1LOLC         ;FORCES TO 1X SCALE
         LDA       0,X
         LDB       2,X
         BRA       POSITN
;
;
POSITD   STA       DAC            ;REL POS IF IN D REG, CHG TO NORM SCALE <----
         PSHS      D
         LDA       #SCAL1X        ;DEFINED IN BEGINNING
         STA       T1LOLC
         CLR       PORT
         BRA       POOSI
;
;
POSIT2   LDB       #SCAL2X        ;REL POSITION VECTORS- 2X SCALE <----
         BRA       POSITB
;
;
POSIT1   LDB       #SCAL1X        ;NORMAL SCALE <----
POSITB   STB       T1LOLC         ;IF 'B' HAS TIMER LENGTH <----
POSITX   LDD       0,X++          ;RELATIVE POS VECTOR- IF TIMER SET <----
POSITN   STA       DAC            ;IF ACCUMS AND TIMER ARE LOADED <----
         CLR       PORT           ;BEGIN SAMPLE
         PSHS      D
POOSI    LDA       #UNZERO
         STA       PPCR
         CLR       SR             ;SURE VID OFF
         INC       PORT           ;Y LOADED
         STB       DAC            ;X VALUE
         CLR       T1HOC          ;START RAMP
;
         PULS      D              ;7 GET Y,X BACK TO FIGURE DELAY
         JSR       ABSVAL         ;37
         STB       -1,S           ;5
         ORA       -1,S           ;5 OR TOGETHER
         LDB       #$40           ;2
         CMPA      #$40           ;2
         BLS       MEDIO          ;3 IF VOLTAGES LOW SKIP DELAY
         CMPA      #$64           ;2
         BLS       FASTO          ;3
         LDA       #8             ;2
         BRA       TEDIO          ;3
;
;
FASTO    LDA       #4             ;2
;
TEDIO    BITB      IFR            ;2 ENDING DELAY FOR VOLTAGES OVER $40
         BEQ       TEDIO          ;3
FARM     DECA                     ;2
         BNE       FARM           ;3
         RTS                      ;5
;
;
;
MEDIO    BITB      IFR            ;4
         BEQ       MEDIO          ;3
         RTS                      ;IF NO EXTRA DELAY-
;
;
;
ZERO.DP  JSR       DPIO           ;LEAD IN TO ZEROIT, SETS DP TO I/O <----
         BRA       ZEROIT
;
;
ZEGO     LDA       ZGO            ;CAUSE ZEROIT IF 'ZGO' NONZERO <----
         BEQ       ZBONG
ZEROIT   LDD       #ZERO          ;VIDEO OFF, RETURN TO CENTER,SET ZERO REF <----
         STB       PPCR
         STA       SR
ZEREF    LDD       #$0302         ;SET ZERO REF ONLY <----
         CLR       DAC
         STA       PORT
         STB       PORT
         STB       PORT           ;(NOP) FOR S/H TIME
         LDB       #$1
         STB       PORT           ;LEAVE 4052 AT 0 ADDRESS
ZBONG    RTS
;
;
;
ZERO.    LDD       #ZERO          ;VID OFF, RET CENTER, NOT SET ZERO REF <----
         STB       PPCR
         STA       SR
         RTS
;
;
;
SIZPRAS  LDD       0,U++          ;GET RASTER SIZE, POSITION BY U REG, JMP TO RASTUR <----
DUSTRAG  STD       SIZRAS
POSNRAS  LDD       0,U++          ;PICK UP INDEUED POSITION, GO TO RASTUR <----
POSDRAS  JSR       POSITD         ;POSITION BY A,B, GO TO RASTUR <----
         JSR       DEL28
         JMP       RASTUR
;
AXOLOT   BSR       SIZPRAS
TEXSIZ   LDA       0,U            ;REPEAT CALLS TO SIZPRAS, 0=END BYTE <----
         BNE       AXOLOT
         RTS
;
;
;
AXLOTL   BSR       POSNRAS
TEXPOS   LDA       0,U            ;REPEAT CALLS TO POSNRAS, 0=END BYTE <----
         BNE       AXLOTL
         RTS
;
;
;
;  TO SHOW NUMBER OF SHIPS
;  INPUT A : ASCII CODE OF SYMBOL   INPUT B : NUMBER - NOT THE ASCII CODE
;                                    ( 0-9 SHOW AS  SUCH ,OTHERS SHOW INFINITEE
;  INPUT X POINTS AT/HOLDS POSITION :
;
;  USES WHATEVER IS IN T1 AND SIZRAS ` 2: ` ` ` ` ' ' '' ` ` ` ' ' ' '
;
SHIPSAT  LDX       ,X             ;<------------:------------- IF X POINTS AT POSITION
;
SHIPSHO  PSHS      B              ;<--------------:----------- IF X HOLDS POSITION
         LDB       #$80 ;                         :
         LEAU      -8,S  ;
         PSHU      D              ;SHIP SYMBOL,DELIMETER
;
         PULS      A              ;
         CMPA      #9
         BLS       UNDER
         LDA       #$3C
UNDER    ADDA      #$30
;
         LDB       #$2D
         PSHU      D              ;NUMBER,DASH
         PSHU      X              ;POSITION
         BRA       POSNRAS        ;EXIT FROM THERE - ALL REGISTERS ? ON EXIT
                                  ;USES 16 BYTES UNDER STACK - HOW LOW CAN YOU GET
;
;
;SCORED   LDY       #SEG7          ;DRAWS SCORE IN 7 SEGMENT FASHION <----
;         LDA       SIZSCR         ;TIME FOR THE VECTORS
;         STA       T1LOLC
;DONDIG   LDA       0,U+           ;U REG POINTS TO LSD OF SCORE- BCD
;         BMI       ZEROIT         ;WILL END THERE
;         LDA       A,Y
;         LDB       #1             ;B USED FOR LOOP CTR
;         STD       SCORET
;         LDX       #DRSEG7
;         BRA       SLORED
;STORED   ASL       SCORET+1
;         BEQ       DONDIG
;         LDB       SCORET+1
;SLORED   BITB      SCORET
;         BEQ       SCORBL
;         BSR       DIFFY
;         BRA       STORED
;SCORBL   BSR       DUFFY
;         BRA       STORED
;
;
DUFFAX   LDA       0,X+           ;<----          
         BRA       DUFLST
;
;
DUFTIM   STB       T1LOLC         ;ALL THESE SAME AS DIFFY ENTRIES BUT FIRST LINE BLANK <----
         BRA       DUFFY
;
;
DUFFX    LDD       0,X++          ;<----
DUFTLS   STB       T1LOLC         ;<----
DUFLST   STA       LIST           ; "   "     "    BUT A HAS LIST VALUE <----
DUFFY    LDD       0,X            ;SAME AS 'DIFFY', FIRST LINE BLANK <----
DUFFAB   STA       DAC            ;SAME AS 'DIFFAB' "     "    "     <----
         CLR       PORT
         LEAX       2,X
         NOP 
         INC       PORT
         STB       DAC
         LDD       #0
         BRA       UNCLE
;
;
DIFFAX   LDA       0,X+           ;ENTRY IF LIST VALUE INDEXED
         BRA       DIFLST
;
;
DIFTIM   STB       T1LOLC         ;ENTRY IF 'B' HAS TIMER VALUE <----
         BRA       DIFFY
;
;
DIFFX    LDD       0,X++          ;ENTER IF #VECTORS, SIZE IS INDEXED <----
DIFTLS   STB       T1LOLC         ;IF 'B' HAS TIMER VAL AND 'A' HAS #VECTORS-1 <----
DIFLST   STA       LIST           ;ENTRY IF 'A' HAS #VECTORS-1 <----
                                  ;DIFFY ROUTINE LEAVES 'LIST'=0
DIFFY    LDD       0,X            ;ENTRY FOR 2 BYTE/ENTRY VECTOR DRAWER <----
DIFFAB   STA       DAC            ;IF ACCUMS ARE VALID <----
         CLR       PORT           ;DIFFY LEAVES 'LIST'=0
         LEAX      2,X            ;WANT TIME DELAY HERE
         NOP
         INC       PORT
         STB       DAC
         LDD       #$FF00
UNCLE    STA       SR
         STB       T1HOC          ;START RAMP
         LDD       #$0040
FEXIT    BITB      IFR
         BEQ       FEXIT
         NOP                      ;MOUSE LIVES
         STA       SR
         LDA       LIST
         DECA
         BPL       DIFLST         ;PUT BACK TO FEB19 VERSION IF CRT LAG PERMITS
         JMP       ZEGO
;
PAC2X    LDB       #SCAL2X        ;ENTRY IF WANT 2X TIMING, AND NOT SET UP <----
         BRA       PACB
;
;
PAC1X    LDB       #SCAL1X        ;ENTRY IF WANT 1X " " " <----
         BRA       PACB
;
;
PACXX    LDB       0,X+           ;PICK UP TIMER INDEXED <----
PACB     STB       T1LOLC         ;ENTRY IF B HAS TIMER VALUE <----
PACKET   LDD       1,X            ;ENTRY POINT HERE IF T1LOLC VALID <----
         STA       DAC            ;FORMAT=STATUS,X,Y
         CLR       PORT
         LDA       0,X            ;PICK UP VIDEO ENABLE
         LEAX      3,X
         INC       PORT           ;Y S/H
         STB       DAC            ;SETUP X
         STA       SR             ;VID CONTROL
         CLR       T1HOC          ;START RAMP- FOR CRT PUT THIS BEFORE 'STB SR'
         LDD       #$40
EXIT     BITB      IFR
         BEQ       EXIT
         NOP                      ;MOUSE DELAY
         STA       SR
         LDA       0,X            ;ANOTHER ONE?
         BLE       PACKET
         JMP       ZEGO
;
DASHE    DECA
DASHEL   STA       LIST           ;DASHED LINES,ENTRY IF 'A' IS #VECTORS-1 <----
DASHY    LDD       0,X            ;DASHED LINES, 2 BYTES PER ENTRY <----
         STA       DAC
         CLR       PORT
         LEAX      2,X
         INC       PORT
         STB       DAC
         LDA       DASH
         LDB       #$40
         STA       SR             ;MOVE TO AFTER 'CLR T1HOC' FOR CRT
         CLR       T1HOC
         BITB      >IFR           ;EXTENDED FOR TIMING
         BEQ       DASIT          ;IF MORE TANE 1 SR LOAD REQ'D
         CLR       SR
         LDA       LIST
         BNE       DASHE          ;DO ONE MORE
         RTS                      ;EXIT POINT IF SHORT VECTOR
;
;
;
DEXIT    LDA       DASH
DASIT    STA       SR
         NOP                      ;TO PRESERVE 18 CYCLES
         BITB      IFR
         BEQ       DEXIT
         LDA       LIST
         CLR       SR
         TSTA
         BNE       DASHE          ;TO CONTINUE
         JMP       ZEGO           ;EXIT FOR MED-LONG VECTOR
;
DASHY3   LDA       ZGO            ;DASHED LINES- 3 BYTE PER VECTOR <----
         PSHS      A
         CLR       ZGO
DASH33   LDA       0,X+
         BPL       NOTSOL
         BSR       DASHY          ;$FF ENABLES DOTTED LINE
         BRA       DASH33
;
;
NOTSOL   BNE       SOLNOT
         JSR       DUFFY          ;$00 REQUESTS BLANK LINE
         BRA       DASH33
;
;
SOLNOT   DECA
         BEQ       DASDON
         JSR       DIFFY          ;$02 IS SOLID LINE
         BRA       DASH33
;
;
DASDON   PULS      A              ;$01 DELIMIT
         STA       ZGO
         JMP       ZEGO
;
;
;CURVY    LDD       #$8118         ;DRAW CURVED LINE FROM UPDATE TABLE <----
;         STA       PORT
;         STB       ACR            ;U - REG IS POINTER
;         BRA       RAINY
;JELLO    STB       PORT           ;FORMAT- Y,ON/OFF,X,X,X
;         STA       SR             ; 0 GOES TO NEXT Y, ADDIT. 0 ENDS
;JALLO    LDA       0,U+
;         BEQ       BRAINY         ;UPDATING X-VALUES
;         STA       DAC
;         BRA       JALLO
;BRAINY   LDB       #$81           ;RAMP OFF
;         STB       PORT
;         STA       SR             ;VID OFF
;RAINY    LDA       0,U+           ;NEXT Y
;         BEQ       JFIN           ;IF DONE
;         STA       DAC            ;IF GOOD Y VAL
;         DECB
;         STB       PORT           ;BEGIN S/H
;         LDD       0,U++          ;A=VID ENBL, B=NEXT X
;         INC       PORT           ;Y S/H DONE
;         STB       DAC
;         LDB       #$01
;         BRA       JELLO
;JFIN     LDA       #$98
;         STA       ACR            ;PUT BACK
;         RTS
;
;
;
RASTUR   STU       MESAGE         ;IF U REG POINTS TO STRING <----
RASTER   LDX       #ASCII-$20     ;FOR STANDARD ASCII <----
         LDD       #$1883         ;ALPHA-NUM USING ASCII CODES <----
         CLR       DAC
                                  ;SUPPORTS CODES $20-$6F. $20-$5F ARE STANDARD.$80=DELIM
         STA       ACR            ;MINIMUM 3 CHARS. CODES $60-$6F=GRAPHIC CHARS, SEE DATA
         LDX       #ASCII-$20     ;DECODE TABLE 5X7
TOFWD    STB       PORT           ;START ZEREF UPDATE
         DEC       PORT
         LDD       #$8081
         NOP
         INC       PORT
;
         STB       PORT
         STA       PORT           ;START Y=0 SAMPLE
         TST       REG0           ;NOP
         INC       PORT           ;Y S/H FINISHED
GOFWD    LDA       SIZRAS+1
         STA       DAC            ;FOR HORIZ SPEED
         LDD       #$0100
         LDU       MESAGE
         STA       PORT           ;START RAMP
         BRA       DECOD.         ;3
;
;
DECOD    LDA       A,X            ;5- CYCLE COUNT
         STA       SR             ;4
DECOD.   LDA       0,U+           ;6
         BPL       DECOD          ;3- TOTAL 18 FOR LOOP
;
EOLINE   LDA       #$81           ;2
         STA       PORT           ;4. STOP RAMP, TIME=18 CYCLES+18 PER CHAR
         NEG       DAC
         LDA       #$01
         STA       PORT           ;START RETRACE 2X FWD SPEED
         CMPX      #ASCII+448     ;$50*6-$20     4
         BEQ       EOMESS         ;3
         LEAX      $50,X          ;5
         TFR       U,D           ;6 CYCLES - CALCULATE #CHARS SHOWN
         SUBD      MESAGE        ;7 - RESULT IN B IS 1+#CHARS
         SUBB      #2            ;2
         ASLB                    ;2
         BRN       DELRAS        ;3
DELRAS   LDA       #$81          ;2 DECR LOOP = 9 PER
         NOP                     ;2
         DECB                    ;2
         BNE      DELRAS         ;3
;
         STA       PORT          ;4  WHOLE RETRACE= 45+ 9 PER B DELAY LOOP
                                 ;39-12 FOR FWD OVERHEAD=27
                                 ;EQUIV OF 3 CHARS
H5
         LDB       SIZRAS
         STB       DAC            ;DO Y SHIFT
         DEC       PORT
         LDD       #$8101
         NOP
         STA       PORT           ;END Y SAMPLE
         CLR       DAC
         STB       PORT           ;START RAMP
;;;;     LDB   ;   #$03
         STA       PORT           ;FINISH RAMP
         LDB       #$03           ;MOD ADDED 4/13/82
;
         BRA       TOFWD          ;END O RASTER
;
;
EOMESS   LDA       #$98
         STA       ACR
         JMP       ZEROIT         ;WILL RETURN FROM THERE
;
;
; - - - - - - ASTD GEN - - - - - - -
;
RAND3    PSHS      B,X            ;RANDOM, 3 NEW BITS <----
         LDB       #2
         BRA       PSUDOH         ;ANSWER RETURNS IA 'A'
;
;
                                  ;THIS ROUTINE DOES NOT USE DP REG
RANDOM   PSHS      B,X            ;RANDOM, 1 NEW BIT <----
         CLRB
PSUDOH   LDX       >SEED          ;'SEED' IS INDIRECT POINTER
HODUSP   LDA       1,X            ;ABOUT 50 USEC
         ROLA                     ;ALL REGS INTACT BESIDES 'A'
         ROLA
         ROLA
         ROLA
         EORA      2,X
         RORA                     ;INTO CARRY
         ROL       0,X
         ROL       1,X
         ROL       2,X
         DECB
         BPL       HODUSP
         LDA       0,X
         PULS      B,X,PC         ;END OF RANDOM
;
;
IREQ     LDB       #$0D           ;CLEAR REQ AREA, SET REQ6=#$3F <----
         LDX       #REQ0          ;(SOUND DRIVER)
         BSR       CLRSOM
         LDA       #$3F
         STA       6,X
         RTS
;
;
;
CLRSOM   CLRA                     ;CLEARS B BYTES STARTING AT X <----
         BRA       GILL
;
;
CLRMEM   LDX       #$C800         ;CLEARS 256 BYTES START AT $C800 <----
CLR256   LDD       #$00FF         ;CLEARS 256 START AT 'X' <----
GILL     CLR       D,X            ;CLEARS 'D' BYTES START AT 'X' <----
         SUBD      #1
         BPL       GILL
         RTS
;
;
;
NEGSOM   LDA       #$80           ;FILL B BYTES WITH $80 START AT X <----
                                  ;B ONLY BETWEEN 0 AND $7F
FILL     STA       B,X            ;LOADS 'A' DATA INTO 'B' REGS START AT 'X'<----
         DECB                     ;ONLY WORKS FOR 'B' BETWEEN 0 AND $7F * * * * * * *
         BNE       FILL
         STA       0,X
         RTS
;
;
;
DEKR3    LDB       #2             ;3 INTERVAL TIMERS <----
         BRA       BEKR
;
;
DEKR     LDB       #5             ;6 INTERVAL TIMERS- COUNT DOWN TO 0 ONCE PER FRAME<----
BEKR     LDX       #X0            ;X IS STARTING TIMER, B = #
AEKR     TST       B,X
         BEQ       GEKR
         DEC       B,X
GEKR     DECB
         BPL       AEKR
         RTS
;
;
;
DEL38    LDB       #3             ;DELAY 38 CYCLES INCLUDING JSR TO GET HERE <----
         BRA       DEL
;
;
DEL33    LDB       #2             ;DEL 33 <----
         BRA       DEL
;
;
DEL28    LDB       #1             ;28 CYCLES <----
         BRA       DEL
;
;
DEL20    CLRB                     ;20 CLOCK CYCLES <----
DEL      DECB                     ;GENERAL DELAY <----
         BPL       DEL
DEL13    RTS                      ;13 CYCLES <----
;
;
;
BITE     LDX       #BITTBL        ;DECODE TO BIT POS FOR 0-7 VALUE IN ACC A <----
         LDA       A,X
         RTS
;
;
;
ABSVAL   TSTA                     ;GIVES ABSOLUTE VALUE FOR A,B REGS <----
         BPL       AOK            ;FUDGES -128 TO +127
         NEGA
         BVC       AOK
         DECA
AOK      TSTB
         BPL       BOK
         NEGB
         BVC       BOK
         DECB
BOK      RTS
;
;
;
;  - - - - - THESE USE DP REG = $C8 - - - - - - - - - -
;
         SETDP     $C8
;
        ;TAKES DELTA X AND DELTA Y IN REG B,A <----
        ;GIVES ANGLE- CLOSEST OUT OF 64 POSSIBLE
        ;ANSWER IN A AND B REG (SAME) AND 'ANGLE' 0-$3F
;
COMPAS   PSHS      X              ;SAVE FOR RETURN
         STD       ABSY           ;2 BYTES ABSY,ABSX
         ROLB                     ;SHIFT SIGNS INTO 'ANGLE'
         LDB       #0
         ROLB
         ROLA
         ROLB
         ASLB
         STB       ANGLE
;
         LDD       ABSY
         BSR       ABSVAL         ;MAKE POS
;
         STA       ABSY           ;SET FLAG IF Y<X
         CMPB      ABSY
         BLS       RETAEB
         INC       ANGLE          ;BITS 2,1,0=X SIGN,Y SIGN, AND X>Y BIT
         EXG       A,B            ;SWAP X,Y ID Y<X
         BRA       RETAEB         ;SQUASH TO <10
;
;
BEATER   LSRA
         LSRB
RETAEB   CMPA      #9
         BHI       BEATER
         STD       ABSY           ;NOW IN RANGE
;
         LDB       ANGLE          ;INDEXT TRANSLATOR WEIRD TABLE
         LDX       #WEIRD
         LDB       B,X
;
         LDX       #ROWTRI        ;DECODES ROWS FOR TRIANGLE LOOKUP
         LDA       A,X            ;(A HAD LARGER VALUE) GET ROW LOCATION
         ADDA      ABSX           ;ADD IN SMALLER VALUE FOR COLUMN
         ADDA      #10            ;GO TO START OF FIBTBL
;
         BITB      #1             ;B HAS 'ANGLE'
         BNE       ITSKUL         ;BIT WAZ SET IF ADJUST NEEDED
;
         ADDB      A,X
         BRA       ITSLOD
;
;
ITSKUL   DECB                     ;IF Y<X
         SUBB      A,X
ITSLOD   STB       ANGLE
         LDA       ANGLE          ;ANSWER IN BOTH A AND B!
         PULS      X,PC           ;DONE
;
;
;
COSGET   ADDA      #$10           ;GETS COSINE OF 'A' <----
SINGET   LDX       #RTRIGS        ;GETS SINE OF 'A' <---- 26-30 CYCLES
         CLRB
         BITA      #$20           ;TEST FOR NEG
         BEQ       SINPOS
         LDB       #$80
SINPOS   ANDA      #$1F
         CMPA      #$10           ;TEST FOR 90 DEG
         BNE       NOTSQR
         INCB                     ;INDICATE OVERFLOW
NOTSQR   LDA       A,X
         RTS                      ;ANSWER IN D REG, A=VALUE,B=SIGN/OVERFLOW
;
;
;
SINCOS   PSHS      X              ;GETS SINE,COSINE OF 'ANGLE' <----
         LDA       ANGLE
         BSR       SINGET
         STD       SINE
         LDA       ANGLE
         BSR       COSGET
         STD       COSINE
         PULS      X,PC
;
;
;
RATOR    SUBB      #$10           ;SINGLE LINE ROT, A=X VALUE,B=ANGLE,Y ASSUMED=0 <----
ROTOR    STB       ANGLE          ;SINGLE LINE ROTATE, B=ANGLE <----
ROTAR    STA       LEG            ;A=Y VALUE, X ASSUMED=0 <----
         BSR       SINCOS
         BSR       RSIN
         NEGA
         PSHS      A
         BSR       RCOS
         PULS      B,PC           ;Y,X COORDS IN A,B REGS AT EXIT
;
;
;
DANROT   STA       >ANGLE         ;'DIFFY' STYLE ROTATE, A HAS ANGLE <----
DISROT   STB       >LIST          ;" ", B HAS #VECTORS-1 <----
DIFROT   PSHS      DP             ;" ", LIST,ANGLE SET <----
         JSR       DPRAM
         BSR       SINCOS
         BRA       RX             ;OTHER DETAILS SIMILAR TO 'POTATE' BELOW
;
;
POTATA   STA       >ANGLE         ;PACKET ROTATE IF ANGLE IN 'A' <----
POTATE   PSHS      DP             ;PACKET ROTATE IF 'ANGLE' VALID <----
         JSR       DPRAM
         STA       LIST           ;MINUS INDICATES POTATE AS OPPOSED TO DIFROT
         BSR       SINCOS         ;X REG POINTS TO PACKET, U REG TO DESTINATION
SHARON   LDA       0,X+           ;TEST CONTROL BYTE
         STA       0,U+
         BLE       RX             ;IF NOT END BYTE
         CLR       LIST
         PULS      DP,PC          ;ADIOS
;
;
;
FROTT    DEC       LIST
;
RX       LDA       0,X+           ;NEXT LINE SPEC Y,X
         BSR       RCOSA
         STA       0,U            ;1 BYTE FOR INTERMEDIATE RESULTS
;
         LDA       0,X
         BSR       RSINA
         ADDA      0,U            ;RESULT IS NEW Y VAL
         STA       0,U+           ;NEW Y=YCOS(ANGLE) + XSIN(ANGLE)
;
         LDA       -1,X
         BSR       RSINA
         STA       0,U
;
         LDA       0,X+           ;LEAVES X AND U POINT TO NEXT VECTOR
         BSR       RCOSA
         SUBA      0,U
         STA       0,U+           ;NEW X=XCOS(ANGLE) - YSIN(ANGLE)
;
         LDA       LIST
         BMI       SHARON
         BNE       FROTT
         PULS      DP,PC          ;END OF DIFROT
;
;
;
RSINA    STA       LEG            ;MULTS 'A' BY SINE PREV STORED <----
RSIN     LDD       SINE           ;MULTS 'LEG' BY SINE PREV STORED <----
         BRA       RSC
;
;
RCOSA    STA       LEG            ;MULTS      'A' BT COSINE PREV STORED <----
RCOS     LDD       COSINE         ;MULTS 'LEG' BY COSINE PREV GOTTEN <----
;
RSC      STB       LAG            ;HOLDS SIGN,OVERFLOW BITS 7,0
                                  ;WARNING NO INPUT=$80 ALLOWED
         BITB       #1            ;SHOULD MULT?
         BEQ       DOALL          ;NO, FLIP ONLY
         LDA       LEG
         BRA       FONLY
;
;
DOALL    LDB       LEG            ;LEG IS INPUT VECTOR
         BPL       BEPL
         COM       LAG            ;TWO WRONGS MAKE A RITE
         NEGB
BEPL     MUL
         ADCA       #0
FONLY    LDB       LAG            ;CHECK SIGN FLIP
         BPL       ROUT
         NEGA
ROUT     RTS                      ;ANSWER IN A REG
;
;
;
BAGAUX   LDB       A,U            ;STUFFS A+1 VALUES FROM U TO X <----
         STB       A,X
STFAUX   DECA                     ;STUFFS A VALUES FROM U TO X <----
         BPL       BAGAUX
RATS     RTS
;
;
;
;      TUNE PLAYER- FOR THIS TO WORK, MUST ALSO CALL 'REQOUT' ONCE/FRAME
;
                         ;TUNE PLAYER <----
;
REPLAY   LDA       TSTAT          ;TO START, PUT $01 IN TSTAT
         BMI       HEYBAB         ;TSTAT= $80 DURING TUNE, =0 AT IDLE
         BEQ       RATS           ;EXIT IF NOT GOING
;
SPLAY    LDX       #NOTES         ;TO START TUNE <----
;
SOPLAY   STX       DOREMI         ;TO START WITH ALTERNATE NOTE TABLE BY X REG <----
YOPLAY   LDA       #$80           ;INIT TUNE, NOT CHANGE NOTE BASE <----
         STA       TSTAT          ;INDICATE START
         LDD       0,U++          ;IF NEW, U SHOULD POINT TO TUNE
         STD       FADE
         LDD       0,U++
         STD       VIBE
         STU       TUNE
         JSR       IREQ           ;CLEAR 13 REGS, SET REQ6=$3F
         LDD       #$1F1F
         STD       FADEB
         LDD       #0
         STD       TONEB
         STD       TONEC
         STA       NEWGEN
         BRA       PICKUP
;
;
HEYBAB   LDU       #FADEA         ;REFRESH TONE AMPLITUDES
         LDB       #2
FAHDE    LDA       B,U
         CMPA      #$1F           ;LIMIT AT $1F
         BEQ       THRILL
         INC       B,U
THRILL   DECB
         BPL       FAHDE
;
         LDX       VIBE           ;PLOGLAMMABLE VIBLATO
         LDU       #RATEA         ;TABLE INDIRECT 'VIBE'
         LDA       #7             ;VARIED RATES
RAZV     INC       0,U
         CMPA      0,U            ;MOD COUNTERS
         BGE       DOC
         CLR       0,U
DOC      LDB       0,U+           ;DECODE TABLE BY TUNE
         ANDB      #7
         LDB       B,X
         STB       0,U+
         INCA
         CMPA      #9
         BLS       RAZV
;
NOYC     DEC       RESTC          ;CHECK REST
         BNE       UPATEN         ;BRANCH IF NOT TIMED OUT
;
HITIT    LDA       NEWGEN         ;CYCLE TO NEXT TONE GEN
         DECA
         BPL       SPIN
         LDA       #2
SPIN      STA      NEWGEN
;
PICKUP   LDB       [TUNE]         ;PICK UP NEW NOTE
         LDU       #FADEA
         CLR       A,U
         BITB      #$40
         BEQ       TONEON
         LDX       #CRT           ;NOISE IS ON FOR THIS NOTE
         LDA       A,X
         ANDA      REQ6
         STA       REQ6
         LDA       NEWGEN
         ADDA      #3
         LDA       A,X
         ORA       REQ6
         STA       REQ6
         ANDB      #$1F
         STB       NOISEF
         BRA       DONALD
;
;
TONEON   LDX       #CRATE
         LDA       A,X
         ANDA      REQ6
         STA       REQ6
         LDA       NEWGEN
         ADDA      #3
         LDA       A,X
         ORA       REQ6
         STA       REQ6
         LDA       NEWGEN
         ASLA                     ;INDEX FOR NOTE LOC
         ADDA       #3            ;POINT TO NOTES
         LEAU       A,U           ;U NOW HAS PLACE TO DUMP NOTE
         ANDB       #$3F          ;STRIP OFF FLAG
         ASLB 
         LDX       DOREMI
         LDD       B,X
         STD       0,U            ;STORE NOTE VALUE
;
DONALD   LDX       TUNE           ;ANOTHER NOTE?
         LDB       0,X+
         STX       TUNE
         TSTB 
         BMI       HITIT          ;YES
         LDB       0,X+
         BPL       RESTY
;
XPLAY    JSR       IREQ           ;TO STOP TUNE BEFORE END <----
         CLR       TSTAT          ;PREPARE TO END
         RTS                      ;CC SET FOR QUICK TEST
;
;
;
RESTY    STX       TUNE           ;SET UP WAIT
         ANDB      #$3F
         STB       RESTC
;
UPATEN   LDY       FADE           ;GETS ADDR OF FADEOUT TABLE
         LDU       #FADEA         ;FADE SPEC TONE GEN A
         LDX       #REQ3
         LDA       #2
IPATEN   LDB       0,U+
         BITB      #1             ;ODD OR EVEN?
         BEQ       PEVEN
         LSRB                     ;DIV/2
         LDB       B,Y
         ANDB      #$F
         BRA       JOWL
;
;
PEVEN    LSRB
         LDB       B,Y            ;GET IGH BYTE
         LSRB
         LSRB
         LSRB
         LSRB
JOWL     STB       A,X
         DECA
         BPL       IPATEN
;
VIBO     LDU       #TONEC+2       ;VIBRATO SECTION HERE
         LDX       #REQ8
HARRY    LDD       0,--U
         TST       -8,U           ;USE ONLY VIBC FOR FIRST
         BPL       RISE
         NEG       -8,U           ;NEG, SHOULD SUBTRACT
         SUBB      -8,U
         SBCA      #0
         NEG       -8,U
         BRA       BUYS
;
;
RISE     ADDB      -8,U
         ADCA      #0
BUYS     STD       0,X++
         CMPX      #REQC+2
         BNE       HARRY
HUMAN    RTS
;
;
;
MPLAY    DB        $20,$C0,$40,$C0,'PLAYER',$80     ;PLAYER OPTION MESSAGE
MGAME    DB        $E0,$C0,$01,$C0,' GAME',$80      ;GAME OPTION MESSAGE
;
;
;
         SETDP     $C8
OPTION   STD       >FADE          ;DISPLAYS AND SETS PLAYER/GAME OPTIONS <----
         TSTA
         BEQ       BOPTO
         LDA       #1
BOPTO    TSTB
         BEQ       IPTOID
         LDB       #1
IPTOID   STD       >PLAYRZ
         JSR       DPRAM
         LDD       #$F850
         STD       SIZRAS
         STA       LAG            ;TO NOT GLITCH ON EDGE
         BRA       SMILE
;
;
POTION   JSR       FRAM20         ;ENTRY VALUES: A=MAX PLAYERS, 0=DON'T SHOW
         CLRA
         JSR       ENPUT          ;B=MAX GAMES,   "  "     "
         JSR       DEKR3
         JSR       INTMAX
         SETDP     $D0
         LDA       PLAYRZ
         LDY       #MPLAY         ;PLAYER MESSAGE
         BSR       SHOWY
         LDA       GAMZ
         LDY       #MGAME
         BSR       SHOWY
;
         SETDP       $C8
         JSR       DPRAM
         LDA       LAG            ;WAIT FOR KEY RELEASE
         BEQ       PWR80
         LDA       TRIGGR
         BNE       SMILE
         CLR       LAG
PWR80    LDA       X1
         BEQ       HUMAN          ;TIMEOUT
;
         LDA       X0             ;KEY0 PLAYER SEL
         BNE       POTION
         LDA       KEY3
         BNE       HUMAN          ;START KEY
         LDA       KEY0
         BEQ       PAS0
         LDA       PLAYRZ
         BEQ       PAS0
         INCA
         CMPA      FADE           ;LIMIT
         BLS       YIPPIE
         LDA       #1
YIPPIE   STA       PLAYRZ
         BRA       SMILE
;
;
PAS0     LDA       GAMZ
         BEQ       POTION
         LDB       KEY1           ;KEY1 GAME UP
         BEQ       PAS1
         INCA
         CMPA      FADE+1
         BLS       BOIGE
         LDA       #1
         BRA       BOIGE
;
;
PAS1     LDB       KEY2           ;KEY2 GAME DOWN
         BEQ       POTION
         DECA
         BNE       BOIGE
         LDA       FADE+1         ;WRAP MINUS
BOIGE    STA       GAMZ
SMILE    LDA       #$F3
         STA       X1             ;TIMEOUT PARAM
         COMA
         STA       X0
         BRA       POTION
;
;
SHOWY    LDX       #FADEA         ;SHOWS NUMBER AND MESSAGE FOR 'OPTION' ABOVE <----
         PSHS       A
         BSR       SCLR
         LDA       0,S+           ;TO SET CC
         BEQ       YOTION
         BSR       SHADD          ;SHOW NUMBER
         TFR       X,U
         LDD       0,Y++
         JSR       POSDRAS
         TFR       Y,U            ;Y HELD NUMBER,MESSAGE LOC, AND MESSAGE
         JSR       POSNRAS
YOTION   RTS
;
;
;
SCLR     LDD       #$2020         ;CLEARS RASTER TYPE SCORE, 6 DIG+ 1 BYTE END <----
         STD       0,X            ;X IS MSD ADDRESS
         STD       2,X            ;SHOWS TWO ZEROS
         STA       4,X
         LDD       #$3080         ;LEAVES X REG ALONE
         STD       5,X
         RTS
;
;
;
SHADD    LDU       #0             ;ADDS 1 BYTE HEX IN 'A' TO SCORE <----
CHAD     CMPA      #99            ;X POINTS TO 6 DIG ASCII STYLE SCORE
         BLS       TEN10
         SUBA      #100           ;BINARY -> BCD CONVERT
         LEAU      $0100,U
         BRA       CHAD
;
;
TEN10    CMPA      #9
         BLS       ONE1
         SUBA      #10
         LEAU      $0010,U
         BRA       TEN10
;
;
ONE1     LEAU      A,U
         TFR       U,D            ;GOES INTO 'SADD'
;
SADD     PSHS      A              ;SCORE ADDING ROUTINE, 'D' HAS 4 DIG PACK BCD <----
         PSHS      B
SADD2    LDB       #5             ;HERE IF DATA ON STACK <----
LLAD     CLRA                     ;X POINTS TO MSD OF 6 DIG(BYTE) SCORE
         CMPB      #1             ;ADDS 4 PACKED BCD DIGS(2 BYTES)
         BLS       SIDJ           ;DP REG NO CARE
         BITB      #1             ;DOES NOT CHANGE X REG
         BEQ       SADJ
         LDA       0,S
         BRA       SEDJ
;
;
SADJ     LDA       0,S+           ;1ST AND 3RD INPUT DIGS
         LSRA
         LSRA
         LSRA
         LSRA
SEDJ     ANDA      #$0F           ;MASK FROM UNPACKING
SIDJ     ADDA      >LIST          ;SERVES AS CARRY IND
         CLR       >LIST
         ADDA      B,X
         CMPA      #$2F
         BGT       SGDJ           ;ADJUST IF JUST ZERO
         ADDA      #$10
SGDJ     CMPA      #$39           ;TEST FOR CARRY
         BLS       SARJ
         SUBA      #10
         INC       >LIST
SARJ     STA       B,X            ;STORE RESULT
         DECB                     ;B TICKER
         BPL       LLAD
         CLR       >LIST
         CLRB
ZERBLNK  LDA       B,X
         CMPA      #$30
         BNE       BLNKIT
         LDA       #$20
         STA       B,X
         INCB
         CMPB      #5
         BLT       ZERBLNK
BLNKIT   RTS
;
;
;
; WINNER      --      ENTRY VALUES:      X --> SCORE 1
; ======                        U --> SCORE 2
;                  RETURN VALUES:      A = 0 -- SCORE 1 = SCORE 2
;                              A = 1 -- SCORE 1 > SCORE 2
;                              A = 2 -- SCORE 2 > SCORE 1
;                  FUNCTION:      DETERMINE THE GREATER OF TWO SCORES
;                              STORED IN ASCII (RASTER) FORM
WINNER:
         PSHS      X,U
         CLRA
WIN.1    LDB       X+
         BMI       WIN.RTS
         CMPB      U+
         BEQ       WIN.1
         BHI       WIN.2
         INCA
WIN.2    INCA
WIN.RTS  PULS      X,U,PC
;
;
;
; HIGHSCR      --   ENTRY VALUES:  X --> SCORE
; =======                          U --> HIGH SCORE
;                  RETURN VALUES:  ???
;                  FUNCTION:       UPDATE HIGH SCORE
;
HIGHSCR  BSR       WINNER
         CMPA      #1
         BNE       HSCR.RTS
HSLOOP   LDA       X+
         STA       U+
         BPL       HSLOOP
HSCR.RTS
         RTS
;
;
;
;      MARK'S COLLISION TEST WITH PAUL'S OFFSET PROVISION
;
;      mangled together and tested 4/7/82--both routines of the use of the
;      "U" register are needed to make the routine useful to all programmers
;      since half the programs are absolute oriented and the other half are
;      location oriented (the difference between stored data and calculated
;      data)
;
;      A = BOX 'Y' DIMENSION (DELTA Y)
;      B = BOX 'X' DIMENSION (DELTA X)
;      X = YX COORDS OF POINT TO BE TESTED
;      Y = YX COORDS OF THE CENTER OF BOX
;            if using the offset:
;                  OFF1BOX -- U = address of location of offset
;                  OFF2BOX -- U = actual offset
;            if not using the offset, U = who cares
;
;      RETURN VALUES -- ALL PERSERVED
;            (hence the reason for the extra push of the center
;             of the box so that any offset added to it will not
;             be returned)
;            CARRY BIT: 0 (CLEAR) IF NO COLLISION
;                     1 (SET)   IF COLLISION
;
BOXDIM   EQU       0
BOXPOS   EQU       4
XBOXPOS  EQU       5
PNTPOS   EQU       2
;
         SETDP     $C8
;
;
OFF1BOX  PSHS      Y      ;U REG INDEXES OFFSET <----
         PSHS      A,B,X,Y
         LDD       BOXPOS,S
         ADDA      ,U
         ADDB      1,U
BADGER   STD       BOXPOS,S
         BRA       CHICAGO
;
;
OFF2BOX  PSHS      Y              ;U REG CONTAINS OFFSET <----
         PSHS      A,B,X,Y
         TFR       U,D
         ADDA      BOXPOS,S
         ADDB      XBOXPOS,S
         BRA       BADGER
;
;
FINDBOX  PSHS      Y              ;TSET XOB <----
         PSHS      A,B,X,Y
CHICAGO  TFR       S,X
         CLRB
;
COLID.0  ABX
         LDA       BOXPOS,X
         ADDA      BOXDIM,X
         BVC       COL.0
         LDA       #$7F
;
COL.0    CMPA      PNTPOS,X
         BLT       N.COLID
;
         LDA       BOXPOS,X
         SUBA      BOXDIM,X
         BVC       COL.2
         LDA       #$80
;
COL.2    CMPA      PNTPOS,X
         BGT       N.COLID
;
         INCB
         CMPB      #2
         BLO       COLID.0
;
         ORCC      #$01
         BRA       COL.EXT
;
;
N.COLID  ANDCC     #$FE
;
COL.EXT  PULS      A,B,X,Y
         PULS      Y,PC
;
;
;
; PROGRAMMABLE EXPLOSION - - - - - - - - - - !
;
; THE FOLLOWING ARE ENTRY VALUES BY WAY OF 'U' POINTER
;
XREQ6    EQU       RATEA          ;LOCAL PSG ENABLES. ROUTINE STEALS REGS FROM TUNE PLAYER
                                  ;LOG ENABLES AS IN PSG BUT USES POS LOG. UP TO 3 TONES, 3 NOISE
                                  ;THE 'AXE' ROUTINE WILL NOT SCREW WITH UNCALLED REGISTERS
;
N.DIR    EQU       VIBA           ;NOISE FREQ MOVE DIR, 3 CONDITIONS:
                                  ;1) IF BIT7 SET, THEN LOWER 5 BITS= STEADY NOISE FREQ
                                  ;2) IF BYTE NON-ZERO NOISE FREQ GOING DOWN
                                  ;3) IF BYTE=0 NOISE GOING UP
;
L.DIR    EQU       RATEB          ;LOUDNESS MOVE DIR, 3 CONDITIONS:
                                  ;1) IF BIT 7 SET, LOWER 4 BITS=STEADY LOUDNESS
                                  ;2) IF BYTE NON=ZERO VOLUME GOING UP
                                  ;3) IF BYTE=0 VOL GOING DOWN
;
X.SPEED  EQU       VIBB           ;COUNTDOWN RATE FROM $7F. RANGE 1(LONGEST) TO $80(SHORTEST)
;
; THE FOLLOWING ARE SCRATCH LOCATIONS
;
T.COUNT  EQU       RATEC          ;FOR RUMBLES
T.NUM    EQU       VIBC
N.NUM    EQU       TUNE
BOTH     EQU       TUNE+1
;
;        ORG       0
         SETDP     $C8
AXE      LDA       SATUS          ;EXPLOSION SEQUENCER FOR GI-PSG <----
         BPL       XGO            ;EITHER GOING, NOT GOING, OR REQUEST
                                  ;DP SHOULD=$C8, USE WITH 'REQOUT'
         ANDA      #$7F           ;REQUEST TO START EXPLOSION
         STA       SATUS
         LDX       #XREQ6         ;LOAD IN 4 PARAMETER BYTES
         LDA       #4
         JSR       STFAUX         ;B HAS 'XREQ6' DATA AT END
         LSRB
         LSRB
         LSRB
         ORB       XREQ6
         ANDB      #$07
         STB       BOTH           ;USE FOR VOL ENABL POS LOG
         LDB       XREQ6
         ANDB      #$38
         STB       N.NUM          ;NONZERO= NOISE SELECTED
         LDB       XREQ6
         ANDB      #$07
         STB       T.NUM          ;BIT POS FOR EACH TONE CHANNEL
         LDB       #2
         STB       T.COUNT        ;BIT FOR WHICH RUMBLE
         LDA       #$7F
         BRA       XORE
;
;
XGO      LDA       XACON
         BEQ       XBYE           ;NOT GOING
;
XMORE    SUBA      X.SPEED        ;SEQUENCER
         BPL       XORE
         CLRB
         STB       XACON          ;PREPARE TO EXIT
         BRA       LOUDIN
;
;
XORE     STA       XACON
         LSRA                     ;SCALE TO $1F RANGE
         LSRA                     ;SUITABLE FOR NOISE SPEC
         LDB       N.NUM          ;IF=0, NO NOISE ENABLED
         BEQ       LIN            ;SKIP THIS, JUST PUT LOUDNESS IN
         STA       NOISEF
         LDB       N.DIR          ;WANT UP, DOWN, OR STEADY?
         BMI       NIN            ;BIT7 SET= STEADY NOISE FREQ
         BEQ       LIN            ;ZERO BYTE= NOISE FREQ GOING UP
         TFR       A,B            ;NOISE GOING DOWN
         COMB
NIN      STB       NOISEF
;
LIN      LSRA                     ;SETUP LOUDNESS ENVELOPE
         CMPA      #7
         BLS       ROLD
         CMPA      #$0F
         BEQ       ROLD
         INCA                     ;STIFFEN ROLLOFF
ROLD     LDB       L.DIR          ;LOUDNESS UP, DOWN, OR STEADY?
         BMI       LGOIN          ;IF BIT 7 SET, STEADY LOUDNESS VAL=L.DIR
         BEQ       LGOINI         ;IF L.DIR=0, LOUDNESS GOING DOWN
         EORA      #$0F
LGOINI   TFR       A,B
LGOIN    BSR       LOUDIN         ;PUTS IN 1-3 REGS CHA,B,CV
;
      ;RUMBLING TONES
;
         LDB       T.NUM
         BEQ       NOBLE          ;SKIP IF NO TONES
BNZ      LDA       T.COUNT
         DECA
         BPL       TNZ
         LDA       #2
TNZ      STA       T.COUNT        ;CYCLE THRU CHANS
         JSR       BITE           ;TEST BIT POS
         BITA      T.NUM
         BEQ       BNZ            ;FIND IT
         LDB       T.COUNT
         ASLB
         NEGB
         LDX       #CHAF
         LEAX      B,X            ;(CAN'T USE ABX)
         JSR       RANDOM
         ANDA      #$0F
         CMPA      #5             ;FILTER HI TONES
         BHI       NOFLT
         ASLA
         ADDA      #5
NOFLT    STA       0,X
         LDA       RANCID+1
         STA       1,X            ;LO BYTE TOO
;
NOBLE    LDA       XREQ6
         COMA
         ANDA      REQ6
         STA       REQ6           ;KEEP ENABLED TO AVOID BUG
;
XBYE     RTS
;
;
;
LOUDIN   LDA       BOTH           ;STUFF A,B,AND/OR C VOLS
         LDX       #CHAV+1        ;PUT LOUD VALUE IN UP TO 3 CHANNELS
NOSE     TSTA
         BEQ       LOUBYE
         LEAX      -1,X
         LSRA
         BCC       NOSE
         STB       0,X
         BRA       NOSE
;
;
LOUBYE   RTS
;
;
;
; - - - - - - - DATA TABLES - - - - - - -
;
BITTBL   DW        $0102,$0408,$1020,$4080
;
;SEG7:                                                   ;7 SEGMENT DECODER FOR SCORE DISPLAY
;         DB        $77,$11,$6B,$3B,$1D,$3E,$7E,$13      ;0-7
;         DB        $7F,$3F,$5F,$7C,$66,$79,$6E,$4E      ;8,9,A-F
;         DB        $00,$20,$08,$02,$0F,$78              ;BLANK,DASHES,SQUARES
;
CRT      DB        $F7,$EF,$DF,$01,$02,$04               ;FOR 'REPLAY'
CRATE    DB        $FE,$FD,$FB,$08,$10,$20
;
;DRSEG7:                                                 ;DRAWS SHAPE OF 7 SEG RECTANGLE
;J        EQU       $40
;         DB        J,0,0,-J,-J,0,0,J,-J,0,0,-J,J,0,0,-J
;
FALWEL   DB        $7F,$7F,$80,$80  ;FOR POWER MONITOR
;
            ;5X7 CHARACTER DECODE $20 THRU $6F
            ;CHAR SET GROUPED BY ROW, TOP TO BOTTOM
            ;$20-$2F= ' !"#$%&'()*+,-./'
            ;$30-$3F= '0123456789:;<=>?'
            ;$40-$4F= '@ABCDEFGHIJKLMNO'
            ;$50-$5F= 'PQRSTUVWXYZ[\]^_'
            ;$60-$6F= W.T. CHAR SET. SEE DOC FOR PICTURES
;
            ;$60 = CAR
            ;$61 = UP ARROW . FOR RIGHT ARROW USE -> 2 CHARS
            ;$62 = MUSICAL NOTE
            ;$63 = DOWN ARROW. FOR LEFT ARROW USE <- 2 CHARS
;
            ;$64 = CIRCLE: LARGE, HOLLOW
            ;$65 = CIRCLE: LARGE, SOLID
            ;$66 = CIRCLE: SMALL, SOLID
            ;$67 = COPYRIGHT CIRCLE
;
            ;$68 = SPACE SHIP
            ;$69 = SPACEPERSON
            ;$6A = HAVE A NICE DAY
            ;$6B = HAVE A BAD DAY
;
            ;$6C = INFINITY
            ;$6D = HOLLOW SQUARE
            ;$6E = 7X8 FILL CHAR .DO NOT USE AT END OF LINE
            ;$6F = 7X7 FILL CHAR. OK AT END OF LINE
;
ASCII    DW        $0020,$5050,$20C8,$2010,$1040,$2000,$0000,$0008      ;ROW7
         DW        $3020,$7070,$10F8,$30F8,$7070,$0060,$0000,$0070
         DW        $7020,$F070,$F0F8,$F878,$8870,$0888,$8088,$88F8
         DW        $F070,$F070,$F888,$8888,$8888,$F870,$8070,$2000
         DW        $0020,$0820,$0000,$0038,$1020,$4444,$00FE,$FFFE
;
         DW        $0070,$5050,$78C8,$5020,$2020,$A820,$0000,$0008      ;ROW6
         DW        $4860,$8888,$3080,$4008,$8888,$6060,$1000,$4088
         DW        $8850,$4888,$4880,$8080,$8820,$0890,$80D8,$C888
         DW        $8888,$8888,$A888,$8888,$8888,$0840,$8008,$5000
         DW        $0070,$0C20,$7070,$0044,$1070,$0000,$6C82,$FFFE
;
         DW        $0070,$50F8,$A010,$5040,$4010,$7020,$0000,$0010      ;ROW5
         DW        $4820,$0808,$50F0,$8010,$8888,$6000,$2078,$2008
         DW        $A888,$4880,$4880,$8080,$8820,$08A0,$80A8,$A888
         DW        $8888,$8840,$2088,$8888,$5050,$1040,$4008,$8800
         DW        $70A8,$0A20,$88F8,$60BA,$3820,$0000,$9282,$FFFE
;
         DW        $0020,$0050,$7020,$6000,$4010,$A8F8,$0070,$0020      ;ROW4
         DW        $4820,$7030,$9008,$F020,$7078,$0060,$4000,$1010
         DW        $B888,$7080,$48E0,$E098,$F820,$08C0,$80A8,$9888
         DW        $F088,$F020,$2088,$50A8,$2020,$2040,$2008,$0000
         DW        $FE20,$0820,$88F8,$F0A2,$38F8,$8238,$9282,$FFFE
;
         DW        $0000,$00F8,$7040,$A800,$4010,$A820,$4000,$0040      ;ROW3
         DW        $4820,$8008,$F808,$8840,$8808,$6060,$2078,$2020
         DW        $B0F8,$4880,$4880,$8088,$8820,$08A0,$8088,$8888
         DW        $80A8,$A010,$2088,$50A8,$5020,$4040,$1008,$0000
         DW        $FE20,$78A8,$88F8,$F0BA,$7C20,$4444,$6C82,$FFFE
;
         DW        $0000,$0050,$2898,$9000,$2020,$0020,$4000,$0080      ;ROW2
         DW        $4820,$8088,$1088,$8880,$8810,$6020,$1000,$4000
         DW        $8088,$4888,$4880,$8088,$8820,$8890,$8888,$8888
         DW        $8090,$9088,$2088,$20A8,$8820,$8040,$0808,$0000
         DW        $4820,$F070,$7070,$6044,$6C50,$3882,$0082,$FFFE
;
         DW        $0020,$0050,$F898,$6800,$1040,$0000,$8000,$8080      ;ROW1
         DW        $3070,$F870,$1070,$7080,$7060,$0040,$0000,$0020
         DW        $7888,$F070,$F0F8,$8078,$8870,$7088,$F888,$88F8
         DW        $8068,$8870,$2070,$2050,$8820,$F870,$0870,$00F8
         DW        $0020,$6020,$0000,$0038,$8288,$0000,$00FE,$FFFE
;
WEIRD    DW        $0011,$4130,$2110,$2031  ;TRANSLATE 8 WAYS
;
ROWTRI   DW        $0001,$0306,$0A0F,$151C,$242D  ;FOR COMPAS ROW DECODE
;
FIBTBL   DB        $08            ;QUICK ARC TAN LOOKUP
         DB        $10,$08
         DB        $10,$0B,$08
         DB        $10,$0D,$0A,$08
         DB        $10,$0E,$0B,$09,$08
         DB        $10,$0E,$0C,$0A,$09,$08
         DB        $10,$0E,$0D,$0B,$0A,$09,$08
         DB        $10,$0F,$0D,$0C,$0B,$0A,$09,$08
         DB        $10,$0F,$0E,$0C,$0B,$0A,$09,$09,$08
         DB        $10,$0F,$0E,$0D,$0C,$0B,$0A,$09,$09,$08
;
RTRIGS   DB        0,25,50,74        ;SINE TABLE- 16 ANGLES/QUADRANT
         DB        98,121,142,162    ;VALUES REPRESENT SINE*256
         DB        181,198,213,226
         DB        237,245,251,255
;
         DB        255,255,251,245
         DB        237,226,213,198
         DB        181,162,142,121
         DB        98,74,50,25
;
;  This is a table of notes (for the game melodies).
;
G2       EQU       0              ; G = 1.5 8ves below middle C
NOTES    DW        957            ; beginning frequency
GS2      EQU       1              ; G sharp (second 8ve) , etc.
         DW        903
A2       EQU       2
         DW        852
AS2      EQU       3
         DW        804
B2       EQU       4
         DW        759
C3       EQU       5
         DW        717
CS3      EQU       6
         DW        676
D3       EQU       7
         DW        638
DS3      EQU       8
         DW        603
E3       EQU       9
         DW        569
F3       EQU       $0A
         DW        537
FS3      EQU       $0B
         DW        507
G3       EQU       $0C
         DW        478
GS3      EQU       $0D
         DW        451
A3       EQU       $0E
         DW        426
AS3      EQU       $0F
         DW        402
B3       EQU       $10
         DW        380
C4       EQU       $11
         DW        358
CS4      EQU       $12
         DW        338
D4       EQU       $13
         DW        319
DS4      EQU       $14
         DW        301
E4       EQU       $15
         DW        284
F4       EQU       $16
         DW        268
FS4      EQU       $17
         DW        253
G4       EQU       $18
         DW        239
GS4      EQU       $19
         DW        226
A4       EQU       $1A
         DW        213
AS4      EQU       $1B
         DW        201
B4       EQU       $1C
         DW        190
C5       EQU       $1D
         DW        179
CS5      EQU       $1E
         DW        169
D5       EQU       $1F
         DW        160
DS5      EQU       $20
         DW        151
E5       EQU       $21
         DW        142
F5       EQU       $22
         DW        134
FS5      EQU       $23
         DW        127
G5       EQU       $24
         DW        120
GS5      EQU       $25
         DW        113
A5       EQU       $26
         DW        107
AS5      EQU       $27
         DW        101
B5       EQU       $28
         DW        95
C6       EQU       $29
         DW        90
CS6      EQU       $2A
         DW        85
D6       EQU       $2B
         DW        80
DS6      EQU       $2C
         DW        75
E6       EQU       $2D
         DW        71
F6       EQU       $2E
         DW        67
FS6      EQU       $2F
         DW        63
G6       EQU       $30
         DW        60
GS6      EQU       $31
         DW        56
A6       EQU       $32
         DW        53
AS6      EQU       $33
         DW        50
B6       EQU       $34
         DW        47
C7       EQU       $35
         DW        45
CS7      EQU       $36
         DW        42
D7       EQU       $37
         DW        40
DS7      EQU       $38
         DW        38
E7       EQU       $39
         DW        36
F7       EQU       $3A
         DW        34
FS7      EQU       $3B
         DW        32
G7       EQU       $3C
         DW        30
GS7      EQU       $3D
         DW        28
A7       EQU       $3E
         DW        27
AS7      EQU       $3F            ;zero frequency for rest
         DW        0
;
;
; This is the tune to accompany the VECTREX (?) title page.
;
VH       EQU       30
VEQ      EQU       18
TRV      EQU       06
;
;
VCTRX    DW        FADE4
         DW        VIBENL
         DB        D4 OR $80
         DB        D5,TRV+TRV
         DB        D4 OR $80
         DB        D5,TRV
         DB        G4 OR $80
         DB        D5 OR $80
         DB        G5,VH*2
         DB        C4,$80
;
; This is the data table for the beginning melody of BERZERK.
;
TR8      EQU       7              ;frame count in milleseconds (tempo)
TRQTR    EQU       14
ARPEG1   EQU       02
HA       EQU       40
;
;
BZERK    DW        FADE0,VIBE0
         DB        E5,TR8         ;eighth note triplets
         DB        E5,TR8
         DB        E5,TR8
         DB        E5,TR8
         DB        E5,TR8
         DB        E5,TR8
         DB        E5,TRQTR       ;qtr. note triplet
         DB        GS4 OR $80
         DB        D5 OR $80
         DB        G5,TRQTR       ;chord
         DB        E4 OR $80
         DB        AS4 OR $80
         DB        DS5,TRQTR      ;chord
         DB        E5,TR8
         DB        E5,TR8
         DB        E5,TR8
         DB        E5,TR8
         DB        E5,TR8
         DB        E5,TR8
;THIS AREA DELETED
         DB        C5 OR $80
         DB        FS5 OR $80
         DB        B5,TRQTR
         DB        DS5 OR $80
         DB        A5 OR $80
         DB        D6,TRQTR
         DB        F5,ARPEG1      ;final chord in arpeggio form
         DB        B5,ARPEG1
         DB        E6,ARPEG1
         DB        B5,ARPEG1
         DB        F5,ARPEG1
         DB        B5,ARPEG1
         DB        E6,ARPEG1
         DB        B5,ARPEG1
         DB        F5,ARPEG1
         DB        B5,ARPEG1
         DB        E6,ARPEG1
         DB        B5,ARPEG1
         DB        F6,ARPEG1
         DB        E6,HA
ENDZ     DB        E5,$80
;
;
FADE0    DW        $EFFF,$FEDC,$BA00,$0000
         DW        0,0,0,0
;
;
;
VIBE0    DB        0,1,2,1,0,$FF,$FE,$FF        ;-1,-2,-1
;
;
; This is the beginning snare-drum pattern for ARMOR ATTACK. It uses
; the noise channel only.
;
;
QUO      EQU       24             ;frame count in milleseconds (tempo)
EGG      EQU       12
SIP      EQU       06
SIT      EQU       04
SNAR     EQU       $10
;
;
ARMOR    DW        FADE12
         DW        VIBENL
         DB        $40 OR SNAR+1,QUO+EGG
         DB        $40 OR SNAR,SIP
         DB        $40 OR SNAR,SIP
         DB        $40 OR SNAR,EGG
         DB        $40 OR SNAR,SIP
         DB        $40 OR SNAR,SIP
         DB        $40 OR SNAR,SIT
         DB        $40 OR SNAR,SIT
         DB        $40 OR SNAR,SIT
         DB        $40 OR SNAR,QUO
         DB        $40 OR SNAR,SIT
         DB        $40 OR SNAR,SIT
         DB        $40 OR SNAR,SIT
         DB        $40 OR SNAR,EGG
         DB        $40 OR SNAR,EGG
         DB        $40 OR SNAR,QUO+EGG    ; repeat
         DB        $40 OR SNAR,SIP
         DB        $40 OR SNAR,SIP
         DB        $40 OR SNAR,EGG
         DB        $40 OR SNAR,SIP
         DB        $40 OR SNAR,SIP
         DB        $40 OR SNAR,SIT
         DB        $40 OR SNAR,SIT
         DB        $40 OR SNAR,SIT
         DB        $40 OR SNAR,QUO
         DB        $40 OR SNAR,SIT
         DB        $40 OR SNAR,SIT
         DB        $40 OR SNAR,SIT
         DB        $40 OR SNAR,EGG
         DB        $40 OR SNAR,QUO
         DB        A5,$80
;
FADE12   DW        $FDBA,$9876,$5544,$3322,$1100,0,0,0
;
; This is the data table for the beginning melody of SCRAMBLE.
;
;
QU       EQU       16             ;frame count in milleseconds (tempo)
EIT      EQU       08
SINTH    EQU       04
HAT      EQU       48
;
;
SCRMBL   DW        FADE1
         DW        VIBE0
         DB        G4 OR $80      ;first measure (qtr. and sixteenth notes)
         DB        B4,QU
         DB        AS7,EIT
         DB        G4 OR $80
         DB        B4,SINTH
         DB        G4 OR $80
         DB        B4,SINTH
         DB        G4 OR $80
         DB        B4,QU
         DB        AS7,EIT
         DB        G4 OR $80
         DB        B4,SINTH
         DB        G4 OR $80
         DB        B4,SINTH
         DB        G4 OR $80      ;all eighth notes until end of tune
         DB        B4,EIT
         DB        D4 OR $80
         DB        G4,EIT
         DB        G4 OR $80
         DB        B4,EIT
         DB        B4 OR $80
         DB        D5,EIT
         DB        G4 OR $80
         DB        B4,EIT
         DB        D4 OR $80
         DB        G4,EIT
         DB        G4 OR $80
         DB        B4,EIT
         DB        D4 OR $80
         DB        G4,EIT
         DB        G4 OR $80      ;third measure
         DB        B4,EIT
         DB        B4 OR $80
         DB        D5,EIT
         DB        G4 OR $80
         DB        B4,EIT
         DB        D4 OR $80
         DB        G4,EIT
         DB        G4 OR $80
         DB        B4,EIT
         DB        D4 OR $80
         DB        G4,EIT
         DB        G4 OR $80
         DB        B4,EIT
         DB        B4 OR $80
         DB        D5,EIT
         DB        G4 OR $80      ;last measure
         DB        B4,EIT
         DB        D4 OR $80
         DB        G4,EIT
         DB        B4 OR $80
         DB        D5,HAT
         DB        A4,$80
;
;
FADE1    DB        $FF,$FE,$DC,$BA,$98,$76,$54,$32,$10
         DB        0,0,0,0,0,0,0
;
;
; This is the data table for the beginning melody of SOLAR QUEST.
;
;
QR       EQU       24             ;frame count in milleseconds (tempo)
DOT8TH   EQU       18
SXTNTH   EQU       06
HALF     EQU       60
;
SOLAR    DW        FADE2
         DW        VIBENL
         DB        G3,QR          ;all quarter notes
         DB        C4,QR
         DB        G3,QR
         DB        C4,QR
         DB        G3,QR
         DB        C4,QR
         DB        G3,DOT8TH
         DB        G3,SXTNTH
         DB        C4,QR
         DB        C5 OR $80      ;double
         DB        E5,QR          ;stops
         DB        D5 OR $80
         DB        FS5,QR
         DB        E5 OR $80
         DB        G5,QR
         DB        FS5 OR $80
         DB        A5,QR
         DB        D5 OR $80      ;triad (5-root-3)
         DB        G5 OR $80      ;.
         DB        B5,QR
         DB        D3,DOT8TH
         DB        D3,SXTNTH
         DB        G2,HALF
ENDS     DB        G4,$80
;
;
FADE2    DB        $DE,$EF,$FE,$DC,$BA,$00,$00,$00
         DB        0,0,0,0,0,0,0,0
;
;
;  This is the data table for the beginning melody of ELECTROLUCKS.
;
;
SNTH     EQU       06
EIGHTH   EQU       12
QUR      EQU       24
DQTR     EQU       36
ARPEG    EQU       04
BIG      EQU       50
;
;
LUCKS    DW        FADE3
         DW        VIBENL
         DB        G4,SNTH
         DB        A4,SNTH
         DB        B4,EIGHTH
         DB        G4,EIGHTH
         DB        A4,EIGHTH+QUR
         DB        FS5,QUR
         DB        FS4,SNTH
         DB        G4,SNTH
         DB        A4,EIGHTH
         DB        FS4,EIGHTH
         DB        G4,EIGHTH+QUR
         DB        G5,QUR
         DB        G5 OR $80
         DB        B5,EIGHTH
         DB        FS5 OR $80
         DB        A5,EIGHTH
         DB        E5 OR $80
         DB        G5,EIGHTH
         DB        D5 OR $80
         DB        FS5,EIGHTH
         DB        C5 OR $80
         DB        E5,QUR
         DB        A4 OR $80
         DB        D5,QUR
         DB        FS4,SNTH
         DB        G4,SNTH
         DB        A4,EIGHTH
         DB        FS4,EIGHTH
         DB        G4,DQTR
         DB        G5,DQTR
ENDL     DB        G4,$80
;
;
FADE3    DB        $FF,$EE,$DD,$CC
VIBENL   DB        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
;
; THIS IS THE BEGINNING MEDLODY FOR GUESS WHAT
;
;
HAQ      EQU       50
TRIQ     EQU       30
ITA      EQU       20
ITB      EQU       10
;
;
TREK     DW        FADE4
         DW        VIBENL
         DB        F4 OR $80      ;1ST BEAT (QTR. NOTE)
         DB        A4 OR $80
         DB        C5,TRIQ
         DB        C4 OR $80      ;2ND BEAT   " "
         DB        E4 OR $80
         DB        G4,TRIQ
         DB        DS4 OR $80     ;3RD BEAT   " "
         DB        G4 OR $80
         DB        AS4,TRIQ
         DB        AS3 OR $80     ;4TH BEAT (TRIPLET)
         DB        DS4 OR $80     ;
         DB        G4,ITA         ;
         DB        F4,ITB         ;
         DB        G3 OR $80      ;5TH BEAT (TRIPLET)
         DB        C4 OR $80      ;
         DB        E4,ITA         ;
         DB        F4,ITB         ;
         DB        C4 OR $80      ;LAST CHORD (HELD)
         DB        E4 OR $80
         DB        G4,HAQ
         DB        G4,$80
;
;
FADE4    DW        $EEFF,$FFEE,$EEDD,$CCBB,$AA99,$8888,$8888,$8888
;
;
;
; Following is a series of six FANFARES set in various keys, tempi
; and styles. They are 2-4 measures long and may be used to highlight
; particular actions in the games or interrupt and celebrate (or what-
; ever) a particular scoring milestone.
;
;
;
;
; #2 is very fast in a country style
;
QRTR2    EQU       06
HAFF2    EQU       30
;
;
FFARE2   DW        FADE8
         DW        VIBENL
         DB        B4,QRTR2
         DB        D5,QRTR2
         DB        B4,QRTR2
         DB        G4,QRTR2
         DB        A4,QRTR2
         DB        G4,QRTR2
         DB        E4,QRTR2
         DB        D4,QRTR2
         DB        G4,QRTR2
         DB        D4,QRTR2
         DB        FS4,QRTR2
         DB        G4,HAFF2
         DB        G4,$80
;
;
FADE8    DW        $FFFF,$EEEE,$DDDD,$CCCC
         DW        0,0,0,0
;
;
; #3 is a traditional trumpet style fanfare
;
QRTR3    EQU       15
TRATH3   EQU       05
QHELD    EQU       50
;
;
FFARE3   DW        FADE1
         DW        VIBENL
         DB        F4,QRTR3
         DB        F4,TRATH3
         DB        F4,TRATH3
         DB        F4,TRATH3
         DB        A4,QRTR3
         DB        F4,QRTR3
         DB        C5,QRTR3
         DB        C5,TRATH3
         DB        C5,TRATH3
         DB        C5,TRATH3
         DB        E5,QRTR3
         DB        C5,QHELD
         DB        C5,$80
;
;
; FF03 is specifically for BERZERK.
;
;
Q3R      EQU       06
TH3R     EQU       02
QHOLD    EQU       50
;
;
FF03     DW        FADE1
         DW        VIBENL
         DB        F4,Q3R
         DB        F4,TH3R
         DB        F4,TH3R
         DB        F4,TH3R
         DB        A4,Q3R
         DB        F4,Q3R
         DB        C5,Q3R
         DB        C5,TH3R
         DB        C5,TH3R
         DB        C5,TH3R
         DB        E5,Q3R
         DB        C5,QHOLD
         DB        C4,$80
;
;
; #4 is a comic version of #3
;
;
Q4       EQU       15
TR4      EQU       05
H4       EQU       48
HR4      EQU       10
;
FFARE4   DW        FADE1
         DW        VIBENL
         DB        AS4,Q4
         DB        F4,TR4
         DB        F4,TR4
         DB        F4,TR4
         DB        FS4,H4
         DB        F4,TR4
         DB        F4,TR4
         DB        F4,TR4
         DB        FS4,H4
         DB        F4,$80
;
;
;
; #9 is a cutesy little riff in thirds
;
;
Q9       EQU       18
TR9      EQU       12
TR9A     EQU       06
HELD9    EQU       50
;
;
FFARE9   DW        FADE0
         DW        VIBENL
         DB        DS5 OR $80
         DB        FS5,Q9
         DB        DS5 OR $80
         DB        FS5,TR9
         DB        B4 OR $80
         DB        DS5,TR9A
         DB        CS5 OR $80
         DB        E5,Q9
         DB        B4 OR $80
         DB        DS5,HELD9
         DB        D4,$80
;
;
;
; Diminished pattern
;
;
Q16      EQU       16
S16      EQU       04
EI16     EQU       08
;
;
FFARE16  DW        FADE12
         DW        VIBENL
         DB        F4,S16
         DB        F4,S16
         DB        F4,S16
         DB        F4,S16
         DB        A4,EI16
         DB        B4,$80
;
;
;
; - - -THESE RAMS ARE FOR 'NIBBY' ONLY
KAOS     EQU       LASRAM      ;CONTROL BYTE
;
; FORMAT OF CONTROL BYTE:
;      0-$F = # VECTORS OF GIVEN TYPE (ON OR OFF)
;      OR WITH $20 = ZERO AFTER AND QUIT
;      OR WITH $80 = BEAM ON FOR STRING
;      OR WITH $10 = DO POSITION MOVE
         SETDP     $D0
NIBBY                             ;1 BYTE/VECTOR, U INDEX <----
         LDA       0,Y+           ;GET CONTROL BYTE
         STA       KAOS
         BITA      #$10
         BEQ       DONIB
MORNIB   BSR       ZXXXX
         LDD       0,Y++
         JSR       POSITN         ;NEXT POSITION VECTOR
DONIB    LDA       0,Y            ;START NEXT ACTUAL VECTOR
         ASRA                     ;CAREFUL TO SIGN EXTEND
         ANDA      #$F8           ;MASK DEPEND ON TIMES SHIFTED
         LDB       0,Y+
         ASLB
         ASLB
         ASLB
         ASLB
;
         ASRB
         ANDB      #$F8
         TST       KAOS
         BNE       NIFBY
         JSR       DUFFAB
         BRA       SIDDER         ;TO GET NEXT ITEM
;
;
NIFBY    JSR       DIFFAB
SIDDER   LDA       KAOS
         BITA      #$0F
         BEQ       DESEID
         DEC       KAOS
         BRA       DONIB          ;IF ANOTHER VECTOR
;
;
DESEID   BITA      #$20
         BEQ       NIBBY          ;GET NEXT CONTROL BYTE
ZXXXX    JMP       ZEROIT
;
;
;
;         DB        'KARRSOFT82LDMCBCJ'
         EOPROG
;
         END