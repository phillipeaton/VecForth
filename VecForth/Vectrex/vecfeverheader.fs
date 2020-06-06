HEX

\ 10 equ v4eRED
\ 20 equ v4eGREEN
\ 40 equ v4eBLUE
\ 30 equ v4eYELLOW
\ 50 equ v4eMAGENTA
\ 60 equ v4eCYAN
\ 70 equ v4eWHITE

\ ***************************************************************************
\  MAGIC CARTHEADER SECTION
\       DO NOT CHANGE THIS STRUCT
\ ***************************************************************************

30 ORG

\ T   h     G     S
54 C, 68 C, 47 C, 53 C,             \ magic handshake marker
HERE EQU v4ecartversion     0001 ,  \ I always have a version
                                    \ in comm. structs
\ $800 KILLS TEXT \ HERE EQU v4ecartflags       18F0 ,  \ v4e flags:
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
