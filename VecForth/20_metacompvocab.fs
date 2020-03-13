\ 2\   gforth extensions & metacompiler vocabularies     31jan16nac
ROOT DEFINITIONS \ Expand ROOT to include..
  ' DEFINITIONS ALIAS DEFINITIONS  ' ALSO  ALIAS ALSO
  ' ONLY        ALIAS ONLY         ' BYE   ALIAS BYE
ONLY FORTH DEFINITIONS     \ extensions
1 CELLS CONSTANT CELL      \ host computer's cell size
: OFFSET ( n - ) CREATE , DOES> @ + ;
: AKA    ' CREATE , DOES> @ EXECUTE ;     \ AKA oldname newname
: THRU   1+ SWAP DO  CR I .  .S I LOAD LOOP ;   \ lo hi THRU
: DEFINED ( -- here 0 | cfa [ -1 | 1 ] ) BL WORD FIND ;
: 2+ 2 + ;
: .XTID ( xt n -- ) >R >NAME DUP 0= IF DROP S" <noname>" ELSE
  NAME>STRING THEN DUP >R TYPE R> R> SWAP - 1 MAX SPACES ;
VOCABULARY META  ALSO META DEFINITIONS    \ metacompiler
VOCABULARY ASSEMBLER                      \ cross-assembler
VOCABULARY TARGET                         \ target words
