\ Vectrex Forth Benchmark Test Words
\ From https://theultimatebenchmark.org/ (Based on the F83 versions)

decimal

: benchme ( xt n -- ) \ executes the word with the execution token "xt" n-times
  dup >r              \ save number of iterations
  0 do dup execute loop \ execute word. word must have a neutral stack effect
  cr r> . ." Iterations." cr \ emit message
;


\ INTEGER CALCULATIONS --------------------------------------------------------

32000 constant intMax
\ variable intResult

: doint
\ 1 dup intResult dup >r !  \ Original test
  1 dup pad       dup >r !  \ Can't cross compile a var, so use PAD instead
  begin
    dup intMax <
  while
    dup  negate r@ +! 1+
    dup         r@ +! 1+
    r@ @ over * r@  ! 1+
    r@ @ over / r@  ! 1+
  repeat
  r> drop drop
;
\ doint: 19.86s


\ FIBONACCI 1 -----------------------------------------------------------------

\ Cross-compiler has no host action for RECURSE, so compile via terminal

\ : fib1 ( n1 -- n2  e.g. 5 -- 8 ; 6 -- 13 ; 7 -- 21 )
\   dup  2 < if drop 1 exit then
\   dup  1-  recurse
\   swap 2 - recurse +
\ ;
\
\ : fib1-bench
\   24 1 do
\     cr i .
\     i fib1 u.
\   loop
\ ;
\ fib1-bench: 36.62s returns "23 46368" - Runs really slowly


\ FIBONACCI 2 -----------------------------------------------------------------

: fib2 ( n1 -- n2 )
  0 1 rot 0 do
    over + swap
  loop
  drop
;

: fib2-bench
  1000 0 do
    20 1 do
      ( cr i . )
      i fib2 drop ( u. )
    loop
  loop
;
\ fib1-bench: 18.40s


\ MEMMOVE BENCHMARK -----------------------------------------------------------

\ 8192 constant bufsize
\ variable buf1 here bufsize 1+ allot buf1 !
\ variable buf2 here bufsize 1+ allot buf2 !

\ Can't cross compile variables, use block of RAM instead
#4096 constant bufsize \ Use half size buffer
$5800 equ      buf1
$6800 equ      buf2    \ Avoid writing over $7FFF, VecFever UART

: test-cmove  50 0 do buf1 buf2 bufsize           cmove  loop ;
: test-cmove> 50 0 do buf2 buf1 bufsize           cmove> loop ;
: test-move>  50 0 do buf1 buf2 bufsize 1 cells /  move  loop ;
: test-<move  50 0 do buf2 buf1 bufsize 1 cells /  move  loop ;
: move-bench test-cmove test-cmove> test-move> test-<move ;
\ move-bench: 10.44s


\ FORTH NESTING (NEXT) BENCHMARK ----------------------------------------------

: bottom ;
: 1st bottom bottom ;  : 2nd 1st 1st ;      : 3rd 2nd 2nd ;
: 4th 3rd 3rd ;        : 5th 4th 4th ;      : 6th 5th 5th ;
: 7th 6th 6th ;        : 8th 7th 7th ;      : 9th 8th 8th ;
: 10th 9th 9th ;       : 11th 10th 10th ;   : 12th 11th 11th ;
: 13th 12th 12th ;     : 14th 13th 13th ;   : 15th 14th 14th ;
: 16th 15th 15th ;     : 17th 16th 16th ;   : 18th 17th 17th ;
: 19th 18th 18th ;     : 20th 19th 19th ;   : 21th 20th 20th ;
: 22th 21th 21th ;     : 23th 22th 22th ;   : 24th 23th 23th ;
: 25th 24th 24th ;

: 32million   cr ." 32 million nest/unnest operations" 25th ;
:  1million   cr ."  1 million nest/unnest operations" 20th ;
\ 1million: 1m06.12s


\ GREATEST COMMON DIVISOR 1 ---------------------------------------------------

: gcd ( a b -- gcd )
  over if
    begin
      dup
    while
      2dup u> if swap then
      over -
    repeat
    drop
  else
    dup if nip else 2drop 1 then
  then ;

: gcd1-bench
  100 0 do
    100 0 do
      j i gcd drop
    loop
  loop ;
\ gcd: 28.09s


\ SIEVE BENCHMARK -- THE CLASSIC FORTH BENCHMARK ------------------------------

\ SEEMS TO STACK UNDERFLOW AT THE END, NEEDS DEBUG ****************************

\ $5ffe equ flags
\ \ 8192 equ size
\ \ 8192 constant size
\ 10 constant size
\ \ variable flags
\  \ 0 flags !
\  \ size allot
\
\  : do-prime
\    flags size 2 + 1 fill
\    0 size 0 do
\      flags i + c@ if
\        i dup + 3 + dup i +
\        begin
\          dup size <
\        while
\          0 over flags + c! over +
\        repeat
\        drop drop 1+
\      then
\    cr .s ." 1"
\    loop
\ \   . ." Primes" cr
\ ;
