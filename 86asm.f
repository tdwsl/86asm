\ 8086 assembler in forth

argc @ 3 <> [if]
  ." usage: 86asm.f <file.asm> <output>" cr
  bye
[then]

variable line-no
0 line-no !

200 constant line-sz
0 value line
0 value line-len
0 value (line)
0 value line-1
0 value line-len-1

: 'line
  line line-len line-1 line-len-1
  to line-len to line to line-len-1 to line-1 ;

: line! to line-len to line ;

: line@ line line-len ;

100 constant buf-sz
create buf buf-sz allot
0 value buf-len

variable org
0 org !

2variable filename

begin-structure label
  2field: label.name
  field: label.value
end-structure

1200 constant max-labels

create labels label max-labels * allot
0 value nlabels

create label-buf 40960 allot
label-buf value label-p

create memory 64 1024 * allot
variable memp
memory memp !

: label label * labels + ;

: save-string ( str len -- str len )
  tuck label-p swap move label-p swap
  2dup + to label-p ;

: add-label ( u str len -- )
  save-string
  nlabels label >r
  r@ label.name 2!
  r> label.value !
  nlabels 1+ to nlabels ;

: find-label-value ( str len -- 0 | u -1 )
  nlabels 0 ?do
    2dup i label label.name 2@ str= if
      2drop i label label.value @ -1 unloop exit
    then
  loop
  2drop 0 ;

begin-structure exp
  2field: exp.str
  2field: exp.filename
  field: exp.org
  field: exp.mem
  field: exp.line
  cfield: exp.type
end-structure

create expressions exp 1800 * allot
0 value nexpressions

: expression exp * expressions + ;

: add-expression ( type str len -- )
  save-string
  nexpressions expression >r
  r@ exp.str 2!
  filename 2@ r@ exp.filename 2!
  org @ r@ exp.org !
  memp @ r@ exp.mem !
  line-no @ r@ exp.line !
  r> exp.type c!
  nexpressions 1+ to nexpressions ;

create regs16
s" AX" 2, s" CX" 2, s" DX" 2, s" BX" 2,
s" SP" 2, s" BP" 2, s" SI" 2, s" DI" 2,
0 ,

regs16 value regs16b

create regs8
s" AL" 2, s" CL" 2, s" DL" 2, s" BL" 2,
s" AH" 2, s" CH" 2, s" DH" 2, s" BH" 2,
0 ,

create regsn
s" BX" 2, s" BP" 2,
create regsi
s" SI" 2, s" DI" 2,
0 ,

create regsb
s" BX" 2, s" BP" 2,
0 ,

create regss
s" CS" 2, s" DS" c, s" ES" c, s" SS" c,
0 ,

create branches
s" JO" 2, s" JNO" 2, s" JB" 2, s" JNB" 2,
s" JZ" 2, s" JNZ" 2, s" JBE" 2, s" JA" 2,
s" JS" 2, s" JNS" 2, s" JPE" 2, s" JPO" 2,
s" JL" 2, s" JGE" 2, s" JLE" 2, s" JG" 2,

s" nn" 2, s" nnn" 2, s" JNAE" 2, s" nnn" 2,
s" JE" 2, s" JNE" 2, s" JNA" 2, s" JNBE" 2,
s" nn" 2, s" nnn" 2, s" JP" 2, s" JNP" 2,
s" JNGE" 2, s" JNL" 2, s" JNG" 2, s" JNLE" 2,
0 ,

create delim
char ( c, char ) c, char [ c, char ] c, char , c, char + c,
char - c, char * c, char / c, char % c, char " c, char ' c,
char : c, char ; c,
delim here delim - 2constant delim

: strchr ( c str len -- )
  0 ?do 2dup c@ = if nip unloop exit then 1+ loop
  2drop 0 ;

: strindex ( str len a -- n )
  dup 2>r begin
    2dup r@ 2@ str= if 2drop r> r> - 2 cells / exit then
    r> cell+ cell+ >r
  r@ @ 0= until
  2drop 2r> 2drop -1 ;

: strin? strindex -1 <> ;

: exit-on-eol line-len 0<= if r> drop 0 0 then ;

: parse-next ( -- str len )
  exit-on-eol
  ( skip leading whitespace )
  begin line c@ 32 <= while
    line 1+ to line line-len 1- to line-len
    exit-on-eol
  repeat
  ( check for delimeter )
  line c@ delim strchr if
    line 1
    line 1+ to line line-len 1- to line-len
    exit
  then
  ( parse until whitespace, delimeter or eol )
  0 begin dup line + c@ dup 32 > swap delim strchr 0= and over line-len < and
  while 1+ repeat

  line swap
  dup line + to line
  line-len over - to line-len ;

: upper ( str len -- str len )
  2dup 0 ?do
  dup c@ [char] a >= over c@ [char] z <= and if
    dup c@ [ char A char a - ]l + over c!
  then 1+ loop drop ;

: parse-next parse-next upper ;

: look-ahead ( -- str len )
  line line-len 2>r parse-next 2r> to line-len to line ;

: skip ( str len -- )
  dup line + to line
  negate line-len + to line-len
  drop ;

: skip-next parse-next 2drop ;

: back ( str len -- )
  drop dup negate line + line-len + to line-len
  to line ;

: .error
  filename 2@ type [char] : emit line-no @ 0 .r [char] : emit space ;

: hton ( str len -- -1 | n 0 )
  ?dup 0= if drop -1 exit then
  0 swap 0 do
    over c@ dup [char] 0 >= swap [char] 9 <= and if
      4 lshift over c@ [char] 0 - +
    else over c@ dup [char] A >= swap [char] F <= and if
      4 lshift over c@ [ 10 char A - ]l + +
    else 2drop unloop -1 exit then then
  swap 1+ swap loop nip 0 ;

: ton ( str len -- -1 | n 0 )
  ?dup 0= if drop -1 exit then
  2dup + 1- c@ [char] H = if 1- hton exit then
  0 swap 0 do
    over c@ dup [char] 0 >= swap [char] 9 <= and if
      10 * over c@ [char] 0 - +
    else 2drop unloop -1 exit then
  swap 1+ swap loop nip 0 ;

defer expr
defer -expr

: expect ( str len -- )
  2dup parse-next str= 0= if
    .error ." expected " type cr bye
  else 2drop then ;

: atom ( -- n )
  parse-next 2dup s" -" str= if 2drop parse-next ton >r negate r>
  else 2dup s" (" str= if 2drop expr 0 s" )" expect
  else 2dup ton if find-label-value 0= else nip nip 0 then
  then then
  if .error ." expected value" cr bye then ;

: -atom
  parse-next 2dup s" -" str= if parse-next 2drop
  else 2dup s" (" str= if -expr s" )" expect
  then then 2drop ;

: term ( -- n )
  atom look-ahead 2dup s" *" str= if skip atom *
  else 2dup s" /" str= if skip atom /
  else 2dup s" %" str= if skip atom mod
  else 2drop then then then ;

: -term
  -atom look-ahead 2dup s" *" str= if skip -atom
  else 2dup s" /" str= if skip -atom
  else 2dup s" %" str= if skip -atom
  else 2drop then then then ;

:noname ( -- n )
  term look-ahead 2dup s" +" str= if skip term +
  else 2dup s" -" str= if skip term -
  else 2drop then then ; is expr

:noname
  -term look-ahead 2dup s" +" str= if skip -term
  else 2dup s" -" str= if skip -term
  else 2drop then then ; is -expr

\ specials are: R=reg, R8, R16, INDEX, BASE, STRING, #, *
\ (# is expression, * is all)
: (match) ( str len str len -- tf )
  2dup s" *"      str= if 2drop back -expr -1 exit then
  2dup s" #"      str= if 2drop
    2dup s" [" str= if 2drop 0 exit then
    2dup regs16 strin? if 2drop 0 exit then
    2dup regs8 strin? if 2drop 0 exit then
    back -expr -1 exit then
  2dup s" R"      str= if 2drop
    2dup regs8 strin? -rot regs16 strin? or exit then
  2dup s" R8"     str= if 2drop regs8 strin? exit then
  2dup s" R16"    str= if 2drop regs16 strin? exit then
  2dup s" INDEX"  str= if 2drop regsn strin? exit then
  2dup s" BASE"   str= if 2drop regsn strindex dup 0>= swap 2 < and exit then
  2dup s" STRING" str= if 2drop regsi strin? exit then
  2dup s" SEG"    str= if 2drop regss strin? exit then
  str= ;

: match ( str len -- tf )
  line line-len 2>r
  'line line! 'line
  begin parse-next 'line parse-next 'line dup while
    (match) 0= if 2r> line! 0 exit then
  repeat
  2drop 2drop 2r> line! -1 ;

: match [char] ? word count save-string postpone 2literal postpone match ;
  immediate

: shimmy ( str len i -- )
  >r 1- r> do dup i + 1+ c@ over i + c! loop drop ;

: parse-string ( -- str len )
  0 1 skip
  0 begin
    dup line + c@ case
    [char] \ of
      line line-len shimmy line-len 1- to line-len
      dup line + c@ case
      [char] n of 10 endof
      [char] t of 9 endof
      [char] " of [char] " endof
      [char] \ of [char] \ endof
      .error ." unknown escape character " dup line + c@ emit cr bye
      endcase
      dup line + c!
    endof
    [char] " of
      line swap 2dup 1+ skip exit
    endof
    endcase
  1+ dup line-len >= until
  .error ." expected " [char] " emit cr bye ;

: b, memp @ c! 1 memp +! 1 org +! ;
: w, dup memp @ c! 8 rshift memp @ 1+ c! 2 memp +! 2 org +! ;
: w! >r dup r@ c! 8 rshift r> 1+ c! ;

: defer-expr ( type -- )
  dup
  line@ -expr line-len -
  add-expression
  2/ 1+ dup org +! memp +! ;

: eol? ?dup 0= if drop -1 else s" :" str= then ;

here char " c, 1 2constant quote
: db
  begin
    look-ahead 2dup quote str= if
      skip parse-string dup >r
      memp @ swap move
      r@ org +! r> memp +!
    else 2drop 0 defer-expr then
    look-ahead eol? if exit then
    s" ," expect
  again ;

: _ skip-next ;
: r8  parse-next regs8 strindex
  dup -1 = if .error ." expected 8 bit register" cr bye then ;
: r16 parse-next regs16b strindex
  dup -1 = if .error ." expected 16 bit register" cr bye then ;
: seg parse-next regss strindex
  dup -1 = if .error ." expected segment register" cr bye then ;
: s parse-next s" DI" str= negate ;
: b parse-next s" BP" str= negate ;
: #16 2 defer-expr ;
: @16 3 defer-expr ;
: #8  0 defer-expr ;
: @8  1 defer-expr ;
: { r> line@ 2>r >r -expr ;
: }16 r> line@ 2r> line! 2 defer-expr line! >r ;
: }8  r> line@ 2r> line! 0 defer-expr line! >r ;
defer r
defer #
: 8! ['] r8 is r ['] #8 is # ;
: 16! ['] r16 is r ['] #16 is # ;

: (r8/16) ( u -- u )
  parse-next 2dup regs16 strindex dup -1 <> if nip nip
    swap 1+ 16!
  else
    drop regs8 strindex 8! swap
  then b, ;

: r8/16 (r8/16) 3 lshift ;

: byte/word?
  match byte? if _ 8! -1 exit then
  match word? if _ 16! 1+ -1 exit then
  0 ;

: byte/word
  byte/word? 0= if
    .error ." must specify operand size" cr bye
  then ;

: add ( u -- )
  match r,r? if
    (r8/16) $c0 + _ r 8 * + b, exit then
  match [bp],r? if
    _ _ _ _ r8/16 $46 + b, 0 b, exit then
  match [bx],r? if
    _ _ _ _ r8/16 $07 + b, exit then
  match [string],r? if
    _ s swap _ _ r8/16 $04 + + b, exit then
  match [base+string],r? if
    _ b 2* _ s + swap _ _ r8/16 + b, exit then
  match [base+string+#],r? if
    _ b 2* _ s + swap _ { _ _ r8/16 $40 + + b, }8 exit then
  match [bp+#],r? if
    _ _ _ { _ _ r8/16 $46 + b, }8 exit then
  match [bx+#],r? if
    _ _ _ { _ _ r8/16 $47 + b, }8 exit then
  match [#],r? if
    _ { _ _ r8/16 $06 + b, }16 exit then
  match #[base+string],r? if
    { _ b 2* _ s + swap _ _ r8/16 $80 + + b, }16 exit then
  match #[bp],r? if
    { _ _ _ _ r8/16 $86 + b, }16 exit then
  match #[bx],r? if
    { _ _ _ _ r8/16 $87 + b, }16 exit then

  2 +
  match r,[bp]? if
    r8/16 $46 + b, 0 b, _ _ _ _ exit then
  match r,[bx]? if
    r8/16 $07 + b, _ _ _ _ exit then
  match r,[string]? if
    r8/16 $04 + _ _ s + b, _ exit then
  match r,[base+string]? if
    r8/16 _ _ b 2* _ s + b, _ exit then
  match r,[base+string+#]? if
    r8/16 _ _ b 2* _ s + + $40 + b, _ #8 _ exit then
  match r,[bp+#]? if
    r8/16 $46 + b, _ _ _ _ #8 _ exit then
  match r,[bx+#]? if
    r8/16 $47 + b, _ _ _ _ #8 _ exit then
  match r,[#]? if
    r8/16 $06 + b, _ _ #16 _ exit then
  match r,#[base+string]? if
    r8/16 _ { _ b 2* _ s + + $80 + b, _ }16 exit then
  match r,#[bp]? if
    r8/16 $86 + b, _ #16 _ _ _ exit then
  match r,#[bx]? if
    r8/16 $87 + b, _ #16 _ _ _ exit then
  2 -

  match al,#? if
    4 + b, _ _ #8 exit then
  match ax,#? if
    5 + b, _ _ #16 exit then

  dup $88 = if drop $00 $c6
  else dup $80 + then

  match r,#? if
    (r8/16) $c0 + or b, _ # exit then

  byte/word? if
  b,
  \ match r,#? if
    \ r 8 * $c0 + or b, _ # exit then
  match [bp],#? if
    $46 or b, 0 b, _ _ _ _ # exit then
  match [string],#? if
    _ s $04 + or b, _ _ # exit then
  match [base+string],#? if
    _ b 2* _ s + or b, _ _ # exit then
  match [base+string+#],#? if
    _ b 2* _ s + $40 + or b, _ #8 _ _ # exit then
  match [bp+#],#? if
    $46 or b, _ _ _ #8 _ _ # exit then
  match [bx+#],r? if
    $47 or b, _ _ _ #8 _ _ # exit then
  match [#],r? if
    $06 or b, _ #16 _ _ # exit then
  match #[base+string],#? if
    { _ b 2* _ s $80 + + or b, }16 _ _ # exit then
  match #[bp],r? if
    $86 or b, #16 _ _ _ _ # exit then
  match #[bx],r? if
    $87 or b, #16 _ _ _ _ # exit then
  then

  .error ." invalid instruction format" cr bye ;

: mov
  match al,[#]? if
    $a0 b, _ _ _ #16 _ exit then
  match ax,[#]? if
    $a1 b, _ _ _ #16 _ exit then
  match [#],al? if
    $a2 b, _ #16 _ _ _ exit then
  match [#],ax? if
    $a3 b, _ #16 _ _ _ exit then
  match r8,[#]? if
    $b0 r8 + b, _ _ #16 _ exit then
  match r16,[#]? if
    $b8 r16 + b, _ _ #16 _ exit then
  match byte [*],#? if
    $88 add exit then
  match word [*],#? if
    $88 add exit then
  match r8,#? if
    $b0 r8 + b, _ #8 exit then
  match r16,#? if
    $b8 r16 + b, _ #16 exit then

  match [*],seg? match seg,[*]? or if
    regss to regs16b
    $8b add
    regs16 to regs16b
    exit then

  $88 add ;

: r/m-1 ( u -- )
  match r? if
    r8/16 $d0 + or b, exit then

  byte/word b,

  match [bp]? if
    _ _ _ $46 or b, $00 b, exit then
  match [bx]? if
    _ _ _ $07 or b, exit then
  match [string]? if
    _ $04 s + or b, _ exit then
  match [base+string]? if
    _ b 2* _ s + _ $00 + or b, exit then
  match [base+string+#]? if
    _ b 2* _ s + _ $40 + or b, #8 _ exit then
  match [bp+#]? if
    _ b $46 + or b, _ #8 _ exit then
  match [bx+#]? if
    _ b $47 + or b, _ #8 _ exit then
  match [#]? if
    $06 or b, _ #16 _ exit then
  match #[base+string]? if
    { _ b 2* _ s + $80 + or b, _ }16 exit then
  match #[bp]? if
    $86 or b, #16 _ _ _ exit then
  match #[bx]? if
    $87 or b, #16 _ _ _ exit then

  .error ." invalid instruction format" cr bye ;

: inc
  match r16? if $40 r16 + b,
  else $00 $fe r/m-1 then ;

: dec
  match r16? if $48 r16 + b,
  else $08 $fe r/m-1 then ;

: jmp
  $e9 b, @16 ;

: pop
  match r16? if $58 r16 + b, exit then
  match seg? if $07 seg + b, exit then
  $00 $8f r/m-1 ;

: push
  match r16? if $50 r16 + b, exit then
  match seg? if $06 seg + b, exit then
  $30 $ff r/m-1 ;

: eol parse-next dup -rot s" :" str= if 2drop drop
  else if .error ." expected : or EOL" cr bye then then ;

: eq" r> postpone 2dup postpone s" postpone str=
  postpone if postpone 2drop >r ; immediate
: /eq r> postpone eol postpone exit postpone then >r ; immediate

: asm-part
  eq" :" exit then
  eq" ;" r> drop exit then

  eq" ORG"  expr org ! /eq
  eq" MOV"  mov /eq
  eq" ADD"  $00 add /eq
  eq" ADC"  $10 add /eq
  eq" SUB"  $28 add /eq
  eq" CMP"  $38 add /eq
  eq" AND"  $20 add /eq
  eq" OR"   $08 add /eq
  eq" XOR"  $30 add /eq
  eq" CALL" $e8 b, @16 /eq
  eq" INT"  $cd b, #8 /eq
  eq" DB"   db /eq
  eq" JMP"  jmp /eq
  eq" INC"  inc /eq
  eq" DEC"  dec /eq
  eq" DIV"  $30 $f6 r/m-1 /eq
  eq" IDIV" $38 $f6 r/m-1 /eq
  eq" MUL"  $20 $f6 r/m-1 /eq
  eq" IMUL" $28 $f6 r/m-1 /eq
  eq" NEG"  $18 $f6 r/m-1 /eq
  eq" NOT"  $10 $f6 r/m-1 /eq
  eq" PUSH" push /eq
  eq" POP"  pop /eq
  eq" RET"  $c3 b, /eq

  2dup branches strindex 1+ ?dup if 1- $f and $70 + b, 2drop @8 eol exit then

  2dup org @ -rot add-label parse-next s" :" str= 0= if
    .error ." expected : after label " type cr bye
  else 2drop then ;

: asm-line
  \ line@ type cr
  begin parse-next ?dup while
    asm-part
  repeat drop ;

: enter-file ( str len -- )
  r> -rot
  (line) >r
  line-no @ >r filename 2@ 2>r
  2dup filename 2!
  0 line-no !
  r/o open-file throw >r
  here to (line) line-sz allot
  >r ;

: leave-file
  r>
  r> close-file throw
  line-sz negate allot
  2r> filename 2! r> line-no !
  r> to (line)
  >r ;

: read-next-line
  r>
  (line) line-sz r@ read-line throw
  swap to line-len
  (line) to line
  1 line-no +!
  swap >r ;

: asm-file ( str len -- )
  enter-file
  begin
    read-next-line
  while
    asm-line
  repeat
  leave-file ;

: .labels
  hex
  nlabels 0 ?do
    i label label.value @ 8 .r space
    i label label.name 2@ type cr
  loop
  decimal ;

: resolve-expressions
  nexpressions 0 ?do
    i expression >r
    r@ exp.line @ line-no !
    r@ exp.filename 2@ filename 2!

    r@ exp.str 2@ line!
    expr
    r@ exp.type c@ case
    0 of r@ exp.mem @ c! endof
    1 of r@ exp.org @ 1+ - r@ exp.mem @ c! endof
    2 of r@ exp.mem @ w! endof
    3 of r@ exp.org @ 2 + - r@ exp.mem @ w! endof
      .error ." invalid expression type" cr bye
    endcase
    r> drop
  loop ;

: save-file ( str len -- )
  w/o open-file throw >r
  memory memp @ memory - r@ write-file throw
  r> close-file throw ;

: report
  ." assembled " memp @ memory - . ." bytes" cr ;

1 arg asm-file
.labels
resolve-expressions
2 arg save-file
report
bye
