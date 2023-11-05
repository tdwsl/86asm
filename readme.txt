86asm - tdwsl 2023

86asm is an 8086 assembler written in Forth. It's not a complete assembler,
and may have some bugs, but it can assemble some simple examples. It has been
tested and works with Gforth, as well as my own Aster Forth.

Implemented assembler directives are:
include  - include an assembly file
incbin   - include a binary file
db       - declare a string of bytes
dw       - declare a string of words
org      - set pc address for labels and instructions
equ      - redefine last label to a value

