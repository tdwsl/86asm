; hex dump

    org 100h

    mov si,100h
    call hexd

    mov ax,4c00h
    int 21h

hexd:
    cld
    mov cx,20*22
hexd0:
    lodsb
    mov bl,al
    call putb
    mov dl,32
    int 21h
    int 21h
    loop hexd0
    ret

include "puth.asm"
