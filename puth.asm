; print hex numbers

; put nibble
putn:
    mov ah,2
    and dl,0fh
    cmp dl,0ah
    jge putn0

    add dl,"0"
    int 21h
    ret
putn0:
    add dl,"A"-10
    int 21h
    ret

; put byte
putb:
    mov dl,bl : shr dl,4
    call putn
    mov dl,bl
    call putn
    ret

; put word
putw:
    mov dx,bx : shr dx,12
    call putn
    mov dx,bx : shr dx,8
    call putn
    mov dx,bx : shr dx,4
    call putn
    mov dx,bx
    call putn
    ret
