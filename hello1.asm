; print numbers

    org 0100h

_putc: equ 2
_exit: equ 4c00h

    mov bx,hellomsg
    call puts

    mov ax,73
    call putn
    mov dl,10
    call putc

    mov ax,-189
    call putn
    mov dl,10
    call putc

    mov ax,_exit
    int 21h

hellomsg:
    db "Hello, world!",10,0

putc:
    mov ah,_putc
    int 21h
    ret

putn:
    or ax,ax
    jns putn0

    mov bx,ax
    mov ah,2
    mov dl,45
    int 21h
    mov ax,bx
    neg ax

putn0:
    mov bx,sp
    mov cx,10

putn1:
    xor dx,dx
    div cx
    dec bx
    mov [bx],dl
    or ax,ax
    jnz putn1

    mov ah,2
    mov bp,sp
    mov sp,bx
putn2:
    mov dl,[bx]
    add dl,"0"
    int 21h
    inc bx
    cmp bx,bp
    jnz putn2

    mov sp,bp
    ret

puts:
    mov ah,2
puts0:
    mov dl,[bx]
    or dl,dl
    jz puts1
    int 21h
    inc bx
    jmp puts0
puts1:
    ret

