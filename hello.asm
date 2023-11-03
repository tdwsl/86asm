; hello world for dos

    org 0100h

    mov bx,hellomsg
    call puts

    mov ax,4c00h
    int 21h

hellomsg:
    db "Hello, world!",10,0

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

