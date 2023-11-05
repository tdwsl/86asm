; test incbin

    org 100h

    mov si,text
    call puts
    mov ax,4c00h
    int 21h
puts:
    cld:mov ah,2
puts0:
    lodsb:or al,al
    jz puts1
    mov dl,al:int 21h
    jmp puts0
puts1:
    ret
text:
    incbin "hello3.asm"
    db 0
