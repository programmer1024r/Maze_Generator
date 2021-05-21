IDEAL
MODEL small
STACK 100h
DATASEG
; --------------------------
offset_list dw 9 dup (0)  ; mabey add more for your comfort 
libraryS db 's1.bmp',0
libraryB db 'bg1.bmp',0
libraryG db 'sg1.bmp',0 
spaceS db 's2.bmp',0
spaceB db 'bg2.bmp',0
spaceG db 'sg2.bmp',0
templeS db 's3.bmp',0
templeB db 'bg3.bmp',0
templeG db 'sg3.bmp',0
; --------------------------
CODESEG



; ,gggggggggggg,                                   
; dP"""88""""""Y8b,                I8               
; Yb,  88       `8b,               I8               
;  `"  88        `8b            88888888            
;      88         Y8               I8               
;      88         d8   ,gggg,gg    I8      ,gggg,gg 
;      88        ,8P  dP"  "Y8I    I8     dP"  "Y8I 
;      88       ,8P' i8'    ,8I   ,I8,   i8'    ,8I 
;      88______,dP' ,d8,   ,d8b, ,d88b, ,d8,   ,d8b,
;     888888888P"   P"Y8888P"`Y888P""Y88P"Y8888P"`Y8
;-----------------------------------------------
; Use: finds all file names offsets
; Input: offset_list(+6), first offset libraryS(+4)
; Output:None - update a list of offsets of the file names 
proc fileOffsets
push bp 
mov bp, sp 
push bx
push ax 
push cx
push si
    
    mov bx, [bp + 4] ; offset libraryS
    mov si, [bp + 6] ; offset_list
    ; Update first offset
    mov [si], bx
    ; nine pictures
    mov ax, si
    add si, 2
    ; this is the end of the list
    add ax, 9*2 
    offset_loop:
    ; if there is null it is the end string
        cmp [byte ptr bx], 0 
        jne notEndString
            ; the next byte is going to be the start of the next string
            inc bx 
            ; save the string offset and update si to point to the next cell
            mov [si], bx 
            add si, 2
        notEndString:
        inc bx 
    cmp ax, si
    jne offset_loop

pop si
pop cx
pop ax 
pop bx 
pop bp 
ret 4
endp fileOffsets


; Main
start:
	mov ax, @data
	mov ds, ax
; --------------------------
push offset offset_list
push offset libraryS
call fileOffsets
; --------------------------
	
exit:
	mov ax, 4c00h
	int 21h
END start


