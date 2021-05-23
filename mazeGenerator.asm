; ------------------------------------------------------------------------------------------------------------                                                                                                                                                            
;  ,ggg, ,ggg,_,ggg,                                          ,gg,                                                                                             
; dP""Y8dP""Y88P""Y8b                                        i8""8i                                                               I8                           
; Yb, `88'  `88'  `88                                        `8,,8'                                                               I8                           
;  `"  88    88    88                                         `Y88aaad8                                                        88888888                        
;      88    88    88                                          d8""""Y8,                                                          I8                           
;      88    88    88    ,gggg,gg     ,gggg,   ,ggg,          ,8P     8b  ,ggg,    ,ggg,,ggg,    ,ggg,    ,gggggg,    ,gggg,gg    I8      ,ggggg,     ,gggggg, 
;      88    88    88   dP"  "Y8I    d8"  Yb  i8" "8i         dP      Y8 i8" "8i  ,8" "8P" "8,  i8" "8i   dP""""8I   dP"  "Y8I    I8     dP"  "Y8ggg  dP""""8I 
;      88    88    88  i8'    ,8I   dP    dP  I8, ,8I     _ ,dP'      I8 I8, ,8I  I8   8I   8I  I8, ,8I  ,8'    8I  i8'    ,8I   ,I8,   i8'    ,8I   ,8'    8I 
;      88    88    Y8,,d8,   ,d8b,,dP  ,adP'  `YbadP'     "888,,_____,dP `YbadP' ,dP   8I   Yb, `YbadP' ,dP     Y8,,d8,   ,d8b, ,d88b, ,d8,   ,d8'  ,dP     Y8,
;      88    88    `Y8P"Y8888P"`Y88"   ""Y8d8888P"Y888    a8P"Y888888P" 888P"Y8888P'   8I   `Y8888P"Y8888P      `Y8P"Y8888P"`Y888P""Y88P"Y8888P"    8P      `Y8
;                                       ,d8I'                                                                                                                  
;                                     ,dP'8I                                                                                                                   
;                                    ,8"  8I                                                                                                                   
;                                    I8   8I                                                                                                                   
;                                    `8, ,8I                                                                                                                   
;                                     `Y8P"                                                                                                                                                                                                                                                  I8                           
; -------------------------------------------------------------------------------------------------------------
IDEAL
MODEL small
STACK 100h
DATASEG


;--------file--------
filehandle dw ?
Header db 54 dup (0)
Palette db 256*4 dup (0)
ScrLine db 320 dup (0)
ErrorMsg db 'Error', 13, 10,'$'
;-------------------
;-------images------
secret db 'ss.bmp', 0
libraryS db 's1.bmp',0
libraryB db 'bg1.bmp',0
libraryG db 'sg1.bmp',0 
spaceS db 's2.bmp',0
spaceB db 'bg2.bmp',0
spaceG db 'sg2.bmp',0
templeS db 's3.bmp',0
templeB db 'bg3.bmp',0
templeG db 'sg3.bmp',0
;-------------------
;------flags------
is_maze dw 0
;----------------- 
;-----pointers----
start_pointer dw offset templeS
bg_pointer dw offset templeB
graphics_pointer dw offset templeG 
stack_pointer dw offset stackC 
graphics_args dw 9 dup (0), offset start_pointer, offset bg_pointer, offset graphics_pointer

run_args dw offset bg_pointer,offset Xn, offset stackC, offset board, offset neighbors_list, offset stack_pointer,  offset current_cell, offset maze_end, offset maze_start, offset is_maze, offset secret, offset maze_start, offset maze_end
;-----------------
;-------variables------
maze_start dw 0   ; works
maze_end dw 399
current_cell dw  0 ;0-399 ; previews cell
Xn dw 0
;----------------------
;------data structions-----
dw 399 dup (0)
stackC dw 0 
neighbors_list dw 4 dup (0)
board dw 800 dup (0) 
;--------------------------
CODESEG

; Consts colors
GREEN equ 2
WHITE equ 0fh
RED equ 4
GRAY equ 7








                                        
;  ,dPYb,        ,dPYb,                   
;  IP'`Yb        IP'`Yb                   
;  I8  8I   gg   I8  8I                   
;  I8  8'   ""   I8  8'                   
;  I8 dP    gg   I8 dP   ,ggg,     ,g,    
;  I8dP     88   I8dP   i8" "8i   ,8'8,   
;  I8P      88   I8P    I8, ,8I  ,8'  Yb  
; ,d8b,_  _,88,_,d8b,_  `YbadP' ,8'_   8) 
; PI8"88888P""Y88P'"Y88888P"Y888P' "YY8P8P
;  I8 `8,                                 
;  I8  `8,                                
;  I8   8I                                
;  I8   8I                                
;  I8, ,8'                                
;   "Y8P'                                 
;___________________________________________________________Files________________________________________________________________
;--------------------------------
; use: open a given file name print erro in case of one 
; Input: file name
; Output: None
proc openFile
push bp
mov bp,sp

; Open file
	mov 	ah, 3Dh
	xor 	al, al
	mov 	dx, [bp+4]
	int 21h
	jc 		openerror ;if there is en error, close the procdure with an error messege.
	mov 	[filehandle], ax

pop bp
ret 2

openerror:
	mov dx, offset ErrorMsg
	mov ah, 9h
	int 21h

pop bp
ret 2
endp openFile
;--------------------------------

;--------------------------------
proc readHeader
    ; Read BMP file header, 54 bytes
    mov ah,3fh
    mov bx, [filehandle]
    mov cx,54
    mov dx,offset Header
    int 21h
ret
endp readHeader
;--------------------------------

;--------------------------------
proc readPalette
    ; Read BMP file color palette, 256 colors * 4 bytes (400h)
    mov ah,3fh
    mov cx,400h
    mov dx,offset Palette
    int 21h
ret
endp readPalette
;--------------------------------

;--------------------------------
proc copyPal

    ; Copy the colors palette to the video memory
    ; The number of the first color should be sent to port 3C8h
    ; The palette is sent to port 3C9h
    mov si,offset Palette
    mov cx,256
    mov dx,3C8h
    mov al,0
    ; Copy starting color to port 3C8h
    out dx,al
    ; Copy palette itself to port 3C9h
    inc dx
    PalLoop:
    ; Note: Colors in a BMP file are saved as BGR values rather than RGB.
    mov al,[si+2] ; Get red value.
    shr al,2 ; Max. is 255, but video palette maximal
    ; value is 63. Therefore dividing by 4.
    out dx,al ; Send it.
    mov al,[si+1] ; Get green value.
    shr al,2
    out dx,al ; Send it.
    mov al,[si] ; Get blue value.
    shr al,2
    out dx,al ; Send it.
    add si,4 ; Point to next color.
    ; (There is a null chr. after every color.)

loop PalLoop
ret
endp copyPal
;--------------------------------

;--------------------------------
proc copyBitmap

    ; BMP graphics are saved upside-down.
    ; Read the graphic line by line (200 lines in VGA format),
    ; displaying the lines from bottom to top.
    mov ax, 0A000h
    mov es, ax
    mov cx,200
    PrintBMPLoop:
    push cx
    ; di = cx*320, point to the correct screen line
    mov di,cx
    shl cx,6    ;*64
    shl di,8    ;*256  ;       256+64 = 320
    add di,cx
    ; Read one line to variable ScrLine (buffer)
    mov ah,3fh  
    mov cx,320
    mov dx,offset ScrLine
    int 21h
    ; Copy one line into video memory
    cld ; Clear direction flag, for movsb for inc si, inc di
    mov cx,320
    mov si,offset ScrLine

    sub di, 320
    rep movsb ; Copy line to the screen
    ;rep movsb is same as the following code:
    ;mov es:di, ds:si
    ;inc si
    ;inc di
    ;dec cx
    ;loop until cx=0
    ; call delay
    pop cx
    loop PrintBMPLoop

ret
endp copyBitmap
;--------------------------------

;--------------------------------
proc CloseFile
mov ah,3Eh
mov bx, [filehandle]
int 21h
ret
endp CloseFile
;-------------------------------
; use: print BMP file on the screen 
; Input: file name(+4)
; Output: None
proc printBMP
push bp 
mov bp, sp 
; open file black box 
push si
push cx
push ax 
push dx 
push bx 
push di 

    ; Process BMP file
    push [bp + 4] 
    call openFile
    call readHeader
    call readPalette
    call copyPal
    call copyBitmap
    call CloseFile
  
    
pop di 
pop bx
pop dx
pop ax 
pop cx 
pop si 
pop bp

ret 2
endp printBMP 
;--------------------------------
;________________________________________________________________________________________________________________________________





                                                                                   
;                                                 ,dPYb,                              
;                                                 IP'`Yb                              
;                                                 I8  8I      gg                      
;                                                 I8  8'      ""                      
;    ,gggg,gg   ,gggggg,    ,gggg,gg  gg,gggg,    I8 dPgg,    gg     ,gggg,    ,g,    
;   dP"  "Y8I   dP""""8I   dP"  "Y8I  I8P"  "Yb   I8dP" "8I   88    dP"  "Yb  ,8'8,   
;  i8'    ,8I  ,8'    8I  i8'    ,8I  I8'    ,8i  I8P    I8   88   i8'       ,8'  Yb  
; ,d8,   ,d8I ,dP     Y8,,d8,   ,d8b,,I8 _  ,d8' ,d8     I8,_,88,_,d8,_    _,8'_   8) 
; P"Y8888P"8888P      `Y8P"Y8888P"`Y8PI8 YY88888P88P     `Y88P""Y8P""Y8888PPP' "YY8P8P
;        ,d8I'                        I8                                              
;      ,dP'8I                         I8                                              
;     ,8"  8I                         I8                                              
;     I8   8I                         I8                                              
;     `8, ,8I                         I8                                              
;      `Y8P"                          I8                                              
;____________________________________________________________Graphics____________________________________________________________

; Use draw current position of the start/end block
; Input: color(+6), position to draw(+4)
;--------------------------------
proc drawCurrentPos
push bp
mov bp, sp
push di
push ax

        mov di, [bp + 4] ; pos 

        push di
        call indexToPlace 
        pop di  ; place 

        push 8  ; input: width, height, color, place 
        push 8
        push [bp + 6]
        push di
        call drawCell

pop ax
pop di
pop bp 
ret 4
endp drawCurrentPos
;--------------------------------

; Use: print a width and height cell
; Input: width(+10), height(+8),  color(+6), place(+4) 
; Output: None  
;--------------------------------
proc drawCell
push bp 
mov bp, sp 
push cx
push di 
push ax 
    ;-----------------------
    mov di, [bp + 4] ; place 
    mov ax, [bp + 6] ; color 
    mov cx, [bp + 8] ; height 
    ;-----------------------
    block:
    push cx 
    mov cx, [bp + 10] ; width
        line:
        mov [es:di], al
        inc di  
        loop line 
    pop cx 
    ;------------
    push ax 
    mov ax, 320
    sub ax, [bp + 10]
    add di, ax ; 320 - width 
    pop ax 
    ;------------
    loop block 

pop ax 
pop di 
pop cx 
pop bp 
ret 8
endp drawCell
;--------------------------------

;--------------------------------
; Use: draw a board on the screen 
; Input: width(+10), height(+8),  color(+6), place(+4)
; Output: None  
proc drawBoard
push bp 
mov bp, sp
push di 
push ax 
push cx
    ;-----------------------
    mov di, [bp + 4] ; place 
    mov ax, [bp + 6] ; color
    mov cx, 20  
    ;-----------------------
        ; Draw Loop: 
        y_blocks:
        push cx
        mov cx, 20
        ;-----------------------
            x_blocks:
                push [bp + 10]  ; width
                push [bp + 8]   ; hieght 
                push ax
                push di 
                call drawCell 
                add di, 10
            loop x_blocks
        ;-----------------------
        pop cx 

        add di, 320 * 10 - 200  ; goes dowm 10 lines! 
        loop y_blocks
pop cx 
pop ax
pop di 
pop bp 
ret 8
endp drawBoard 
;--------------------------------

;--------------------------------
; use: change the graphics mode 
; input:  offset graphics_args(+4)
; graphics_args values:
; offset_list(0-16), offset start_pointer(+18), offset bg_pointer(+20), offset graphics_pointer(+22) 
; offset_list values:
; libraryS libraryB libraryG spaceS spaceB spaceG templeS templeB templeG
; output: None
proc graphics
push bp 
mov bp, sp
push ax 
push bx 
push si

    mov si, [bp + 4]

    mov bx, [si + 22]
    push [bx]
    call printBMP


    input_analyze:
     ; Wait for key press
    mov ah,0
    int 16h
    ;-----------------
        cmp al, '1' 
        je library
        cmp al, '2'
        je space
        cmp al, '3'  
        je temple 

    jmp input_analyze 
    ;-----------------
    ;------update graphics pointers--------
    library:
    ; start
    mov bx, [si + 18]   
    mov ax, [si]
    mov [word ptr bx],        ax
    ; algorithem background 
    mov bx, [si + 20]
    mov ax, [si + 2]
    mov [word ptr bx],        ax  
    ; graphic 
    mov bx, [si + 22]
    mov ax, [si + 4]
    mov [word ptr bx],        ax
    jmp end1 
    
    space:
    ; start
    mov bx, [si + 18]   
    mov ax, [si + 6]
    mov [word ptr bx],        ax
    ; algorithem background 
    mov bx, [si + 20]
    mov ax, [si + 8]
    mov [word ptr bx],        ax  
    ; graphic 
    mov bx, [si + 22]
    mov ax, [si + 10]
    mov [word ptr bx],        ax
    jmp end1 

    temple:
    ; start
    mov bx, [si + 18]   
    mov ax, [si + 12]
    mov [word ptr bx],        ax
    ; algorithem background 
    mov bx, [si + 20]
    mov ax, [si + 14]
    mov [word ptr bx],        ax  
    ; graphic 
    mov bx, [si + 22]
    mov ax, [si + 16]
    mov [word ptr bx],        ax
    jmp end1 
    ;--------------------------------------
 

    end1:
pop si
pop bx 
pop ax 
pop bp 
;-------------
ret 8
endp graphics
;--------------------------------
;----------------delay----------------
; input: None 
; output: None
proc delay
push cx	
	
		mov cx, 0fffh  
	;-------------------------------- 
	out_loop:
		push cx
		mov cx, 30
		;----------------
		inloop:
			loop inloop
		;----------------
	
		pop cx
		loop out_loop
	;--------------------------------

pop cx	; black box
ret     ; retrun the ip in order to 
endp delay
;------------------------------------


;--------------------------------
; Use: draw up and down cells with connection 
; up - new_cell 
; down - current_cell
; Input: new cell index(+4)/current_cell(+4) 
; Output: None 
proc drawY
push bp 
mov bp, sp
push ax 

    ;-------draw from the current cell dowm-----
    push [bp + 4]
    call indexToPlace 
    pop ax 
    ; draw 
    push 8 
    push 18
    push 0fh
    push ax
    call drawCell
    ;-------------------------------------------
    
pop ax 
pop bp
ret 2
endp drawY
;--------------------------------

;--------------------------------
; Use: draw left and right cells with connection 
; left - new_cell 
; right - current_cell
; Input: new cell index(+4)/current_cell(+4) 
; Output: None 
proc drawX
push bp 
mov bp, sp
push ax 

    ;-------draw from the current cell dowm-----
    push [bp + 4]
    call indexToPlace 
    pop ax 
    ; draw 
    push 18 
    push 8
    push 0fh
    push ax
    call drawCell
    ;-------------------------------------------
 
pop ax 
pop bp
ret 2
endp drawX
;--------------------------------
;________________________________________________________________________________________________________________________________









;                                 ,dPYb,                                         
;                                 IP'`Yb                                         
;                                 I8  8I                           gg            
;                                 I8  8'                           ""            
;                                 I8 dP    ,ggggg,      ,gggg,gg   gg     ,gggg, 
;                                 I8dP    dP"  "Y8ggg  dP"  "Y8I   88    dP"  "Yb
;                                 I8P    i8'    ,8I   i8'    ,8I   88   i8'      
;                                ,d8b,_ ,d8,   ,d8'  ,d8,   ,d8I _,88,_,d8,_    _
;                                8P'"Y88P"Y8888P"    P"Y8888P"8888P""Y8P""Y8888PP
;                                                           ,d8I'                
;                                                         ,dP'8I                 
;                                                        ,8"  8I                 
;                                                        I8   8I                 
;                                                        `8, ,8I                 
;                                                         `Y8P"            

;__________________________________________________________Logic__________________________________________________________________

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

;  ,ggggggggggggggg                              ,gg,                                            
; dP""""""88""""""" ,dPYb,                      i8""8i     I8                          ,dPYb,    
; Yb,_    88        IP'`Yb                      `8,,8'     I8                          IP'`Yb    
;  `""    88        I8  8I                       `88'   88888888                       I8  8I    
;         88        I8  8'                       dP"8,     I8                          I8  8bgg, 
;         88        I8 dPgg,    ,ggg,           dP' `8a    I8      ,gggg,gg    ,gggg,  I8 dP" "8 
;         88        I8dP" "8I  i8" "8i         dP'   `Yb   I8     dP"  "Y8I   dP"  "Yb I8d8bggP" 
;   gg,   88        I8P    I8  I8, ,8I     _ ,dP'     I8  ,I8,   i8'    ,8I  i8'       I8P' "Yb, 
;    "Yb,,8P       ,d8     I8, `YbadP'     "888,,____,dP ,d88b, ,d8,   ,d8b,,d8,_    _,d8    `Yb,
;      "Y8P'       88P     `Y8888P"Y888    a8P"Y88888P" 88P""Y88P"Y8888P"`Y8P""Y8888PP88P      Y8                                                                                                                                                                                              
;-----------------------------------------------
; Use: push given data to the stackC 
; Input: data to push(+6), p stack_pointer(+4)  
; Output: None  
proc pushC
push bp 
mov bp, sp 
push bx 
push di 

    
    mov bx, [bp + 4]  ; bx = offset stack pointer 
    mov di, [bx]       ; di = [stack pointer]
    sub [word ptr bx], 2       ; update stack pointer 

    ;----update stack value----
    mov bx,  [bp + 6]
    mov [di], bx
  

pop di 
pop bx 
pop bp 
ret 4
endp pushC
;-----------------------------------------------

;-----------------------------------------------
; Use: pop the data from the stackC  
; Input: p stack_pointer(+4)  
; Output: stack value
proc popC
push bp 
mov bp, sp
push bx
push di 

     ;------update stack pointer------- 
    mov bx, [bp + 4]        ; bx = offset stack pointer 
	add [word ptr  bx], 2  
    
	mov di, [bx]            ; di = [stack pointer]
    ;-------output------
    mov bx, [di]
    mov [bp + 4], bx        ; output the stack value
 
pop di 
pop bx 
pop bp 
ret
endp popC 
;-----------------------------------------------

;      ,gggg,                                                                                                          
;    ,88"""Y8b,                                                                 I8                                     
;   d8"     `Y8                                                                 I8                                     
;  d8'   8b  d8                                                              88888888                                  
; ,8I    "Y88P'                                                                 I8                                     
; I8'             ,ggggg,     ,ggg,,ggg,      ggg    gg    ,ggg,    ,gggggg,    I8      ,ggggg,     ,gggggg,    ,g,    
; d8             dP"  "Y8ggg ,8" "8P" "8,    d8"Yb   88bg i8" "8i   dP""""8I    I8     dP"  "Y8ggg  dP""""8I   ,8'8,   
; Y8,           i8'    ,8I   I8   8I   8I   dP  I8   8I   I8, ,8I  ,8'    8I   ,I8,   i8'    ,8I   ,8'    8I  ,8'  Yb  
; `Yba,,_____, ,d8,   ,d8'  ,dP   8I   Yb,,dP   I8, ,8I   `YbadP' ,dP     Y8, ,d88b, ,d8,   ,d8'  ,dP     Y8,,8'_   8) 
;   `"Y8888888 P"Y8888P"    8P'   8I   `Y88"     "Y8P"   888P"Y8888P      `Y888P""Y88P"Y8888P"    8P      `Y8P' "YY8P8P

;-----------------------------------------------
; Use: convert baord index to cell on the screen 
; Input: current_cell(+4) 
; Output: the place on the screen 
proc indexToPlace  ; keep in mind the board is 800 word not 400 
push bp 
mov bp, sp 
push ax
push dx 
push bx 
push cx 
    ; reset all registers 
    xor ax, ax
    xor bx, bx
    xor dx, dx
    xor cx, cx 

    mov ax, [bp + 4]
    
    mov bx, 20 
    div bx ; dx module ax division

    mov cx, dx 

    mov bx, 3200 
    mul bx ; ax = 320* division * 10
    
    ; switch values 
    xor ax, cx ; ax = module 
    xor cx, ax ; cx = 320 * division * 10 
    xor ax, cx 

    mov bx, 10 
    mul bx    ; ax = module * 10

    add cx, ax  ; dx = module * 10 + 320 * division * 10
    add cx, 381 ; dx  = 381 + module * 10 + 320 * division * 10

    mov [bp + 4], cx 
pop cx 
pop bx 
pop dx 
pop ax 
pop bp 
ret 
endp indexToPlace
;-----------------------------------------------


;-----------------------------------------------
; Use: given index in data and output index in board list 
; Input: offset board(+6), index in data(+4)
; Output: index in board   
proc dataToBoard 
push bp 
mov bp, sp 
push dx
push ax 
push bx 

    xor dx, dx ; solve div bug 
    
    mov ax, [bp + 4]     
    sub ax, [bp + 6]
    mov bx, 4
    div bx

    mov [bp + 6], ax  

pop bx 
pop ax 
pop dx 
pop bp 
ret 2
endp dataToBoard 
;-----------------------------------------------


;-----------------------------------------------
; Use: given index in board list and output index in data 
; Input: offset board(+6), boardIndex(+4)  
; Output: index in data 
proc boardToData
push bp 
mov bp, sp 
push dx
push ax 
push bx 

    xor dx, dx ; solve mul bug 
    ;------index * 4 + board----
    mov ax, [bp + 4]     
    mov bx, 4
    mul bx
    add ax, [bp + 6]
    ;--------------------------
    mov [bp + 6], ax  

pop bx 
pop ax 
pop dx 
pop bp 
ret 2
endp boardToData 
;-----------------------------------------------





;  ,ggg, ,ggggggg,                                                                                      
; dP""Y8,8P"""""Y8b                             ,dPYb,     ,dPYb,                                       
; Yb, `8dP'     `88                             IP'`Yb     IP'`Yb                                       
;  `"  88'       88            gg               I8  8I     I8  8I                                       
;      88        88            ""               I8  8'     I8  8'                                       
;      88        88   ,ggg,    gg     ,gggg,gg  I8 dPgg,   I8 dP         ,ggggg,     ,gggggg,    ,g,    
;      88        88  i8" "8i   88    dP"  "Y8I  I8dP" "8I  I8dP   88gg  dP"  "Y8ggg  dP""""8I   ,8'8,   
;      88        88  I8, ,8I   88   i8'    ,8I  I8P    I8  I8P    8I   i8'    ,8I   ,8'    8I  ,8'  Yb  
;      88        Y8, `YbadP' _,88,_,d8,   ,d8I ,d8     I8,,d8b,  ,8I  ,d8,   ,d8'  ,dP     Y8,,8'_   8) 
;      88        `Y8888P"Y8888P""Y8P"Y8888P"88888P     `Y88P'"Y88P"'  P"Y8888P"    8P      `Y8P' "YY8P8P
;                                         ,d8I'                                                         
;                                       ,dP'8I                                                          
;                                      ,8"  8I                                                          
;                                      I8   8I                                                          
;                                      `8, ,8I                                                          
;                                       `Y8P"                                    

;-----------------------------------------------
; Use: check for invalid movement to the left and right 
; Input: current_cell(+6), which neighbor +1/-1/20/-20(+4)
; Output: valid = 0 invalid = -1 
proc leftRight 
push bp
mov bp, sp
push ax 
push dx 
push cx 

    mov ax, [bp + 6]
    xor dx, dx 
    mov cx, 20 
    div cx

    cmp dx, 0 
    je left 
    cmp dx, 19
    je right 

    jmp not_left
    left: 
    cmp [word ptr bp + 4], -1    ; if the next cell is in the wall then invalid
    je invalid_edge 
    not_left:


    jmp not_right
    right: 
    cmp [word ptr bp + 4] , 1   ; if the next cell is in the wall then invalid
    je invalid_edge
    not_right:

    mov [word ptr bp + 6], 0 
    jmp valid1 
    invalid_edge:
    mov [word ptr bp + 6], -1
    valid1: 

pop cx
pop dx 
pop ax 
pop bp 
ret 2
endp leftRight
;-----------------------------------------------

;-----------------------------------------------
; Use: find current_cell neighbors that are not visited 
; Input: offset board(+8), current_cell(+6), which neighbor +1/-1/20/-20(+4)
; Output: valid: offset neighbor 
;         invalid: -1  
proc validNeighbor 
push bp 
mov bp, sp
push ax
push bx 


    xor ax, ax
    xor bx, bx 
    
    mov bx, [bp + 6]    ; current_cell index in the board 0 - 399
    mov ax, [bp + 4]    ; X = +1/-1, Y = +20/-20
    add bx, ax 
    
    ; 0 <= cell in board <= 399 and sides -> %20 -> left  %19 -> right
    cmp bx, 0
    jl invalid
    cmp bx, 399
    ja invalid

    push [bp + 6]
    push [bp + 4]
    call leftRight
    pop ax 
    cmp ax, -1 
    je invalid

    ; index in data 
    shl bx, 2      ; two word = 4 bytes
    mov ax, [bp + 8]
    add bx, ax 
    
    ; cell is visited 
    mov ax, [bx]    ;ax:dx 
    cmp ax, 1
    je invalid

    mov [bp + 8], bx    ; valid cell
    jmp valid

    invalid:
    mov [word ptr bp + 8], -1    ; invalid cell 
    valid:


pop bx
pop ax 
pop bp 
ret 4 
endp validNeighbor
;-----------------------------------------------

;-----------------------------------------------
; Use: generate the neighbors list and return if there is no neighbors  
; Input:offset neighbors_list(+8) p_board(+6), current_cell(+4) 
; Output: sum of neighbors 
proc findNeighbors
push bp 
mov bp, sp
push bx 
push ax  
push dx 
     
    xor ax, ax 
    xor dx, dx 

    mov bx, [bp + 8]

    ; Up
    push [bp + 6]
    push [bp + 4]; offset board(+8), current_cell(+6), which neighbor +1/-1/20/-20(+4)
    push -20
    call validNeighbor
    pop ax
    mov [bx], ax
    add dx, ax ; counter of invalid 
     

    ; Down 
    push [bp + 6]
    push [bp + 4]
    push 20
    call validNeighbor
    pop ax
    mov [bx + 2], ax
    add dx, ax ; counter of invalid 
    

    ; Left  
    push [bp + 6]
    push [bp + 4]
    push -1
    call validNeighbor
    pop ax
    mov [bx + 4], ax
    add dx, ax ; counter of invalid 
    
    
    ; Right 
    push [bp + 6]
    push [bp + 4]
    push 1
    call validNeighbor
    pop ax
    mov [bx + 6], ax
    add dx, ax ; counter of invalid 
    
    mov [bp + 8], dx 

pop dx 
pop ax 
pop bx 
pop bp 
ret 4
endp findNeighbors
;-----------------------------------------------

;  ,ggggggggggg,                                                                         
; dP"""88""""""Y8,                                    8I                                 
; Yb,  88      `8b                                    8I                                 
;  `"  88      ,8P                                    8I                                 
;      88aaaad8P"                                     8I                                 
;      88""""Yb,      ,gggg,gg   ,ggg,,ggg,     ,gggg,8I    ,ggggg,     ,ggg,,ggg,,ggg,  
;      88     "8b    dP"  "Y8I  ,8" "8P" "8,   dP"  "Y8I   dP"  "Y8ggg ,8" "8P" "8P" "8, 
;      88      `8i  i8'    ,8I  I8   8I   8I  i8'    ,8I  i8'    ,8I   I8   8I   8I   8I 
;      88       Yb,,d8,   ,d8b,,dP   8I   Yb,,d8,   ,d8b,,d8,   ,d8'  ,dP   8I   8I   Yb,
;      88        Y8P"Y8888P"`Y88P'   8I   `Y8P"Y8888P"`Y8P"Y8888P"    8P'   8I   8I   `Y8
;-----------------------------------------------
; Use: generate a randomSeed number using the clock 
;input: None - (push dx)
;output: randomSeed place (si)
proc randomSeed
push bp 
mov bp, sp 
push es  ; mess with es 
	push ax  ; Mathematical operations
	push si  ; Multiplier  
	push dx  ; holds the dosbox clock, and the Module = answer 
    push cx	 ; black box

			
			;------------------
			mov ah, 0 ; takes the value in dosbox clock and put it in dx and cx 
			int 1ah
			;------------------

			mov ax, dx	; ax = clock time 

			; add xor with randomSeed data for full randomSeed:
			mov bx, cx
			xor ax, [bx]

			;------------------
            xor dx, dx  
			mov si, 3298
			div si    
			;------------------
			
			mov [bp + 4], dx ; dx = 0 - 3

	pop cx 
	pop dx
	pop si
	pop ax
pop es
pop bp 
ret 
endp randomSeed
;-----------------------------------------------

;-----------------------------------------------
; Use: generate a radom number 
; Input: offset Xn(+4)
; Output: random number  
proc LCG
push bp 
mov bp, sp
push ax 
push dx 
push bx
push cx 

    xor dx, dx
    xor bx, bx
    xor ax, ax 

    mov bx, [bp + 4]
    mov ax, [bx]    ; ax = Xn
    mov cx, 3 
 
    mul cx          ; a*Xn 

    mov cx, 5
    add ax, cx      ; a*Xn + c 

    mov cx, 401 
    div cx          ; a*Xn + c % m

    mov [word ptr bx], dx
    ; random number between 0 - 3 
    mov ax, dx 
    mov cx, 4
    xor dx, dx ; fix div bug
    div cx 
    mov [bp + 4], dx 

pop cx 
pop bx
pop dx 
pop ax 
pop bp 
ret 
endp LCG
;-----------------------------------------------

;-----------------------------------------------
; Use: choose randomSeed neighbor to go 
; Input: offset Xn(+6), offset neighbor_list(+4)
; Output: data index of choosen neighbor
proc randomNeighbor
push bp
mov bp, sp 
    push bx 
    push si 
        ; reset registers 
        xor bx, bx
        xor si, si 

        reganerate:
        ;-----------
        push [bp + 6]
        call LCG
        pop si 
        ;-----------

        shl si, 1        ; word list 
        mov bx, [bp + 4] ; list of neighbors 
        add bx, si       ; index in the list  
        mov si, [bx]     ; the value of this index 

        cmp si, -1 
        je reganerate


        mov [bp + 6], si ; si = data place for the next cell 

pop si 
pop bx
pop bp
ret 2
endp randomNeighbor
;-----------------------------------------------

; ,ggg,         gg                                                      
; dP""Y8a        88                      8I                I8            
; Yb, `88        88                      8I                I8            
;  `"  88        88                      8I             88888888         
;      88        88                      8I                I8            
;      88        88  gg,gggg,      ,gggg,8I    ,gggg,gg    I8     ,ggg,  
;      88        88  I8P"  "Yb    dP"  "Y8I   dP"  "Y8I    I8    i8" "8i 
;      88        88  I8'    ,8i  i8'    ,8I  i8'    ,8I   ,I8,   I8, ,8I 
;      Y8b,____,d88,,I8 _  ,d8' ,d8,   ,d8b,,d8,   ,d8b, ,d88b,  `YbadP' 
;       "Y888888P"Y8PI8 YY88888PP"Y8888P"`Y8P"Y8888P"`Y888P""Y88888P"Y888
;                    I8                                                  
;                    I8                                                  
;                    I8                                                  
;                    I8                                                  
;                    I8                                                  
;                    I8                                                  


;-----------------
; Use: mark cell visited, and put the previous cell into the previous header
; Input: [previous cell](+6), new cell index in data(+4)  
; Output: None
proc updateCellHeader
push bp 
mov bp, sp 
push bx
push ax 

    ;--------------
    mov bx, [bp + 4]
    mov [word ptr bx], 1     ; visited
    ;-------------- 
    mov ax, [bp + 6]         ; value of current_cell
    mov [bx + 2], ax         ; previous cell 
    ;--------------
pop ax 
pop bx 
pop bp 
ret 4
endp updateCellHeader
;-----------------


;-----------------
; Use: call the functions that update the new cell
; Input: new cell - index in data(+10), new cell(+8), offset current_cell(+6), offset stack_pointer(+4)
; Output: None 
proc updateChoosenCell
push bp 
mov bp, sp 
push ax 
push bx 

    mov ax, [bp + 8]
    ;-------------------------     
    push  ax          ; new cell 
    push [bp + 4]     ; stack_pointer
    call pushC
    ;-------------------------
    mov bx, [bp + 6]    ;  offset current_cell
    ;-------------------------
    push [bx]                    ; previous cell 
    push [bp + 10]               ; index in data 
    call updateCellHeader
    ;-------------------------
    mov [bx], ax; update the current cell to the new one 
    ;-------------------------

pop bx 
pop ax 
pop bp 
ret 8
endp updateChoosenCell 
;-----------------

;      ,gg,                                                                            
;      i8""8i                ,dPYb,               I8                                    
;      `8,,8'                IP'`Yb               I8                                    
;       `88'                 I8  8I            88888888  gg                             
;       dP"8,                I8  8'               I8     ""                             
;      dP' `8a    ,ggggg,    I8 dP  gg      gg    I8     gg     ,ggggg,     ,ggg,,ggg,  
;     dP'   `Yb  dP"  "Y8ggg I8dP   I8      8I    I8     88    dP"  "Y8ggg ,8" "8P" "8, 
; _ ,dP'     I8 i8'    ,8I   I8P    I8,    ,8I   ,I8,    88   i8'    ,8I   I8   8I   8I 
; "888,,____,dP,d8,   ,d8'  ,d8b,_ ,d8b,  ,d8b, ,d88b, _,88,_,d8,   ,d8'  ,dP   8I   Yb,
; a8P"Y88888P" P"Y8888P"    8P'"Y888P'"Y88P"`Y888P""Y888P""Y8P"Y8888P"    8P'   8I   `Y8
;-----------------------------------------------
; Use: show the maze solution
; Input: is_maze(+10), maze_start(+8), maze_end(+6), offset board(+4)
; Output: None
proc solution
push bp 
mov bp, sp 
push bx 
push di 

    mov di, [bp + 6]
    cmp [word ptr bp + 10], 1
    jne no_maze

        ; di = 0 - 399

        push [bp + 4]
        push di 
        call boardToData
        pop bx ; the end of the maze link list

        ; bx = data index  
        mov di, [bx + 2] ; bx = previous cell 0 - 399
        ; if the end and start cell are side by side
        cmp di, [bp + 8]
        je no_maze

    pervious:
        call delay
     
        ;-------------draw-----------
        push LIGHT_GREEN
        push di
        call drawCurrentPos
        ;---------------------------
        ; di = 0 - 399

        push [bp + 4]
        push di 
        call boardToData
        pop bx

        ; bx = data index 
        mov di, [bx + 2] ; bx = previous cell 0 - 399

        cmp di, [bp + 8]       ; start cell the previous cell is 0 
    jne pervious; need condition 

    no_maze:


pop di 
pop bx
pop bp 
ret 8
endp solution
;-----------------------------------------------



; ,ggggggggggg,                                       
; dP"""88""""""Y8,                                I8   
; Yb,  88      `8b                                I8   
;  `"  88      ,8P                             88888888
;      88aaaad8P"                                 I8   
;      88""""Yb,     ,ggg,     ,g,      ,ggg,     I8   
;      88     "8b   i8" "8i   ,8'8,    i8" "8i    I8   
;      88      `8i  I8, ,8I  ,8'  Yb   I8, ,8I   ,I8,  
;      88       Yb, `YbadP' ,8'_   8)  `YbadP'  ,d88b, 
;      88        Y8888P"Y888P' "YY8P8P888P"Y88888P""Y88
                                                     


;-----------------------------------------------
; Use: reset the board graphics and data plus reset the algorithem variables 
; Input: is_maze(+14), maze_start(+12), maze_end(+10), bg_pointer(+8), p_current_cell(+6), p_board(+4) 
; Output: None 
proc resetMaze
push bp 
mov bp, sp 
push ax
push bx 
push cx 
push si

    mov si, [bp + 14]
    mov [word ptr si], 0
    ;----------Reset Graphics-------- 
    push [bp + 8]
    call printBMP
    push 8  ; input: width, height, color, place 
    push 8
    push 7
    push 381
    call  drawBoard


    push GREEN
    push [bp + 10]
    call drawCurrentPos
    push RED
    push [bp + 12]
    call drawCurrentPos
    ;--------------------------------

    ;----------Reset Data--------     
    mov bx, [bp + 4]
    mov cx, 800 
    resetBoard:
        mov [word ptr bx], 0
        add bx, 2
    loop resetBoard
    ;----------------------------
    mov si, [bp + 12] 
    mov ax, [si]
    ; reset current cell 
    mov bx, [bp + 6]
    mov [word ptr bx], ax

pop si
pop cx 
pop bx 
pop ax
pop bp 
ret 12 
endp resetMaze
;--------------------------------

;            ,ggg,                                                                                         
;           dP""8I   ,dPYb,                                             I8    ,dPYb,                       
;          dP   88   IP'`Yb                                             I8    IP'`Yb                       
;         dP    88   I8  8I                                      gg  88888888 I8  8I                       
;        ,8'    88   I8  8'                                      ""     I8    I8  8'                       
;        d88888888   I8 dP    ,gggg,gg    ,ggggg,     ,gggggg,   gg     I8    I8 dPgg,    ,ggg,,ggg,,ggg,  
;  __   ,8"     88   I8dP    dP"  "Y8I   dP"  "Y8ggg  dP""""8I   88     I8    I8dP" "8I  ,8" "8P" "8P" "8, 
; dP"  ,8P      Y8   I8P    i8'    ,8I  i8'    ,8I   ,8'    8I   88    ,I8,   I8P    I8  I8   8I   8I   8I 
; Yb,_,dP       `8b,,d8b,_ ,d8,   ,d8I ,d8,   ,d8'  ,dP     Y8,_,88,_ ,d88b, ,d8     I8,,dP   8I   8I   Yb,
;  "Y8P"         `Y88P'"Y88P"Y8888P"888P"Y8888P"    8P      `Y88P""Y888P""Y8888P     `Y88P'   8I   8I   `Y8
;                                 ,d8I'                                                                    
;                               ,dP'8I                                                                     
;                              ,8"  8I                                                                     
;                              I8   8I                                                                     
;                              `8, ,8I                                                                     
;                               `Y8P"                                                                      

P_XN equ [bp + 14]
P_STACKC equ [bp + 12]
P_BOARD equ [bp + 10]
P_NEIGHBORS_LIST equ [bp + 8]
P_STACK_POINTER equ [bp + 6]
P_CURRENT_CELL equ [bp + 4] 
;-----------------------------------------------
; Use: carve the maze useing functions 
; Input: offset maze_start(+16), offset Xn(+14), offset stackC(+12), offset board(+10), offset neighbors_list(+8), 
;        offset stack_pointer(+6), offset current_cell(+4)
; Output: None 
proc carveMaze
push bp
mov bp, sp
push ax
push bx 
push dx 
push si

;-----------------------------------------------
    mov si, P_CURRENT_CELL      ; si = current cell pointer
    push P_BOARD
    mov bx, [bp + 16]
    push [bx] 
    call boardToData
    pop ax 
;-----------------------------------------------
    push ax
    mov bx, [bp + 16]
    push [bx] 
    push si
    push P_STACK_POINTER
    call updateChoosenCell

    ; algorithem
    carveLoop:
         call delay 

        ;-------create neighbors list-------
        push P_NEIGHBORS_LIST
        push P_BOARD
        push [si]
        call findNeighbors      ;offset neighbors_list(+8) p_board(+6), current_cell(+4) 
        pop ax 
        ;------------------------------------
            ;---------- no neighbors ------------
            cmp ax, -4  
            je go_back
        ;---------choose random neighbor-----------
        push P_XN
        push P_NEIGHBORS_LIST
        call randomNeighbor
        pop ax  ; index in data of the choosen neighbor 0-399
        ;------------------------------------

            ;-------- bx = new cell in board-----
            push P_BOARD
            push ax      
            call dataToBoard
            pop dx        
            ;-----------------------------------
        
        mov bx, P_NEIGHBORS_LIST
        cmp ax, [bx]       ; up  
        je up_cell 
        cmp ax, [bx + 2]   ; down
        je down_cell 
        cmp ax, [bx + 4]   ; left
        je left_cell 
        cmp ax, [bx + 6]   ; right
        je right_cell 


        up_cell:
            push dx 
            call drawY 
        jmp update_cell 

        down_cell:
            push [si]
            call drawY ; current cell value 
        jmp update_cell 
        
        left_cell:
            push dx 
            call drawX 
        jmp update_cell 


        right_cell:
            push [si]
            call drawX ; current cell value 
        jmp update_cell 
        
         go_back:
            push P_STACK_POINTER      
            call popC
            pop [si]

        jmp check_end  

        update_cell: 
            push ax
            push dx 
            push P_CURRENT_CELL
            push P_STACK_POINTER
            call updateChoosenCell
        jmp check_end


        check_end:
        mov bx, P_STACK_POINTER
        mov dx, P_STACKC
        cmp [bx], dx
    jne carveLoop
pop si
pop dx 
pop bx 
pop ax 
pop bp 
ret 14
endp carveMaze
;-----------------------------------------------
; Use: print and update varibles for end of craving the maze
; Input: maze_end(+8), maze_start(+6), offset is_maze(+4)
proc endCrave
push bp
mov bp, sp
push bx

            push RED
            mov bx, [bp + 6]
            push [bx]
            call drawCurrentPos

            push GREEN
            mov bx, [bp + 8]
            push [bx] 
            call drawCurrentPos

            mov bx, [bp + 4]
            mov [bx], 1
pop bx
pop bp 
ret 6
endp endCrave
;-----------------------------------------------
; Use: run all the function of the algorithm 
; Input: offset run_args (+4)
; run_args values:
;     offset is_maze(+18), offset maze_start(+16), offset maze_end(+14), offset current_cell(+12)B, offset stack_pointer(+10)B, offset neighbors_list(+8),
;     offset board(+6)B, offset stackC(+4)B, offset Xn(+2)B, offset bg_pointer(+0) 
; Output: None
proc run 
push bp 
mov bp, sp
push ax 
push bx 
push si

    mov si, [bp + 4] ; the head of the args
    set_graphics:
    ;----------Set Graphics---------
        push [si + 18]
        mov bx, [si + 16]
        push [bx]
        mov bx, [si + 14]
        push [bx]
        mov bx, [si]
        push [bx] 
        push [si + 12]
        push [si + 6]
        call resetMaze
    ;-------------------------------

    ;-----------option loop-------------
    optionLoop:
    input:
        ; Wait for key press
        mov ah,0
        int 16h
        ;---------------
            cmp al, 'c'
            je create_maze 
            cmp al, 'r'
            je reset_maze
            cmp al, 's'
            je solve_maze
            cmp al, 'S'
            je secret_settings
            cmp al, 27
            je end_run 
        jmp input
        ;---------------

        create_maze:
            push [si + 16]
            push [si + 2]
            push [si + 4]
            push [si + 6]
            push [si + 8]
            push [si + 10]
            push [si + 12] ;; look at here
            call carveMaze  ; buged 
            push [si + 14]
            push [si + 16]
            push [si + 18]
            call endCrave

        jmp optionLoop

        reset_maze:
            push [si + 18]
            mov bx, [si + 16]
            push [bx]
            mov bx, [si + 14]
            push [bx]
            mov bx, [si]
            push [bx] 
            push [si + 12]
            push [si + 6]
            call resetMaze
        jmp optionLoop

        solve_maze:
            mov bx, [si + 18]
            push [bx]
            mov bx, [si + 16]
            push [bx]
            mov bx, [si + 14]
            push [bx]
            push [si + 6]
            call solution
        jmp optionLoop

        secret_settings:
            push [si + 18]
            push [si]
            push [si + 12]
            push [si + 6]
            push offset secret ; TODO 
            push [si + 16]
            push [si + 14]
            call secretSettings
        jmp set_graphics
    ;-------------------------------
end_run:

pop si
pop bx 
pop ax 
pop bp
ret 2
endp run 










;       ,gg,                                                            ,gg,                                                                      
;      i8""8i                                            I8            i8""8i              I8      I8                                             
;      `8,,8'                                            I8            `8,,8'              I8      I8                                             
;       `88'                                          88888888          `88'            8888888888888888  gg                                      
;       dP"8,                                            I8             dP"8,              I8      I8     ""                                      
;      dP' `8a   ,ggg,     ,gggg,   ,gggggg,   ,ggg,     I8            dP' `8a   ,ggg,     I8      I8     gg    ,ggg,,ggg,     ,gggg,gg    ,g,    
;     dP'   `Yb i8" "8i   dP"  "Yb  dP""""8I  i8" "8i    I8           dP'   `Yb i8" "8i    I8      I8     88   ,8" "8P" "8,   dP"  "Y8I   ,8'8,   
; _ ,dP'     I8 I8, ,8I  i8'       ,8'    8I  I8, ,8I   ,I8,      _ ,dP'     I8 I8, ,8I   ,I8,    ,I8,    88   I8   8I   8I  i8'    ,8I  ,8'  Yb  
; "888,,____,dP `YbadP' ,d8,_    _,dP     Y8, `YbadP'  ,d88b,     "888,,____,dP `YbadP'  ,d88b,  ,d88b, _,88,_,dP   8I   Yb,,d8,   ,d8I ,8'_   8) 
; a8P"Y88888P" 888P"Y888P""Y8888PP8P      `Y8888P"Y88888P""Y88    a8P"Y88888P" 888P"Y88888P""Y8888P""Y888P""Y88P'   8I   `Y8P"Y8888P"888P' "YY8P8P
;                                                                                                                                  ,d8I'          
;                                                                                                                                ,dP'8I           
;                                                                                                                               ,8"  8I           
;                                                                                                                               I8   8I           
;                                                                                                                               `8, ,8I           
;                                                                                                                                `Y8P"            

; Use: give the user the option of choosing the end and the start of the maze
; Input: is_maze(+16), bg_pointer(+14), p_current_cell(+12), p_board(+10)  offset secret(+8), offset maze_start(+6), offset maze_end(+4)
; Output: None
proc secretSettings
push bp
mov bp, sp
push bx
push ax
push cx
push si
push dx
push di


    secretLoop:
    ; print backgroud
    push [bp + 8] 
    call printBMP

    inputSecret:
        ; Wait for key press
        mov ah,0
        int 16h
        ;---------------
            cmp al, 's'
            je start_point
            cmp al, 'e'
            je end_point
            cmp al, 27
            je end_setting

        jmp inputSecret
        
        start_point:
            mov bx, [bp + 6] ;offset maze_start 
            mov cx, RED

            mov dx, GREEN
            mov di, [bp + 4]
        jmp visualizeInput


        end_point:
            mov bx,  [bp + 4] ; offset maze_end
            mov cx, GREEN

            mov dx, RED
            mov di, [bp + 6]
        jmp visualizeInput

        visualizeInput:
            ;-------reset_board------  
            ; bg_pointer, p_current_cell, p_board 
            push [bp + 16]
            mov si, [bp + 6]
            push [si]
            mov si, [bp + 4]
            push [si]
            mov si, [bp + 14]
            push [si] 
            push [bp + 12]
            push [bp + 10]
            call resetMaze

            ;-------starts_move_cell--------
            push cx ; the color of the block to move 
            push bx ; the offset of the varible to change 
            push di ; the offset pos that need to stay 
            call chooseCell
            ; after the player choose his start\end point
        jmp secretLoop

    end_setting:

pop di
pop dx
pop si
pop cx
pop ax
pop bx
pop bp 
ret 14
endp secretSettings

; Use: print the player choosen cell of a end\start point
; Input: color(+8), point offset(+6), still point offset(+4)
; Output: None
proc chooseCell
push bp
mov bp, sp 
push ax
push bx 
push si ; change the chosen position and keep tarck of the position
push di ; draw the chosen block
push cx

        mov si, [bp + 6] ; pos 
        mov di, [bp + 8] ; color


        add di, 8 ; convert to the lighter form of the color - visual effect 

visaul_movement:

    stepInput:
        ; Wait for key press
        mov ah,0
        int 16h
        ;---------------
            cmp al, 'w'
            je up_m
            cmp al, 's'
            je down_m
            cmp al, 'a'
            je left_m
            cmp al, 'd'
            je right_m
            cmp al, 13 ; Enter
            je pressEnter
        jmp stepInput

            up_m:
                mov cx, -20
            jmp move

            down_m:
                mov cx, 20
            jmp move

            left_m:
                mov cx, -1
            jmp move
            
            right_m:
                mov cx, 1
            jmp move

            move:
                push [bp + 4] ; still point
                push di
                push [si]
                push cx
                call updateMovement 
                pop [si]
            jmp visaul_movement
    pressEnter:
    
pop cx
pop di
pop si
pop bx
pop ax
pop bp 
ret 6
endp chooseCell

; Use: update movement -  check if touch_edges return the same var
;            if not - return the update var, and print visualize movement
; Input: still point(+10), color(+8), current pos(+6), the choosen movement(20/-20/1/-1)(+4)
; Output: the updated movement
proc updateMovement
push bp
mov bp, sp 
push ax
push bx
push si

    mov si, [bp + 6] ; current pos 
    mov bx, si
    add bx, [bp + 4] ; next pos

    push [bp + 4]
    push [bp + 10]
    push bx
    call touch_edges
    pop ax ; 1 - invalid ,0 - valid
    cmp ax, 1 
    je invalidMove
            ; delete last pos
            push GRAY
            push si
            call drawCurrentPos
            

            ; print new pos 
            push [bp + 8] ; color
            push bx
            call drawCurrentPos

            mov [bp + 10], bx ; return updated move


    jmp validMove
    invalidMove:
        mov [bp + 10], si ; return updated move
    validMove:

pop si
pop bx
pop ax
pop bp 
ret 6
endp updateMovement
; Use: check if the user touch the edges |1 - he touched |0 - he didn't touch|
; Input: dir(+8), still point(+6), the next move of the user(0-399)(+4) 
; Output: 1 - touched
;         0 - didn't touched
proc touch_edges
push bp 
mov bp, sp 
push ax
push bx 
push cx 
	;------------------------
	mov ax, [bp + 4]	; first list of positions value 
	mov cl, 20  
	;------------------------

    
    mov bx, [bp + 6] 
    ; still point
    cmp ax, [bx]
    je touch
	; Up
	cmp ax, 0
	jl touch 
	; Down
	cmp ax, 399 
	ja touch 
	div cl

    ; check in which direction the user went - left/ right because the cmp doesn't work properly without this check 
    ; 19 - is vaild in the left but not in right the same to 0
    cmp [bp + 8], 1
    jne not_left1
        cmp ah, 0
        je touch 
    not_left1:

    cmp [bp + 8], -1
    jne not_right1
        cmp ah, 19
        je touch 
    not_right1:

    ; didn't touch
    mov [bp + 8], 0 
	jmp not_touch
	touch:
    mov [bp + 8], 1 
	not_touch:


pop cx
pop bx
pop ax 
pop bp
ret 4
endp touch_edges

;________________________________________________________________________________________________________________________________



;                                    ,ggg, ,ggg,_,ggg,                                  
;                                    dP""Y8dP""Y88P""Y8b                                 
;                                    Yb, `88'  `88'  `88                                 
;                                    `"  88    88    88               gg                
;                                        88    88    88               ""                
;                                        88    88    88    ,gggg,gg   gg    ,ggg,,ggg,  
;                                        88    88    88   dP"  "Y8I   88   ,8" "8P" "8, 
;                                        88    88    88  i8'    ,8I   88   I8   8I   8I 
;                                        88    88    Y8,,d8,   ,d8b,_,88,_,dP   8I   Yb,
;                                        88    88    `Y8P"Y8888P"`Y88P""Y88P'   8I   `Y8

start:
;--------------
mov ax, @data
mov ds, ax
;extra segment 
mov ax, 0A000h 
mov es, ax 
; Graphic mode
mov ax, 13h
int 10h
;--------------
; Main:
push offset graphics_args
push offset libraryS
call fileOffsets
; Initialzing random seed from the clock 
push ax 
call randomSeed
pop [Xn]

startMenu:
push [start_pointer]
call printBMP

inputloop:
    ;-----------------
    ; Wait for key press
    mov ah,0
    int 16h

        cmp al, 'r' 
        je start_run
        cmp al, 'g'
        je graphics_options
        cmp al, 27
        je exit
    jmp inputloop 
    ;-----------------

    graphics_options:
        push offset graphics_args
        call graphics 
    jmp startMenu

    start_run:  
        push offset run_args
        call run
    jmp startMenu

;-------------------------------
exit:
; Back to text mode
mov ah, 0
mov al, 2
int 10h
; end 
mov ax, 4c00h
int 21h
END start