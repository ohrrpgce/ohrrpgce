;***************************************************************************

;*** Set mode x
;* Hope this works..
;* Brian Fisher
;*
;*** Jan 21, '98

.286
Ideal
Model Small

Public Setmodex,Restoremode,CopyPage,clearpage,SetVisPage,Setpal,fadeto,fadetopal

Codeseg
oldmode	db 0
doneyet dw 0
red     db 0
green   db 0
blue    db 0
paloff  dw 0
palseg  dw 0

proc    Setpal  ; palbuff()
		;    6
	push bp
	mov bp,sp
	push ds si es di
	
	cld
	mov bx,[ss:bp+06]
	mov si,[ds:bx+0ah]
	mov ds,[ds:bx+02h]
	mov dx,03dah
	mov cx,768
rtr:
	in al,dx
	test al,08h
	jnz rtr
tra:
	in al,dx
	test al,08h
	jz tra
	mov dx,03c8h
	mov al,0
	out dx,al
	inc dx
setit:
	outsb
	inc si
	loop setit
	pop di es si ds bp
	retf 2
endp    setpal

Proc    Fadeto  ;  buff(), red, green, blue
		;    12     10    8     6
	push bp
	mov bp,sp
	push ds si es di

	cld
	mov ax,[ss:bp+10]
	mov [red],al
	mov ax,[ss:bp+08]
	mov [green],al
	mov ax,[ss:bp+06]
	mov [blue],al
	mov dx,03c7h
	mov al,0
	out dx,al
	mov dx,03c9h
	mov cx,768
	mov bx,[ss:bp+12]
	mov si,[ds:bx+0ah]
	mov es,[ds:bx+02h]
	mov ds,[ds:bx+02h]
	mov bx,si
	mov di,bx
	rep insb
fadeloop:
	mov si,bx
	mov di,bx
	mov dx,0
	mov cx,256
redloop:
	lodsb 
	mov ah,[red]
	cmp al,ah
	ja rdown
	jb rup
	inc dx
	jmp grnloop
rdown:
	dec al
	jmp grnloop
rup:
	inc al
grnloop:
	stosb
	lodsb 
	mov ah,[green]
	cmp al,ah
	ja gdown
	jb gup
	inc dx
	jmp bluloop
gdown:
	dec al
	jmp bluloop
gup:
	inc al
bluloop:
	stosb
	lodsb 
	mov ah,[blue]
	cmp al,ah
	ja bdown
	jb bup
	inc dx
	jmp nextclr
bdown:
	dec al
	jmp nextclr
bup:
	inc al
nextclr:
	stosb
	loop redloop
	mov [doneyet],dx
	mov si,bx
	mov dx,03dah
	mov cx,768
retr:
	in al,dx
	test al,08h
	jnz retr
trac:
	in al,dx
	test al,08h
	jz trac
	mov dx,03c8h
	mov al,0
	out dx,al
	inc dx
	rep outsb
	mov ax,[doneyet]
	cmp ax,768
	jb fadeloop

	pop di es si ds bp
	retf 8
endp    fadeto        

Proc    Fadetopal;  pal(), buff()
	push bp
	mov bp,sp
	push ds si es di

	cld
	mov dx,03c7h
	mov al,0
	out dx,al
	mov dx,03c9h
	mov cx,768
	mov bx,[ss:bp+06]
	mov di,[ds:bx+0ah]
	mov es,[ds:bx+02h]
	mov bx,[ss:bp+08]
	mov si,[ds:bx+0ah]
	mov ds,[ds:bx+02h]
	mov [cs:paloff],si
	mov [cs:palseg],ds
	rep insb
	sub di,768
fadeit:
	mov dx,0
	mov cx,768
	mov ds,[cs:palseg]
	mov si,[cs:paloff]
clrloop:
	lodsw 
	mov ah,al
	mov al,[es:di]
	cmp al,ah
	ja down
	jb up
	inc dx
	jmp next
down:
	dec al
	jmp next
up:
	inc al
next:
	stosb
	loop clrloop
	mov [doneyet],dx
	mov dx,03dah
	mov cx,768
	sub di,768
	mov si,di
	mov ax,es
	mov ds,ax
retra:
	in al,dx
	test al,08h
	jnz retra
trace:
	in al,dx
	test al,08h
	jz trace
	mov dx,03c8h
	mov al,0
	out dx,al
	inc dx
	rep outsb
	mov ax,[doneyet]
	cmp ax,768
	jb fadeit

	pop di es si ds bp
	retf 4
endp    fadetopal

Proc	Restoremode
	mov al,[cs:oldmode]
	mov ah,00h
	int 10h
	retf
endp	Restoremode

Proc    Setmodex
	push es di

	mov ah,0fh
	int 10h
	mov [cs:oldmode],al

	mov ah,00h
	mov al,13h
	int 10h

	mov ax,0604h
	mov dx,03c4h
	out dx,ax

	mov ax,0e317h
	mov dx,03d4h
	out dx,ax

	mov ax,0014h
	out dx,ax
	
	mov ax,0f02h
	mov dx,03c4h
	out dx,ax

	mov ax,0a000h
	mov es,ax
	mov ax,0000h
	xor di,di
	mov cx,32768
	cld
	rep stosw

	pop di es
	retf 0
Endp    Setmodex

Proc    CopyPage        ; Offset1, Offset2
			;    8        6
	push bp
	mov bp,sp
	push es di ds si

	mov bh,0
	mov bl,1
	mov ax,0a000h
	mov ds,ax
	mov es,ax
	mov ax,[ss:bp+8] ;get source offset
	shl ax,14
	mov si,ax
	mov ax,[ss:bp+6] ;get dest. offset
	shl ax,14
	mov di,ax
	mov ax,0f02h
	mov dx,03c4h
	out dx,ax        ;turn the write plane enable to all planes
	mov ax,0008h
	mov dx,03ceh
	out dx,ax        ;set to latches
	mov cx,16000
	rep movsb
	inc dx
	mov al,0ffh
	out dx,al        ;set to CPU value
	
	pop si ds di es bp
	retf 4
endp    Copypage

Proc    Clearpage       ; page
			;  6
	push bp
	mov bp,sp
	push es di
	mov bx,[ss:bp+06]
	shl bx,14
	mov di,bx
	mov ax,0a000h
	mov es,ax
	mov dx,03c4h
	mov al,02h
	mov ah,0fh
	out dx,ax
	mov ax,0
	mov cx,8000
	cld
	rep stosw
	pop di es bp
	retf 2
endp    clearpage

Proc    SetVisPage      ; Pageoffset
			; 6
	push bp
	mov bp,sp

	mov cx,[ss:bp+06]
	shl cx,14
	mov bh,cl
	mov bl,0dh
	mov cl,0ch
	mov dx,03dah
waitde:
	in al,dx
	test al,01h
	jnz waitde
	mov dx,03d4h
	mov ax,bx
	out dx,ax
	mov ax,cx
	out dx,ax
	mov dx,03dah
waitvr:
	in al,dx
	test al,08h
	jz waitvr
	pop bp
	retf 2
endp    SetVisPage
end
