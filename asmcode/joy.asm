;************************************************************************
;
;
;******* Joystick reading routines, by Brian Fisher
;
;**** modifed July 22 (clear interrupts when polling)
.286
Ideal
Model small
Public Readjoy
Codeseg

Init    dw 0
fact    dw 0
horiz   dw 0
vert    dw 0
b1      dw 0
b2      dw 0
badcloc	dw 0
detect	dw 0
badcnt	dw 0

Proc    Readjoy         ;buff(), which
	push bp
	mov bp,sp
	push es di

	xor ax,ax
	mov [cs:badcloc],ax
	mov bx,[ss:bp+08]
	mov di,[ds:bx+0ah]
	mov ax,[ds:bx+02h]
	mov es,ax
	mov ax,[ss:bp+06]
	mov [cs:fact],ax
	cmp ax,0
	jz good
	mov [cs:fact],2
good:
	cmp [cs:Init],1
	jz setup
reinit:
	in al,61h
	and al,0fdh
	or  al,1
	out 61h,al              ;clear pc speaker
	mov [cs:Init],1
	mov al,010110100b             ;program timer 2 with modus 2 */
	out 43h,al
	mov al,0
	out 42h,al
	out 42h,al
setup:
	xor ax,ax
	mov [cs:detect],ax
	mov [cs:badcnt],ax
	mov dx,201h             ;201h is the joystick port
	in al,dx
	mov cx,[cs:fact]
	mov bx,1
	shl bx,cl
	mov [cs:fact],bx        ;set fact to be x-axis bit
	add cx,4
	shr al,cl
	mov bl,al
	and bx,1
	mov [cs:b1],bx
	shr al,1
	and ax,1
	mov [cs:b2],ax          ;read the buttons
	xor cx,cx
	xor al,al
	out dx,al               ;send 0 to joystick
	mov [cs:horiz],-1
	mov [cs:vert],-1
	cli
	mov al,80h
	out 43h,al
	in al,42h
	mov bl,al
	in al,42h
	mov bh,al               ;read timer 2 in bx
joyloop:
	mov dx,201h
	in al,dx                ;get the joystick info
	mov cl,al
	mov al,080h
	out 43h,al
	in al,42h
	mov ah,al
	in al,42h
	xchg ah,al              ;read timer 2 in ax
	sub ax,bx
	neg ax
	cmp ax,5000             ;leave routine after 5000 CLK
	ja noax
	cmp ax,0
	jnz goody
	inc [cs:badcnt]
	cmp [cs:badcnt],1000h
	ja badt2
goody:
	cmp [cs:horiz],-1
	jnz noh
	mov dx,[cs:fact]
	test cl,dl              ;check horizontal bit
	jnz noh
	mov [cs:horiz],ax
noh:
	cmp [cs:vert],-1
	jnz nov
	mov dx,[cs:fact]
	shl dx,1
	test cl,dl              ;check vertical bit
	jnz nov
	mov [cs:vert],ax
nov:
	mov ax,-1
	cmp [cs:horiz],ax
	jz joyloop
	cmp [cs:vert],ax
	jz joyloop
	mov [cs:detect],ax
noax:
	mov ax,[cs:horiz]
	cmp ax,0
	jnz gotem
badt2:
	sti
	mov [cs:detect],0
	cmp [cs:badcloc],1
	jz gotem
	mov [cs:badcloc],1
	jmp reinit
gotem:
	sti
	stosw
	mov ax,[cs:vert]
	stosw
	mov ax,[cs:b1]
	stosw
	mov ax,[cs:b2]
	stosw
	mov ax,[cs:detect]
	pop di es bp
	retf 4
endp    readjoy
end