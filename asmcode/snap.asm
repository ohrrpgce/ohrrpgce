;****************************************************************************
;*
;***** scrnshot.asm - for taking 24-bit bmp screenshots
;*
;* by Brian Fisher
;*
;*  modified February 12th 1998

.286
ideal
Model Small
Public screenshot

Codeseg

palseg	dw ?
paloff	dw ?
bufseg	dw ?
bufoff	dw ?
pagoff	dw ?
scroff	dw ?
read	db 0
bmp	dw ?
count	db ?
lines	db ?

header	db 'BM'
bsize	dd 192054
br1	dw 0
br2	dw 0
boff	dd 54
bisize	dd 40
bwidth	dd 320
bheigth	dd 200
bplane	dw 1
bbit	dw 24
bcomp	dd 0
bdata	dd 192000
bxmeter	dd 1184
bymeter dd 958
bcused	dd 256
bimport	dd 256

proc	screenshot	;fil$, page, pal(), buf()
	push bp
	mov bp,sp
	push es di ds si

	mov si,[ss:bp+12]
	add si,2
	mov dx,[ds:si]
	mov ax,3c00h
	xor cx,cx
	int 21h
	jnc opened
	jmp noshot
opened:
	mov [cs:bmp],ax
	mov bx,[ss:bp+08]
	mov ax,[ds:bx+02h]
	mov [cs:palseg],ax
	mov ax,[ds:bx+0ah]
	mov [cs:paloff],ax
	mov bx,[ss:bp+06]
	mov ax,[ds:bx+02h]
	mov es,ax
	mov [cs:bufseg],ax
	mov ax,[ds:bx+0ah]
	mov [cs:bufoff],ax
	lea dx,[cs:header]
	mov ax,cs
	mov ds,ax
	mov cx,54
	mov bx,[cs:bmp]
	mov ax,4000h
	int 21h
	mov ax,[ss:bp+10]
	shl ax,14
	add ax,15920
	mov [cs:scroff],ax
	mov [cs:count],8
doeigth:
	mov di,[cs:bufoff]
	mov ax,0a000h
	mov ds,ax
	mov [cs:read],0
	mov [cs:lines],25
dofourth:
	mov si,[cs:scroff]
	mov dx,03ceh
	mov al,04h
	mov ah,[read]
	out dx,ax
	mov cx,80
readsome:
	lodsb
	xor ah,ah
	mov bx,6
	mul bx
	push ds si
	mov ds,[cs:palseg]
	mov si,[cs:paloff]
	add si,ax
	lodsw
	shl al,2
	mov bh,al
	lodsw
	shl al,2
	mov bl,al
	lodsw
	shl al,2
	stosb
	mov al,bl
	stosb
	mov al,bh
	stosb
	add di,9
	pop si ds
	loop readsome
	sub di,957
	mov al,[cs:read]
	inc al
	mov [cs:read],al
	cmp al,4		;jmp if need to do more planes
	jb dofourth
	add di,948
	sub [cs:scroff],80
	mov [cs:read],0
	mov al,[cs:lines]
	dec al
	mov [cs:lines],al
	cmp al,0		;jmp if you haven't done 25 lines
	jnz dofourth
	mov ds,[cs:bufseg]
	mov dx,[cs:bufoff]
	mov cx,24000
	mov bx,[cs:bmp]
	mov ax,4000h		;write the block
	int 21h
	dec [cs:count]
	cmp [cs:count],0
	jz closeit
	jmp doeigth
closeit:
	mov bx,[cs:bmp]
	mov ax,3e00h
	int 21h
noshot:
	pop si ds di es bp
	retf 8
endp	screenshot
end