;***************************************************************************

;*** Set mode x
;* Hope this works..
;* Brian Fisher
;*
;*
.286
Ideal
Model Small

Public Setmodex,CopyPage,clearpage,SetVisPage

CodeSeg

Proc    Setmodex
	push es di

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
