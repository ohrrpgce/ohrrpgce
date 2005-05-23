;*****************************************************************************

;*** Timing routines using interrupt 15h function 83h
;* used in terms of microseconds
;* Brian Fisher
;*
;*

.286
Ideal
Model Small
Public setwait,dowait
CodeSeg

cseg	dw ?
coff	dw ?
time 	dw ?
step 	dw 2

Proc    SetWait         ; var(), number of 1/1024 seconds
			;    8        6
	push bp
	mov bp,sp
	push es di

	mov bx,[ss:bp+08]
	mov es,[ds:bx+02h]
	mov bx,[ds:bx+0ah]
	mov [cs:cseg],es
	mov [cs:coff],bx
	mov di,bx
	mov ax,0
	mov [es:di],ax
	mov ax,976
	mov cx,[ss:bp+06]
	mul cx
	mov cx,dx
	mov dx,ax
	mov ax,8300h
	int 15h
	mov ax,040h
	mov es,ax
	mov ax,[ss:bp+06]
	mov bx,55
	xor dx,dx
	div bx
	add ax,[cs:step]
	add ax,[es:06ch]
	mov [cs:time],ax
	pop di es bp
	retf 4
endp    SetWait

proc	Dowait
	push bp
	mov bp,sp
	push ds es di

	mov es,[cs:cseg]
	mov di,[cs:coff]
	mov ax,040h
	mov ds,ax
motime:
	mov ax,[ds:06ch]
	cmp ax,[cs:time]
	jnb clock
	mov ax,[es:di]
	cmp ax,0
	jz motime
	mov [cs:step],2
	jmp done
clock:
	mov [cs:step],0
done:
	pop di es ds bp
	retf 0
endp 	Dowait
end
