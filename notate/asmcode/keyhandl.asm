;*****************************************************************************

;**** Keyboard Handler
;*
;*
.286
Ideal
Model Small
public keyseg,keyoff,keyval,getkey,setkeys
Codeseg
keyvals db      128 dup (?)
gamekey db	128 dup (?)
proc    keyhandler
	push si ax
	mov si,offset keyvals
	in al,60h
	cmp al,128
	jb pressed
	and ax,127
	add si,ax
	and [byte cs:si],2
	jmp done
pressed:
	and ax,127
	mov [byte cs:si],al
	add si,ax
	mov [byte cs:si],3
done:
	mov al,20h
	out 20h,al
	pop ax si
	iret 
endp    keyhandler

proc    keyseg
	mov ax,seg keyhandler
	retf 0
endp    keyseg

proc    keyoff
	mov ax,offset keyhandler
	retf 0
endp    keyoff

proc    keyval  ;whichkey
	push bp
	mov bp,sp
	push si ds
	mov ax,seg gamekey
	mov ds,ax
	mov ax,[ss:bp+06]
	mov si,offset gamekey
	add si,ax
	mov al,[byte ds:si]
	and ax,3
	pop ds si bp
	retf 2
endp    keyval

proc    getkey
	push di es
	mov ax,seg keyvals
	mov es,ax
	mov di,offset keyvals
	xor ax,ax
	mov [byte es:di],al
poll:
	mov bl,[byte es:di]
	cmp bl,0
	je poll
	mov cx,126
	rep stosb
	mov al,bl
	pop es di
	retf 0
endp    getkey

proc 	setkeys
	push es di ds si
	mov ax,seg keyvals
	mov ds,ax
	mov ax,seg gamekey
	mov es,ax
	mov di,offset gamekey
	mov si,offset keyvals
	mov cx,64
	rep movsw
	mov cx,64
	mov si,offset keyvals
setit:
	and [word ds:si],257
	add si,2
	loop setit
	pop si ds di es
	retf 0
endp	setkeys
end
