;*************************************************************************

;****** String to Array and Array to String
;*
;*
;* last modified July 9, 98

.286
Ideal
Model Small
Public str2array,array2str

Codeseg

Proc	Str2array 	;str	array	offset
	push bp
	mov bp,sp
	push ds si es di

	mov bx,[ss:bp+08]
	mov ax,[ds:bx+02h]
	mov es,ax
	mov di,[ds:bx+0ah]
	mov ax,[ss:bp+06]
	add di,ax
	mov si,[ss:bp+10]
	lodsw
	mov cx,ax
	lodsw
	mov si,ax
	rep movsb

	pop di es si ds bp
	retf 6
endp	Str2array

Proc	Array2str	;array	offset	str
	push bp
	mov bp,sp
	push ds si es di
	
	mov si,[ss:bp+06]
	lodsw
	mov cx,ax
	mov di,[ds:si]
	mov ax,ds
	mov es,ax
	mov bx,[ss:bp+10]
	mov si,[ds:bx+0ah]
	mov ax,[ds:bx+02h]
	mov ds,ax
	mov ax,[ss:bp+08]
	add si,ax
	rep movsb

	pop di es si ds bp
	retf 6
endp	Array2str
end