;***************************************************************************

;***** BYBYTE.ASM 
;*
;* 
.286
Ideal
Model Small
Public setbit,readbit
codeseg

Proc    Setbit                  ;buff, word #, bit #,value
	push bp
	mov bp,sp
	push es di
 
	mov bx,[ss:bp+12]
	mov es,[ds:bx+02h]
	mov di,[ds:bx+0ah]
	mov ax,[ss:bp+10]
	shl ax,1
	add di,ax
	mov cx,[ss:bp+08]
	mov ax,cx
	and cx,15
	shr ax,4
	shl ax,1
	add di,ax
	mov bx,1
	shl bx,cl
	mov ax,[es:di]
	mov cx,[ss:bp+06]
	cmp cx,0
	jz clear
	or ax,bx
	jmp isset
clear:
	not bx
	and ax,bx
isset:
	mov [es:di],ax

	pop di es bp
	retf 8
endp    setbit

proc    readbit                ;buff, word #, bit #
	push bp
	mov bp,sp
	push ds si

	mov bx,[ss:bp+10]
	mov si,[ds:bx+0ah]
	mov ds,[ds:bx+02h]
	mov ax,[ss:bp+08]
	shl ax,1
	add si,ax
	mov bx,1
	mov cx,[ss:bp+06]
	mov ax,cx
	and cx,15
	shr ax,4
	shl ax,1
	add si,ax
	shl bx,cl
	lodsw
	and ax,bx
	shr ax,cl

	pop si ds bp
	retf 6
endp    readbit
End
		
