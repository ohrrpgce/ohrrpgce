;***************************************************************************

;***** MOUSE.ASM 
;*
;* 
.286
Ideal
Model Small
Public setmouse,readmouse,movemouse
codeseg

Proc    Setmouse        ;buff
	push bp
	mov bp,sp
	push es di
 
	mov bx,[ss:bp+06]
	mov es,[ds:bx+02h]
	mov di,[ds:bx+0ah]
	mov ax,0
	mov [es:di],ax
	int 33h
	cmp ax,0
	je nomouse
	and bx,3
	mov [es:di],bx
	mov cx,4
	mov dx,9
	mov ax,15
	int 33h
	mov cx,318
	mov dx,88
	mov ax,4
	int 33h
nomouse:
	pop di es bp
	retf 2
endp    setmouse

Proc    readmouse        ;buff
	push bp
	mov bp,sp
	push es di
 
	mov bx,[ss:bp+06]
	mov es,[ds:bx+02h]
	mov di,[ds:bx+0ah]
	mov ax,3
	int 33h
	cmp cx,638
	jb xok
	mov cx,636
	mov ax,4
	int 33h
xok:
	cmp dx,199
	jb yok
	mov dx,198
	mov ax,4
	int 33h
yok:
	mov ax,cx
	shr ax,1
	stosw
	mov ax,dx
	stosw
	mov ax,5
	mov bx,0
	int 33h
	stosw
	mov ax,bx
	stosw
	pop di es bp
	retf 2
endp    readmouse

proc 	movemouse	;x, y
	push bp
	mov bp,sp
	
	mov ax,4
	mov cx,[ss:bp+08]
	shl cx,1
	mov dx,[ss:bp+06]
	int 33h
	
	pop bp
	retf 4
endp 	movemouse
End
		
