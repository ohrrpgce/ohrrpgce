;*************************************************************************

;****** File manipulation code for installation procedures
;*
;*
.286
Ideal
Model Small
Public Copyfile
Codeseg

dest	dw ?
source	dw ?
wrote	dw ?

Proc    Copyfile        ;fil$, newfile$, buff()
	push bp
	mov bp,sp
	push ds si es di

	mov si,[ss:bp+10]
	add si,2
	mov dx,[ds:si]
	mov ax,3d00h
	int 21h
	jc done
	mov [source],ax
	mov si,[ss:bp+08]
	add si,2
	mov dx,[ds:si]
	mov ax,3c00h
	mov cx,0
	int 21h
	jc close2
	mov [dest],ax
	mov bx,[ss:bp+06]
	mov dx,[ds:bx+0ah]
	mov ax,[ds:bx+02h]
	mov ds,ax
copy:
	mov bx,[source]
	mov cx,08000h
	mov ax,3f00h
	int 21h
	jc close1
	mov cx,ax
	mov bx,[dest]
	mov ax,4000h
	int 21h
	jc close1
	cmp ax,08000h
	jz copy
close1:
	mov bx,[dest]
	mov ax,3e00h
	int 21h
close2:
	mov bx,[source]
	mov ax,3e00h
	int 21h
done:   
	pop di es si ds bp
	retf 6
endp    Copyfile
end

