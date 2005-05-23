;*************************************************************************

;****** Storescrn and loadscrn, for saving VGA memory to disk
;*
;*
.286
Ideal
Model Small
Public Storepage,Loadpage,setdiskpages
Codeseg
planetouse  db ?
buffoff     dw ?
buffseg     dw ?
numbytes    dw 16000
numword     dw 8000
loadline    dw 0

Proc	Setdiskpages	;buffer,lines,line
	push bp
	mov bp,sp
	push ds

	mov bx,[ss:bp+10]
	mov ax,[ds:bx+0ah]
	mov [cs:buffoff],ax
	mov ax,[ds:bx+02h]
	mov [cs:buffseg],ax
	mov ax,[ss:bp+08]
	mov bx,ax
	shl ax,6
	shl bx,4
	add ax,bx
	mov [cs:numbytes],ax
	shr ax,1
	mov [cs:numword],ax
	mov ax,[ss:bp+06]
	mov bx,ax
	shl ax,6
	shl bx,4
	add ax,bx
	mov [cs:loadline],ax

	pop ds bp
	retf 6
endp	setdiskpages

Proc    Storepage       ;fil$, index, page
	push bp
	mov bp,sp
	push ds si es di

	mov si,[ss:bp+10]
	add si,2
	mov dx,[ds:si]
	mov ah,3dh
	mov al,2        
	int 21h
	jnc storeit
	mov ah,3ch
	mov cx,0
	int 21h
	jc done
storeit:
	mov bx,ax
	mov cx,[ss:bp+08]
	cmp cx,0
	jz page0
	xor si,si
	xor dx,dx
	mov ax,[numbytes]
	shl ax,2
calcpoint:
	add dx,ax
	jnc fine
	inc si
fine:
	loop calcpoint
	mov cx,si
	mov ax,4200h
	int 21h
page0:
	mov cl,0
	mov [cs:planetouse], cl
	mov ax,[cs:buffseg]
	mov es,ax
planes:
	mov dx,03ceh
	mov al,04h
	mov ah,[cs:planetouse]
	out dx,ax
	mov ax,0a000h
	mov ds,ax
	mov di,[cs:buffoff]
	mov si,[ss:bp+06]
	shl si,14
	add si,[cs:loadline]
	mov cx,[cs:numword]
	rep movsw
	mov cx,[cs:numbytes]
	mov ax,es
	mov ds,ax
	mov dx,[cs:buffoff]
	mov ah,40h
	int 21h
	mov cl,[cs:planetouse]
	inc cl
	mov [cs:planetouse],cl
	cmp cl,4
	jb planes
	mov ah,3eh
	int 21h
done:   
	pop di es si ds bp
	retf 6
endp    storepage

Proc    Loadpage       ;fil$, index, page
	push bp
	mov bp,sp
	push ds si es di

	mov si,[ss:bp+10]
	add si,2
	mov dx,[ds:si]
	mov ah,3dh
	mov al,0
	int 21h
	jc nofile
	mov cl,1
	mov [cs:planetouse], cl
	mov bx,ax
	mov cx,[ss:bp+08]
	cmp cx,0
	jz first
	xor si,si
	xor dx,dx
	mov ax,[numbytes]
	shl ax,2
calcoff:
	add dx,ax
	jnc under
	inc si
under:
	loop calcoff
	mov cx,si
	mov ax,4200h
	int 21h
first:
	mov ax,[cs:buffseg]
	mov ds,ax
	mov ax,0a000h
	mov es,ax
	cld
drawit:
	mov dx,3c4h
	mov al,02h
	mov ah,[cs:planetouse]
	out dx,ax
	mov di,[ss:bp+06]
	shl di,14
	add di,[cs:loadline]
	mov dx,[cs:buffoff]
	mov si,dx
	mov cx,[cs:numbytes]
	mov ah,3fh
	int 21h
	mov cx,[cs:numword]
	rep movsw
	mov cl,[cs:planetouse]
	shl cl,1
	mov [cs:planetouse],cl
	cmp cl,16
	jb drawit
	mov ah,3eh
	int 21h
nofile:   
	pop di es si ds bp
	retf 6
endp    loadpage
end

