;*************************************************************************

;****** Storeset and loadset for direspam
;*
;*
.386
Ideal
Model Small
Public Storeset,loadset,setpicstuf
Codeseg
planetouse  db ?
buffoff     dw ?
buffseg     dw ?
numbytes    dw ?
pageoff     dw 0ffffh

Proc	setpicstuf	;buffer, bytes, page
	push bp
	mov bp,sp
	push ds

	mov bx,[ss:bp+10]
	mov ax,[ds:bx+0ah]
	mov [cs:buffoff],ax
	mov ax,[ds:bx+02h]
	mov [cs:buffseg],ax
	mov ax,[ss:bp+08]
	mov [cs:numbytes],ax
	mov [cs:pageoff],0ffffh
	mov ax,[ss:bp+06]
	and ax,ax
	js noscreen			;check to see if is just file
	shl ax,14
	mov [cs:pageoff],ax
noscreen:
	pop ds bp
	retf 6
endp	setpicstuf

Proc    Storeset	;fil$, index, line
	push bp
	mov bp,sp
	push ds si es di

	mov si,[ss:bp+10]
	add si,2
	mov dx,[ds:si]			;ds:dx=filename
	mov ah,3dh
	mov al,2        
	int 21h				;open file
	jnc doit
	mov ah,3ch
	mov cx,0
	int 21h				;make if none
	jc done
doit:
	mov bx,ax			;get file number in bx
	mov cx,[ss:bp+08]
	cmp cx,0
	jz storeit
	mov ax,[cs:numbytes]
	mul cx
	mov cx,dx
	mov dx,ax
	mov ax,4200h
	int 21h				;set file pointer to desired record
storeit:
	cmp [cs:pageoff],0ffffh
	jz juststor			;skip if only memory
	mov cl,0
	mov [cs:planetouse], cl
	mov ax,[cs:buffseg]
	mov es,ax
	mov di,[cs:buffoff]
	mov ax,0a000h
	mov ds,ax 
	mov ax,[ss:bp+06]
	mov cx,50h
	mul cx
	mov si,[cs:pageoff]
	add si,ax
	mov dx,03ceh
	mov cx,[cs:numbytes]		;set all kinds of graphics stuff
copylines:
	mov al,04h
	mov ah,[cs:planetouse]
	out dx,ax			;set plane to read
	mov al,[ds:si]
	stosb				;get single byte
	inc ah
	mov al,ah
	and ah,3
	shr al,2
	mov [cs:planetouse],ah
	xor ah,ah
	add si,ax
	loop copylines			;transfers screen to record in order
juststor:
	mov cx,[cs:numbytes]
	mov ax,[cs:buffseg]
	mov ds,ax
	mov dx,[cs:buffoff]
	mov ah,40h
	int 21h				;store it
	mov ah,3eh
	int 21h				;close it
done:   
	pop di es si ds bp
	retf 6
endp    storeset

Proc    Loadset 	;fil$, index, line
	push bp
	mov bp,sp
	push ds si es di

	mov si,[ss:bp+10]
	add si,2
	mov dx,[ds:si]			;set ds:dx to filename
	mov ah,3dh
	mov al,0
	int 21h				;open file
	jc nofile
	mov bx,ax			;set bx to file number
	mov cx,[ss:bp+08]
	cmp cx,0
	jz loadit
	mov ax,[cs:numbytes]
	mul cx
	mov cx,dx
	mov dx,ax
	mov ax,4200h
	int 21h				;set file offset
loadit:
	mov ax,[cs:buffseg]
	mov ds,ax
	mov dx,[cs:buffoff]
	mov cx,[cs:numbytes]
	mov ah,3fh
	int 21h				;load to buffer
	cmp [cs:pageoff],0ffffh
	jz justload			;check to see if just memory
	mov ax,0a000h
	mov es,ax
	mov di,[cs:pageoff]
	mov ax,[ss:bp+06]
	mov cx,50h
	mul cx
	mov si,[cs:buffoff]
	add di,ax
	mov cx,[cs:numbytes]
	mov ah,1
	mov [cs:planetouse],ah
	mov dx,3c4h			;set graphics stuff
drawit:
	mov al,02h
	mov ah,[cs:planetouse]
	out dx,ax			;set plane write enable
	lodsb
	mov [es:di],al
	mov al,ah
	shr al,3
	shl ah,1
	add ah,al
	and ah,0fh
	mov [cs:planetouse],ah
	xor ah,ah
	add di,ax
	loop drawit			;draw what's in buffer in order
justload:
	mov ah,3eh
	int 21h				;close file
nofile:   
	pop di es si ds bp
	retf 6
endp    loadset
end

