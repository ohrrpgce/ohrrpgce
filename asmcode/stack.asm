;*************************************************************************

;****** File manipulation code for installation procedures
;*
;* Created on August 9th, 1999
;* Supported byte, word and dword ops correctly

.286
Ideal
Model Small
Public SetupStack, Pushb, Popb, Pushw, Popw, Pushd, Popd, ReleaseStack, StackPos
Codeseg

filebuf dw -1
stackseg dw ?
stackoff dw ?
stacksize dw 0
stackptr dw 0
filesize dw 0
stackbuf dw 0

Proc	SetupStack 	; Buf(), Size, fil$
	push bp
	mov bp,sp
	push si

	mov bx,[ss:bp+10]
	mov ax,[ds:bx+02h]
	mov [cs:stackseg],ax
	mov ax,[ds:bx+0ah]
	mov [cs:stackoff],ax
	mov ax,[ss:bp+08]
	shr ax,1
	mov [cs:stackbuf],ax
	shl ax,1
	mov [cs:stacksize],ax
	mov si,[ss:bp+06]
	add si,2
	mov dx,[ds:si]
	mov ax,3c00h
	xor cx,cx
	mov [cs:stackptr],cx
	mov [cs:filesize],cx
	int 21h
	jc nostack
	mov [cs:filebuf],ax
	mov ax,-1
	jmp quitsetup
nostack:
	mov [cs:stacksize],0
	mov [cs:filebuf],-1
	xor ax,ax
quitsetup:

	pop si bp
	retf 6
endp	SetupStack

Proc	ReleaseStack
	push bp
	mov bp,sp
	
	cmp [cs:filebuf],-1
	jz nostackfile
	mov bx,[cs:filebuf]
	mov ax,3e00h
	int 21h
nostackfile:
	mov [cs:filebuf], -1
	mov [cs:stacksize],0

	pop bp
	retf
endp	ReleaseStack

Proc	LoadHalf
	push ax cx dx
	cmp [cs:filesize],0
	jz none2load
	mov ax,[cs:filesize]
	dec ax
	mov [cs:filesize],ax
	mul [cs:stackbuf]
	mov cx,dx
	mov dx,ax
	mov ax,4200h
	mov bx,[cs:filebuf]
	int 21h
	mov ax,3f00h
	mov bx,[cs:filebuf]
	mov cx,[cs:stackbuf]
	mov dx,[cs:stackoff]
	int 21h
none2load:
	mov bx,[cs:stackbuf]
	pop dx cx ax
	retn
endp	LoadHalf

Proc	StoreHalf
	push ax cx dx
	mov ax,4202h
	mov bx,[cs:filebuf]
	xor cx,cx
	xor dx,dx
	int 21h
	mov ax,4000h
	mov bx,[cs:filebuf]
	mov cx,[cs:stackbuf]
	mov dx,[cs:stackoff]
	int 21h
	inc [cs:filesize]
	push si es di
	mov ax,ds
	mov es,ax
	mov di,[cs:stackoff]
	mov si,di
	add si,[cs:stackbuf]
	mov cx,[cs:stackbuf]
	cld
	rep movsb
	pop di es si
	mov bx,[cs:stackbuf]
	pop dx cx ax
	retn
endp	StoreHalf

Proc	Pushnbytes	;pushes al on to the stack
	cmp [cs:filebuf],-1
	jz badpush
	push ds si
	and cx,3
	inc cx
	mov bx,[cs:stackseg]
	mov ds,bx
	mov si,[cs:stackoff]
	mov bx,[cs:stackptr]
pushbyte:
	mov [ds:si+bx],al
	shr ax,8
	mov ah,dl
	shr dx,8
	inc bx
	cmp bx,[cs:stacksize]
	jnz inlimits
	call StoreHalf
inlimits:
	loop pushbyte
	mov [cs:stackptr],bx

	pop si ds	
badpush:
	retn
endp	Pushnbytes
	
Proc	Popnbytes		;pushes al off of the stack
	xor ax,ax
	xor dx,dx
	cmp [cs:filebuf],-1
	jz badpop
	push ds si
	mov bx,[cs:stackseg]
	mov ds,bx
	mov bx,[cs:stackptr]
	mov si,[cs:stackoff]
	and cx,3
	inc cx
popbyte:	
	cmp bx,0
	jnz havestack
	call LoadHalf
havestack:
	shl dx,8
	mov dl,ah
	shl ax,8
	dec bx
	mov al,[ds:si+bx]
	loop popbyte
	mov [cs:stackptr],bx
	pop si ds
badpop:
	retn
endp	Popnbytes
	
Proc	Pushb 		;byte
	push bp
	mov bp,sp
	mov ax,[ss:bp+06]
	mov cx,0
	Call Pushnbytes

	pop bp
	retf 2
endp	Pushb

Proc	Popb
	mov cx,0
	Call Popnbytes
	retf
endp	Popb

Proc	Pushw 		;word
	push bp
	mov bp,sp
	xor dx,dx
	mov ax,[ss:bp+06]
	mov cx,1
	Call Pushnbytes

	pop bp
	retf 2
endp	Pushw

Proc	Popw
	mov cx,1
	Call Popnbytes
	retf
endp	Popw

Proc	Pushd 		;dword
	push bp
	mov bp,sp
	mov ax,[ss:bp+06]
	mov dx,[ss:bp+08]
	mov cx,3
	Call Pushnbytes

	pop bp
	retf 4
endp	Pushd

Proc	Popd
	mov cx,3
	Call Popnbytes
	retf
endp	Popd

Proc	StackPos
	mov ax,[cs:stackptr]
	retf
endp	StackPos
end