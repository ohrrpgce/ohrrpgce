; Copyright (c) 1996 Brian Fisher
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
; 1. Redistributions of source code must retain the above copyright
;    notice, this list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in the
;    documentation and/or other materials provided with the distribution.
; 3. The name of the author may not be used to endorse or promote products
;    derived from this software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;*************************************************************************

;****** File manipulation code for installation procedures
;*
;* Created on August 9th, 1999
;*
;* Last Modified July 10th, '02
;* Commented heavily, Fixed the storehalf routine to not use end of file and added the stackbufferPos routine

.286
Ideal
Model Small
Public SetupStack, Pushb, Popb, Pushw, Popw, Pushd, Popd, ReleaseStack, StackPos, StackBufferPos
Codeseg

filebuf dw -1		; handle for the temp file
stackseg dw ?		; segment for the buffer
stackoff dw ?		; offset for the buffer
stacksize dw 0		; size of the stack (Even value)
stackptr dw 0		; index into the stack buffer
filesize dw 0		; size of the file in stackbuf blocks
stackbuf dw 0		; half the size of the tack

Proc	SetupStack 	; Buf(), Size, fil$
	push bp
	mov bp,sp
	push si

	mov bx,[ss:bp+10]
	mov ax,[ds:bx+02h]
	mov [cs:stackseg],ax
	mov ax,[ds:bx+0ah]
	mov [cs:stackoff],ax		; store the buffer address
	mov ax,[ss:bp+08]
	shr ax,1
	mov [cs:stackbuf],ax		; save half the buffer size for overflow stuff
	shl ax,1
	mov [cs:stacksize],ax		; store the buffer size as an even value
	mov si,[ss:bp+06]
	add si,2			; get the null-terminated file name
	mov dx,[ds:si]
	mov ax,3c00h
	xor cx,cx
	mov [cs:stackptr],cx
	mov [cs:filesize],cx		; store the index & buffer count as 0
	int 21h				; create a file
	jc nostack
	mov [cs:filebuf],ax
	mov ax,-1			; return true
	jmp quitsetup
nostack:
	mov [cs:stacksize],0		; failed to create file, set values to prevent stack use
	mov [cs:filebuf],-1
	xor ax,ax			; return false
quitsetup:

	pop si bp
	retf 6
endp	SetupStack

Proc	ReleaseStack
	push bp
	mov bp,sp
	
	cmp [cs:filebuf],-1		; fail if we have no buffer file
	jz nostackfile
	mov bx,[cs:filebuf]
	mov ax,3e00h
	int 21h				; close the buffer file
nostackfile:
	mov [cs:filebuf], -1
	mov [cs:stacksize],0		; set values to prevent stack use

	pop bp
	retf
endp	ReleaseStack

Proc	LoadHalf
	push ax cx dx
	cmp [cs:filesize],0
	jz none2load			; load nothing if the buffer file is empty
	mov ax,[cs:filesize]
	dec ax
	mov [cs:filesize],ax		; decrement the buffer file count
	mul [cs:stackbuf]
	mov cx,dx
	mov dx,ax			; cx:dx is the offset of the last buffer block
	mov ax,4200h
	mov bx,[cs:filebuf]
	int 21h				; set the file pointer
	mov ax,3f00h
	mov bx,[cs:filebuf]
	mov cx,[cs:stackbuf]
	mov dx,[cs:stackoff]
	int 21h				; read the last buffer block in at the buffer start
none2load:
	mov bx,[cs:stackbuf]		; set the index to the halfway point and return in bx
	pop dx cx ax
	retn
endp	LoadHalf

Proc	StoreHalf
	push ax cx dx
	mov ax,[cs:filesize]
	mul [cs:stackbuf]
	mov cx,dx
	mov dx,ax			; cx:dx is the offset for the next buffer block
	mov ax,4200h
	mov bx,[cs:filebuf]
	int 21h				; set the file pointer
	mov ax,4000h
	mov bx,[cs:filebuf]
	mov cx,[cs:stackbuf]
	mov dx,[cs:stackoff]
	int 21h				; write the first half of the buffer
	inc [cs:filesize]		; increment the buffer file count
	push si es di
	mov ax,ds
	mov es,ax
	mov di,[cs:stackoff]		; es:di is the start of the buffer
	mov si,di
	add si,[cs:stackbuf]		; ds:si is the halfway part of the buffer
	mov cx,[cs:stackbuf]
	cld
	rep movsb			; copy the second half to the first hald
	pop di es si
	mov bx,[cs:stackbuf]		; set the index to the halfway point and return in bx
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

Proc	StackBufferPos
	mov ax,[cs:stackptr]		; buffer position is just the ptr
	retf
endp	StackBufferPos

Proc	StackPos
	mov ax,[cs:stackbuf]
	mov bx,[cs:filesize]
	mul bx				; find the size of the stuff in the file buffer
	add ax,[cs:stackptr]
	adc dx, 0			; add the index into the buffer to get the stack position
	retf
endp	StackPos
end