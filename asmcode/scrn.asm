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
	shl cx,2
	mov ax,[cs:numbytes]
	mul cx
	mov cx,dx
	mov dx,ax
	mov ax,4200h
	int 21h
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
	shl cx,2
	mov ax,[cs:numbytes]
	mul cx
	mov cx,dx
	mov dx,ax
	mov ax,4200h
	int 21h
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

