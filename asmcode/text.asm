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
;****************************************************************************
;*
;*
;*** text routines
;* By Brian Fisher
;*
;* last modified jan 21, 98

.286
Ideal 
Model Small
Public printstr,textcolor,setfont

CodeSeg

plane   db 0 
forec   db 0
backc   db 0
fonto   dw 0
fonts   dw 0
messo   dw 0
messs   dw 0
len     dw 0

proc    printstr        ; str$, xloc, yloc, page
	push bp
	mov bp,sp
	push ds si es di
	
	mov [cs:messs],ds
	mov si,[ss:bp+12]
	mov ax,0a000h
	mov es,ax
	mov ax,[ss:bp+08]       ;figure out the starting vga offset
	mov bx,ax
	shl ax,6
	shl bx,4
	add ax,bx
	mov bx,[ss:bp+10]
	cmp bx,320
	jb goodx
	jmp nostring
goodx:
	shr bx,2
	add bx,ax
	mov ax,[ss:bp+06]
	shl ax,14
	add bx,ax
	mov di,bx
	mov cx,[ss:bp+10]       ;figure out the plane of the first pixel
	and cl,3
	mov bx,1
	shl bl,cl
	mov [cs:plane],bl
	lodsw
	mov [cs:len],ax
	cmp ax,0
	jnz goodsize
	jmp nostring
goodsize:
	mov bx,[ss:bp+10]
	shr bx,3
	add ax,bx
	cmp ax,40
	jb insize
	mov ax,40
	sub ax,bx
	mov [cs:len],ax
insize:
	mov ax,[ds:si]
	mov [cs:messo],ax
	add [cs:len],ax
letter:
	mov ds,[cs:messs]
	mov si,[cs:messo]
	mov al,[ds:si]
	xor ah,ah
	shl ax,3
	mov ds,[cs:fonts]
	mov si,[cs:fonto]
	add si,ax
	mov cx,8
row:
	mov al,02h
	mov dx,03c4h
	mov ah,[cs:plane]
	out dx,ax
	mov ah,1
column:
	mov al,[ds:si]
	and al,ah
	cmp al,0
	mov al,[cs:backc]
	jz off
	mov al,[cs:forec]
off:
	cmp al,0
	jz done
	mov [es:di],al
done:
	add di,50h
	shl ah,1
	cmp ah,0
	ja column
	sub di,0280h
	mov al,[cs:plane]
	shl al,1
	mov bx,ax
	shr bl,4
	add al,bl
	and al,15
	mov [cs:plane],al
	add di,bx
	add si,1
	loop row
	add [cs:messo],1
	mov ax,[cs:len]
	cmp [cs:messo],ax
	jb letter
nostring:
	pop di es si ds
	pop bp  
	retf 8
endp    printstr

proc    textcolor       ;fore, back
	push bp
	mov bp,sp
	
	mov ax,[ss:bp+06]
	mov [cs:backc],al
	mov ax,[ss:bp+08]
	mov [cs:forec],al
	
	pop bp
	retf 4
endp    textcolor               

proc    setfont         ;font
	push bp
	mov bp,sp
	
	mov bx,[ss:bp+06]
	mov ax,[ds:bx+0ah]
	mov [word cs:fonto],ax
	mov ax,[ds:bx+02h]
	mov [word cs:fonts],ax

	pop bp
	retf 2
endp    setfont
end
