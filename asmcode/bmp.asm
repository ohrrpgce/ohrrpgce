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
;*** Bitmap to Mode X Graphics page convertor
;* By Brian Fisher
;*
;*
.386
Ideal 
Model Small
Public Bitmap2page

CodeSeg
palseg  dw 0
palptr  dw 0
plane   db 0
handle  dw 0
bpart   dw 0
count   dw 0
widt	dw 0
blu     db 0
gree    db 0
re      db 0
blue    dw 0
green   dw 0
red     dw 0
color   db 0
diff    dw 0

Proc    bitmap2page     ;pal()  file    page
			; 10     8       6
	push bp
	mov bp,sp
	push ds si es di
	
	mov si,[ss:bp+08]
	add si,2
	mov dx,[ds:si]
	mov ax,3d00h
	int 21h                 ;open file
	jnc goon
	jmp done
goon:
	mov [handle],ax
	mov bx,ax
	mov ax,4200h
	xor cx,cx
	mov dx,10
	int 21h                 ;set pointer to 10th byte
	mov bx,[handle]
	push ds
	mov ax,cs
	mov ds,ax
	lea dx,[bpart]
	mov cx,2
	mov ah,3fh
	int 21h                 ;get start point
	mov ax,4200h
	xor cx,cx
	mov dx,[bpart]
	int 21h                 ;set file pointer
	pop ds
	mov bx,[ss:bp+10]
	mov ax,[ds:bx+0ah]
	mov [palptr],ax         ;set pointers
	mov si,ax
	mov ax,[ds:bx+02h]
	mov [palseg],ax
	mov ax,0a000h
	mov es,ax
	mov [plane],1
	mov ax,[ss:bp+06]       ;set graphics pointer
	shl ax,14
	mov di,ax
	add di,15920
	mov [count],0
	mov [widt],0
drawpixel:
	mov ah,[plane]
	mov dx,03c4h
	mov al,02h
	out dx,ax               ;set plane write enable
	mov bx,[handle]
	mov cx,3
	mov ax,cs
	mov ds,ax
	lea dx,[blu]
	mov ah,3fh
	int 21h                 ;load triple
	mov ds,[palseg]
	mov si,[palptr]
	mov [diff],1000
	xor cx,cx
findcolor:
	xor ah,ah
	mov al,[re]
	mov [red],ax
	mov al,[gree]
	mov [green],ax
	mov al,[blu]
	mov [blue],ax
	lodsw
	shl ax,2
	cmp ax,[red]
	jb low1
	sub ax,[red]
	mov [red],ax
	jmp next1
low1:
	sub [red],ax
next1:
	lodsw
	shl ax,2
	cmp ax,[green]
	jb low2
	sub ax,[green]
	mov [green],ax
	jmp next2
low2:
	sub [green],ax
next2:
	lodsw
	shl ax,2
	cmp ax,[blue]
	jb low3
	sub ax,[blue]
	mov [blue],ax
	jmp next3
low3:
	sub [blue],ax
next3:
	call maxcolor
	cmp [diff],ax
	jb oldcolor
	mov [color],cl
	mov [diff],ax
oldcolor:
	inc cx
	cmp cx,256
	jnz findcolor
	mov al,[color]
	mov [es:di],al
	xor ah,ah
	mov al,[plane]
	mov bx,ax
	shl ax,1
	shr bx,3
	add di,bx
	add ax,bx
	and ax,0fh
	mov [plane],al
	inc [widt]
	cmp [widt],320
	jnz notyet
	mov [widt],0
	sub di,160
notyet:
	inc [count]
	cmp [count],64000
	jz drawn
	jmp drawpixel
drawn:
	mov ah,3eh
	mov bx,[handle]
	int 21h
done:
	pop di es si ds bp
	retf 6
endp    bitmap2page

proc    maxcolor
	mov ax,[red]
	cmp ax,[green]
	ja redgood
	mov ax,[green]
redgood:
	cmp ax,[blue]
	ja notblue
	mov ax,[blue]
notblue:
        mov bx,[red]
        add ax,bx
        mov bx,[green]
        add ax,bx
        mov bx,[blue]
        add ax,bx
        shr ax,2
	retn
endp    maxcolor
end
