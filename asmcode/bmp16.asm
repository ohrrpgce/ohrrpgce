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
;*** 16 color bmp importer
;* By Brian Fisher
;*
;**** last modifed 11-04-01
;* added extra comments
;* Fixed the bitmap being loaded shifted down 1 pixel
;* Fixed the bitmap being shifted right and cropped 1 pixel

.386
Ideal 
Model Small
Public Loadbmp,getbmppal,bmpinfo

CodeSeg
handl   dw 0
palptr  dw 0
palseg  dw 0
blu     db 0
gree    db 0
re      db 0
reserv  db 0
red     dw 0
green   dw 0
blue    dw 0
diff    dw 0
color   db 0
count   db 0
start   dw 0
splane  db 0
plane   db 0
fsize   dw 0
exitc   dw 0
widt    dw 0
heigh   dw 0

Proc    bmpinfo         ;file   buff
			; 8      6
	push bp
	mov bp,sp
	push ds si

	mov ax,0
	mov [cs:exitc],ax
	mov si,[ss:bp+08]
	add si,2
	mov dx,[ds:si]
	mov ax,3d00h
	int 21h                 ;open file
	jc notthere
	mov [cs:handl],ax
	mov bx,[ss:bp+06]
	mov dx,[ds:bx+0ah]
	mov si,dx
	mov ds,[ds:bx+02h]
	mov cx,38
	mov ax,3f00h
	mov bx,[cs:handl]
	int 21h			;load the bitmap header (38 bytes)
	mov ax,[ds:si]
	cmp ax,19778
	jnz notbmp		;make sure it's a bmp
	mov [cs:exitc],-1
	mov ax,[ds:si+28]	;store the bit depth
	mov [ds:si],ax
	mov ax,[ds:si+18]	;then the width
	mov [ds:si+2],ax
	mov ax,[ds:si+22]	;then the height
	mov [ds:si+4],ax
	mov ax,[ds:si+30]	;finally store some compression type value
	mov [ds:si+8],ax
notbmp:
	mov bx,[cs:handl]
	mov ax,3e00h
	int 21h
notthere:
	mov ax,[cs:exitc]
	pop si ds bp
	retf 4
endp    bmpinfo

Proc    Loadbmp         ;file  xloc   yloc   buff  page
			; 14     12    10     8     6
	push bp
	mov bp,sp
	push ds si es di
	
	mov si,[ss:bp+14]
	add si,2
	mov dx,[ds:si]
	mov ax,3d00h
	int 21h                 ;open file
	jc done
	mov [cs:handl],ax
	mov bx,[ss:bp+08]
	mov dx,[ds:bx+0ah]
	mov si,dx
	mov ds,[ds:bx+02h]
	mov cx,38
	mov ax,3f00h
	mov bx,[cs:handl]
	int 21h			;read the bitmap header
	mov ax,[ds:si]
	cmp ax,19778
	jnz exit
	mov ax,[ds:si+28]
	cmp ax,4
	jnz exit		;verify it's a 16-color (4-bit) bitmap
	mov dx,[ds:si+10]
	mov cx,[ds:si+12]
	mov bx,[cs:handl]
	mov ax,4200h		;move the file pointer to the start of data
	int 21h
	mov ax,[ds:si+18]
	mov [cs:widt],ax
	mov ax,[ds:si+22]
	mov [cs:heigh],ax
	mov ax,[ds:si+30]
	mov [cs:exitc],ax
	mov cx,[ds:si+34]
	mov [cs:fsize],cx	;store the bitmap's properties
	cmp cx,0
	jnz goodsize
	mov cx,-1
goodsize:
	mov bx,[cs:handl]
	mov dx,si
	mov ax,3f00h
	int 21h			;read the bitmap contents
	cmp [cs:fsize],0
	jnz dontgetsize
	mov [cs:fsize],ax	;if the header didn't specify file size, store the bytes read as the size
dontgetsize:
	mov di,[ss:bp+06]
	shl di,14
	mov ax,0a000h
	mov es,ax
	mov ax,[ss:bp+10]
	add ax,[cs:heigh]	
	dec ax			;fix for shift down was to decrement the starting y location
	mov bx,ax
	shl ax,4
	shl bx,6
	add ax,bx
	add di,ax
	mov ax,[ss:bp+12]
	mov bx,ax
	shr bx,2
	add di,bx
	mov [cs:start],di	;store the video offset for the start location
	and ax,3
	mov cx,ax
	mov al,1
	shl al,cl
	mov [cs:splane],al	;store the starting video plane for drawing
	mov [cs:plane],al
	cmp [cs:exitc],0
	jnz morebytes
	call drawstraight
	jmp exit
morebytes:
	lodsw			;load the next data byte
	cmp al,0
	jz escape
	xor cx,cx
	mov cl,al
	mov bl,ah
	shr bl,4
	and ah,15
drawencoded:
	call drawpixel
	xchg ah,bl
	loop drawencoded	;draw the color pairs for the repeat count
	jmp morebytes
escape:
	cmp ah,0		;process the escape code
	jz newline
	cmp ah,1
	jz exit
	cmp ah,2
	jz delta
	xor cx,cx		;not an escape code so it's a word-aligned run
	mov cl,ah
getword:
	xor dx,dx
	lodsw			;grab a word at a time
drawmore:
	mov bl,al
	test dx,1
	jz gethigh
	and bl,15
	mov al,ah
	jmp gotlow
gethigh:
	shr bl,4		;select the correct nibble of the current word
gotlow:
	call drawpixel
	sub cx,1
	jz morebytes		;quit if we are done drawing
	inc dx
	cmp dx,4
	jz getword		;grab another word if we're done with this one..
	jmp drawmore		;otherwise draw the next nibble
delta:
	lodsw
	;CHECK: need finish delta!
	jmp morebytes
newline:
	sub [cs:start],80	;compression code to end the current line..
	mov di,[cs:start]
	mov al,[cs:splane]
	mov [cs:plane],al
	jmp morebytes
exit:
	mov bx,[cs:handl]	;done drawing!
	mov ax,3e00h
	int 21h
done:
	mov ax,[cs:exitc]
	pop di es si ds bp
	retf 10
endp    Loadbmp

proc    drawstraight			;procedure to draw uncompressed bitmaps
	mov cx,[cs:heigh]
	mov dx,[cs:widt]
	xor ax,ax
nextpixel:
	test ah,1
	jnz noloadbyte		;fix for shifting was to change this jz to a jnz..
	lodsb			;load a new byte
	mov bl,al
	shr bl,4		;use the high nibble of a new byte
	jmp gotval
noloadbyte:
	mov bl,al
	and bl,15		;use the low nibble of the last byte
gotval:
	call drawpixel
	inc ah
	and ah,7
	dec dx
	cmp dx,0
	jnz nextpixel
	mov dx,[cs:widt]
	sub [cs:start],80	;move up one row
	mov di,[cs:start]
	mov bl,[cs:splane]
	mov [cs:plane],bl
	cmp ah,0		;ensure dword alignment of source data for each row
	jz noskip
	mov bl,ah
	xor bh,bh
	shr bx,1
	add si,4
	sub si,bx
	xor ax,ax
noskip:
	loop nextpixel
	retn
endp    drawstraight

proc    drawpixel			;select the proper plane, draw the pixel, and increment the location
	push ax dx
	mov ah,[cs:plane]
	mov al,02h
	mov dx,3c4h
	out dx,ax
	mov [es:di],bl
	mov al,ah
	shl al,1
	shr ah,3
	add al,ah
	and al,0fh
	mov [cs:plane],al
	xor ah,ah
	and ax,1
	add di,ax
	pop dx ax
	retn
endp    drawpixel

proc    getbmppal       ;file  master()  pal()  offset
			; 12     10       8       6
	push bp
	mov bp,sp
	push ds si es di
	
	mov si,[ss:bp+12]
	add si,2
	mov dx,[ds:si]
	mov ax,3d00h
	int 21h                 ;open file
	jc done2
	mov [cs:handl],ax
	mov bx,ax
	xor cx,cx
	mov dx,36h
	mov ax,4200h
	int 21h
	mov bx,[ss:bp+08]
	mov di,[ds:bx+0ah]
	mov ax,[ds:bx+02h]
	mov es,ax
	mov ax,[ss:bp+06]
	add di,ax
	mov bx,[ss:bp+10]
	mov ax,[ds:bx+0ah]
	mov [cs:palptr],ax
	mov ax,[ds:bx+02h]
	mov [cs:palseg],ax
	mov [cs:count],16
nextcolor:
	mov cx,4
	mov ax,cs
	mov ds,ax
	lea dx,[cs:blu]
	mov ah,3fh
	mov bx,[cs:handl]
	int 21h                 ;load triple
	mov ds,[cs:palseg]
	mov si,[cs:palptr]
	mov [cs:diff],1000
	xor cx,cx
findcolor:
	xor ah,ah
	mov al,[cs:re]
	mov [cs:red],ax
	mov al,[cs:gree]
	mov [cs:green],ax
	mov al,[cs:blu]
	mov [cs:blue],ax
	lodsw
	shl ax,2
	cmp ax,[cs:red]
	jb low1
	sub ax,[cs:red]
	mov [cs:red],ax
	jmp next1
low1:
	sub [cs:red],ax
next1:
	lodsw
	shl ax,2
	cmp ax,[cs:green]
	jb low2
	sub ax,[cs:green]
	mov [cs:green],ax
	jmp next2
low2:
	sub [cs:green],ax
next2:
	lodsw
	shl ax,2
	cmp ax,[cs:blue]
	jb low3
	sub ax,[cs:blue]
	mov [cs:blue],ax
	jmp next3
low3:
	sub [cs:blue],ax
next3:
	call maxcolor
	cmp [cs:diff],ax
	jb oldcolor
	mov [cs:color],cl
	mov [cs:diff],ax
oldcolor:
	inc cx
	cmp cx,256
	jnz findcolor
	mov al,[cs:color]
	stosb
	mov al,[cs:count]
	dec al
	mov [cs:count],al
	cmp al,0
	ja nextcolor
	mov bx,[cs:handl]
	mov ah,3eh
	int 21h
done2:
	pop di es si ds bp
	retf 8
endp    getbmppal

proc    maxcolor
	mov ax,[cs:red]
	cmp ax,[cs:green]
	ja redgood
	mov ax,[cs:green]
redgood:
	cmp ax,[cs:blue]
	ja notblue
	mov ax,[cs:blue]
notblue:
	mov bx,[cs:red]
	add ax,bx
	mov bx,[cs:green]
	add ax,bx
	mov bx,[cs:blue]
	add ax,bx
	shr ax,2
	retn
endp    maxcolor
end
