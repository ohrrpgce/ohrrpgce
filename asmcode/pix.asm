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
;*** putpixel and readpixel for mode x
;* By Brian Fisher
;*
;* modifed April 23, 1998

.286
Ideal 
Model Small
Public Putpixel,Readpixel,Rectangle,Fuzzyrect,Drawline,Paintat

CodeSeg

xloc    dw 0
yloc    dw 0
x2      dw 0
y2      dw 0
wid     dw 0
height  dw 0
middle  db 0
mov1    dw 0
mov2    dw 0
dmask   db 0
rmask	db 0

Proc Putpixel   ;  Xloc,  Yloc,  Color, page
		;   12     10      8      6
	push bp
	mov bp,sp
	push es di
	
	mov ax,[ss:bp+10]
	mov bx,ax
	shl ax,6
	shl bx,4
	add ax,bx
	mov bx,[ss:bp+12]
	shr bx,2
	add ax,bx
	mov di,ax
	mov ax,[ss:bp+06]
	shl ax,14
	add di,ax
	mov cx,[ss:bp+12]
	and cl,3
	mov bl,1
	shl bl,cl
	mov dx,03c4h
	mov al,02h
	mov ah,bl
	out dx,ax
	mov ax,0a000h
	mov es,ax
	mov ax,[ss:bp+08]
	mov [es:di],al

	pop di es bp
	retf 8
endp    Putpixel

Proc    Readpixel  ;  Xloc,  Yloc,  page
		   ;   10      8     6
	push bp
	mov bp,sp
	push ds si
	
	mov ax,[ss:bp+08]
	mov bx,ax
	shl ax,6
	shl bx,4
	add ax,bx
	mov bx,[ss:bp+10]
	shr bx,2
	add ax,bx
	mov si,[ss:bp+06]
	shl si,14
	add si,ax
	mov cx,[ss:bp+10]
	and cl,3
	mov dx,03ceh
	mov al,04h
	mov ah,cl
	out dx,ax
	mov ax,0a000h
	mov ds,ax
	mov al,[ds:si]
	xor ah,ah

	pop si ds bp
	retf 6
endp    Readpixel

Proc    Rectangle       ;x, y, width, height, color, page
	push bp
	mov bp,sp
	push es ds si di

	cld
	xor ax,ax
	mov [cs:middle],al
	mov ax,[ss:bp+14]       ;figure out the starting vga offset
	cmp ax,199
	jg bady
	mov bx,[ss:bp+10]
	cmp bx,0
	jz bady
	mov cx,ax
	add cx,bx
	cmp cx,0
	js bady
	cmp ax,0
	jns notnegy
	mov bx,cx
	mov ax,0
	jmp goodheight
notnegy:
	cmp cx,200
	jng goodheight
	mov bx,200
	sub bx,ax
	jmp goodheight
bady:
	jmp rectdone
goodheight:
	mov [cs:yloc],ax
	mov [cs:height],bx
	mov bx,ax
	shl ax,6
	shl bx,4
	add ax,bx
	mov bx,[ss:bp+16]       ;figure out the starting vga offset
	cmp bx,319
	jg badx
	mov dx,[ss:bp+12]
	cmp dx,0
	jz badx
	mov cx,bx
	add cx,dx
	cmp cx,0
	js badx
	cmp bx,0
	jns notnegx
	mov dx,cx
	mov bx,0
	jmp goodwidth
notnegx:
	cmp cx,320
	jng goodwidth
	mov dx,320
	sub dx,bx
	jmp goodwidth
badx:
	jmp rectdone
goodwidth:
	mov [cs:xloc],bx
	mov [cs:wid],dx
	shr bx,2
	add bx,ax
	mov ax,[ss:bp+06]
	shl ax,14
	add bx,ax
	mov si,bx
	mov ax,0a000h           ;set up es
	mov es,ax
	mov cx,[cs:xloc]
	and cx,3
	cmp cx,0
	jz nextblock
	xor ah,ah
	mov al,1
	shl al,cl
	mov bx,cx
	mov cx,4
	sub cx,bx
	cmp cx,[cs:wid]
	jb goodsize
	mov cx,[cs:wid]
goodsize:
	sub [cs:wid],cx
fillah:
	shl ah,1
	add ah,al
	loop fillah
setdraw:
	mov al,02h
	mov dx,03c4h
	out dx,ax
xlop:
	mov di,si
	xor dx,dx
	mov al,[ss:bp+08]
ylop:
	mov [es:di],al
	add di,50h
	inc dx
	cmp dx,[cs:height]
	jb ylop
	inc si
nextblock:
	mov cx,[cs:wid]
	cmp cx,0
	jz rectdone
	cmp cx,3
	ja doblock
	mov ah,0
	mov al,1
	mov [cs:wid],0
	jmp fillah
doblock:
	sub [cs:wid],4
	mov ah,0fh
	cmp [cs:middle],ah
	jz xlop
	mov [cs:middle],ah
	jmp setdraw
rectdone:
	pop di si ds es bp
	retf 12
endp    rectangle

Proc    Fuzzyrect       ;x, y, width, height, color, page
	push bp
	mov bp,sp
	push es ds si di

	cld
	mov ax,[ss:bp+14]       ;figure out the starting vga offset
	mov bx,ax
	shl ax,6
	shl bx,4
	add ax,bx
	mov bx,[ss:bp+16]
	shr bx,2
	add bx,ax
	mov ax,[ss:bp+06]
	shl ax,14
	add bx,ax
	mov si,bx
	mov bx,[ss:bp+10]
	mov bh,bl
	mov bl,10
	xor cx,cx
	mov ax,0a000h           ;set up es
	mov es,ax
nexline:
	mov di,si
	mov al,02h              ;set the plane write enable
	mov dx,03c4h
	mov ah,bl
	out dx,ax
	mov al,[ss:bp+08]
	mov [es:di],al
	mov cx,[ss:bp+12]
	add cx,3
	shr cx,2
	rep stosb
	add si,50h
	xor bl,0fh
	dec bh
	cmp bh,0
	jnz nexline
	pop di si ds es bp
	retf 12
endp    fuzzyrect

proc    drawline        ;x1 y1 x2 y2 c p
	push bp
	mov bp,sp
	push es di

	mov cx,[ss:bp+12]
	mov [cs:x2],cx
	mov ax,[ss:bp+16]
	mov [cs:xloc],ax
	sub cx,ax
	mov [cs:wid],cx
	mov dx,[ss:bp+10]
	mov [cs:y2],dx
	mov bx,[ss:bp+14]
	mov [cs:yloc],bx
	sub dx,bx
	mov [cs:height],dx
	cmp ax,0
	jnl nlx1                        ;correct bad x's & y's
	cmp [cs:wid],0
	jg fix1
	jmp noline
fix1:
	imul [cs:height]
	idiv [cs:wid]
	sub [cs:yloc],ax
	mov [cs:xloc],0
	jmp gdx1
nlx1:
	cmp ax,319
	jng gdx1
	cmp [cs:wid],0
	jl fix2
	jmp noline
fix2:
	sub ax,319
	imul [cs:height]
	idiv [cs:wid]
	sub [cs:yloc],ax
	mov [cs:xloc],319
gdx1:
	mov ax,[cs:yloc]
	cmp ax,0
	jnl nly1                        ;correct bad x's & y's
	cmp [cs:height],0
	jg fix3
	jmp noline
fix3:
	imul [cs:wid]
	idiv [cs:height]
	sub [cs:xloc],ax
	mov [cs:yloc],0
	jmp gdy1
nly1:
	cmp ax,199
	jng gdy1
	cmp [cs:height],0
	jl fix4
	jmp noline
fix4:
	sub ax,199
	imul [cs:wid]
	idiv [cs:height]
	sub [cs:xloc],ax
	mov [cs:yloc],199
gdy1:
	mov ax,[cs:x2]
	cmp ax,0
	jnl nlx2                        ;correct bad x2's
	imul [cs:height]
	idiv [cs:wid]
	sub [cs:y2],ax
	mov [cs:x2],0
	jmp gdx2
nlx2:
	cmp ax,319
	jng gdx2
	sub ax,319
	imul [cs:height]
	idiv [cs:wid]
	sub [cs:y2],ax
	mov [cs:x2],319
gdx2:
	mov ax,[cs:yloc]
	cmp ax,0
	jnl nly2                        ;correct bad x's & y's
	imul [cs:wid]
	idiv [cs:height]
	sub [cs:x2],ax
	mov [cs:y2],0
	jmp gdy2
nly2:
	cmp ax,199
	jng gdy2
	sub ax,199
	imul [cs:wid]
	idiv [cs:height]
	sub [cs:x2],ax
	mov [cs:y2],199
gdy2:
	mov ax,0a000h
	mov es,ax
	mov ax,[cs:yloc]
	mov bx,ax
	shl ax,4
	shl bx,6
	add ax,bx
	mov bx,[cs:xloc]
	shr bx,2
	add bx,ax
	mov di,[ss:bp+06]
	shl di,14
	add di,bx
	mov cx,[cs:xloc]
	and cx,3
	mov ah,1
	shl ah,cl
	mov [cs:dmask],ah
	mov al,02h
	mov dx,03c4h
	out dx,ax
	mov [es:di],al
	lea dx,[moveright]
	mov ax,[cs:x2]
	sub ax,[cs:xloc]
	jns gor
	neg ax
	lea dx,[moveleft]
gor:
	mov [cs:mov1],dx
	shl ax,1
	mov [cs:wid],ax
	lea dx,[movedown]
	mov ax,[cs:y2]
	sub ax,[cs:yloc]
	jns god
	neg ax
	lea dx,[moveup]
god:
	mov [cs:mov2],dx
	shl ax,1
	mov [cs:height],ax
	mov bx,[cs:wid]
	cmp ax,bx
	jna okaynow
	mov [cs:wid],ax
	mov [cs:height],bx
	mov ax,[cs:mov1]
	mov bx,[cs:mov2]
	mov [cs:mov1],bx
	mov [cs:mov2],ax
okaynow:
	add [cs:wid],2
	mov cx,[cs:wid]
	shr cx,1
	xor dx,dx
	sub dx,cx
	mov ax,[ss:bp+08]
doline:
	mov [es:di],al
	add dx,[cs:height]
	cmp dx,0
	jl noymove
	sub dx,[cs:wid]
	call [cs:mov2]
noymove:
	call [cs:mov1]
	loop doline
noline:
	pop di es bp
	retf 12
endp    drawline

proc    moveright
	push ax dx
	mov ah,[cs:dmask]
	shl ah,1
	cmp ah,16
	jb nogor
	mov ah,1
	inc di  
nogor:
	mov [cs:dmask],ah
	mov al,02h
	mov dx,03c4h
	out dx,ax
	pop dx ax
	retn
endp    moveright

proc    moveleft
	push ax dx
	mov ah,[cs:dmask]
	shr ah,1
	cmp ah,0
	jnz nogol
	mov ah,8
	dec di
nogol:
	mov [cs:dmask],ah
	mov al,02h
	mov dx,03c4h
	out dx,ax
	pop dx ax
	retn
endp    moveleft

proc    movedown
	add di,80
	retn
endp    movedown

proc    moveup
	sub di,80
	retn
endp    moveup

Proc    Paintat    ;x, y, color, page, buffer, max
	push bp
	mov bp,sp
	push ds si es di

	mov [cs:rmask],-1
	mov [cs:dmask],-1
	mov bx,[ss:bp+08]
	mov ax,[ds:bx+02h]
	mov cx,[ds:bx+0ah]
	mov si,cx
	mov di,cx
	add cx,[ss:bp+06]
	mov [cs:wid],cx
	mov es,ax
	mov ds,ax
	mov ax,[ss:bp+14]
	cmp ax,0
	jl badpxy
	cmp ax,199
	jg badpxy
	mov bx,320
	mul bx
	mov bx,[ss:bp+16]
	cmp bx,0
	jl badpxy
	cmp bx,319
	jg badpxy
	jmp goodpxy
badpxy:
	jmp donepaint
goodpxy:
	add ax,bx
	mov bx,[ss:bp+10]
	shl bx,14
	mov [cs:height],bx
	mov cx,ax
	and ax,3
	mov [cs:rmask],al
	mov ah,al
	mov al,4
	mov dx,03ceh
	out dx,ax
	mov ax,0a000h
	mov ds,ax
	mov bx,cx
	shr bx,2
	add bx,[cs:height]
	mov al,[ds:bx]
	mov [cs:middle],al
	cmp al,[ss:bp+12]
	jz donepaint
	mov ax,cx
	call dopixel
paintloop:
	cmp si,di
	jz donepaint
	mov ax,es
	mov ds,ax
	lodsw
	cmp si,[cs:wid]
	jb okayload
	sub si,[ss:bp+06]
okayload:
	mov cx,ax
	mov ax,0a000h
	mov ds,ax
	mov ax,cx
	mov bx,320
	xor dx,dx
	div bx
	mov bx,ax
	mov ax,cx
	cmp dx,0
	jz nopleft
	sub ax,1
	call dopixel
	mov ax,cx
nopleft:
	cmp dx,319
	jz nopright
	add ax,1
	call dopixel
	mov ax,cx
nopright:
	cmp bx,0
	jz nopup
	sub ax,320
	call dopixel
	mov ax,cx
nopup:
	cmp bx,199
	jz nopaint
	add ax,320
	call dopixel
nopaint:
	jmp paintloop
donepaint:
	pop di es si ds bp
	retf 12
endp    Paintat

Proc	dopixel
	push dx cx bx ax
	stosw
	mov bx,ax
	mov ah,bl
	shr bx,2
	add bx,[cs:height]
	and ah,3
	cmp ah,[cs:rmask]
	jz noset
	mov dx,03ceh
	mov al,04h
	out dx,ax
	mov [cs:rmask],ah
noset:
	mov al,[ds:bx]
	cmp al,[cs:middle]
	jz goodone
	sub di,2
	jmp doneone
goodone:
	mov ah,1
	mov cl,[cs:rmask]
	shl ah,cl
	cmp ah,[cs:dmask]
	jz nosetd
	mov dx,03c4h
	mov al,02h
	out dx,ax
	mov [cs:dmask],ah
nosetd:
	mov al,[ss:bp+12]
	mov [ds:bx],al
	cmp di,[cs:wid]
	jb doneone
	sub di,[ss:bp+06]
doneone:
	pop ax bx cx dx
	retn
endp	dopixel
end
