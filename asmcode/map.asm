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
;*****************************************************************************

;*** Overlay stuff
;* This should work okay...
;* Brian Fisher
;*
;* modified Mar 11 '99 last made sure map offsets all work

.386
Ideal
Model Small
Public setmapdata,setmapblock,readmapblock,setpassblock,readpassblock,drawmap,setanim,setoutside
CodeSeg

mapx    dw ?
mapy    dw ?
topbar  dw 0
botbar  dw 200
mapoff  dw ?
mapseg  dw ?
passoff dw ?
passseg dw ?
write   db ?
awrit   db ?
read    db 0
howmany dw ?
lines   dw ?
extra   dw 0
blocks  dw 0
screen  dw 0
rowscr  dw 0
gap     dw 0
widt    dw 0
pixels  dw 0
sizev   dw 0
rot1    dw 0
rot2    dw 0
drawm   db 0
drawit  db 0
xpos    dw 0
ypos    dw 0
outside db 0
todraw  db 0

Proc    Setoutside      ; outside block num, -1 is wrap
	push bp
	mov bp,sp

	mov ax,[ss:bp+06]
	mov [cs:outside],al
	pop bp
	retf 2
endp    setoutside

Proc    Setmapdata      ; array, pass, top bar, bottom
	push bp
	mov bp,sp
	push ds si

	mov bx,[ss:bp+10]
	mov ax,[ds:bx+0ah]
	add ax,4
	mov [cs:passoff],ax
	mov ax,[ds:bx+02h]
	mov [cs:passseg],ax
	mov bx,[ss:bp+12]
	mov si,[ds:bx+0ah]
	mov ds,[ds:bx+02h]
	lodsw
	mov [cs:mapx],ax
	lodsw
	mov [cs:mapy],ax
	mov [cs:mapseg],ds
	mov [cs:mapoff],si
	mov ax,[ss:bp+08]
	mov [cs:topbar],ax         ;topbar= lines to move down
	mov bx,200
	sub bx,ax
	mov ax,[ss:bp+06]
	sub bx,ax
	mov [cs:botbar],bx         ;Botbar= lines to draw
	pop si ds bp
	retf 8
endp    Setmapdata

Proc    SetMapBlock     ; X, Y, Value
	push bp
	mov bp,sp
	push es di
	
	mov ax,[cs:mapseg]
	mov es,ax
	mov di,[cs:mapoff]
	mov ax,[ss:bp+08]
	mov bx,[cs:mapx]
	mul bx
	mov bx,[ss:bp+10]
	add ax,bx
	add di,ax
	mov ax,[ss:bp+06]
	stosb

	pop di es bp
	retf 6
endp    SetMapBlock

Proc    ReadMapBlock    ; X, Y
	push bp
	mov bp,sp
	push ds si

	mov ax,[cs:mapseg]
	mov ds,ax
	mov si,[cs:mapoff]
	mov ax,[cs:mapx]
	mov bx,[ss:bp+06]
	mul bx
	mov bx,[ss:bp+08]
	add ax,bx
	add si,ax
	lodsb
	xor ah,ah

	pop si ds bp
	retf 4
endp    ReadMapBlock

Proc    SetPassBlock     ; X, Y, Value
	push bp
	mov bp,sp
	push es di
	
	mov ax,[cs:passseg]
	mov es,ax
	mov di,[cs:passoff]
	mov ax,[ss:bp+08]
	mov bx,[cs:mapx]
	mul bx
	mov bx,[ss:bp+10]
	add ax,bx
	add di,ax
	mov ax,[ss:bp+06]
	stosb

	pop di es bp
	retf 6
endp    SetPassBlock

Proc    ReadPassBlock    ; X, Y
	push bp
	mov bp,sp
	push ds si

	mov ax,[cs:passseg]
	mov ds,ax
	mov si,[cs:passoff]
	mov ax,[cs:mapx]
	mov bx,[ss:bp+06]
	mul bx
	mov bx,[ss:bp+08]
	add ax,bx
	add si,ax
	lodsb
	xor ah,ah

	pop si ds bp
	retf 4
endp    ReadPassBlock
	
Proc    Setanim         ; 1, 2
	push bp
	mov bp,sp

	mov ax,[ss:bp+08]
	mov [cs:rot1],ax
	mov ax,[ss:bp+06]
	mov [cs:rot2],ax

	pop bp
	retf 4
endp    Setanim

Proc    Drawmap        ; X, Y, trans, Page
	push bp
	mov bp,sp
	push es di ds si

	mov ax,[ss:bp+08]
	mov [cs:drawm],al
	mov ax,0a000h
	mov es,ax
	mov ax,[ss:bp+06]       ;get dest. offset
	shl ax,14
	mov [cs:rowscr],ax         ;set screen to offset for screen
	mov ax,80
	mul [cs:topbar]
	add [cs:rowscr],ax         ;move down for top status bar
	mov ax,[ss:bp+10]
	add ax,[cs:topbar]
	mov bx,20
	cwd
	idiv bx
	cmp dx,0
	jge notnegystart
	add dx,20
	dec ax
notnegystart:
	mov [cs:ypos],ax           ;set in-map y position
	sub bx,dx
	mov [cs:howmany],bx        ;set howmany for top row
	mov cx,dx
	mov bx,cx
	shl bx,4
	shl cx,6
	add cx,bx
	mov [cs:extra],cx          ;set pixel adjustment to read blocks for first row
	mov ax,[ss:bp+12]
	mov cx,20
	cwd
	idiv cx
	cmp dx,0
	jge notnegxstart
	add dx,20
	dec ax
notnegxstart:
	mov [cs:xpos],ax           ;set in-map x position
	mov [cs:gap],20
	sub cx,dx
	mov [cs:widt],0
	cmp cx,20
	jz nosides
	mov [cs:gap],dx
	mov [cs:widt],cx
nosides:
	mov ax,[cs:botbar]
	mov [cs:lines],ax
	mov cx,[cs:widt]
	mov ax,1
	and cx,3
	shl al,cl
	mov [cs:awrit],al
loop4:          ;set up a column
	push [cs:xpos]
	mov ax,[cs:rowscr]
	mov [cs:screen],ax
	mov [cs:blocks],16
	mov bx,[cs:widt]
	cmp bx,0
	jz loop3
	mov [cs:blocks],17
loop3:          ;set up a new block
	call near calcblock
	mov al,[cs:todraw]
	mov di,[cs:screen]
	xor ah,ah
	cmp ax,160
	jb noanim
	cmp ax,207
	ja type2
	sub ax,160
	add ax,[cs:rot1]
	jmp noanim
type2:
	sub ax,208
	add ax,[cs:rot2]
noanim:
	mov bx,es
	mov ds,bx
	mov si,49152
	mov cx,ax
	and ax,15
	mov bx,5
	mul bx
	add si,ax               ;add blocknumber mod 16 times 5 to the reading screen
	shr cx,4
	mov ax,1600
	mul cx
	add si,ax               ;add blocknumber div 16 times 1600
	add si,[cs:extra]          ;add for starting row
	mov al,[cs:awrit]
	mov [cs:write],al
nodrawb:
	mov ax,20
	mov [cs:read],0
	cmp [cs:blocks],17
	jnz normalblock
	mov ax,[cs:gap]
	mov bx,ax
	and ax,3
	mov [cs:read],al           ;if it's the left edge, start read from
	inc ah                  ;the right place
	mov [cs:write],ah          ;start write from the left edge
	shr bx,2                
	add si,bx
	mov ax,[cs:widt]
normalblock:
	cmp [cs:blocks],1
	jnz notlastinrow
	mov ax,[cs:gap]
notlastinrow:
	mov [cs:pixels],ax
	cmp [cs:drawit],0
	jz skipthisone
	call near drawablock            ;draw the block
skipthisone:
	mov ax,[cs:pixels]
	shr ax,2
	add [cs:screen],ax
	inc [cs:xpos]
	dec [cs:blocks]
	cmp [cs:blocks],0
	jz donerow                 ;do another block
	jmp loop3
donerow:
	pop [cs:xpos]
	mov ax,80
	mul [cs:howmany]
	add [cs:rowscr],ax
	add [cs:ypos],1
	mov [cs:extra],0
	mov ax,[cs:howmany]
	sub [cs:lines],ax
	jna donewithcenter
	mov [cs:howmany],20
	cmp [cs:lines],20
	ja notlist
	mov ax,[cs:lines]
	mov [cs:howmany],ax
notlist:
	jmp loop4               ;do another row
donewithcenter:
	pop si ds di es bp
	retf 8
endp    Drawmap

proc    calcblock
	mov al,[cs:outside]
	mov [cs:drawit],1
	mov [cs:todraw],al
	mov ax,[cs:ypos]
	cwd
	idiv [cs:mapy]
	cmp dx,0
	jge ypositive
	add dx,[cs:mapy]
	dec ax
ypositive:
	mov cx,dx
	cmp ax,0
	jz insidey
	cmp [cs:outside],-1
	jnz goodb
insidey:
	mov ax,[cs:xpos]
	cwd
	idiv [cs:mapx]
	cmp dx,0
	jge xpositive
	add dx,[cs:mapx]
	dec ax
xpositive:
	mov bx,dx
	cmp ax,0
	jz insidex
	cmp [cs:outside],-1
	jnz goodb
insidex:
	mov ax,cx
	mul [cs:mapx]
	add bx,ax
	mov ds,[cs:mapseg]
	mov si,[cs:mapoff]
	add si,bx
	mov al,[ds:si]          ;read map blocknumber to draw
	mov [cs:todraw],al
	cmp [cs:drawm],0        ;check if using overlay
	jz goodb
	mov ds,[cs:passseg]
	mov si,[cs:passoff]
	add si,bx
	mov al,[ds:si]
	and al,128
	add al,[cs:drawm]
	cmp al,130
	jz goodb
	cmp al,1
	jz goodb
	mov [cs:drawit],0       ;set to no drawing if block is overlay
goodb:
	retn
endp    calcblock

proc    drawablock      ; si:read start of block
	push si di
	mov ax,[cs:pixels]
	add ax,3        
	mov [cs:sizev],ax  
loop2:                
	mov al,02h
	mov ah,[cs:write]
	mov dx,03c4h
	out dx,ax               ;set the write plane enable
	mov dx,3ceh
	mov al,04h
	mov ah,[cs:read]
	out dx,ax               ;set the read plane enable
	cmp [cs:pixels],20
	jnz edge
	mov cx,[cs:howmany]
	push si di
drawlinefourth:
	movsd
	movsb
	add si,75
	add di,75
	loop drawlinefourth
	pop di si
	jmp nonehere
edge:
	mov dx,[cs:howmany]
	mov bx,[cs:sizev]
	shr bx,2
	cmp bx,0
	jz nonehere
	push si di
loop1:          ;draw a fourth of a block
	mov cx,bx
	rep movsb
	mov ax,80
	sub ax,bx
	add si,ax
	add di,ax
	sub dx,1
	jnz loop1
	pop di si
nonehere:
	mov al,[cs:write]
	mov bl,al
	shr al,3
	xor ah,ah
	add di,ax
	shl bl,1
	and bx,0fh
	add ax,bx
	mov [cs:write],al
	mov al,[cs:read]
	inc al
	cmp al,4
	jb goodread
	inc si
	mov al,0
goodread:
	mov [cs:read],al
	dec [cs:sizev]
	mov ax,[cs:sizev]
	cmp ax,[cs:pixels]
	jnb loop2                ;do another set o' columns
	pop di si
	retn
endp    drawablock
end
 
