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
;***************************************************************************

;***** Playsnd and Recsnd - For playing and recording voices to mem.
;*
;*

.286
Ideal
Model Small

macro   DOWRITE
local   waitloop, doneloop
	push cx
	xor cx,cx
waitloop:
	in al,dx
	or al,al
	jns doneloop
	sub cx,1
	jnz waitloop
doneloop:
	mov al,ah
	out dx,al
	pop cx
endm

macro   DOREAD
local   waitloop, doneloop
	push cx
	xor cx,cx
waitloop:
	in al,dx
	or al,al
	js doneloop
	sub cx,1
	jnz waitloop
doneloop:
	sub dx,4
	in al,dx
	pop cx
endm

Public Setitup,Resetdsp,Playsnd,Savesnd,Closefile
Codeseg

reset   dw 226h
read    dw 22ah
write   dw 22ch
data    dw 22eh
oldsize dw 0
buffseg dw 0
buffoff dw 0
buffadd dw 0
buffpag db 0
tempseg dw 0
tempoff dw 0
filenum dw 0
voiclo  dw 0
voichi  dw 0
len     dw 0
sampnum db 0
header  db 'CBVS '

Proc    Setitup ; file$ buff temp base 
	push bp
	mov bp,sp
	push ds si

	mov bx,[ss:bp+08]
	mov ax,[ds:bx+02h]
	mov [cs:tempseg],ax
	mov ax,[ds:bx+0ah]
	mov [cs:tempoff],ax
	mov bx,[ss:bp+10]
	mov ax,[ds:bx+02h]
	mov [cs:buffseg],ax
	mov cx,[ds:bx+0ah]
	mov [cs:buffoff],cx
	mov bx,ax
	shl bx,4
	add bx,cx
	shr cx,4
	add cx,ax
	shr cx,12               ;bx=offset cl=page
	mov [cs:buffadd],bx
	mov [cs:buffpag],cl

	cmp [cs:filenum],0
	jnz dne
	mov si,[ss:bp+12]
	add si,2
	mov dx,[ds:si]
	mov ax,3d02h
	int 21h
	jnc ok
	cmp ax,02h
	jnz no
	mov ax,3c00h
	mov cx,0
	int 21h
	jc no
	mov [cs:filenum],ax
	mov bx,ax
	mov cx,6
	mov ax,cs
	mov ds,ax
	lea dx,[cs:sampnum]
	mov [cs:sampnum],0
	mov ax,4000h
	int 21h
	mov dx,[cs:buffoff]
	mov ax,[cs:buffseg]
	mov ds,ax
	mov cx,1530
	mov ax,4000h
	int 21h
	jmp dne
ok:
	mov [cs:filenum],ax
	mov cx,1
	mov ax,cs
	mov ds,ax
	lea dx,[cs:sampnum]
	mov ax,3f00h
	int 21h
dne:
	mov ax,[ss:bp+06]
	shl ax,4
	mov dx,0206h
	add dx,ax
	mov [reset],dx
	mov dx,20ah
	add dx,ax
	mov [read],dx
	mov dx,20ch
	add dx,ax
	mov [write],dx
	mov dx,20eh
	add dx,ax
	mov [data],dx
no:
	pop si ds bp
	retf 8
endp    setitup
	
Proc    resetDsp
	cmp [cs:filenum],0
	jz nosb
	xor cx,cx
	mov dx,[reset]
	mov al,1
	out dx,al
	in al,dx
	in al,dx
	in al,dx
	in al,dx
	xor al,al
	out dx,al
waitforit:
	mov dx,[data]
	add cx,1
	jc nosb
	in al,dx
	or al,al
	jns waitforit
	sub dx,4
	in al,dx
	cmp al,0AAh
	jne waitforit
	mov ax,1
	jmp resetted
nosb:
	xor cx,cx
	xor ax,ax
	mov [cs:filenum],0
resetted:
	retf 0
endp    resetdsp

proc    Playsnd  ; which, frequency
	push bp
	mov bp,sp
	push ds si es di

	cmp [cs:filenum],0
	ja play
	jmp noplay
play:	
	mov dx,[ss:bp+08]       ;get file address
	mov bx,dx
	shl dx,1
	shl bx,2
	add dx,bx
	xor cx,cx
	mov ax,4200h
	mov bx,[cs:filenum]
	int 21h
	mov ax,cs
	mov ds,ax
	lea dx,[cs:voiclo]
	mov cx,6
	mov ax,3f00h
	int 21h
	mov dx,[cs:voiclo]
	mov cx,[cs:voichi]
	mov ax,4200h
	int 21h
	mov ax,[cs:tempseg]
	mov ds,ax
	mov dx,[cs:tempoff]
	mov cx,[cs:len]
	mov ax,3f00h
	int 21h
	
	mov dx,[write]
	mov ah,0d0h
	DOWRITE
	mov ah,0d1h
	DOWRITE		;Stop the voice

	in al,03h
	mov bl,al
	in al,03h
	mov bh,al		
	inc bx		;bx=count left to play
	mov cx,[cs:oldsize]
	sub cx,bx
	mov si,[cs:buffoff]
	add si,cx	;si= buffer pointer (last 4 bits)
	mov ax,[cs:buffseg]
	mov ds,ax	;ds:si = start of old sound
	mov es,ax	;es:di = buffer start
	mov di,[cs:buffoff]
	mov cx,bx	;cx= count left to play
	cmp cx,0
	jz nosound	;skip is count is zero
	rep movsb	;move count left to play bytes from ds:si to es:di
nosound:
	mov cx,[cs:len]
	cmp bx,cx	
	jnb littler	;skip if length of sample is below or equal to count
	sub cx,bx	;take difference between count
	mov ax,128
	rep stosb	;fill difference bytes of es:di with 128
	mov bx,[cs:len] ;bx= length of sample
littler:
	mov cx,[cs:len]
	mov ax,[cs:tempseg]
	mov ds,ax
	mov ax,[cs:buffseg]
	mov es,ax
	mov si,[cs:tempoff]
	mov di,[cs:buffoff]	;ds:si= temp start, es:di= buffer start
	xor ax,ax
	xor dx,dx
mixit:
	mov dl,[es:di]
	lodsb
	add ax,dx
	sub ax,128
	jns isokay
	mov ax,0
isokay:
	cmp ax,255
	jb isgood
	mov ax,255
isgood:
	stosb
	loop mixit

	mov [cs:oldsize],bx
	mov cx,bx
	sub cx,1
	mov al,5                ;start up the voice
	out 0ah,al              ;mask channel 1
	mov al,0
	out 0ch,al              ;clr byte ptr
	mov al,49h
	out 0bh,al              ;playback on channel 1
	mov bx,[cs:buffadd]     ;get address
	mov al,bl
	out 02h,al              ;send low byte of offset
	mov al,bh
	out 02h,al              ;send high byte
	mov al,cl               ;send size
	out 03h,al              ;send low byte
	mov al,ch
	out 03h,al              ;send high byte
	mov al,[cs:buffpag]     ;get page
	out 83h,al              ;send page
	mov al,1  
	out 0ah,al              ;unmask channel 1
	mov dx,[write]
	mov ah,40h              ;playback
	DOWRITE
	mov ah,[ss:bp+06]       ;frequency
	DOWRITE
	mov ah,14h              ;send size
	DOWRITE
	mov ah,cl               ;low byte length
	DOWRITE
	mov ah,ch               ;high byte length
	DOWRITE
noplay:
	pop di es si ds bp
	retf 4
endp    playsnd

proc    savesnd ; vocfil which
	push bp
	mov bp,sp
	push ds si

	cmp [cs:filenum],0
	ja save
	jmp nosave
save:
	mov si,[bp+08]
	add si,2
	mov dx,[ds:si]
	mov ax,3d02h
	int 21h
	mov bx,ax
	mov dx,27
	xor cx,cx
	mov ax,4200h
	int 21h
	mov ax,cs
	mov ds,ax
	lea dx,[cs:len]
	mov cx,2
	mov ax,3f00h
	int 21h
	xor cx,cx
	mov dx,47
	mov ax,4200h
	int 21h
	mov ax,[cs:buffseg]
	mov ds,ax
	mov dx,[cs:buffoff]
	mov cx,[cs:len]
	sub cx,17
	mov [cs:len],cx
	mov si,cx
	mov ax,3f00h
	int 21h
	mov ax,3e00h
	int 21h
	mov bx,[cs:filenum]
	mov ax,cs
	mov ds,ax
	mov dx,[ss:bp+06]
	cmp dx,1
	ja no1
	xor cx,cx
	mov [cs:voichi],cx
	mov dx,6
	mov ax,4200h
	int 21h
	mov ax,1536
	mov [cs:voiclo],ax
	jmp noa
no1:
	sub dx,1
	mov ax,dx
	shl ax,1
	shl dx,2
	add dx,ax
	xor cx,cx
	mov ax,4200h
	int 21h
	mov cx,6
	lea dx,[cs:voiclo]
	mov ax,3f00h
	int 21h
	mov ax,[cs:len]
	add [cs:voiclo],ax
	jnc noa
	inc [cs:voichi]
noa:
	lea dx,[cs:voiclo]
	mov [cs:len],si
	mov cx,6
	mov ax,4000h
	int 21h
	mov dx,[cs:voiclo]
	mov cx,[cs:voichi]
	mov ax,4200h
	int 21h
	mov ax,[cs:buffseg]
	mov ds,ax
	mov dx,[cs:buffoff]
	mov cx,[cs:len]
	mov ax,4000h
	int 21h
nosave:
	pop si ds bp
	retf 4
endp    savesnd

proc    closefile
	cmp [cs:filenum],0
	jz nofile
	mov ax,3e00h
	mov bx,[cs:filenum]
	int 21h
	mov [cs:filenum],0
nofile:
	retf 0
endp    closefile
end
