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

;*** Timing routines using interrupt 15h function 83h
;* used in terms of microseconds
;* Brian Fisher
;*
;*

.286
Ideal
Model Small
Public setwait,dowait
CodeSeg

cseg	dw ?
coff	dw ?
timechk	dw ?
time	dw ?
step 	dw 2



Proc    SetWait         ; var(), number of 1/1024 seconds
			;    8        6
	push bp
	mov bp,sp
	push es di

	mov bx,[ss:bp+08]
	mov es,[ds:bx+02h]
	mov bx,[ds:bx+0ah]
	mov [cs:cseg],es
	mov [cs:coff],bx
	mov di,bx
	mov ax,0
	mov [es:di],ax
	mov ax,976
	mov cx,[ss:bp+06]
	mul cx
	mov cx,dx
	mov dx,ax
	mov ax,8300h
	int 15h
	mov ax,040h
	mov es,ax
	mov ax,[ss:bp+06]
	mov bx,55
	xor dx,dx
	div bx
	add ax,[cs:step]
	add ax,[es:06ch]
	mov [cs:time],ax
	mov ax,[es:06ch]
	mov [cs:timechk],ax
	pop di es bp
	retf 4
endp    SetWait

proc	Dowait
	push bp
	mov bp,sp
	push ds es di

	mov es,[cs:cseg]
	mov di,[cs:coff]
	mov ax,040h
	mov ds,ax
motime:
	mov ax,[ds:06ch]
	cmp ax,[cs:timechk]
	jl done
	cmp ax,[cs:time]
	jnb clock
	mov ax,[es:di]
	cmp ax,0
	jz motime
	mov [cs:step],2
	jmp done
clock:
	mov [cs:step],0
done:
	pop di es ds bp
	retf 0
endp 	Dowait
end
