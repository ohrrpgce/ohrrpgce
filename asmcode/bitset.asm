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

;***** BYBYTE.ASM 
;*
;* 
.286
Ideal
Model Small
Public setbit,readbit
codeseg

Proc    Setbit                  ;buff, word #, bit #,value
	push bp
	mov bp,sp
	push es di
 
	mov bx,[ss:bp+12]
	mov es,[ds:bx+02h]
	mov di,[ds:bx+0ah]
	mov ax,[ss:bp+10]
	shl ax,1
	add di,ax
	mov cx,[ss:bp+08]
	mov ax,cx
	and cx,15
	shr ax,4
	shl ax,1
	add di,ax
	mov bx,1
	shl bx,cl
	mov ax,[es:di]
	mov cx,[ss:bp+06]
	cmp cx,0
	jz clear
	or ax,bx
	jmp isset
clear:
	not bx
	and ax,bx
isset:
	mov [es:di],ax

	pop di es bp
	retf 8
endp    setbit

proc    readbit                ;buff, word #, bit #
	push bp
	mov bp,sp
	push ds si

	mov bx,[ss:bp+10]
	mov si,[ds:bx+0ah]
	mov ds,[ds:bx+02h]
	mov ax,[ss:bp+08]
	shl ax,1
	add si,ax
	mov bx,1
	mov cx,[ss:bp+06]
	mov ax,cx
	and cx,15
	shr ax,4
	shl ax,1
	add si,ax
	shl bx,cl
	lodsw
	and ax,bx
	shr ax,cl

	pop si ds bp
	retf 6
endp    readbit
End
		
