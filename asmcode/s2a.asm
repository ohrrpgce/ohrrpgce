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

;****** String to Array and Array to String
;*
;*
;* last modified July 9, 98

.286
Ideal
Model Small
Public str2array,array2str

Codeseg

Proc	Str2array 	;str	array	offset
	push bp
	mov bp,sp
	push ds si es di

	mov bx,[ss:bp+08]
	mov ax,[ds:bx+02h]
	mov es,ax
	mov di,[ds:bx+0ah]
	mov ax,[ss:bp+06]
	add di,ax
	mov si,[ss:bp+10]
	lodsw
	mov cx,ax
	lodsw
	mov si,ax
	rep movsb

	pop di es si ds bp
	retf 6
endp	Str2array

Proc	Array2str	;array	offset	str
	push bp
	mov bp,sp
	push ds si es di
	
	mov si,[ss:bp+06]
	lodsw
	mov cx,ax
	mov di,[ds:si]
	mov ax,ds
	mov es,ax
	mov bx,[ss:bp+10]
	mov si,[ds:bx+0ah]
	mov ax,[ds:bx+02h]
	mov ds,ax
	mov ax,[ss:bp+08]
	add si,ax
	rep movsb

	pop di es si ds bp
	retf 6
endp	Array2str
end