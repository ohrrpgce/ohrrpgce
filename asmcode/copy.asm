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

;****** File manipulation code for installation procedures
;*
;*
.286
Ideal
Model Small
Public Copyfile
Codeseg

dest	dw ?
source	dw ?
wrote	dw ?

Proc    Copyfile        ;fil$, newfile$, buff()
	push bp
	mov bp,sp
	push ds si es di

	mov si,[ss:bp+10]
	add si,2
	mov dx,[ds:si]
	mov ax,3d00h
	int 21h
	jc done
	mov [source],ax
	mov si,[ss:bp+08]
	add si,2
	mov dx,[ds:si]
	mov ax,3c00h
	mov cx,0
	int 21h
	jc close2
	mov [dest],ax
	mov bx,[ss:bp+06]
	mov dx,[ds:bx+0ah]
	mov ax,[ds:bx+02h]
	mov ds,ax
copy:
	mov bx,[source]
	mov cx,08000h
	mov ax,3f00h
	int 21h
	jc close1
	mov cx,ax
	mov bx,[dest]
	mov ax,4000h
	int 21h
	jc close1
	cmp ax,08000h
	jz copy
close1:
	mov bx,[dest]
	mov ax,3e00h
	int 21h
close2:
	mov bx,[source]
	mov ax,3e00h
	int 21h
done:   
	pop di es si ds bp
	retf 6
endp    Copyfile
end

