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

;***** MOUSE.ASM 
;*
;* 
.286
Ideal
Model Small
Public setmouse,readmouse,movemouse
codeseg

Proc    Setmouse        ;buff
	push bp
	mov bp,sp
	push es di
 
	mov bx,[ss:bp+06]
	mov es,[ds:bx+02h]
	mov di,[ds:bx+0ah]
	mov ax,0
	mov [es:di],ax
	int 33h
	cmp ax,0
	je nomouse
	and bx,3
	mov [es:di],bx
	mov cx,4
	mov dx,9
	mov ax,15
	int 33h
	mov cx,318
	mov dx,88
	mov ax,4
	int 33h
nomouse:
	pop di es bp
	retf 2
endp    setmouse

Proc    readmouse        ;buff
	push bp
	mov bp,sp
	push es di
 
	mov bx,[ss:bp+06]
	mov es,[ds:bx+02h]
	mov di,[ds:bx+0ah]
	mov ax,3
	int 33h
	cmp cx,638
	jb xok
	mov cx,636
	mov ax,4
	int 33h
xok:
	cmp dx,199
	jb yok
	mov dx,198
	mov ax,4
	int 33h
yok:
	mov ax,cx
	shr ax,1
	stosw
	mov ax,dx
	stosw
	mov ax,5
	mov bx,0
	int 33h
	stosw
	mov ax,bx
	stosw
	pop di es bp
	retf 2
endp    readmouse

proc 	movemouse	;x, y
	push bp
	mov bp,sp
	
	mov ax,4
	mov cx,[ss:bp+08]
	shl cx,1
	mov dx,[ss:bp+06]
	int 33h
	
	pop bp
	retf 4
endp 	movemouse
End
		
