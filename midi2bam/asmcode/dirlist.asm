;*************************************************************************

;****** directory stuff
;*
;*
;* last modified Feb 18, '98

.286
Ideal
Model Small
Public findfiles, isfile, pathlength, getpath
Codeseg

dta     dw ?
src     dw ?
dest    dw ?
lump    dw ?
wrote   dw ?
found   dw ?
data    dw ?
bseg    dw ?
list    dw ?
listlen dw ?
path    dw ?
amask   dw ?
cpath   db 70 dup (0)
plen    dw ?

Proc    Findfiles       ;fil$, attrib, newfile$, buff()
	push bp
	mov bp,sp
	push ds si es di

	mov [cs:data],ds        ;store data segment
	mov bx,[ss:bp+06]
	mov di,[ds:bx+0ah]
	mov [cs:dta],di
	add di,21
	mov [cs:amask],di
	add di,9
	mov [cs:found],di
	add di,98
	mov [cs:wrote],di       ;store saved file address
	mov ax,[ds:bx+02h]
	mov [cs:bseg],ax        ;store buffer segment
	mov es,ax               ;set es to buffer seg
	mov si,[ss:bp+12]
	mov ds,[cs:data]
	lodsw
	mov [cs:listlen],ax     ;store filemask string length
	mov dx,[ds:si]
	mov [cs:list],dx        ;store pointer to next filemask
momask:
	mov ds,[cs:bseg]
	mov dx,[cs:dta]
	mov ax,1a00h
	int 21h                 ;set dta
	mov ds,[cs:data]
	mov dx,[cs:list]
	mov cx,[ss:bp+10]       ;set attribute mask
	mov ax,4e00h
	int 21h
	jc file                 ;find first file
mofile:
	mov ds,[cs:bseg]
	mov si,[cs:amask]
	mov al,[ds:si]
	and al,[ss:bp+10]
	cmp al,0
	jz badtype              ;compare attribute mask for match
	mov si,[cs:found]       ;ds:si = found file
moletter:
	lodsb
	stosb                   ;write filename to write buffer
	cmp al,0
	jnz moletter
	mov ax,0a0dh
	stosw
badtype:
	mov ax,4f00h
	int 21h
	jnc mofile              ;find next file
	mov ds,[cs:data]
	mov si,[cs:list]
checkit:
	lodsb                   ;load a character
	sub [cs:listlen],1
	jz file                 ;if len = 0 done
	cmp al,0
	jnz checkit             ;if it's not chr$(0), keep looking
	mov [cs:list],si        ;set list pointer to next set
	jmp momask
file:
	mov ds,[cs:data]
	mov si,[ss:bp+08]
	add si,2
	mov dx,[ds:si]
	mov ax,3c00h
	mov cx,0
	int 21h
	jc done
	mov [cs:dest],ax
	mov dx,[cs:wrote]
	mov cx,di
	sub cx,dx
	jz nodata
	mov bx,[cs:dest]
	mov ds,[cs:bseg]
	mov ax,4000h
	int 21h
nodata:
	mov bx,[cs:dest]
	mov ax,3e00h
	int 21h
done:   
	pop di es si ds bp
	retf 8
endp    Findfiles

Proc    isfile          ; filename
	push bp
	mov bp,sp
	push si

	mov si,[ss:bp+06]
	add si,2
	mov dx,[ds:si]
	mov ax,3d00h
	int 21h
	jc notthere
	mov bx,ax
	mov ax,3e00h
	int 21h
	mov ax,-1
	jmp donelook
notthere:
	xor ax,ax
donelook:
	pop si bp
	retf 2
endp    isfile

Proc    pathlength
	push ds si

	mov ax,cs
	mov ds,ax
	lea si,[cs:cpath]
	mov ax,1900h
	int 21h
	add al,41h
	mov [ds:si],al
	inc si
	mov ax,'\:'
	mov [ds:si],ax
	add si,2
	mov ah,47h
	int 21h
	mov cx,65
seek0:
	lodsb   
	cmp al,0
	jz got0
	loop seek0
got0:
	cmp cx,0
	jz nopath
	cmp cx,65
	jz nopath
	dec cx
	mov al,'\'
	mov [ds:si-1],al
nopath:
	mov ax,68
	sub ax,cx
	mov [cs:plen],ax
	pop si ds
	retf
endp    pathlength

proc    getpath         ;string
	push bp
	mov bp,sp
	push ds si es di

	mov si,[ss:bp+06]
	lodsw
	cmp ax,[cs:plen]
	mov cx,ax
	jnz bad
	mov di,[ds:si]
	lea si,[cs:cpath]
	mov ax,ds
	mov es,ax
	mov ax,cs
	mov ds,ax
	rep movsb
bad:
	pop di es si ds bp
	retf 2
endp    getpath
end

