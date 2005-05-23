;***************************************************************************

;*** Play FM Music through Adlib card
;* This better work...
;* Brian Fisher
;*
;*

.286
Ideal
Model Small
Public ResetFM,FMKeyOn,FMKeyOff,Setvoice,Getvoice
CodeSeg

fmport  dw      0388h

oldb0   db      9 dup (0)

modulat db      0,1,2,8,9,10,16,17,18

noteval dw      172,182,193,205,217,230,243,258,274,290,307,326,345,365,387,410,435,460,489,517,547,580,614,651,1369,1389,1411,1434,1459,1484,1513,1541,1571,1604,1638,1675,2393,2413,2435,2458,2483,2508,2537,2565,2595,2628,2662,2699,3417,3437,3459,3482,3507,3532,3561,3589,3619,3652,3686,3723,4441,4461,4483,4506,4531,4556,4585,4613,4643,4676,4710,4747,5465,5485,5507,5530,5555,5580,5609,5637,5667,5700,5734,5771,6489,6509,6531,6554,6579,6604,6633,6661,6691,6724,6758,6795,7513,7533,7555,7578,7603,7628,7657,7685,7715,7748,7782,7819,7858,7898,7942,7988,8037,8089,8143,8191,8191,8191,8191,8191,8191,8191,8191,8191,8191,8191,8191

Proc    WriteFM         ;al= register   ah= value
	push cx
	mov dx,[cs:fmport]
	out dx,al
	mov cx,6
	rep in al,dx
	mov al,ah
	inc dx
	out dx,al
	mov cx,36
	rep in al,dx
	pop cx
	retn    
endp    writefm

Proc    KeyOn           ;bx= voice      cx= freqnum
	push si
	mov si,offset oldb0
	add si,bx
	mov al,bl
	add al,0a0h
	mov ah,cl
	call writefm
	mov al,bl
	add al,0b0h
	mov ah,ch
	mov [cs:si],ah
	or ah,32
	call writefm
	pop si
	retn
endp    keyon

Proc    KeyOff          ;bx= voice
	push si
	mov si,offset oldb0
	add si,bx
	mov ah,[cs:si]
	mov al,bl
	add al,0b0h
	call writefm
	pop si
	retn
endp    keyoff

proc    setvoic         ;bx= voice      es:di= instrument
	push cx si
	mov si,offset modulat
	add si,bx
	mov cl,[cs:si]
	mov ah,[es:di]
	inc di
	mov al,20h
	add al,cl
	call writefm
	mov ah,[es:di]
	inc di
	mov al,23h
	add al,cl
	call writefm
	mov ah,[es:di]
	inc di
	mov al,40h
	add al,cl
	call writefm
	mov ah,[es:di]
	inc di
	mov al,43h
	add al,cl
	call writefm
	mov ah,[es:di]
	inc di
	mov al,60h
	add al,cl
	call writefm
	mov ah,[es:di]
	inc di
	mov al,63h
	add al,cl
	call writefm
	mov ah,[es:di]
	inc di
	mov al,80h
	add al,cl
	call writefm
	mov ah,[es:di]
	inc di
	mov al,83h
	add al,cl
	call writefm
	mov ah,[es:di]
	inc di
	mov al,0e0h
	add al,cl
	call writefm
	mov ah,[es:di]
	inc di
	mov al,0e3h
	add al,cl
	call writefm
	mov ah,[es:di]
	mov al,0c0h
	add al,bl
	call writefm
	pop si cx
	retn
endp    setvoic
	
Proc    ResetFM
	mov cx,0f5h
clearit:
	mov al,cl
	xor ah,ah
	call writefm
	loop clearit
	mov ax,2001h
	call writefm
	retf
endp    resetfm

Proc    FMKeyOn         ;voice, note
	push bp
	mov bp,sp
	push si

	mov bx,[ss:bp+08]
	mov si,offset noteval
	mov ax,[ss:bp+06]
	shl ax,1
	add si,ax
	mov cx,[cs:si]
	call keyon
	pop si bp
	retf 4
endp    fmkeyon

Proc    SetVoice        ;voice, data
	push bp
	mov bp,sp
	push es di

	mov bx,[ss:bp+06]
	mov ax,[ds:bx+02h]
	mov es,ax
	mov di,[ds:bx+0ah]
	mov bx,[ss:bp+08]
	call setvoic
	pop di es bp
	retf 4
endp    SetVoice

Proc    GetVoice        ;voice, ins, file$, buffer
	push bp
	mov bp,sp
	push ds si es di

	mov si,[ss:bp+08]
	add si,2
	mov dx,[ds:si]
	mov ax,3d00h
	int 21h
	jc nofile
	mov bx,ax
	mov ax,4200h
	xor cx,cx
	mov dx,[ss:bp+10]
	shl dx,4
	add dx,4
	int 21h
	mov cx,bx
	mov bx,[ss:bp+06]
	mov dx,[ds:bx+0ah]
	mov di,[ds:bx+0ah]
	mov ax,[ds:bx+02h]
	mov es,ax
	mov ds,ax
	mov bx,cx
	mov cx,11
	mov ax,3f00h
	int 21h
	push bx
	mov bx,[ss:bp+12]
	call setvoic
	pop bx
	mov ax,3e00h
	int 21h
nofile:
	pop di es si ds bp
	retf 8
endp    GetVoice

Proc    FMKeyOff        ;voice
	push bp
	mov bp,sp

	mov bx,[ss:bp+06]
	call keyoff
	pop bp
	retf 2
endp    fmkeyoff
end
