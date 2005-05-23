;***************************************************************************

;*** Play FM Music through Adlib card
;* This better work...
;* Brian Fisher
;*
;*  modified january 8th

.286
Model Small
Public ResetFM,FMKeyOn,FMKeyOff,Setvoice,Getvoice,Setupmusic,closemusic,loadsong,stopsong,resumesong,fademusic,getfmvol,setfmvol
CodeSeg

timer   dw      0,0

buffseg dw      ?
buffptr dw      ?

songptr dw      ?
gosub   db      0
oldptr  dw      ?

tagset  dw      1
tagptr  dw      ?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?
repptr  dw      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

tempo   db      ?

tickc   db      ?
playc   db      0

songon  db      0

fmport  dw      0388h

oldb0   db      0,0,0,0,0,0,0,0,0

modulat db      0,1,2,8,9,10,16,17,18

insnum  db      ?

volume  db      15
fader   db      0

insport db      20h,23h,40h,43h,60h,63h,80h,83h,0e0h,0e3h

oldsb	db	0

noteval dw      172,182,193,205,217,230,243,258,274,290,307,326,345,365,387,410,435,460,489,517,547,580,614,651,1369,1389,1411,1434,1459,1484,1513,1541,1571,1604,1638,1675,2393,2413,2435,2458,2483,2508,2537,2565,2595,2628,2662,2699,3417,3437,3459,3482,3507,3532,3561,3589,3619,3652,3686,3723,4441,4461,4483,4506,4531,4556,4585,4613,4643,4676,4710,4747,5465,5485,5507,5530,5555,5580,5609,5637,5667,5700,5734,5771,6489,6509,6531,6554,6579,6604,6633,6661,6691,6724,6758,6795,7513,7533,7555,7578,7603,7628,7657,7685,7715,7748,7782,7819,7858,7898,7942,7988,8037,8089,8143,8191,8191,8191,8191,8191,8191,8191,8191,8191,8191,8191,8191

Proc    WriteFM         ;al= register   ah= value
			;writes to the adlib card
	push cx ax
	mov dx,[cs:fmport]
	mov cx,36
	rep in al,dx
	pop ax
	out dx,al
	mov cx,6
	rep in al,dx
	mov al,ah
	inc dx
	out dx,al
	mov dx,[cs:fmport]
	mov cx,36
	rep in al,dx
	pop cx
	retn    
endp    writefm

Proc    KeyOn           ;bx= voice      cx= freqnum
			;turns on a voice at a frequency
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
			;turns off a voice
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

proc    setvol
			;sets the music volume
	mov dx,224h
	mov al,26h
	out dx,al
	mov al,ah
	shl ah,4
	add al,ah
	inc dx
	out dx,al
	retn
endp    setvol

proc    getvol
			;gets the fm volume from the mixer
	mov dx,224h
	mov al,26h
	out dx,al
	inc dx
	in al,dx
	and ax,15
	retn
endp    getvol

proc    musicplayer
			;The Interrupt Handler
			;traps the timer interrupt 1ch
	push ax bx cx dx ds si es di
	mov ds,[cs:buffseg]
	mov si,[cs:songptr]
	cmp [cs:songon],0
	jnz playsong
	jmp done
playsong:                       ;music to play
	mov al,[cs:fader]
	add al,2
	sub al,[cs:oldsb]
	mov [cs:fader],al
	cmp al,5
	jb goodvol
	mov byte ptr [cs:fader],0
	call getvol
	mov ah,[cs:volume]
	or ah,[cs:oldsb]
	cmp al,ah
	jz goodvol
	ja highvol
	mov ah,al
	inc ah
	add ah,[cs:oldsb]
	call setvol
	jmp goodvol
highvol:
	mov ah,al
	dec ah
	sub ah,[cs:oldsb]
	call setvol
goodvol:
	dec [cs:playc]
	cmp [cs:playc],0
	jz playloop
	jmp done
playloop:
	lodsb
	test al,128
	jz event
	and al,127
	inc al
	mov [cs:playc],al
	jmp done
event:
	mov ah,al
	shr ah,4
	cmp ah,1
	jz num1
	cmp ah,2
	jz num2
	cmp ah,3
	jz num3
	cmp ah,7
	jz num7
	cmp ah,6
	jz num6
	cmp ah,5
	jz num5
	jmp playloop
num1:                           ;turn on key
	and ax,15
	mov bx,ax
	lodsb
	and ax,127
	shl ax,1
	mov di,offset noteval
	add di,ax
	mov cx,[cs:di]
	call keyon
	jmp playloop
num2:                           ;turn off key
	and ax,15
	mov bx,ax
	call keyoff
	jmp playloop
num3:                           ;set instrument
	and ax,15
	mov bx,ax
	mov ax,ds
	mov es,ax
	mov di,si
	add si,11
	call setvoic
	jmp playloop
num5:                           ;set tag pointer
	and ax,15
	mov cx,ax
	mov bx,1
	shl bx,cl
	or [cs:tagset],bx
	shl ax,1
	mov di,offset tagptr
	add di,ax
	mov [cs:di],si
	jmp playloop
num7:                           ;return from gosub      
	cmp byte ptr [cs:gosub],1
	jnz nosub
	mov si,[cs:oldptr]
	mov byte ptr [cs:gosub],0
nosub:
	jmp playloop
num6:                           ;jump/repeat to tag
	and ax,15
	mov cx,ax
	shl ax,1
	mov di,offset tagptr
	add di,ax
	lodsb
	cmp al,0ffh
	jnz notsub
	mov byte ptr [cs:gosub],1
	mov [cs:oldptr],si
	jmp nocount
notsub:
	cmp al,0feh
	jz nocount
	add di,32
	mov bx,[cs:di]
	dec word ptr [cs:di]
	cmp bx,1
	jz badtag
	cmp bx,0
	jnz inloop
	xor ah,ah
	mov [cs:di],ax
inloop:
	sub di,32
nocount:
	mov bx,1
	shl bx,cl
	test [cs:tagset],bx
	jz badtag
	mov si,[cs:di]
badtag:
	jmp playloop
done:
	mov [cs:songptr],si
	pop di es si ds dx cx bx ax
	jmp dword ptr [cs:timer]        ;call old interrupt
endp    musicplayer

proc    setvoic         ;bx= voice      es:di= instrument
			;set instrument data
	push cx ds si
	mov si,offset modulat
	add si,bx
	mov cl,[cs:si]
	mov si,offset insport
	mov ax,cs
	mov ds,ax
	mov [cs:insnum],10
setnextval:
	mov ah,[es:di]
	inc di
	lodsb
	add al,cl
	call writefm
	dec [cs:insnum]
	cmp [cs:insnum],0
	jnz setnextval
	mov ah,[es:di]
	mov al,0c0h
	add al,bl
	call writefm
	pop si ds cx
	retn
endp    setvoic
	
Proc    Setupmusic      ;buf()
	push bp
	mov bp,sp
	push es ds

	mov bx,[ss:bp+06]
	mov ax,[ds:bx+02h]
	mov [cs:buffseg],ax
	mov ax,[ds:bx+0ah]
	mov [cs:buffptr],ax
	mov [cs:songon],0
	cmp [cs:timer+2],0
	jnz nosetup
	mov ax,351ch
	int 21h
	mov [cs:timer],bx
	mov [cs:timer+2],es             ;store old vector
	mov ax,seg musicplayer
	mov ds,ax
	mov dx,offset musicplayer
	mov ax,251ch
	int 21h                         ;set new interrupt
nosetup:
	pop ds es bp
	retf 2
endp    setupmusic

Proc    closemusic
	push ds
	mov [cs:songon],0
	mov cx,9
shutoffvoice:
	mov bx,cx
	dec bx
	call keyoff
	loop shutoffvoice
	cmp [cs:timer+2],0
	jz notset
	mov ax,[cs:timer+2]
	mov ds,ax
	mov dx,[cs:timer]
	mov ax,251ch
	int 21h
	mov [cs:timer],0
	mov [cs:timer+2],0
notset:
	pop ds
	retf
endp    closemusic

Proc    stopsong
	mov [cs:songon],0
	mov cx,9
shutoffv:
	mov bx,cx
	dec bx
	call keyoff
	loop shutoffv
	retf
endp    stopsong

Proc    Resumesong
	mov [cs:songon],1
	retf
endp    resumesong

Proc    Loadsong        ;fil$
	push bp
	mov bp,sp
	push ds si es di

	mov [cs:songon],0
	mov cx,9
clearv:
	mov bx,cx
	dec bx
	call keyoff
	loop clearv
	mov si,[ss:bp+06]
	add si,2
	mov dx,[ds:si]
	mov ax,3d00h
	int 21h
	jc nofil
	mov bx,ax
	mov ax,[cs:buffseg]
	mov ds,ax
	mov dx,[cs:buffptr]
	mov cx,0ffffh
	mov ax,3f00h
	int 21h
	mov ax,3e00h
	int 21h
	mov ax,[cs:buffptr]
	add ax,4
	mov [cs:songptr],ax
	mov [cs:tagptr],ax
	mov [cs:playc],1
	mov [cs:tagset],1
	mov [cs:gosub],0
	mov ax,cs
	mov es,ax
	mov di,offset repptr
	mov cx,15
	mov ax,0
	rep stosw
	mov [cs:songon],1
nofil:
	pop di es si ds bp
	retf 2
endp    loadsong

Proc    ResetFM
	mov cx,0f5h
clearit:
	mov al,cl
	xor ah,ah
	call writefm
	loop clearit
	mov ax,2001h
	call writefm
	call getvol
	mov bl,al
	xor ax,ax
	call setvol
	call getvol
	mov [cs:oldsb],al
	mov ah,bl
	call setvol
	retf
endp    resetfm

Proc    fademusic       ;vol
	push bp
	mov bp,sp
	mov ax,[ss:bp+06]
	and ax,15
	mov [cs:volume],al
	pop bp
	retf 2
endp    fademusic

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

proc    setfmvol        ;vol
	push bp
	mov bp,sp
	mov ax,[ss:bp+06]
	and ax,15
	mov [cs:volume],al
	mov ah,al
	call setvol
	pop bp
	retf 2
endp    setfmvol

proc    getfmvol
	call getvol
	retf
endp    getfmvol
end
