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

;*** Play FM Music through Adlib card
;* This better work...
;* Brian Fisher
;*
;*  last mofified November 21st 2002
;*  put in checks to not exceed loaded song length
;*
;*  modified December 12th 2000
;*  tested with Borland TASM 2.01
;*  uses QuickBasic calling conventions
;*  designed for the OPL2 & OPL3 chipset, best with a fully SB compatible card

.286
Model Small
Public ResetFM,FMKeyOn,FMKeyOff,Setvoice,Getvoice,Setupmusic,closemusic,loadsong,stopsong,resumesong,fademusic,getfmvol,setfmvol,SetFMBase,SetFMMixerBase
CodeSeg

;;;; header for a BAM file
header	db	'CBMF'

;;;; storage for the original timer Interrupt Service Routine
timer   dw      0,0

;;;; pointer to the song buffer, and song location
buffseg dw      ?
buffptr dw      ?
songptr dw      ?
songlen	dw	?

;;;; pointers for gosubs and repeats
gosub   db      0
oldptr  dw      ?
tagset  dw      1
tagptr  dw      ?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?
repptr  dw      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

;;;; variable for the tempo - not implemented
tempo   db      ?

;;;; variables used for dividing timer interrupt rate down
tickc   db      ?
playc   db      0

;;;; flags for song state
songon  db      0
gotsong	db	0

;;;; port bases
fmport  dw      0388h
mixport	dw	0220h

;;;; config stuff
opl3	db	0

;;;; a buffer for the old block & high frequency bits for a channel
oldb0   db      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

;;;; variables for the current volume, target volume, and an adjustment for interpreting volume
volume  db      15
fader   db      0
volbits	db	0

;;;; here is a count of the allowable number of voices (channels)
maxchan	dw	15
;;;; an array of channel->modulator mappings (opl3 repeats mappings over set2 for next 9)
modulat db      0,1,2,8,9,10,16,17,18, 0,1,2,8,9,10,16,17,18

;;;; an array of register bases for the 10 fm instrument data bytes (alternates between modulator & generator)
insport db      20h,23h,40h,43h,60h,63h,80h,83h,0e0h,0e3h

;;;; an array that maps MIDI key to octave (block number) and frequency for the FM music
noteval dw      172,182,193,205,217,230,243,258,274,290,307,326,345,365,387,410,435,460,489,517,547,580,614,651,1369,1389,1411,1434,1459,1484,1513,1541,1571,1604,1638,1675,2393,2413,2435,2458,2483,2508,2537,2565,2595,2628,2662,2699,3417,3437,3459,3482,3507,3532,3561,3589,3619,3652,3686,3723,4441,4461,4483,4506,4531,4556,4585,4613,4643,4676,4710,4747,5465,5485,5507,5530,5555,5580,5609,5637,5667,5700,5734,5771,6489,6509,6531,6554,6579,6604,6633,6661,6691,6724,6758,6795,7513,7533,7555,7578,7603,7628,7657,7685,7715,7748,7782,7819,7858,7898,7942,7988,8037,8089,8143,8191,8191,8191,8191,8191,8191,8191,8191,8191,8191,8191,8191



;;;;;;;; Internal use, reads the status register
Proc    ReadFM
	push dx
	mov dx,[cs:fmport]
	in al,dx
	pop dx
	retn
endp	ReadFM

;;;;;;;; Internal use, writes to a register in an OPL chip, using OPL2 delays
Proc    WriteFM         ;al= register   ah= value
	push dx cx ax
	mov dx,[cs:fmport]
	mov cx,36
	rep in al,dx		;do a delay of 36 bus clocks (as added insurance)
	pop ax
	out dx,al
	mov cx,6
	rep in al,dx		;do a delay of 6 bus clocks (need >3.3 us delay for OPL2)
	mov al,ah
	inc dx
	out dx,al
	dec dx
	mov cx,36
	rep in al,dx		;do a delay of 36 bus clocks (need >23 us delay for OPL2)
	pop cx dx
	retn    
endp    writefm

;;;;;;;; Internal use, writes to set2 registers in an OPL3 chip, using OPL2 delays as a precaution
Proc    WriteFM3         ;al= register   ah= value
	push dx cx ax
	mov dx,[cs:fmport]
	mov cx,36
	rep in al,dx		;do a delay of 36 bus clocks (as added insurance)
	pop ax
	add dx,2
	out dx,al
	mov cx,6
	rep in al,dx		;do a delay of 6 bus clocks (need >3.3 us delay for OPL2)
	mov al,ah
	inc dx
	out dx,al
	sub dx,3
	mov cx,36
	rep in al,dx		;do a delay of 36 bus clocks (need >.28 us delay for OPL2)
	pop cx dx
	retn    
endp    writefm3

;;;;;;;; Internal use, writes to the specified base channel reg
Proc	writefmchannel	;al= base register	ah= value	bl= channel
	cmp bx,8
	ja opl3channel
	add al,bl
	call writefm
	jmp writechanneldone
opl3channel:
	add al,bl
	sub al,9
	call writefm3
writechanneldone:
	retn
endp	writefmchannel

;;;;;;;; Internal use, turns on a channel to the given frequency & octave
Proc    KeyOn           ;bx= channel      cx= freqnum	destroys: ax
	cmp bx,[cs:maxchan]
	ja keyondone		;make sure we are using a legal channel number
	push si
	mov si,offset oldb0
	add si,bx		;set [cs:si] to a buffer for this channels block number
	mov al,0a0h
	mov ah,cl
	call writefmchannel	;set the low bits for the frequency
	mov al,0b0h
	mov ah,ch
	mov [cs:si],ah		;store the block info for the current channel 
	or ah,32		;set the key on bit
	call writefmchannel
	pop si
keyondone:
	retn
endp    keyon

;;;;;;;; Internal use, turns off a channel
Proc    KeyOff          ;bx= channel	destroys: ax
	cmp bx,[cs:maxchan]
	ja keyoffdone		;make sure we are using a legal channel number
	push si
	mov si,offset oldb0
	add si,bx
	mov ah,[cs:si]		;read the block info from the buffer for the channel
	mov al,0b0h
	call writefmchannel	;set the key on bit off, but leave the block & frequency
	pop si
keyoffdone:
	retn
endp    keyoff

;;;;;;;; Internal use, sets the music volume using the mixer
proc    setvol		;ah= volume	destroys: ax dx
	mov dx,[cs:mixport]
	add dx,4
	mov al,26h
	out dx,al
	mov al,ah
	shl ah,4
	add al,ah		;set both left and right to same volume
	inc dx
	out dx,al
	retn
endp    setvol

;;;;;;;; Internal use, gets the fm volume from the mixer
proc    getvol		;retval: ax	destroys: dx
	mov dx,[cs:mixport]
	add dx,4
	mov al,26h
	out dx,al
	inc dx
	in al,dx
	and ax,15		;take the low 4 bits (right channel)
	retn
endp    getvol

;;;;;;;; Interrupt Service Routine - runs on timer interrupt 1ch
;;;;;;;; This is the BAM player main loop
proc    musicplayer
	push ax bx cx dx ds si es di
	mov ds,[cs:buffseg]
	mov si,[cs:songptr]
	cmp [cs:songon],0
	jnz playsong
	jmp done
playsong:                       ;music to play
	mov al,[cs:fader]
	add al,2
	sub al,[cs:volbits]
	mov [cs:fader],al
	cmp al,5
	jb goodvol
        mov [cs:fader],0
	call getvol
	mov ah,[cs:volume]
	or ah,[cs:volbits]
	cmp al,ah
	jz goodvol
	ja highvol
	mov ah,al
	inc ah
	add ah,[cs:volbits]
	call setvol
	jmp goodvol
highvol:
	mov ah,al
	dec ah
	sub ah,[cs:volbits]
	call setvol
goodvol:
	dec [cs:playc]
	cmp [cs:playc],0
	jz playloop
	jmp done
checkptr:
	mov ax,si
	sub ax,[cs:buffptr]
	cmp ax,[cs:songlen]
	jb playloop
	mov si,[cs:buffptr]
	add si,4
	mov [cs:songon],0
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
	cmp ah,5
	jz num5
	cmp ah,7
	jz num7
	cmp ah,6
	jz num6
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
	jmp checkptr
num2:                           ;turn off key
	and ax,15
	mov bx,ax
	call keyoff
	jmp checkptr
num3:                           ;set instrument
	and ax,15
	mov bx,ax
	mov ax,ds
	mov es,ax
	mov di,si
	add si,11
	call setvoic
	jmp checkptr
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
	jmp checkptr
num7:                           ;return from gosub      
        cmp [cs:gosub],1
	jnz nosub
	mov si,[cs:oldptr]
        mov [cs:gosub],0
nosub:
	jmp checkptr
num6:                           ;jump/repeat to tag
	and ax,15
	mov cx,ax
	shl ax,1
	mov di,offset tagptr
	add di,ax
	lodsb
	cmp al,0ffh
	jnz notsub
        mov [cs:gosub],1
	mov [cs:oldptr],si
	jmp nocount
notsub:
	cmp al,0feh
	jz nocount
	add di,32
	mov bx,[cs:di]
        dec WORD PTR [cs:di]
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
	jmp checkptr
done:
	mov [cs:songptr],si
	pop di es si ds dx cx bx ax
        jmp DWORD PTR [cs:timer]        ;call old interrupt
endp    musicplayer


;;;;;;;; Internal use, Sets the channel's FM data
proc    setvoic         ;bx= voice      es:di= instrument
	cmp bx,[cs:maxchan]
	ja setvoicedone			;make sure we are using a legal channel number
	push cx ds si
	mov si,offset modulat
	add si,bx
	mov cl,[cs:si]
	mov si,offset insport
	mov ax,cs
	mov ds,ax
	mov dx,10
setnextval:
	mov ah,[es:di]
	inc di
	lodsb
	add al,cl
	cmp bx,8
	ja opl3voic			;Select port based on voice - this should probably be wrapped up
	call writefm
	jmp opl2voic
opl3voic:
	call writefm3
opl2voic:
	dec dx
	cmp dx,0			;loop through all the data for the voice
	jnz setnextval
	mov ah,[es:di]
	mov al,0c0h
	or ah,30h			;NEW IN OPL3 - ensure the channel is set to go to both left and right output
	call writefmchannel
	pop si ds cx
setvoicedone:
	retn
endp    setvoic
	
;;;;;;;; External interface, Loads the BAM player and sets up the song buffer
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

;;;;;;;; External interface, Unloads the BAM player
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

;;;;;;;; External interface, Turns off the BAM player, and quiets all channels used for BAM
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

;;;;;;;; External interface, Turns on the BAM player if a song has been loaded
Proc    Resumesong
	mov al,[cs:gotsong]
	mov [cs:songon],al
	retf
endp    resumesong

;;;;;;;; External interface, Loads the BAM music in the given file, and begins playing
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
	int 21h				;open the file
	jc nofil
	mov bx,ax
	mov ax,[cs:buffseg]
	mov ds,ax
	mov dx,[cs:buffptr]
	mov cx,0ffffh
	mov ax,3f00h
	int 21h				;load up to 64k of file info
	jc readfail
	mov [cs:songlen],ax
	mov ax,[cs:buffptr]
	add ax,4
	mov [cs:songptr],ax
	mov [cs:tagptr],ax
	mov [cs:playc],1
	mov [cs:tagset],1
	mov [cs:gosub],0		;initialize the jump tags
	mov ax,cs
	mov es,ax
	mov di,offset repptr
	mov cx,15
	mov ax,0
	rep stosw			;clear all the repeat tags
	mov [cs:songon],1
	mov [cs:gotsong],1
readfail:
	mov ax,3e00h
	int 21h				;close the file
nofil:
	pop di es si ds bp
	retf 2
endp    loadsong

;;;;;;;; External interface, to set the base OPL2/3 chip register
Proc	SetFMBase	;port
	push bp
	mov bp,sp
	mov ax,[ss:bp+06]
	mov [cs:fmport], ax
	pop bp
	retf 2
endp	SetFMBase

;;;;;;;; External interface, to set the base Mixer register
Proc	SetFMMixerBase	;port
	push bp
	mov bp,sp
	mov ax,[ss:bp+06]
	mov [cs:mixport], ax
	pop bp
	retf 2
endp	SetFMMixerBase

;;;;;;;; External interface, to reset the OPL chip and detect it's type
Proc    ResetFM
	mov cx,0f5h
clearit:
	mov al,cl
	xor ah,ah
	call writefm
	loop clearit			;clear the opl2 (or opl3 set 1) registers
	mov ax,6004h
	call writefm
	mov ax,8004h
	call writefm
	mov [cs:maxchan],8
	mov [cs:opl3],0		;initialize for opl2...
	call readfm
	and al,06h			;check for opl3
	jnz noopl3
	mov [cs:opl3],1
noopl3:
	mov [cs:maxchan],17
	mov cx,0f5h
clearit2:
	mov al,cl
	xor ah,ah
	call writefm3
	loop clearit2			;clear the second set data
	mov ax,0105h
	call writefm3			;enable opl3
	mov ax,0004h
	call writefm3			;ensure 18-channel
	mov ax,2001h
	call writefm3			;set to use normal sine wave
	mov ax,2001h
	call writefm			;set to use normal sine wave
	call getvol
	mov bl,al
	xor ax,ax
	call setvol
	call getvol
	mov [cs:volbits],al		;check to see if any volume bits are tied high
	mov ah,bl
	call setvol
	mov ax,1
	add al,[cs:opl3]
nocard:
	retf
endp    resetfm

;;;;;;;; External interface, sets the target volume for BAM playback
Proc    fademusic       ;vol
	push bp
	mov bp,sp
	mov ax,[ss:bp+06]
	and ax,15
	mov [cs:volume],al
	pop bp
	retf 2
endp    fademusic

;;;;;;;; External interface, turns the specified channel on with the given note
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

;;;;;;;; External interface, sets the FM data for the specified channel
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

;;;;;;;; External interface, loads instrument data for the voice from an IBK using the given buffer
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

;;;;;;;; External interface, turns off the given channel
Proc    FMKeyOff        ;voice
	push bp
	mov bp,sp

	mov bx,[ss:bp+06]
	call keyoff
	pop bp
	retf 2
endp    fmkeyoff

;;;;;;;; External interface, sets the FM volume
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

;;;;;;;; External interface, gets the current FM volume
proc    getfmvol
	call getvol
	retf
endp    getfmvol
end
