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

;****** File stuff for directories and lump files
;*
;*
;* last modified February 1, 1900
;*         modularized lump code
;*       made unlumpfile (uses file mask - supports *, ?)
;*         ? means any 1 char, * means match 0-n chars up to next char
;* August 1999 changes
;*         isdir now works (fixed bug with carry flag)
;*         drivelist still wrong
;*       now halting unlump on a starting \, : and ..
;* Info on unlumping:
;*       supports no compression, unlumps only to named dir. (not subs)

.286
Ideal
Model Small
Public findfiles, lumpfiles, unlump, unlumpfile, isfile, pathlength, getstring, drivelist, rpathlength, setdrive, envlength
Public isdir, isvirtual, isremovable, hasmedia

Codeseg

dta     dw ?
src     dw ?
dest    dw ?
lump    dw ?
wrote   dw ?
found   dw ?
data    dw ?
bseg    dw ?
ulpseg	dw ?
ulpath	dw ?
ulplen	dw ?
ulmseg	dw ?
ulmask	dw ?
ulmlen	dw ?
list    dw ?
listlen dw ?
lumplenhigh	dw ?
lumplenlow	dw ?
lumpdatahigh	dw ?
lumpdatalow 	dw ?
blocks	dw ?
path    dw ?
amask   dw ?
cpath	db 70 dup (0)
plen	dw ?
media	db ?

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

Proc    Lumpfiles       ;files$, dest$, path$, buf()
	push bp
	mov bp,sp
	push ds si es di

	mov [cs:data],ds
	mov bx,[ss:bp+06]
	mov di,[ds:bx+0ah]
	mov [cs:path],di        ;set path to start of buffer
	mov ax,[ds:bx+02h]
	mov [cs:bseg],ax        ;set bseg to buffer segment
	mov es,ax
	mov si,[ss:bp+08]
	lodsw
	mov cx,ax
	mov si,[ds:si]
	rep movsb               ;write path to start of buffer
	mov [cs:found],di       ;set found to beginning of filename
	mov si,[ss:bp+12]
	add si,2
	mov dx,[ds:si]
	mov ax,3d02h
	int 21h                 ;open filelist as list
	jnc gotalistfile
	jmp badquit
gotalistfile:
	mov [cs:list],ax
	mov si,[ss:bp+10]
	add si,2
	mov dx,[ds:si]
	mov ax,3c00h
	int 21h
	jnc createddest
	jmp closelist
createddest:
	mov [cs:dest],ax        ;open the destination file
nextfile:
	mov bx,[cs:list]        
	mov cx,13
	mov ds,[cs:bseg]
	mov dx,[cs:found]
	mov ax,3f00h
	int 21h                 ;read the next filename
	jnc stillreadmore
	jmp closem
stillreadmore:
	cmp ax,0
	jnz stillmorefiles      ;quit if no more files
	jmp closem
stillmorefiles:
	mov si,[cs:found]
	mov cx,ax
find0:
	lodsb
	cmp al,0
	jz found0
	loop find0
	jmp closem
found0:
	neg cx
	add cx,3
	mov dx,cx
	xor cx,cx
	cmp dx,0
	jns notneg
	mov cx,0ffffh           ;cx|dx = signed offset for the file pointer
notneg:
	mov bx,[cs:list]
	mov ax,4201h
	int 21h                 ;set file pointer to start of next name
	mov ds,[cs:bseg]
	mov dx,[cs:path]
	mov ax,3d00h
	int 21h
	jc nextfile             ;open file to write
	mov [cs:lump],ax
	mov ax,4202h
	xor cx,cx
	xor dx,dx
	mov bx,[cs:lump]
	int 21h			;find file length
	mov ds,[cs:bseg]
	mov [ds:si],dx
	add si,2
	mov [ds:si],ax
	add si,2
	mov [cs:wrote],si       ;set wrote to end of filename + length
	mov bx,[cs:dest]
	mov dx,[cs:found]
	mov cx,[cs:wrote]
	sub cx,[cs:found]
	mov ax,4000h
	int 21h                 ;write filename and length
	mov bx,[cs:lump]
	xor cx,cx
	xor dx,dx
	mov ax,4200h
	int 21h			;set file pointer to start
nextblock:
	mov dx,[cs:wrote]
	mov cx,4000h
	mov bx,[cs:lump]
	mov ax,3f00h
	int 21h                 ;read 16k bytes
	jc closelump
	cmp ax,0
	jz closelump
	mov cx,ax
	mov bx,[cs:dest]
	mov ax,4000h
	int 21h                 ;write how much we read
	jmp nextblock
closelump:
	mov bx,[cs:lump]
	mov ax,3e00h
	int 21h                 ;close the file I just lumped
	jmp nextfile
closem:
	mov bx,[cs:dest]
	mov ax,3e00h
	int 21h                 ;close the lump file
closelist:
	mov bx,[cs:list]
	mov ax,3e00h
	int 21h                 ;close the list file
badquit:        
	pop di es si ds bp
	retf 8
endp    Lumpfiles

Proc    Unlump          ; file$, path$, temp()
	push bp
	mov bp,sp
	push ds si es di
	
	mov [cs:data],ds
	mov bx,[ss:bp+06]
	mov di,[ds:bx+0ah]
	mov [cs:path],di        ;set path to start of buffer
	mov ax,[ds:bx+02h]
	mov [cs:bseg],ax        ;set bseg to buffer segment

	mov ds,[cs:data]

	mov si,[ss:bp+08]	;get unlump path info
	lodsw
	mov [cs:ulplen],ax
	lodsw
	mov [cs:ulpseg],ds
	mov [cs:ulpath],ax

	mov si,[ss:bp+10]	;get path to lumpfile
	add si,2
	mov dx,[ds:si]
	mov ax,3d02h
	int 21h			;open lumpfile
	jnc goodwad
	jmp nowad
goodwad:
	mov [cs:lump],ax	;store file handle for wad
nextunlump:
	call Findnextlump
	jc nomore
	call Unlumpit		;sets carry flag if successful
	jnc unlumped
	call Skipit
unlumped:
	jmp nextunlump
nomore:
	mov bx,[cs:lump]
	mov ax,3e00h
	int 21h
nowad:
	pop di es si ds bp
	retf 6
endp    Unlump

Proc    Unlumpfile          ; file$, mask$, path$, temp()
	push bp
	mov bp,sp
	push ds si es di
	
	mov [cs:data],ds
	mov bx,[ss:bp+06]
	mov di,[ds:bx+0ah]
	mov [cs:path],di        ;set path to start of buffer
	mov ax,[ds:bx+02h]
	mov [cs:bseg],ax        ;set bseg to buffer segment

	mov ds,[cs:data]

	mov si,[ss:bp+08]	;get unlump path info
	lodsw
	mov [cs:ulplen],ax
	lodsw
	mov [cs:ulpseg],ds
	mov [cs:ulpath],ax

	mov si,[ss:bp+10]	;get unlump mask info
	lodsw
	mov [cs:ulmlen],ax
	lodsw
	mov [cs:ulmseg],ds
	mov [cs:ulmask],ax

	mov si,[ss:bp+12]	;get path to lumpfile
	add si,2
	mov dx,[ds:si]
	mov ax,3d02h
	int 21h			;open lumpfile
	jnc goodwad2
	jmp nowad2
goodwad2:
	mov [cs:lump],ax	;store file handle for wad
nextunlump2:
	call Findnextlump
	jc nomore2
	call Checklumpmask
	jc dontunlump2
	call Unlumpit		;sets carry flag if successful
	jnc unlumped2
dontunlump2:
	call Skipit
unlumped2:
	jmp nextunlump2
nomore2:
	mov bx,[cs:lump]
	mov ax,3e00h
	int 21h
nowad2:
	pop di es si ds bp
	retf 8
endp	Unlumpfile

Proc	Findnextlump
	; assumes the file pointer lump points to new lump data,
	;	bseg:path is buffer start,
	;	ulpseg:ulpath is ulplen unlump path string
	; if the lump data has a friendly name and data,
	;	it sets lumplen and lumpdata to the data,
	;	and it writes the full path to bseg:path,
	;	and the the filename to bseg:found
	; it uses wrote, and moves the file pointer
	; destroys registers
	; CF = 0 if successful, 1 if failed
	mov es,[cs:bseg]
	mov di,[cs:path]
	mov ds,[cs:ulpseg]
	mov si,[cs:ulpath]
	mov cx,[cs:ulplen]
	rep movsb               ;write path to start of buffer
	mov [cs:found],di       ;set found to beginning of filename
	mov ds,[cs:bseg]
	mov dx,[cs:found]
	mov bx,[cs:lump]
	mov cx,18
	mov ax,3f00h
	int 21h
	jnc canread
	jmp badfile
canread:
	cmp ax,0
	jnz readsome
	jmp badfile
readsome:
	mov [cs:wrote],ax
	mov si,[cs:found]
	mov cx,13
	mov al,[ds:si]
	cmp al,'\'		;stop if file starts with \
	jz badfile
	cmp al,0		;stop if first char is a 0
	jz badfile
findlen:
	lodsb
	cmp al,0
	jz foundit
	cmp al,':'		;stop if file has a : in it
	jz badfile
	mov ah,[ds:si]
	cmp ax,'..'		;stop if file has a .. in it
	jz badfile
	dec cx
	cmp cx,0
	jnz findlen
badfile:
	stc			;no more good lumps so fail
	retn
foundit:			;found termination
	lodsw
	mov [cs:lumplenhigh],ax
	lodsw
	mov [cs:lumplenlow],ax	;put length in lumplen
	mov dx,si
	sub dx,[cs:found]
	sub dx,[cs:wrote]
	mov cx,0ffffh
	mov bx,[cs:lump]
	mov ax,4201h		;move file pointer to end of filename
	int 21h
	mov [cs:lumpdatahigh],dx
	mov [cs:lumpdatalow],ax
	clc
	retn
endp	Findnextlump

Proc	Unlumpit
	; assumes the file pointer lump points to the data, 
	;	filename is at bseg:path, 
	;	and lumplen = length
	; it writes the unlumped file to the filename,
	;	it also demolishes the buffer
	; uses blocks and dest
	; destroys registers
	; CF = 0 if successful, 1 if failed
	mov bx,[cs:lump]
	mov cx,[cs:lumpdatahigh]
	mov dx,[cs:lumpdatalow]
	mov ax,4200h
	int 21h			;set lump to point to the data
	mov ds,[cs:bseg]
	mov dx,[cs:path]
	mov ax,3c00h
	xor cx,cx
	int 21h
	jnc goodfile		;if you can't make the file exit failure
	stc
	retn
goodfile:
	mov [cs:dest],ax
	mov cx,[cs:lumplenhigh]
	shl cx,1
	mov ax,[cs:lumplenlow]
	shr ax,15
	add ax,cx
	inc ax
	mov [cs:blocks],ax
	mov cx,[cs:lumplenlow]
	and cx,07fffh
	cmp cx,0
	jz notmuch
doblocks:
	mov bx,[cs:lump]
	mov ds,[cs:bseg]
	mov dx,[cs:path]
	mov ax,3f00h
	int 21h
	mov cx,ax
	mov bx,[cs:dest]
	mov ds,[cs:bseg]
	mov dx,[cs:path]
	mov ax,4000h
	int 21h
notmuch:
	mov cx,08000h
	dec [cs:blocks]
	cmp [cs:blocks],0
	jnz doblocks
	mov bx,[cs:dest]
	mov ax,3e00h
	int 21h
	clc
	retn
endp	Unlumpit

Proc	Skipit
	;assumes lump is an open file handle to the lump file, 
	;	lumpdata points to lump data, and lumplen = size
	;it moves the file pointer past the lump data
	;destroys registers
	mov bx,[cs:lump]
	mov cx,[cs:lumpdatahigh]
	mov dx,[cs:lumpdatalow]
	add dx,[cs:lumplenlow]
	adc cx,[cs:lumplenhigh]
	mov ax,4200h
	int 21h
	retn
endp	Skipit

Proc	Checklumpmask
	; assumes the file pointer lump points to the data, 
	;	filename is at bseg:found
	; it compares the mask to the filename
	; destroys registers
	; CF = 0 if matched, 1 if no match
	mov es,[cs:bseg]
	mov di,[cs:found]
	mov ds,[cs:ulmseg]
	mov si,[cs:ulmask]
	mov cx,[cs:ulmlen]
	cmp cx,0
	jz goodmatch
nextchar:
	xor al,al
	cmp cx,0
	jz matchchar
	lodsb
	dec cx
	cmp al,'?'
	jz wildcard
	cmp al,'*'
	jnz matchchar
	xor al,al
	cmp cx,0
	jz scanforit
	lodsb
	dec cx
	cmp al,97
	jb scanforit
	cmp al,122
	ja scanforit
	sub al,32
scanforit:
	mov ah,[es:di]
	inc di
	cmp al,ah
	jz passmatch
	cmp ah,0
	jz failmatch
	cmp ah,'.'
	jz failmatch
	jmp scanforit
wildcard:
	mov ah,[es:di]
	inc di
	cmp ah,0
	jz failmatch
	cmp ah,'.'
	jz failmatch
	jmp passmatch
matchchar:
	cmp al,97
	jb notlcase
	cmp al,122
	ja notlcase
	sub al,32
notlcase:
	mov ah,[es:di]
	inc di
	cmp al,ah
	jz passmatch
failmatch:
	stc
	retn
passmatch:
	cmp al,0
	jnz nextchar
goodmatch:
	clc
	retn
endp	Checklumpmask

Proc	isfile		; filename
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
endp	isfile

Proc	pathlength
	push ds si

	mov ax,cs
	mov ds,ax
	lea si,[cs:cpath]
	mov ax,1900h
	int 21h
	mov dl,al
	inc dl
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
endp	pathlength

proc	getstring		;string
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
endp	getstring

proc	drivelist	;buffer
	push bp
	mov bp,sp
	push es di

	mov ax,0040h
	mov es,ax
	mov di,10h
	mov cx,[es:di]
	shr cx,6
	and cx,3
	inc cx
	mov bx,[ss:bp+06]
	mov di,[ds:bx+0ah]
	mov es,[ds:bx+02h]
	mov [cs:found],0
	mov ax,1900h
	int 21h
	xor ah,ah
	mov [cs:src],ax
	mov [cs:list],0
checkall:
	mov dx,[cs:list]
	mov ax,0e00h
	int 21h
	mov ax,1900h
	int 21h
	xor ah,ah
	cmp ax,[cs:list]
	jnz notdrive
	mov bx,[cs:list]
	inc bx
	mov ax,4408h
	int 21h
	cmp ax,1
	jz isdrive
	cmp cx,0
	jz notdrive
	dec cx
isdrive:
	inc [cs:found]
	mov ax,bx
	stosw
notdrive:
	inc [cs:list]
	cmp [cs:list],26
	jb checkall
	mov dx,[cs:src]
	mov ax,0e00h
	int 21h
	mov ax,[cs:found]

	pop di es bp
	retf 2
endp	drivelist

Proc    rpathlength
	push ds si es di

	mov ax,cs
	mov es,ax
	lea di,[cs:cpath]
	mov ax,5100h
	int 21h
	mov ds,bx
	mov si,2ch
	mov ax,[ds:si]
	mov ds,ax
	xor si,si
scanenv:
	mov ax,[ds:si]
	cmp ax,0000h
	jz readrpath
	inc si
	jmp scanenv
readrpath:
	add si,4
	mov bx,si
scanrpath:
	lodsb
	cmp al,0
	jnz scanrpath
scanback:
	dec si
	mov al,[ds:si]
	cmp al,'\'
	jnz scanback
	mov cx,si
	sub cx,bx
	inc cx
	mov [cs:plen],cx
	mov si,bx
	rep movsb
	mov ax,[cs:plen]

	pop di es si ds
	retf
endp    rpathlength

proc	setdrive	;num
	push bp
	mov bp,sp

	mov dx,[ss:bp+06]
	mov ax,0e00h
	int 21h
	pop bp
	retf 2
endp	setdrive

proc	envlength		;string
	push bp
	mov bp,sp
	push ds si es di

	mov si,[ss:bp+06]
	lodsw
	mov [cs:listlen],ax
	mov di,[ds:si]
	mov [cs:list],di
	mov ax,ds
	mov es,ax
	mov ax,5100h
	int 21h
	mov ds,bx
	mov bx,02ch
	mov ax,[ds:bx]
	mov ds,ax
	xor si,si
nextenv:
	mov ax,0
	cmp al,[ds:si]
	jz noenv
	mov cx,[cs:listlen]
checkvar:
	mov di,[cs:list]
	rep cmpsb
	jz gotenv
findnext:
	lodsb
	cmp al,0
	jnz findnext
	jmp nextenv
gotenv:
	lodsb
	cmp al,61
	jnz findnext
	mov ax,cs
	mov es,ax
	lea di,[cs:cpath]
	mov cx,0
loadenv:
	lodsb
	cmp al,0
	jz gotvar
	stosb
	inc cx
	jmp loadenv
gotvar:
	mov ax,cx
	mov [cs:plen],cx
noenv:
	pop di es si ds bp
	retf 2	
endp	envlength

proc	isdir		;name
	push bp
	mov bp,sp
	push si

	mov si,[ss:bp+06]
	add si,2
	mov dx,[ds:si]
	mov ax,4300h
	int 21h
	jc notdir
	test cx,16
	jz notdir
	mov ax,-1
	jmp isdirdone
notdir:
	xor ax,ax
isdirdone:
	pop si bp
	retf 2
endp	isdir

proc	isremovable	;dnum
	push bp
	mov bp,sp

	mov bx,[ss:bp+06]
	xor bh,bh
	mov ax,4408h
	int 21h
	xor ah,ah
	pop bp
	retf 2
endp	isremovable	

proc	isvirtual	;dnum
	push bp
	mov bp,sp

	mov bx,[ss:bp+06]
	mov ax,4409h
	int 21h
	xor ax,ax
	test dx,4096
	jz notvirtual
	mov ax,-1
notvirtual:
	pop bp
	retf 2
endp	isvirtual

proc	hasmedia	;dnum
	push bp
	mov bp,sp
	push ds

	mov ax,cs
	mov ds,ax
	lea bx,[cs:media]
	mov dx,[ss:bp+06]
	mov ax,1c00h
	int 21h
	xor ax,ax
	mov bl,[ds:bx]
	xor bh,bh
	cmp bl,0
	jz nomedia
	mov ax,-1
nomedia:

	pop ds bp
	retf 2
endp	hasmedia
end

