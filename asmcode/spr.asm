;****************************************************************************

;*** Draw Picarray with Palarray in modex
;*
;* Brian Fisher
;*
;*** Modified Aug 12, added array offsets

.286
Ideal
Model Small
Public Drawsprite,Wardsprite,getsprite,stosprite,Loadsprite,bigsprite,hugesprite
CodeSeg

memoff  dw 0
picseg  dw 0
palseg  dw 0
paloff  dw 0
Planenum db 1
evenflag db 4
Xsize   db 0
Ysize   db 0
Xloc    dw 0
ytop	db 0
ybot	db 0
drawy	db 0

Proc    Drawsprite ; Pic() Offset Pal() Paloff Xloc Yloc Page
		   ;  18     16    14     12    10   8    6
	push bp
	mov bp,sp
	push es ds si di

	cld
	mov al,4
	mov [cs:evenflag],al
	mov bx,[ss:bp+18]
	mov si,[ds:bx+0ah]
	mov ax,[ss:bp+16]
	shl ax,1
	add si,ax
	mov es,[ds:bx+02h]
	mov [cs:picseg],es
	mov bx,[ss:bp+14]
	mov di,[ds:bx+0ah]
	add di,[ss:bp+12]
	mov es,[ds:bx+02h]
	mov ds,[cs:picseg]
	mov [cs:paloff],di
	mov [cs:palseg],es
	lodsw                   ;get the x and y dimensions of pic
	mov [cs:xsize],al
	mov [cs:ysize],ah
	mov bx,[ss:bp+10]
	and bx,bx
	jns okee
	not bx
	inc bx
	sub [cs:xsize],bl
	ja good
	jmp nope
good:
	mov al,[cs:ysize]
	xor ah,ah
	mul bx
	mov bx,ax
	and ax,1
	xor ax,1
	shr bx,1
	add si,bx
	shl ax,2
	mov [cs:evenflag],al
	xor bx,bx
	jmp okay
okee:
	xor ah,ah
	add ax,bx
	cmp ax,321
	jb okay
	sub ax,320
	sub [cs:xsize],al
	cmp [cs:xsize],0
	jg okay
	jmp nope
okay:
	mov [cs:xloc],bx
	mov [cs:ytop],0
	mov ax,[ss:bp+08]
	and ax,ax
	jns goodtop
	mov bx,ax
	not bx
	inc bx
	mov [cs:ytop],bl
	cmp bl,[cs:ysize]
	jb goodtop
	jmp nope
goodtop:
	cmp ax,200
	jl goodystart
	jmp nope
goodystart:
	mov bl,[cs:ysize]
	mov [cs:ybot],bl
	xor bh,bh
	add ax,bx
	cmp ax,201
	jb goodbot
	sub ax,200
	sub bl,al
	mov [cs:ybot],bl
goodbot:
	mov ax,[ss:bp+08]       ;figure out the starting vga offset
	mov bx,80
	imul bx
	mov bx,[cs:xloc]
	shr bx,2
	add bx,ax
	mov ax,[ss:bp+06]
	shl ax,14
	add bx,ax
	mov [cs:memoff],bx
	mov cx,[cs:xloc]        ;figure out the plane of the first pixel
	and cl,3
	mov bx,1
	shl bl,cl
	mov [cs:planenum],bl
	xor ch,ch
	mov ax,0a000h           ;set up es
	mov es,ax
	xor ax,ax
pOOlx:
	xor al,al
	mov [cs:drawy],al
	cmp [cs:ytop],al
	jz notopcrop
	mov [cs:drawy],1
notopcrop:
	mov di,[cs:memoff]              ;reset di
	mov al,02h              ;set the plane write enable
	mov dx,03c4h
	mov ah,[cs:planenum]
	out dx,ax
	xor ax,ax
	xor dl,dl               ;clear dl for the y-counter
pOOly:
	lodsb                   ;get the first pixel data
	mov cl,[cs:evenflag]    ;get selected data using read flag
	shr al,cl                               
	shr cl,2
	mov bl,cl
	sub si,bx               ;increase si if evenflag is on
	xor bl,1                ;switch evenflag
	shl bl,2
	mov [cs:evenflag],bl            
	and al,15               ;take 4-bits
	cmp al,0                ;check for color 0
	je wardon
	cmp [cs:drawy],0
	jnz wardon
	mov ds,[cs:palseg]
	mov bx,[cs:paloff]
	add bx,ax
	mov al,[ds:bx]
	xor bh,bh
	mov [es:di],al
	mov ds,[cs:picseg]
wardon:
	add di,50h
	inc dl
	cmp dl,[cs:ytop]
	jb lowy
	mov [cs:drawy],0
lowy:
	cmp dl,[cs:ybot]
	jb nothighy
	mov [cs:drawy],1
nothighy:
	cmp dl,[cs:ysize]
	jb pooly
	inc ch
	mov al,[cs:planenum]
	shl al,1
	mov bx,ax
	shr bl,4
	add al,bl
	and al,15
	mov [cs:planenum],al
	mov ax,[cs:memoff]
	add ax,bx
	mov [cs:memoff],ax
	xor ax,ax
	cmp ch,[cs:xsize]
	jnb nope
	jmp poolx
nope:
	pop di si ds es bp
	retf 14
endp    Drawsprite

Proc    Wardsprite ; Pic() Offset Pal() Paloff Xloc Yloc Page
		   ;  18     16    14    12     10   8    6
	push bp
	mov bp,sp
	push es ds si di

	cld
	mov al,4
	mov [cs:evenflag],al
	mov bx,[ss:bp+18]
	mov si,[ds:bx+0ah]
	mov ax,[ss:bp+16]
	shl ax,1
	add si,ax
	mov es,[ds:bx+02h]
	mov [cs:picseg],es
	mov bx,[ss:bp+14]
	mov di,[ds:bx+0ah]
	add di,[ss:bp+12]
	mov es,[ds:bx+02h]
	mov ds,[cs:picseg]
	mov [cs:paloff],di
	mov [cs:palseg],es
	lodsw                   ;get the x and y dimensions of pic
	mov [cs:xsize],al
	mov cl,al
	xor ch,ch
	dec cx
	mov [cs:ysize],ah
	mov bx,[ss:bp+10]
	add bx,cx
	cmp bx,320
	jb eeko
	sub bx,319
	sub [cs:xsize],bl
	ja doog
	jmp nope2
doog:
	mov al,[cs:ysize]
	xor ah,ah
	mul bx
	mov bx,ax
	and ax,1
	xor ax,1
	shr bx,1
	add si,bx
	shl ax,2
	mov [cs:evenflag],al
	mov bx,319
	jmp yako
eeko:
	mov cx,[ss:bp+10]
	and cx,cx
	jns yako
	not cx
	inc cx
	sub [cs:xsize],cl
	ja yako
	jmp nope2
yako:
	mov [cs:xloc],bx
	mov [cs:ytop],0
	mov ax,[ss:bp+08]
	and ax,ax
	jns goodtop2
	mov bx,ax
	not bx
	inc bx
	mov [cs:ytop],bl
	cmp bl,[cs:ysize]
	jb goodtop2
	jmp nope
goodtop2:
	cmp ax,200
	jl goodystart2
	jmp nope
goodystart2:
	mov bl,[cs:ysize]
	mov [cs:ybot],bl
	xor bh,bh
	add ax,bx
	cmp ax,201
	jb goodbot2
	sub ax,200
	sub bl,al
	mov [cs:ybot],bl
goodbot2:
	mov ax,[ss:bp+08]       ;figure out the starting vga offset
	mov bx,80
	imul bx
	mov bx,[cs:xloc]
	shr bx,2
	add bx,ax
	mov ax,[ss:bp+06]
	shl ax,14
	add bx,ax
	mov [cs:memoff],bx
	mov cx,[cs:xloc]        ;figure out the plane of the first pixel
	and cx,3
	mov bx,1
	shl bl,cl
	mov [cs:planenum],bl
	mov ax,0a000h           ;set up es
	mov es,ax
XLOOP:
	xor al,al
	mov [cs:drawy],al
	cmp [cs:ytop],al
	jz notopcrop2
	mov [cs:drawy],1
notopcrop2:
	mov di,[cs:memoff]              ;reset di
	mov dx,03c4h
	mov al,02h
	mov ah,[cs:planenum]
	out dx,ax
	xor ax,ax
	xor dl,dl               ;clear dl for the y-counter
YLOOP:
	lodsb                   ;get the first pixel data
	mov cl,[cs:evenflag]    ;get selected data using read flag
	shr al,cl                               
	shr cl,2
	mov bl,cl
	sub si,bx               ;increase si if evenflag is on
	xor bl,1                ;switch evenflag
	shl bl,2
	mov [cs:evenflag],bl            
	and al,15               ;take 4-bits
	cmp al,0                ;check for color 0
	je NODRAW
	cmp [cs:drawy],0
	jnz NODRAW
	mov ds,[cs:palseg]
	mov bx,[cs:paloff]
	add bx,ax
	mov al,[ds:bx]
	xor bh,bh
	mov [es:di],al
	mov ds,[cs:picseg]
NODRAW:
	add di,50h
	inc dl
	cmp dl,[cs:ytop]
	jb lowy2
	mov [cs:drawy],0
lowy2:
	cmp dl,[cs:ybot]
	jb nothighy2
	mov [cs:drawy],1
nothighy2:
	cmp dl,[cs:ysize]
	jb YLOOP
	inc ch
	mov al,[cs:planenum]
	mov bx,ax
	shr al,1
	shl bl,3
	add bl,al
	and bl,15
	mov [cs:planenum],bl
	shr bx,3
	sub [cs:memoff],bx
	cmp ch,[cs:xsize]
	jnb nope2
	jmp XLOOP
nope2:
	pop di si ds es bp
	retf 14
endp    Wardsprite

Proc    getsprite       ; Pic() Offset Xloc Yloc xwid ywid Page
			;  18     16    14   12   10   8    6
	push bp
	mov bp,sp
	push es ds si di

	cld
	mov al,4
	mov [cs:evenflag],al
	mov bx,[ss:bp+18]
	mov di,[ds:bx+0ah]
	mov ax,[ss:bp+16]
	shl ax,1
	add di,ax
	mov es,[ds:bx+02h]
	mov al,[ds:bp+10]
	mov [cs:xsize],al
	stosb
	mov al,[ds:bp+08]
	mov [cs:ysize],al
	stosb
	mov ax,[ss:bp+12]       ;figure out the starting vga offset
	mov bx,ax
	shl ax,6
	shl bx,4
	add ax,bx
	mov bx,[ss:bp+14]
	shr bx,2
	add bx,ax
	mov ax,[ss:bp+06]
	shl ax,14
	add bx,ax
	mov [cs:memoff],bx
	mov cx,[ss:bp+14]       ;figure out the plane of the first pixel
	and cl,3
	mov [cs:planenum],cl
	xor ch,ch
	mov ax,0a000h           ;set up ds
	mov ds,ax
	xor ax,ax
	xor bx,bx
Xcycle:
	mov si,[cs:memoff]              ;reset si
	mov al,04h              ;set the plane read enable
	mov dx,03ceh
	mov ah,[cs:planenum]
	out dx,ax
	xor ax,ax
	xor dl,dl               ;clear dl for the y-counter
Ycycle:
	mov al,[ds:si]          ;get the first pixel data
	mov cl,[cs:evenflag]    ;get selected data using read flag
	and al,15                               
	mov bl,[es:di]
	and bl,240
	shl al,cl
	shl bl,cl
	or al,bl
	stosb
	shr cl,2
	mov bl,cl
	sub di,bx               ;increase di if evenflag is on
	xor bl,1                ;switch evenflag
	shl bl,2
	mov [cs:evenflag],bl            
	add si,50h
	inc dl
	xor ax,ax
	cmp dl,[cs:ysize]
	jb Ycycle
	inc ch
	mov al,[cs:planenum]
	add al,1
	mov bl,al
	shr bl,2
	and al,3
	mov [cs:planenum],al
	mov ax,[cs:memoff]
	add ax,bx
	mov [cs:memoff],ax
	xor ax,ax
	cmp ch,[cs:xsize]
	jb Xcycle
	pop di si ds es bp
	retf 14
endp    getsprite

Proc    Stosprite  ; Pic() Offset Xloc Yloc Page
		   ;  14     12    10   8    6
	push bp
	mov bp,sp
	push es ds si di

	cld
	mov bx,[ss:bp+14]
	mov si,[ds:bx+0ah]      ;put seg and offset of pic() in ds:si
	mov ax,[ss:bp+12]
	shl ax,1
	add si,ax
	mov ds,[ds:bx+02h]
	mov ax,[ss:bp+08]       ;figure out the starting vga offset
	mov bx,ax
	shl ax,6
	shl bx,4
	add ax,bx               ;get yloc*80
	mov bx,[ss:bp+10]
	shr bx,2
	add bx,ax               ;add xloc/4
	mov ax,[ss:bp+06]
	shl ax,14
	add bx,ax               ;add page*16384
	mov di,bx               ;put the offset in di
	mov cx,[ss:bp+10]       ;figure out the plane of the first pixel
	and cl,3
	mov bl,1
	shl bl,cl               ;put it in bl
	mov ax,0a000h           ;set up es
	mov es,ax
	lodsb                   ;get the x and y dimensions of pic
	mov cl,al
	lodsb
	mul cl
	inc ax
	shr ax,1
	mov cx,ax               ;put num of bytes to transfer in cx
STORELOOP:
	mov al,02h              ;set the plane write enable
	mov dx,03c4h
	mov ah,bl
	out dx,ax
	lodsb
	mov [es:di],al
	shl bl,1
	mov al,bl
	shr al,4
	add bl,al
	and bl,15
	mov ah,0
	add di,ax
	loop storeloop
	pop di si ds es bp
	retf 10
endp    Stosprite

Proc    loadsprite ; Pic() offset Xloc Yloc Width Height Page
	push bp
	mov bp,sp
	push es ds si di

	cld
	mov bx,[ss:bp+18]
	mov di,[ds:bx+0ah]
	mov ax,[ss:bp+16]
	shl ax,1
	add di,ax
	mov es,[ds:bx+02h]
	mov ax,[ss:bp+12]       ;figure out the starting vga offset
	mov bx,ax
	shl ax,6
	shl bx,4
	add ax,bx
	mov bx,[ss:bp+14]
	shr bx,2
	add bx,ax
	mov ax,[ss:bp+06]
	shl ax,14
	add bx,ax
	mov si,bx
	mov bx,[ss:bp+14]       ;figure out the plane of the first pixel
	and bl,3
	xor bh,bh
	xor ch,ch
	mov ax,0a000h           ;set up ds
	mov ds,ax
	xor ax,ax
	mov ax,[ss:bp+10]
	stosb                   ;get the x and y dimensions of pic
	mov cl,al
	mov ax,[ss:bp+08]
	stosb
	mul cl
	inc ax
	shr ax,1
	mov cx,ax
LoadLOOP:
	mov al,04h              ;set the plane read enable
	mov dx,03ceh
	mov ah,bl
	out dx,ax
	mov al,[ds:si]
	stosb
	add bl,1
	mov al,bl
	shr al,2
	mov ah,0
	add si,ax
	and bl,3
	loop Loadloop
	pop di si ds es bp
	retf 14
endp    Loadsprite

Proc    bigsprite ; Pic() Pal() Xloc Yloc Page
		   ;  16    14    10   8    6
	push bp
	mov bp,sp
	push es ds si di

	cld
	mov al,4
	mov [cs:evenflag],al
	mov bx,[ss:bp+16]
	mov si,[ds:bx+0ah]
	mov es,[ds:bx+02h]
	mov [cs:picseg],es
	mov bx,[ss:bp+14]
	mov di,[ds:bx+0ah]
	add di,[ss:bp+12]
	mov es,[ds:bx+02h]
	mov ds,[cs:picseg]
	mov [cs:paloff],di
	mov [cs:palseg],es
	lodsb                   ;get the x and y dimensions of pic
	mov [cs:xsize],al
	lodsb
	mov [cs:ysize],al
	mov ax,[ss:bp+08]       ;figure out the starting vga offset
	mov bx,ax
	shl ax,6
	shl bx,4
	add ax,bx
	mov bx,[ss:bp+10]
	shr bx,2
	add bx,ax
	mov ax,[ss:bp+06]
	shl ax,14
	add bx,ax
	mov [cs:memoff],bx
	mov bx,3               ;figure out the plane of the first pixel
	mov [cs:planenum],bl
	xor ch,ch
	mov ax,0a000h           ;set up es
	mov es,ax
	xor ax,ax
XLOOP2:
	mov di,[cs:memoff]              ;reset di
	mov dx,03c4h
	mov al,02h
	mov ah,[cs:planenum]
	out dx,ax
	xor ax,ax
	xor dl,dl               ;clear dl for the y-counter
YLOOP2:
	lodsb                   ;get the first pixel data
	mov cl,[cs:evenflag]    ;get selected data using read flag
	shr al,cl                               
	shr cl,2
	mov bl,cl
	sub si,bx               ;increase si if evenflag is on
	xor bl,1                ;switch evenflag
	shl bl,2
	mov [cs:evenflag],bl            
	and al,15               ;take 4-bits
	mov ds,[cs:palseg]
	mov bx,[cs:paloff]
	add bx,ax
	mov al,[ds:bx]
	xor bh,bh
	mov [es:di],al
	add di,50h
	mov [es:di],al
	add di,50h
	mov ds,[cs:picseg]
	inc dl
	cmp dl,[cs:ysize]
	jb YLOOP2
	inc ch
	mov al,[cs:planenum]
	mov bx,ax
	shr al,2
	shl bl,2
	add bl,al
	and bl,15
	mov [cs:planenum],bl
	shr ax,1
	add [cs:memoff],ax
	cmp ch,[cs:xsize]
	jb XLOOP2
	pop di si ds es bp
	retf 12
endp    bigsprite

Proc    hugesprite ; Pic() Pal() Xloc Yloc Page
		   ;  16    14    10   8    6
	push bp
	mov bp,sp
	push es ds si di

	cld
	mov al,4
	mov [cs:evenflag],al
	mov bx,[ss:bp+16]
	mov si,[ds:bx+0ah]
	mov es,[ds:bx+02h]
	mov [cs:picseg],es
	mov bx,[ss:bp+14]
	mov di,[ds:bx+0ah]
	add di,[ss:bp+12]
	mov es,[ds:bx+02h]
	mov ds,[cs:picseg]
	mov [cs:paloff],di
	mov [cs:palseg],es
	lodsb                   ;get the x and y dimensions of pic
	mov [cs:xsize],al
	lodsb
	mov [cs:ysize],al
	mov ax,[ss:bp+08]       ;figure out the starting vga offset
	mov bx,ax
	shl ax,6
	shl bx,4
	add ax,bx
	mov bx,[ss:bp+10]
	shr bx,2
	add bx,ax
	mov ax,[ss:bp+06]
	shl ax,14
	add bx,ax
	mov [cs:memoff],bx
	mov bx,15               ;figure out the plane of the first pixel
	mov [cs:planenum],bl
	xor ch,ch
	mov ax,0a000h           ;set up es
	mov es,ax
	xor ax,ax
XLOP:
	mov di,[cs:memoff]              ;reset di
	mov dx,03c4h
	mov al,02h
	mov ah,[cs:planenum]
	out dx,ax
	xor ax,ax
	xor dl,dl               ;clear dl for the y-counter
YLOP:
	lodsb                   ;get the first pixel data
	mov cl,[cs:evenflag]    ;get selected data using read flag
	shr al,cl                               
	shr cl,2
	mov bl,cl
	sub si,bx               ;increase si if evenflag is on
	xor bl,1                ;switch evenflag
	shl bl,2
	mov [cs:evenflag],bl            
	and al,15               ;take 4-bits
	mov ds,[cs:palseg]
	mov bx,[cs:paloff]
	add bx,ax
	mov al,[ds:bx]
	xor bh,bh
	mov [es:di],al
	add di,50h
	mov [es:di],al
	add di,50h
	mov [es:di],al
	add di,50h
	mov [es:di],al
	add di,50h
	mov ds,[cs:picseg]
	inc dl
	cmp dl,[cs:ysize]
	jb YLOP
	inc ch
	add [cs:memoff],1
	xor ax,ax
	cmp ch,[cs:xsize]
	jb XLOP
	pop di si ds es bp
	retf 12
endp    hugesprite
end

