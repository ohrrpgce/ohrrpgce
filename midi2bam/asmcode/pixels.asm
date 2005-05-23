;****************************************************************************
;*
;*
;*** putpixel and readpixel for mode x
;* By Brian Fisher
;*
;* modifed March 6, 1998

.286
Ideal 
Model Small
Public Putpixel,Readpixel,Rectangle,Fuzzyrect

CodeSeg

xloc    dw 0
yloc    dw 0
wid     dw 0
height  dw 0
middle	db 0

Proc Putpixel   ;  Xloc,  Yloc,  Color, page
		;   12     10      8      6
	push bp
	mov bp,sp
	push es di
	
	mov ax,[ss:bp+10]
	mov bx,ax
	shl ax,6
	shl bx,4
	add ax,bx
	mov bx,[ss:bp+12]
	shr bx,2
	add ax,bx
	mov di,ax
	mov ax,[ss:bp+06]
	shl ax,14
	add di,ax
	mov cx,[ss:bp+12]
	and cl,3
	mov bl,1
	shl bl,cl
	mov dx,03c4h
	mov al,02h
	mov ah,bl
	out dx,ax
	mov ax,0a000h
	mov es,ax
	mov ax,[ss:bp+08]
	mov [es:di],al

	pop di es bp
	retf 8
endp    Putpixel

Proc    Readpixel  ;  Xloc,  Yloc,  page
		   ;   10      8     6
	push bp
	mov bp,sp
	push ds si
	
	mov ax,[ss:bp+08]
	mov bx,ax
	shl ax,6
	shl bx,4
	add ax,bx
	mov bx,[ss:bp+10]
	shr bx,2
	add ax,bx
	mov si,[ss:bp+06]
	shl si,14
	add si,ax
	mov cx,[ss:bp+10]
	and cl,3
	mov dx,03ceh
	mov al,04h
	mov ah,cl
	out dx,ax
	mov ax,0a000h
	mov ds,ax
	mov al,[ds:si]
	xor ah,ah

	pop si ds bp
	retf 6
endp    Readpixel

Proc    Rectangle       ;x, y, width, height, color, page
	push bp
	mov bp,sp
	push es ds si di

	cld
	xor ax,ax
	mov [cs:middle],al
	mov ax,[ss:bp+14]       ;figure out the starting vga offset
	cmp ax,199
	jg bady
	mov bx,[ss:bp+10]
	cmp bx,0
	jz bady
	mov cx,ax
	add cx,bx
	cmp cx,0
	js bady
	cmp ax,0
	jns notnegy
	mov bx,cx
	mov ax,0
	jmp goodheight
notnegy:
	cmp cx,200
	jng goodheight
	mov bx,200
	sub bx,ax
	jmp goodheight
bady:
	jmp rectdone
goodheight:
	mov [cs:yloc],ax
	mov [cs:height],bx
	mov bx,ax
	shl ax,6
	shl bx,4
	add ax,bx
	mov bx,[ss:bp+16]       ;figure out the starting vga offset
	cmp bx,319
	jg badx
	mov dx,[ss:bp+12]
	cmp dx,0
	jz badx
	mov cx,bx
	add cx,dx
	cmp cx,0
	js badx
	cmp bx,0
	jns notnegx
	mov dx,cx
	mov bx,0
	jmp goodwidth
notnegx:
	cmp cx,320
	jng goodwidth
	mov dx,320
	sub dx,bx
	jmp goodwidth
badx:
	jmp rectdone
goodwidth:
	mov [cs:xloc],bx
	mov [cs:wid],dx
	shr bx,2
	add bx,ax
	mov ax,[ss:bp+06]
	shl ax,14
	add bx,ax
	mov si,bx
	mov ax,0a000h           ;set up es
	mov es,ax
	mov cx,[cs:xloc]
	and cx,3
	cmp cx,0
	jz nextblock
	xor ah,ah
	mov al,1
	shl al,cl
	mov bx,cx
	mov cx,4
	sub cx,bx
	cmp cx,[cs:wid]
	jb goodsize
	mov cx,[cs:wid]
goodsize:
	sub [cs:wid],cx
fillah:
	shl ah,1
	add ah,al
	loop fillah
setdraw:
	mov al,02h
	mov dx,03c4h
	out dx,ax
xlop:
	mov di,si
	xor dx,dx
	mov al,[ss:bp+08]
ylop:
	mov [es:di],al
	add di,50h
	inc dx
	cmp dx,[cs:height]
	jb ylop
	inc si
nextblock:
	mov cx,[cs:wid]
	cmp cx,0
	jz rectdone
	cmp cx,3
	ja doblock
	mov ah,0
	mov al,1
	mov [cs:wid],0
	jmp fillah
doblock:
	sub [cs:wid],4
	mov ah,0fh
	cmp [cs:middle],ah
	jz xlop
	mov [cs:middle],ah
	jmp setdraw
rectdone:
	pop di si ds es bp
	retf 12
endp    rectangle

Proc    Fuzzyrect       ;x, y, width, height, color, page
	push bp
	mov bp,sp
	push es ds si di

	cld
	mov ax,[ss:bp+14]       ;figure out the starting vga offset
	mov bx,ax
	shl ax,6
	shl bx,4
	add ax,bx
	mov bx,[ss:bp+16]
	shr bx,2
	add bx,ax
	mov ax,[ss:bp+06]
	shl ax,14
	add bx,ax
	mov si,bx
	mov bx,[ss:bp+10]
	mov bh,bl
	mov bl,10
	xor cx,cx
	mov ax,0a000h           ;set up es
	mov es,ax
nexline:
	mov di,si
	mov al,02h              ;set the plane write enable
	mov dx,03c4h
	mov ah,bl
	out dx,ax
	mov al,[ss:bp+08]
	mov [es:di],al
	mov cx,[ss:bp+12]
	add cx,3
	shr cx,2
	rep stosb
	add si,50h
	xor bl,0fh
	dec bh
	cmp bh,0
	jnz nexline
	pop di si ds es bp
	retf 12
endp    fuzzyrect
end
