;****************************************************************************
;*
;*
;*** putpixel and readpixel for mode x
;* By Brian Fisher
;*
;*
.286
Ideal 
Model Small
Public Putpixel,Readpixel,Rectangle

CodeSeg

Xloc    dw 0
Yloc    dw 0
Color   db 0

Proc Putpixel   ;  Xloc,  Yloc,  Color
		;   10      8      6
	push bp
	mov bp,sp
	push ds es si di
	
	mov al,[ss:bp+06]
	mov [cs:color],al
	
	mov ax,[ss:bp+08]
	mov [cs:yloc],ax
	mov bx,[cs:yloc]
	shl ax,6
	shl bx,4
	add ax,bx
	mov bx,[ss:bp+10]
	mov [cs:xloc],bx
	shr bx,2
	add ax,bx
	mov di,ax
	mov cx,[cs:xloc]
	and cl,3
	mov bl,1
	shl bl,cl
	mov dx,03c4h
	mov al,02h
	mov ah,bl
	out dx,ax
	mov ax,0a000h
	mov es,ax
	mov al,[cs:color]
	stosb

	pop di si es ds bp
	retf 6
endp    Putpixel
Proc    Readpixel  ;  Xloc,  Yloc,  page
		   ;   10      8     6
	push bp
	mov bp,sp
	push ds si
	
	mov ax,[ss:bp+08]
	mov [cs:yloc],ax
	mov bx,[cs:yloc]
	shl ax,6
	shl bx,4
	add ax,bx
	mov bx,[ss:bp+10]
	mov [cs:xloc],bx
	shr bx,2
	add ax,bx
	mov si,[ss:bp+06]
	shl si,14
	add si,ax
	mov cx,[cs:xloc]
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
	mov cx,[ss:bp+16]       ;figure out the plane of the first pixel
	and cl,3
	mov bx,1
	shl bl,cl
	xor cx,cx
	mov ax,0a000h           ;set up es
	mov es,ax
xlop:
	mov di,si
	mov al,02h              ;set the plane write enable
	mov dx,03c4h
	mov ah,bl
	out dx,ax
	xor dl,dl               ;clear dl for the y-counter
ylop:
	mov al,[ss:bp+08]
	mov [es:di],al
	add di,50h
	inc dl
	mov al,[ss:bp+10]
	cmp dl,al
	jb ylop
	inc cx
	shl bl,1
	mov ax,bx
	shr al,4
	add bl,al
	and bx,15
	add si,ax
	mov ax,[ss:bp+12]
	cmp cx,ax
	jb xlop
	pop di si ds es bp
	retf 12
endp    rectangle
end
