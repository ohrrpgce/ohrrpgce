;****************************************************************************
;*
;*
;*** 16 color bmp importer
;* By Brian Fisher
;*
;**** modifed 2-11-98

.386
Ideal 
Model Small
Public Loadbmp,getbmppal,bmpinfo

CodeSeg
handl   dw 0
palptr  dw 0
palseg  dw 0
blu     db 0
gree    db 0
re      db 0
reserv  db 0
red     dw 0
green   dw 0
blue    dw 0
diff    dw 0
color   db 0
count   db 0
start   dw 0
splane  db 0
plane   db 0
fsize   dw 0
exitc   dw 0
widt    dw 0
heigh   dw 0

Proc    bmpinfo         ;file   buff
			; 8      6
	push bp
	mov bp,sp
	push ds si

	mov ax,0
	mov [cs:exitc],ax
	mov si,[ss:bp+08]
	add si,2
	mov dx,[ds:si]
	mov ax,3d00h
	int 21h                 ;open file
	jc notthere
	mov [cs:handl],ax
	mov bx,[ss:bp+06]
	mov dx,[ds:bx+0ah]
	mov si,dx
	mov ds,[ds:bx+02h]
	mov cx,38
	mov ax,3f00h
	mov bx,[cs:handl]
	int 21h
	mov ax,[ds:si]
	cmp ax,19778
	jnz notbmp
	mov [cs:exitc],-1
	mov ax,[ds:si+28]
	mov [ds:si],ax
	mov ax,[ds:si+18]
	mov [ds:si+2],ax
	mov ax,[ds:si+22]
	mov [ds:si+4],ax
	mov ax,[ds:si+30]
	mov [ds:si+8],ax
notbmp:
	mov bx,[cs:handl]
	mov ax,3e00h
	int 21h
notthere:
	mov ax,[cs:exitc]
	pop si ds bp
	retf 4
endp    bmpinfo

Proc    Loadbmp         ;file  xloc   yloc   buff  page
			; 14     12    10     8     6
	push bp
	mov bp,sp
	push ds si es di
	
	mov si,[ss:bp+14]
	add si,2
	mov dx,[ds:si]
	mov ax,3d00h
	int 21h                 ;open file
	jc done
	mov [cs:handl],ax
	mov bx,[ss:bp+08]
	mov dx,[ds:bx+0ah]
	mov si,dx
	mov ds,[ds:bx+02h]
	mov cx,38
	mov ax,3f00h
	mov bx,[cs:handl]
	int 21h
	mov ax,[ds:si]
	cmp ax,19778
	jnz exit
	mov ax,[ds:si+28]
	cmp ax,4
	jnz exit
	mov dx,[ds:si+10]
	mov cx,[ds:si+12]
	mov bx,[cs:handl]
	mov ax,4200h
	int 21h
	mov ax,[ds:si+18]
	mov [cs:widt],ax
	mov ax,[ds:si+22]
	mov [cs:heigh],ax
	mov ax,[ds:si+30]
	mov [cs:exitc],ax
	mov cx,[ds:si+34]
	mov [cs:fsize],cx
	cmp cx,0
	jnz goodsize
	mov cx,-1
goodsize:
	mov bx,[cs:handl]
	mov dx,si
	mov ax,3f00h
	int 21h
	cmp [cs:fsize],0
	jnz dontgetsize
	mov [cs:fsize],ax
dontgetsize:
	mov di,[ss:bp+06]
	shl di,14
	mov ax,0a000h
	mov es,ax
	mov ax,[ss:bp+10]
	add ax,[cs:heigh]
	mov bx,ax
	shl ax,4
	shl bx,6
	add ax,bx
	add di,ax
	mov ax,[ss:bp+12]
	mov bx,ax
	shr bx,2
	add di,bx
	mov [cs:start],di
	and ax,3
	mov cx,ax
	mov al,1
	shl al,cl
	mov [cs:splane],al
	mov [cs:plane],al
	cmp [cs:exitc],0
	jnz morebytes
	call drawstraight
	jmp exit
morebytes:
	lodsw
	cmp al,0
	jz escape
	xor cx,cx
	mov cl,al
	mov bl,ah
	shr bl,4
	and ah,15
drawencoded:
	call drawpixel
	xchg ah,bl
	loop drawencoded
	jmp morebytes
escape:
	cmp ah,0
	jz newline
	cmp ah,1
	jz exit
	cmp ah,2
	jz delta
	xor cx,cx
	mov cl,ah
getword:
	xor dx,dx
	lodsw
drawmore:
	mov bl,al
	test dx,1
	jz gethigh
	and bl,15
	mov al,ah
	jmp gotlow
gethigh:
	shr bl,4
gotlow:
	call drawpixel
	sub cx,1
	jz morebytes
	inc dx
	cmp dx,4
	jz getword
	jmp drawmore
delta:
	lodsw
	;need finish delta
	jmp morebytes
newline:
	sub [cs:start],80
	mov di,[cs:start]
	mov al,[cs:splane]
	mov [cs:plane],al
	jmp morebytes
exit:
	mov bx,[cs:handl]
	mov ax,3e00h
	int 21h
done:
	mov ax,[cs:exitc]
	pop di es si ds bp
	retf 10
endp    Loadbmp

proc    drawstraight
	mov cx,[cs:heigh]
	mov dx,[cs:widt]
	xor ax,ax
nextpixel:
	test ah,1
	jz noloadbyte
	lodsb
	mov bl,al
	shr bl,4
	jmp gotval
noloadbyte:
	mov bl,al
	and bl,15
gotval:
	call drawpixel
	inc ah
	and ah,7
	dec dx
	cmp dx,0
	jnz nextpixel
	mov dx,[cs:widt]
	sub [cs:start],80
	mov di,[cs:start]
	mov bl,[cs:splane]
	mov [cs:plane],bl
	cmp ah,0
	jz noskip
	mov bl,ah
	xor bh,bh
	shr bx,1
	add si,4
	sub si,bx
	xor ax,ax
noskip:
	loop nextpixel
	retn
endp    drawstraight

proc    drawpixel
	push ax dx
	mov ah,[cs:plane]
	mov al,02h
	mov dx,3c4h
	out dx,ax
	mov [es:di],bl
	mov al,ah
	shl al,1
	shr ah,3
	add al,ah
	and al,0fh
	mov [cs:plane],al
	xor ah,ah
	and ax,1
	add di,ax
	pop dx ax
	retn
endp    drawpixel

proc    getbmppal       ;file  master()  pal()  offset
			; 12     10       8       6
	push bp
	mov bp,sp
	push ds si es di
	
	mov si,[ss:bp+12]
	add si,2
	mov dx,[ds:si]
	mov ax,3d00h
	int 21h                 ;open file
	jc done2
	mov [cs:handl],ax
	mov bx,ax
	xor cx,cx
	mov dx,36h
	mov ax,4200h
	int 21h
	mov bx,[ss:bp+08]
	mov di,[ds:bx+0ah]
	mov ax,[ds:bx+02h]
	mov es,ax
	mov ax,[ss:bp+06]
	add di,ax
	mov bx,[ss:bp+10]
	mov ax,[ds:bx+0ah]
	mov [cs:palptr],ax
	mov ax,[ds:bx+02h]
	mov [cs:palseg],ax
	mov [cs:count],16
nextcolor:
	mov cx,4
	mov ax,cs
	mov ds,ax
	lea dx,[cs:blu]
	mov ah,3fh
	mov bx,[cs:handl]
	int 21h                 ;load triple
	mov ds,[cs:palseg]
	mov si,[cs:palptr]
	mov [cs:diff],1000
	xor cx,cx
findcolor:
	xor ah,ah
	mov al,[cs:re]
	mov [cs:red],ax
	mov al,[cs:gree]
	mov [cs:green],ax
	mov al,[cs:blu]
	mov [cs:blue],ax
	lodsw
	shl ax,2
	cmp ax,[cs:red]
	jb low1
	sub ax,[cs:red]
	mov [cs:red],ax
	jmp next1
low1:
	sub [cs:red],ax
next1:
	lodsw
	shl ax,2
	cmp ax,[cs:green]
	jb low2
	sub ax,[cs:green]
	mov [cs:green],ax
	jmp next2
low2:
	sub [cs:green],ax
next2:
	lodsw
	shl ax,2
	cmp ax,[cs:blue]
	jb low3
	sub ax,[cs:blue]
	mov [cs:blue],ax
	jmp next3
low3:
	sub [cs:blue],ax
next3:
	call maxcolor
	cmp [cs:diff],ax
	jb oldcolor
	mov [cs:color],cl
	mov [cs:diff],ax
oldcolor:
	inc cx
	cmp cx,256
	jnz findcolor
	mov al,[cs:color]
	stosb
	mov al,[cs:count]
	dec al
	mov [cs:count],al
	cmp al,0
	ja nextcolor
	mov bx,[cs:handl]
	mov ah,3eh
	int 21h
done2:
	pop di es si ds bp
	retf 8
endp    getbmppal

proc    maxcolor
	mov ax,[cs:red]
	cmp ax,[cs:green]
	ja redgood
	mov ax,[cs:green]
redgood:
	cmp ax,[cs:blue]
	ja notblue
	mov ax,[cs:blue]
notblue:
	mov bx,[cs:red]
	add ax,bx
	mov bx,[cs:green]
	add ax,bx
	mov bx,[cs:blue]
	add ax,bx
	shr ax,2
	retn
endp    maxcolor
end
