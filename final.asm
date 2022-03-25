IDEAL
MODEL small
STACK 100h
DATASEG
xcharacter dw 1
ycharacter dw ?
turncount  dw 0
count dw 0
counter db 0
posx dw 0
posy dw 0 
color1 db ?
level1count dw 0
level2count dw 0
level3count dw 0
level4count dw 0
level5count dw 0
level6count dw 0
level7count dw 0
level8count dw 0
level9count dw 0
character db 0Eh,0Eh,0Eh,0Eh,00,00,00,00,00,00,00
  db 04,00,0Eh,00,00,00,0Eh,00,00,00,00,0Eh,00,00
  db 04,00,0Eh,00,00,00,0Eh,00,00,00,0Eh,0Eh,00,00
  db 04,00,0Eh,00,00,00,0Eh,00,00,0Eh,00,0Eh,00,00
  db 04,00,0Eh,00,00,00,0Eh,00,0Eh,00,00,0Eh,00,00
  db 04,00,0Eh,00,00,00,0Eh,00,00,00,00,0Eh,00,00
  db 04,00,0Eh,0Eh,0Eh,0Eh,00,00,00,00,0Eh,00,00
  db 04,00,0Eh,00,00,00,00,00,00,00,00,0Eh,00,00
  db 04,00,0Eh,00,00,00,00,00,00,00,00,0Eh,00,00
  db 04,00,0Eh,00,00,00,00,00,00,00,00,0Eh,00,00
  db 04,00,0Eh,00,00,00,00,00,00,00,00,0Eh,00,00
  db 04,00,0Eh,00,00,00,00,00,00,00,00,0Eh,00,00
  db 04,00,0Eh,00,00,00,00,00,00,00,00,0Eh,00,00
  db 04,00,0Eh,00,00,00,00,00,00,00,00,0Eh,00,00
  db 04,00,0Eh,00,00,00,00,00,00,00,00,0Eh,00,00
filename db 'start.bmp',0

filename1 db 'player1.bmp',0

filename2 db 'gameover.bmp',0

filehandle dw ?

Header db 54 dup (0)

Palette db 256*4 dup (0)

ScrLine db 320 dup (0)

;ErrorMsg db 'Error', 13, 10,'$'
x dw 0
y dw 0
len dw 0
color db ?
empty1 db '          $'
empty db '            $'
dice1 db 'you rolled a one$'
dice2 db 'you rolled a two$'
dice3 db 'you rolled a three$'
dice4 db 'you rolled a four$'
dice5 db 'you rolled a five$'
dice6 db 'you rolled a six$'
message db 'Welcome to ladders and snakes:$'
message1 db 'press R to roll green=snake\brown=ladder$'
message2 db ' press space to continue game$'
turnsleft19 db '	you have 19 turns left to win$'
turnsleft18 db '	you have 18 turns left to win$'
turnsleft17 db '	you have 17 turns left to win$'
turnsleft16 db '	you have 16 turns left to win$'
turnsleft15 db '	you have 15 turns left to win$'
turnsleft14 db '	you have 14 turns left to win$'
turnsleft13 db '	you have 13 turns left to win$'
turnsleft12 db '	you have 12 turns left to win$'
turnsleft11 db '	you have 11 turns left to win$'
turnsleft10 db '	you have 10 turns left to win$'
turnsleft9 db  '	you have 9 turns left to win$'
turnsleft8 db  '	you have 8 turns left to win$'
turnsleft7 db  '	you have 7 turns left to win$'
turnsleft6 db  '	you have 6 turns left to win$'
turnsleft5 db  '	you have 5 turns left to win$'
turnsleft4 db  '	you have 4 turns left to win$'
turnsleft3 db  '	you have 3 turns left to win$'
turnsleft2 db  '	you have 2 turns left to win$'
turnsleft1 db  '	you have 1 turn  left to win$'
Clock equ es:6Ch
EndMessage db 'Done',13,10,'$'
divisorTable db 10,1,0
; --------------------------
; Your variables here
; --------------------------		 
CODESEG
;this procedure prints the character on the screen.
proc printcharacter
	push [y] ;saves y value.
	push [x] ;saves x value.
	cmp [level1count],1  ;checks if its the first time the player is on the level.
	jne print            
	cmp [level2count],1	 ;checks if its the first time the player is on the level.
	jne print
	cmp [level3count],1	 ;checks if its the first time the player is on the level.
	jne print
	cmp [level4count],1  ;checks if its the first time the player is on the level.
	jne print
	cmp [level5count],1  ;checks if its the first time the player is on the level.
	jne print
	cmp [level6count],1  ;checks if its the first time the player is on the level.
	jne print
	cmp [level7count],1  ;checks if its the first time the player is on the level.
	jne print
	cmp [level8count],1  ;checks if its the first time the player is on the level.
	jne print
	cmp [level9count],1  ;checks if its the first time the player is on the level.
	jne print
	mov [posx],0		 ;resets the players position to first square on the level if it is first time on level.
print:
	mov ax,[posx]        ;moves the number of squares it has moved on the level to ax.
	mov [x],1			 ;moves 1 into the players x position to move it to start.
	add [x],ax			 ;adds the number of squares it has moved on level to x position.
	mov ax, [ycharacter] ;in order to print the character we move the number of levels it has moved to ax.
	mov [y],ax			 ;moves into the y position the level the character is on.
	mov dx,offset character ;moves offset in order to print character.
	mov bx,dx 			 ; moves into bx to move up in memory.
	mov cx,84			 ;moves into cx the numebr of times the loop goes.
printcharacter1:
	push cx				;saves value in cx.
	mov ah,[bx] 		;moves into ah the color of the pixel of the character.
	mov bh,0h			
	mov cx,[x]			;moves x position into cx
	mov dx,[y]			;moves y positon into dx
	mov al,ah			;moves color into al
	mov ah,0ch			
	int 10h
	inc bx				;adds 1 to bx moves up in memory.
	inc [y]				;adds 1 to y
	inc [counter]		;adds 1 to counter
	cmp [counter],12	;checks if characters y position. 
	jne continue
	mov [counter],0		;if it is reset the character's y position.
	inc [x]				;moves one to the right inc x position.
	mov ax,[ycharacter] ;moves into ax the y position
	mov [y],ax			;moves into y position original character y position
continue:
	pop cx ;returns value into cx in order to initiate the loop
	loop printcharacter1 ;loop that prints the character
	pop [x] ;returns value into x position.
	pop [y] ;returns value into y position.
	ret 
endp printcharacter
;-----------------------------------------------------------
;-----------------------------------------------------------
proc printturncount
	cmp [turncount],1 ;checks what turn the player is on.
	je turn1
	cmp [turncount],2 ;checks what turn the player is on.
	je turn2
	cmp [turncount],3 ;checks what turn the player is on.
	je turn3
	cmp [turncount],4 ;checks what turn the player is on.
	je turn4
	cmp [turncount],5 ;checks what turn the player is on.
	je turn5 
	cmp [turncount],6 ;checks what turn the player is on.
	je turn6
	cmp [turncount],7 ;checks what turn the player is on.
	je turn7
	cmp [turncount],8 ;checks what turn the player is on.
	je turn8 
	cmp [turncount],9 ;checks what turn the player is on.
	je turn9
	cmp [turncount],10 ;checks what turn the player is on.
	je turn10
	cmp [turncount],11 ;checks what turn the player is on.
	je turn11
	cmp [turncount],12 ;checks what turn the player is on.
	je turn12
	cmp [turncount],13 ;checks what turn the player is on.
	je turn13
	cmp [turncount],14 ;checks what turn the player is on.
	je turn14
	cmp [turncount],15 ;checks what turn the player is on.
	je turn15
	cmp [turncount],16 ;checks what turn the player is on.
	je turn16
	cmp [turncount],17 ;checks what turn the player is on.
	je turn17
	cmp [turncount],18 ;checks what turn the player is on.
	je turn18
	cmp [turncount],19 ;checks what turn the player is on.
	je turn19
turn1:  ;calls procedure to print the turn for the user
	call print19turnsleft
	jmp finish
turn2:  ;calls procedure to print the turn for the user
	call print18turnsleft
	jmp finish
turn3:  ;calls procedure to print the turn for the user
	call print17turnsleft
	jmp finish
turn4:  ;calls procedure to print the turn for the user
	call print16turnsleft
	jmp finish
turn5:  ;calls procedure to print the turn for the user
	call print15turnsleft
	jmp finish
turn6:  ;calls procedure to print the turn for the user
	call print14turnsleft
	jmp finish
turn7:  ;calls procedure to print the turn for the user
	call print13turnsleft
	jmp finish
turn8:  ;calls procedure to print the turn for the user
	call print12turnsleft
	jmp finish
turn9:  ;calls procedure to print the turn for the user
	call print11turnsleft
	jmp finish
turn10: ;calls procedure to print the turn for the user
	call print10turnsleft
	jmp finish
turn11: ;calls procedure to print the turn for the user
	call print9turnsleft
	jmp finish
turn12: ;calls procedure to print the turn for the user
	call print8turnsleft
	jmp finish
turn13: ;calls procedure to print the turn for the user
	call print7turnsleft
	jmp finish
turn14: ;calls procedure to print the turn for the user
	call print6turnsleft
	jmp finish
turn15: ;calls procedure to print the turn for the user
	call print5turnsleft
	jmp finish
turn16: ;calls procedure to print the turn for the user
	call print4turnsleft
	jmp finish
turn17: ;calls procedure to print the turn for the user
	call print3turnsleft
	jmp finish
turn18: ;calls procedure to print the turn for the user
	call print2turnsleft
	jmp finish
turn19: ;calls procedure to print the turn for the user
	call print1turnsleft
	jmp finish
finish:
	ret
endp printturncount
;-----------------------------------------------------------
;-----------------------------------------------------------
proc print19turnsleft
	push dx ;saves value in dx
	mov dx, offset turnsleft19 ;moves offset of msg into dx
	mov ah,9  ;prints message on screen
	int 21h
	pop dx	  ;moves value back into dx
	ret
endp print19turnsleft
;-----------------------------------------------------------
;-----------------------------------------------------------
proc print18turnsleft
	push dx  ;saves value in dx
	mov dx, offset turnsleft18 ;moves offset of msg into dx
	mov ah,9  ;prints message on screen
	int 21h
	pop dx   ;moves value back into dx
	ret
endp print18turnsleft
;-----------------------------------------------------------
;-----------------------------------------------------------
proc print17turnsleft
	push dx	 ;saves value in dx
	mov dx, offset turnsleft17 ;moves offset of msg into dx
	mov ah,9  ;prints message on screen
	int 21h 
	pop dx   ;moves value back into dx
	ret
endp print17turnsleft
;-----------------------------------------------------------
;-----------------------------------------------------------
proc print16turnsleft
	push dx	 ;saves value in dx
	mov dx, offset turnsleft16 ;moves offset of msg into dx
	mov ah,9  ;prints message on screen
	int 21h
	pop dx   ;moves value back into dx
	ret
endp print16turnsleft
;-----------------------------------------------------------
;-----------------------------------------------------------
proc print15turnsleft
	push dx	 ;saves value in dx
	mov dx, offset turnsleft15 ;moves offset of msg into dx
	mov ah,9  ;prints message on screen
	int 21h
	pop dx   ;moves value back into dx
	ret
endp print15turnsleft
;-----------------------------------------------------------
;-----------------------------------------------------------
proc print14turnsleft
	push dx  ;saves value in dx
	mov dx, offset turnsleft14 ;moves offset of msg into dx
	mov ah,9  ;prints message on screen
	int 21h
	pop dx   ;moves value back into dx
	ret
endp print14turnsleft
;-----------------------------------------------------------
;-----------------------------------------------------------
proc print13turnsleft
	push dx  ;saves value in dx
	mov dx, offset turnsleft13 ;moves offset of msg into dx
	mov ah,9  ;prints message on screen
	int 21h
	pop dx   ;moves value back into dx
	ret
endp print13turnsleft
;-----------------------------------------------------------
;-----------------------------------------------------------
proc print12turnsleft
	push dx  ;saves value in dx
	mov dx, offset turnsleft12 ;moves offset of msg into dx
	mov ah,9  ;prints message on screen
	int 21h
	pop dx   ;moves value back into dx
	ret
endp print12turnsleft
;-----------------------------------------------------------
;-----------------------------------------------------------
proc print11turnsleft
	push dx  ;saves value in dx
	mov dx, offset turnsleft11 ;moves offset of msg into dx
	mov ah,9  ;prints message on screen
	int 21h
	pop dx   ;moves value back into dx
	ret
endp print11turnsleft
;-----------------------------------------------------------
;-----------------------------------------------------------
proc print10turnsleft
	push dx  ;saves value in dx
	mov dx, offset turnsleft10 ;moves offset of msg into dx
	mov ah,9  ;prints message on screen
	int 21h
	pop dx   ;moves value back into dx
	ret 
endp print10turnsleft
;-----------------------------------------------------------
;-----------------------------------------------------------
proc print9turnsleft
	push dx  ;saves value in dx
	mov dx, offset turnsleft9 ;moves offset of msg into dx
	mov ah,9  ;prints message on screen
	int 21h
	pop dx   ;moves value back into dx
	ret
endp print9turnsleft
;-----------------------------------------------------------
;-----------------------------------------------------------
proc print8turnsleft
	push dx  ;saves value in dx
	mov dx, offset turnsleft8 ;moves offset of msg into dx
	mov ah,9  ;prints message on screen
	int 21h
	pop dx   ;moves value back into dx
	ret
endp print8turnsleft
;-----------------------------------------------------------
;-----------------------------------------------------------
proc print7turnsleft
	push dx  ;saves value in dx
	mov dx, offset turnsleft7 ;moves offset of msg into dx
	mov ah,9  ;prints message on screen
	int 21h
	pop dx   ;moves value back into dx
	ret
endp print7turnsleft
;-----------------------------------------------------------
;-----------------------------------------------------------
proc print6turnsleft
	push dx  ;saves value in dx
	mov dx, offset turnsleft6 ;moves offset of msg into dx
	mov ah,9  ;prints message on screen
	int 21h
	pop dx   ;moves value back into dx
	ret
endp print6turnsleft
;-----------------------------------------------------------
;-----------------------------------------------------------
proc print5turnsleft
	push dx  ;saves value in dx
	mov dx, offset turnsleft5 ;moves offset of msg into dx
	mov ah,9  ;prints message on screen
	int 21h
	pop dx   ;moves value back into dx
	ret
endp print5turnsleft
;-----------------------------------------------------------
;-----------------------------------------------------------
proc print4turnsleft
	push dx  ;saves value in dx
	mov dx, offset turnsleft4 ;moves offset of msg into dx
	mov ah,9  ;prints message on screen
	int 21h
	pop dx   ;moves value back into dx
	ret
endp print4turnsleft
;-----------------------------------------------------------
;-----------------------------------------------------------
proc print3turnsleft
	push dx  ;saves value in dx
	mov dx, offset turnsleft3 ;moves offset of msg into dx
	mov ah,9  ;prints message on screen
	int 21h
	pop dx   ;moves value back into dx
	ret
endp print3turnsleft
;-----------------------------------------------------------
;-----------------------------------------------------------
proc print2turnsleft
	push dx  ;saves value in dx
	mov dx, offset turnsleft2 ;moves offset of msg into dx
	mov ah,9  ;prints message on screen
	int 21h 
	pop dx   ;moves value back into dx
	ret
endp print2turnsleft
;-----------------------------------------------------------
;-----------------------------------------------------------
proc print1turnsleft
	push dx  ;saves value in dx
	mov dx, offset turnsleft1 ;moves offset of msg into dx
	mov ah,9  ;prints message on screen
	int 21h
	pop dx   ;moves value back into dx
	ret
endp print1turnsleft
;-----------------------------------------------------------
;-----------------------------------------------------------	
proc OpenFile

    ;Open file

    mov ah, 3Dh
    xor al, al
    mov dx, offset filename
    int 21h

    ;jc openerror
    mov [filehandle], ax
    ret

    ;openerror:
    ;mov dx, offset ErrorMsg
    ;mov ah, 9h
    ;int 21h
    ret
endp OpenFile
;-----------------------------------------------------------
;-----------------------------------------------------------
proc OpenFile1

    ; Open file

    mov ah, 3Dh
    xor al, al
    mov dx, offset filename1
    int 21h

    ;jc openerror
    mov [filehandle], ax
    ret

    ;openerror:
    ;mov dx, offset ErrorMsg
    ;mov ah, 9h
    ;int 21h
    ret
endp OpenFile1
;-----------------------------------------------------------
;-----------------------------------------------------------
proc OpenFile2

    ; Open file

    mov ah, 3Dh
    xor al, al
    mov dx, offset filename2
    int 21h

    ;jc openerror
    mov [filehandle], ax
    ret

    ;openerror:
    ;mov dx, offset ErrorMsg
    ;mov ah, 9h
    ;int 21h
    ret
endp OpenFile2
;-----------------------------------------------------------
;-----------------------------------------------------------
proc ReadHeader

    ; Read BMP file header, 54 bytes

    mov ah,3fh
    mov bx, [filehandle]
    mov cx,54
    mov dx,offset Header
    int 21h
    ret
endp ReadHeader
;-----------------------------------------------------------
;-----------------------------------------------------------
proc ReadPalette
    ; Read BMP file color palette, 256 colors * 4 bytes (400h)

    mov ah,3fh
    mov cx,400h
    mov dx,offset Palette
    int 21h
    ret
endp ReadPalette
;-----------------------------------------------------------
;-----------------------------------------------------------
proc CopyPal

    ; Copy the colors palette to the video memory
    ; The number of the first color should be sent to port 3C8h
    ; The palette is sent to port 3C9h

    mov si,offset Palette
    mov cx,256
    mov dx,3C8h
    mov al,0

    ; Copy starting color to port 3C8h

    out dx,al

    ; Copy palette itself to port 3C9h

    inc dx
PalLoop:

    ; Note: Colors in a BMP file are saved as BGR values rather than RGB.

    mov al,[si+2] ; Get red value.
    shr al,2 ; Max. is 255, but video palette maximal

    ; value is 63. Therefore dividing by 4.

    out dx,al ; Send it.
    mov al,[si+1] ; Get green value.
    shr al,2
    out dx,al ; Send it.
    mov al,[si] ; Get blue value.
    shr al,2
    out dx,al ; Send it.
    add si,4 ; Point to next color.

    ; (There is a null chr. after every color.)

    loop PalLoop
    ret
endp CopyPal
;-----------------------------------------------------------
;-----------------------------------------------------------
proc CopyBitmap

    ; BMP graphics are saved upside-down.
    ; Read the graphic line by line (200 lines in VGA format),
    ; displaying the lines from bottom to top.

    mov ax, 0A000h
    mov es, ax
    mov cx,200
PrintBMPLoop:
    push cx

    ; di = cx*320, point to the correct screen line

    mov di,cx
    shl cx,6
    shl di,8
    add di,cx

    ; Read one line

    mov ah,3fh
    mov cx,320
    mov dx,offset ScrLine
    int 21h

    ; Copy one line into video memory

    cld 

    ; Clear direction flag, for movsb

    mov cx,320
    mov si,offset ScrLine
    rep movsb 

    ; Copy line to the screen
    ;rep movsb is same as the following code:
    ;mov es:di, ds:si
    ;inc si
    ;inc di
    ;dec cx
    ;loop until cx=0

    pop cx
    loop PrintBMPLoop
    ret
endp CopyBitmap
;-----------------------------------------------------------
;-----------------------------------------------------------
proc printNumber
	push ax
	push bx
	push dx
	mov bx,offset divisorTable
nextDigit:
	xor ah,ah ; dx:ax = number
	div [byte ptr bx] ; al = quotient, ah = remainder
	add al,'0'
	call printCharacter ; Display the quotient
	mov al,ah ; ah = remainder
	add bx,1 ; bx = address of next divisor
	cmp [byte ptr bx],0 ; Have all divisors been done?
	jne nextDigit
	mov ah,2
	mov dl,13	;starts a new line
	int 21h
	mov dl,10	;goes to begining of line
	int 21
	pop dx
	pop bx
	pop ax
	ret
endp printNumber
;-----------------------------------------------------------
;-----------------------------------------------------------
proc drawhorline
	push cx
	mov cx, [len]    ;puts the amount of pixels that need to be printed in cx for loop.
horizontal:
	push cx
	mov bh,0h		
	mov cx,[x]	;moves x position into cx
	mov dx,[y]  ;moves y position into dx
	mov al,[color]  ;moves color into al
	mov ah,0ch
	int 10h		;prints the pixel on the screen.
	inc [x]
	pop cx
	loop horizontal
	pop cx
	ret
endp drawhorline
;-----------------------------------------------------------
;-----------------------------------------------------------
proc drawvertline
	push cx
	mov cx, [len]   ;puts the amount of pixels that need to be printed in cx for loop.
vertical:
	push cx
	mov bh,0h
	mov cx,[x] ;moves x position into cx
	mov dx,[y] ;moves y position into dx
	mov al,[color] ;moves color into al
	mov ah,0ch
	int 10h    ;prints the pixel on the screen.
	inc [y]
	pop cx
	loop vertical
	pop cx
	ret
endp drawvertline
;-----------------------------------------------------------
;-----------------------------------------------------------
start:
	mov ax, @data
	mov ds, ax
; --------------------------
; Your code here
; --------------------------
	; Graphic mode
	mov ax, 13h
	int 10h
	; Process BMP file
    call OpenFile
    call ReadHeader
    call ReadPalette
    call CopyPal
    call CopyBitmap

    ; Wait for key press
    mov ah,1
	int 21h
printboard:
	mov ax,13h     ;graphic mode
	int 10h
	mov cx, 70	
	space:
	mov dx, offset empty
	mov ah,9
	int 21h
	loop space   ;loops an empty message to move down in lines in order to print other message on bottom.
	mov dx, offset message
	mov ah,9	;prints message on screen
	int 21h
	mov dx, offset empty1
	mov ah,9    ;prints a space in between the messages.
	int 21h
	mov dx, offset message1
	mov ah,9    ;prints message on screen
	int 21h
	mov cx,11
	jmp linehor
player1wins:   ;if player wins displayes a "you win" bmp
	; Process BMP file
    call OpenFile1
    call ReadHeader
    call ReadPalette
    call CopyPal
    call CopyBitmap

    ; Wait for key press
    mov ah,1
	int 21h
	jmp exit
linehor:
	mov [len],320       ;moves 320 into len because the screen is 320 in length.
	mov [color],0Fh	    ;moves 0Fh into color or the color white.
	call drawhorline	;calls procedure that prints hor lines of the game board.
	add [y],15
	loop linehor
	mov cx,10           ;does this 10 times in order to draw full horizontal board.
	mov [x],0
	mov [y],0
linevert:	
	mov [len],160      ; moves 160 into len becasue the board doesnt go all the way down to 200.
	mov [color],0Fh	   ;moves 0Fh into color or the color white.
	call drawvertline  ;calls procedure that prints vertical lines of the game board.
	mov [y],0
	add [x],32
	loop linevert      ;does this 10 times in order to draw full vertical board.
	cmp [count],99     ;count is the number of sqaures the player went if the player went more than 99 he wins.
	jae player1wins
	cmp [count],98     ;theres a snake on square 98 if the player lands on it he goes down.
	je snakemov40
	cmp [count],89	   ;if the player has gone past 89 he goes up a level.
	ja shortcut10000
	cmp [count],83	   ;theres a snake on square 83 if the player lands on it he goes down.
	je snakemov300
	cmp [count],79     ;if the player has gone past 89 he goes up a level.
	ja shortcut9000
	cmp [count],76     ;theres a ladder on square 76 if the player lands on it he goes up.
	je  shortcutladder400
	cmp [count],69     ;if the player has gone past 89 he goes up a level.
	ja shortcut8000
	cmp [count],59     ;if the player has gone past 89 he goes up a level.
	ja shortcut7000
	cmp [count],49     ;if the player has gone past 89 he goes up a level.
	ja shortcut16000
	cmp [count],45     ;theres a snake on square 45 if the player lands on it he goes down.
	je snakemov120
	cmp [count],39     ;if the player has gone past 89 he goes up a level.
	ja shortcut1500
	cmp [count],38     ;theres a ladder on square 38 if the player lands on it he goes up.
	je laddermov300
	cmp [count],32     ;theres a ladder on square 32 if the player lands on it he goes up.
	je laddermov200
	cmp [count],29    ;if the player has gone past 89 he goes up a level.
	ja shortcut400
	cmp [count],20    ;theres a snake on square 20 if the player lands on it he goes down.
	je snakemov10
	cmp [count],19    ;if the player has gone past 89 he goes up a level.
	ja shortcut300
	cmp [count],9     ;if the player has gone past 89 he goes up a level.
	ja shortcut1200
	cmp [count],6     ;theres a ladder on square 6 if the player lands on it he goes up.
	je laddermov10
	mov [ycharacter],145
	call printcharacter  ;prints character on screen after checking what level he is on.
	jmp snake1
;-------------------------------------------------
;-------------------------------------------------
;from here up to where said otherwise are shortcuts to put jump commands in range.
snakemov40:
	jmp snakemov4
shortcut10000:
	jmp shortcut1000
snakemov300:
	jmp snakemov30
shortcutladder400:
	jmp shortcutladder40
shortcut1200:
	jmp shortcut120
shortcut9000:
	jmp shortcut900
shortcut7000:
	jmp shortcut70
shortcut16000:
	jmp shortcut1600
shortcut8000:
	jmp shortcut800
laddermov200:
	jmp laddermov20
laddermov300:
	jmp laddermov30
snakemov4:
	mov [count],92
	mov [posx],64
	jmp printboard
shortcut1000:
	jmp shortcut100
snakemov30:
	jmp snakemov3
shortcut800:
	jmp shortcut80
snakemov120:
	jmp snakemov12
shortcut1500:
	jmp shortcut150
shortcut400:
	jmp shortcut40
shortcutladder40:
	jmp shortcutladder4
shortcut900:
	jmp shortcut90
shortcut300:
	jmp shortcut30
laddermov10:
	jmp laddermov1
shorcut1000:
	jmp shortcut100
shortcut700:
	jmp shortcut70
shortcut1600:
	jmp shortcut160
laddermov30:
	jmp laddermov3
laddermov20:
	jmp laddermov2
snakemov10:
	jmp snakemov1
snakemov3:
	mov [count],53
	mov [level8count],0
	mov [level7count],0
	mov [level6count],0
	mov [level5count],0
	mov [level4count],0
	mov [level3count],0
	mov [level2count],0
	mov [level1count],0
	jmp printboard
snakemov12:
	jmp snakemov2
shortcut120:
	jmp shortcut20
shortcut150:
	jmp shortcut50
shortcut160:
	jmp shortcut60
shortcutladder4:
	jmp laddermov4
shortcut100:
	jmp level9
shortcut90:
	jmp level8
shortcut80:
	jmp level7
shortcut70:
	jmp level6
shortcut40:
	jmp level3
shortcut30:
	jmp level2
laddermov1:
	mov [count],46
	jmp printboard
laddermov2:
	mov [count],62
	jmp printboard
laddermov3:
	mov [count],48
	jmp printboard
laddermov4:
	mov [count],96
	jmp printboard
snakemov1:
	mov [count],10
	mov [level2count],0
	mov [level1count],0
snakemov2:
	mov [count],5
	mov [level4count],0
	mov [level3count],0
	mov [level2count],0
	mov [level1count],0
	jmp printboard
shortcut60:
	jmp level5
shortcut50:
	jmp level4
shortcut20:
	jmp level1
;-------------------------------------------------------
;-------------------------------------------------------
;up to here are a bunch of shortcuts to put the jump commands in range.
level9:
	inc [level9count]    ;indicates a time that a player is found on the level.
	mov [ycharacter],1   ;moves the y position into the y character in order to print on the right level.
	call printcharacter  ;calls procedure to print character.
	jmp snake1
level8:
	inc [level8count]	 ;indicates a time that a player is found on the level.
	mov [ycharacter],17  ;moves the y position into the y character in order to print on the right level.
	call printcharacter   ;calls procedure to print character.
	jmp snake1
level1:
	inc [level1count]	 ;indicates a time that a player is found on the level.
	mov [ycharacter],129 ;moves the y position into the y character in order to print on the right level.
	call printcharacter   ;calls procedure to print character.
	jmp snake1  
level2:
	inc [level2count]	 ;indicates a time that a player is found on the level.
	mov [ycharacter],113 ;moves the y position into the y character in order to print on the right level.
	call printcharacter   ;calls procedure to print character.
	jmp snake1
level3:
	inc [level3count]	 ;indicates a time that a player is found on the level.
	mov [ycharacter],97  ;moves the y position into the y character in order to print on the right level.
	call printcharacter  ;calls procedure to print character.
	jmp snake1
level4:
	inc [level4count]	 ;indicates a time that a player is found on the level.
	mov [ycharacter],81  ;moves the y position into the y character in order to print on the right level.
	call printcharacter  ;calls procedure to print character.
	jmp snake1
level5:
	inc [level5count]	 ;indicates a time that a player is found on the level.
	mov [ycharacter],65  ;moves the y position into the y character in order to print on the right level.
	call printcharacter  ;calls procedure to print character.
	jmp snake1
level6:
	inc [level6count]	 ;indicates a time that a player is found on the level.
	mov [ycharacter],49  ;moves the y position into the y character in order to print on the right level.
	call printcharacter  ;calls procedure to print character.
	jmp snake1
level7:
	inc [level7count]	 ;indicates a time that a player is found on the level.
	mov [ycharacter],33  ;moves the y position into the y character in order to print on the right level.
	call printcharacter  ;calls procedure to print character.
	jmp snake1
snake1:   ;prints snake on game board
	push [x]
	push [y]
	mov [color],2 ;moves 2 into color or the color green
	mov [x],16  ;moves snake position into x positon
	mov [y],120 ;moves snake position into y postion 
	mov [len],16 ;moves length of snake into len
	call drawvertline ;draws the snake by calling the vertical line procedure
	pop [y]
	pop [x]
snake2:    ;prints snake on game board
	push [x]
	push [y]
	mov [color],2 ;moves 2 into color or the color green
	mov [x],112  ;moves snake position into x positon
	mov [y],24   ;moves snake position into y postion 
	mov [len],48 ;moves length of snake into len
	call drawvertline ;draws the snake by calling the vertical line procedure
	pop [y]
	pop [x]
snake3:   ;prints snake on game board
	push [x]
	push [y]
	mov [color],2 ;moves 2 into color or the color green
	mov [x],80  ;moves snake position into x positon
	mov [y],8   ;moves snake position into y postion 
	mov [len],192 ;moves length of snake into len
	call drawhorline ;draws the snake by calling the vertical line procedure
	pop [y]
	pop [x]
snake4:   ;prints snake on game board
	push [x]
	push [y]
	mov [color],2 ;moves 2 into color or the color green
	mov [x],176  ;moves snake position into x positon
	mov [y],88   ;moves snake position into y postion 
	mov [len],64 ;moves length of snake into len
	call drawvertline ;draws the snake by calling the vertical line procedure
	pop [y]
	pop [x]
ladder1:   ;prints ladder on game board
	push [x]
	push [y]
	mov [color],6 ;moves 6 into color or the color brown
	mov [x],76  ;moves ladder position into x positon
	mov [y],54  ;moves ladder position into y postion 
	mov [len],50 ;moves length of ladder into len
	call drawvertline ;draws the left side of ladder by the vert line procedure.
	mov [y],54
	add [x],8
	call drawvertline ;draws the right side of ladder by the vert line procedure.
	mov [y],58 ;moves position of steps into y
	mov [x],76 ;moves positon of steps into x
	mov [len],8 ;moves length of ladder steps into len.
	call drawhorline ;draws the steps using the hor line procedure
	mov cx,3
	mov [x],76
steps1: ;draws the steps of the ladder on screen.
	mov [x],76
	add [y],14
	call drawhorline
	loop steps1
	pop [y]
	pop [x]
ladder2: ;prints ladder on game board
	push [x]
	push [y]
	mov [color],6 ;moves 6 into color or the color brown
	mov [x],204 ;moves ladder position into x positon
	mov [y],86 ;moves ladder position into y postion 
	mov [len],68 ;moves length of ladder into len
	call drawvertline ;draws the left side of ladder by the vert line procedure.
	mov [y],86
	add [x],8
	call drawvertline ;draws the right side of ladder by the vert line procedure.
	mov [y],90  ;moves position of steps into y
	mov [x],204 ;moves positon of steps into x
	mov [len],8 ;moves length of ladder steps into len.
	call drawhorline ;draws the steps using the hor line procedure
	mov cx,4
	mov [x],204
steps2: ;draws the steps of the ladder on screen.
	mov [x],204
	add [y],15
	call drawhorline
	loop steps2
	pop [y]
	pop [x]
ladder3: ;prints ladder on game board
	push [x]
	push [y]
	mov [color],6 ;moves 6 into color or the color brown
	mov [x],204 ;moves ladder position into x positon
	mov [y],6 ;moves ladder position into y postion 
	mov [len],36  ;moves length of ladder into len
	call drawvertline ;draws the left side of ladder by the vert line procedure.
	mov [y],6
	add [x],8 
	call drawvertline ;draws the right side of ladder by the vert line procedure.
	mov [y],10 ;moves position of steps into y
	mov [x],204 ;moves positon of steps into x
	mov [len],8 ;moves length of ladder steps into len.
	call drawhorline ;draws the steps using the hor line procedure
	mov cx,2
	mov [x],204
steps3: ;draws the steps of the ladder on screen.
	mov [x],204
	add [y],13
	call drawhorline
	loop steps3
	pop [y]
	pop [x]
ladder4: ;prints ladder on game board
	push [x]
	push [y]
	mov [color],6 ;moves 6 into color or the color brown
	mov [x],268 ;moves ladder position into x positon
	mov [y],86 ;moves ladder position into y postion 
	mov [len],20 ;moves length of ladder into len
	call drawvertline ;draws the left side of ladder by the vert line procedure.
	mov [y],86
	add [x],8
	call drawvertline ;draws the right side of ladder by the vert line procedure.
	mov [y],90 ;moves position of steps into y
	mov [x],268 ;moves positon of steps into x
	mov [len],8 ;moves length of ladder steps into len.
	call drawhorline ;draws the steps using the hor line procedure
	mov cx,1
	mov [x],268
steps4: ;draws the steps of the ladder on screen.
	mov [x],268
	add [y],11
	call drawhorline
	loop steps4
	pop [y]
	pop [x]
;-----------------------------------------------------------
;-----------------------------------------------------------
;up to here we print the board and everything on it.
;from here we start randomizing numbers and playing the game.	
	mov ax, 40h
	mov es, ax
	mov cx, 1
	mov bx, 0
waitfordata:
	mov ah, 1                ; check keyboard status 
    int 16h
	je waitfordata           ; keyboard buffer empty, we still waiting for input
	
	mov ah, 0                ; we have get code in keyboard buffer, 
	                         ; we will get the scan code in <ah> register 
							 ; and remove this code from keyboard buffer
	int 16h
	
	;cmp ah, 1h                ; is it esc button?
	;je exit
	
	cmp ah, 13h  ; is it the r button?
	jne waitfordata
	inc [turncount]
RandLoop:
	; generate random number, cx number of times
	mov ax, [Clock] ; read timer counter
	mov ah, [byte cs:bx] ; read one byte from memory
	xor al, ah ; xor memory and counter
	and al, 00001111b ; leave result between 0-15
	inc bx
;=====================================
;checks that numbers are in 1-6 range
	cmp al,15
	je RandLoop
	cmp al,14
	je RandLoop
	cmp al,13
	je RandLoop
	cmp al,12
	je RandLoop
	cmp al,11
	je RandLoop
	cmp al,10
	je RandLoop
	cmp al,9
	je RandLoop
	cmp al,8
	je RandLoop
	cmp al,7
	je RandLoop
;=====================================
;checks that numbers are in 1-6 range up to here.
	cmp al,1        ;checks what number was randomized
	je print1
	cmp al,2        ;checks what number was randomized
	je print2
	cmp al,3        ;checks what number was randomized
	je shortcut2
	cmp al,4        ;checks what number was randomized
	je shortcut1
	cmp al,5        ;checks what number was randomized
	je shortcut
	jmp print6
print1: ;prints to the user that he rolled a one
	add [count],1d   ;adds to count one to indicate that user moved a square
	cmp [x],289
	jne continue20
continue20:
	add [posx],32   ;moves the player one square.
	mov ax, 13h
	int 10h
	mov dx, offset dice1 ;moves offset of message to dx.
	mov ah,9 
	int 21h			;prints message on screen.
	mov dl,10
	int 21h         ;moves to begining of line.
	mov cx, 4
space1:  ;pritns a space between the messages.
	mov dx, offset empty
	mov ah,9
	int 21h
	loop space1
	mov dx, offset message2  ;moves offset of message to dx.
	mov ah,9
	int 21h  ;prints second message on the screen.
	cmp [turncount],20	;if player has went 20 turns or more game is over.	
	jae exitshortcut
	call printturncount ;lets the player know how many turns he has gone.
waitfordata1:
	mov ah, 1                ; check keyboard status 
    int 16h
	je waitfordata1           ; keyboard buffer empty, we still waiting for input
	
	mov ah, 0                ; we have get code in keyboard buffer, 
	                         ; we will get the scan code in <ah> register 
							 ; and remove this code from keyboard buffer
	int 16h
	
	;cmp ah, 1h                ; is it esc button?
	;je exit
	
	cmp ah, 39h  ; is it the space button?
	jne waitfordata1
	jmp printboard
exitshortcut:
	jmp exit1
shortcut:
	jmp print5
shortcut1:
	jmp print4
shortcut2:
	jmp print3
;===================================================================================
;===================================================================================
;from here on out the same explinations as the print 1 apply to every print label other than the number of sqaures it has gone.
print2:
	add [count],2d
	cmp [x],289
	jne continue21
	add [posy],16
continue21:
	add [posx],64
	mov ax, 13h
	int 10h
	mov dx, offset dice2
	mov ah,9
	int 21h
	mov dl,10
	int 21h
	mov cx, 4
space2:
	mov dx, offset empty
	mov ah,9
	int 21h
	loop space2
	mov dx, offset message2
	mov ah,9
	int 21h
	cmp [turncount],20
	jae exitshortcut1
	call printturncount
waitfordata2:
	mov ah, 1                ; check keyboard status 
    int 16h
	je waitfordata2           ; keyboard buffer empty, we still waiting for input
	
	mov ah, 0                ; we have get code in keyboard buffer, 
	                         ; we will get the scan code in <ah> register 
							 ; and remove this code from keyboard buffer
	int 16h
	
	;cmp ah, 1h                ; is it esc button?
	;je exit
	
	cmp ah, 39h  ; is it the space button?
	jne waitfordata2
	jmp printboard
exitshortcut1:
	jmp exit1
print3:
	add [count],3d
	cmp [x],289
	jne continue23
	add [posy],16
continue23:
	add [posx],96
	mov ax, 13h
	int 10h
	mov dx, offset dice3
	mov ah,9
	int 21h
	mov dl,10
	int 21h
	mov cx, 4
space3:
	mov dx, offset empty
	mov ah,9
	int 21h
	loop space3
	mov dx, offset message2
	mov ah,9
	int 21h
	cmp [turncount],20
	jae exitshortcut2
	call printturncount
waitfordata3:
	mov ah, 1                ; check keyboard status 
    int 16h
	je waitfordata3           ; keyboard buffer empty, we still waiting for input
	
	mov ah, 0                ; we have get code in keyboard buffer, 
	                         ; we will get the scan code in <ah> register 
							 ; and remove this code from keyboard buffer
	int 16h
	
	;cmp ah, 1h                ; is it esc button?
	;je exit
	
	cmp ah, 39h  ; is it the space button?
	jne waitfordata3
	jmp printboard
exitshortcut2:
	jmp exit1
print4:
	add [count],4d
	cmp [x],289
	jne continue22
	add [posy],16
continue22:
	add [posx],128
	mov ax, 13h
	int 10h
	mov dx, offset dice4
	mov ah,9
	int 21h
	mov dl,10
	int 21h
	mov cx, 4
space4:
	mov dx, offset empty
	mov ah,9
	int 21h
	loop space4
	mov dx, offset message2
	mov ah,9
	int 21h
	cmp [turncount],20
	jae exitshortcut3
	call printturncount
waitfordata4:
	mov ah, 1                ; check keyboard status 
    int 16h
	je waitfordata4           ; keyboard buffer empty, we still waiting for input
	
	mov ah, 0                ; we have get code in keyboard buffer, 
	                         ; we will get the scan code in <ah> register 
							 ; and remove this code from keyboard buffer
	int 16h
	
	;cmp ah, 1h                ; is it esc button?
	;je exit
	
	cmp ah, 39h  ; is it the space button?
	jne waitfordata4
	jmp printboard
exitshortcut3:
	jmp exit1
print5:
	add [count],5d
	cmp [x],289
	jne continue25
	add [posy],16
continue25:
	add [posx],160
	mov ax, 13h
	int 10h
	mov dx, offset dice5
	mov ah,9
	int 21h
	mov dl,10
	int 21h
	mov cx, 4
space5:
	mov dx, offset empty
	mov ah,9
	int 21h
	loop space5
	mov dx, offset message2
	mov ah,9
	int 21h
	cmp [turncount],20
	jae exit1
	call printturncount
waitfordata5:
	mov ah, 1                ; check keyboard status 
    int 16h
	je waitfordata5           ; keyboard buffer empty, we still waiting for input
	
	mov ah, 0                ; we have get code in keyboard buffer, 
	                         ; we will get the scan code in <ah> register 
							 ; and remove this code from keyboard buffer
	int 16h
	
	;cmp ah, 1h                ; is it esc button?
	;je exit
	
	cmp ah, 39h  ; is it the space button?
	jne waitfordata5
	jmp printboard
print6:
	add [count],6d
	cmp [x],289
	jne continue26
	add [posy],16
continue26:
	add [posx],192
	mov ax, 13h
	int 10h
	mov dx, offset dice6
	mov ah,9
	int 21h
	mov dl,10
	mov ah,2
	int 21h
	mov cx, 4
space6:
	mov dx, offset empty
	mov ah,9
	int 21h
	loop space6
	mov dx, offset message2
	mov ah,9
	int 21h
	cmp [turncount],20
	jae exit1
	call printturncount
waitfordata6:
	mov ah, 1                ; check keyboard status 
    int 16h
	je waitfordata6        ; keyboard buffer empty, we still waiting for input
	
	mov ah, 0                ; we have get code in keyboard buffer, 
	                         ; we will get the scan code in <ah> register 
							 ; and remove this code from keyboard buffer
	int 16h
	
	;cmp ah, 1h                ; is it esc button?
	;je exit
	
	cmp ah, 39h  ; is it the space button?
	jne waitfordata6
	jmp printboard
;==============================================================
;==============================================================
exit1: ;if player loses prints a "you lose" bmp.
	call OpenFile2
    call ReadHeader
    call ReadPalette
    call CopyPal
    call CopyBitmap
	 mov ah,1
	int 21h
exit:
	mov ax, 4c00h
	int 21h
END start


