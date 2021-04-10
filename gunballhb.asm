disp    macro  char,cursor

           mov ah,2
           mov dx,cursor;move cursor
           int 10h

           mov ah,9 ;Display
           mov bh,0 ;Page 0
           mov al,byte ptr[char]
           mov cx,1h ;1 times
           mov bl,0fh ;to set the color to Green
           int 10h

            endm  disp

WRITECHAR   MACRO   HEX,clr

        mov ah,9
        mov bh,0
        mov al,HEX
        mov cx,1
        mov bl,clr
        INT 10H

        ENDM        WRITECHAR

Cursorpos MACRO	yx

		 MOV AH,2
        MOV DX,yx  ;Cursor postion y,x
        INT 10H

		endm cursorpos

drawcarcube		macro col,row,color
		local back

		mov ax,carwidth
		add ax,col
		mov word ptr [carwidth],ax

		mov ax,carheight
		add ax,row
		mov word ptr [carheight],ax


		mov cx,col         ;Column
		mov dx,row        ;Row
		mov al,color         ;Pixel color
		mov ah,0ch 		;Draw Pixel Command

		back:   int 10h
		inc cx

		cmp cx,carwidth
		jnz back
		mov  cx,col
		inc dx
		cmp  dx,carheight
		jnz back

		mov ax,40
		mov word ptr [carwidth],ax
		mov ax,20
		mov word ptr [carheight],ax

endm		drawcarcube




drawpowerup1		MACRO col,row,color
		local againtany

		mov cx,col         ;Column
		mov dx,row         ;Row
		mov al,color         ;Pixel color
		mov ah,0ch 		;Draw Pixel Command

		againtany:   int 10h
		inc cx

		cmp cx,50
		jnz againtany
		mov  cx,col
		inc dx
		cmp  dx,280
		jnz againtany

endm	drawpowerup1

drawpowerup2	MACRO col,row,color
		local againtany2

		mov cx,col         ;Column
		mov dx,row         ;Row
		mov al,color         ;Pixel color
		mov ah,0ch 		;Draw Pixel Command

		againtany2:   int 10h
		inc cx

		cmp cx,610
		jnz againtany2
		mov  cx,col
		inc dx
		cmp  dx,280
		jnz againtany2

endm	drawpowerup2

.model small
.stack 64
.data


;--------------------Bullets1-------------------------
Bulletx1 	dw	75

Bullety1	dw	180

bulletsize dw  4

bulletx11 dw 71

bulletcheck db	1	;bullet in motion

bulletarrived db 0	;arrived

spacecheck	db	0	; if space yet7arak //mayet7araksh	39h

;--------------------Bullets2-------------------------

Bulletx2	dw	586 ;green

bulletx22 	dw 	590;black

Bullety2	dw 	180

Bulletsize2 dw	 4

Bulletcheck2	db 1	;bullet in motion

Bulletarrived2	db	0	;arrived

Qcheck	db	0	; if Q yet7arak //mayet7araksh	10h

cursorrr dw 0101h

cursorrr2 dw 0c01h

cursorinline dw 1300h

cursorinline2 dw 1500h

;---------------------Car----------------------------
carwidth	dw  40

carheight	dw	20

time db 34h,35h,39h

;---------------------Displayables-------------------

Scorep1 db "Score:$"

;--------------levels---------------------
Level1  db "Press 1 for level 1$"

Level2  db "Press 2 for level 2$"

Level1activated db 0

Level2activated	db 0

;-----------------------------------------

Thetime db "Time$"

EName	db	"Please Enter p1 Name: $"

p1name	db	15,?,15 dup ('$')

E2Name	db	"Please Enter p2 Name: $"

p2name	db	15,?,15 dup ('$')

entermesg db "Press any key to continue..$"

start 	db	"Press S to The start Game$"

Endit	db	"Press ESC To end the game$"

Chat	db	"Press C to Chat$"

Tutorial1	db	"Press t to see tutorial$"

Car1X   dw  30

carxx   dw 70

Car1Y	dw	180

Car2X	dw	590

Car2Y	dw	180
;------------------------Health properties---------------------
Healthbarp db 100

H1x     dw 120

H11x    dw 115

H2x     dw 630

H22x    dw 625

Healthcounter1	db	0	;When Car1 is destroyed

Healthcounter2	db	0	;When car2 is destroyed

Gameover	db		"Game Over !$"

TheWinner		db		"The winner is : $"

status		db		"Status: Press ESC to quit the game.$"

statuschat		db		"Status: Press ESC to exit from chatting.$"


tutorialq	db		"-Press q to shoot with Blue car.$"

tutorials	db		"-Press SPACE to shoot with Red car.$"

tutorialc1	db		"-Move Red car with up and down arrow keys.$"

tutorialc2	db		"-Move Blue car with w and s keys.$"

;------------------------------Boundaries----------------------
boundaryup		db		03h

boundarydown	db		03h

;---------------------The Time Limit---------------------

ms	dd		3D090h	;500,000=1,000,000 micro second = 1seconds in hex

TIME_AUX DB 0

alfsec	dw	3E8h	;1,000 second in hex

alfsec1	dw	3E8h	;2,000 second in hex

alfsec3 dw  2710h	;10,000 second in hex
;---------------------The Cars Data----------------------

cWidth EQU 40

cHeight EQU 20

cWidth1 dw 40

cHeight1 dw 20

c1x	dw	30

c1y dw	180

C2X	dw	590

C2Y	dw	180

cFilename DB 'car11.bin', 0

cFilehandle DW ?

cData DB cWidth*cHeight dup(0)

;------------------------Ball Properties-----------------------

BALLSIZE DW 8

;----------------------RED ball---------------------
BALLX DW 320 ;position of red ball

BALLY DW 180

BALLVX DW 20

BALLVY DW 10

;----------------------African american ball---------------------

BALLXC DW 300 ;position of green ball

BALLYC DW 170  ;CLEARS BALL IN Y DIRECTION

BALLVXC DW 20

BALLVYC DW 10

;-------------------Car2--------------------

cWidth2 EQU 40

cHeight2 EQU 20

cWidth12 dw 40

cHeight12 dw 20

cy2	dw	180

cFilename2 DB 'car22.bin', 0

cFilehandle2 DW ?

cData2 DB cWidth2*cHeight2 dup(0)

;-----------------------PowerUps----------------------

powerup1x			dw		30

powerup1y			dw		260

powerup2x			dw		590

powerup2y			dw		260

powerupactivated1	db 		0

powerupactivated2	db 		0

powerupappeared		db 		0

;--------------------Goal scores----------------------
Car1Score		db 30h

car2score		db 30h

;---------------------Serial vars-----------------------

receivevar		db    0

sendvar			db	  0

.code

.386

main    proc far

        mov ax,@data
        mov ds,ax


		lea DI,time

		CALL ClearScreen

		CALL player1name

		CALL player2name

		CALL ClearScreen

		CALL Levels

mainmenu:


		CALL ClearScreen



		cursorpos 042AH
        WRITECHAR 47H,04	; GUNBALL TEXT
		cursorpos 042BH
		WRITECHAR 55h,04
		cursorpos 042CH
		WRITECHAR 4eh,04
		cursorpos 042DH
		WRITECHAR 42h,09
		cursorpos 042EH
		WRITECHAR 41h,09
		cursorpos 042FH
		WRITECHAR 4ch,09
		cursorpos 0430H
		WRITECHAR 4ch,09

		cursorpos 081EH



		mov ah,9
		lea dx ,start
		int 21h

		cursorpos 0A1EH
		mov ah,9
		lea dx ,Chat
		int 21h

		cursorpos 0C1EH
		mov ah,9

		lea dx , Tutorial1
		int 21h

		cursorpos 0e1EH
		mov ah,9

		lea dx , Endit
		int 21h

again:
negarab0:
		mov ah,01 ;DO NOT WAIT FOR KEY

		int 16h
		jz negarab0
		mov ah,0
		int 16h


		CMP AL,73H   ;STARTGAME
		JNZ CHAT1
STARTGAME:

		call Gamescreen



CHAT1:
		CMP Al,63h	;CHATMODE
		JNZ	TUTORIAL

		call Chatting


TUTORIAL:

			CMP AL,74h
			jnz	FINISH

			Call TheTutorial
			jmp mainmenu

FINISH:

		CMP Al,1bh	;EXIT
		jnz again
		CALL ClearScreen

		MOV AH,0		;BACK TEXTMODE
		MOV AL,03
		INT 10H

		mov ah,4ch   ;closes
		int 21h

main    endp



ClearScreen	proc NEAR

		MOV AX, 0600h    ; Scroll up function
		XOR AL, AL     ; Clear entire screen
		XOR CX, CX     ; Upper left corner CH=row, CL=column
		MOV DX, 184FH  ; lower right corner DH=row, DL=column
		MOV BH,03h    ; blk
		INT 10H

		MOV AH,2
        MOV DX,080AH  ;Cursor postion y,x
        INT 10H


		cursorpos 042AH ;specially for clearscreen
        WRITECHAR ' ',03

		RET
clearscreen	endp

Gamescreen proc


		mov ax,4f02h		;640x400
		mov bx,100h
		int 10h

		CALL Field

		call drawlines ;Drawing boundaries

		cursorpos 042AH ;specially for clearscreen
        WRITECHAR ' ',03

		Cursorpos 00C5h

		mov ah,9
		lea dx,Thetime
		int 21h

		Cursorpos 1700h

		mov ah,9
		lea dx,status
		int 21h

		cursorpos 0105h

		mov dx, offset p1name+2;Display player1name
		mov ah, 9
		int 21h

;---------score1 disp----------------------------------------
		cursorpos 0210h

		mov dx, offset Scorep1;Display player1score
		mov ah, 9
		int 21h

		cursorpos 0218h

		WRITECHAR Car1Score,02

		cursorpos 0146h
		mov dx, offset p2name+2;Display player2name
		mov ah, 9
		int 21h

;---------score2 disp----------------------------------------
		cursorpos 0237h

		mov dx, offset scorep1;Display player2score
		mov ah, 9
		int 21h

		cursorpos 023fh

		WRITECHAR Car2Score,02

		CALL DrawHealthbar


		CALL Theactualcar1
		call Theactualcar2

		xor ax,ax
		call Timelimit
negarab:

		call RECEIVING
		Call Timeinmotion

			mov ah,0
			int 16h

      ;-----------------SERIAL CONNECTION CHECK--------------------

		 CALL SENDING

		;


      ;------------------------------------------------------------
	  push cx
	  mov cl,receivevar
				cmp cl,71h	;is it Q
				jnz nope1	;Q->move qcheck1
pop cx
				mov Qcheck,1
				mov word ptr [bulletx2],586 ;reset el bullet 1 postion in x
				mov word ptr [bulletx22],590	;reset el bullet 1 postion in x
					call RECEIVING
				call Legendary	;mix of time and bullet

					mov ah,0
					int 16h


					call SENDING


	nope1:
	pop cx
				cmp ah,39h ; is it space?
				 jnz checkagain

				 mov spacecheck,1

				mov word ptr bulletx1,75	;reset el bullet 1 postion in x
			mov word ptr bulletx11,71	;reset el bullet 1 postion in x


				 call RECEIVING
				 call Legendary	;mix of time and bullet

					mov ah,0
					int 16h


					call SENDING

checkagain:
				cmp ah,18h		;up arrow car1?
				jz jmpup

				cmp al,77h		;up w  car2?
				jnz noselnosjmp

UP2:
		mov bx,car2y
		sub bx,20
		drawcarcube car2x,bx,1
		jmp tanesh22		;long jmp

noselnosjmp: jmp jmpdwn

jmpup:		jmp up

tanesh22:

		drawcarcube car2x,car2y,2 ;erase

		mov	word ptr [car2y],bx

		jmp fakes

	jmpdwn:	jmp nosjmp

	fakes:

nostany22:
call RECEIVING
		call Legendary
		 mov ah,0
		 int 16h


		 call SENDING

				cmp al,71h	;is it Q
				jnz nope2	;Q->move qcheck1

				mov Qcheck,1

					push ax
					mov ax,car2y
					mov word ptr [Bullety2],ax
					 mov word ptr [bulletx2],586 ;reset el bullet 1 postion in x
					mov word ptr [bulletx22],590	;reset el bullet 1 postion in x
					pop ax
					call RECEIVING
				 call Legendary	;mix of time and bullet

					mov ah,0
					int 16h


					call SENDING

	nope2:
		cmp ah,39h ; is it space?
		jnz upchk2
		mov spacecheck,1
		;mov bulletarrived,0
		push ax
		mov ax,car1y
		mov word ptr [Bullety1],ax

		mov word ptr bulletx1,75	;reset el bullet 1 postion in x
			mov word ptr bulletx11,71	;reset el bullet 1 postion in x

		pop ax
call RECEIVING
		call Legendary

		mov ah,0
		int 16h


		call SENDING




upchk2:

		cmp al,73h		;down s key
		jz nosjmpchk22

		cmp al,1bh		;escape?
		jz nosjmp

		cmp al,77h		;up w  car2?
		jnz nostany122

		cmp  word ptr [car2y],60 ; Boundaries top
		jz  nostany22


check2again:
		jmp CheckAgain ; is it up?

UP:

		xor bx,bx
		mov bx,car1y
		sub bx,20

		drawcarcube car1x,bx,4
		jmp tanesh		;long jmp

nosjmp:			jmp down

nosjmpchk22:

			jmp	dwnchk22tany

jmpupchk2:		jmp upchk2

nostany122:

				jmp upchk

nostany222:




		cmp al,1bh		;escape?
		jz nosjmp

		cmp al,77h	;pressed w
		jz emshy

		cmp al,73h	;pressed s
		jz emshy
		call RECEIVING

		CALL Legendary

		mov ah,0
		int 16h

		call SENDING

emshy:

		jmp upchk2


tanesh:

		drawcarcube car1x,car1y,2 ;erase



		mov	word ptr [car1y],bx


nostany:

call RECEIVING
		call Legendary

		 mov ah,0
		 int 16h


		 call SENDING

erg3q:		 cmp al,71h	;is it Q
				jnz nope3	;Q->move qcheck1

				mov Qcheck,1

				 push ax
					mov ax,car2y
					mov word ptr [Bullety2],ax
					 mov word ptr [bulletx2],586 ;reset el bullet 1 postion in x
			 mov word ptr [bulletx22],590	;reset el bullet 1 postion in x
					pop ax
call RECEIVING
				 call Legendary	;mix of time and bullet

					mov ah,0
					int 16h


					call SENDING
	nope3:

		cmp ah,39h ; is it space?
		jnz upchk
		mov spacecheck,1


		push ax
		mov ax,car1y
		mov word ptr [Bullety1],ax
		mov word ptr bulletx1,75	;reset el bullet 1 postion in x
			mov word ptr bulletx11,71	;reset el bullet 1 postion in x

		pop ax
call RECEIVING
		call Legendary

		mov ah,0
		int 16h


		call SENDING

upchk:


		cmp ah,26h		;down arrow?
		jz dwnchk12tany


		cmp al,71h		;is it q
		jz erg3q

		cmp ah,39h		;is it space
		jz nope3

		cmp ah,18h		; up arrow check tany
		jnz nostany222

		cmp  word ptr [car1y],60 ; Boundaries top
		jz  nostany

		cmp al,77h ;w key?
		jnz down



		jmp Check2Again ; is it up?

halfagain: jmp check2again

upupchk2: jmp jmpupchk2


Down:

		cmp al,77h ;up w car2
		jz halfagain

		cmp al,73h	;down s car2
		jz itISdown2

		cmp ah,26h	;down arrow
		jz nositisdwn

		cmp ah,18h	;up	arrow
		jz halfagain

		cmp al,1bh		;escape?
		jz escapeithalf2

		cmp al,71h	;is Q
		jz erg3q

		cmp ah,39h	;is space
		jz nope3

		jmp nostany ;neither up nor down
dwnchk22tany: jmp downchk12
dwnchk12tany: jmp dwnchk122

upthechk2: jmp upupchk2

uparwchk: jmp upchk

itISdown2:

		mov bx,car2y
		add bx,20

		drawcarcube car2x,bx,1


				jmp tanesh12		;long jmp

nositisdwn: 	jmp itISdown

escapeithalf2:	jmp escapeithalf

jmpuptheupchk2: jmp upthechk2

nosjmp12:		jmp down

arwupchk:		jmp uparwchk

dwnchk122: 		jmp  jmpchk22



tanesh12:
		drawcarcube car2x,car2y,2
		mov	word ptr [car2y],bx


boundary280:
call RECEIVING
	call Legendary
		 mov ah,0
		 int 16h


		 call SENDING

		 cmp al,71h	;is it Q
				jnz nope4	;Q->move qcheck1

				mov Qcheck,1

					push ax
					mov ax,car2y
					mov word ptr [Bullety2],ax
					 mov word ptr [bulletx2],586 ;reset el bullet 1 postion in x
					mov word ptr [bulletx22],590	;reset el bullet 1 postion in x
					pop ax
call RECEIVING
				 call Legendary	;mix of time and bullet

					mov ah,0
					int 16h


					call SENDING
	nope4:

		cmp ah,39h ; is it space?
		jnz downchk22
		mov spacecheck,1
		;mov bulletarrived,0
		push ax
		mov ax,car1y
		mov word ptr [Bullety1],ax
		 mov word ptr bulletx1,75	;reset el bullet 1 postion in x
			mov word ptr bulletx11,71	;reset el bullet 1 postion in x
		pop ax
call RECEIVING
		call Legendary

		mov ah,0
		int 16h


		call SENDING


downchk22:

				cmp al,77h	;up w key?
				jz jmpuptheupchk2

				cmp al,1bh		;escape?
				jz escapeithalf

				cmp al,73h	;down s key?
				jnz jmpchk22

				cmp word ptr [car2y],280	; boundaries bot
				jz boundary280


nostany1:		jmp nosjmp12

uptheup:		jmp arwupchk

itISdown:

				mov bx,car1y
				add bx,20

				drawcarcube car1x,bx,4


		jmp tanesh1		;long jmp

escapeithalf:jmp escapeit

jmpchk22: jmp downchk12

jmpuptheup: jmp uptheup

nosjmp1:
			cmp ah,18h		;up?
			jnz nostany1

			jmp halfagain

jmpch12:

			cmp al,73h	;pressed s
			jz skiplegendary
			cmp al,77h;pressed w
			jz skiplegendary
call RECEIVING
			call Legendary
			mov ah,0
			int 16h


			call SENDING



skiplegendary:
			jmp downchk22

tanesh1:
		drawcarcube car1x,car1y,2
		mov	word ptr [car1y],bx

boundary300:
call RECEIVING
		call Legendary
		 mov ah,0
		 int 16h
		 Call Sending

				cmp al,71h	;is it Q
				jnz nope5	;Q->move qcheck1

				mov Qcheck,1

					push ax
					mov ax,car2y
					mov word ptr [Bullety2],ax
					 mov word ptr [bulletx2],586 ;reset el bullet 1 postion in x
			 mov word ptr [bulletx22],590	;reset el bullet 1 postion in x
					pop ax
	call RECEIVING
					call Legendary	;mix of time and bullet

					mov ah,0
					int 16h


					call SENDING
	nope5:


		cmp ah,39h ; is it space?
		jnz downchk12
		mov spacecheck,1

		push ax
		mov ax,car1y
		mov word ptr [Bullety1],ax

		mov word ptr bulletx1,75	;reset el bullet 1 postion in x
			mov word ptr bulletx11,71	;reset el bullet 1 postion in x
		pop ax

		call RECEIVING
		call Legendary

		mov ah,0
		int 16h

		call SENDING


downchk12:

		cmp ah,18h	;up arrow key?
		jz jmpuptheup

		cmp al,1bh		;escape?
		jz escapeit

		cmp ah,26h	; down arrow key?
		jnz jmpch12

		cmp word ptr [car1y],280	; boundaries bot
		jz boundary300


		jmp nosjmp1


escapeit:

		ret

Gamescreen Endp

player1name	proc

			Cursorpos 0a0dh

			mov ah,9
			lea dx,EName
			int 21h

			Cursorpos 0c0dh

			mov ah,9
			lea dx,E2Name
			int 21h

			cursorpos 0f0dh

			mov ah,9
			lea dx,entermesg
			int 21h

again1:
			cursorpos 0a25h

			xor cx,cx
			mov cx,3

loop1:		mov ah,2 ;erase
			mov dl,09
			int 21h
			loop loop1

			cursorpos 0a25h ;back to line again

			mov ah,0AH        ;Read from keyboard
			lea dx, p1name
			int 21h


			lea bx,p1name


check1:
			cmp byte ptr [bx+2],41h
			jae capital
			mov bx,0
			jmp again1

smaller:	cmp byte ptr [bx+2],61h
			jae smallercheck


smallercheck:
			cmp byte ptr [bx+2],7Ah
			jbe yescapital
			mov bx,0
			jmp again1

capital:	cmp byte ptr [bx+2],5Ah
			jbe yescapital
			jmp smaller

yescapital:
			inc bx
			cmp	byte ptr [bx+2],0dh
			jnz check1


			ret
player1name	endp

player2name	proc

				Cursorpos 0c0dh

			mov ah,9
			lea dx,E2Name
			int 21h

			cursorpos 0f0dh

			mov ah,9
			lea dx,entermesg
			int 21h

again12:
			cursorpos 0c25h

			xor cx,cx
			mov cx,3

loop12:		mov ah,2 ;erase
			mov dl,09
			int 21h
			loop loop12

			cursorpos 0c25h ;back to line again

			mov ah,0AH        ;Read from keyboard
			lea dx, p2name
			int 21h


			lea bx,p2name


check12:
			cmp byte ptr [bx+2],41h
			jae capital2
			mov bx,0
			jmp again12

smaller2:	cmp byte ptr [bx+2],61h
			jae smallercheck2


smallercheck2:
			cmp byte ptr [bx+2],7Ah
			jbe yescapital2
			mov bx,0
			jmp again12

capital2:	cmp byte ptr [bx+2],5Ah
			jbe yescapital2
			jmp smaller2

yescapital2:
			inc bx
			cmp	byte ptr [bx+2],0dh
			jnz check12



			ret
player2name	endp



drawlines  proc

             mov cx,0h		;up boundary
             mov dx,59
             mov al,5fh
             mov ah,0ch
       back:
             int 10h
             inc cx
             cmp cx,640
             jnz back

			 ;------------------------------------

			 mov cx,0h		;down boundary status
             mov dx,367
             mov al,5fh
             mov ah,0ch
       back1:
             int 10h
             inc cx
             cmp cx,640
             jnz back1

			 ;--------------------------------------
			  mov cx,0h		;chat boundary
             mov dx,300
             mov al,5fh
             mov ah,0ch
       back2:
             int 10h
             inc cx
             cmp cx,640
             jnz back2
			 
			 	;--------------------------------------
			  mov cx,0h		;chat2 boundary
             mov dx,330
             mov al,5fh
             mov ah,0ch
       back23:
             int 10h
             inc cx
             cmp cx,640
             jnz back23

			 RET
drawlines    endp

Timelimit		proc

		cursorpos 042AH       ;specially for clearscreen
        WRITECHAR ' ',03


		;lea DI,time

  here22:



        disp [DI],01C5h

        disp 3ah,01c6h

        disp  [DI+1],01c7h

        disp [DI+2],01c8h

      ;  call  wait1sec

        cmp byte ptr [DI+2],30h
        jE bigger
        dec BYTE PTR [DI+2]
       ; jmp here22
        jmp nope
    bigger:
        cmp byte ptr [DI+1],30h
        jE yes
        mov byte ptr [DI+2],39h                      ;reset the right part of seconds
        dec BYTE PTR [DI+1]                          ;decrease the left part if right one reached 0
    nope:
          jmp la2a

        ;jmp here22              ;compares if 3 digit number

    yes:
        cmp byte ptr [DI],30h

        jz endit1

        mov byte ptr [DI+1],35h ;reset the left part of seconds
        mov byte ptr [DI+2],39h ;reset the right part of seconds
        dec BYTE PTR [DI]       ;reset the part of min
    la2a:



        ;jmp here22


   endit1:

   ;---------------------Power up conditions-----------------

			cmp byte ptr[di],34h
			jne lesnotequal3

			cmp byte ptr[di+1],34h
			jne lesnotequal3

			mov byte ptr [powerupappeared],1

lesnotequal3:
   ;----------------------------powerup2---------------------
			cmp byte ptr [powerupactivated2],1
			je lesnotequal2

			cmp byte ptr[di],34h
			jne lesnotequal2

			cmp byte ptr[di+1],34h
			jne lesnotequal2

			drawpowerup2 powerup2x,powerup2y,0fh


			;-------------------write 2X power up-------------------

			cursorpos 104ah
			WRITECHAR '2',04h

			cursorpos 104bh
			WRITECHAR 'X',04h

	lesnotequal2:

;------------------------powerup1----------------------------------

			cmp byte ptr [powerupactivated1],1
			je lesnotequal

			cmp byte ptr[di],34h
			jne lesnotequal

			cmp byte ptr[di+1],34h
			jne lesnotequal

			drawpowerup1 powerup1x,powerup1y,0fh

;-------------------write 2X power up-------------------

			cursorpos 1004h
			WRITECHAR '2',04h

			cursorpos 1005h
			WRITECHAR 'X',04h


lesnotequal:

            ret

Timelimit		endp


 wait1sec       proc near

            MOV     CX, 0FH
            MOV     DX, 4240H
            MOV     AH, 86H
            INT     15H
            mov ax,0
            ret
  wait1sec  endp

  DrawHealthbar		proc

			mov cx,20		;player1 hp
            mov dx,25h
            mov al,02h
            mov ah,0ch
       back12:
             int 10h
             inc cx
             cmp cx,120
             jnz back12
			 mov cx,20
			 inc dx
			 cmp dx,2ah
			 jnz back12

			;---------------
			mov cx,530		;player2 hp
            mov dx,25h
            mov al,02h
            mov ah,0ch
       back122:
            int 10h
            inc cx
            cmp cx,630
            jnz back122
			mov cx,530
			inc dx
			cmp dx,2ah
			jnz back122


			ret

  DrawHealthbar		endp

Health1_after		proc
    cmp word ptr[H2X],15
        jE enddd1
		    mov cx,word ptr [H1x]
            mov dx,29h
            mov al,04h
            mov ah,0ch
       back1222:
            int 10h
            dec cx
            cmp cx,word ptr [H11x]
            jnz back1222
			mov cx,word ptr[H1x]
			dec dx
			cmp dx,24h
			jnz back1222
		    sub word ptr[H1x],5
		    sub word ptr[H11x],5
	enddd1:
	inc Healthcounter1
	ret
  Health1_after		endp

    Health2_after		proc
        cmp word ptr[H2X],525
        jE enddd
		    mov cx,word ptr[H2X]
            mov dx,29h
            mov al,04h
            mov ah,0ch
       back12222:
            int 10h
            dec cx
            cmp cx,word ptr[H22x]
            jnz back12222
			mov cx,word ptr[H2x]
			dec dx
			cmp dx,24h
			jnz back12222
		    sub word ptr[H2x],5
		    sub word ptr[H22x],5
	enddd:
  inc Healthcounter2
  ret
  Health2_after		endp

  ;------------------------------------Bullets------------------------------------

  printbolt1 proc


            mov cx,Bulletx1
            mov dx,Bullety1
			mov bulletcheck,1	;inmotion
			;mov bulletarrived,0 ;arrived

			cmp bulletarrived,1
			jz	lesa

			DRAWBALLHOR:
					mov al,04h; erases the last bullet
					mov ah,0ch
					int 10h

					INC CX     			;MOVEIN X DIRECTION
					MOV AX,CX
					SUB AX,Bulletx1
					CMP AX,Bulletsize		;CHECK IF BALL REACHES SIZE
					JNG DRAWBALLHOR

					MOV CX,Bulletx1 		;BACK TO X POSITION
					INC DX        		;MOV Y POSITION

					MOV AX,DX
					SUB AX,Bullety1
					CMP AX,Bulletsize		;CHECK IF BALL REACHES SIZE
					JNG DRAWBALLHOR
			;************
					mov cx, Bulletx11
					mov dx,Bullety1
					; erases the last bullet
		DRAWBALLHOR2:
					mov al,02h
					mov ah,0ch
					int 10h

					INC CX     			;MOVEIN X DIRECTION
					MOV AX,CX
					SUB AX,Bulletx11
					CMP AX, Bulletsize		;CHECK IF BALL REACHES SIZE
					JNG DRAWBALLHOR2

					MOV CX,Bulletx11
					;BACK TO X POSITION
					INC DX        		;MOV Y POSITION

					MOV AX,DX
					SUB AX,Bullety1
					CMP AX,Bulletsize ;CHECK IF BALL REACHES SIZE
					JNG DRAWBALLHOR2


			;*************
			add bulletx1,5
			add bulletx11,5

			cmp cx,car2x		;bullet arrived to other car?
			jna	erg3yasta
			mov bulletcheck,0	;bullet arrived
	erg3yasta:
			mov al,bulletcheck
			cmp al,bulletarrived
			jnz	lesa
			mov bulletarrived,1

			mov word ptr bulletx1,75	;reset el bullet 1 postion in x
			mov word ptr bulletx11,71	;reset el bullet 1 postion in x

			mov ax,bullety1		;if it bullet1 arrived? and y is equal to car2y
			cmp ax,car2y
			jnz lesa

			call Health2_after
			call Health2_after
			call Health2_after

			cmp byte ptr [powerupactivated1],0
			je la2fakes

			call Health2_after
			call Health2_after
			call Health2_after
la2fakes:
			cmp Healthcounter2,21
			jb lesa
			Call Winner

	lesa:



			ret
printbolt1	     Endp


printbolt2 proc


            mov cx,Bulletx2
            mov dx,Bullety2
			mov bulletcheck2,1	;inmotion
			;mov bulletarrived,0 ;arrived

			cmp bulletarrived2,1
			jz	lesa2

		DRAWBALLHOR3:
					mov al,01;  the bullet of car2
					mov ah,0ch
					int 10h

					dec CX     			;MOVEIN X DIRECTION
					MOV AX,CX
					SUB AX,Bulletx2
					neg ax
					CMP AX,Bulletsize2		;CHECK IF BALL REACHES SIZE
					JNa DRAWBALLHOR3

					MOV CX,Bulletx2 		;BACK TO X POSITION
					INC DX        		;MOV Y POSITION

					MOV AX,DX
					SUB AX,Bullety2

					CMP AX,Bulletsize2		;CHECK IF BALL REACHES SIZE
					JNa DRAWBALLHOR3
			;************
			;mov Bulletx2, cx
					mov cx, Bulletx22
					mov dx,Bullety2
					; erases the last bullet
		DRAWBALLHOR22:
					mov al,02h	;turns to green erases
					mov ah,0ch
					int 10h

					dec CX     			;MOVEIN X DIRECTION
					MOV AX,CX
					SUB AX,Bulletx22
					neg ax
					CMP AX, Bulletsize2		;CHECK IF BALL REACHES SIZE
					JNa DRAWBALLHOR22


					MOV CX,Bulletx22
					;BACK TO X POSITION
					inc DX        		;MOV Y POSITION

					MOV AX,DX
					SUB AX,Bullety2
					CMP AX,Bulletsize2 ;CHECK IF BALL REACHES SIZE
					JNa DRAWBALLHOR22


			;*************

			sub bulletx2,5
			sub bulletx22,5

			cmp cx,carxx		;bullet arrived to other car?
			jnb	erg3yasta2

			mov bulletcheck2,0	;bullet arrived
	erg3yasta2:
			mov al,bulletcheck2
			cmp al,bulletarrived2
			jnz	lesa2
			mov bulletarrived2,1

			mov word ptr [bulletx2],586 ;reset el bullet 1 postion in x
			mov word ptr [bulletx22],590	;reset el bullet 1 postion in x

			mov ax,bullety2		;if it bullet2 arrived? and y is equal to car2y
			cmp ax,car1y
			jnz lesa2

			call Health1_after
			call Health1_after
			call Health1_after

			cmp byte ptr [powerupactivated2],0
			je la2fakes2

			call Health1_after
			call Health1_after
			call Health1_after
la2fakes2:

; game over
			cmp  Healthcounter1,21
			jb lesa2
			Call Winner
	lesa2:


			ret
printbolt2	     Endp



  ;------------------------------------cars---------------------------------------

Theactualcar1		proc


	CALL OpenFile
    CALL ReadData

    LEA BX , cData ; BL contains index at the current drawn pixel


    MOV CX,c1x
    MOV DX,c1y

	mov ax,cHeight1
	add ax,dx
	mov word ptr[cHeight1],ax

	mov ax,cwidth1
	add ax,cx
	mov word ptr [cwidth1],ax

	xor ax,ax

	mov ah,0ch



; Drawing loop
drawLoop:
    MOV AL,[BX]
    INT 10h
    INC CX
    INC BX
    CMP CX,cWidth1
JNE drawLoop

    MOV CX , c1x
    INC DX
    CMP DX , cHeight1
JNE drawLoop


    call CloseFile

	ret
Theactualcar1 ENDP




OpenFile PROC

    ; Open file

    MOV AH, 3Dh
    MOV AL, 0 ; read only
    LEA DX, cFilename
    INT 21h

    ; you should check carry flag to make sure it worked correctly
    ; carry = 0 -> successful , file handle -> AX
    ; carry = 1 -> failed , AX -> error code

    MOV [cFilehandle], AX

    RET

OpenFile ENDP

ReadData PROC

    MOV AH,3Fh
    MOV BX, [cFilehandle]
    MOV CX,cWidth*cHeight ; number of bytes to read
    LEA DX, cData
    INT 21h
    RET
ReadData ENDP


CloseFile PROC
	MOV AH, 3Eh
	MOV BX, [cFilehandle]

	INT 21h
	RET
CloseFile ENDP
;------------------car2--------------------------------------

Theactualcar2 		proc

	CALL OpenFile2
    CALL ReadData2

    LEA BX , cData2 ; BL contains index at the current drawn pixel


    MOV CX,C2X
    MOV DX,cy2

	mov ax,cHeight12
	add ax,dx
	mov word ptr[cHeight12],ax

	mov ax,cwidth12
	add ax,cx
	mov word ptr [cwidth12],ax

	xor ax,ax

	mov ah,0ch



; Drawing loop
drawLoop2:
    MOV AL,[BX]
    INT 10h
    INC CX
    INC BX
    CMP CX,cWidth12
JNE drawLoop2

    MOV CX , C2X
    INC DX
    CMP DX , cHeight12
JNE drawLoop2


    call CloseFile2
    ret
 Theactualcar2 		endp




OpenFile2 PROC

    ; Open file

    MOV AH, 3Dh
    MOV AL, 0 ; read only
    LEA DX, cFilename2
    INT 21h

    ; you should check carry flag to make sure it worked correctly
    ; carry = 0 -> successful , file handle -> AX
    ; carry = 1 -> failed , AX -> error code

    MOV [cFilehandle2], AX

    RET

OpenFile2 ENDP

ReadData2 PROC

    MOV AH,3Fh
    MOV BX, [cFilehandle2]
    MOV CX,cWidth2*cHeight2 ; number of bytes to read
    LEA DX, cData2
    INT 21h
    RET
ReadData2 ENDP


CloseFile2 PROC
	MOV AH, 3Eh
	MOV BX, [cFilehandle2]

	INT 21h
	RET
CloseFile2 ENDP

Timeinmotion	proc

	helwa:
			mov ah,01		;Do not wait for key pressed
			dec dword ptr [ms]
			cmp dword ptr [ms],0
			jnz passy
		push ax
			call Timelimit
			mov dword ptr [ms],3D090h
		pop ax

			int 16h
			jz helwa
	passy:
	 CMP ah,1ch
      JNZ chatgame2
      call INLINECHAT
  chatgame2:


		ret
Timeinmotion	endp
Legendary	proc

	negarab4:
			mov ah,01		;Do not wait for key pressed
			dec dword ptr [ms]
			cmp dword ptr [ms],0
			jnz pass5
			push ax
			call Timelimit
			mov dword ptr [ms],3D090h
			pop ax
pass5:

			;---------------------------Level 2 activated----------------------------------

			cmp byte ptr [Level2activated],1
			jne pass6


			 dec word ptr [alfsec3]
             cmp word ptr [alfsec3],0
             jnz pass6
				pusha
                call PRINTBALL
				call PRINTBALL2

             mov word ptr [alfsec3],2710h
             popa

pass6:


;----------------------pressing Spaece car1---------------------------

			cmp spacecheck,1 ; is the space button active?
			jnz pass224

			dec word ptr [alfsec ]
			cmp word ptr [alfsec],0
			jnz pass224
				pusha
				call printbolt1

			mov word ptr [alfsec],3E8h
			popa
pass224:


;----------------------pressingQcar2------------------

			cmp Qcheck,1 ; is the space button active?
			jnz pass225

			dec word ptr [alfsec1]
			cmp word ptr [alfsec1],0
			jnz pass225
				pusha
				call printbolt2

			mov word ptr [alfsec1],3E8h
			popa
pass225:




			int 16h
			jz negarab4

      CMP ah,1ch
      JNZ chatgame
      call INLINECHAT
  chatgame:



			cmp bulletarrived,1
			jnz notarrived1
			mov bulletarrived,0
			mov spacecheck,0

notarrived1:

			cmp bulletarrived2,1
			jnz notarrived2
			mov bulletarrived2,0
			mov Qcheck,0

notarrived2:

		cmp byte ptr [powerupappeared],1
		jne	la2anotequal2

;-----------------------PowerUpcheck car1 ---------------------

		cmp word ptr [Car1Y],260
		jne la2anotequal

		mov byte ptr [powerupactivated1],1

la2anotequal:

	;-----------------------PowerUpcheck car2 ---------------------

		cmp word ptr [Car2Y],260
		jne la2anotequal2

		mov byte ptr [powerupactivated2],1

la2anotequal2:

lvl1ba2a:

			RET
Legendary ENDP

Field		proc


		mov cx,0         ;Column
		mov dx,59       ;Row
		mov al,02h         ;Pixel color green
		mov ah,0ch 		;Draw Pixel Command

		bak:   int 10h
		inc cx

		cmp cx,640
		jnz bak
		mov  cx,0
		inc dx
		cmp dx,300
		jnz bak


		ret
Field		endp

Winner		proc

		CALL	clearscreen

		MOV AH,0		;BACK TEXTMODE
		MOV AL,03
		INT 10H

		cursorpos 0a0ah

		mov ah,9
		lea dx ,Gameover
		int 21h

		cursorpos 0c0ah

		mov ah,9
		lea dx ,TheWinner
		int 21h

		mov al,Healthcounter1
		cmp al,Healthcounter2
		ja	winner2

		CMP byte ptr Car1Score,3ah
		je Winner1

		CMP byte ptr Car2Score,3ah
		je Winner2


Winner1:
		mov ah,9
		lea dx ,p1name+2
		int 21h
		jmp tmam
winner2:

		mov ah,9
		lea dx ,p2name+2
		int 21h

tmam:

		cursorpos 0e0ah

		mov ah,9
		lea dx , entermesg
		int 21h

		mov ah,0
		int 16h



		mov ah,4ch   ;closes
		int 21h

			ret
Winner		endp

TheTutorial		proc

		Call clearscreen

		MOV AH,0		;BACK TEXTMODE
		MOV AL,03
		INT 10H

		cursorpos 0a0ah

		mov ah,9
		lea dx ,tutorialc1
		int 21h

		cursorpos 0c0ah

		mov ah,9
		lea dx ,tutorials
		int 21h

		cursorpos 0e0ah

		mov ah,9
		lea dx ,tutorialc2
		int 21h

		cursorpos 100ah

		mov ah,9
		lea dx ,tutorialq
		int 21h

		cursorpos 120ah

		mov ah,9
		lea dx , entermesg
		int 21h

		mov ah,0
		int 16h




			ret
TheTutorial		endp

Chatting	proc

		call clearscreen

		cursorpos 042AH ;specially for clearscreen
        WRITECHAR ' ',03

		cursorpos 0A00h ;middle of the screen

		mov ah,9		;Boundaries of chat mode
        mov bh,0
        mov al,'_'
        mov cx,80
        mov bl,0fh
        INT 10H

		;-----------------Status boundary----------------------

		cursorpos 1500h

		mov ah,9		;status of chat mode
        mov bh,0
        mov al,'_'
        mov cx,80
        mov bl,0fh
        INT 10H

		Cursorpos 1700h	;print status

		mov ah,9
		lea dx,statuschat
		int 21h

		;---------------printing each player name----------------

		cursorpos 0001h

		mov dx, offset p1name+2;Display player1name
		mov ah, 9
		int 21h

		mov ah, 02
		mov dl, '-'
		int 21h

		cursorpos 0B01h
		mov dx, offset p2name+2;Display player2name
		mov ah, 9
		int 21h

		mov ah, 02
		mov dl, '-'
		int 21h

    cursorpos 0101h
  ;  call clearscreen
   ;  call split
     call intialization


     mov ah,0h
     mov dx,1h
     mov al,0c3h
     int 14h
     mov ah,2

     call  checkpressed

			ret
Chatting 	endp

Levels		proc

		Cursorpos 0a0dh

			mov ah,9
			lea dx,Level1	;displayes level1
			int 21h

		Cursorpos 0c0dh

			mov ah,9
			lea dx,Level2	;displayed level2
			int 21h

yalatany:
			mov ah,0		;wait for key
			int 16h

			cmp al,31h		;pressed level1
			jne lvl2

			mov byte ptr [Level1activated],1
			jmp eshta3alek

	lvl2:	cmp al,32h		;pressed level2
			jne yalatany

			mov byte ptr [Level2activated],1
eshta3alek:
			ret
Levels		endp

   PRINTBALL proc


        mov cx,BALLX
        mov dx,BALLY

  			DRAWBALLH:
  					mov al,04h; erases the last bullet
  					mov ah,0ch
  					int 10h

  					INC CX     			;MOVEIN X DIRECTION
  					MOV AX,CX
  					SUB AX,BALLX
  					CMP AX,BALLSIZE		;CHECK IF BULLET REACHES SIZE
  					JNG DRAWBALLH

  					MOV CX,BALLX 		;BACK TO X POSITION
  					INC DX        		;MOV Y POSITION

  					MOV AX,DX
  					SUB AX,BALLY
  					CMP AX,BALLSIZE		;CHECK IF BULLET REACHES SIZE
  					JNG DRAWBALLH
            ;*************

            MOV AX,BALLVX
            ADD BALLX,AX             ;move the ball horizontally



			;----------------Ball and car check--------------------
			MOV AX,car1y
            CMP Word PTR [BALLY],AX	;y1
            JB La2a12

			ADD AX,carheight	;y2
            CMP Word ptr [BALLY],AX
            JA La2a12
	;------------------------------------------------------------------
			MOV AX,Car1X
			sub ax,20
			CMP word ptr [BALLX],AX
			JBE la2a12
	;---------------------------------------------------------------------
			XOR ax,ax
			MOV AX,Car1X
			add ax,20
			CMP word ptr [BALLX],AX
			JBE NEG_VELOCITY_X


La2a12:
						XOR ax,ax

;------------------------------car2 checks------------------------

			MOV AX,car2y
            CMP Word PTR [BALLY],AX	;y1
            JB La2a122

			ADD AX,carheight	;y2
            CMP Word ptr [BALLY],AX
            JA La2a122

			XOR ax,ax

;-----------------------------------------------------------------------

			MOV AX,Car2X
			add ax,20
			CMP word ptr [BALLX],AX
			JAE la2a122

;-----------------------------------------------------------------------

			XOR ax,ax
			MOV AX,Car2X
			sub ax,8
			CMP word ptr [BALLX],AX
			JAE NEG_VELOCITY_X


La2a122:
			;------------------------------------------------------
			XOR ax,ax

			MOV AX,5
            CMP BALLX,AX
            JLe Goalscorecar22         ;BALL_X < 0 + WINDOW_BOUNDS (Y -> collided)

            MOV AX,640
            SUB AX,BALLSIZE
            SUB AX,5
            CMP BALLX,AX	          ;BALLX > WINDOW_WIDTH - BALL_SIZE  - WINDOW_BOUNDS (Y -> collided)
            JGe Goalscorecar11

            MOV AX,BALLVY
            ADD BALLY,AX             ;move the ball vertically

            MOV AX,67
            CMP BALLY,AX   ;BALL_Y < 0 + WINDOW_BOUNDS (Y -> collided)
            JL NEG_VELOCITY_Y

            MOV AX,300
            SUB AX,BALLSIZE
            SUB AX,5
            CMP BALLY,AX
            JG NEG_VELOCITY_Y		  ;BALLY > WINDOW_HEIGHT - BALL_SIZE - WINDOW_BOUNDS (Y -> collided)

			RET


        NEG_VELOCITY_X:
          NEG BALLVX   ;BALL_VELOCITY_X = - BALL_VELOCITY_X
          RET

        NEG_VELOCITY_Y:
          NEG BALLVY
        		;BALLVY = - BALLVY
          RET


Goalscorecar11:

			Mov word ptr [ballx],320

			mov word ptr [bally],180

			Mov word ptr [BALLVX],20

			mov word ptr [BALLVY],10

;--------------------reset eraser--------------------------------------
			Mov word ptr [ballxc],280

			mov word ptr [ballyc],160

			Mov word ptr [BALLVXC],20

			mov word ptr [BALLVYC],10

			INC Car1Score

			CALL ScoreUpdate

			RET

;----------------------------CAR2 Scores------------------------------
Goalscorecar22:

			Mov word ptr [ballx],320

			mov word ptr [bally],180

			Mov word ptr [BALLVX],20

			mov word ptr [BALLVY],10

			;-----------resert eraser------------
			Mov word ptr [ballxc],280

			mov word ptr [ballyc],160

			Mov word ptr [BALLVXC],20

			mov word ptr [BALLVYC],10

			INC Car2Score

			CALL ScoreUpdate

			RET
  PRINTBALL	     Endp

;----------------------------------------printball2---------------
  PRINTBALL2 proc

        mov cx, BALLXC
        mov dx,BALLYC
        ; erases the last bullet
  DRAWBALLHO2:
        mov al,02h
        mov ah,0ch
        int 10h

        INC CX     			;MOVEIN X DIRECTION
        MOV AX,CX
        SUB AX,BALLXC
        CMP AX, BALLSIZE		;CHECK IF BALL REACHES SIZE
        JNG DRAWBALLHO2

        MOV CX,BALLXC
        ;BACK TO X POSITION
        INC DX        		;MOV Y POSITION
          ;*********STARTS MOVING BALL************
        MOV AX,DX
        SUB AX,BALLYC
        CMP AX,BALLSIZE ;CHECK IF BALL REACHES SIZE
        JNG DRAWBALLHO2

        ;*************
        MOV AX,BALLVXC
        ADD BALLXC,AX             ;move the ball horizontally


		;----------------Ball and car check--------------------
			MOV AX,car1y
            CMP Word PTR [BALLYc],AX	;y1
            JB La2a123

			ADD AX,carheight	;y2
            CMP Word ptr [BALLYc],AX
            JA La2a123

			XOR ax,ax
;-----------------------------------------------------------------------

			MOV AX,Car1X
			sub ax,20
			CMP word ptr [BALLXc],AX
			JBE la2a123

;-----------------------------------------------------------------------

			XOR ax,ax
			MOV AX,Car1X

			add ax,20
			CMP word ptr [BALLXC],AX
			JBE NEG_VELOCITY_Xc

La2a123:
		xor ax,ax

		;------------------------------car2 checks------------------------

			MOV AX,car2y
            CMP Word PTR [BALLYc],AX	;y1
            JB La2a1223

			ADD AX,carheight	;y2
            CMP Word ptr [BALLYc],AX
            JA La2a1223

				XOR ax,ax
;-----------------------------------------------------------------------

			MOV AX,Car2X
			add ax,20
			CMP word ptr [BALLXc],AX
			JAE la2a1223

;-----------------------------------------------------------------------
			XOR ax,ax

			MOV AX,Car2X
			sub ax,8
			CMP word ptr [BALLXc],AX
			JAE NEG_VELOCITY_Xc


La2a1223:
			;------------------------------------------------------
			XOR ax,ax

        MOV AX,5
        CMP BALLXC,AX
        JLe Goalscorecar1          ;BALL_X < 0 + WINDOW_BOUNDS (Y -> collided)

        MOV AX,640
        SUB AX,BALLSIZE
        SUB AX,5
        CMP BALLXC,AX	          ;BALLX > WINDOW_WIDTH - BALL_SIZE  - WINDOW_BOUNDS (Y -> collided)
        JGe Goalscorecar1

        MOV AX,BALLVYC
        ADD BALLYC,AX             ;move the ball vertically

        MOV AX,67
        CMP BALLYC,AX   ;BALL_Y < 0 + WINDOW_BOUNDS (Y -> collided)
        JL NEG_VELOCITY_YC

        MOV AX,300
        SUB AX,BALLSIZE
        SUB AX,5
        CMP BALLYC,AX
        JG NEG_VELOCITY_YC		  ;BALLY > WINDOW_HEIGHT - BALL_SIZE - WINDOW_BOUNDS (Y -> collided)

        RET

          NEG_VELOCITY_XC:
            NEG BALLVXC   ;BALL_VELOCITY_X = - BALL_VELOCITY_X
            RET

          NEG_VELOCITY_YC:
            NEG BALLVYC   	;BALLVY = - BALLVY
            RET

		Goalscorecar1:


			RET

; ;-----------------------------------------------------------------
			 ; NEG_VELOCITY_XC:
            ; NEG BALLVXC   ;BALL_VELOCITY_X = - BALL_VELOCITY_X
            ; RET

          ; NEG_VELOCITY_YC:
            ; NEG BALLVYC   	;BALLVY = - BALLVY
            ; RET
  PRINTBALL2	     Endp

  ScoreUpdate		proc


			CMP byte ptr Car1Score,3ah
			je Winner11

			CMP byte ptr Car2Score,3ah
			je Winner11

			;---------score1 disp----------------------------------------
		cursorpos 0218h

		WRITECHAR Car1Score,00

		WRITECHAR Car1Score,04

;---------score2 disp----------------------------------------

		cursorpos 023fh

		WRITECHAR Car2Score,00

		WRITECHAR Car2Score,04

		Jmp eshtaawy
;-------------------------------Winner------------------------------------

Winner11:

	CALL Winner


eshtaawy:



	RET
  ScoreUpdate 		endp

SENDING     PROC


		push ax
        mov dx , 3FDH		; Line Status Register

AGAIN22:
          In al , dx 			;Read Line Status
          test al , 00100000b
          JZ AGAIN22
                              ;If empty put the VALUE in Transmit data register
          mov dx , 3F8H		; Transmit data register
          out dx , al

		  pop ax
          mov sendvar, al
          RET
SENDING     ENDP



RECEIVING     PROC


mov dx , 3FDH		; Line Status Register
in al , dx
test al , 1
JZ ma3leshwenabytany

;If Ready read the VALUE in Receive data register
mov dx , 03F8H
in al , dx

mov receivevar,al

ma3leshwenabytany:


            RET

RECEIVING     ENDP

;------------------------------------------
intialization proc

        mov dx,3fbh
        mov al,1000000b
        out dx,al
        mov dx,3f8h
        mov al,0ch
        out dx,al
        mov dx,3f9h
        mov al,0h
        out dx,al
        mov dx,3fbh
        mov al,00011011b
        out dx,al
        ret

      intialization endp



;;***********************************CHATTING***************************

checkpressed proc
      ; mov cx,0c01h

       m3l4tany:
       mov ah,1
       int 16h
       jz next

       mov ah,0
       int 16h

       push ax

       mov ah,2h
       mov dx,cursorrr
       int 10h

     ;   mov ah,3h
      ; mov bh,0h
       ;int 10h
inc cursorrr
cmp byte ptr[cursorrr],0
jne qs
add cursorrr,0200h
qs:

       cmp cursorrr,961*3-400
       jbe a
      ; call clearup
       call split1
       mov cursorrr,0101h
      a: mov dl,al
       mov ah,02h
       int 21h

       cmp al,1bh
       jz r


       mov dx , 3FDH		; Line Status Register

       AGAIN2:  	In al , dx 			;Read Line Status
    AND al , 00100000b
    JZ AGAIN2
       pop ax
                           ;If empty put the VALUE in Transmit data register
    mov dx , 3F8H		; Transmit data register
    out dx , al



       next:
       mov dx , 3FDH		; Line Status Register
  in al , dx
    AND al , 1
    JZ m3l4tany

;If Ready read the VALUE in Receive data register
    mov dx , 03F8H
    in al , dx
     ;  mov ah,2
;        mov dx,0h
;        int 10h

       push ax
       ;mov byte ptr[cursorrr],cl
       mov ah,2h
       mov dx,cursorrr2
       int 10h

       inc word ptr [cursorrr2]

       cmp byte ptr [cursorrr2],0
       jne qqqq
       add cursorrr2,020fh

       qqqq:
cmp cursorrr2,1500h
jbe G

call split2

mov cursorrr2,0c01h
G:

       pop ax

       mov dl,al
       mov ah,02
       int 21h


       jmp m3l4tany

   r:
       mov ah,4ch
       int 21h

   ret


       checkpressed endp



split1 proc

       mov ax,0b800h
       mov es,ax
       mov ax,0A20h
       mov di,0h
       mov cx,20*24
       mov cursorrr,0101h
       rep stosw


ret
split1 endp

split2 proc
       mov ax,0b800h
       mov es,ax
       mov ax,0A20h
       mov di,80*12*2
       mov cx,80*12
       rep stosw

       mov cursorrr2,0c01h
       ret

       split2 endp

clearup proc

     mov ax,0b800h
       mov es,ax
       mov ax,0A020h
       mov di,0
       mov cx,80*12
       rep stosw
       ret

       clearup endp

INLINECHAT proc

MOV AH,00H
INT 16H





m3l4tany2:
mov ah,1
int 16h
jz nextt

mov ah,0
int 16h

push ax

mov ah,2h
mov dx,cursorinline
int 10h


inc cursorinline
cmp byte ptr[cursorinline],0
jne qst
add cursorinline,0200h
qst:

cmp cursorinline,961*8-400
jbe ab
mov cursorinline,1300h
ab: mov dl,al
mov ah,02h
int 21h

cmp al,1bh
jz r2


mov dx , 3FDH		; Line Status Register

AGAIN24:  	In al , dx 			;Read Line Status
AND al , 00100000b
JZ AGAIN24
pop ax
                    ;If empty put the VALUE in Transmit data register
mov dx , 3F8H		; Transmit data register
out dx , al



nextt:
mov dx , 3FDH		; Line Status Register
in al , dx
AND al , 1
JZ m3l4tany2

;If Ready read the VALUE in Receive data register
mov dx , 03F8H
in al , dx
;  mov ah,2
;        mov dx,0h
;        int 10h

push ax
;mov byte ptr[cursorrr],cl
mov ah,2h
mov dx,cursorinline2
int 10h

inc word ptr [cursorinline2]

cmp byte ptr [cursorinline2],0
jne qqqq2
add cursorinline2,020fh

qqqq2:
cmp cursorinline2,1700h
jbe G2


mov cursorinline2,1500h
G2:

pop ax

mov dl,al
mov ah,02
int 21h


jmp m3l4tany2

r2:



      RET
INLINECHAT ENDP




   end main
