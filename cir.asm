;;Project By Abdul Rafay Zahid, Momin Imran Qureshi
;;		Roll Number: 20L-1203, 20L-1014			
[org 0x0100]

jmp start

midpoint: dw 	0, 0			;x, y coordinate
midpoint_coordinates: dw 0
baloon1:	dw	0, 0
baloon2:	dw	0, 0
baloon3:	dw	0, 0

timeStr: db 'time remaining', 0
scoreStr: db 'this is the score', 0
inputStr: db 'this is the input', 0

tickcount: dw 0 


sleep:  				push cx
						mov cx, 0xFFFF
						delay:  loop delay
						pop cx
						ret



print_rectangle:
						push bp
						mov bp , sp
						 
						push ax
						push si
						push cx
						push es
						push di
						push dx

						push word[bp + 12]
						call color_checker
						mov dh  , ah
						mov dl , 0x20

  ;----------Storing index of top left coordinate in SI register----------------
					    push word[bp + 10]
					    push word[bp + 8]
					    call converter
   					    mov si  , ax


  ;----------Storing index of bottom right coordinate in DI register----------------
						push  word[bp + 10]
						push word[bp +4]
						call converter
						mov di , ax
						
  ;--------calculating midpoint of circle--------------------------						
						push word[bp + 10]
					    push word[bp + 8] 
						push word[bp + 6]
						push word[bp +4]
						call calculate_midpoint
						
						push word[midpoint]
						push word[midpoint+2]
						call converter
						mov [midpoint_coordinates], ax
						
  ;-------storing the width of rectangle in Cx-------
						mov cx , [bp + 6]
						sub cx , [bp + 10]
 
  ;================= printing parallel horizontal lines of rectangle ======================
					    mov ax , 0xb800
					    mov es , ax
					 
					    mov ax , si  ; storing the si in ax

l1:
   
						mov word[es: si] ,dx
						mov word[es:di] , dx
						
						add si , 2
						add di , 2
						sub cx , 1
						jnz l1

  ;----------------------------------


  ;--------printing number ---------
					    mov cl, [bp+14]
						mov ch, 0x07
						mov si, ax
						add si, 160
						add si, [midpoint]
						dec si
						mov word[es:si], cx
  

  ;-------storing the length of rectangle in Cx-------
		   			    mov cx , [bp +4]
	 				    sub cx , [bp + 8]

 ;---------- Storing index of top right coordinate in DI register ----------------
					   mov si , ax       ; restoring the value on si as stored in ax previously (top left coordinate)
					   push  word[bp + 6]
					   push word[bp +8]
					   call converter
					   mov di , ax
   

 
  ;================= printing vertical parallel lines of rectangle ======================
   
l2:
						mov word[es:di] , dx
						mov word[es: si] ,dx
					   
						add si , 160
						add di , 160
						sub cx , 1
						jnz l2
						mov word[es:di] , dx

						;------------------

						pop dx
						pop di
						pop es
						pop cx
						pop si
				     	pop ax
					    pop bp

						ret 10


;---------------------------------erase-----------------------------------------------
erase:					push bp
						mov bp , sp
						 
						push ax
						push si
						push cx
						push es
						push di
						push dx
						
						mov dx, 0x0720
						
  ;----------Storing index of top left coordinate in SI register----------------
					    push word[bp + 10]
					    push word[bp + 8]
					    call converter
   					    mov si  , ax


  ;----------Storing index of bottom right coordinate in DI register----------------
						push  word[bp + 10]
						push word[bp +4]
						call converter
						mov di , ax	
						
  ;-------storing the width of rectangle in Cx-------
						mov cx , [bp + 6]
						sub cx , [bp + 10]
 
  ;================= printing parallel horizontal lines of rectangle ======================
					    mov ax , 0xb800
					    mov es , ax
					 
					    mov ax , si  ; storing the si in ax

lop1:
   
						mov word[es: si] ,dx
						mov word[es:di] , dx
						
						add si , 2
						add di , 2
						sub cx , 1
						jnz lop1

  ;----------------------------------

  ;-------storing the length of rectangle in Cx-------
		   			    mov cx , [bp +4]
	 				    sub cx , [bp + 8]
						
 ;--------printing number ---------
					    mov si, ax
						add si, 160
						add si, [midpoint]
						dec si
						mov word[es:si], 0x0720						

 ;---------- Storing index of top right coordinate in DI register ----------------
					   mov si , ax       ; restoring the value on si as stored in ax previously (top left coordinate)
					   push  word[bp + 6]
					   push word[bp +8]
					   call converter
					   mov di , ax
   

 
  ;================= printing vertical parallel lines of rectangle ======================
   
lop2:
						mov word[es:di] , dx
						mov word[es: si] ,dx
					   
						add si , 160
						add di , 160
						sub cx , 1
						jnz lop2
						mov word[es:di] , dx

						;------------------

						pop dx
						pop di
						pop es
						pop cx
						pop si
				     	pop ax
					    pop bp

						ret 10

						
						
color_checker:
						push bp
						mov bp , sp

						mov ah , 0x47
						cmp word[bp + 4] , 1
						jnz second
						mov ah , 0x17
						jmp end

second:
						cmp word[bp + 4] , 2
						jnz third
						mov ah , 0x47
						jmp end
third:
						cmp word[bp + 4] , 3
						jnz end
						mov ah , 0x27

					   
end:
						pop bp
						ret 2

converter:
						 push bp
						 mov bp , sp
						 
						 push bx
						 mov ax , 0
						 mov al , 80
						 mul byte[bp + 4]
						 add ax , [bp + 6]
						 shl ax , 1


						 pop bx
						 pop bp
						 
						 ret 4

 
calculate_midpoint:		push bp
						mov bp , sp
						push ax
						push bx
						
						mov ax, [bp+6]
						sub ax, [bp+10]				;x coordinate
						
						mov bx, [bp+4]
						sub bx, [bp+8]				;ycoordinate
						
						mov [midpoint], ax
						mov [midpoint+2], bx
						
						pop bx
						pop ax
						pop bp
						ret 8


; subroutine to print a number at top left of screen 
; takes the number to be printed as its parameter 
printnum: push bp 
 mov bp, sp 
 push es 
 push ax 
 push bx 
 push cx 
 push dx 
 push di 
 mov ax, 0xb800 
 mov es, ax ; point es to video base 
 mov ax, [bp+4] ; load number in ax 
 mov bx, 10 ; use base 10 for division 
 mov cx, 0 ; initialize count of digits 
nextdigit: mov dx, 0 ; zero upper half of dividend 
 div bx ; divide by 10 
 add dl, 0x30 ; convert digit into ascii value 
 push dx ; save ascii value on stack 
 inc cx ; increment count of values 
 cmp ax, 0 ; is the quotient zero 
 jnz nextdigit ; if no divide it again 
 mov di, 630; point di to 76th column 
nextpos: pop dx ; remove a digit from the stack 
 mov dh, 0x07 ; use normal attribute 
 mov [es:di], dx ; print char on screen 
 add di, 2 ; move to next screen location 
 loop nextpos ; repeat for all digits on stack 
 pop di 
 pop dx 
 pop cx 
 pop bx 
 pop ax
 pop es 
 pop bp 
 ret 2 
; timer interrupt service routine 
timer: push ax 
 inc word [cs:tickcount]; increment tick count 
 push word [cs:tickcount] 
 call printnum ; print tick count 
 mov al, 0x20 
 out 0x20, al ; end of interrupt 
 pop ax 
 iret ; return from interrupt 

scrollup: 				push bp
						mov bp,sp
						push ax
						push cx
						push si
						push di
						push es
						push ds
						mov ax, 80 ; load chars per row in ax
						mul byte [bp+4] ; calculate source position
						mov si, ax ; load source position in si
						push si ; save position for later use
						shl si, 1 ; convert to byte offset
						mov cx, 2000 ; number of screen locations
						sub cx, ax ; count of words to move
						mov ax, 0xb800
						mov es, ax ; point es to video base
						mov ds, ax ; point ds to video base
						xor di, di ; point di to top left column
						cld ; set auto increment mode
				;		call sleep
						rep movsw ; scroll up
						mov ax, 0x0720 ; space in normal attribute
						pop cx ; count of positions to clear
						rep stosw ; clear the scrolled space
						pop ds
						pop es
						pop di
						pop si
						pop cx
						pop ax
						pop bp
						ret 2	
						 
clrScr:
						push ax
						push es
						push di

						;-------------------
						mov ax , 0xb800
						mov es , ax

						mov di , 0
next:

						mov word[es : di] , 0x0720
						add di , 2
						cmp di , 4000
						jnz next

						;------------------
						pop di
						pop es
						pop ax
						ret

print_bubbles:			push bp
						mov bp,sp
						;sub bp, 12
						push ax
						push bx
						push cx
						push dx
						mov cx, 0
						
						mov dx, [bp+4]
						mov bx, [bp+8]
		
						
						
lps1:					push word[bp+4]
						push word[bp+12]
						push word[bp+10]
						push bx
						push word[bp+6]
						push dx
						call print_rectangle
						
						call sleep
						call sleep
						call sleep
						call sleep
						call sleep
						call sleep
						call sleep
						call sleep
						call sleep
							
						push word[bp+10]
						push bx
						push word[bp+6]
						push dx
						call erase
						
						call sleep
						call sleep
						call sleep
						call sleep
						call sleep
						call sleep
						call sleep
						call sleep
						call sleep
						
						sub bx, 4
						sub dx, 4
						
						push word[bp+14]
						push word[bp+12]
						push word[bp+10]
						push bx
						push word[bp+6]
						push dx
						call print_rectangle
						
						add cx, 2
						cmp cx, 10
						jnz lps1
						
						pop dx
						pop cx
						pop bx
						pop ax
						pop bp
						ret 10
					
; subroutine to print a string
; takes the x position, y position, attribute, and address of a null
; terminated string as parameters
printstr: 				push bp
						 mov bp, sp
						 push es
						 push ax
						 push cx
						 push si
						 push di
						 push ds
						 pop es ; load ds in es
						 mov di, [bp+4] ; point di to string
						 mov cx, 0xffff ; load maximum number in cx
						 xor al, al ; load a zero in al
						 repne scasb ; find zero in the string
						 mov ax, 0xffff ; load maximum number in ax
						 sub ax, cx ; find change in cx
						 dec ax ; exclude null from length
						 jz exit ; no printing if string is empty
						 mov cx, ax ; load string length in cx
						 mov ax, 0xb800
						 mov es, ax ; point es to video base
						 mov al, 80 ; load al with columns per row
						 mul byte [bp+8] ; multiply with y position
						 add ax, [bp+10] ; add x position
						 shl ax, 1 ; turn into byte offset
						 mov di,ax ; point di to required location
						 mov si, [bp+4] ; point si to string
						 mov ah, [bp+6] ; load attribute in ah
						 cld ; auto increment mode
						 
nextchar: 				lodsb ; load next char in al
						stosw ; print char/attribute pair
						loop nextchar ; repeat for the whole string
						
						

exit: 					pop di
						pop si
						pop cx
						pop ax
						pop es
						pop bp
						ret 8



					
start:			
		  call clrScr
 xor ax, ax 
 mov es, ax ; point es to IVT base 
 cli ; disable interrupts 
 mov word [es:8*4], timer; store offset at n*4 
 mov [es:8*4+2], cs ; store segment at n*4+2 
 sti ; enable interrupts 
 mov dx, start ; end of resident portion 
 add dx, 15 ; round up to next para 
 mov cl, 4 
 shr dx, cl ; number of paras 
 mov ax, 0x3100 ; terminate and stay resident 

		  
			mov ax, 60
			push ax
			mov ax, 3
			push ax
			mov ax, 2
			push ax
			mov ax, timeStr
			push ax
			call printstr

			mov ax, 60
			push ax
			mov ax, 8
			push ax
			mov ax, 5
			push ax
			mov ax, scoreStr
			push ax
			call printstr

			mov ax, 60
			push ax
			mov ax, 18
			push ax
			mov ax, 6
			push ax
			mov ax, inputStr
			push ax
			call printstr
					  
		  
		  
; color input 1-Blue , 2- Red , 3- Green
			 mov word[baloon1], 21
			 mov word[baloon1+2], 23

			 mov word[baloon2], 21	
			 mov word[baloon2+2], 23	
			 
			 mov word[baloon3], 21
			 mov word[baloon3+2], 23
			 
			 mov cx, 0
			
;-----------------1st three baloons			
lp:			;Red Ballon with 2
			push 0x32						
			push 2
			push 30						;x L
			push word[baloon1]			; y L
			push 35						; x R
			push word[baloon1+2]		; y R
			call print_rectangle
			
			;Blue Baloon with 5
			
			push 0x35
			push 1
			push 44
			push word[baloon2]
			push 49
			push word[baloon2+2]
			call print_rectangle
			
			;Green Baloon with 6
			push 0x36
			push 3
			push 20
			push word[baloon3]
			push 25
			push word[baloon3+2]
			call print_rectangle
			
			call sleep
			call sleep
			call sleep
			call sleep
			call sleep
			call sleep
			call sleep
			call sleep
			call sleep
			call sleep
			call sleep
			
			
			push 30
			push word[baloon1]
			push 35
			push word[baloon1+2]
			call erase
			
			
			push 44
			push word[baloon2]
			push 49
			push word[baloon2+2]
			call erase
			
			push 20
			push word[baloon3]
			push 25
			push word[baloon3+2]
			call erase
	
			
			sub word[baloon1], 1
			sub word[baloon1+2], 1
			
			sub word[baloon2], 2
			sub word[baloon2+2], 2
			
			sub word[baloon3], 3
			sub word[baloon3+2], 3
			
			
			add cx, 2
			cmp cx, 44
			jnz lp

;----------------------2nd three baloons-----------------	

			 mov word[baloon1], 21
			 mov word[baloon1+2], 23

			 mov word[baloon2], 21	
			 mov word[baloon2+2], 23
			 
			 mov word[baloon3], 21
			 mov word[baloon3+2], 23
			 
			 mov cx, 0
			
			
lp1:			;Green Ballon with 6
			push 0x36						
			push 3
			push 25
			push word[baloon1]
			push 30
			push word[baloon1+2]
			call print_rectangle
			
			;Red Baloon with 1
			
			push 0x31
			push 2
			push 15
			push word[baloon2]
			push 20
			push word[baloon2+2]
			call print_rectangle
			
			;Blue Baloon with 5
			push 0x35
			push 1
			push 35
			push word[baloon3]
			push 40
			push word[baloon3+2]
			call print_rectangle
			
			call sleep
			call sleep
			call sleep
			call sleep
			call sleep
			call sleep
			call sleep
			call sleep
			call sleep
			call sleep
			call sleep
			
			
			push 25
			push word[baloon1]
			push 30
			push word[baloon1+2]
			call erase
			
			
			push 15
			push word[baloon2]
			push 20
			push word[baloon2+2]
			call erase
	
			push 35
			push word[baloon3]
			push 40
			push word[baloon3+2]
			call erase
			
			sub word[baloon1], 2
			sub word[baloon1+2], 2
			
			sub word[baloon2], 1
			sub word[baloon2+2], 1
			
			sub word[baloon3], 3
			sub word[baloon3+2], 3
			
			
			add cx, 2
			cmp cx, 44
			jnz lp1		
		  
;----------------------3rd tthree baloons-----------------	

			 mov word[baloon1], 21
			 mov word[baloon1+2], 23

			 mov word[baloon2], 21	
			 mov word[baloon2+2], 23
			 
			 mov word[baloon3], 21
			 mov word[baloon3+2], 23
			 
			 mov cx, 0
			
			
lp2:		;Green Ballon with 3
			push 0x33						
			push 3
			push 25
			push word[baloon1]
			push 30
			push word[baloon1+2]
			call print_rectangle
			
			;Blue Baloon with 9
			push 0x39
			push 1
			push 15
			push word[baloon2]
			push 20
			push word[baloon2+2]
			call print_rectangle
			
			;Red Baloon with 5
			push 0x35
			push 2
			push 40
			push word[baloon3]
			push 45
			push word[baloon3+2]
			call print_rectangle
			
			call sleep
			call sleep
			call sleep
			call sleep
			call sleep
			call sleep
			call sleep
			call sleep
			call sleep
			call sleep
			call sleep
			
			
			push 25
			push word[baloon1]
			push 30
			push word[baloon1+2]
			call erase
			
			
			push 15
			push word[baloon2]
			push 20
			push word[baloon2+2]
			call erase
	
			push 40
			push word[baloon3]
			push 45
			push word[baloon3+2]
			call erase
			
			sub word[baloon1], 3
			sub word[baloon1+2], 3
			
			sub word[baloon2], 1
			sub word[baloon2+2], 1
			
			sub word[baloon3], 2
			sub word[baloon3+2], 2
			
			add cx, 2
			cmp cx, 44
			jnz lp2			  
		  
	mov ax , 0x4c00
	int 0x21