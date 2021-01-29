; ISR_example.asm: a) Increments/decrements a BCD variable every half second using
; an ISR for timer 2; b) Generates a 440 Hz square wave at pin P3.7 using
; an ISR for timer 0; and c) in the 'main' loop it displays the variable
; incremented/decremented using the ISR for timer 2 on the LCD.  Also resets it to 
; zero if the 'BOOT' pushbutton connected to P3.7 is pressed.
$NOLIST
$MODEFM8LB1
$LIST

CLK           EQU 24000000 ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 2000*2    ; The tone we want out is A major.  Interrupt rate must be twice as fast.
TIMER0_RELOAD EQU ((65536-(CLK/(TIMER0_RATE))))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/(TIMER2_RATE))))

BOOT_BUTTON   equ P3.7
SOUND_OUT     equ P2.1
UPDOWN        equ P0.0
MODE		  equ P2.2
INCSECONDS	  equ P2.5
INCMINUTES	  equ P2.4
INCHOURS	  equ P2.3
DAY			  equ P2.6

; Reset vector
org 0x0000
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
	ljmp Timer0_ISR

; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR

; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
Count1ms:     ds 2 ; Used to determine when half second has passed
BCD_counter:  ds 1 ; The BCD counter incrememted in the ISR and displayed in the main loop
Seconds: 	  ds 1 ;
Minutes:	  ds 1 ;
Hours:		  ds 1 ;
hourcount: 	  ds 1 ;

; In the 8051 we have variables that are 1-bit in size.  We can use the setb, clr, jb, and jnb
; instructions with these variables.  This is how you define a 1-bit variable:
bseg
one_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed

Twelve: dbit 1
AM: dbit 1
PM:	dbit 1
AMgate: dbit 1
PMgate: dbit 1
ignore_DayGATE: dbit 1 ;


cseg
; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS equ P2.0
LCD_RW equ P1.7
LCD_E  equ P1.6
LCD_D4 equ P1.1
LCD_D5 equ P1.0
LCD_D6 equ P0.7
LCD_D7 equ P0.6
$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
$LIST

;                     1234567890123456    <- This helps determine the location of the counter
;Initial:  db 		  '00:00:00 AM', 0
timeAM: db 'AM', 0
timePM: db 'PM', 0
First: db '00:00:00 AM', 0
SetTime: db 'Set Time', 0
Clearline: db '          ', 0
Debug: db 'D', 0

;-----------------------------------;
; Routine to initialize the timer 0 ;
;-----------------------------------;
Timer0_Init:
	orl CKCON0, #00000100B ; Timer 0 uses the system clock
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	; Enable the timer and interrupts
    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.                ;
;---------------------------------;
Timer0_ISR:
	;clr TF0  ; According to the data sheet this is done for us already.
	; Timer 0 can not autoreload so we need to reload it in the ISR:
	clr TR0
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	setb TR0
	cpl SOUND_OUT ; Toggle the pin connected to the speaker
	reti

;---------------------------------;
; Routine to initialize timer 2   ;
;---------------------------------;
Timer2_Init:
	orl CKCON0, #0b00010000 ; Timer 2 uses the system clock
	mov TMR2CN0, #0 ; Stop timer/counter.  Autoreload mode.
	mov TMR2H, #high(TIMER2_RELOAD)
	mov TMR2L, #low(TIMER2_RELOAD)
	; Set the reload value
	mov TMR2RLH, #high(TIMER2_RELOAD)
	mov TMR2RLL, #low(TIMER2_RELOAD)
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	ret

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2H  ; Timer 2 doesn't clear TF2H automatically. Do it in ISR
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
   ;push rb
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1

Inc_Done:
	; Check if half second has passed
	mov a, Count1ms+0
	cjne a, #low(1), gotoTimer2_ISR_done ; Warning: this instruction changes the carry flag!
	mov a, Count1ms+1
	cjne a, #high(1), gotoTimer2_ISR_done
	sjmp continue
	
gotoTimer2_ISR_done:
	ljmp Timer2_ISR_done
	
continue:
	; 1000 milliseconds have passed.  Set a flag so the main program knows
	setb one_seconds_flag ; Let the main program know one second has passed
	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	setb SOUND_OUT
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	
    ; Increment the Seconds counter
	mov a, Seconds
	jnb UPDOWN, Timer2_ISR_decrement
	add a, #0x01    
    sjmp Timer2_ISR_da
    
Timer2_ISR_decrement:
	add a, #0x99 ; Adding the 10-complement of -1 is like subtracting 1.
Timer2_ISR_da:
	da a ; Decimal adjust instruction.  Check datasheet for more details!
	mov Seconds, a
	
	mov a, Seconds
	cjne a, #0x60, Timer2_ISR_done ; If Seconds is BCD 60, reset to zero
    mov Seconds, #0x00
    
    ; Increment the Minutes counter
    mov a, Minutes
    add a, #0x01
    da  a
    mov Minutes, a
    
    mov a, Minutes
    cjne a, #0x60, Timer2_ISR_done ; If Minutes is BCD 60, reset to zero
    mov Minutes, #0x00
    
    ; Increment the Hours counter
    mov a, Hours	
    add a, #0x01
    da  a
    mov Hours, a
    
   	mov a, Hours
    cjne a, #0x12, next  ;At 12 decide to switch from AM to PM or PM to AM
    
    setb Twelve
    
   	jb PM, makeodd
   	jb AM, makeeven
    
    ;Increment hour counter
    mov a, hourcount
 	add a, #0x01
 	mov hourcount, a
 	mov b, #2 ;set the b register to 2
 	sjmp skipmod
 	
 makeodd:
 	mov a, hourcount
 	mov b, #2
 	mul AB
 	add a, #0x01
 	da a
 	mov hourcount, a
 	sjmp skipmod
 	
 makeeven:
 	mov a, hourcount
 	mov b, #2
 	mul AB 
 	da a
 	mov hourcount, a
 	
 	
 	
 skipmod:
 	;If the count is odd set the AM bit to 1 if even set the PM bit
 	;This will alternate between setting the AM and PM Bits
 	mov b, #2
 	div AB
    djnz b, BreakfastTime
  	
  	;ODD COUNT Means set PM Bit
    setb PM
    sjmp next
 	
 	;EVEN Count Means set AM Bit
BreakfastTime:

 	setb AM	 
 	
 	
next:

	mov a, Hours
    cjne a, #0x13, Timer2_ISR_done ; If Hours is BCD 13, reset to one
    mov Hours, #0x01 
  

Timer2_ISR_done:
   ;pop rb
	pop psw
	pop acc
	reti

;---------------------------------;
; Hardware initialization         ;
;---------------------------------;
Initialize_All:
    ; DISABLE WDT: provide Watchdog disable keys
	mov	WDTCN,#0xDE ; First key
	mov	WDTCN,#0xAD ; Second key

    ; Enable crossbar and weak pull-ups
	mov	XBR0,#0x00
	mov	XBR1,#0x00
	mov	XBR2,#0x40

	mov	P2MDOUT,#0x02 ; make sound output pin (P2.1) push-pull
	
	; Switch clock to 24 MHz
	mov	CLKSEL, #0x00 ; 
	mov	CLKSEL, #0x00 ; Second write to CLKSEL is required according to the user manual (page 77)
	
	; Wait for 24 MHz clock to stabilze by checking bit DIVRDY in CLKSEL
waitclockstable:
	mov a, CLKSEL
	jnb acc.7, waitclockstable 

	; Initialize the two timers used in this program
    lcall Timer0_Init
    lcall Timer2_Init

    lcall LCD_4BIT ; Initialize LCD
    
    setb EA   ; Enable Global interrupts

	ret

;---------------------------------;
; Main program.                   ;
;---------------------------------;

main:
	; Setup the stack start to the begining of memory only accesible with pointers
    mov SP, #7FH
    
	lcall Initialize_All
	
    ; For convenience a few handy macros are included in 'LCD_4bit.inc':
	Set_Cursor(1, 1)
    Send_Constant_String(#First)
    setb one_seconds_flag
	mov Hours,   #0x011
	mov Minutes, #0x00
	mov Seconds, #0x00
	
	mov hourcount, #0x01
	
	setb AMgate
	; After initialization the program stays in this 'forever' loop
loop:
	
	; IF MODE IS PUSHED WHILE IN CLOCK MODE ENTER EDIT MODE
	
	jb MODE, gotoRESET  	; if the 'MODE' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb  MODE,gotoRESET  	; if the 'MODE' button is not pressed skip
	jnb MODE, $				; Wait for button release.  The '$' means: jump to same instruction.
	sjmp EDITMODE
	
gotoRESET:
	ljmp RESET

	
	; EDITMODE BEGINS
	
EDITMODE:
	
	;INITIALIZE EDITMODE
	clr TR2
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	
	; DISPLAY 12:00:00 
	
	mov Seconds, a
	mov Minutes, a
	mov Hours, #0x11
	
	Set_Cursor(1, 7)     	; Setting where we want the Seconds to be displayed
	Display_BCD(Seconds) 	; Display Seconds
	Set_Cursor(1, 4)     	; Setting where we want the Minutes to be displayed
	Display_BCD(Minutes) 	; Display Minutes
	Set_Cursor(1, 1)     	; Setting where we want the Hours to be displayed
	Display_BCD(Hours)	 	; Display Hours
	Set_Cursor(2, 1)	 	; Setting where we want Am or PM to be displayed
	Send_Constant_String(#SetTime)
	
	jb AMgate, TurnonPM
	jb PMgate, TurnonAM
	sjmp SecondsGate
	
TurnonAM:
	setb AM
	sjmp SecondsGate
	
TurnonPM:

	setb PM
	

	; SECONDS GATE BEGINS
	
SecondsGate:

	;INCREMENT SECONDS BUTTON
	jb INCSECONDS,  MinutesGATE  	; if the 'INCSECONDS' button is not pressed skip
	Wait_Milli_Seconds(#50)			; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb  INCSECONDS, MinutesGATE  	; if the 'INCSECONDS' button is not pressed skip
	jnb DAY, $	
	
	; Increment seconds when the button is pressed
	mov a, Seconds
    add a, #0x01
    da  a
    mov Seconds, a
    
    ; If seconds reaches 60 reset to zero
    mov a, Seconds
	cjne a, #0x60, stepoverA		
    mov Seconds, #0x00
    
stepoverA:

	Set_Cursor(1, 7)     	; Setting where we want the Seconds to be displayed
	Display_BCD(Seconds) 	; Display Seconds
	
	; MINUTES GATE BEGINS
a
MinutesGATE:

	;INCREMENT MINUTES BUTTON
	jb INCMINUTES,  HoursGATE  	; if the 'MODE' button is not pressed skip
	Wait_Milli_Seconds(#50)		; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb  INCMINUTES, HoursGATE  	; if the 'MODE' button is not pressed skip
	jnb DAY, $	
	
	; Increment Minutes if the INCMINUTES Button is pressed
	mov a, Minutes
    add a, #0x01
    da  a
    mov Minutes, a
    
    ; If Minutes reaches 60 reset to zero
    mov a, Minutes
	cjne a, #0x60, stepoverB		
    mov Minutes, #0x00
    
stepoverB:

	Set_Cursor(1, 4)     	; Setting where we want the Seconds to be displayed
	Display_BCD(Minutes) 	; Display Seconds
	
	; HOURS GATE BEGINS
	
HoursGATE:

	;INCREMENT HOURS BUTTON
	jb INCHOURS, 	DayGate  	; if the 'MODE' button is not pressed skip
	Wait_Milli_Seconds(#50)		; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb  INCHOURS, 	DayGate  	; if the 'MODE' button is not pressed skip
	jnb DAY, $	
	
	; Increment Hours if the INCHOURS Button is pressed
	mov a, Hours
    add a, #0x01
    da  a
    mov Hours, a
    
    ; If Hours reaches 13 then reset to one
    mov a, Hours
	cjne a, #0x13, stepoverC		; If Minutes is BCD 60, reset to zero
    mov Hours, #0x01
    
stepoverC:

	Set_Cursor(1, 1)     	; Setting where we want the Seconds to be displayed
	Display_BCD(Hours) 		; Display Seconds

	; AMPM GATES BEGIN	

DayGate:
	
	jb PM, NightGate
	jb AM, MorningGate
	ljmp EXITEDITMODE

MorningGATE:
	
	jb  DAY, NightGATE      ; if the 'MODE' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb  DAY, NightGATE  	; if the 'MODE' button is not pressed skip
	jnb DAY, $	
	
	Set_Cursor(1, 10)
	Send_Constant_String (#timeAM)
	
	clr AM
	setb PM
	
	
NightGATE:
	
	jb  DAY, EXITEDITMODE  ; if the 'MODE' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb  DAY, EXITEDITMODE	; if the 'MODE' button is not pressed skip
	jnb DAY, $	
	
	Set_Cursor(1, 10)
	Send_Constant_String (#timePM)
	
	clr PM
	setb AM

EXITEDITMODE:
	jb MODE,  gotoSecondsGate  	; if the 'MODE' button is not pressed skip
	Wait_Milli_Seconds(#50)		; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb  MODE, gotoSecondsGate  	; if the 'MODE' button is not pressed skip
	jnb MODE, $					; Wait for button release.  The '$' means: jump to same instruction.
	sjmp stepoverD
	
gotoSecondsGate:
	ljmp SecondsGate

stepoverD:
	setb TR2 					; Renable timer 2
	Set_Cursor(2, 1)
	Send_Constant_String(#Clearline)
	
RESET:
	jb BOOT_BUTTON, loop_a  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb BOOT_BUTTON, loop_a  ; if the 'BOOT' button is not pressed skip
	jnb BOOT_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'BOOT' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
	clr TR2                 ; Stop timer 2
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Now clear the BCD counter
	mov  Seconds, a
	setb TR2                ; Start timer 2
	sjmp loop_b             ; Display the new value
loop_a:
	jnb one_seconds_flag, gotoloop
	sjmp loop_b
gotoloop:
	ljmp loop
loop_b:
    clr one_seconds_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
	Set_Cursor(1, 7)     ; Setting where we want the Seconds to be displayed
	Display_BCD(Seconds) ; Display Seconds
	Set_Cursor(1, 4)     ; Setting where we want the Minutes to be displayed
	Display_BCD(Minutes) ; Display Minutes
	Set_Cursor(1, 1)     ; Setting where we want the Hours to be displayed
	Display_BCD(Hours)	 ; Display Hours
	Set_Cursor (1, 10)
	
	jb Twelve, AtTwelve
	sjmp skipit
	
AtTwelve:
	
	jb AM, Morning
	jb PM, Night
	sjmp skipit

Night:
	
	setb PMgate
	clr  AMgate
	
	clr PM ;Clear the PM bit
	Send_Constant_String(#timePM) ;Display PM
	clr Twelve
	sjmp skipit
	
Morning:
	
	setb AMgate
	clr  PMgate
	
	clr AM ;Clear the AM bit
	clr Twelve
	Send_Constant_String(#timeAM) ;Display AM
	
skipit:
    ljmp loop
    
END
