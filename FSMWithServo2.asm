; FSMv7.asm
; fsm with states, timer 1 interrupt added in, states moved to another file
$MODLP52
$NOLIST

; Reset vector
org 0000H
    ljmp MainProgram

; External interrupt 0 vector (not used in this code)
org 0003H
    reti

; Timer/Counter 0 overflow interrupt vector
org 000BH
    reti

; External interrupt 1 vector (not used in this code)
org 0013H
    reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 001BH
    ljmp Timer1_ISR

; Serial port receive/transmit interrupt vector (not used in this code)
org 0023H 
    reti
    
; Timer/Counter 2 overflow interrupt vector
org 002BH
    reti

CLK                    equ 22118400    ; Microcontroller system crystal frequency in Hz
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER1_RATE            equ 1000        ; 1000Hz, for a timer tick of 1ms
TIMER1_RELOAD        equ ((65536-(CLK/TIMER1_RATE)))
BAUD equ 115200
T2LOAD equ (0x10000-(CLK/(16*BAUD)))

; These 'EQU' must match the wiring between the microcontroller and ADC
CE_ADC                EQU                P2.0
MY_MOSI                EQU                P2.1
MY_MISO                EQU                P2.2
MY_SCLK                EQU                P2.3

$NOLIST
$include    (math32.inc)
$include    (LCD_4bit.inc)            		; library of LCD functions
$include    (FSMinclude.inc)        		; macros for the FSM
$include     (UserInterface.inc)    		; user interface things
$include    (song.inc)
$include    (linearapproxv1.1.inc)       	; linear approx for temps
$LIST

DSEG at 30H
;----------------------------------------------------;
;    user defined variables                ;
;----------------------------------------------------;
WhichVal:            	ds 2
x:                   	ds 4
y:                   	ds 4
bcd:					ds 4

MaxVal:                	ds 2        ; maximum value for some value to set
MinVal:                	ds 2        ; minimum value for some value to set
soakTime:            	ds 2        ; time to soak
soakTmp:            	ds 2        ; temperature to soak at
reflowTime:            	ds 2        ; time to reflow for
reflowTmp:            	ds 2        ; temperature to reflow at
coolTmp:				ds 2
state:                	ds 1        ; which state
TICKS:                	ds 2        ; timer
Temp:                	ds 2        ; temporary variable for displaying

currTmp:            	ds 2        ; current temperature, from temp sensor
timerCount:            	ds 1        ; timer count from timer (how many seconds in each stage)
runTime_s:            	ds 2        ; seconds component of run time
runTime_m:            	ds 2        ; minutes component of run time
countPs:            	ds 1
Result:                	ds 2
coldj: 					ds 1 		;cold junction variable

; These 'equ' must match the wiring between the microcontroller and the LCD!
LCD_RS					equ P1.4
LCD_RW                	equ P1.5
LCD_E                   equ P1.6
LCD_D4                 	equ P3.2
LCD_D5                 	equ P3.3
LCD_D6                 	equ P3.4
LCD_D7                 	equ P3.5

SOUND_OUT     			equ P0.0
SERVO					equ P2.4
SSR_Power            	equ P3.7	; pin to turn on or off solid state relay
INC_B                	equ P0.7
DEC_B                	equ P0.5
SET_B                	equ P0.3
STOP                	equ P0.1
GREEN 			equ P0.2
PURPLE			equ P0.4
YELLOW			equ P0.6
TRICOLOUR		equ P2.7
    
bseg
start:                	dbit 1		; start = 1
chkbit:                	dbit 1    	; true = 1, false = 0
power20:             	dbit 1 		; enable to turn on 20% power
start_enable:        	dbit 1
mf:                    	dbit 1
hundred:                dbit 1
hundred2:               dbit 1
ledflag1:               dbit 1
ledflag2:               dbit 1

cseg

CSEG
; Configure the serial port and baud rate using timer 2
InitSerialPort:
        clr TR2 				; Disable timer 2
        mov T2CON, #30H 		; RCLK=1, TCLK=1 
        mov RCAP2H, #high(T2LOAD)  
        mov RCAP2L, #low(T2LOAD)
        setb TR2 				; Enable timer 2
        mov SCON, #52H
        ret

; Send a character using the serial port
putchar:
    JNB TI, putchar
    CLR TI
    MOV SBUF, a
    RET

; Send a constant-zero-terminated string through the serial port
SendString:
    CLR A
    MOVC A, @A+DPTR
    JZ SendStringDone
    LCALL putchar
    INC DPTR
    SJMP SendString
SendStringDone:
    ret
    
; for SPI communication
INIT_SPI:
	setb MY_MISO                ; Make MISO an input pin
  	clr MY_SCLK					; For mode (0,0) SCLK is zero
  	ret

DO_SPI_G:
 	push acc
   	mov R1, #0        			; Reeived byte stored in R1
  	mov R2, #8              	; Loop counter (8-bits)
DO_SPI_G_LOOP:
  	mov a, R0            		; Byte to write is in R0
   	rlc a                    	; Carry flag has bit to write
  	mov R0, a
   	mov MY_MOSI, c
   	setb MY_SCLK              	; Transmit
   	mov c, MY_MISO         		; Read received bit
   	mov a, R1          			; Save received bit in R1
  	rlc a
  	mov R1, a
   	clr MY_SCLK
   	djnz R2, DO_SPI_G_LOOP
   	pop acc
   	ret

checktemp:
  	clr CE_ADC
   	mov R0, #00000001B        	; start bit:1
   	lcall DO_SPI_G
        
  	mov R0, #10000000B        	; single ended, read channel 0
   	lcall DO_SPI_G
   	mov a, R1                  	; R1 contains bits 8 and 9
   	anl a, #00000011B        	; we need only the two LSB
   	mov Result+1, a             ; save result high
        
  	mov R0, #55H                ; It doesn't matter what we transmit...
   	lcall DO_SPI_G
   	mov Result, R1          	; R1 contains bits 0 to 7. Save result low.
   	setb CE_ADC
  	lcall SomeFunction
  	ret
          
WaitHalfSec:
    mov R2, #89
X3: mov R1, #250
X2: mov R0, #166
X1: djnz R0, X1 					; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, X2 					; 22.51519us*250=5.629ms
    djnz R2, X3 					; 5.629ms*89=0.5s (approximately)
    ret
    
WaitSec:
    mov R2, #178
Y3: mov R1, #250
Y2: mov R0, #166
Y1: djnz R0, Y1 					; 3 cycles->3*45.21123ns*166=22.51519us
    djnz R1, Y2 					; 22.51519us*250=5.629ms
    djnz R2, Y3 					; 5.629ms*89=0.5s (approximately)
    ret

stopProcess:
    clr start
	clr start_enable
	clr SSR_Power
	Set_Cursor(1,1)
	Send_Constant_String(#Blank)
	Set_Cursor(2,1)
	Send_Constant_String(#Blank)
	Set_Cursor(1,1)
	Send_Constant_String(#StopM)
loop2:
	jb STOP, loop2
	Wait_Milli_Seconds(#50)
	jb STOP, loop2
	jnb STOP, $
	Set_Cursor(1,1)
	Send_Constant_String(#Blank)
	Set_Cursor(2,1)
	Send_Constant_String(#Blank)
    ljmp state0
	
; A little macro to increment BCD variables
increment_BCD mac
    mov a, %0
    add a, #0x01
    da a
    mov %0, a
endmac

; Routine to initialize the ISR
Timer1_Init:
    clr TR1   ; turn off timer
    mov a, TMOD  
    anl a, #0x0Fh ; clear bits for timer 1
    orl a, #0x10h ; Configure timer 1 as 16-timer
    mov TMOD, a

    mov TH1, #high(TIMER1_RELOAD)
    mov TL1, #low(TIMER1_RELOAD)
    clr TF1   ; clear timer flag
    ; Enable the timer and interrupts
    setb ET1  ; Enable timer 0 interrupt
    setb TR1  ; Start timer 0
    setb EA   ; Enable Global interrupts
 ret

; ISR for timer 1
Timer1_ISR:
    clr TR1   ; stop the timer
    ; The two registers used in the ISR must be saved in the stack
    push acc
    push psw 
    push cy
    mov TH1, #high TIMER1_RELOAD
    mov TL1, #low TIMER1_RELOAD 
    setb TR1 ; start timer
    
    ; Increment the 16-bit counter
    inc TICKS+0    ; Increment the low 8-bits first
    mov a, TICKS+0 ; If the low 8-bits overflow, then increment high 8-bits
    jnz Inc_Done
    inc TICKS+1
Inc_Done:
    ; Check if 1000 milli-seconds had passed
    mov a, TICKS+0
    cjne a, #low(1000), EXIT
    mov a, TICKS+1
    cjne a, #high(1000), EXIT
    ; Re-start tick counter
    clr a
    mov TICKS+0, a
    mov TICKS+1, a
    ; increment timer
	mov a, timercount
	add a, #0x01
	mov timercount, a
    ; increment runTime_s
    mov a, runTime_s
    add a, #0x01
    da a
    mov runTime_s, a
    
    ; 20% power (to enable, setb power20 in state, but remember to clr when leaving state)
    jnb power20, EXIT                ; checks if 20% power bit is set. If not, jump to exit
    clr SSR_Power                        ; if power 20% bit is set and 4 seconds has not passed, power should be off
    mov a, countPs                    ; move 4s counter to a
    subb a, #1                            ; decrements the counter
    mov countPs, a                    ; returns the value
    jnz exit                                    ; if count is not = 0, then skip, otherwise that means 4 seconds has passed
    setb SSR_Power                    ; turn on power for 1 second
    mov countPs, #5                    ; sets the count back at 5

    
    
EXIT:
    pop cy        ; restore carry flag
    pop PSW     ; restore PSW register
    pop ACC     ; restore ACC register
    reti

cseg
jmpstate1:
    lcall initscreen
    mov timerCount, #0x00
    ljmp state1    


state0:
	mov state, #0x00
 	clr power20
    Set_Cursor(1,1)
    Send_Constant_String (#SetM)
    clr SSR_Power
    mov runTime_s, #0x00                     ; sets seconds to zeo right before state 1
    mov runTime_m, #0x00                    ; sets minutes to zero right before state 1
    clr ledflag2
    setb ledflag1
    cpl TRICOLOUR
    Wait_Milli_Seconds(#80)
    cpl TRICOLOUR
    cpl PURPLE
    Wait_Milli_Seconds(#80)
    cpl PURPLE
    jb start, jmpstate1                        ; if start = 1, jump to state 1
    jb SET_B, state0_start                  ; if the 'set' button is not pressed skip to checking the start button
    Wait_Milli_Seconds(#50)
    jb SET_B, state0_start 
    jnb SET_B, $
    lcall set_Values
    setb start_enable
state0_start:
    jnb start_enable, state0                ; don't allow user to start process without setting values
    jb STOP, state0                          ; if the 'start' button is not pressed skip back up to state0
    Wait_Milli_Seconds(#50)
    jb STOP, state0      
    jnb STOP, $            
    Set_Cursor(2,1)        
    Send_Constant_String(#Blank)        
    setb start                                ; set the start bit if the start button was pressed
    ljmp state0
    
starts1: 
	ljmp starts

jump2state0:
	clr start
	clr start_enable
	ljmp state0

; ramp to soak stage
; 100% power; stays in state until current temperature reaches soak temperature 
state1:
	
	mov state, #0x01
    lcall displayTime
    lcall displaytimer
    lcall WaitHalfSec
    setb SSR_Power                            	; set power = 100%
    Set_Cursor(1,1)
    Send_Constant_String(#Ramp)
    setb TRICOLOUR
    setb PURPLE
    clr YELLOW
    clr ledflag1
    setb ledflag2
    jb STOP, check1
    Wait_Milli_Seconds(#50)
    jb STOP, check1
    jnb STOP, $
    ljmp stopProcess
    
check1:
    ; compare if temp <= soakTmp
    lcall checktemp								; check the temperature
    
    ; check if timer is less than 60
    BLE(timerCount, #60)
    jb chkbit, s1cont							; if less than 60 then skip checking of temperature
    BLE(currTmp, #50)							; if current temp is less than 50 while time is greater than 60 then stop
    jb chkbit, jumpstate0
s1cont:
    BLE(currTmp, soakTmp)                    	; check if currTmp <= soakTmp
    jb chkbit, jumpstate1  						; if true, loop
    Notes(#130,#85,#6);C4
    mov timerCount, #0x00                    	; set timer to 0 right before going to next state
    clr SSR_Power
    ljmp state2                                	; else cont states

jumpstate1:
	ljmp state1

; jump label to go to state0 - used for if time is greater than 60 but temp is less than 50  
jumpstate0:
	clr start
	clr start_enable
	clr SSR_Power
	Set_Cursor(1,1)
	Send_Constant_String(#Blank)
	Set_Cursor(2,1)
	Send_Constant_String(#Blank)
	Set_Cursor(1,1)
	Send_Constant_String(#StopM)
loop:
	Notes(#130,#85,#6);C4
	jb STOP, loop
	Wait_Milli_Seconds(#50)
	jb STOP, loop
	jnb STOP, $
	Set_Cursor(1,1)
	Send_Constant_String(#Blank)
	Set_Cursor(2,1)
	Send_Constant_String(#Blank)
    ljmp state0
    
; at preheat/soak state
; stays in this state until soak time has been reached (20% power)
state2:

	mov state, #0x02
    lcall displayTime
    lcall checktemp
    lcall displaytimer
    lcall WaitHalfSec
    Set_Cursor(1,1)
    Send_Constant_String(#Soak)
    setb YELLOW
    clr PURPLE
    clr ledflag2
    setb ledflag1
    setb power20
    jb STOP, check2
    Wait_Milli_Seconds(#50)
    jb STOP, check2
    jnb STOP, $
    ljmp stopProcess
    
check2:
    BLE(timerCount, soakTime)                	; check if timerCount < = soakTime
    jb chkbit, state2                        	; if true, loop
s2cont:
    clr power20
    Notes(#130,#85,#6);C4
    mov timerCount,#0x00
    sjmp state3                                ; else cont states
    
; ramp to peak state
; 100% power, stays in state until selected reflow temperature has been reached
state3:
	mov state, #0x03
    lcall displayTime
	lcall displayTimer
    setb SSR_Power                            ; put 100% power
    Set_Cursor(1,1)
    Send_Constant_String(#Peak)
    setb PURPLE
    clr GREEN
    clr ledflag1
    setb ledflag2
    jb STOP, check3
    Wait_Milli_Seconds(#50)
    jb STOP, check3
    jnb STOP, $
    ljmp stopProcess

check3:
    lcall checktemp
    BLE(currTmp, reflowTmp)                    ; check if currTmp <= reflowTmp
    lcall WaitHalfSec
    jb chkbit, state3                        ; if true, loop
s3cont:
    clr SSR_Power
    Notes(#130,#85,#6);C4
   	mov timerCount, #0x00
    sjmp state4                                ; else cont states
   
; reflow stage
; 20% power, stays in stage until selected reflow time has been reached
state4:

	mov state, #0x04
    lcall displayTime
   	lcall displaytimer
    lcall checktemp
    lcall WaitHalfSec
   
    ; if timerCount <= reflowTime, loop
    Set_Cursor(1,1)
    Send_Constant_String(#Reflow)
    setb power20
    setb GREEN
    clr TRICOLOUR
    clr ledflag2
    setb ledflag1
    jb STOP, check4
    Wait_Milli_Seconds(#50)
    jb STOP, check4
    jnb STOP, $
    ljmp stopProcess
    
check4:
    BLE(timerCount, reflowTime)            		; check if timerCount <= reflowTime
    lcall WaitHalfSec
    jb chkbit, state4                        	; if true, loop
s4cont:
    clr power20
    clr SSR_Power								; set power to 0%
   	lcall song
    Set_Cursor(1,1)
    Send_Constant_String(#Cool)					; writing in state 5 causes delay in display
	
	mov a, #0
	; turn servo
servoloop:
	setb SERVO
	wait_milli_seconds(#1)
	clr SERVO
	wait_milli_seconds(#19)
	add a, #1
	cjne a, #60, servoloop
	mov a, #0
servoloop2:
	setb SERVO
	wait_milli_seconds(#3)
	clr SERVO
	wait_milli_seconds(#17)
	add a, #1
	cjne a, #60, servoloop2
	mov timerCount, #0x00
    sjmp state5
    
jump5state0:
	clr start
	clr start_enable
	ljmp state0

; cooling stage
; 0% power, stays in stage until current temperature has dropped to 60
state5:

	mov state, #0x05
    lcall displayTime
	lcall displayTimer
	lcall WaitHalfSec
    ; if temp >= 60C, loop (code is same as <= except for jump to state 0
    setb TRICOLOUR
    clr YELLOW
    clr ledflag1
    setb ledflag2
    jb STOP, check5
    Wait_Milli_Seconds(#50)
    jb STOP, check5
    jnb STOP, $
    ljmp stopProcess
    
check5:
    lcall checktemp
    BLE(currTmp, coolTmp)                    	; check if currTmp >= coolTmp
    lcall WaitHalfSec
    jnb chkbit, state5                       ; if true, loop
    ; display done message
	set_cursor(1,1)
	send_Constant_string(#blank)
	set_cursor(2,1)
	send_Constant_string(#blank)
	Set_Cursor(1,1)
	Send_Constant_String(#DoneMessage)
	mov a, #0x00
s5cont:
	push acc
    lcall WaitHalfSec
    Notes(#130,#85,#6);C4
    pop acc
    add a, #0x01
    cjne a, #0x06, s5cont
goOn:
    clr start_enable
    clr start
	; wait for done button to be pressed before returning to state 0
check_done_button:
	jb STOP, check_done_button
	Wait_Milli_Seconds(#50)
	jb STOP, check_done_button
	jnb STOP, $
	set_cursor(1,1)
	send_Constant_string(#blank)
	set_cursor(2,1)
	send_Constant_string(#blank)
    ljmp state0                                ; else cont states

; status messages
SetM:                db 'SET',0
StartMessage:         db 'START?', 0
StopM:					db 'STOP!', 0
DoneMessage:		db 'DONE!', 0
Ramp:                db 'RAMP', 0
Soak:                db 'SOAK', 0
Peak:                db 'PEAK', 0
Reflow:             db 'RFLW', 0
Cool:                 db 'COOL', 0
SetMessage:            db 'SET ',0
ReflowTmpM:            db 'REFLOW TEMP',0
SoakTmpM:            db 'SOAK TEMP ',0
SoakTimeM:            db 'SOAK TIME  ',0
ReflowTimeM:        db 'REFLOW TIME',0
Blank:                db '                ',0
screenInit1:                db 'STAT   00s  000C',0            ; count@8, temp@13
screenInit2:                db 'RUNTIME:   00:00',0            ; min@12, sec@15
STe:					db 'ST', 0
STi:					db 'SS', 0
RTe:					db 'RT', 0
RTi:					db 'RS', 0
Blank2:					db '  ', 0

starts:
    Wait_Milli_Seconds(#50)
    jnb P2.4, $
    cpl start
    Wait_Milli_Seconds(#50)
    sjmp Forever

MainProgram:
    mov SP, #7FH
    mov PMOD, #0        ; Configure all ports in bidirectional mode
    LCALL InitSerialPort
    lcall INIT_SPI                ; initialize SPI communication
    clr SSR_Power
    lcall LCD_4BIT
    clr start
    clr chkbit
    clr start_enable
    clr power20
    lcall Timer1_Init
    
    mov soakTmp, #125
    mov soakTime, #50
    mov reflowTmp, #180
    mov reflowTime, #30
    mov currTmp, #22
    mov coolTmp, #60
    Set_Cursor(1,1)
    Send_Constant_String(#Blank)
    clr ET0
    clr TR0
Forever: 

    ljmp state0
    sjmp Forever
        
END
