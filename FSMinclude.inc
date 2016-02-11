cseg
; sub-routine to set a value
; user must move value to be changed into accumulator
; move maximum value and minimum value into MaxVal and MinVal respectively
set_Values:
	lcall checktemp
    mov a, soakTmp 
    mov maxVal, #165
    mov minVal, #125
    mov WhichVal, #0x01
   lcall checkButton
   mov soakTmp, a
   jb SET_B, set_Values
   Wait_Milli_Seconds(#50)
   jb SET_B, set_Values
   jnb SET_B, $
   Set_Cursor(1,5)
   Send_Constant_String(#Blank2)
next1:
	lcall checktemp
    mov a, SoakTime
    mov maxVal, #120
    mov minVal, #50
    mov WhichVal, #0x02
    lcall checkButton
    mov soakTime, a
    jb SET_B, next1
    Wait_Milli_Seconds(#50)
    jb SET_B, next1
    jnb SET_B, $
    Set_Cursor(2,5)
   	Send_Constant_String(#Blank2)
next2:
	lcall checktemp
    mov a, reflowTmp
    mov maxVal, #210
    mov minVal, #180
    mov WhichVal, #0x03
   lcall checkButton
   mov reflowTmp, a
   jb SET_B, next2
   Wait_Milli_Seconds(#50)
   jb SET_B, next2
   jnb SET_B, $
   Set_Cursor(1,11)
   Send_Constant_String(#Blank2)
next3:
	lcall checktemp
   	mov a, reflowTime
    mov maxVal, #40
    mov minVal, #30
    mov WhichVal, #0x04
   	lcall checkButton
   	mov reflowTime, a
   	jb SET_B, next3
   	Wait_Milli_Seconds(#50)
   	jb SET_B, next3
   	jnb SET_B, $
	Set_Cursor(2,11)
   	Send_Constant_String(#Blank2)
   	ret
 
; increment(value,min, max, col, row)
Increment:
cjne a, MaxVal, incont         
mov a, MinVal                   
sjmp incDisp
incont:
add a, #0x01
incDisp:
lcall displayVal
ret

; decrement(value, min, max, col, row)
Decrement:
cjne a, minVal, decont 
mov a, maxVal 
sjmp decDisp
decont:
add a, #0xFF
decDisp:
lcall displayVal
ret

displayVal:
    mov x+3, #0 
    mov x+2, #0
    mov x+1, #0
    mov x+0, a
    ; check which variable we are incre/decre to determine where to display
    mov a, WhichVal
    cjne a, #0x01, checkOthers
	Set_Cursor(1,5)
	Send_Constant_String(#STe)
    Set_Cursor(1,7)
    ljmp checkOver
checkOthers:
    cjne a, #0x02, checkOthers2
	Set_Cursor(2,5)
	Send_Constant_String(#STi)
    Set_Cursor(2,7)
    ljmp checkOver
checkOthers2:
    cjne a, #0x03, checkOthers3
	Set_Cursor(1,11)
	Send_Constant_String(#RTe)
    Set_Cursor(1,13)
    ljmp checkOver
checkOthers3:
    cjne a, #0x04, checkOver
	Set_Cursor(2,11)
	Send_Constant_String(#RTi)
    Set_Cursor(2,13)
checkOver:
    lcall hex2bcd
    mov a, x
;   Display_BCD(bcd+3)
 ;  Display_BCD(bcd+2)
    Display_BCD(bcd+1)
    Display_BCD(bcd+0)
    ret


checkButton:
    jb INC_B, skip1
    Wait_Milli_Seconds(#50)
    jb INC_B, skip1
    jnb INC_B, $
    lcall increment
skip1:
    jb DEC_B, done
    Wait_Milli_Seconds(#50)
    jb DEC_B, done
    jnb DEC_B, $
    lcall decrement
done:
    lcall DisplayVal
    ret
    
