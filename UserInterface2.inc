cseg
; sub-routine to set a value
; user must move value to be changed into accumulator
; move maximum value and minimum value into MaxVal and MinVal respectively
 ; set bcd_num if value to be changed is a bcd number
set_val:
check_inc:
    clr chkbit
    jb INC_B, check_dec        ; if the 'increment' button is not pressed skip to decr
    Wait_Milli_Seconds(#50)    
    jb INC_B, check_dec  
    jnb INC_B, $            
    BGE(x, MaxVal)
    jnb chkbit, incr       ; if already at max value, loop back to min value
    Load_X(MinVal)
    sjmp check_done
incr:
    Load_Y(1)
    lcall add32
    ljmp check_done
check_dec:
    jb DEC_B, check_done        ; if the 'decrement' button is not pressed skip to done button
    Wait_Milli_Seconds(#50)    
    jb DEC_B, check_done
    jnb DEC_B, $
    BLE(x, MinVal)
    jnb chkbit, decr
    Load_X(MaxVal)
    sjmp check_done
decr:
    Load_Y(1)
    lcall sub32
    ljmp check_done
check_done:
    lcall hex2bcd
    mov a, WhichVal
    cjne a, #0x01, check2
    Set_Cursor(1,7)
    sjmp check_button
check2:
    cjne a, #0x02, check3
    Set_Cursor(2,7)
    sjmp check_button
check3:
    cjne a, #0x03, check4
    Set_Cursor(1,13)
    sjmp check_button
check4:
    Set_Cursor(2,13)
    
check_button:
    Display_BCD(bcd+1)
    Display_BCd(bcd)
    jb SET_B, set_val_jump  ; if the 'done' button is not pressed check other buttons again
    Wait_Milli_Seconds(#50)    
    jb SET_B, set_val_jump
    jnb SET_B, $    
    ret        
set_val_jump:
    ljmp set_val

; sub-routine to go through value setting process    
set_Values:        ; soak temp
    Load_x(SoakTmp)
    mov WhichVal, #0x01
    mov MaxVal, #165            ; should be 165
    mov MinVal, #125            ; should be 125
    lcall set_val
    mov SoakTmp+1, x+1
    mov SoakTmp+0, x+0
    Set_Cursor(1,7)
    WriteData(#' ')                    ; clears asterisk
next1:        ; soak time
    Set_Cursor(2,7)
    WriteData(#'*')
    Load_x(SoakTime)
    mov WhichVal, #0x02
    mov MaxVal, #120                ; should be 120
    mov MinVal, #60
    lcall set_val
     mov SoakTime+1, x+1
    mov SoakTime+0, x+0
    Set_Cursor(2,7)
    WriteData(#' ')
next2:        ; Reflow temp
    Set_Cursor(1,13)
    WriteData(#'*')
    Load_x(ReflowTmp)
    mov WhichVal,  #0x03
    mov MaxVal,    #210            ; should be 210
    mov MinVal,    #180            ; should be 180
    lcall set_val
    mov ReflowTmp+1, x+1
    mov ReflowTmp+0, x+0
    Set_Cursor(1, 13)
    WriteData(#' ')
next3:        ; reflow time
    Set_Cursor(2,13)
    WriteData(#'*')
    Load_x(ReflowTime)
    mov WhichVal, #0x04
    mov MaxVal,    #40
    mov MinVal,    #30
    lcall set_val
    mov ReflowTime+1, x+1
    mov ReflowTime+0, x+0
    Set_Cursor(2,13)
    WriteData(#' ')
    
; check if set or start buttons are pressed - set to continue setting values, start to go back to state0
checking:
    jb SET_B, finish                         ; if the keep setting button is not pressed skip
    Wait_Milli_Seconds(#50)
    jb SET_B, finish  
    jnb SET_B, $
    ljmp set_Values                                ; skip back up to setting soakTmp if button pressed
finish:
    jb STOP, checking                          ; if the done button is not pressed skip
    Wait_Milli_Seconds(#50)
    jb STOP, checking 
    jnb STOP, $        

    Set_Cursor(2,1)                                ; blank out bottom row
    Send_Constant_String(#Blank)        
    ret
END

