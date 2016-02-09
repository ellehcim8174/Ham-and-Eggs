; FSMinclude.inc
; macros
$NOLIST
CSEG

setAst mac
    push acc
    push psw
    ; clears asterisks
    Set_Cursor(1, 6)
    WriteData(#' ')
    Set_Cursor(2, 6)
    WriteData(#' ')
    Set_Cursor(1, 12)
    WriteData(#' ')
    Set_Cursor(2, 12)
    WriteData(#' ')
    ; write asterisk
    Set_Cursor(%0, %1)
    WriteData(#'*')
    pop psw
	pop acc
endmac    

; display temp
displayTemp:
    set_cursor(1, 12)
    display_BCD(bcd+1)
    display_BCD(bcd)
    ret

; displays the time
displayTime:
    mov a, runTime_s                        ; check if 60 seconds
    cjne a, #0x60, displayTime_cont
    mov runTime_s, #0x00                ; if so, reset seconds to zero and increment minutes
    mov a, runTime_m
    add a, #0x01
	mov runTime_m, a
displayTime_cont:
    Set_Cursor(2,12)
    Display_BCD(runTime_m)
    Set_Cursor(2,15)
    Display_BCD(runTime_s)
ret

initscreen:
    set_Cursor(1,1)
    send_Constant_string(#screeninit1)
    set_Cursor(2,1)
    send_Constant_string(#screeninit2)
    ret

; macro for value >= chk and returns chkbit = true/false
; BGE(value,chk)
BGE mac
    clr chkbit                                    ; clear chkbit
    clr c                                        ; clear carry
    mov a, %0                            ; mov a, value
    subb a, %1                            ; subtract check with value
    lcall ?BGE
endmac
?BGE:
    jz BGE_true            ; if acc = 0, true
    jc BGE_true            ; if carry = 0, true
    sjmp BGE_done
BGE_true:
    setb chkbit            ; true, so return chkbit = 1
BGE_done:
    ret

; macro for value <= chk and returns chkbit = true/false
; BLE(value,chk)
BLE mac
    Set_Cursor(2,1)
    clr chkbit                                    ; clear chkbit
    clr c                                        ; clear carry
    mov a, %0                            ; mov a, value
    subb a, %1                            ; subtract value by chk
    lcall ?BLE
endmac

?BLE:
    jz BLE_true                        ; if acc = 0, true
    jc BLE_true                        ; if carry = 1, true
    sjmp BLE_done                   ; else finish macro
BLE_true:
    setb chkbit                        ; true, set return chkbit = 1
BLE_done:
    ret

; macro to do PWM
; recieves power percentage, turns on for that number of milliseconds, off for 100 minus that percentage
Power_Percentage mac
    Set_Cursor(2, 14)                        ; set cursor for timer
    Display_BCD(timerCount)                    ; display timer
    setb SSR_Power                ; turn on power
   ; mov a, %0
   ; mov b, #10
   ; div ab
   ; mov R4, %0
    lcall wait_percentage

endmac

wait_percentage:
    Set_Cursor(2, 14)                        ; set cursor for timer
    Display_BCD(timerCount)                    ; display timer
    lcall WaitHalfSec      ; leave power on for percentage number of milliseconds
    
    clr SSR_Power                    ; turn off power
    mov a, #4 
    ;mov a, #100
    ;subb a, R4
    ;mov b, #10
    ;div ab
gowait:
    Set_Cursor(2, 14)                        ; set cursor for timer
    Display_BCD(timerCount)                    ; display timer
    lcall WaitHalfSec   ;  leave power off for 100-percentage number of milliseconds

    dec a
    jnz gowait
    ret
