;--------------------;
;START OF ACTUAL CODE;
;--------------------;
greater28a: ljmp greater28 ;because couldn't short jump
SomeFunction:
        mov        x+0, result+0
        mov x+1, result+1
        mov x+2, #0
        mov x+3, #0
        
        Load_y(500000);next 3 lines are converting ADC to Vout *100 using that formula
        lcall mul32 ;mul 500000
        Load_y(1023)
        lcall div32 ;div 1023
        Load_y(41554)
        
        lcall x_gt_y
        jb mf, greater28a ;checking mf bit that x_gt_y sets
        Load_y(1462)
        lcall div32 ;div 1023
        Load_y(coldj)
        lcall add32
        
display:
        lcall hex2bcd
        Some_Macro(bcd+4)
        Some_Macro(bcd+3)
        Some_Macro(bcd+2)
        Some_Macro(bcd+1)
        Some_Macro(bcd+0)
        mov a,#'\n'
        lcall putchar
        mov a,#'\r'
        lcall putchar
        
        Set_Cursor(1,13)
        
        
        
        ret
        
;first estimation        
greater28: 
        Load_y(89714)
        lcall x_gt_y
        jb mf,  greater60
        Load_y(41554)
        lcall sub32
        Load_y(1505)
        lcall div32
        Load_y(28)
        lcall add32
        Load_y(22)
        lcall add32        
        ljmp display
        
;second estimation
greater60: 
        Load_y(183734)
        lcall x_gt_y
        jb mf,  greater121
        Load_y(89714)
        lcall sub32
        Load_y(1534)
        lcall div32 
        
        Load_y(60)
        lcall add32
        Load_y(22)
        lcall add32
        ljmp display
        
;3rd est.
greater121: 
        Load_y(219877)
        lcall x_gt_y
jb mf, greater145
        Load_y(183734)
        lcall sub32
        Load_y(1514)
        lcall div32
        

        Load_y(121)
        lcall add32 
        Load_y(22)
        lcall add32
        ljmp display

;4th est
greater145: 
        Load_y(260019)
        lcall x_gt_y
jb mf,  greater172
        Load_y(219877)
        lcall sub32
        Load_y(1495)
        lcall div32
        
        Load_y(145)
        lcall add32 
                Load_y(22)
        lcall add32
        ljmp display

;5th est.
greater172: 
        Load_y(343036)
        lcall x_gt_y
        jb mf,  greater228
        Load_y(260019)
        lcall sub32
        Load_y(1479)
        lcall div32
        
        Load_y(172)
        lcall add32 
                Load_y(22)
        lcall add32
        ljmp display

;6th est
greater228: 
        Load_y(343036)
        Load_y(260019)
        lcall sub32
        Load_y(1493)
        lcall div32 
        
        Load_y(228)
        lcall add32
        Load_y(22)
        lcall add32
        ljmp display
