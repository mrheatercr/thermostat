 list       F=inhx8m, P=16F526, R=hex, N=0
#include  P16F526.inc; PIC definitions
 __config _CPDF_OFF  & _IOSCFS_8MHz & _MCLRE_ON  & _CP_OFF & _WDTE_OFF & _IntRC_OSC_RB4

 Errorlevel -302      ; switches off Message [302]: Register in operand not in bank 0.
; Definitions -------------------------------------------------------------

#define RelayPin portb,1
#define DigitControlPin portb,5
#define ButtonPin portb,0

; RAM preserved -----------------------------------------------------------
	;BANKSEL EEADR
	cblock 0x0d
		D1_H,D1_L
		D2_H,D2_L
		passarg1,passarg2,passarg3,passarg4,passarg5,passarg6,passarg7
		passarg8,passarg9,passarg10,passarg11,passarg12,passarg13
		WaitCounter,WaitCounter2
	endc

	cblock 0x30
		Temp_High,Temp_Low
		KeyPressCount
	endc



; Conastants --------------------------------------------------------------
GLLowThreshold			=       d'39'
GLHighThreshold			=	d'42'
; Program Memory ----------------------------------------------------------
                org     0
				movlw b'01111001'
				movwf adcon0
				movlw b'11001111'
				option
				movlw b'11110001'
				movwf cm1con0
				movwf cm2con0
Start
				;TRIS setup
				movlw b'11001101'
				tris portb
				movlw b'11000000'
				tris portc


				movlw b'00111111'
				movwf portc

				bank1
				movlw GLLowThreshold
				movwf temp_low
				movlw GLHighThreshold
				movwf temp_high
				clrf KeyPressCount
				bank0

				bcf RelayPin
				clrf D2_H
				clrf D2_L
				clrf D1_H
				clrf D1_L

Mainloop
				call Wait250mSec
				call Wait250mSec
MainLoopCont
				pagesel GetAndCalculateADC_Value
				goto GetAndCalculateADC_Value
ADC_ValueAcquired
				movwf PassArg1
				movwf PassArg2
				btfss relaypin
				goto WasOFF
WasON
				bank1
				movf temp_high,w
				bank0

				subwf PassArg2,w
				btfsc carry
				bcf relaypin
				goto Continue
WasOFF
				bank1
				movf temp_low,w
				bank0
				subwf PassArg2,w
				btfss carry
				bsf relaypin
Continue
				call Bin8ToDecimal
				pagesel UpdateNumbers
				goto UpdateNumbers
				goto mainloop

StartDelayCount 
				movwf WaitCounter2
BackWaitLoop2
				movlw b'00010000'
				ANDWF D2_H,w	
				bCf DigitControlPin
				BCF PORTB,4
				IORWF PORTB,F 
				movf D2_L,w
				movwf portc

				movlw d'164'
				movwf WaitCounter
				decfsz WaitCounter,f
                		goto $-1
				movlw d'164'
				movwf WaitCounter
				decfsz WaitCounter,f
                		goto $-1
				movlw b'00010000'
				ANDWF D1_H,w	
				bsf DigitControlPin
				BCF PORTB,4
				IORWF PORTB,F 
				movf D1_L,w
				movwf portc

				movlw d'164'
				movwf WaitCounter
				decfsz WaitCounter,f
                goto $-1
				movlw d'164'
				movwf WaitCounter
				decfsz WaitCounter,f
                goto $-1
				nop
				nop
                decf WaitCounter2,f
                btfss zero
                goto BackWaitLoop2
                retlw 0x00


Wait250mSec     movlw d'250'
                goto StartDelayCount

Bin8ToDecimal
                movf PassArg1,w
                movwf PassArg2
                movlw 8
                movwf PassArg3
                clrf PassArg4
                clrf PassArg5
                clrf PassArg6

Bin8ToDecimal_BCDADD3       
		movlw 5
                subwf PassArg4, 0
                btfsc STATUS, C
                CALL Bin8ToDecimal_ADD3HUNS
                movlw 5
                subwf PassArg5, 0
                btfsc STATUS, C
                CALL Bin8ToDecimal_ADD3TENS
                movlw 5
                subwf PassArg6, 0
                btfsc STATUS, C
                CALL Bin8ToDecimal_ADD3ONES
                decf PassArg3, 1
                bcf STATUS, C
                rlf PassArg1, 1
                rlf PassArg6, 1
                btfsc PassArg6,4 ; 
                CALL Bin8ToDecimal_CARRYONES
                rlf PassArg5, 1
                btfsc PassArg5,4 ; 
                CALL Bin8ToDecimal_CARRYTENS
                rlf PassArg4,1
                bcf STATUS, C
                movf PassArg3, 0
                btfss STATUS, Z
                GOTO Bin8ToDecimal_BCDADD3
                movf PassArg2,w
                movwf PassArg1
                retlw 0
Bin8ToDecimal_ADD3HUNS        
		movlw 3
                addwf PassArg4,1
                retlw 0
Bin8ToDecimal_ADD3TENS
		movlw 3
                addwf PassArg5,1
                retlw 0
Bin8ToDecimal_ADD3ONES
		movlw 3
                addwf PassArg6,1
                retlw 0
Bin8ToDecimal_CARRYONES
		bcf PassArg6, 4
                bsf STATUS, C
                retlw 0
Bin8ToDecimal_CARRYTENS
			bcf PassArg5,4
            bsf STATUS,C

			goto UpdateNumbers




				org 0x100
GetAndCalculateADC_Value
				BSF ADCON0, 1 
				BTFSC ADCON0, 1
				GOTO $-1
				MOVF ADRES, W ;read result
				MOVWF PASSARG1
				movlw d'64'
				subwf passarg1,f
				clrf passarg2
				clrf passarg4
				movlw d'100'
				movwf passarg3
				pagesel Multiply16x16
				call Multiply16x16
				movf passarg7,w
				movwf passarg1
				movf passarg6,w
				movwf passarg2
				movf passarg5,w
				movwf passarg3
				clrf passarg4
				clrf passarg5
				clrf passarg6
				incf passarg5,f
				incf passarg6,f
				pagesel Divide24By24
				call Divide24By24
				movlw b'00000001'
				andwf PassArg9,f			;Clear 0.5 
				movf passarg3,w
				pagesel ADC_ValueAcquired
				goto ADC_ValueAcquired

				org 0x200
Divide24By24
				movlw .24
				movwf PassArg13
				movf PassArg1,w
				movwf PassArg12
				movf PassArg2,w
				movwf PassArg11
				movf PassArg3,w
				movwf PassArg10
				clrf PassArg1
				clrf PassArg2
				clrf PassArg3
				clrf PassArg7
				clrf PassArg8
				clrf PassArg9
Divide24By24_dloop
				bcf status,c
				rlf PassArg10,f
				rlf PassArg11,f
				rlf PassArg12,f
				rlf PassArg9,f
				rlf PassArg8,f
				rlf PassArg7,f
				movf PassArg4,w
				subwf PassArg7,w
				btfss status,z
				goto Divide24By24_nochk
				movf PassArg5,w
				subwf PassArg8,w
				btfss status,z
				goto Divide24By24_nochk
				movf PassArg6,w
				subwf PassArg9,w
Divide24By24_nochk
				btfss status,c
				goto Divide24By24_nogo
				movf PassArg6,w
				subwf PassArg9,f
				btfss status,c
				decf PassArg8,f
				movf PassArg8,w
				xorlw 0xff
				btfsc status,z
				decf PassArg7,f
				movf PassArg5,w
				subwf PassArg8,f
				btfss status,c
				decf PassArg7,f
				movf PassArg4,w
				subwf PassArg7,f
				bsf status,c
Divide24By24_nogo
				rlf PassArg3,f
				rlf PassArg2,f
				rlf PassArg1,f
				decfsz PassArg13,f
				goto Divide24By24_dloop
				pagesel GetAndCalculateADC_Value
				retlw 0

Multiply16x16
Multiply16x16
				local m1, m2
				clrf PassArg8
				clrf PassArg7
				clrf PassArg6
				clrf PassArg5
				bsf PassArg6, 7
m1:
				rrf PassArg2, f
				rrf PassArg1, f
				skpc
				goto m2
				movf PassArg3, w
				addwf PassArg7, f
				movf PassArg4, w
				skpnc
				incfsz PassArg4, w
				addwf PassArg8, f
m2:
				rrf PassArg8, f
				rrf PassArg7, f
				rrf PassArg6, f
				rrf PassArg5, f
				skpc
				goto m1
				pagesel GetAndCalculateADC_Value
				retlw 0
UpdateNumbers
				movlw 0x00
				subwf passarg5,w
				btfsc zero
				goto D1_0
				movlw 0x01
				subwf passarg5,w
				btfsc zero
				goto D1_1
				movlw 0x02
				subwf passarg5,w
				btfsc zero
				goto D1_2
				movlw 0x03
				subwf passarg5,w
				btfsc zero
				goto D1_3
				movlw 0x04
				subwf passarg5,w
				btfsc zero
				goto D1_4
				movlw 0x05
				subwf passarg5,w
				btfsc zero
				goto D1_5
				movlw 0x06
				subwf passarg5,w
				btfsc zero
				goto D1_6
				movlw 0x07
				subwf passarg5,w
				btfsc zero
				goto D1_7
				movlw 0x08
				subwf passarg5,w
				btfsc zero
				goto D1_8
D1_9
				movlw b'00101111'
				movwf D1_L
				movlw b'11111111'
				movwf D1_H
				goto D1_Cont
D1_8
				movlw b'00111111'
				movwf D1_L
				movlw b'11111111'
				movwf D1_H
				goto D1_Cont
D1_7
				movlw b'00000111'
				movwf D1_L
				movlw b'11101111'
				movwf D1_H
				goto D1_Cont
D1_6
				movlw b'00111101'
				movwf D1_L
				movlw b'11111111'
				movwf D1_H
				goto D1_Cont
D1_5
				movlw b'00101101'
				movwf D1_L
				movlw b'11111111'
				movwf D1_H
				goto D1_Cont
D1_4
				movlw b'00100110'
				movwf D1_L
				movlw b'11111111'
				movwf D1_H
				goto D1_Cont
D1_3
				movlw b'00001111'
				movwf D1_L
				movlw b'11111111'
				movwf D1_H
				goto D1_Cont
D1_2
				movlw b'00011011'
				movwf D1_L
				movlw b'11111111'
				movwf D1_H
				goto D1_Cont
D1_1
				movlw b'00000110'
				movwf D1_L
				movlw b'11101111'
				movwf D1_H
				goto D1_Cont
D1_0
				movlw b'00111111'
				movwf D1_L
				movlw b'11101111'
				movwf D1_H
				goto D1_Cont
D1_Cont
				movlw 0x00
				subwf passarg6,w
				btfsc zero
				goto D2_0
				movlw 0x01
				subwf passarg6,w
				btfsc zero
				goto D2_1
				movlw 0x02
				subwf passarg6,w
				btfsc zero
				goto D2_2
				movlw 0x03
				subwf passarg6,w
				btfsc zero
				goto D2_3
				movlw 0x04
				subwf passarg6,w
				btfsc zero
				goto D2_4
				movlw 0x05
				subwf passarg6,w
				btfsc zero
				goto D2_5
				movlw 0x06
				subwf passarg6,w
				btfsc zero
				goto D2_6
				movlw 0x07
				subwf passarg6,w
				btfsc zero
				goto D2_7
				movlw 0x08
				subwf passarg6,w
				btfsc zero
				goto D2_8
D2_9
				movlw b'00101111'
				movwf D2_L
				movlw b'11111111'
				movwf D2_H
				goto D2_Cont
D2_8
				movlw b'00111111'
				movwf D2_L
				movlw b'11111111'
				movwf D2_H
				goto D2_Cont
D2_7
				movlw b'00000111'
				movwf D2_L
				movlw b'11101111'
				movwf D2_H
				goto D2_Cont
D2_6
				movlw b'00111101'
				movwf D2_L
				movlw b'11111111'
				movwf D2_H
				goto D2_Cont
D2_5
				movlw b'00101101'
				movwf D2_L
				movlw b'11111111'
				movwf D2_H
				goto D2_Cont
D2_4
				movlw b'00100110'
				movwf D2_L
				movlw b'11111111'
				movwf D2_H
				goto D2_Cont
D2_3
				movlw b'00001111'
				movwf D2_L
				movlw b'11111111'
				movwf D2_H
				goto D2_Cont
D2_2
				movlw b'00011011'
				movwf D2_L
				movlw b'11111111'
				movwf D2_H
				goto D2_Cont
D2_1
				movlw b'00000110'
				movwf D2_L
				movlw b'11101111'
				movwf D2_H
				goto D2_Cont
D2_0
				movlw b'00111111'
				movwf D2_L
				movlw b'11101111'
				movwf D2_H
				goto D2_Cont
D2_Cont

				pagesel mainloop
				goto MainLoop
Fins

                end             ; end of program
