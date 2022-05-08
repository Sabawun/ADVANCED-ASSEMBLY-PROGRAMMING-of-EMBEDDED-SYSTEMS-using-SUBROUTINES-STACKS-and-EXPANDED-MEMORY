;Sabawun, Ihab, Hassaan


.include "m128def.inc"

.EQU Ones = 0xFF
.EQU Zeros= 0x00
.EQU Packet_Out = porte
.EQU Packet_In = pinf
.EQU Control_Switch = pinb
.EQU Read_Out = portd
.EQU Ready = portg
.EQU Receive = ping

.macro Port_Config
	
	LDI R16, Zeros
	OUT DDRB, R16 ; portb[0-2] input
	STS DDRF, R16 ; portf for packet_in

	LDI R16, Ones
	OUT DDRD, R16 ; portd Read_out [8 LEDS]
	OUT DDRE, R16 ; porte Packet_out

	LDI R16, 0x10 ; 
	STS DDRG, R16 ; G(Ready[4], Recieve[3])  

.endmacro

.macro XMEM_Config
		
	LDI R16, 0x80
	OUT MCUCR, R16

.endmacro

.macro Pointer_Config

	LDI xl, 0x00
	LDI xh, 0x01

.endmacro

.macro Stack_Init

	LDI R16, low(RAMEND)
	OUT SPL, R16
	LDI R16, high(RAMEND)
	OUT SPH, R16

.endmacro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

.cseg
.org 0x0000
rjmp power

.org 0x0100
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;SUBROUTINES/FUNCTIONS THAT THE PROGRAM USES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



power:
			;PORTS INITILIAZATION 94/2F
			Port_Config
			;XMEM MODE
			XMEM_Config
			; DEFINING MEMORY POINTER
			Pointer_Config
		

SP_location:
			Stack_Init
			CALL INIT						 ; INIT 
			IN R20,Packet_Out				 ; 
			PUSH R20						 ; PACKET_OUT TO TOS

SR_location:
			CALL SERVICE_READOUT			 ; SUBROUTINE CALL 
			IN R20, Control_Switch           ; PINB[0] FOR START/STOP
			SBRS R20,0						 ; SKIP IF START  
			RJMP SP_location				 ; GO TO SP_location IF STOP 


			LDS R20,Ready				     ; LOADING PORTG TO TURN ON LED 
			SBR R20,0x10					 ; SETTING PORTG[4] HIGH
			STS Ready,R20					 


			LDS R20,Receive					 ; CHECKING RECIEVE 
			SBRS R20,3					     ; IF ON  SKIP NEXT
			RJMP SR_location				 ; IF OFF GO TO SR_location 

CHECK_AG:
			LDS R20,Receive					 
			SBRC R20,3						
			RJMP CHECK_AG					
			LDS R20,Ready					


			LDI R21,0b11101111				;AND MASKING     
			AND R20,R21						;AND MASKING TO TURN LED OFF        
			STS Ready,R20					 
		
		      
			IN R20,Packet_In			    
			SBRC R20,7						;SKIP NEXT IF COMMAND PACKET 
			RJMP DATA_PKT					;JUMP IF  DATA PACKET
			POP R20							
			PUSH R20						
			SBRS R20,7						;SKIP NEXT IF  DATA PACKET	
			RJMP CRC3_LOC					
			IN R21,Packet_In				; R21 USED IN SUBROUTINE
			CALL CRC_CHECK11				;CALLING CRC_CHECK11 SUBROUTINE IF DATA PKT
			CPI R16,0						
			BRNE PASS						
			POP R20							
			CALL REPEAT_REQUEST				
		
			RJMP SR_location				

PASS:
			IN R20,Packet_In				
			SBRC R20,6						;SKIP NEXT IF BIT 6 IS 0 [LOG PACKET[01]]
			RJMP SP_location				
			SBRS R20,5						;SKIP NEXT IF BIT 5 IS 1 [LOG PACKET[01]]
			RJMP SP_location				

			LDI R20,0xEB					;INTERNAL MEMORY LIMIT 
			LDI R21,0x10					; "" "" "" ""
			CP XL,R20						;COMPARING CURRENT MEM WITH CONFIG POINTERS 
			CPC XH,R21						
			BRCS INT_SUC					;BRANCH IF MEMORY SPACE EXIST IN INTERNAL MEMORY 
			LDI XL,0x00						;SWITCH TO EXTERNAL MEMORY 0X1100
			LDI XH,0x11						;"" "" "" ""
INT_SUC:
			LDI R20,0x00					;EXTERNAL MEMORY LIMIT
			LDI R21,0x19					;"" "" "" ""
			CP XL,R20						
			CPC XH,R21						
			BRCS EXT_SUC					;BRANCH IF MEMORY SPACE EXIST IN EXTERNAL MEMORY
			LDI XL,0x00						;OVERWRITING
			LDI XH,0x01						;"" ""
EXT_SUC:
			 POP R20						
			 ST X+,R20						
			 LDI R16,0b01000000				
			 CALL CRC3
			 PUSH R16						
			 CALL TRANSMIT					
			 
			 TO_SR: 
			 	RJMP SR_LOCATION			

CRC3_LOC:  
			MOV R16,R20
			CALL CRC_CHECK3					;CALLING SUBROUTINE
			CPI R16,0						;SKIP NEXT LINE IF FAILURE OF CRC_CHECK3
			BRNE  PASS1						
			CALL REPEAT_REQUEST				

SR_LOCATION2:
			RJMP TO_SR                      ;DIRECT JUMP NOT POSSIBLE

PASS1:
			LDS R20,Packet_In				
			SBRS R20,6						;SKIP NEXT IF BIT 6 IS 1 [ACKNOWLEDGE[10]]
			RJMP RPT_PKT
			SBRC R20,5					    ;SKIP NEXT IF BIT 5 IS 0 [ACKNOWLEDGE[10]]
			RJMP RPT_PKT
			IN R16,SPL						
			IN R17,SPH						
			LDI R18,0xFF					
			LDI R19,0x10				    
			CP R16,R18						
			CPC R17,R19						
			BREQ SR2						
			POP R20							
			JMP SR_LOCATION					

RPT_PKT: 
			IN R20,Packet_In				
			SBRS R20,6					    ;SKIP NEXT IF BIT 6 IS 1 [REPEAT[11]]
			JMP SR_LOCATION					
			SBRS R20,5						;SKIP NEXT IF BIT 5 IS 1 [REPEAT[11]]
			RJMP SR_LOCATION				
			IN R16,SPL						
			IN R17,SPH						
			LDI R18,0xFF					
			LDI R19,0x10					
			CP R16,R18						
			CPC R17,R19						
			BREQ SR2						
			POP R20							
			PUSH R20						 
			MOV R16,R20						
			CALL TRANSMIT					
			JMP SR_LOCATION
        
DATA_PKT:
			IN R16,SPL						
			IN R17,SPH						
			LDI R18,0xFF					
			LDI R19,0x10					
			CP R16,R18						
			CPC R17,R19						
			BREQ EMPT						
			POP R20							
EMPT:
			IN R20,Packet_In				
			PUSH R20						
			JMP SR_LOCATION				

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;SUBROUTINES;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

INIT: 
			IN R16,Packet_Out; 
			LDI R17,0b10000000 ; 
			CALL CRC3  
			CALL TRANSMIT   
			RET

TRANSMIT :
			OUT Packet_Out,R16 
			RET

SERVICE_READOUT:
				MOV YL,XL 
				MOV YH,XH
				IN R19,Control_Switch  
				SBRS R19,1   
				RJMP SERVICE_READOUT_LOOP2
				
				SERVICE_READOUT_LOOP1:  
					LDI R19,0x01  
					LD R16,-Y  ;0x0106, to 0x0100 
					OUT PORTD,R16
					CPI YL,0x00  
					CPC YH,R19  
					BRNE SERVICE_READOUT_LOOP1   
					RJMP SERVICE_READOUT_LOOP5
				
				SERVICE_READOUT_LOOP2:
					SBRS R19,2  
					RJMP SERVICE_READOUT_LOOP5
					LD R16,-Y   
					OUT PORTD,R16 
				
				SERVICE_READOUT_LOOP5: 
					RET

REPEAT_REQUEST:
				IN R16,Packet_Out											
				LDI R16,0b01100000											
				CALL CRC3
				CALL TRANSMIT
				RET

CRC_CHECK11:
																		
																	    
		MOV R17,R21													    
		LDI R22,0b11010100											    
		ANDI R21,0b11100000												
		LDI R24,11																										

		CRC_CHECK11_LOOP1:
				SBRC R20,7								;IF 20[7] IS 1 SKIP NEXT [R20 HAS DATA PACKET]
				EOR R20,R22								
				LSL R21									
				ROL R20									
				DEC R24
				BRNE CRC_CHECK11_LOOP1									
				ANDI R17,0b00011111						;COMMAND PACKET IS IN R17
				LSR R20									
				LSR R20									
				LSR R20									
				CP R17,R20								
				BREQ CRC_CHECK11_LOOP2									
				LDI R16,0								
				RJMP CRC_CHECK11_LOOP3
				
				CRC_CHECK11_LOOP2:
					LDI R16,1 
				
				CRC_CHECK11_LOOP3: 
					RET


CRC3:
													 
	  MOV R19,R16									 
	  LDI R21,0b11010100							 
	  LDI R22,3										
	  
	  CRC3_LOOP1: 
			SBRC R16,7								
			EOR R16,R21							
			LSL  R16								
			DEC R22									
			BRNE CRC3_LOOP1
			LSR R16								
			LSR R16									
			LSR R16									
			ANDI R19,0b11100000						
			OR R16,R19 ; 
			RET  




CRC_CHECK3:
													  
													  
			MOV R19,R16								
			LDI R17,0b11010100						  
			ANDI R16,0b11100000						  
			LDI R22,3							

			CRC_CHECK3_LOOP1:
				SBRC R16,7								
				EOR R16,R21							
				LSL  R16								
				DEC R22									
				BRNE CRC_CHECK3_LOOP1
				LSR R16									
				LSR R16									
				LSR R16								
				ANDI R19,0b00011111						
				CP R19,R16								
				BRNE CRC_CHECK3_LOOP2									
				LDI R16,1								
				RJMP CRC_CHECK3_LOOP3									
			
			CRC_CHECK3_LOOP2: 
				LDI R16,0								
			
			CRC_CHECK3_LOOP3: 
				RET