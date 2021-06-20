; Calculate the value of X, given that
; X = 3*(10A - (B+2))/(A+B) - (B+2)
;
; This ain't the only way to do this
.586p
.model flat, stdcall
.data
; Declare varA varB and varX
	VarA	DB ?
	VarB	DB ?
	VarX	DB ?
; Some useful temp variables
	Temp1	DB ?
	Temp2	DB ?
.stack
.code
main PROC
	MOV VarA, 4		; Assign inital values to A
	MOV VarB, 5		; and B (A=4, B=5)

; First calculate (A+B) and store it in Temp1
	MOV AL, VarA
	ADD AL, VarB	
	MOV Temp1, AL	; Temp1 = A+B
; Now Calculate (B+2)
	MOV AL, VarB 
	MOV Temp2, AL
	ADD Temp2, 2	; Temp2 = B+2
; Now calculate 3*(10A - (B+2))
	MOV AL, VarA	; AL is the lower bit part of EAX
	MOV BL, 10		; BL is the lower bit part of EBX
	MUL BL			; Multiplies EBX with EAX (stores it in EAX idk why)
	SUB AL, Temp2	; Subtract (B+2) from EAX
	MOV BL, 3		; Move 3 into EBX
	MUL BL			; Multiply the lot by 3
; Can now divide by (A+B) and subtract by (B+2)
	MOV BL, Temp1	; EBX = A+B
	MOV AH, 0		; AH is lower middle bit part of EAX (this doesn't move 0 into eax it just extends the byte)
	DIV BL			; divide EAX by EBX (3*(10A -(B+2)/A+B) 
; Finally, put the result in VarX
	MOV VarX, AL
	NOP
	NOP
main ENDP
END main


