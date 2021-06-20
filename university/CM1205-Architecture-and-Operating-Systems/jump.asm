.586
.model flat, stdcall
option casemap :none
.stack 4096
ExitProcess proto,dwExitCode:dword
.data
	;PUT ANY DATA YOUR PROGRAM NEEDS HERE
	;i.e define your variables.
.code
main proc
	;PUT YOUR CODE HERE.
		MOV CX,AX		; Move contents of AX to CX 
		SUB AX,BX		; Subtract contents of BX from AX ie AX = AX - BX
		JZ	ITSAMATCH	; If the last operation ended in 0 then jump to 'ITSAMATCH'
		MOV DX,0
		JMP RESETAX
	ITSAMATCH:
		MOV DX,1
	RESETAX:
		MOV AX,CX
finish:
	invoke ExitProcess,0
main endp
end main