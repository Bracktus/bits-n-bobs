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
		;MOV AX,1
		;MOV BX,1
		;MOV CX,2
		MOV DX,165		; Move ascii code for 'n' to DX
		ADD AX,BX		; AX = AX + BX
		SUB CX,AX		; CX = CX - AX
		JZ	MATCH		; If the last operation ended in 0 then jump to 'MATCH'
		JMP RESET
	MATCH:
		MOV DX,121		; Move ascii code for 'y' to DX
	RESET:
		MOV AX,0
		MOV BX,0
		MOV CX,0
	finish:
		invoke ExitProcess,0
main endp
end main