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
			MOV AX,03h
			MOV BX,04h
			MOV CX,0 
	AGAIN:	
			ADD CX,AX
			DEC BX
			JZ	DONE
			JMP AGAIN
	DONE:	
			MOV AX,0
			MOV BX,0
finish:
	;invoke ExitProcess,0
main endp
end main