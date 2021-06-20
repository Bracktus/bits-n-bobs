.586p
.model flat,stdcall
.data
.stack
.code
main PROC
	mov eax,0
	mov ebx,0
	mov ecx,0
	mov edx,0
	MOV AL,4
	DEC AL
	DEC AL
	DEC AL
	DEC AL
	DEC AL
	MOV BL,0FEH
	INC BL
	INC BL
main ENDP
END main