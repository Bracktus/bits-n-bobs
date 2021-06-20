.586
.model flat, stdcall
	
GetStdHandle proto :dword
ReadConsoleA  proto :dword, :dword, :dword, :dword, :dword
ExitProcess proto :dword
WriteConsoleA proto :dword, :dword, :dword, :dword, :dword
 	
STD_INPUT_HANDLE equ -10
STD_OUTPUT_HANDLE equ -11
 	
.data

	myString db "Enter F to convert from farenheit to celsius or C to convert from celsius to farenheit",0
	myString2 db "Enter your value you want converted",0
	newLine db 0dh,0ah,0

	choice db ?
	finalOutput db 10 dup(0)
	
	outputHandle DWORD ?
	bytes_written dd ?
	bytes_written2 dd ?
	
	temp db ?
	actualNum dw 0

	bufSize = 140
 	inputHandle DWORD ?
 	buffer db bufSize dup(?)
 	bytes_read  DWORD  ?
	
.code
main proc
	
	invoke GetStdHandle, STD_OUTPUT_HANDLE
	mov outputHandle, eax
	mov eax, LENGTHOF myString ;Instead of using bufSize use this so that length of the message is the perfect size
	invoke WriteConsoleA, outputHandle, addr myString, eax, addr bytes_written, 0
	mov eax,0
	
	; ASCII code for 'F' is 70
	; ASCII code for 'C' is 67

	invoke GetStdHandle, STD_INPUT_HANDLE
	mov inputHandle, eax
 	invoke ReadConsoleA, inputHandle, addr buffer, bufSize, addr bytes_read,0
	sub bytes_read, 2 ; To remove carrige return and line finish
	
	mov al, 70
	cmp al, buffer
	jz choice_is_F			;Basically if input is 'F' make choice '0' else make it '1'
	mov choice, 1
	jmp getNum
	choice_is_F:
		mov choice, 0
	
	;Getting the number from the user.
	;It stores the final output in the variable 'actualNum'
	getNum:

		invoke GetStdHandle, STD_OUTPUT_HANDLE
		mov outputHandle, eax
		mov eax, LENGTHOF myString2 ; Instead of using bufSize use this so that length of the message is the perfect size
		invoke WriteConsoleA, outputHandle, addr myString2, eax, addr bytes_written, 0
		mov eax,0
		
		invoke GetStdHandle, STD_INPUT_HANDLE
		mov inputHandle, eax
 		invoke ReadConsoleA, inputHandle, addr buffer, bufSize, addr bytes_read,0
	
		; 123 = 1*100 + 2*10 + 3*1 also (10 * (10 * 1 + 2) + 3)

		;First digit of num
		mov eax, 0
		mov al, buffer
		sub al, 30H

	
		mov ecx, 0
		mov actualNum, ax	
		mov cl, 1
		sub bytes_read, 2
		lea edx, actualNum
	
		;START LOOP
		next_char:
			cmp ecx, bytes_read
			jz conversion_finished
	
			mov ax, 10
			mul actualNum
			mov actualNum, ax
		
			mov al, buffer + [ecx]
			sub al, 30H
			add actualNum, ax
		
			inc cl
			jmp next_char
		;END LOOP
	
		conversion_finished:
			cmp choice, 0
			je farenheit_to_celsius
			jmp celsius_to_farenheit


	farenheit_to_celsius:
		mov eax, 0
		mov ax, 32
		sub actualNum, ax
		mov ax, actualNum

		mov ax, 5
		mul actualNum
		mov actualNum, ax

		mov temp, 9
		div temp
		and ax, 0ffh ; This resets all the bits except for the ones I want
		mov actualNum, ax
		jmp convertBackToASCII
	
	celsius_to_farenheit:
		mov eax, 0
		mov ax, 9
		mul actualNum
		mov actualNum, ax
	
		; For the div command ax must be dividend and an operand for divisior
		; Also AL will contain quotient and AH will contain remainder
		mov temp, 5
		div temp
		and ax, 0ffh ; This resets all the bits except for the ones I want
		mov actualNum, ax
	
		mov al, 32
		add actualNum, ax
		mov ax, actualNum
		
		jmp convertBackToASCII

convertBackToASCII:
	mov ebx, 0
	mov bl, 10	
	mov cl, 9
	
	next_dig:
		div bl
		add ah, 30H
		lea edx, finalOutput
		mov finalOutput + [ecx], ah
		dec cl
		mov ah, 0
		cmp al, 0
		jnz next_dig


invoke GetStdHandle, STD_OUTPUT_HANDLE
mov outputHandle, eax
mov eax, LENGTHOF finalOutput ;Instead of using bufSize use this so that length of the message is the perfect size
invoke WriteConsoleA, outputHandle, addr finalOutput, eax, addr bytes_written, 0
mov eax,0



finish:
	invoke ExitProcess, 0
main endp

 	
end main
