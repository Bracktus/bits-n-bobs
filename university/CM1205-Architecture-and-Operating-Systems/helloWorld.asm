.586
.model flat, stdcall
option casemap :none
.stack 4096
GetStdHandle	proto :dword
ExitProcess		proto,dwExitCode:dword
WriteConsoleA		   proto :dword, :dword, :dword, :dword, :dword
MessageBoxA		proto :dword, :dword, :dword, :dword

.data
		msg_txt				db		"Test program",0
		msg_caption			db		"Hello World",0
		STD_OUTPUT_HANDLE	equ		-11
		outputHandle		DWORD	?
		bytes_written		dd		?


.code
		main proc
		invoke GetStdHandle, STD_OUTPUT_HANDLE
		mov outputHandle, eax
		invoke WriteConsoleA,outputHandle, addr msg_caption, eax, addr bytes_written, 0
		invoke MessageBoxA, outputHandle, addr msg_txt, addr msg_caption,0

		invoke ExitProcess,0
		main endp
end main
