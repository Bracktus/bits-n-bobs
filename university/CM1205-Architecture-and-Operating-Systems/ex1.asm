.586
.model flat,stdcall
.stack 4096
ExitProcess proto,dwExitCode:dword
.data
.code
main proc
	 mov eax,0
	 mov  ebx,0
	 mov  ecx,0
	 mov  edx,0
	 mov   al,7
	 mov   bh,2
	 add  al,bh
	 mov  eax,1
	 sub eax,ebx
	 invoke ExitProcess,0
main endp
end main