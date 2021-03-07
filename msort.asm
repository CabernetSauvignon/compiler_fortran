COMMENT*

	THIS ASM CODE WAS AUTOMATICALY BUILT 
	BY FORTRAN->MASM32 KOMPILER, WHICH WAS MADE BY BGTU STUDENTS FROM GROUP I584:
 	VITOV ARTEM, MOROZOV KIRILL, MORDOVSKIY ALEKSANDR
	Fri Jul 10 16:50:37 2020

*

.386
.model flat, stdcall
.stack 4096h
include \masm32\include\masm32rt.inc
include customMacros.inc

.data
.code
FUN proc FACT:dword, N:dword
	mov eax, FACT
	mov ebx, N
	mul ebx
	push eax
	pop FACT
	mov eax, N
	mov ebx, 1
	sub eax, ebx
	push eax
	pop N
	push N
	push 0
	pop ebx
	pop eax
	cmp eax, ebx
	jg M1@
	jmp M2@

M1@:
	push N
	push FACT
	call FUN
	jmp M3@

M2@:
M3@:
	printf("%d\n", FACT)
	ret
FUN endp

main PROC
	LOCAL FACT: dword
	LOCAL N: dword
	push 1
	pop FACT
	push 5
	pop N
	push N
	push FACT
	call FUN
	inkey
	call ExitProcess
main ENDP
END main

