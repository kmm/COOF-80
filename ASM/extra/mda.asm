; 	0x00 - NOP
; 	0x01 - Get character from UART, returned in A
;	0x02 - Send character in A to UART
;	0x03 - Send zero-terminated string pointed to by HL to UART
;	0x04 - Split rxbuf and place in argbuf
;	0x05 - Convert byte in A to ASCII hex, leave two chars in HL
;	0x06 - Convert two ASCII chars in HL to byte, leave byte in A
;	0x07 - Delay number of loops specified by HL
;	0x08 - Get argbuf address and return it in HL
;	0x09 - Get rxbuf address and return it in HL
;	0x0A - Get argument from split specified by index in A and return pointer in IX
INT8_GETCHAR		.equ 01h
INT8_PUTCHAR		.equ 02h
INT8_PUTSTR			.equ 03h
INT8_SPLIT			.equ 04h
INT8_B2A			.equ 05h
INT8_A2B			.equ 06h
INT8_DELAY			.equ 07h
INT8_ARGPTR			.equ 08h
INT8_RXBUFPTR		.equ 09h
INT8_GET_SUBARG		.equ 0Ah

start: .org 3000h
	ld a, INT8_PUTSTR			
	push af						; push call number
	ld hl, hello				; set call parameters
	rst 08h
	
	ld bc, 03b8h
	ld a, 1
	out (c), a
	
	; ld a, 0
	; ld l, 61h
	; call crtc_setreg
	
	; ld a, 1
	; ld l, 50h
	; call crtc_setreg

	; ld a, 2
	; ld l, 52h
	; call crtc_setreg
	
	; ld a, 3
	; ld l, 0Fh
	; call crtc_setreg
	
	; ld a, 4
	; ld l, 19h
	; call crtc_setreg

	; ld a, 5
	; ld l, 06h
	; call crtc_setreg	

	; ld a, 6
	; ld l, 19h
	; call crtc_setreg

	; ld a, 7
	; ld l, 19h
	; call crtc_setreg
	
	; ld a, 8
	; ld l, 02h
	; call crtc_setreg

	; ld a, 9
	; ld l, 0Dh
	; call crtc_setreg

	; ld a, 10
	; ld l, 0Bh
	; call crtc_setreg
	
	; ld a, 11
	; ld l, 0Ch
	; call crtc_setreg
	
	; ld a, 12
	; ld l, 00h
	; call crtc_setreg
	
	; ld a, 13
	; ld l, 00h
	; call crtc_setreg	
	
	; ld a, 14
	; ld l, 00h
	; call crtc_setreg
	
	; ld a, 15
	; ld l, 00h
	; call crtc_setreg	
	
	ld a, 0
	ld l, 49h
	call crtc_setreg
	
	ld a, 1
	ld l, 39h
	call crtc_setreg

	ld a, 2
	ld l, 3Dh
	call crtc_setreg
	
	ld a, 3
	ld l, 02h
	call crtc_setreg
	
	ld a, 4
	ld l, 22h
	call crtc_setreg

	ld a, 5
	ld l, 08h
	call crtc_setreg	

	ld a, 6
	ld l, 1Fh
	call crtc_setreg

	ld a, 7
	ld l, 1Eh
	call crtc_setreg
	
	ld a, 8
	ld l, 00h
	call crtc_setreg

	ld a, 9
	ld l, 0Ah
	call crtc_setreg

	ld a, 10
	ld l, 01h
	call crtc_setreg
	
	ld a, 11
	ld l, 0Ah
	call crtc_setreg
	
	ld a, 12
	ld l, 00h
	call crtc_setreg
	
	ld a, 13
	ld l, 00h
	call crtc_setreg	
	
	ld a, 14
	ld l, 00h
	call crtc_setreg
	
	ld a, 15
	ld l, 00h
	call crtc_setreg	
	
	ld bc, 03b8h
	ld a, 9
	out (c), a	
	
	ret							; return
; reg in A, value in L
crtc_setreg:
	ld bc, 03B4h
	out (c), a
	
	ld bc, 03B5h
	ld a, l
	out (c), a
	ret

.org 3100h
crlf	.db "\r\n",0
hello	.db "Initializing ISA MDA card",0
poop	.db "Poop!",0
.end