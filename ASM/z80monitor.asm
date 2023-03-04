; COOFMON-Z80
; -----------------------------------------------------------------------------
; A simple ROM monitor for a simple Z80 computer, written during the first 
; weeks of lockdown for the unknown pathogen of unspecified origin in 1Q2020.
; (c) kmm 2020, MIT license
; -----------------------------------------------------------------------------
; CLI Commands
; d[ump] <hexaddr> <hexlen>
;	Dump <hexlen> bytes memory starting at <hexaddr>
; x[ecute] <hexaddr>
;	Jump to <hexaddr> and execute
; c[all] <hexaddr>
;	Call <hexaddr> and return to monitor
; f[ill] <hexaddr> <hexlen> <hexval>
;	Fill <hexlen> bytes starting at <hexaddr> with <hexval>
; :ihxstring
;	Load Intel hex string into memory
; w[rite] <hexval> <hexaddr>
;	Write <hexval> to memory location <hexaddr>
; i[nput] <hexport>
;	Read <hexport> and print the value (high order byte ignored)
; o[utput] <hexport> <hexval>
;	Write <hexval> to <hexport> (high order byte ignored)
; -----------------------------------------------------------------------------
; Also provides some "BIOS" type interrupt utility routines that can be 
; used by other code. Refer to implementations for more details.
; API call IDs:
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
;	0x0A - Get substring from argbuf
; Calling convention:
; 	- Push call number on stack
;	- Load registers as appropriate for the API call being used
;	- Execute RST 08h

; UART Parameters
BAUDRATE			.equ 9600					; console baud rate
UARTCLK				.equ 4000000				; 16C550 crystal frequency
; Program Constants
STACKTOP			.equ 2040h 					; set stack pointer to 64 bytes above rom end
RXBUFLEN			.equ 160
TXBUFLEN			.equ 32
ARGBUFLEN			.equ 160
; Argument indexes, mostly for clarity in code
IDX_CMD				.equ 0
IDX_ARG_1			.equ 1
IDX_ARG_2			.equ 2
IDX_ARG_3			.equ 3
IDX_ARG_4			.equ 4
; UART ports and registers
#ifdef ZEMU
UART_BASE			.equ 20h					; Simulated UART starts at IO address 0x20 in ZEMU
UART_LSR			.equ UART_BASE+1
#else
UART_BASE			.equ 08h					; 16C550 starts at 0x08 in real hardware
UART_LSR			.equ UART_BASE+5
#endif
UART_RHR			.equ UART_BASE				; rx holding register
UART_THR			.equ UART_BASE				; tx holding register
UART_IER			.equ UART_BASE + 1			; interrupt enable register
UART_FCR			.equ UART_BASE + 2			; fifo control register
UART_ISR			.equ UART_BASE + 2			; interrupt status register
UART_LCR			.equ UART_BASE + 3			; line control register
UART_MCR			.equ UART_BASE + 4			; modem control register
UART_MSR			.equ UART_BASE + 6			; modem status register
UART_SPR			.equ UART_BASE + 7			; scratchpad register
UART_BDL			.equ UART_BASE				; baud rate divisor low byte at base + 0 (with lcr bit 7 set)
UART_BDH			.equ UART_BASE + 1			; baud rate divisor low byte at base + 1 (with lcr bit 7 set)

UART_LCR_bit_dle	.equ 80h					; lcr bit 7, divisor latch enable
UART_LCR_bit_brk	.equ 40h					; lcr bit 6, set break mode
UART_LCR_bit_sp		.equ 20h					; lcr bit 5, set parity
UART_LCR_bit_ep		.equ 10h					; lcr bit 4, even parity
UART_LCR_bit_pe		.equ 8h						; lcr bit 3, parity enable 
UART_LCR_bit_sb		.equ 4h						; lcr bit 2, stop bit length (0: 1, 1: 2)
UART_LCR_5b			.equ 0h						; lcr bits 0 and 1, 5 bits
UART_LCR_6b			.equ 1h						; lcr bits 0 and 1, 6 bits
UART_LCR_7b			.equ 2h						; lcr bits 0 and 1, 7 bits
UART_LCR_8b			.equ 3h						; lcr bits 0 and 1, 8 bits

UART_LSR_bit_txe	.equ 6						; lsr bit 6, tx holding register and shift register empty
UART_LSR_bit_thre	.equ 5						; lsr bit 5, tx holding register empty
UART_LSR_bit_rxr	.equ 0						; lsr bit 0, rx data ready

uart_con_baudl		.equ UARTCLK/(16*BAUDRATE)	; baud rate divisor [uart clock / (16 * baud rate)]
uart_con_baudh		.equ 00h					; high byte not used

uart_con_lcr		.equ UART_LCR_8b			; 8, n, 1

; Static allocations
v_rxbuf				.equ STACKTOP+1	     		; 160 byte rx buffer
v_rxbufpos			.equ v_rxbuf+RXBUFLEN+1		; 1 byte rx buffer pointer
v_txbuf				.equ v_rxbufpos+1   		; 32 byte tx buffer
v_txbufpos			.equ v_txbuf+TXBUFLEN+1		; 1 byte tx buffer pointer
v_argbufpos   		.equ v_txbufpos+1			; the current position in the buffer
v_argcount    		.equ v_argbufpos+1			; the number of arguments found
v_argbuf      		.equ v_argcount+1			; the buffer that will hold the arguments
v_dump_start		.equ v_argbuf+1				; dump start variable (16 bit)
v_dump_length		.equ v_dump_start+2			; dump length variable (16 bit)
v_scratch			.equ v_dump_length+2		; scratch buffer

; Startup vector
start:	.org 0000h
	jp init
; Interrupt vectors
int8:	.org 0008h								; vector for INT 8 API calls
	jp int8_handler
; Main code start
init: .org 007Fh
	ld sp, STACKTOP								; set stack pointer
	call uart_init								; init uart
	ld hl, ansi_csi_clear
	call uart_putstr
	ld hl, ansi_csi_home
	call uart_putstr
	ld hl, ansi_bg_blue
	call uart_putstr
	ld hl, ansi_fg_yellow
	call uart_putstr
	ld hl, hello								; load address of string into hl (arg for uart_txstr)
    call uart_putstr							; send it
	ld hl, ansi_bg_black
	call uart_putstr
	ld hl, ansi_fg_white
	call uart_putstr	
	ld hl, crlf									; send a CRLF
	call uart_putstr
	ld hl, prompt								; send a prompt
	call uart_putstr
	ld a, 0
	ld (v_rxbufpos), a							; initialize the RX buffer position
	jp mainloop

; This exposes some handy routines for use by other code loaded by
; the monitor. Think DOS's INT 21h API.
;
; API call IDs:
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
;	0x0A - Get substring from argbuf
; Calling convention:
; 	- Push call number on stack
;	- Load registers as appropriate for the API call being used
;	- Execute RST 08h
int8_handler:						; handler for INT 8 API calls
	pop iy							; save return address in IY
	ex af, af'						; swap in shadow register A
	pop af							; pop the API call number into A
	push iy							; push the return address back on the stack
	cp 01h							; jump to requested routine
	jr z, int8_getch
	cp 02h 
	jr z, int8_putch
	cp 03h
	jr z, int8_putstr
	cp 04h
	jr z, int8_split
	cp 05h
	jr z, int8_byte_to_ahex
	cp 06h
	jr z, int8_ahex_to_byte
	cp 07h
	jr z, int8_delay
	cp 08h
	jr z, int8_get_argbuf_ptr
	cp 09h
	jr z, int8_get_rxbuf_ptr
	cp 0ah
	jr z, int8_get_substr
	ret								; return if no match
int8_getch:
	ex af, af' 						; swap out shadow register A
	jp uart_getch					; jump to routine, return address was already pushed by RST
int8_putch:
	ex af, af'						
	jp uart_putch
int8_putstr:
	ex af, af'						
	jp uart_putstr
int8_split:
	ex af, af'
	jp split
int8_byte_to_ahex:
	ex af, af'
	jp byte_to_ahex
int8_ahex_to_byte:
	ex af, af'
	jp ahex_to_byte
int8_delay:
	ex af, af'
	jp delay
int8_get_argbuf_ptr:
	ex af, af'
	ld hl, v_argbuf
	ret
int8_get_rxbuf_ptr:
	ex af, af'
	ld hl, v_rxbuf
	ret
int8_get_substr:
	ex af, af'
	jp get_substr

; Commands
;
; Numeric arguments all in hex
;
; Dump memory hex
; d [addr] [length]
;
; Load Intel Hex string into memory
; :IHXSTRING
; 
; Execute (jump to addr)
; x [addr]
;
; Write memory (1 byte)
; w [addr] [value]
;
; Fill memory
; f [addr] [value] [length]
;
; Test memory
; t [start-addr] [end-addr]
;
; Set blinkenlights
; b [value]
;
mainloop:
	call uart_getch					; read char from uart (blocking)
									; handle some control characters
	cp 0Ah							; throw it away if it's a LF
	jr z, mainloop					; ...
	cp 0Dh							; process command if it's a CR (enter key)
	jr z, ml_process				; ...
	cp 08h							; process backspace
	jr z, ml_backspace				; ...
	cp 0Ch							; process ctrl-L (clear buffer)
	jr z, ml_clearbuf				; ...
	
	ld d, a							; otherwise save char in d
	ld hl, v_rxbuf					; load RX buffer address in hl
	ld a, (v_rxbufpos)				; load RX buffer position
	call add_a_hl                   ; add RX buffer position to hl
	ld a, d							; copy inputted char from d to a
	ld (hl), a                      ; save inputted char to RX buf
	ld a, (v_rxbufpos)				; load buffer position into a
	inc a                           ; increment RX buf position
	ld (v_rxbufpos), a				; save RX buf position
	inc hl							; increment RX buf position
	ld (hl), 0                      ; zero terminate
	ld a, d							; load d (input char) into a
	call uart_putch					; echo back character in a
	jr mainloop						; continue looping
	
ml_backspace:
	ld a, (v_rxbufpos)				; load buffer position
	cp 0							; if it's already zero just return
	jr z, mainloop					; ...
	
	ld hl, termbs					; send terminal backspace sequence
	call uart_putstr				; ...
	
	ld a, (v_rxbufpos)				; load buffer position	
	dec a							; decrement it
	ld (v_rxbufpos), a				; save it
	ld hl, v_rxbuf					; load rx buffer pointer
	call add_a_hl					; add position to it
	ld (hl), 0						; zero terminate it
	jr mainloop						; start the loop again

ml_clearbuf:
	ld hl, v_rxbufpos				; zero bufpos
	ld (hl), 0						; ...
	ld hl, v_rxbuf					; zero terminate input buffer
	ld (hl), 0						; ...
	ld hl, ansi_csi_clear			; clear the screen
	call uart_putstr				; ...
	ld hl, ansi_csi_home			; home the cursor
	call uart_putstr				; ...
	call display_prompt				; display the prompt
	jp mainloop						; start the loop again

ml_process:
	ld hl, crlf
	call uart_putstr
	
	ld hl, v_rxbuf					; split the input string
	call split						; ...
	
	ld a, IDX_CMD					; get first field
	call get_substr					; ...
	
	ld a, (hl)						; load first char into a
	
	cp 20h
	jp z, ml_prompt_restart
	cp 0h
	jp z, ml_prompt_restart
	cp 'x'							
	jp z, cmd_execute
	cp 'c'
	jp z, cmd_call
	cp 'd'
	jp z, cmd_dump
	cp 'w'
	jp z, cmd_write
	cp ':'
	jp z, cmd_loadihx
	cp 'f'
	jp z, cmd_fill
	cp 'i'
	jp z, cmd_input
	cp 'o'
	jp z, cmd_output
	cp 'r'
	jp z, cmd_restart
	cp 'h'
	jp z, cmd_help
	cp '?'
	jp z, cmd_help	
	jp cmd_bad
	
ml_prompt_restart:
	ld a, 0
	ld (v_rxbufpos), a				; reset buffer position
	ld (v_rxbuf), a					; null terminate the head of the input buffer	
	call display_prompt				; print prompt
	jp mainloop						; restart loop

cmd_bad:
	ld hl, ansi_fg_red
	call uart_putstr
	ld hl, err_bad_command
	call uart_putstr
	ld hl, ansi_fg_white
	call uart_putstr
	jp ml_prompt_restart

; -----------------------------------------------------------------------------
; Command: x <hexaddr>
; 	Jump execution to <hexaddr>.
; -----------------------------------------------------------------------------
cmd_execute:
	ld a, IDX_ARG_1					; get address argument string pointer
	call get_substr					; ...
	call get_str_hex_val			; convert string to hex value and leave it in HL
	call check_hexarg				; validate the argument (exits the command if fail)
	jp (hl)
	jp mainloop

; -----------------------------------------------------------------------------
; Command: c <hexaddr>
; 	Call <hexaddr>.
; -----------------------------------------------------------------------------
cmd_call:
	ld a, IDX_ARG_1					; get address argument string pointer
	call get_substr					; ...
	call get_str_hex_val			; convert string to hex value and leave it in HL
	call check_hexarg				; validate the argument (exits the command if fail)
	ld iy, $+7
	push iy
	jp (hl)
	jp ml_prompt_restart

; -----------------------------------------------------------------------------
; Command: r <hexaddr>
; 	Clears IX and IY, resets SP, jumps to (start)
; -----------------------------------------------------------------------------
cmd_restart:
	ld ix, 0000h
	ld iy, 0000h
	ld sp, STACKTOP
	jp start
	

; -----------------------------------------------------------------------------
; Command: d <hexaddr> <hexlen>
; 	Print <hexlen> bytes of memory starting at <hexaddr>.
; -----------------------------------------------------------------------------
cmd_dump:
	; get address argument
	ld a, IDX_ARG_1					; get address argument string pointer
	call get_substr					; ...
	call get_str_hex_val			; convert string to hex value and leave it in HL
	call check_hexarg				; validate the argument (exits the command if fail)
	push hl							; load it into BC via the stack because get_str will smash IX next time we call it
	pop bc							; ...
									; get length argument
	ld a, IDX_ARG_2					; get length argument string pointer
	call get_substr					; ...
	call get_str_hex_val			; convert string to hex value and leave it in HL
	call check_hexarg				; validate the argument	
	push hl							; load it into DE via the stack
	pop de							; ...
	ld a, d							; check for zero length
	or e							; ...
	jr z, cmd_dump_end				; exit if so
	push bc							; copy BC to IX
	pop ix							
	ld bc, 0						; zero out BC (position counter)
	jr cmd_dump_start				; start dumping
cmd_dump_start_line:
	ld hl, crlf						; print a newline
	call uart_putstr				; ...
cmd_dump_start:
	ld hl, ansi_fg_cyan				; set fg color to cyan
	call uart_putstr				; ...
	push ix							; copy IX to HL
	pop hl							; ...
	ld a, h							; convert H to ascii and print it
	call byte_to_ahex				; ...
	call uart_put_hl				; ...
	push ix							; copy IX to HL
	pop hl							; ...
	ld a, l							; convert L to ascii and print it
	call byte_to_ahex				; ...
	call uart_put_hl				; ...
	ld hl, ansi_fg_white			; set fg color to white
	call uart_putstr				; ...
	ld a, ' '						; print a space
	call uart_putch					; ...
cmd_dump_print:
	ld a, (ix)						; load byte pointed to by IX
	call byte_to_ahex				; convert it to two ASCII bytes (in HL)
	call uart_put_hl				; send the two characters in HL
	inc ix							; increment pointer
									; check for terminal condition
	push de							; load DE into HL via the stack
	pop hl							; ...
	or a							; 16 bit compare HL [DE] and BC
	sbc hl, bc						; ...
	add hl, bc						; ...
	jr z, cmd_dump_end				; exit routine
	inc bc							; increment counter
	push bc
	push de
	ld d, 10h
	call c_div_d
	cp 0
	pop de
	pop bc
	jp z, cmd_dump_start_line
	ld a, ' '
	call uart_putch
	jr cmd_dump_print
cmd_dump_end:	
	jp ml_prompt_restart

; -----------------------------------------------------------------------------
; Command: w <hexaddr> <value>
; 	Write low byte of <value> to memory at <hexaddr>.
; -----------------------------------------------------------------------------
cmd_write:
	ld a, IDX_ARG_1					; get address argument string pointer
	call get_substr					; ...
	call get_str_hex_val			; convert string to hex value and leave it in HL
	call check_hexarg				; validate the argument (exits the command if fail)
	push hl							; load it into BC via the stack because get_str will smash IX next time we call it
	pop bc							; ...
									; get length argument
	ld a, IDX_ARG_2					; get length argument string pointer
	call get_substr					; ...
	call get_str_hex_val			; convert string to hex value and leave it in HL
	call check_hexarg				; validate the argument
	
	ld a, l
	ld (bc), a
	jp ml_prompt_restart

; -----------------------------------------------------------------------------
; Command: <intel hex string>
; 	Write <intel hex string> to memory as specified in the string
; -----------------------------------------------------------------------------	
cmd_loadihx:
	ld ix, v_rxbuf					; load IX with base of input buf
	ld b, (ix)						
	inc ix
	ld h, (ix+6)					; load the record type high nybble into H
	ld l, (ix+7)					; and the low nybble into L
	call ahex_to_byte				; convert it to a byte and leave it in A
	cp 1							; check the type
	jp z, ihx_eof					; if type == 1 it's an EOF and we're done
	jp nc, ihx_bad_record			; if type > 1 it's a record we don't support
									; else it's a data record
	ld h, (ix)						; load the high nybble of the length field into H
	ld l, (ix+1)					; and the low nybble into L
	call ahex_to_byte				; convert it to a number
	ld c, a							; save length to C
	ld h, (ix+2)					; load the high byte of the address field into HL
	ld l, (ix+3)					; ...
	call ahex_to_byte				; convert it
	ld d, a							; save it in D
	ld h, (ix+4)					; load the low byte of the address field into HL
	ld l, (ix+5)					; ...
	call ahex_to_byte				; convert it
	ld e, a							; save it in E
									; length is in C, address is in DE
	ld ix, v_rxbuf+9				; load start of data section of record into IX
	ld b, 0
ihx_load_loop:
	ld h, (ix)
	ld l, (ix+1)
	ld a, h
	cp 0
	jr z, ihx_record_short
	call ahex_to_byte
	ld (de), a
	add a, b						; add A to B to generate the checksum
	ld b, a							; store it in B
	inc ix
	inc ix
	inc de
	dec c
	ld a, c
	cp 0
	jr z, ihx_ok
	jp ihx_load_loop

ihx_record_short:
	ld hl, msg_ihx_shortrec
	call uart_putstr
	jr ihx_end
ihx_bad_record:
	ld hl, msg_ihx_badrec
	call uart_putstr
	jr ihx_end
ihx_eof:
	ld hl, msg_ihx_eof
	call uart_putstr
	jr ihx_end
ihx_ok:
	ld h, 'O'
	ld l, 'K'
	call uart_put_hl
ihx_end:
	jp ml_prompt_restart
	
; -----------------------------------------------------------------------------
; Command: f <hexaddr> <hexlen> <hexval>
; 	Write <hexval> to memory starting at <hexaddr> for <hexlen> bytes.
; -----------------------------------------------------------------------------	
cmd_fill:
	ld a, IDX_ARG_1					; get start address string pointer
	call get_substr					; ...
	call get_str_hex_val			; convert string to hex value and leave it in HL
	call check_hexarg				; validate the argument (exits the command if fail)
	push hl							; load it into BC via the stack because get_str will smash IX next time we call it
	pop de							; ...
	
	ld a, IDX_ARG_2					; get length string pointer
	call get_substr					; ...
	call get_str_hex_val			; convert string to hex value and leave it in HL
	call check_hexarg				; validate the argument
	push hl
	pop bc
	
	ld a, IDX_ARG_3					; get value argument string pointer
	call get_substr					; ...
	call get_str_hex_val			; convert string to hex value and leave it in HL
	call check_hexarg				; validate the argument
	
	ld a, l
	push de
	pop hl
	push bc
	pop de
	call memset
	jp ml_prompt_restart
	
; -----------------------------------------------------------------------------
; Command: i <hexport>
; 	Read value of port at low byte of <hexport> and print it.
; -----------------------------------------------------------------------------
cmd_input:
	ld a, IDX_ARG_1					; get port argument string pointer
	call get_substr					; ...
	call get_str_hex_val			; convert string to hex value and leave it in HL
	call check_hexarg				; validate the argument (exits the command if fail)
	push hl							; load it into BC via the stack because get_str will smash IX next time we call it
	pop bc							; ...
	in a, (c)						; read port
	call byte_to_ahex				; convert it to ASCII hex
	call uart_put_hl				; print it
	jp ml_prompt_restart

; -----------------------------------------------------------------------------
; Command: o <hexport> <hexval>
; 	Write low byte of <hexval> to port at low byte of <hexport>.
; -----------------------------------------------------------------------------
cmd_output:
	ld a, IDX_ARG_1					; get port argument string pointer
	call get_substr					; ...
	call get_str_hex_val			; convert string to hex value and leave it in HL
	call check_hexarg				; validate the argument (exits the command if fail)
	push hl							; load it into BC via the stack because get_str will smash IX next time we call it
	pop bc							; ...
	
	ld a, IDX_ARG_2					; get value argument string pointer
	call get_substr					; ...
	call get_str_hex_val			; convert string to hex value and leave it in HL
	call check_hexarg				; validate the argument
	ld a, l							; load low byte of HL (input value) into A
	out (c), a						; and output it to port at C
	jp ml_prompt_restart
	
cmd_help:
	ld hl, help
	call uart_putstr
	jp ml_prompt_restart

; -----------------------------------------------------------------------------
; Handler for get_str_hex_val return codes in command processing
; If no error, returns to procedure it was called from.
; If error, prints an appropriate error message, fixes the stack, 
; and jumps back to the tail of the CLI loop.
; This should only be called from the command handler after get_str_hex_val
; has been called or it will corrupt the call stack.
; -----------------------------------------------------------------------------
check_hexarg:
	cp 1
	jr z, check_err_no_arg
	cp 2
	jr nc, check_err_arg_too_long
check_ok:
	ret
check_err_no_arg:
	ld hl, ansi_fg_red
	call uart_putstr
	ld hl, err_hexarg_ne
	call uart_putstr
	ld hl, ansi_fg_white
	call uart_putstr
	pop hl							; pop the stack so we don't leak the previous call
	jp ml_prompt_restart			; jump back to the main CLI loop
check_err_arg_too_long:
	ld hl, ansi_fg_red
	call uart_putstr
	ld hl, err_hexarg_range
	call uart_putstr
	ld hl, ansi_fg_white
	call uart_putstr
	pop hl							; pop the stack so we don't leak the previous call
	jp ml_prompt_restart			; jump back to the main CLI loop
	

; -----------------------------------------------------------------------------
; Get hex argument as 16 bit value
; Inputs:
;	IX: ASCII string pointer
; Outputs:
;	A: 0 if no error, 1 if no argument, 2 if argument too long
;	H: high byte of hex value
;	L: low byte of hex value
; Uses:
;	DE IX
; -----------------------------------------------------------------------------
get_str_hex_val:
	push bc
	push de
	push ix
	call strlen						; get length of string pointed to by IX
	cp 0
	jp z, get_hex_end_err_1
	cp 5
	jp nc, get_hex_end_err_2
	cp 1							
	jr z, get_hex_4_bit				; 4 bit (1 hex char) argument
	cp 2
	jr z, get_hex_8_bit				; 8 bit (2 hex char) argument
	cp 3
	jr z, get_hex_12_bit			; 12 bit (3 hex char) argument

get_hex_16_bit:
    ld h, (ix)						; get high order nybble of high order byte and stash it in h
    ld l, (ix+1)					; get low order nybble of high order byte and stash it in l
	call ahex_to_byte				; convert ASCII value to a byte
	ld d, a							; save it to d
	ld h, (ix+2)					; do the same for the low order byte
	ld l, (ix+3)					; ...
	call ahex_to_byte				; ...
	ld e, a							; and save it to e
	ld a, 0
	jr get_hex_end
get_hex_12_bit:
    ld h, '0'						; pad high order byte with 0 (ASCII)
    ld l, (ix)						; get low order nybble of high order byte and stash it in l
	call ahex_to_byte				; convert ASCII value to a byte
	ld d, a							; save it to d
	ld h, (ix+1)					; do the same for the low order byte
	ld l, (ix+2)					; ...
	call ahex_to_byte				; ...
	ld e, a							; and save it to e
	ld a, 0
	jr get_hex_end
get_hex_8_bit:
	ld a, 0
	ld d, a							; load zero into d
	ld h, (ix)						; load value into low order byte
	ld l, (ix+1)					; ...
	call ahex_to_byte				; ...
	ld e, a							; and save it to e
	ld a, 0
	jr get_hex_end
get_hex_4_bit:
	ld a, 0
	ld d, a							; load zero into d
	ld h, '0'						; pad high order nybble with zero (ASCII)
	ld l, (ix)						; ...
	call ahex_to_byte				; ...
	ld e, a							; and save it to e
	ld a, 0
	jr get_hex_end
get_hex_end_err_1:
	ld a, 1
	jp get_hex_end
get_hex_end_err_2:
	ld a, 2
get_hex_end:
	push de							; transfer value from de to hl via the stack
	pop hl							; ...
	
	pop ix
	pop de
	pop bc
	ret
	
; -----------------------------------------------------------------------------
; Fill memory with byte
; Inputs:
;	A: fill value
;	HL: start address
;	DE:	number of bytes to fill
; -----------------------------------------------------------------------------
memset:
	ld c, a					; save a into c
	ld (hl), a				; write value in a into memory pointed to by hl
	inc hl					; move to next position to be filled
	dec de					; decrement de
	ld a, d					; check de for zero
	or e					; ...
	ld a, c					; restore fill value to a
	jr nz, memset			; if de != 0 keep looping
	ret						; else return

; -----------------------------------------------------------------------------
; Get length of zero-terminated string
; Inputs:
;	IX: string start address
; Outputs:
;	A: string length
; -----------------------------------------------------------------------------
strlen:
	push ix
	push bc
	ld c, 0
strlen_loop:
	ld a, (ix)
	cp 0
	jr z, strlen_end
	inc ix
	inc c
	jr strlen_loop
strlen_end:
	ld a, c
	pop bc
	pop ix
	ret
	
; -----------------------------------------------------------------------------
; Copy string in rxbuf to argbuf, replacing spaces with null terminators,
; compressing runs of spaces, and counting instances of the replacements
; Inputs: (rxbuf)
; Outputs: (argbuf)
; Used:
;	AF, CD, EF, HL
; -----------------------------------------------------------------------------
split:
	push af
	push bc
	push de
	push hl
	ld a, 0			
	ld hl, v_argbuf
	ld de, ARGBUFLEN
	call memset					; clear argument buffer
	ld a, 0
	ld (v_argcount), a    		; initialize v_argcount to 0
	ld (v_argbufpos), a   		; initialize v_argbufpos to 0
	ld de, v_rxbuf          	; load input string base address in de
split_loop:                 	; main loop
	ld a, (de)              	; load character pointed to by de into a
	ld b, a                 	; copy it to b
	cp ' '                  	; is it a space
	jr z, split_sep         	; if so jump to seperator handling
	cp 0                    	; is it a zero terminator
	jr z, split_end         	; if so we're done
	ld a, (v_argbufpos)   		; load ARGBUFPOS value into a
	ld hl, v_argbuf       		; load ARGBUFP base address into hl
	call add_a_hl           	; add a to hl
	ld a, b                 	; get character from b
	ld (hl), a              	; save character into ARGBUF location pointed to by hl
	ld hl, v_argbufpos			; load ARGBUFPOS pointer
	inc (hl)               	 	; increment ARGBUFPOSP value
	inc de                  	; increment de
	jr split_loop           	; jump back and process the next character
split_sep:
	ld hl, v_argbufpos			; get ARGBUFPOS pointer
	inc (hl)                	; increment value pointed to
	ld a, (hl)              	; load value into a
	ld hl, v_argbuf				; load argbuf base address in hl
	call add_a_hl           	; add a to hl
	ld (hl), 0              	; store a zero terminator
	ld hl, v_argcount
	inc (hl)
	inc de                  	; increment de
skip_repeated_sep:
	ld a, (de)
	cp ' '
	jr nz, split_loop
	cp 0
	jr z, split_end
	inc de
	jr skip_repeated_sep
split_end:
	pop hl
	pop de
	pop bc
	pop af
	ret

; -----------------------------------------------------------------------------
; Search argbuf for the nth null, then advance one and return the address.
; Inputs:
;	A: argument index to search for
; Outputs:
;	IX: pointer to argument substring
; Uses:
;	BC
; -----------------------------------------------------------------------------
get_substr:
	push af
	push bc
	cp 0                    ; if index == 0 just return the base argbuf pointer
	ld ix, v_argbuf    
	jr nz, get_substr_start
	jr get_substr_end
get_substr_start:
	ld c, a                 ; save the target index in c
	ld b, 0                 ; initialize b (search counter) to zero
get_substr_loop:
	inc ix
	ld a, (ix)
	cp 0
	jr z, get_substr_sep_found
	jr get_substr_loop
get_substr_sep_found:
	inc b
	ld a, c
	cp b
	jr nz, get_substr_loop
	inc ix
get_substr_end:
	pop bc
	pop af
	ret

; -----------------------------------------------------------------------------
; Print the command prompt
; Inputs: none
; Outputs: none
; -----------------------------------------------------------------------------
display_prompt:
	ld hl, crlf
	call uart_putstr
	ld hl, prompt
	call uart_putstr
	ret

; -----------------------------------------------------------------------------
; Convert ASCII hex representation into integer byte.
; Inputs:
;	H: high ASCII nybble
; 	L: low ASCII nybble
; Outputs:
; 	A: integer representation of ASCII byte
; Source:
;	"Z80 Assembly Language Subroutines", Leventhal and Saville, 1983, p. 175
; -----------------------------------------------------------------------------
ahex_to_byte:
	ld a, l
	call a_to_bin
	ld b, a
	ld a, h
	call a_to_bin
	rrca
	rrca
	rrca
	rrca
	or b
	ret
a_to_bin:
	sub '0'
	cp 10
	jr c, a_to_bin_1
	sub 7
a_to_bin_1:
	ret

; -----------------------------------------------------------------------------
; Convert integer byte into ASCII hex representation.
; Inputs:
;	A: integer byte to convert
; Outputs:
; 	H: ASCII representation of high nybble
;	L: ASCII representation of low nybble
; Uses:
;	BC, DE
; -----------------------------------------------------------------------------
byte_to_ahex:
	push bc
	push de
	ld c, a						; load A into C
	ld d, 16					; load 16 into D
	call c_div_d				; divide C by D
	ld h, c						; load the int result (isolated high nybble) into H
	ld l, a						; load the remainder (isolated low nybble) into L
	ld a, h						; load the high nybble into A
	call nyb_to_hex_asc			; convert it to ASCII
	ld h, a						; load the ASCII value into H
	ld a, l						; load the low nybble into A
	call nyb_to_hex_asc			; convert it to ASCII
	ld l, a						; load the ASCII value into L
	pop de
	pop bc
	ret
nyb_to_hex_asc:
	and 0fh						; mask off high nybble we don't need (or want) it
	cp 10						; is nybble >= 10?
	jp nc, ntha_ge_ten			; if so jump to the >= 10 part
	add a, 48					; otherwise add 48 to get the ascii representation
	ret
ntha_ge_ten:
	sub 10						; subtract 10 from the nybble
	add a, 65					; add 65 to get the ascii representation
	ret
	
; -----------------------------------------------------------------------------
; Divide C by D
; Input:
;	C: numerator
;	D: denominator
; Output:
;	A: remainder
;	B: 0
;	C: integer result of C/D
; Source:
;	http://z80-heaven.wikidot.com/math#toc13
; -----------------------------------------------------------------------------
c_div_d:
	ld b, 8
	xor a
	sla c
	rla
	cp d
	jr c, $+4
	inc c
	sub d
	djnz $-8
	ret

; -----------------------------------------------------------------------------
; Add A to HL
; -----------------------------------------------------------------------------
add_a_hl:
    add a, l
    ld l, a
    adc a, h
    sub l
    ld h, a
    ret

; -----------------------------------------------------------------------------
; UART init routine
; -----------------------------------------------------------------------------
uart_init:
	ld a, UART_LCR_bit_dle			; set divisor latch enable
	out (UART_LCR), a				; send it
	ld a, uart_con_baudl			; set divisor low byte
	out (UART_BDL), a				; send it
	ld a, uart_con_baudh			; set divisor high byte
	out (UART_BDH), a				; send it
	;ld a, 1h						; set FCR bit 0 (enable 16550 FIFOs)
	;out (UART_FCR), a				; send it
	ld a, uart_con_lcr				; clear dle, set format
	out (UART_LCR), a				; send it
	ret

; -----------------------------------------------------------------------------
; UART transmit string routine
; Transmits a zero-terminated string pointed to by HL
; -----------------------------------------------------------------------------
uart_putstr:
	ld a, (hl)						; load char pointed to by hl into a
	inc hl							; increment hl
	and a							; A & A to set zero flag if A = 0
	jr z, uart_tx_end				; jump to end if zero
	push af							; save flags while we poll
	out (UART_THR), a				; send it
uart_tx_poll:						; poll uart tx status
	in a, (UART_LSR)				; read the uart line status register
	bit UART_LSR_bit_thre, a		; check if bit 5 is zero (tx holding register full)
	jr z, uart_tx_poll				; poll until uart finishes transmitting (bit 5 set)
	pop af							; restore flags
	jr nz, uart_putstr				; if not zero loop back and get the next char
uart_tx_end:
	ret								; return

; -----------------------------------------------------------------------------
; UART transmit char routine
; Inputs:
;	A: character to transmit
; -----------------------------------------------------------------------------
uart_putch:
	out (UART_THR), a
putch_tx_poll:						; poll uart tx status
	in a, (UART_LSR)				; read the uart line status register
	bit UART_LSR_bit_thre, a		; check if bit 5 is zero (tx holding register full)
	jr z, putch_tx_poll				; poll until uart finishes transmitting (bit 5 set)	
	ret

; -----------------------------------------------------------------------------
; Transmit characters in HL
; Inputs:
;	H:	first character to transmit
;	L:	second character to transmit
; -----------------------------------------------------------------------------
uart_put_hl:
	ld a, h							; load h into a
	call uart_putch					; send it
	ld a, l							; load l into a
	call uart_putch					; send it
	ret

; -----------------------------------------------------------------------------
; UART recieve character routine
; Blocks until a character is received, places received char in A
; -----------------------------------------------------------------------------
uart_getch:
	ld a, 02h
	out (UART_MCR), a				; set the RTS enable bit
uart_getch_loop:
	in a, (UART_LSR)				; poll for a byte recieved
	bit UART_LSR_bit_rxr, a
	jr z, uart_getch_loop
	ld a, 00h
	out (UART_MCR), a				; clear the RTS enable bit		
	in a, (UART_RHR)
	ret

; -----------------------------------------------------------------------------
; Delay Subroutine
; Load number of loops (16 bit, up to 65,535) to delay into BC and call this.
; -----------------------------------------------------------------------------
delay:
	ld a, b						; Load B into A
	or c						; OR it with C, if the result is zero the 'or' instruction sets the Z flag
	dec bc						; Decrement BC		
	jr nz, delay				; If it's not zero loop back to the start of the subroutine
	ret							; If so, we're done delaying

; Some ANSI terminal control codes
ansi_csi_clear		.db 1Bh,"[2J",0
ansi_csi_home		.db 1Bh,"[1;1H",0
ansi_fg_black		.db 1Bh,"[30m",0
ansi_fg_red			.db 1Bh,"[31m",0
ansi_fg_green		.db 1Bh,"[32m",0
ansi_fg_yellow		.db 1Bh,"[33m",0
ansi_fg_blue		.db 1Bh,"[34m",0
ansi_fg_magenta		.db 1Bh,"[35m",0
ansi_fg_cyan		.db 1Bh,"[36m",0
ansi_fg_white		.db 1Bh,"[37m",0
ansi_bg_black		.db 1Bh,"[40m",0
ansi_bg_red			.db 1Bh,"[41m",0
ansi_bg_green		.db 1Bh,"[42m",0
ansi_bg_yellow		.db 1Bh,"[43m",0
ansi_bg_blue		.db 1Bh,"[44m",0
ansi_bg_magenta 	.db 1Bh,"[45m",0
ansi_bg_cyan		.db 1Bh,"[46m",0
ansi_bg_white		.db 1Bh,"[47m",0	
; String table
crlf				.db "\r\n",0
termbs				.db 08h,' ',08h,0
hello				.db "COOFMON-Z80 ROM Monitor",0
prompt				.db "coofmon> ",0
toolong				.db "Input buffer overflow.",0
err_bad_command		.db "ERR: Unknown command",0
err_hexarg_ne		.db "ERR: Argument required",0
err_hexarg_range	.db "ERR: Argument out of range",0
msg_ihx_badrec		.db "ERR: Unsupported Intel Hex record type",0
msg_ihx_shortrec	.db "ERR: Intel Hex record seems to be incomplete",0
msg_ihx_eof			.db "INFO: Intel Hex EOF record reached",0
help				.db "COOFMON-Z80 Commands\r\n"
					.db "d[ump] <hexaddr> <hexlen>\r\n"
					.db "	Dump <hexlen> bytes memory starting at <hexaddr>\r\n\r\n"
					.db "x[ecute] <hexaddr>\r\n"
					.db "	Jump to <hexaddr> and execute\r\n\r\n"
					.db "c[all] <hexaddr>\r\n"
					.db "	Call <hexaddr>\r\n\r\n"
					.db "f[ill] <hexaddr> <hexlen> <hexval>\r\n"
					.db "	Fill <hexlen> bytes starting at <hexaddr> with <hexval>\r\n\r\n"
					.db ":ihxstring\r\n"
					.db "	Load Intel hex string into memory\r\n\r\n"
					.db "w[rite] <hexval> <hexaddr>\r\n"
					.db "	Write <hexval> to memory location <hexaddr>\r\n\r\n"
					.db "i[nput] <hexport>\r\n"
					.db "	Read <hexport> and print the value (high order byte ignored)\r\n\r\n"
					.db "o[utput] <hexport> <hexval>\r\n"
					.db "	Write <hexval> to <hexport> (high order byte ignored)\r\n",0
; Character pattern table for use with an LCD or similar.
CHARTABLE:		.org $1000
Char_000		.db	$00, $00, $00, $00, $00, $00, $00, $00	; (.)
Char_001		.db	$7E, $81, $A5, $81, $BD, $99, $81, $7E	; (.)
Char_002		.db	$3C, $7E, $DB, $FF, $C3, $7E, $3C, $00	; (.)
Char_003		.db	$00, $EE, $FE, $FE, $7C, $38, $10, $00	; (.)
Char_004		.db	$10, $38, $7C, $FE, $7C, $38, $10, $00	; (.)
Char_005		.db	$00, $3C, $18, $FF, $FF, $08, $18, $00	; (.)
Char_006		.db	$10, $38, $7C, $FE, $FE, $10, $38, $00	; (.)
Char_007		.db	$00, $00, $18, $3C, $18, $00, $00, $00	; (.)
Char_008		.db	$FF, $FF, $E7, $C3, $E7, $FF, $FF, $FF	; (.)
Char_009		.db	$00, $3C, $42, $81, $81, $42, $3C, $00	; (.)
Char_010		.db	$FF, $C3, $BD, $7E, $7E, $BD, $C3, $FF	; (.)
Char_011		.db	$1F, $07, $0D, $7C, $C6, $C6, $7C, $00	; (.)
Char_012		.db	$00, $7E, $C3, $C3, $7E, $18, $7E, $18	; (.)
Char_013		.db	$04, $06, $07, $04, $04, $FC, $F8, $00	; (.)
Char_014		.db	$0C, $0A, $0D, $0B, $F9, $F9, $1F, $1F	; (.)
Char_015		.db	$00, $92, $7C, $44, $C6, $7C, $92, $00	; (.)
Char_016		.db	$00, $00, $60, $78, $7E, $78, $60, $00	; (.)
Char_017		.db	$00, $00, $06, $1E, $7E, $1E, $06, $00	; (.)
Char_018		.db	$18, $7E, $18, $18, $18, $18, $7E, $18	; (.)
Char_019		.db	$66, $66, $66, $66, $66, $00, $66, $00	; (.)
Char_020		.db	$FF, $B6, $76, $36, $36, $36, $36, $00	; (.)
Char_021		.db	$7E, $C1, $DC, $22, $22, $1F, $83, $7E	; (.)
Char_022		.db	$00, $00, $00, $7E, $7E, $00, $00, $00	; (.)
Char_023		.db	$18, $7E, $18, $18, $7E, $18, $00, $FF	; (.)
Char_024		.db	$18, $7E, $18, $18, $18, $18, $18, $00	; (.)
Char_025		.db	$18, $18, $18, $18, $18, $7E, $18, $00	; (.)
Char_026		.db	$00, $04, $06, $FF, $06, $04, $00, $00	; (.)
Char_027		.db	$00, $20, $60, $FF, $60, $20, $00, $00	; (.)
Char_028		.db	$00, $00, $00, $C0, $C0, $C0, $FF, $00	; (.)
Char_029		.db	$00, $24, $66, $FF, $66, $24, $00, $00	; (.)
Char_030		.db	$00, $00, $10, $38, $7C, $FE, $00, $00	; (.)
Char_031		.db	$00, $00, $00, $FE, $7C, $38, $10, $00	; (.)
Char_032		.db	$00, $00, $00, $00, $00, $00, $00, $00	; ( )
Char_033		.db	$30, $30, $30, $30, $30, $00, $30, $00	; (!)
Char_034		.db	$66, $66, $00, $00, $00, $00, $00, $00	; (")
Char_035		.db	$6C, $6C, $FE, $6C, $FE, $6C, $6C, $00	; (#)
Char_036		.db	$10, $7C, $D2, $7C, $86, $7C, $10, $00	; ($)
Char_037		.db	$F0, $96, $FC, $18, $3E, $72, $DE, $00	; (%)
Char_038		.db	$30, $48, $30, $78, $CE, $CC, $78, $00	; (&)
Char_039		.db	$0C, $0C, $18, $00, $00, $00, $00, $00	; (')
Char_040		.db	$10, $60, $C0, $C0, $C0, $60, $10, $00	; (()
Char_041		.db	$10, $0C, $06, $06, $06, $0C, $10, $00	; ())
Char_042		.db	$00, $54, $38, $FE, $38, $54, $00, $00	; (*)
Char_043		.db	$00, $18, $18, $7E, $18, $18, $00, $00	; (+)
Char_044		.db	$00, $00, $00, $00, $00, $00, $18, $70	; (,)
Char_045		.db	$00, $00, $00, $7E, $00, $00, $00, $00	; (-)
Char_046		.db	$00, $00, $00, $00, $00, $00, $18, $00	; (.)
Char_047		.db	$02, $06, $0C, $18, $30, $60, $C0, $00	; (/)
Char_048		.db	$7C, $CE, $DE, $F6, $E6, $E6, $7C, $00	; (0)
Char_049		.db	$18, $38, $78, $18, $18, $18, $3C, $00	; (1)
Char_050		.db	$7C, $C6, $06, $0C, $30, $60, $FE, $00	; (2)
Char_051		.db	$7C, $C6, $06, $3C, $06, $C6, $7C, $00	; (3)
Char_052		.db	$0E, $1E, $36, $66, $FE, $06, $06, $00	; (4)
Char_053		.db	$FE, $C0, $C0, $FC, $06, $06, $FC, $00	; (5)
Char_054		.db	$7C, $C6, $C0, $FC, $C6, $C6, $7C, $00	; (6)
Char_055		.db	$FE, $06, $0C, $18, $30, $60, $60, $00	; (7)
Char_056		.db	$7C, $C6, $C6, $7C, $C6, $C6, $7C, $00	; (8)
Char_057		.db	$7C, $C6, $C6, $7E, $06, $C6, $7C, $00	; (9)
Char_058		.db	$00, $30, $00, $00, $00, $30, $00, $00	; (:)
Char_059		.db	$00, $30, $00, $00, $00, $30, $20, $00	; (;)
Char_060		.db	$00, $1C, $30, $60, $30, $1C, $00, $00	; (<)
Char_061		.db	$00, $00, $7E, $00, $7E, $00, $00, $00	; (=)
Char_062		.db	$00, $70, $18, $0C, $18, $70, $00, $00	; (>)
Char_063		.db	$7C, $C6, $0C, $18, $30, $00, $30, $00	; (?)
Char_064		.db	$7C, $82, $9A, $AA, $AA, $9E, $7C, $00	; (@)
Char_065		.db	$7C, $C6, $C6, $FE, $C6, $C6, $C6, $00	; (A)
Char_066		.db	$FC, $66, $66, $7C, $66, $66, $FC, $00	; (B)
Char_067		.db	$7C, $C6, $C0, $C0, $C0, $C6, $7C, $00	; (C)
Char_068		.db	$FC, $66, $66, $66, $66, $66, $FC, $00	; (D)
Char_069		.db	$FE, $62, $68, $78, $68, $62, $FE, $00	; (E)
Char_070		.db	$FE, $62, $68, $78, $68, $60, $F0, $00	; (F)
Char_071		.db	$7C, $C6, $C6, $C0, $DE, $C6, $7C, $00	; (G)
Char_072		.db	$C6, $C6, $C6, $FE, $C6, $C6, $C6, $00	; (H)
Char_073		.db	$3C, $18, $18, $18, $18, $18, $3C, $00	; (I)
Char_074		.db	$1E, $0C, $0C, $0C, $0C, $CC, $78, $00	; (J)
Char_075		.db	$C6, $CC, $D8, $F0, $D8, $CC, $C6, $00	; (K)
Char_076		.db	$F0, $60, $60, $60, $60, $62, $FE, $00	; (L)
Char_077		.db	$C6, $EE, $FE, $D6, $C6, $C6, $C6, $00	; (M)
Char_078		.db	$C6, $E6, $F6, $DE, $CE, $C6, $C6, $00	; (N)
Char_079		.db	$7C, $C6, $C6, $C6, $C6, $C6, $7C, $00	; (O)
Char_080		.db	$FC, $66, $66, $7C, $60, $60, $F0, $00	; (P)
Char_081		.db	$7C, $C6, $C6, $C6, $C6, $C6, $7C, $0C	; (Q)
Char_082		.db	$FC, $66, $66, $7C, $66, $66, $E6, $00	; (R)
Char_083		.db	$7C, $C6, $C0, $7C, $06, $C6, $7C, $00	; (S)
Char_084		.db	$7E, $5A, $18, $18, $18, $18, $3C, $00	; (T)
Char_085		.db	$C6, $C6, $C6, $C6, $C6, $C6, $7C, $00	; (U)
Char_086		.db	$C6, $C6, $C6, $C6, $C6, $6C, $38, $00	; (V)
Char_087		.db	$C6, $C6, $C6, $C6, $D6, $EE, $C6, $00	; (W)
Char_088		.db	$C6, $6C, $38, $38, $38, $6C, $C6, $00	; (X)
Char_089		.db	$66, $66, $66, $3C, $18, $18, $3C, $00	; (Y)
Char_090		.db	$FE, $C6, $0C, $18, $30, $66, $FE, $00	; (Z)
Char_091		.db	$1C, $18, $18, $18, $18, $18, $1C, $00	; ([)
Char_092		.db	$C0, $60, $30, $18, $0C, $06, $02, $00	; (\)
Char_093		.db	$70, $30, $30, $30, $30, $30, $70, $00	; (])
Char_094		.db	$00, $00, $10, $38, $6C, $C6, $00, $00	; (^)
Char_095		.db	$00, $00, $00, $00, $00, $00, $00, $FF	; (_)
Char_096		.db	$30, $30, $18, $00, $00, $00, $00, $00	; (`)
Char_097		.db	$00, $00, $7C, $06, $7E, $C6, $7E, $00	; (a)
Char_098		.db	$C0, $C0, $FC, $C6, $C6, $C6, $FC, $00	; (b)
Char_099		.db	$00, $00, $7C, $C6, $C0, $C6, $7C, $00	; (c)
Char_100		.db	$06, $06, $7E, $C6, $C6, $C6, $7E, $00	; (d)
Char_101		.db	$00, $00, $7C, $C6, $FE, $C0, $7C, $00	; (e)
Char_102		.db	$3C, $66, $60, $F0, $60, $60, $60, $00	; (f)
Char_103		.db	$00, $00, $7E, $C6, $C6, $7E, $06, $7C	; (g)
Char_104		.db	$C0, $C0, $FC, $C6, $C6, $C6, $C6, $00	; (h)
Char_105		.db	$18, $00, $38, $18, $18, $18, $3C, $00	; (i)
Char_106		.db	$00, $0C, $00, $1C, $0C, $0C, $CC, $78	; (j)
Char_107		.db	$C0, $C0, $C6, $D8, $F0, $D8, $C6, $00	; (k)
Char_108		.db	$38, $18, $18, $18, $18, $18, $3C, $00	; (l)
Char_109		.db	$00, $00, $EE, $FE, $D6, $C6, $C6, $00	; (m)
Char_110		.db	$00, $00, $FC, $C6, $C6, $C6, $C6, $00	; (n)
Char_111		.db	$00, $00, $7C, $C6, $C6, $C6, $7C, $00	; (o)
Char_112		.db	$00, $00, $FC, $C6, $C6, $FC, $C0, $C0	; (p)
Char_113		.db	$00, $00, $7E, $C6, $C6, $7E, $06, $06	; (q)
Char_114		.db	$00, $00, $DE, $76, $60, $60, $60, $00	; (r)
Char_115		.db	$00, $00, $7C, $C0, $7C, $06, $7C, $00	; (s)
Char_116		.db	$18, $18, $7E, $18, $18, $18, $1E, $00	; (t)
Char_117		.db	$00, $00, $C6, $C6, $C6, $C6, $7E, $00	; (u)
Char_118		.db	$00, $00, $C6, $C6, $C6, $6C, $38, $00	; (v)
Char_119		.db	$00, $00, $C6, $C6, $D6, $FE, $C6, $00	; (w)
Char_120		.db	$00, $00, $C6, $6C, $38, $6C, $C6, $00	; (x)
Char_121		.db	$00, $00, $C6, $C6, $C6, $7E, $06, $7C	; (y)
Char_122		.db	$00, $00, $FE, $0C, $18, $60, $FE, $00	; (z)
Char_123		.db	$0E, $18, $18, $70, $18, $18, $0E, $00	; ({)
Char_124		.db	$18, $18, $18, $00, $18, $18, $18, $00	; (|)
Char_125		.db	$E0, $30, $30, $1C, $30, $30, $E0, $00	; (})
Char_126		.db	$00, $00, $70, $9A, $0E, $00, $00, $00	; (~)
Char_127		.db	$00, $00, $18, $3C, $66, $FF, $00, $00	; (.)
Char_128		.db	$7C, $C6, $C0, $C0, $C6, $7C, $18, $70	; (.)
Char_129		.db	$66, $00, $C6, $C6, $C6, $C6, $7E, $00	; (.)
Char_130		.db	$0E, $18, $7C, $C6, $FE, $C0, $7C, $00	; (.)
Char_131		.db	$18, $24, $7C, $06, $7E, $C6, $7E, $00	; (.)
Char_132		.db	$66, $00, $7C, $06, $7E, $C6, $7E, $00	; (.)
Char_133		.db	$38, $0C, $7C, $06, $7E, $C6, $7E, $00	; (.)
Char_134		.db	$18, $00, $7C, $06, $7E, $C6, $7E, $00	; (.)
Char_135		.db	$00, $00, $7C, $C0, $C0, $7C, $18, $70	; (.)
Char_136		.db	$18, $24, $7C, $C6, $FE, $C0, $7C, $00	; (.)
Char_137		.db	$66, $00, $7C, $C6, $FE, $C0, $7C, $00	; (.)
Char_138		.db	$70, $18, $7C, $C6, $FE, $C0, $7C, $00	; (.)
Char_139		.db	$66, $00, $38, $18, $18, $18, $3C, $00	; (.)
Char_140		.db	$18, $24, $38, $18, $18, $18, $3C, $00	; (.)
Char_141		.db	$38, $0C, $38, $18, $18, $18, $3C, $00	; (.)
Char_142		.db	$66, $00, $7C, $C6, $FE, $C6, $C6, $00	; (.)
Char_143		.db	$18, $00, $7C, $C6, $FE, $C6, $C6, $00	; (.)
Char_144		.db	$0E, $18, $FE, $60, $78, $60, $FE, $00	; (.)
Char_145		.db	$00, $00, $7C, $1A, $7E, $D8, $7E, $00	; (.)
Char_146		.db	$7E, $D8, $D8, $DE, $F8, $D8, $DE, $00	; (.)
Char_147		.db	$18, $24, $7C, $C6, $C6, $C6, $7C, $00	; (.)
Char_148		.db	$66, $00, $7C, $C6, $C6, $C6, $7C, $00	; (.)
Char_149		.db	$38, $0C, $7C, $C6, $C6, $C6, $7C, $00	; (.)
Char_150		.db	$18, $24, $C6, $C6, $C6, $C6, $7E, $00	; (.)
Char_151		.db	$38, $0C, $C6, $C6, $C6, $C6, $7E, $00	; (.)
Char_152		.db	$66, $00, $C6, $C6, $C6, $7E, $06, $7C	; (.)
Char_153		.db	$66, $7C, $C6, $C6, $C6, $C6, $7C, $00	; (.)
Char_154		.db	$C6, $00, $C6, $C6, $C6, $C6, $7C, $00	; (.)
Char_155		.db	$18, $7C, $C6, $C0, $C6, $7C, $18, $00	; (.)
Char_156		.db	$1E, $32, $30, $78, $30, $70, $FE, $00	; (.)
Char_157		.db	$66, $3C, $18, $7E, $18, $3C, $18, $00	; (.)
Char_158		.db	$FC, $C6, $FC, $C0, $CC, $DE, $CC, $0E	; (.)
Char_159		.db	$00, $1C, $32, $30, $FC, $30, $F0, $00	; (.)
Char_160		.db	$0E, $18, $7C, $06, $7E, $C6, $7E, $00	; (.)
Char_161		.db	$1A, $30, $38, $18, $18, $18, $3C, $00	; (.)
Char_162		.db	$0E, $18, $7C, $C6, $C6, $C6, $7C, $00	; (.)
Char_163		.db	$0E, $18, $C6, $C6, $C6, $C6, $7E, $00	; (.)
Char_164		.db	$66, $98, $FC, $C6, $C6, $C6, $C6, $00	; (.)
Char_165		.db	$66, $98, $E6, $F6, $DE, $CE, $C6, $00	; (.)
Char_166		.db	$7C, $06, $7E, $C6, $7E, $00, $FE, $00	; (.)
Char_167		.db	$7C, $C6, $C6, $C6, $7C, $00, $FE, $00	; (.)
Char_168		.db	$18, $00, $18, $30, $60, $C6, $7C, $00	; (.)
Char_169		.db	$00, $00, $FE, $C0, $C0, $C0, $C0, $00	; (.)
Char_170		.db	$00, $00, $FE, $06, $06, $06, $06, $00	; (.)
Char_171		.db	$C0, $C0, $C0, $DE, $06, $0C, $1E, $00	; (.)
Char_172		.db	$C0, $C0, $C0, $CC, $1C, $3E, $0C, $00	; (.)
Char_173		.db	$30, $00, $30, $30, $30, $30, $30, $00	; (.)
Char_174		.db	$00, $36, $6C, $D8, $6C, $36, $00, $00	; (.)
Char_175		.db	$00, $D8, $6C, $36, $6C, $D8, $00, $00	; (.)
Char_176		.db	$AA, $AA, $AA, $AA, $AA, $AA, $AA, $AA	; (.)
Char_177		.db	$AA, $55, $AA, $55, $AA, $55, $AA, $55	; (.)
Char_178		.db	$44, $22, $44, $22, $44, $22, $44, $22	; (.)
Char_179		.db	$18, $18, $18, $18, $18, $18, $18, $18	; (.)
Char_180		.db	$18, $18, $18, $F8, $18, $18, $18, $18	; (.)
Char_181		.db	$18, $18, $18, $F8, $18, $F8, $18, $18	; (.)
Char_182		.db	$36, $36, $36, $F6, $36, $36, $36, $36	; (.)
Char_183		.db	$00, $00, $00, $FE, $36, $36, $36, $36	; (.)
Char_184		.db	$00, $00, $00, $F8, $18, $F8, $18, $18	; (.)
Char_185		.db	$36, $36, $36, $F6, $06, $F6, $36, $36	; (.)
Char_186		.db	$36, $36, $36, $36, $36, $36, $36, $36	; (.)
Char_187		.db	$00, $00, $00, $FE, $06, $F6, $36, $36	; (.)
Char_188		.db	$36, $36, $36, $F6, $06, $FE, $00, $00	; (.)
Char_189		.db	$36, $36, $36, $FE, $00, $00, $00, $00	; (.)
Char_190		.db	$18, $18, $18, $F8, $18, $F8, $00, $00	; (.)
Char_191		.db	$00, $00, $00, $F8, $18, $18, $18, $18	; (.)
Char_192		.db	$18, $18, $18, $1F, $00, $00, $00, $00	; (.)
Char_193		.db	$18, $18, $18, $FF, $00, $00, $00, $00	; (.)
Char_194		.db	$00, $00, $00, $FF, $18, $18, $18, $18	; (.)
Char_195		.db	$18, $18, $18, $1F, $18, $18, $18, $18	; (.)
Char_196		.db	$00, $00, $00, $FF, $00, $00, $00, $00	; (.)
Char_197		.db	$18, $18, $18, $FF, $18, $18, $18, $18	; (.)
Char_198		.db	$18, $18, $18, $1F, $18, $1F, $18, $18	; (.)
Char_199		.db	$36, $36, $36, $37, $36, $36, $36, $36	; (.)
Char_200		.db	$36, $36, $36, $37, $30, $3F, $00, $00	; (.)
Char_201		.db	$00, $00, $00, $3F, $30, $37, $36, $36	; (.)
Char_202		.db	$36, $36, $36, $F7, $00, $FF, $00, $00	; (.)
Char_203		.db	$00, $00, $00, $FF, $00, $F7, $36, $36	; (.)
Char_204		.db	$36, $36, $36, $37, $30, $37, $36, $36	; (.)
Char_205		.db	$00, $00, $00, $FF, $00, $FF, $00, $00	; (.)
Char_206		.db	$36, $36, $36, $F7, $00, $F7, $36, $36	; (.)
Char_207		.db	$18, $18, $18, $FF, $00, $FF, $00, $00	; (.)
Char_208		.db	$36, $36, $36, $FF, $00, $00, $00, $00	; (.)
Char_209		.db	$00, $00, $00, $FF, $00, $FF, $18, $18	; (.)
Char_210		.db	$00, $00, $00, $FF, $36, $36, $36, $36	; (.)
Char_211		.db	$36, $36, $36, $3F, $00, $00, $00, $00	; (.)
Char_212		.db	$18, $18, $18, $1F, $18, $1F, $00, $00	; (.)
Char_213		.db	$00, $00, $00, $1F, $18, $1F, $18, $18	; (.)
Char_214		.db	$00, $00, $00, $3F, $36, $36, $36, $36	; (.)
Char_215		.db	$36, $36, $36, $FF, $36, $36, $36, $36	; (.)
Char_216		.db	$18, $18, $18, $FF, $18, $FF, $18, $18	; (.)
Char_217		.db	$18, $18, $18, $F8, $00, $00, $00, $00	; (.)
Char_218		.db	$00, $00, $00, $1F, $18, $18, $18, $18	; (.)
Char_219		.db	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF	; (.)
Char_220		.db	$00, $00, $00, $00, $FF, $FF, $FF, $FF	; (.)
Char_221		.db	$F0, $F0, $F0, $F0, $F0, $F0, $F0, $F0	; (.)
Char_222		.db	$0F, $0F, $0F, $0F, $0F, $0F, $0F, $0F	; (.)
Char_223		.db	$FF, $FF, $FF, $FF, $00, $00, $00, $00	; (.)
Char_224		.db	$00, $00, $77, $98, $98, $77, $00, $00	; (.)
Char_225		.db	$1C, $36, $66, $FC, $C6, $C6, $FC, $C0	; (.)
Char_226		.db	$FE, $62, $60, $60, $60, $60, $60, $00	; (.)
Char_227		.db	$00, $00, $FF, $66, $66, $66, $66, $00	; (.)
Char_228		.db	$FE, $62, $30, $18, $30, $62, $FE, $00	; (.)
Char_229		.db	$00, $00, $3F, $66, $C6, $CC, $78, $00	; (.)
Char_230		.db	$00, $00, $33, $33, $33, $3E, $30, $F0	; (.)
Char_231		.db	$00, $00, $FF, $18, $18, $18, $18, $00	; (.)
Char_232		.db	$3C, $18, $3C, $66, $66, $3C, $18, $3C	; (.)
Char_233		.db	$00, $7C, $C6, $FE, $C6, $7C, $00, $00	; (.)
Char_234		.db	$00, $7E, $C3, $C3, $C3, $66, $E7, $00	; (.)
Char_235		.db	$1E, $19, $3C, $66, $C6, $CC, $78, $00	; (.)
Char_236		.db	$00, $00, $66, $99, $99, $66, $00, $00	; (.)
Char_237		.db	$00, $03, $7C, $CE, $E6, $7C, $C0, $00	; (.)
Char_238		.db	$00, $3E, $C0, $FE, $C0, $3E, $00, $00	; (.)
Char_239		.db	$00, $7E, $C3, $C3, $C3, $C3, $00, $00	; (.)
Char_240		.db	$00, $FE, $00, $FE, $00, $FE, $00, $00	; (.)
Char_241		.db	$18, $18, $7E, $18, $18, $7E, $00, $00	; (.)
Char_242		.db	$70, $18, $0C, $18, $70, $00, $FE, $00	; (.)
Char_243		.db	$1C, $30, $60, $30, $1C, $00, $FE, $00	; (.)
Char_244		.db	$00, $0E, $1B, $18, $18, $18, $18, $18	; (.)
Char_245		.db	$18, $18, $18, $18, $18, $D8, $70, $00	; (.)
Char_246		.db	$00, $18, $00, $7E, $00, $18, $00, $00	; (.)
Char_247		.db	$00, $76, $DC, $00, $76, $DC, $00, $00	; (.)
Char_248		.db	$3C, $66, $3C, $00, $00, $00, $00, $00	; (.)
Char_249		.db	$00, $18, $3C, $18, $00, $00, $00, $00	; (.)
Char_250		.db	$00, $00, $00, $00, $18, $00, $00, $00	; (.)
Char_251		.db	$0F, $0C, $0C, $0C, $EC, $6C, $38, $00	; (.)
Char_252		.db	$D8, $EC, $CC, $CC, $00, $00, $00, $00	; (.)
Char_253		.db	$F0, $30, $C0, $F0, $00, $00, $00, $00	; (.)
Char_254		.db	$00, $00, $00, $3C, $3C, $3C, $3C, $00	; (.)
Char_255		.db	$00, $00, $00, $00, $00, $00, $00, $00	; (.)
.end