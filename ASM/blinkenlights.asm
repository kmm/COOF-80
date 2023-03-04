; -----------------------------------------------------------------------------
; Ping-pong blinkenlight scroll example (Z80)
; Illuminates the LEDs of the blinkenlights port in sequence, back and forth, forever.
; Great for automotive grille lighting effects in 1980s TV shows.
; kmm 2020, public domain
; -----------------------------------------------------------------------------

; -----------------------------------------------------------------------------
; Macro Parameters
; These are similar to C #defines, in that they're symbolic representations
; of constants and make the code easier to read/modify. The Z80 will never
; see "STACKTOP", just the 0x2040 that gets inserted by the assembler in its place.
; -----------------------------------------------------------------------------
STACKTOP 	.equ 2040h			; RAM starts at 0x2000, put the stack 64 bytes above
BLPORTADDR	.equ 40h			; Blinkenlights are latched by an IO write with A6 active
DELAYTIME	.equ 24000			; Sets the speed of the scroll, at 4.6MHz this is around 250ms to my eye
DIRECTION	.equ STACKTOP + 1	; Memory location to store the current direction.
								; The Z80 stack grows downwards so we can put this right above it in memory.

; -----------------------------------------------------------------------------
; Initialization
; -----------------------------------------------------------------------------
start .org 0000h				; Locate this code at the reset vector address, 0x0000
	ld sp, STACKTOP				; Set up the stack pointer by loading STACKTOP into the SP register
	ld a, 0						; Load 0 in A to initialize the direction value with
	ld (DIRECTION), a			; Save it to memory at the location defined by DIRECTION (0x2041)
	ld e, 1						; E will be where we store the bit pattern, set a 1 a the LSB position
	ld a, 00ffh					; Lamp test at start up, illuminate all the blinkenlights
	out (BLPORTADDR), a			; Write it to the blinkenlights port
	ld bc, 48000				; Load delay value into BC, the delay subroutine will pick this up
	call delay					; Call the delay subroutine to wait a little bit with all the LEDs illuminated

; -----------------------------------------------------------------------------
; Main Loop
; -----------------------------------------------------------------------------
loop:							; This is a label, another assembler convenience.
	ld a, e						; Load E into A
	out (BLPORTADDR), a			; Write it to the blinkenlatch
	ld a, (DIRECTION)			; Load the value of DIRECTION into A
	cp 0						; Is it zero?
	jr nz, roll_down			; If not jump to roll_down
roll_up:						; Otherwise, roll_up
	rlc e						; Rotate the bits in E left
	ld bc, DELAYTIME			; Wait for it
	call delay					; ...
	ld a, e						; Load E into A
	cp 80h						; Is bit 7 (MSB) in A set?
	jr nz, loop					; If not, start over and continue rotating in this direction
	ld a, 1						; If so, set the DIRECTION flag to 1
	ld (DIRECTION), a			; ...
	jr loop						; And continue from the top
roll_down:						; We get here if DIRECTION == 1
	rrc e						; Rotate the bits in E right
	ld bc, DELAYTIME			; Wait for it
	call delay					; ...
	ld a, e						; Load E into A
	cp 1h						; Is bit 0 in A set?
	jr nz, loop					; If not, continue rotating in this direction
	ld a, 0						; If so, set the DIRECTION flag to 0
	ld (DIRECTION), a			; ...
	jr loop						; And continue from the top

; -----------------------------------------------------------------------------
; Delay Subroutine
; Load number of loops (16 bit, up to 65,535) to delay into BC and call this.
; -----------------------------------------------------------------------------
delay:
	dec bc						; Decrement BC
	ld a, b						; Load B into A
	or c						; OR it with C, if the result is zero the 'or' instruction sets the Z flag
	jr nz, delay				; If it's not zero loop back to the start of the subroutine
	ret							; If so, we're done delaying
.end