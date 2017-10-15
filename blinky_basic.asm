;blinky basic
; Copywrite 2015, Bob Burns
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
;  (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;

;avrdude -v -p atmega328p -c arduino -P /dev/tty.usbserial-ADAOFCP1s -b 115200 -D -U flash:w:blinkyBasic.hex:i
;---- defines ----
.device atmega328p
;
.equ MSG_FLG = 0
.equ RD_FLG = 1
;
.equ SPI_PORT = 0X05
.equ SPI_DDR = 0x04
.equ SPI_PIN = 0x03     ; I/O registers port B
.equ S_MISO = 4
.equ S_SS = 2
.equ S_MOSI = 3
.equ S_SCK = 5          ; port b pins
;
.equ CS_PORT = 0x08 	;move chip select on SRAM to port C
.equ CS_DDR = 0x07
.equ SRM_CS = 5
;
.equ UBBRvalue = 103     ;to init usart, based on cpu speed 1000000
;
.equ PAGEMAX = 0x40     ;maximum write length
.equ W_INP = 0          ;eeprom sreg flag bits ** write in progress
.equ W_ENL = 1          ;                       * write enable latch
.equ BP0 = 2            ;                       * block protect 0
.equ BP1 = 1            ;                       * block protect 1
;**** opcodes for eeprom ****
.equ E_READ = 0b00000011
.equ E_WRITE = 0b00000010
.equ E_WRDI = 0b00000100        ;write disable
.equ E_WREN = 0b00000110        ;write enable
.equ E_RDSR = 0b00000101        ;read status register
.equ E_WRSR = 0b00000001        ;write status register
;
;**** opcodes for SRAM ****
.equ S_READ = 0b00000011
.equ S_WRITE = 0b00000010
.equ S_RDSR = 0b00000101
.equ S_WRSR = 0b00000001
;
.equ MYSTACK = 0x8ff
;*** delay constant
.equ dlp_init = 50000
;***** flags ******
.equ cmd_ln = 1
.equ err = 2
.equ frnxt = 3
.equ gsb = 4
.equ sgn = 5
.equ eol = 6
.equ str = 7
;---- registers
.def e_lndx = r15
.def temp = r16
.def count = r17
.def temp2 = r18
.def byte_trx = r19
.def tuk = r23
.def tempL = r24
.def tempH = r25

.def flags = r20                ;used to keep track of if there is a message in the in buffer
.def delayL = r24
.def delayH = r25
.def xL = r26
.def xH = r27
.def yL = r28
.def yH = r29
.def zL = r30
.def zH = r31

.equ maxcmd = 9
.equ maxstm = 50
.cseg
;---- Interrupt Vector ---
.org 0
        rjmp    reset
        rjmp    RDYN_L                  ;INT0 vector
.org 0x24
	rjmp	h_kint
h_kint:
	push 	temp
	push	xL
	push	xH
        lds     temp,UDR0;
	cpi	temp, 0x03
	breq	hk10
	ldi	byte_trx, 0x23
	rcall	transmit
	pop	xH
	pop	xL
	pop	temp
	reti
hk10:
	sbr	flags, (1 << err)
	ldi	xL, low(errno)
	ldi	xH, high(errno)
	ldi	temp, 0xFA
	st	X, temp
	pop	xH
	pop	xL
	pop 	temp
	reti
RDYN_L:
	nop
reset:
	;Initialize Stack
        ldi     temp,low(MYSTACK)
        out     SPL,temp
        ldi     temp,high(MYSTACK)
        out     SPH,temp
;initialize USART
        ldi     temp,high(UBBRvalue)    ;baud rate param
        sts     UBRR0H,temp
        ldi     temp,low(UBBRvalue)
        sts     UBRR0L,temp
;        lds     temp,UCSR0A
;        ori     temp,(1 << U2X0)        ;set use 2x because %error actual baud > .5
;        sts     UCSR0A,temp
;--- USART register values
        ldi     temp,(1 << TXEN0) | (1 << RXEN0) ;enable transmit and receive
        sts     UCSR0B,temp
        ldi     temp,(1 << UCSZ01) | (1 << UCSZ00) ;8 data bits, 1 stop bit
        sts     UCSR0C,temp
;Initialize SPI
        sbi     SPI_DDR,S_SS            ;output on slave select
        sbi     SPI_PORT,S_SS           ;start off not selected (high) ** always enable pullup before configuring SPI
	sbi	CS_DDR, SRM_CS
	sbi	CS_PORT, SRM_CS		;SRAM chip select on analog 5(arduino)
        sbi     SPI_DDR,S_MOSI          ;output on MOSI
        sbi     SPI_PORT,S_MISO         ;set pullup on MISO
        sbi     SPI_DDR,S_SCK           ;output on SCK
;
;** don't have to set phase, polarity b/c default works with 25LCxxx chips
; ** same with 24K256 SRAM **
        in      temp,SPCR
        sbr     temp,(1 << SPR0)        ;divide f_cpu by 16, better for breadboards
        sbr     temp,(1 << MSTR)        ;set AVR to SPI Master mode (use its clock)
        sbr     temp,(1 << SPE)         ;enable SPI
        out     SPCR,temp
	cbi	CS_PORT, SRM_CS
	sbi	CS_PORT, SRM_CS
;
;----- entry into main program -------
	;rcall	cold
	;rcall	warm
cold:
;init basic variables, pointers
	clr	flags
	clr	e_lndx
	ldi 	xL, low(himem)
	ldi 	xH, high(himem)
	ldi 	temp, 0xff
	st	X+, temp	;low byte first
	ldi 	temp, 0x1f
	st	X, temp
	ldi 	xL,low(lomem)
	ldi	xH, high(lomem)
	ldi 	temp, 0x00
	st	X+, temp
	st	X, temp
	ldi	xL, low(curline)
	ldi	xH, high(curline)
	st	X+, temp
	ldi	temp, 0x01
	st	X, temp
	ldi	xL, low(curline)
	ldi	xH, high(curline)
	ld	yL, X+
	ld	yH, X
	ldi	temp, 0x00
	st	Y+, temp
	st	y+, temp
	ldi	temp, 0x06
	st	Y+, temp
	ldi	temp, 0x01
	st	Y+, temp
	ldi	xL, low(lastline)
	ldi	xH, high(lastline)
	ldi	temp, 0x06
	mov	yL, temp
	st	X+, temp
	ldi	temp, 0x01		;last line ptr 106
	mov	yH, temp
	st	X+, temp
	ldi	temp, 0xFF
	st	Y+, temp
	st	Y, temp			;init value at lastline to ffff
	ldi	xL, low(frnxtndx)
	ldi	xH, high(frnxtndx)
	ldi	temp, 0
	st	X, temp
	ldi	XL, low(gsubndx)
	ldi	xH, high(gsubndx)
	st	X, temp
	ldi	xL, low(pstack)
	ldi	xH, high(pstack)
	ldi	temp, 0x63
	st	X+, temp
	ldi	temp, 0x05
	st	X+, temp
	ldi	xL, low(epp)
	ldi	xH, high(epp)
	ldi	temp, 0x00
	st	X+, temp
	st	X, temp
	ldi	xL, low(inttabp)
	ldi	xH, high(inttabp)
	ldi	temp, 0x69
	st	X+, temp
	ldi	temp, 0x05
	st	X, temp
	ldi	xL, low(intnxte)
	ldi	xH, high(intnxte)
	ldi	temp, 0x00
	st	X+, temp
	ldi	temp, 0x50
	st	X, temp
	ldi	xL, low(strtabp)
	ldi	xH, high(strtabp)
	ldi	temp, 0x23
	st	X+, temp
	ldi	temp, 0x06
	st	X, temp
	ldi	xL, low(strnxte)
	ldi	xH, high(strnxte)
	ldi	temp, 0x00
	st 	X+, temp
	ldi	temp, 0x30
	st	X, temp
	ldi	xL, low(txtndx)
	ldi	xH, high(txtndx)
	ldi	temp, 0x00
	st	X, temp
	ldi	xL, low(intmax)
	ldi	xH, high(intmax)
	st	X, temp
	ldi	xL, low(strmax)
	ldi	xH, high(strmax)
	st	X, temp
	ldi	xL, low(eqp)
	ldi	xH, high(eqp)
	st	X, temp
	ldi	xL, low(dimptr)
	ldi	xH, high(dimptr)
	ldi	temp, 0xFF
	st	X+, temp
	ldi	temp, 0x7F
	st	X, temp
; etc...
warm:
	rjmp getline

;----- subroutines ------
receive:        ; needs byte_tx defined by caller
;	sbis	UCSR0A, RXC0
;	rjmp	receive
;	in	byte_trx, UDR0
;	ret
        lds     temp,UCSR0A
        sbrs    temp,RXC0       ;is byte in rx buffer?
        rjmp	receive                     ;not yet
        lds     byte_trx,UDR0;
        ret
transmit:
;	sbis	UCSR0A, UDRE0
;	rjmp	transmit
;	out	UDR0, byte_trx
;	ret
        lds     temp,UCSR0A
        sbrs    temp,UDRE0      ;wait for Tx buffer to be empty
        rjmp    transmit        ;not ready
        sts     UDR0,byte_trx;
        ret
;------ getline ----------
getline:
	cli
	clr	e_lndx
	ldi	xL, low(txtndx)
	ldi	xH, high(txtndx)
	ldi	temp, 0
	st	X, temp
	ldi	xL, low(in_buf)
	ldi	xH, high(in_buf)
	ldi	count, 0
;	ldi	byte_trx, 'b'
;	rcall	transmit
	ldi	byte_trx, ']'
	rcall 	transmit
getchar:
	rcall	receive
	cpi	byte_trx, 0x08		;backspace
	breq	getline_bs
	st	X+, byte_trx
	cpi	byte_trx, 0x0d		;CR
	breq	getline_done
	rcall	transmit
	inc	count
	cpi	count, 173
	brlo	getchar	
	rjmp	getline
getline_bs:
	cpi	count, 1
	brsh	gb_nxt
	ldi	byte_trx, 0x07
	rcall	transmit
	rjmp	getchar
gb_nxt:
	dec 	count
	subi	xL, 1
	brcc	gb_nocarry
	dec	xH
gb_nocarry:
	ldi	byte_trx, 0x08
	rcall	transmit		;just echo dont store
	rjmp	getchar
getline_done:
	rcall	receive			;get last byte 0x0a
	ldi	byte_trx, 0x0d
	rcall	transmit
	ldi	byte_trx, 0x0a
	rcall	transmit
	ldi	xL, low(in_buf)
	ldi	xH, high(in_buf)
	ldi	count, 4
	ld	temp2, X
	cpi	temp2, 0x0d
	brne	get_nxt
	rjmp	getline
get_nxt:
	cpi	temp2, '9' + 1
	brsh	getline_cmd
	subi	temp2, '0'
	brcs	getline_cmd
	rjmp 	tokenize
getline_cmd:
	rcall	cmp_cmd
	sbrc	flags, err
	rjmp 	handle_error
	sbr	flags,(1 << cmd_ln)
	rcall	run_cmd
	rjmp	getline
tokenize:
;todo
	rcall	a2u
	sbrc	flags, err
	rjmp 	handle_error
	rcall	fndline
tok_loop:
	rcall	nxt_char
	sbrc	flags, err
	rjmp	handle_error
	sbrc	flags, eol
	rjmp	tok_done
	rcall	cmp_num
	brcc	tok_nxt
	clc
	rjmp	tok_loop
tok_nxt:
	rcall	cmp_stm
	rjmp	tok_loop
	rjmp	getline
tok_done:
;todo update pointers
	cbr	flags, (1 << eol)
	ldi	temp, 0x7F
	rcall	cpst_store
	rjmp	getline
	
nxt_char:
	clr 	count
	ldi	xL, low(in_buf)
	ldi	xH, high(in_buf)
	ldi	yL, low(txtndx)
	ldi	yH, high(txtndx)
	ld	temp, Y
	add	xL, temp
	brcc	nxtchr_nc
	inc	xH
nxtchr_nc:
	ld	temp, X+
	cpi	temp, 0x20		;discard whitespace
	breq	nxtchr_no
	cpi	temp, 0x09		;tab
	breq	nxtchr_no
	cpi	temp, 0x0d
	brne	nxtchr_done
	ori	flags, (1 << eol)
nxtchr_done:
	ldi	xL, low(txtndx)
	ldi	xH, high(txtndx)
	ld	temp, X
	add	temp, count
	st	X, temp
	ret
nxtchr_no:
	inc	count
	cpi	count, 0x64
	brsh	nxtchr_err
	rjmp	nxtchr_nc
nxtchr_err:
	sbr	flags, (1 << err)
	ldi	temp, 0xFF
	ldi	xL, low(errno)
	ldi	xH, high(errno)
	st	X, temp
	ret
;copmare number routine
;checks in-buf for number and stores value as token with high bit set
cmp_num:
	clr	count
	ldi	xL, low(txtndx)
	ldi	xH, high(txtndx)
	ld	temp, X
	ldi	xL, low(in_buf)
	ldi	xH, high(in_buf)
	add	xL, temp
	brcc	cmpn_cc
	inc	xH
cmpn_cc:	
	ld	temp, X+
	cpi 	temp, '9' + 1
	brsh	cmpn_done
	subi	temp, '0'
	brcs	cmpn_done
	inc 	count
	ori	temp, 0x80		;set high bit
	rcall	cpst_store
	ldi	xL, low(txtndx)
	ldi	xH, high(txtndx)
	ld	temp, X
	add	temp, count
	st 	X, temp
	sec
	ret
cmpn_done:
	ldi	xL, low(txtndx)
	ldi	xH, high(txtndx)
	ld	temp, X
	add	temp, count
;todo handle error for txtndx over
	st 	X, temp
	clc
	ret


;todo parse statements or variable assignments
.def char = r2
.def char2 = r3
.def index = r10
.def index2 = r21
.def up_txt = r22
	clr	index
	clr	index2
	clr	temp2
cmp_stm:
	clr	index
	clr	index2
	ldi	temp2, 0
	ldi	zL, low(stabndx << 1)
	ldi	zH, high(stabndx << 1)
cs_loop:
	ldi	xL, low(txtndx)
	ldi	xH, high(txtndx)
	ld	temp, X
	ldi	xL, low(in_buf)
	ldi	xH, high(in_buf)
	add	xL, temp
	brcc	cs_cc
	inc	xH
cs_cc:	
	lpm	index, Z+
	push	zL
	push	zH
	ldi	zL, low(stab << 1)
	ldi	zH, high(stab << 1)
	lsl	index
	brcc	cs_nc3
	inc	zH
cs_nc3:
	add	zL, index
	brcc	cs_nc
	inc	zH
cs_nc:	
	lpm	count, Z+
	mov	up_txt, count
cs_nxtchr:
	ld	char, X+
	lpm	char2, Z+
	cp	char, char2
	brne	cs_nxtcmd
	dec 	count
	brne	cs_nxtchr
	lpm	char2, Z		;got it
;	mov	byte_trx, char2
;	rcall 	transmit
	ldi	yL, low(tmp_tok)
	ldi	yH, high(tmp_tok)
	st	Y, char2		;should point to token
	pop	temp
	pop	temp			;pop off z reg from stack
	ldi	temp, 0x50
	cp	char2, temp
	breq	getstr
	ldi	xL, low(txtndx)
	ldi	xH, high(txtndx)
	ld	temp, X
	add	temp, up_txt
	st	X, temp			;update text index
	mov	temp, char2
	rcall	cpst_store
	ret
getstr:
	mov	temp, char2
	rcall	cpst_store
gstr2:	ld	char, X+
	inc	up_txt
	cpi	up_txt, 0x40
	breq	gstr_err
;todo deal with line length
	ldi	temp, 0x22
	cp	char, temp
	breq	gstr_done
	mov	temp, char
	ori	temp, (1 << 7)		;set high bit
	rcall	cpst_store		
	rjmp	gstr2
gstr_err:
	ori	flags, (1 << err)
	ldi	xL, low(errno)
	ldi	xH, high(errno)
	ldi	temp, 0xFF
	st	X, temp
	ret
gstr_done:
	ldi	temp, 0x50
	rcall	cpst_store
	ldi	xL, low(txtndx)
	ldi	xH, high(txtndx)
	ld	temp, X
	add	temp, up_txt
	brcs	gstr_err
	st	X, temp
	ret
cs_nxtcmd:
	subi	xL, 1
	brcc	cs_nc2
	dec	xH
cs_nc2:
	inc	index2
	cpi	index2, maxstm
	breq	gvar
cn_nxt2:
	pop 	zH
	pop	zL
	rjmp	cs_loop
; get variable
gvar:
	pop	temp
	pop	temp
	clr	count
	ldi	xL, low(txtndx)
	ldi	xH, high(txtndx)
	ld	temp, X
	ldi	xL, low(in_buf)
	ldi	xH, high(in_buf)
	add	xL, temp
	brcc	gvar_cc
	inc	xH
gvar_cc:	
	push	xL
	push	xH
	ld	char, X+
	ld	temp, X+
	cpi	temp, '$'
	breq	gvar_stab
	ld	temp, X
	cpi	temp, '$'
	brne	gvar_itab
; check for int
gvar_stab:
;	pop	temp
;	pop	temp
;	rjmp	gstab
	ori	flags, (1 << str)
	ldi	yL, low(strtab)
	ldi	yH, high(strtab)
	rjmp	s_next
gvar_itab:
	cbr	flags, (1 << str)
	ldi	yL, low(inttab)
	ldi	yH, high(inttab)
s_next:
	ldi	index2, 2
	pop	xH
	pop	xL
	push	yL
	push	yH
	sbrs	flags, str
	rjmp	itn10
	inc	index2			;handle $
	ldi	yL, low(strmax)
	ldi	yH, high(strmax)
	rjmp	tn10
itn10:	ldi	yL, low(intmax)
	ldi	yH, high(intmax)
tn10:	ld	temp, Y
	cp	count, temp
	brne	gvar_nxt2
	pop	temp
	pop	temp
	rjmp	gvar_make
gvar_nxt2:
	pop	yH
	pop	yL
	ld	char2, Y+
	ld	char, X+
	cp	char, char2
	breq	gvar_equ
	inc	count
	sbiw	xH:xL, 1
	adiw	yH:yL, 1
	rjmp	gvar_nxt
gvar_equ:
	ld	char, X
;check alpha numeric
	clc
	mov	temp, char
	rcall	ck_alnu
	brcc	gvar_nochar
	rjmp	g_chr
gvar_nochar:
	dec	index2
	ldi	temp, 0x00
	mov	char, temp
g_chr:	
	ld	char2, Y+
	cp	char, char2
	breq	gvar_done
	sbiw	xH:xL, 2
	inc	count
gvar_nxt:
;todo	fix this
	adiw	yH:yL, 2
	push	xL
	push	xH
	rjmp	s_next
gvar_done:
	sbrs	flags, str
	rjmp	itn15
	ldi	temp, 0x71
	rjmp	tn15
itn15:	ldi	temp, 0x70		; = intvar token
tn15:	rcall	cpst_store
	ld	temp, Y+
	rcall	cpst_store
	ld	temp, Y
	rcall	cpst_store
	ldi	xL, low(txtndx)
	ldi	xH, high(txtndx)
	ld	temp, X
	add	temp, index2
	st 	X, temp			;update text pointer
;	ldi	byte_trx, 0xea
;	rcall	transmit
	ret
;done
gvar_make:
	ldi	xL, low(txtndx)
	ldi	xH, high(txtndx)
	ld	temp, X
	ldi	xL, low(in_buf)
	ldi	xH, high(in_buf)
	add	xL, temp
	brcc	gmake_cc
	inc	xH
gmake_cc:	
	ldi	index2, 2		; index2 keeps track of characters to parse
	sbrs	flags, str
	rjmp	itn11
	inc 	index2
	ldi	yL, low(strnxte)
	ldi	yH, high(strnxte)
	rjmp	tn11
itn11:	ldi	yL, low(intnxte)
	ldi	yH, high(intnxte)
tn11:	ld	tempL, Y+
	ld	tempH, Y
gmnxt:
	ld	char, X+
	ld	char2, X
	mov	temp, char2
	clc
	rcall	ck_alnu
	brcs	aokint
	dec	index2
	ldi	temp, 0x00
	mov	char2, temp
aokint:
	sbrs	flags, str
	rjmp	itn12
	ldi	xL, low(strtabp)
	ldi	xH, high(strtabp)
	rjmp	tn12
itn12:	ldi	xL, low(inttabp)
	ldi	xH, high(inttabp)
tn12:
aok_nxt:
	ld	yL, X+
	ld	yH, X+
	st	Y+, char
	st	Y+, char2
	st	Y+, tempL		;eeprom address
	st	Y+, tempH
	push	tempL
	push	tempH
	sbrs	flags, str
	rjmp	itn13
	ldi	temp, 0x40
	add	tempL, temp
	brcc	itn13cc
	inc	tempH
itn13cc:
	rjmp	tn13
itn13:	adiw	tempH:tempL, 2
tn13:	st	-X, yH
	st	-X, yL		;update strtabp / inttabp
;	ldi	byte_trx, 0xee
;	rcall	transmit
	sbrs	flags, str
	rjmp	itn14
	ldi	xL, low(strnxte)
	ldi	xH, high(strnxte)
	st	X+, tempL
	st	X, tempH
	ldi	xL, low(strmax)
	ldi	xH, high(strmax)
	ld	temp, X
	inc	temp
	cpi	temp, 0x2E
	brlo	tn16
	rjmp	st_err
tn16:	st	X, temp
	pop	yH
	pop	yL
	ldi	temp, 0x71
	rcall	cpst_store
	rjmp	tn14	
itn14:	ldi	xL, low(intnxte)
	ldi	xH, high(intnxte)
	st	X+, tempL
	st	X, tempH		;update next eeprom address
	ldi	xL, low(intmax)
	ldi	xH, high(intmax)
	ld	temp, X
	inc	temp
	cpi	temp, 0x2E
	brlo	tn17
	rjmp	st_err
tn17:	st	X, temp
	pop	yH
	pop	yL		;pop eeprom address and move into y to store
	ldi	temp, 0x70		; = intvar token
	rcall	cpst_store
tn14:	mov	temp, yL
	rcall	cpst_store
	mov	temp, YH
	rcall	cpst_store
	ldi	xL, low(txtndx)
	ldi	xH, high(txtndx)
	ld	temp, X
	add	temp, index2
	st 	X, temp			;update text pointer
;	ldi	byte_trx, 0xea
;	rcall	transmit
	ret
st_err:
	ori	flags, (1 << err)
	ldi	xL, low(errno)
	ldi	xH, high(errno)
	ldi	temp, 0xFF		;syntax err for now
	st	X, temp
	ret

;??	pop	temp
;	pop 	temp
;cpst_store takes byte to store in temp
cpst_store:
	push	count
	push	yL
	push	yH
	push	xL
	push	xH
	ldi	xL, low(curline)
	ldi	xH, high(curline)
	ld	yL, X+
	ld	yH, X
	adiw	yH:yL, 4	;eeprom pointer
	ld	xL, Y+
	ld	xH, Y+
	add	xL, e_lndx
	brcc	cps_cc
	inc	xH
cps_cc:
	push	temp
;	mov	byte_trx, xL
;	rcall	transmit
;	mov	byte_trx, xH
;	rcall	transmit
	pop 	temp
	call	S_writeByte
	inc	e_lndx
	pop	xH
	pop	xL
	pop	yH
	pop	yL
	pop	count
	ret			;done
; create string var
gstab:
	ret
cmp_cmd:
	clr	index
	clr	index2
	ldi	temp2, 0
	ldi	zL, low(ctabndx << 1)
	ldi	zH, high(ctabndx << 1)
cc_loop:
	ldi	xl, low(in_buf)
	ldi	xH, high(in_buf)
	lpm	index, Z+
	push	zL
	push	zH
	ldi	zL, low(ctab << 1)
	ldi	zH, high(ctab << 1)
	lsl	index
	brcc 	cc_nc2
	inc	zH
cc_nc2:
	add	zL, index
	brcc	cc_nc
	inc	zH
cc_nc:	
	lpm	count, Z+
cc_nxtchr:
	ld	char, X+
	lpm	char2, Z+
	cp	char, char2
	brne	cc_nxtcmd
	dec 	count
	brne	cc_nxtchr
	lpm	char2, Z		;got it
;	mov	byte_trx, char2
;	rcall 	transmit
	ldi	yL, low(tmp_tok)
	ldi	yH, high(tmp_tok)
	st	Y, char2		;should point to token
	pop	temp
	pop	temp			;pop off z reg from stack
	ret
cc_nxtcmd:
	subi	xL, 1
	brcc	cn_nc
	dec	xH
cn_nc:
	inc	index2
	cpi	index2, maxcmd
	brne	cc_nxt2
	ori	flags, (1 << err)
	ldi	temp, 0xFF
	ldi	yL, low(errno)
	ldi	yH, high(errno)
	st	Y, temp
	pop	temp
	pop 	temp
	ret
cc_nxt2:
	pop 	zH
	pop	zL
	rjmp	cc_loop
;------- run_cmd ---------
; executes cmd token from tmp_token

run_cmd:
	ldi	yL, low(tmp_tok)
	ldi	yH, high(tmp_tok)
	ld	temp, Y
	cpi	temp, 0x7f
	brsh	rc0
	ldi	zH, high(r_jtab)
	ldi	zL, low(r_jtab)
	add	zL, temp
	brcc	rc_nocarry
	inc 	zH
rc_nocarry:
	ijmp
rc0:	ldi 	temp, 0xFF
	ldi	yL, low(errno)
	ldi	yH, high(errno)
	st 	Y, temp
	sbr	flags, (1 << err)
	rjmp 	handle_error		
rc1:	ldi	byte_trx, 0x31
;	rcall	transmit
;	ldi	tempL, 0xFC
;	ldi	tempH, 0xFF
;	rcall	sgn2asc
;	rcall	prt_num
	rjmp	cold
	rjmp 	getline
rc2:	ldi	byte_trx, 0x32
	rcall	transmit
	rjmp 	getline
rc3:	ldi	byte_trx, 0x33
	rcall	transmit
	rjmp 	getline
rc4:	
	ldi	xL, 0x00
	ldi	xH, 0x00
;for key interrupt
	lds	temp, UCSR0B
	ori	temp, (1 << RXCIE0)
	sts	UCSR0B, temp
	sei
rc4_lp3:
;	call 	S_readByte
;	mov	byte_trx, temp
;	rcall	transmit
;	inc	xL
;	cpi	xL, 0x10
;	brne	rc4_lp3
;	ldi	xL, low(inttab)
;	ldi	xH, high(inttab)
;	ldi	count, 0x10
;ritlp:	ld	byte_trx, X+
;	rcall	transmit
;	dec	count
;	brne	ritlp
;run command
	ldi	xL, low(ppmem)
	ldi	xH, high(ppmem)
	adiw	xH:xL, 2
	ld	yL, X+
	ld	yH, X
	adiw	yH:yL, 2
	ldi	xL, low(rnep)
	ldi	xH, high(rnep)
	st	X+, yL
	st	X, yH
rc_lp:
	adiw	yH:yL, 2		;points to eeprom address
	ld	xL, Y+
	ld	xH, Y
rc_lp2:
;	rcall	reckint
	sbrc	flags, err
	rjmp	handle_error
	call	S_readByte
;	mov	byte_trx, temp
;	rcall	transmit
;	mov	temp, byte_trx
	cpi	temp, 0x2F
	brlo	rc4nxt
	cpi	temp, 0x70
	breq	letj
	cpi	temp, 0x71
	breq	letj
	cpi	temp, 0x7F
	breq	rc_ns
	rjmp	syn_err
letj:	rjmp	let2
rc4nxt:
	mov	char, temp
	ldi	temp, 0x10
	sub	char, temp
	brcc	rc4nxt2
	rjmp	syn_err
rc4nxt2:
	ldi	zH, high(exe_jtab)
	ldi	zL, low(exe_jtab)
	add	zL, char
	brcc	rc4cc
	inc	zH
rc4cc:
	ijmp
rc_ns:	
;	rcall	reckint
	sbrc	flags, err
	rjmp	handle_error
	cpi	temp, 0x4B
	breq	rc_same
;goto next line
	ldi	xL, low(rnep)
	ldi	xH, high(rnep)
	ld	yL, X+
	ld	yH, X

	ld	xL, y+
	ld	xH, Y+
	adiw	xH:xL, 2	;=next pointer
	mov	tempL, xL
	mov	tempH, xH
	adiw	xH:xL, 2
	ld	yL, X+
	ld	yH, X
	mov	xL, yL
	mov	xH, yH		; x is eeprom address
	cpi	xL, 0xff
	brne	rcns_nxt
	cpi	xH, 0xff
	brne	rcns_nxt
	ori	flags, (1 << err)
	ldi	xL, low(errno)
	ldi	xH, high(errno)
	ldi	temp, 0xfc
	st	X, temp
	rjmp	handle_error
rcns_nxt:
	ldi	yL, low(rnep)
	ldi	yH, high(rnep)
	st	Y+, tempL
	st	Y, tempH
;	mov	byte_trx, tempL
;	rcall	transmit
;	mov	byte_trx, tempH
;	rcall	transmit
	rjmp	rc_lp2
rc_same:
	adiw	xH:xL, 1
	rjmp	rc_lp2

;jump table
;	ldi	byte_trx, 0x34
;	rcall	transmit
;	ldi	xL, 0x00
;	ldi	xH, 0x00
;	ldi	yL, low(in_buf)
;	ldi	yH, high(in_buf)
;	
rc_end:
	cli
;	ldi	xL, 0x00
;	ldi	xH, 0x00
;rc4_loop:
;	rcall 	S_readByte
;	mov	byte_trx, temp
;	rcall	transmit
;	inc	xL
;	cpi	xL, 0x10
;	brne	rc4_loop
;	ldi	xL, 0x00
;	ldi	xH, 0x50
;rc4_lp2:
;	rcall 	S_readByte
;	mov	byte_trx, temp
;	rcall	transmit
;	inc	xL
;	cpi	xL, 0x10
;	brne	rc4_lp2
	rjmp	getline
;	ldi	xL, low(in_buf)
;	ldi	xH, high(in_buf)
;	rcall	sy_eval
;	ldi	yL, low(out_buf)
;	ldi	yH, high(out_buf)
;	ldi	count, 30
;rc4_lp2:
;	ld	byte_trx, Y+
;	rcall	transmit
;	dec	count
;	brne	rc4_lp2
;	ldi	yL, low(temp_16int)
;	ldi	yH, high(temp_16int)
;	ld	byte_trx, Y+
;	rcall	transmit
;	ld	byte_trx, Y
;	rcall	transmit
;	rjmp 	getline
reckint:
	lds	temp2, UCSR0A
	sbrs	temp2, RXC0
	ret
	lds	byte_trx, UDR0
	cpi	byte_trx, 0x21
	breq	rckint10
	ret
rckint10:
	sbr	flags, (1 << err)
	ldi	yL, low(errno)
	ldi	yH, high(errno)
	ldi	temp, 0xFA
	st	Y, temp
	ret
syn_err:
	sbr	flags, (1 << err)
	ldi	yL, low(errno)
	ldi	yH, high(errno)
	ldi	temp, 0xff
	st	Y, temp
	rjmp	handle_error
rc5:	ldi	byte_trx, 0x35
	rcall	transmit
	rjmp 	getline
rc6:	ldi	byte_trx, 0x36
	rcall	transmit
	rjmp 	getline
rc7:	ldi	byte_trx, 0x37
	rcall	transmit
	rjmp 	getline
rc8:	ldi	byte_trx, 0x38
	rcall	transmit
	rjmp 	getline
rc9:	
;list command
;
;	ldi	xL, 0
;	ldi	xH, 0
;	ldi	count, 0xff
;rdump:
;	call	S_readByte
;	mov	byte_trx, temp
;	call	transmit
;	adiw	xH:xL, 1
;	dec	count
;	breq	r_out
;	rjmp	rdump
;r_out:	rjmp 	getline
;r_out:
.def tok = r2	; also char
	ldi	yL, low(ppmem)
	ldi	yH, high(ppmem)
	adiw	yH:yL, 2
	ld	tempL, Y+
	ld	tempH, Y
	mov	yL, tempL
	mov	yH, tempH
rcp_lp2:			;Y points to begining of line in ppmem
	ld	tempL, Y+
	ld	tempH, Y+
	cpi	tempL, 0xff
	brne	rcplp3
	cpi	tempH, 0xff
	brne	rcplp3
	rjmp	getline
rcplp3:
	call	sgn2asc
	call	prt_num
	ldi	byte_trx, 0x20
	call	transmit
	ld	tempL, Y+
	ld	tempH, Y+		;next line ptr
	ldi	xL, low(tempaddr2)
	ldi	xH, high(tempaddr2)
	st	X+, tempL
	st	X, tempH		;store next line ptr
	ld	xL, Y+
	ld	xH, Y			;SRAM ptr
rc9_lp:	call	S_readByte
	cpi	temp, 0x7F
	breq	rc9_done
	cpi	temp, 0x70
	breq	rc9_ivar
	cpi	temp, 0x71
	breq	rc9_svar
	brlo	rc9_tok
	cpi	temp, 0x90
	brlo	rc9_num
	rjmp	rc9_basc
rc9_done:
	rjmp	rc9_rdon
rc9_ivar:
	rjmp	rc970
rc9_svar:
	rjmp	rc971
rc9_tok:
	rjmp	rctok2
rc9_num:
	rjmp	rcnum2
rc9_basc:
	rjmp	rcbasc2

rc970:	adiw 	xH:xL, 1
	call	S_readByte
	mov	qlineL, temp
	adiw	xH:xL, 1
	call	S_readByte
	mov	qlineH, temp
	mov	tempL, xL
	mov	tempH, xH
	ldi	yL, low(tempaddr)	;save x and y reg
	ldi	yH, high(tempaddr)
	st	Y+, tempL
	st 	Y, tempH
;	ldi	xL, low(rndx)
;	ldi	xH, high(rndx)
;	st	X+, yL
;	st	X, yH		;preserve text index
	ldi	yL, low(intmax)
	ldi	yH, high(intmax)
	ld	count, Y
	ldi	yL, low(inttab)
	ldi	yH, high(inttab)
	adiw	yH:yL, 2
rc970lp:
	ld	tempL, Y+
	ld	tempH, Y+
	cp	tempL, qlineL
	brne	rc970n
	cp	tempH, qlineH
	brne	rc970n2
;got it
	rjmp	rc970d
rc970er:
	rjmp	syn_err
rc970n:	
	dec 	count
	breq	rc970er
	adiw	yH:yL, 3
	rjmp	rc970lp
rc970n2:
	dec	count
	breq	rc970er
	adiw	yH:yL, 2
	rjmp	rc970lp
rc970d:	sbiw	yH:yL, 4
	ld	byte_trx, Y+
	call	transmit
	ld	byte_trx, Y
	tst	byte_trx
	breq	rc970n3
	call	transmit
rc970n3:
	ldi	byte_trx, 0x20
	call	transmit
	ldi	yL, low(tempaddr)
	ldi	yH, high(tempaddr)
	ld	xL, y+
	ld	xH, Y
	adiw	xH:xL, 1
	rjmp	rc9_lp
rc971:	adiw 	xH:xL, 1
	call	S_readByte
	mov	qlineL, temp
	adiw	xH:xL, 1
	call	S_readByte
	mov	qlineH, temp
	mov	tempL, xL
	mov	tempH, xH
	ldi	yL, low(tempaddr)	;save x and y reg
	ldi	yH, high(tempaddr)
	st	Y+, tempL
	st 	Y, tempH
;	ldi	xL, low(rndx)
;	ldi	xH, high(rndx)
;	st	X+, yL
;	st	X, yH		;preserve text index
	ldi	yL, low(strmax)
	ldi	yH, high(strmax)
	ld	count, Y
	ldi	yL, low(strtab)
	ldi	yH, high(strtab)
	adiw	yH:yL, 2
rc971lp:
	ld	tempL, Y+
	cp	tempL, qlineL
	brne	rc971n
	ld	tempH, Y+
	cp	tempH, qlineH
	brne	rc971n2
;got it
	rjmp	rc971d
rc971er:
	rjmp	syn_err
rc971n:	
	dec 	count
	breq	rc971er
	adiw	yH:yL, 3
	rjmp	rc971lp
rc971n2:
	dec	count
	breq	rc971er
	adiw	yH:yL, 2
	rjmp	rc971lp
rc971d:	sbiw	yH:yL, 4
	ld	byte_trx, Y+
	call	transmit
	ld	byte_trx, Y
	tst	byte_trx
	breq	rc971n3
	call	transmit
rc971n3:
	ldi	byte_trx, '$'
	call	transmit
	ldi	byte_trx, 0x20
	call	transmit
	ldi	yL, low(tempaddr)
	ldi	yH, high(tempaddr)
	ld	xL, y+
	ld	xH, Y
	adiw	xH:xL, 1
	rjmp	rc9_lp
rctok2:
	clr	count
	mov	tok, temp
	mov	tempL, xL
	mov	tempH, xH
	ldi	yL, low(tempaddr)	;save x and y reg
	ldi	yH, high(tempaddr)
	st	Y+, tempL
	st 	Y, tempH
;	ldi	xL, low(rndx)
;	ldi	xH, high(rndx)
;	st	X+, yL
;	st	X, yH		;preserve text index
rct10:	
	ldi	zL, low(stabndx << 1)
	ldi	zH, high(stabndx << 1)
	lpm	index, Z+
rct11:	lpm	index, Z+	;start at 1
	push	zL
	push	zH
	ldi	zL, low(stab << 1)
	ldi	zH, high(stab << 1)
	lsl	index
	brcc	rct_nc3
	inc	zH
rct_nc3:
	add	zL, index
	brcc	rct_nc
	inc	zH
rct_nc:	
	sbiw	zH:zL, 2
	sbr	zL, (1 << 0)
rct_cld:
	lpm	temp, Z	;check token
	tst	temp
	brne	rct14
;	sbiw	zH:zL, 1
	cbr	zL, (1<<0)
rct14:
	lpm	temp, Z		;word padding
	cp	temp, tok
	breq	rct_good
	inc	count
	cpi	count, maxstm	
	breq	rtc_err
	pop	zH
	pop	zL
	rjmp	rct11
rtc_err:
	pop	temp
	pop	temp
	jmp	syn_err
rct_good:
	pop	zH
	pop	zL
	sbiw	zH:zL, 2
;	sbrc	zL, 0
;	rjmp	rct_st
;	sbiw	zH:zL, 2
;	sbr	zL, (1 << 0)
;	rjmp	rctok1
;rct_st:
;	cbr	zL, (1 << 0)
rctok1:
	lpm	index, Z	;get last index
	ldi	zL, low(stab << 1)
	ldi	zH, high(stab << 1)
	lsl	index
	brcc	rct_nc4
	inc	zH
rct_nc4:
	add	zL, index
	brcc	rct_nc2
	inc	zH
rct_nc2:	
	lpm	index2, Z+	;check token
rct_lp2:
	lpm	byte_trx, Z+
	call	transmit
	dec	index2
	breq	rct_done
	rjmp	rct_lp2
rct_done:
	mov	temp, tok
	cpi	temp, 0x50
	breq	rct_3 
	ldi	byte_trx, 0x20
	call	transmit
rct_3:	ldi	yL, low(tempaddr)
	ldi	yH, high(tempaddr)
	ld	xL, y+
	ld	xH, Y
	adiw	xH:xL, 1
	rjmp	rc9_lp
rc9_rdon:
	ldi	byte_trx, 0x0d
	call	transmit
	ldi	byte_trx, 0x0a
	call	transmit
	ldi	xL, low(tempaddr2)
	ldi	xH, high(tempaddr2)
	ld	yL, X+
	ld	yH, X
	rjmp	rcp_lp2	
rcnum2:
	cbr	temp, (1 << 7)
	ldi	temp2, 0x30
	add	temp, temp2
	mov	byte_trx, temp
	call	transmit
rcn_done:
	adiw	xH:xL, 1
	call	S_readByte
	cpi	temp, 0x80
	brlo	rcn_d2
	cpi	temp, 0x90
	brsh	rcn_d2
	rjmp	rcnum2
rcn_d2:	
	ldi	byte_trx, 0x20
	call	transmit
	rjmp	rc9_lp
rcbasc2:
	cbr	temp, (1 << 7)
	mov	byte_trx, temp
	call	transmit	
;	ldi	byte_trx, 0x20
;	call	transmit
	adiw	xH:xL, 1
	rjmp	rc9_lp
	

;57
;
;	ldi	count, 0x40
;rc9_lp:	ld	byte_trx, X+	;hex dump of 0x0100 - 0x0120
;	rcall	transmit
;	dec	count
;	brne	rc9_lp
;	ldi	xL, 0x63
;	ldi	xH, 0x04
;	ldi	count, 0x06
;rc9_lp2:
;	ld	byte_trx, X+
;	rcall	transmit
;	dec	count
;	brne	rc9_lp2
;	ldi	xL, 0
;	ldi	xH, 0
;	ldi	count, 0xff
;rc9_lp3:
;	call	S_readByte
;	mov	byte_trx, temp
;	call	transmit
;	adiw	xH:xL, 1
;	dec	count
;	breq	r_out
;	rjmp	rc9_lp3
;r_out:	rjmp 	getline
;
r_jtab:
	rjmp	rc0
	rjmp	rc1
	rjmp	rc2
	rjmp	rc3
	rjmp	rc4
	rjmp	rc5
	rjmp	rc6
	rjmp	rc7
	rjmp	rc8
	rjmp	rc9
;----- handle error ------
; err flag set, errno in errno
handle_error:
	clr	temp2
	ldi	yL, low(errno)
	ldi	yH, high(errno)
	ld	temp2, Y
	com	temp2
	inc	temp2
	ldi	zL, low(errndx << 1)
	ldi	zH, high(errndx << 1)
	lsl 	temp2
	brcc	he_nc2
	inc	zH
he_nc2:
	add	zL, temp2
	brcc	he_nc
	inc	zH
he_nc:
	lpm	temp2, Z
;	mov 	byte_trx, temp2
;	rcall 	transmit
	ldi	zL, low(errtab << 1)
	ldi	zH, high(errtab << 1)
	lsl	temp2
	brcc	he_nc3
	inc	zH
he_nc3:
	add	zL, temp2
	brcc	he_nc4
	inc	zH
he_nc4:	

	call	print_s
	cbr	flags, (1 << err)
	rjmp	getline
	
;todo set err flag and err no
;------ a2u ------
; converts bcode digit to unsigned value
; takes pointer to text in X reg where digit is
; returns with digit in temp_16int
; max value ffff +/- 32767
a2u_sign:
	ld	r16, X
	cpi	r16, 0x31	;neg token
	brne	a2u
	ori	flags, (1 << sgn)
a2u:
	push	xL
	push	xH
	clr	r0		;r1:r0 16 bit accumulator
	clr 	r1
	clr	r2
	clr	r3		;r2:r3 accumulator for 16 bit
	clr	r4
	clr	r17
	ldi	r18, 10		;radix 10
a2u_next:
	ld 	r16, X+
	sbrc	r16, 7		;check if high bit set
	rjmp	a2u_chkh
	cpi 	r16, '9' + 1
	brsh	a2u_done
	subi	r16, '0'
	brcs	a2u_done
a2u_bascii:
	inc	r17
	push	r3		;high acc
	mul	r2, r18		;multiply low acc * 10
	mov	r2, r0
	mov	r3, r1
	pop	r4
	clr	r1
	mul	r4, r18
	tst	r1		;see if result is over
	brne	a2u_err
	mov	r4, r0		;copy hi result anything over ffff doesnt count
	add	r2, r16
	adc	r3, r4
	rjmp	a2u_next
a2u_err:
	ori	flags,(1 << err)
	ldi	temp, 0xFE
	ldi	xL, low(errno)
	ldi	xH, high(errno)
	st	X, temp
	rjmp	a2u_tu
a2u_chkh:
	cpi	r16, 0x80
	brlo	a2u_done
	cpi	r16, 0x90
	brsh	a2u_done
	andi	r16, 0x0F
	rjmp	a2u_bascii
a2u_done:
	ldi	xL, low(temp_16int)
	ldi	xH, high(temp_16int)
	st	X+, r2		;low byte first
	st 	X+, r3
a2u_tu:
	ldi	xL, low(txtndx)
	ldi	xH, high(txtndx)
	ld	temp, X
	add	temp, r17
	st	X, temp		;update txt pointer
	pop	xH
	pop	xL
	ret
; findline function
; line to find in temp_16int
; returns updated line in curline
; updates lastline
;
.def qlineL = r2
.def qlineH = r3
.def ptrL = r4
.def ptrH = r5
.def clineL = r6
.def clineH = r7
fndline:
	clr	e_lndx
	ldi	xL, low(temp_16int)
	ldi	xH, high(temp_16int)
	ld	clineL, X+
	ld	clineH, X			;get input line #
	ldi	xL, low(curline)
	ldi	xH, high(curline)
	ldi	temp, 0x00
	st	X+, temp
	ldi	temp, 0x01
	st	X, temp
fl_next:
	ldi	xL, low(curline)		;*****curline should be startlne
	ldi	xH, high(curline)
	ld	tempL, X+
	ld	tempH, X				;ptrL = pointer to next line
	adiw	tempH:tempL, 2
	mov	yL, tempL
	mov	yH, tempH
	mov	ptrL, tempL
	mov	ptrH, tempH
	ld	xL, Y+
	ld	xH, Y
	ldi	yL, low(curline)
	ldi	yH, high(curline)		;copy current line ptr
	st	Y+, xL	
	st	Y, xH			;curline is now what tempL pointed to
	ld	qlineL, X+
	ld	qlineH, X+
	cp	qlineH, clineH
	brlo	fl_nxt0
	cp 	qlineL, clineL
	brlo	fl_nxt0
	cp	qlineH, clineH
	brne	fl_h
	cp	qlineL, clineL
	brne	fl_h
	rjmp	fl_equ
fl_nxt0:
	rjmp	fl_next
fl_equ:
	ret					;do nothing curline points to correct line
fl_h:	ldi	xL, low(curline)
	ldi	xH, high(curline)
	ld	yL, X+
	ld	yH, X
	ld	tempL, Y+
	ld	tempH, Y
	ldi	temp, 0xFF
	cp	tempL, temp
	brne	fl_chgt
	cp	tempH, temp
	brne	fl_chgt
	rjmp	fl_open
; pointer = 0xffff so spot is open
fl_chgt:
	rjmp	fl_update
fl_open:
;	ptrL = old pointer to current line
	ldi	xL, low(curline)
	ldi	xH, high(curline)
	ld	yL, X+
	ld	yH, X	
	mov	tempL, yL
	mov	tempH, yH
	st	Y+, clineL
	st	Y+, clineH
	mov	tempL, yL
	mov	tempH, yH
	adiw	tempH:tempL, 4
	st	Y+, tempL
	st	Y+, tempH		;points to next line
	push	tempL
	push	tempH
	ldi	xL, low(epp)
	ldi	xH, high(epp)
	ld	tempL, X+
	ld	tempH, X+
	st	Y+, tempL
	st	Y, tempH
	ldi	temp, 0x41
	add	tempL, temp
	brcc	flo_cc
	inc	tempH
flo_cc:
	st	-X, tempH
	st	-X, tempL		
	ldi	xL, low(lastline)
	ldi	xH, high(lastline)
	pop	tempH
	pop	tempL
	st	X+, tempL
	st	X, tempH
	mov	xL, tempL
	mov	xH, tempH
	ldi	temp, 0xff
	st	X+, temp
	st	X+, temp
	st 	X+, temp
	st	X+, temp
;	mov	byte_trx, clineL
;	rcall	transmit
	ret

fl_update:
	ldi	xL, low(pstack)
	ldi	xH, high(pstack)
	ld	yL, X+			; y has address of dest line
	ld	yH, X
	ldi	xL, low(curline)	;update curline for cpst_store
	ldi	xH, high(curline)
	st	X+, yL
	st	X, yH
	mov	xL, ptrL		;ptrL has previous line ptr address
	mov	xH, ptrH
	ld	tempL, X+
	push 	tempL
	ld	tempH, X+
	push	tempH
	mov	xL, ptrL
	mov	xH, ptrH
	st	X+, yL
	st	X, yH			;move old pointer to new
; now move new to old

	st	Y+, clineL
	st	Y+, clineH
	pop	tempH
	pop	tempL
	st	Y+, tempL		;store pointer to old address in dest
	st	Y+, tempH			
	ldi	xL, low(epp)
	ldi	xH, high(epp)
	ld	tempL, X+
	ld	tempH, X
	st	Y+, tempL
	st	Y, tempH		;update eeprom pointer
	ldi	temp, 0x41
	add	tempL, temp
	brcc	fup_cc
	inc	tempH
fup_cc:
	st 	X, tempH
	st 	-X, tempL
; nothing else changes except update to pstack
	ldi	xL, low(pstack)
	ldi	xH, high(pstack)
	ld	yL, X+
	ld	yH, X+
	sbiw	yH:yL, 6
	st	-X, yH
	st	-X, yL	
	ret
; check alpha numberic
; takes temp as arg returns with carry set if true
ck_alnu:
	cpi	temp, '9' + 1
	brsh	cknxt
	cpi	temp, '0'
	brlo	no_chr
	rjmp	ckok
cknxt:	cpi	temp, 'Z' + 1
	brsh	cknxt2
	cpi	temp, 'A'
	brlo	no_chr
	rjmp	ckok
cknxt2:
	cpi	temp, 'z' + 1
	brsh	no_chr
	cpi	temp, 'a'
	brlo	no_chr
ckok:	sec
;	ldi	byte_trx, 0xab
;	rcall	transmit
	ret
no_chr:	clc
	ret
;evaluate function
; takes pointer to first number in expression
; returns with result in temp_16int
; requires out_buffer, stack
; may use in_buf for temp storage
sy_eval:
	pop	tempL
	pop	tempH		;save return address in case i screw up
	push	tempH
	push	tempL
	ldi	temp, 0xFF
	push	temp		;top of stack
	ldi	temp, 0x00
	push	temp		;for parens cp
	clr	index
	ldi	yL, low(out_buf)
	ldi	yH, high(out_buf)
sy_start:
	ld	char, X+
	ldi	temp, 0x7f
	cp	char, temp
	breq	sy_done
	ldi	temp, 0x4B
	cp	char, temp
	breq	sy_done
	ldi	temp, 0x70
	cp	char, temp
	breq	sy_varj
	ldi 	temp, 0x80
	cp	char, temp
	brlo	sy_opj
	ldi	temp, 0x90
	cp	char, temp
	brsh	sy_done	
	rjmp	sy_num
sy_varj:
	rjmp	sy_var
sy_opj:
	rjmp	sy_op
sy_done:
	;pop stack onto out queue
	ldi	temp, 0x00
sy_don1:
	pop	char
	cp	char, temp
	breq	sy_rd
	ldi	temp, 0x7D
	st	Y+, temp
	st	Y+, char 	
	rjmp	sy_done	
sy_rd:
	ldi	temp, 0x7B
	st	Y+, temp	;end of rpn
	pop	temp		;last ff
	rjmp	rpn_eval
sy_num:
syn_lp:	st	Y+, char
	ld	char, X+
	ldi	temp, 0x80
	cp	char, temp
	brlo	sy_endn
	rjmp	syn_lp	
sy_endn:
	ldi	temp, 0x7c		;end of number
	st	Y+, temp
	sbiw	xH:xL, 1
	rjmp	sy_start
sydvr:
	adiw	xH:xL, 1
	ld	tempL, X+
	ld	tempH, X+
	ldi	yL, low(dmndx)
	ldi	yH, high(dmndx)
	st	Y+, xL
	st	Y, xH
	mov	xL, tempL
	mov	xH, tempH
	call	S_readByte
	mov	tempL, temp
	adiw	xH:xL, 1
	call	S_readByte
	mov	tempH, temp
	ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)
	st	Y+, tempL
	st	Y, tempH
	ldi	xL, low(dmndx)
	ldi	xH, high(dmndx)
	ld	tempL, X+
	ld	tempH, X
	mov	xL, tempL
	mov	xH, tempH
	rjmp	syd_ent1
syvar_dm:
	push	xL
	push	xH
	ldi	xL, low(tempaddr2)
	ldi	xH, high(tempaddr2)
	st	X+, temp
	st	X, temp2
	ldi	xL, low(temp3_16)
	ldi	xH, high(temp3_16)
	st	X+, yL
	st	X, yH		;store ptr to out_buf in temp3_16
	pop	xH
	pop	xL
;begining of dim
sy_dimi:
	adiw	xH:xL, 1
	ld	temp, X
	cpi	temp, 0x70
	breq 	sydvr
	ldi	yL, low(dmndx)
	ldi	yH, high(dmndx)
	st	Y+, xL
	st	Y, xH		;save pointer to in_buf
	call	a2u
	ldi	yL, low(dmndx)
	ldi	yH, high(dmndx)
	ld	xL, Y+
	ld	xH, Y
	add	xL, r17
	brcc	syd_cc
	inc	xH
syd_cc:
	st	Y, xH
	st	-Y, xL
syd_ent1:
	ld	temp, X+
	cpi	temp, 0x51
	breq	syd_mor
	ldi	yL, low(tempaddr2)
	ldi	yH, high(tempaddr2)
	ld	xL, Y+
	ld	xH, Y		
	call	S_readByte
	mov	yL, temp
	adiw	xH:xL, 1
	call	S_readByte
	mov	yH, temp
	ldi	xL, low(temp_16int)
	ldi	xH, high(temp_16int)
	ld	r0, X+
	ld	r1, X
	sbiw	yH:yL, 2		;beginning of array
	clc
	lsl	r0
	rol	r1			;multipy by 2
	ldi	temp, 1
	add	r0, temp
	brcc	syd_cc4
	inc	r1
syd_cc4:
	sub	yL, r0
	sbc	yH, r1
	ldi	xL, low(tempaddr2)
	ldi	xH, high(tempaddr2)
	st	X+, yL
	st	X, yH
	ldi	yL, low(dmndx)
	ldi	yH, high(dmndx)
	ld	xL, Y+
	ld	xH, Y
	rjmp	sydm_ret
syd_mor:
	jmp	syn_err
;no more x(1,2)
	ldi	xL, low(temp_16int)
	ldi	xH, high(temp_16int)
	ld	tempL, X+
	ld	tempH, X
	ldi	xL, low(temp2_16int)
	ldi	xH, high(temp2_16int)
	st	X+, tempL
	st	X, tempH		;store first dim in temp4_16
	ldi	yL, low(dmndx)
	ldi	yH, high(dmndx)
	ld	xL, Y+
	ld	xH, Y
	adiw	xH:xL, 1
	ld	temp, X
	cpi	temp, 0x70
	brne	sydok1
	rjmp	sydvar3
sydok1:	ldi	yL, low(dmndx)
	ldi	yH, high(dmndx)
	st	Y+, xL
	st	Y, XH
	call	a2u
	ldi	yL, low(dmndx)
	ldi	yH, high(dmndx)
	ld	xL, Y+
	ld	xH, Y
	add	xL, r17
	brcc	syd_cc2
	inc	xH
syd_cc2:
	st	Y, xH
	st	-Y, xL
syd_ent2:
	call	mul16		;temp_16int and temp2_16int should be loaded
	ldi	yL, low(temp2_16int)
	ldi	yH, high(temp2_16int)
	ld	tempL, Y+
	ld	tempH, Y
	add	r6, tempL
	adc	r7, tempH
	ldi	yL, low(tempaddr2)
	ldi	yH, high(tempaddr2)
	ld	xL, Y+
	ld	xH, Y		
	call	S_readByte
	mov	yL, temp
	adiw	xH:xL, 1
	call	S_readByte
	mov	yH, temp
	;ptr->intsram->dimsram
	ldi	temp, 1
	add	r6, temp
	brcc	syd_cc3
	inc	r7
syd_cc3:
	sub	yL, r6
	sbc	yH, r7
	ldi	xL, low(tempaddr2)
	ldi	xH, high(tempaddr2)
	st	X+, yL
	st	X, yH
	ldi	yL, low(dmndx)
	ldi	yH, high(dmndx)
	ld	xL, Y+
	ld	xH, Y
;end sy_dim
sydm_ret:
	push	xL
	push	xH
	ldi	yL, low(tempaddr2)
	ldi	yH, high(tempaddr2)
	ld	xL, Y+
	ld	xH, Y
	ldi	yL, low(temp3_16)
	ldi	yH, high(temp3_16)
	ld	tempL, Y+
	ld	tempH, Y
	mov	yL, tempL
	mov	yH, tempH
	call	S_readByte
	st	Y+, temp
	adiw	xH:xL, 1
	call	S_readByte
	st	Y+, temp
	pop	xH
	pop	xL
	adiw	xH:xL, 1
	rjmp	sy_start
sydvar3:
	adiw	xH:xL, 1
	ld	tempL, X+
	ld	tempH, X+
	ld	temp, X
	cpi	temp, 0x4A
	breq	sydok6
	jmp	syn_err
sydok6:	ldi	yL, low(dmndx)
	ldi	yH, high(dmndx)
	st	Y+, xL
	st	Y, xH
	mov	xL, tempL
	mov	xH, tempH
	call	S_readByte
	mov	tempL, temp
	adiw	xH:xL, 1
	call	S_readByte
	mov	tempH, temp
	ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)
	st	Y+, tempL
	st	Y, tempH
	rjmp	syd_ent2
sy_var:
	ldi	temp, 0x7e
	st	Y+, temp	;7e = 2 val bytes next
	ld	temp, X+
	ld	temp2, X+	;next two char are address to value
	ld	tempH, X
	cpi	tempH, 0x49	;'('
	brne	syvar1
;	ldi	byte_trx, 0xEA
;	call	transmit
;	mov	temp, byte_trx
	rjmp	syvar_dm
syvar1:	push	xL
	push	xH
	mov	xL, temp
	mov	xH, temp2
	call	S_readByte
	st	Y+, temp
	adiw	xH:xL, 1
	call	S_readByte
	st	Y+, temp
	pop	xH
	pop	xL
	rjmp	sy_start
sy_op:
	pop	char2
	tst	char2
	brne	syop2
	push	char2
	push	char
	rjmp	sy_start
syop2:
	ldi	temp, 0x41
	cp	char, temp
	brlo	syckp
	ldi	temp, 0x43
	cp	char, temp
	brlo	sy_p1
	ldi	temp, 0x45
	cp	char, temp
	brlo	sy_p2
	ldi	temp, 0x4C	; ^
	cp	char, temp
	breq	sy_p3
syckp:	ldi	temp, 0x49	;left parens
	cp	char, temp
	breq	sy_lpns
	ldi	temp, 0x4A	;right parens tok
	cp	char, temp
	breq	sy_rpns	
	rjmp	sy_notop
sy_notop:
sy_p1:
	ldi	temp, 0x49
	cp	char2, temp
	brne	sypnxt
	push	char2
	push	char			;operator on stack is '(' so push new op
	rjmp	sy_start
sypnxt:
	ldi	temp, 0x43
	cp	char2, temp		;equal precedence
	brsh	sy_p1n
	push	char2
	push	char
	rjmp	sy_start
sy_p1n:
	ldi	temp, 0x7D		;precedes op token
	st	Y+, temp
	st	Y+, char2
	push	char	
	rjmp	sy_start
sy_p2:
	ldi	temp, 0x49
	cp	char2, temp
	brne	sypnxt2
	push	char2
	push	char			;operator on stack is '(' so push new op
	rjmp	sy_start
sypnxt2:
	ldi	temp, 0x46
	cp	char2, temp
	brsh	sy_p2n
	;op2 has greater prec or is parens
	push	char2
	push	char
	rjmp	sy_start
sy_p2n:
	ldi	temp, 0x7D		;precedes op token
	st	Y+, temp
	st	Y+, char2		;equal or less prec so pop operator onto output
	push	char
	rjmp	sy_start
sy_p3:
	push	char2
	push	char
	rjmp	sy_start
sy_lpns:
	push	char2
	push	char
	rjmp	sy_start
sy_rpns:
;todo
	ldi	temp, 0x49
	cp	char2, temp
	breq	syrp_done
	ldi	temp, 0xFF	; no left parens
	cp	char2, temp
	breq	sy_err
	;move op2 onto output queue
	ldi	temp, 0x7d
	st	Y+, temp
	st	Y+, char2
	pop	char2
	rjmp	sy_rpns
syrp_done:
	rjmp	sy_start	;discard char ')'
sy_err:
	ori	flags, (1 << err)
	ldi	temp, 0xfd
	ldi	xL, low(errno)
	ldi	xH, high(errno)
	st	X, temp
	rjmp	getline
;rpn eval
; rpn in out_buf
rpn_eval:
	ldi	yL, low(txtndx)
	ldi	yH, high(txtndx)
	ldi	temp, 0
	st	Y, temp
	push	temp		;top of stack marker
	push	temp		; pad for neg
	clr	index
	ldi	xL, low(out_buf)
	ldi	xH, high(out_buf)
rpn_elp:
	ld	char, X+
	ldi	temp, 0x80
	cp	char, temp
	brlo	rpnxt
	ldi	temp, 0x90
	cp	char, temp
	brlo	rp_num
rpnxt:	ldi	temp, 0x7C
	cp	char, temp
	brne	rpnxt2
	rjmp	rpn_elp		;discard eonumber
rpnxt2:
	ldi	temp, 0x7E
	cp	char, temp
	brne	rpnxt3
	rjmp	rp_var
rpnxt3:
	ldi	temp, 0x7D
	cp	char, temp
	brne	rpnxt4
	rjmp	rp_op
rpnxt4:
	ldi	temp, 0x7B
	cp	char, temp
	breq	rpnxt5
	rjmp	rpn_err
rpnxt5:	rjmp	rp_done
rp_num:
	sbiw	xH:xL, 1
	rcall	a2u
	ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)
;	ld	byte_trx, Y+
;	rcall	transmit
;	ld	byte_trx, Y
;	rcall	transmit
	add	xL, r17			; index from a2u
	brcc	rpnu_cc
	inc	xH
rpnu_cc:
	ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)
	ld	temp, Y+
	push	temp		;push a2u result onto stack
	ld	temp, Y
	push	temp	
	rjmp	rpn_elp
rp_var:
	ld	char, X+
	push	char
	ld	char, X+
	push	char
	rjmp	rpn_elp
rp_op:
	ld	char, X+
	ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)
	pop	temp2
	pop	temp
	st	Y+, temp
	st	Y, temp2
	ldi	yL, low(temp2_16int)		;temp2 is first value
	ldi	yH, high(temp2_16int)
	pop	temp2
	pop	temp
	st	Y+, temp
	st	Y, temp2
	ldi	temp, 0x42	;'+'
	cp	char, temp
	brne	ropnxt
	rjmp	rp_add
ropnxt: ldi	temp, 0x41	;'-'	
	cp	char, temp
	brne	ropnxt2
	rjmp	rp_sub
ropnxt2:
	ldi	temp, 0x44
	cp	char, temp
	brne	ropnxt3
	rjmp	rp_mul
ropnxt3:
	ldi	temp, 0x43
	cp	char, temp
	brne	ropnxt4
	rjmp	rp_div
ropnxt4:
	ldi	temp, 0x45
	cp	char, temp
	brne	ropnxt5
	rjmp	rp_mod
ropnxt5:
	ldi	temp, 0x4C
	cp	char, temp
	brne	rpn_err
	rjmp	rp_exp
rpn_err:
	rjmp	rpn_elp		;there can't be any errors
rp_done:
	ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)
	pop	temp
	pop	temp2
	sbrs	flags, sgn
	rjmp	rp_store
	ldi	r21, 1
	ldi	r17, 0xff
	eor	temp2, r17
	add	temp2, r21
	brcc	rpdcc
	inc	temp
rpdcc:
	eor	temp, r17
	cbr	flags, (1 << sgn)
rp_store:
	st	Y+, temp2
	st	Y, temp
	pop	temp
	cpi	temp, 0
	breq	rp10
	cp	temp, tempL
	breq	rp12
	rjmp	rp13
rp12:	push	temp
	ret
rp10:	pop	temp2
	cpi	temp2, 0
	breq	rp11
rp13:	push	tempH
	push	tempL
	ori	flags, (1 << err)
	ldi	xL, low(errno)
	ldi	xH, high(errno)
	ldi	temp, 0xFD
	st	X, temp
	ret
rp11:
;	ldi	byte_trx, 0xea
;	rcall	transmit
	ret
rp_add: ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)
	ld	r2, Y+
	ld	r3, Y
	ldi	yL, low(temp2_16int)
	ldi	yH, high(temp2_16int)	
	ld	r4, Y+
	ld	r5, Y
	add	r2, r4
	adc	r3, r5
	brvs	rp_oe
	push	r2
	push	r3
	rjmp	rpn_elp
rp_oe:
	ori	flags, (1 << err)
	ldi	temp, 0xFE
	ldi	yL, low(errno)
	ldi	yH, high(errno)
	st	Y, temp
	push	r2
	push	r3
	rjmp	rpn_elp
;todo	write neg sub
rp_sub: 
	sbrs	flags, sgn
	rjmp	notneg
	rjmp	rp_add
notneg:
	ldi	yL, low(temp2_16int)
	ldi	yH, high(temp2_16int)
	ld	r2, Y+
	ld	r3, Y
	ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)	
	ld	r4, Y+
	ld	r5, Y
	sub	r2, r4
	sbc	r3, r5
	brmi	rp_neg
;	brvs	rp_vs
	push	r2
	push	r3
	rjmp	rpn_elp
rp_neg:
	ldi	temp, 1
	ldi	temp2, 0xff
	eor	r2, temp2
	add	r2, temp
	brcc	rpng_cc
	dec	r3
rpng_cc:
	eor	r3, temp2
	ldi	tuk, (1 << sgn)
	eor	flags, tuk
	push	r2
	push	r3
	rjmp	rpn_elp
rp_vs:
	ori	flags, (1 << err)
	ldi	temp, 0xFE
	ldi	yL, low(errno)
	ldi	yH, high(errno)
	st	Y, temp
	push	r2
	push	r3
	rjmp	rpn_elp
rp_mul:
	rcall	mul16
	push	r6
	push	r7
	rjmp	rpn_elp
;16 bit multipy
; accl r7:r6 acch r9:r8 H:L
;
mul16:
	clr	r6
	clr	r7
	clr	r8
	clr	r9
	ldi	tuk, (1 << sgn)
	ldi	yL, low(temp2_16int)
	ldi	yH, high(temp2_16int)
	ld	r2, Y+
	ld	r3, Y
	tst	r3
	brpl	m16nn
	eor	flags, tuk
	ldi	temp, 1
	ldi	temp2, 0xff
	eor	r2, temp2
	add	r2, temp
	brcc	mnegcc
	dec	r3
mnegcc:
	eor	r3, temp2
m16nn:		
	ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)
	ld	r4, Y+
	ld	r5, Y
	tst	r5
	brpl	m16nn2
	eor	flags, tuk
	ldi	temp, 1
	ldi	temp2, 0xff
	eor	r4, temp2
	add	r4, temp
	brcc	mnegcc2
	dec	r5
mnegcc2:
	eor	r5, temp2
m16nn2:
	mul	r4, r2
	mov	r6, r0
	mov	r7, r1
	mul	r4, r3
	add	r7, r0
	adc	r8, r1
	mul	r5, r2
	add	r7, r0
	adc 	r8, r1
	mul	r5, r3
	add	r8, r0
	adc	r9, r1
	tst	r8
	brne	mul_gt
	tst	r9
	brne	mul_gt
	sbrc	r7, 7
	rjmp	mul_gt	
	ret
mul_gt:
	ori	flags, (1 << err)
	ldi	temp, 0xFE
	ldi	yL, low(errno)
	ldi	yH, high(errno)
	st	Y, temp
	ret
rp_div:
	rcall	div16
	push	r0
	push	r1
	rjmp	rpn_elp
rp_mod:
	rcall	div16
	push	r2
	push	r3
	rjmp	rpn_elp
rp_exp:
;todo
	ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)
	ld	count, Y
	dec 	count
	ldi	yL, low(temp2_16int)
	ldi	yH, high(temp2_16int)
	ld	temp, Y+
	ld	temp2, Y
	ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)
	st	Y+, temp
	st	Y, temp2
rex_lp:
	rcall	mul16
	ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)
	st	Y+, r1
	st	Y, r0
	dec	count
	brne	rex_lp
	push	r0
	push 	r1
	rjmp	rpn_elp
;**** div16 *****
; 16bit X 16 bit division
; r1:r0	H:L 	dividend / quotient
; r3:r2		dividend window / remainder
; r5:r4		divisor
div16:
	ldi	tuk, (1 << sgn)
	clr	r3
	clr	r2
	ldi	yL, low(temp2_16int)
	ldi	yH, high(temp2_16int)
	ld	r0, Y+
	ld	r1, Y
	tst	r1
	brpl	div16nn
	eor	flags, tuk
	ldi	temp, 1
	ldi	temp2, 0xff
	eor	r0, temp2
	add	r0, temp
	brcc	dvnegcc
	dec	r1
dvnegcc:
	eor	r1, temp2
div16nn:		
	ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)
	ld	r4, Y+
	ld	r5, Y
	tst	r5
	brpl	div16nn2
	eor	flags, tuk
	ldi	temp, 1
	ldi	temp2, 0xff
	eor	r4, temp2
	add	r4, temp
	brcc	dvnegcc2
	dec	r5
dvnegcc2:
	eor	r5, temp2
div16nn2:		
	ldi	count, 17	;16 bits + 1
div_lp:	rol	r0
	rol	r1
	dec	count
	breq	div_done
	rol	r2
	rol	r3
	sub	r2, r4
	sbc	r3, r5
	brcc	d_good		;goes into 
	add	r2, r4
	adc	r3, r5
	clc
	rjmp	div_lp
d_good:	sec
	rjmp	div_lp
div_done:
	st	-Y, r3
	st	-Y, r2
	ret
;31
;************* main statement functions ***********
;c	on entry xH:xL has eeprom address of line being executed
	
let:
	adiw	xH:xL, 1
	call	S_readByte
	mov	tuk, temp
let2:	cpi	temp, 0x70
	breq	lt_nint
	cpi	temp, 0x71
	breq	lt_nstr
	ori	flags, (1 << err)
	ldi	xL, low(errno)
	ldi	xH, high(errno)
	ldi	temp, 0xfe
	st	X, temp
	rjmp	handle_error
lt_nint:
	rjmp	lt_nxt
lt_nstr:
	ldi	yL, low(tempaddr)		; use for eprom address holder
	ldi	yH, high(tempaddr)
	adiw	xH:xL, 1
	call	S_readByte
	st	Y+, temp
;	mov	byte_trx,temp
;	call	transmit
	adiw	xH:xL, 1
	call	S_readByte
	st	Y, temp
;	mov 	byte_trx, temp
;	call	transmit
	adiw	xH:xL, 1
	call	S_readByte
	cpi	temp, 0x40		;expect '=' 
	breq	lt10
	rjmp	syn_err
lt10:	
	
	adiw	xH:xL, 1
	call	S_readByte
	cpi	temp, 0x71
	brne	lt11
	rjmp	lt_sv 	
lt11:	cpi	temp, 0x50
	breq	lt12
	rjmp	syn_err
lt12:	
	ldi	count, 0x74
	ldi	yL, low(in_buf)
	ldi	yH, high(in_buf)
lt_lp2:	adiw	xH:xL, 1
	call	S_readByte
	st	Y+, temp
	cpi	temp, 0x50
	breq	lt13
	cpi	temp, 0x7F
	breq	lt13err
	dec	count
	brne	lt_lp2
	rjmp	syn_err
lt13:
	mov	tuk, temp
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	st	Y+, xL
	st	Y, xH
	ldi	yL, low(tempaddr)
	ldi	yH, high(tempaddr)
	ld	xL, Y+
	ld	xH, Y
	ldi	yL, low(in_buf)
	ldi	yH, high(in_buf)
lt13lp:	ld	temp, Y+
	cpi	temp, 0x50
	breq	lt13_done
	dec	count
	breq	lt13err	
	call	S_writeByte
	adiw	xH:xL, 1
	rjmp	lt13lp
lt13err:
	rjmp	syn_err
lt13_done:
	ldi	temp, 0
;	adiw	xH:xL, 1
	call	S_writeByte		;null terminate
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	ld	xL, Y+
	ld	xH, Y
	adiw	xH:xL, 1
	ld	temp, X
	rjmp	rc_ns
;assign variable to variable
lt_sv:
	ldi	count, 0x7f
	ldi	yL, low(tempaddr)
	ldi	yH, high(tempaddr)
	ld	r0, Y+
	ld	r1, Y
	adiw	xH:xL, 1
	call	S_readByte
	mov	xL, temp
	adiw	xH:xL, 1
	call	S_readByte
	mov	xH, temp	;xH:xL has string eeprom address
ltsv10:	call	S_readByte
	mov	tempL, xL
	mov	tempH, xH
	mov	xL, r0
	mov	xH, r1
	cpi	temp, 0
	breq	ltsv_done
	dec	count
	breq	ltsverr
	call	S_writeByte
	adiw	xH:xL, 1
	mov	r0, xL
	mov	r1, xH
	mov	xL, tempL
	mov	xH, tempH
	adiw	xH:xL, 1
	rjmp	ltsv10
ltsverr:
	rjmp	syn_err
ltsv_done:
	call	S_writeByte
	mov	temp, tuk
	rjmp	rc_ns	
			
lt_nxt:
;	ldi	byte_trx, 0xbb
;	call	transmit
	ldi	yL, low(tempaddr)		; use for eprom address holder
	ldi	yH, high(tempaddr)
	adiw	xH:xL, 1
	call	S_readByte
	st	Y+, temp
;	mov	byte_trx,temp
;	call	transmit
	adiw	xH:xL, 1
	call	S_readByte
	st	Y, temp
;	mov 	byte_trx, temp
;	call	transmit
	adiw	xH:xL, 1
	call	S_readByte
	cpi	temp, 0x40		;expect '=' 
	breq	lt_norm
	cpi	temp, 0x49		;'('
	brne	lt_err
	rcall	lt_dimi
	adiw	xH:xL, 1
	call	S_readByte
	cpi	temp, 0x40		;'='
	breq	lt_norm
	rjmp	syn_err
lt_norm:
	adiw	xH:xL, 1
	call	S_readByte
	cpi	temp, 0x28
	breq	lt_rdpj
	cpi	temp, 0x29
	breq	lt_pk2
	rjmp	lt_nxt2
lt_rdpj:
	rjmp	lt_rdp
lt_pk2:
	rjmp	lt_peek
lt_nxt2:
	sbiw	xH:xL, 1
	ldi	count, 0x74
	ldi	yL, low(in_buf)
	ldi	yH, high(in_buf)
lt_lp:	adiw	xH:xL, 1
	call	S_readByte
	st	Y+, temp
	cpi	temp, 0x4b
	breq	lt_done
	cpi	temp, 0x7F
	breq	lt_done
	dec	count
	breq	lt_err
	rjmp	lt_lp
lt_err:
	rjmp	syn_err
lt_done:
	mov	tuk, temp
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	st	Y+, xL		
	st	Y, xH
	ldi	xL, low(in_buf)
	ldi	xH, high(in_buf)
	rcall	sy_eval
	ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)
	ld	tempL, Y+
;	mov	byte_trx, tempL
;	call	transmit
	ld	tempH, Y
;	mov	byte_trx, tempH
;	call	transmit
	ldi	yL, low(tempaddr)
	ldi	yH, high(tempaddr)
	ld	xL, Y+
;	mov	byte_trx, xL
;	call	transmit
	ld	xH, Y
;	mov	byte_trx, xH
;	call	transmit
	mov	temp, tempL
	call	S_writeByte
	mov	temp, tempH
	adiw	xH:xL, 1
	call	S_writeByte
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	ld	xL, Y+
	ld	xH, Y
	mov	temp, tuk		; i had to stash it somewhere
	rjmp	rc_ns
;25
ltdvr:
	adiw	xH:xL, 1
	call	S_readByte
	mov	tempL, temp
	adiw	xH:xL, 1
	call	S_readByte
	mov	tempH, temp	
	adiw	xH:xL, 1
	call	S_readByte
	mov	temp2, temp
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	st	Y+, xL
	st	Y, xH
	mov	xL, tempL
	mov	xH, tempH
	call	S_readByte
	mov	tempL, temp
	adiw 	xH:xL, 1
	call	S_readByte
	mov	tempH, temp
	ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)
	st	Y+, tempL
	st	Y, tempH
	ldi	yL, low(tmp_tok)
	ldi	yH, high(tmp_tok)
	st	Y, temp2
	rjmp	ltd_ent1
;161
lt_dimi:
	adiw	xH:xL, 1
	call	S_readByte
	cpi	temp, 0x70
	breq 	ltdvr
	ldi	yL, low(dim_buf)
	ldi	yH, high(dim_buf)
	ldi	count, 0x20
lt_dmlp:
	st	Y+, temp
	adiw	xH:xL, 1
	call	S_readByte
	cpi	temp, 0x80
	brlo	ltd_don1
	cpi	temp, 0x90
	brsh	ltd_don1
	dec	count
	breq	ltd_err2
	rjmp	lt_dmlp	
ltd_err2:
	pop	temp
	pop	temp
	jmp	syn_err
ltd_don1:
	ldi	temp2, 0x7f
	st	Y, temp2
	ldi	yL, low(tmp_tok)
	ldi	yH, high(tmp_tok)
	st	Y, temp
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	st	Y+, xL
	st	Y, XH
	ldi	xL, low(dim_buf)
	ldi	xH, high(dim_buf)
	call	a2u
ltd_ent1:
	ldi	yL, low(tmp_tok)
	ldi	yH, high(tmp_tok)
	ld	temp, Y
	cpi	temp, 0x51		;","
	breq	ltd_mor
	ldi	yL, low(tempaddr)
	ldi	yH, high(tempaddr)
	ld	xL, Y+
	ld	xH, Y		
	call	S_readByte
	mov	yL, temp
	adiw	xH:xL, 1
	call	S_readByte
	mov	yH, temp
	ldi	xL, low(temp_16int)
	ldi	xH, high(temp_16int)
	ld	r0, X+
	ld	r1, X
	sbiw	yH:yL, 2		;beginning of array
	lsl	r0
	rol	r1			;multipy by 2
	ldi	temp, 1
	add	r0, temp
	brcc	ltd_cc
	inc	r1
ltd_cc:
	sub	yL, r0
	sbc	yH, r1
	ldi	xL, low(tempaddr)
	ldi	xH, high(tempaddr)
	st	X+, yL
	st	X, yH
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	ld	xL, Y+
	ld	xH, Y
	ret
ltd_mor:
	jmp	syn_err
;I've taken out 2 dim
	ldi	xL, low(temp_16int)
	ldi	xH, high(temp_16int)
	ld	tempL, X+
	ld	tempH, X
	ldi	xL, low(temp2_16int)
	ldi	xH, high(temp2_16int)
	st	X+, tempL
	st	X, tempH		;store first dim in temp4_16
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	ld	xL, Y+
	ld	xH, Y
	adiw	xH:xL, 1
	call	S_readByte
	cpi	temp, 0x70
	breq	ltdvar2
	ldi	yL, low(dim_buf)
	ldi	yH, high(dim_buf)
	ldi	count, 0x0A
ltdlp2:
	st	Y+, temp
	adiw	xH:xL, 1
	call	S_readByte
	cpi	temp, 0x80
	brlo	ltd_don2
	cpi	temp, 0x90
	brsh	ltd_don2
	dec	count
	breq	ltd_err
	rjmp	ltdlp2	
ltdvar2:
	rjmp	ltdvar3
ltd_err:
	pop	temp
	pop	temp		;pop off return address
	jmp	syn_err
ltd_don2:
	mov	byte_trx, temp
	call	transmit
	mov	temp, byte_trx
	cpi	temp, 0x4A
	breq	ltdok3
	rjmp	syn_err
;ltdok2:	adiw	xH:xL, 1
;	call	S_readByte
;	cpi	temp, 0x7f
;	breq	ltdok
;	cpi	temp, 0x4B	;':'
;	breq	ltdok
;	st	Y+, temp
;	rjmp	ltdlp2
ltdok3:
;	sbiw	xH:xL, 1
	ldi	temp2, 0x7f
	st	Y, temp2
	
ltdok:	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	st	Y+, xL
	st	Y, XH
	ldi	xL, low(dim_buf)
	ldi	xH, high(dim_buf)

	call	a2u
ltd_ent2:
	call	mul16		;temp_16int and temp2_16int should be loaded
	ldi	byte_trx, 0xFF
	call	transmit
	ldi	yL, low(temp2_16int)
	ldi	yH, high(temp2_16int)
	ld	tempL, Y+
	ld	tempH, Y
	add	r6, tempL
	adc	r7, tempH
	ldi	yL, low(tempaddr)
	ldi	yH, high(tempaddr)
	ld	xL, Y+
	ld	xH, Y		
	call	S_readByte
	mov	yL, temp
	adiw	xH:xL, 1
	call	S_readByte
	mov	yH, temp
	;ptr->intsram->dimsram
	sub	yL, r6
	sbc	yH, r7
	ldi	xL, low(tempaddr)
	ldi	xH, high(tempaddr)
	st	X+, yL
	st	X, yH
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	ld	xL, Y+
	ld	xH, Y
	ret
ltdvar3:
	adiw	xH:xL, 1
	call	S_readByte
	mov	tempL, temp
	adiw	xH:xL, 1
	call	S_readByte
	mov	tempH, temp	
	adiw	xH:xL, 1
	call	S_readByte
	cpi	temp, 0x4A
	breq	ltdok6
	jmp	syn_err
ltdok6:	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	st	Y+, xL
	st	Y, xH
	mov	xL, tempL
	mov	xH, tempH
	call	S_readByte
	mov	tempL, temp
	adiw	xH:xL, 1
	call	S_readByte
	mov	tempH, temp
	ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)
	st	Y+, tempL
	st	Y, tempH
	rjmp	ltd_ent2
lt_err3:
lt_str:
	rjmp	syn_err
lt_rdp:
	ldi	count, 0x7F
	adiw	xH:xL, 1
	call	S_readByte
	cpi	temp, 0x49
	brne	lt_err3
	ldi	yl, low(in_buf)
	ldi	yH, high(in_buf)
rdp_lp1:
	adiw	xH:xL, 1
	call	S_readByte
	cpi	temp, 0x4A
	breq	lt_rdp2
	dec	count
	breq	lt_err3
	st	Y+, temp
	rjmp	rdp_lp1
lt_rdp2:
	ldi	temp, 0x7F
	st	Y+, temp
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	st	Y+, xL
	st	Y, xH
	ldi	xL, low(in_buf)
	ldi	xH, high(in_buf)
	rcall	sy_eval
	ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)
	ld	tempL, Y+
	ld	tempH, Y
	tst	tempH
	brne	rdp_err	
	cpi	tempL, 0x0A
	brsh	rdp_err
	rjmp	lt_rdp3
rdp_err:
	sbr	flags, (1 << err)
	ldi	xL, low(errno)
	ldi	xH, high(errno)
	ldi	temp, 0xF7
	st	X, temp
	rjmp	handle_error
lt_rdp3:
	cpi	tempL, 0x0a
	brsh	rdp_err
	cpi	tempL, 0x08
	breq	rdp_pb0
	cpi	tempL, 0x09
	breq	rdp_pb1
	cpi	tempL, 0x00
	breq	rdp_pd0
	cpi	tempL, 0x01
	breq	rdp_pd1
	cpi	tempL, 0x02
	breq	rdp_pd2
	cpi	tempL, 0x03
	breq	rdp_pd3
	cpi	tempL, 0x04
	breq	rdp_pd4
	cpi	tempL, 0x05
	breq	rp5jmp
	cpi	tempL, 0x06
	breq	rp6jmp
	in	temp2, 0x09
	ldi	temp, (1 << 7)
	and	temp, temp2
	breq	rdp7f
	ldi	tempL, 1
	rjmp	rdp_done
rp5jmp:	rjmp	rdp_pd5
rp6jmp:	rjmp	rdp_pd6
rdp7f:	ldi	tempL, 0
	rjmp	rdp_done
rdp_pb0:
	in	temp2, 0x03
	ldi	temp, (1 << 0)
	and	temp, temp2
	breq	rdpb0f
	ldi	tempL, 1
	rjmp	rdp_done
rdpb0f:	ldi	tempL, 0
	rjmp	rdp_done
rdp_pb1:
	in	temp2, 0x03
	ldi	temp, (1 << 1)
	and	temp, temp2
	breq	rdpb1f
	ldi	tempL, 1
	rjmp	rdp_done
rdpb1f:	ldi	tempL, 0
	rjmp	rdp_done
rdp_pd0:
	in	temp2, 0x09
	ldi	temp, (1 << 0)
	and	temp, temp2
	breq	rdpd0f
	ldi	tempL, 1
	rjmp	rdp_done
rdpd0f:	ldi	tempL, 0
	rjmp	rdp_done
rdp_pd1:
	in	temp2, 0x09
	ldi	temp, (1 << 1)
	and	temp, temp2
	breq	rdpd1f
	ldi	tempL, 1
	rjmp	rdp_done
rdpd1f:	ldi	tempL, 0
	rjmp	rdp_done
rdp_pd2:
	in	temp2, 0x09
	ldi	temp, (1 << 2)
	and	temp, temp2
	breq	rdpd2f
	ldi	tempL, 1
	rjmp	rdp_done
rdpd2f:	ldi	tempL, 0
	rjmp	rdp_done
rdp_pd3:
	in	temp2, 0x09
	ldi	temp, (1 << 3)
	and	temp, temp2
	breq	rdpd3f
	ldi	tempL, 1
	rjmp	rdp_done
rdpd3f:	ldi	tempL, 0
	rjmp	rdp_done
rdp_pd4:
	in	temp2, 0x09
	ldi	temp, (1 << 4)
	and	temp, temp2
	breq	rdpd4f
	ldi	tempL, 1
	rjmp	rdp_done
rdpd4f:	ldi	tempL, 0
	rjmp	rdp_done
rdp_pd5:
	in	temp2, 0x09
	ldi	temp, (1 << 5)
	and	temp, temp2
	breq	rdpd5f
	ldi	tempL, 1
	rjmp	rdp_done
rdpd5f:	ldi	tempL, 0
	rjmp	rdp_done
	cbi	0x0A, 5
	rjmp	rdp_done
rdp_pd6:
	in	temp2, 0x09
	ldi	temp, (1 << 6)
	and	temp, temp2
	breq	rdpd6f
	ldi	tempL, 1
	rjmp	rdp_done
rdpd6f:	ldi	tempL, 0
rdp_done:
	ldi	yL, low(tempaddr)
	ldi	yH, high(tempaddr)
	ld	xL, Y+
	ld	xH, Y
	mov	temp, tempL
	call	S_writeByte
	ldi	temp, 0
	adiw	xH:xL, 1
	call	S_writeByte
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	ld	xL, Y+
	ld	xH, Y
	adiw	xH:xL, 1
	call	S_readByte
	rjmp	rc_ns
ltpk_er2:
	sbr	flags, (1 << err)
	ldi	yL, low(errno)
	ldi	yH, high(errno)
	ldi	temp, 0xFE
	st	Y, temp
	rjmp	handle_error
ltpk_er1:
	jmp	syn_err
lt_peek:
	adiw	xH:xL, 1
	call	S_readByte
	cpi	temp, 0x49	;'('
	breq	ltpk_n1
	rjmp	ltpk_er1
ltpk_n1:
	ldi	yL, low(in_buf)
	ldi	yH, high(in_buf)
	ldi	count, 0x10
peek_lp1:
	adiw	xH:xL, 1
	call	S_readByte
	cpi	temp, 0x80
	brlo	peek_dn1
	cpi	temp, 0x90
	brsh	peek_dn1
	st	Y+, temp
	dec	count
	breq	ltpk_er1
	rjmp	peek_lp1
peek_dn1:
	ldi	temp, 0x7F
	st	Y, temp
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	st	Y+, xL
	st	Y, xH
	ldi	xL, low(in_buf)
	ldi	xH, high(in_buf)
	rcall	a2u
	ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)
	ld	xL, Y+
	ld	xH, Y
	tst	xH
	breq	peek_d2
	rjmp	ltpk_er2
peek_d2:
	ld	tempL, X		;load tempL with value at sram addr 00xx	
	ldi	yL, low(tempaddr)
	ldi	yH, high(tempaddr)
	ld	xL, Y+
	ld	xH, Y
	mov	temp, tempL
	call 	S_writeByte
	ldi	temp, 0
	adiw	xH:xL, 1
	call	S_writeByte
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	ld	xL, Y+
	ld	xH, Y
	adiw	xH:xL, 1
	call	S_readByte
	rjmp	rc_ns

dim:
	adiw	xH:xL, 1
	call	S_readByte
	cpi	temp, 0x70
	breq	dm_int
	cpi	temp, 0x71
	breq	dm_strj
dm_err1:
	rjmp	syn_err
dm_strj:
	rjmp	dm_str
dm_int:
	ldi	yL, low(tempaddr)
	ldi	yH, high(tempaddr)
;todo rework pointer to pointer int SRAM->dim SRAM
	adiw	xH:xL, 1
	call	S_readByte
	st	Y+, temp
	adiw	xH:xL, 1
	call	S_readByte
	st	Y, temp			;store int var ptr in tempaddr
	adiw	xH:xL, 1
	call	S_readByte
	cpi	temp, 0x49		; '(' token
	breq	dmok1
	rjmp	syn_err
dmok1:	ldi	count, 0x50
	ldi	yL, low(in_buf)
	ldi	yH, high(in_buf)
dm_lp1:	adiw	xH:xL, 1
	call	S_readByte
	cpi	temp, 0x51
	breq	dmdon2
	cpi	temp, 0x4A
	breq	dmdon2
	dec	count
	breq	dm_err1
	st	Y+, temp
	rjmp	dm_lp1
dmdon2:	
	mov	tuk, temp	
dmok2:	ldi	temp, 0x7f
	st	Y, temp
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	st	Y+, xL
	st	Y, xH
	ldi	xL, low(tmp_tok)
	ldi	xH, high(tmp_tok)
	st	X, tuk			;preserve ',' or ')' token
	ldi	xL, low(in_buf)
	ldi	xH, high(in_buf)
	rcall	sy_eval
	ldi	xL, low(temp_16int)
	ldi	xH, high(temp_16int)
	ld	tempL, X+
	ld	tempH, X
	tst	tempH
	breq	dmok4
	rjmp	dm_err3
dmok4:	ldi	xL, low(tempaddr)
	ldi	xH, high(tempaddr)
	ld	r0, X+
	ld	r1, X			;y has int var ptr
	ldi	xL, low(dimptr)
	ldi	xH, high(dimptr)
	ld	yL, X+
	ld	yH, X
	mov	xL, r0
	mov	xH, r1
	mov	temp, yL
	call	S_writeByte
	adiw	xH:xL, 1
	mov	temp, yH
	call	S_writeByte		;the address int var points to now has pointer to dim sram
	mov	xL, yL
	mov	xH, yH
	mov	temp, tempL
	call	S_writeByte
	sbiw	xH:xL, 1		;do this in reverse
	mov	temp, tempH
	call	S_writeByte	
	ldi	yL, low(temp4_16)
	ldi	yH, high(temp4_16)
	st	Y+, tempL
	st	Y, tempH
	ldi	yL, low(tmp_tok)
	ldi	yH, high(tmp_tok)
	ld	temp, Y
	cpi	temp, 0x51
	breq	dm_err2
	ldi	temp, 0
	sbiw	xH:xL, 1
	call	S_writeByte
	sbiw	xH:xL, 1
	call	S_writeByte		;write 00 00 to next spot in SRAM 
	rjmp	dm_rdon
dm_err2:
	rjmp	syn_err
dm_err3:
	sbr	flags, (1 << err)
	ldi	xL, low(errno)
	ldi	xH, high(errno)
	ldi	temp, 0xFE
	st	X, temp
	jmp	handle_error
;todo clean up 2 dim
dm_more:
	ldi	yL, low(tempaddr)
	ldi	yH, high(tempaddr)
	st	Y+, xL
	st	Y, xH			;store sram address
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	ld	xL, Y+
	ld	xH, Y
	ldi	count, 0x50
	ldi	yL, low(in_buf)
	ldi	yH, high(in_buf)
dm_lp2:	adiw	xH:xL, 1
	call	S_readByte
	cpi	temp, 0x4A
	breq	dmdon3
	dec	count
	breq	dm_err2
	st	Y+, temp
	rjmp	dm_lp2
dmdon3:	
	adiw	xH:xL, 1
	call	S_readByte
	cpi	temp, 0x7F
	breq	dmdnok2
	cpi	temp, 0x4B
	breq	dmdnok2
	st	Y+, temp
	rjmp	dm_lp2
dmdnok2:
	sbiw	xH:xL, 1
	ldi	temp, 0x7f
	st	Y, temp
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	st	Y+, xL
	st	Y, xH
	ldi	xL, low(tmp_tok)
	ldi	xH, high(tmp_tok)
	st	X, tuk			;preserve ',' or ')' token
	ldi	xL, low(in_buf)
	ldi	xH, high(in_buf)
	call	sy_eval
	ldi	xL, low(temp_16int)
	ldi	xH, high(temp_16int)
	ld	tempL, X+
	ld	tempH, X
	tst	tempH
	breq	dmok6
	rjmp	dm_err3
dmok6:	ldi 	yL, low(tempaddr)
	ldi	yH, high(tempaddr)
	ld	xL, Y+
	ld	xH, Y
	sbiw	xH:xL, 1
	mov	temp, tempL
	call	S_writeByte
	sbiw	xH:xL, 1
	mov	temp, tempH
	call	S_writeByte
	rjmp	dm_rdon2
dm_rdon:
	;one dimension length is first value	
	ldi	yL, low(dimptr)
	ldi	yH, high(dimptr)
	ld	r0, Y+
	ld	r1, Y
	sub	r0, tempL
	sbc 	r1, tempH
	st	Y, r1
	st	-Y, r0
	rjmp	dm_exit
dm_rdon2:
	ldi	yL, low(temp4_16)
	ldi	yH, high(temp4_16)
	ld	tempL, Y+		;temp4_16 has first dim
	ld	tempH, Y
	ldi	yL, low(temp2_16int)
	ldi	yH, high(temp2_16int)
	st	Y+, tempL
	st	Y, tempH
	call	mul16		;mul16 takes operands in temp_16int and temp2_16int
				; temp16_int has already been loaded from last sy_eval call
	ldi	yL, low(dimptr)
	ldi	yH, high(dimptr)
	ld	r0, Y+
	ld	r1, Y
	sub	r0, r6		;mul16 returns with result in r7:r6 H:L
	sbc	r1, r7
	st	Y, r1
	st	-Y, r0
dm_exit:
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	ld	xL, Y+
	ld	xH, Y
	adiw	xH:xL, 1
	call	S_readByte
	jmp	rc_ns	
dm_str:
;todo
	ret
;*** run jump table ***
syn_err2:
	jmp	syn_err
exe_jtab:
	rjmp	let
	rjmp	dim
	rjmp	goto
	rjmp	gosub
	rjmp	return
	rjmp	print
	rjmp	pop
	rjmp	for
	rjmp	syn_err2		;to
	rjmp	syn_err2		;step
	rjmp	next
	rjmp	if
	rjmp	syn_err2
	rjmp	end
	rjmp	stop
	rjmp	input
	rjmp	data
	rjmp	read
	rjmp	rem
	rjmp	setin
	rjmp	setout
	rjmp	pinhi
	rjmp	pinlo
	rjmp	clrpin
	rjmp	rdpin
	rjmp	peek
	rjmp	poke
	rjmp	store
	rjmp	retrive
	rjmp	tgpin
goto:
;start of new
	ldi	count, 0x7F
	ldi	yl, low(in_buf)
	ldi	yH, high(in_buf)
gt_lp1:
	adiw	xH:xL, 1
	rcall	S_readByte
	cpi	temp, 0x7F
	breq	gt2
	dec	count
	breq	gt_err2
	st	Y+, temp
	rjmp	gt_lp1
gt_err2:
	jmp	syn_err
gt2:
	ldi	temp, 0x7F
	st	Y+, temp
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	st	Y+, xL
	st	Y, xH
	ldi	xL, low(in_buf)
	ldi	xH, high(in_buf)
	rcall	sy_eval
	ldi	xL, low(temp_16int)
	ldi	xH, high(temp_16int)
	ld	qlineL, X+
	ld	qlineH, X
	ldi	yL, low(ppmem)
	ldi	yH, high(ppmem)
gt12:	adiw	yH:yL, 2
	ld	xL, Y+
	ld	xH, Y
gt122:	ld	tempL, X+
	ld	tempH, X+
;	mov	byte_trx, qlineL
;	rcall	transmit
;	mov	byte_trx, tempL
;	rcall	transmit
	cp	qlineL, tempL
	brne	gt10
	cp	qlineH, tempH
	brne	gt10
;	mov	byte_trx, qlineH
;	rcall	transmit
;	mov	byte_trx, tempH
;	rcall	transmit
;got it
	ldi	yL, low(rnep)
	ldi	yH, high(rnep)
	st 	Y+, xL
	st 	Y, xH
	adiw	xH:xL, 2
	ld	yL, X+
	ld	yH, X
	mov	xL, yL
	mov	xH, yH		;eeprom address
	ldi	temp, 0x7F
	jmp	rc_lp2	
gt10:	ld	yL, X+
	ld	yH, X
	mov	xL, yL
	mov	xH, yH
	cpi	tempL, 0xff
	brne	gt11
	cpi	tempH, 0xff
	brne	gt11
	rjmp	gt_err
gt11:	rjmp	gt122
gt_err:
	ori	flags, (1 << err)
	ldi	xL, low(errno)
	ldi	xH, high(errno)
	ldi	temp, 0xFB
	st	X, temp
	rjmp	handle_error
gosub:
	ldi	yL, low(rnep)
	ldi	yH, high(rnep)
	ld	tempL, Y+
	ld	tempH, Y
	ldi	yL, low(gsubndx)
	ldi	yH, high(gsubndx)
	ld	index, Y
	ldi	yL, low(gsubstk)
	ldi	yH, high(gsubstk)
	add	yL, index
	brcc	gs_cc1
	inc	yH
gs_cc1:
	st	Y+, tempL	;store address of ptr to next line in gsub stack
	st	Y, tempH
	inc 	index
	inc	index
	ldi	temp, 9
	cp	index, temp
	brlo	gs_noe1
	sbr	flags, (1 << err)
	ldi	yL, low(errno)
	ldi	yH, high(errno)
	ldi	temp, 0xF9
	st	Y, temp
	rjmp	handle_error
gs_noe1:	
	ldi	yL, low(gsubndx)
	ldi	yH, high(gsubndx)
	st	Y, index
	rjmp	goto
return:
	ldi	yL, low(gsubndx)
	ldi	yH, high(gsubndx)
	ld	index, Y
	ldi	temp, 2
	sub	index, temp
	brcc	ret_ok
	sbr	flags, (1 << err)
	ldi	yL, low(errno)
	ldi	yH, high(errno)
	ldi	temp, 0xF8
	st	Y, temp
	rjmp	handle_error
ret_ok:	
	st	Y, index		;update gosub stack ptr
	ldi	yL, low(gsubstk)
	ldi	yH, high(gsubstk)
	add	yL, index
	brcc	ret_cc1
	inc 	yH
ret_cc1:
	ld	tempL, Y+
	ld	tempH, Y
	ldi	yL, low(rnep)
	ldi	yH, high(rnep)
	mov	xL, tempL
	mov	xH, tempH
	st 	Y+, xL
	st 	Y, xH
	adiw	xH:xL, 2
	ld	yL, X+
	ld	yH, X
	mov	xL, yL
	mov	xH, yH		;eeprom address
	ldi	temp, 0x7F
	jmp	rc_ns	
		
	jmp	getline	
print:
	adiw	xH:xL, 1
	rcall	S_readByte
	cpi	temp, 0x70
	breq	pr_nxt2
	cpi	temp, 0x71
	breq	pr_strv
	cpi	temp, 0x50
	breq	pr_str2
	ori	flags, (1 << err)
	ldi	xL, low(errno)
	ldi	xH, high(errno)
	ldi	temp, 0xfe
	st	X, temp
	rjmp	handle_error
pr_nxt2:
	rjmp	pr_nxt
pr_str2:
	rjmp	pr_str
pr_strv:
	adiw	xH:xL, 1
	rcall	S_readByte
	mov	yL, temp
	adiw	xH:xL, 1
	rcall	S_readByte
	mov	yH, temp
	mov	tempL, xL
	mov	tempH, xH
	mov	xL, yL
	mov	xH, yH
prstrvlp:
	rcall	S_readByte
	cpi	temp, 0
	breq	prstrv_d
	cbr	temp, (1 << 7)
	mov	byte_trx, temp
	call 	transmit
	adiw	xH:xL, 1
	rjmp	prstrvlp
prstrv_d:
	mov	xL, tempL
	mov	xH, tempH
	rjmp	prst_dn
	jmp	syn_err

pr_str:
	adiw	xL:xH, 1
	rcall	S_readByte
	cpi	temp, 0
	breq	prst_dn
	cpi	temp, 0x50
	breq	prst_dn
	cbr	temp, (1 << 7)
	mov	byte_trx, temp
	call	transmit
	rjmp	pr_str
prst_dn:
	adiw	xH:xL, 1
prdn2:	rcall	S_readByte
	cpi	temp, 0x52
	breq	p_more
	cpi	temp, 0x51
	brne	pr_rd
	ldi	byte_trx, 0x09
	call	transmit
p_more:	rjmp	print
pr_rd:	mov 	tuk, temp
	ldi	byte_trx, 0x0d
	call	transmit
	ldi	byte_trx, 0x0a
	call	transmit
	mov	temp, tuk
	jmp	rc_ns	
	jmp	syn_err	
pr_nxt:
;	ldi	byte_trx, 0x0d
;	rcall	transmit
;	ldi	byte_trx, 0x0a
;	rcall	transmit
	ldi	yL, low(in_buf)
	ldi	yH, high(in_buf)
	st	Y+, temp
prn_lp:	adiw	xH:xL, 1
	call	S_readByte
	cpi	temp, 0x7F
	breq	prnx_d1
	cpi	temp, 0x4B
	breq	prnx_d1
	cpi	temp, 0x51	;","
	breq	prnx_d1
	cpi	temp, 0x52
	breq	prnx_d1
	st	Y+, temp
	rjmp	prn_lp
prnx_d1:
	ldi	temp, 0x7F
	st	Y, temp
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	st	Y+, xL
	st	Y, xH
	ldi	xL, low(in_buf)
	ldi	xH, high(in_buf)
	rcall	sy_eval
	ldi	xL, low(temp_16int)
	ldi	xH, high(temp_16int)
	ld	tempL, X+
	ld	tempH, X
	rcall	sgn2asc
	rcall	prt_num

;	adiw	xH:xL, 1
;	rcall	S_readByte
;	mov	tempL, temp
;	adiw	xH:xL, 1
;	rcall	S_readByte
;	mov	tempH, temp
;	adiw	xH:xL, 1 
;	st	Y+, xL
;	st	Y, xH
;	mov	xL, tempL
;	mov	xH, tempH
;	rcall	S_readByte
;	mov	tempL, temp
;	adiw	xH:xL, 1
;	rcall	S_readByte
;	mov	tempH, temp
;	rcall	sgn2asc
;	rcall	prt_num
;	ldi	byte_trx, 0x0d
;	rcall	transmit
;	ldi	byte_trx, 0x0a
;	rcall	transmit
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	ld	xL, Y+
	ld	xH, Y
	rjmp	prdn2
pop:
	jmp	getline	
fn_err:
	jmp	syn_err
fn_err2:
	sbr	flags, (1 << err)
	ldi	xL, low(errno)
	ldi	xH, high(errno)
	ldi	temp, 0xF6
	st	X, temp
	jmp	handle_error
for:
	adiw	xH:xL, 1
	rcall 	S_readByte
	cpi	temp, 0x70
	breq	fn_n1
	rjmp	fn_err
fn_n1:	adiw	xH:xL, 1
	rcall	S_readByte
	mov 	tempL, temp
	adiw	xH:xL, 1
	rcall	S_readByte
	mov	tempH, temp
	ldi	yL, low(frnxtndx)
	ldi	yH, high(frnxtndx)
	ld	index, Y
	ldi	temp, 32
	cp	index, temp
	brlo	fn_ok
	rjmp	fn_err2
fn_ok:	ldi	yL, low(frnxtstk)
	ldi	yH, high(frnxtstk)
	add	yL, index
	brcc	fn_cc1
	inc	yH
fn_cc1:	st	Y+, tempL
	st	Y+, tempH
	adiw	xH:xL, 1
	rcall	S_readByte
	cpi	temp, 0x40
	breq	fn_n2
	rjmp	fn_err
fn_n2:	
	ldi	count, 0x50
	mov	tempL, yL
	mov	tempH, yH	;store ptr to frnxtstk
	ldi	yL, low(temp3_16)
	ldi	yH, high(temp3_16)
	st	Y+, tempL
	st	Y, tempH
	ldi	yL, low(in_buf)
	ldi	yH, high(in_buf)	
fn_lp1:	adiw 	xH:xL, 1
	rcall	S_readByte
	cpi	temp, 0x18
	breq	fn_d1
	st	Y+, temp
	dec	count
	breq	fn_errj
	rjmp	fn_lp1
fn_errj:
	rjmp	fn_err
fn_d1:	ldi	temp, 0x7F
	st	Y, temp
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	st	Y+, xL
	st	Y, xH
	ldi	xL, low(in_buf)
	ldi	xH, high(in_buf)
	rcall	sy_eval
	ldi	xL, low(temp3_16)
	ldi	xH, high(temp3_16)
	ld	yL, X+
	ld	yH, X
	ldi	xL, low(temp_16int)
	ldi	xH, high(temp_16int)
	ld	tempL, X+
	ld	tempH, X	;36
	sbiw	yH:yL, 2
	ld	xL, Y+
	ld	xH, Y+		; var int eeprom pointer
	mov	temp, tempL
	rcall	S_writeByte
	adiw	xH:xL, 1
	mov	temp, tempH
	rcall	S_writeByte	;store initial for value in variable
	ldi	xL, low(rndx)
	ldi	xH, high(rndx)
	ld	tempL, X+
	ld	tempH, X
	mov	xL, tempL
	mov	xH, tempH
;do it again for next val
	ldi	count, 0x50
	mov	tempL, yL
	mov	tempH, yH	;store ptr to frnxtstk
	ldi	yL, low(temp3_16)
	ldi	yH, high(temp3_16)
	st	Y+, tempL
	st	Y, tempH
	ldi	yL, low(in_buf)
	ldi	yH, high(in_buf)	
fn_lp2:	adiw 	xH:xL, 1
	rcall	S_readByte
	cpi	temp, 0x19
	breq	fn_d2
	cpi	temp, 0x4B	;':'
	breq	fn_d2
	cpi	temp, 0x7F
	breq	fn_d2
	st	Y+, temp
	dec	count
	breq	fn_errj2
	rjmp	fn_lp2
fn_errj2:
	rjmp	fn_err
fn_d2:	
	mov 	tuk, temp
	ldi	temp, 0x7F
	st	Y, temp
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	st	Y+, xL
	st	Y, xH
	ldi	yL, low(tmp_tok)
	ldi	yH, high(tmp_tok)
	st	Y, tuk
	ldi	xL, low(in_buf)
	ldi	xH, high(in_buf)
	rcall	sy_eval
	ldi	xL, low(temp3_16)
	ldi	xH, high(temp3_16)		;has frnxtstk
	ld	yL, X+
	ld	yH, X
	ldi	xL, low(temp_16int)
	ldi	xH, high(temp_16int)
	ld	tempL, X+
	ld	tempH, X	;36
	st	Y+, tempL
	st	Y+, tempH
	ldi	xL, low(rndx)
	ldi	xH, high(rndx)
	ld	tempL, X+
	ld	tempH, X
	mov	xL, tempL
	mov	xH, tempH
	ldi	count, 0x50
	mov	tempL, yL
	mov	tempH, yH	;store ptr to frnxtstk
	ldi	yL, low(temp3_16)
	ldi	yH, high(temp3_16)
	st	Y+, tempL
	st	Y, tempH
	ldi	yL, low(tmp_tok)
	ldi	yH, high(tmp_tok)
	ld	tuk, Y
	ldi	temp, 0x7F
	cp	tuk, temp
	breq	step_no
	rjmp	step_yes
step_no:
	mov	yL, tempL
	mov	yH, tempH
	ldi	temp, 0x01
	st	Y+, temp
	ldi	temp, 0x00
	st	Y+, temp
	mov	tempL, yL
	mov	tempH, yH
	ldi	yL, low(temp3_16)
	ldi	yH, high(temp3_16)
	st	Y+, tempL
	st	Y+, tempH
	rjmp	fn_d5	

;last time for step
step_yes:
	ldi	yL, low(in_buf)
	ldi	yH, high(in_buf)	
fn_lp3:	adiw 	xH:xL, 1
	rcall	S_readByte
	cpi	temp, 0x4B
	breq	fn_d3
	cpi	temp, 0x7F
	breq	fn_d3
	st	Y+, temp
	dec	count
	breq	fn_errj3
	rjmp	fn_lp3
fn_errj3:
	rjmp	fn_err
fn_d3:	
	ldi	temp, 0x7F
	st	Y, temp
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	st	Y+, xL
	st	Y, xH
	ldi	xL, low(in_buf)
	ldi	xH, high(in_buf)
	rcall	sy_eval
	ldi	xL, low(temp3_16)
	ldi	xH, high(temp3_16)
	ld	yL, X+
	ld	yH, X
	ldi	xL, low(temp_16int)
	ldi	xH, high(temp_16int)
	ld	tempL, X+
	ld	tempH, X	
	st	Y+, tempL
	st	Y+, tempH		;store step val in frnxtstk
	ldi	xL, low(rndx)
	ldi	xH, high(rndx)
	ld	tempL, X+
	ld	tempH, X
	mov	xL, tempL
	mov	xH, tempH
fn_d4:	mov	tempL, yL
	mov	tempH, yH
fn_d5:	ldi	yL, low(rnep)
	ldi	yH, high(rnep)
	ld	temp, Y+
	ld	temp2, Y+
	mov	yL, tempL
	mov	yH, tempH
	st	Y+, temp
	st	Y, temp2		;should store fornext line next line ptr address	
	ldi	yL, low(frnxtndx)
	ldi	yH, high(frnxtndx)
	ld	temp, Y
	ldi	temp2, 0x08
	add	temp, temp2
	st	Y, temp
	ldi	temp, 0x7F
	jmp	rc_ns
	
next:
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	st	Y+, xL
	st	Y, xH
	ldi	yL, low(frnxtndx)
	ldi	yH, high(frnxtndx)
	ld	index, Y
	ldi	yL, low(frnxtstk)
	ldi	yH, high(frnxtstk)
	add	yL, index
	brcc	nxt_cc1
	inc	yH
nxt_cc1:
	sbiw	yH:yL, 8
	ld	xL, Y+		
	ld	xH, Y+
	ld	r2, Y+
	ld	r3, Y+
	ld	r4, Y+
	ld	r5, Y+
	rcall	S_readByte
	mov	r0, temp
	adiw	xH:xL, 1
	rcall	S_readByte
	mov	r1, temp
;	mov	byte_trx, r0
;	call	transmit
;	mov	byte_trx, r1
;	call	transmit
;	mov	byte_trx, r5
;	call 	transmit
	tst	r5
	brmi	nx_sub
	rjmp	nx_add
nx_sub:
	ldi	r21, 1		;convert and subtract
	ldi	r17, 0xff
	eor	r4, r17
	add	r4, r21
	brcc	nx_cc2
	inc	r5
nx_cc2:
	eor	r5, r17
	sub	r0, r4
	sbc 	r1, r5
	brvs	ns_oe
	cp	r1, r3		;switch for brge
	breq	nx_cpl
	brlt	nx_done1
	rjmp	nx_st1
nx_cpl:	cp	r0, r2
	breq	nx_done1
	brlt	nx_done1
	rjmp	nx_st1
nx_done1:
	rjmp	nx_done3
ns_oe:	sbr	flags, (1 << err)
	ldi	xL, low(errno)
	ldi	xH, high(errno)
	ldi	temp, 0xFE
	st	X, temp
	jmp	handle_error
nx_add:
	add	r0, r4
	adc	r1, r5
	brvs	ns_oe
	cp	r1, r3
	breq	nx_cpl2
	brge	nx_done2
	rjmp	nx_st1
nx_cpl2:
	cp	r0, r2
	brge	nx_done2
	rjmp	nx_st1
nx_done2:
	rjmp	nx_done3
nx_st1:
	ld	tempL, Y+	;get return address
	ld	tempH, Y+
	sbiw	yH:yL, 8
	ld	xL, Y+
	ld	xH, Y	
	mov	temp, r0
	rcall	S_writeByte
	adiw	xH:xL, 1
	mov	temp, r1
	rcall	S_writeByte
	ldi	xL, low(rnep)
	ldi	xH, high(rnep)
	st	X+, tempL
	st	X, tempH
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	ld	xL, Y+
	ld	xH, Y	
	ldi	temp, 0x7F
	jmp	rc_ns
nx_done3:
	ldi	yL, low(frnxtndx)
	ldi	yH, high(frnxtndx)
	ld	index, Y
	ldi	temp, 8
	sub	index, temp
	brcc	nx_ok2
	sbr	flags, (1 << err)
	ldi	xL, low(errno)
	ldi	xH, high(errno)
	ldi	temp, 0xF5
	st 	X, temp
	jmp	handle_error
nx_ok2:	
	st	Y, index
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	ld	xL, Y+
	ld	xH, Y
	ldi	temp, 0x7F
	jmp	rc_ns
	;add goes here

	jmp	getline	
if:
	clr	index
	adiw	xH:xL, 1
if2:	rcall	S_readByte
	cpi	temp, 0x49	;'('
	breq	ifnum		;treat as number exp
	cpi	temp, 0x41	;'-'
	breq	ifnum
	cpi	temp, 0x70
	breq	ifintv
	cpi	temp, 0x71
	breq	ifstr
	cpi	temp, 0x50
	breq	ifstr
	cpi	temp, 0x80
	brlo	ifch1
	cpi	temp, 0x90
	brlo	ifnum
ifch1:	rjmp	if_err
ifintv:
	rjmp	r_ifnum2
ifnum:
	rjmp	r_ifnum2
ifstr:	cbr 	flags, (1 << str)
	cpi	temp, 0x71
	breq	ifstv
	adiw	xH:xL, 1
	mov	tempL, xL
	mov	tempH, xH	;tempH:L has ptr to string
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	st	Y+, xL
	st	Y, xH		;preserve line ptr
	rjmp	ifstr1
ifstv:	adiw	xH:xL, 1
	rcall	S_readByte
	mov	tempL, temp
	adiw	xH:xL, 1
	rcall	S_readByte
	mov	tempH, temp
	adiw	xH:xL, 1
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	st	Y+, xL
	st	Y, xH
ifstr1:	mov	xL, tempL
	mov	xH, tempH
	ldi	yL, low(in_buf)
	ldi	yH, high(in_buf)
istrlp1:
	rcall	S_readByte
	adiw 	xH:xL, 1
	cpi	temp, 0
	breq	ifstr1d
	cpi	temp, 0x50
	breq	ifstr2d
	st	Y+, temp
	rjmp	istrlp1
ifstr2d:
	adiw	xH:xL, 1
	ldi	temp, 0
	st	-Y, temp
	rjmp	ifstr3
ifstr1d:
	st	Y, temp
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	ld	xL, Y+
	ld	xH, Y
ifstr3:
	rcall	S_readByte
;	mov	byte_trx, temp
;	rcall	transmit
;	mov	temp, byte_trx
	cpi	temp, 0x40
	breq	ifstr2
	cpi	temp, 0x48	;'#'
	breq	ifstr2
	rjmp	if_err
ifstr2:	
	mov	temp2, temp
	adiw	xH:xL, 1
	rcall	S_readByte
	cpi	temp, 0x71
	breq	ifstv2
	cpi	temp, 0x50
	breq	ifstl2
	rjmp	if_err
ifstl2:
	adiw	xH:xL, 1
	mov	tempL, xL
	mov	tempH, xH
	ori	flags, (1 << str)
	rjmp	ifstr5
ifstv2:
;	ldi	byte_trx, 0xea
;	rcall	transmit
	adiw	xH:xL, 1
	rcall	S_readByte
	mov	tempL, temp
	adiw	xH:xL, 1
	rcall	S_readByte
	mov	tempH, temp
	adiw	xH:xL, 1
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	st	Y+, xL
	st	Y, xH	
ifstr5:
	ldi	yL, low(in_buf)
	ldi	yH, high(in_buf)
	mov	xL, tempL
	mov	xH, tempH
ifst3lp:
	ld	r0, Y+
	rcall	S_readByte
	adiw	xH:xL, 1
	mov	r1, temp
;	mov	byte_trx, r0
;	rcall	transmit
;	mov	byte_trx, r1
;	rcall	transmit
	ldi	temp, 0x50
	cp	r1, temp
	brne	ifstrn4
	ldi	temp, 0
	mov	r1, temp
ifstrn4:
	cp	r0, r1
	brne	ifstrno	
	ldi	temp, 0
	cp	r0, temp
	breq	ifs_done
	rjmp	ifst3lp	
ifstrno:
	sbrs	flags, str
	rjmp	ifsno4
	cbr	flags, (1 << str)
	ldi	temp, 0
	cp	r1, temp
	breq	ifsno3
ifsnolp:
	adiw	xH:xL, 1
	rcall	S_readByte
	cpi	temp, 0x50
	breq	ifsno4e
	rjmp	ifsnolp		;read out string
ifsno4e:
	adiw	xH:xL, 1
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	st	Y+, xL
	st	Y, xH
ifsno4:
	cpi	temp2, 0x48
	breq	ifsno2
	ldi	tempL, 0
	rjmp	ift_ent2
ifsno2:
	ldi	tempL, 1
	rjmp	ift_ent2	
ifs_done:
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	st	Y+, xL
	st	Y, xH
	cpi	temp2, 0x48
	breq	ifsno3
	ldi	tempL, 1
	rjmp	ift_ent2
ifsno3:	ldi	tempL, 0
	rjmp	ift_ent2
ift_ent2:
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	ld	xL, Y+
	ld	xH, Y
	ldi	yL, low(tmp_tok2)
	ldi	yH, high(tmp_tok2)
	rcall	S_readByte
	st	Y, temp	
	rjmp	ift_ent
iflstr:
	jmp	r_iflstr
r_ifintv:
	jmp	getline
r_ifstr:
	jmp	getline
	ret
r_iflstr:
	jmp	getline
	ret
iflpns:
	push	xL
	push	xH
	ldi	xL, low(eqp)
	ldi	xH, high(eqp)
	ld	index, X
	ldi	xL, low(evalq)
	ldi	xH, high(evalq)
	add	xL, index
	brcc	ipnscc
	inc	xH
ipnscc:
	st	X, temp
	inc	index
	ldi	xL, low(eqp)
	ldi	xH, high(eqp)
	st	X, index
	pop	xH
	pop	xL
	rjmp	if
if_err:
	jmp	syn_err
r_ifnum:
	adiw	xH:xL, 1
	rcall	S_readByte
r_ifnum2:
	ldi	count, 0x74
	ldi	yL, low(in_buf)
	ldi	yH, high(in_buf)
	st	Y+, temp
ifn_lp:	adiw	xH:xL, 1
	rcall	S_readByte
	cpi	temp, 0x40
	breq	ifn_dn
	cpi	temp, 0x46
	breq	ifn_dn2			;test for >=
	cpi	temp, 0x47
	breq	ifn_dn3
	cpi	temp, 0x48
	breq	ifn_dn
	cpi	temp, 0x1C
	breq	ifn_dn
	dec	count
	breq	if_err
	st	Y+, temp
	rjmp	ifn_lp
;todo check iflog
ifn_dn2:
	mov	tempL, temp
	adiw	xH:xL, 1
	rcall	S_readByte
	cpi	temp, 0x40
	breq	ifgte
	mov	temp, tempL
	ldi	temp2, 1
	sub	xL, temp2
	brcc	ifdcc
	dec	xH
ifdcc:	rjmp	ifn_dn	
ifgte:	ldi	temp, 0x60		;has special meaning here '>='
	rjmp	ifn_dn
ifn_dn3:
	mov	tempL, temp
	adiw	xH:xL, 1
	rcall	S_readByte
	cpi	temp, 0x40
	breq	iflte
	mov	temp, tempL
	ldi	temp2, 1
	sub	xL, temp2
	brcc	ifdcc2
	dec	xH
ifdcc2:	rjmp	ifn_dn
iflte:	ldi	temp, 0x61		;special token '<='
ifn_dn:
	ldi	temp2, 0x7F		;terminate for eval
	st	Y, temp2
	ldi	yL, low(tmp_tok)
	ldi	yH, high(tmp_tok)
	st	Y, temp	
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	st	Y+, xL
	st	Y, xH
	ldi	xL, low(in_buf)
	ldi	xH, high(in_buf)
	call	sy_eval
	ldi	xL, low(temp_16int)
	ldi	xH, high(temp_16int)
	ld	temp, X+
	ld	temp2, X
	ldi	xL, low(temp3_16)
	ldi	xH, high(temp3_16)
	st	X+, temp
	st	X, temp2
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	ld	xL, Y+
	ld	xH, Y
	ldi	count, 0x74
	ldi	yL, low(in_buf)
	ldi	yH, high(in_buf)
ifnlp2:
	adiw	xH:xL, 1
	rcall	S_readByte
	cpi	temp, 0x1C	;then
	breq	ifnd4
	cpi	temp, 0x4D	;and
	breq	ifnd4
	cpi	temp, 0x4E	;or
	breq	ifnd4
	cpi	temp, 0x4F	;not
	breq	ifnd4
	cpi	temp, 0x49	;'('
	breq	ifnd4
	cpi	temp, 0x4a	;')'
	breq	ifnd4
	dec	count
	breq	iferr2
	st	Y+, temp
	rjmp	ifnlp2
iferr2:	jmp	syn_err
ifnd4:	
	ldi	temp2, 0x7F
	st	Y, temp2
	ldi	yL, low(tmp_tok2)
	ldi	yH, high(tmp_tok2)
	st	Y, temp
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	st	Y+, xL
	st	Y, xH
	ldi	xL, low(in_buf)
	ldi	xH, high(in_buf)
	call	sy_eval
	rcall	if_eval		;should return with evaluation in tempL
ift_ent:			;entry point for string eval
	push	xL
	push	xH
	ldi	xL, low(eqp)
	ldi	xH, high(eqp)
	ld	index, X
	ldi	xL, low(evalq)
	ldi	xH, high(evalq)
	add	xL, index
	brcc	ifnd4cc
	inc	xH
ifnd4cc:
	st	X+, tempL
	ldi	yL, low(tmp_tok2)
	ldi	yH, high(tmp_tok2)
	ld	temp, Y
	st	X, temp		;pushes then token on stack
	pop	xH
	pop	xL 
	cpi	temp, 0x1C
	breq	if_then
	inc	index
	inc 	index
	ldi	xL, low(eqp)
	ldi	xH, high(eqp)
	st	X, index
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	ld	xL, Y+
	ld	xH, Y
	rjmp	if
if_then:
	ldi	xL, low(evalq)
	ldi	xH, high(evalq)
	ld	char, X+
ifthlp:	ld	temp, X+
;	mov	byte_trx, char
;	rcall	transmit
;	mov	byte_trx, char2
;	rcall	transmit
	ld	char2, X+
;	mov	byte_trx, temp
;	rcall	transmit
;	mov	temp, byte_trx
	cpi	temp, 0x1C
	breq	if_dnj
	cpi	temp, 0x4D
	breq	ift_and
	cpi	temp, 0x4E
	breq	ift_or
	cpi	temp, 0x4F
	breq	ift_not
	jmp	syn_err
if_dnj:	rjmp	if_done
ift_and:
	and	char, char2
	breq	ifand1
	ldi	temp, 1 
	mov	char, temp
	rjmp	ifthlp
ifand1:	ldi	temp, 0
	mov	char, temp
	rjmp	ifthlp
	rjmp	if_then
ift_or:	or	char, char2
	breq	ifor1
	ldi	temp, 1
	mov	char, temp
	rjmp	ifthlp
ifor1:	ldi	temp, 0	
	mov	char, temp
	rjmp	ifthlp
ift_not:
	eor	char, char2
	breq	ifnot1
	ldi	temp, 1
	mov	char, temp
	rjmp	ifthlp
ifnot1:	ldi	temp, 0
	mov	char, temp
	rjmp	ifthlp
if_done:
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	ld	xL, Y+
	ld	xH, Y
	ldi	temp, 0x01
	cp	char, temp
	breq	if_true
	ldi	temp, 0x7F
	jmp	rc_ns
if_true:
	ldi	temp, 0x4B
	jmp	rc_ns
;stack should have operator and evalq should have eval ;
;if_eval
;takes 16ints in temp_16int and temp3_16
; returns with true or false in temp2
if_eval:
	ldi	yL, low(tmp_tok)
	ldi	yH, high(tmp_tok)
	ld	temp, Y
;	mov	byte_trx, temp
;	rcall	transmit
;	mov	temp, byte_trx
	cpi	temp, 0x40
	breq	ifeveq
	cpi	temp, 0x46
	breq	ifev_grt 
	cpi	temp, 0x47
	breq	ifev_less
	cpi	temp, 0x48
	breq	ifev_ne
	cpi	temp, 0x60
	breq	ifev_ge
	cpi	temp, 0x61
	breq	ifev_le
	jmp	syn_err
ifeveq:	rjmp	if_equ
ifev_less:
	rjmp	if_less
ifev_grt:
	rjmp	if_grt
ifev_ne:
	rjmp	if_ne
ifev_ge:
	rjmp	if_ge
ifev_le:
	rjmp	if_le
if_equ:
	ldi	yL, low(temp3_16)
	ldi	yH, high(temp3_16)
	ld	temp, Y+
	ld	temp2, Y
;	mov	byte_trx, temp
;	rcall	transmit
;	mov	temp, byte_trx
;	mov	byte_trx, temp2
;	rcall	transmit
;	mov	temp2, byte_trx
	ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)
	ld	tempL, Y+
	ld	tempH, Y
;	mov	byte_trx, tempL
;	rcall	transmit
;	mov	byte_trx, tempH
;	rcall	transmit
	cp	temp, tempL
	brne	ifeqno
	cp	temp2, tempH
	brne	ifeqno
	ldi	tempL, 1
	ret
ifeqno:	ldi	tempL, 0
	ret
if_less:
	ldi	yL, low(temp3_16)
	ldi	yH, high(temp3_16)
	ld	temp, Y+
	ld	temp2, Y
	ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)
	ld	tempL, Y+
	ld	tempH, Y
	cp	temp2, tempH
	brlt	ifloyes
	breq	iflonxt
	rjmp	iflono
iflonxt:
	cp	temp, tempL
	brlt	ifloyes
iflono:	ldi	tempL, 0
	ret
ifloyes:
	ldi	tempL, 1
	ret
if_grt:
	ldi	yL, low(temp3_16)
	ldi	yH, high(temp3_16)
	ld	temp, Y+
	ld	temp2, Y
	ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)
	ld	tempL, Y+
	ld	tempH, Y
	cp	temp2, tempH
	brlt	ifgrno
	breq	ifgrnxt
ifgryes:
	ldi	tempL, 1
	ret
ifgrno:
	ldi	tempL, 0
	ret
ifgrnxt:
	cp	temp, tempL
	brlt	ifgrno
	breq	ifgrno
	rjmp	ifgryes
if_ne:
	ldi	yL, low(temp3_16)
	ldi	yH, high(temp3_16)
	ld	temp, Y+
	ld	temp2, Y
	ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)
	ld	tempL, Y+
	ld	tempH, Y
	cp	temp, tempL
	breq	ifneno
	cp	temp2, tempH
	breq	ifneno
	ldi	tempL, 1
	ret
ifneno:
	ldi	tempL, 0
	ret
if_ge:
	ldi	yL, low(temp3_16)
	ldi	yH, high(temp3_16)
	ld	temp, Y+
	ld	temp2, Y
	ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)
	ld	tempL, Y+
	ld	tempH, Y
	cp	temp2, tempH
	brge	ifgenxt
ifgeno:
	ldi	tempL, 0
	ret
ifgenxt:
	cp	temp, tempL
	brge	ifgeyes
	rjmp	ifgeno
ifgeyes:
	ldi	tempL, 1
	ret
if_le:
	ldi	yL, low(temp3_16)
	ldi	yH, high(temp3_16)
	ld	temp, Y+
	ld	temp2, Y
	ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)
	ld	tempL, Y+
	ld	tempH, Y
	cp	temp2, tempH
	breq	iflenxt1
	brlt	iflenxt1
ifleno:
	ldi	tempL, 0
	ret
iflenxt1:
	cp	temp, tempL
	breq	ifleyes
	brlt	ifleyes
	rjmp	ifleno
ifleyes:
	ldi	tempL, 1
	ret
end:	jmp	rc_end
	jmp	getline
stop:	jmp	getline
input:	ldi	byte_trx, 0xBF
	call	transmit
	jmp	getline
data:	ldi	byte_trx, 0xBA
	call	transmit
	jmp	getline
read:	jmp	getline
rem:	jmp	getline
setin:
	adiw	xH:xL, 1
	rcall	S_readByte
	cpi	temp, 0x49
	brne	sti_err
;start of new
	ldi	count, 0x7F
	ldi	yl, low(in_buf)
	ldi	yH, high(in_buf)
si_lp1:
	adiw	xH:xL, 1
	rcall	S_readByte
	cpi	temp, 0x4A
	breq	si2
	dec	count
	breq	sti_err
	st	Y+, temp
	rjmp	si_lp1
si2:
	ldi	temp, 0x7F
	st	Y+, temp
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	st	Y+, xL
	st	Y, xH
	ldi	xL, low(in_buf)
	ldi	xH, high(in_buf)
	call	sy_eval
	ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)
	ld	tempL, Y+
	ld	tempH, Y
	tst	tempH
	brne	sti_err	
	mov	temp, tempL
;end of new
	cpi	temp, 0x0a
	brsh	sti_err
	cpi	temp, 0x08
	breq	sti_pb0
	cpi	temp, 0x09
	breq	sti_pb1
	cpi	temp, 0x00
	breq	sti_pd0
	cpi	temp, 0x01
	breq	sti_pd1
	cpi	temp, 0x02
	breq	sti_pd2
	cpi	temp, 0x03
	breq	sti_pd3
	cpi	temp, 0x04
	breq	sti_pd4
	cpi	temp, 0x05
	breq	sti_pd5
	cpi	temp, 0x06
	breq	sti_pd6
	cbi	0x0A, 7		;portd pin 7
	sbi	0x0B, 7
	rjmp	sti_done
sti_err:
	ori	flags, (1 << err)
	ldi	xL, low(errno)
	ldi	xH, high(errno)
	ldi	temp, 0xF7
	st	X, temp
	jmp	handle_error
sti_pb0:
	cbi	0x04, 0
	sbi	0x05, 0
	rjmp	sti_done
sti_pb1:
	cbi	0x04, 1
	sbi	0x05, 1
	rjmp 	sti_done
sti_pd0:
	cbi	0x0A, 0
	sbi	0x0B, 0
	rjmp	sti_done
sti_pd1:
	cbi	0x0A, 1
	sbi	0x0B, 1	
	rjmp	sti_done
sti_pd2:
	cbi	0x0A, 2
	sbi	0x0B, 2
	rjmp	sti_done
sti_pd3:
	cbi	0x0A, 3
	sbi	0x0B, 3
	rjmp	sti_done
sti_pd4:
	cbi	0x0A, 4
	sbi	0x0B, 4
	rjmp	sti_done
sti_pd5:
	cbi	0x0A, 5
	sbi	0x0B, 5
	rjmp	sti_done
sti_pd6:
	cbi	0x0A, 6
	sbi	0x0B, 6
sti_done:
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	ld	xL, Y+
	ld	xH, Y
	adiw	xH:xL, 1
	call	S_readByte
	jmp	rc_ns
setout:	
	adiw	xH:xL, 1
	rcall	S_readByte
	cpi	temp, 0x49
	brne	sto_err
;start of new
	ldi	count, 0x7F
	ldi	yl, low(in_buf)
	ldi	yH, high(in_buf)
sto_lp1:
	adiw	xH:xL, 1
	rcall	S_readByte
	cpi	temp, 0x4A
	breq	sto2
	dec	count
	breq	sto_err
	st	Y+, temp
	rjmp	sto_lp1
sto2:
	ldi	temp, 0x7F
	st	Y+, temp
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	st	Y+, xL
	st	Y, xH
	ldi	xL, low(in_buf)
	ldi	xH, high(in_buf)
	call	sy_eval
	ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)
	ld	tempL, Y+
	ld	tempH, Y
	tst	tempH
	brne	sto_err	
	mov	temp, tempL
;end of new
	cpi	temp, 0x0a
	brsh	sto_err
	cpi	temp, 0x08
	breq	sto_pb0
	cpi	temp, 0x09
	breq	sto_pb1
	cpi	temp, 0x00
	breq	sto_pd0
	cpi	temp, 0x01
	breq	sto_pd1
	cpi	temp, 0x02
	breq	sto_pd2
	cpi	temp, 0x03
	breq	sto_pd3
	cpi	temp, 0x04
	breq	sto_pd4
	cpi	temp, 0x05
	breq	sto_pd5
	cpi	temp, 0x06
	breq	sto_pd6
	sbi	0x0A, 7		;portd pin 7
	rjmp	sto_done
sto_err:
	ori	flags, (1 << err)
	ldi	xL, low(errno)
	ldi	xH, high(errno)
	ldi	temp, 0xF7
	st	X, temp
	jmp	handle_error
sto_pb0:
	sbi	0x04, 0
	rjmp	sto_done
sto_pb1:
	sbi	0x04, 1
	rjmp 	sto_done
sto_pd0:
	sbi	0x0A, 0
	rjmp	sto_done
sto_pd1:
	sbi	0x0A, 1
	rjmp	sto_done
sto_pd2:
	sbi	0x0A, 2
	rjmp	sto_done
sto_pd3:
	sbi	0x0A, 3
	rjmp	sto_done
sto_pd4:
	sbi	0x0A, 4
	rjmp	sto_done
sto_pd5:
	sbi	0x0A, 5
	rjmp	sto_done
sto_pd6:
	sbi	0x0A, 6
sto_done:
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	ld	xL, Y+
	ld	xH, Y
	adiw	xH:xL, 1
	rcall	S_readByte
	jmp	rc_ns
pinhi:	
	adiw	xH:xL, 1
	rcall	S_readByte
	cpi	temp, 0x49
	brne	phi_err
;start of new
	ldi	count, 0x7F
	ldi	yl, low(in_buf)
	ldi	yH, high(in_buf)
phi_lp1:
	adiw	xH:xL, 1
	rcall	S_readByte
	cpi	temp, 0x4A
	breq	phi2
	dec	count
	breq	phi_err
	st	Y+, temp
	rjmp	phi_lp1
phi2:
	ldi	temp, 0x7F
	st	Y+, temp
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	st	Y+, xL
	st	Y, xH
	ldi	xL, low(in_buf)
	ldi	xH, high(in_buf)
	call	sy_eval
	ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)
	ld	tempL, Y+
	ld	tempH, Y
	tst	tempH
	brne	phi_err	
	mov	temp, tempL
;end of new
	cpi	temp, 0x0a
	brsh	phi_err
	cpi	temp, 0x08
	breq	phi_pb0
	cpi	temp, 0x09
	breq	phi_pb1
	cpi	temp, 0x00
	breq	phi_pd0
	cpi	temp, 0x01
	breq	phi_pd1
	cpi	temp, 0x02
	breq	phi_pd2
	cpi	temp, 0x03
	breq	phi_pd3
	cpi	temp, 0x04
	breq	phi_pd4
	cpi	temp, 0x05
	breq	phi_pd5
	cpi	temp, 0x06
	breq	phi_pd6
	sbi	0x0B, 7		;portd pin 7
	rjmp	phi_done
phi_err:
	ori	flags, (1 << err)
	ldi	xL, low(errno)
	ldi	xH, high(errno)
	ldi	temp, 0xF7
	st	X, temp
	jmp	handle_error
phi_pb0:
	sbi	0x05, 0
	rjmp	phi_done
phi_pb1:
	sbi	0x05, 1
	rjmp 	phi_done
phi_pd0:
	sbi	0x0B, 0
	rjmp	phi_done
phi_pd1:
	sbi	0x0B, 1
	rjmp	phi_done
phi_pd2:
	sbi	0x0B, 2
	rjmp	phi_done
phi_pd3:
	sbi	0x0B, 3
	rjmp	phi_done
phi_pd4:
	sbi	0x0B, 4
	rjmp	phi_done
phi_pd5:
	sbi	0x0B, 5
	rjmp	phi_done
phi_pd6:
	sbi	0x0B, 6
phi_done:
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	ld	xL, Y+
	ld	xH, Y
	adiw	xH:xL, 1
	rcall	S_readByte
	jmp	rc_ns
pinlo:	
	adiw	xH:xL, 1
	rcall	S_readByte
	cpi	temp, 0x49
	brne	plo_err
;start of new
	ldi	count, 0x7F
	ldi	yl, low(in_buf)
	ldi	yH, high(in_buf)
plo_lp1:
	adiw	xH:xL, 1
	rcall	S_readByte
	cpi	temp, 0x4A
	breq	plo2
	dec	count
	breq	plo_err
	st	Y+, temp
	rjmp	plo_lp1
plo2:
	ldi	temp, 0x7F
	st	Y+, temp
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	st	Y+, xL
	st	Y, xH
	ldi	xL, low(in_buf)
	ldi	xH, high(in_buf)
	call	sy_eval
	ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)
	ld	tempL, Y+
	ld	tempH, Y
	tst	tempH
	brne	plo_err	
	mov	temp, tempL
;end of new
	cpi	temp, 0x0a
	brsh	plo_err
	cpi	temp, 0x08
	breq	plo_pb0
	cpi	temp, 0x09
	breq	plo_pb1
	cpi	temp, 0x00
	breq	plo_pd0
	cpi	temp, 0x01
	breq	plo_pd1
	cpi	temp, 0x02
	breq	plo_pd2
	cpi	temp, 0x03
	breq	plo_pd3
	cpi	temp, 0x04
	breq	plo_pd4
	cpi	temp, 0x05
	breq	plo_pd5
	cpi	temp, 0x06
	breq	plo_pd6
	cbi	0x0B, 7		;portd pin 7
	rjmp	plo_done
plo_err:
	ori	flags, (1 << err)
	ldi	xL, low(errno)
	ldi	xH, high(errno)
	ldi	temp, 0xF7
	st	X, temp
	jmp	handle_error
plo_pb0:
	cbi	0x05, 0
	rjmp	plo_done
plo_pb1:
	cbi	0x05, 1
	rjmp 	plo_done
plo_pd0:
	cbi	0x0B, 0
	rjmp	plo_done
plo_pd1:
	cbi	0x0B, 1
	rjmp	plo_done
plo_pd2:
	cbi	0x0B, 2
	rjmp	plo_done
plo_pd3:
	cbi	0x0B, 3
	rjmp	plo_done
plo_pd4:
	cbi	0x0B, 4
	rjmp	plo_done
plo_pd5:
	cbi	0x0B, 5
	rjmp	plo_done
plo_pd6:
	cbi	0x0B, 6
plo_done:
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	ld	xL, Y+
	ld	xH, Y
	adiw	xH:xL, 1
	rcall	S_readByte
	jmp	rc_ns
clrpin:
	jmp	syn_err	
	jmp	getline
rdpin:	jmp	getline
peek:	jmp	getline
poke:	jmp	getline
store:	jmp	getline
retrive:
	jmp	getline
tgpin:	
	adiw	xH:xL, 1
	rcall	S_readByte
	cpi	temp, 0x49
	brne	tg_err
;start of new
	ldi	count, 0x7F
	ldi	yl, low(in_buf)
	ldi	yH, high(in_buf)
tg_lp1:
	adiw	xH:xL, 1
	rcall	S_readByte
	cpi	temp, 0x4A
	breq	tg2
	dec	count
	breq	tg_err
	st	Y+, temp
	rjmp	tg_lp1
tg2:
	ldi	temp, 0x7F
	st	Y+, temp
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	st	Y+, xL
	st	Y, xH
	ldi	xL, low(in_buf)
	ldi	xH, high(in_buf)
	call	sy_eval
	ldi	yL, low(temp_16int)
	ldi	yH, high(temp_16int)
	ld	tempL, Y+
	ld	tempH, Y
	tst	tempH
	brne	tg_err	
	mov	temp, tempL
;end of new
	cpi	temp, 0x0a
	brsh	tg_err
	cpi	temp, 0x08
	breq	tg_pb0
	cpi	temp, 0x09
	breq	tg_pb1
	cpi	temp, 0x00
	breq	tg_pd0
	cpi	temp, 0x01
	breq	tg_pd1
	cpi	temp, 0x02
	breq	tg_pd2
	cpi	temp, 0x03
	breq	tg_pd3
	cpi	temp, 0x04
	breq	tg_pd4
	cpi	temp, 0x05
	breq	tg_jpd5
	cpi	temp, 0x06
	breq	tg_jpd6
	in	temp, 0x0B
	ldi	temp2, (1 << 7)
	eor 	temp, temp2
	out	0x0B, temp
	rjmp	tg_done
tg_jpd5:
	rjmp	tg_pd5
tg_jpd6:
	rjmp	tg_pd6
tg_err:
	ori	flags, (1 << err)
	ldi	xL, low(errno)
	ldi	xH, high(errno)
	ldi	temp, 0xF7
	st	X, temp
	jmp	handle_error
tg_pb0:
	in	temp, 0x05
	ldi	temp2, (1 << 0)
	eor 	temp, temp2
	out	0x05, temp
	rjmp	tg_done
tg_pb1:
	in	temp, 0x05
	ldi	temp2, (1 << 1)
	eor 	temp, temp2
	out	0x05, temp
	rjmp 	tg_done
tg_pd0:
	in	temp, 0x0B
	ldi	temp2, (1 << 0)
	eor 	temp, temp2
	out	0x0B, temp
	rjmp	tg_done
tg_pd1:
	in	temp, 0x0B
	ldi	temp2, (1 << 1)
	eor 	temp, temp2
	out	0x0B, temp
	rjmp	tg_done
tg_pd2:
	in	temp, 0x0B
	ldi	temp2, (1 << 2)
	eor 	temp, temp2
	out	0x0B, temp
	rjmp	tg_done
tg_pd3:
	in	temp, 0x0B
	ldi	temp2, (1 << 3)
	eor 	temp, temp2
	out	0x0B, temp
	rjmp	tg_done
tg_pd4:
	in	temp, 0x0B
	ldi	temp2, (1 << 4)
	eor 	temp, temp2
	out	0x0B, temp
	rjmp	tg_done
tg_pd5:
	in	temp, 0x0B
	ldi	temp2, (1 << 5)
	eor 	temp, temp2
	out	0x0B, temp
	rjmp	tg_done
tg_pd6:
	in	temp, 0x0B
	ldi	temp2, (1 << 6)
	eor 	temp, temp2
	out	0x0B, temp
tg_done:
	ldi	yL, low(rndx)
	ldi	yH, high(rndx)
	ld	xL, Y+
	ld	xH, Y
	adiw	xH:xL, 1
	rcall	S_readByte
	jmp	rc_ns
;**** print number ***
; number in out_buf null term
prt_num:
	ldi	xL, low(out_buf)
	ldi	xH, high(out_buf)
	clr	tempL
	clr	count
	sbrs	flags, sgn
	rjmp	prtn_lp
	ldi	byte_trx, '-'
	call	transmit
	cbr	flags, (1 << sgn)
prtn_lp:
	inc 	count
	ld	byte_trx, X+
	cpi	byte_trx, 0
	breq	prtn_done
	tst	tempL
	brne	pgood
	cpi	byte_trx, '0'
	breq	prtn_lp
	sbr	tempL, 1
pgood:
	cpi	count, 2
	brne	pgd2
	call	transmit
	ldi	byte_trx, ','
	call	transmit
	rjmp	prtn_lp
pgd2:
	call	transmit
	rjmp	prtn_lp
prtn_done:
	tst	tempL
	breq	p_zero
	ret
p_zero:
	ldi	byte_trx, '0'
	call	transmit
	ret
;******* convert signed 16bit to ascii
; takes signed number in tempH:tempL (r24:r25)
; puts result in out_buf null terminated
;
sgn2asc:
	clr	count
	ldi	r21, 5
	ldi	xL, low(out_buf)
	ldi	xH, high(out_buf)
	sbrs	tempH, 7
	rjmp	s_nos
	ldi	temp, 0xff
	eor	tempL, temp
	ldi	temp2, 1
	add	tempL, temp2
	brcc	s_cc
	dec	tempH
s_cc:	eor	tempH, temp
	ori	flags, (1 << sgn)
; start
s_nos:
	ldi	zL, low(bcdtab << 1)
	ldi	zH, high(bcdtab << 1)
	push	tempH
	push	tempL
s2a_lp:
	lpm	temp, Z+
	lpm	temp2, Z+
tthlp:
	clc
	sub	tempL, temp
	sbc	tempH, temp2
	brcs	s_cs
	inc	count
	pop	r0
	pop	r0	
	push	tempH
	push	tempL
	rjmp	tthlp
s_cs:
	ori	count, '0'
	st 	X+, count
	clr 	count
	dec	r21
	breq	s2a_done
	pop	tempL
	pop	tempH
	push	tempH
	push	tempL
	rjmp	s2a_lp
s2a_done:
	pop	temp
	pop	temp
	ldi	temp, 0
	st	X, temp
	ret
;print string function
;takes Z loaded with pointer to string with first byte being length
;uses r16,r17,r24
print_s:
        lpm     count,Z+
for1:   lpm     byte_trx,Z+
wait:   call	transmit
        dec     count
        brne    for1
	ldi	byte_trx, 0x0d
	call	transmit
	ldi	byte_trx, 0x0a
	call	transmit
        ret
;
;uses temp r16 as tx byte and returns with rx byte in SPDR
SPI_tradeByte:
        out     SPDR,temp
lp1:    in      r12,SPSR
        sbrs    r12,SPIF
        rjmp    lp1
        ret
;---- function to send 16 bit address ---
; takes address in xH and xL registers
E_sendAddr:
        mov     temp,xH
        rcall   SPI_tradeByte
        mov     temp,xL
        rcall   SPI_tradeByte
        ret
;----- function to get EEPROM sreg ----
;returns eeprom status reg in temp r16
E_readSts:
        cbi     SPI_PORT,S_SS           ;lower SS // select slave
        ldi     temp,E_RDSR             ;read eeprom sreg instruction
        rcall   SPI_tradeByte
        ldi     temp,0x00
        rcall   SPI_tradeByte           ;clock out 8 bits while eeprom writes to SPDR
        sbi     SPI_PORT,S_SS           ;de-select Slave
        in      temp,SPDR
        ret
;---- function to set write enable ----
E_writeEnable:
        cbi     SPI_PORT,S_SS           ;lower SS = slave select
        ldi     temp,E_WREN
        rcall   SPI_tradeByte
        sbi     SPI_PORT,S_SS           ;deselct slave
        ret
;---- function to read a byte from memory address in eeprom ic ---
;takes address in x registers
E_readByte:
        cbi     SPI_PORT,S_SS           ;slave select
        ldi     temp,E_READ
        rcall   SPI_tradeByte
        rcall   E_sendAddr
        ldi     temp,0x00
        rcall   SPI_tradeByte           ;clock 8 bits to get return byte
        sbi     SPI_PORT,S_SS           ;deselect slave
        in      temp,SPDR
        ret
;----- function to write a byte to memory in eeprom ic ---
;takes address in x registers and byte to write in temp r16
E_writeByte:
        push    temp                    ;save r16 while E_writeEnable is called
        rcall   E_writeEnable
        cbi     SPI_PORT,S_SS
        ldi     temp,E_WRITE
        rcall   SPI_tradeByte
;        ldi     xL,0x00
;        ldi     xH,0x00                 ;send addr takes address in x reg
        rcall   E_sendAddr
        pop     temp
        rcall   SPI_tradeByte
        sbi     SPI_PORT,S_SS
lp2:    rcall   E_readSts
        sbrc    temp,W_INP              ;loop while write in progress bit is set
        rjmp    lp2
        ret
;--- function to read a string of bytes from eeprom
;--- and put them in memory
;--- takes address to read in x registers
;--- and uses y registers as pointers to write address
;--- length byte in count r17
E_readStr:
        push    temp                    ;preserve temp r16
;        ldi     yL,low(str_buf)
;        ldi     yH,high(str_buf)        ;init address pointer
        cbi     SPI_PORT,S_SS           ;set slave select low to send instruction
        ldi     temp,E_READ             ;read instruction
        rcall   SPI_tradeByte
        rcall   E_sendAddr              ;address to read already in xregisters
        ldi     temp,0x00
        rcall   SPI_tradeByte
        in      temp,SPDR
        mov     count,temp              ;first byte is length
        st      Y+,temp                 ;store length byte too
rd_lp:  ldi     temp,0x00
        rcall   SPI_tradeByte           ;address index automaticly increments
        in      temp,SPDR               ;remember tradebyte leaves return byte in SPDR
        st      Y+,temp                 ;store read byte in str_buf and increment pointer
        dec     count
        brne    rd_lp
        sbi     SPI_PORT,S_SS           ;** done ** de-select slave
        pop     temp
        ret
;--- function to write a string of bytes in eeprom ic
;--- takes address to write in x registers
;--- uses y registers for pointer to read string. first byte is length
E_writeStr:
        push    temp
        rcall   E_writeEnable           ;send write enable instruction
        cbi     SPI_PORT,S_SS           ;set slave select
        ldi     temp,E_WRITE
        rcall   SPI_tradeByte
        rcall   E_sendAddr              ;x registers should contain address to write to
;        ldi     zH,high(hello_str * 2)
;        ldi     zL,low(hello_str * 2)
        lpm     count,Z+
        mov     temp,count
        rcall   SPI_tradeByte           ;send length byte
w_lp:   lpm     temp,Z+
        rcall   SPI_tradeByte
        push    zL
        push    zH
        push    temp
        push    count
;        ldi     zL,low(write_str * 2)   ;print progress
;        ldi     zH,high(write_str * 2)
        rcall   print_s
        pop     count
        pop     temp
        mov     r24,temp
        push    temp
        call   transmit
        ldi     r24,0x0a
        call   transmit
        ldi     r24,0x0d
        call   transmit
        pop     temp
        pop     zH
        pop     zL
        dec     count
        brne    w_lp
        sbi     SPI_PORT,S_SS           ;de-select slave
        pop     temp
lp3:    rcall   E_readSts
        sbrc    temp,W_INP              ;loop while write in progress bit is set
        rjmp    lp3
        ret
;***** SRAM read and write functions *****
S_readByte:
        cbi     SPI_PORT,S_SS           ;slave select
        ldi     temp,S_READ
        rcall   SPI_tradeByte
	mov	temp, xH		;high byte first
	rcall	SPI_tradeByte
	mov	temp, xL
	rcall	SPI_tradeByte
        ldi     temp,0x00
        rcall   SPI_tradeByte           ;clock 8 bits to get return byte
        sbi     SPI_PORT,S_SS           ;deselect slave
        in      temp,SPDR
        ret
S_writeByte:
	push	temp
	cbi	SPI_PORT, S_SS
	ldi	temp, S_WRITE
	rcall	SPI_tradeByte
	mov	temp, xH
	rcall	SPI_tradeByte
	mov	temp, xL
	rcall	SPI_tradeByte
	pop	temp
	rcall	SPI_tradeByte
	sbi	SPI_PORT, S_SS
	ret
;**************** program memory data ***************
;run cmd jump table
;....
; commands
; index has to be padded because of avr Z addressing
ctabndx: .db 0, 3, 6, 9, 12, 15, 18, 21, 24, 27
ctab: .db 3, 'n', 'e', 'w', 0x01, 0, 4, 's', 'a', 'v', 'e', 0x02, 4, 'l', 'o', 'a', 'd', 0x03, 3, 'r', 'u', 'n', 0x04, 0
.db 3, 'c', 'o', 'n', 0x05, 0, 4, 's', 'l', 'o', 'g', 0x06, 4, 'l', 'l', 'o', 'g', 0x07, 4, 'd', 'l', 'o', 'g', 0x08, 4, 'l', 'i', 's', 't', 0x09
; error table
errndx: .dw 0,0, 7, 16, 24, 33, 41, 51, 61, 71, 81, 91
errtab: .db 13, '*', '*', ' ', 's', 'y', 'n', 't', 'a', 'x', ' ', 'e', 'r', 'r'
.db 16, '*', '*', ' ', 'v', 'a', 'l', 'u', 'e', ' ', 't', 'o', ' ', 'h', 'i', 'g', 'h', 0
.db 14, "** eval err **", 0
.db 16, "** no end err **", 0
.db 14, "** goto err **", 0
.db 19, "** key interrupt **"
.db 19, "** too many gosubs "
.db 19, "** return w/o gosub"
.db 19, "** pin in use **   "
.db 19, "** too many fors **"
.db 19, "** next w/o for  **"
stabndx: .db 0, 3, 6, 9, 13, 17, 21, 24, 27, 29, 32, 35, 37, 40, 43, 46, 50, 53, 56, 59, 63, 67, 71, 75, 79, 83, 86, 89, 93, 98
.db  102, 104, 106, 108, 110, 112, 115, 117, 119, 121, 123, 125, 127, 129, 132, 134, 137, 139, 141, 0
stab: .db 3, "let", 0x10, 0, 3, "dim", 0x11, 0, 4, "goto", 0x12, 5, "gosub", 0x13, 0, 6, "return", 0x14, 5, "print", 0x15, 0, 3, "pop", 0x16, 0
.db 3, "for", 0x17, 0, 2, "to", 0x18, 4, "step", 0x19, 4, "next", 0x1a, 2, "if", 0x1b, 4, "then", 0x1c, 3, "end", 0x1d, 0, 4, "stop", 0x1e
.db 5, "input", 0x1f, 0, 4, "data", 0x20, 4, "read", 0x21, 3, "rem", 0x22, 0, 5, "setin", 0x23, 0, 6, "setout", 0x24, 5, "pinhi", 0x25, 0, 5, "pinlo", 0x26, 0
.db 6, "clrpin", 0x27, 5, "rdpin", 0x28, 0, 4, "peek", 0x29, 4, "poke", 0x2a, 5, "store", 0x2b, 0, 7, "retrive", 0x2c, 0, 6, "tglpin", 0x2d
.db 1, '=', 0x40, 0, 1, '-', 0x41, 0, 1, '+', 0x42, 0, 1, '/', 0x43, 0, 1, '*', 0x44, 0, 3, "mod", 0x45, 0, 1, '>', 0x46, 0, 1, '<', 0x47, 0
.db 1, 0x23, 0x48, 0, 1, '(', 0x49, 0, 1, ')', 0x4a, 0, 1, ':', 0x4b, 0, 1, '^', 0x4c, 0, 3, "and", 0x4d, 0, 2, "or", 0x4e, 3, "not", 0x4F, 0, 1, 0x22, 0x50, 0, 1, ',', 0x51, 0, 1, 0x3B, 0x52, 0 
bcdtab: .db 0x10, 0x27, 0xe8, 0x03, 0x64, 0x00, 0x0a, 0x00, 0x01, 0x00
.dseg
;**************** SRAM addresses ********************
; zpage
.org 0x0100
ppmem:
	.byte 0x469
inttab:
	.byte 0xba
strtab:
	.byte 0xba
;------ start of bbasic vars --------
himem:
	.byte 2
lomem:
	.byte 2
curline:	
	.byte 2
lastline:
	.byte 2
pstack:	.byte 2
txtndx:	.byte 1
frnxtndx:
	.byte 1
frnxtstk:
	.byte 32
gsubndx:
	.byte 1
gsubstk:
	.byte 8
temp_16int:
	.byte 2
temp2_16int:
	.byte 2
temp3_16:
	.byte 2
temp4_16:
	.byte 2
errno:
	.byte 1
tmp_tok:
	.byte 1
tmp_tok2:
	.byte 1
inttabp:
	.byte 2
intnxte:
	.byte 2
intmax:
	.byte 1
strtabp:
	.byte 2
strnxte:
	.byte 2
strmax:
	.byte 1
epp:
	.byte 2
rnep:
	.byte 2
rndx:
	.byte 2
tempaddr:
	.byte 2
tempaddr2:
	.byte 2
evalq:
	.byte 10
eqp:
	.byte 1
dimptr:
	.byte 2
dmndx:
	.byte 2
dim_buf:
	.byte 10
;27 total
.org 0x0760
in_buf:
	.byte 0xAC
out_buf:
	.byte 0x74
stack_top:
	.byte 0x7D		;126 bytes of stack!
