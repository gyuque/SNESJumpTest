;----------------------------------------------------------------------------
;			Pad Input Test
;----------------------------------------------------------------------------
.setcpu		"65816"
.autoimport	on

; Global Variables
.include "Globals.inc"
.define kFieldMapSize #896

.define kPlayerCRectLeft      #8
.define kPlayerCRectRight     #24
.define kPlayerCRectBottom #60

.export PatternBase
.export PaletteBase
.import	InitRegs
.import transferPatternData
.import transferPaletteData
.import initSystemStates
.import zeroFillBG
.import enableScreen
.import setSpriteGlobalAttribute
.import initSpritePositionBulk
.import initSpriteSecondTableBulk
.import placeVerticalCombinedSprites
.import setAttrForVerticalCombinedSprites
.segment "STARTUP"

; Reset int. (Entry Point) -------------------
.proc Reset
	sei
	clc
	xce	; Native Mode
	phk
	plb	; DB = 0

	rep	#$30	; A,I 16bit
.a16
.i16
	; Initialize stack
	ldx	#$1fff
	txs
	
	jsr	InitRegs
	jsr configureBG
	jsr loadGraphicsForMainScene

	ldx #$4000
	jsr zeroFillBG

	; Hide screen (but enabled)
	ldx #$01
	jsr enableScreen

	jsr initAppStates
	jsr initSystemStates
	jsr initSpritePositionBulk
	jsr initSpriteSecondTableBulk
	jsr initSpriteAttributes
	jsr buildFieldBG

	; Turn on screen
	ldx #$0f
	jsr enableScreen

	jsr enterMainLoop

	rti
.endproc

; ================= main procs =================
.include "snippets.asm"

; == Sub
; MUST UNDER: A16, I16
.proc configureBG
.a16
.i16
	save_paxy

	; BG configuration
	sep	#$20
.a8
	; Specify BGMode (16/16/4/0)
	lda #$01
	sta $2105


	lda	#$40
	; sta $2107 ; BG1 map/size
	sta $2108 ; BG2 map/size
	stz	$210b ; BG1/2 ptn

	rep	#$20
.a16

	restore_paxy
	rts
.endproc


; == Sub
; MUST UNDER: A16, I16
.proc initAppStates
.a16
.i16
	save_paxy

	stz gFillPos
	stz gPadState

	; Player states - - - -
	lda #$80
	sta gPlayerX4

	lda #$40
	sta gPlayerY4

	stz gPlayerVX4
	stz gPlayerVY4
	stz gPlayerAnimationFrame
	stz gPlayerWalkCount
	stz gPlayerOnFloorFlag
	stz gPlayerFlipX
	stz gPlayerJumpCount
	stz gPlayerJumpSuppressRepeat

	jsr loadFieldMapOnRAM

	restore_paxy
	rts
.endproc

; == Sub
; MUST UNDER: A16, I16
.proc initSpriteAttributes
.a16
.i16
	save_paxy

	ldx #$01 ; Sprite tiles name selection
	jsr setSpriteGlobalAttribute

	restore_paxy
	rts
.endproc

; == Sub
; MUST UNDER: A16, I16
.proc enterMainLoop
.a16
.i16

mainloop_start:
		;mWaitVBlankStart
		;mWaitVBlankEnd
	jmp	mainloop_start

	rts
.endproc

.proc mainUpdate
.a16
.i16
	save_paxy

	lda gFillPos
	inc
	and #$3ff
	sta gFillPos

	clc
	lda gFillPos
	adc #$4000
	sta	$2116

	lda gPadState
	beq nopush
		lda #$3
	jmp endif
	nopush:
		lda #$2
	endif:
;	sta	$2118

	jsr updatePlayerStatus
	jsr updatePlayerGraphics

	restore_paxy
	rts
.endproc


.proc VBlank
	pha
	phx
	php

	rep	#$30
.a16
.i16

	jsr mainUpdate

; Pad routine should be placed AFTER other routines
; Wait Joypad Autoread
	sep	#$20
.a8 

:	lda $4212
	and #$01
	bne :-

	rep	#$20
.a16

; Read Pad Status
	lda $4218
	sta gPadState

	plp
	plx
	pla
	rti
.endproc


; == Sub
; MUST UNDER: A16, I16
.proc loadGraphicsForMainScene
.a16
.i16
	save_paxy

	; BG Palette
	ldx	#$0000 ; src start
	ldy #$0000 ; dest start in words
	jsr transferPaletteData

	; Sprite Palette
	ldx	#$0020 ; src start
	ldy #$0080 ; dest start in words
	jsr transferPaletteData

	; BG Pattern
	lda	#$0000 ; dest start
	ldx #$0000 ; src start
	ldy	#$0800 ; size in words
	jsr transferPatternData

	; Sprite Pattern
	lda	#$2000 ; dest start
	ldx #$1000 ; src start
	ldy	#$1000 ; size in words
	jsr transferPatternData

	restore_paxy
	rts
.endproc

; ===================================================
; Application specific subroutines
; ===================================================

; == Sub
; MUST UNDER: A16, I16
.proc updatePlayerStatus
.a16
.i16
	save_paxy

	ldx gPadState
	jsr processDirectionInput
	jsr dropPlayerVY
	jsr incPlayerJumpCount

	; Check jump trigger
	lda gPadState
	and #$8000
	beq nojump

	lda gPlayerJumpCount ; check long-press limit
	and #8
	bne endjump

		; Do jump
		lda #-15
		sta gPlayerVY4

		lda #1
		sta gPlayerJumpSuppressRepeat

		jmp endjump
	nojump:
		lda gPlayerOnFloorFlag
		beq endjump
			stz gPlayerJumpSuppressRepeat ; clear when on the ground and button is released
	endjump:

	; player pos += velo.
	; X
	clc
	lda gPlayerX4
	adc gPlayerVX4

	tax
	jsr checkPlayerSideHit
	; out X: fixed X(4x)
	txa

	sta gPlayerX4


	; Y
	clc
	lda gPlayerY4
	adc gPlayerVY4

	tay
	jsr checkPlayerFoot
	; out Y: fixed Y(4x)
	tya
	sta gPlayerY4

	restore_paxy
	rts
.endproc

; == Sub
; MUST UNDER: A16, I16
.proc updatePlayerGraphics
.a16
.i16
	save_paxy

	lda gPlayerOnFloorFlag
	beq fr_jumping

		; Determine frame
		lda gPlayerWalkCount
		lsr
		lsr
		sta gPlayerAnimationFrame

	jmp frend
	fr_jumping:

		lda #$20
		sta gPlayerAnimationFrame

	frend:

	; X <- gPlayerX4 / 4
	; Y <- gPlayerY4 / 4

	lda gPlayerX4
	right_shift_2
	tax

	lda gPlayerY4
	right_shift_2
	tay

	lda #$00 ; sprite index (0, 1)
	; x,y is set above
	jsr placeVerticalCombinedSprites

	lda #$00 ; sprite index (0, 1)
	jsr makeCombinedTileIndex ; x <- combined tile index
	jsr makePlayerSpriteAttrBits ; y <- other attrs
	jsr setAttrForVerticalCombinedSprites

	restore_paxy
	rts
.endproc

; == Sub
; MUST UNDER: A16, I16
; out X: combined (2nd|1st) tile index
.proc makeCombinedTileIndex
.a16
.i16
	save_pa

	; For first sprite
	lda gPlayerAnimationFrame
	asl
	asl
	sta gAccTemp
	
	; For second sprite
	clc
	adc #$40
	left_shift_8

	; combine
	ora gAccTemp
	tax

	restore_pa
	rts
.endproc

; == Sub
; MUST UNDER: A16, I16
; out Y: sprite attribute bits
.proc makePlayerSpriteAttrBits
.a16
.i16
	save_pa

	lda gPlayerFlipX
	beq noflip
		ldy #$60
		jmp endf
	noflip:
		ldy #$20
	endf:

	restore_pa
	rts
.endproc


; == Sub
; MUST UNDER: A16, I16
; in X: pad input bits
.proc processDirectionInput
.a16
.i16
	save_paxy
	stz gPlayerHMoveFlag

	txa
	and #$0300
	cmp #$0200
	bne not_left

		; Left pushed
		jsr decPlayerVX
		inc gPlayerHMoveFlag
		lda #1
		sta gPlayerFlipX
		jmp dircheck_end
	
	not_left:
	cmp #$0100
	bne not_right

		; Right pushed
		jsr incPlayerVX
		inc gPlayerHMoveFlag
		stz gPlayerFlipX
		jmp dircheck_end

	not_right:
		stz gPlayerWalkCount
		jsr stopPlayerVX
	dircheck_end:

	; advance walk count
	lda gPlayerHMoveFlag
	beq nowc
	inc gPlayerWalkCount
	lda #$0f
	and gPlayerWalkCount
	sta gPlayerWalkCount
nowc:

	restore_paxy
	rts
.endproc

.define kPlayerVeloMax #8
.define kPlayerVeloMin #-8

; == Sub
; MUST UNDER: A16, I16
.proc incPlayerVX
.a16
.i16
	save_paxy

	lda gPlayerVX4
	inc
	bmi nocap
	cmp kPlayerVeloMax
	bcc nocap
	
		; Cap to max
		lda kPlayerVeloMax

	nocap:
	sta gPlayerVX4

	restore_paxy
	rts
.endproc


; == Sub
; MUST UNDER: A16, I16
.proc decPlayerVX
.a16
.i16
	save_paxy

	lda gPlayerVX4
	dec
	bpl nocap
	cmp kPlayerVeloMin
	bcs nocap
	
		; Cap to max
		lda kPlayerVeloMin

	nocap:
	sta gPlayerVX4

	restore_paxy
	rts
.endproc


; == Sub
; MUST UNDER: A16, I16
.proc stopPlayerVX
.a16
.i16
	save_paxy

	lda gPlayerVX4
	beq endsw ; already stopped
	bmi negx

		; vx > 0
		dec

	jmp endsw
	negx:

		; vx < 0
		inc

	endsw:

	sta gPlayerVX4
	restore_paxy
	rts
.endproc



; == Sub
; MUST UNDER: A16, I16
.define kDropYMax #18
.proc dropPlayerVY
.a16
.i16
	save_pa

	lda gPlayerVY4
	inc
	bmi nocap ; ignore when VY goes up
	cmp kDropYMax
	bcc nocap
	lda kDropYMax

nocap:
	sta gPlayerVY4

	restore_pa
	rts
.endproc


; == Sub
; MUST UNDER: A16, I16
; in X: next PlayerX(4x)
; out X: fixed X(4x)
.proc checkPlayerSideHit
.a16
.i16
	save_pay
	stx gAccTemp2 ; save original input

	; nextX / 4
	txa
	right_shift_2
	sta gAccTemp

	; y /= 4
	lda gPlayerY4
	right_shift_2

	; y + bottomY - 1
	clc
	adc kPlayerCRectBottom
	dec
	tay

	; Do hit test
	; (LEFT)
	lda gPlayerVX4
	bpl noleft ; only when moving to left
		; x += leftX
		lda gAccTemp
		clc
		adc kPlayerCRectLeft
		tax

		jsr hittestWithField
		; x <- out
		; set fix amount
		lda #8
		sta gAccTemp3
		
		cpx #0
		bne onhit
	noleft:

	lda gPlayerVX4
	bmi noright ; only when moving to right
		; x += rightX
		lda gAccTemp
		clc
		adc kPlayerCRectRight
		tax

		jsr hittestWithField
		; x <- out
		; set fix amount
		lda #-1
		sta gAccTemp3

		cpx #0
		bne onhit
	noright:

	ldx gAccTemp2 ; return original input
	jmp endp
	onhit:

		stz gPlayerVX4

		lda gAccTemp
		right_shift_3
		left_shift_3

		clc
		adc gAccTemp3

		left_shift_2 ; to 4x
		tax

	endp:

	restore_pay
	rts
.endproc

; == Sub
; MUST UNDER: A16, I16
; in Y: next PlayerY(4x)
; out Y: fixed Y(4x)
.proc checkPlayerFoot
.a16
.i16
	save_pax
	sty gAccTemp2

	; y /= 4
	tya
	right_shift_2
	sta gAccTemp

	; y + bottomY
	clc
	adc kPlayerCRectBottom
	tay

	; x /= 4
	lda gPlayerX4	
	right_shift_2

	; Do hit test
	; (LEFT)
	; x += leftX
	pha
	clc
	adc kPlayerCRectLeft
	tax

	jsr hittestWithField
	pla
	; x <- out
	cpx #0
	bne onhit

	; (RIGHT)
	; x += rightX
	clc
	adc kPlayerCRectRight
	tax

	jsr hittestWithField
	; x <- out
	cpx #0
	bne onhit


	; Proces result
	stz gPlayerOnFloorFlag
	ldy gAccTemp2
	jmp endp
	onhit:

		lda #1
		sta gPlayerOnFloorFlag
		stz gPlayerVY4

		; calc fixed Y
		; A <- Foot Y(1x)
		tya

		; A <- int(A/8) * 8 + 1
		right_shift_3
		left_shift_3
		inc

		; Fixed Y(1x) -= bottomY
		clc
		sbc kPlayerCRectBottom
		left_shift_2 ; 1x -> 4x

		tay

	endp:

	restore_pax
	rts
.endproc

; == Sub
; MUST UNDER: A16, I16
; in X: x coordinate(1x)
; in Y: y coordinate(1x)
; out X: hit?
.proc hittestWithField
.a16
.i16
	save_pay

	; Calc map cell address

	; Calc Y base
	tya
	right_shift_3 ; to cell coord

	; *= 32
	left_shift_3
	left_shift_2

	pha
	txa
	right_shift_3
	sta gAccTemp3
	pla

	; Y base + X
	clc
	adc gAccTemp3

	; * size of word
	asl
	tax

	lda gFieldMapArea, x
	and #$10
	bne onhit

	ldx #0
	jmp endp

	onhit:
		ldx #1
	endp:

	restore_pay
	rts
.endproc

; == Sub
; MUST UNDER: A16, I16
.proc incPlayerJumpCount
.a16
.i16
	save_pa

	lda gPlayerOnFloorFlag
	bne onground
		; now jumping...
		lda gPlayerJumpCount
		cmp #8
		bcs endif

		inc gPlayerJumpCount
		jmp endif
	onground:
		lda gPlayerJumpSuppressRepeat
		left_shift_3
		sta gPlayerJumpCount
	endif:

	restore_pa
	rts
.endproc


.proc loadFieldMapOnRAM
.a16
.i16
	save_paxy

	ldx #0
	ldy kFieldMapSize


:	lda f:FieldData, x
	and #$00ff

	phx
	pha

	; x *= 2
	txa
	asl
	tax

	pla
	sta gFieldMapArea, x
	plx

	inx
	dey
	bne :-

	restore_paxy
	rts
.endproc


.proc buildFieldBG
.a16
.i16
	save_paxy

	mWaitVBlankEnd

	; Set base address
	lda #$4000
	sta	$2116

	sep	#$20
.a8

	; Word increment
	lda #$01
	sta $4300

	; write to $2118
	lda #$18
	sta $4301

	ldx #gFieldMapArea
	stx $4302

	lda #$00
	sta $4304

	; Set DMA size to 0x700 (NOT 0x380)
	lda #$00
	sta $4305

	lda #$07
	sta $4306

	; Go
	lda #$01
	sta $420b

	rep	#$20
.a16

	restore_paxy
	rts
.endproc

.proc EmptyInt
	rti
.endproc


WalkAnimationTable:
	.byte $01, $02, $03, $02

; ===================================================
; Metadata and Assets
; ===================================================

; Cartridge metadata - - -
.segment "CARTINFO"
	.byte	"GAMETEST             "	; Game Title
	.byte	$00				; 0x01:HiRom, 0x30:FastRom(3.57MHz)
	.byte	$00				; ROM only
	.byte	$08				; 32KB=256KBits
	.byte	$00				; RAM Size (8KByte * N)
	.byte	$00				; NTSC
	.byte	$01				; Licensee
	.byte	$00				; Version
	.byte	$ff, $b0, $00, $4f		; checksum(empty here)
	.byte	$ff, $ff, $ff, $ff		; unknown

	.word	EmptyInt	; Native:COP
	.word	EmptyInt	; Native:BRK
	.word	EmptyInt	; Native:ABORT
	.word	VBlank		; Native:NMI
	.word	$0000		; 
	.word	EmptyInt	; Native:IRQ

	.word	$0000	; 
	.word	$0000	; 

	.word	EmptyInt	; Emulation:COP
	.word	EmptyInt	; 
	.word	EmptyInt	; Emulation:ABORT
	.word	VBlank		; Emulation:NMI
	.word	Reset		; Emulation:RESET
	.word	EmptyInt	; Emulation:IRQ/BRK

; Graphics assets - - - - - - - -
.segment "HIRODATA": far
PaletteBase:
	.incbin	"bg-palette.bin"
	.incbin	"sp-palette.bin"

PatternBase:
	.incbin	"bg-ptn.bin"
	.incbin	"sprite-ptn.bin"

FieldData:
	.incbin	"field-data.bin"

.segment "HIRODATA2": far
TestHiData2:
	.byte "SECOND!_HI_AREA_TEST_DATA"

.segment "HIRODATA3": far
TestHiData3:
	.byte "Third_HI_AREA_TEST_DATA"

.segment "DONTUSE"
	.byte "Padding area"

	; adjust checksum
	.byte $ff, $ff, $ff, 95
