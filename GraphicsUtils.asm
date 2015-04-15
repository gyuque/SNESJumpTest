.setcpu		"65816"
.autoimport	on
.include "Globals.inc"

.export	transferPatternData
.export transferPaletteData
.export initSystemStates
.export zeroFillBG
.export enableScreen
.export setSpriteGlobalAttribute
.export initSpritePositionBulk
.export initSpriteSecondTableBulk
.export placeVerticalCombinedSprites
.export setAttrForVerticalCombinedSprites
.import PatternBase:far
.import PaletteBase:far

.segment "STARTUP"

.include "snippets.asm"


; == Sub
; MUST UNDER: A16, I16
; in A: Dest address
; in X: Start offset
; in Y: Size (in words)
.proc transferPatternData
.a16
.i16
	sta	$2116
	save_paxy

copyptn:
	lda	f:PatternBase, x
	sta	$2118
	inx
	inx
	dey
	bne	copyptn

	restore_paxy
	rts
.endproc


; == Sub
; MUST UNDER: A16, I16
; in X: Source offset
; in Y: Dest offset (in words)
.proc transferPaletteData
.a16
.i16
	save_paxy

	sep #$20
.a8

	tya
	sta	$2121

	ldy	#$0020 ; N=16 entries
copypal:
	lda	f:PaletteBase, x
	sta $2122

	inx
	dey
	bne	copypal

	rep	#$20
.a16

	restore_paxy
	rts
.endproc


; == Sub
; MUST UNDER: A16, I16
.proc initSystemStates
.a16
.i16
	save_paxy

	; VSync and Joypad
	sep	#$20
.a8

	stz $4016

	lda #$81
	sta $4200

	rep	#$20
.a16	

	restore_paxy
	rts
.endproc


; == Sub
; MUST UNDER: A16, I16
; in X: base address
.proc zeroFillBG
.a16
.i16
	php
	pha

	mWaitVBlankEnd

	; Set base address
	txa
	sta	$2116

	lda #$400
	fillloop:
	stz	$2118
	dec
	bne fillloop

	pla
	plp
	rts
.endproc


; == Sub
; MUST UNDER: A16, I16
; in X: brightness(1-15)
.proc enableScreen
.a16
.i16
	php
	pha

	sep	#$20
.a8
	; flags = [X][X][X][SP] [BG4][BG3][BG2][BG1]
;	lda	#%00010001 ; BG1 only
	lda	#%00010010 ; BG2 only
	sta	$212c ; Main screen
	stz	$212d ; Sub screen

	; brightness
	txa
	and #$0f
	sta $2100

	rep	#$20
.a16

	pla
	plp
	rts
.endproc


; == Sub
; MUST UNDER: A16, I16
; in X: Name selection
.proc setSpriteGlobalAttribute
.a16
.i16
	save_paxy

	sep	#$20
.a8

	; sss=size  bb=base  nnn=name

	; name | offset
	; -----+-------
	;    0 |  $0000
	;    1 |  $2000
	;    2 |  $3000

	lda #%10100000
	stx gAccTemp
	ora gAccTemp
	sta $2101

	rep	#$20
.a16

	restore_paxy
	rts
.endproc

; == Sub
; MUST UNDER: A16, I16
.proc initSpriteSecondTableBulk
.a16
.i16
	save_paxy


	ldy #$20 ; 32entries (32*4 = 128 sprites)
	mWaitVBlankEnd

	sep	#$20
.a8

	; Access second table
	stz $2102
	lda #$01
	sta $2103
	
:	stz $2104
	dey
	bne :-

	rep	#$20
.a16

	restore_paxy
	rts
.endproc


; == Sub
; MUST UNDER: A16, I16
.proc initSpritePositionBulk
.a16
.i16
	save_paxy

	ldy #$80 ; 128 Sprites
	mWaitVBlankEnd

	; A <- (y-1)*2
:	tya
	dec
	asl

	; Set write position
	sta $2102

	sep	#$20
.a8

	stz $2104 ; set x

	lda #$e0  ; y=above the top
	sta $2104 ; set y
	stz $2104 ; tile
	stz $2104 ; others

	rep	#$20
.a16

	dey
	bne :-


	restore_paxy
	rts
.endproc


; == Sub
; MUST UNDER: A16, I16
; in A: first sprite index
; in X: x coordinate
; in Y: y coordinate
.proc placeVerticalCombinedSprites
.a16
.i16
	save_paxy
	pha ; Use later.


	; Set write position (in A * 2)
	asl
	sta $2102

	; Write x, y
	sep	#$20
.a8
	txa
	sta $2104 ; ** Don't use stx/sty (to do 8bit access)
	tya
	sta $2104
	rep	#$20
.a16

	; Y += height
	tya
	clc
	adc #$20
	tay

	; Set write position ((in A + 1) * 2)
	pla
	inc
	asl
	sta $2102

	; Write x, y
	sep	#$20
.a8
	txa
	sta $2104
	tya
	sta $2104
	rep	#$20
.a16

	restore_paxy
	rts
.endproc


; == Sub
; MUST UNDER: A16, I16
; in A: first sprite index
; in X: tile index (hi:second, lo:first)
; in Y: other attribute bits (shared)
.proc setAttrForVerticalCombinedSprites
.a16
.i16
	save_paxy
	pha ; Use later.


	; First sprite
	; Set write position (in A*2 + 1)
	asl
	inc
	sta $2102

	sep #$20
.a8
	txa
	sta $2104 ; ** Don't use stx/sty (to do 8bit access)
	tya
	sta $2104
	rep #$20
.a16

	; Second sprite
	; Set write position ((in A+1)*2 + 1)
	pla
	inc
	asl
	inc
	sta $2102

	; pick higher bits
	txa
	right_shift_8

	sep #$20
.a8
	sta $2104

	tya
	sta $2104

	rep #$20
.a16

	restore_paxy
	rts
.endproc


