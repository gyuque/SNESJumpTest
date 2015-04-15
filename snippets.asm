; Bulk push/pop

.macro save_paxy
	php
	pha
	phx
	phy
.endmacro

.macro restore_paxy
	ply
	plx
	pla
	plp
.endmacro

.macro save_pa
	php
	pha
.endmacro

.macro restore_pa
	pla
	plp
.endmacro


.macro save_pax
	php
	pha
	phx
.endmacro

.macro restore_pax
	plx
	pla
	plp
.endmacro


.macro save_pay
	php
	pha
	phy
.endmacro

.macro restore_pay
	ply
	pla
	plp
.endmacro


.macro left_shift_2
	asl
	asl
.endmacro

.macro left_shift_3
	asl
	asl
	asl
.endmacro

.macro left_shift_8
	asl
	asl
	asl
	asl
	asl
	asl
	asl
	asl
.endmacro

.macro right_shift_8
	lsr
	lsr
	lsr
	lsr
	lsr
	lsr
	lsr
	lsr
.endmacro

.macro right_shift_2
	lsr
	lsr
.endmacro

.macro right_shift_3
	lsr
	lsr
	lsr
.endmacro


; VBlank wait routines

.macro mWaitVBlankStart
	.local vbwait

	sep	#$20
.a8

vbwait:
	lda $4210
	and #$80
	bne vbwait

	rep	#$20
.a16

.endmacro


.macro mWaitVBlankEnd
	.local vewait

	sep	#$20
.a8

vewait:
	lda $4210
	and #$80
	beq vewait

	rep	#$20
.a16

.endmacro
