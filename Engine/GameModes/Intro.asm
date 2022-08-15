; Turbocharged intro code by DevEd

oamentry: macro
    db  \1+16,\2+8,\3
    endm

section "Intro RAM",wram0

Intro_CarY:             db
Intro_CarX:             db
Intro_LogoFrame:        db

Intro_BGFadeOffset:     db
Intro_BGFadeTick:       db

Intro_LogoX:            db
Intro_LogoY:            db
Intro_CheckerX:         db
Intro_CheckerY:         db
Intro_DoDrawChecker:	db

Intro_ScrollerPos:      dw
Intro_ScrollerOffset:   db
Intro_ScrollerFrame:    db

section "Intro code",rom0

; ================================================================

GM_Intro:
    ; clear shadow OAM
    ld      hl,OAMBuffer
    ld      b,OAMBuffer.end-OAMBuffer
    xor     a
    call    _FillRAMSmall
    rst     DoOAMDMA

    SetDMGPal   rBGP, 0,3,2,1
    SetDMGPal   rOBP0,0,0,1,3

    ld      a,LCDCF_ON | LCDCF_BG8000 | LCDCF_BGON | LCDCF_OBJON | LCDCF_OBJ16
    ldh     [rLCDC],a
    ld      a,IEF_VBLANK
    ldh     [rIE],a
	ld		a,STATF_LYC
	ldh		[rSTAT],a

    ld      hl,Intro_LogoTiles
    ld      de,$8800
    ld      bc,Intro_LogoTiles.end-Intro_LogoTiles
    call    Intro_Load2BPP

    ld      hl,Intro_CheckerboardTiles
    ld      de,$9010
    ld      bc,Intro_CheckerboardTiles.end-Intro_CheckerboardTiles
    call    Intro_Load1BPP

    ld      hl,Intro_CarTiles
    ld      de,$8d80
    ld      bc,Intro_CarTiles.end-Intro_CarTiles
    call    Intro_Load2BPP
	
    ld      hl,Font
    ld      bc,Font.end-Font
    ld      de,$8200
    call    Intro_Load1BPP	

    ; initialize checkerboard pattern
	
	ld		e,1
    ld      hl,$9c00
	call	.checkerboardloop
    ld      hl,$9e00
	call	.checkerboardloop
	jr		.continue
.checkerboardloop
	ld		bc,$1403
:	WaitForVRAM
	ld		[hl],e
	inc		hl
	inc		e
	dec		b
	jr		nz,:-
	ld		a,l
	and		$e0
	add		$20
	ld		l,a
	jr		nc,:+
	inc		h
:	dec		c
	ld		b,20
	jr		nz,:--
	ret

.continue


	; decrunch vertical checkerboard heights
	ld		b,24
	ld		de,$c800
	ld		hl,Intro_CheckerboardHeights
.decrunchloop
	push	hl
	ld		a,[hl+]
	ld		h,[hl]
	ld		l,a
	ld		a,[hl+]
	ld		c,a
.decrunchloop2
	ld		a,[hl+]
	cp		$ff
	jr		z,.decrunch_nextframe
:	push	af
	ld		a,c
	ld		[de],a
	inc		e
	pop		af
	dec		a
	jr		nz,:-
:	ld		a,c
	xor		$80
	ld		c,a
	jr		.decrunchloop2
.decrunch_nextframe
	ld		e,0
	inc		d
	pop		hl
	inc		hl
	inc		hl
	dec		b
	jr		nz,.decrunchloop
	
	; initialize car
    ld      a,-40
    ld      [Intro_CarX],a
    ld      a,72
    ld      [Intro_CarY],a
    xor     a
    ld      [Intro_LogoFrame],a

    xor     a
    ld      [Intro_BGFadeOffset],a
    ld      [Intro_BGFadeTick],a
    ld      [Intro_LogoX],a
    ld      [Intro_LogoY],a
    ld      [Intro_CheckerX],a
    ld      [Intro_CheckerY],a

    ei

; ================================================================

Intro_CarLogoReveal:
    ld      a,[Intro_CarX]
    add     8
    ld      [Intro_CarX],a
    push    af
    call    Intro_DrawCar
    pop     af

    cp      -40
    jr      nc,:++
:   cp      208
    jr      c,:+
    ; exit from logo reveal part

    ld      a,bank(Module_Turbo)-1
    call    GBM_LoadModule
    rst     WaitVBlank
    jr      Intro_FadeInCheckerboard

:   and     a
    jr      c,.skiplogo

    ; load logo tiles
    cp      180
    jr      nc,.skiplogo
    cp      $18
    jr      c,.skiplogo

    ld      a,[Intro_LogoFrame]
    push    af
    ld      de,$0020
    ld      hl,$98e0
    add     $80
    ld      b,a
    and     $7f
    add     l
    ld      l,a
    jr      nc,:+
    inc     h
:   ; first row
    WaitForVRAM
    ld      [hl],b
    add     hl,de
    ld      a,b
    add     20
    ld      b,a
    ; second row
    WaitForVRAM
    ld      [hl],b
    add     hl,de
    ld      a,b
    add     20
    ld      b,a
    ; third row
    WaitForVRAM
    ld      [hl],b
    add     hl,de
    ld      a,b
    add     20
    ld      b,a
    ; fourth row
    WaitForVRAM
    ld      [hl],b
    ; done
    pop     af
    inc     a
    ld      [Intro_LogoFrame],a


.skiplogo
    rst     WaitVBlank
    jr      Intro_CarLogoReveal
:

; ================================================================

Intro_FadeInCheckerboard:
    ld      a,IEF_VBLANK | IEF_TIMER | IEF_STAT
    ldh     [rIE],a
    ld      a,LCDCF_ON | LCDCF_BG8800 | LCDCF_BGON | LCDCF_OBJON
    ldh     [rLCDC],a

    xor     a
    ld      [Intro_ScrollerPos],a
    ld      [Intro_ScrollerPos+1],a
    ld      [Intro_ScrollerOffset],a
    ld      [Intro_ScrollerFrame],a

.loop
    ld      a,[Intro_BGFadeOffset]
    inc     a
    cp      $81
    jr      z,.done
    ld      [Intro_BGFadeOffset],a
:   call    Intro_UpdateScroller
    rst     WaitVBlank
    jr      .loop

.done
    
; ================================================================

Intro_Loop:
    call    Intro_UpdateScroller
    rst     WaitVBlank
    jr      Intro_Loop

; ================================================================

Intro_RunCheckerboardLCD:	
	ldh		a,[rLY]
	inc		a
	ldh		[rLYC],a
	
	ld		a,[Intro_DoDrawChecker]
	and		a
	ret		z

	push	bc
	push	de
	push	hl
	
	; horizontal
	ldh		a,[rLY]
	ld		b,a	
	ld		a,[sys_CurrentFrame]
	ld		hl,Intro_CheckerboardSineTable
	add		l
	ld		l,a
	ld		a,[hl]
	ld		d,a
	sub		b
	dec		a
	ldh		[rSCY],a
	
	; vertical

	ld		a,$c8
	add		d
	ld		h,a
	ld		l,b
	ld		a,[hl]
	ldh		a,[rSCY]
	xor		[hl]
	ldh		[rSCY],a	
	
	pop		hl
	pop		de
	pop		bc
	ret

Intro_CheckerboardHeights:
	dw		.00,.01,.02,.03,.04,.05,.06,.07,.08,.09
	dw		.10,.11,.12,.13,.14,.15,.16,.17,.18,.19
	dw		.20,.21,.22,.23

.00	db		$00, 2, 6, 6, 7, 6, 6, 7, 6, 6, 6, 7, 6, 6, 7, 6, 6, 6, 7, 6, 6, 6, 7, 6, 6, 7, 6, 1,$ff
.01	db		$80, 6, 6, 7, 6, 7, 6, 7, 6, 7, 6, 7, 6, 7, 6, 7, 6, 7, 6, 6, 7, 6, 7, 6, 7, 5,$ff
.02	db		$80, 4, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 7, 6, 7, 3,$ff
.03	db		$80, 1, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7, 6, 7, 7, 7, 7, 7, 7, 7, 6, 1,$ff
.04	db		$00, 6, 7, 7, 7, 7, 7, 8, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 5,$ff
.05	db		$00, 4, 7, 8, 7, 7, 7, 8, 7, 7, 7, 8, 7, 7, 7, 8, 7, 7, 8, 7, 7, 7, 8, 3,$ff
.06	db		$00, 2, 7, 8, 7, 8, 7, 8, 7, 8, 7, 8, 7, 8, 7, 7, 8, 7, 8, 7, 8, 7, 8, 1,$ff
.07	db		$80, 8, 7, 8, 8, 7, 8, 8, 7, 8, 7, 8, 8, 7, 8, 8, 7, 8, 8, 7, 8, 7,$ff
.08	db		$80, 6, 8, 7, 8, 8, 8, 8, 8, 8, 7, 8, 8, 8, 8, 8, 8, 7, 8, 8, 8, 5,$ff
.09	db		$80, 4, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 9, 8, 8, 8, 8, 8, 3,$ff
.10	db		$80, 2, 8, 8, 9, 8, 8, 8, 9, 8, 8, 8, 9, 8, 8, 8, 9, 8, 8, 8, 9, 1,$ff
.11	db		$00, 9, 8, 8, 9, 8, 9, 8, 9, 8, 9, 8, 8, 9, 8, 9, 8, 9, 8, 8,$ff
.12	db		$00, 7, 9, 8, 9, 8, 9, 9, 8, 9, 9, 8, 9, 9, 8, 9, 8, 9, 9, 6,$ff
.13	db		$00, 5, 9, 9, 9, 9, 8, 9, 9, 9, 9, 9, 8, 9, 9, 9, 9, 9, 8, 5,$ff
.14	db		$00, 4, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 3,$ff
.15	db		$00, 2, 9, 9,10, 9, 9, 9, 9,10, 9, 9, 9,10, 9, 9, 9,10, 9, 1,$ff
.16	db		$80,10, 9, 9,10, 9,10, 9,10, 9, 9,10, 9,10, 9,10, 9, 9,$ff
.17	db		$80, 8,10, 9,10,10, 9,10, 9,10,10, 9,10,10, 9,10, 9, 8,$ff
.18	db		$80, 7, 9,10,10,10,10,10, 9,10,10,10,10, 9,10,10,10, 6,$ff
.19	db		$80, 5,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10, 5,$ff
.20	db		$80, 4,10,10,10,11,10,10,10,10,11,10,10,10,10,11,10, 3,$ff
.21	db		$80, 2,11,10,10,11,10,11,10,10,11,10,11,10,11,10,10, 2,$ff
.22	db		$80, 1,10,11,11,10,11,10,11,11,10,11,10,11,11,10,11,$ff
.23	db		$00,10,11,11,10,11,11,11,11,10,11,11,11,11,10,10,$ff

; ================================================================

; =================
; Scroller routines
; =================
    
Intro_UpdateScroller:
    ld      a,[sys_CurrentFrame]
    and     1
    jr      z,:+
    ld      hl,Intro_ScrollerFrame
    inc     [hl]
:

    ld      hl,Intro_ScrollerPos
    push    hl
    ld      a,[hl+]
    ld      h,[hl]
    ld      l,a
    ld      a,h
    cp      high(Intro_ScrollTextSize)
    jr      nz,.skip3
    ld      a,l
    cp      low(Intro_ScrollTextSize)
    jr      nz,.skip3
    xor     a
    ld      [Intro_ScrollerPos],a
    ld      [Intro_ScrollerPos+1],a
    ; fall through
.skip3
    ld      a,[Intro_ScrollerOffset]
    dec     a
    jr      nz,.skip2
    pop     hl
    ld      a,[hl+]
    ld      b,[hl]
    ld      c,a
    inc     bc
    ld      a,b
    ld      h,b
    ld      [Intro_ScrollerPos+1],a
    ld      a,c
    ld      l,c
    ld      [Intro_ScrollerPos],a
    ld      a,8
    ld      [Intro_ScrollerOffset],a
    jr      .skip
.skip2
    ld      [Intro_ScrollerOffset],a
    pop     hl
    ld      a,[hl+]
    ld      h,[hl]
    ld      l,a
.skip
    ld      bc,Intro_ScrollText
    add     hl,bc
    ld      de,OAMBuffer
    ld      b,21
.loop
    ; sprite Y pos
    push    hl
    push    bc
    ld      a,b
    dec     a
    add     a
    add     a
    add     a
    ld      b,a
    ld      a,[Intro_ScrollerOffset]
    ld      c,a
    ld      a,[Intro_ScrollerFrame]
    sub     c
    add     b
    pop     bc
    ld      h,high(Intro_ScrollerSineTable)
    ld      l,a
    ld      a,[hl]
    add     16
    ld      [de],a
    inc     e
    ; sprite x pos
    ld      a,21
    sub     b
    add     a   ; x2
    add     a   ; x4
    add     a   ; x8
    ld      l,a
    ld      a,[Intro_ScrollerOffset]
    add     l
    dec     a
    ld      [de],a
    inc e
    ; tile number
    pop     hl
    ld      a,[hl+]
    ld      [de],a
    inc     e
    ; attributes
    xor     a
    ld      [de],a
    inc     e
    dec     b
    jr      nz,.loop
    ret

Intro_ScrollText::
    db      "                     Hello there! AYCE is coming in hot with an all-new compofiller intro speedcoded just in time for Nova 2022! This was supposed to be for Lovebyte Turbo but it ended up being too big...  **  "
	db		"The music you're hearing is originally from ",$22,"Turbo Racing Challenge",$22,", an unreleased Game Boy Color game by Karma Studios. Thanks to Superogue/Marquee Design (the game's programmer) for letting me use it!  **  "
    db		"By the way, we're nowhere near done with the Game Boy yet. Be on the lookout for a BIG GBC demo from us soon...  **  "
	db      "Code: DevEd    GFX: DevEd    Font: Damien Guard    Music: Faried Verheul  **  "
    db      "Greets in no particular order: * MARQUEE DESIGN * SNORPUNG * CNCD * GENESIS PROJECT * MATT CURRENT * ABYSS * BOTB * FAIRLIGHT * TRIAD * BOILED LEAF ENJOYERS COLLECTIVE * ANYONE ELSE I MISSED  **  "
    db      "Scroller lapping around now..."
.end
    db      "                    "

Intro_ScrollTextSize = Intro_ScrollText.end-Intro_ScrollText

; ================================================================

Intro_RunCheckerboard:
    ld      a,[Intro_BGFadeOffset]
:   ld      b,a
    ld      a,[Intro_BGFadeTick]
    add     b
    ld      [Intro_BGFadeTick],a
    ldh     a,[rLCDC]
    ld      b,a
    jr      c,.bg1
.bg0
    and     LCDCF_BG9C00
    xor     b
    ldh     [rLCDC],a
    ld      a,[Intro_LogoX]
    ldh     [rSCX],a
    ld      a,[Intro_LogoY]
    ldh     [rSCY],a
	xor		a
	ld		[Intro_DoDrawChecker],a
    ret
.bg1
    or      LCDCF_BG9C00
    ldh     [rLCDC],a
	ld		a,1
	ld		[Intro_DoDrawChecker],a
    ret

; ================================================================

Intro_DrawCar:
    ; store Y offset in B + X offset in C
    ld      hl,Intro_CarY
    ld      a,[hl+]
    ld      b,[hl]
    ld      c,a

    ld      hl,Intro_CarSprite
    ld      de,OAMBuffer
:   ld      a,[hl+] ; get Y position
    cp      $ff     ; terminator byte hit?
    ret     z       ; if yes, exit
    add     c       ; add Y offset
    ld      [de],a  ; store in shadow OAM
    inc     e

    ld      a,[hl+] ; get X position
    add     b       ; add X offset
    ld      [de],a  ; store in shadow OAM
    inc     e       ; next byte

    ld      a,[hl+] ; get sprite ID
    ld      [de],a  ; store in shadow OAM
    inc     e       ; next byte

    inc     e       ; attributes aren't used here
    jr      :-      ; loop until we hit a terminator byte

; ================================================================

Intro_Load2BPP:
    WaitForVRAM
    ld      a,[hl+]
    ld      [de],a
    inc     de
    dec     bc
    ld      a,b
    or      c
    jr      nz,Intro_Load2BPP
    ret

Intro_Load1BPP:
    WaitForVRAM
    ld      a,[hl+]
    ld      [de],a
    inc     de
    ld      [de],a
    inc     de
    dec     bc
    ld      a,b
    or      c
    jr      nz,Intro_Load1BPP
    ret

; ================================================================

Intro_CarSprite:
    oamentry    -15,-40,$d8
    oamentry    -15,-32,$da
    oamentry    -15,-24,$dc
    oamentry    -15,-16,$de
    oamentry    -15, -8,$e0
    oamentry    -15,  0,$e2
    oamentry    -15,  8,$e4
    oamentry    -15, 16,$e6
    oamentry    -15, 24,$e8
    oamentry    -15, 32,$ea
    oamentry      0,-40,$ec
    oamentry      0,-32,$ee
    oamentry      0,-24,$f0
    oamentry      0,-16,$f2
    oamentry      0, -8,$f4
    oamentry      0,  0,$f6
    oamentry      0,  8,$f8
    oamentry      0, 16,$fa
    oamentry      0, 24,$fc
    oamentry      0, 32,$fe
    db          $ff

section "Scroller sine tables",rom0,align[8] ; alignment used to speed up processing
Intro_ScrollerSineTable:  ; used for scrolltext
    db  $44,$46,$47,$49,$4b,$4c,$4e,$50,$51,$53,$55,$56,$58,$59,$5b,$5c
    db  $5e,$60,$61,$63,$64,$66,$67,$68,$6a,$6b,$6d,$6e,$6f,$70,$72,$73
    db  $74,$75,$76,$77,$79,$7a,$7b,$7c,$7d,$7d,$7e,$7f,$80,$81,$81,$82
    db  $83,$83,$84,$85,$85,$86,$86,$86,$87,$87,$87,$87,$88,$88,$88,$88
    db  $88,$88,$88,$88,$88,$87,$87,$87,$87,$86,$86,$86,$85,$85,$84,$83
    db  $83,$82,$81,$81,$80,$7f,$7e,$7d,$7d,$7c,$7b,$7a,$79,$77,$76,$75
    db  $74,$73,$72,$70,$6f,$6e,$6d,$6b,$6a,$68,$67,$66,$64,$63,$61,$60
    db  $5e,$5c,$5b,$59,$58,$56,$55,$53,$51,$50,$4e,$4c,$4b,$49,$47,$46
    db  $44,$42,$41,$3f,$3d,$3c,$3a,$38,$37,$35,$33,$32,$30,$2f,$2d,$2c
    db  $2a,$28,$27,$25,$24,$22,$21,$20,$1e,$1d,$1b,$1a,$19,$18,$16,$15
    db  $14,$13,$12,$11,$0f,$0e,$0d,$0c,$0b,$0b,$0a,$09,$08,$07,$07,$06
    db  $05,$05,$04,$03,$03,$02,$02,$02,$01,$01,$01,$01,$00,$00,$00,$00
    db  $00,$00,$00,$00,$00,$01,$01,$01,$01,$02,$02,$02,$03,$03,$04,$05
    db  $05,$06,$07,$07,$08,$09,$0a,$0b,$0b,$0c,$0d,$0e,$0f,$11,$12,$13
    db  $14,$15,$16,$18,$19,$1a,$1b,$1d,$1e,$20,$21,$22,$24,$25,$27,$28
    db  $2a,$2c,$2d,$2f,$30,$32,$33,$35,$37,$38,$3a,$3c,$3d,$3f,$41,$42
	
Intro_CheckerboardSineTable:
	db	12,12,13,13,14,14,15,15,16,16,17,17,18,18,19,19
	db	20,20,20,21,21,21,22,22,22,22,23,23,23,23,23,23
	db	23,23,23,23,23,23,23,22,22,22,22,21,21,21,20,20
	db	20,19,19,18,18,17,17,16,16,15,15,14,14,13,13,12
	db	12,11,10,10,9,9,8,8,7,7,6,6,5,5,4,4
	db	3,3,3,2,2,2,1,1,1,1,0,0,0,0,0,0
	db	0,0,0,0,0,0,0,1,1,1,1,2,2,2,3,3
	db	3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11
	db	12,12,13,13,14,14,15,15,16,16,17,17,18,18,19,19
	db	20,20,20,21,21,21,22,22,22,22,23,23,23,23,23,23
	db	23,23,23,23,23,23,23,22,22,22,22,21,21,21,20,20
	db	20,19,19,18,18,17,17,16,16,15,15,14,14,13,13,12
	db	12,11,10,10,9,9,8,8,7,7,6,6,5,5,4,4
	db	3,3,3,2,2,2,1,1,1,1,0,0,0,0,0,0
	db	0,0,0,0,0,0,0,1,1,1,1,2,2,2,3,3
	db	3,4,4,5,5,6,6,7,7,8,8,9,9,10,10,11

; ================================================================

Intro_CarTiles:
    incbin  "GFX/car.2bpp"
.end

Intro_LogoTiles:
    incbin  "GFX/logo.2bpp"
.end

Intro_CheckerboardTiles:
    incbin  "GFX/checkerboard.1bpp"
.end

