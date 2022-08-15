; =======================
; Rebound GBC main source
; =======================

; If set to 1, enable debugging features.
DebugMode   = 1

; Defines
include "Defines.asm"

; =============
; Reset vectors
; =============
section "Reset $00",rom0[$00]
Bankswitch::    jp  _Bankswitch
    
section "Reset $08",rom0[$08]
FillRAM::       jp  _FillRAM
    
section "Reset $10",rom0[$10]
WaitVBlank::    jp  _WaitVBlank

section "Reset $18",rom0[$18]
WaitTimer::     jp  _WaitTimer

section "Reset $20",rom0[$20]
WaitLCDC::      jp  _WaitLCDC
    
section "Reset $30",rom0[$30]
DoOAMDMA::      jp  OAM_DMA

section "Reset $38",rom0[$38]
Trap::          jr  @
    
    
; ==================
; Interrupt handlers
; ==================

section "VBlank IRQ",rom0[$40]
IRQ_VBlank::    jp  DoVBlank

section "STAT IRQ",rom0[$48]
IRQ_Stat::      jp  DoStat

section "Timer IRQ",rom0[$50]
IRQ_Timer::     jp  DoTimer

section "Serial IRQ",rom0[$58]
IRQ_Serial::    jp  DoSerial

section "Joypad IRQ",rom0[$60]
IRQ_Joypad::    jp  DoJoypad

; ===============
; System routines
; ===============
    
; ================================================================
; Call HL
; ================================================================

_CallHL:
    ld      a,h
    bit     7,a
    jr      nz,@    ; trap
.skip
    jp      hl

; ==========
; ROM header
; ==========

section "ROM header",rom0[$100]

EntryPoint::
    nop
    jp  ProgramStart
if !def(BUILD_POCKET)
NintendoLogo:   ds  48,0                        ; Nintendo logo bitmap (handled by post-linking tool)
else
AnalogueLogo:   
    db      $01,$10,$CE,$EF,$00,$00,$44,$AA,$00,$74,$00,$18,$11,$95,$00,$34
    db      $00,$1A,$00,$D5,$00,$22,$00,$69,$6F,$F6,$F7,$73,$09,$90,$E1,$10
    db      $44,$40,$9A,$90,$D5,$D0,$44,$30,$A9,$21,$5D,$48,$22,$E0,$F8,$60
endc
ROMTitle:       dbp "TURBOCHARGED",15,0         ; ROM title (15 bytes)
GBCSupport:     db  $00                         ; GBC support (0 = DMG only, $80 = DMG/GBC, $C0 = GBC only)
NewLicenseCode: db  "56"                        ; new license code (2 bytes)
SGBSupport:     db  0                           ; SGB support
CartType:       db  $19
ROMSize:        db  0                           ; ROM size (handled by post-linking tool)
RAMSize:        db  0
DestCode:       db  1                           ; Destination code (0 = Japan, 1 = All others)
OldLicenseCode: db  $33                         ; Old license code (if $33, check new license code)
ROMVersion:     db  0                           ; ROM version
HeaderChecksum: db  0                           ; Header checksum (handled by post-linking tool)
ROMChecksum:    dw  0                           ; ROM checksum (2 bytes) (handled by post-linking tool)

; =====================
; Start of program code
; =====================

section fragment "Program code",rom0[$150]
ProgramStart::
    di
    ld      sp,$c800
    ; preserve A and B for later to determine console type
    push    bc
    push    af
    
    ; clear HRAM    
    ld      bc,$7f80
    xor     a
.loop
    ld      [c],a
    inc     c
    dec     b
    jr      nz,.loop
    call    CopyDMARoutine
    
; check GB type
; sets sys_GBType to 0 if DMG/SGB/GBP/GBL/SGB2, 1 if GBC, 2 if GBA/GBA SP/GB Player
    pop     af
    pop     bc
    and     1       ; a = 0 on DMG, 1 on GBC
    jr      z,:+
    add     b
:   ld      [sys_GBType],a
    
        
    ld      a,LCDCF_ON | LCDCF_BG8000 | LCDCF_BGON
    ldh     [rLCDC],a
    ld      a,IEF_VBLANK | IEF_TIMER | IEF_STAT
    ldh     [rIE],a

:   

    ; clear vars
    xor     a
    ld      [sys_CurrentFrame],a
    ld      [sys_btnPress],a
    ld      [sys_btnHold],a
    ld      [sys_VBlankFlag],a
    ld      [sys_TimerFlag],a
    ld      [sys_LCDCFlag],a

    jp  GM_Intro
    
; ================================

include "Engine/GameModes/Intro.asm"

; ================================

; ==================
; Interrupt handlers
; ==================

section "Interrupt handlers",rom0

DoVBlank::
    push    af
    push    bc
    push    de
    push    hl
    ld  a,1
    ld  [sys_VBlankFlag],a      ; set VBlank flag
    
    ld  a,[sys_CurrentFrame]
    inc a
    ld  [sys_CurrentFrame],a    ; tick frame counter

    call    Intro_RunCheckerboard

:
    ; check input
    ld      a,[sys_btnHold]
    ld      c,a
    ld      a,P1F_5
    ldh     [rP1],a
    ldh     a,[rP1]
    ldh     a,[rP1]
    cpl
    and     $f
    swap    a
    ld      b,a
    
    ld      a,P1F_4
    ldh     [rP1],a
    ldh     a,[rP1]
    ldh     a,[rP1]
    ldh     a,[rP1]
    ldh     a,[rP1]
    ldh     a,[rP1]
    ldh     a,[rP1]
    cpl
    and     $f
    or      b
    ld      b,a
    
    ld      a,[sys_btnHold]
    xor     b
    and     b
    ld      [sys_btnPress],a    ; store buttons pressed this frame
    ld      e,a
    ld      a,b
    ld      [sys_btnHold],a     ; store held buttons
    xor     c
    xor     e
    ld      [sys_btnRelease],a  ; store buttons released this frame
    ld      a,P1F_5|P1F_4
    ldh     [rP1],a
    
    rst     DoOAMDMA  
	
	xor		a
	ldh		[rLYC],a

    pop     hl
    pop     de
    pop     bc
    pop     af
    reti                            ; done!
   
DoStat::
    push    af
    ld      a,1
    ld      [sys_LCDCFlag],a
	call	Intro_RunCheckerboardLCD
    pop     af
    reti

DoTimer::
    ei      ; natt wants scanline interrupts for the checkerboard :V
    push    af

    ld      a,[GBM_EnableTimer]
    and     a
    jr      z,:+
    push    bc
    push    de
    push    hl
    call    GBM_Update
    pop     hl
    pop     de
    pop     bc

:   ld      a,1
    ld      [sys_TimerFlag],a
    pop     af
    ret

; Currently not used
DoSerial::
    push    af
    ld      a,1
    ld      [sys_SerialFlag],a
    pop     af
    reti

; Currently not used
DoJoypad::
    push    af
    ld      a,1
    ld      [sys_JoypadFlag],a
    pop     af
    reti
    
; =======================
; Interrupt wait routines
; =======================

; Wait for vertical blank.
_WaitVBlank::
    push    af
    ldh     a,[rIE]
    bit     0,a
    jr      z,.done
.wait
    halt
    ld      a,[sys_VBlankFlag]
    and     a
    jr      z,.wait
    xor     a
    ld      [sys_VBlankFlag],a
.done
    pop     af
    ret

; Wait for LCD status interrupt.
_WaitLCDC::
    push    af
    ldh     a,[rIE]
    bit     1,a
    jr      z,.done
.wait
    halt
    ld      a,[sys_LCDCFlag]
    and     a
    jr      z,.wait
    xor     a
    ld      [sys_LCDCFlag],a
.done
    pop     af
    ret

; Wait for timer interrupt. (Currently unused)
_WaitTimer::
    push    af
    ldh     a,[rIE]
    bit     2,a
    jr      z,.done
.wait
    halt
    ld      a,[sys_TimerFlag]
    and     a
    jr      z,.wait
    xor     a
    ld      [sys_VBlankFlag],a
.done
    pop     af
    ret

; Wait for serial transfer interrupt. (Currently unused)
_WaitSerial::
    push    af
    ldh     a,[rIE]
    bit     3,a
    jr      z,.done
.wait
    halt
    ld      a,[sys_SerialFlag]
    and     a
    jr      z,.wait
    xor     a
    ld      [sys_SerialFlag],a
.done
    pop     af
    ret

; Wait for joypad interrupt. (Currently unused)
_WaitJoypad: 
    push    af
    ldh     a,[rIE]
    bit     4,a
    jr      z,.done
.wait
    halt
    ld      a,[sys_JoypadFlag]
    and     a
    jr      z,.wait
    xor     a
    ld      [sys_JoypadFlag],a
.done
    pop     af
    ret
    
; =================
; Graphics routines
; =================

; =========

; Clear the screen.
; TRASHES: a, bc, hl
; RESTRICTIONS: Requires the LCD to be disabled, otherwise screen will not be properly cleared.
ClearScreen:
    ; clear VRAM bank 0
    xor     a
    ldh     [rVBK],a
    ld      hl,_VRAM        ; clear from start of VRAM...
    ld      bc,_SRAM-_VRAM   ; ...to end of VRAM.
    rst     FillRAM
    
    xor     a
    ld      hl,_VRAM        ; clear from start of VRAM...
    ld      bc,_SRAM-_VRAM   ; ...to end of VRAM.
    rst     FillRAM
    
    ; clear OAM
    ld      hl,OAMBuffer
    ld      b,OAMBuffer.end-OAMBuffer
    xor     a
    call    _FillRAMSmall

    ; reset scrolling
    xor     a
    ldh     [rSCX],a
    ldh     [rSCY],a
    ret

; Same as ClearScreen, but preserves loaded graphics.
ClearScreen2:
    ; clear VRAM bank 0
    xor     a
    ldh     [rVBK],a
    ld      hl,_SCRN0        ; clear from start of VRAM...
    ld      bc,_SRAM-_SCRN0   ; ...to end of VRAM.
    rst     FillRAM
    
    ; clear VRAM bank 1
    xor     a
    ld      hl,_SCRN0        ; clear from start of VRAM...
    ld      bc,_SRAM-_SCRN0   ; ...to end of VRAM.
    rst     FillRAM
    
    ; clear OAM
    ld      hl,OAMBuffer
    ld      b,OAMBuffer.end-OAMBuffer
    xor     a
    call    _FillRAMSmall
    
    ; reset scrolling
    xor     a
    ldh     [rSCX],a
    ldh     [rSCY],a
    ret

; Loads a 20x18 tilemap to VRAM.
; INPUT:   hl = source
; TRASHES: a, bc, de, hl
; RESTRICTIONS: Must run during VBlank or while VRAM is accessible, otherwise written data will be corrupted
LoadTilemapScreen:
    ld  de,_SCRN0
    lb  bc,$12,$14
.loop
    ld  a,[hl+]
    ld  [de],a
    inc de
    dec c
    jr  nz,.loop
    ld  c,$14
    ld  a,e
    add $c
    jr  nc,.continue
    inc d
.continue
    ld  e,a
    dec b
    jr  nz,.loop
    ret

; Same as LoadTilemapScreen, but performs ASCII conversion.
; INPUT:   hl = source
; TRASHES: a, bc, de, hl
; RESTRICTIONS: Must run during VBlank or while VRAM is accessible, otherwise written data will be corrupted
LoadTilemapText:
    ld  de,_SCRN0
    lb  bc,$12,$14
.loop
    ld  a,[hl+]
    sub " "
    ld  [de],a
    inc de
    dec c
    jr  nz,.loop
    ld  c,$14
    ld  a,e
    add $C
    jr  nc,.continue
    inc d
.continue
    ld  e,a
    dec b
    jr  nz,.loop
    ret

; Prints a null-terminated string.
; INPUT:   hl = source
;          de = destination
PrintString:
    WaitForVRAM
    ld      a,[hl+]
    and     a           ; terminator byte reached?
    ret     z           ; if yes, return
    sub     " "
    ld      [de],a
    inc     de
    jr      PrintString    

; ============
; Sprite stuff
; ============

; Copies OAM DMA routine to HRAM.
; Called once during startup sequence.
; TRASHES: a, bc, hl
CopyDMARoutine::
    ld  bc,low(OAM_DMA) + ((_OAM_DMA_End-_OAM_DMA) << 8)
    ld  hl,_OAM_DMA
.loop
    ld  a,[hl+]
    ld  [c],a
    inc c
    dec b
    jr  nz,.loop
    ret

; OAM DMA routine.
; This is copied to HRAM by CopyDMARoutine and run from there.
; TRASHES: a
_OAM_DMA::
    ld  a,high(OAMBuffer)
    ldh [rDMA],a
    ld  a,$28
.wait   ; wait for OAM DMA to finish
    dec a
    jr  nz,.wait
    ret
_OAM_DMA_End:

; =============
; Misc routines
; =============

; include "Engine/GBPrinter.asm"

; ================

; Performs a bankswitch to bank B, preserving previous ROM bank.
; INPUT:    b = bank
_Bankswitch:
    push    af
    ldh     a,[sys_CurrentBank]
    ldh     [sys_LastBank],a        ; preserve old ROM bank
    ld      a,b
    ldh     [sys_CurrentBank],a     ; set new ROM bank
    ld      [rROMB0],a              ; perform bankswitch
    pop     af
    ret
    
; Fill RAM with a value.
; INPUT:    a = value
;          hl = address
;          bc = size
; TRASHES: a, bc, e, hl
_FillRAM::
    ld  e,a
.loop
    ld  [hl],e
    inc hl
    dec bc
    ld  a,b
    or  c
    jr  nz,.loop
    ret
    
; Fill up to 256 bytes of RAM with a value.
; INPUT:    a = value
;          hl = address
;           b = size
; TRASHES: a, b, e, hl
_FillRAMSmall::
    ld  e,a
.loop
    ld  [hl],e
    inc hl
    dec b
    jr  nz,.loop
    ret

; ================
    
; Copy up to 65536 bytes to RAM.
; INPUT:   hl = source
;          de = destination
;          bc = size
; TRASHES: a, bc, de, hl
_CopyRAM::
    ld  a,[hl+]
    ld  [de],a
    inc de
    dec bc
    ld  a,b
    or  c
    jr  nz,_CopyRAM
    ret
    
; Copy up to 256 bytes to RAM.
; INPUT:   hl = source
;          de = destination
;           b = size
; TRASHES: a, b, de, hl
_CopyRAMSmall::
    ld  a,[hl+]
    ld  [de],a
    inc de
    dec b
    jr  nz,_CopyRAMSmall
    ret

; ================
    
; ==============
; Sound routines
; ==============

include "Audio/GBMod_Player.asm"

; =============
; Graphics data
; =============

section "Font",rom0
Font::
    incbin  "GFX/font.1bpp"
.end

section "GBMod module - Turbo Racing Challenge Menu",romx[$4000]
Module_Turbo:
    incbin  "Audio/Modules/Turbo.bin"
