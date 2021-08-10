****************************************************************** 
;  Example inspired by Photon's Tutorial:
;  https://www.youtube.com/user/ScoopexUs
;  expanded and modified 20201 Hank van Bastard
****************************************************************** 
;---------- Includes ----------
              INCDIR      "include"
              INCLUDE     "hw.i"
              INCLUDE     "funcdef.i"
              INCLUDE     "exec/exec_lib.i"
              INCLUDE     "graphics/graphics_lib.i"
              INCLUDE     "hardware/cia.i"

******************************************************************       
;---------- Const ----------
CIAA         = $00bfe001

ScreenW      = 320
ScreenH      = 256
ScreenBpls   = 4
ScreenBpl    = ScreenW/8
ScreenBtl    = ScreenBpl*ScreenBpls

CopBarMin    = $2c
CopBarHeight = 9
CopBarMax    = $2c+LogoH-10

BmpW         = 320                                                                                      ;bitmap witdth
BmpH         = ScreenH-LogoH-ScrollerH                                                                  ;bitmap height
BmpMargin    = (BmpW-ScreenW)/2                                                                         ;bitmap H margin
BmpBpls      = 4                                                                                        ;no. of bitplanes
BmpBpl       = BmpW/8                                                                                   ;bitmap bytes per line
BmpBtl       = BmpBpl*BmpBpls                                                                           ;bitmap byte-width
              
LogoW        = 176
LogoH        = 126
LogoMargin   = (ScreenW-LogoW)/2                                                                        ;margin/2 because ddfstart works in 4px steps
LogoBpls     = 4                                                                                        ;no. of bitplanes
LogoBpl      = LogoW/8                                                                                  ;bitmap bytes per line
LogoBtl      = LogoBpl*LogoBpls
                                                                                                             
ScrollerW    = 320+32                                                                                   ;bitmap witdth
ScrollerH    = 32                                                                                       ;bitmap height
ScrollerBpls = 4                                                                                        ;no. of bitplanes
ScrollerBpl  = ScrollerW/8                                                                              ;bitmap bytes per line
ScrollerBtl  = ScrollerBpl*ScrollerBpls                                                                 ;bitmap byte-width

FontW        = 320
FontH        = 256
FontBpls     = 4
FontBpl      = FontW/8
FontBtl      = FontBpl*FontBpls

CharW        = 32
CharH        = 32
CharBpl      = CharW/8*FontBpls
CharBtl      = CharBpl*CharH

CharSW       = 16
CharSH       = 16
CharSBpl     = CharSW/8*FontBpls
CharSBtl     = CharSBpl*CharSH

PrintSpeed   = 4

******************************************************************       
;main loop
start:              
              movem.l     d0-a6,-(sp)                                                                   ;save registers to stack
              jsr         init              

mainloop:
		; Wait for vertical blank
              move.w      #$02a,d0                                                                      ;No buffering, so wait until raster
              bsr.w       WaitRaster                                                                    ;is below the Display Window.

              jsr         Copperbar

              cmp.b       #PrintSpeed,PrintCnt
              bne.b       .noprint             
              move.b      #0,PrintCnt
              jsr         Blittext
.noprint
              add.b       #1,PrintCnt
              
              jsr         Scrollit

              
checkmouse:
              btst        #CIAB_GAMEPORT0,CIAA+ciapra
              bne.b       mainloop

exit:
              move.w      #$7fff,DMACON(a6)                                                             ; disable all bits in DMACON
              or.w        #$8200,(DMACONSave)                                                           ; Bit mask inversion for activation
              move.w      (DMACONSave),DMACON(a6)                                                       ; Restore values
              move.l      (CopperSave),COP1LC(a6)                                                       ; Restore values
              or          #$c000,(INTENARSave)         
              move        (INTENARSave),INTENA(a6)                                                      ; interruptions reactivation
              movem.l     (sp)+,d0-a6
              clr         d0                                                                            ; Return code of the program
              rts                                                                                       ; End
	
******************************************************************       
init:
              move.l      4.w,a6                                                                        ;execbase
              move.l      #gfxname,a1
              jsr         -408(a6)                                                                      ;oldopenlibrary()
              move.l      d0,a1
              move.l      38(a1),CopperSave                                                             ;original copper ptr
              jsr         -414(a6)                                                                      ;closelibrary()                                                                     ; custom chip registers baseadress

              lea         CUSTOM,a6
              move.w      INTENAR(a6),INTENARSave                                                       ; save INTENAR  
              move.w      DMACONR(a6),DMACONSave                                                        ; save DMACONR 
              move.w      #$138,d0                                                                      ; wait for eoframe
              bsr.w       WaitRaster                              
              move.w      #$7fff,INTENA(a6)                                                             ; disable all bits in INTENA
              move.w      #$7fff,INTREQ(a6)                                                             ; disable all bits in INTREQ
              move.w      #$7fff,INTREQ(a6)                                                             ; disable all bits in INTREQ
              move.w      #$7fff,DMACON(a6)                                                             ; disable all bits in DMACON
              move.w      #$87e0,DMACON(a6)                                                             ; activate DMACON for classic demos setup
              move.w      #$8440,DMACON(a6)                                                             ; enable blitter DMA
              move.l      #Copperlist,COP1LC(a6)                                                        ; activate copperlist
              move.w      #$0,COPJMP1(a6)                                                               ; restart adress for COP1LC

              move.l      #BmpBpls-1,d0                                                                 ; write the bitplane adresses to the copper list
              lea         BmpScreen,a0                        
              lea         CopScreenP,a1                           
.bpll
              move.l      a0,d1
              move.w      d1,6(a1)              
              swap        d1                                                                
              move.w      d1,2(a1)
              lea         8(a1),a1
              lea         BmpBpl(a0),a0              
              dbf         d0,.bpll

              move.l      #LogoBpls-1,d0                                                                ;write the bitplane adresses to the copper list
              lea         BmpLogo,a0                        
              lea         CopLogoP,a1                           
.bpllogo
              move.l      a0,d1
              move.w      d1,6(a1)              
              swap        d1                                                                
              move.w      d1,2(a1)
              lea         8(a1),a1
              lea         LogoBpl(a0),a0              
              dbf         d0,.bpllogo

              move.l      #FontBpls-1,d0                                                                ;write the bitplane adresses to the copper list
              lea         BmpScroller,a0                        
              lea         CopScrollP,a1                           
.bplScroll
              move.l      a0,d1
              move.w      d1,6(a1)              
              swap        d1                                                                
              move.w      d1,2(a1)
              lea         8(a1),a1
              lea         ScrollerBpl(a0),a0              
              dbf         d0,.bplScroll

              lea         CopSprP,a0                                                                    ;init sprite pointers
              lea         Sprite1,a1
              move.l      a1,d1
              move.w      d1,6(a0)
              swap        d1
              move.w      d1,2(a0)

              moveq       #6,d0                                                                         ;make sprite 1-7 invisible
              lea         NullSpr,a1
              move.l      a1,d1
.sploop       addq.l      #8,a0
              move.w      d1,6(a0)
              swap        d1
              move.w      d1,2(a0)
              dbf         d0,.sploop
              rts

******************************************************************       
;Wait for scanline in d0. Trashes d1.
WaitRaster:   
.l:           move.l      $dff004,d1
              lsr.l       #1,d1
              lsr.w       #7,d1
              cmp.w       d0,d1
              bne.s       .l                                                                            ;wait until it matches (eq)
              rts
       
******************************************************************
;Copperbar
Copbaracc    = 2
Copbardec    = 2

Copperbar:
              lea         CopBar,a0 
              cmp.b       #CopBarMin,(a0)
              bhi.b       .cbok1
              move.b      #1,CopbarDir
.cbok1:
              cmp.b       #CopBarMax,(a0)
              blo.b       .cbok2
              move.b      #-1,CopbarDir
.cbok2:
              
              move.b      CopbarDir,d2
              add.b       d2,(a0)
              lea         CopBarCol,a0 
              move.w      #CopBarHeight,d0
.cbloop:
              add.b       d2,(a0)
              lea         8(a0),a0              
              dbf         d0,.cbloop
              rts
******************************************************************
;    ---  scrolltext scroll ---
BltY         = 0
Bltoffs      = BltY*ScrollerBtl
Blth         = 32
Bltw         = ScrollerW/16
Bltskip      = 0                                                                   
Brcorner     = Blth*ScrollerBtl-2

Scrollit:
              movem.l     d0-a6,-(sp)
              bsr         BlitWait

              move.l      #$49f00002,BLTCON0(a6)
              move.l      #$ffffffff,BLTAFWM(a6)
              move.l      #$ffffffff,BLTALWM(a6)
              move.l      #BmpScroller+Bltoffs+Brcorner,BLTAPTH(a6)
              move.l      #BmpScroller+Bltoffs+Brcorner,BLTDPTH(a6)
              move.w      #Bltskip,BLTAMOD(a6)
              move.w      #Bltskip,BLTDMOD(a6)
              move.w      #((Blth*ScrollerBpls)<<6)+Bltw,BLTSIZE(a6)

              movem.l     (sp)+,d0-a6

              add.b       #1,ScrollCnt
              and.b       #%00000111,ScrollCnt
              cmp.b       #0,ScrollCnt
              bne.b       .scitexit
              nop
              bsr.b       PlotChar
.scitexit
              rts
       
******************************************************************
;    ---  blits char to scrolltext  ---

PlotChar:
              bsr         BlitWait

              move.l      #0,d0

              lea         Scrolltext,a0
              lea         ScrollTextP,a1
              bsr.b       ReadChar
              
              cmp.b       #32,d0                                                                        ; check bounds of char
              blo.b       plcexit
              cmp.b       #32+80,d0
              bhi.b       plcexit

              move.l      #0,d2
              move.l      #0,d3
              sub.b       #32,d0
              divu        #10,d0
              move.b      d0,d2
              swap        d0
              move.b      d0,d3
              lsl.w       #2,d3
              move.l      #FontBtl,d1
              mulu        #CharH,d1
              mulu        d2,d1
              add.l       d3,d1
              lea         BitmapFont,a0              
              add.l       d1,a0              
.pcplot
              move.l      #$09f00000,BLTCON0(a6)
              move.l      #$ffffffff,BLTAFWM(a6)             
              move.l      a0,BLTAPTH(a6)
              move.l      #BmpScroller+(BltY*ScrollerBtl)+40,BLTDPTH(a6)
              move.w      #(FontW-CharW)/8,BLTAMOD(a6)
              move.w      #(ScrollerW-CharW)/8,BLTDMOD(a6)
              move.w      #((CharH*FontBpls)<<6)+(CharW/16),BLTSIZE(a6)
plcexit:
              rts

******************************************************************
; TextP < a0, TextCnt < a1, Ascci > d0
ReadChar:
              move.l      #0,d1
              move.w      (a1),d1
              move.b      (a0,d1),d0
              bne.b       .rccont
              move.w      #0,(a1)
              move.l      #0,d0
              move.l      #0,a1
              bra         .rcexit
.rccont:
              add.w       #1,(a1)
.rcexit
              rts

*******************************************************************	
BlitWait:
              tst         DMACONR(a6)                                                                   ;for compatibility
.waitblit:
              btst        #6,DMACONR(a6)
              bne.s       .waitblit
              rts
       
******************************************************************
;blit a text to the screen
Blittext:
              move.l      #0,d0
              lea         Printtext,a0
              lea         PrinttextP,a1

              bsr.b       ReadChar
              
              cmp.b       #91,d0
              bne.b       .btnnl
              add.w       #CharSH,PrinttextY
              move.w      #0,PrinttextX
              bra         .ptexit
.btnnl
              cmp         #0,a1
              bne         .ptnoreset
              move.w      #0,PrinttextX
              move.w      #16,PrinttextY

.ptnoreset
              cmp.b       #32,d0                                                                        ; check bounds of char
              blo         .ptexit
              cmp.b       #32+80,d0
              bhi         .ptexit
              ;calculate blit-source
              move.l      #0,d2                                                                          
              move.l      #0,d3
              sub.b       #32,d0                                                                        ;spc is our first char
              divu        #20,d0                                                                        ;20char/line
              move.b      d0,d2                                                                         ;row (div)
              swap        d0
              move.b      d0,d3                                                                         ;column (remainder)
              lsl.w       #1,d3                                                                         ;mul 4 = no bytes
              move.l      #FontBtl,d1                                                                   ;bytes per line 
              mulu        #CharSH,d1                                                                    ;char height
              mulu        d2,d1
              add.l       d3,d1
              lea         BitmapFontSmall,a0              
              add.l       d1,a0       
              move.l      a0,BLTAPTH(a6)
              ;calculate blit-dest
              lea         BmpScreen,a0
              move.l      #BmpBtl,d0
              mulu        PrinttextY,d0
              move.w      PrinttextX,d1 
              lsr.w       #3,d1
              add.l       d1,d0
              add.l       d0,a0
              move.l      a0,BLTDPTH(a6)
              
              move.l      #$09f00000,BLTCON0(a6)
              move.l      #$ffffffff,BLTAFWM(a6)             
              move.l      #$ffffffff,BLTALWM(a6)
              move.w      #(FontW-CharSW)/8,BLTAMOD(a6)
              move.w      #(BmpW-CharSW)/8,BLTDMOD(a6)
              move.w      #((CharSH*FontBpls)<<6)+(CharSW/16),BLTSIZE(a6)
              add.w       #CharSW,PrinttextX
.ptexit
              rts

******************************************************************
gfxname:
              GRAFNAME                                                                                  ; inserts the graphics library name

BarDir:       dc.b        1
PrintCnt:     dc.b        0
Scrolltext:   dc.b        "#2021# HANK VAN BASTARD.  I'M HAVING A GOOD TIME HERE. "
              dc.b        "AMIGA CODING IS FUN. FOLLOWING THE PATH OF SCOOPEX' PHOTON. "
              dc.b        "I LOVE YOUR YOUTUBE AMIGA HARDWARE PROGRAMMING TUTORIALS. "
              dc.b        "GREAT STUFF MAN - HOPE YOU CONTINUE AND I CAN FOLLOW YOU FOR A WHILE. "
              dc.b        "GOODNIGHT AND KEEP GOING .... HANK LOVES FRESH BLACKBERRIES           ",0

Printtext:    dc.b        "    KEEP THE AMIGA[     SPIRIT ALIVE[       FOREVER",0
ScrollCnt:    dc.b        0             
CopbarDir:    dc.b        1
              EVEN
ScrollTextP:  dc.w        0
ScrollTextY:  dc.w        160
PrinttextP:   dc.w        0
PrinttextX:   dc.w        0
PrinttextY:   dc.w        16
DMACONSave:   DC.w        1
CopperSave:   DC.l        1
INTENARSave:  DC.w        1

******************************************************************
              SECTION     SEC_CopperList,DATA_C 

Copperlist:  
              dc.w        $1fc,0                                                                        ;slow fetch mode, AGA compatibility
              dc.w        BPLCON0,$2000                                                                 ;don't turn on bitmaps until output sizes are set
              dc.w        DIWSTRT
              dc.b        $2c,$81                                                                       ;define display area top-left
              dc.w        DIWSTOP
              dc.b        $2c,$c1                                                                       ;define display area bottom-right
              dc.w        DDFSTRT,$38+LogoMargin/2
              dc.w        DDFSTOP,$d0-LogoMargin/2
              dc.w        BPL1MOD,LogoBtl-LogoBpl
              dc.w        BPL2MOD,LogoBtl-LogoBpl
              
              include     "images/hvb.cop.s"
              ;sprite colors
              dc.w        COLOR17,$fc4                           
              dc.w        COLOR18,$000
              dc.w        COLOR19,$a00
CopSprP:
              ; sprite pointers
              dc.w        SPR0PTH,0
              dc.w        SPR0PTL,0                              
              dc.w        SPR1PTH,0
              dc.w        SPR1PTL,0
              dc.w        SPR2PTH,0
              dc.w        SPR2PTL,0
              dc.w        SPR3PTH,0
              dc.w        SPR3PTL,0
              dc.w        SPR4PTH,0
              dc.w        SPR4PTL,0
              dc.w        SPR5PTH,0
              dc.w        SPR5PTL,0
              dc.w        SPR6PTH,0
              dc.w        SPR6PTL,0
              dc.w        SPR7PTH,0
              dc.w        SPR7PTL,0
CopLogoP:
              dc.w        BPL1PTH,$0                                                                    ;bitplane pointers
              dc.w        BPL1PTL,$0
              dc.w        BPL2PTH,$0
              dc.w        BPL2PTL,$0
              dc.w        BPL3PTH,$0
              dc.w        BPL3PTL,$0
              dc.w        BPL4PTH,$0
              dc.w        BPL4PTL,$0 
              dc.w        BPLCON0,$4200

              dc.w        COLOR00,$000

CopBar:             
              dc.w        $3ddf,COPPER_HALT 
              dc.w        COLOR13,$fff
              dc.w        COLOR14,$fd0
              dc.w        COLOR15,$f00
CopBarCol:
              dc.w        $3c07,COPPER_HALT 
              dc.w        COLOR00,$333
              dc.w        $3d07,COPPER_HALT            
              dc.w        COLOR00,$666
              dc.w        $3e07,COPPER_HALT            
              dc.w        COLOR00,$999
              dc.w        $3f07,COPPER_HALT            
              dc.w        COLOR00,$ccc
              dc.w        $4007,COPPER_HALT            
              dc.w        COLOR00,$fff
              dc.w        $4107,COPPER_HALT            
              dc.w        COLOR00,$ccc
              dc.w        $4207,COPPER_HALT            
              dc.w        COLOR00,$999
              dc.w        $4307,COPPER_HALT            
              dc.w        COLOR00,$666
              dc.w        $4407,COPPER_HALT            
              dc.w        COLOR00,$333
              dc.w        $4507,COPPER_HALT              
              dc.w        COLOR13,$06f
              dc.w        COLOR14,$19e
              dc.w        COLOR15,$00a
              dc.w        COLOR00,$000

              dc.w        $a9cf,COPPER_HALT            
              dc.w        BPLCON0,$0200
CopScreenP:
              dc.w        BPL1PTH,$0                                                                    
              dc.w        BPL1PTL,$0
              dc.w        BPL2PTH,$0
              dc.w        BPL2PTL,$0
              dc.w        BPL3PTH,$0
              dc.w        BPL3PTL,$0
              dc.w        BPL4PTH,$0
              dc.w        BPL4PTL,$0 
              dc.w        DDFSTRT,$38                                                                   
              dc.w        DDFSTOP,$d0                                                                   
              dc.w        BPL1MOD,BmpBtl-ScreenBpl
              dc.w        BPL2MOD,BmpBtl-ScreenBpl
              dc.w        BPLCON0,$4200                                                                 ;enable 4 bitplanes
              include     "images/16x16_font_hank_01_cop.s"                                             ;bitplane modulo for even bitplanes

              dc.w        $ffdf,COPPER_HALT            
              dc.w        $0ddf,COPPER_HALT
              dc.w        BPLCON0,$0200
CopScrollP:
              dc.w        BPL1PTH,$0                                                                    
              dc.w        BPL1PTL,$0
              dc.w        BPL2PTH,$0
              dc.w        BPL2PTL,$0
              dc.w        BPL3PTH,$0
              dc.w        BPL3PTL,$0
              dc.w        BPL4PTH,$0
              dc.w        BPL4PTL,$0 
              dc.w        BPL1MOD,ScrollerBtl-ScreenBpl
              dc.w        BPL2MOD,ScrollerBtl-ScreenBpl
              dc.w        BPLCON0,$4200                                                                 ;enable 4 bitplanes
              include     "images/font_hank_01_col.s"                                                   ;bitplane modulo for even bitplanes


;end of copperlist
              dc.w        $ffff,COPPER_HALT   

******************************************************************
              cnop        2,0
Sprite1:
              dc.w        $2ca0,$3c00                                                                   ;Vstart.b,Hstart/2.b,Vstop.b,%A0000SEH
              dc.w        %0000011111000000,%0000000000000000
              dc.w        %0001111111110000,%0000000000000000
              dc.w        %0011111111111000,%0000000000000000
              dc.w        %0111111111111100,%0000000000000000
              dc.w        %0110011111001100,%0001100000110000
              dc.w        %1110011111001110,%0001100000110000
              dc.w        %1111111111111110,%0000000000000000
              dc.w        %1111111111111110,%0000000000000000
              dc.w        %1111111111111110,%0010000000001000
              dc.w        %1111111111111110,%0001100000110000
              dc.w        %0111111111111100,%0000011111000000
              dc.w        %0111111111111100,%0000000000000000
              dc.w        %0011111111111000,%0000000000000000
              dc.w        %0001111111110000,%0000000000000000
              dc.w        %0000011111000000,%0000000000000000
              dc.w        %0000000000000000,%0000000000000000
              dc.w        0,0

NullSpr:
              dc.w        $a20,$2b00
              dc.w        0,0
              dc.w        0,0

              SECTION     BmpData, DATA_C
******************************************************************
;the bitmaps
BmpScreen:      
              dcb.b       BmpH*BmpBtl,0                                                                 ;-LogoH*LogoBtl
              
BmpScreenE:

BmpLogo:      
              incbin      "images/hvb.raw"              
BmpLogoE:
BmpScroller:
              dcb.b       ScrollerH*ScrollerBtl,0
BmpScrollerE:
BitmapFont:
              incbin      "images/32x32_font_hank_01.raw"
BitmapFontE:
BitmapFontSmall:
              incbin      "images/16x16_font_hank_01.raw"
BitmapFontSmallE:

******************************************************************

              END

Bit	Channel
              ABCD        -> D
0	000	0
1	001	0
2	010	0
3	011	0
4	100	1
5	101	1
6	110	1
7	111	1

%11110000	=$f0
******************************************************************
