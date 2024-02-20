module Target.TVC.Machine (machine_) where

import CHIP80.CPU
import Z80.Machine.TVC.Defs
import Target.TVC.Input
import Target.TVC.Video

import Z80.ZX0
import Z80
import Z80.Utils

windowStart :: Location
windowStart = (videoStart + 32 * 64)

-- | Pre: `HL` contains address of quirks settings
-- | Pre: `IY` contains address of compressed program
machine_ :: Location -> Z80ASM
machine_ baseAddr = mdo
    call init

    -- Uncompress program into CHIP-8 RAM
    push IY
    pop HL
    ld DE $ baseAddr + 0x200
    call uncompress

    clearFullScreen

    -- Set up interrupt handler to redraw screen
    di
    setInterruptHandler intHandler
    setupLineInt 239
    ei

    -- Run CPU
    call resetCPU
    loopForever do
        call stepCPU
        -- TODO: `ret` if RUN/BRK is pressed

    uncompress <- labelled standardFwd

    keyBuf1 <- labelled $ db $ replicate 16 0
    keyBuf2 <- labelled $ db $ replicate 16 0
    currentKeybuf <- labelled $ db [0]
    readKeys <- Just <$> labelled do
        push AF
        push BC
        push DE
        push HL
        ld DE keyBuf
        ld BC 16
        ld HL keyBuf2
        skippable \useKeyBuf2 -> do
            ld A [currentKeybuf]
            cp 0xff
            jp NZ useKeyBuf2
            ld HL keyBuf1
        ldir
        pop HL
        pop DE
        pop BC
        pop AF
        ret


    intHandler <- labelled do
        push AF
        push BC
        push DE
        push HL
        push IX
        push IY
        out [0x07] A

        -- -- Set border color to dark green
        -- ld A 0b00_10_00_00
        -- out [0x00] A

        ld HL keyBuf1
        skippable \useKeyBuf1 -> do
            ld A [currentKeybuf]
            cp 0xff
            jp NZ useKeyBuf1
            ld HL keyBuf2
        cpl
        ld [currentKeybuf] A

        call scanKeys
        -- blitPicture frameBuf
        call newFrame

        -- -- Set border color to red
        -- ld A 0b00_00_10_00
        -- out [0x00] A

        pop IY
        pop IX
        pop HL
        pop DE
        pop BC
        pop AF
        ei
        ret

    let platform = Platform{..}
    CPU{..} <- cpu platform

    clearScreen <- labelled do
        pageVideo
        clearPicture 4 windowStart
        pageRAM
        ret

    spriteX <- labelled $ db [0]
    spriteY <- labelled $ db [0]
    spriteH <- labelled $ db [0]

    spritePre <- labelled do
        ld [spriteX] A
        push AF
        ldVia A [spriteY] C
        ldVia A [spriteH] B
        pop AF
        ret

    spritePost <- labelled do
        ldVia A B [spriteH]
        ldVia A C [spriteY]
        ld A [spriteX]
        pageVideo
        -- drawPicture 1 vidBuf frameBuf
        drawPicture 4 vidBuf windowStart
        pageRAM
        ret

    frameBuf <- labelled $ db $ replicate (64 * 32) 0

    scanKeys <- labelled scanKeys_

    -- scanKeys <- labelled do
    --     ld A 0
    --     ld DE keyBuf
    --     decLoopB 16 do
    --         ld [DE] A
    --         inc DE
    --     ret
    pure ()
  where
    setupLineInt y = do
        let (lo, hi) = wordBytes $ y * 16 {-+ 63-} -- - 46
        crtcOut 0x0e hi
        crtcOut 0x0f lo
