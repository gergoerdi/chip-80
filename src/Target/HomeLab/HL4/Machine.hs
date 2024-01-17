module Target.HomeLab.HL4.Machine (machine_) where

import Target.HomeLab.HL4.Defs
import CHIP80.CPU
import Target.HomeLab.HL4.Input
import Target.HomeLab.HL4.Video64
import ZX0

import Z80
import Z80.Utils
import Control.Monad

-- | Pre: `IX` contains address of quirks settings
-- | Pre: `IY` contains address of compressed program
machine_ :: Location -> Z80ASM
machine_ baseAddr = mdo
    call init

    -- Uncompress program into CHIP-8 RAM
    push IY
    pop HL
    ld DE $ baseAddr + 0x200
    call uncompress

    -- Run CPU...
    call resetCPU
    loopForever do
        call stepCPU

        -- Check for end of vblank
        ld A [0xe802]
        Z80.bit 0 A
        unlessFlag NZ do
            call scanKeys
            ret Z          -- ... until RUN/BRK is pressed
            call newFrame

            -- Wait for start of vblank
            withLabel \waitFrame2 -> do
                ld A [0xe802]
                Z80.bit 0 A
                jp Z waitFrame2

    uncompress <- labelled standardFwd

    let platform = Platform{..}
          where
            readKeys = Nothing
    CPU{..} <- cpu platform

    clearScreen <- labelled do
        ld HL windowStart
        ld DE $ rowstride - fromIntegral windowWidth
        pageVideo
        decLoopB (fromIntegral windowHeight) do
            ld C B
            decLoopB (fromIntegral windowWidth) do
                ld [HL] 0x20
                inc HL
            add HL DE
            ld B C
        pageRAM
        ret

    spritePre <- labelled do
        ld [spriteX] A
        push AF
        ldVia A [spriteY] C
        ldVia A [spriteH] B
        pop AF
        ret

    spritePost <- labelled do
        pageVideo
        call spritePost'
        pageRAM
        ret

    spritePost' <- label
    ld A 0x00
    spriteX <- subtract 1 <$> label
    ld C 0x00
    spriteY <- subtract 1 <$> label
    ld B 0x00
    spriteH <- subtract 1 <$> label
    drawSprite vidBuf

    -- Scan the keyboard and write its state to the 16 bytes starting at `keyBuf`
    scanKeys <- labelled $ scanKeys_ keyBuf
    pure ()
