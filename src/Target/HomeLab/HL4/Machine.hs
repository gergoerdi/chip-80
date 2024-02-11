module Target.HomeLab.HL4.Machine (machine_) where

import Z80.Machine.HomeLab.HL34
import CHIP80.CPU
import Target.HomeLab.HL4.Input
import Target.HomeLab.HL4.Video64

import Z80
import Z80.Utils
import Z80.ZX0
import Control.Monad

-- | Pre: `IX` contains address of quirks settings followed by the compressed program
-- | Pre: `IY` contains address of joystick settings
machine_ :: Location -> Z80ASM
machine_ baseAddr = mdo
    pageIO

    -- Store joystick keys, reordering them to match HL-4 keyboard encoding
    ldVia A [joyKeys + 1] [IY + 0]
    ldVia A [joyKeys + 0] [IY + 1]
    ldVia A [joyKeys + 3] [IY + 2]
    ldVia A [joyKeys + 2] [IY + 3]
    ldVia A [joyKeys + 4] [IY + 4]

    -- Initialize CHIP-8 engine. This clobbers `IX`, and we'll need to
    -- transfer it to `HL` before unpacking anyway.
    push IX
    call init

    -- Uncompress program into CHIP-8 RAM
    pop HL
    ld DE 5 -- skip 5 bytes of quirks
    add HL DE

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
        decLoopB (fromIntegral windowHeight) do
            ld C B
            decLoopB (fromIntegral windowWidth) do
                ld [HL] 0x20
                inc HL
            add HL DE
            ld B C
        ret

    spritePre <- labelled do
        ld [spriteX] A
        push AF
        ldVia A [spriteY] C
        ldVia A [spriteH] B
        pop AF
        ret

    spritePost <- labelled do
        call spritePost'
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
    scanKeys <- labelled $ scanKeys_ joyKeys keyBuf
    joyKeys <- labelled $ db $ replicate 5 0x0
    pure ()
