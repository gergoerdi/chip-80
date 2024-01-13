module Target.HomeLab.HL2.Machine (machine_) where

import Target.HomeLab.HL2.Defs
import CHIP80.CPU
import CHIP80.Quirks
import Target.HomeLab.HL2.Input
import Target.HomeLab.HL2.Video
import ZX0

import Z80
import Z80.Utils

-- | Pre: `IX` contains address of quirks settings
-- | Pre: `IY` contains address of compressed program
machine_ :: Location -> Z80ASM
machine_ baseAddr = mdo
    ldVia A [shiftVY] [IX + 0]
    ldVia A [resetVF] [IX + 1]
    ldVia A [incrementPtr] [IX + 2]
    ldVia A [videoWait] [IX + 3]
    ldVia A [clipSprites] [IX + 4]

    call init

    -- Uncompress program into CHIP-8 RAM
    push IY
    pop HL
    ld DE $ baseAddr + 0x200
    call uncompress

    call resetCPU

    -- Run CPU...
    loopForever do
        call stepCPU
        ld HL lastFrame
        ld A [0x403f]
        cp [HL]
        unlessFlag Z do
            ld [HL] A
            call scanKeys
            ret Z              -- ... until RUN/BRK is pressed
            call newFrame
    lastFrame <- labelled $ db [0]

    uncompress <- labelled standardFwd
    ~quirks@Quirks{..} <- do
        shiftVY <- labelled $ db [1]
        resetVF <- labelled $ db [1]
        incrementPtr <- labelled $ db [1]
        videoWait <- labelled $ db [1]
        clipSprites <- labelled $ db [1]
        pure Quirks{..}


    let platform = Platform{..}
    CPU{..} <- cpu quirks platform

    clearScreen <- labelled do
        ld HL $ videoStart + 4 + 40 * 3
        ld DE 8
        decLoopB 16 do
            ld C B
            decLoopB 32 do
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

    spritePost <- label
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
