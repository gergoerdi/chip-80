{-# LANGUAGE NumericUnderscores, BlockArguments, BinaryLiterals, RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
module Z80.BCD where

import Z80
import Z80.Utils

import Data.Word
import Data.Foldable (for_)

-- | Pre: `HL` is pointer to a `numBytes`-byte unsigned int in little-endian
-- | Post: `HL` is pointer to `numDigits`-digit decimal number
-- Based on https://artemis.sh/2014/11/06/z80-assembly-binary-coded-decimal.html
toBCD :: Word8 -> Word8 -> Z80ASM
toBCD numBytes numDigits = mdo
    -- Copy input to `src`
    ld DE src
    ld BC $ fromIntegral numBytes
    ldir
    ld IX src

    -- Clear out `dst`
    clearA
    ld HL dst
    decLoopB numDigits do
        ld [HL] A
        inc HL

    decLoopB (numBytes * 8) do
        ld HL dst
        ld C numDigits
        -- Iterate through each BCD digit. If digit > 4, add 3
        withLabel \incLoop -> do
            ld A [HL]
            skippable \lessThan4 -> do
                cp 5
                jr C lessThan4
                add A 3
            ld [HL] A
            inc HL

            dec C
            jr NZ incLoop

        -- Shift SRC bits
        sla [IX]
        for_ [1 .. numBytes - 1] \i -> do
            rl [IX + fromIntegral i]

        ld HL dst
        ld C numDigits
        withLabel \shiftLoop -> do
            ld A [HL]
            rla
            skippable \not4 -> do
                Z80.bit 4 A
                jr Z not4
                Z80.and 0x0f -- Mask out high bits, since we only want the lower 4 bits for the digit
                scf          -- Set carry if bit 4 is set
            ld [HL] A
            inc HL

            dec C
            jr NZ shiftLoop

    ld HL dst
    ret

    src <- labelled $ db $ replicate (fromIntegral numBytes) 0
    dst <- labelled $ db $ replicate (fromIntegral numDigits) 0
    pure ()
