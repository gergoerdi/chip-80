{-# LANGUAGE BlockArguments #-}
module LFSR where

import Z80
import Z80.Utils

-- | An 10-bit maximal LFSR
-- | Pre: `DE` is the current state
-- | Post: `DE` is the new state
-- | Clobbers: `A`
lfsr10 :: Z80ASM
lfsr10 = do
    srl D
    ld A E
    rra
    ld E A
    ret NC
    Z80.xor 0x04
    ld E A
    ld A 0x02
    Z80.xor D
    ld D A
    ret
