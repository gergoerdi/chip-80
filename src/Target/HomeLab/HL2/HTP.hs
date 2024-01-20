module Target.HomeLab.HL2.HTP where

import Z80
import Z80.Utils
import Target.HomeLab.HTP

import qualified Data.ByteString as BS

htpWithAutoStart :: BS.ByteString -> ASMBlock -> BS.ByteString
htpWithAutoStart label mainBlock = htp 100 label
    [ org 0x4002 $ dw [asmOrg mainBlock]
    , mainBlock
    ]
