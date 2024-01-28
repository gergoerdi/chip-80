module Target.HomeLab.HL4.HTP where

import Z80
import Z80.Utils
import Z80.Machine.HomeLab.HTP

import qualified Data.ByteString as BS

htpWithAutoStart :: BS.ByteString -> ASMBlock -> BS.ByteString
htpWithAutoStart label mainBlock = htp label
    [ mainBlock
    , org 0x4002 $ dw [asmOrg mainBlock, asmOrg mainBlock]
    ]
