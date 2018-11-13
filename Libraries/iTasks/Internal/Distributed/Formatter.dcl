definition module iTasks.Internal.Distributed.Formatter

import StdMaybe
import symbols_in_program

deserializeFromBase64 :: String !{#Symbol} -> a

serializeToBase64 :: a -> String
