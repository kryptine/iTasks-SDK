definition module GinDomain

import iTasks
import GinSyntax

::GinEditor = { gMod           :: GModule
              , checkSyntax    :: Bool
              }

newEditor :: GinEditor

derive gEq GinEditor
derive gVisualize GinEditor
derive gUpdate GinEditor
derive gDefaultMask GinEditor
derive gVerify GinEditor
derive JSONEncode GinEditor
derive JSONDecode GinEditor
