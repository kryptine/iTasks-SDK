definition module editletGraphics

import iTasks
import iTasks.API.Core.Client.Editlet

svg_rects :: Task Void
::  MR = MR [ModelRect] Int

derive JSONEncode     MR
derive JSONDecode     MR
derive gEditMeta      MR
derive gVerify        MR
derive gEq            MR
derive gDefault       MR
derive gVisualizeText MR
derive gUpdate        MR
derive gEditor        MR

::  ModelRect = { pos :: !(!Int,!Int), size :: !(!Int,!Int), frame :: !String, framew :: !Int, fill :: !String, opacity :: !Real }
derive class iTask ModelRect
