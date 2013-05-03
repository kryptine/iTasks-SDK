implementation module iTasks.API.Extensions.Gin.Domain

from StdEnv import id, undef

import Text, Text.JSON, Data.Maybe
import iTasks.Framework.HtmlUtil
import iTasks.Gin.Syntax
import iTasks.Gin.FlowLibrary
import iTasks.Gin.Compiler
import iTasks.Gin.Parser
import iTasks.Gin.ORYX

gVisualizeText{|ORYXEditor|} _ _ = ["(ORYX editor: No textual representation available)"]
gVisualizeEditor{|ORYXEditor|} val vst = undef // TODO visualizeControlSimple (UIORYXControl oryx.ORYXEditor.stencilset.ORYXStencilSetReference.url) val vst TODO
where
	oryx = fromMaybe emptyORYXEditor val
	
instance toString ORYXEditor
where
	toString {diagram} = toString (toJSON diagram)

gUpdate{|ORYXEditor|} mode ust _ = undef // TODO basicUpdate mode parseUpdate emptyORYXEditor ust // TODO
where
	parseUpdate diagram orig = { ORYXEditor | orig & diagram = diagram }

gVerify{|ORYXEditor|} _ _ vst = undef // alwaysValid vst // TODO
derive JSONEncode ORYXEditor
derive JSONDecode ORYXEditor
derive gEq ORYXEditor
