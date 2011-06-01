implementation module GinDomain

from StdEnv import id

import iTasks, Text, HtmlUtil
import GinSyntax, GinFlowLibrary
import GinCompiler, GinParser

import GinORYX

gVisualize {|ORYXEditor|} val vst = visualizeControl (TUIORYXControl oryx.ORYXEditor.stencilset.ORYXStencilSetReference.url) (mkText,mkHtml) val vst
where
	oryx = fromMaybe emptyORYXEditor val
		        
	mkText v = "(ORYX editor: No textual representation available)"
	mkHtml v = nl2br (mkText v)
	
instance toString ORYXEditor
where
	toString {diagram} = toString (toJSON diagram)

gUpdate{|ORYXEditor|} mode ust = basicUpdate mode parseUpdate emptyORYXEditor ust
where
	parseUpdate diagram orig = { ORYXEditor | orig & diagram = diagram }

gDefaultMask{|ORYXEditor|} _ = [Touched []]
gVerify{|ORYXEditor|} _ vst = alwaysValid vst
derive JSONEncode ORYXEditor
derive JSONDecode ORYXEditor

gEq{|ORYXEditor|} _ _ = False // ORYXEditors are never equal
