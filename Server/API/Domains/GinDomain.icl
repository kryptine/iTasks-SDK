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
	parseUpdate update orig
		= case fromJSON (fromString update) of	
			Just diagram = { ORYXEditor | orig & diagram = diagram }
			Nothing = orig

gDefaultMask{|ORYXEditor|} _ = [Touched []]

gVerify{|ORYXEditor|} Nothing vst = alwaysValid vst
gVerify{|ORYXEditor|} val=:(Just {verify}) vst = customWorldVerify Nothing verify val vst

JSONEncode {|ORYXEditor|} { diagram, stencilset, verify }
	= [ JSONArray		[  JSONString "ORYXEditor"
						:  JSONEncode{|*|} diagram
						++ JSONEncode{|*|} stencilset
						++ dynamicJSONEncode verify
						]]

JSONDecode{|ORYXEditor|} [JSONArray [JSONString "ORYXEditor",diagram,stencilset,verify]:c]
	# mbDiagram		= fromJSON diagram
	# mbStencilset	= fromJSON stencilset
	# mbVerify		= dynamicJSONDecode verify
	|  isJust mbDiagram
	&& isJust mbStencilset
	&& isJust mbVerify
		= (Just	{ ORYXEditor
				| diagram		= fromJust mbDiagram
				, stencilset	= fromJust mbStencilset
				, verify		= fromJust mbVerify
				},c)
	| otherwise
		= (Nothing,c)
JSONDecode{|ORYXEditor|} c = (Nothing,c)

gEq{|ORYXEditor|} _ _ = False // ORYXEditors are never equal
