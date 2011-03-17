implementation module GinDomain

from StdEnv import id

import iTasks, Text, HtmlUtil
import GinSyntax, GinFlowLibrary
import GinCompiler, GinParser

import GinORYX

gVisualize {|ORYXEditor|} val vst = visualizeControl mkControl (mkText,mkHtml) val vst
where
	mkControl name id val _ _ err hnt
		= TUIORYXControl oryx
	where
	    oryx
	    	# value = case val of
				Nothing			= emptyORYXEditor
				Just editor		= editor
	        =	{ TUIORYXControl
		        | name = name
		        , id = id
		        , value = toString (toJSON value.ORYXEditor.diagram)
		        , errorMsg = err
		        , hintMsg = hnt
		        , stencilsetURL = value.ORYXEditor.stencilset.ORYXStencilSetReference.url
		        }
		        
	mkText v = "(ORYX editor: No textual representation available)"
	mkHtml v _ = nl2br (mkText v)

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
						++ encodeFunc verify
						]]

JSONDecode{|ORYXEditor|} [JSONArray [JSONString "ORYXEditor",diagram,stencilset,verify]:c]
	# mbDiagram		= fromJSON diagram
	# mbStencilset	= fromJSON stencilset
	# mbVerify		= decodeFunc verify
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
