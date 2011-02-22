implementation module GinDomain

from StdEnv import id

import iTasks, Text, HtmlUtil
import GinSyntax, GinFlowLibrary
import GinCompiler, GinParser

import GinORYX

derive gEq ORYXEditor
derive JSONEncode ORYXEditor
derive JSONDecode ORYXEditor

gVisualize {|ORYXEditor|} val vst = visualizeControl mkControl (mkText,mkHtml) val vst
where
	mkControl name id val _ _ hnt err
		= TUIORYXControl oryx
	where
	    oryx
	    	# value = case val of
				Nothing			= petriNetORYXEditor //TODO: Should not be fixed
				Just editor		= editor
	        =	{ TUIORYXControl
		        | name = name
		        , id = id
		        , value = toString (toJSON value.ORYXEditor.diagram)
		        , errorMsg = err
		        , hintMsg = hnt
		        , stencilsetURL = value.ORYXEditor.stencilset.ORYXStencilSet.url
		        }
		        
	mkText v = toString (fmap (\oryxeditor -> oryxeditor.toString oryxeditor) v)
	mkHtml v _ = nl2br (mkText v)
     
gUpdate{|ORYXEditor|} mode ust = basicUpdate mode parseUpdate petriNetORYXEditor ust //TODO: should not be fixed
where
	parseUpdate update orig
		= case fromJSON (fromString update) of	
			Just diagram = { ORYXEditor | orig & diagram = diagram }
			Nothing = orig

gDefaultMask{|ORYXEditor|} _ = [Touched []]

gVerify{|ORYXEditor|} val vst = customWorldVerify Nothing check val vst where
	check editor iworld = (WPRValid Nothing, iworld) //check nothing
/*
gVerify{|ORYXEditor|} val vst = customWorldVerify Nothing check val vst where
  check editor iworld
  | not editor.checkSyntax = (WPRValid Nothing, iworld)
  #(compileresult, iworld) = syntaxCheck editor.gMod iworld
  # hint = case compileresult of
       CompileSuccess _ = Nothing
       CompileGlobalError error = Just (toString (toJSON [("/", error)]))
       CompilePathError errors = Just (toString (toJSON errors))
  = (WPRValid hint, iworld )
*/

tryRender :: GModule Bool -> String
tryRender gMod expand = 
    case runParse (gToAModule gMod) of
        GSuccess aMod -> renderAModule [] ((if expand expandModule id) aMod) 
        GError errors -> "Parse error:\n" +++ ((join "\n" (map (\(path,msg) = toString path +++ ":" +++ msg) errors)))
