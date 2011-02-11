implementation module GinDomain

from StdEnv import id

import iTasks, Text, HtmlUtil
import GinSyntax, GinFlowLibrary
import GinCompiler, GinParser

derive gEq GinEditor
derive JSONEncode GinEditor
derive JSONDecode GinEditor

newEditor :: GinEditor
newEditor = { gMod        = newModule
            , checkSyntax = False
            }

gVisualize {|GinEditor|} val vst = visualizeControl mkControl (mkText,mkHtml) val vst
where
	mkControl name id val _ _ hnt err
		= TUIAppletControl applet
	where
	    applet
	    	# value = case val of
				Nothing		= newModule
				Just {gMod}	= gMod
	        =	{ TUIAppletControl
		        | appletcode = "nl/ru/icis/mbsd/itasks/gin/Applet.class"
		        , archives = ["/gin.jar"]
		        , width = "100%"
		        , height = "500"
		        , name = name
		        , id = id
		        , value = gModuleToJSON value
		        , errorMsg = err
		        , hintMsg = hnt
		        }
		        
	mkText v = toString (fmap (\{gMod} -> tryRender gMod False) v)
	mkHtml v _ = nl2br (mkText v)
     
gUpdate{|GinEditor|} mode ust = basicUpdate mode parseUpdate newEditor ust
where
	parseUpdate update orig
		= case gModuleFromJSON update of	
			Just mod = { GinEditor | orig & gMod = mod }
			Nothing = orig

gDefaultMask{|GinEditor|} _ = [Touched []]

gVerify{|GinEditor|} val vst = customWorldVerify Nothing check val vst where
  check editor iworld
  | not editor.checkSyntax = (WPRValid Nothing, iworld)
  #(compileresult, iworld) = syntaxCheck editor.gMod iworld
  # hint = case compileresult of
       CompileSuccess _ = Nothing
       CompileGlobalError error = Just (toString (toJSON [("/", error)]))
       CompilePathError errors = Just (toString (toJSON errors))
  = (WPRValid hint, iworld )
  
tryRender :: GModule Bool -> String
tryRender gMod expand = 
    case runParse (gToAModule gMod) of
        GSuccess aMod -> renderAModule [] ((if expand expandModule id) aMod) 
        GError errors -> "Parse error:\n" +++ ((join "\n" (map (\(path,msg) = toString path +++ ":" +++ msg) errors)))
