implementation module GinDomain

from StdEnv import id

import iTasks
import GinSyntax, GinFlowLibrary
import GinCompiler

derive gEq GinEditor
derive JSONEncode GinEditor
derive JSONDecode GinEditor

newEditor :: GinEditor
newEditor = { gMod        = newModule
            , checkSyntax = False
            }

gVisualize {|GinEditor|} val vst=:{vizType, label, idPrefix, currentPath, optional, useLabels, updateMask, verifyMask}
    # (cmu,um) = popMask updateMask
    # (cmv,vm) = popMask verifyMask
	# (err,hnt) = verifyElementStr cmu cmv 
    = case vizType of
        VEditorDefinition = ([TUIFragment (TUIAppletControl (appletPanel val idPrefix currentPath err hnt))]
                             , {VSt | vst & currentPath = stepDataPath currentPath, updateMask = um, verifyMask = vm})
where
    appletPanel Nothing                          idp cp errMsg hntMsg = applet newModule                 idp cp errMsg hntMsg 
    appletPanel (Just editor) idp cp errMsg hntMsg = applet (addDefaultLibrary editor.gMod) idp cp errMsg hntMsg

    applet value idp cp errMsg hntMsg = 
        { TUIAppletControl
        | appletcode = "nl/ru/icis/mbsd/itasks/gin/Applet.class"
        , archives = ["/gin.jar"]
        , width = "100%"
        , height = "500"
        , name = dp2s cp
        , id = dp2id idp cp
        , value = gModuleToJSON value
        , errorMsg = errMsg
        , hintMsg = hntMsg
        }
        
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
