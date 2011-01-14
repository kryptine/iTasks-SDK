implementation module GinDomain

from StdEnv import id

import iTasks
import GinSyntax, GinFlowLibrary
import GinCompiler

derive JSONEncode GinEditor
derive JSONDecode GinEditor

gVisualize {|GinEditor|} val vst=:{vizType, label, idPrefix, currentPath, optional, useLabels, updateMask, verifyMask}
    # (cmu,um) = popMask updateMask
    # (cmv,vm) = popMask verifyMask
	# (err,hnt) = verifyElementStr cmu cmv 
    = case vizType of
        VEditorDefinition = ([TUIFragment (TUIAppletControl (appletPanel val idPrefix currentPath err hnt))]
                             , {VSt | vst & currentPath = stepDataPath currentPath, updateMask = um, verifyMask = vm})
where
    appletPanel Nothing                          idp cp errMsg hntMsg = applet newModule                 idp cp errMsg hntMsg 
    appletPanel (Just (GinEditor value)) idp cp errMsg hntMsg = applet (addDefaultLibrary value) idp cp errMsg hntMsg

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

gUpdate {|GinEditor|} _ ust =: {USt | mode=UDCreate,newMask} = ((GinEditor newModule),{USt | ust & newMask = appendToMask newMask Untouched})

gUpdate {|GinEditor|} s ust =: {USt | mode=UDSearch, searchPath, currentPath, update,oldMask,newMask}
    # (cm,om) = popMask oldMask
    | currentPath == searchPath
        = (parseUpdate s update, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched True []), oldMask = om})
    | otherwise
        = (s, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (cleanUpdMask cm), oldMask = om})
where
    parseUpdate :: GinEditor String -> GinEditor
    parseUpdate orig update = case gModuleFromJSON update of
					Just mod = GinEditor mod
					Nothing = orig

gUpdate {|GinEditor|} s ust =: {USt | mode = UDMask, currentPath, newMask}
    = (s, {USt | ust & currentPath = stepDataPath currentPath, newMask = appendToMask newMask (Touched True [])})

gVerify{|GinEditor|} val vst = worldVerify check val vst where
  check Nothing iworld = (Nothing, Nothing, iworld)
  check (Just (GinEditor gMod)) iworld =:{ world }
  #(compileresult, world) = syntaxCheck gMod world
  # hint = case compileresult of
       CompileSuccess _ = Nothing
       CompileGlobalError error = Just (toString (toJSON [("/", error)]))
       CompilePathError errors = Just (toString (toJSON errors))
  = (hint, Nothing, { IWorld | iworld & world = world } )

