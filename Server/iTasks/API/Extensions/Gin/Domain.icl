implementation module iTasks.API.Extensions.Gin.Domain

from StdEnv import id, undef

import Text, Text.JSON, Data.Maybe, Data.Map
import iTasks.API.Core.LayoutCombinators
import iTasks.Framework.HtmlUtil
import iTasks.Framework.GenUpdate
import iTasks.Framework.GenVisualize
import iTasks.Gin.Syntax
import iTasks.Gin.FlowLibrary
import iTasks.Gin.Compiler
import iTasks.Gin.Parser
import iTasks.Gin.ORYX

// visualizeAsEditor :: !a !VerifyMask !TaskId !Layout !*IWorld -> (![(!UIControl,!UIAttributes)],!*IWorld) | gVisualizeEditor{|*|} a
// generic gVisualizeEditor a | gVisualizeText a, gHeaders a, gGridRows a :: !(Maybe a) !*VSt -> (!VisualizationResult,!*VSt)
gVisualizeText{|ORYXEditor|} _ _ = ["(ORYX editor: No textual representation available)"]

// TODO: Custom
//derive gVisualizeEditor ORYXEditor
gVisualizeEditor{|ORYXEditor|} val vst=:{VSt | taskId, iworld}
  //# (uias, iworld) = visualizeAsEditor (UIEditOryx sizeOpts editOpts oryxOpts) (VMUntouched Nothing True []) taskId autoLayout iworld
  = (NormalEditor [(UIEditOryx sizeOpts editOpts oryxOpts, newMap)], {VSt | vst & iworld = iworld})
  where  sizeOpts = { UISizeOpts | width = Just FlexSize, minWidth = Just (ExactMin 400), height = Just FlexSize, minHeight = Just (ExactMin 400), margins = Nothing }
         editOpts = { UIEditOpts | taskId = toString taskId, editorId = "ORYX Editor", value = Nothing }//TODO
         oryxOpts = { UIOryxOpts | stencilsetUrl = ""} // oryx.ORYXEditor.stencilset.ORYXStencilSetReference.url}

//gVisualizeEditor{|ORYXEditor|} val vst=:{VSt | taskId, iworld}
  //# (uis, iw) = visualizeAsEditor val (VMUntouched Nothing True []) taskId autoLayout iworld
 //= (NormalEditor uis, {VSt | vst & iworld = iw})
   //TODO visualizeControlSimple (UIEditOryx {UIOryxOpts | oryx.ORYXEditor.stencilset.ORYXStencilSetReference.url} val vst TODO
//where
	//oryx = fromMaybe emptyORYXEditor val
	
instance toString ORYXEditor
where
	toString {diagram} = toString (toJSON diagram)

//generic gUpdate a | gDefault a :: ![Int] !JSONNode !(!a,![InteractionMask]) -> (!a, ![InteractionMa
gUpdate{|ORYXEditor|} target upd ois = basicUpdate parseUpdate target upd ois
//  basicUpdate mode parseUpdate emptyORYXEditor ust // TODO
where
	parseUpdate diagram orig = { ORYXEditor | orig & diagram = diagram }

gVerify{|ORYXEditor|} _ ims _ = alwaysValid ims
derive JSONEncode ORYXEditor
derive JSONDecode ORYXEditor
derive gEq ORYXEditor
derive gHeaders ORYXEditor
derive gGridRows ORYXEditor
derive gDefault ORYXEditor
