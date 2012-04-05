implementation module TUIDefinition

import JSON_NG, StdList, StdBool, StdTuple, GenEq_NG, StdFunc, HTML, Text, List_NG
from SystemTypes	import :: Document, :: DocumentId, :: Hotkey, :: TaskId, :: Action(..), :: ActionName
from SystemTypes	import actionIcon, actionName
from Task			import  :: TaskAction
import LayoutCombinators

defaultDef :: !TUIDefContent -> TUIDef
defaultDef content =	{TUIDef
						| content = content
						, width = Nothing
						, height = Nothing
						, margins = Nothing}

defaultContainer :: ![TUIDef] -> TUIContainer
defaultContainer items ={TUIContainer
						| items			= items
						, direction		= Vertical
						, halign		= AlignLeft
						, valign		= AlignTop
						, padding		= Nothing
						, purpose		= Nothing
						, baseCls		= Nothing
						}
defaultPanel :: ![TUIDef] -> TUIPanel
defaultPanel items =	{TUIPanel
						| items			= items
						, direction		= Vertical
						, halign		= AlignLeft
						, valign		= AlignTop
						, padding		= Nothing
						, purpose		= Nothing
						, title			= Nothing
						, frame			= False
						, menus			= []
						, iconCls		= Nothing
						, baseCls		= Nothing
						, bodyCls		= Nothing
						, bodyPadding	= Nothing
						}
defaultWindow :: ![TUIDef] -> TUIWindow				
defaultWindow items =	{TUIWindow
						| items			= items
						, direction		= Vertical
						, halign		= AlignLeft
						, valign		= AlignTop
						, padding		= Nothing
						, purpose		= Nothing
						, menus			= []
						, iconCls		= Nothing
						, baseCls		= Nothing
						}

stringDisplay :: !String -> TUIDef
stringDisplay s
	= defaultDef (TUIShowControl TUIStringControl {TUIShowControl|value = JSONString s})
