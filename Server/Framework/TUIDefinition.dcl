definition module TUIDefinition
/**
* This module provides a data representation of 'T'ask 'U'ser 'I'nterface
* component definitions and a specialized instance of
* JSONEncode for serializing them to JSON
*/
import JSON, GenEq
from SystemTypes	import	:: Document, :: DocumentId, :: Hotkey, :: TaskId, :: InteractionTaskType, :: Action, :: TaskMeta
from Task			import	:: TaskAction


:: TUIInteraction =	{ title				:: !String
					, instruction		:: !Maybe String
					, content			:: ![TUIDef]
					, actions			:: ![TaskAction]
					, type				:: !Maybe InteractionTaskType
					, localInteraction	:: !Bool
					, warning			:: !Maybe String
					}

:: TUIStep =		{ title				:: !String
					, instruction		:: !Maybe String
					, content			:: !TUIDef
					, actions			:: ![TaskAction]
					, steps				:: ![TaskAction]
					}
/**
* To layout a set of parallel tasks the following information is available:
* - Title : The title of the parallel combination
* - Instruction: The instruction of the combination
* For each parallel item:
* - Task index     : The index in the parallel set
* - Task order     : An integer indicating a relative ordering in the items of the set.
*                    May be used to determine z-index in windows or active item in tabs.
* - Task meta      : Meta data of the task (title, type etc..)
* - TUI definition : Specification of the task's user interface
* - Task actions   : The possible actions to complete tasks in the parallel items
*/
:: TUIParallel =	{ taskId			:: !TaskId
					, title				:: !String
					, instruction		:: !Maybe String
					, items				:: ![(!Int,!Int,!TaskMeta,!Maybe TUIDef,![TaskAction])]
					}


:: TUIName		:== String

:: TUIDef =	{ content	:: !TUIDefContent
			, width		:: !Maybe TUISize
			, height	:: !Maybe TUISize
			, margins	:: !Maybe TUIMargins
			}

:: TUIDefContent
	= TUIEditControl		!TUIControlType !TUIEditControl
	| TUIShowControl		!TUIControlType !TUIShowControl
	| TUIContainer			!TUIContainer
	| TUIPanel				!TUIPanel
	| TUIWindow				!TUIWindow
	| TUITabContainer		!TUITabContainer
	| TUITabItem			!TUITabItem
	| TUIBorderContainer	!TUIBorderContainer
	| TUIBorderItem			!TUIBorderItem
	| TUIListContainer		!TUIListContainer
	| TUIListItem			!TUIListItem
	| TUIIcon				!TUIIcon
	| TUIRadioChoice		!TUIRadioChoice
	| TUICheckChoice		!TUICheckChoice
	| TUIButton				!TUIButton
	| TUIMenuButton			!TUIMenuButton
	| TUIMenuItem			!TUIMenuItem
	| TUIHtml				!TUIHtml
	| TUICustom				!JSONNode
	
:: TUIControlType	= TUIStringControl
					| TUICharControl
					| TUIRealControl
					| TUIIntControl
					| TUIBoolControl
					| TUINoteControl
					| TUIDateControl
					| TUITimeControl
					| TUIPasswordControl
					| TUIUserControl
					| TUICurrencyControl
					| TUIDocumentControl	!Document
					| TUIButtonControl		!TUIButtonControl
					| TUIComboControl		![String]
					| TUIGridControl		!TUIGridControl
					| TUITreeControl		![TUITree]
					| TUIORYXControl		!String // stencilset URL
					| TUICustomControl		!String // xtype

:: TUIEditControl =
	{ name			:: !TUIName
	, value			:: !JSONNode
	, taskId		:: !TaskId
	, eventValue	:: !Maybe JSONNode
	}
:: TUIShowControl =
	{ value			:: !JSONNode
	}
:: TUIButtonControl =
	{ label			:: !String
	, iconCls		:: !String
	}
:: TUITree =
	{ text		:: !String
	, children	:: !Maybe [TUITree]
	, leaf		:: !Bool
	, value		:: !Int
	}
:: TUIContainer =
	{ items				:: ![TUIDef]
	, direction			:: !TUIDirection
	, halign			:: !TUIHAlign
	, valign			:: !TUIVAlign
	, padding			:: !Maybe Int
	, purpose			:: !Maybe String
	, baseCls			:: !Maybe String
	}
:: TUIPanel =
	{ items				:: ![TUIDef]
	, direction			:: !TUIDirection
	, halign			:: !TUIHAlign
	, valign			:: !TUIVAlign
	, padding			:: !Maybe Int
	, purpose			:: !Maybe String
	, title				:: !PanelTitle
	, frame				:: !Bool
	, menus				:: ![TUIMenuButton]
	, iconCls			:: !Maybe PanelIcon
	, baseCls			:: !Maybe String
	}
:: TUIWindow =
	{ items				:: ![TUIDef]
	, direction			:: !TUIDirection
	, halign			:: !TUIHAlign
	, valign			:: !TUIVAlign
	, padding			:: !Maybe Int
	, purpose			:: !Maybe String
	, baseCls			:: !Maybe String
	}		
:: TUITabContainer =
	{ taskId			:: !Maybe TaskId
	, active			:: !Int
	, items				:: ![TUITabItem]
	}
:: TUITabItem =
	{ index				:: !Int
	, title				:: !PanelTitle
	, iconCls			:: !Maybe String
	, items				:: !Maybe TUIDef
	, menus				:: ![TUIMenuButton]
	, closeAction		:: !Maybe (!TUIName,!TaskId)
	}
:: TUIBorderContainer =
	{ direction			:: !TUIDirection
	, itemA				:: !TUIBorderItem
	, itemB				:: !TUIBorderItem
	, initSplit			:: !TUIFixedSize
	, collapsible		:: !Bool
	}
:: TUIBorderItem = 
	{ title				:: !Maybe PanelTitle
	, iconCls			:: !Maybe String
	, item				:: !TUIDef
	}
:: TUIListContainer =
	{ items			:: ![TUIListItem]
	, taskId		:: !Maybe TaskId
	, name			:: !Maybe TUIName
	}
:: TUIListItem =
	{ items			:: !TUIDef
	, index			:: !Int
	}
:: TUIIcon =
	{ type			:: !String
	, tooltip		:: !Maybe String
	}
:: TUIRadioChoice =
	{ items			:: ![TUIDef]
	, taskId		:: !Maybe TaskId
	, name			:: !TUIName
	, index			:: !Int
	, checked		:: !Bool
	}
:: TUICheckChoice =
	{ items			:: ![TUIDef]
	, taskId		:: !Maybe TaskId
	, name			:: !TUIName
	, index			:: !Int
	, checked		:: !Bool
	}
:: TUIButton =
	{ taskId		:: !TaskId
	, name			:: !TUIName
	, text			:: !String
	, disabled		:: !Bool
	, iconCls		:: !String
	, actionButton	:: !Bool
	}
:: TUIMenuButton =
	{ text			:: !String
	, target		:: !Maybe String
	, action		:: !Maybe String
	, disabled		:: !Bool
	, iconCls		:: !Maybe String
	, menu			:: !Maybe TUIMenu
	}
:: TUIMenu =
	{ items			:: ![TUIMenuItem]
	}
:: TUIMenuItem =
	{ text			:: !String
	, target		:: !Maybe String
	, action		:: !Maybe String
	, disabled		:: !Bool
	, iconCls		:: !Maybe String
	, hotkey		:: !Maybe Hotkey
	, menu			:: !Maybe TUIMenu
	}
:: TUIHtml =
	{ html			:: !String
	}
	
:: TUIGridControl =
	{ headers		:: ![String]
	, cells			:: ![[String]]
	}
	
:: PanelTitle	:== String
:: PanelIcon	:== String
	
:: TUISize			= WrapContent !TUIFixedSize				// The tui element's size becomes the minimal size of its content, but can't become smaller than the given minimal size
					| FillParent !TUIWeight !TUIMinSize		// The tui element fills the entire parent container
															// If there is more than one 'FillParent' element in one container the available space is distributed according to the weights (my size = my weight/sum of weights * available space)
															// If the space becomes smaller than the minimal size, the element behaves as if its minimal size was its fixed size
					| Fixed !TUIFixedSize					// The tui element has a fixed size
				
:: TUIMargins =	{ top		:: !TUIFixedSize
				, right		:: !TUIFixedSize
				, bottom	:: !TUIFixedSize
				, left		:: !TUIFixedSize
				}
					
:: TUIFixedSize		:== Int
:: TUIWeight		:== Int
:: TUIMinSize		= ContentSize							// The container's minimal size is the minimal size of its content
					| FixedMinSize !TUIFixedSize			// The container has a fixed minimal size
:: TUIHAlign		= AlignLeft | AlignCenter | AlignRight
:: TUIVAlign		= AlignTop | AlignMiddle | AlignBottom
:: TUIDirection		= Horizontal | Vertical

:: Tooltip :== String

stringDisplay			:: !String -> TUIDef
defaultLayoutContainer	:: ![TUIDef] -> TUIContainer
defaultLayoutPanel		:: ![TUIDef] -> TUIPanel
sameMargins				:: !TUIFixedSize -> Maybe TUIMargins
leftMargin				:: !TUIFixedSize -> Maybe TUIMargins
topMargin				:: !TUIFixedSize -> Maybe TUIMargins

defaultDef				:: !TUIDefContent -> TUIDef
fillDef					:: !TUIDefContent -> TUIDef

// Modifiers
setSize		:: !TUISize !TUISize !TUIDef -> TUIDef
setWidth	:: !TUISize !TUIDef -> TUIDef
setHeight	:: !TUISize !TUIDef -> TUIDef
fill		:: !TUIDef -> TUIDef
fillHeight	:: !TUIDef -> TUIDef
fillWidth	:: !TUIDef -> TUIDef
fixedHeight	:: !Int !TUIDef -> TUIDef
fixedWidth	:: !Int !TUIDef -> TUIDef

hjoin :: ![TUIDef] -> TUIDef
vjoin :: ![TUIDef] -> TUIDef

vsplit :: !Int ![TUIDef] ![TUIDef] -> TUIDef
hsplit :: !Int ![TUIDef] ![TUIDef] -> TUIDef 

// Layouts
:: InteractionLayouter	:== TUIInteraction			-> (TUIDef, [TaskAction]) 
:: StepLayouter			:==	TUIStep					-> (TUIDef, [TaskAction])
:: ParallelLayouter		:== TUIParallel				-> (TUIDef, [TaskAction])

:: LayoutTweak			:== (TUIDef, [TaskAction])	-> (TUIDef, [TaskAction])
:: TUITweak				:== TUIDef					-> TUIDef
:: ActionTweak			:== [TaskAction]			-> [TaskAction]

// Pre-defined interaction layouts
defaultInteractionLayout	:: InteractionLayouter //Interaction parts, action buttons, title and instructions
plainInteractionLayout		:: InteractionLayouter //Just the interaction parts and action buttons 
minimalInteractionLayout	:: InteractionLayouter //Only the interaction parts
fullWidthInteractionLayout	:: InteractionLayouter
wrapWidthInteractionLayout	:: InteractionLayouter
maximalInteractionLayout	:: InteractionLayouter
fillInteractionLayout		:: InteractionLayouter

singleViewLayout			:: TUISize !TUISize -> InteractionLayouter

// Pre-defined step layouts
defaultStepLayout			:: StepLayouter

// Pre-defined parallel layouts
defaultParallelLayout		:: ParallelLayouter
horizontalParallelLayout	:: ParallelLayouter
tabLayout					:: ParallelLayouter

vsplitLayout				:: Int ([TUIDef] -> ([TUIDef],[TUIDef])) -> ParallelLayouter
hsplitLayout				:: Int ([TUIDef] -> ([TUIDef],[TUIDef])) -> ParallelLayouter
fuseParallelLayout			:: ParallelLayouter //"Fuses" a set of panels/or containers into one container

// layout aux functions
defaultPanelDescr			:: !PanelTitle !PanelIcon !(Maybe String) !(Maybe String) 	!TUISize ![TUIDef]	-> TUIDef
defaultPanel				:: !PanelTitle !PanelIcon							!TUISize ![TUIDef]	-> TUIDef
defaultDescriptionPanel		:: !(Maybe String) !(Maybe String)										-> Maybe TUIDef
defaultContentPanel			:: ![TUIDef]															-> TUIDef
defaultContent				:: ![TUIDef] ![TUIDef]													-> [TUIDef]
defaultInteractionIcon		:: !(Maybe InteractionTaskType) !Bool								-> PanelIcon
defaultButtons				:: ![TaskAction]														-> (![TUIDef],![TaskAction])
defaultMenus				:: ![TaskAction]														-> (![TUIMenuButton],![TaskAction])

addButtons					:: ![TUIDef] TUIDef														-> TUIDef


columnLayout				:: !Int ![TUIDef] 														-> TUIDef