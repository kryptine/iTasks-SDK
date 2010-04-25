definition module AppState

import iTasks, CommonDomain
from Configuration import :: IDEConfig

:: AppState =	{ srcEditorContent	:: !FormattedText
				, config			:: !IDEConfig
				}
				
derive gPrint			AppState
derive gParse			AppState
derive gVisualize		AppState
derive gUpdate			AppState
derive gMerge			AppState
derive gMakeSharedCopy	AppState
derive gMakeLocalCopy	AppState

initAppState :: !IDEConfig -> AppState