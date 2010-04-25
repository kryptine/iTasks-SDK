implementation module AppState

import CommonDomain
from Configuration import :: IDEConfig

derive gPrint			AppState
derive gParse			AppState
derive gVisualize		AppState
derive gUpdate			AppState
derive gMerge			AppState
derive gMakeSharedCopy	AppState
derive gMakeLocalCopy	AppState
derive bimap			Maybe, (,)

initAppState :: !IDEConfig -> AppState
initAppState config =	{ srcEditorContent	= mkEmptyFormattedText noControls
						, config			= config
						}