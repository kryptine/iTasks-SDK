definition module Configuration

import iTasks, AppState

:: IDEConfig =	{ oldIDEPath	:: !Path
				, projectsPath	:: !Path
				}
				
derive gPrint			IDEConfig
derive gParse			IDEConfig
derive gVisualize		IDEConfig
derive gUpdate			IDEConfig
derive gHint			IDEConfig
derive gError			IDEConfig
derive gMerge			IDEConfig
derive gMakeSharedCopy	IDEConfig
derive gMakeLocalCopy	IDEConfig
				
loadConfig :: Task (Maybe IDEConfig)
getConfig :: !(DBid AppState) -> Task IDEConfig