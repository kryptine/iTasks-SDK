implementation module AppState

import ExperimentalDomain
from Configuration import :: IDEConfig

derive gPrint			AppState, SyntaxHighlighterColors
derive gParse			AppState, SyntaxHighlighterColors
derive gVisualize		AppState, SyntaxHighlighterColors
derive gUpdate			AppState, SyntaxHighlighterColors
derive gMerge			AppState, SyntaxHighlighterColors
derive gMakeSharedCopy	AppState, SyntaxHighlighterColors
derive gMakeLocalCopy	AppState, SyntaxHighlighterColors
derive bimap			Maybe, (,)

initAppState :: !IDEConfig -> AppState
initAppState config =	{ srcEditorContent	= ""
						, config			= config
						, syntaxHighlColors =	{ keywords				= colorPurple
												, typeDefinitions		= colorRed
												, singleLineComments	= colorAqua
												, multiLineComments		= colorBlue
												, strings				= colorGreen
												, characters			= colorFuchsia
												, numbers				= colorOrange
												}
						}