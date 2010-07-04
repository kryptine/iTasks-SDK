implementation module AppState

import ExperimentalDomain
from Configuration import :: IDEConfig

derive gPrint			AppState, SyntaxHighlighterColors
derive gParse			AppState, SyntaxHighlighterColors
derive gVisualize		AppState, SyntaxHighlighterColors
derive gUpdate			AppState, SyntaxHighlighterColors
derive gHint			AppState, SyntaxHighlighterColors
derive gError			AppState, SyntaxHighlighterColors
derive gMerge			AppState, SyntaxHighlighterColors
derive bimap			Maybe, (,)

initAppState :: !IDEConfig -> AppState
initAppState config =	{ srcEditorContent	= ""
						, ideConfig			= config
						, syntaxHighlColors =	{ keywords				= colorPurple
												, typeDefinitions		= colorRed
												, singleLineComments	= colorAqua
												, multiLineComments		= colorBlue
												, strings				= colorGreen
												, characters			= colorFuchsia
												, numbers				= colorOrange
												}
						}