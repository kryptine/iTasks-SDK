definition module AppState

import iTasks, CommonDomain
from Configuration import :: IDEConfig

:: AppState =	{ srcEditorContent	:: !String
				, config			:: !IDEConfig
				, syntaxHighlColors	:: !SyntaxHighlighterColors
				}
				
:: SyntaxHighlighterColors =	{ keywords				:: !Color
								, typeDefinitions		:: !Color
								, singleLineComments	:: !Color
								, multiLineComments		:: !Color
								, strings				:: !Color
								, characters			:: !Color
								, numbers				:: !Color
								}
				
derive gPrint			AppState, SyntaxHighlighterColors
derive gParse			AppState, SyntaxHighlighterColors
derive gVisualize		AppState, SyntaxHighlighterColors
derive gUpdate			AppState, SyntaxHighlighterColors
derive gMerge			AppState
derive gMakeSharedCopy	AppState
derive gMakeLocalCopy	AppState

initAppState :: !IDEConfig -> AppState