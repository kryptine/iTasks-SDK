definition module AppState

import iTasks, ExperimentalDomain
from Configuration import :: IDEConfig

:: AppState =	{ srcEditorContent	:: !String
				, ideConfig			:: !IDEConfig
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
derive gHint			AppState, SyntaxHighlighterColors
derive gError			AppState, SyntaxHighlighterColors
derive gMerge			AppState

initAppState :: !IDEConfig -> AppState