module WasmTest

import StdEnv
import iTasks
import iTasks.UI.JavaScript

// This is a simple test program to try out things with the WebAssembly ABC interpreter.

Start w = doTasks task w
where
	task = updateInformation
		[UpdateUsing id (flip const) editor]
		() <<@ Title "WebAssembly test program"

editor :: Editor ()
editor = leafEditorToEditor
	{ LeafEditor
    | genUI          = withClientSideInit initUI genUI
    , onEdit         = onEdit
    , onRefresh      = onRefresh
    , valueFromState = valueFromState
    }
where
	genUI attributes datapath mode vst = (Ok (uia UITextView (valueAttr (JSONString "Check the browser console.")), ()), vst)
	onEdit datapath (_,()) st vst = (Ok (NoChange,st), vst)
	onRefresh datapath st _ vst = (Ok (NoChange,st), vst)
	valueFromState s = Just s

	initUI me world
		= jsTrace "Hello world from WebAssembly!" world
