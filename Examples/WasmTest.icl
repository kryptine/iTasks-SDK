module WasmTest

import StdEnv
import iTasks
import ABC.Interpreter.JavaScript

// This is a simple test program to try out things with the WebAssembly ABC interpreter.

Start w = doTasks task w
where
	task = updateInformation
		[UpdateUsing id (flip const) editor]
		() <<@ Title "WebAssembly test program"

editor :: Editor () (Maybe ())
editor = leafEditorToEditor
	{ LeafEditor
    | onReset        = withClientSideInit initUI onReset
    , onEdit         = onEdit
    , onRefresh      = onRefresh
    , valueFromState = valueFromState
    }
where
	onReset attributes datapath mode vst = (Ok (uia UITextView (valueAttr (JSONString "Check the browser console.")), (), Nothing), vst)
	onEdit datapath (_,()) st vst = (Ok (NoChange,st,Nothing), vst)
	onRefresh datapath st _ vst = (Ok (NoChange,st,Nothing), vst)
	valueFromState s = Just s

	initUI me world
		= jsTrace "Hello world from WebAssembly!" world
