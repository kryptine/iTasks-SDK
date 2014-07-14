module CodeMirror

import iTasks, iTasks.API.Core.Client.Tasklet
import iTasks.API.Core.Client.Interface
import iTasks.API.Extensions.CodeMirror

from StdArray import class Array(uselect), instance Array {} a

//-------------------------------------------------------------------------

taskletExamples :: [Workflow]
taskletExamples =
	[workflow "Editor" "Editor" editor]

defcm = { configuration = [CMMode "haskell", CMLineNumbers True]
        , position = 0
        , selection = Nothing
        , highlighted = [(1,3)]
        , source = 
			"definition module iTasks\n"+++
			"\n"+++
			"/**\n"+++
			"* Main iTask module exporting all end user iTask modules\n"+++
			"*/\n"+++
			"import	iTasks.Framework.Engine				// iTasks engine\n"+++
			"    // iTasks API\n"+++
			"    ,   iTasks.API\n"+++
			"	\n"+++
			"	//	Miscellaneous machinery\n"+++
			"	,	Text.JSON							// JSON is used for serializing/deserializing strings\n"+++
			"	,	iTasks.Framework.Generic			// Generic foundation modules\n"+++
			"	\n"+++
			"	//	API extensions for user  & workflow management\n"+++
			"	,	iTasks.API.Extensions.Admin.UserAdmin\n"+++
			"	,	iTasks.API.Extensions.Admin.WorkflowAdmin\n"+++
			"	\n"+++
			"	//StdEnv modules\n"+++
			"	,	StdInt\n"+++
			"	,	StdBool\n"+++
			"	,	StdString\n"+++
			"	,	StdList\n"+++
			"	,	StdOrdList\n"+++
			"	,	StdTuple\n"+++
			"	,	StdEnum\n"+++
			"	,	StdOverloaded\n"+++
			"\n"+++
			"from StdFunc import id, const, o\n"+++
			"from Data.List import instance Functor []\n"}

editor :: Task CodeMirror
editor
	= mkTask (codeMirrorTasklet defcm)
							 
ifValue pred (Value v _) | pred v
	= Just (return v)
	= Nothing


ifStable (Value v True) = Just (return v)
ifStable _				= Nothing

returnC :: b (TaskValue a) -> Task b | iTask b
returnC v _ = return v

returnV :: (TaskValue a) -> Task a | iTask a
returnV (Value v _) = return v
    
Start :: *World -> *World
Start world = startEngine editor world 
