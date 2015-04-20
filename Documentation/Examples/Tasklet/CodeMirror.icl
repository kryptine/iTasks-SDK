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
        , position = (0,0)
        , selection = Nothing
        , highlighted = [((0,1),(0,3))]
        , source = [ 
			"definition module iTasks", 
			"", 
			"/**", 
			"* Main iTask module exporting all end user iTask modules", 
			"*/", 
			"import	iTasks.Framework.Engine				// iTasks engine", 
			"    // iTasks API", 
			"    ,   iTasks.API", 
			"	", 
			"	//	Miscellaneous machinery", 
			"	,	Text.JSON							// JSON is used for serializing/deserializing strings", 
			"	,	iTasks.Framework.Generic			// Generic foundation modules", 
			"	", 
			"	//	API extensions for user  & workflow management", 
			"	,	iTasks.API.Extensions.Admin.UserAdmin", 
			"	,	iTasks.API.Extensions.Admin.WorkflowAdmin", 
			"	", 
			"	//StdEnv modules", 
			"	,	StdInt", 
			"	,	StdBool", 
			"	,	StdString", 
			"	,	StdList", 
			"	,	StdOrdList", 
			"	,	StdTuple", 
			"	,	StdEnum", 
			"	,	StdOverloaded", 
			"", 
			"from StdFunc import id, const, o", 
			"from Data.List import instance Functor []"] }

//editor :: Task CodeMirror
//editor
//	= mkTask (codeMirrorTasklet defcm) <<@ AfterLayout (tweakUI (setSize (ExactSize 300) (ExactSize 300)))
		    
		    
editor :: Task [String]
editor
	= (updateInformation "" [UpdateWith (\source -> codeMirrorEditlet {defcm & source = source} [])
									   (\_ editlet -> editlet.currVal.source)] defcm.source) <<@ AfterLayout (tweakUI (setSize (ExactSize 300) (ExactSize 300)))		    
		                                                  				 							 
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

import Text.JSON

//Start :: Maybe CodeMirrorDiff
//Start = fromJSON (fromString "[[\"SetHighlights\",[[[0,1],[0,3]]]]]")

//Start :: 	
//Start = fromJSON (fromString "[[\"SetSelection\",[[[0,0],[30,0]]]],[\"SetHighlights\",[[[0,1],[0,3]]]]]")

//Start :: Maybe [CodeMirrorDiff]
//Start = fromJSON (fromString "[[[\"SetHighlights\",[[[0,1],[0,3]]]]]]")

//buu = [SetSelection (Just ((0,0),(30,0)))]
//Start :: JSONNode
//Start = toString (toJSON buu)
