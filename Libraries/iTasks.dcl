definition module iTasks

/**
* Main iTask module exporting all end user iTask modules
*/
import
    // iTasks engine
        iTasks.Engine
    // iTasks API
    ,   iTasks.SDS.Definition
    ,   iTasks.SDS.Sources.Core
    ,   iTasks.SDS.Sources.Store
    ,   iTasks.SDS.Sources.System
    ,   iTasks.SDS.Combinators.Core
    ,   iTasks.SDS.Combinators.Common

    ,   iTasks.WF.Definition
    ,   iTasks.WF.Derives
    ,   iTasks.WF.Tasks.Core
    ,   iTasks.WF.Tasks.SDS
    ,   iTasks.WF.Tasks.IO
    ,   iTasks.WF.Tasks.System
    ,   iTasks.WF.Tasks.Interaction
    ,   iTasks.WF.Combinators.Core
    ,   iTasks.WF.Combinators.SDS
    ,   iTasks.WF.Combinators.Tune
    ,   iTasks.WF.Combinators.Overloaded
    ,   iTasks.WF.Combinators.Common
	// Distributed iTasks
	,   iTasks.Internal.Distributed.Domain

	//  Custom task GUI's
    ,   iTasks.UI.Editor.Controls
    ,   iTasks.UI.Editor.Containers
    ,   iTasks.UI.Editor.Modifiers

	//	Miscellaneous machinery
	,	Text.GenJSON							// JSON is used for serializing/deserializing strings
	,   iTasks.UI.Prompt 					// Standard for creating prompts
	,   iTasks.UI.Layout.Common 			// Standard layout patterns

	//	API extensions for user  & workflow management
	,	iTasks.Extensions.Admin.UserAdmin
	,	iTasks.Extensions.Admin.WorkflowAdmin

	//StdEnv modules
	,	StdInt
	,	StdBool
	,	StdString
	,	StdList
	,	StdOrdList
	,	StdTuple
	,	StdEnum
	,	StdOverloaded

//JSON(En|De)code for Dynamic and (->)
from iTasks.Internal.Serialization import generic JSONEncode, generic JSONDecode

from iTasks.Internal.SDSService import sdsServiceTask

from iTasks.Internal.SDS import instance Identifiable (SDSSource p r w), instance Readable (SDSSource p r w) p r,instance Writeable (SDSSource p r w) p w,instance Modifiable (SDSSource p r w) p r w,instance Registrable (SDSSource p r w) p r,instance Identifiable (SDSLens p r w),instance Readable (SDSLens p r w) p r,instance Writeable (SDSLens p r w) p w,instance Modifiable (SDSLens p r w) p r w,instance Registrable (SDSLens p r w) p r,instance Identifiable (SDSCache p r w),instance Readable (SDSCache p r w) p r,instance Writeable (SDSCache p r w) p w,instance Modifiable (SDSCache p r w) p r w,instance Registrable (SDSCache p r w) p r,instance Identifiable (SDSSequence p r w),instance Readable (SDSSequence p r w) p r,instance Writeable (SDSSequence p r w) p w,instance Modifiable (SDSSequence p r w) p r w,instance Registrable (SDSSequence p r w) p r,instance Identifiable (SDSSelect p r w),instance Readable (SDSSelect p r w) p r,instance Writeable (SDSSelect p r w) p w,instance Modifiable (SDSSelect p r w) p r w,instance Registrable (SDSSelect p r w) p r,instance Identifiable (SDSParallel p r w),instance Readable (SDSParallel p r w) p r,instance Writeable (SDSParallel p r w) p w,instance Modifiable (SDSParallel p r w) p r w,instance Registrable (SDSParallel p r w) p r,instance Identifiable (SDSRemoteService p r w),instance Readable (SDSRemoteService p r w) p r,instance Writeable (SDSRemoteService p r w) p w,instance Modifiable (SDSRemoteService p r w) p r w,instance Registrable (SDSRemoteService p r w) p r,instance Identifiable (SDSRemoteSource p r w),instance Readable (SDSRemoteSource p r w) p r,instance Writeable (SDSRemoteSource p r w) p w,instance Modifiable (SDSRemoteSource p r w) p r w,instance Registrable (SDSRemoteSource p r w) p r, instance Identifiable (SDSDebug p r w), instance Readable (SDSDebug p r w) p r, instance Writeable (SDSDebug p r w) p w, instance Registrable (SDSDebug p r w) p r, instance Modifiable (SDSDebug p r w) p r w

from StdFunc import id, const, o
from Data.List import instance Functor []
