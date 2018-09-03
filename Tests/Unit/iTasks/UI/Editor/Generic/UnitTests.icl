module iTasks.UI.Editor.Generic.UnitTests

import iTasks.UI.Editor.Generic
import iTasks.Util.Testing
import qualified Data.Map as DM

//Unfortunately the javascript linker introduces
//the need to carry the iworld around in editors
import iTasks.Internal.IWorld
from iTasks.Engine import defaultEngineOptions

derive gPrint LUI, LUIChanges, LUIEffects, LUIEffectStage, LUINo, Set, UI, UIType, JSONNode, Map
derive gPrint MaybeError, Maybe, UIChange, UIChildChange, UIAttributeChange
derive gPrint EditMask, FieldMask

tests = flatten
	[primitiveEditorTests
	,compositeEditorTests
	]

primitiveEditorTests = flatten
	[intEditorTests
	,realEditorTests
	,charEditorTests
	,boolEditorTests
	,stringEditorTests
	]

compositeEditorTests = flatten
	[adtEditorTests
	,recordEditorTests
	,tupleEditorTests
	]

//For different types consider:
//- Generate the initial UI (mandatory and optional editors) 
//- Process different types of events (mandatory and optional editors)
//- Refresh with different values  (mandatory and optional editors)
//- Consider different edit modes 

intEditorTests =
	[genRequiredIntUI
	,genOptionalIntUI
	]

genRequiredIntUI = assertEqualWorld "Generate UI for Editor of type Int"
	(Ok (UI UIIntegerField 
				('DM'.fromList
					[("hint-type",JSONString "info")
					,("editorId",JSONString "v")
					,("hint",JSONString "Please enter a whole number (this value is required)")
					,("optional",JSONBool False)
					,("mode",JSONString "enter")
					,("taskId",JSONString "4-2")
					,("value",JSONNull)
					] 
				)
			[]
	    , FieldMask {touched=False,valid=False,state=JSONNull}))
	(genUIWrapper [] 0 Enter intEditor)
where
	intEditor :: Editor Int
	intEditor = gEditor{|*|}

genOptionalIntUI = assertEqualWorld "Generate UI for Editor of type Maybe Int"
	(Ok (UI UIIntegerField 
				('DM'.fromList
					[("hint-type",JSONString "info")
					,("editorId",JSONString "v")
					,("hint",JSONString "Please enter a whole number")
					,("optional",JSONBool True)
					,("mode",JSONString "enter")
					,("taskId",JSONString "4-2")
					,("value",JSONNull)
					] 
				)
			[]
	    , FieldMask {touched=False,valid=True,state=JSONNull}))
	(genUIWrapper [] Nothing Enter intEditor)
where
	intEditor :: Editor (Maybe Int)
	intEditor = gEditor{|*|}

realEditorTests = []
charEditorTests = []
boolEditorTests = []
stringEditorTests = []

:: EnumADT = ACons | BCons | CCons

:: GroupADT = GroupCons Int Int

:: RecursiveADT = NilCons | RecCons RecursiveADT

:: MixedADT = MixedGroupA Int String | MixedSimpleA | MixedGroupB Bool

derive gEditor EnumADT, GroupADT, RecursiveADT, MixedADT
derive gDefault RecursiveADT

adtEditorTests = []

:: TwoFieldRecord =
	{ fieldA :: String
	, fieldB :: Int
	}
derive gEditor TwoFieldRecord
derive gPrint TwoFieldRecord
derive gEq TwoFieldRecord

recordEditorTests = 
	[genRequiredTwoFieldRecordUI
	,editRequiredTwoFieldRecord
	]

genRequiredTwoFieldRecordUI = assertEqualWorld "Generate UI for Editor of type TwoFieldRecord"
	(Ok (UI UIRecord
			'DM'.newMap
			[ UI UITextField 
				('DM'.fromList
					[("hint-type",JSONString "info")
					,("editorId",JSONString "v0")
					,("hint",JSONString "Please enter a single line of text (this value is required)")
					,("optional",JSONBool False)
					,("mode",JSONString "enter")
					,("taskId",JSONString "4-2")
					,("value",JSONNull)
					,("label",JSONString "fieldA")
					,("minlength",JSONInt 1)
					] 
				)
				[]
			, UI UIIntegerField 
				('DM'.fromList
					[("hint-type",JSONString "info")
					,("editorId",JSONString "v1")
					,("hint",JSONString "Please enter a whole number (this value is required)")
					,("optional",JSONBool False)
					,("mode",JSONString "enter")
					,("taskId",JSONString "4-2")
					,("value",JSONNull)
					,("label",JSONString "fieldB")
					] 
				)
				[]
			]
	    , CompoundMask 
			[FieldMask {touched=False,valid=False,state=JSONNull}
			,FieldMask {touched=False,valid=False,state=JSONNull}
			]
		)
	)
	(genUIWrapper [] {fieldA="",fieldB=0} Enter editor)
where
	editor :: Editor TwoFieldRecord
	editor = gEditor{|*|}


editRequiredTwoFieldRecord = assertEqualWorld "Edit UI for Editor of type TwoFieldRecord"
	((Ok (postChange
		, CompoundMask 
				[FieldMask {touched=True,valid=True,state=JSONString "x"}
				,FieldMask {touched=False,valid=False,state=JSONNull}
				]
		)
	,{fieldA="x",fieldB=0}
	)
	)
	(onEditWrapper [] edit preValue preMask Enter editor)
where
	editor :: Editor TwoFieldRecord
	editor = gEditor{|*|}

	edit = ([0],JSONString "x")
	preValue = {fieldA="",fieldB=0}
	preMask = CompoundMask 
			[FieldMask {touched=False,valid=False,state=JSONNull}
			,FieldMask {touched=False,valid=False,state=JSONNull}
			]

	postChange = 
		ChangeUI
		[]
		[
			(0,ChangeChild 
				(ChangeUI
					[SetAttribute "value" (JSONString "x")
					,SetAttribute "hint" (JSONString "You have correctly entered a single line of text")
					,SetAttribute "hint-type" (JSONString "valid")
					]
					[]
				)
			)
		]
	postValue = {fieldA="x",fieldB=0}
	postMask = CompoundMask 
				[FieldMask {touched=True,valid=True,state=JSONString "x"}
				,FieldMask {touched=False,valid=False,state=JSONNull}
				]

tupleEditorTests = []

genUIWrapper datapath value mode editor world	
	# (options,world) = defaultEngineOptions world 
	# iworld = createIWorld options world
	# vst = {taskId = "4-2", mode = mode, optional = False,selectedConsIndex=0,iworld=iworld}
	# (res,{VSt|iworld={IWorld|world}}) = editor.genUI datapath value vst
	= (res,world)

onEditWrapper datapath edit preValue preMask mode editor world
	# (options,world) = defaultEngineOptions world 
	# iworld = createIWorld options world
	# vst = {taskId = "4-2", mode = mode, optional = False,selectedConsIndex=0,iworld=iworld}
	# (res,postValue,{VSt|iworld={IWorld|world}}) = editor.Editor.onEdit datapath edit preValue preMask vst
	= ((res,postValue),world)

Start w = runUnitTests tests w
