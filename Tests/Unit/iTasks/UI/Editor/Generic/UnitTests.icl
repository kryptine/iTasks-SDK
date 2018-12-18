module iTasks.UI.Editor.Generic.UnitTests

import iTasks.UI.Editor.Generic
import iTasks.Util.Testing
import qualified Data.Map as DM

//Unfortunately the javascript linker introduces
//the need to carry the iworld around in editors
import iTasks.Internal.IWorld
from iTasks.Engine import defaultEngineOptions

derive gPrint LUI, LUIChanges, LUIEffects, LUIEffectStage, LUINo, Set, UI, UIType, JSONNode, Map, LUINode
derive gPrint MaybeError, Maybe, UIChange, UIChildChange, UIAttributeChange
derive gPrint EditState, LeafState

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
	[skip genRequiredIntUI
	,skip genOptionalIntUI
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
	    , LeafState {touched=False,state=JSONNull}))
	(genUIWrapper [] Enter intEditor)
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
	    , LeafState {touched=False,state=JSONNull}))
	(genUIWrapper [] Enter intEditor)
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
	[skip genRequiredTwoFieldRecordUI
	,skip editRequiredTwoFieldRecord
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
	    , CompoundState JSONNull
			[LeafState {touched=False,state=JSONNull}
			,LeafState {touched=False,state=JSONNull}
			]
		)
	)
	(genUIWrapper [] Enter editor)
where
	editor :: Editor TwoFieldRecord
	editor = gEditor{|*|}


editRequiredTwoFieldRecord = assertEqualWorld "Edit UI for Editor of type TwoFieldRecord"
	(Ok (postChange,postState))
	(onEditWrapper [] edit preState editor)
where
	editor :: Editor TwoFieldRecord
	editor = gEditor{|*|}

	edit = ([0],JSONString "x")
	preState = CompoundState JSONNull
			[LeafState {touched=False,state=JSONNull}
			,LeafState {touched=False,state=JSONNull}
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
	postState = CompoundState JSONNull
				[LeafState {touched=True,state=JSONString "x"}
				,LeafState {touched=False,state=JSONNull}
				]

tupleEditorTests = []

genUIWrapper datapath mode editor world	
	# (options,world) = defaultEngineOptions world 
	# iworld = createIWorld options world
	# vst = {taskId = "4-2", optional = False,selectedConsIndex=0,pathInEditMode=[],iworld=iworld}
	# (res,{VSt|iworld={IWorld|world}}) = editor.Editor.genUI datapath (mapEditMode id mode) vst
	= (res,world)

onEditWrapper datapath edit state editor world
	# (options,world) = defaultEngineOptions world 
	# iworld = createIWorld options world
	# vst = {taskId = "4-2", optional = False,selectedConsIndex=0,pathInEditMode=[],iworld=iworld}
	# (res,{VSt|iworld={IWorld|world}}) = editor.Editor.onEdit datapath edit state vst
	= (res,world)

Start w = runUnitTests tests w
