module iTasks.UI.Layout.UnitTests

import iTasks.UI.Layout.ReferenceImplementations

import iTasks.Internal.Test.Definition
import iTasks.UI.Layout
import qualified Data.Map as DM
import qualified Data.Set as DS

derive gEq LUI, LUIChanges, LUIEffects, LUIEffectStage, LUINo
derive gPrettyTrace LUI, LUIChanges, LUIEffects, LUIEffectStage, LUINo, JSONNode, Set, Maybe
derive gPrettyTrace UIChange, UIChildChange, UIAttributeChange, UI, UIType

import Data.GenLexOrd
derive gLexOrd LUIEffectStage

instance < (LUIEffectStage a) | gLexOrd{|*|} a
where
	(<) x y = (gLexOrd{|*|} x y) === LT

//Same ui as unchanged ui state
lui0 = LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
	[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
	,LUINode UIStep 'DM'.newMap [] noChanges noEffects
	] noChanges noEffects

//Somewhat bigger ui for testing shift operations
lui00 = LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
	[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
	,LUINode UIStep 'DM'.newMap [] noChanges noEffects
	,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
	,LUINode UIRecord 'DM'.newMap [] noChanges noEffects
	] noChanges noEffects

lui01 = LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
	[LUINode UIInteract 'DM'.newMap [] {noChanges & toBeShifted = Just 0} noEffects
	,LUINode UIStep 'DM'.newMap [] noChanges noEffects
	,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
	,LUIShiftDestination 0
	,LUINode UIRecord 'DM'.newMap [] noChanges noEffects
	] noChanges noEffects

//Same, with an additional node
lui1 = LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
	[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
	,LUINode UIEmpty 'DM'.newMap [] noChanges {noEffects & additional = ESApplied (LUINo [0])}
	,LUINode UIStep 'DM'.newMap [] noChanges noEffects
	] noChanges noEffects

//Check if upstream changes are correctly put in the ui tree
applyUpstreamChangeTests =
	[assertEqual "No change" 
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIStep 'DM'.newMap [] noChanges noEffects
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange NoChange (lui0,initLUIMoves))
	,assertEqual "Root replace" 
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")])  
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIStep 'DM'.newMap [] noChanges noEffects
			] {noChanges & toBeReplaced = Just (LUINode UIEmpty 'DM'.newMap [] noChanges noEffects)} noEffects
		,initLUIMoves) 
		(applyUpstreamChange (ReplaceUI (UI UIEmpty 'DM'.newMap [])) (lui0,initLUIMoves))
	,assertEqual "Child replace" 
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIStep 'DM'.newMap [] {noChanges & toBeReplaced = Just (LUINode UIEmpty 'DM'.newMap [] noChanges noEffects)} noEffects
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [] [(1,ChangeChild (ReplaceUI (UI UIEmpty 'DM'.newMap [])))]) (lui0,initLUIMoves))
	,assertEqual "Helper function for replace adjustIndex"
		2
		(adjustIndex_ (LUINo []) 1 
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIEmpty 'DM'.newMap [] noChanges {noEffects & additional = ESApplied (LUINo [1])}
			,LUINode UIStep 'DM'.newMap [] {noChanges & toBeReplaced = Just (LUINode UIEmpty 'DM'.newMap [] noChanges noEffects)} noEffects
			] initLUIMoves
		)
	,assertEqual "Child replace (with additional node)" 
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIEmpty 'DM'.newMap [] noChanges {noEffects & additional = ESApplied (LUINo [0])}
			,LUINode UIStep 'DM'.newMap [] {noChanges & toBeReplaced = Just (LUINode UIEmpty 'DM'.newMap [] noChanges noEffects)} noEffects
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [] [(1,ChangeChild (ReplaceUI (UI UIEmpty 'DM'.newMap [])))]) (lui1,initLUIMoves))

	,assertEqual "Child remove" 
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIStep 'DM'.newMap [] {noChanges & toBeRemoved = True} noEffects
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [] [(1,RemoveChild)]) (lui0,initLUIMoves))
	,assertEqual "Child remove (with additional node)" 
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIEmpty 'DM'.newMap [] noChanges {noEffects & additional = ESApplied (LUINo [0])}
			,LUINode UIStep 'DM'.newMap [] {noChanges & toBeRemoved = True} noEffects
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [] [(1,RemoveChild)]) (lui1,initLUIMoves))
	,assertEqual "Child insert" 
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIEmpty 'DM'.newMap [] {noChanges & toBeInserted = True} noEffects
			,LUINode UIStep 'DM'.newMap [] noChanges noEffects
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [] [(1,InsertChild (UI UIEmpty 'DM'.newMap []))]) (lui0,initLUIMoves))

	,assertEqual "Child insert (with additional node)" 
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIEmpty 'DM'.newMap [] noChanges {noEffects & additional = ESApplied (LUINo [0])}
			,LUINode UIEmpty 'DM'.newMap [] {noChanges & toBeInserted = True} noEffects
			,LUINode UIStep 'DM'.newMap [] noChanges noEffects
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [] [(1,InsertChild (UI UIEmpty 'DM'.newMap []))]) (lui1,initLUIMoves))
	,assertEqual "Child replace a newly inserted child" 
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIEmpty 'DM'.newMap [] {noChanges & toBeInserted = True} noEffects
			,LUINode UIStep 'DM'.newMap [] noChanges noEffects
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [] [(1,InsertChild (UI UIInteract 'DM'.newMap [])),(1,ChangeChild (ReplaceUI (UI UIEmpty 'DM'.newMap [])))]) (lui0,initLUIMoves))
	,assertEqual "Helper function for shift adjustIndex"
		2
		(adjustIndex_ (LUINo [0]) 1 
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIStep 'DM'.newMap [] {noChanges & toBeShifted = Just 0} noEffects
			] initLUIMoves
		)
	,assertEqual "Child shift down" 
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUINode UIInteract 'DM'.newMap [] {noChanges & toBeShifted = Just 0} noEffects
			,LUINode UIStep 'DM'.newMap [] noChanges noEffects
			,LUIShiftDestination 0
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [] [(0,MoveChild 1)]) (lui0,initLUIMoves))
	,assertEqual "Child shift in two steps" 
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUINode UIInteract 'DM'.newMap [] {noChanges & toBeShifted = Just 0} noEffects
			,LUINode UIStep 'DM'.newMap [] noChanges noEffects
			,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
			,LUINode UIRecord 'DM'.newMap [] noChanges noEffects
			,LUIShiftDestination 0
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [] [(0,MoveChild 2),(2,MoveChild 3)]) (lui00,initLUIMoves))
	,assertEqual "Child shift to original position in multiple steps" 
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIStep 'DM'.newMap [] noChanges noEffects
			,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
			,LUINode UIRecord 'DM'.newMap [] noChanges noEffects
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [] [(1,MoveChild 2),(2,MoveChild 3),(3,MoveChild 1)]) (lui00,initLUIMoves))
	,assertEqual "Root set attribute"
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")])
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIStep 'DM'.newMap [] noChanges noEffects
			] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "changed-title")]} noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [SetAttribute "title" (JSONString "changed-title")] []) (lui0,initLUIMoves))
	,assertEqual "Root delete attribute"
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIStep 'DM'.newMap [] noChanges noEffects
			] {noChanges & delAttributes = 'DS'.fromList ["title"]} noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [DelAttribute "title"] []) (lui0,initLUIMoves))
	,assertEqual "Set attribute after shift"
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUINode UIInteract 'DM'.newMap [] {noChanges & toBeShifted = Just 0, setAttributes = 'DM'.fromList [("title",JSONString "changed-title")]} noEffects
			,LUINode UIStep 'DM'.newMap [] noChanges noEffects
			,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
			,LUIShiftDestination 0
			,LUINode UIRecord 'DM'.newMap [] noChanges noEffects
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [] [(0,MoveChild 2)
		                                  ,(2,ChangeChild (ChangeUI [SetAttribute "title" (JSONString "changed-title")] []))
										  ]) (lui00,initLUIMoves))
	,assertEqual "Remove child after shift"
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUINode UIInteract 'DM'.newMap [] {noChanges & toBeRemoved = True } noEffects
			,LUINode UIStep 'DM'.newMap [] noChanges noEffects
			,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
			,LUINode UIRecord 'DM'.newMap [] noChanges noEffects
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [] [(0,MoveChild 2)
		                                  ,(2,RemoveChild)
										  ]) (lui00,initLUIMoves))
	,assertEqual "Set attribute on wrapped child"
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")])
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIStep 'DM'.newMap 
				[LUINode UIDebug 'DM'.newMap [] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "changed-title")]} noEffects
				] noChanges {noEffects & wrapper = ESApplied (LUINo [0])}
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [] [(1,ChangeChild (ChangeUI [SetAttribute "title" (JSONString "changed-title")] []))]) 
			(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap 
					[LUINode UIDebug 'DM'.newMap [] noChanges noEffects
					] noChanges {noEffects & wrapper = ESApplied (LUINo [0])}
				] noChanges noEffects, initLUIMoves)
		)
	,assertEqual "Set attribute in moved child"
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")])
			[LUIMoveSource (LUINo [0]) 0
			,LUINode UIStep 'DM'.newMap 
				[LUINode UIDebug 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects 
			,LUIMoveDestination (LUINo [0]) 0
			] noChanges noEffects
		,'DM'.fromList [((LUINo [0],0),LUINode UIInteract 'DM'.newMap [] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "changed-title")]} {noEffects & moved = ESApplied (LUINo [0])})])
		(applyUpstreamChange (ChangeUI [] [(0,ChangeChild (ChangeUI [SetAttribute "title" (JSONString "changed-title")] []))]) 
			(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUIMoveSource (LUINo [0]) 0
				,LUINode UIStep 'DM'.newMap 
					[LUINode UIDebug 'DM'.newMap [] noChanges noEffects
					] noChanges noEffects
				,LUIMoveDestination (LUINo [0]) 0
				] noChanges noEffects
			,'DM'.fromList [((LUINo [0],0),LUINode UIInteract 'DM'.newMap [] noChanges {noEffects & moved = ESApplied (LUINo [0])})])
		)
	]

//Check if pending downstream changes are correctly extracted from the tree
extractDownstreamChangeTests = 
	[extractDownstreamChangeTest_TopLevelReplace
	,extractDownstreamChangeTest_TopLevelReplaceWithReplacedChild
	,extractDownstreamChangeTest_TopLevelReplaceWithMovedChild
	,extractDownstreamChangeTest_TopLevelReplaceWithShiftedChildren
	,extractDownstreamChangeTest_TopLevelReplaceWithChangedAttribute
	,extractDownstreamChangeTest_TopLevelReplaceWithOverwrittenType
	,extractDownstreamChangeTest_TopLevelReplaceWithOverwrittenAttribute
	,extractDownstreamChangeTest_TopLevelReplaceWithHiddenAttribute
	,extractDownstreamChangeTest_TopLevelReplaceWithInsertedChild
	,extractDownstreamChangeTest_TopLevelOverwrittenType
	,extractDownstreamChangeTest_RemovedChild
	,extractDownstreamChangeTest_InsertedChild
	,extractDownstreamChangeTest_ShiftedChildren
	,extractDownstreamChangeTest_SetChildAttribute
	,extractDownstreamChangeTest_DeleteChildAttribute
	,extractDownstreamChangeTest_OverwrittenChildAttribute
	,extractDownstreamChangeTest_NewAdditionalChild
	,extractDownstreamChangeTest_RemovedAdditionalChild
	,extractDownstreamChangeTest_NewHiddenChild
	,extractDownstreamChangeTest_RemovedHiddenChild
	,extractDownstreamChangeTest_NewMovedChild
	,extractDownstreamChangeTest_NewMovedChildren
	,extractDownstreamChangeTest_NoLongerMovedChild
	,extractDownstreamChangeTest_ChangeInChildrenWithMoves
	,extractDownstreamChangeTest_NewWrappedChild
	,extractDownstreamChangeTest_NoLongerWrappedChild
	,extractDownstreamChangeTest_NewUnwrappedChild
	,extractDownstreamChangeTest_NoLongerUnwrappedChild
	,extractDownstreamChangeTest_ChangingAnUnwrappedAttribute
	,extractDownstreamChangeTest_InsertIntoUnwrappedContainer
	,extractDownstreamChangeTest_RemovingUnwrappedContainer
	,extractDownstreamChangeTest_ShiftingInUnwrappedContainer
	,extractDownstreamChangeTest_MovingIntoAdditionalContainer
	]

extractDownstreamChangeTest_TopLevelReplace =
	assertEqual "Simple top-level replace without effects" 
		(ReplaceUI (UI UIEmpty 'DM'.newMap [])
		,(LUINode UIEmpty 'DM'.newMap [] noChanges noEffects
		 ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & toBeReplaced = Just (LUINode UIEmpty 'DM'.newMap [] noChanges noEffects)} noEffects
		,initLUIMoves))

extractDownstreamChangeTest_TopLevelReplaceWithReplacedChild =
	assertEqual "Top-level replace with a replaced child without effects" 
		(ReplaceUI (UI UIParallel 'DM'.newMap [])
		,(LUINode UIParallel 'DM'.newMap [] noChanges noEffects
		 ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & toBeReplaced = Just (LUINode UIEmpty 'DM'.newMap []
					{noChanges & toBeReplaced = Just (LUINode UIParallel 'DM'.newMap [] noChanges noEffects)} noEffects)
				  } noEffects
		,initLUIMoves))

extractDownstreamChangeTest_TopLevelReplaceWithMovedChild =
	assertEqual "Top-level replace with a removed child without effects" 
		(ReplaceUI (UI UIParallel 'DM'.newMap [UI UIRecord 'DM'.newMap []])
		,(LUINode UIParallel 'DM'.newMap
			[LUINode UIRecord 'DM'.newMap [] noChanges noEffects
			] noChanges noEffects
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & toBeReplaced = Just (LUINode UIEmpty 'DM'.newMap []
					{noChanges & toBeReplaced = Just (LUINode UIParallel 'DM'.newMap
						[LUINode UIDebug 'DM'.newMap [] {noChanges & toBeRemoved = True} noEffects
						,LUINode UIRecord 'DM'.newMap [] noChanges noEffects
						] noChanges noEffects)} noEffects)
				  } noEffects
		,initLUIMoves))

extractDownstreamChangeTest_TopLevelReplaceWithShiftedChildren =
	assertEqual "Top-level replace with shifted children without effects" 
		(ReplaceUI (UI UIParallel 'DM'.newMap [UI UIRecord 'DM'.newMap [],UI UIContainer 'DM'.newMap [], UI UIPanel 'DM'.newMap [], UI UIDebug 'DM'.newMap []])
		,(LUINode UIParallel 'DM'.newMap
			[LUINode UIRecord 'DM'.newMap [] noChanges noEffects
			,LUINode UIContainer 'DM'.newMap [] noChanges noEffects
			,LUINode UIPanel 'DM'.newMap [] noChanges noEffects
			,LUINode UIDebug 'DM'.newMap [] noChanges noEffects
			] noChanges noEffects
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & toBeReplaced = Just (LUINode UIEmpty 'DM'.newMap []
					{noChanges & toBeReplaced = Just (LUINode UIParallel 'DM'.newMap
						[LUINode UIDebug 'DM'.newMap [] {noChanges & toBeShifted = Just 0} noEffects
						,LUINode UIRecord 'DM'.newMap [] noChanges noEffects
						,LUIShiftDestination 1
						,LUINode UIPanel 'DM'.newMap [] noChanges noEffects
						,LUINode UIContainer 'DM'.newMap [] {noChanges & toBeShifted = Just 1} noEffects
						,LUIShiftDestination 0
						] noChanges noEffects)} noEffects)
				  } noEffects
		,initLUIMoves))

extractDownstreamChangeTest_TopLevelReplaceWithChangedAttribute =
	assertEqual "Top-level replace with a changed attribute child without effects" 
		(ReplaceUI (UI UIParallel 'DM'.newMap [UI UIRecord 'DM'.newMap [], UI UIInteract ('DM'.fromList [("attr",JSONString "B")]) []])
		,(LUINode UIParallel 'DM'.newMap
			[LUINode UIRecord 'DM'.newMap [] noChanges noEffects
			,LUINode UIInteract ('DM'.fromList [("attr",JSONString "B")]) [] noChanges noEffects
			] noChanges noEffects
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & toBeReplaced = Just (LUINode UIParallel 'DM'.newMap 
						[LUINode UIRecord 'DM'.newMap [] noChanges noEffects
						,LUINode UIInteract ('DM'.fromList [("attr",JSONString "A")]) [] {noChanges & setAttributes ='DM'.fromList [("attr",JSONString "B")] } noEffects
						] noChanges noEffects)
				  } noEffects
		,initLUIMoves))

extractDownstreamChangeTest_TopLevelReplaceWithOverwrittenType =
	assertEqual "Top-level replace with an overwritten type" 
		(ReplaceUI (UI UIParallel 'DM'.newMap [])
		,(LUINode UIEmpty 'DM'.newMap
			[] noChanges {noEffects & overwrittenType = ESApplied UIParallel}
		 ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & toBeReplaced = Just (LUINode UIEmpty 'DM'.newMap []
					noChanges {noEffects & overwrittenType = ESToBeApplied UIParallel})
				  } noEffects

		,initLUIMoves))

extractDownstreamChangeTest_TopLevelReplaceWithOverwrittenAttribute =
	assertEqual "Top-level replace with an overwritten attribute" 
		(ReplaceUI (UI UIEmpty ('DM'.fromList [("title",JSONString "B")]) [])
		,(LUINode UIEmpty ('DM'.fromList [("title",JSONString "A")])
			[] noChanges {noEffects & overwrittenAttributes = 'DM'.fromList [("title",ESApplied (JSONString "B"))]}
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & toBeReplaced = Just (LUINode UIEmpty ('DM'.fromList [("title",JSONString "A")]) []
					noChanges {noEffects & overwrittenAttributes = 'DM'.fromList [("title",ESToBeApplied (JSONString "B"))]})
				  } noEffects
		,initLUIMoves))

extractDownstreamChangeTest_TopLevelReplaceWithHiddenAttribute =
	assertEqual "Top-level replace with a hidden attribute" 
		(ReplaceUI (UI UIEmpty 'DM'.newMap [])
		,(LUINode UIEmpty ('DM'.fromList [("title",JSONString "A")])
			[] noChanges {noEffects & hiddenAttributes = 'DM'.fromList [("title",ESApplied ())]}
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & toBeReplaced = Just (LUINode UIEmpty ('DM'.fromList [("title",JSONString "A")]) []
					noChanges {noEffects & hiddenAttributes = 'DM'.fromList [("title",ESToBeApplied ())]})
				  } noEffects
		,initLUIMoves))

extractDownstreamChangeTest_TopLevelReplaceWithInsertedChild =
	assertEqual "Top-level replace with an inserted child " 
		(ReplaceUI (UI UIEmpty 'DM'.newMap [UI UIPanel 'DM'.newMap []])
		,(LUINode UIEmpty 'DM'.newMap
			[LUINode UIPanel 'DM'.newMap [] noChanges {noEffects & additional = ESApplied (LUINo [0])}
			] noChanges noEffects
		 ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & toBeReplaced = Just 
						(LUINode UIEmpty ('DM'.newMap)
							[ LUINode UIPanel 'DM'.newMap [] noChanges {noEffects & additional = ESToBeApplied (LUINo [0])}
							] noChanges noEffects
						)
				  } noEffects
		,initLUIMoves))

extractDownstreamChangeTest_TopLevelOverwrittenType =
	assertEqual "Top-level overwritten type"
		(ReplaceUI (UI UIContainer 'DM'.newMap
			[UI UIInteract 'DM'.newMap []
			])
		,(LUINode UIStep 'DM'.newMap
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			] noChanges {noEffects & overwrittenType = ESApplied UIContainer}
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIStep 'DM'.newMap
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				] noChanges {noEffects & overwrittenType = ESToBeApplied UIContainer}
		,initLUIMoves))

extractDownstreamChangeTest_RemovedChild =
	assertEqual "Removed child without effects" 
		(ChangeUI [] [(1,RemoveChild)]
		,(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] {noChanges & toBeRemoved = True} noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_InsertedChild =
	assertEqual "Inserted child without effects" 
		(ChangeUI [] [(1,InsertChild (UI UIStep 'DM'.newMap []))]
		,(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] {noChanges & toBeInserted = True} noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_ShiftedChildren =
	assertEqual "Shifted children without effects" 
		(ChangeUI [] [(2,MoveChild 4),(3,MoveChild 2)]
		,(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIDebug 'DM'.newMap [] noChanges noEffects
				,LUINode UIRecord 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] {noChanges & toBeShifted = Just 0} noEffects
				,LUIShiftDestination 1
				,LUINode UIRecord 'DM'.newMap [] noChanges noEffects
				,LUINode UIDebug 'DM'.newMap [] {noChanges & toBeShifted = Just 1} noEffects
				,LUIShiftDestination 0
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_SetChildAttribute =
	assertEqual "Set attribute in child without effects" 
		(ChangeUI [] [(2,ChangeChild (ChangeUI [SetAttribute "title" (JSONString "New attribute")] []))]
		,(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel ('DM'.fromList [("title",JSONString "New attribute")]) [] noChanges noEffects
				] noChanges noEffects
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "New attribute")]} noEffects
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_DeleteChildAttribute =
	assertEqual "Delete attribute in child without effects" 
		(ChangeUI [] [(2,ChangeChild (ChangeUI [DelAttribute "title"] []))]
		,(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel ('DM'.fromList [("title",JSONString "Old attribute")]) [] {noChanges & delAttributes = 'DS'.fromList ["title"]} noEffects
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_OverwrittenChildAttribute =
	assertEqual "Overwritten attribute in child" 
		(ChangeUI [] [(2,ChangeChild (ChangeUI [SetAttribute "title" (JSONString "B")] []))]
		,(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel ('DM'.fromList [("title",JSONString "A")]) [] noChanges
					{noEffects & overwrittenAttributes = 'DM'.fromList [("title",ESApplied (JSONString "B"))]}
				] noChanges noEffects
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel ('DM'.fromList [("title",JSONString "A")]) [] noChanges
					{noEffects & overwrittenAttributes = 'DM'.fromList [("title",ESToBeApplied (JSONString "B"))]}
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_NewAdditionalChild =
	assertEqual "New additional child" 
		(ChangeUI [] [(2,InsertChild (UI UIParallel 'DM'.newMap []))]
		 ,(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges {noEffects & additional = ESApplied (LUINo [0])}
				] noChanges noEffects
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges {noEffects & additional = ESToBeApplied (LUINo [0])}
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_RemovedAdditionalChild =
	assertEqual "Removed additional child" 
		(ChangeUI [] [(2,RemoveChild)]
		 ,(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges {noEffects & additional = ESToBeRemoved (LUINo [0])}
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_NewHiddenChild =
	assertEqual "New hidden child" 
		(ChangeUI [] [(2,RemoveChild)]
		 ,(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges {noEffects & hidden = ESApplied (LUINo [0])}
				] noChanges noEffects
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges {noEffects & hidden = ESToBeApplied (LUINo [0])}
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_RemovedHiddenChild =
	assertEqual "Removed hidden child" 
		(ChangeUI [] [(2,InsertChild (UI UIParallel 'DM'.newMap []))]
		 ,(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		 ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges {noEffects & hidden = ESToBeRemoved (LUINo [0])}
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_NewMovedChild =
	assertEqual "New moved child" 
		(ChangeUI [] [(1,ChangeChild (ChangeUI [] [(0,InsertChild (UI UIParallel 'DM'.newMap []))])),(2,RemoveChild)]
		 ,(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [
						LUIMoveDestination (LUINo [0]) 0
					] noChanges noEffects
				,LUIMoveSource (LUINo [0]) 0
				] noChanges noEffects 
		  ,'DM'.fromList [((LUINo [0],0), LUINode UIParallel 'DM'.newMap [] noChanges {LUIEffects|noEffects & moved = ESApplied (LUINo [0])})])
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [
						LUIMoveDestination (LUINo [0]) 0
					] noChanges noEffects
				,LUIMoveSource (LUINo [0]) 0
				] noChanges noEffects 
		,'DM'.fromList [((LUINo [0],0),LUINode UIParallel 'DM'.newMap [] noChanges {LUIEffects|noEffects & moved = ESToBeApplied (LUINo [0])})]))

extractDownstreamChangeTest_NewMovedChildren =
	assertEqual "New moved children" 
		(ChangeUI [] [(0,RemoveChild),(0,ChangeChild (ChangeUI [] [(0,InsertChild (UI UIInteract 'DM'.newMap [])),(1,InsertChild (UI UIParallel 'DM'.newMap []))])),(1,RemoveChild)]
		 ,(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUIMoveSource (LUINo [0]) 0
				,LUINode UIStep 'DM'.newMap [
						LUIMoveDestination (LUINo [0]) 0
						,LUIMoveDestination (LUINo [0]) 1
					] noChanges noEffects
				,LUIMoveSource (LUINo [0]) 1
				] noChanges noEffects
		  ,'DM'.fromList 
				[((LUINo [0],0),LUINode UIInteract 'DM'.newMap [] noChanges {LUIEffects|noEffects & moved = ESApplied (LUINo [0])})
				,((LUINo [0],1),LUINode UIParallel 'DM'.newMap [] noChanges {LUIEffects|noEffects & moved = ESApplied (LUINo [0])})
				])
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUIMoveSource (LUINo [0]) 0
				,LUINode UIStep 'DM'.newMap [
						LUIMoveDestination (LUINo [0]) 0
						,LUIMoveDestination (LUINo [0]) 1
					] noChanges noEffects
				,LUIMoveSource (LUINo [0]) 1
				] noChanges noEffects
		,'DM'.fromList 
			[((LUINo [0],0),LUINode UIInteract 'DM'.newMap [] noChanges {LUIEffects|noEffects & moved = ESToBeApplied (LUINo [0])})
			,((LUINo [0],1),LUINode UIParallel 'DM'.newMap [] noChanges {LUIEffects|noEffects & moved = ESToBeApplied (LUINo [0])})
			]))

extractDownstreamChangeTest_NoLongerMovedChild =
	assertEqual "No longer moved child" 
		(ChangeUI [] [(1,ChangeChild (ChangeUI [] [(0,RemoveChild)])),(2,InsertChild (UI UIParallel 'DM'.newMap []))]
		 ,(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
	 	 ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [
						LUIMoveDestination (LUINo [0]) 0
					] noChanges noEffects
				,LUIMoveSource (LUINo [0]) 0
				] noChanges noEffects
			,'DM'.fromList [((LUINo [0],0),LUINode UIParallel 'DM'.newMap [] noChanges {LUIEffects|noEffects & moved = ESToBeRemoved (LUINo [0])})]
		))

extractDownstreamChangeTest_ChangeInChildrenWithMoves =
	assertEqual "Change in set of children with moved item" 
		(ChangeUI [] [(1,ChangeChild (ChangeUI [SetAttribute "title" (JSONString "test")] []))]
		 ,(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUIMoveSource (LUINo [0]) 0
				,LUINode UIStep 'DM'.newMap [LUIMoveDestination (LUINo [0]) 0] noChanges noEffects
				,LUINode UIParallel ('DM'.fromList [("title",JSONString "test")]) [] noChanges noEffects
				] noChanges noEffects
		  ,'DM'.fromList [((LUINo [0],0),LUINode UIInteract 'DM'.newMap [] noChanges {LUIEffects|noEffects & moved = ESApplied (LUINo [0])})])
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUIMoveSource (LUINo [0]) 0
				,LUINode UIStep 'DM'.newMap [LUIMoveDestination (LUINo [0]) 0] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "test")] } noEffects
				] noChanges noEffects
		,'DM'.fromList [((LUINo [0],0),LUINode UIInteract 'DM'.newMap [] noChanges {LUIEffects|noEffects & moved = ESApplied (LUINo [0])})]))

extractDownstreamChangeTest_NewWrappedChild =
	assertEqual "New wrapped child" 
		(ChangeUI [] [(1,ChangeChild (ReplaceUI (UI UIStep 'DM'.newMap [UI UIDebug 'DM'.newMap []])))]
			,(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap
					[ LUINode UIDebug 'DM'.newMap [] noChanges noEffects
					] noChanges {noEffects & wrapper = ESApplied (LUINo [0])}
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			 ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap
					[ LUINode UIDebug 'DM'.newMap [] noChanges noEffects
					] noChanges {noEffects & wrapper = ESToBeApplied (LUINo [0])}
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_NoLongerWrappedChild =
	assertEqual "No longer wrapped child" 
		(ChangeUI [] [(1,ChangeChild (ReplaceUI (UI UIDebug 'DM'.newMap [])))]
			,(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIDebug 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			 ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap
					[LUINode UIInteract 'DM'.newMap [] noChanges {noEffects & additional = ESApplied (LUINo [1])} //Added after the wrapping, should be removed too
					,LUINode UIDebug 'DM'.newMap [] noChanges noEffects
					] noChanges {noEffects & wrapper = ESToBeRemoved (LUINo [0])}
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_NewUnwrappedChild =
	assertEqual "New unwrapped child" 
		(ChangeUI [] [(1,ChangeChild (ReplaceUI (UI UIDebug 'DM'.newMap [])))]
			,(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap
					[ LUINode UIDebug 'DM'.newMap [] noChanges noEffects
					] noChanges {noEffects & unwrapped = ESApplied (LUINo [0])}
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			 ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap
					[ LUINode UIDebug 'DM'.newMap [] noChanges noEffects
					] noChanges {noEffects & unwrapped = ESToBeApplied (LUINo [0])}
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_NoLongerUnwrappedChild =
	assertEqual "No longer unwrapped child" 
		(ChangeUI [] [(1,ChangeChild (ReplaceUI (UI UIStep 'DM'.newMap [UI UIDebug 'DM'.newMap []])))]
			,(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap
					[ LUINode UIDebug 'DM'.newMap [] noChanges noEffects
					] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			 ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap
					[ LUINode UIDebug 'DM'.newMap [] noChanges noEffects
					] noChanges {noEffects & unwrapped = ESToBeRemoved (LUINo [0])}
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_ChangingAnUnwrappedAttribute =
	assertEqual "Changing an attribute on an unwrapped child" 
		(ChangeUI [] [(1,ChangeChild (ChangeUI [SetAttribute "title" (JSONString "test")] []))]
			,(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap
					[ LUINode UIDebug ('DM'.fromList [("title",JSONString "test")]) [] noChanges noEffects
					] noChanges {noEffects & unwrapped = ESApplied (LUINo [0])}
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			 ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap
					[ LUINode UIDebug 'DM'.newMap [] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "test")]} noEffects
					] noChanges {noEffects & unwrapped = ESApplied (LUINo [0])}
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_InsertIntoUnwrappedContainer =
	assertEqual "Inserting into an unwrapped container" 
		(ReplaceUI (UI UIDebug 'DM'.newMap [])
			,(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIDebug 'DM'.newMap [] noChanges noEffects
				,LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges {noEffects & unwrapped = ESApplied (LUINo [0])}
			 ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIDebug 'DM'.newMap [] {noChanges & toBeInserted=True} noEffects
				,LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges {noEffects & unwrapped = ESApplied (LUINo [0])}
		,initLUIMoves))

extractDownstreamChangeTest_RemovingUnwrappedContainer =
	assertEqual "Removing from an unwrapped container" 
		(ReplaceUI (UI UIStep 'DM'.newMap [])
			,(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges {noEffects & unwrapped = ESApplied (LUINo [0])}
			 ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] {noChanges & toBeRemoved = True} noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges {noEffects & unwrapped = ESApplied (LUINo [0])}
		,initLUIMoves))

extractDownstreamChangeTest_ShiftingInUnwrappedContainer =
	assertEqual "Shifting in an unwrapped container" 
		(ReplaceUI (UI UIStep 'DM'.newMap [])
			,(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				,LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				] noChanges {noEffects & unwrapped = ESApplied (LUINo [0])}
			 ,initLUIMoves)
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] {noChanges & toBeShifted = Just 1} noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				,LUIShiftDestination 1
				] noChanges {noEffects & unwrapped = ESApplied (LUINo [0])}
		,initLUIMoves))

extractDownstreamChangeTest_MovingIntoAdditionalContainer =
	assertEqual "Moving items to a newly inserted container" 
	(ChangeUI [] [(1,InsertChild (UI UIParallel 'DM'.newMap [UI UIStep 'DM'.newMap []])), (2,RemoveChild)]
		,(LUINode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [
					LUIMoveDestination (LUINo [3]) 0
					] noChanges {noEffects & additional = ESApplied (LUINo [1])}
				,LUIMoveSource (LUINo [3]) 0
				] noChanges noEffects
		 ,'DM'.fromList [((LUINo [3],0),LUINode UIStep 'DM'.newMap [] noChanges {LUIEffects|noEffects & moved = ESApplied (LUINo [3])})])
	)
	(extractDownstreamChange (
		LUINode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [
					LUIMoveDestination (LUINo [3]) 0
					] noChanges {noEffects & additional = ESToBeApplied (LUINo [1])}
				,LUIMoveSource (LUINo [3]) 0
				] noChanges noEffects
		,'DM'.fromList [((LUINo [3],0),LUINode UIStep 'DM'.newMap [] noChanges {LUIEffects|noEffects & moved = ESToBeApplied (LUINo [3])})]))

extractUIWithEffectsTests =
	[assertEqual "Extract UI with newly moved items" 
		(UI UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[UI UIStep 'DM'.newMap
				[UI UIInteract 'DM'.newMap []
				,UI UIParallel 'DM'.newMap []
				]
			]
		,(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUIMoveSource (LUINo [0]) 0
			,LUINode UIStep 'DM'.newMap [LUIMoveDestination (LUINo [0]) 0,LUIMoveDestination (LUINo [0]) 1] noChanges noEffects
			,LUIMoveSource (LUINo [0]) 1
			] noChanges noEffects
		  ,'DM'.fromList 
				[((LUINo [0],0),LUINode UIInteract 'DM'.newMap [] noChanges {LUIEffects|noEffects & moved = ESApplied (LUINo [0])})
				,((LUINo [0],1),LUINode UIParallel 'DM'.newMap [] noChanges {LUIEffects|noEffects & moved = ESApplied (LUINo [0])})
				])
		)
		(extractUIWithEffects (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUIMoveSource (LUINo [0]) 0
				,LUINode UIStep 'DM'.newMap [LUIMoveDestination (LUINo [0]) 0,LUIMoveDestination (LUINo [0]) 1] noChanges noEffects
				,LUIMoveSource (LUINo [0]) 1
				] noChanges noEffects
		,'DM'.fromList 
			[((LUINo [0],0),LUINode UIInteract 'DM'.newMap [] noChanges {LUIEffects|noEffects & moved = ESToBeApplied (LUINo [0])})
			,((LUINo [0],1),LUINode UIParallel 'DM'.newMap [] noChanges {LUIEffects|noEffects & moved = ESApplied (LUINo [0])})
			]))
	]

selectNode_Tests = 
	[ assertEqual "Selecting a shifted child node"
		(Just (LUINode UIDebug 'DM'.newMap [] {noChanges & toBeShifted = Just 1} noEffects))
		(selectNode_ (LUINo [0]) [2] (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] {noChanges & toBeShifted = Just 0} noEffects
				,LUIShiftDestination 1
				,LUINode UIRecord 'DM'.newMap [] noChanges noEffects
				,LUINode UIDebug 'DM'.newMap [] {noChanges & toBeShifted = Just 1} noEffects
				,LUIShiftDestination 0
				] noChanges noEffects
		,initLUIMoves))
	]
updateNode_Tests = 
	[ assertEqual "Updating a shifted child node"
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] {noChanges & toBeShifted = Just 0} noEffects
				,LUIShiftDestination 1
				,LUINode UIRecord 'DM'.newMap [] noChanges noEffects
				,LUINode UIInteract 'DM'.newMap [] {noChanges & toBeShifted = Just 1} noEffects
				,LUIShiftDestination 0
				] noChanges noEffects
		,initLUIMoves)

		(updateNode_ (LUINo [0]) [2]
			(\(LUINode _ attr items changes effects,m) -> (LUINode UIInteract attr items changes effects,m)) 
			(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] {noChanges & toBeShifted = Just 0} noEffects
				,LUIShiftDestination 1
				,LUINode UIRecord 'DM'.newMap [] noChanges noEffects
				,LUINode UIDebug 'DM'.newMap [] {noChanges & toBeShifted = Just 1} noEffects
				,LUIShiftDestination 0
				] noChanges noEffects
			,initLUIMoves))
	]

scanToPosition_Tests = 
	[assertEqual "Scanning to an added node"
		(3,True,Just (LUINode UIInteract 'DM'.newMap [] noChanges {noEffects & additional = ESToBeApplied (LUINo [5])}))
		(scanToPosition_ (LUINo [5]) 2
			[LUINode UIStep 'DM'.newMap [] noChanges noEffects
			,LUINode UIDebug 'DM'.newMap [] noChanges {noEffects & additional = ESToBeApplied (LUINo [6])} //Should be ignored
			,LUINode UIInteract 'DM'.newMap [] noChanges {noEffects & additional = ESToBeApplied (LUINo [1])} //Should be counted
			,LUINode UIInteract 'DM'.newMap [] noChanges {noEffects & additional = ESToBeApplied (LUINo [5])}
			,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
			]
			initLUIMoves
		)
	]

compareLUINoTests =
	[assertEqual "Comparison of ruleNumbers: 1 < 1.2" True (LUINo [1] < LUINo [1,2]) 
	,assertEqual "Comparison of ruleNumbers: 1.2 < 3" True (LUINo [1,2] < LUINo [3]) 
	,assertEqual "Comparison of ruleNumbers: 4.1 < 3.2.1" False (LUINo [4,1] < LUINo [3,2,1])
	]
setUITypeTests =
	[ //TODO
	]

setUIAttributesTests =
	[ //TODO
	]

delUIAttributesTests =
	[ //TODO
	]

modifyUIAttributesTests =
	[assertEqual "Modify attributes rule: change title to a hint" 
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "B")]}
					{noEffects
					& overwrittenAttributes = 'DM'.fromList [("hint",ESToBeApplied (JSONString "C"))]
					, hiddenAttributes = 'DM'.fromList [("title",ESToBeApplied ())]
					}
		,initLUIMoves)
		(modifyUIAttributes
			(SelectKeys ["title"])
			(\attr -> 'DM'.fromList [("hint",JSONString "C")])
			(LUINo [0])
			(LUINode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "B")]} noEffects
			,initLUIMoves)
		)
	]
copySubUIAttributesTests =
	[assertEqual "Copy sub attributes rule: copy title to attribute to a child"
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges {noEffects & overwrittenAttributes = 'DM'.fromList [("title",ESToBeApplied (JSONString "B"))]}
				] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "B")]} noEffects
		,initLUIMoves)
		(copySubUIAttributes 
			(SelectKeys ["title"]) [] [1] (LUINo [0])
			(LUINode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "B")]} noEffects
			,initLUIMoves)
		)
	]
insertChildUITests =
	[assertEqual "Insert a child rule: insert in known set"
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges {noEffects & additional = ESToBeApplied (LUINo [0])}
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		,initLUIMoves)
		(insertChildUI 1 (UI UIParallel 'DM'.newMap []) (LUINo [0])
			(LUINode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			,initLUIMoves)
		)
	]

removeSubUIsTests =
	[assertEqual "Remove children rule: Remove all UIStep nodes "
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges {noEffects & hidden = ESToBeApplied (LUINo [0])}
				] noChanges noEffects
		,initLUIMoves)
		(removeSubUIs (SelectByType UIStep) (LUINo [0])
			(LUINode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			,initLUIMoves)
		)
	]
moveSubUIsTests =
	[assertEqual "Move children rule: Move All UIStep nodes into the first parallel"
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [
					LUIMoveDestination (LUINo [0]) 0
					] noChanges noEffects
				,LUIMoveSource (LUINo [0]) 0
				] noChanges noEffects
		,'DM'.fromList [((LUINo [0],0),LUINode UIStep 'DM'.newMap [] noChanges {LUIEffects|noEffects & moved = ESToBeApplied (LUINo [0])})])
		(moveSubUIs (SelectByType UIStep) [1] 0 (LUINo [0])
			(LUINode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			,initLUIMoves)
		)
	,assertEqual "Move children rule: Move All UIStep nodes into an additional node"
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [
					LUIMoveDestination (LUINo [3]) 0
					] noChanges {noEffects & additional = ESToBeApplied (LUINo [1])}
				,LUIMoveSource (LUINo [3]) 0
				] noChanges noEffects
		,'DM'.fromList [((LUINo [3],0),LUINode UIStep 'DM'.newMap [] noChanges {LUIEffects|noEffects & moved = ESToBeApplied (LUINo [3])})])
		(moveSubUIs (SelectByType UIStep) [1] 0 (LUINo [3])
			(LUINode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects 
				,LUINode UIParallel 'DM'.newMap [] noChanges {noEffects & additional = ESToBeApplied (LUINo [1])}
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			,initLUIMoves)
		)
	]
wrapUITests =
	[assertEqual "Wrap rule: wrap root as UIStep"
		(LUINode UIStep 'DM'.newMap [
			LUINode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			] noChanges {noEffects & wrapper = ESToBeApplied (LUINo [0])}
		,initLUIMoves)
		(wrapUI UIStep (LUINo [0])
			(LUINode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			,initLUIMoves)
		)
	]

unwrapUITests =
	[assertEqual "Unwrap rule: unwrap root"
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges {noEffects & unwrapped = ESToBeApplied (LUINo [0])}
		,initLUIMoves)
		(unwrapUI (LUINo [0])
			(LUINode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			,initLUIMoves)
		)
	]

layoutSubUIsTests =
	[assertEqual "Layout sub-uis rule: change type"
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges {noEffects & overwrittenType = ESToBeApplied UIDebug}
				] noChanges noEffects
		,initLUIMoves)
		(layoutSubUIs (SelectByType UIStep) (setUIType UIDebug) (LUINo [0])
			(LUINode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			,initLUIMoves)
		)
	]



tests =  applyUpstreamChangeTests 
      ++ extractDownstreamChangeTests
	  ++ extractUIWithEffectsTests
	  ++ selectNode_Tests
	  ++ updateNode_Tests
	  ++ scanToPosition_Tests
	  ++ compareLUINoTests
	  ++ setUITypeTests
	  ++ setUIAttributesTests
	  ++ delUIAttributesTests
	  ++ modifyUIAttributesTests
	  ++ copySubUIAttributesTests
	  ++ insertChildUITests
	  ++ removeSubUIsTests
	  ++ moveSubUIsTests
	  ++ wrapUITests
	  ++ unwrapUITests
	  ++ layoutSubUIsTests
	

Start w = runUnitTestsCLI [testsuite "Test.iTasks.UI.Layout" "Duh.." tests] w
