module iTasks.UI.Layout.UnitTests

import iTasks.UI.Layout
import iTasks.UI.Layout.ReferenceImplementations

import iTasks.Util.Testing
import qualified Data.Map as DM
import qualified Data.Set as DS
import Data.Maybe

derive gEq LUI, LUIChanges, LUIEffects, LUIEffectStage, LUINo
derive gPrint LUI, LUIChanges, LUIEffects, LUIEffectStage, LUINo, Set, UI, UIType, JSONNode, Map, LUINode
derive gPrint Maybe, UIChange, UIChildChange, UIAttributeChange

import Data.GenLexOrd
derive gLexOrd LUIEffectStage

instance < (LUIEffectStage a) | gLexOrd{|*|} a
where
	(<) x y = (gLexOrd{|*|} x y) === LT

//Same ui as unchanged ui state
lui0 = luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
	[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
	,luiNode UIStep 'DM'.newMap [] noChanges noEffects
	] noChanges noEffects

//Somewhat bigger ui for testing shift operations
lui00 = luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
	[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
	,luiNode UIStep 'DM'.newMap [] noChanges noEffects
	,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
	,luiNode UIRecord 'DM'.newMap [] noChanges noEffects
	] noChanges noEffects

lui01 = luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
	[luiNode UIInteract 'DM'.newMap [] {noChanges & toBeShifted = Just 0} noEffects
	,luiNode UIStep 'DM'.newMap [] noChanges noEffects
	,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
	,LUIShiftDestination 0
	,luiNode UIRecord 'DM'.newMap [] noChanges noEffects
	] noChanges noEffects

//Same, with an additional node
lui1 = luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
	[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
	,luiNode UIEmpty 'DM'.newMap [] noChanges {noEffects & additional = ESApplied (LUINo [0])}
	,luiNode UIStep 'DM'.newMap [] noChanges noEffects
	] noChanges noEffects

//Check if upstream changes are correctly put in the ui tree
applyUpstreamChangeTests =
	[assertEqual "No change" 
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
			,luiNode UIStep 'DM'.newMap [] noChanges noEffects
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange NoChange (lui0,initLUIMoves))
	,assertEqual "Root replace" 
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")])  
			[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
			,luiNode UIStep 'DM'.newMap [] noChanges noEffects
			] {noChanges & toBeReplaced = Just (luiNode UIEmpty 'DM'.newMap [] noChanges noEffects)} noEffects
		,initLUIMoves) 
		(applyUpstreamChange (ReplaceUI (UI UIEmpty 'DM'.newMap [])) (lui0,initLUIMoves))
	,assertEqual "Child replace" 
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
			,luiNode UIStep 'DM'.newMap [] {noChanges & toBeReplaced = Just (luiNode UIEmpty 'DM'.newMap [] noChanges noEffects)} noEffects
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [] [(1,ChangeChild (ReplaceUI (UI UIEmpty 'DM'.newMap [])))]) (lui0,initLUIMoves))
	,assertEqual "Child replace (with additional node)" 
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
			,luiNode UIEmpty 'DM'.newMap [] noChanges {noEffects & additional = ESApplied (LUINo [0])}
			,luiNode UIStep 'DM'.newMap [] {noChanges & toBeReplaced = Just (luiNode UIEmpty 'DM'.newMap [] noChanges noEffects)} noEffects
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [] [(1,ChangeChild (ReplaceUI (UI UIEmpty 'DM'.newMap [])))]) (lui1,initLUIMoves))
	,assertEqual "Moved child replace" 
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUIMoveDestination 2 (LUINo [0])
			,luiNode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUIMoveSource 2
			] noChanges noEffects
		,'DM'.fromList [(2,(ESApplied (LUINo [0]),luiNode UIStep 'DM'.newMap [] {noChanges & toBeReplaced = Just (luiNode UIEmpty 'DM'.newMap [] noChanges noEffects)} noEffects))])

		(applyUpstreamChange (ChangeUI [] [(1,ChangeChild (ReplaceUI (UI UIEmpty 'DM'.newMap [])))])
			(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUIMoveDestination 2 (LUINo [0])
				,luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUIMoveSource 2
				] noChanges noEffects
			,'DM'.fromList [(2,(ESApplied (LUINo [0]),luiNode UIStep 'DM'.newMap [] noChanges noEffects))]))

	,assertEqual "Child remove" 
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
			,luiNode UIStep 'DM'.newMap [] {noChanges & toBeRemoved = True} noEffects
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [] [(1,RemoveChild)]) (lui0,initLUIMoves))
	,assertEqual "Child remove (with additional node)" 
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
			,luiNode UIEmpty 'DM'.newMap [] noChanges {noEffects & additional = ESApplied (LUINo [0])}
			,luiNode UIStep 'DM'.newMap [] {noChanges & toBeRemoved = True} noEffects
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [] [(1,RemoveChild)]) (lui1,initLUIMoves))
	,assertEqual "Child insert" 
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
			,luiNode UIEmpty 'DM'.newMap [] {noChanges & toBeInserted = True} noEffects
			,luiNode UIStep 'DM'.newMap [] noChanges noEffects
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [] [(1,InsertChild (UI UIEmpty 'DM'.newMap []))]) (lui0,initLUIMoves))
	,assertEqual "Child insert at last position" 
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
			,luiNode UIStep 'DM'.newMap [] noChanges noEffects
			,luiNode UIEmpty 'DM'.newMap [] {noChanges & toBeInserted = True} noEffects
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [] [(2,InsertChild (UI UIEmpty 'DM'.newMap []))]) (lui0,initLUIMoves))
	,assertEqual "Child insert (with additional node)" 
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
			,luiNode UIEmpty 'DM'.newMap [] noChanges {noEffects & additional = ESApplied (LUINo [0])}
			,luiNode UIEmpty 'DM'.newMap [] {noChanges & toBeInserted = True} noEffects
			,luiNode UIStep 'DM'.newMap [] noChanges noEffects
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [] [(1,InsertChild (UI UIEmpty 'DM'.newMap []))]) (lui1,initLUIMoves))
	,assertEqual "Child replace a newly inserted child" 
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
			,luiNode UIEmpty 'DM'.newMap [] {noChanges & toBeInserted = True} noEffects
			,luiNode UIStep 'DM'.newMap [] noChanges noEffects
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [] [(1,InsertChild (UI UIInteract 'DM'.newMap [])),(1,ChangeChild (ReplaceUI (UI UIEmpty 'DM'.newMap [])))]) (lui0,initLUIMoves))
	,assertEqual "Child shift down" 
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[luiNode UIInteract 'DM'.newMap [] {noChanges & toBeShifted = Just 0} noEffects
			,luiNode UIStep 'DM'.newMap [] noChanges noEffects
			,LUIShiftDestination 0
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [] [(0,MoveChild 1)]) (lui0,initLUIMoves))
	,assertEqual "Child shift in two steps" 
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[luiNode UIInteract 'DM'.newMap [] {noChanges & toBeShifted = Just 0} noEffects
			,luiNode UIStep 'DM'.newMap [] noChanges noEffects
			,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
			,luiNode UIRecord 'DM'.newMap [] noChanges noEffects
			,LUIShiftDestination 0
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [] [(0,MoveChild 2),(2,MoveChild 3)]) (lui00,initLUIMoves))
	,assertEqual "Child shift to original position in multiple steps" 
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
			,luiNode UIStep 'DM'.newMap [] noChanges noEffects
			,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
			,luiNode UIRecord 'DM'.newMap [] noChanges noEffects
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [] [(1,MoveChild 2),(2,MoveChild 3),(3,MoveChild 1)]) (lui00,initLUIMoves))
	,assertEqual "Root set attribute"
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")])
			[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
			,luiNode UIStep 'DM'.newMap [] noChanges noEffects
			] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "changed-title")]} noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [SetAttribute "title" (JSONString "changed-title")] []) (lui0,initLUIMoves))
	,assertEqual "Root delete attribute"
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
			,luiNode UIStep 'DM'.newMap [] noChanges noEffects
			] {noChanges & delAttributes = 'DS'.fromList ["title"]} noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [DelAttribute "title"] []) (lui0,initLUIMoves))
	,assertEqual "Set attribute after shift"
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[luiNode UIInteract 'DM'.newMap [] {noChanges & toBeShifted = Just 0, setAttributes = 'DM'.fromList [("title",JSONString "changed-title")]} noEffects
			,luiNode UIStep 'DM'.newMap [] noChanges noEffects
			,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
			,LUIShiftDestination 0
			,luiNode UIRecord 'DM'.newMap [] noChanges noEffects
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [] [(0,MoveChild 2)
		                                  ,(2,ChangeChild (ChangeUI [SetAttribute "title" (JSONString "changed-title")] []))
										  ]) (lui00,initLUIMoves))
	,assertEqual "Remove child after shift"
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[luiNode UIInteract 'DM'.newMap [] {noChanges & toBeRemoved = True } noEffects
			,luiNode UIStep 'DM'.newMap [] noChanges noEffects
			,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
			,luiNode UIRecord 'DM'.newMap [] noChanges noEffects
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [] [(0,MoveChild 2)
		                                  ,(2,RemoveChild)
										  ]) (lui00,initLUIMoves))
	,assertEqual "Set attribute on a wrapped child"
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")])
			[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
			,luiNode UIStep 'DM'.newMap 
				[luiNode UIDebug 'DM'.newMap [] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "changed-title")]} noEffects
				] noChanges {noEffects & wrapper = ESApplied (LUINo [0])}
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [] [(1,ChangeChild (ChangeUI [SetAttribute "title" (JSONString "changed-title")] []))]) 
			(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap 
					[luiNode UIDebug 'DM'.newMap [] noChanges noEffects
					] noChanges {noEffects & wrapper = ESApplied (LUINo [0])}
				] noChanges noEffects, initLUIMoves)
		)
	,assertEqual "Set attribute on an unwrapped child"
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")])
			[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
			,luiNode UIStep 'DM'.newMap 
				[luiNode UIDebug 'DM'.newMap [] noChanges  noEffects
				] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "changed-title")]} {noEffects & unwrapped = ESApplied (LUINo [0])}
			] noChanges noEffects
		,initLUIMoves)
		(applyUpstreamChange (ChangeUI [] [(1,ChangeChild (ChangeUI [SetAttribute "title" (JSONString "changed-title")] []))]) 
			(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap 
					[luiNode UIDebug 'DM'.newMap [] noChanges noEffects
					] noChanges {noEffects & unwrapped = ESApplied (LUINo [0])}
				] noChanges noEffects, initLUIMoves)
		)
	,assertEqual "Set attribute in moved child"
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")])
			[LUIMoveSource 1
			,luiNode UIStep 'DM'.newMap 
				[luiNode UIDebug 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects 
			,LUIMoveDestination 1 (LUINo [0])
			] noChanges noEffects
		,'DM'.fromList [(1,(ESApplied (LUINo [0]),luiNode UIInteract 'DM'.newMap [] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "changed-title")]} noEffects))])
		(applyUpstreamChange (ChangeUI [] [(0,ChangeChild (ChangeUI [SetAttribute "title" (JSONString "changed-title")] []))]) 
			(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUIMoveSource 1
				,luiNode UIStep 'DM'.newMap 
					[luiNode UIDebug 'DM'.newMap [] noChanges noEffects
					] noChanges noEffects
				,LUIMoveDestination 1 (LUINo [0])
				] noChanges noEffects
			,'DM'.fromList [(1,(ESApplied (LUINo [0]),luiNode UIInteract 'DM'.newMap [] noChanges noEffects))])
		)
	,assertEqual "Set attribute after removed child"
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIEmpty 'DM'.newMap [] noChanges {noEffects & hidden = ESApplied (LUINo [0])}
				,luiNode UIStep 'DM'.newMap 
					[luiNode UIDebug 'DM'.newMap [] noChanges noEffects
					] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "changed-title")]} noEffects
				] noChanges noEffects
		,'DM'.newMap)
		(applyUpstreamChange (ChangeUI [] [(1,ChangeChild (ChangeUI [SetAttribute "title" (JSONString "changed-title")] []))]) 
			(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIEmpty 'DM'.newMap [] noChanges {noEffects & hidden = ESApplied (LUINo [0])}
				,luiNode UIStep 'DM'.newMap 
					[luiNode UIDebug 'DM'.newMap [] noChanges noEffects
					] noChanges noEffects
				] noChanges noEffects
			,'DM'.newMap)
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
	,extractDownstreamChangeTest_UpdatedMovedChild
	,extractDownstreamChangeTest_NewMovedChildren
	,extractDownstreamChangeTest_NoLongerMovedChild
	,extractDownstreamChangeTest_ChangeInChildrenWithMoves
	,extractDownstreamChangeTest_NewWrappedChild
	,extractDownstreamChangeTest_NoLongerWrappedChild
	,extractDownstreamChangeTest_ChangingAWrappedAttribute
	,extractDownstreamChangeTest_InsertedWrappedChild
	,extractDownstreamChangeTest_NewUnwrappedChild
	,extractDownstreamChangeTest_NoLongerUnwrappedChild
	,extractDownstreamChangeTest_ChangingAnUnwrappedAttribute
	,extractDownstreamChangeTest_ChangingAnUnwrappedAttributeNotZero 
	,extractDownstreamChangeTest_ChangingAfterUnwrappedHiddenAttribute
	,extractDownstreamChangeTest_InsertIntoUnwrappedContainer
	,extractDownstreamChangeTest_RemovingUnwrappedContainer
	,extractDownstreamChangeTest_ShiftingInUnwrappedContainer
	,extractDownstreamChangeTest_MovingIntoAdditionalContainer
	,extractDownstreamChangeTest_MovingIntoInsertedContainer
	,extractDownstreamChangeTest_MovingIntoMovedContainer
	]

extractDownstreamChangeTest_TopLevelReplace =
	assertEqual "Simple top-level replace without effects" 
		(ReplaceUI (UI UIEmpty 'DM'.newMap [])
		,(luiNode UIEmpty 'DM'.newMap [] noChanges noEffects
		 ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & toBeReplaced = Just (luiNode UIEmpty 'DM'.newMap [] noChanges noEffects)} noEffects
		,initLUIMoves))

extractDownstreamChangeTest_TopLevelReplaceWithReplacedChild =
	assertEqual "Top-level replace with a replaced child without effects" 
		(ReplaceUI (UI UIParallel 'DM'.newMap [])
		,(luiNode UIParallel 'DM'.newMap [] noChanges noEffects
		 ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & toBeReplaced = Just (luiNode UIEmpty 'DM'.newMap []
					{noChanges & toBeReplaced = Just (luiNode UIParallel 'DM'.newMap [] noChanges noEffects)} noEffects)
				  } noEffects
		,initLUIMoves))

extractDownstreamChangeTest_TopLevelReplaceWithMovedChild =
	assertEqual "Top-level replace with a removed child without effects" 
		(ReplaceUI (UI UIParallel 'DM'.newMap [UI UIRecord 'DM'.newMap []])
		,(luiNode UIParallel 'DM'.newMap
			[luiNode UIRecord 'DM'.newMap [] noChanges noEffects
			] noChanges noEffects
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & toBeReplaced = Just (luiNode UIEmpty 'DM'.newMap []
					{noChanges & toBeReplaced = Just (luiNode UIParallel 'DM'.newMap
						[luiNode UIDebug 'DM'.newMap [] {noChanges & toBeRemoved = True} noEffects
						,luiNode UIRecord 'DM'.newMap [] noChanges noEffects
						] noChanges noEffects)} noEffects)
				  } noEffects
		,initLUIMoves))

extractDownstreamChangeTest_TopLevelReplaceWithShiftedChildren =
	assertEqual "Top-level replace with shifted children without effects" 
		(ReplaceUI (UI UIParallel 'DM'.newMap [UI UIRecord 'DM'.newMap [],UI UIContainer 'DM'.newMap [], UI UIPanel 'DM'.newMap [], UI UIDebug 'DM'.newMap []])
		,(luiNode UIParallel 'DM'.newMap
			[luiNode UIRecord 'DM'.newMap [] noChanges noEffects
			,luiNode UIContainer 'DM'.newMap [] noChanges noEffects
			,luiNode UIPanel 'DM'.newMap [] noChanges noEffects
			,luiNode UIDebug 'DM'.newMap [] noChanges noEffects
			] noChanges noEffects
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & toBeReplaced = Just (luiNode UIEmpty 'DM'.newMap []
					{noChanges & toBeReplaced = Just (luiNode UIParallel 'DM'.newMap
						[luiNode UIDebug 'DM'.newMap [] {noChanges & toBeShifted = Just 0} noEffects
						,luiNode UIRecord 'DM'.newMap [] noChanges noEffects
						,LUIShiftDestination 1
						,luiNode UIPanel 'DM'.newMap [] noChanges noEffects
						,luiNode UIContainer 'DM'.newMap [] {noChanges & toBeShifted = Just 1} noEffects
						,LUIShiftDestination 0
						] noChanges noEffects)} noEffects)
				  } noEffects
		,initLUIMoves))

extractDownstreamChangeTest_TopLevelReplaceWithChangedAttribute =
	assertEqual "Top-level replace with a changed attribute child without effects" 
		(ReplaceUI (UI UIParallel 'DM'.newMap [UI UIRecord 'DM'.newMap [], UI UIInteract ('DM'.fromList [("attr",JSONString "B")]) []])
		,(luiNode UIParallel 'DM'.newMap
			[luiNode UIRecord 'DM'.newMap [] noChanges noEffects
			,luiNode UIInteract ('DM'.fromList [("attr",JSONString "B")]) [] noChanges noEffects
			] noChanges noEffects
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & toBeReplaced = Just (luiNode UIParallel 'DM'.newMap 
						[luiNode UIRecord 'DM'.newMap [] noChanges noEffects
						,luiNode UIInteract ('DM'.fromList [("attr",JSONString "A")]) [] {noChanges & setAttributes ='DM'.fromList [("attr",JSONString "B")] } noEffects
						] noChanges noEffects)
				  } noEffects
		,initLUIMoves))

extractDownstreamChangeTest_TopLevelReplaceWithOverwrittenType =
	assertEqual "Top-level replace with an overwritten type" 
		(ReplaceUI (UI UIParallel 'DM'.newMap [])
		,(luiNode UIEmpty 'DM'.newMap
			[] noChanges {noEffects & overwrittenType = ESApplied (LUINo [2],UIParallel)}
		 ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & toBeReplaced = Just (luiNode UIEmpty 'DM'.newMap []
					noChanges {noEffects & overwrittenType = ESToBeApplied (LUINo [2],UIParallel)})
				  } noEffects

		,initLUIMoves))

extractDownstreamChangeTest_TopLevelReplaceWithOverwrittenAttribute =
	assertEqual "Top-level replace with an overwritten attribute" 
		(ReplaceUI (UI UIEmpty ('DM'.fromList [("title",JSONString "B")]) [])
		,(luiNode UIEmpty ('DM'.fromList [("title",JSONString "A")])
			[] noChanges {noEffects & overwrittenAttributes = 'DM'.fromList [("title",ESApplied (LUINo [0],JSONString "B"))]}
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & toBeReplaced = Just (luiNode UIEmpty ('DM'.fromList [("title",JSONString "A")]) []
					noChanges {noEffects & overwrittenAttributes = 'DM'.fromList [("title",ESToBeApplied (LUINo [0],JSONString "B"))]})
				  } noEffects
		,initLUIMoves))

extractDownstreamChangeTest_TopLevelReplaceWithHiddenAttribute =
	assertEqual "Top-level replace with a hidden attribute" 
		(ReplaceUI (UI UIEmpty 'DM'.newMap [])
		,(luiNode UIEmpty ('DM'.fromList [("title",JSONString "A")])
			[] noChanges {noEffects & hiddenAttributes = 'DM'.fromList [("title",ESApplied (LUINo [0]))]}
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & toBeReplaced = Just (luiNode UIEmpty ('DM'.fromList [("title",JSONString "A")]) []
					noChanges {noEffects & hiddenAttributes = 'DM'.fromList [("title",ESToBeApplied (LUINo [0]))]})
				  } noEffects
		,initLUIMoves))

extractDownstreamChangeTest_TopLevelReplaceWithInsertedChild =
	assertEqual "Top-level replace with an inserted child " 
		(ReplaceUI (UI UIEmpty 'DM'.newMap [UI UIPanel 'DM'.newMap []])
		,(luiNode UIEmpty 'DM'.newMap
			[luiNode UIPanel 'DM'.newMap [] noChanges {noEffects & additional = ESApplied (LUINo [0])}
			] noChanges noEffects
		 ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & toBeReplaced = Just 
						(luiNode UIEmpty ('DM'.newMap)
							[ luiNode UIPanel 'DM'.newMap [] noChanges {noEffects & additional = ESToBeApplied (LUINo [0])}
							] noChanges noEffects
						)
				  } noEffects
		,initLUIMoves))

extractDownstreamChangeTest_TopLevelOverwrittenType =
	assertEqual "Top-level overwritten type"
		(ReplaceUI (UI UIContainer 'DM'.newMap
			[UI UIInteract 'DM'.newMap []
			])
		,(luiNode UIStep 'DM'.newMap
			[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
			] noChanges {noEffects & overwrittenType = ESApplied (LUINo [3],UIContainer)}
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIStep 'DM'.newMap
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				] noChanges {noEffects & overwrittenType = ESToBeApplied (LUINo [3],UIContainer)}
		,initLUIMoves))

extractDownstreamChangeTest_RemovedChild =
	assertEqual "Removed child without effects" 
		(ChangeUI [] [(1,RemoveChild)]
		,(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] {noChanges & toBeRemoved = True} noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_InsertedChild =
	assertEqual "Inserted child without effects" 
		(ChangeUI [] [(1,InsertChild (UI UIStep 'DM'.newMap []))]
		,(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] {noChanges & toBeInserted = True} noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_ShiftedChildren =
	assertEqual "Shifted children without effects" 
		(ChangeUI [] [(2,MoveChild 4),(3,MoveChild 2)]
		,(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				,luiNode UIDebug 'DM'.newMap [] noChanges noEffects
				,luiNode UIRecord 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] {noChanges & toBeShifted = Just 0} noEffects
				,LUIShiftDestination 1
				,luiNode UIRecord 'DM'.newMap [] noChanges noEffects
				,luiNode UIDebug 'DM'.newMap [] {noChanges & toBeShifted = Just 1} noEffects
				,LUIShiftDestination 0
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_SetChildAttribute =
	assertEqual "Set attribute in child without effects" 
		(ChangeUI [] [(2,ChangeChild (ChangeUI [SetAttribute "title" (JSONString "New attribute")] []))]
		,(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel ('DM'.fromList [("title",JSONString "New attribute")]) [] noChanges noEffects
				] noChanges noEffects
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "New attribute")]} noEffects
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_DeleteChildAttribute =
	assertEqual "Delete attribute in child without effects" 
		(ChangeUI [] [(2,ChangeChild (ChangeUI [DelAttribute "title"] []))]
		,(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel ('DM'.fromList [("title",JSONString "Old attribute")]) [] {noChanges & delAttributes = 'DS'.fromList ["title"]} noEffects
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_OverwrittenChildAttribute =
	assertEqual "Overwritten attribute in child" 
		(ChangeUI [] [(2,ChangeChild (ChangeUI [SetAttribute "title" (JSONString "B")] []))]
		,(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel ('DM'.fromList [("title",JSONString "A")]) [] noChanges
					{noEffects & overwrittenAttributes = 'DM'.fromList [("title",ESApplied (LUINo [0],JSONString "B"))]}
				] noChanges noEffects
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel ('DM'.fromList [("title",JSONString "A")]) [] noChanges
					{noEffects & overwrittenAttributes = 'DM'.fromList [("title",ESToBeApplied (LUINo [0],JSONString "B"))]}
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_NewAdditionalChild =
	assertEqual "New additional child" 
		(ChangeUI [] [(2,InsertChild (UI UIParallel 'DM'.newMap []))]
		 ,(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges {noEffects & additional = ESApplied (LUINo [0])}
				] noChanges noEffects
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges {noEffects & additional = ESToBeApplied (LUINo [0])}
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_RemovedAdditionalChild =
	assertEqual "Removed additional child" 
		(ChangeUI [] [(2,RemoveChild)]
		 ,(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges {noEffects & additional = ESToBeRemoved (LUINo [0])}
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_NewHiddenChild =
	assertEqual "New hidden child" 
		(ChangeUI [] [(2,RemoveChild)]
		 ,(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges {noEffects & hidden = ESApplied (LUINo [0])}
				] noChanges noEffects
		  ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges {noEffects & hidden = ESToBeApplied (LUINo [0])}
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_RemovedHiddenChild =
	assertEqual "Removed hidden child" 
		(ChangeUI [] [(2,InsertChild (UI UIParallel 'DM'.newMap []))]
		 ,(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		 ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges {noEffects & hidden = ESToBeRemoved (LUINo [0])}
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_NewMovedChild =
	assertEqual "New moved child" 
		(ChangeUI [] [(1,ChangeChild (ChangeUI [] [(0,InsertChild (UI UIParallel 'DM'.newMap []))])),(2,RemoveChild)]
		 ,(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [
						LUIMoveDestination 1 (LUINo [0])
					] noChanges noEffects
				,LUIMoveSource 1
				] noChanges noEffects 
		  ,'DM'.fromList [(1,(ESApplied (LUINo [0]),luiNode UIParallel 'DM'.newMap [] noChanges noEffects))])
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [
						LUIMoveDestination 1 (LUINo [0])
					] noChanges noEffects
				,LUIMoveSource 1
				] noChanges noEffects 
		,'DM'.fromList [(1,(ESToBeApplied (LUINo [0]),luiNode UIParallel 'DM'.newMap [] noChanges noEffects))]))

extractDownstreamChangeTest_UpdatedMovedChild =
	assertEqual "Updated moved child" 
		(ChangeUI [] [(1,InsertChild (UI UIParallel 'DM'.newMap [])), (2,ChangeChild (ChangeUI [] [(0,RemoveChild)]))]
		 ,(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUIMoveDestination 1 (LUINo [0])
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				,LUIMoveSource 1
				] noChanges noEffects 
		  ,'DM'.fromList [(1,(ESApplied (LUINo [0]),luiNode UIParallel 'DM'.newMap [] noChanges noEffects))])
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUIMoveDestination 1 (LUINo [0])
				,luiNode UIStep 'DM'.newMap [
						LUIMoveDestination 1 (LUINo [1])
					] noChanges noEffects
				,LUIMoveSource 1
				] noChanges noEffects 
		,'DM'.fromList [(1,(ESToBeUpdated (LUINo [1]) (LUINo [0]), luiNode UIParallel 'DM'.newMap [] noChanges noEffects))]))

extractDownstreamChangeTest_NewMovedChildren =
	assertEqual "New moved children" 
		(ChangeUI [] [(0,RemoveChild),(0,ChangeChild (ChangeUI [] [(0,InsertChild (UI UIInteract 'DM'.newMap [])),(1,InsertChild (UI UIParallel 'DM'.newMap []))])),(1,RemoveChild)]
		 ,(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUIMoveSource 1
				,luiNode UIStep 'DM'.newMap [
						 LUIMoveDestination 1 (LUINo [0])
						,LUIMoveDestination 2 (LUINo [0])
					] noChanges noEffects
				,LUIMoveSource 2
				] noChanges noEffects
		  ,'DM'.fromList 
				[(1,(ESApplied (LUINo [0]), luiNode UIInteract 'DM'.newMap [] noChanges noEffects))
				,(2,(ESApplied (LUINo [0]), luiNode UIParallel 'DM'.newMap [] noChanges noEffects))
				])
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUIMoveSource 1
				,luiNode UIStep 'DM'.newMap [
						LUIMoveDestination 1 (LUINo [0])
						,LUIMoveDestination 2 (LUINo [0])
					] noChanges noEffects
				,LUIMoveSource 2
				] noChanges noEffects
		,'DM'.fromList 
			[(1,(ESToBeApplied (LUINo [0]), luiNode UIInteract 'DM'.newMap [] noChanges noEffects))
			,(2,(ESToBeApplied (LUINo [0]), luiNode UIParallel 'DM'.newMap [] noChanges noEffects))
			]))

extractDownstreamChangeTest_NoLongerMovedChild =
	assertEqual "No longer moved child" 
		(ChangeUI [] [(1,ChangeChild (ChangeUI [] [(0,RemoveChild)])),(2,InsertChild (UI UIParallel 'DM'.newMap []))]
		 ,(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
	 	 ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [
						LUIMoveDestination 1 (LUINo [0])
					] noChanges noEffects
				,LUIMoveSource 1
				] noChanges noEffects
			,'DM'.fromList [(1,(ESToBeRemoved (LUINo [0]), luiNode UIParallel 'DM'.newMap [] noChanges noEffects))]
		))

extractDownstreamChangeTest_ChangeInChildrenWithMoves =
	assertEqual "Change in set of children with moved item" 
		(ChangeUI [] [(1,ChangeChild (ChangeUI [SetAttribute "title" (JSONString "test")] []))]
		 ,(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUIMoveSource 1
				,luiNode UIStep 'DM'.newMap [LUIMoveDestination 1 (LUINo [0])] noChanges noEffects
				,luiNode UIParallel ('DM'.fromList [("title",JSONString "test")]) [] noChanges noEffects
				] noChanges noEffects
		  ,'DM'.fromList [(1,(ESApplied (LUINo [0]),luiNode UIInteract 'DM'.newMap [] noChanges noEffects))])
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUIMoveSource 1
				,luiNode UIStep 'DM'.newMap [LUIMoveDestination 1 (LUINo [0])] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "test")] } noEffects
				] noChanges noEffects
		,'DM'.fromList [(1,(ESApplied (LUINo [0]), luiNode UIInteract 'DM'.newMap [] noChanges noEffects))]))

extractDownstreamChangeTest_NewWrappedChild =
	assertEqual "New wrapped child" 
		(ChangeUI [] [(1,ChangeChild (ReplaceUI (UI UIStep 'DM'.newMap [UI UIDebug 'DM'.newMap []])))]
			,(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap
					[ luiNode UIDebug 'DM'.newMap [] noChanges noEffects
					] noChanges {noEffects & wrapper = ESApplied (LUINo [0])}
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			 ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap
					[ luiNode UIDebug 'DM'.newMap [] noChanges noEffects
					] noChanges {noEffects & wrapper = ESToBeApplied (LUINo [0])}
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_InsertedWrappedChild =
	assertEqual "Inserted and directly wrapped child"
		(ChangeUI [] [(1,InsertChild (UI UIStep 'DM'.newMap [UI UIDebug 'DM'.newMap []]))]
			,(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")])
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap
					[ luiNode UIDebug 'DM'.newMap [] noChanges noEffects
					] noChanges {noEffects & wrapper = ESApplied (LUINo [0])}
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			 ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")])
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap
					[ luiNode UIDebug 'DM'.newMap [] {noChanges & toBeInserted = True} noEffects
					] noChanges {noEffects & wrapper = ESToBeApplied (LUINo [0])}
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_NoLongerWrappedChild =
	assertEqual "No longer wrapped child" 
		(ChangeUI [] [(1,ChangeChild (ReplaceUI (UI UIDebug 'DM'.newMap [])))]
			,(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIDebug 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			 ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap
					[luiNode UIInteract 'DM'.newMap [] noChanges {noEffects & additional = ESApplied (LUINo [1])} //Added after the wrapping, should be removed too
					,luiNode UIDebug 'DM'.newMap [] noChanges noEffects
					] noChanges {noEffects & wrapper = ESToBeRemoved (LUINo [0])}
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_ChangingAWrappedAttribute =
	assertEqual "Changing an attribute of a wrapped child" 
		(ChangeUI [] [(1,ChangeChild (ChangeUI [] [(0,ChangeChild (ChangeUI [SetAttribute "value" (JSONInt 42)] []))]))]
			,(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap
					[luiNode UIInteract ('DM'.fromList [("value",JSONInt 42)]) [] noChanges noEffects 
					] noChanges {noEffects & wrapper = ESApplied (LUINo [0])}
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			 ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap
					[luiNode UIInteract 'DM'.newMap [] 
						{noChanges & setAttributes = 'DM'.fromList [("value",JSONInt 42)]}
						 noEffects 
					] noChanges {noEffects & wrapper = ESApplied (LUINo [0])}
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_NewUnwrappedChild =
	assertEqual "New unwrapped child" 
		(ChangeUI [] [(1,ChangeChild (ReplaceUI (UI UIDebug 'DM'.newMap [])))]
			,(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap
					[ luiNode UIDebug 'DM'.newMap [] noChanges noEffects
					] noChanges {noEffects & unwrapped = ESApplied (LUINo [0])}
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			 ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap
					[ luiNode UIDebug 'DM'.newMap [] noChanges noEffects
					] noChanges {noEffects & unwrapped = ESToBeApplied (LUINo [0])}
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_NoLongerUnwrappedChild =
	assertEqual "No longer unwrapped child" 
		(ChangeUI [] [(1,ChangeChild (ReplaceUI (UI UIStep 'DM'.newMap [UI UIDebug 'DM'.newMap []])))]
			,(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap
					[ luiNode UIDebug 'DM'.newMap [] noChanges noEffects
					] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			 ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap
					[ luiNode UIDebug 'DM'.newMap [] noChanges noEffects
					] noChanges {noEffects & unwrapped = ESToBeRemoved (LUINo [0])}
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_ChangingAnUnwrappedAttribute =
	assertEqual "Changing an attribute on an unwrapped child" 
		(ChangeUI [] [(1,ChangeChild (ChangeUI [SetAttribute "title" (JSONString "test")] []))]
			,(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap
					[ luiNode UIDebug ('DM'.fromList [("title",JSONString "test")]) [] noChanges noEffects
					] noChanges {noEffects & unwrapped = ESApplied (LUINo [0])}
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			 ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap
					[ luiNode UIDebug 'DM'.newMap [] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "test")]} noEffects
					] noChanges {noEffects & unwrapped = ESApplied (LUINo [0])}
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_ChangingAnUnwrappedAttributeNotZero =
	assertEqual "Changing an attribute on an unwrapped child that is not the first child" 
		(ChangeUI [] [(1,ChangeChild (ChangeUI [SetAttribute "title" (JSONString "test")] []))]
			,(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap
					[luiNode UIInteract 'DM'.newMap [] noChanges {noEffects & hidden = ESApplied (LUINo [0])}
					,luiNode UIDebug ('DM'.fromList [("title",JSONString "test")]) [] noChanges noEffects
					] noChanges {noEffects & unwrapped = ESApplied (LUINo [1])}
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			 ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap
					[luiNode UIInteract 'DM'.newMap [] noChanges {noEffects & hidden = ESApplied (LUINo [0])}
 					,luiNode UIDebug 'DM'.newMap [] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "test")]} noEffects
					] noChanges {noEffects & unwrapped = ESApplied (LUINo [1])}
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_ChangingAfterUnwrappedHiddenAttribute =
	assertEqual "Changing an attribute after an unwrapped hidden child" 
		(ChangeUI [] [(1,ChangeChild (ChangeUI [SetAttribute "title" (JSONString "test")] []))]
			,(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap
					[ luiNode UIDebug 'DM'.newMap [] noChanges {noEffects & hidden = ESApplied (LUINo [1])}
					] noChanges {noEffects & unwrapped = ESApplied (LUINo [0])}
				,luiNode UIParallel ('DM'.fromList [("title",JSONString "test")]) [] noChanges noEffects
				] noChanges noEffects
			 ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap
					[ luiNode UIDebug 'DM'.newMap [] noChanges {noEffects & hidden = ESApplied (LUINo [1])}
					] noChanges {noEffects & unwrapped = ESApplied (LUINo [0])}
				,luiNode UIParallel 'DM'.newMap [] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "test")]} noEffects
				] noChanges noEffects
		,initLUIMoves))

extractDownstreamChangeTest_InsertIntoUnwrappedContainer =
	assertEqual "Inserting into an unwrapped container" 
		(ReplaceUI (UI UIDebug 'DM'.newMap [])
			,(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIDebug 'DM'.newMap [] noChanges noEffects
				,luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges {noEffects & unwrapped = ESApplied (LUINo [0])}
			 ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIDebug 'DM'.newMap [] {noChanges & toBeInserted=True} noEffects
				,luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges {noEffects & unwrapped = ESApplied (LUINo [0])}
		,initLUIMoves))

extractDownstreamChangeTest_RemovingUnwrappedContainer =
	assertEqual "Removing from an unwrapped container" 
		(ReplaceUI (UI UIStep 'DM'.newMap [])
			,(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIStep 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges {noEffects & unwrapped = ESApplied (LUINo [0])}
			 ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] {noChanges & toBeRemoved = True} noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges {noEffects & unwrapped = ESApplied (LUINo [0])}
		,initLUIMoves))

extractDownstreamChangeTest_ShiftingInUnwrappedContainer =
	assertEqual "Shifting in an unwrapped container" 
		(ReplaceUI (UI UIStep 'DM'.newMap [])
			,(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIStep 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				,luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				] noChanges {noEffects & unwrapped = ESApplied (LUINo [0])}
			 ,initLUIMoves)
		)
		(extractDownstreamChange (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] {noChanges & toBeShifted = Just 1} noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				,LUIShiftDestination 1
				] noChanges {noEffects & unwrapped = ESApplied (LUINo [0])}
		,initLUIMoves))

extractDownstreamChangeTest_MovingIntoAdditionalContainer =
	assertEqual "Moving items to a newly inserted container" 
	(ChangeUI [] [(1,InsertChild (UI UIParallel 'DM'.newMap [UI UIStep 'DM'.newMap []])), (2,RemoveChild)]
		,(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [
					LUIMoveDestination 1 (LUINo [3])
					] noChanges {noEffects & additional = ESApplied (LUINo [1])}
				,LUIMoveSource 1 
				] noChanges noEffects
		 ,'DM'.fromList [(1,(ESApplied (LUINo [3]), luiNode UIStep 'DM'.newMap [] noChanges noEffects))])
	)
	(extractDownstreamChange (
		luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [
					LUIMoveDestination 1 (LUINo [3])
					] noChanges {noEffects & additional = ESToBeApplied (LUINo [1])}
				,LUIMoveSource 1
				] noChanges noEffects
		,'DM'.fromList [(1,(ESToBeApplied (LUINo [3]), luiNode UIStep 'DM'.newMap [] noChanges noEffects))]))

extractDownstreamChangeTest_MovingIntoInsertedContainer =
	assertEqual "Moving items to an upstream inserted container" 
	(ChangeUI [] [(1,InsertChild (UI UIParallel 'DM'.newMap [UI UIStep 'DM'.newMap []])), (2,RemoveChild)]
		,(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [
					LUIMoveDestination 1 (LUINo [2])
					] noChanges noEffects
				,LUIMoveSource 1 
				] noChanges noEffects
		 ,'DM'.fromList [(1,(ESApplied (LUINo [2]), luiNode UIStep 'DM'.newMap [] noChanges noEffects))])
	)
	(extractDownstreamChange (
		luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [
					LUIMoveDestination 1 (LUINo [2])
					] {noChanges & toBeInserted = True} noEffects
				,LUIMoveSource 1
				] noChanges noEffects
		,'DM'.fromList [(1,(ESToBeApplied (LUINo [2]), luiNode UIStep 'DM'.newMap [] noChanges noEffects))]))

extractDownstreamChangeTest_MovingIntoMovedContainer =
	assertEqual "Moving items to into a container that was moved itself" 
		(ChangeUI [] [(1,InsertChild (UI UIParallel 'DM'.newMap [UI UIStep 'DM'.newMap [UI UIEmpty 'DM'.newMap []]])), (2,RemoveChild),(2,RemoveChild)],
		(luiNode UIPanel 'DM'.newMap 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [
					LUIMoveDestination 1 (LUINo [2])
					] noChanges noEffects
				,LUIMoveSource 1
				,LUIMoveSource 2
				] noChanges noEffects
		,'DM'.fromList 
			[(1,(ESApplied (LUINo [2]),luiNode UIStep 'DM'.newMap [
					LUIMoveDestination 2 (LUINo [3])
					] noChanges noEffects))
			,(2,(ESApplied (LUINo [3]),luiNode UIEmpty 'DM'.newMap [] noChanges noEffects))
			])
	)
	(extractDownstreamChange (
		luiNode UIPanel 'DM'.newMap 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [
					LUIMoveDestination 1 (LUINo [2])
					] {noChanges & toBeInserted = True} noEffects
				,LUIMoveSource 1
				,LUIMoveSource 2
				] noChanges noEffects
		,'DM'.fromList 
			[(1,(ESToBeApplied (LUINo [2]), luiNode UIStep 'DM'.newMap [
					LUIMoveDestination 2 (LUINo [3])
					] noChanges noEffects))
			,(2,(ESToBeApplied (LUINo [3]), luiNode UIEmpty 'DM'.newMap [] noChanges noEffects))
			]))

extractUIWithEffects_Tests =
	[assertEqual "Extract UI with newly moved items" 
		(Just (UI UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[UI UIStep 'DM'.newMap
				[UI UIInteract 'DM'.newMap []
				,UI UIParallel 'DM'.newMap []
				]
			])
		)
		(extractUIWithEffects_ (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUIMoveSource 1
				,luiNode UIStep 'DM'.newMap [LUIMoveDestination 1 (LUINo [0]),LUIMoveDestination 2 (LUINo [0])] noChanges noEffects
				,LUIMoveSource 2
				] noChanges noEffects
		,'DM'.fromList 
			[(1,(ESToBeApplied (LUINo [0]),luiNode UIInteract 'DM'.newMap [] noChanges noEffects))
			,(2,(ESApplied (LUINo [0]), luiNode UIParallel 'DM'.newMap [] noChanges noEffects))
			]))
	,assertEqual "Extract UI with updated moved items" 
		(Just (UI UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[UI UIParallel 'DM'.newMap []
			,UI UIStep 'DM'.newMap
				[UI UIInteract 'DM'.newMap []
				]
			])
		)
		(extractUIWithEffects_ (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUIMoveSource 1
				,LUIMoveDestination 2 (LUINo [1])
				,luiNode UIStep 'DM'.newMap [LUIMoveDestination 1 (LUINo [0]),LUIMoveDestination 2 (LUINo [0])] noChanges noEffects
				,LUIMoveSource 2
				] noChanges noEffects
		,'DM'.fromList 
			[(1,(ESApplied (LUINo [0]), luiNode UIInteract 'DM'.newMap [] noChanges noEffects))
			,(2,(ESToBeUpdated (LUINo [0]) (LUINo [1]), luiNode UIParallel 'DM'.newMap [] noChanges noEffects))
			]))
	,assertEqual "Extract UI with new wrapped"
		(Just (UI UIContainer 'DM'.newMap [UI UIStep 'DM'.newMap []])
		)
		(extractUIWithEffects_ (
			luiNode UIContainer 'DM'.newMap
			[luiNode UIStep 'DM'.newMap [] noChanges noEffects
			] noChanges {noEffects & wrapper = ESToBeApplied (LUINo [2])}
		,initLUIMoves))	
	]

updateChildNodes_Tests = 
	[ assertEqual "Updating selected child nodes"
		(
			[luiNode UIDebug 'DM'.newMap [] noChanges noEffects
			,luiNode UIStep 'DM'.newMap [] noChanges {noEffects & additional = ESApplied (LUINo [5])}
			,luiNode UIDebug 'DM'.newMap [] {noChanges & toBeShifted = Just 0} noEffects
			,LUIShiftDestination 1
			,luiNode UIDebug 'DM'.newMap [luiNode UIRecord 'DM'.newMap [] noChanges noEffects] noChanges {noEffects & wrapper = ESApplied (LUINo [7])}
			,luiNode UIDebug 'DM'.newMap [] {noChanges & toBeShifted = Just 1} noEffects
			,LUIShiftDestination 0
			]
		,initLUIMoves)
		(updateChildNodes_ (LUINo [4]) (\i (LUINode node, mv) -> (LUINode {LUINode|node & type = UIDebug} ,mv)) (
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects //Should be updated
				,luiNode UIStep 'DM'.newMap [] noChanges {noEffects & additional = ESApplied (LUINo [5])} //Should not be updated (node does not exist yet)
				,luiNode UIParallel 'DM'.newMap [] {noChanges & toBeShifted = Just 0} noEffects //Should be updated
				,LUIShiftDestination 1
				,luiNode UIDebug 'DM'.newMap [luiNode UIRecord 'DM'.newMap [] noChanges noEffects] noChanges {noEffects & wrapper = ESApplied (LUINo [7])} //Should be updated
				,luiNode UIDebug 'DM'.newMap [] {noChanges & toBeShifted = Just 1} noEffects //Should be updated
				,LUIShiftDestination 0
				],initLUIMoves))
	]

selectSubNode_Tests = 
	[ assertEqual "Selecting a shifted child node"
		(Just (luiNode UIDebug 'DM'.newMap [] {noChanges & toBeShifted = Just 1} noEffects))
		(selectSubNode_ (LUINo [0]) [2] (
			luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] {noChanges & toBeShifted = Just 0} noEffects
				,LUIShiftDestination 1
				,luiNode UIRecord 'DM'.newMap [] noChanges noEffects
				,luiNode UIDebug 'DM'.newMap [] {noChanges & toBeShifted = Just 1} noEffects
				,LUIShiftDestination 0
				] noChanges noEffects
		,initLUIMoves))
	]
updateSubNode_Tests = 
	[ assertEqual "Updating a shifted child node"
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] {noChanges & toBeShifted = Just 0} noEffects
				,LUIShiftDestination 1
				,luiNode UIRecord 'DM'.newMap [] noChanges noEffects
				,luiNode UIInteract 'DM'.newMap [] {noChanges & toBeShifted = Just 1} noEffects
				,LUIShiftDestination 0
				] noChanges noEffects
		,initLUIMoves)

		(updateSubNode_ (LUINo [0]) [2]
			(\(LUINode node, m) -> (LUINode {LUINode|node & type = UIInteract} ,m)) 
			(luiNode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] {noChanges & toBeShifted = Just 0} noEffects
				,LUIShiftDestination 1
				,luiNode UIRecord 'DM'.newMap [] noChanges noEffects
				,luiNode UIDebug 'DM'.newMap [] {noChanges & toBeShifted = Just 1} noEffects
				,LUIShiftDestination 0
				] noChanges noEffects
			,initLUIMoves))
	]

scanToPosition_Tests = 
	[assertEqual "Scanning to an added node"
		(3,True,Just (luiNode UIInteract 'DM'.newMap [] noChanges {noEffects & additional = ESToBeApplied (LUINo [5])}))
		(scanToPosition_ (LUINo [5]) 2
			[luiNode UIStep 'DM'.newMap [] noChanges noEffects
			,luiNode UIDebug 'DM'.newMap [] noChanges {noEffects & additional = ESToBeApplied (LUINo [6])} //Should be ignored
			,luiNode UIInteract 'DM'.newMap [] noChanges {noEffects & additional = ESToBeApplied (LUINo [1])} //Should be counted
			,luiNode UIInteract 'DM'.newMap [] noChanges {noEffects & additional = ESToBeApplied (LUINo [5])}
			,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
			]
			initLUIMoves
		)
	]
nodeExists_Tests =
	[assertEqual "Node exists: unwrapped hidden node"
		False
		(nodeExists_  (LUINo [2])
			(luiNode UIStep 'DM'.newMap [
				luiNode UIInteract 'DM'.newMap [] noChanges {noEffects & hidden = ESApplied (LUINo [1])}
			] noChanges {noEffects & unwrapped = ESApplied (LUINo [0])})
			'DM'.newMap
		)
	]

isPartOf_Tests = 
	[assertEqual "Is part of ruleNumbers: 1.1.3 1" True (isPartOf_ (LUINo [1,1,3]) (LUINo [1]))
	,assertEqual "Is part of ruleNumbers: 1 2" False (isPartOf_ (LUINo [1]) (LUINo [2]))
	,assertEqual "Is part of ruleNumbers: 1 2.4" False (isPartOf_ (LUINo [1]) (LUINo [2,4]))
	,assertEqual "Is part of ruleNumbers: 3 3" True (isPartOf_ (LUINo [3]) (LUINo [3]))
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
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "B")]}
					{noEffects
					& overwrittenAttributes = 'DM'.fromList [("hint",ESToBeApplied (LUINo [1],JSONString "BB"))]
					, hiddenAttributes = 'DM'.fromList [("title",ESToBeApplied (LUINo [1]))]
					}
		,initLUIMoves)
		(modifyUIAttributes
			(SelectKeys ["title"])
			(\attr -> 'DM'.fromList [("hint",(maybe (JSONString "C") (\(JSONString t) ->JSONString  (t+++t)) ('DM'.get "title" attr)))])
			(LUINo [1])
			(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "B")]} noEffects
			,initLUIMoves)
		)
	,assertEqual "Modify attributes rule: change title that was set by earlier rule" 
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges {noEffects & overwrittenAttributes = 'DM'.fromList [("title",ESToBeApplied (LUINo [2], JSONString "BB"))]}
		,initLUIMoves)
		(modifyUIAttributes
			(SelectKeys ["title"])
			(\attr -> 'DM'.fromList [("title",(maybe (JSONString "WRONG") (\(JSONString t) ->JSONString  (t+++t)) ('DM'.get "title" attr)))])
			(LUINo [2])
			(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges {noEffects & overwrittenAttributes = 'DM'.fromList [("title",ESToBeApplied (LUINo [1],JSONString "B"))]}
			,initLUIMoves)
		)
	]
copySubUIAttributesTests =
	[assertEqual "Copy sub attributes rule: copy title to attribute to a child"
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges {noEffects & overwrittenAttributes = 'DM'.fromList [("title",ESToBeApplied (LUINo [0],JSONString "B"))]}
				] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "B")]} noEffects
		,initLUIMoves)
		(copySubUIAttributes 
			(SelectKeys ["title"]) [] [1] (LUINo [0])
			(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "B")]} noEffects
			,initLUIMoves)
		)
	,assertEqual "Copy sub attributes rule: copy attribute between children to a child"
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges {noEffects & overwrittenAttributes = 'DM'.fromList [("title",ESToBeApplied (LUINo [0],JSONString "Test"))]}
				,luiNode UIStep ('DM'.fromList [("title",JSONString "Test")]) [] noChanges noEffects 
				] noChanges noEffects
		,initLUIMoves)
		(copySubUIAttributes 
			(SelectKeys ["title"]) [1] [0] (LUINo [0])
			(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep ('DM'.fromList [("title",JSONString "Test")]) [] noChanges noEffects 
				] noChanges noEffects
			,initLUIMoves)
		)

	]
insertChildUITests =
	[assertEqual "Insert a child rule: insert in known set"
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges {noEffects & additional = ESToBeApplied (LUINo [0])}
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		,initLUIMoves)
		(insertChildUI 1 (UI UIParallel 'DM'.newMap []) (LUINo [0])
			(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			,initLUIMoves)
		)
	]

removeSubUIsTests =
	[assertEqual "Remove children rule: Remove all UIStep nodes "
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges {noEffects & hidden = ESToBeApplied (LUINo [0])}
				] noChanges noEffects
		,initLUIMoves)
		(removeSubUIs (SelectByType UIStep) (LUINo [0])
			(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			,initLUIMoves)
		)
	]
moveSubUIsTests =
	[assertEqual "Move children rule: Move All UIStep nodes into the first parallel"
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [
					LUIMoveDestination 1 (LUINo [0])
					] noChanges noEffects
				,LUIMoveSource 1
				] noChanges noEffects
		,'DM'.fromList [(1,(ESToBeApplied (LUINo [0]), luiNode UIStep 'DM'.newMap [] noChanges noEffects))])
		(moveSubUIs (SelectByType UIStep) [1] 0 (LUINo [0])
			(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			,initLUIMoves)
		)
	,assertEqual "Move children rule: Move All UIStep nodes into an additional node"
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [
					LUIMoveDestination 1 (LUINo [3])
					] noChanges {noEffects & additional = ESToBeApplied (LUINo [1])}
				,LUIMoveSource 1
				] noChanges noEffects
		,'DM'.fromList [(1,(ESToBeApplied (LUINo [3]), luiNode UIStep 'DM'.newMap [] noChanges noEffects))])
		(moveSubUIs (SelectByType UIStep) [1] 0 (LUINo [3])
			(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects 
				,luiNode UIParallel 'DM'.newMap [] noChanges {noEffects & additional = ESToBeApplied (LUINo [1])}
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			,initLUIMoves)
		)
	,assertEqual "Move children rule: Move a node that was moved by an earlier rule"
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUIMoveDestination 1 (LUINo [0])
				,luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [
					LUIMoveDestination 1 (LUINo [1])
					] noChanges noEffects
				,LUIMoveSource 1
				] noChanges noEffects
		,'DM'.fromList [(1,(ESToBeUpdated (LUINo [0]) (LUINo [1]), luiNode UIStep 'DM'.newMap [] noChanges noEffects))])
		(moveSubUIs (SelectByType UIStep) [2] 0 (LUINo [1])
			(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUIMoveDestination 1 (LUINo [0])
				,luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				,LUIMoveSource 1
				] noChanges noEffects
			,'DM'.fromList [(1,(ESApplied (LUINo [0]), luiNode UIStep 'DM'.newMap [] noChanges noEffects))])
		)
	]
wrapUITests =
	[assertEqual "Wrap rule: wrap root as UIStep"
		(luiNode UIStep 'DM'.newMap [
			luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			] noChanges {noEffects & wrapper = ESToBeApplied (LUINo [0])}
		,initLUIMoves)
		(wrapUI UIStep (LUINo [0])
			(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			,initLUIMoves)
		)
	,assertEqual "Wrap rule: wrap root twice. First as UIStep, then as UIParallel"
		(luiNode UIParallel 'DM'.newMap [luiNode UIStep 'DM'.newMap [
			luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			] noChanges {noEffects & wrapper = ESToBeApplied (LUINo [0,0])}
		] noChanges {noEffects & wrapper = ESToBeApplied (LUINo [0,1])}
		,initLUIMoves)
		(sequenceLayouts [wrapUI UIStep ,wrapUI UIParallel] (LUINo [0])
			(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			,initLUIMoves)
		)
	,assertEqual "Wrap ui: check stability (no change)"
		(luiNode UIStep ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges {noEffects & wrapper = ESApplied (LUINo [0])} 
			,initLUIMoves)
		(wrapUI UIStep (LUINo [0])
			(luiNode UIStep ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges {noEffects & wrapper = ESApplied (LUINo [0])} 
			,initLUIMoves)
		)
	]

unwrapUITests =
	[assertEqual "Unwrap rule: unwrap root"
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges {noEffects & unwrapped = ESToBeApplied (LUINo [0])}
		,initLUIMoves)
		(unwrapUI (LUINo [0])
			(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			,initLUIMoves)
		)
	,assertEqual "Unwrap rule: unwrap root twice"
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [
					luiNode UIDebug 'DM'.newMap [] noChanges noEffects
					] noChanges {noEffects & unwrapped = ESToBeApplied (LUINo [0,1])}
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges {noEffects & unwrapped = ESToBeApplied (LUINo [0,0])}
		,initLUIMoves)
		(sequenceLayouts [unwrapUI,unwrapUI] (LUINo [0])
			(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [
					luiNode UIDebug 'DM'.newMap [] noChanges noEffects
					] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			,initLUIMoves)
		)
	]

layoutSubUIsTests =
	[assertEqual "Layout sub-uis rule: change type"
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges {noEffects & overwrittenType = ESToBeApplied (LUINo [0],UIDebug)}
				] noChanges noEffects
		,initLUIMoves)
		(layoutSubUIs (SelectByType UIStep) (setUIType UIDebug) (LUINo [0])
			(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIParallel 'DM'.newMap [] noChanges noEffects
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			,initLUIMoves)
		)
	,assertEqual "Layout sub-uis rule: change type of a wrapped item"
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIContainer 'DM'.newMap
					[luiNode UIParallel 'DM'.newMap [] noChanges {noEffects & overwrittenType = ESToBeApplied (LUINo [2],UIDebug)}]
					noChanges {noEffects & wrapper = ESToBeApplied (LUINo [1])}
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects 
				] noChanges noEffects
		,initLUIMoves)
		(layoutSubUIs (SelectByType UIParallel) (setUIType UIDebug) (LUINo [2])
			(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIContainer 'DM'.newMap
					[luiNode UIParallel 'DM'.newMap [] noChanges noEffects]
					noChanges {noEffects & wrapper = ESToBeApplied (LUINo [1])}
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			,initLUIMoves)
		)
		,assertEqual "Layout sub-uis rule: wrapping based on a predicate (already applied)"
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIContainer 'DM'.newMap
					[luiNode UIParallel 'DM'.newMap [] noChanges noEffects]
					noChanges {noEffects & wrapper = ESApplied (LUINo [2,0])}
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects 
				] noChanges noEffects
		,initLUIMoves)
		(layoutSubUIs (SelectByType UIParallel) (sequenceLayouts [wrapUI UIContainer]) (LUINo [2])
			(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIContainer 'DM'.newMap
					[luiNode UIParallel 'DM'.newMap [] noChanges noEffects]
					noChanges {noEffects & wrapper = ESApplied (LUINo [2,0])}
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects 
				] noChanges noEffects
			,initLUIMoves)
		)
		,assertEqual "Layout sub-uis rule: resetting when a selection no-longer holds"
		(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIContainer 'DM'.newMap []
					noChanges {noEffects & overwrittenAttributes = 'DM'.fromList [("help", ESToBeRemoved (LUINo [2,0],JSONString "Title"))]}
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects 
				] {noChanges & delAttributes = 'DS'.fromList ["title"]} noEffects
		,initLUIMoves)
		(layoutSubUIs (SelectByHasAttribute "title") (layoutSubUIs (SelectByPath [1]) (setUIAttributes ('DM'.fromList [("help",JSONString "Title")]))) (LUINo [2])
			(luiNode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[luiNode UIInteract 'DM'.newMap [] noChanges noEffects
				,luiNode UIContainer 'DM'.newMap [] noChanges
					{noEffects & overwrittenAttributes = 'DM'.fromList [("help",ESApplied (LUINo [2,0],JSONString "Title"))]}
				,luiNode UIStep 'DM'.newMap [] noChanges noEffects 
				] {noChanges & delAttributes = 'DS'.fromList ["title"]} noEffects
			,initLUIMoves)
		)
		,assertEqual "Layout sub-uis rule: stability of wrapped node (no change)"
			(luiNode UIDebug 'DM'.newMap 
					[luiNode UITextField 'DM'.newMap [] noChanges noEffects
					] noChanges {noEffects & wrapper = ESApplied (LUINo [0])}
			,initLUIMoves)
			(layoutSubUIs (SelectByType UITextField) (wrapUI UIDebug) (LUINo [0])
				(luiNode UIDebug 'DM'.newMap 
					[luiNode UITextField 'DM'.newMap [] noChanges noEffects
					] noChanges {noEffects & wrapper = ESApplied (LUINo [0])}
				,initLUIMoves)
			)	
	]
combinationTests =
	//Applying the first change
	[assertEqual "Combination rule: wrapping after unwrapping -> first rule"
		(luiNode UIStep 'DM'.newMap
			[luiNode UIDebug 'DM'.newMap [
					luiNode UIInteract 'DM'.newMap [
					 	 luiNode UIEmpty 'DM'.newMap [] noChanges noEffects
						,luiNode UITextField 'DM'.newMap [] noChanges noEffects
						] noChanges noEffects
				] noChanges {noEffects & wrapper = ESToBeApplied (LUINo [0,1])}
			] noChanges {noEffects & unwrapped = ESToBeApplied (LUINo [0,0])}
		,'DM'.newMap)
		((sequenceLayouts [unwrapUI,wrapUI UIDebug]) (LUINo [0])
			(luiNode UIStep 'DM'.newMap
				[luiNode UIInteract 'DM'.newMap [
					 luiNode UIEmpty 'DM'.newMap [] noChanges noEffects
					,luiNode UITextField 'DM'.newMap [] noChanges noEffects
					] noChanges noEffects
				] noChanges noEffects
			,'DM'.newMap)
		)
	//Extract the initial ui
	,assertEqual "Combination rule: wrapping after unwrapping -> initial ui"
		(Just (UI UIDebug 'DM'.newMap [UI UIInteract 'DM'.newMap []]))
		(extractUIWithEffects_
			(luiNode UIStep 'DM'.newMap
				[luiNode UIDebug 'DM'.newMap [
						luiNode UIInteract 'DM'.newMap [
						] noChanges noEffects
					] noChanges {noEffects & wrapper = ESToBeApplied (LUINo [0,1])}
				] noChanges {noEffects & unwrapped = ESToBeApplied (LUINo [0,0])}
			,'DM'.newMap)
		)
	//Apply a change 
	,assertEqual "Combination rule: wrapping after unwrapping -> new event"
		(luiNode UIStep 'DM'.newMap
			[luiNode UIDebug 'DM'.newMap [
					luiNode UIInteract 'DM'.newMap [] {noChanges & setAttributes = 'DM'.fromList [("test",JSONString "x")]} noEffects
				] noChanges {noEffects & wrapper = ESApplied (LUINo [0,1])}
			] noChanges {noEffects & unwrapped = ESApplied (LUINo [0,0])}
		,'DM'.newMap)
		(applyUpstreamChange (ChangeUI [] [(0,ChangeChild (ChangeUI [SetAttribute "test" (JSONString "x")] []))])
			(luiNode UIStep 'DM'.newMap
				[luiNode UIDebug 'DM'.newMap [
						luiNode UIInteract 'DM'.newMap [] {noChanges & setAttributes = 'DM'.fromList [("test",JSONString "x")]} noEffects
					] noChanges {noEffects & wrapper = ESApplied (LUINo [0,1])}
				] noChanges {noEffects & unwrapped = ESApplied (LUINo [0,0])}
			,'DM'.newMap)
		)
	//Reapply the rule
	,assertEqual "Combination rule: wrapping after unwrapping -> reapply rule"
		(luiNode UIStep 'DM'.newMap
			[luiNode UIDebug 'DM'.newMap [
					luiNode UIInteract 'DM'.newMap [] {noChanges & setAttributes = 'DM'.fromList [("test",JSONString "x")]} noEffects
				] noChanges {noEffects & wrapper = ESApplied (LUINo [0,1])}
			] noChanges {noEffects & unwrapped = ESApplied (LUINo [0,0])}
		,'DM'.newMap)
		((sequenceLayouts [unwrapUI,wrapUI UIDebug]) (LUINo [0])
			(luiNode UIStep 'DM'.newMap
			[luiNode UIDebug 'DM'.newMap [
					luiNode UIInteract 'DM'.newMap [] {noChanges & setAttributes = 'DM'.fromList [("test",JSONString "x")]} noEffects
				] noChanges {noEffects & wrapper = ESApplied (LUINo [0,1])}
			] noChanges {noEffects & unwrapped = ESApplied (LUINo [0,0])}
		,'DM'.newMap)
		)
	//Extract the downstream change
	,assertEqual "Combination rule: wrapping after unwrapping -> downstream"
		(ChangeUI [] [(0,ChangeChild (ChangeUI [SetAttribute "test" (JSONString "x")] []))]
		,(luiNode UIStep 'DM'.newMap
			[luiNode UIDebug 'DM'.newMap [
					luiNode UIInteract ('DM'.fromList [("test",JSONString "x")]) [] noChanges noEffects
				] noChanges {noEffects & wrapper = ESApplied (LUINo [0,1])}
			] noChanges {noEffects & unwrapped = ESApplied (LUINo [0,0])}
		,'DM'.newMap))
		(extractDownstreamChange (luiNode UIStep 'DM'.newMap
			[luiNode UIDebug 'DM'.newMap [
					luiNode UIInteract 'DM'.newMap [] {noChanges & setAttributes = 'DM'.fromList [("test",JSONString "x")]} noEffects
				] noChanges {noEffects & wrapper = ESApplied (LUINo [0,1])}
			] noChanges {noEffects & unwrapped = ESApplied (LUINo [0,0])}
		,'DM'.newMap))
	]

luiNode :: !UIType !UIAttributes ![LUI] !LUIChanges !LUIEffects -> LUI
luiNode type attributes items changes effects =
	LUINode {type = type, attributes = attributes, items = items, changes = changes, effects = effects}

tests =  applyUpstreamChangeTests 
      ++ extractDownstreamChangeTests
	  ++ extractUIWithEffects_Tests
	  ++ updateChildNodes_Tests 
	  ++ selectSubNode_Tests
	  ++ updateSubNode_Tests
	  ++ scanToPosition_Tests
	  ++ nodeExists_Tests
      ++ isPartOf_Tests
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
	  ++ combinationTests

Start w = runUnitTests tests w
