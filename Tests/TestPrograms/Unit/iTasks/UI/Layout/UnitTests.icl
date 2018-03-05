module iTasks.UI.Layout.UnitTests

import iTasks.Internal.Test.Definition
import iTasks.UI.Layout
import qualified Data.Map as DM
import qualified Data.Set as DS

derive gEq LUI, LUIChanges, LUIEffects, LUIEffectStage
derive gPrettyTrace LUI, LUIChanges, LUIEffects, LUIEffectStage, JSONNode, Set, Maybe
derive gPrettyTrace UIChange, UIChildChange, UIAttributeChange, UI, UIType

import Data.Generics.GenLexOrd
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
	,LUINode UIEmpty 'DM'.newMap [] noChanges {noEffects & additional = ESApplied 0}
	,LUINode UIStep 'DM'.newMap [] noChanges noEffects
	] noChanges noEffects

//Check if upstream changes are correctly put in the ui tree
applyUpstreamChangeTests =
	[assertEqual "No change" 
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIStep 'DM'.newMap [] noChanges noEffects
			] noChanges noEffects
		)
		(applyUpstreamChange NoChange lui0)
	,assertEqual "Root replace" 
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")])  
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIStep 'DM'.newMap [] noChanges noEffects
			] {noChanges & toBeReplaced = Just (LUINode UIEmpty 'DM'.newMap [] noChanges noEffects)} noEffects
		) 
		(applyUpstreamChange (ReplaceUI (UI UIEmpty 'DM'.newMap [])) lui0)
	,assertEqual "Child replace" 
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIStep 'DM'.newMap [] {noChanges & toBeReplaced = Just (LUINode UIEmpty 'DM'.newMap [] noChanges noEffects)} noEffects
			] noChanges noEffects
		)
		(applyUpstreamChange (ChangeUI [] [(1,ChangeChild (ReplaceUI (UI UIEmpty 'DM'.newMap [])))]) lui0)
	,assertEqual "Helper function for replace adjustIndex"
		2
		(adjustIndex_ 1 Nothing
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIEmpty 'DM'.newMap [] noChanges {noEffects & additional = ESApplied 0}
			,LUINode UIStep 'DM'.newMap [] {noChanges & toBeReplaced = Just (LUINode UIEmpty 'DM'.newMap [] noChanges noEffects)} noEffects
			]
		)
	,assertEqual "Child replace (with additional node)" 
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIEmpty 'DM'.newMap [] noChanges {noEffects & additional = ESApplied 0}
			,LUINode UIStep 'DM'.newMap [] {noChanges & toBeReplaced = Just (LUINode UIEmpty 'DM'.newMap [] noChanges noEffects)} noEffects
			] noChanges noEffects
		)
		(applyUpstreamChange (ChangeUI [] [(1,ChangeChild (ReplaceUI (UI UIEmpty 'DM'.newMap [])))]) lui1)

	,assertEqual "Child remove" 
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIStep 'DM'.newMap [] {noChanges & toBeRemoved = True} noEffects
			] noChanges noEffects
		)
		(applyUpstreamChange (ChangeUI [] [(1,RemoveChild)]) lui0)
	,assertEqual "Child remove (with additional node)" 
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIEmpty 'DM'.newMap [] noChanges {noEffects & additional = ESApplied 0}
			,LUINode UIStep 'DM'.newMap [] {noChanges & toBeRemoved = True} noEffects
			] noChanges noEffects
		)
		(applyUpstreamChange (ChangeUI [] [(1,RemoveChild)]) lui1)
	,assertEqual "Child insert" 
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIEmpty 'DM'.newMap [] {noChanges & toBeInserted = True} noEffects
			,LUINode UIStep 'DM'.newMap [] noChanges noEffects
			] noChanges noEffects
		)
		(applyUpstreamChange (ChangeUI [] [(1,InsertChild (UI UIEmpty 'DM'.newMap []))]) lui0)

	,assertEqual "Child insert (with additional node)" 
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIEmpty 'DM'.newMap [] noChanges {noEffects & additional = ESApplied 0}
			,LUINode UIEmpty 'DM'.newMap [] {noChanges & toBeInserted = True} noEffects
			,LUINode UIStep 'DM'.newMap [] noChanges noEffects
			] noChanges noEffects
		)
		(applyUpstreamChange (ChangeUI [] [(1,InsertChild (UI UIEmpty 'DM'.newMap []))]) lui1)
	,assertEqual "Child replace a newly inserted child" 
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIEmpty 'DM'.newMap [] {noChanges & toBeInserted = True} noEffects
			,LUINode UIStep 'DM'.newMap [] noChanges noEffects
			] noChanges noEffects
		)
		(applyUpstreamChange (ChangeUI [] [(1,InsertChild (UI UIInteract 'DM'.newMap [])),(1,ChangeChild (ReplaceUI (UI UIEmpty 'DM'.newMap [])))]) lui0)
	,assertEqual "Helper function for shift adjustIndex"
		2
		(adjustIndex_ 1 Nothing
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIStep 'DM'.newMap [] {noChanges & toBeShifted = Just 0} noEffects
			]
		)
	,assertEqual "Child shift down" 
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUINode UIInteract 'DM'.newMap [] {noChanges & toBeShifted = Just 0} noEffects
			,LUINode UIStep 'DM'.newMap [] noChanges noEffects
			,LUIShiftDestination 0
			] noChanges noEffects
		)
		(applyUpstreamChange (ChangeUI [] [(0,MoveChild 1)]) lui0)
	,assertEqual "Child shift in two steps" 
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUINode UIInteract 'DM'.newMap [] {noChanges & toBeShifted = Just 0} noEffects
			,LUINode UIStep 'DM'.newMap [] noChanges noEffects
			,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
			,LUINode UIRecord 'DM'.newMap [] noChanges noEffects
			,LUIShiftDestination 0
			] noChanges noEffects
		)
		(applyUpstreamChange (ChangeUI [] [(0,MoveChild 2),(2,MoveChild 3)]) lui00)
	,assertEqual "Child shift to original position in multiple steps" 
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIStep 'DM'.newMap [] noChanges noEffects
			,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
			,LUINode UIRecord 'DM'.newMap [] noChanges noEffects
			] noChanges noEffects
		)
		(applyUpstreamChange (ChangeUI [] [(1,MoveChild 2),(2,MoveChild 3),(3,MoveChild 1)]) lui00)
	,assertEqual "Root set attribute"
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")])
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIStep 'DM'.newMap [] noChanges noEffects
			] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "changed-title")]} noEffects
		)
		(applyUpstreamChange (ChangeUI [SetAttribute "title" (JSONString "changed-title")] []) lui0)
	,assertEqual "Root delete attribute"
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			,LUINode UIStep 'DM'.newMap [] noChanges noEffects
			] {noChanges & delAttributes = 'DS'.fromList ["title"]} noEffects
		)
		(applyUpstreamChange (ChangeUI [DelAttribute "title"] []) lui0)
	,assertEqual "Set attribute after shift"
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUINode UIInteract 'DM'.newMap [] {noChanges & toBeShifted = Just 0, setAttributes = 'DM'.fromList [("title",JSONString "changed-title")]} noEffects
			,LUINode UIStep 'DM'.newMap [] noChanges noEffects
			,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
			,LUIShiftDestination 0
			,LUINode UIRecord 'DM'.newMap [] noChanges noEffects
			] noChanges noEffects
		)
		(applyUpstreamChange (ChangeUI [] [(0,MoveChild 2)
		                                  ,(2,ChangeChild (ChangeUI [SetAttribute "title" (JSONString "changed-title")] []))
										  ]) lui00)
	,assertEqual "Remove child after shift"
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
			[LUINode UIInteract 'DM'.newMap [] {noChanges & toBeRemoved = True } noEffects
			,LUINode UIStep 'DM'.newMap [] noChanges noEffects
			,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
			,LUINode UIRecord 'DM'.newMap [] noChanges noEffects
			] noChanges noEffects
		)
		(applyUpstreamChange (ChangeUI [] [(0,MoveChild 2)
		                                  ,(2,RemoveChild)
										  ]) lui00)
	]

//Check if pending downstream changes are correctly extracted from the tree
extractDownstreamChangeTests = 
	[assertEqual "Simple top-level replace without effects" 
		(ReplaceUI (UI UIEmpty 'DM'.newMap [])
		,LUINode UIEmpty 'DM'.newMap [] noChanges noEffects
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & toBeReplaced = Just (LUINode UIEmpty 'DM'.newMap [] noChanges noEffects)} noEffects
		))
	,assertEqual "Top-level replace with a replaced child without effects" 
		(ReplaceUI (UI UIParallel 'DM'.newMap [])
		,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & toBeReplaced = Just (LUINode UIEmpty 'DM'.newMap []
					{noChanges & toBeReplaced = Just (LUINode UIParallel 'DM'.newMap [] noChanges noEffects)} noEffects)
				  } noEffects
		))
	,assertEqual "Top-level replace with a removed child without effects" 
		(ReplaceUI (UI UIParallel 'DM'.newMap [UI UIRecord 'DM'.newMap []])
		,LUINode UIParallel 'DM'.newMap
			[LUINode UIRecord 'DM'.newMap [] noChanges noEffects
			] noChanges noEffects
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
		))
	,assertEqual "Top-level replace with shifted children without effects" 
		(ReplaceUI (UI UIParallel 'DM'.newMap [UI UIRecord 'DM'.newMap [],UI UIContainer 'DM'.newMap [], UI UIPanel 'DM'.newMap [], UI UIDebug 'DM'.newMap []])
		,LUINode UIParallel 'DM'.newMap
			[LUINode UIRecord 'DM'.newMap [] noChanges noEffects
			,LUINode UIContainer 'DM'.newMap [] noChanges noEffects
			,LUINode UIPanel 'DM'.newMap [] noChanges noEffects
			,LUINode UIDebug 'DM'.newMap [] noChanges noEffects
			] noChanges noEffects
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
		))

	,assertEqual "Top-level replace with a changed attribute child without effects" 
		(ReplaceUI (UI UIParallel 'DM'.newMap [UI UIRecord 'DM'.newMap [], UI UIInteract ('DM'.fromList [("attr",JSONString "B")]) []])
		,LUINode UIParallel 'DM'.newMap
			[LUINode UIRecord 'DM'.newMap [] noChanges noEffects
			,LUINode UIInteract ('DM'.fromList [("attr",JSONString "B")]) [] noChanges noEffects
			] noChanges noEffects
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
		))

	,assertEqual "Top-level replace with an overwritten type" 
		(ReplaceUI (UI UIParallel 'DM'.newMap [])
		,LUINode UIEmpty 'DM'.newMap
			[] noChanges {noEffects & overwrittenType = ESApplied UIParallel}
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & toBeReplaced = Just (LUINode UIEmpty 'DM'.newMap []
					noChanges {noEffects & overwrittenType = ESToBeApplied UIParallel})
				  } noEffects
		))
	,assertEqual "Top-level replace with an overwritten attribute" 
		(ReplaceUI (UI UIEmpty ('DM'.fromList [("title",JSONString "B")]) [])
		,LUINode UIEmpty ('DM'.fromList [("title",JSONString "A")])
			[] noChanges {noEffects & overwrittenAttributes = 'DM'.fromList [("title",ESApplied (JSONString "B"))]}
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & toBeReplaced = Just (LUINode UIEmpty ('DM'.fromList [("title",JSONString "A")]) []
					noChanges {noEffects & overwrittenAttributes = 'DM'.fromList [("title",ESToBeApplied (JSONString "B"))]})
				  } noEffects
		))
	,assertEqual "Top-level replace with a hidden attribute" 
		(ReplaceUI (UI UIEmpty 'DM'.newMap [])
		,LUINode UIEmpty ('DM'.fromList [("title",JSONString "A")])
			[] noChanges {noEffects & hiddenAttributes = 'DM'.fromList [("title",ESApplied ())]}
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & toBeReplaced = Just (LUINode UIEmpty ('DM'.fromList [("title",JSONString "A")]) []
					noChanges {noEffects & hiddenAttributes = 'DM'.fromList [("title",ESToBeApplied ())]})
				  } noEffects
		))
	,assertEqual "Top-level replace with an inserted child " 
		(ReplaceUI (UI UIEmpty 'DM'.newMap [UI UIPanel 'DM'.newMap []])
		,LUINode UIEmpty 'DM'.newMap
			[LUINode UIPanel 'DM'.newMap [] noChanges {noEffects & additional = ESApplied 0}
			] noChanges noEffects
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & toBeReplaced = Just 
						(LUINode UIEmpty ('DM'.newMap)
							[ LUINode UIPanel 'DM'.newMap [] noChanges {noEffects & additional = ESToBeApplied 0}
							] noChanges noEffects
						)
				  } noEffects
		))
	,assertEqual "Top-level overwritten type"
		(ReplaceUI (UI UIContainer 'DM'.newMap
			[UI UIInteract 'DM'.newMap []
			])
		,LUINode UIStep 'DM'.newMap
			[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
			] noChanges {noEffects & overwrittenType = ESApplied UIContainer}
		)
		(extractDownstreamChange (
			LUINode UIStep 'DM'.newMap
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				] noChanges {noEffects & overwrittenType = ESToBeApplied UIContainer}
		))
	,assertEqual "Removed child without effects" 
		(ChangeUI [] [(1,RemoveChild)]
		,LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] {noChanges & toBeRemoved = True} noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		))
	,assertEqual "Inserted child without effects" 
		(ChangeUI [] [(1,InsertChild (UI UIStep 'DM'.newMap []))]
		,LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] {noChanges & toBeInserted = True} noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		))
	,assertEqual "Shifted children without effects" 
		(ChangeUI [] [(2,MoveChild 4),(3,MoveChild 2)]
		,LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIDebug 'DM'.newMap [] noChanges noEffects
				,LUINode UIRecord 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
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
		))
	,assertEqual "Set attribute in child without effects" 
		(ChangeUI [] [(2,ChangeChild (ChangeUI [SetAttribute "title" (JSONString "New attribute")] []))]
		,LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel ('DM'.fromList [("title",JSONString "New attribute")]) [] noChanges noEffects
				] noChanges noEffects
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "New attribute")]} noEffects
				] noChanges noEffects
		))
	,assertEqual "Delete attribute in child without effects" 
		(ChangeUI [] [(2,ChangeChild (ChangeUI [DelAttribute "title"] []))]
		,LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel ('DM'.fromList [("title",JSONString "Old attribute")]) [] {noChanges & delAttributes = 'DS'.fromList ["title"]} noEffects
				] noChanges noEffects
		))
	,assertEqual "Overwritten attribute in child" 
		(ChangeUI [] [(2,ChangeChild (ChangeUI [SetAttribute "title" (JSONString "B")] []))]
		,LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel ('DM'.fromList [("title",JSONString "A")]) [] noChanges
					{noEffects & overwrittenAttributes = 'DM'.fromList [("title",ESApplied (JSONString "B"))]}
				] noChanges noEffects
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel ('DM'.fromList [("title",JSONString "A")]) [] noChanges
					{noEffects & overwrittenAttributes = 'DM'.fromList [("title",ESToBeApplied (JSONString "B"))]}
				] noChanges noEffects
		))
	,assertEqual "New additional child" 
		(ChangeUI [] [(2,InsertChild (UI UIParallel 'DM'.newMap []))]
		 ,LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges {noEffects & additional = ESApplied 0}
				] noChanges noEffects
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges {noEffects & additional = ESToBeApplied 0}
				] noChanges noEffects
		))
	,assertEqual "Removed additional child" 
		(ChangeUI [] [(2,RemoveChild)]
		 ,LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges {noEffects & additional = ESToBeRemoved 0}
				] noChanges noEffects
		))
	,assertEqual "New hidden child" 
		(ChangeUI [] [(2,RemoveChild)]
		 ,LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges {noEffects & hidden = ESApplied 0}
				] noChanges noEffects
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges {noEffects & hidden = ESToBeApplied 0}
				] noChanges noEffects
		))
	,assertEqual "Removed hidden child" 
		(ChangeUI [] [(2,InsertChild (UI UIParallel 'DM'.newMap []))]
		 ,LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		)
		(extractDownstreamChange (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges {noEffects & hidden = ESToBeRemoved 0}
				] noChanges noEffects
		))
	]
selectNode_Tests = 
	[ assertEqual "Selecting a shifted child node"
		(Just (LUINode UIDebug 'DM'.newMap [] {noChanges & toBeShifted = Just 1} noEffects))
		(selectNode_ [2] (
			LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] {noChanges & toBeShifted = Just 0} noEffects
				,LUIShiftDestination 1
				,LUINode UIRecord 'DM'.newMap [] noChanges noEffects
				,LUINode UIDebug 'DM'.newMap [] {noChanges & toBeShifted = Just 1} noEffects
				,LUIShiftDestination 0
				] noChanges noEffects
		))
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
		)

		(updateNode_ [2]
			(\(LUINode _ attr items changes effects) -> LUINode UIInteract attr items changes effects) 
			(LUINode UIPanel ('DM'.fromList [("title",JSONString "Parent panel")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] {noChanges & toBeShifted = Just 0} noEffects
				,LUIShiftDestination 1
				,LUINode UIRecord 'DM'.newMap [] noChanges noEffects
				,LUINode UIDebug 'DM'.newMap [] {noChanges & toBeShifted = Just 1} noEffects
				,LUIShiftDestination 0
				] noChanges noEffects
		))
	]

setUITypeRuleTests =
	[ //TODO
	]

setUIAttributesRuleTests =
	[ //TODO
	]

delUIAttributesRuleTests =
	[ //TODO
	]

modifyUIAttributesRuleTests =
	[assertEqual "Modify attributes rule: change title to a hint" 
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "B")]}
					{noEffects
					& overwrittenAttributes = 'DM'.fromList [("hint",ESToBeApplied (JSONString "C"))]
					, hiddenAttributes = 'DM'.fromList [("title",ESToBeApplied ())]
					}
		)
		(modifyUIAttributesRule 
			(SelectKeys ["title"])
			(\attr -> 'DM'.fromList [("hint",JSONString "C")])
			0 
			(LUINode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "B")]} noEffects
			)
		)
	]
copySubUIAttributesRuleTests =
	[assertEqual "Copy sub attributes rule: copy title to attribute to a child"
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges {noEffects & overwrittenAttributes = 'DM'.fromList [("title",ESToBeApplied (JSONString "B"))]}
				] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "B")]} noEffects
		)
		(copySubUIAttributesRule 
			(SelectKeys ["title"]) [] [1] 0
			(LUINode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] {noChanges & setAttributes = 'DM'.fromList [("title",JSONString "B")]} noEffects
			)
		)
	]
insertChildUIRuleTests =
	[assertEqual "Insert a child rule: insert in known set"
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges {noEffects & additional = ESToBeApplied 0}
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
		)
		(insertChildUIRule 1 (UI UIParallel 'DM'.newMap []) 0
			(LUINode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			)
		)
	]

removeSubUIsRuleTests =
	[assertEqual "Remove children rule: Remove all UIStep nodes "
		(LUINode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges {noEffects & hidden = ESToBeApplied 0}
				] noChanges noEffects
		)
		(removeSubUIsRule (SelectByType UIStep) 0
			(LUINode UIPanel ('DM'.fromList [("title",JSONString "A")]) 
				[LUINode UIInteract 'DM'.newMap [] noChanges noEffects
				,LUINode UIParallel 'DM'.newMap [] noChanges noEffects
				,LUINode UIStep 'DM'.newMap [] noChanges noEffects
				] noChanges noEffects
			)
		)
	]

tests =  applyUpstreamChangeTests 
      ++ extractDownstreamChangeTests
	  ++ selectNode_Tests
	  ++ updateNode_Tests
	  ++ setUITypeRuleTests
	  ++ setUIAttributesRuleTests
	  ++ delUIAttributesRuleTests
	  ++ modifyUIAttributesRuleTests
	  ++ copySubUIAttributesRuleTests
	  ++ insertChildUIRuleTests
	  ++ removeSubUIsRuleTests

Start w = runUnitTestsCLI [testsuite "Test.iTasks.UI.Layout" "Duh.." tests] w
