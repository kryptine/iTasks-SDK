implementation module iTasks.UI.Layout

import StdTuple, StdList, StdBool, StdInt, StdOrdList, StdArray, StdMisc, StdString
import Data.GenLexOrd
import Data.Maybe, Data.Either, Text, Data.Tuple, Data.List, Data.Either, Data.Functor, Data.Func
import iTasks.Internal.Util, iTasks.Internal.HtmlUtil, iTasks.UI.Definition
import iTasks.Internal.Generic.Defaults 
import StdEnum

from Data.Map as DM import qualified newMap, put, get, del, toList, fromList, delList, alter, union, keys, unions, singleton, member, null
from Data.Set as DS import qualified newSet, insert, delete, toList, fromList, null
from Data.Tuple import appSnd

import Text.GenJSON

from StdFunc import o, const, id, flip
from iTasks.Internal.TaskState import :: TIMeta(..), :: TaskTree(..), :: DeferredJSON
from iTasks.Internal.TaskEval import :: TaskTime
from iTasks.WF.Combinators.Core import :: AttachmentStatus
import iTasks.WF.Definition
import Data.GenEq

//This type records the states of layouts applied somewhere in a ui tree
derive JSONEncode LUI, LUIChanges, LUIEffects, LUIEffectStage, LUINo, Set
derive JSONDecode LUI, LUIChanges, LUIEffects, LUIEffectStage, LUINo, Set

derive gEq LUIEffectStage, LUINo

derive gLexOrd LUIEffectStage

instance < (LUIEffectStage a) | gLexOrd{|*|} a
where
	(<) x y = (gLexOrd{|*|} x y) === LT

instance < LUINo
where
	(<) (LUINo xs) (LUINo ys) = xs < ys

instance == LUINo
where
	(==) (LUINo xs) (LUINo ys) = xs == ys

instance toString LUINo
where
	toString (LUINo steps) = join "." (map toString steps)

/*
* The first thing we need for rule-based layouts is a datastructure that can keep track
* of what changes have been made by layout rules and that can 'buffer' upstream changes 
* such that layout rules can be applied before they are passed on.
* 
* The following functions are used to initialize this datastructure
*/

noChanges :: LUIChanges
noChanges = {toBeInserted=False, toBeRemoved=False, toBeReplaced=Nothing, toBeShifted=Nothing, setAttributes='DM'.newMap, delAttributes = 'DS'.newSet}

noEffects :: LUIEffects
noEffects = {overwrittenType = ESNotApplied, overwrittenAttributes = 'DM'.newMap, hiddenAttributes = 'DM'.newMap, additional = ESNotApplied, hidden = ESNotApplied, moved = ESNotApplied, wrapper = ESNotApplied, unwrapped = ESNotApplied}

//Initialize an LUI tree from a regular UI tree
initLUI :: UI -> LUI
initLUI (UI type attr items) = LUINode type attr (map initLUI items) noChanges noEffects

initLUIMoves :: LUIMoves
initLUIMoves = 'DM'.newMap

extractResetChange :: (LUI,LUIMoves) -> (UIChange,(LUI,LUIMoves))
extractResetChange (lui,moves)
	# mbUI = extractUIWithEffects_ (lui,moves)
	= case confirmChangesAndEffects_ (lui,moves) of
		(Just lui,moves) = (ReplaceUI (fromMaybe (UI UIEmpty 'DM'.newMap []) mbUI), (lui,moves))
		_                = abort "extractResetChange: top-level node removed accidentally"
/*
* When upstream changes 'arrive' they are tracked in the 'buffer' data structure.
* All information about what should be modified according to the upstream change is
* recorded in the tree.
* 
* The following functions implement this recording of changes in the tree
*/

//When an upstream UI change is applied to the LUI it is recorded in the LUI tree
//in such a way that it can easily be extracted as a downstream change later on
applyUpstreamChange :: UIChange (LUI,LUIMoves) -> (LUI,LUIMoves)
applyUpstreamChange change (lui=:(LUIMoveSource moveId),moves)
	# movedLui = getMovedNode_ moveId moves
	# (movedLui,moves) = applyUpstreamChange change (movedLui,moves)
	# moves = putMovedNode_ moveId movedLui moves
	= (lui,moves)
//If the node is a wrapper, apply the change to the wrapped child
applyUpstreamChange change (LUINode type attr items changes effects=:{LUIEffects|wrapper=ESApplied _},moves)
	# (items,moves) = mapSt (\i m -> if (isAdditional_ i) (i,m) (applyUpstreamChange change (i,m))) items moves
	= (LUINode type attr items changes effects,moves)
applyUpstreamChange NoChange (lui,moves) = (lui,moves)
applyUpstreamChange (ReplaceUI ui) (LUINode type attr items changes effects,moves)
	| changes.toBeInserted //If it is a new node, we can replace it
		= (setToBeInserted_ (initLUI ui),moves)
	= (LUINode type attr items {changes & toBeReplaced = Just (initLUI ui)} effects,moves)
applyUpstreamChange (ReplaceUI ui) (lui,moves) = abort "applyUpstreamChange: can't replace an non-LUINode constructor"
applyUpstreamChange (ChangeUI attributeChanges childChanges) (lui,moves)
	= (foldl applyUpstreamChildChange (foldl applyUpstreamAttributeChange (lui,moves) attributeChanges) childChanges)
where
	applyUpstreamAttributeChange :: (LUI,LUIMoves) UIAttributeChange -> (LUI,LUIMoves)
	applyUpstreamAttributeChange (LUINode type attr items changes=:{setAttributes,delAttributes} effects,moves) (SetAttribute key value)
		# setAttributes = 'DM'.put key value setAttributes
		# delAttributes = 'DS'.delete key delAttributes
		= (LUINode type attr items {changes & setAttributes = setAttributes, delAttributes = delAttributes} effects, moves)
	applyUpstreamAttributeChange (LUINode type attr items changes=:{setAttributes,delAttributes} effects,moves) (DelAttribute key)
		# setAttributes = 'DM'.del key setAttributes
		# delAttributes = 'DS'.insert key delAttributes
		= (LUINode type attr items {changes & setAttributes = setAttributes, delAttributes = delAttributes} effects, moves)
	applyUpstreamAttributeChange (lui,moves) _ = (lui,moves)

	applyUpstreamChildChange :: (LUI,LUIMoves) (Int,UIChildChange) -> (LUI,LUIMoves)
	applyUpstreamChildChange (lui,moves) (index,ChangeChild change) = case lui of
		(LUINode type attr items changes effects)
			# adjustedIndex = adjustIndex_ index items moves
			| index < 0 || adjustedIndex >= length items = (lui,moves)
			# (items,moves) = updateItem (applyUpstreamChange change) adjustedIndex items moves
			= (LUINode type attr items changes effects, moves)
		_
			= (lui,moves)
	applyUpstreamChildChange (lui,moves) (index,RemoveChild) = case lui of
		(LUINode type attr items changes effects)
			# adjustedIndex = adjustIndex_ index items moves
			| index < 0 || adjustedIndex >= length items = (lui,moves)
			= (LUINode type attr (removeItem adjustedIndex items) changes effects,moves)
		_ = (lui,moves)
	applyUpstreamChildChange (lui,moves) (index,InsertChild ui) = case lui of
		(LUINode type attr items changes effects)
			# adjustedIndex = adjustIndex_ index items moves
			| index < 0 || adjustedIndex > length items = (lui,moves)
			= (LUINode type attr (insertAt adjustedIndex (setToBeInserted_ (initLUI ui)) items) changes effects, moves)
		_ = (lui,moves)
	applyUpstreamChildChange (lui,moves) (index,MoveChild destination) = case lui of
		(LUINode type attr items changes effects)
			# shiftId = nextShiftID_ items
			# adjustedIndex = adjustIndex_ index items moves
			| index < 0 || adjustedIndex >= length items = (lui,moves)
			= (LUINode type attr (shiftItem shiftId adjustedIndex destination items) changes effects, moves)
		_ = (lui,moves)

	//An index may point to the destination of a shifted child node. In that case we want to apply
	//the update to the node that will be shifted to that destination
	updateItem updateFunction index items moves = case items !! index of
		(LUIShiftDestination shiftId) = updateItem updateFunction (fst (lookupShiftSource_ shiftId items)) items moves
		lui 
			# (lui,moves) = applyUpdate (lui,moves)
			= (updateAt index lui items, moves)
	where
		applyUpdate (LUINode type attr items changes=:{toBeReplaced = Just replacement} effects,moves)
			# (replacement,moves) = applyUpdate (replacement,moves)
			= (LUINode type attr items {changes & toBeReplaced = Just replacement} effects, moves)
		applyUpdate (lui,moves) = updateFunction (lui,moves)

	//When upstream removes a shifted note, we can forget that it was shifted and mark the source node as removed instead
	removeItem index items = case items !! index of
		(LUIShiftDestination shiftId) = map (removeShiftSource shiftId) (removeAt index items)
		(LUINode type attr citems changes effects) = updateAt index (LUINode type attr citems {changes & toBeRemoved = True} effects) items
	where
		removeShiftSource shiftId lui=:(LUINode type attr items changes=:{toBeShifted = Just sourceId} effects)
			| sourceId == shiftId = LUINode type attr items {changes & toBeShifted = Nothing, toBeRemoved = True} effects
			                      = lui
		removeShiftSource _ lui = lui

	shiftItem shiftId index destination items = case items !! index of
		//A shift destination: that means upstream expects the node to be already moved
		//We set a new destination or remove the destination if we move back to the original position
		(LUIShiftDestination prevShiftId)
			//Remove the current destination
			# items = removeAt index items
			//If we move it back to the original position, we can consider the node never to be moved
			//otherwise, we create a new destination node
			= case findSamePositionShift prevShiftId destination items of
				Just (sourcePosition,LUINode type attr citems changes effects)
					//Update the source
					= updateAt sourcePosition (LUINode type attr citems {changes & toBeShifted = Nothing} effects) items
				Nothing
					//And add the new destination
					= insertAt (adjustIndex_ destination items moves) (LUIShiftDestination prevShiftId) items
		//Regular node
		(LUINode type attr citems changes effects)
			//Mark the node as a shifted node
			# items = updateAt index (LUINode type attr citems {changes & toBeShifted = Just shiftId} effects) items
			//Record the destination
			= insertAt (adjustIndex_ destination items moves) (LUIShiftDestination shiftId) items
	where
		findSamePositionShift shiftId destination items = find 0 0 items
		where
			find i ai [] = Nothing
			find i ai [x=:(LUINode _ _ _ {toBeShifted=Just sourceId} _):xs]
				| sourceId == shiftId = if (ai == destination) (Just (ai,x)) Nothing
				                      = find (i + 1) ai xs
			find i ai [x:xs]
				| nodeExists_ (LUINo []) x moves = find (i + 1) (ai + 1) xs
				                                              = find (i + 1) ai xs

	//Adjust a change index for additional nodes inserted by layout rules
	adjustIndex_ :: Int [LUI] LUIMoves -> Int
	adjustIndex_ index items moves = fst3 (scanToPosition_ (LUINo []) index items moves)

/*
* Layout rules transform the 'buffered' tree and annotate the places where layout effects
* should be applied, or should no longer be applied
*/
setUIType :: UIType -> LayoutRule
setUIType newType = rule
where
	rule ruleNo (lui,moves) = updateNode_ ruleNo (rule` ruleNo) (lui,moves)

	rule` ruleNo (LUINode type attr items changes effects=:{overwrittenType},moves)
		# overwrittenType = case overwrittenType of
			(ESNotApplied) = ESToBeApplied (ruleNo,newType)
			(ESToBeApplied _) = ESToBeApplied (ruleNo,newType)
			(ESApplied (curRule,curType))
				| curType === newType = (ESApplied (curRule,curType))
				| otherwise             = (ESToBeUpdated (curRule,curType) (ruleNo,newType))
			(ESToBeUpdated (curRule,curType) _)
				| curType === newType = (ESApplied (curRule,curType))
				| otherwise           = (ESToBeUpdated (curRule,curType) (ruleNo,newType))
			(ESToBeRemoved (curRule,curType))
				| curType === newType = (ESApplied (curRule,curType))
				| otherwise           = (ESToBeUpdated (curRule,curType) (ruleNo,newType))
		= (LUINode type attr items changes {effects & overwrittenType = overwrittenType},moves)

setUIAttributes :: UIAttributes -> LayoutRule
setUIAttributes setAttributes = rule
where
	rule ruleNo (lui,moves) = updateNode_ ruleNo (rule` ruleNo) (lui,moves)

	rule` ruleNo (LUINode type attr items changes effects=:{overwrittenAttributes},moves)
		# overwrittenAttributes = foldr (overwriteAttribute_ ruleNo) overwrittenAttributes ('DM'.toList setAttributes)
		= (LUINode type attr items changes {effects & overwrittenAttributes = overwrittenAttributes},moves)

delUIAttributes :: UIAttributeSelection -> LayoutRule
delUIAttributes selection = rule 
where
	rule ruleNo (lui,moves) = updateNode_ ruleNo (rule` ruleNo) (lui,moves)

	rule` ruleNo (lui=:(LUINode type attr items changes effects=:{hiddenAttributes}),moves)
		# keys = 'DM'.keys (getAttributesAtRuleApplication_ ruleNo lui)
		# hiddenAttributes = foldr (hideAttribute_ ruleNo (matchAttributeKey_ selection)) hiddenAttributes keys
		= (LUINode type attr items changes {effects & hiddenAttributes = hiddenAttributes}, moves)

modifyUIAttributes :: UIAttributeSelection (UIAttributes -> UIAttributes) -> LayoutRule
modifyUIAttributes selection modifier = rule 
where
	rule ruleNo (lui,moves) = updateNode_ ruleNo (rule` ruleNo) (lui,moves)

	rule` ruleNo (lui=:(LUINode type attr items changes effects=:{overwrittenAttributes,hiddenAttributes}),moves)
		//1. Apply the modifier function to the current of attributes that match the selection
		# selectedAttr = selectAttributes_ selection (getAttributesAtRuleApplication_ ruleNo lui)
		# modifiedAttr = modifier selectedAttr
		//2. Override new attributes and hide attributes that match the selection 
		# overwrittenAttributes = overrideModifiedAttributes ruleNo modifiedAttr overwrittenAttributes
		# hiddenAttributes = hideRemovedAttributes ruleNo selectedAttr modifiedAttr hiddenAttributes
		= (LUINode type attr items changes {effects & overwrittenAttributes = overwrittenAttributes, hiddenAttributes = hiddenAttributes}, moves)

	overrideModifiedAttributes ruleNo modified overwritten = foldr (overwriteAttribute_ ruleNo) overwritten ('DM'.toList modified)
	hideRemovedAttributes ruleNo selected modified hidden = foldr (hideAttribute_ ruleNo isRemoved) hidden ('DM'.keys selected)
	where
		isRemoved key = not ('DM'.member key modified)

copySubUIAttributes :: UIAttributeSelection UIPath UIPath -> LayoutRule
copySubUIAttributes selection src dst = rule
where
	rule ruleNo (lui,moves)
		//Find the selected attributes in the source node... 
		//Then use the setUIAttributes layout rule to copy the changes
		= maybe (lui,moves) (withEffect (lui,moves)) (selectSource (lui,moves))
	where
		selectSource (lui,moves) = fmap (selectAttributes_ selection o getAttributesAtRuleApplication_ ruleNo) (selectSubNode_ ruleNo src (lui,moves))
		withEffect (lui,moves) attr = updateSubNode_ ruleNo dst ((setUIAttributes attr) ruleNo) (lui,moves)

wrapUI :: UIType -> LayoutRule
wrapUI type = rule
where
	rule ruleNo (lui,moves) = updateNode_ ruleNo rule` (lui,moves)
	where
		rule` (lui=:(LUINode _ _ _ _ {LUIEffects|wrapper}),moves)
			//Check if we already wrapped
			| wrapper === (ESApplied ruleNo) || wrapper === (ESToBeApplied ruleNo)
				= (lui,moves)
			//Not yet wrapped
			| otherwise
				= (LUINode type 'DM'.newMap [lui] noChanges {noEffects & wrapper = ESToBeApplied ruleNo},moves)

unwrapUI :: LayoutRule
unwrapUI = rule
where
	rule ruleNo (lui,moves) = updateNode_ ruleNo (rule` ruleNo) (lui,moves)

	rule` ruleNo (LUINode type attr items changes effects=:{LUIEffects|unwrapped}, moves)
		# hasChildren = (selectChildNodes_ ruleNo (items,moves)) =: [_:_]
		# unwrapped = case unwrapped of
			ESNotApplied = if hasChildren (ESToBeApplied ruleNo) ESNotApplied
			ESToBeApplied _ = if hasChildren (ESToBeApplied ruleNo) ESNotApplied
			ESApplied prevRuleNo = if hasChildren (ESApplied ruleNo) (ESToBeRemoved prevRuleNo)
			ESToBeRemoved prevRuleNo = if hasChildren (ESApplied ruleNo) (ESToBeRemoved prevRuleNo)
		= (LUINode type attr items changes {effects & unwrapped = unwrapped}, moves)

hideUI :: LayoutRule
hideUI = rule
where
	rule ruleNo (lui,moves) = updateNode_ ruleNo (rule` ruleNo) (lui,moves)
	rule` ruleNo (LUINode type attr items changes effects=:{hidden},moves)
		# hidden = case hidden of
			ESNotApplied = ESToBeApplied ruleNo
			ESToBeApplied _ = ESToBeApplied ruleNo
			ESApplied _ = ESApplied ruleNo
			ESToBeRemoved _ = ESApplied ruleNo
		= (LUINode type attr items changes {effects & hidden=hidden},moves)

insertChildUI :: Int UI -> LayoutRule
insertChildUI position insertion = rule
where
	rule ruleNo (lui=:(LUINode type attr items changes=:{toBeReplaced=Just replacement} effects),moves)
		# (replacement,moves) = rule ruleNo (replacement, moves)
		= (LUINode type attr items {changes & toBeReplaced=Just replacement} effects, moves)

	rule ruleNo (lui=:(LUINode type attr items changes effects),moves)
		= case scanToPosition_ ruleNo position items moves of
			(_,True,Nothing)	
				//If the index is at the end of the range, add the item
				= (LUINode type attr (undoAdditions ruleNo items ++ [setToBeAdded_ ruleNo (initLUI insertion)]) changes effects, moves)
			(index,True,Just selected)
				| getAdditional selected === ESToBeApplied ruleNo || getAdditional selected === ESApplied ruleNo
					= (lui, moves)
				| otherwise
					= (LUINode type attr (insertAt index (setToBeAdded_ ruleNo (initLUI insertion)) (undoAdditions ruleNo items)) changes effects, moves)
			_
				= (lui,moves)

	rule ruleNo (lui,moves) = (lui,moves)

	getAdditional (LUINode _ _ _ _ {additional}) = additional
	getAdditional _ = ESNotApplied

	undoAdditions ruleNo items = map undo items
	where
		undo lui=:(LUINode type attr items changes effects=:{additional})
			| additional === (ESToBeApplied ruleNo)
				= LUINode type attr items changes {effects & additional = ESNotApplied}
			| additional === (ESApplied ruleNo)
				= LUINode type attr items changes {effects & additional = ESToBeRemoved ruleNo}
			| otherwise
				= lui
		undo lui = lui

moveSubUIs :: UISelection UIPath Int -> LayoutRule
moveSubUIs selection path pos = rule
where
	rule ruleNo (lui,moves) = updateNode_ ruleNo (rule` ruleNo) (lui,moves)

	rule` ruleNo (lui,moves)
		# (moved,lui,moves) = checkNodes selection (destinationExists path pos (lui,moves)) lui moves
		# (lui,moves) = updateDestination moved (lui,moves)
		= (lui,moves)
	where
		//1 Check if the destination position. If it does not, we can't move anything
		destinationExists path pos (lui,moves)
			= maybe False (positionExists pos) (selectSubNode_ ruleNo path (lui,moves))
		where
			positionExists pos (LUINode _ _ items _ _)
				= snd3 (scanToPosition_ ruleNo pos items moves)

		//2 Recursively remove/restore based on the selection nodes 
		checkNodes selection destinationExists (LUINode type attr items changes effects) moves
			# nextId = nextMoveID_ moves
			# ((_,acc),items,moves) = processChildNodes_ ruleNo (check []) ((nextId,[]),items,moves)
			= (reverse acc, LUINode type attr items changes effects, moves)
		where
			check path i ((nextId,acc),lui,moves)
				//Check if the node should be moved
				| destinationExists && nodeSelected_ ruleNo selection (path ++ [i]) lui moves //Node should be moved
					= case lui of
						(LUINode type attr items changes effects)
							# moves = 'DM'.put nextId (LUINode type attr items changes {effects & moved = ESToBeApplied ruleNo}) moves
							= ((nextId + 1,[nextId:acc]), LUIMoveSource nextId, moves)
						(LUIMoveSource moveId)
							//If we match a source here, either this rule moved it or a later rule.
							//If the referenced node was moved by a later rule, we update it because this rule comes first 
							# moves = case getMovedNode_ moveId moves of
								(LUINode type attr items changes effects=:{moved=ESApplied movedBy})
									| movedBy == ruleNo
										= moves //We moved it, no need to update anything
									| otherwise
										= putMovedNode_ moveId (LUINode type attr items changes {effects & moved = ESToBeUpdated movedBy ruleNo}) moves
								(LUINode type attr items changes effects=:{moved=ESToBeUpdated _ movedBy})
									| movedBy == ruleNo
										= moves //We Just updated it, no need to update anything
									| otherwise
										= abort "moveSubUIs: We should never reach this case"
								_	
									= abort "moveSubUIs: We should never reach this case"
							= ((nextId,[moveId:acc]), LUIMoveSource moveId, moves)
						(LUIMoveDestination moveId _)
							//If we match a move destination here, an earlier rule moved it here
							# moves = case getMovedNode_ moveId moves of
								(LUINode type attr items changes effects=:{moved=ESToBeApplied _}) //It is not yet moved
									= putMovedNode_ moveId (LUINode type attr items changes {effects & moved = ESToBeApplied ruleNo}) moves
								(LUINode type attr items changes effects=:{moved=ESApplied movedBy}) //It was already moved

									= (putMovedNode_ moveId (LUINode type attr items changes {effects & moved = ESToBeUpdated movedBy ruleNo}) moves)
								(LUINode type attr items changes effects=:{moved=ESToBeUpdated movedBy _}) //It was already moved (twice)
									= putMovedNode_ moveId (LUINode type attr items changes {effects & moved = ESToBeUpdated movedBy ruleNo}) moves
							= ((nextId,[moveId:acc]),lui,moves)	
						_
							= abort "moveSubUIs: We don't expect shift destinations here (because we are called by processChildNodes_)"
				| otherwise
					= case lui of
						(LUINode type attr items changes effects)
							//Search in children
							# ((nextId,acc),items,moves) = processChildNodes_ ruleNo (check (path ++ [i])) ((nextId,acc),items,moves)
							= ((nextId,acc),LUINode type attr items changes effects,moves)	
						(LUIMoveSource moveId)
							//If we match a source here, either this rule moved it or a later rule.
							# (LUINode type attr items changes effects=:{moved}) = getMovedNode_ moveId moves
							//If this rule moved it here we should mark it as no longer moved
							# effects = case moved of
								(ESApplied movedBy) | movedBy == ruleNo
									= {effects & moved = ESToBeRemoved ruleNo}
								_
									= effects
							# ((nextId,acc),items,moves) = processChildNodes_ ruleNo (check (path ++ [i])) ((nextId,acc),items,moves)
							# moves = putMovedNode_ moveId (LUINode type attr items changes effects) moves
							= ((nextId,acc),lui,moves)	
						(LUIMoveDestination moveId _)
							//An earlier rule moved it here, just process the children
							# (LUINode type attr items changes effects) = getMovedNode_ moveId moves
							# ((nextId,acc),items,moves) = processChildNodes_ ruleNo (check (path ++ [i])) ((nextId,acc),items,moves)
							# moves = putMovedNode_ moveId (LUINode type attr items changes effects) moves
							= ((nextId,acc),lui,moves)	
						_	
							= ((nextId,acc),lui,moves)	

		//3 Mark the destination with the moved nodes
		updateDestination moved (lui,moves) = updateSubNode_ ruleNo path update (lui,moves)
		where
			update (LUINode type attr items changes effects, moves)
				//Remove the current destination markers
				# items = filter (not o currentMove) items
				// (re)Insert the destination marker in the right place
				# destinations = [LUIMoveDestination moveId ruleNo \\ moveId <- moved]
				# items = case scanToPosition_ ruleNo pos items moves of
					(index,True,_) = ( take index items ++ destinations ++ drop index items)
					_ = items
				= (LUINode type attr items changes effects, moves)

			currentMove (LUIMoveDestination moveId _) = isMember moveId moved
			currentMove  _ = False
import StdDebug

layoutSubUIs :: UISelection LayoutRule -> LayoutRule
layoutSubUIs selection sub = rule
where
	rule ruleNo (lui,moves) = apply [] (lui,moves)
	where
		//Check if the layout matches (without any dereferencing or selecting wrapped nodes)
		apply path (lui,moves)
			| nodeSelected_ ruleNo selection path lui moves //If the layout matches, apply the rule
				= sub ruleNo (lui,moves)
			| otherwise
				//We want to check the set of children at the time of `ruleNo`
				= updateNode_ ruleNo (applyc path) (lui,moves)

		applyc path (lui=:(LUINode type attr items changes effects),moves)
			# (items,moves) = updateChildNodes_ ruleNo (\i (item,moves) -> apply (path ++ [i]) (item,moves)) (items,moves)
			= (LUINode type attr items changes (revertEffect_ ruleNo effects),moves)

sequenceLayouts :: [LayoutRule] -> LayoutRule
sequenceLayouts subs = rule
where
	rule (LUINo ruleNo) lui = snd (foldl apply (0,lui) subs)
	where
		apply (i,lui) sub = (i + 1, sub (LUINo (ruleNo ++ [i])) lui)

//Utility functions shared by the layout rules:
setToBeInserted_ :: LUI -> LUI
setToBeInserted_ (LUINode type attr items changes effects) = LUINode type attr items {noChanges & toBeInserted = True} effects

setToBeAdded_ :: LUINo LUI -> LUI
setToBeAdded_ ruleNo (LUINode type attr items changes effects) = LUINode type attr items changes {effects & additional = ESToBeApplied ruleNo}

nextShiftID_ :: [LUI] -> LUIShiftID
nextShiftID_ items = maximum [-1:map shiftID items] + 1
where
	shiftID (LUINode _ _ _ {toBeShifted=Just x} _) = x
	shiftID (LUIShiftDestination x) = x
	shiftID _ = -1

nextMoveID_ :: LUIMoves -> LUIMoveID
nextMoveID_ moves = (foldr max 0 ('DM'.keys moves)) + 1

//Test if a certain node exists at the time of rule application
nodeExists_ :: !LUINo !LUI LUIMoves -> Bool
//Upstream nodes that no longer exist (here)
nodeExists_ ruleNo (LUINode _ _ _ {toBeRemoved=True} _) moves = False
nodeExists_ ruleNo (LUINode _ _ _ {toBeShifted=Just _} _) moves = False
//Nodes that were hidden by effects
nodeExists_ ruleNo (LUINode _ _ _ _ {hidden=ESToBeApplied hiddenBy}) moves = hiddenBy >= ruleNo
nodeExists_ ruleNo (LUINode _ _ _ _ {hidden=ESApplied hiddenBy}) moves = hiddenBy >= ruleNo
nodeExists_ ruleNo (LUINode _ _ _ _ {hidden=ESToBeUpdated _ hiddenBy}) moves = hiddenBy >= ruleNo
//Nodes that were introduced by effects
nodeExists_ ruleNo (LUINode _ _ _ _ {additional=ESToBeApplied addedBy}) moves = addedBy <= ruleNo
nodeExists_ ruleNo (LUINode _ _ _ _ {additional=ESApplied addedBy}) moves = addedBy <= ruleNo
nodeExists_ ruleNo (LUINode _ _ _ _ {additional=ESToBeRemoved _}) moves = False //Marked to be removed

nodeExists_ ruleNo (LUINode _ _ items _ {wrapper=ESToBeApplied wrappedBy}) moves | wrappedBy > ruleNo 
	= case scanToPosition_ wrappedBy 0 items moves of
		(_,_,Just wrapped) = nodeExists_ ruleNo wrapped moves //Check the wrapped child
		_ = False // The wrapped child does not exist, nothing to check
nodeExists_ ruleNo (LUINode _ _ items _ {wrapper=ESApplied wrappedBy}) moves | wrappedBy > ruleNo 
	= case scanToPosition_ wrappedBy 0 items moves of
		(_,_,Just wrapped) = nodeExists_ ruleNo wrapped moves //Check the wrapped child
		_ = False // The wrapped child does not exist, nothing to check
nodeExists_ ruleNo (LUINode _ _ items _ {wrapper=ESToBeRemoved wrappedBy}) moves //No longer wrapped
	= case scanToPosition_ wrappedBy 0 items moves of //Consider the wrapped child (that will be restored)
		(_,_,Just wrapped) = nodeExists_ ruleNo wrapped moves //Check the wrapped child
		_ = False // The wrapped child does not exist, nothing to check
nodeExists_ ruleNo (LUINode _ _ items _ {unwrapped=ESToBeApplied unwrappedBy}) moves | unwrappedBy <= ruleNo
	= case scanToPosition_ unwrappedBy 0 items moves of
		(_,_,Just unwrapped) = nodeExists_ ruleNo unwrapped moves
		_ = False
nodeExists_ ruleNo (LUINode _ _ items _ {unwrapped=ESApplied unwrappedBy}) moves | unwrappedBy <= ruleNo
	= case scanToPosition_ unwrappedBy 0 items moves of
		(_,_,Just unwrapped) = nodeExists_ ruleNo unwrapped moves
		_ = False
//Moved nodes
nodeExists_ ruleNo (LUIMoveSource moveId) moves
	= case getMovedNode_ moveId moves of
		//Already moved (so does not exist at the source)
		(LUINode _ _ _ _ {moved=ESToBeApplied movedBy}) | ruleNo > movedBy = False
		(LUINode _ _ _ _ {moved=ESApplied movedBy}) | ruleNo > movedBy = False
		(LUINode _ _ _ _ {moved=ESToBeUpdated _ movedBy}) | movedBy == ruleNo = False
		(LUINode _ _ _ _ {moved=ESToBeUpdated _ movedBy}) | ruleNo > movedBy = False
		lui = nodeExists_ ruleNo lui moves //Check the dereference node
nodeExists_ ruleNo (LUIMoveDestination moveId moveRule) moves
	= case getMovedNode_ moveId moves of
		//Not yet moved (so does not exist at the destination)
		(LUINode _ _ _ _ {moved=ESToBeApplied movedBy}) | ruleNo <= movedBy = False
		(LUINode _ _ _ _ {moved=ESApplied movedBy}) | ruleNo <= movedBy = False
		(LUINode _ _ _ _ {moved=ESToBeUpdated _ movedBy}) | movedBy == ruleNo = True 
		(LUINode _ _ _ _ {moved=ESToBeUpdated _ movedBy}) | ruleNo < movedBy = False
		lui = nodeExists_ ruleNo lui moves //Check the dereference node
nodeExists_ _ _ _ = True

//Test if a node matches a selection UI at a path is in the selection
nodeSelected_ :: LUINo UISelection UIPath LUI LUIMoves -> Bool
nodeSelected_ ruleNo (SelectByPath p) path _ moves = p === path
nodeSelected_ ruleNo (SelectByDepth n) p _ moves = length p == n
nodeSelected_ ruleNo (SelectDescendents) [_:_] _ moves = True
nodeSelected_ ruleNo (SelectDescendents) _ _ moves = False
nodeSelected_ ruleNo (SelectByType t) _ lui moves
	= fromMaybe False (selectNode_ ruleNo (\(x,_) -> getTypeAtRuleApplication_ ruleNo x === t) (lui,moves))
nodeSelected_ ruleNo (SelectByHasAttribute k) _ lui moves
	= fromMaybe False (selectNode_ ruleNo (\(x,_) -> isJust ('DM'.get k (getAttributesAtRuleApplication_ ruleNo x))) (lui,moves))
nodeSelected_ ruleNo (SelectByAttribute k p) _ lui moves
	= fromMaybe False (selectNode_ ruleNo (\(x,_) -> maybe False p ('DM'.get k (getAttributesAtRuleApplication_ ruleNo x))) (lui,moves))
nodeSelected_ ruleNo (SelectByNumChildren num) _ lui moves
	= fromMaybe False (selectNode_ ruleNo (\(LUINode _ _ items _ _,m) -> length (selectChildNodes_ ruleNo (items,m)) == num) (lui,moves))
nodeSelected_ ruleNo (SelectByContains selection) path lui moves
	= fromMaybe False (selectNode_ ruleNo
		(\(x=:LUINode _ _ items _ _,m) = nodeSelected_ ruleNo selection path x m
	                                  || or [nodeSelected_ ruleNo (SelectByContains selection) (path ++ [i]) item m
	                                        \\ item <- selectChildNodes_ ruleNo (items,m) & i <- [0..]])
		(lui,moves))

nodeSelected_ ruleNo (SelectRelative prefix sel) absolutePath lui moves
	= maybe False (\relativePath -> nodeSelected_ ruleNo sel relativePath lui moves) (removePrefix prefix absolutePath)
where
	removePrefix [] psb = Just psb
	removePrefix [pa:psa] [pb:psb] = if (pa == pb) (removePrefix psa psb) Nothing
	removePrefix _ _ = Nothing
nodeSelected_ ruleNo (SelectNone) _ _ moves = False
nodeSelected_ ruleNo (SelectAND sell selr) path ui moves = nodeSelected_ ruleNo sell path ui moves && nodeSelected_ ruleNo selr path ui moves
nodeSelected_ ruleNo (SelectOR sell selr) path ui moves = nodeSelected_ ruleNo sell path ui moves || nodeSelected_ ruleNo selr path ui moves
nodeSelected_ ruleNo (SelectNOT sel) path ui moves = not (nodeSelected_ ruleNo sel path ui moves)
nodeSelected_ ruleNo _ _ _ moves = False

matchAttributeKey_ :: UIAttributeSelection UIAttributeKey -> Bool
matchAttributeKey_ (SelectAll) _ = True
matchAttributeKey_ (SelectKeys keys) k = isMember k keys

selectAttributes_ :: UIAttributeSelection UIAttributes -> UIAttributes
selectAttributes_ selection attr = case selection of
	SelectAll         = attr
	(SelectKeys keys) = 'DM'.fromList [a \\ a=:(k,_) <- 'DM'.toList attr | isMember k keys]

//Lookup an item by position in a list set of children.
//This lookup takes into consideration that it is possible that:
//- items were removed or shifted by an upstream change
//- items have been hidden by earlier rules
//- items were added by earlier rules
//It ignores nodes that are not relevant such as:
//- items that were added by later rules

scanToPosition_ :: LUINo Int [LUI] LUIMoves -> (Int,Bool,Maybe LUI)
scanToPosition_ ruleNo position items moves = scan position 0 items
where
	scan r i [] = (i, r == 0, Nothing) //Scanned beyond the list
	scan r i [x:xs]
		| not (nodeExists_ ruleNo x moves) = scan r (i + 1) xs //Skip
		| r == 0 = (i,True,Just x)
		| otherwise = scan (r - 1) (i + 1) xs

isAdditional_ :: LUI -> Bool
isAdditional_ (LUINode _ _ _ _ {additional=ESToBeApplied _}) = True
isAdditional_ (LUINode _ _ _ _ {additional=ESApplied _}) = True
isAdditional_ (LUINode _ _ _ _ {additional=ESToBeRemoved _}) = True
isAdditional_ (LUIMoveDestination _ _) = True  //TODO: Create a test for this case
isAdditional_ _ = False

isShifted_ :: LUI -> Bool
isShifted_ (LUINode _ _ _ {LUIChanges|toBeShifted} _) = isJust toBeShifted
isShifted_ _ = False

isUnwrapped_ :: LUI -> Bool
isUnwrapped_ (LUINode _ _ _ _ {LUIEffects|unwrapped=ESToBeApplied _}) = True
isUnwrapped_ (LUINode _ _ _ _ {LUIEffects|unwrapped=ESApplied _}) = True
isUnwrapped_ _ = False

isHidden_ :: LUI -> Bool
isHidden_ (LUINode _ _ _ _ {LUIEffects|hidden=ESToBeApplied _}) = True
isHidden_ (LUINode _ _ _ _ {LUIEffects|hidden=ESApplied _}) = True
isHidden_ _ = False

lookupShiftSource_ :: Int [LUI] -> (Int,LUI)
lookupShiftSource_ shiftId items = lookup 0 items 
where
	lookup _ [] = abort "lookupShiftSource_: could not find source"
	lookup i [x:xs] = if (isSource x) (i,x) (lookup (i+1) xs) 
		
	isSource (LUINode _ _ _ {toBeShifted = Just sourceId} _) = sourceId == shiftId
	isSource _ = False

selectNode_ :: LUINo ((LUI,LUIMoves) -> a) (LUI,LUIMoves) -> Maybe a
selectNode_ ruleNo selector (lui,moves) = fst3 (processNode_ ruleNo fun (lui,moves))
where
	fun (lui,moves) = (selector (lui,moves),lui,moves)

selectChildNodes_ :: LUINo ([LUI],LUIMoves) -> [LUI] 
selectChildNodes_ ruleNo (items,moves)
	# (selection,_,moves) = processChildNodes_ ruleNo fun ([],items,moves)
	= map snd (sortBy fstEl selection)
where
	fun i (acc,item,moves) = ([(i,item):acc],item,moves)
	fstEl (x,_) (y,_) = x < y	

selectSubNode_ :: LUINo UIPath (LUI,LUIMoves) -> Maybe LUI 
selectSubNode_ ruleNo [] (lui,moves)= selectNode_ ruleNo fst (lui,moves)
selectSubNode_ ruleNo [s:ss] (lui,moves) = maybe Nothing id (selectNode_ ruleNo select (lui,moves))
where
	select (LUINode type attr items changes effects,moves) 
		# items = selectChildNodes_ ruleNo (items,moves)
		| s < 0 || s >= length items = Nothing
		| otherwise = selectSubNode_ ruleNo ss (items !! s, moves)

updateNode_ :: LUINo ((LUI,LUIMoves) -> (LUI,LUIMoves)) (LUI,LUIMoves) -> (LUI,LUIMoves)
updateNode_ ruleNo update (lui,moves)
	# (_,lui,moves) = processNode_ ruleNo fun (lui,moves)
	= (lui,moves)
where
	fun (item,moves) 
		# (item,moves) = update (item,moves)
		= ((),item,moves)

updateChildNodes_ :: LUINo (Int (LUI,LUIMoves) -> (LUI,LUIMoves)) ([LUI],LUIMoves) -> ([LUI],LUIMoves)
updateChildNodes_ ruleNo update (items,moves)
	# (_,items,moves) = processChildNodes_ ruleNo fun ((),items,moves)
	= (items, moves)
where
	fun i (s,items,moves) 
		# (items,moves) = update i (items,moves)
		= (s,items,moves)

updateSubNode_ :: LUINo UIPath ((LUI,LUIMoves) -> (LUI,LUIMoves)) (LUI,LUIMoves) -> (LUI,LUIMoves)
updateSubNode_ ruleNo [] update (lui,moves) = updateNode_ ruleNo update (lui,moves)
updateSubNode_ ruleNo [s:ss] update (lui,moves) = updateNode_ ruleNo apply (lui,moves)
where
	apply (LUINode type attr items changes effects,moves)
		# (items,moves) = updateChildNodes_ ruleNo applyc (items,moves)
		= (LUINode type attr items changes effects,moves)

	applyc i (lui,moves) = if (i == s) (updateSubNode_ ruleNo ss update (lui,moves)) (lui,moves)

processNode_ :: LUINo ((LUI,LUIMoves) -> (a,LUI,LUIMoves)) (LUI,LUIMoves) -> (Maybe a, LUI, LUIMoves)
processNode_ ruleNo fun (lui,moves) = case lui of
	//When a move source exists (it is moved by a later rule), we update the referenced node
	(LUIMoveSource moveId)
		# movedItem = getMovedNode_ moveId moves
		= case getMovedBy_ movedItem of
			Just movedBy 
				| movedBy >= ruleNo
					# (result,movedItem,moves) = fun (movedItem,moves)
					= (Just result,lui, putMovedNode_ moveId movedItem moves)
				| movedBy < ruleNo //Do nothing, the node has been moved somewhere else
					= (Nothing,lui,moves)
			//The node is no longer moved, it will be restored to this location
			Nothing
				# (result,movedItem,moves) = fun (movedItem,moves)
				= (Just result ,lui, putMovedNode_ moveId movedItem moves)
	//When an item was moved here (by an earlier rule), we update the referenced node
	(LUIMoveDestination moveId moveRule) 
		# movedItem = getMovedNode_ moveId moves
		= case getMovedBy_ movedItem of
			Just movedBy 
				| movedBy >= ruleNo //No nothing, the node has not been moved yet
					= (Nothing,lui,moves)
				| movedBy < ruleNo //The node has been moved, Update the referenced node
					# (result,movedItem,moves) = fun (movedItem,moves)
					= (Just result, lui, putMovedNode_ moveId movedItem moves)
			//The node is no longer moved, it will be removed from this destination
			Nothing
				= (Nothing,lui,moves)
	//When an item is scheduled to be replaced, update the replacement
	(LUINode type attr items changes=:{toBeReplaced=Just replacement} effects)
		# (result,replacement,moves) = processNode_ ruleNo fun (replacement,moves)
		= (result, LUINode type attr items {changes & toBeReplaced=Just replacement} effects, moves)
	//TODO: Refactor the different cases of wrapped nodes. there is some overlap
	//When an item is wrapped by a later rule, we update the wrapped child instead of the wrapper 
	(LUINode type attr items changes effects=:{wrapper=ESApplied wrappedBy})
		//Not yet wrapped, process the wrapped item
		| wrappedBy > ruleNo
			= case scanToPosition_ wrappedBy 0 items moves of
				(index,True,Just wrapped)	
					# (result,wrapped,moves) = processNode_ ruleNo fun (wrapped,moves) 
					= (result,LUINode type attr (updateAt index wrapped items) changes effects, moves)
				_ = (Nothing,lui,moves)
		| otherwise
			# (result,lui,moves) = fun (lui,moves)
			= (Just result,lui,moves)
	//When an item is wrapped by a later rule, we update the wrapped child instead of the wrapper
	(LUINode type attr items changes effects=:{wrapper=ESToBeApplied wrappedBy})
		//Not yet wrapped, process the wrapped item
		| wrappedBy > ruleNo
			= case scanToPosition_ wrappedBy 0 items moves of
				(index,True,Just wrapped)
					# (result,wrapped,moves) = processNode_ ruleNo fun (wrapped,moves) 
					= (result, LUINode type attr (updateAt index wrapped items) changes effects, moves)
				_ = (Nothing,lui,moves)
		| otherwise
			# (result,lui,moves) = fun (lui,moves)
			= (Just result,lui,moves)
	//Similarly, when the wrapping is set to be removed, we need to update the wrapped child
	(LUINode type attr items changes effects=:{wrapper=ESToBeRemoved wrappedBy})
		= case scanToPosition_ wrappedBy 0 items moves of
			(index,True,Just wrapped)	
				# (result,wrapped,moves) = processNode_ ruleNo fun (wrapped,moves)
				= (result,LUINode type attr (updateAt index wrapped items) changes effects, moves)
			_ = (Nothing,lui,moves)
	//When a node is unwrapped, we have to process the first inner node (if it exists...)
	(LUINode type attr items changes effects=:{unwrapped=ESToBeApplied unwrappedBy}) | unwrappedBy < ruleNo 
		= case scanToPosition_ ruleNo 0 items moves of
			(index,True,Just item) 
				# (result,item,moves) = processNode_ ruleNo fun (item,moves)
				= (result,LUINode type attr (updateAt index item items) changes effects, moves)
			_
				= (Nothing,lui,moves)					
	//TODO: Refactor to reduce overlap in cases
	(LUINode type attr items changes effects=:{unwrapped=ESApplied unwrappedBy}) | unwrappedBy < ruleNo
		 //Already wrapped, process the inner node
		= case scanToPosition_ ruleNo 0 items moves of
			(index,True,Just item) 
				# (result,item,moves) = processNode_ ruleNo fun (item,moves)
				= (result,LUINode type attr (updateAt index item items) changes effects, moves)
			_
				= (Nothing,lui,moves)					
	//Default case: Just apply the update function
	_ 
		# (result,lui,moves) = fun (lui,moves)
		= (Just result,lui,moves)

processChildNodes_ :: LUINo (Int (a,LUI,LUIMoves) -> (a,LUI,LUIMoves)) (a,[LUI],LUIMoves) -> (a,[LUI],LUIMoves) 
processChildNodes_ ruleNo fun (state,items,moves) = processItems (indexShiftDestinations items moves) state items moves
where
	indexShiftDestinations items moves = snd (foldl index (0,'DM'.newMap) items) 
	where
		index (i,positions) item
			| nodeExists_ ruleNo item moves = case item of
				(LUIShiftDestination shiftId) = (i + 1, 'DM'.put shiftId i positions)
				_                             = (i + 1, positions)
			| otherwise                                  = (i, positions)

	processItems shiftDestinations state items moves
		# (_,state,items,moves) = foldl updateItem (0,state,[],moves) items
		= (state,reverse items, moves)
	where
		updateItem (i,state,acc,moves) item 
			| nodeExists_ ruleNo item moves
				# (state,item,moves) = case item of 
					//For shifted items we update the source
					(LUIShiftDestination shiftId) = (state,item,moves)
					_                             = fun i (state,item,moves)
				= (i + 1, state, [item:acc], moves)
			| otherwise
				# (state,item,moves) = case item of
					//For shifted items we update the source (using the index of the destination)
					(LUINode _ _ _ {toBeShifted=Just shiftId} _)
						//If we did not index the destination, apparently it no longer exists
						= maybe (state,item,moves) (\index -> fun index (state,item,moves)) ('DM'.get shiftId shiftDestinations)
					_ = (state,item,moves)
				= (i, state,[item:acc], moves)

getMovedNode_ :: LUIMoveID LUIMoves -> LUI
getMovedNode_ moveId moves = case 'DM'.get moveId moves of
	Nothing = abort ("Get: Unknown id " +++ toString moveId +++ " in moved items\n")
	Just item = item

putMovedNode_ :: LUIMoveID LUI LUIMoves -> LUIMoves
putMovedNode_ moveId node moves = 'DM'.put moveId node moves

delMovedNode_ :: LUIMoveID LUIMoves -> LUIMoves
delMovedNode_ moveId moves = 'DM'.del moveId moves

getMovedBy_ :: LUI -> Maybe LUINo
getMovedBy_ (LUINode _ _ _ _ {LUIEffects|moved=ESApplied ruleNo}) = Just ruleNo
getMovedBy_ (LUINode _ _ _ _ {LUIEffects|moved=ESToBeApplied ruleNo}) = Just ruleNo
getMovedBy_ (LUINode _ _ _ _ {LUIEffects|moved=ESToBeUpdated _ ruleNo}) = Just ruleNo
getMovedBy_ _ = Nothing

getTypeAtRuleApplication_ :: LUINo LUI -> UIType
getTypeAtRuleApplication_ ruleNo (LUINode _ _ _ _ {overwrittenType=ESToBeApplied (appliedAt,type)}) | appliedAt < ruleNo = type
getTypeAtRuleApplication_ ruleNo (LUINode _ _ _ _ {overwrittenType=ESApplied (appliedAt,type)}) | appliedAt < ruleNo = type
getTypeAtRuleApplication_ ruleNo (LUINode _ _ _ _ {overwrittenType=ESToBeUpdated _ (appliedAt,type)}) | appliedAt < ruleNo = type
getTypeAtRuleApplication_ ruleNo (LUINode type _ _ _ _) = type 

getAttributesAtRuleApplication_ :: LUINo LUI -> UIAttributes
getAttributesAtRuleApplication_ ruleNo (LUINode _ attr _ changes effects=:{overwrittenAttributes,hiddenAttributes})
	//Consider upstream changes
	# attr = applyAttributeChanges_ changes attr 
	//Consider overwritten attributes
	# attr = foldl overwrite attr ('DM'.toList overwrittenAttributes)
	//Consider hidden attributes
	# attr = foldl hide attr ('DM'.toList hiddenAttributes)
	= attr	
where
	overwrite attr (key,ESToBeApplied (appliedAt,value)) | appliedAt < ruleNo = 'DM'.put key value attr
	overwrite attr (key,ESApplied (appliedAt,value)) | appliedAt < ruleNo = 'DM'.put key value attr
	overwrite attr (key,ESToBeUpdated _ (appliedAt,value)) | appliedAt < ruleNo = 'DM'.put key value attr
	overwrite attr _ = attr

	hide attr (key,ESToBeApplied appliedAt) | appliedAt < ruleNo = 'DM'.del key attr
	hide attr (key,ESApplied appliedAt) | appliedAt < ruleNo = 'DM'.del key attr
	hide attr (key,ESToBeUpdated _ appliedAt) | appliedAt < ruleNo = 'DM'.del key attr
	hide attr _ = attr

applyTypeEffect_ :: UIType LUIEffects -> UIType
applyTypeEffect_ ltype effects=:{overwrittenType} = case overwrittenType of
	ESToBeApplied (_,type) = type
	ESApplied (_,type) = type
	ESToBeUpdated _ (_,type) = type
	_ = ltype

applyAttributeChanges_ :: LUIChanges UIAttributes -> UIAttributes 
applyAttributeChanges_ {setAttributes,delAttributes} attr
	= 'DM'.delList ('DS'.toList delAttributes) ('DM'.union setAttributes attr)

applyAttributeEffects_ :: UIAttributes LUIEffects -> UIAttributes
applyAttributeEffects_ attr effects=:{overwrittenAttributes,hiddenAttributes} 
	# attr = foldl overwrite attr ('DM'.toList overwrittenAttributes)
	# attr = foldl hide attr ('DM'.toList hiddenAttributes)
	= attr 
where
	overwrite attr (key,ESNotApplied) = attr
	overwrite attr (key,ESToBeApplied (ruleNo,value)) = 'DM'.put key value attr
	overwrite attr (key,ESApplied (ruleNo,value)) = 'DM'.put key value attr
	overwrite attr (key,ESToBeUpdated _ (ruleNo,value)) = 'DM'.put key value attr
	overwrite attr (key,ESToBeRemoved _) = attr

	hide attr (key,ESNotApplied) = attr
	hide attr (key,ESToBeApplied ruleNo) = 'DM'.del key attr
	hide attr (key,ESApplied ruleNo) = 'DM'.del key attr
	hide attr (key,ESToBeUpdated _ ruleNo) = 'DM'.del key attr
	hide attr (key,ESToBeRemoved _) = attr

overwriteAttribute_ :: LUINo UIAttribute (Map UIAttributeKey (LUIEffectStage (LUINo,JSONNode))) -> (Map UIAttributeKey (LUIEffectStage (LUINo,JSONNode)))
overwriteAttribute_ ruleNo (key,value) overwrittenAttributes
	# override = case 'DM'.get key overwrittenAttributes of
		//Not set yet
		Nothing = ESToBeApplied (ruleNo,value)
		Just (ESNotApplied) = ESToBeApplied (ruleNo,value)
		Just (ESToBeApplied (curRule,curValue))
			| curValue == value = ESToBeApplied (curRule,curValue)
			| otherwise         = ESToBeApplied (ruleNo,value)
		//Already set,
		Just (ESApplied (curRule,curValue))
			| curValue == value = ESApplied (curRule,curValue)
			| otherwise         = ESToBeUpdated (curRule,curValue) (ruleNo,value) 
		Just (ESToBeUpdated (curRule,curValue) _)
			| curValue == value = ESApplied (ruleNo, value)
			| otherwise         = ESToBeUpdated (curRule,curValue) (ruleNo,value)
		Just (ESToBeRemoved (curRule,curValue))
			| curValue == value = ESApplied (ruleNo,value)
			| otherwise         = ESToBeUpdated (curRule,curValue) (ruleNo,value)
	= 'DM'.put key override overwrittenAttributes

hideAttribute_ :: LUINo (UIAttributeKey -> Bool) UIAttributeKey (Map UIAttributeKey (LUIEffectStage LUINo)) -> (Map UIAttributeKey (LUIEffectStage LUINo))
hideAttribute_ ruleNo condition key hiddenAttributes
	| isAlreadyHidden key hiddenAttributes
		= if (condition key)
			('DM'.put key (ESApplied ruleNo) hiddenAttributes)
			('DM'.put key (ESToBeRemoved ruleNo) hiddenAttributes)
	| otherwise
		= if (condition key)
			('DM'.put key (ESToBeApplied ruleNo) hiddenAttributes)
			hiddenAttributes
where
	isAlreadyHidden key attr = case 'DM'.get key attr of
		Just (ESApplied _) = True
		Just (ESToBeUpdated _ _) = True
		Just (ESToBeRemoved _) = True
		_ = False

//TODO Effects on types and attributes are not yet reverted
revertEffect_ :: LUINo LUIEffects -> LUIEffects 
revertEffect_ ruleId effects=:{LUIEffects|additional,hidden,moved,wrapper,unwrapped}
	| additional === (ESApplied ruleId) = {LUIEffects|effects & additional = ESToBeRemoved ruleId}
	| additional === (ESToBeApplied ruleId) = {LUIEffects|effects & additional = ESNotApplied}
	| hidden === (ESApplied ruleId) = {LUIEffects|effects & hidden = ESToBeRemoved ruleId}
	| hidden === (ESToBeApplied ruleId) = {LUIEffects|effects & hidden = ESNotApplied}
	| wrapper === (ESApplied ruleId) = {LUIEffects|effects & wrapper = ESToBeRemoved ruleId}
	| wrapper === (ESToBeApplied ruleId) = {LUIEffects|effects & wrapper = ESNotApplied}
	| unwrapped === (ESApplied ruleId) = {LUIEffects|effects & unwrapped = ESToBeRemoved ruleId}
	| unwrapped === (ESToBeApplied ruleId) = {LUIEffects|effects & unwrapped = ESNotApplied}
	= effects

/*
* Once layout rules have annotated their effects, the change that has to be applied downstream
* can be extracted from the buffered structure. The result of doing this is both the combination of
* the ui change that has to be applied downstream, and a new version of the buffered tree that
* in which all pending changes and effects have been applied.
*/
extractDownstreamChange :: (LUI,LUIMoves) -> (!UIChange,!(LUI,LUIMoves))
extractDownstreamChange (lui,moves) 
	# mbChildChange = extractDownstreamChildChange lui moves
	# (mbLui,moves)  = confirmChangesAndEffects_ (lui, moves)
	= case (mbChildChange,mbLui) of
		(Just (InsertChild ui), Just lui)     = (ReplaceUI ui,(lui,moves))
		(Just RemoveChild, Just lui)          = (ReplaceUI (UI UIEmpty 'DM'.newMap []),(lui,moves))
		(Just (ChangeChild change), Just lui) = (change,(lui,moves))
		_                                     = (NoChange,(lui,moves)) //TODO: This case should not really happen

//For each node we need to extract one of the following changes:
// 1. Just (InsertChild x)        - The node did not exist client-side, but does now
// 2. Just (RemoveChild)          - The node existed previously but should not
// 3. Just (ChangeChild x)        - The node existed previously and still does
// 4. Nothing                     - The node did not exist client-side and still does not...
extractDownstreamChildChange :: LUI LUIMoves -> Maybe UIChildChange
extractDownstreamChildChange lui moves
	# (mbMoveId,mbDestinationRule,current=:LUINode _ _ items changes effects=:{moved}) = case lui of
		(LUINode _ _ _ _ _)                  = (Nothing, Nothing, lui)
		(LUIMoveSource moveId)               = (Just moveId, Nothing, getMovedNode_ moveId moves)
		(LUIMoveDestination moveId moveRule) = (Just moveId, Just moveRule, getMovedNode_ moveId moves)
	| isTemporaryDestination_ moved mbDestinationRule
		= Nothing
	//Cases where a new ui should be inserted
	| needsInsert_ changes effects mbDestinationRule
		= determineInsert_ lui current moves
	//Cases where we need a full replace
	| needsRemove_ changes effects mbDestinationRule
		= determineRemove_ lui current moves
	//Cases where a full replace is needed
	| needsReplace_ items changes effects
		= determineReplace_ lui current moves
	| otherwise
		= determineChange_ lui current moves
where				
	isTemporaryDestination_ (ESToBeUpdated curRule newRule) (Just destRule) = destRule <> curRule && destRule <> newRule
	isTemporaryDestination_ (ESApplied newRule) (Just destRule) = destRule <> newRule
	isTemporaryDestination_ _ _ = False

	needsInsert_ {toBeInserted=True} _ _ = True
	needsInsert_ _ {additional=ESToBeApplied _} _ = True
	needsInsert_ _ {hidden=ESToBeRemoved _} _ = True
	needsInsert_ _ {moved=ESToBeApplied _} (Just _) = True
	needsInsert_ _ {moved=ESToBeRemoved _} Nothing = True
	needsInsert_ _ {moved=ESToBeUpdated _ ruleNo} (Just destNo) = ruleNo == destNo
	needsInsert_ _ _ _ = False	

	needsRemove_ {toBeRemoved=True} _ _ = True
	needsRemove_ _ {additional=ESToBeRemoved _} _ = True
	needsRemove_ _ {hidden=ESToBeApplied _} _ = True
	needsRemove_ _ {moved=ESToBeApplied _} Nothing = True
	needsRemove_ _ {moved=ESToBeRemoved _} (Just _) = True
	needsRemove_ _ {moved=ESToBeUpdated ruleNo _} (Just destNo) = ruleNo == destNo
	needsRemove_ _ _ _ = False	

	needsReplace_ _ {toBeReplaced=Just _} _ = True
	needsReplace_ _ _ {overwrittenType=ESToBeApplied _} = True
	needsReplace_ _ _ {overwrittenType=ESToBeUpdated _ _} = True
	needsReplace_ _ _ {overwrittenType=ESToBeRemoved _} = True
	needsReplace_ _ _ {wrapper=ESToBeApplied _} = True
	needsReplace_ _ _ {wrapper=ESToBeRemoved _} = True
	needsReplace_ _ _ {unwrapped=ESToBeApplied _} = True
	needsReplace_ _ _ {unwrapped=ESToBeRemoved _} = True
	needsReplace_ items _ {unwrapped=ESApplied _} = differentFirstChild items
	needsReplace_ _ _ _ = False

	//When the parent is unwrapped, we may need to replace the ui if another child ends up at position 0
	differentFirstChild [LUINode _ _ _ {toBeInserted=True} _:_]= True
	differentFirstChild [LUINode _ _ _ {toBeRemoved=True} _:_]= True
	differentFirstChild [LUINode _ _ _ {toBeShifted=Just _} _:_]= True
	differentFirstChild [LUIShiftDestination _:_]= True
	differentFirstChild items = False

	//TODO: make 'moves' an explicit argument
	existsDownstream_ _ (LUINode _ _ _ _ {LUIEffects|hidden=ESApplied _}) = False
	existsDownstream_ _ (LUINode _ _ _ _ {LUIEffects|hidden=ESToBeRemoved _}) = False
	existsDownstream_ (LUIMoveSource _) (LUINode _ _ _ _ {LUIEffects|moved=ESApplied _}) = False
	existsDownstream_ (LUIMoveSource _) (LUINode _ _ _ _ {LUIEffects|moved=ESToBeUpdated _ _}) = False
	existsDownstream_ ref (LUINode _ _ items _ {LUIEffects|unwrapped=ESApplied unwrappedBy})
		= case scanToPosition_ unwrappedBy 0 items moves of
			(_,_,Just item) = existsDownstream_ ref item
			_                    = False
	existsDownstream_ _ _ = True

 	determineInsert_ node current moves = fmap InsertChild (extractUIWithEffects_ (current,moves))

	determineRemove_ node current=:(LUINode _ _ _ changes effects) moves	
		| existsDownstream_ node current = Just RemoveChild
		| otherwise = Nothing

	determineReplace_ node current moves
		= case extractUIWithEffects_ (current,moves) of
			(Just ui)
				| existsDownstream_ node current = Just (ChangeChild (ReplaceUI ui))
				| otherwise                 = Just (InsertChild ui)
			(Nothing)
				| existsDownstream_ node current = Just RemoveChild
				| otherwise = Nothing

	determineChange_ node current=:(LUINode type attr items changes effects=:{unwrapped}) moves
		//Determine changes to attributes
		# (attributeChanges,_,_,_) = extractAttributeChanges attr changes effects
		//Determine changes to children
		# (childShifts,items) = extractChildShifts items
		# childUpdates = extractChildInsertsAndRemoves items moves
		//Combine into one change
		# change = if (unwrapped =: (ESApplied _))
			//Check if there is an update to the first child
			(case [c \\ (i,ChangeChild c) <- childUpdates] of
				[c:_] = c
				_     = NoChange
			)
			(case (attributeChanges,childShifts,childUpdates) of
				([],[],[]) = NoChange
				_          = ChangeUI attributeChanges (childShifts ++ childUpdates)
			)
		| existsDownstream_ node current
			= Just (ChangeChild change)
		| otherwise
			= Nothing

//TODO REMOVE CONFIRMING
extractAttributeChanges :: UIAttributes LUIChanges LUIEffects -> ([UIAttributeChange],UIAttributes,LUIChanges,LUIEffects)
extractAttributeChanges attr changes=:{setAttributes,delAttributes} effects=:{overwrittenAttributes,hiddenAttributes}
	//Apply changes to the attributes
	# (attr,attrChanges)
		= foldl (applySetAttribute overwrittenAttributes hiddenAttributes) (attr,[]) ('DM'.toList setAttributes)
	# (attr,attrChanges)
		= foldl (applyDelAttribute overwrittenAttributes hiddenAttributes) (attr,attrChanges) ('DS'.toList delAttributes)
	//Apply remaining effects (these no longer affect the stored attributes)
	# (attrChanges,overwrittenAttributesList) = foldl (applyOverrideAttribute attr) (attrChanges,[]) ('DM'.toList overwrittenAttributes)
	# (attrChanges,hiddenAttributesList) = foldl (applyHideAttribute attr) (attrChanges,[]) ('DM'.toList hiddenAttributes)
	= (reverse attrChanges
	  ,attr
	  ,{changes & setAttributes = 'DM'.newMap, delAttributes = 'DS'.newSet}
	  ,{effects
	   & overwrittenAttributes = 'DM'.fromList overwrittenAttributesList
	   , hiddenAttributes = 'DM'.fromList hiddenAttributesList
	   })
where
	applySetAttribute overwrittenAttributes hiddenAttributes (attr,changes) (key,value)
		//If an attribute has an effect applied, we don't want to change it downstream
		| isOverwritten key overwrittenAttributes || isHidden key hiddenAttributes
			= ('DM'.put key value attr,changes)
		| otherwise
			= ('DM'.put key value attr,[SetAttribute key value:changes])
	applyDelAttribute overwrittenAttributes hiddenAttributes (attr,changes) key
		//If an attribute was overwritten, we don't want to delete it downstream
		| isOverwritten key overwrittenAttributes
			= ('DM'.del key attr,changes)
		//If an attribute is hidden we don't need to delete it downstream (it is not shown there)
		| isHidden key hiddenAttributes
			= ('DM'.del key attr,changes)
		| otherwise
			= ('DM'.del key attr,[DelAttribute key:changes])

	applyOverrideAttribute attr (attrChanges, overrides) (key,ESNotApplied)
		= (attrChanges, overrides) //Remove from overrides (they have no meaning here)
	applyOverrideAttribute attr (attrChanges, overrides) (key,ESToBeApplied (ruleNo,value))
		= ([SetAttribute key value:attrChanges], [(key,ESApplied (ruleNo,value)):overrides])
	applyOverrideAttribute attr (attrChanges, overrides) (key,ESApplied (ruleNo,value)) //Already applied
		= (attrChanges, [(key,ESApplied (ruleNo,value)):overrides])
	applyOverrideAttribute attr (attrChanges, overrides) (key,ESToBeUpdated _ (ruleNo,value))
		= ([SetAttribute key value:attrChanges], [(key,ESApplied (ruleNo,value)):overrides])
	applyOverrideAttribute attr (attrChanges, overrides) (key,ESToBeRemoved _) //Either restore the original, or remove the attribute 
		= case 'DM'.get key attr of
			Nothing = ([DelAttribute key:attrChanges],overrides)
			Just value = ([SetAttribute key value:attrChanges],overrides)

	applyHideAttribute attr (attrChanges, hidden) (key,ESNotApplied)
		= (attrChanges,hidden)
	applyHideAttribute attr (attrChanges, hidden) (key,ESToBeApplied ruleNo)
		= ([DelAttribute key:attrChanges],[(key,ESApplied ruleNo):hidden])
	applyHideAttribute attr (attrChanges, hidden) (key,ESApplied ruleNo)
		= (attrChanges,[(key,ESApplied ruleNo):hidden])
	applyHideAttribute attr (attrChanges, hidden) (key,ESToBeUpdated _ ruleNo)
		= (attrChanges,[(key,ESApplied ruleNo):hidden])
	applyHideAttribute attr (attrChanges, hidden) (key,ESToBeRemoved _)
		= case 'DM'.get key attr of
			//Original attribute no longer exists, nothing to do
			Nothing = (attrChanges,hidden)
			//Original attribute still exists, restore its value
			Just value = ([SetAttribute key value:attrChanges],hidden)

	isHidden key hiddenAttributes = check ('DM'.toList hiddenAttributes)
	where
		check [] = False
		check [(hiddenKey,ESToBeApplied _):_] | hiddenKey == key = True
		check [(hiddenKey,ESApplied _):_] | hiddenKey == key = True
		check [(hiddenKey,ESToBeUpdated _ _):_] | hiddenKey == key = True
		check [_:xs] = check xs

	isOverwritten key overwrittenAttributes = check ('DM'.toList overwrittenAttributes)
	where
		check [] = False
		check [(hiddenKey,ESToBeApplied _):_] | hiddenKey == key = True
		check [(hiddenKey,ESApplied _):_] | hiddenKey == key = True
		check [(hiddenKey,ESToBeUpdated _ _):_] | hiddenKey == key = True
		check [_:xs] = check xs

//Important: Shifts are done before inserts and removes
//           so we ignore items that are not yet inserted, but still
//           count items that are to be removed
extractChildShifts :: [LUI] -> ([(Int,UIChildChange)],[LUI])
extractChildShifts items = extract 0 [] items
where
	extract i acc [] = ([],reverse acc)
	extract i acc [x=:(LUINode _ _ _ {toBeShifted = Just shiftID} _):xs]
		//First look back for the destination
		= case findAndReplaceDestination shiftID x True acc of
			(Left d, acc)
				# (changes,items) = extract (i + 1) acc xs
				= ([(i,MoveChild d):changes],items)
			(Right n, acc)
				//Look forward for the destination
				= case findAndReplaceDestination shiftID x False xs of
					(Left d, xs)
						# (changes,items) = extract i acc xs
						= ([(i,MoveChild (n + d)):changes],items)
					(Right _, xs)
						= abort "Could not find a destination for a shifted UI element"
	//Ignore not yet inserted nodes and shift destinations
	extract i acc [x=:(LUINode _ _ _ {toBeInserted=True} _):xs] = extract i [x:acc] xs
	extract i acc [x=:(LUIShiftDestination _):xs] = extract i [x:acc] xs
	//Continue
	extract i acc [x:xs] = extract (i + 1) [x:acc] xs

	findAndReplaceDestination shiftID x backwards items = find start items
	where
		numItems = adjustedLength items
		start = if backwards numItems 0

		find i [] = (Right numItems,[])
		find i [(LUIShiftDestination matchId):xs] | matchId == shiftID
			# x = resetToBeShifted x
			= (Left i,[x:xs])
		//Ignore not yet inserted nodes and shift destinations
		find i [x=:(LUIShiftDestination _):xs]
			# (mbd,xs) = find i xs
			= (mbd,[x:xs])
		find i [x=:(LUINode _ _ _ {toBeInserted=True} _):xs]
			# (mbd,xs) = find i xs
			= (mbd,[x:xs])
		//Just keep searching
		find i [x:xs]
			# (mbd,xs) = find (if backwards (i - 1) (i + 1)) xs
			= (mbd,[x:xs])

		adjustedLength items = count visible items
		where
			count pred list = foldr (\x n -> if (pred x) (n + 1) n) 0 list

			visible (LUIShiftDestination _) = False
			visible (LUINode _ _ _ {toBeInserted=True} _) = False
			visible _ = True

extractChildInsertsAndRemoves :: [LUI] LUIMoves -> [(Int,UIChildChange)]
extractChildInsertsAndRemoves items moves = extract 0 items moves
where
	extract i [] moves = []
	extract i [x:xs] moves = case extractDownstreamChildChange x moves of
		(Just (InsertChild ui)) = [(i,InsertChild ui):extract (i + 1) xs moves]
		(Just RemoveChild) = [(i,RemoveChild):extract i xs moves]
		(Just (ChangeChild NoChange)) = extract (i + 1) xs moves
		(Just (ChangeChild c)) = [(i,ChangeChild c):extract (i + 1) xs moves]
		Nothing = extract i xs moves

extractUIWithEffects_ :: (LUI,LUIMoves) -> Maybe UI
extractUIWithEffects_ (LUINode ltype lattr litems changes=:{toBeReplaced=Just replacement} effects, moves)
	= extractUIWithEffects_ (replacement,moves)
extractUIWithEffects_ (LUINode ltype lattr litems changes effects=:{wrapper=ESToBeRemoved _}, moves)
	= case dropWhile isAdditional_ litems of
		[wrapped:_] = extractUIWithEffects_ (wrapped,moves)
		_           = abort "extractUIWithEffects_: Wrapped item is missing"
extractUIWithEffects_ (lui=:LUINode ltype lattr litems changes=:{setAttributes,delAttributes} effects=:{overwrittenType},moves)
	//Determin type
	# type = applyTypeEffect_ ltype effects
	//Determine attributes and apply attribute effects
	# attr = applyAttributeEffects_ (applyAttributeChanges_ changes lattr) effects
	//Recursively extract children with effects 
	# items = extractChildUIsWithEffects_ litems moves
	//Determine the final ui
	| isHidden_ lui = Nothing
	| isUnwrapped_ lui = case items of
		[item:_] = Just item
		_        = Nothing
	| otherwise = Just (UI type attr items)

extractUIWithEffects_ _ = abort "extractUIWithEffects_: can only extract UI from LUINodes"

extractChildUIsWithEffects_ :: [LUI] LUIMoves -> [UI]
extractChildUIsWithEffects_ litems moves = foldr extract [] litems
where
	shiftSources = foldr collect 'DM'.newMap litems
	where
		collect n=:(LUINode type attr items changes=:{toBeShifted=Just shiftId} effects) sources
			= 'DM'.put shiftId (LUINode type attr items {changes & toBeShifted = Nothing} effects) sources
		collect n sources = sources

	//Move shifted items to the right place
	extract (LUIShiftDestination shiftID) items
		= maybe items (\item -> extract item items) ('DM'.get shiftID shiftSources)
	//Skip over items that still need to be shifted
	extract litem=:(LUINode _ _ _ {toBeShifted=Just _} _) items
		= items
	//Items that need to be removed
	extract litem=:(LUINode _ _ _ {LUIChanges|toBeRemoved=True} _) items
		= items
	extract litem=:(LUINode _ _ _ {toBeReplaced=Nothing} {additional = ESToBeRemoved _}) items
		= items
	//Dealing with moved nodes
	extract litem=:(LUIMoveSource moveId) items
		= case getMovedNode_ moveId moves of
			movedItem=:(LUINode _ _ _ _ {moved=ESToBeRemoved ruleId}) //Restore (destination still exists)
				# mbItem = extractUIWithEffects_ (movedItem,moves)
				= maybe items (\item -> [item:items]) mbItem
			movedItem
				= items

	extract litem=:(LUIMoveDestination moveId moveRule) items
		= case getMovedNode_ moveId moves of
			movedItem=:(LUINode _ _ _ _ {moved=ESToBeApplied ruleId})	
				# mbItem = extractUIWithEffects_ (movedItem,moves)
				= maybe items (\item -> [item:items]) mbItem
			movedItem=:(LUINode _ _ _ _ {moved=ESApplied ruleId})	
				# mbItem = extractUIWithEffects_ (movedItem,moves)
				= maybe items (\item -> [item:items]) mbItem
			movedItem=:(LUINode _ _ _ _ {moved=ESToBeUpdated previousId ruleId})	
				| moveRule == previousId //Old destination, don't include it here
					= items
				| moveRule == ruleId //New destination, include it here
					# mbItem = extractUIWithEffects_ (movedItem,moves)
					= maybe items (\item -> [item:items]) mbItem
				| otherwise
					= items
			movedItem=:(LUINode _ _ _ _ {moved=ESToBeRemoved ruleId}) //To be be restored
				= items
				
	extract litem items
		# mbItem = extractUIWithEffects_ (litem,moves)
		= maybe items (\item -> [item:items]) mbItem

resetToBeShifted (LUINode type attr items changes effects)
	= LUINode type attr items {changes & toBeShifted = Nothing} effects

confirmChangesAndEffects_ :: (LUI,LUIMoves) -> (Maybe LUI,LUIMoves)
confirmChangesAndEffects_ (LUINode _ _ _ {toBeReplaced=Just replacement} _, moves)
	= confirmChangesAndEffects_ (replacement,moves)
confirmChangesAndEffects_ (LUINode _ _ _ {toBeRemoved=True} _, moves)
	= (Nothing,moves)
confirmChangesAndEffects_ (LUINode _ _ _ _ {additional=ESToBeRemoved _}, moves)
	= (Nothing,moves)
confirmChangesAndEffects_ (LUINode type attr items changes effects=:{wrapper=ESToBeRemoved wrappedBy}, moves)
	= case scanToPosition_ wrappedBy 0 items moves of
		(_,True, Just wrapped) = confirmChangesAndEffects_ (wrapped,moves)
		_           = abort "confirmChangesAndEffects: Wrapped item is missing"
confirmChangesAndEffects_ (LUINode type attr items changes effects, moves)
	# attr = applyAttributeChanges_ changes attr
	# effects = confirmEffects_ effects
	# (items,moves) = confirmChildChangesAndEffects_ items moves
	= (Just (LUINode type attr items noChanges effects), moves)
confirmChangesAndEffects_ (ref=:(LUIMoveSource moveId),moves)
	= case 'DM'.get moveId moves of
		Nothing = (Nothing, 'DM'.del moveId moves)
		Just moved = case confirmChangesAndEffects_ (moved,moves) of
			(Just item=:(LUINode _ _ _ _ {moved=ESApplied _}),moves)
				= (Just ref, 'DM'.put moveId item moves)
			(mbitem,moves)
				= (mbitem, 'DM'.del moveId moves)
confirmChangesAndEffects_ (ref=:(LUIMoveDestination moveId moveRule),moves)
	= case 'DM'.get moveId moves of
		Just (LUINode _ _ _ _ {moved=ESApplied movedBy}) | movedBy === moveRule = (Just ref,moves)
		Just (LUINode _ _ _ _ {moved=ESToBeApplied movedBy}) | movedBy === moveRule = (Just ref,moves)
		Just (LUINode _ _ _ _ {moved=ESToBeUpdated _ movedBy}) | movedBy === moveRule = (Just ref,moves)
		_ = (Nothing,moves)

confirmChildChangesAndEffects_ :: [LUI] LUIMoves -> ([LUI],LUIMoves)
confirmChildChangesAndEffects_ items moves = foldr confirm ([],moves) items
where
	shiftSources = foldr collect 'DM'.newMap items
	where
		collect (LUINode type attr items changes=:{toBeShifted=Just shiftId} effects) sources
			= 'DM'.put shiftId (LUINode type attr items {changes & toBeShifted=Nothing} effects) sources
		collect n sources = sources

	confirm (LUINode _ _ _ {toBeShifted=Just _} _) (items,moves) = (items,moves)
	confirm (LUIShiftDestination shiftId) (items,moves)
		= maybe (items,moves) (\item -> confirm item (items,moves)) ('DM'.get shiftId shiftSources)
	confirm item (items,moves)
		# (mbitem,moves) = confirmChangesAndEffects_ (item,moves)
		= (maybe items (\i -> [i:items]) mbitem, moves)

confirmEffects_ :: LUIEffects -> LUIEffects
confirmEffects_ {overwrittenType,overwrittenAttributes,hiddenAttributes,additional,hidden,moved,wrapper,unwrapped}
	= {overwrittenType = confirm overwrittenType
	  ,overwrittenAttributes = confirmMap overwrittenAttributes
	  ,hiddenAttributes = confirmMap hiddenAttributes
	  ,additional = confirm additional
	  ,hidden = confirm hidden
	  ,moved = confirm moved
	  ,wrapper = confirm wrapper
	  ,unwrapped = confirm unwrapped
	  }
where
	confirm (ESToBeApplied x) = ESApplied x
	confirm (ESToBeUpdated _ x) = ESApplied x
	confirm (ESToBeRemoved x) = ESNotApplied
	confirm es = es

	confirmMap m = ('DM'.fromList o (map (appSnd confirm)) o 'DM'.toList) m

//After downstream changes have been extracted there should be no pending changes
//and all effects are either ESNotApplied or ESApplied
fullyApplied_ :: (LUI,LUIMoves) -> Bool
fullyApplied_ _ = True
/*
fullyApplied_ (LUINode _ _ items c e, moves)
	| c.toBeInserted || c.toBeRemoved || c.toBeReplaced =: (Just _) || c.toBeShifted =: (Just _) = False
	| (not ('DM'.null c.setAttributes)) || (not ('DS'.null c.delAttributes)) = False
	| not (all (\(_,a) -> (a =: (ESApplied _) || a =: (ESNotApplied))) ('DM'.toList e.overwrittenAttributes)) = False
	| not (all (\(_,a) -> (a =: (ESApplied _) || a =: (ESNotApplied))) ('DM'.toList e.hiddenAttributes)) = False
	| not (e.overwrittenType =: (ESApplied _) || e.overwrittenType =: (ESNotApplied)) = False
	| not (e.additional =: (ESApplied _) || e.additional =: (ESNotApplied)) = False
	| not (e.hidden =: (ESApplied _) || e.hidden =: (ESNotApplied)) = False
	| not (e.moved =: (ESApplied _) || e.moved =: (ESNotApplied)) = False
	| not (e.wrapper =: (ESApplied _) || e.wrapper =: (ESNotApplied)) = False
	| not (e.unwrapped =: (ESApplied _) || e.unwrapped =: (ESNotApplied)) = False
	| otherwise
		= all (\l -> fullyApplied_ (l,moves)) items
fullyApplied_ (LUIMoveSource moveId, moves)
	= maybe False (\l -> fullyApplied_ (l,moves)) ('DM'.get moveId moves)
fullyApplied_ (LUIMoveDestination moveId _, moves)
	= maybe False (\l -> fullyApplied_ (l,moves)) ('DM'.get moveId moves)
fullyApplied_ (LUIShiftDestination _, moves) = False
*/
