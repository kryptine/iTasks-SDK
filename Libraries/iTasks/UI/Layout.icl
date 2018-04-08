implementation module iTasks.UI.Layout

import StdTuple, StdList, StdBool, StdInt, StdOrdList, StdArray, StdMisc, StdString
import Data.GenLexOrd
import Data.Maybe, Data.Either, Text, Data.Tuple, Data.List, Data.Either, Data.Functor
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
derive JSONEncode LayoutState, LayoutTree, LUI, LUIChanges, LUIEffects, LUIEffectStage, Set
derive JSONDecode LayoutState, LayoutTree, LUI, LUIChanges, LUIEffects, LUIEffectStage, Set

derive gEq LUIEffectStage

derive gLexOrd LUIEffectStage

instance < (LUIEffectStage a) | gLexOrd{|*|} a
where
	(<) x y = (gLexOrd{|*|} x y) === LT

//Test if a specific UI at a path is in the selection
inUISelection :: UISelection UIPath UI -> Bool
inUISelection (SelectByPath p) path _ = p === path
inUISelection (SelectByDepth n) p _ = length p == n
inUISelection (SelectDescendents) [_:_] _ = True
inUISelection (SelectDescendents) _ _ = False
inUISelection (SelectByType t) _ (UI type _ _) = t === type
inUISelection (SelectByHasAttribute k) _ (UI _ attr _) = isJust ('DM'.get k attr)
inUISelection (SelectByAttribute k p) _ (UI _ attr _) = maybe False p ('DM'.get k attr)
inUISelection (SelectByNumChildren num) _ (UI _ _  items) = length items == num
inUISelection (SelectByContains selection) path ui=:(UI _ _ items)
	| inUISelection selection path ui = True 
			  						  = or [inUISelection (SelectByContains selection) (path ++ [i]) item \\ item <- items & i <- [0..]]
inUISelection (SelectRelative prefix sel) absolutePath ui 
	= maybe False (\relativePath -> inUISelection sel relativePath ui) (removePrefix prefix absolutePath)
where
	removePrefix [] psb = Just psb
	removePrefix [pa:psa] [pb:psb] = if (pa == pb) (removePrefix psa psb) Nothing
	removePrefix _ _ = Nothing
inUISelection (SelectNone) _ _ = False
inUISelection (SelectAND sell selr) path ui = inUISelection sell path ui && inUISelection selr path ui 
inUISelection (SelectOR sell selr) path ui = inUISelection sell path ui || inUISelection selr path ui 
inUISelection (SelectNOT sel) path ui = not (inUISelection sel path ui)

inUISelectionAfterChange :: UISelection UIPath UI UIChange -> Bool
inUISelectionAfterChange selection path ui change //TODO: This needs a more efficient implemenation that does not apply the full change if it is not necessary
	= inUISelection selection path (applyUIChange change ui)

//A layout that has no effect at all
idLayout :: LayoutExpression
idLayout = SequenceLayouts []

setUITypeRef_ :: UIType -> Layout
setUITypeRef_ type = referenceLayout ref
where 
	ref (UI _ attr items) = UI type attr items

setUIAttributesRef_ :: UIAttributes -> Layout
setUIAttributesRef_ extraAttr = referenceLayout ref
where
	ref (UI type attr items) = UI type ('DM'.union extraAttr attr) items

delUIAttributesRef_ :: UIAttributeSelection -> Layout 
delUIAttributesRef_ selection = referenceLayout ref
where
	ref (UI type attr items) = UI type (foldl (\a k -> if (matchKey_ selection k) ('DM'.del k a) a) attr ('DM'.keys attr)) items

modifyUIAttributesRef_ :: UIAttributeSelection (UIAttributes -> UIAttributes) -> Layout
modifyUIAttributesRef_ selection modifier = referenceLayout ref
where
	ref (UI type attr items) = UI type ('DM'.union (modifier selected) attr) items
	where
		selected = selectAttributesOLD selection attr

		selectAttributesOLD SelectAll attr = attr
		selectAttributesOLD (SelectKeys keys) attr = 'DM'.fromList [a \\ a=:(k,_) <- 'DM'.toList attr | isMember k keys]

copySubUIAttributesRef_ :: UIAttributeSelection UIPath UIPath -> Layout
copySubUIAttributesRef_ selection src dst = referenceLayout ref
where
	ref ui = updDst dst (selAttr src ui) ui

	selAttr [] (UI _ attr _) = [a \\ a=:(k,_) <- 'DM'.toList attr | matchKey_ selection k]
	selAttr [s:ss] (UI _ _ items) = if (s >= 0 && s < length items) (selAttr ss (items !! s)) []

	updDst [] selected (UI type attr items) = UI type (foldl (\a (k,v) -> 'DM'.put k v a) attr selected) items
	updDst [s:ss] selected ui=:(UI type attr items) = if (s >= 0 && s < length items) (UI type attr (updateAt s (updDst ss selected (items !! s)) items)) ui

wrapUIRef_ :: UIType -> Layout
wrapUIRef_ type = referenceLayout ref
where
	ref ui = uic type [ui]

unwrapUIRef_ :: Layout
unwrapUIRef_ = referenceLayout ref
where
	ref (UI _ _ [ui:_]) = ui
	ref ui = ui

insertChildUIRef_ :: Int UI -> Layout
insertChildUIRef_ idx insert = referenceLayout ref
where
	ref ui=:(UI type attr items)
		| idx >= 0 && idx <= length items = UI type attr (insertAt idx insert items)
										  = ui

removeSubUIsRef_ :: UISelection -> Layout
removeSubUIsRef_ selection = referenceLayout ref
where
	ref ui=:(UI type attr items) //Special case for the root node
	  | inUISelection selection [] ui = UI UIEmpty 'DM'.newMap []
							          = UI type attr (flatten [rem [i] x \\ x <- items & i <- [0..]])

	rem path ui=:(UI type attr items)
	  | inUISelection selection path ui = []
						      = [UI type attr (flatten [rem (path ++ [i]) x \\ x <- items & i <- [0..]])]

moveSubUIsRef_ :: UISelection UIPath Int -> Layout 
moveSubUIsRef_ selection dst pos = referenceLayout ref
where
	ref ui
		# (selected,Just ui`) = collect [] ui   //Find and remove all matching nodes
		# (dst`,pos`)   = adjust [] dst pos ui  //Adjust the path and position for the removals
		= (insert selected dst` pos` ui`)       //Insert the selected nodes at the destination

	collect path ui=:(UI type attr items)
		| not (startsWith path dst) && inUISelection selection path ui //Match
			= ([ui],Nothing)
		| otherwise //Check all children
			# (collects,items) = unzip [collect (path ++ [i]) x \\ x <- items & i <- [0..]]
			= (flatten collects, Just (UI type attr [x \\ Just x <- items]))

	startsWith [] dst = True
	startsWith [p:ps] [d:ds] = if (p == d) (startsWith ps ds) False
	startsWith _ _ = False

	adjust cur [] pos (UI _ _ items) = ([],index cur pos items)
	adjust cur [s:ss] pos (UI _ _ items)
		| s >= 0 && s < length items
			# (ss`,pos`) = adjust (cur ++ [s]) ss pos (items !! s)
			= ([index cur s items:ss`],pos`)
		| otherwise
			= ([s:ss],pos)

	index cur n items = length [x \\ x <- take n items & i <- [0..] | not (inUISelection selection (cur ++ [i]) x)]

	insert selected [] i (UI type attr items)
		| i >= 0 && i <= length items = UI type attr (take i items ++ selected  ++ drop i items)
									  = UI type attr items
	insert selected [s:ss] i (UI type attr items)
		| s >= 0 && s < length items
			= UI type attr (updateAt s (insert selected ss i (items !! s)) items)
			= UI type attr items

layoutSubUIs :: UISelection Layout -> Layout
//layoutSubUIs selection layout = layoutSubUIsRef_ selection layout 
layoutSubUIs selection layout = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	//Find all places that match the selection
	//Keep track of the original UI in the state to enable dynamic matches
	apply ui
		# (change, state) = apply` [] ui
		= (change, LSLayoutSubUIs ui state)

	apply` path ui=:(UI type attr items)
		| inUISelection selection path ui 
			# (change,state) = layout.Layout.apply ui
			= (change,UIModified state)
		| otherwise
			# (itemChanges,itemStates) = unzip [apply` (path ++ [i]) ui \\ ui <- items & i <- [0..]]
			//Cleanup item changes (only keep those that actually change something
			# itemChanges =	[(i,ChangeChild c) \\ c <- itemChanges & i <- [0..] | not (c =: NoChange || c =: (ChangeUI [] []))]
			//Also cleanup item states
			# itemStates = [(i,s) \\ s <- itemStates & i <- [0..] | not s =: (SubUIsModified _ [])]
			= (ChangeUI [] itemChanges,SubUIsModified () itemStates)

	adjust (change, LSLayoutSubUIs ui state)
		# (change,ui,state) = adjust` [] change ui state
		= (change,LSLayoutSubUIs ui state)

	//For replacements, just use the apply rule on the new ui
	adjust` path (ReplaceUI ui) _ state 
		# (change, state) = apply` path ui
		= (ReplaceUI (applyUIChange change ui), ui, state)

	//When we get a change for a previously modified ui, we need to check whether the change still holds
	adjust` path change ui (UIModified state)
		# ui = applyUIChange change ui //Keep the 'shadow' copy of the UI up-to date
		| inUISelection selection path ui 
			//The layout should still be applied
			# (change,state) = layout.Layout.adjust (change, state)
			= (change,ui,UIModified state)
		| otherwise
			//The layout should no longer be applied, use the restore function to undo the layout
			//then apply the upstream change
			# rchange = layout.Layout.restore state
			//Now that this ui no longer matches, maybe its descendents do
			# (achange, state) = apply` path ui
			//The result is, the combination of first restoring, then updating, t
			# change = mergeUIChanges rchange (mergeUIChanges change achange)
			= (change, ui, state)

	//When we get a change, we need to check which sub-uis were affected
	adjust` path change ui state=:(SubUIsModified _ states)
		//Check if the change means that the layout is now applicable to this node
		| inUISelectionAfterChange selection path ui change
			//Update the 'shadow' copy of the UI
			# ui             = applyUIChange change ui
			//If the UI now matches we need to restore all changes to sub-ui's and then apply the layout
			# restore        = restoreSubUIs state
			# (change,state) = layout.Layout.apply ui
			= (mergeUIChanges restore change, ui, UIModified state)
		//Apply the change, and modify it if necessary
		| otherwise
			= case change of
				(ChangeUI attrChanges childChanges)
					//Update the attributes of the 'shadow' ui
					# (UI type attr items) = applyUIChange (ChangeUI attrChanges []) ui
					# (childChanges, items, states) = adjustChildChanges childChanges items states
					= (ChangeUI attrChanges childChanges, UI type attr items, SubUIsModified () states)
				NoChange
					//Recursively check all children
					# (UI type attr items) = ui
					# (childChanges, items, states) = adjustChildChanges [(i, ChangeChild NoChange) \\ i <- [0 .. (length items - 1)]] items states
					# change = if (childChanges =: []) NoChange	(ChangeUI [] childChanges)
					= (change, UI type attr items, SubUIsModified () states)
	where
		adjustChildChanges [] items states = ([], items, states)
		adjustChildChanges [(i,c):cs] items states
			# (c, items, states)  = adjustChildChange i c items states
			# (cs, items, states) = adjustChildChanges cs items states
			= (c ++ cs, items, states)

		adjustChildChange i (ChangeChild change) items states
			//Recursively adjust the change
			| i >= 0 && i < length items
				# (change, item, state) = adjust` (path ++ [i]) change (items !! i) (ltGet i states)
				= (case change of NoChange = []; _ = [(i,ChangeChild change)], updateAt i item items, ltPut i state states)
			| otherwise
				= ([],items, states)
		adjustChildChange i (InsertChild ui) items states
			| i >= 0 && i <= length items
				//(potentially) apply the layout to the inserted item
				# (change,state) = apply` (path ++ [i]) ui
				//Check the siblings, because their path has changed
				# (schanges, items, states) = adjustSiblings path (\x -> x > i) (insertAt i ui items) (ltInsert i state states)
				= ([(i,InsertChild (applyUIChange change ui)):schanges], items, states)
			| otherwise
				= ([], items, states)
		adjustChildChange i RemoveChild items states
			| i >= 0 && i < length items
				//Check the siblings, because their path has changed
				# (schanges, items, states) = adjustSiblings path (\x -> x >= i) (removeAt i items) (ltRemove i states)
				= ([(i,RemoveChild):schanges], items, states)
			| otherwise
				= ([], items, states)
		adjustChildChange i (MoveChild d) items states
			| i >= 0 && i < length items && d >= 0 && d < length items
				//Check the siblings, because their path has changed //TODO: We can do better... don't need to check all
				# (schanges, items, states) = adjustSiblings path (const True) (listMove i d items) (ltMove i d states)
				= ([(i,MoveChild d):schanges], items, states)
			| otherwise
				= ([], items, states)

		adjustSiblings path whichSiblings items states = adjust 0 items states
		where
            adjust :: !Int ![UI] ![(!Int, !LayoutTree LayoutState ())]
                   -> (![(!Int, !UIChildChange)], ![UI], ![(!Int, !LayoutTree LayoutState ())])
			adjust i [] states = ([],[],states)
			adjust i [item:items] states
				| whichSiblings i
					//Check
					# (change,item,state) = adjust` (path ++ [i]) NoChange item (ltGet i states)
					//Check the remaining items
					# (changes, items, states) = adjust (i + 1) items (ltPut i state states)
					= case change of 
						NoChange = (changes, [item:items], states)
						_        = ([(i,ChangeChild change):changes], [item:items], states)
				| otherwise
					# (changes, items, states) = adjust (i + 1) items states
					= (changes, [item:items], states)

	restoreSubUIs (UIModified state) = layout.Layout.restore state
	restoreSubUIs (SubUIsModified _ states)
		= case [(i,ChangeChild (restoreSubUIs s)) \\ (i,s) <- states] of
			[]      = NoChange
			changes = ChangeUI [] changes

	restore (LSLayoutSubUIs ui _) = ReplaceUI ui //VERY CRUDE RESTORE... TODO:We can do better than this

layoutSubUIsRef_ :: UISelection Layout -> Layout
layoutSubUIsRef_ selection layout = referenceLayout ref
where
	ref ui = app [] ui
	app path ui=:(UI type attr items) 
		| inUISelection selection path ui = applyLayout layout ui
										  = UI type attr [app (path ++ [i]) x \\ x <- items & i <- [0..]]

sequenceLayouts :: Layout Layout -> Layout 
//sequenceLayouts layout1 layout2 = sequenceLayoutsRef_ layout1 layout2
sequenceLayouts layout1 layout2 = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply ui
		# (change1,s1) = layout1.Layout.apply ui
		# (change2,s2) = layout2.Layout.apply (applyUIChange change1 ui)
		= (mergeUIChanges change1 change2, LSSequence s1 s2)

	adjust (change,LSSequence s1 s2) 
		# (change,s1) = layout1.Layout.adjust (change,s1)
		# (change,s2) = layout2.Layout.adjust (change,s2)
		= (change,LSSequence s1 s2)
	adjust (change,s) = (change,s)

	restore (LSSequence s1 s2)
		//Restore in reverse order
		# change2 = layout2.Layout.restore s2
		# change1 = layout1.Layout.restore s1
		= mergeUIChanges change2 change1	

sequenceLayoutsRef_ :: Layout Layout -> Layout
sequenceLayoutsRef_ layout1 layout2 = referenceLayout ref
where
	ref ui = applyLayout layout2 (applyLayout layout1 ui)

referenceLayout :: (UI -> UI) -> Layout
referenceLayout ref = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply ui = (ReplaceUI (ref ui), LSReference ui)
		
	adjust (NoChange,state) = (NoChange,state)
	adjust (change, LSReference ui) 
		# ui = applyUIChange change ui
		= (ReplaceUI (ref ui),LSReference ui)

	restore (LSReference ui)
		= ReplaceUI ui

//Helper for sequence layouts
applyLayout :: Layout UI -> UI 
applyLayout {Layout|apply} ui = applyUIChange (fst (apply ui)) ui

//Util functions on the layout tree structure
ltGet :: Int [(Int,LayoutTree a b)] -> (LayoutTree a b) | gDefault{|*|} b
ltGet index [] = SubUIsModified defaultValue []
ltGet index [(i,tree):ts] = if (i == index) tree (ltGet index ts)

ltPut :: Int (LayoutTree a b) [(Int,LayoutTree a b)] -> [(Int,LayoutTree a b)]
// It is pointless to store empty trees in a sparse representation, just make sure we delete the previously stored value
ltPut index (SubUIsModified _ []) list = [x \\ x=:(i,_) <- list | i <> index] 
ltPut index item list
	= [x \\ x=:(i,_) <- list | i < index] ++ [(index,item)] ++ [x \\ x=:(i,_) <- list | i > index]

ltInsert :: Int (LayoutTree a b) [(Int,LayoutTree a b)] -> [(Int,LayoutTree a b)]
ltInsert index (SubUIsModified _ []) list = [(if (i >= index) (i + 1) i, x) \\ (i,x) <- list]
ltInsert index item list 
	= [ x \\ x=:(i,_) <- list | i < index] ++ [(index,item)] ++ [(i + 1,x) \\ (i,x) <- list | i >= index]

ltRemove :: Int [(Int,LayoutTree a b)] -> [(Int,LayoutTree a b)]
ltRemove index list = [(if (i > index) (i - 1) i, x) \\ (i,x) <- list | i <> index]

ltMove :: Int Int [(Int,LayoutTree a b)] -> [(Int,LayoutTree a b)] | gDefault{|*|} b
ltMove src dst list = ltInsert dst (ltGet src list) (ltRemove src list)

ltCount :: Bool (a -> Bool) [(Int,LayoutTree a b)] -> Int
ltCount recursive pred list = foldr count 0 (map snd list)
where
	count (UIModified x) n = if (pred x) (n + 1) n
	count (SubUIsModified _ mods) n = if recursive (n + ltCount recursive pred mods) n

listMove :: Int Int [a] -> [a]
listMove src dst list = insertAt dst (list !! src) (removeAt src list)


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
noEffects = {overwrittenType = ESNotApplied, overwrittenAttributes = 'DM'.newMap, hiddenAttributes = 'DM'.newMap, additional = ESNotApplied, hidden = ESNotApplied, moved = ESNotApplied, containsMovesBy = 'DM'.newMap, wrapper = ESNotApplied, unwrapped = ESNotApplied}

//Initialize an LUI tree from a regular UI tree
initLUI :: Bool UI -> LUI
initLUI toBeInserted (UI type attr items) = LUINode type attr (map (initLUI toBeInserted) items) {noChanges & toBeInserted = toBeInserted} noEffects

toBeAdded :: Int LUI -> LUI
toBeAdded ruleId (LUINode type attr items changes effects) = LUINode type attr items changes {effects & additional = ESToBeApplied ruleId}

initLUIExtractState :: LUIExtractState
initLUIExtractState = {movedChanges = 'DM'.newMap, movedUIs = 'DM'.newMap}
/*
* When upstream changes 'arrive' they are tracked in the 'buffer' data structure.
* All information about what should be modified according to the upstream change is
* recorded in the tree.
* 
* The following functions implement this recording of changes in the tree
*/

//When an upstream UI change is applied to the LUI it is recorded in the LUI tree
//in such a way that it can easily be extracted as a downstream change later on
applyUpstreamChange :: UIChange LUI -> LUI
//If the node is a wrapper, apply the change to the wrapped child
applyUpstreamChange change (LUINode type attr items changes effects=:{LUIEffects|wrapper=ESApplied _})
	= LUINode type attr [if (isAdditional_ i) i (applyUpstreamChange change i) \\ i <- items] changes effects
applyUpstreamChange NoChange lui = lui
applyUpstreamChange (ReplaceUI ui) (LUINode type attr items changes effects)
	| changes.toBeInserted //If it is a new node, we can replace it
		= initLUI True ui
	= LUINode type attr items {changes & toBeReplaced = Just (initLUI False ui)} effects
applyUpstreamChange (ReplaceUI ui) lui = lui
applyUpstreamChange (ChangeUI attributeChanges childChanges) lui
	= (foldl applyUpstreamChildChange (foldl applyUpstreamAttributeChange lui attributeChanges) childChanges)
where
	applyUpstreamAttributeChange :: LUI UIAttributeChange -> LUI
	applyUpstreamAttributeChange (LUINode type attr items changes=:{setAttributes,delAttributes} effects) (SetAttribute key value)
		# setAttributes = 'DM'.put key value setAttributes
		# delAttributes = 'DS'.delete key delAttributes
		= LUINode type attr items {changes & setAttributes = setAttributes, delAttributes = delAttributes} effects
	applyUpstreamAttributeChange (LUINode type attr items changes=:{setAttributes,delAttributes} effects) (DelAttribute key)
		# setAttributes = 'DM'.del key setAttributes
		# delAttributes = 'DS'.insert key delAttributes
		= LUINode type attr items {changes & setAttributes = setAttributes, delAttributes = delAttributes} effects
	applyUpstreamAttributeChange lui _ = lui

	applyUpstreamChildChange :: LUI (Int,UIChildChange) -> LUI
	applyUpstreamChildChange lui (index,ChangeChild change) = case lui of
		(LUINode type attr items changes effects)
			# adjustedIndex = adjustIndex_ index items
			| index < 0 || adjustedIndex >= length items = lui
			= LUINode type attr (updateItem (applyUpstreamChange change) adjustedIndex items) changes effects
		_
			= lui
	applyUpstreamChildChange lui (index,RemoveChild) = case lui of
		(LUINode type attr items changes effects)
			# adjustedIndex = adjustIndex_ index items
			| index < 0 || adjustedIndex >= length items = lui
			= LUINode type attr (removeItem adjustedIndex items) changes effects
		_ = lui
	applyUpstreamChildChange lui (index,InsertChild ui) = case lui of
		(LUINode type attr items changes effects)
			# adjustedIndex = adjustIndex_ index items
			| index < 0 || adjustedIndex >= length items = lui
			= LUINode type attr (insertAt adjustedIndex (initLUI True ui) items) changes effects
		_ = lui
	applyUpstreamChildChange lui (index,MoveChild destination) = case lui of
		(LUINode type attr items changes effects)
			# shiftId = nextShiftID_ items
			# adjustedIndex = adjustIndex_ index items
			| index < 0 || adjustedIndex >= length items = lui
			= LUINode type attr (shiftItem shiftId adjustedIndex destination items) changes effects
		_ = lui

	//An index may point to the destination of a shifted child node. In that case we want to apply
	//the update to the node that will be shifted to that destination
	updateItem updateFunction index items = case items !! index of
		(LUIShiftDestination shiftId) = updateItem updateFunction (fst (lookupShiftSource_ shiftId items)) items
		lui = updateAt index (applyUpdate lui) items
	where
		applyUpdate (LUINode type attr items changes=:{toBeReplaced = Just replacement} effects)
			= LUINode type attr items {changes & toBeReplaced = Just (applyUpdate replacement)} effects
		applyUpdate lui = updateFunction lui

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
					= insertAt (adjustIndex_ destination items) (LUIShiftDestination prevShiftId) items
		//Regular node
		(LUINode type attr citems changes effects)
			//Mark the node as a shifted node
			# items = updateAt index (LUINode type attr citems {changes & toBeShifted = Just shiftId} effects) items
			//Record the destination
			= insertAt (adjustIndex_ destination items) (LUIShiftDestination shiftId) items
	where
		findSamePositionShift shiftId destination items = find 0 0 items
		where
			find i ai [] = Nothing
			find i ai [x=:(LUINode _ _ _ {toBeShifted=Just sourceId} _):xs]
				| sourceId == shiftId = if (ai == destination) (Just (ai,x)) Nothing
				                      = find (i + 1) ai xs
			find i ai [x:xs]
				| isInvisibleUpstream_ x = find (i + 1) ai xs
				                         = find (i + 1) (ai + 1) xs
nextShiftID_ :: [LUI] -> Int
nextShiftID_ items = maximum [-1:map shiftID items] + 1
where
	shiftID (LUINode _ _ _ {toBeShifted=Just x} _) = x
	shiftID (LUIShiftDestination x) = x
	shiftID _ = -1

applyLayoutEffects :: LayoutExpression LUI -> LUI
applyLayoutEffects layout lui = applyLayoutEffectsAs 0 layout lui

applyLayoutEffectsAs :: LayoutRuleNo LayoutExpression LUI -> LUI
applyLayoutEffectsAs ruleNo (SetUIType newType) lui = setUITypeRule newType ruleNo lui
applyLayoutEffectsAs ruleNo (SetUIAttributes setAttributes) lui = setUIAttributesRule setAttributes ruleNo lui
applyLayoutEffectsAs ruleNo (DelUIAttributes selection) lui = delUIAttributesRule selection ruleNo lui
applyLayoutEffectsAs ruleNo (ModifyUIAttributes selection modifier) lui = modifyUIAttributesRule selection modifier ruleNo lui
applyLayoutEffectsAs ruleNo (CopySubUIAttributes selection src dst) lui = copySubUIAttributesRule selection src dst ruleNo lui
applyLayoutEffectsAs ruleNo (WrapUI type) lui = wrapUIRule type ruleNo lui
applyLayoutEffectsAs ruleNo (UnwrapUI) lui = unwrapUIRule ruleNo lui
applyLayoutEffectsAs ruleNo (InsertChildUI position insertion) lui = insertChildUIRule position insertion ruleNo lui
applyLayoutEffectsAs ruleNo (RemoveSubUIs selection) lui = removeSubUIsRule selection ruleNo lui
applyLayoutEffectsAs ruleNo (MoveSubUIs selection path pos) lui = moveSubUIsRule selection path pos ruleNo lui
applyLayoutEffectsAs ruleNo (LayoutSubUIs selection sub) lui = layoutSubUIsRule selection sub ruleNo lui
applyLayoutEffectsAs ruleNo (SequenceLayouts subs) lui = sequenceLayoutsRule subs ruleNo lui

/*
* Layout rules transform the 'buffered' tree and annotate the places where layout effects
* should be applied, or should no longer be applied
*/

setUITypeRule :: UIType -> LayoutRule
setUITypeRule newType = rule
where
	rule ruleId lui=:(LUINode type attr items changes=:{toBeReplaced=Just replacement} effects)
		= LUINode type attr items {changes & toBeReplaced=Just (rule ruleId replacement)} effects
	rule ruleId lui=:(LUINode type attr items changes effects=:{overwrittenType})
		# overwrittenType = case overwrittenType of
			(ESNotApplied) = ESToBeApplied newType
			(ESToBeApplied _) = ESToBeApplied newType
			(ESApplied curType) = if (curType === newType) (ESApplied curType) (ESToBeUpdated curType newType)
			(ESToBeUpdated curType _) = if (curType	=== newType) (ESApplied curType) (ESToBeUpdated curType newType)
			(ESToBeRemoved curType) = if (curType	=== newType) (ESApplied curType) (ESToBeUpdated curType newType)
		= LUINode type attr items changes {effects & overwrittenType = overwrittenType}
	rule ruleId lui = lui

setUIAttributesRule :: UIAttributes -> LayoutRule
setUIAttributesRule setAttributes = rule
where
	rule ruleId lui=:(LUINode type attr items changes=:{toBeReplaced=Just replacement} effects)
		= LUINode type attr items {changes & toBeReplaced=Just (rule ruleId replacement)} effects
	rule ruleId lui=:(LUINode type attr items changes effects=:{overwrittenAttributes})
		# overwrittenAttributes = foldr overwriteAttribute_ overwrittenAttributes ('DM'.toList setAttributes)
		= LUINode type attr items changes {effects & overwrittenAttributes = overwrittenAttributes}
	rule ruleId lui = lui

delUIAttributesRule :: UIAttributeSelection -> LayoutRule
delUIAttributesRule selection = rule 
where
	rule ruleId lui=:(LUINode type attr items changes=:{toBeReplaced=Just replacement} effects)
		= LUINode type attr items {changes & toBeReplaced=Just (rule ruleId replacement)} effects

	rule ruleId lui=:(LUINode type attr items changes=:{setAttributes,delAttributes} effects=:{hiddenAttributes})
		//For all attribute keys (including changes), we decide if the attribute should be hidden or not
		# hiddenAttributes = foldr (hideAttribute_ (matchKey_ selection)) hiddenAttributes keys
		= LUINode type attr items changes {effects & hiddenAttributes = hiddenAttributes}
	where
		keys = filter (\x -> not (isMember x ('DS'.toList delAttributes)))
			(removeDup ('DM'.keys attr ++ 'DM'.keys setAttributes))

	rule ruleId lui = lui

modifyUIAttributesRule :: UIAttributeSelection (UIAttributes -> UIAttributes) -> LayoutRule
modifyUIAttributesRule selection modifier = rule 
where
	rule ruleId lui=:(LUINode type attr items changes=:{toBeReplaced=Just replacement} effects)
		= LUINode type attr items {changes & toBeReplaced=Just (rule ruleId replacement)} effects
	rule ruleId lui=:(LUINode type attr items changes effects=:{overwrittenAttributes,hiddenAttributes})
		//1. Apply the modifier function to the current of attributes that match the selection
		# selectedAttr = selectAttributesWithChanges_ selection lui
		# modifiedAttr = modifier selectedAttr
		//2. Override new attributes and hide attributes that match the selection 
		# overwrittenAttributes = overrideModifiedAttributes modifiedAttr overwrittenAttributes
		# hiddenAttributes = hideRemovedAttributes selectedAttr modifiedAttr hiddenAttributes
		= (LUINode type attr items changes {effects & overwrittenAttributes = overwrittenAttributes,hiddenAttributes = hiddenAttributes})
	rule ruleId lui = lui

	overrideModifiedAttributes modified overwritten = foldr overwriteAttribute_ overwritten ('DM'.toList modified)
	hideRemovedAttributes selected modified hidden = foldr (hideAttribute_ isRemoved) hidden ('DM'.keys selected)
	where
		isRemoved key = not ('DM'.member key modified)

copySubUIAttributesRule :: UIAttributeSelection UIPath UIPath -> LayoutRule
copySubUIAttributesRule selection src dst = rule
where
	rule ruleId lui=:(LUINode type attr items changes=:{toBeReplaced=Just replacement} effects)
		= LUINode type attr items {changes & toBeReplaced=Just (rule ruleId replacement)} effects
	rule ruleId lui
		//Find the selected attributes in the source node... 
		//Then use the setUIAttributes layout rule to copy the changes
		= maybe lui (withEffect lui) (selectSource lui)
	where
		selectSource lui = fmap (selectAttributesWithChanges_ selection) (selectNode_ src lui)
		withEffect lui attr = updateNode_ dst ((setUIAttributesRule attr) ruleId) lui

wrapUIRule :: UIType -> LayoutRule
wrapUIRule type = rule
where
	rule ruleId lui=:(LUINode type attr items changes=:{toBeReplaced=Just replacement} effects)
		= LUINode type attr items {changes & toBeReplaced=Just (rule ruleId replacement)} effects

	rule ruleId lui
		| wrappedBy ruleId lui = lui
		| otherwise = wrap ruleId type lui
	where
		wrappedBy ruleId (LUINode _ _ _ _ {LUIEffects|wrapper=ESApplied matchId}) = ruleId == matchId
		wrappedBy ruleId (LUINode _ _ _ _ {LUIEffects|wrapper=ESToBeApplied matchId}) = ruleId == matchId
		wrappedBy _ _ = False

		wrap ruleId type lui = LUINode type 'DM'.newMap [lui] noChanges {noEffects & wrapper = ESToBeApplied ruleId}

unwrapUIRule :: LayoutRule
unwrapUIRule = rule
where
	rule ruleId lui=:(LUINode type attr items changes=:{toBeReplaced=Just replacement} effects)
		= LUINode type attr items {changes & toBeReplaced=Just (rule ruleId replacement)} effects

	rule ruleId lui=:(LUINode type attr items changes effects=:{unwrapped})
		# hasChildren = lengthAfterChanges_ items > 0
		= case unwrapped of
			ESApplied matchId | matchId == ruleId
				= if hasChildren lui (LUINode type attr items changes {effects & unwrapped = ESToBeRemoved ruleId})
			ESToBeApplied matchId | matchId == ruleId
				= if hasChildren lui (LUINode type attr items changes {effects & unwrapped = ESNotApplied})
			ESToBeRemoved matchId
				= if hasChildren (LUINode type attr items changes {LUIEffects|effects & unwrapped = ESToBeApplied ruleId}) lui
			ESNotApplied
				= if hasChildren (LUINode type attr items changes {LUIEffects|effects & unwrapped = ESToBeApplied ruleId}) lui
			_
				= abort "TODO: Already unwrapped by another rule, unwrap the first child.."
	rule ruleId lui = lui

insertChildUIRule :: Int UI -> LayoutRule
insertChildUIRule position insertion = rule
where
	rule ruleId lui=:(LUINode type attr items changes=:{toBeReplaced=Just replacement} effects)
		= LUINode type attr items {changes & toBeReplaced=Just (rule ruleId replacement)} effects
	rule ruleId lui=:(LUINode type attr items changes effects)
		= case scanToUpstreamPosition_ position (isAddedBy_ ruleId) items of
			(_,True,Nothing)	
				//If the index is at the end of the range, add the item
				= LUINode type attr (undoAdditions ruleId items ++ [toBeAdded ruleId (initLUI False insertion)]) changes effects
			(index,True,Just selected)
				| getAdditional selected === ESToBeApplied ruleId || getAdditional selected === ESApplied ruleId 
					= lui
				| otherwise
					= LUINode type attr (insertAt index (toBeAdded ruleId (initLUI False insertion)) (undoAdditions ruleId items)) changes effects
			_
				= lui

	rule ruleId lui = lui

	getAdditional (LUINode _ _ _ _ {additional}) = additional
	getAdditional _ = ESNotApplied

	undoAdditions ruleId items = map undo items
	where
		undo lui=:(LUINode type attr items changes effects=:{additional})
			| additional === (ESToBeApplied ruleId)
				= LUINode type attr items changes {effects & additional = ESNotApplied}
			| additional === (ESApplied ruleId)
				= LUINode type attr items changes {effects & additional = ESToBeRemoved ruleId}
			| otherwise
				= lui
		undo lui = lui

removeSubUIsRule :: UISelection -> LayoutRule
removeSubUIsRule selection = rule
where
	rule ruleId lui=:(LUINode type attr items changes=:{toBeReplaced=Just replacement} effects)
		= LUINode type attr items {changes & toBeReplaced=Just (rule ruleId replacement)} effects

	rule ruleId lui = remove [] lui
	where
		remove path lui=:(LUINode type attr items changes effects)
			//Check if this matches the selection
			| inLUISelection_ selection path lui
				= LUINode type attr (map clear items) changes (hide ruleId effects)
			| otherwise
				# items = [maybe item (\i -> remove (path ++ [i]) item) mbi \\ (mbi,item) <- indicesAfterChanges_ items]
				= LUINode type attr items changes (unhide ruleId effects)
		remove path lui = lui

		clear (LUINode type attr items changes effects) = LUINode type attr (map clear items) changes (unhide ruleId effects)
		clear lui = lui

	hide ruleId effects=:{hidden=ESNotApplied} = {effects & hidden = ESToBeApplied ruleId}
	hide ruleId effects=:{hidden=ESToBeApplied _} = {effects & hidden = ESToBeApplied ruleId}
	hide ruleId effects=:{hidden=ESApplied _} = {effects & hidden = ESApplied ruleId}
	hide ruleId effects=:{hidden=ESToBeRemoved _} = {effects & hidden = ESApplied ruleId}

	unhide ruleId effects=:{hidden=ESNotApplied} = {effects & hidden = ESNotApplied}
	unhide ruleId effects=:{hidden=ESToBeApplied _} = {effects & hidden = ESNotApplied}
	unhide ruleId effects=:{hidden=ESApplied _} = {effects & hidden = ESToBeRemoved ruleId}
	unhide ruleId effects=:{hidden=ESToBeRemoved _} = {effects & hidden = ESToBeRemoved ruleId}

moveSubUIsRule :: UISelection UIPath Int -> LayoutRule
moveSubUIsRule selection path pos = rule
where
	rule ruleId lui=:(LUINode type attr items changes=:{toBeReplaced=Just replacement} effects)
		= LUINode type attr items {changes & toBeReplaced=Just (rule ruleId replacement)} effects

	rule ruleId lui
		# (numMoved,lui) = markNodes selection (destinationExists path pos lui) lui
		# lui = updateDestination numMoved lui
		# lui = markRoot numMoved lui
		= lui
	where
		//1 Check if the destination position. If it does not, we can't move anything
		destinationExists path pos lui
			= maybe False (positionExists pos) (selectNode_ path lui)
		where
			positionExists pos (LUINode _ _ items _ _)
				= case scanToUpstreamPosition_ pos (\c -> c =: (LUIMoveDestination _ _)) items of
					(i, True, Just (LUIMoveDestination _ _ )) = True
					(i, found, _) = found
		
		//2 Recursively check nodes that match the path and toggle nodes on or off
		//  We also count the number of moved nodes
		markNodes selection destinationExists lui = mark [] lui
		where
			mark path lui=:(LUINode type attr items changes effects)
				//Check if this matches the selection
				| destinationExists && inLUISelection_ selection path lui
					= (1,LUINode type attr (map clear items) changes (move ruleId effects))
				| otherwise
					# (nums,items) = unzip [maybe (0,item) (\i -> mark (path ++ [i]) item) mbi \\ (mbi,item) <- indicesAfterChanges_ items]
					= (sum nums, LUINode type attr items changes (revertEffect_ ruleId effects))
			mark _ lui = (0,lui)

			clear (LUINode type attr items changes effects) = LUINode type attr (map clear items) changes (revertEffect_ ruleId effects)
			clear lui = lui

		//3 Mark the destination with the correct number of moved nodes
		updateDestination numMoved lui = updateNode_ path update lui
		where
			update lui=:(LUINode type attr items changes effects)
				= case scanToUpstreamPosition_ pos (\c -> c =: (LUIMoveDestination _ _)) items of
					(index,_,Just (LUIMoveDestination _ _))
						| numMoved == 0
							= LUINode type attr (removeAt index items) changes effects
						| otherwise
							= LUINode type attr (updateAt index (LUIMoveDestination ruleId numMoved) items) changes effects
					(index,_,Just _)
						| numMoved == 0
							= lui
						| otherwise
							= LUINode type attr (insertAt index (LUIMoveDestination ruleId numMoved) items) changes effects
					(index,True,Nothing)
						| numMoved == 0
							= lui	
						| otherwise
							= LUINode type attr (insertAt index (LUIMoveDestination ruleId numMoved) items) changes effects
					_ = lui

			update lui = lui

		//4 Mark the root node to indicate whether something was moved in the tree
		markRoot numMoved lui=:(LUINode type attr items changes effects=:{containsMovesBy})
			//Record the information when nodes are moved, or were previously moved
			| numMoved > 0 || isJust ('DM'.get ruleId containsMovesBy)
				= LUINode type attr items changes {effects & containsMovesBy = 'DM'.put ruleId numMoved containsMovesBy}
			| otherwise
				= lui
		markRoot _ lui = lui

	move ruleId effects=:{LUIEffects|moved=ESNotApplied} = {LUIEffects|effects & moved = ESToBeApplied ruleId}
	move ruleId effects=:{LUIEffects|moved=ESToBeApplied _} = {LUIEffects|effects & moved = ESToBeApplied ruleId}
	move ruleId effects=:{LUIEffects|moved=ESApplied _} = {LUIEffects|effects & moved = ESApplied ruleId}
	move ruleId effects=:{LUIEffects|moved=ESToBeRemoved _} = {LUIEffects|effects & moved= ESApplied ruleId}

layoutSubUIsRule :: UISelection LayoutExpression -> LayoutRule
layoutSubUIsRule selection sub = rule
where
	rule ruleId lui=:(LUINode type attr items changes=:{toBeReplaced=Just replacement} effects)
		= LUINode type attr items {changes & toBeReplaced=Just (rule ruleId replacement)} effects

	rule ruleId lui = apply [] lui
	where
		apply path lui=:(LUINode type attr items changes effects)
			//Check if this matches the selection
			| inLUISelection_ selection path lui
				= applyLayoutEffectsAs ruleId sub lui
			| otherwise
				# items = [maybe item (\i -> apply (path ++ [i]) item) mbi \\ (mbi,item) <- indicesAfterChanges_ items]
				= LUINode type attr items changes (revertEffect_ ruleId effects)
		apply path lui = lui

sequenceLayoutsRule :: [LayoutExpression] -> LayoutRule
sequenceLayoutsRule subs = rule
where
	rule ruleId lui = snd (foldl apply (ruleId,lui) subs)
	where
		apply (ruleId,lui) sub = (ruleId + maxNumSteps_ sub, applyLayoutEffectsAs ruleId sub lui)

//Utility functions shared by the layout rules:

//Adjust the index and length for additional nodes inserted by layout rules
//TODO: Remove applications of adjustIndex_ where the index is used to immediately lookup the list element 
//      that scanToUpstreamPosition_ already found
adjustIndex_ :: Int [LUI] -> Int
adjustIndex_ index  items = fst3 (scanToUpstreamPosition_ index (const False) items)

maxNumSteps_ :: LayoutExpression -> Int
maxNumSteps_ (LayoutSubUIs _ sub) = maxNumSteps_ sub
maxNumSteps_ (SequenceLayouts subs) = sum (map maxNumSteps_ subs)
maxNumSteps_ _ = 1

//Lookup an item specified by an upstream position, skipping over invisible nodes
scanToUpstreamPosition_ :: Int (LUI -> Bool) [LUI] -> (Int,Bool,Maybe LUI)
scanToUpstreamPosition_ pos skipException items = scan pos 0 items
where
	scan r i [] = (i, r == 0, Nothing) //Scanned beyond the list
	scan r i [x:xs]
		| isInvisibleUpstream_ x && not (skipException x) = scan r (i + 1) xs
		| r == 0 = (i,True,Just x)
		| otherwise = scan (r - 1) (i + 1) xs

//Some nodes are not visible upstream, so we should not count them when using indexes
isInvisibleUpstream_ :: !LUI -> Bool
isInvisibleUpstream_ (LUINode _ _ _ {toBeRemoved=True} _) = True
isInvisibleUpstream_ (LUINode _ _ _ {toBeShifted=(Just _)} _) = True
isInvisibleUpstream_ (LUINode _ _ _ _ {additional=ESToBeApplied _}) = True
isInvisibleUpstream_ (LUINode _ _ _ _ {additional=ESApplied _}) = True
isInvisibleUpstream_ (LUINode _ _ _ _ {additional=ESToBeRemoved _}) = True
isInvisibleUpstream_ (LUIMoveDestination _ _) = True
isInvisibleUpstream_ _  = False

isAddedBy_ :: LayoutRuleNo LUI -> Bool
isAddedBy_ lid (LUINode _ _ _ _ {additional=ESToBeApplied ruleId}) = lid == ruleId
isAddedBy_ lid (LUINode _ _ _ _ {additional=ESApplied ruleId}) = lid == ruleId
isAddedBy_ lid (LUINode _ _ _ _ {additional=ESToBeRemoved ruleId}) = lid == ruleId
isAddedBy_ _ _ = False

isAdditional_ :: LUI -> Bool
isAdditional_ (LUINode _ _ _ _ {additional=ESToBeApplied _}) = True
isAdditional_ (LUINode _ _ _ _ {additional=ESApplied _}) = True
isAdditional_ (LUINode _ _ _ _ {additional=ESToBeRemoved _}) = True
isAdditional_ _ = False

isRemoved_ :: LUI -> Bool
isRemoved_ (LUINode _ _ _ {LUIChanges|toBeRemoved} _) = toBeRemoved
isRemoved_ _ = False

isUnwrapped_ :: LUI -> Bool
isUnwrapped_ (LUINode _ _ _ _ {LUIEffects|unwrapped=ESToBeApplied _}) = True
isUnwrapped_ (LUINode _ _ _ _ {LUIEffects|unwrapped=ESApplied _}) = True
isUnwrapped_ _ = False

isMoved_ :: LUI -> Bool
isMoved_ (LUINode _ _ _ _ {LUIEffects|moved=ESToBeApplied _}) = True
isMoved_ (LUINode _ _ _ _ {LUIEffects|moved=ESApplied _}) = True
isMoved_ _ = False

isHidden_ :: LUI -> Bool
isHidden_ (LUINode _ _ _ _ {LUIEffects|hidden=ESToBeApplied _}) = True
isHidden_ (LUINode _ _ _ _ {LUIEffects|hidden=ESApplied _}) = True
isHidden_ _ = False

hasMovedNodes_ :: LUI -> Bool
hasMovedNodes_ (LUINode _ _ _ _ {containsMovesBy}) = not ('DM'.null containsMovesBy)
hasMovedNodes_ _ = False

lookupShiftSource_ :: Int [LUI] -> (Int,LUI)
lookupShiftSource_ shiftId items = lookup 0 items 
where
	lookup _ [] = abort "lookupShiftSource_: could not find source"
	lookup i [x:xs] = if (isSource x) (i,x) (lookup (i+1) xs) 
		
	isSource (LUINode _ _ _ {toBeShifted = Just sourceId} _) = sourceId == shiftId
	isSource _ = False

lengthAfterChanges_ :: [LUI] -> Int
lengthAfterChanges_ items = foldr count 0 items
where
	count (LUINode _ _ _ {toBeRemoved = False} _) num = num + 1
	count _ num = num //Don't count shift destinations and removed nodes

indicesAfterChanges_ :: [LUI] -> [(Maybe Int,LUI)]
indicesAfterChanges_ items = addIndices (indexShiftDestinations items) items
where
	indexShiftDestinations items = snd (foldl count (0,'DM'.newMap) items)
	where
		count (i,positions) (LUIShiftDestination shiftId) = (i + 1,'DM'.put shiftId i positions)
		count (i,positions) (LUINode _ _ _ {toBeRemoved=True} _) = (i,positions)
		count (i,positions) (LUINode _ _ _ {toBeShifted=Just _} _) = (i,positions)
		count (i,positions) (LUINode _ _ _ _ _) = (i + 1, positions)
		count (i,positions) (LUIMoveDestination _ num) = (i, positions)

	addIndices destinations items = reverse (snd (foldl add (0,[]) items))
	where
		add (i,acc) lui=:(LUIShiftDestination _) = (i + 1,[(Nothing,lui):acc])
		add (i,acc) lui=:(LUINode _ _ _ {toBeRemoved=True} _) = (i,[(Nothing,lui):acc])
		add (i,acc) lui=:(LUINode _ _ _ {toBeShifted=Just shiftId} _) = (i,[('DM'.get shiftId destinations,lui):acc])
		add (i,acc) lui=:(LUINode _ _ _ _ _) = (i + 1,[(Just i,lui):acc])
		add (i,acc) lui=:(LUIMoveDestination _ num) = (i,[(Nothing,lui):acc])

selectNode_ :: UIPath LUI -> Maybe LUI 
selectNode_ [] lui = case lui of
	(LUINode _ _ _ {toBeRemoved=True} _)              = Nothing
	(LUINode _ _ _ {toBeReplaced=Just replacement} _) = selectNode_ [] replacement
	(LUINode _ _ _ _ _)                               = Just lui
	_                                                 = Nothing
selectNode_ [s:ss] (LUINode _ _ items _ _)
	= case scanToUpstreamPosition_ s (const False) items of
		(_,_,Just (LUINode _ _ _ {toBeReplaced=Just replacement} _)) = selectNode_ ss replacement
		(_,_,Just (LUIShiftDestination shiftId)) = selectNode_ ss (snd (lookupShiftSource_ shiftId items))
		(_,_,Just child) = selectNode_ ss child
		_ = Nothing

updateNode_ :: UIPath (LUI -> LUI) LUI -> LUI 
updateNode_ [] update lui = case lui of
	(LUINode _ _ _ {toBeRemoved=True} _) = lui
	(LUINode _ _ _ {toBeReplaced=Just replacement} _) = updateNode_ [] update replacement
	(LUINode _ _ _ _ _)                               = update lui
	_                                                 = lui

updateNode_ [s:ss] update lui=:(LUINode type attr items changes effects)
	# items = case scanToUpstreamPosition_ s (const False) items of
		(index,_,Just (LUINode ctype cattr citems cchanges=:{toBeReplaced=Just replacement} ceffects))
			# replacement = updateNode_ ss update replacement
			= updateAt index (LUINode ctype cattr citems {cchanges & toBeReplaced = Just replacement} ceffects) items
		(index,_,Just (LUIShiftDestination shiftId))
			# (sourceIndex,source) = lookupShiftSource_ shiftId items
			= updateAt sourceIndex (updateNode_ ss update source) items
		(index,_,Just child) 
			= updateAt index (updateNode_ ss update child) items
		_ = items
	= LUINode type attr items changes effects

selectAttributesWithChanges_ :: UIAttributeSelection LUI -> UIAttributes
selectAttributesWithChanges_ selection lui = selectAttributes_ selection True lui

selectAttributesWithoutChanges_ ::UIAttributeSelection LUI -> UIAttributes
selectAttributesWithoutChanges_ selection lui = selectAttributes_ selection False lui

selectAttributes_ :: UIAttributeSelection Bool LUI -> UIAttributes
selectAttributes_ selection withChanges (LUINode _ attr _ changes _)
	# attr = if withChanges (applyAttributeChanges_ changes attr) attr
	= case selection of
		SelectAll         = attr
		(SelectKeys keys) = 'DM'.fromList [a \\ a=:(k,_) <- 'DM'.toList attr | isMember k keys]

matchKey_ :: UIAttributeSelection UIAttributeKey -> Bool
matchKey_ (SelectAll) _ = True
matchKey_ (SelectKeys keys) k = isMember k keys

applyAttributeChanges_ :: LUIChanges UIAttributes -> UIAttributes 
applyAttributeChanges_ {setAttributes,delAttributes} attr
	= 'DM'.delList ('DS'.toList delAttributes) ('DM'.union setAttributes attr)

overwriteAttribute_ :: UIAttribute (Map UIAttributeKey (LUIEffectStage JSONNode)) -> (Map UIAttributeKey (LUIEffectStage JSONNode))
overwriteAttribute_ (key,value) overwrittenAttributes
	# override = case 'DM'.get key overwrittenAttributes of
		//Not set yet
		Nothing = ESToBeApplied value
		Just (ESNotApplied) = ESToBeApplied value
		Just (ESToBeApplied _) = ESToBeApplied value
		//Already set,
		Just (ESApplied curValue)
			| curValue == value = ESApplied value 
			| otherwise         = ESToBeUpdated curValue value
		Just (ESToBeUpdated curValue _)
			| curValue == value = ESApplied value 
			| otherwise         = ESToBeUpdated curValue value
		Just (ESToBeRemoved curValue)
			| curValue == value = ESApplied value 
			| otherwise         = ESToBeUpdated curValue value
	= 'DM'.put key override overwrittenAttributes

hideAttribute_ :: (UIAttributeKey -> Bool) UIAttributeKey (Map UIAttributeKey (LUIEffectStage ())) -> (Map UIAttributeKey (LUIEffectStage ()))
hideAttribute_ condition key hiddenAttributes
	| isAlreadyHidden key hiddenAttributes
		= if (condition key)
			('DM'.put key (ESApplied ()) hiddenAttributes)
			('DM'.put key (ESToBeRemoved ()) hiddenAttributes)
	| otherwise
		= if (condition key)
			('DM'.put key (ESToBeApplied ()) hiddenAttributes)
			hiddenAttributes
where
	isAlreadyHidden key attr = case 'DM'.get key attr of
		Just (ESApplied _) = True
		Just (ESToBeUpdated _ _) = True
		Just (ESToBeRemoved _) = True
		_ = False

inLUISelection_ :: UISelection UIPath LUI -> Bool
inLUISelection_ (SelectByPath p) path _ = p === path
inLUISelection_ (SelectByDepth n) p _ = length p == n
inLUISelection_ (SelectDescendents) [_:_] _ = True
inLUISelection_ (SelectDescendents) _ _ = False
inLUISelection_ (SelectByType t) _ (LUINode type _ _ _ _) = t === type
inLUISelection_ (SelectByHasAttribute k) _ (LUINode _ attr _ changes _) = isJust ('DM'.get k (applyAttributeChanges_ changes attr))
inLUISelection_ (SelectByAttribute k p) _ (LUINode _ attr _ changes _) = maybe False p ('DM'.get k (applyAttributeChanges_ changes attr))
inLUISelection_ (SelectByNumChildren num) _ (LUINode _ _ items changes effects) = lengthAfterChanges_ items == num
inLUISelection_ (SelectByContains selection) path lui=:(LUINode _ _ items _ _)
	=  inLUISelection_ selection path lui
	|| or [inLUISelection_ (SelectByContains selection) (path ++ [i]) item \\ (Just i,item) <- indicesAfterChanges_ items]
inLUISelection_ (SelectRelative prefix sel) absolutePath ui
	= maybe False (\relativePath -> inLUISelection_ sel relativePath ui) (removePrefix prefix absolutePath)
where
	removePrefix [] psb = Just psb
	removePrefix [pa:psa] [pb:psb] = if (pa == pb) (removePrefix psa psb) Nothing
	removePrefix _ _ = Nothing

inLUISelection_ (SelectNone) _ _ = False
inLUISelection_ (SelectAND sell selr) path ui = inLUISelection_ sell path ui && inLUISelection_ selr path ui 
inLUISelection_ (SelectOR sell selr) path ui = inLUISelection_ sell path ui || inLUISelection_ selr path ui 
inLUISelection_ (SelectNOT sel) path ui = not (inLUISelection_ sel path ui)
inLUISelection_ _ _ _ = False

revertEffect_ :: LayoutRuleNo LUIEffects -> LUIEffects //TODO Not all effects can currently be identified by a ruleId
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
extractDownstreamChange :: LUI LUIExtractState -> (!UIChange,!LUI)
extractDownstreamChange (LUINode type attr items changes=:{toBeReplaced=Just replacement} effects) estate
	//When the UI is to be replaced, we need to determine the replacement UI with all effects applied
	# (ui,lui) = extractUIWithEffects replacement estate
	= (ReplaceUI ui, lui)
extractDownstreamChange lui=:(LUINode _ _ _ _ {LUIEffects|wrapper=ESToBeApplied _}) estate
	//New wrappings have to be done by full replacement
	# (ui,lui) = extractUIWithEffects lui estate
	= (ReplaceUI ui, lui)
extractDownstreamChange lui=:(LUINode _ _ _ _ {LUIEffects|wrapper=ESToBeRemoved _}) estate
	//The same holds for removal of  wrappings
	# (ui,lui) = extractUIWithEffects lui estate
	= (ReplaceUI ui, lui)
extractDownstreamChange lui=:(LUINode _ _ _ _ {LUIEffects|unwrapped=ESToBeApplied _}) estate
	//Same for unwrappings
	# (ui,lui) = extractUIWithEffects lui estate
	= (ReplaceUI ui, lui)
extractDownstreamChange lui=:(LUINode _ _ _ _ {LUIEffects|unwrapped=ESToBeRemoved _}) estate
	//And for removal of unwrappings
	# (ui,lui) = extractUIWithEffects lui estate
	= (ReplaceUI ui, lui)
extractDownstreamChange lui=:(LUINode _ _ _ _ _) estate
	//First check if there are moved nodes in this subtree, if so we first collect the changes that need to be done at the
	//the destination such that we can inject them at the right place
	# (LUINode type attr items changes effects,estate) = if (hasMovedNodes_ lui)
		(collectMoveDestinationChanges lui estate)
		(lui,estate)
	//Check overwritten ui-types: There is no way to set a type downstream, so a full replace is needed
	| typeNeedsUpdate type effects
		# (ui,lui) = extractUIWithEffects lui estate
		= (ReplaceUI ui,lui)
	//Hiding or unhiding can only be done by replacement for the top-level node
	| needsToBeHiddenOrUnhidden effects
		# (ui,lui) = extractUIWithEffects lui estate
		= (ReplaceUI ui,lui)
	//Determine changes to attributes
	# (attributeChanges,attr,effects) = extractAttributeChanges changes attr effects
	//Determine changes to children
	# unwrapped = effects.unwrapped =: (ESApplied _)
	# (childChanges,items) = extractChildChanges items estate unwrapped
	# change = case (unwrapped,attributeChanges,childChanges) of
		(True,_,[(_,ChangeChild change)]) = change
		(_,[],[])                         = NoChange
		_                                 = ChangeUI attributeChanges childChanges
	= (change, LUINode type attr items (resetChanges changes) effects)
where
	typeNeedsUpdate type {overwrittenType=ESToBeApplied _} = True
	typeNeedsUpdate type {overwrittenType=ESToBeUpdated _ _} = True
	typeNeedsUpdate type {overwrittenType=ESToBeRemoved _} = True
	typeNeedsUpdate _ _ = False

	needsToBeHiddenOrUnhidden {hidden=ESToBeApplied _} = True
	needsToBeHiddenOrUnhidden {hidden=ESToBeRemoved _} = True
	needsToBeHiddenOrUnhidden _ = False

	extractAttributeChanges changes=:{setAttributes,delAttributes} attr effects=:{overwrittenAttributes,hiddenAttributes}
		//Apply changes to the attributes
		# (attr,attrChanges)
			= foldl (applySetAttribute overwrittenAttributes hiddenAttributes) (attr,[]) ('DM'.toList setAttributes)
		# (attr,attrChanges)
			= foldl (applyDelAttribute overwrittenAttributes hiddenAttributes) (attr,attrChanges) ('DS'.toList delAttributes)
		//Apply remaining effects (these no longer affect the stored attributes)
		# (attrChanges,overwrittenAttributesList) = foldl (applyOverrideAttribute attr) (attrChanges,[]) ('DM'.toList overwrittenAttributes)
		# (attrChanges,hiddenAttributesList) = foldl (applyHideAttribute attr) (attrChanges,[]) ('DM'.toList hiddenAttributes)
		= (reverse attrChanges,attr,
		   {effects
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
		applyOverrideAttribute attr (attrChanges, overrides) (key,ESToBeApplied value)
			= ([SetAttribute key value:attrChanges], [(key,ESApplied value):overrides])
		applyOverrideAttribute attr (attrChanges, overrides) (key,ESApplied value) //Already applied
			= (attrChanges, [(key,ESApplied value):overrides])
		applyOverrideAttribute attr (attrChanges, overrides) (key,ESToBeUpdated _ value)
			= ([SetAttribute key value:attrChanges], [(key,ESApplied value):overrides])
		applyOverrideAttribute attr (attrChanges, overrides) (key,ESToBeRemoved _) //Either restore the original, or remove the attribute 
			= case 'DM'.get key attr of
				Nothing = ([DelAttribute key:attrChanges],overrides)
				Just value = ([SetAttribute key value:attrChanges],overrides)

		applyHideAttribute attr (attrChanges, hidden) (key,ESNotApplied)
			= (attrChanges,hidden)
		applyHideAttribute attr (attrChanges, hidden) (key,ESToBeApplied _)
			= ([DelAttribute key:attrChanges],[(key,ESApplied ()):hidden])
		applyHideAttribute attr (attrChanges, hidden) (key,ESApplied _)
			= (attrChanges,[(key,ESApplied ()):hidden])
		applyHideAttribute attr (attrChanges, hidden) (key,ESToBeUpdated _ _)
			= (attrChanges,[(key,ESApplied ()):hidden])
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

	//Find out what changes need to be done at the destination first
	collectMoveDestinationChanges lui=:(LUINode _ _ _ _ {containsMovesBy}) estate=:{movedChanges}
		//Foreach move in the set collect the changes
		# (LUINode type attr items changes effects,movedChanges,containsMovesBy)
			= foldr collectForDestination (lui,movedChanges,'DM'.newMap) ('DM'.toList containsMovesBy)
		= (LUINode type attr items changes {effects & containsMovesBy = containsMovesBy}, {estate & movedChanges = movedChanges})
	where
		collectForDestination (ruleId,numMoved) (lui,movedChanges,containsMovesBy) 
			# (_,changes, lui) = collect ruleId 0 lui
			= (lui, 'DM'.put ruleId changes movedChanges, if (numMoved > 0) ('DM'.put ruleId numMoved containsMovesBy) containsMovesBy)

		collect ruleId i (LUINode type attr items changes effects)
			# (i,reversedCollected,reversedItems) = foldl collect` (i,[],[]) items 
			= (i,reverse reversedCollected,LUINode type attr (reverse reversedItems) changes effects)
		where
			collect` (i,collected,acc) lui=:(LUINode type attr items changes effects) 
				| toBeMovedBy ruleId effects //Insert with effects at the destination
					# (ui,lui) = extractUIWithEffects lui estate
					= (i + 1, [(i,InsertChild ui):collected], [lui:acc])
				| isMovedBy ruleId effects //Extract the downstream changes, to be applied at the destination
					# (change,lui) = extractDownstreamChange lui estate
					= case change of
						NoChange = (i + 1, collected, [lui:acc])
						_        = (i + 1, [(i,ChangeChild change):collected], [lui:acc])
				| toBeUnMovedBy ruleId effects //Remove at the destination, but check for children that now need to be insterted
					# (i, collectedInChildren, lui) = collect ruleId i lui
					= (i, reverse collectedInChildren ++ [(i,RemoveChild):collected], [lui:acc])
				| otherwise //Collect changes in children
					# (i, collectedInChildren, lui) = collect ruleId i lui
					= (i, reverse collectedInChildren ++ collected, [lui:acc])
			collect` (i,collected,acc) lui = (i,collected,[lui:acc])

			toBeMovedBy ruleId {LUIEffects|moved=ESToBeApplied matchId} = ruleId == matchId
			toBeMovedBy _ _ = False

			isMovedBy ruleId {LUIEffects|moved=ESApplied matchId} = ruleId == matchId
			isMovedBy _ _ = False

			toBeUnMovedBy ruleId {LUIEffects|moved=ESToBeRemoved matchId} = ruleId == matchId
			toBeUnMovedBy _ _ = False

		collect ruleId i lui = (i,[],lui)

	extractChildChanges [] estate unwrapped = ([],[])
	extractChildChanges items estate unwrapped
		| unwrapped
			| differentFirstChild items
				# (_,items) = extractShifts items
				# ([ui:_],items) = extractUIsWithEffects items estate
				= ([(0,ChangeChild (ReplaceUI ui))],items)
			| otherwise
				# (_,[i:is]) = extractShifts items
				//Extract the changes of the first item, and just update the rest
				# (change,i) = extractDownstreamChange i estate
				# (_,is) = extractUIsWithEffects is estate
				= ([(0,ChangeChild change)],[i:is])
		| otherwise
			# (shifts,items) = extractShifts items
			# (insertsAndRemoves,items) = extractInsertsAndRemoves items
			= (shifts ++ insertsAndRemoves, items)
	where
		//When the parent is unwrapped, we may need to update the ui if another child ends up at position 0
		differentFirstChild [LUINode _ _ _ {toBeInserted=True} _:_]= True
		differentFirstChild [LUINode _ _ _ {toBeRemoved=True} _:_]= True
		differentFirstChild [LUINode _ _ _ {toBeShifted=Just _} _:_]= True
		differentFirstChild [LUIShiftDestination _:_]= True
		differentFirstChild items = False

		//Important: Shifts are done before inserts and removes
		//           so we ignore items that are not yet inserted, but still
		//           count items that are to be removed
		extractShifts items = extract 0 [] items
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

		extractInsertsAndRemoves items = extract 0 items
		where
			extract i [] = ([],[])
			extract i [x=:(LUINode _ _ _ {toBeRemoved=True} _):xs]
				# (cs,xs) = extract i xs
				= ([(i,RemoveChild):cs],xs)
			extract i [x=:(LUINode _ _ _ {toBeInserted=True} _):xs]
				# (ui,x) = extractUIWithEffects x estate
				# (cs,xs) = extract (i + 1) xs
				= ([(i,InsertChild ui):cs],[x:xs])
			extract i [x=:(LUINode _ _ _ _ {LUIEffects|additional=ESToBeApplied _}):xs]
				# (ui,x) = extractUIWithEffects x estate
				# (cs,xs) = extract (i + 1) xs
				= ([(i,InsertChild ui):cs],[x:xs])
			extract i [x=:(LUINode _ _ _ _ {LUIEffects|additional=ESToBeRemoved _}):xs]
				# (cs,xs) = extract i xs
				= ([(i,RemoveChild):cs],xs)

			extract i [x=:(LUINode type attr items changes effects=:{LUIEffects|hidden=ESToBeApplied ruleId}):xs]
				# (_,x) = extractDownstreamChange x estate
				# (cs,xs) = extract i xs
				= ([(i,RemoveChild):cs],[x:xs])
			extract i [x=:(LUINode type attr items changes effects=:{LUIEffects|hidden=ESApplied ruleId}):xs]
				# (_,x) = extractDownstreamChange x estate
				# (cs,xs) = extract i xs
				= (cs,[x:xs])
			extract i [x=:(LUINode type attr items changes effects=:{LUIEffects|hidden=ESToBeRemoved ruleId}):xs]
				# (ui,x) = extractUIWithEffects x estate
				# (cs,xs) = extract (i + 1) xs
				= ([(i,InsertChild ui):cs],[x:xs])

			extract i [x=:(LUINode type attr items changes effects=:{LUIEffects|moved=ESToBeApplied ruleId}):xs]
				# (_,x) = extractDownstreamChange x estate
				# (cs,xs) = extract i xs
				= ([(i,RemoveChild):cs],[confirmEffect x:xs])
			where
				confirmEffect (LUINode type attr items changes effects=:{LUIEffects|moved=ESToBeApplied ruleId})
					= LUINode type attr items changes {LUIEffects|effects & moved=ESApplied ruleId}
			extract i [x=:(LUINode type attr items changes effects=:{LUIEffects|moved=ESApplied ruleId}):xs]
				# (_,x) = extractDownstreamChange x estate
				# (cs,xs) = extract i xs
				= (cs,[x:xs])

			extract i [x=:(LUINode type attr items changes effects=:{LUIEffects|moved=ESToBeRemoved ruleId}):xs]
				# (ui,x) = extractUIWithEffects x estate
				# (cs,xs) = extract (i + 1) xs
				= ([(i,InsertChild ui):cs],[confirmEffect x:xs])
			where
				confirmEffect (LUINode type attr items changes effects)
					= LUINode type attr items changes {LUIEffects|effects & moved=ESNotApplied}

			extract i [x=:(LUIMoveDestination ruleId num):xs]
				# (cs,xs) = extract (i + num) xs
				// The moved changes are all indexed relative to 0.
				//At this point, with the actual index of the destination known, the indices of the changes can be computed
				# destinationChanges = [(idx + i,change) \\ (idx,change) <- fromMaybe [] ('DM'.get ruleId estate.LUIExtractState.movedChanges)]
				= (destinationChanges ++ cs,if (num == 0) xs [x:xs])
	
			extract i [x:xs]
				# (c,x) = extractDownstreamChange x estate
				# (cs,xs) = extract (i + 1) xs
				= case c of
					NoChange = (cs,[x:xs])
					_        = ([(i,ChangeChild c):cs],[x:xs])

extractDownstreamChange lui estate = (NoChange,lui)

extractUIWithEffects :: LUI LUIExtractState -> (!UI,!LUI)
extractUIWithEffects (LUINode ltype lattr litems changes=:{toBeReplaced=Just replacement} effects) estate
	= extractUIWithEffects replacement estate
extractUIWithEffects (LUINode ltype lattr litems changes effects=:{wrapper=ESToBeRemoved _}) estate
	= case dropWhile isAdditional_ litems of
		[wrapped:_] = extractUIWithEffects wrapped estate
		_           = abort "extractUIWithEffects: Wrapped item is missing"
extractUIWithEffects lui=:(LUINode _ _ _ _ _) estate
	//First collect moved ui's
	# (LUINode ltype lattr litems changes=:{setAttributes,delAttributes} effects=:{overwrittenType},estate) = if (hasMovedNodes_ lui)
		(collectMoveDestinationUIs lui estate)
		(lui,estate)
	//Update type
	# (type,effects) = case overwrittenType of
		ESNotApplied = (ltype,{effects & overwrittenType = ESNotApplied})
		ESToBeApplied otype = (otype,{effects & overwrittenType = ESApplied otype})
		ESApplied otype = (otype,{effects & overwrittenType = ESApplied otype})
		ESToBeUpdated _ otype = (otype,{effects & overwrittenType = ESApplied otype})
		ESToBeRemoved _ = (ltype,{effects & overwrittenType = ESNotApplied})
	//Update attributes and apply attribute effects
	# lattr = applyAttributeChanges_ changes lattr
	# (attr,effects) = applyAttributeEffects_ effects lattr
	//Remove items marked as removed
	# litems = filter (not o remove) litems
	//Move shifted items to their destinations
	# (sources,litems) = collectShiftSources litems
	# litems = replaceShiftDestinations sources litems
	//Recursively extract all effects
	# (items,litems) = extractUIsWithEffects litems estate
	//Determine the ui
	# ui = if (isHidden_ lui)
		(UI UIEmpty 'DM'.newMap [])
		(if (isUnwrapped_ lui) (hd items) (UI type attr items))
	= (ui, LUINode ltype lattr litems noChanges (confirmEffects effects))
where
	remove (LUINode _ _ _ {toBeRemoved=True} _) = True
	remove (LUINode _ _ _ {toBeReplaced=Nothing} {additional = ESToBeRemoved _}) = True
	remove _ = False

	collectMoveDestinationUIs lui=:(LUINode _ _ _ _ {containsMovesBy}) estate=:{movedUIs}
		//Foreach move in the set collect the ui's
		# (LUINode type attr items changes effects,movedUIs,containsMovesBy)
			= foldr collectForDestination (lui,movedUIs,'DM'.newMap) ('DM'.toList containsMovesBy)
		= (LUINode type attr items changes {effects & containsMovesBy = containsMovesBy}, {estate & movedUIs = movedUIs})
	where	
		collectForDestination (ruleId,numMoved) (lui,movedUIs,containsMovesBy) 
			# (uis, lui) = collect ruleId lui
			= (lui, 'DM'.put ruleId uis movedUIs, if (numMoved > 0) ('DM'.put ruleId numMoved containsMovesBy) containsMovesBy)

		collect ruleId (LUINode type attr items changes effects)
			# (reversedCollected,reversedItems) = foldl collect` ([],[]) items 
			= (reverse reversedCollected,LUINode type attr (reverse reversedItems) changes effects)
		where
			collect` (collected,acc) lui=:(LUINode type attr items changes effects) 
				| isMoved ruleId effects
					# (ui,lui) = extractUIWithEffects lui estate
					= ([ui:collected], [confirmMoved lui:acc])
				| otherwise //Collect changes in children
					# (collectedInChildren, lui) = collect ruleId lui 
					= (reverse collectedInChildren ++ collected, [confirmMoved lui:acc])
			collect` (collected,acc) lui = (collected,[lui:acc])

			isMoved ruleId {LUIEffects|moved=ESToBeApplied matchId} = ruleId == matchId
			isMoved ruleId {LUIEffects|moved=ESApplied matchId} = ruleId == matchId
			isMoved _ _ = False

			confirmMoved (LUINode type attr items changes effects=:{LUIEffects|moved=ESToBeApplied ruleId})
				= LUINode type attr items changes {LUIEffects|effects & moved=ESApplied ruleId}
			confirmMoved (LUINode type attr items changes effects=:{LUIEffects|moved=ESToBeRemoved ruleId})
				= LUINode type attr items changes {LUIEffects|effects & moved=ESNotApplied}
			confirmMoved lui = lui

		collect ruleId lui = ([],lui) 

	collectMoveDestinationUIs lui estate = (lui,estate)

	collectShiftSources items = foldr collect ('DM'.newMap,[]) items
	where
		collect n=:(LUINode _ _ _ {toBeShifted=Just shiftID} _) (sources,items) = ('DM'.put shiftID n sources,items)
		collect n (sources,items) = (sources,[n:items])

	replaceShiftDestinations sources items = foldr replace [] items
	where
		replace (LUIShiftDestination shiftID) items = maybe items (\n -> [resetToBeShifted n:items]) ('DM'.get shiftID sources)
		replace n items = [n:items]

	//TODO: Consider moving confirmation out of extract function, maybe to extract state
	confirmEffects effects=:{LUIEffects|additional=ESToBeApplied ruleId} = {LUIEffects|effects & additional=ESApplied ruleId}
	confirmEffects effects=:{LUIEffects|hidden=ESToBeApplied ruleId} = {LUIEffects|effects & hidden=ESApplied ruleId}
	confirmEffects effects=:{LUIEffects|hidden=ESToBeRemoved ruleId} = {LUIEffects|effects & hidden=ESNotApplied}
	confirmEffects effects=:{LUIEffects|wrapper=ESToBeApplied ruleId} = {LUIEffects|effects & wrapper=ESApplied ruleId}
	confirmEffects effects=:{LUIEffects|unwrapped=ESToBeApplied ruleId} = {LUIEffects|effects & unwrapped=ESApplied ruleId}
	confirmEffects effects=:{LUIEffects|unwrapped=ESToBeRemoved ruleId} = {LUIEffects|effects & unwrapped=ESNotApplied}
	confirmEffects effects = effects

extractUIWithEffects _ _ = abort "extractUIWithEffects: can only extract UI from LUINodes"

extractUIsWithEffects :: [LUI] LUIExtractState -> ([UI],[LUI])
extractUIsWithEffects litems estate=:{movedUIs} = foldr extract ([],[]) litems
where
	extract litem=:(LUIMoveDestination ruleId numItems) (items,litems)
		= (fromMaybe [] ('DM'.get ruleId movedUIs) ++ items, [litem:litems])
	extract litem (items,litems)
		| isRemoved_ litem = (items,litems)
		| isMoved_ litem = (items,[litem:litems])
		| otherwise
			# (item,litem) = extractUIWithEffects litem estate
			= (if (isHidden_ litem) items [item:items],[litem:litems])

resetToBeShifted (LUINode type attr items changes effects)
	= LUINode type attr items {changes & toBeShifted = Nothing} effects

resetChanges changes = {changes & setAttributes = 'DM'.newMap, delAttributes = 'DS'.newSet}

applyAttributeEffects_ effects=:{overwrittenAttributes,hiddenAttributes} attr
	# (attr,overwrittenAttributes) = foldl overwrite (attr,'DM'.newMap) ('DM'.toList overwrittenAttributes)
	# (attr,hiddenAttributes) = foldl hide (attr,'DM'.newMap) ('DM'.toList hiddenAttributes)
	= (attr, {effects & overwrittenAttributes = overwrittenAttributes, hiddenAttributes = hiddenAttributes})
where
	overwrite (attr,overrides) (key,ESNotApplied) = (attr,overrides)
	overwrite (attr,overrides) (key,ESToBeApplied value) = ('DM'.put key value attr, 'DM'.put key (ESApplied value) overrides)
	overwrite (attr,overrides) (key,ESApplied value) = ('DM'.put key value attr, 'DM'.put key (ESApplied value) overrides)
	overwrite (attr,overrides) (key,ESToBeUpdated _ value) = ('DM'.put key value attr, 'DM'.put key (ESApplied value) overrides)
	overwrite (attr,overrides) (key,ESToBeRemoved _) = (attr,overrides)

	hide (attr,hides) (key,ESNotApplied) = (attr,hides)
	hide (attr,hides) (key,ESToBeApplied _) = ('DM'.del key attr,'DM'.put key (ESApplied ()) hides)
	hide (attr,hides) (key,ESApplied _) = ('DM'.del key attr,'DM'.put key (ESApplied ()) hides)
	hide (attr,hides) (key,ESToBeUpdated _ _) = ('DM'.del key attr,'DM'.put key (ESApplied ()) hides)
	hide (attr,hides) (key,ESToBeRemoved _) = (attr,hides)

revertEffects :: LUI -> LUI
revertEffects (LUINode type attr items changes effects=:{additional,wrapper,unwrapped})
	//Remove existing additional nodes
	# additional = case additional of
		(ESApplied ruleId) = ESToBeRemoved ruleId
		_ 				   = additional
	//Remove newly added additional nodes
	# items = filter notNewAdditional items
	//Undo wrapping and unwrapping
	# wrapper = case wrapper of
		(ESApplied ruleId) = ESToBeRemoved ruleId
		_                  = wrapper
	# unwrapped = case unwrapped of
		(ESApplied ruleId) = ESToBeRemoved ruleId
		_                  = unwrapped

	= LUINode type attr (map revertEffects items) changes {effects & additional = additional, wrapper = wrapper, unwrapped = unwrapped}
where
	notNewAdditional (LUINode _ _ _ _ effects=:{additional=ESToBeApplied _}) = False
	notNewAdditional _ = True

revertEffects lui = lui

ruleBasedLayout :: LayoutExpression -> Layout
ruleBasedLayout expr = {Layout|apply,adjust,restore}
where
	apply ui
		= appSnd LSRule (extractDownstreamChange (applyLayoutEffects expr (initLUI False ui)) initLUIExtractState)
	adjust (change, LSRule lui)
		= appSnd LSRule (extractDownstreamChange (applyLayoutEffects expr (applyUpstreamChange change lui)) initLUIExtractState)
	restore (LSRule lui)
		= fst (extractDownstreamChange (revertEffects lui) initLUIExtractState) 

