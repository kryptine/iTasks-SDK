implementation module iTasks.UI.Layout

import StdTuple, StdList, StdBool, StdInt, StdOrdList, StdArray, StdMisc
import Data.Maybe, Data.Either, Text, Data.Tuple, Data.List, Data.Either, Data.Functor
import iTasks._Framework.Util, iTasks._Framework.HtmlUtil, iTasks.UI.Definition
import iTasks.API.Core.Types, iTasks.API.Core.TaskCombinators
import StdEnum
from Data.Map as DM import qualified put, get, del, newMap, toList, fromList, alter, union, keys, unions, singleton
from Data.Tuple import appSnd

from StdFunc import o, const, id, flip
from iTasks._Framework.TaskState import :: TIMeta(..), :: TaskTree(..), :: DeferredJSON
import StdDebug

import iTasks.Util.Trace
derive gPrettyTrace LayoutTree, LayoutRemoval, UIChange, UIChildChange, UI, UIAttributeChange, JSONNode, UINodeType, UISelection, Either

//This type records the states of layouts applied somewhere in a ui tree
derive JSONEncode LayoutState, LayoutTree, LayoutRemoval
derive JSONDecode LayoutState, LayoutTree, LayoutRemoval

instance tune ApplyLayout
where
	tune (ApplyLayout l) task=:(Task evala) = Task eval
	where
		eval event evalOpts (TCDestroy (TCLayout s tt)) iworld //Cleanup duty simply passed to inner task
			= evala event evalOpts (TCDestroy tt) iworld

		eval event evalOpts tt=:(TCInit _ _) iworld
			= eval event evalOpts (TCLayout JSONNull tt) iworld

		//On Reset events, we (re-)apply the layout
		eval ResetEvent evalOpts (TCLayout _ tt) iworld = case evala ResetEvent evalOpts tt iworld of
			(ValueResult value info (ReplaceUI ui) tt,iworld)
				//Determine the change the layout makes to the UI
				# (change,state) = l.Layout.apply ui
				//Modify the layout accorgingly
				# ui = applyUIChange change ui
				= (ValueResult value info (ReplaceUI ui) (TCLayout (toJSON state) tt), iworld)		
            (res,iworld) = (res,iworld)

		eval event evalOpts (TCLayout json tt) iworld = case evala event evalOpts tt iworld of
	        (ValueResult value info change tt,iworld) 
				= case fromJSON json of
					(Just s)	
						# (change,s) = l.Layout.adjust (change,s)
						= (ValueResult value info change (TCLayout (toJSON s) tt), iworld)
					Nothing	
						= (ExceptionResult (exception "Corrupt layout state"), iworld)
            (res,iworld) = (res,iworld)
		
		eval event evalOpts state iworld = evala event evalOpts state iworld //Catchall

//Test if a specific UI at a path is in the selection
inUISelection :: UISelection UIPath UI -> Bool
inUISelection (SelectByPath p) path _ = p === path
inUISelection (SelectChildren) [_] _ = True
inUISelection (SelectChildren) _ _ = False
inUISelection (SelectDescendents) [_:_] _ = True
inUISelection (SelectDescendents) _ _ = False
inUISelection (SelectByType t) _ (UI type _ _) = t === type
inUISelection (SelectByHasAttribute k) _ (UI _ attr _) = isJust ('DM'.get k attr)
inUISelection (SelectByAttribute k v) _ (UI _ attr _) = maybe False ((==) v) ('DM'.get k attr)
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
inUISelection (SelectAND sell selr) path ui = inUISelection sell path ui && inUISelection selr path ui 
inUISelection (SelectOR sell selr) path ui = inUISelection sell path ui || inUISelection selr path ui 
inUISelection (SelectNOT sel) path ui = not (inUISelection sel path ui)

inUISelectionAfterChange :: UISelection UIPath UI UIChange -> Bool
inUISelectionAfterChange selection path ui change //TODO: This needs a more efficient implemenation that does not apply the full change if it is not necessary
	= inUISelection selection path (applyUIChange change ui)

//A layout that has no effect at all
idLayout :: Layout 
idLayout = {Layout|apply=const (NoChange,LSNone),adjust=id,restore=const NoChange}

setUIType :: UINodeType -> Layout
setUIType type = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply ui=:(UI _ attr items) = (ReplaceUI (UI type attr items), LSType ui) //Crude replacement (no instruction possible)

	adjust (NoChange,s)   = (NoChange,s)
	adjust (ReplaceUI ui,_) = apply ui 
	adjust (change,LSType ui) = (change, LSType (applyUIChange change ui))

	//Crude restore...
	restore (LSType ui) = ReplaceUI ui 

setUIAttributes :: UIAttributes -> Layout
setUIAttributes extraAttr = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	
	apply (UI _ attr _)
		= (ChangeUI [SetAttribute k v \\ (k,v) <- 'DM'.toList extraAttr] [],LSAttributes attr)

	adjust (ChangeUI attrChanges itemChanges,LSAttributes attr)
		//Update the shadow attributes
		# attr = foldl (flip applyUIAttributeChange) attr attrChanges
		//Filter out updates for the attributes that this layout has overwritten setting here
		# attrChanges = filter (not o matchChange) attrChanges
		= (ChangeUI attrChanges itemChanges, LSAttributes attr)
	where
		matchChange (SetAttribute k _) = isMember k ('DM'.keys extraAttr)
		matchChange (DelAttribute k) = isMember k ('DM'.keys extraAttr)

	adjust (ReplaceUI ui,LSAttributes attr)
		# (change,state) = apply ui
		= (ReplaceUI (applyUIChange change ui),state)
	adjust (change,s) = (change,s)

	restore (LSAttributes attr) //Restore or delete the extra attributes
		= ChangeUI [maybe (DelAttribute k) (\v -> SetAttribute k v) ('DM'.get k attr) \\ k <- 'DM'.keys extraAttr] []

delUIAttributes :: UIAttributeSelection -> Layout 
delUIAttributes selection = {Layout|apply=apply,adjust=adjust,restore=restore} 
where
	apply (UI _ attr _) //There is no delete instruction, so deleting means setting the value to null 
		= (ChangeUI [DelAttribute k \\ k <- 'DM'.keys attr | matchKey selection k] [],LSAttributes attr)

	adjust (ChangeUI attrChanges itemChanges,LSAttributes attr)
		//Update the shadow attributes
		# attr = foldl (flip applyUIAttributeChange) attr attrChanges
		//Remove changes that affect the deleted attributes
		# attrChanges = filter (not o matchChange) attrChanges
		= (ChangeUI attrChanges itemChanges,LSAttributes attr)
	where
		matchChange (SetAttribute k _) = matchKey selection k
		matchChange (DelAttribute k) = matchKey selection k 

	adjust (ReplaceUI ui,_)
		# (change,state) = apply ui
		= (ReplaceUI (applyUIChange change ui),state)

	adjust (change,s) = (change,s)
	
	restore (LSAttributes attr) //Restore the attributes
		= ChangeUI [SetAttribute k v \\ (k,v) <- 'DM'.toList attr | matchKey selection k] []

modifyUIAttributes :: UIAttributeSelection (UIAttributes -> UIAttributes) -> Layout
modifyUIAttributes selection modifier = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply (UI type attr items)
		# mods = modifier (selectAttributes selection attr)
		= (ChangeUI [SetAttribute k v \\ (k,v) <- 'DM'.toList mods] [],LSAttributeChanges attr mods)

	adjust (ChangeUI attrChanges childChanges, LSAttributeChanges attr mods)
		//Update the shadow attributes
		# attr = foldl (flip applyUIAttributeChange) attr attrChanges
		//Recompute the modifications
		# newMods = modifier (selectAttributes selection attr)
		# modChanges = diffAttributes mods newMods
		//Remove changes that affect the modified attributes
		# attrChanges = filter (not o (matchMods ('DM'.keys newMods))) attrChanges
		= (ChangeUI (attrChanges ++ modChanges) childChanges, LSAttributeChanges attr newMods)
	where
		matchMods modKeys (SetAttribute k _) = isMember k modKeys
		matchMods modKeys (DelAttribute k)   = isMember k modKeys
	
	adjust (ReplaceUI ui,_)
		# (change,state) = apply ui
		= (ReplaceUI (applyUIChange change ui),state)
	adjust (change,s) = (change,s)

	restore (LSAttributeChanges attr mods) //Restore the attributes
		= ChangeUI [SetAttribute k v \\ (k,v) <- 'DM'.toList attr] []

copySubUIAttributes :: UIAttributeSelection UIPath UIPath -> Layout
copySubUIAttributes selection src dst = {Layout|apply=apply,adjust=adjust,restore=restore} 
where
	apply ui = case selectAttr src ui of //Check if source exists
		Just srcAttr = case selectAttr dst ui of //Check if the destination exists
 			Just dstAttr = (changeAtPath dst (ChangeUI [SetAttribute k v \\ (k,v) <- 'DM'.toList srcAttr | matchKey selection k] []), LSAttributeChanges srcAttr dstAttr)
			Nothing      = (NoChange, LSAttributeChanges srcAttr 'DM'.newMap)
		Nothing = (NoChange, LSAttributeChanges 'DM'.newMap 'DM'.newMap)

	adjust (NoChange, s) 
		= (NoChange, s)

	adjust (ReplaceUI ui,_)
		# (change,state) = apply ui
		= (ReplaceUI (applyUIChange change ui),state)

	adjust (change, LSAttributeChanges srcAttr dstAttr)
		//Determine the effect of the change to both the src and dst attributes
		# srcChanges = selectChanges src change
		# dstChanges = selectChanges dst change
		//Update the 'shadow' attributes
		# srcAttr = foldl (flip applyUIAttributeChange) srcAttr srcChanges
		# dstAttr = foldl (flip applyUIAttributeChange) dstAttr dstChanges
		//Determine the modified change in the destination
		# change = mergeUIChanges change (changeAtPath dst (ChangeUI (filter (matchChange selection) srcChanges) []))
		//TODO: Should actually replace the updates in the dst location, not just set original values first and then the copies
		= (change, LSAttributeChanges srcAttr dstAttr)

	adjust (change,s) = (change,s)

	//Find the attributes of a sub ui
	selectAttr [] (UI type attr items) = Just attr
	selectAttr [s:ss] (UI type attr items) 
		| s >= 0 && s < length items = selectAttr ss (items !! s)
							         = Nothing

	//Determine changes to the attributes of a sub ui
	selectChanges _ NoChange = []
	selectChanges ss (ReplaceUI ui) = case selectAttr ss ui of
		Just attr = [SetAttribute k v \\ (k,v) <- 'DM'.toList attr]
		Nothing = [] //FIXME this should produce deletes for all current attributes

	selectChanges [] (ChangeUI attrChanges _) = attrChanges
	selectChanges [s:ss] (ChangeUI _ childChanges) = flatten (map selectChildChanges childChanges)
	where	
		selectChildChanges (i,ChangeChild c) = if (i == s) (selectChanges ss c) []
		selectChildChanges _ = [] //FIXME Properly deal with inserts and removes

	matchChange selection (SetAttribute k v) = matchKey selection k
	matchChange selection (DelAttribute k) = matchKey selection k

	//TODO, track which attributes were chagned and restore accordingly
	restore _ = NoChange

matchKey (SelectAll) _ = True
matchKey (SelectKeys keys) k = isMember k keys

selectAttributes SelectAll attr = attr
selectAttributes (SelectKeys keys) attr = 'DM'.fromList [a \\ a=:(k,_) <- 'DM'.toList attr | isMember k keys]

//Set attributes in 'new' if they are different than, or not found in 'old'
//Remove attributes that were in old, but are no longer in new
diffAttributes old new = [SetAttribute k v \\ (k,v) <- 'DM'.toList new | maybe True (\ov -> ov <> v) ('DM'.get k old)] 
					  ++ [DelAttribute k \\ k <- 'DM'.keys old | isNothing ('DM'.get k new)]

wrapUI :: UINodeType -> Layout
wrapUI type = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply ui = (ReplaceUI (uic type [ui]), LSWrap ui)

	adjust (ReplaceUI ui,_) = apply ui 

	adjust (NoChange,s)   = (NoChange,s)
	adjust (change,LSWrap ui) 
		= (ChangeUI [] [(0,ChangeChild change)],LSWrap (applyUIChange change ui))

	//Crude restore...
	//As long as the UIChange type does not support moving elements up and down the tree we cannot do better
	restore (LSWrap ui) = ReplaceUI ui 

unwrapUI :: Layout
unwrapUI = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply ui=:(UI type attr [i:is]) = (ReplaceUI i, LSUnwrap ui)
	apply ui 					    = (NoChange, LSUnwrap ui)	

	adjust (ReplaceUI ui,_)
		# (change,state) = apply ui
		= (ReplaceUI (applyUIChange change ui), state)

	adjust (ChangeUI attrChanges childChanges, LSUnwrap	ui)
		//First update attributes
		# ui = applyUIChange (ChangeUI attrChanges []) ui
		//Process the child changes
		# (change, ui) = foldl adjust` (NoChange, ui) childChanges
		= (change, LSUnwrap ui)
	where
		adjust` (change, ui) c=:(n, ChangeChild cc)
			= (if (n == 0) (mergeUIChanges cc change) change, applyUIChange (ChangeUI [] [c]) ui)

		//When the first element (the one that was unwrapped) is removed, the first sibling is now the unwrapped element.
		//If there is no first sibling, we undo the unwrapping by replacing with the stored ui
		adjust` (change, UI type attr [_,i:is]) (0, RemoveChild) = (ReplaceUI i, UI type attr [i:is])
		adjust` (change, UI type attr [_]) (0, RemoveChild) = (ReplaceUI (UI type attr []), UI type attr [])
		adjust` (change, ui) (n, RemoveChild) = (change, applyUIChange (ChangeUI [] [(n,RemoveChild)]) ui)

		//When a new element is inserted at position 0, it should now be the shown element
		adjust` (change, ui) c=:(n, InsertChild i)
			= (if (n == 0) (ReplaceUI i) change, applyUIChange (ChangeUI [] [c]) ui)

		//When a move affects the first position, we need to update the shown element
		adjust` (change, ui) c=:(nfrom, MoveChild nto)
			# ui = applyUIChange (ChangeUI [] [c]) ui
			| nfrom == 0 || nto == 0
				= case ui of (UI _ _ [i:_]) = (ReplaceUI i, ui) ; _ = (change, ui)
			| otherwise 
				= (change, ui)

	adjust change = change

	//Crude restore...
	//As long as the UIChange type does not support moving elements up and down the tree we cannot do better
	restore (LSUnwrap ui) = ReplaceUI ui 

flattenUI :: Layout //TODO
flattenUI = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply _ = (NoChange,LSNone)
	adjust change = change
	restore _ = NoChange

//Insert the element at the specified index.
//Only insert if there are at least as many children as the specified index
insertChildUI :: Int UI -> Layout
insertChildUI idx insert
	| idx >= 0  = {Layout|apply=apply,adjust=adjust,restore=restore}
				= idLayout
where
	apply (UI _ _ items) 
		# num = length items
		| idx >= 0 && idx <= num = (ChangeUI [] [(idx,InsertChild insert)],LSInsert num)
					             = (NoChange, LSInsert num)

	adjust (NoChange,state) = (NoChange,state)

	adjust (ReplaceUI ui, _) //We are replacing everything, so don't keep state
		# (change,state) = apply ui
		= (ReplaceUI (applyUIChange change ui), state)

	//Adjust the child changes to account for the additional insert
	adjust (ChangeUI attrChanges childChanges, LSInsert num) 
		# (childChanges,num) = adjustChildChanges childChanges num
		= (ChangeUI attrChanges childChanges, LSInsert num)

	adjustChildChanges [] num = ([],num)
	adjustChildChanges [(i,ChangeChild change):cs] num
		| i >= 0 && i < num //The child is in a valid range
			//Adjust everything 'after' the inserted element
			# (cs,num) = adjustChildChanges cs num
			= ([(if (i < idx) i (i + 1),ChangeChild change):cs], num)
		| otherwise = adjustChildChanges cs num //The change is targeted out of range, drop it 
	adjustChildChanges [(i,RemoveChild):cs] num
		| i >= 0 && i < num //The child is in a valid range
			//This removal means we also have to remove the inserted element
			| num == idx 
				# (cs,num) = adjustChildChanges cs (num - 1)
				= ([(idx,RemoveChild),(i,RemoveChild):cs], num)
			//The element has not been inserted because there are too few children, there is no effect 
			| num < idx
				# (cs,num) = adjustChildChanges cs (num - 1)
				= ([(i,RemoveChild):cs], num)
			//The extra element was inserted:
			// If we remove an element 'after' the inserted index we have to offset the index of the removal
			| i >= idx
				# (cs,num) = adjustChildChanges cs (num - 1)
				= ([(i + 1, RemoveChild):cs], num)
			// If we remove an element 'before' the inserted element
			// it affects its position, so we need to move its sibling to adjust for that.
			| otherwise
				# (cs,num) = adjustChildChanges cs (num - 1)
				= ([(i,RemoveChild),(idx - 1,MoveChild idx):cs], num)
		| otherwise = adjustChildChanges cs num //The change is targeted out of range, drop it 

	adjustChildChanges [(i,InsertChild ui):cs] num
		| i >= 0 && i <= num //The child is in a valid range
			//This addition means we have to insert the extra element now
			| num == (idx - 1)
				# (cs,num) = adjustChildChanges cs (num + 1)
				= ([(i,InsertChild ui),(idx,InsertChild insert):cs], num)
			//The element has not been inserted because (even with this insert) there are too few children, there is no effect 
			| num < (idx - 1)
				# (cs,num) = adjustChildChanges cs (num + 1)
				= ([(i,InsertChild ui):cs], num)
			//The extra element was inserted:
			// If we insert an element 'after' the inserted index we have to offset the index of the insert
			| i >= idx
				# (cs,num) = adjustChildChanges cs (num + 1)
				= ([(i + 1,InsertChild ui):cs], num)
			// If we insert an element 'before' the inserted element
			// it affects its position, so we need to move its sibling to adjust for that.
			| otherwise
				# (cs,num) = adjustChildChanges cs (num + 1)
				= ([(i,InsertChild ui),(idx + 1,MoveChild idx):cs], num)
		| otherwise = adjustChildChanges cs num //The change is targeted out of range, drop it 
	adjustChildChanges [(s,MoveChild d):cs] num
		| s >= 0 && s < num && d >= 0 && d < num //Both source and destination are in a valid range
			//The element has not been inserted, there is no effect
			| num < idx 
				# (cs,num) = adjustChildChanges cs num 
				= adjustChildChanges [(s,MoveChild d):cs] num
			//Both are 'before' the inserted element, there is no effect
			| s < idx && d < idx
				# (cs,num) = adjustChildChanges cs num 
				= adjustChildChanges [(s,MoveChild d):cs] num
			//Only the source is 'before' the inserted element. We need to offset the destination and adjust
			| s < idx 
				# (cs,num) = adjustChildChanges cs num
				= adjustChildChanges [(s,MoveChild (d + 1)),(idx - 1,MoveChild idx):cs] num
			//Only the destination is 'before' the in
			| d < idx 	
				# (cs,num) = adjustChildChanges cs num
				= adjustChildChanges [(s + 1, MoveChild d),(idx + 1,MoveChild idx):cs] num
			//Both are 'after' the inserted element, offset the indices
			| otherwise
				# (cs,num) = adjustChildChanges cs num 
				= adjustChildChanges [(s + 1,MoveChild (d + 1)):cs] num
		| otherwise = adjustChildChanges cs num //The change is targeted out of range, drop it 

	//Check in the state if the extra element was inserted or not
	restore (LSInsert num)
		| idx >= 0 && idx <= num = ChangeUI [] [(idx,RemoveChild)]
					             = NoChange

removeSubUIs :: UISelection -> Layout
removeSubUIs selection = moveSubUIs` selection Nothing

moveSubUIs :: UISelection UIPath Int -> Layout 
moveSubUIs selection path pos = moveSubUIs` selection (Just (path,pos)) 

moveSubUIs` :: UISelection (Maybe (!UIPath,!Int)) -> Layout
moveSubUIs` selection mbDst = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply ui
		//First remove and mark 
		# (cchange, state) = applyRem [] ui
		# removeChange = case cchange of
			(ChangeChild change) = change
			(RemoveChild)        = ReplaceUI (UI UIEmpty 'DM'.newMap []) //If the root-level UI needs to be removed, replace it with UIEmpty
		//If there is a destination path, insert all removed ui's at that location
		# (destinationChange, state) = maybe (NoChange,state) (\dst -> determineDestinationChange dst ui state) mbDst
		= (mergeUIChanges removeChange destinationChange, LSRemoveSubUIs ui state)

	//Handle the removal of the selected sub-UI's
	applyRem path ui=:(UI type attr items)
		| not (onDestinationPath path) && inUISelection selection path ui
			= (RemoveChild, UIModified (LRRemoved 0))
		| otherwise	
			# (childChanges,childStates) = remove 0 0 items
			= (ChangeChild (ChangeUI [] childChanges), SubUIsModified [] childStates)
	where
		remove i n [] = ([],[])
		remove i n [ui:uis]
			# (cchange,state) = applyRem (path ++ [i]) ui
			# (changes,states) = remove (i + 1) (if (state =:(UIModified _)) n (n + 1)) uis
			= ([(n,cchange):changes], case state of (SubUIsModified _ []) = states ; _ = [(i,state):states])

	adjust (change, LSRemoveSubUIs ui state)
		//First adjust for the removals
		# (cchange, numRestored, ui, state) = adjustRem [] change ui state 
		# change = case cchange of
			(ChangeChild change) = change
			(RemoveChild)        = ReplaceUI (UI UIEmpty 'DM'.newMap []) //If the root-level UI needs to be removed, replace it with UIEmpty
			(InsertChild ui)     = ReplaceUI ui           				 //If the root-level UI is going to be restored, replace it
		//If there is a target, also update the target location
		# (change, state) = maybe (change,state) (\dst -> adjustIns dst change ui state) mbDst
		= (change, LSRemoveSubUIs ui state)

	//If there is no change, but a node was moved to a different path, we recheck the selection
	adjustRem path NoChange ui state=:(UIModified removal)
		= case applyRem path ui of
			(RemoveChild, UIModified _) = (ChangeChild NoChange, 0, ui,UIModified removal)
			(ChangeChild change, SubUIsModified _ mods)
				= (InsertChild (applyUIChange change ui), 1, ui, SubUIsModified [] mods)

	adjustRem path NoChange ui state=:(SubUIsModified restores mods)
		| not (onDestinationPath path) && inUISelection selection path ui
			//Removing this ui means that earlier matches of descendents are no longer separate ui's in the destination
			# numMoved = ltCount True (\s -> s =: (LRMoved _)) mods
			# numRestored = sum (map snd restores) 
			= (RemoveChild, 0, ui, UIModified (LRRemoved (numMoved + numRestored)))
		| otherwise
			= (ChangeChild NoChange, 0, ui, state)

	adjustRem path (ReplaceUI ui) _ state
		//First determine how many nodes were already moved in the current ui
		# numMoved = case state of
			(UIModified (LRMoved _)) = 1
			(UIModified (LRRemoved _)) = 0
			(SubUIsModified _ mods) = ltCount True (\s -> s =: (LRMoved _)) mods
		//Apply the layout to the replacement UI
		= case applyRem path ui of
			(RemoveChild, state)
				= (RemoveChild, numMoved, ui, state)
			(ChangeChild change,SubUIsModified _ mods )
				= (ChangeChild (ReplaceUI (applyUIChange change ui)), numMoved, ui, SubUIsModified [] mods)

	//The UI was removed earlier
	adjustRem path change=:(ChangeUI attrChanges childChanges) ui (UIModified removal)
		//Update the 'shadow' UI
		# ui = applyUIChange change ui
		//Check if the UI should still be removed after the effects of the change 
		= case applyRem path ui of
			(RemoveChild,state)
				//Store the change for application in the target location
				# removal = case removal of 
					(LRMoved curChange) = LRMoved (mergeUIChanges curChange change)
					_					= removal
				= (ChangeChild NoChange, 0, ui, UIModified removal)
			//Restore the UI, but make sure that the layout still applies to the children of the UI
			(ChangeChild change, state)
				= (InsertChild (applyUIChange change ui), 1, ui, state)

	adjustRem path change=:(ChangeUI attrChanges childChanges) ui state=:(SubUIsModified restores mods)
		| not (onDestinationPath path) && inUISelectionAfterChange selection path ui change
			//If the change causes the selection to match
			# ui = applyUIChange change ui
			//Removing this ui means that earlier matches of descendents are no longer separate ui's in the destination
			# numMoved = ltCount True (\s -> s =: (LRMoved _)) mods
			# numRestored = sum (map snd restores) 
			= (RemoveChild, 0, ui, UIModified (LRRemoved (numMoved + numRestored)))
		| otherwise
			//Update the attributes in the 'shadow' UI
			# (UI type attr items) = applyUIChange (ChangeUI attrChanges []) ui
			//Adjust the child changes
			# (childChanges, items, restores, mods) = adjustRemChildChanges childChanges items ('DM'.fromList restores) mods
			= (ChangeChild (ChangeUI attrChanges childChanges), 0, UI type attr items, SubUIsModified ('DM'.toList restores) mods)
	where
		adjustRemChildChanges [] items restores mods = ([], items, restores, mods)
		adjustRemChildChanges [(i,c):cs] items restores mods
			# (c, items, restores, mods) = adjustRemChildChange i c items restores mods
			# (cs, items, restores, mods) = adjustRemChildChanges cs items restores mods
			= (c ++ cs, items, restores, mods)

		adjustRemChildChange i (ChangeChild change) items restores mods
			| i >= 0 && i < length items 
				//Recursively adjust the change
				# (cchange, numRestores, item, mod) = adjustRem (path ++ [i]) change (items !! i) (ltGet i mods)
				# changes = case cchange of
					(ChangeChild NoChange)         = []
					(ChangeChild (ChangeUI [] [])) = []
					_                              = [(adjustIndex i mods, cchange)]
				# restores = if (numRestores > 0) ('DM'.alter (\n -> Just (fromMaybe 0 n + numRestores)) i restores) restores
				= (changes, updateAt i item items, restores, ltPut i mod mods)
			| otherwise //Out of range, ignore
				= ([],items,restores,mods)

		adjustRemChildChange i (InsertChild ui) items restores mods
			| i >= 0 && i <= length items 
				# (rchange,mod) = applyRem (path ++ [i]) ui
				//If the child is immediately matched, don't insert insert it upstream
				# cchange = case rchange of
					RemoveChild = []
					ChangeChild change = [(adjustIndex i mods, InsertChild (applyUIChange change ui))]
				//The insertion potentially affects all siblings after the insertion point, we need to check them
				# (schanges, items, restores, mods) = adjustRemSiblings path (\x -> x > i) (insertAt i ui items) restores (ltInsert i mod mods)
				= (cchange ++ schanges, items, restores, mods)
			| otherwise //Out of range, ignore
				= ([],items,restores,mods)

		adjustRemChildChange i RemoveChild items restores mods
			| i >= 0 && i < length items 
				# (cchange,restores) = case (ltGet i mods) of
					//If the child was already removed by this layout it no longer need to be removed from the UI:
					//The child was already moved to the destination, we also need to remove it there
					UIModified (LRMoved _)   = ([],'DM'.alter (\n -> Just (fromMaybe 0 n + 1)) i restores) 
					UIModified (LRRemoved r) = ([],'DM'.alter (\n -> Just (fromMaybe 0 n + r)) i restores)
					//If the child was yet not removed by the layout it will now removed.
					//We therefore additionally we need to check for moved descendents. If this ui is removed its moved parts should be removed too.
					SubUIsModified srestores smods 
						# numMoved = ltCount True (\s -> s =: (LRMoved _)) smods
						# numRestored = sum (map snd srestores) 
						# restores = 'DM'.alter (\n -> Just (fromMaybe 0 n + numMoved + numRestored)) i restores
						= ([(adjustIndex i mods, RemoveChild)],restores)
				# (schanges, items, restores, mods) = adjustRemSiblings path (\x -> x >= i) (removeAt i items) restores (ltRemove i mods)
				= (cchange ++ schanges, items, restores, mods)
			| otherwise //Out of range, ignore
				= ([],items,restores,mods)

		adjustRemChildChange i (MoveChild d) items restores mods
			| i >= 0 && i < length items &&  d >= 0 && d < length items
				# cchange = (adjustIndex i mods, MoveChild (adjustIndex d mods))
				# (schanges, items, restores, mods) = adjustRemSiblings path (const True) (listMove i d items) restores (ltMove i d mods) //TODO we don't need to check *all* siblings
				= ([cchange:schanges], items, restores, mods)
			| otherwise //Out of range, ignore
				= ([],items,restores,mods)

		//For the selected items call adjustRem with NoChange to check if they need to be removed or restored
		adjustRemSiblings path whichSiblings items restores mods = adjust 0 items restores mods
		where
			adjust i [] restores mods = ([],[],restores,mods)
			adjust i [item:items] restores mods
				| whichSiblings i
					//Check the ui
					# (cchange, numRestores, item, mod) = adjustRem (path ++ [i]) NoChange item (ltGet i mods)
					# change = case cchange of
						(ChangeChild NoChange)         = []
						(ChangeChild (ChangeUI [] [])) = []
						_                              = [(adjustIndex i mods,cchange)]
					//Update the amount of restored items for this position
					# restores = if (numRestores > 0) ('DM'.alter (\n -> Just (fromMaybe 0 n + numRestores)) i restores) restores
					//Check the remaining items
					# (changes, items, restores, mods) = adjust (i + 1) items restores (ltPut i mod mods)
					= (change ++ changes, [item:items], restores, mods)
				| otherwise
					# (changes, items, restores, mods) = adjust (i + 1) items restores mods
					= (changes, [item:items], restores, mods)

	//Check if the destination for moving elements is this node or one of its descendents
	onDestinationPath path = maybe False (startsWith path o fst) mbDst
	where
		startsWith [] dst = True
		startsWith [p:ps] [d:ds] = if (p == d) (startsWith ps ds) False
		startsWith _ _ = False

	//Correct an index for the number of removed sibling preceding it
	adjustIndex idx mods = idx - foldr (\(i,m) n -> if (i <= idx && (m =: (UIModified _))) (n + 1) n) 0 mods

	adjustIns dst change ui state
		//FIXME: Too simple...
		# (achange,state) = determineDestinationChange dst ui state
		= (mergeUIChanges change achange,state)

	//Handle the insertion of the removed sub-UI's in the target location
	determineDestinationChange (path,pos) ui state = case determineAdjustedPath path ui state of
		Just path
				//Extract the child changes to the destionation from the layout state
				# (_,changes,state) = collect 0 state ui
				| changes =:[] = (NoChange, state)
			                   = (changeAtPath path (ChangeUI [] changes), state)
		Nothing
			//Clear all state about moves (the destination apparently no longer exists
			= (NoChange, clear state)
	where
		collect n (UIModified (LRRemoved numBefore)) ui //A new removal
			= (n + 1, repeatn numBefore (n,RemoveChild) ++ [(n,InsertChild ui)], UIModified (LRMoved NoChange))
		collect n (UIModified (LRMoved NoChange)) ui   //An old removal, no need to change anything..
			= (n + 1, [], UIModified (LRMoved NoChange))
		collect n (UIModified (LRMoved change)) ui     //An old removal, that was changed in the source location
			# changes = case change of
				NoChange = []
				(ChangeUI [] []) = []
				_                = [(n,ChangeChild change)]
			= (n + 1, changes, UIModified (LRMoved NoChange))
		collect n (SubUIsModified reverts mods) (UI _ _ items) //Recursive case
			//Create an interleaved list from the reverts and the branches with modifications
			# potential = sortBy collectOrder ((map Left reverts) ++ [Right (i,m,items !! i) \\ (i,m) <- mods])
			# (n`, changes, mods) = collectInChildren n potential
			= (n`, changes, SubUIsModified [] mods)

		collectInChildren n [] = (n,[],[])
		collectInChildren n [Left (i,num):ms] 
			# cchanges = repeatn num (n,RemoveChild)
			# (n,rchanges,ms) = collectInChildren n ms
			= (n, cchanges ++ rchanges, ms)
		collectInChildren n [Right (i,m,ui):ms] 
			# (n,cchanges,m)  = collect n m ui
			# (n,rchanges,ms) = collectInChildren n ms
			= (n, cchanges ++ rchanges, [(i,m):ms])

		collectOrder (Left (i1,_)) (Left (i2,_)) = i1 < i2
		collectOrder (Right (i1,_,_)) (Right (i2,_,_)) = i1 < i2
		//For the same index, reverts (Left) are handled before mods (Right)
		collectOrder (Left (i1,_)) (Right (i2,_,_)) = if (i1 == i2) True (i1 < i2)
		collectOrder (Right (i1,_,_)) (Left (i2,_)) = if (i1 == i2) False (i1 < i2)

		clear (UIModified _) = UIModified (LRRemoved 0)
		clear (SubUIsModified _ mods) = SubUIsModified [] (map (appSnd clear) mods)
	
		determineAdjustedPath :: UIPath UI (LayoutTree LayoutRemoval LayoutRestores) -> Maybe UIPath
		determineAdjustedPath _ _ (UIModified _) = Nothing //The destination is part of removed node
		determineAdjustedPath [] _ _             = Just [] //The path (root node) is ok
		determineAdjustedPath [s:ss] (UI _ _ items) (SubUIsModified _ mods)
			| s >= 0 && s < length items 
				= fmap (\ss`-> [adjustIndex s mods:ss`]) (determineAdjustedPath ss (items !! s) (ltGet s mods))
				= Nothing //Out of range

	restore (LSRemoveSubUIs ui _) = ReplaceUI ui //VERY CRUDE RESTORE..

layoutSubUIs :: UISelection Layout -> Layout
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
			| i >= length items = abort "adjustChildChange index too large"
			# (change, item, state) = adjust` (path ++ [i]) change (items !! i) (ltGet i states)
			= (case change of NoChange = []; _ = [(i,ChangeChild change)], updateAt i item items, ltPut i state states)
		adjustChildChange i (InsertChild ui) items states
			//(potentially) apply the layout to the inserted item
			# (change,state) = apply` (path ++ [i]) ui
			//Check the siblings, because their path has changed
			# (schanges, items, states) = adjustSiblings path (\x -> x > i) (insertAt i ui items) (ltInsert i state states)
			= ([(i,InsertChild (applyUIChange change ui)):schanges], items, states)
		adjustChildChange i RemoveChild items states
			//Check the siblings, because their path has changed
			# (schanges, items, states) = adjustSiblings path (\x -> x >= i) (removeAt i items) (ltRemove i states)
			= ([(i,RemoveChild):schanges], items, states)
		adjustChildChange i (MoveChild d) items states
			//Check the siblings, because their path has changed //TODO: We can do better... don't need to check all
			# (schanges, items, states) = adjustSiblings path (const True) (listMove i d items) (ltMove i d states)
			= ([(i,MoveChild d):schanges], items, states)

		adjustSiblings path whichSiblings items states = adjust 0 items states
		where
			adjust i [] states = ([],[],states)
			adjust i [item:items] states
				| whichSiblings i
					//Check
					# (change,_,state) = adjust` (path ++ [i]) NoChange item (ltGet i states)
					//Check the remaining items
					# (changes, items, states) = adjust (i + 1) items (ltPut i state states)
					= case change of 
						NoChange = (changes, items, states)
						_        = ([(i,ChangeChild change):changes], items, states)
				| otherwise
					# (changes, items, states) = adjust (i + 1) items states
					= (changes, [item:items], states)

	restoreSubUIs (UIModified state) = layout.Layout.restore state
	restoreSubUIs (SubUIsModified _ states)
		= case [(i,ChangeChild (restoreSubUIs s)) \\ (i,s) <- states] of
			[]      = NoChange
			changes = ChangeUI [] changes

	restore (LSLayoutSubUIs ui _) = ReplaceUI ui //VERY CRUDE RESTORE... TODO:We can do better than this

sequenceLayouts :: Layout Layout -> Layout
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

traceLayout :: String Layout -> Layout
traceLayout name layout = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply _ = (NoChange,LSNone)

	adjust (change,state)
		# (change`,state`) = layout.Layout.adjust (change,state)
		# msg = join "\n" 
			["Layout trace ("+++ name +++")"
			,"ORIGINAL CHANGE:"			
			,toString (toJSON change)
			,"REWRITTEN CHANGE:"
			,toString (toJSON change`)]
		= trace_n msg (change`,state`)

	restore _ = NoChange

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
listMove src dst list = insertAt dst (if (src >= length list) (abort "NEE") (list !! src)) (removeAt src list)

changeAtPath :: UIPath UIChange -> UIChange
changeAtPath [] change = change
changeAtPath [s:ss] change = ChangeUI [] [(s,ChangeChild (changeAtPath ss change))]

//Experiment to create an alternative, more declarative way of specifying layouts
/*

Function UI -> UI s.t. h o g o f
Where
f : UI -> TaskUITree
g : TaskUITree -> TaskUILayout
h : TaskUILayout -> UI

f transforms the original UI into a TaskUITree the same way we do now
g transforms the UILayout into a layout
h transforms the UILayout into a sparse new UI with some attributes (like direction), including an attribute origin that contains the original UIPath in the original tree

*/
:: TaskUITree
  = Ed  UIPath
  | Par UIPath [TaskUITree]

:: TaskUILayout a
  = UIBeside [TaskUILayout a]
  | UIAbove  [TaskUILayout a]
  | UINode   UIPath

uiOf :: TaskUITree -> TaskUILayout a
uiOf (Ed  path  ) = UINode path
uiOf (Par path _) = UINode path

besideT ts = UIBeside ts
aboveT ts = UIAbove ts

uiToRefs :: UI -> TaskUITree
uiToRefs ui
  = case ui of
      UI UIParallel _ subs = Par [] (recurse [] subs)
      UI _          _ subs = case recurse [] subs of
                               [x : _] -> x
                               _       -> Ed []
  where
  uiToRefs` :: UIPath (Int, UI) -> [TaskUITree]
  uiToRefs` path (i, UI UIParallel _ subs)
    # curPath = path ++ [i]
    = [Par curPath (recurse curPath subs)]
  uiToRefs` path (i, UI x _ _)
    # curPath = path ++ [i]
    = [Ed curPath]
  recurse curPath subs = flatten (map (uiToRefs` curPath) (zip2 [0..] subs))

taskUILayoutToUI :: (TaskUILayout a) -> UI
taskUILayoutToUI (UIBeside ls)
  = UI UIParallel ('DM'.singleton "direction" (encodeUI Horizontal)) (map taskUILayoutToUI ls)
taskUILayoutToUI (UIAbove ls)
  = UI UIParallel ('DM'.singleton "direction" (encodeUI Vertical)) (map taskUILayoutToUI ls)
taskUILayoutToUI (UINode path)
  = UI UIContainer ('DM'.singleton "origin" (toJSON path)) []

reorderUI :: (UI -> UI) -> Layout 
reorderUI reorder = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply _ = (NoChange,LSNone)

	adjust (NoChange,s)
		 = (NoChange,s)
	adjust (ReplaceUI ui,_) 
		//Determine a skeleton of the reordered ui, and replace references
		//Replace references to parts of the original ui
		# (moves,ui) = derefAll ui [] (reorder ui)
		= (ReplaceUI ui,LSNone)
	//Adjust followup changes to the moved parts
	adjust (c,s) = (c,s)
	//adjust (c,s) = (adjust` (fromMaybe 'DM'.newMap (fromJSON s)) c,s)

	restore _ = NoChange

	derefAll :: UI UIPath UI -> (Map UIPath UIPath,UI)
	derefAll origUI curNp (UI type attr items) = case 'DM'.get "include" attr of
		(Just jsonNp)
			# refNp = fromMaybe [] (fromJSON jsonNp)
			= ('DM'.singleton curNp refNp, lookup refNp origUI)
		Nothing
			# (paths,items) = unzip [derefAll origUI (curNp ++ [i]) item \\ item <- items & i <- [0..]]
			= ('DM'.unions paths,UI type attr items)

	lookup :: UIPath UI -> UI //ASSUMES SUCCESS
	lookup [] ui = ui
	lookup [p:ps] (UI _ _ items) = lookup ps (items !! p)

	adjust` :: (Map UIPath UIPath) UIChange -> UIChange //TODO
	adjust` moves change = change 
	where
		selectChanges :: [UIPath] UIChange -> [(UIPath,UIChange)]
		selectChanges _ _ = []

		remap :: (Map UIPath UIPath) [(UIPath,UIChange)] -> [(UIPath,UIChange)]
		remap moves changes =[(fromJust ('DM'.get path moves),change) \\ (path,change) <- changes]
		
		combineChanges :: [(UIPath,UIChange)] -> UIChange
		combineChanges _ = NoChange
