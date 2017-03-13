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
inUISelection (SelectRoot) [] _ = True
inUISelection (SelectRoot) _ _ = False
inUISelection (SelectChildren) [_] _ = True
inUISelection (SelectChildren) _ _ = False
inUISelection (SelectDescendents) [_:_] _ = True
inUISelection (SelectDescendents) _ _ = False
inUISelection (SelectByType t) _ (UI type _ _) = t === type
inUISelection (SelectByHasAttribute k) _ (UI _ attr _) = isJust ('DM'.get k attr)
inUISelection (SelectByAttribute k v) _ (UI _ attr _) = maybe False ((==) v) ('DM'.get k attr)
inUISelection (SelectByNumChildren num) _ (UI _ _  items) = length items == num
inUISelection (SelectByHasChildrenOfType t) _ (UI _ _  items) = any (\(UI type _ _) -> type === t) items
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
	apply (UI _ attr items) = (ReplaceUI (UI type attr items), LSNone) //Crude replacement (no instruction possible)

	adjust (change,s) = (change,s)

	restore _ = NoChange //Cannot be restored...

setUIAttributes :: UIAttributes -> Layout
setUIAttributes extraAttr = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply ui = (ChangeUI [SetAttribute k v \\ (k,v) <- 'DM'.toList extraAttr] [],LSNone)

	adjust (ChangeUI attrChanges itemChanges,s)
		//Filter out updates for the attributes that this layout has overwritten setting here
		# attrChanges = filter (\(SetAttribute k _) -> not (isMember k ('DM'.keys extraAttr))) attrChanges
		= (ChangeUI attrChanges itemChanges,s)
	adjust (change,s) = (change,s)

	restore _ = NoChange 

delUIAttributes :: [String] -> Layout
delUIAttributes delAttr = {Layout|apply=apply,adjust=adjust,restore=restore} //There is no delete instruction, so deleting means setting the value to null 
where
	apply ui = (ChangeUI [SetAttribute k JSONNull \\ k <- delAttr] [],LSNone)

	adjust (ChangeUI attrChanges itemChanges,s)
		# attrChanges = filter (\(SetAttribute k _) -> not (isMember k delAttr)) attrChanges
		= (ChangeUI attrChanges itemChanges,s)
	adjust (change,s) = (change,s)
	
	restore _ = NoChange

modifyUIAttributes :: String (JSONNode -> UIAttributes) -> Layout //TODO
modifyUIAttributes name modifier = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply (UI type attr items)
		= (maybe NoChange (\val -> ChangeUI [SetAttribute k v \\ (k,v) <- 'DM'.toList (modifier val)] []) ('DM'.get name attr),LSNone)

	adjust (ReplaceUI ui,_)
		# (change,s) = apply ui
		# ui = applyUIChange change ui
		= (ReplaceUI ui,s)

	adjust (ChangeUI attrChanges childChanges,s)
		# attrChanges = flatten [if (key == name) [SetAttribute k v \\ (k,v) <- 'DM'.toList (modifier value)] [c] \\c=:(SetAttribute key value) <- attrChanges]
		= (ChangeUI attrChanges childChanges,s)
	adjust (c,s) = (c,s)

	restore _ = NoChange

copySubUIAttributes :: UIAttributeSelection UIPath UIPath -> Layout //TODO
copySubUIAttributes selection src dst = {Layout|apply=apply,adjust=adjust,restore=restore} 
where
	apply _ = (NoChange,LSNone)

	//TODO: Also handle attribute updates in the src location, and partial replacements along the path
	adjust (ReplaceUI ui,s) = case selectAttr src ui of 
		Just attr = (ReplaceUI (addAttr attr dst ui),s)
		Nothing   = (ReplaceUI ui,s)
	adjust (change,s) = (change,s)

	selectAttr [] (UI type attr items) = Just attr
	selectAttr [s:ss] (UI type attr items) 
		| s < length items  = selectAttr ss (items !! s)
							= Nothing

	addAttr extra [] (UI type attr items)
		= UI type (foldl (\m (k,v) -> 'DM'.put k v m) attr [(k,v) \\ (k,v) <- 'DM'.toList extra | condition selection k]) items
	addAttr extra [s:ss] (UI type attr items) 
		| s < length items = UI type attr (updateAt s (addAttr extra ss (items !! s)) items) 
						   = UI type attr items

	condition (SelectAll) _ = True
	condition (SelectKeys keys) k = isMember k keys

	restore _ = NoChange

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

insertSubUI :: UIPath UI-> Layout
insertSubUI [] _ = idLayout 
insertSubUI path ui = layoutSubUIs (SelectByPath (init path)) (insertChildUI (last path) ui)

insertChildUI :: Int UI -> Layout
insertChildUI idx insert = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply _ = (ChangeUI [] [(idx,InsertChild insert)],LSNone)

	adjust change=:(NoChange,_) = change
	//Simply (re-) insert the ui
	adjust change=:(ReplaceUI (UI type attr items), s) = (ReplaceUI (UI type attr (insertAt idx insert items)), s)
	//Increment the id's of child changes to adjust for the inserted static child
	adjust change=:(ChangeUI attrChanges childChanges, s) = (ChangeUI attrChanges [(if (i >= idx) (i + 1) i,c) \\ (i,c) <- childChanges], s)

	restore _ = ChangeUI [] [(idx,RemoveChild)]

moveSubUIs :: UISelection UIPath -> Layout 
moveSubUIs selection path = moveSubUIs` selection (Just path) 

removeSubUIs :: UISelection -> Layout
removeSubUIs selection = moveSubUIs` selection Nothing

moveSubUIs` :: UISelection (Maybe UIPath) -> Layout
moveSubUIs` selection mbDst = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply ui
		//First remove and mark 
		# (cchange, state) = applyRem [] ui
		# removeChange = case cchange of
			(ChangeChild change) = change
			(RemoveChild)        = ReplaceUI (UI UIEmpty 'DM'.newMap []) //If the root-level UI needs to be removed, replace it with UIEmpty
		//If there is a destination path, insert all removed ui's at that location
		# (destinationChange, state) = maybe (NoChange,state) (\dst -> extractDestinationChange dst ui state) mbDst
		= (mergeUIChanges removeChange destinationChange, LSRemoveSubUIs ui state)

	//Handle the removal of the selected sub-UI's
	applyRem path ui=:(UI type attr items)
		| inUISelection selection path ui
			= (RemoveChild, UIModified (LRRemoved 0))
		| otherwise	
			# (childChanges,childStates) = remove 0 0 items
			= (ChangeChild (ChangeUI [] childChanges), SubUIsModified 0 childStates)
	where
		remove i n [] = ([],[])
		remove i n [ui:uis]
			# (cchange,state) = applyRem (path ++ [i]) ui
			# (changes,states) = remove (i + 1) (if (state =:(UIModified _)) n (n + 1)) uis
			= ([(n,cchange):changes], [(i,state):states])

	adjust (change, LSRemoveSubUIs ui state)
		//First update all removals
		# (cchange, ui, state) = adjustRem [] change ui state
		# removeChange = case cchange of
			(ChangeChild change) = change
			(RemoveChild)        = ReplaceUI (UI UIEmpty 'DM'.newMap []) //If the root-level UI needs to be removed, replace it with UIEmpty
			(InsertChild ui)     = ReplaceUI ui           				 //If the root-level UI is going to be restored, replace it
		//If there is a target, also update the target location
		# (destinationChange, state) = maybe (NoChange,state) (\dst -> extractDestinationChange dst ui state) mbDst
		= (mergeUIChanges removeChange destinationChange, LSRemoveSubUIs ui state)

	//If there is no change, but a node was moved to a different path, we recheck the selection
	adjustRem path NoChange ui state=:(UIModified _)
		= case applyRem path ui of
			(RemoveChild, state) = (ChangeChild NoChange, ui,state)
			(ChangeChild change, state) = (InsertChild (applyUIChange change ui), ui, state)

	adjustRem path NoChange ui state=:(SubUIsModified _ mods)
		| inUISelection selection path ui
			= (RemoveChild, ui, UIModified (LRRemoved (ltCount True mods)))
		| otherwise
			= (ChangeChild NoChange, ui, state)

	adjustRem path (ReplaceUI ui) _ state
		//First determine how many UI's were removed in the previous layout
		# numCurRemoved = case state of
			(UIModified _) = 1
			(SubUIsModified _ mods) = ltCount True mods
		//Apply the layout to the replacement UI
		= case applyRem path ui of
			(RemoveChild, state)
				= (RemoveChild, ui, state)
			(ChangeChild change,SubUIsModified _ mods )
				= (ChangeChild (ReplaceUI (applyUIChange change ui)), ui, SubUIsModified numCurRemoved mods)

	//The UI was removed earlier
	adjustRem path change=:(ChangeUI attrChanges childChanges) ui (UIModified _)
		//Update the 'shadow' UI
		# ui = applyUIChange change ui
		//Check if the UI should still be removed after the effects of the change 
		= case applyRem path ui of
			(RemoveChild,state) = (ChangeChild NoChange, ui, state)
			//Restore the UI, but make sure that the layout still applies to the children of the UI
			(ChangeChild change, state) = (InsertChild (applyUIChange change ui), ui, state)

	adjustRem path change=:(ChangeUI attrChanges childChanges) ui state=:(SubUIsModified _ mods)
		| inUISelectionAfterChange selection path ui change
			//If the change causes the selection to match
			# ui = applyUIChange change ui
			= (RemoveChild, ui, UIModified (LRRemoved (ltCount True mods)))
		| otherwise
			//Update the attributes in the 'shadow' UI
			# (UI type attr items) = applyUIChange (ChangeUI attrChanges []) ui
			//Adjust the child changes
			# (childChanges, items, mods) = adjustRemChildChanges childChanges items mods
			= (ChangeChild (ChangeUI attrChanges childChanges), UI type attr items, SubUIsModified 0 mods)
	where
		adjustRemChildChanges [] items states = ([], items, states)
		adjustRemChildChanges [(i,c):cs] items states
			# (c, items, states)  = adjustRemChildChange i c items states
			# (cs, items, states) = adjustRemChildChanges cs items states
			= (c ++ cs, items, states)

		adjustRemChildChange i (ChangeChild change) items states
			//Recursively adjust the change
			# (cchange, item, state) = adjustRem (path ++ [i]) change (items !! i) (ltGet i states)
			= ([(adjustIndex i states, cchange)], updateAt i item items, ltPut i state states)

		adjustRemChildChange i (InsertChild ui) items states
			# (rchange,state) = applyRem (path ++ [i]) ui
			//If the child is immediately matched, don't insert insert it upstream
			# cchange = case rchange of
				RemoveChild = []
				ChangeChild change = [(adjustIndex i states, InsertChild (applyUIChange change ui))]
			//The insertion potentially affects all siblings after the insertion point, we need to check them
			# (schanges, items, states) = adjustRemSiblings path (\x -> x > i) (insertAt i ui items) (ltInsert i state states)
			= (cchange ++ schanges, items, states)

		adjustRemChildChange i RemoveChild items states
			//If the child was already removed by this layout it no longer need to be removed from the UI
			# cchange = if ((ltGet i states) =:(UIModified _)) [] [(adjustIndex i states, RemoveChild)]
			# (schanges, items, states) = adjustRemSiblings path (\x -> x >= i) (removeAt i items) (ltRemove i states)
			= (cchange ++ schanges, items, states)

		adjustRemChildChange i (MoveChild d) items states
			# cchange = (adjustIndex i states,MoveChild (adjustIndex d states))
			# (schanges, items, states) = adjustRemSiblings path (const True) (listMove i d items) (ltMove i d states) //TODO we don't need to check *all* siblings
			= ([cchange:schanges], items, states)

		//For the selected items call adjustRem with NoChange to check if they need to be removed or restored
		adjustRemSiblings path whichSiblings items states = adjust 0 items states
		where
			adjust i [] states = ([],[],states)
			adjust i [item:items] states
				| whichSiblings i
					//Check the ui
					# (cchange, item, state) = adjustRem (path ++ [i]) change item (ltGet i states)
					# change = [(adjustIndex i states,cchange)]
					//Check the remaining items
					# (changes, items, states) = adjust (i + 1) items (ltPut i state states)
					= (change ++ changes, items, states)
				| otherwise
					# (changes, items, states) = adjust (i + 1) items states
					= (changes, [item:items], states)

	//Correct an index for the number of removed sibling preceding it
	adjustIndex idx mods = idx - foldr (\(i,m) n -> if (i <= idx && (m =: (UIModified _))) (n + 1) n) 0 mods

	//Handle the insertion of the removed sub-UI's in the target location
	extractDestinationChange dst ui state
		| uiPathExists dst ui
			//Extract the child changes to the destionation from the layout state
			# (_,changes,state) = collect 0 state ui
			= (changeAtUIPath dst (ChangeUI [] changes), state)
		| otherwise 
			//Clear all state about moves (the destination apparently no longer exists
			= (NoChange, clear state)
	where
		collect n (UIModified (LRRemoved numBefore)) ui //A new removal
			= (n + 1, repeatn numBefore (n,RemoveChild) ++ [(n,InsertChild ui)], UIModified (LRMoved NoChange))
		collect n (UIModified (LRMoved NoChange)) ui   //An old removal, no need to change anything..
			= (n + 1, [], UIModified (LRMoved NoChange))
		collect n (UIModified (LRMoved change)) ui     //An old removal, that was changed in the source location
			= (n + 1, [(n,ChangeChild change)], UIModified (LRMoved NoChange))
		collect n (SubUIsModified numBefore mods) (UI _ _ items) //Recursive case
			# (n`, changes, mods) = collectInChildren n [(i,m,items !! i) \\ (i,m) <- mods]
			= (n`, repeatn numBefore (n,RemoveChild) ++ changes, SubUIsModified 0 mods)

		collectInChildren n [] = (n,[],[])
		collectInChildren n [(i,m,ui):ms] 
			# (n,cchanges,m)  = collect n m ui
			# (n,rchanges,ms) = collectInChildren n ms
			= (n, cchanges ++ rchanges, [(i,m):ms])

		clear (UIModified _) = UIModified (LRRemoved 0)
		clear (SubUIsModified _ mods) = SubUIsModified 0 (map (appSnd clear) mods)

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

	//No change, is still no change
	adjust` path NoChange ui state
		= (NoChange,ui,state)
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
			# change = mergeUIChanges (layout.Layout.restore state) change
			= (change, ui, SubUIsModified () [])
	//When we get a change, we need to check which sub-uis were affected
	adjust` path change=:(ChangeUI attrChanges childChanges) ui state=:(SubUIsModified _ states)
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
			//Update the attributes of the 'shadow' ui
			# (UI type attr items) = applyUIChange (ChangeUI attrChanges []) ui
			# (childChanges, items, states) = adjustChildChanges childChanges items states
			= (ChangeUI attrChanges childChanges, UI type attr items, SubUIsModified () states)
	where
		adjustChildChanges [] items states = ([], items, states)
		adjustChildChanges [(i,c):cs] items states
			# (c, items, states)  = adjustChildChange i c items states
			# (cs, items, states) = adjustChildChanges cs items states
			= ([(i,c):cs], items, states)

		//FIXME: The rearrangement of children can cause their siblings to either match, or not match a path
		//       We need to recheck all affected siblings after an InsertChild, RemoveChild or MoveChild
		adjustChildChange i (ChangeChild change) items states
			//Recursively adjust the change
			# (change, item, state) = adjust` (path ++ [i]) change (items !! i) (ltGet i states)
			= (ChangeChild change, updateAt i item items, ltPut i state states)
		adjustChildChange i (InsertChild ui) items states
			//(potentially) apply the layout to the inserted item
			# (change,state) = apply` (path ++ [i]) ui
			= (InsertChild (applyUIChange change ui), insertAt i ui items, ltInsert i state states)
		adjustChildChange i RemoveChild items states
			= (RemoveChild, removeAt i items, ltRemove i states)
		adjustChildChange i (MoveChild d) items states
			= (MoveChild d, listMove i d items, ltMove i d states)

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

ltCount :: Bool [(Int,LayoutTree a b)] -> Int
ltCount recursive list = foldr count 0 (map snd list)
where
	count (UIModified _) n = n + 1
	count (SubUIsModified _ mods) n = if recursive (n + ltCount recursive mods) n

listMove :: Int Int [a] -> [a]
listMove src dst list = insertAt dst (list !! src) (removeAt src list)

//Check if the UI node that the path targets exists in the UI
uiPathExists :: UIPath UI -> Bool
uiPathExists [] _ = True
uiPathExists [s:ss] (UI _ _ items) = (s < length items) && uiPathExists ss (items !! s)

changeAtUIPath :: UIPath UIChange -> UIChange
changeAtUIPath [] change = change
changeAtUIPath [s:ss] change = ChangeUI [] [(s,ChangeChild (changeAtUIPath ss change))]

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
