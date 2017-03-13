implementation module iTasks.UI.Layout

import StdTuple, StdList, StdBool, StdInt, StdOrdList, StdArray, StdMisc
import Data.Maybe, Data.Either, Text, Data.Tuple, Data.List, Data.Either, Data.Functor
import iTasks._Framework.Util, iTasks._Framework.HtmlUtil, iTasks.UI.Definition
import iTasks.API.Core.Types, iTasks.API.Core.TaskCombinators
import StdEnum
from Data.Map as DM import qualified put, get, del, newMap, toList, fromList, alter, union, keys, unions, singleton

from StdFunc import o, const, id, flip
from iTasks._Framework.TaskState import :: TIMeta(..), :: TaskTree(..), :: DeferredJSON
import StdDebug

//This type records the states of layouts applied somewhere in a ui tree
derive JSONEncode LayoutState, LayoutTree
derive JSONDecode LayoutState, LayoutTree

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
/*
moveSubUIs :: UISelection UIPath -> Layout //TODO
moveSubUIs selection dst = {Layout|apply=apply,adjust=adjust,restore=restore} 
where
	pred = inUISelection selection

	apply ui = (NoChange, LSRemoveSubUIs ui (SubUIsModified []))

	adjust (change, LSRemoveSubUIs ui (SubUIsModified moves))
		# moves = if (change=:(ReplaceUI _)) [] moves//On a replace, we reset the state 
		# startIdx = last dst
		//Remove based on the predicate
		# (change,moves,inserts) = removeAndAdjust_ [] pred False startIdx change moves
		//If there is a destination path, adjust the change for these moves
	 	# change = insertAndAdjust_ (init dst) startIdx (countMoves_ moves True) inserts change
	    = (change, LSRemoveSubUIs ui (SubUIsModified moves))

	restore _ = NoChange

removeSubUIs :: UISelection -> Layout //TODO
removeSubUIs selection = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply ui = (NoChange, LSRemoveSubUIs ui (SubUIsModified []))

	pred = inUISelection selection
	adjust (change,LSRemoveSubUIs ui (SubUIsModified moves))
		# moves = if (change=:(ReplaceUI _)) [] moves //On a replace, we reset the state
		# (change, moves,_) = removeAndAdjust_ [] pred True 0 change moves
		= (change, LSRemoveSubUIs ui (SubUIsModified moves))

	restore _ = NoChange
*/

//Situaties bij een stuk vervangen: Zit de target in deze subtree? Dan moeten de 'inserts' er aan toegevoegd worden
//Kun je het verwijderen 'los' zien?
//Anders gezegd: Kun je aan de state en de 'removeChange' zien of er nieuwe bijgekomen zijn?...

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
		# (insertChange, state) = maybe (NoChange,state) (\dst -> applyIns dst ui state) mbDst
		= (mergeUIChanges removeChange insertChange, LSRemoveSubUIs ui state)

	//Handle the removal of the selected sub-UI's
	applyRem path ui=:(UI type attr items)
		| inUISelection selection path ui
			= (RemoveChild, UIModified ())
		| otherwise	
			# (childChanges,childStates) = remove 0 0 items
			= (ChangeChild (ChangeUI [] childChanges), SubUIsModified childStates)
	where
		remove i n [] = ([],[])
		remove i n [ui:uis]
			# (cchange,state) = applyRem (path ++ [i]) ui
			# (changes,states) = remove (i + 1) (if (state =:(UIModified _)) n (n + 1)) uis
			= ([(n,cchange):changes], [(i,state):states])

	//Handle the insertion of the removed sub-UI's in the target location
	applyIns dst ui state
		| uiPathExists dst ui = (changeAtUIPath dst (ChangeUI [] [(i,InsertChild ui) \\ ui <- collectMoved ui state & i <- [0..]]), state)
		| otherwise           = (NoChange, state)

	adjust (change, LSRemoveSubUIs ui state)
		//First update all removals
		# (cchange, ui, state) = adjustRem [] change ui state
		# change = case cchange of
			(ChangeChild change) = change
			(RemoveChild)        = ReplaceUI (UI UIEmpty 'DM'.newMap []) //If the root-level UI needs to be removed, replace it with UIEmpty
			(InsertChild ui)     = ReplaceUI ui           				 //If the root-level UI is going to be restored, replace it
		//If there is a target, also update the target location
		# (change, state) = maybe (change, state) (\dst -> adjustIns dst change state) mbDst
		= (change, LSRemoveSubUIs ui state)

	//If there is no change, but a node was moved to a different path, we recheck the selection
	adjustRem path NoChange ui state=:(UIModified _)
		= case applyRem path ui of
			(RemoveChild, state) = (ChangeChild NoChange, ui,state)
			(ChangeChild change, state) = (InsertChild (applyUIChange change ui), ui, state)

	adjustRem path NoChange ui state=:(SubUIsModified _)
		| inUISelection selection path ui
			//TODO make sure we keep the information about the previously removed parts, in case they were inserted elsewhere
			= (RemoveChild, ui, UIModified ())
		| otherwise
			= (ChangeChild NoChange, ui, state)

	adjustRem path (ReplaceUI ui) _ state
		//TODO make sure we keep the information about the previously removed parts, in case they were inserted elsewhere
		//--> here
		//Apply the layout to the replacement UI
		= case applyRem path ui of
			(RemoveChild, state) = (RemoveChild, ui, state)
			(ChangeChild change,state) = (ChangeChild (ReplaceUI (applyUIChange change ui)), ui, state)

	//The UI was removed earlier
	adjustRem path change=:(ChangeUI attrChanges childChanges) ui (UIModified _)
		//Update the 'shadow' UI
		# ui = applyUIChange change ui
		//Check if the UI should still be removed after the effects of the change 
		= case applyRem path ui of
			(RemoveChild,state) = (ChangeChild NoChange, ui, state)
			//Restore the UI, but make sure that the layout still applies to the children of the UI
			(ChangeChild change, state) = (InsertChild (applyUIChange change ui), ui, state)

	adjustRem path change=:(ChangeUI attrChanges childChanges) ui (SubUIsModified mods)
		| inUISelectionAfterChange selection path ui change
			//If the change causes the selection to match
			# ui = applyUIChange change ui
			= (RemoveChild, ui, UIModified ())
		| otherwise
			//Update the attributes in the 'shadow' UI
			# (UI type attr items) = applyUIChange (ChangeUI attrChanges []) ui
			//Adjust the child changes
			# (childChanges, items, mods) = adjustRemChildChanges childChanges items mods
			= (ChangeChild (ChangeUI attrChanges childChanges), UI type attr items, SubUIsModified mods)
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
				
	adjustIns dst change state
		= (change, state)

	//Correct an index for the number of removed sibling preceding it
	adjustIndex idx mods = idx - foldr (\(i,m) n -> if (i <= idx && (m =: (UIModified _))) (n + 1) n) 0 mods

	restore (LSRemoveSubUIs ui _) = ReplaceUI ui //VERY CRUDE RESTORE..

/**
* This is the core function that tranforms UIChange instructions to effect the layout
* It uses a datastructure to track changes that have been applied in 'previous' calls to this function
*
* @param path::UIPath: The location in the (unmodified) tree where they original change was targeted at
* @param pred::(UIPath UI -> Bool): The predicate that tests if a node should be (re) moved
* @param hide::Bool: A flag that indicates if the removed node should be stored in the state
* @param targetIdx::Int: The index in the destination node where the nodes are moved to
* @param change:UIChange: The change that needs to be transformed
*/
/*
removeAndAdjust_ :: UIPath (UIPath UI -> Bool) Bool Int UIChange [(Int,LayoutTree UI)] -> (!UIChange,![(Int,LayoutTree UI)],![(Int,UIChildChange)])
//Basic NoChange case: if there is no change we don't need to transform anything
//            We do need to count how many nodes were removed to keep track of the targetIndex in other branches
removeAndAdjust_ path pred hide tidx NoChange moves //Only adjust the targetIdx by counting the moved nodes
	= (NoChange, moves, [])
//Replacement case: this part of the UI is replaced. We need to remove the nodes we previously moved and find which ones to move in the new UI
removeAndAdjust_ path pred hide tidx (ReplaceUI ui) moves //If the node is replaced, adjust the new ui and determine changes to the previously moved nodes
	//Remove all previously moved nodes
	# removals = repeatn (ltCount True moves) (tidx,RemoveChild)
	//Determine new moves in the replacement ui
	= case collectNodes_ path pred hide tidx ui of
		//If the predicate matches the root node don't change anything.
		//It is impossible to create an adjusted ReplaceUI instruction if the root node is removed
		(_,Nothing,_,_)             = (ReplaceUI ui, moves, [])
		(_,Just ui, moves, inserts) = (ReplaceUI ui, moves, removals ++ inserts)
//The change case: We need to adjust the changes to the child branches
removeAndAdjust_ path pred hide tidx (ChangeUI localChanges childChanges) moves 
	# (moves, childChanges, inserts) = adjustChildChanges tidx moves childChanges 
	= (cleanChange (ChangeUI localChanges childChanges), moves, inserts)
where
	cleanChange (ChangeUI localChanges childChanges) = case (localChanges,[c \\ c <- childChanges | not (c =:(_,ChangeChild NoChange))]) of
		([],[]) = NoChange
		(l,c)   = ChangeUI l c
	cleanChange change = change

	adjustChildChanges tidx moves [] 
		//Sort for easier debugging
		# moves = sortBy (\(i1,m1) (i2,m2) -> i1 < i2) moves
		= (moves,[],[])
	// - Insert 
	adjustChildChanges tidx moves [(idx,InsertChild ui):cs] 
		//Determine additional moves in the replacement ui
		# (_,mbUI,subMoves,subInserts) = collectNodes_ (path ++ [idx]) pred hide (adjustTargetIndex moves idx tidx) ui	
		//Adjust the change
		# change = case mbUI of 
			Nothing = [] //The top node of the inserted UI matched, record the move, but don't insert anything
			Just ui = [(adjustIndex moves idx,InsertChild ui)]
		//Adjust the moved nodes state to adjust for the 'inserted' branch
		# moves = [(if (i >= idx) (i + 1) i , m) \\ (i,m) <- moves]
		# moves = if (mbUI =:Nothing) [(idx,UIModified ui):moves] moves
		# moves = if (subMoves =:[]) moves [(idx,SubUIsModified subMoves):moves]
		//Recurse
		# (moves,cs,inserts) = adjustChildChanges tidx moves cs
		= (moves, change ++ cs, subInserts ++ inserts)
	// - Remove
	adjustChildChanges tidx moves [(idx,RemoveChild):cs] 
		//Check if the branch was moved by the layout 
		# (change,moves,subInserts) = case findMove idx moves of
			Nothing = ([(adjustIndex moves idx, RemoveChild)]
					  ,[(if (i > idx) (i - 1) i , m) \\ (i,m) <- moves]	
					  ,[])
			Just (UIModified _) 
					//The branch was moved, generate a remove instruction at the destination 
					= ([]
					  ,[(if (i > idx) (i - 1) i, m) \\ (i,m) <- moves | i <> idx]
					  ,[(adjustTargetIndex moves idx tidx,RemoveChild)])
			Just (SubUIsModified subMoves)
					//Children of the branch were moved, generate instructions for those
					= ([(adjustIndex moves idx, RemoveChild)]	
					  ,[(if (i > idx) (i - 1) i, m) \\ (i,m) <- moves | i <> idx]
					  ,repeatn (ltCount True subMoves) (adjustTargetIndex moves idx tidx, RemoveChild))
		# (moves, cs, inserts) = adjustChildChanges tidx moves cs
		= (moves, change ++ cs, subInserts ++ inserts)
	// - Move
	adjustChildChanges tidx moves [(idx,MoveChild dst):cs] 
		| ltCount False moves > 0 = abort "Cannot adjust move instructions at a level where previous layout rules have matched" 
		//Apply the rearrangement to the move information 
		# srcMove = findMove idx moves //Select moved branch
		# moves = [(if (i > idx) (i - 1) i, m) \\ (i,m) <- moves | i <> idx] //Everything moves down when we remove the branch
		# moves = [(if (i >= dst) (i + 1) i, m) \\ (i,m) <- moves] //Move everything after the destination move up to make 'space' for the move
		# moves = maybe [] (\m -> [(dst,m)]) srcMove ++ moves //Move to destination
		//Pass on the change
		# change = [(idx,MoveChild dst)]
		# subInserts = []
		# (moves, cs, inserts) = adjustChildChanges tidx moves cs
		= (moves, change ++ cs, subInserts ++ inserts)
	// - Replace
	adjustChildChanges tidx moves [(idx,ChangeChild change=:(ReplaceUI ui)):cs] 
		# (change,moves,subInserts) = case findMove idx moves of
			//Previously the child did not match
			Nothing 
				= case collectNodes_ (path ++ [idx]) pred hide (adjustTargetIndex moves idx tidx) ui of
					(_,Nothing,subMoves,subInserts) //The inserted UI matched, record the move and remove the child
						= ([(adjustIndex moves idx, RemoveChild)]
                      	  ,[(idx, UIModified ui):moves]
                      	  ,subInserts)
					(_,_,[],_) //Nothing matched, no need to record anything
						= ([(adjustIndex moves idx,ChangeChild (ReplaceUI ui))]
					  	  ,moves
                          ,[])
					(_,Just ui,subMoves,subInserts) //One or more sub nodes matched, we need to record the moves for this branch
					    = ([(adjustIndex moves idx, ChangeChild (ReplaceUI ui))]
					      ,[(idx,SubUIsModified subMoves):[(i,m) \\ (i,m) <- moves | i <> idx]]
					      ,subInserts)
			//Previously this child node matched the predicate
			Just (UIModified _)
				| pred (path ++ [idx]) ui //The replacement still matches, just replace in the target locatation
					= ([]
                      ,moves
					  ,[(adjustTargetIndex moves idx tidx, ChangeChild change)])
				| otherwise //The Moved node should no longer be moved -> change the replacement to an insert instruction
					= case collectNodes_ (path ++ [idx]) pred hide (adjustTargetIndex moves idx tidx) ui of
						(_,_,[],_) //Nothing matched, no need to record anything
							# moves = [(i,m) \\ (i,m) <- moves | i <> idx]
							= ([(adjustIndex moves idx,InsertChild ui)]
					   	  	  ,moves 
					          ,[(adjustTargetIndex moves idx tidx,RemoveChild)])
						(_,Just ui,subMoves,subInserts)
							# moves = [(i,m) \\ (i,m) <- moves | i <> idx]
						    = ([(adjustIndex moves idx,InsertChild ui)]
						      ,[(idx,SubUIsModified subMoves):moves]
						      ,[(adjustTargetIndex moves idx tidx,RemoveChild):subInserts])
			//Previously children of the child node matched the predicate
			Just (SubUIsModified subMoves)
				//Create remove instructions for the replaced nodes
				# inserts = repeatn (ltCount True subMoves) (adjustTargetIndex moves idx tidx,RemoveChild) 
				//Find out what needs to be replaced in the new ui
				= case collectNodes_ (path ++ [idx]) pred hide (adjustTargetIndex moves idx tidx) ui of
					(_,Nothing,subMoves,subInserts) //The replacement UI matched, record the move
						= ([(adjustIndex moves idx, RemoveChild)]
					      ,[(idx,UIModified ui):[(i,m) \\ (i,m) <- moves | i <> idx]]
					      ,inserts ++ subInserts)
					(_,_,[],_) //Nothing matched, no longer need to record anything
						= ([(adjustIndex moves idx, ChangeChild (ReplaceUI ui))]
                          ,[(i,m) \\ (i,m) <- moves | i <> idx]
                          ,inserts)
					(_,Just ui,subMoves,subInserts) //One or more sub nodes matched, we need to record the moves for this branch
				        = ([(adjustIndex moves idx, ChangeChild (ReplaceUI ui))]
                          ,[(idx,SubUIsModified subMoves):[(i,m) \\ (i,m) <- moves | i <> idx]]
					      ,inserts ++ subInserts)
		# (moves,cs,inserts) = adjustChildChanges tidx moves cs
		= (moves, change ++ cs, subInserts ++ inserts)
	// - Other recursive changes
	adjustChildChanges tidx moves [(idx,ChangeChild change):cs] 
		# (change,moves,subInserts) = case findMove idx moves of
			//Nothing moved yet, but the predicate might match on inserts or replace instructions in descendant nodes
			Nothing
				# (change,subMoves,subInserts) = removeAndAdjust_ (path ++ [idx]) pred hide (adjustTargetIndex moves idx tidx) change []
				# moves = case subMoves of
					[] = moves
					_  = [(idx,SubUIsModified subMoves):moves]
				= ([(adjustIndex moves idx,ChangeChild change)]
					  ,moves
					  ,[])
			//Apply the change to the hidden UI and check if the predicate still holds
			Just (UIModified ui)
				# ui = applyUIChange change ui
				| pred (path ++ [idx]) ui //The predicate still matches update the moves
					= ([],[(idx,UIModified ui):[(i,m) \\ (i,m) <- moves | i <> idx]], []) 
				| otherwise //The predicate no longer matches -> re-insert the change
					# moves = [(i,m) \\ (i,m) <- moves | i <> idx]
					= ([(adjustIndex moves idx, InsertChild ui)],moves,[]) 
			//Recursively adjust the change 
			Just (SubUIsModified subMoves)
					# (change,subMoves,subInserts) = removeAndAdjust_ (path ++ [idx]) pred hide (adjustTargetIndex moves idx tidx) change subMoves
					= ([(adjustIndex moves idx,ChangeChild change)]
                      ,[(idx,SubUIsModified subMoves):[(i,m) \\ (i,m) <- moves | i <> idx]]
					  ,subInserts)
		# (moves,cs,inserts) = adjustChildChanges tidx moves cs
		= (moves, change ++ cs, subInserts ++ inserts)

	adjustIndex moves idx = idx - foldr (\(i,m) n -> if (i <= idx && (m =: (UIModified _))) (n + 1) n) 0 moves

	adjustTargetIndex moves idx tidx = tidx + ltCount True [(i,m) \\ (i,m) <- moves | i < idx]

	findMove idx moves = listToMaybe [m \\ (i,m) <- moves | i == idx]
*/
/*
//Collect parts of a UI and record their positions
collectNodes_ :: UIPath (UIPath UI -> Bool) Bool Int UI -> (Int, Maybe UI, [(Int,LayoutTree UI)], [(Int,UIChildChange)])
collectNodes_ path pred hide idx ui=:(UI type attr items)
	| pred path ui = (idx + 1, Nothing, [], [(idx,InsertChild ui)])
	| otherwise 
		# (idx, items, moves, inserts) = collectInItems idx 0 items
		= (idx, Just (UI type attr items), moves, inserts)
where
	collectInItems idx i [] = (idx,[],[],[])
	collectInItems idx i [item:items]
		# (idx, mbItem, itemMoves, itemInserts) = collectNodes_ (path ++ [i]) pred hide idx item
		# (idx, items, moves, inserts)          = collectInItems idx (i + 1) items
		= case mbItem of 
			Nothing   //The item itself was collected
				= (idx, items, [(i,UIModified item):moves], itemInserts ++ inserts)
			Just item //Maybe modified
				| itemMoves =:[] //If there are no moves in the branch, we don't need to add it
					= (idx, [item:items], moves, itemInserts ++ inserts)
				| otherwise	
					= (idx, [item:items], [(i,SubUIsModified itemMoves):moves], itemInserts ++ inserts)

insertAndAdjust_ :: UIPath Int Int [(Int,UIChildChange)] UIChange -> UIChange
insertAndAdjust_ path=:[] startIdx numInserts insertChanges change = case change of //Add the inserts here
	NoChange
		= ChangeUI [] insertChanges
	ChangeUI localChanges childChanges
		= ChangeUI localChanges
			([(i,c) \\ (i,c) <- childChanges | i < startIdx] //Child changes before the insert index unaffected
             ++ insertChanges
             ++ [(i + numInserts,c) \\ (i,c) <- childChanges | i >= startIdx]) //Child changes after are adjusted
	ReplaceUI ui
		= ReplaceUI (insertNodes_ path insertChanges ui)
insertAndAdjust_ path=:[s:ss] startIdx numInserts insertChanges change = case change of //Find the container
	NoChange
        = (ChangeUI [] [(s,ChangeChild (insertAndAdjust_ ss startIdx numInserts insertChanges NoChange))])
	ChangeUI localChanges childChanges
        = (ChangeUI localChanges (adjustChildChanges s childChanges))
	ReplaceUI ui
        = ReplaceUI (insertNodes_ path insertChanges ui)
where
	adjustChildChanges idx [] = [(idx,ChangeChild (insertAndAdjust_ ss startIdx numInserts insertChanges NoChange))]
	adjustChildChanges idx [(i,ChangeChild change):cs]
		| i == idx  = [(i,ChangeChild (insertAndAdjust_ ss startIdx numInserts insertChanges change)):cs] //Adjust an existing branch
		| i < idx 	= [(i,ChangeChild change):adjustChildChanges idx cs] //Scan forward
					= [(idx,ChangeChild (insertAndAdjust_ ss startIdx numInserts insertChanges NoChange)),(i,ChangeChild change):cs] //Add a branch
	adjustChildChanges idx [c:cs] = [c:adjustChildChanges idx cs] //TODO: Figure out if we can properly handle structure changes on the path
	
insertNodes_ :: UIPath [(Int,UIChildChange)] UI -> UI
insertNodes_ [] changes (UI type attr items) = UI type attr (foldl apply items changes)
where
	apply items (i,RemoveChild) = removeAt i items
	apply items (i,InsertChild ui) = insertAt i ui items
	apply items change = items

insertNodes_ [s:ss] changes (UI type attr items)
	| s < length items  = UI type attr (updateAt s (insertNodes_ ss changes (items !! s)) items)
	| otherwise 		= UI type attr items
*/

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
			# itemStates = [(i,s) \\ s <- itemStates & i <- [0..] | not s =: (SubUIsModified [])]
			= (ChangeUI [] itemChanges,SubUIsModified itemStates)

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
			= (change, ui, SubUIsModified [])
	//When we get a change, we need to check which sub-uis were affected
	adjust` path change=:(ChangeUI attrChanges childChanges) ui state=:(SubUIsModified states)
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
			= (ChangeUI attrChanges childChanges, UI type attr items, SubUIsModified states)
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
	restoreSubUIs (SubUIsModified states)
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
ltGet :: Int [(Int,LayoutTree a)] -> (LayoutTree a)
ltGet index [] = SubUIsModified []
ltGet index [(i,tree):ts] = if (i == index) tree (ltGet index ts)

ltPut :: Int (LayoutTree a) [(Int,LayoutTree a)] -> [(Int,LayoutTree a)]
// It is pointless to store empty trees in a sparse representation, just make sure we delete the previously stored value
ltPut index (SubUIsModified []) list = [x \\ x=:(i,_) <- list | i <> index] 
ltPut index item list
	= [x \\ x=:(i,_) <- list | i < index] ++ [(index,item)] ++ [x \\ x=:(i,_) <- list | i > index]

ltInsert :: Int (LayoutTree a) [(Int,LayoutTree a)] -> [(Int,LayoutTree a)]
ltInsert index (SubUIsModified []) list = [(if (i >= index) (i + 1) i, x) \\ (i,x) <- list]
ltInsert index item list 
	= [ x \\ x=:(i,_) <- list | i < index] ++ [(index,item)] ++ [(i + 1,x) \\ (i,x) <- list | i >= index]

ltRemove :: Int [(Int,LayoutTree a)] -> [(Int,LayoutTree a)]
ltRemove index list = [(if (i > index) (i - 1) i, x) \\ (i,x) <- list | i <> index]

ltMove :: Int Int [(Int,LayoutTree a)] -> [(Int,LayoutTree a)]
ltMove src dst list = ltInsert dst (ltGet src list) (ltRemove src list)

ltCount :: Bool [(Int,LayoutTree a)] -> Int
ltCount recursive list = foldr count 0 (map snd list)
where
	count (UIModified _) n = n + 1
	count (SubUIsModified mods) n = if recursive (n + ltCount recursive mods) n

ltList :: (LayoutTree a) -> [a] // Recursive listing of all elements
ltList tree = list [] [tree]
where
	list acc [] = reverse acc
	list acc [UIModified x:xs] = list [x:acc] xs
	list acc [SubUIsModified x:xs] = list (list acc (map snd x)) xs


listMove :: Int Int [a] -> [a]
listMove src dst list = insertAt dst (list !! src) (removeAt src list)

//Check if the UI node that the path targets exists in the UI
uiPathExists :: UIPath UI -> Bool
uiPathExists [] _ = True
uiPathExists [s:ss] (UI _ _ items) = (s < length items) && uiPathExists ss (items !! s)

changeAtUIPath :: UIPath UIChange -> UIChange
changeAtUIPath [] change = change
changeAtUIPath [s:ss] change = ChangeUI [] [(s,ChangeChild (changeAtUIPath ss change))]

collectMoved :: UI (LayoutTree ()) -> [UI]
collectMoved ui tree = collect [] [(ui,tree)]
where
	collect acc [] = reverse acc
	collect acc [(ui,UIModified ()):xs] = collect [ui:acc] xs
	collect acc [(UI _ _ items,SubUIsModified x):xs] = collect (collect acc [(items !! i,s) \\ (i,s) <- x]) xs

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
