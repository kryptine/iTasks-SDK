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

//All different types of state that are used by the various core layouts
//This type records where parts were removed from a ui tree
:: NodeMoves :== [(Int,NodeMove)] 
:: NodeMove = BranchMoved                  //This branch was moved to another location (or removed)
			| BranchHidden UI              //This branch was hidden and can be restored
            | ChildBranchesMoved NodeMoves

//This type records the states of layouts applied somewhere in a ui tree
/*
:: LayoutState
	= LSNone                                      //No state is tracked for a layout
	| LSJson JSONNode                             //Temporary constructor for migration
	| LSSequence LayoutState LayoutState          //Combined state of two sequenced layouts
	| LSLayoutSubUIs UI (LayoutTree LayoutState)  //States of layouts applied to sub-ui's 

:: LayoutTree a
	= UIModified a
	| SubUIsModified [(Int,LayoutTree a)]
*/
			
derive JSONEncode NodeMove, NodeLayoutState, LayoutState, LayoutTree
derive JSONDecode NodeMove, NodeLayoutState, LayoutState, LayoutTree

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
				# s = fromMaybe (LSJson JSONNull) (fromJSON json)
				# (change,s) = l.Layout.adjust (change,s)
				= (ValueResult value info change (TCLayout (toJSON s) tt), iworld)
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

setUIType :: UINodeType -> Layout
setUIType type = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply (UI _ attr items) = (ReplaceUI (UI type attr items), LSNone) //Crude replacement (no instruction possible

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

modifyUIAttributes :: String (JSONNode -> UIAttributes) -> Layout
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

copySubUIAttributes :: UIAttributeSelection UIPath UIPath -> Layout
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
	apply _ = (NoChange,LSNone)

	adjust (ReplaceUI def,_) = (ReplaceUI (uic type [def]),LSNone)
	adjust (NoChange,def) = (NoChange,def)
	adjust (change,s) = (ChangeUI [] [(0,ChangeChild change)],s)

	restore _ = NoChange

unwrapUI :: Layout
unwrapUI = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply _ = (NoChange,LSNone)

	adjust (ReplaceUI def,_) = case def of
		(UI _ _ [child:_])  = (ReplaceUI child,LSJson (JSONBool False))
		_ 					= (ReplaceUI (ui UIEmpty),LSJson (JSONBool True)) //If there is no inner component, remember we replaced it with empty...

	adjust (ChangeUI _ childChanges,s=:(LSJson (JSONBool False))) = case [change \\ (0,ChangeChild change) <- childChanges] of
		[change] = (change,s) //TODO: Check if there are cases with multiple changes to child 0
		[change:x] = trace_n "Warning: unwrapUI: edge case" (NoChange,s) //TODO: Check if there are cases with multiple changes to child 0
		_        = (NoChange,s)

	adjust (change,s) = (NoChange,s) 

	restore _ = NoChange

flattenUI :: Layout
flattenUI = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply _ = (NoChange,LSNone)

	adjust (ReplaceUI def,_)
		# (def,spine) = flattenWithSpine def
		= (ReplaceUI def,LSNone)
	adjust (change,s) = (change,s) //TODO

	flattenWithSpine  ui=:(UI type attr items) 
		/*
		# (items,spines) = unzip (map flattenWithSpine items)
		# items = flatten [[UI type attr []:children] \\ UI type attr children <- items]
*/
		= (UI type attr items,JSONNull)

	restore _ = NoChange

insertSubUI :: UIPath UI-> Layout
insertSubUI [] def = {Layout|apply=const (NoChange,LSNone),adjust=id,restore=const NoChange}
insertSubUI path def = layoutSubUIs (SelectByPath (init path)) {Layout|apply=apply,adjust=(insertSub (last path) def), restore=restore}
where
	apply _ = (NoChange,LSNone)

	insertSub idx def (ReplaceUI (UI type attr items),s) = (ReplaceUI (UI type attr (insertAt idx def items)),s)
	insertSub idx _ (ChangeUI attrChanges childChanges,s) = (ChangeUI attrChanges (insert idx childChanges),s)
	where
		insert idx [] = []
		insert idx [c:cs]
			| fst c < idx  = [c:insert idx cs]
                       	   = [(idx,ChangeChild (ChangeUI [] [])): [(n+1,x) \\(n,x) <-[c:cs]]]

	insertSub _ _ (change,s) = (change,s)

	restore _ = NoChange

moveSubUIs :: UISelection UIPath -> Layout
moveSubUIs selection dst = {Layout|apply=apply,adjust=adjust,restore=restore} 
where
	pred = inUISelection selection

	apply _ = (NoChange,LSJson JSONNull)

	adjust (change,LSJson s)
		# moves = if (change=:(ReplaceUI _)) [] (fromMaybe [] (fromJSON s)) //On a replace, we reset the state
		# startIdx = last dst
		//Remove based on the predicate
		# (change,moves,inserts) = removeAndAdjust_ [] pred False startIdx change moves
		//If there is a destination path, adjust the change for these moves
	 	# change = insertAndAdjust_ (init dst) startIdx (countMoves_ moves True) inserts change
	    = (change, LSJson (toJSON moves))

	restore _ = NoChange

removeSubUIs :: UISelection -> Layout 
removeSubUIs selection = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply _ = (NoChange,LSJson JSONNull)

	pred = inUISelection selection
	adjust (change,LSJson s)
		# moves = if (change=:(ReplaceUI _)) [] (fromMaybe [] (fromJSON s)) //On a replace, we reset the state
		# (change,moves,_) = removeAndAdjust_ [] pred True 0 change moves
		= (change, LSJson (toJSON moves))

	restore _ = NoChange
/**
* This is the core function that tranforms UIChange instructions to effect the layout
* It uses a datastructure (NodeMoves) to track changes that have been applied in 'previous' calls to this function
*
* @param path::UIPath: The location in the (unmodified) tree where they original change was targeted at
* @param pred::(UIPath UI -> Bool): The predicate that tests if a node should be (re) moved
* @param hide::Bool: A flag that indicates if the removed node should be stored in the state
* @param targetIdx::Int: The index in the destination node where the nodes are moved to
* @param change:UIChange: The change that needs to be transformed
*/
removeAndAdjust_ :: UIPath (UIPath UI -> Bool) Bool Int UIChange NodeMoves -> (!UIChange,!NodeMoves,![(Int,UIChildChange)])
//Basic NoChange case: if there is no change we don't need to transform anything
//            We do need to count how many nodes were removed to keep track of the targetIndex in other branches
removeAndAdjust_ path pred hide tidx NoChange moves //Only adjust the targetIdx by counting the moved nodes
	= (NoChange, moves, [])
//Replacement case: this part of the UI is replaced. We need to remove the nodes we previously moved and find which ones to move in the new UI
removeAndAdjust_ path pred hide tidx (ReplaceUI ui) moves //If the node is replaced, adjust the new ui and determine changes to the previously moved nodes
	//Remove all previously moved nodes
	# removals = repeatn (countMoves_ moves True) (tidx,RemoveChild)
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
		# moves = if (mbUI =:Nothing) [(idx,if hide (BranchHidden ui) BranchMoved):moves] moves
		# moves = if (subMoves =:[]) moves [(idx,ChildBranchesMoved subMoves):moves]
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
			Just BranchMoved
					//The branch was moved, generate a remove instruction at the destination 
					= ([]
					  ,[(if (i > idx) (i - 1) i, m) \\ (i,m) <- moves | i <> idx]
					  ,[(adjustTargetIndex moves idx tidx,RemoveChild)])
			Just (BranchHidden _) //TODO: This exactly the same case as BranchMoved
					= ([]
					  ,[(if (i > idx) (i - 1) i, m) \\ (i,m) <- moves | i <> idx]
					  ,[(adjustTargetIndex moves idx tidx,RemoveChild)])
			Just (ChildBranchesMoved subMoves)
					//Children of the branch were moved, generate instructions for those
					= ([(adjustIndex moves idx, RemoveChild)]	
					  ,[(if (i > idx) (i - 1) i, m) \\ (i,m) <- moves | i <> idx]
					  ,repeatn (countMoves_ subMoves True) (adjustTargetIndex moves idx tidx, RemoveChild))
		# (moves, cs, inserts) = adjustChildChanges tidx moves cs
		= (moves, change ++ cs, subInserts ++ inserts)
	// - Move
	adjustChildChanges tidx moves [(idx,MoveChild dst):cs] 
		| countMoves_ moves False > 0 = abort "Cannot adjust move instructions at a level where previous layout rules have matched" 
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
                      	  ,[(idx,if hide (BranchHidden ui) BranchMoved):moves]
                      	  ,subInserts)
					(_,_,[],_) //Nothing matched, no need to record anything
						= ([(adjustIndex moves idx,ChangeChild (ReplaceUI ui))]
					  	  ,moves
                          ,[])
					(_,Just ui,subMoves,subInserts) //One or more sub nodes matched, we need to record the moves for this branch
					    = ([(adjustIndex moves idx, ChangeChild (ReplaceUI ui))]
					      ,[(idx,ChildBranchesMoved subMoves):[(i,m) \\ (i,m) <- moves | i <> idx]]
					      ,subInserts)
			//Previously this child node matched the predicate
			Just BranchMoved 
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
						      ,[(idx,ChildBranchesMoved subMoves):moves]
						      ,[(adjustTargetIndex moves idx tidx,RemoveChild):subInserts])
			//Previously this child node matched the predicate and was hidden
			Just (BranchHidden _) //TODO: This case is exactly the same as the (Just BranchMoved case)
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
						      ,[(idx,ChildBranchesMoved subMoves):moves]
						      ,[(adjustTargetIndex moves idx tidx,RemoveChild):subInserts])			
			//Previously children of the child node matched the predicate
			Just (ChildBranchesMoved subMoves)
				//Create remove instructions for the replaced nodes
				# inserts = repeatn (countMoves_ subMoves True) (adjustTargetIndex moves idx tidx,RemoveChild) 
				//Find out what needs to be replaced in the new ui
				= case collectNodes_ (path ++ [idx]) pred hide (adjustTargetIndex moves idx tidx) ui of
					(_,Nothing,subMoves,subInserts) //The replacement UI matched, record the move
						= ([(adjustIndex moves idx, RemoveChild)]
					      ,[(idx,BranchMoved):[(i,m) \\ (i,m) <- moves | i <> idx]]
					      ,inserts ++ subInserts)
					(_,_,[],_) //Nothing matched, no longer need to record anything
						= ([(adjustIndex moves idx, ChangeChild (ReplaceUI ui))]
                          ,[(i,m) \\ (i,m) <- moves | i <> idx]
                          ,inserts)
					(_,Just ui,subMoves,subInserts) //One or more sub nodes matched, we need to record the moves for this branch
				        = ([(adjustIndex moves idx, ChangeChild (ReplaceUI ui))]
                          ,[(idx,ChildBranchesMoved subMoves):[(i,m) \\ (i,m) <- moves | i <> idx]]
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
					_  = [(idx,ChildBranchesMoved subMoves):moves]
				= ([(adjustIndex moves idx,ChangeChild change)]
					  ,moves
					  ,[])
			//Redirect the change
			Just BranchMoved
					= ([]
					  ,moves
					  ,[(adjustTargetIndex moves idx tidx,ChangeChild change)]) 
			//Apply the change to the hidden UI and check if the predicate still holds
			Just (BranchHidden ui)
				# ui = applyUIChange change ui
				| pred (path ++ [idx]) ui //The predicate still matches update the moves
					= ([],[(idx,BranchHidden ui):[(i,m) \\ (i,m) <- moves | i <> idx]], []) 
				| otherwise //The predicate no longer matches -> re-insert the change
					# moves = [(i,m) \\ (i,m) <- moves | i <> idx]
					= ([(adjustIndex moves idx, InsertChild ui)],moves,[]) 
			//Recursively adjust the change 
			Just (ChildBranchesMoved subMoves)
					# (change,subMoves,subInserts) = removeAndAdjust_ (path ++ [idx]) pred hide (adjustTargetIndex moves idx tidx) change subMoves
					= ([(adjustIndex moves idx,ChangeChild change)]
                      ,[(idx,ChildBranchesMoved subMoves):[(i,m) \\ (i,m) <- moves | i <> idx]]
					  ,subInserts)
		# (moves,cs,inserts) = adjustChildChanges tidx moves cs
		= (moves, change ++ cs, subInserts ++ inserts)

	adjustIndex moves idx = idx - foldr (\(i,m) n -> if (i <= idx && (m =: BranchMoved || m =: (BranchHidden _))) (n + 1) n) 0 moves

	adjustTargetIndex moves idx tidx = tidx + countMoves_ [(i,m) \\ (i,m) <- moves | i < idx] True

	findMove idx moves = listToMaybe [m \\ (i,m) <- moves | i == idx]

//Collect parts of a UI and record their positions
collectNodes_ :: UIPath (UIPath UI -> Bool) Bool Int UI -> (Int, Maybe UI, NodeMoves, [(Int,UIChildChange)])
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
				= (idx, items, [(i,if hide (BranchHidden item) BranchMoved):moves], itemInserts ++ inserts)
			Just item //Maybe modified
				| itemMoves =:[] //If there are no moves in the branch, we don't need to add it
					= (idx, [item:items], moves, itemInserts ++ inserts)
				| otherwise	
					= (idx, [item:items], [(i,ChildBranchesMoved itemMoves):moves], itemInserts ++ inserts)

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
	
countMoves_ :: NodeMoves Bool -> Int
countMoves_  moves recursive = foldr count 0 (map snd moves)
where
	count BranchMoved n = n + 1
	count (BranchHidden _) n = n + 1
	count (ChildBranchesMoved moves) n = if recursive (n + countMoves_ moves recursive) n

insertNodes_ :: UIPath [(Int,UIChildChange)] UI -> UI
insertNodes_ [] changes (UI type attr items) = UI type attr (foldl apply items changes)
where
	apply items (i,RemoveChild) = removeAt i items
	apply items (i,InsertChild ui) = insertAt i ui items
	apply items change = items

insertNodes_ [s:ss] changes (UI type attr items)
	| s < length items  = UI type attr (updateAt s (insertNodes_ ss changes (items !! s)) items)
	| otherwise 		= UI type attr items

layoutSubUIs :: UISelection Layout -> Layout
layoutSubUIs selection layout = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	pred = inUISelection selection

	//Find all places that match the selection
	//Keep track of the original UI in the state to enable dynamic matches
	apply ui
		# (change, state) = apply` [] ui
		= (change, LSLayoutSubUIs ui state)
	where
		apply` path ui=:(UI type attr items)
			| inUISelection selection path ui
				# (change,state) = layout.Layout.apply ui
				= (change,UIModified state)
			| otherwise
				# (itemChanges,itemStates) = unzip
					[apply` (path ++ [i]) ui \\ ui <- items & i <- [0..]]
				//Cleanup item changes (only keep those that actually change something
				# itemChanges =	[(i,ChangeChild c) \\ c <- itemChanges & i <- [0..] | not (c =: NoChange || c =: (ChangeUI [] []))]
				//Also cleanup item states
				# itemStates = [(i,s) \\ s <- itemStates & i <- [0..] | not s =: (SubUIsModified [])]
				= (ChangeUI [] itemChanges,SubUIsModified itemStates)

	adjust (ReplaceUI ui, LSLayoutSubUIs _ s)
		# (change, state) = apply ui
		= (ReplaceUI (applyUIChange change ui), state)

	adjust (change,LSLayoutSubUIs ui (UIModified state))
		# ui = applyUIChange change ui //Keep the 'shadow' copy of the UI up-to date
		| inUISelection selection [] ui //The layout should still be applied
			# (change,state) = layout.Layout.adjust (change, state)
			= (change, LSLayoutSubUIs ui (UIModified state))
		| otherwise //The layout should no longer be applied, use the restore function to undo the layout
			# change = mergeUIChanges (layout.Layout.restore state) change
			= (change, LSLayoutSubUIs ui (SubUIsModified []))

	adjust (change, LSLayoutSubUIs ui (SubUIsModified states))
		# (change,state) = layoutChange_ [] pred layout change states	
		= (change,LSLayoutSubUIs ui state)

	restore (LSLayoutSubUIs ui _) = ReplaceUI ui //VERY CRUDE RESTORE... TODO:We can do better than this

layoutChange_ :: UIPath (UIPath UI -> Bool) Layout UIChange [(Int,LayoutTree LayoutState)] -> (UIChange,LayoutTree LayoutState)
layoutChange_ path pred layout (ReplaceUI ui) states
	# (ui,eitherState) = layoutUI_ path pred layout ui
	= (ReplaceUI ui,eitherState)
layoutChange_ path pred layout (ChangeUI localChanges childChanges) states
	# (childChanges,states) = layoutChildChanges_ path pred layout childChanges states
	= (ChangeUI localChanges childChanges, SubUIsModified states)
layoutChange_ path pred layout change states
	= (change,SubUIsModified states)

layoutUI_ :: UIPath (UIPath UI -> Bool) Layout UI -> (UI,LayoutTree LayoutState)
layoutUI_ path pred layout ui=:(UI type attr items)
	| pred path ui
		# (change,state) = layout.Layout.apply ui 	
		= (applyUIChange change ui,UIModified state)
	| otherwise
		# (items,states) = unzip [let (ui`,s) = layoutUI_ (path ++ [i]) pred layout ui in (ui`,(i,s)) \\ ui <- items & i <- [0..]]
		# states = filter (\(_,s) -> not (s =:(SubUIsModified []))) states //Filter unnecessary state
		= (UI type attr items, SubUIsModified states)

layoutChildChanges_ :: UIPath (UIPath UI -> Bool) Layout [(Int,UIChildChange)] [(Int,LayoutTree LayoutState)] 
                    -> (![(Int,UIChildChange)],![(Int,LayoutTree LayoutState)])
layoutChildChanges_ path pred layout [] states = ([],states)
layoutChildChanges_ path pred layout [c:cs] states
	# (c,states) = layoutChildChange_ path pred layout c states
	# (cs,states) = layoutChildChanges_ path pred layout cs states
	= ([c:cs],states)
where
	//Check if there is existing state for the change, in that case a layout was applied and we need
	//to pass the change and the state to that layout, otherwise we need to recursively adjust the change
	layoutChildChange_ path pred layout (idx,ChangeChild change) states = case selectState idx states of
		(Just (UIModified state),states) //Reapply the layout with the stored state
			# (change,state) = layout.Layout.adjust (change,state)
			= ((idx,ChangeChild change),[(idx,UIModified state):states])
		(Just (SubUIsModified childStates),states) //Recursively adjust the change
			# (change,state) = layoutChange_ (path ++ [idx]) pred layout change childStates
			= case state of
				SubUIsModified [] = ((idx,ChangeChild change),states) //Don't store empty state
				_ 				  = ((idx,ChangeChild change),[(idx,state):states])
		(Nothing,states) //Recursively adjust the change
			# (change,state) = layoutChange_ (path ++ [idx]) pred layout change []
			= case state of
				SubUIsModified [] = ((idx,ChangeChild change),states) //Don't store empty state
				_ 				  = ((idx,ChangeChild change),[(idx,state):states])

	layoutChildChange_ path pred layout (idx,InsertChild ui) states
		# (ui,eitherState) = layoutUI_ (path ++ [idx]) pred layout ui
		= ((idx,InsertChild ui),[(idx,eitherState):[(if (i >= idx) (i + 1) i,s)  \\ (i,s) <- states]]) //Also adjust the indices of the other states
	layoutChildChange_ path pred layout (idx,RemoveChild) states
		= ((idx,RemoveChild),[(if (i > idx) (i - 1) i, s) \\ (i,s) <- states | i <> idx]) //Remove the current state from the states and adjust the indices accordingly
	layoutChildChange_ path pred layout (idx,MoveChild dst) states //Move the states 
		# (srcState,states) = selectState idx states
		# states = [(if (i > idx) (i - 1) i, s) \\ (i,s) <- states] //Everything moves down when we remove the branch
		# states = [(if (i >= dst) (i + 1) i, s) \\ (i,s) <- states] //Move everything after the destination move up to make 'space' for the move
		# states = maybe [] (\s -> [(dst,s)]) srcState ++ states //Move to destination
		= ((idx,MoveChild dst),states)

	selectState idx states = case splitWith (((==) idx) o fst) states of
        ([(_,s):_],states) = (Just s,states)
        _                  = (Nothing,states)

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
	apply _ = (NoChange,LSJson JSONNull)

	adjust (NoChange,s)
		 = (NoChange,s)
	adjust (ReplaceUI ui,_) 
		//Determine a skeleton of the reordered ui, and replace references
		//Replace references to parts of the original ui
		# (moves,ui) = derefAll ui [] (reorder ui)
		= (ReplaceUI ui,LSJson (toJSON moves))
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
