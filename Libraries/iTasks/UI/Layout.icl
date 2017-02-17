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


//This type records where parts were removed from a ui tree
:: NodeMoves :== [(Int,NodeMove)] 
:: NodeMove = BranchMoved                  //This branch was moved to another location (or removed)
			| BranchHidden UI              //This branch was hidden and can be restored
            | ChildBranchesMoved NodeMoves

//This type records the states of layouts applied somewhere in a ui tree
:: NodeLayoutStates :== [(Int,NodeLayoutState)]
:: NodeLayoutState
	= BranchLayout JSONNode
	| ChildBranchLayout NodeLayoutStates
			
//This type represents the structure of a ui tree, which needs to be remembered when flattening a tree
:: NodeSpine = NS [NodeSpine]

derive JSONEncode NodeMove, NodeLayoutState, NodeSpine
derive JSONDecode NodeMove, NodeLayoutState, NodeSpine

instance tune ApplyLayout
where
	tune (ApplyLayout f) task=:(Task evala) = Task eval
	where
		eval event evalOpts (TCDestroy (TCLayout s tt)) iworld //Cleanup duty simply passed to inner task
			= evala event evalOpts (TCDestroy tt) iworld

		eval event evalOpts tt=:(TCInit _ _) iworld
			= eval event evalOpts (TCLayout JSONNull tt) iworld

		eval event evalOpts (TCLayout s tt) iworld = case evala event evalOpts tt iworld of
	        (ValueResult value info change tt,iworld) 
				# s = fromMaybe JSONNull (fromJSON s)	
				# (change,s) = f (change,s)
				= (ValueResult value info change (TCLayout s tt), iworld)
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

setNodeType :: UINodeType -> Layout
setNodeType type = layout
where
	layout (ReplaceUI (UI _ attr items),s) = (ReplaceUI (UI type attr items),s)
	layout (change,s) = (change,s)

setAttributes :: UIAttributes -> Layout
setAttributes extraAttr = layout 
where
	layout (ReplaceUI (UI type attr items),s) = (ReplaceUI (UI type ('DM'.union extraAttr attr) items),s)
	layout (ChangeUI attrChanges itemChanges,s)
		//Filter out updates for the attributes that we are setting here
		# attrChanges = filter (\(SetAttribute k _) -> not (isMember k ('DM'.keys extraAttr))) attrChanges
		= (ChangeUI attrChanges itemChanges,s)
	layout (change,s) = (change,s)

delAttributes :: [String] -> Layout
delAttributes delAttr = layout
where
	layout (ReplaceUI (UI type attr items),s) = (ReplaceUI (UI type (foldr 'DM'.del attr delAttr) items),s)
	layout (ChangeUI attrChanges itemChanges,s)
		# attrChanges = filter (\(SetAttribute k _) -> not (isMember k delAttr)) attrChanges
		= (ChangeUI attrChanges itemChanges,s)
	layout (change,s) = (change,s)
	
copyAttributes :: [String] UIPath UIPath -> Layout
copyAttributes selection src dst = copyAttributes` (Just selection) src dst

copyAllAttributes :: UIPath UIPath -> Layout
copyAllAttributes src dst = copyAttributes` Nothing src dst

copyAttributes` :: (Maybe [String]) UIPath UIPath -> Layout
copyAttributes` selection src dst = layout //TODO: Also handle attribute updates in the src location, and partial replacements along the path
where
	layout (ReplaceUI ui,s) = case selectAttr src ui of 
		Just attr = (ReplaceUI (addAttr attr dst ui),s)
		Nothing   = (ReplaceUI ui,s)
	layout (change,s) = (change,s)

	selectAttr [] (UI type attr items) = Just attr
	selectAttr [s:ss] (UI type attr items) 
		| s < length items  = selectAttr ss (items !! s)
							= Nothing

	addAttr extra [] (UI type attr items)
		= UI type (foldl (\m (k,v) -> 'DM'.put k v m) attr [(k,v) \\ (k,v) <- 'DM'.toList extra | condition k]) items
	addAttr extra [s:ss] (UI type attr items) 
		| s < length items = UI type attr (updateAt s (addAttr extra ss (items !! s)) items) 
						   = UI type attr items

	condition = maybe (const True) (flip isMember) selection

modifyAttribute :: String (JSONNode -> UIAttributes) -> Layout
modifyAttribute name modifier = layout
where
	layout (ReplaceUI (UI type attr items),s)
		# attr = maybe attr (\val -> 'DM'.union (modifier val) attr) ('DM'.get name attr)
		= (ReplaceUI (UI type attr items),s)

	layout (ChangeUI attrChanges childChanges,s)
		# attrChanges = flatten [if (key == name) [SetAttribute k v \\ (k,v) <- 'DM'.toList (modifier value)] [c] \\c=:(SetAttribute key value) <- attrChanges]
		= (ChangeUI attrChanges childChanges,s)
	layout (c,s) = (c,s)

wrapUI :: UINodeType -> Layout
wrapUI type = layout
where
	layout (ReplaceUI def,_) = (ReplaceUI (uic type [def]),JSONNull)
	layout (NoChange,def) = (NoChange,def)
	layout (change,s) = (ChangeUI [] [(0,ChangeChild change)],s)

unwrapUI :: Layout
unwrapUI = layout
where
	layout (ReplaceUI def,_) = case def of
		(UI _ _ [child:_])  = (ReplaceUI child,JSONBool False)
		_ 					= (ReplaceUI (ui UIEmpty),JSONBool True) //If there is no inner component, remember we replaced it with empty...

	layout (ChangeUI _ childChanges,s=:(JSONBool False)) = case [change \\ (0,ChangeChild change) <- childChanges] of
		[change] = (change,s) //TODO: Check if there are cases with multiple changes to child 0
		[change:x] = trace_n "Warning: unwrapUI: edge case" (NoChange,s) //TODO: Check if there are cases with multiple changes to child 0
		_        = (NoChange,s)

	layout (change,s) = (NoChange,s) 

flattenUI :: Layout
flattenUI = layout
where
	layout (ReplaceUI def,_)
		# (def,spine) = flattenWithSpine def
		= (ReplaceUI def, toJSON spine)
	layout (change,s) = (change,s) //TODO

	flattenWithSpine  ui=:(UI type attr items) 
		# (items,spines) = unzip (map flattenWithSpine items)
		# items = flatten [[UI type attr []:children] \\ UI type attr children <- items]
		= (UI type attr items,NS spines)

insertSubAt :: UIPath UI-> Layout
insertSubAt [] def = id
insertSubAt path def = layoutSubAt (init path) (insertSub (last path) def)
where
	insertSub idx def (ReplaceUI (UI type attr items),s) = (ReplaceUI (UI type attr (insertAt idx def items)),s)
	insertSub idx _ (ChangeUI attrChanges childChanges,s) = (ChangeUI attrChanges (insert idx childChanges),s)
	where
		insert idx [] = []
		insert idx [c:cs]
			| fst c < idx  = [c:insert idx cs]
                       	   = [(idx,ChangeChild (ChangeUI [] [])): [(n+1,x) \\(n,x) <-[c:cs]]]

	insertSub _ _ (change,s) = (change,s)

moveSubAt :: UIPath UIPath -> Layout 
moveSubAt src dst = moveSubs (SelectByPath src) dst

removeSubAt :: UIPath -> Layout
removeSubAt src = removeSubs (SelectByPath src)

layoutSubAt :: UIPath Layout -> Layout
layoutSubAt target layout = layoutSubs (SelectByPath target) layout

/*
layoutSubsOfType :: UIPath [UINodeType] Layout -> Layout
layoutSubsOfType src [t:ts] layout = layoutSubs (inUISelection (SelectAND SelectDescendents selectTypes)) layout
where
	selectTypes	= foldl SelectOR (SelectByType t) (map SelectByType ts)
*/

removeSubs :: UISelection -> Layout 
removeSubs selection = layout
where
	pred = inUISelection selection
	layout (change,s)
		# moves = if (change=:(ReplaceUI _)) [] (fromMaybe [] (fromJSON s)) //On a replace, we reset the state
		# (change,moves,_) = removeAndAdjust_ [] pred False 0 change moves
		= (change, toJSON moves)

moveSubs :: UISelection UIPath -> Layout
moveSubs selection dst = layout
where
	pred = inUISelection selection
	layout (change,s)
		# moves = if (change=:(ReplaceUI _)) [] (fromMaybe [] (fromJSON s)) //On a replace, we reset the state
		# startIdx = last dst
		//Remove based on the predicate
		# (change,moves,inserts) = removeAndAdjust_ [] pred False startIdx change moves
		//If there is a destination path, adjust the change for these moves
	 	# change = insertAndAdjust_ (init dst) startIdx (countMoves_ moves True) inserts change
	    = (change, toJSON moves)

hideSubs :: UISelection -> Layout 
hideSubs selection = layout
where
	pred = inUISelection selection
	layout (change,s)
		# moves = if (change=:(ReplaceUI _)) [] (fromMaybe [] (fromJSON s)) //On a replace, we reset the state
		# (change,moves,_) = removeAndAdjust_ [] pred True 0 change moves
		= (change, toJSON moves)

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
	//- Insert 
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
	//- Remove
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
	//- Move
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
	//- Replace
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
	//- Other recursive changes
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

layoutSubs :: UISelection Layout -> Layout
layoutSubs selection layout = layout`
where
	pred = inUISelection selection
	layout` (change,s)
		| change=:(ReplaceUI _)
			# (change,eitherState) = layoutChange_ [] pred layout change []
			= (change,toJSON eitherState)
		| otherwise
			# (change,eitherState) = case fromMaybe (ChildBranchLayout []) (fromJSON s) of
				(BranchLayout state) = appSnd BranchLayout (layout (change,state))
				(ChildBranchLayout states) = layoutChange_ [] pred layout change states
			= (change,toJSON eitherState)

layoutChange_ :: UIPath (UIPath UI -> Bool) Layout UIChange NodeLayoutStates -> (UIChange,NodeLayoutState)
layoutChange_ path pred layout (ReplaceUI ui) states
	# (ui,eitherState) = layoutUI_ path pred layout ui
	= (ReplaceUI ui,eitherState)
layoutChange_ path pred layout (ChangeUI localChanges childChanges) states
	# (childChanges,states) = layoutChildChanges_ path pred layout childChanges states
	= (ChangeUI localChanges childChanges, ChildBranchLayout states)
layoutChange_ path pred layout change states
	= (change,ChildBranchLayout states)

layoutUI_ :: UIPath (UIPath UI -> Bool) Layout UI -> (UI,NodeLayoutState)
layoutUI_ path pred layout ui=:(UI type attr items)
	| pred path ui
		= case layout (ReplaceUI ui,JSONNull) of
			(ReplaceUI ui,state) = (ui,BranchLayout state)
			_                    = (ui,ChildBranchLayout []) //Consider it a non-match if the layout function behaves flakey
	| otherwise
		# (items,states) = unzip [let (ui`,s) = layoutUI_ (path ++ [i]) pred layout ui in (ui`,(i,s)) \\ ui <- items & i <- [0..]]
		# states = filter (\(_,s) -> not (s =:(ChildBranchLayout []))) states //Filter unnecessary state
		= (UI type attr items, ChildBranchLayout states)

layoutChildChanges_ :: UIPath (UIPath UI -> Bool) Layout [(Int,UIChildChange)] NodeLayoutStates
                    -> (![(Int,UIChildChange)],!NodeLayoutStates)
layoutChildChanges_ path pred layout [] states = ([],states)
layoutChildChanges_ path pred layout [c:cs] states
	# (c,states) = layoutChildChange_ path pred layout c states
	# (cs,states) = layoutChildChanges_ path pred layout cs states
	= ([c:cs],states)
where
	//Check if there is existing state for the change, in that case a layout was applied and we need
	//to pass the change and the state to that layout, otherwise we need to recursively adjust the change
	layoutChildChange_ path pred layout (idx,ChangeChild change) states = case selectState idx states of
		(Just (BranchLayout state),states) //Reapply the layout with the stored state
			# (change,state) = layout (change,state)
			= ((idx,ChangeChild change),[(idx,BranchLayout state):states])
		(Just (ChildBranchLayout childStates),states) //Recursively adjust the change
			# (change,state) = layoutChange_ (path ++ [idx]) pred layout change childStates
			= case state of
				ChildBranchLayout [] = ((idx,ChangeChild change),states) //Don't store empty state
				_ 					 = ((idx,ChangeChild change),[(idx,state):states])
		(Nothing,states) //Recursively adjust the change
			# (change,state) = layoutChange_ (path ++ [idx]) pred layout change []
			= case state of
				ChildBranchLayout [] = ((idx,ChangeChild change),states) //Don't store empty state
				_ 					 = ((idx,ChangeChild change),[(idx,state):states])

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

sequenceLayouts :: [Layout] -> Layout
sequenceLayouts layouts = layout
where
	layout (change=:(ReplaceUI _),_)
		# (change,states) = applyAll layouts [] change 
		= (change,JSONArray states)
	
	layout (change,JSONArray states) 
		# (change,states) = applyAll layouts states change 
		= (change,JSONArray states)
	layout (change,s) = (change,s)

	applyAll [] _ change = (change,[])
	applyAll [l:ls] states change 
		# [s:ss] = case states of [] = [JSONNull]; _ = states;
		# (change,s) = l (change,s) 
		# (change,ss) = applyAll ls ss change
		= (change,[s:ss])


//Common patterns
/*
layoutChildrenOf :: UIPath Layout -> Layout
layoutChildrenOf container layout = layoutSubs pred layout
where
	pred path ui = isSubPathOf_ path container && length path == length container + 1
*/

traceLayout :: String Layout -> Layout
traceLayout name layout = layout`
where
	layout` (change,state)
		# (change`,state`) = layout (change,state)
		# msg = join "\n" 
			["Layout trace ("+++ name +++")"
			,"ORIGINAL CHANGE:"			
			,toString (toJSON change)
			,"REWRITTEN CHANGE:"
			,toString (toJSON change`)]
		= trace_n msg (change`,state`)

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
reorderUI reorder = layout
where
	layout (NoChange,s)
		 = (NoChange,s)
	layout (ReplaceUI ui,_) 
		//Determine a skeleton of the reordered ui, and replace references
		//Replace references to parts of the original ui
		# (moves,ui) = derefAll ui [] (reorder ui)
		= (ReplaceUI ui,toJSON moves)
	//Adjust followup changes to the moved parts
	layout (c,s) = (adjust (fromMaybe 'DM'.newMap (fromJSON s)) c,s)

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

	adjust :: (Map UIPath UIPath) UIChange -> UIChange //TODO
	adjust moves change = change 
	where
		selectChanges :: [UIPath] UIChange -> [(UIPath,UIChange)]
		selectChanges _ _ = []

		remap :: (Map UIPath UIPath) [(UIPath,UIChange)] -> [(UIPath,UIChange)]
		remap moves changes =[(fromJust ('DM'.get path moves),change) \\ (path,change) <- changes]
		
		combineChanges :: [(UIPath,UIChange)] -> UIChange
		combineChanges _ = NoChange
