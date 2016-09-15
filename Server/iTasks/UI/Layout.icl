implementation module iTasks.UI.Layout

import StdTuple, StdList, StdBool, StdInt, StdOrdList, StdArray, StdMisc
import Data.Maybe, Data.Either, Text, Data.Tuple, Data.List, Data.Either, Data.Functor
import iTasks._Framework.Util, iTasks._Framework.HtmlUtil, iTasks.UI.Definition
import iTasks.API.Core.Types, iTasks.API.Core.TaskCombinators

from Data.Map as DM import qualified put, get, del, newMap, toList, fromList, alter, union, keys

from StdFunc import o, const, id, flip
from iTasks._Framework.TaskState import :: TIMeta(..), :: TaskTree(..), :: DeferredJSON

//This type records where parts were removed from a ui tree
:: NodeMoves :== [(Int,NodeMove)] 
:: NodeMove = BranchMoved
            | ChildBranchesMoved NodeMoves

//This type records the states of layouts applied somewhere in a ui tree
:: NodeLayoutStates = NL [(Int,Either JSONNode NodeLayoutStates)]

//This type represents the structure of a ui tree, which needs to be remembered when flattening a tree
:: NodeSpine = NS [NodeSpine]

derive JSONEncode NodeMove, NodeLayoutStates, NodeSpine
derive JSONDecode NodeMove, NodeLayoutStates, NodeSpine

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
		# attrChanges = filter (\(SetAttribute k v) -> not (isMember k ('DM'.keys extraAttr))) attrChanges
		= (ChangeUI attrChanges itemChanges,s)
	layout (change,s) = (change,s)

copyAttributes :: [String] NodePath NodePath -> Layout
copyAttributes selection src dst = copyAttributes` (Just selection) src dst

copyAllAttributes :: NodePath NodePath -> Layout
copyAllAttributes src dst = copyAttributes` Nothing src dst

copyAttributes` :: (Maybe [String]) NodePath NodePath -> Layout
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
		(UI _ _ [child:_])  = (ReplaceUI child,JSONNull)
		_ 					= (ReplaceUI (ui UIEmpty),JSONNull)

	layout (ChangeUI _ childChanges,s) = case [change \\ (0,ChangeChild change) <- childChanges] of
		[change] = (change,s)
		_        = (NoChange,s)

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

insertSubAt :: NodePath UI-> Layout
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

moveSubAt :: NodePath NodePath -> Layout 
moveSubAt src dst = moveSubs_ pred (Just dst)
where
	pred path _ = path == src

removeSubAt :: NodePath -> Layout
removeSubAt src = moveSubs_ pred Nothing
where
	pred path _ = path == src

layoutSubAt :: NodePath Layout -> Layout
layoutSubAt target layout = layoutSubs_ pred layout
where
	pred path _ = path == target

removeSubsMatching :: NodePath (UI -> Bool) -> Layout
removeSubsMatching src pred = moveSubs_ pred` Nothing
where
	pred` path ui = isSubPathOf_ path src && pred ui

moveSubsMatching :: NodePath (UI -> Bool) NodePath -> Layout
moveSubsMatching src pred dst = moveSubs_ pred` (Just dst)
where
	pred` path ui = isSubPathOf_ path src && pred ui

layoutSubsMatching :: NodePath (UI -> Bool) Layout -> Layout
layoutSubsMatching src pred layout = layoutSubs_ pred` layout
where
	pred` path ui = isSubPathOf_ path src && pred ui

//Test if a path extends another path
isSubPathOf_ :: NodePath NodePath -> Bool
isSubPathOf_ p [] = True  //Everything is a sub path of the root path
isSubPathOf_ [] p = False //If the path we are checking against is longer, it can't be a sub path
isSubPathOf_ [p1:ps1] [p2:ps2] //Check if prefix is the same
	| p1 == p2  = isSubPathOf_ ps1 ps2	
				= False

moveSubs_ :: (NodePath UI -> Bool) (Maybe NodePath) -> Layout
moveSubs_ pred mbDst = layout
where
	layout (change,s)
		# moves = if (change=:(ReplaceUI _)) [] (fromMaybe [] (fromJSON s)) //On a replace, we reset the state
		# startIdx = maybe 0 last mbDst
		//Remove based on the predicate
		# (change,moves,inserts) = removeAndAdjust_ [] pred startIdx change moves
		//If there is a destination path, adjust the change for these moves
		= case mbDst of
			Just dst = (insertAndAdjust_ (init dst) startIdx (countMoves_ moves True) inserts change, toJSON moves)
			Nothing  = (change, toJSON moves)

/**
* This is the core function that tranforms UIChange instructions to effect the layout
* It uses a datastructure (NodeMoves) to track changes that have been applied in 'previous' calls to this function
*
* @param path::NodePath: The location in the (unmodified) tree where they original change was targeted at
* @param pred::(NodePath UI -> Bool): The predicate that tests if a node should be moved
* @param targetIdx::Int: The index in the destination node where the nodes are moved to
* @param change:UIChange: The change that needs to be transformed
*/
removeAndAdjust_ :: NodePath (NodePath UI -> Bool) Int UIChange NodeMoves -> (!UIChange,!NodeMoves,![(Int,UIChildChange)])
//Basic NoChange case: if there is no change we don't need to transform anything
//            We do need to count how many nodes were removed to keep track of the targetIndex in other branches
removeAndAdjust_ path pred targetIdx NoChange moves //Only adjust the targetIdx by counting the moved nodes
	= (NoChange, moves, [])
//Replacement case: this part of the UI is replaced. We need to remove the nodes we previously moved and find which ones to move in the new UI
removeAndAdjust_ path pred targetIdx (ReplaceUI ui) moves //If the node is replaced, adjust the new ui and determine changes to the previously moved nodes
	//Remove all previously moved nodes
	# removals = repeatn (countMoves_ moves True) (targetIdx,RemoveChild)
	//Determine new moves in the replacement ui
	= case collectNodes_ path pred targetIdx ui of
		//If the predicate matches the root node don't change anything.
		//It is impossible to create an adjusted ReplaceUI instruction if the root node is removed
		(Nothing,_,_)             = (ReplaceUI ui, moves, [])
		(Just ui, moves, inserts) = (ReplaceUI ui, moves, removals ++  inserts )
//The change case: We need to adjust the changes to the child branches
removeAndAdjust_ path pred targetIdx (ChangeUI localChanges childChanges) moves 
	# (moves, childChanges, inserts) = adjustChildChanges targetIdx moves childChanges 
	= (ChangeUI localChanges childChanges, moves, inserts)
where
	adjustChildChanges tidx moves [] = (moves,[],[])
	//- Insert 
	adjustChildChanges tidx moves [(idx,InsertChild ui):cs] 
		//Determine additional moves in the replacement ui
		# (mbUI,subMoves,subInserts) = collectNodes_ (path ++ [idx]) pred (adjustTargetIndex moves idx tidx) ui	
		//Adjust the change
		# change = case mbUI of 
			Nothing = [] //The top node of the inserted UI matched, record the move, but don't insert anything
			Just ui = [(adjustIndex moves idx,InsertChild ui)]
		//Adjust the moved nodes state to adjust for the 'inserted' branch
		# moves = [(if (i >= idx) (i + 1) i , m) \\ (i,m) <- moves]
		# moves = if (mbUI =:Nothing) [(idx,BranchMoved):moves] moves
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
				= case collectNodes_ (path ++ [idx]) pred (adjustTargetIndex moves idx tidx) ui of
					(Nothing,subMoves,subInserts) //The inserted UI matched, record the move and remove the child
						= ([(adjustIndex moves idx, RemoveChild)]
                      	  ,[(idx,BranchMoved):moves]
                      	  ,subInserts)
					(_,[],_) //Nothing matched, no need to record anything
						= ([(adjustIndex moves idx,ChangeChild (ReplaceUI ui))]
					  	  ,moves
                          ,[])
					(Just ui,subMoves,subInserts) //One or more sub nodes matched, we need to record the moves for this branch
					    = ([(adjustIndex moves idx, ChangeChild (ReplaceUI ui))]
					      ,[(idx,ChildBranchesMoved subMoves)]
					      ,subInserts)
			//Previously this child node matched the predicate
			Just BranchMoved 
				| pred (path ++ [idx]) ui //The replacement still matches, just replace in the target locatation
					= ([]
                      ,moves
					  ,[(adjustTargetIndex moves idx tidx,ChangeChild change)])
				| otherwise //The Moved node should no longer be moved -> change the replacement to an insert instruction
					= case collectNodes_ (path ++ [idx]) pred (adjustTargetIndex moves idx tidx) ui of
						(_,[],_) //Nothing matched, no need to record anything
							= ([(adjustIndex moves idx,InsertChild ui)]
					   	  	  ,[(i,m) \\ (i,m) <- moves | i <> idx]
					          ,[(adjustTargetIndex moves idx tidx,RemoveChild)])
						(Just ui,subMoves,subInserts)
						    = ([(adjustIndex moves idx,InsertChild ui)]
						      ,[(idx,ChildBranchesMoved subMoves):[(i,m) \\ (i,m) <- moves | i <> idx]]
						      ,[(adjustTargetIndex moves idx tidx,RemoveChild):subInserts])
			//Previously children of the child node matched the predicate
			Just (ChildBranchesMoved subMoves)
				//Create remove instructions for the replaced nodes
				# inserts = repeatn (countMoves_ subMoves True) (adjustTargetIndex moves idx tidx,RemoveChild) 
				//Find out what needs to be replaced in the new ui
				= case collectNodes_ (path ++ [idx]) pred (adjustTargetIndex moves idx tidx) ui of
					(Nothing,subMoves,subInserts) //The replacement UI matched, record the move
						= ([(adjustIndex moves idx, RemoveChild)]
					      ,[(idx,BranchMoved):[(i,m) \\ (i,m) <- moves | i <> idx]]
					      ,inserts ++ subInserts)
					(_,[],_) //Nothing matched, no longer need to record anything
						= ([(adjustIndex moves idx, ChangeChild (ReplaceUI ui))]
                          ,[(i,m) \\ (i,m) <- moves | i <> idx]
                          ,inserts)
					(Just ui,subMoves,subInserts) //One or more sub nodes matched, we need to record the moves for this branch
				        = ([(adjustIndex moves idx, ChangeChild (ReplaceUI ui))]
                          ,[(idx,ChildBranchesMoved subMoves):[(i,m) \\ (i,m) <- moves | i <> idx]]
					      ,inserts ++ subInserts)
		# (moves,cs,inserts) = adjustChildChanges tidx moves cs
		= (moves, change ++ cs, subInserts ++ inserts)
	//- Other recursive changes
	adjustChildChanges tidx moves [(idx,ChangeChild change):cs] 
		# (change,moves,subInserts) = case findMove idx moves of
			//Nothing to do, only adjust the index
			Nothing = ([(adjustIndex moves idx,ChangeChild change)]
					  ,moves
					  ,[])
			//Redirect the change
			Just BranchMoved
					= ([]
					  ,moves
					  ,[(adjustTargetIndex moves idx tidx,ChangeChild change)])
			//Recursively adjust the change 
			Just (ChildBranchesMoved subMoves)
					# (change,subMoves,subInserts) = removeAndAdjust_ (path ++ [idx]) pred (adjustTargetIndex moves idx tidx) change subMoves
					= ([(adjustIndex moves idx,ChangeChild change)]
                      ,[(idx,ChildBranchesMoved subMoves):[(i,m) \\ (i,m) <- moves | i <> idx]]
					  ,subInserts)
		# (moves,cs,inserts) = adjustChildChanges tidx moves cs
		= (moves, change ++ cs, subInserts ++ inserts)

	adjustIndex moves idx = idx - foldr (\(i,m) n -> if (i <= idx && m =: BranchMoved) (n + 1) n) 0 moves

	adjustTargetIndex moves idx tidx = tidx + countMoves_ [(i,m) \\ (i,m) <- moves | i < idx] True

	findMove idx moves = listToMaybe [m \\ (i,m) <- moves | i == idx]

insertAndAdjust_ :: NodePath Int Int [(Int,UIChildChange)] UIChange -> UIChange
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
	count (ChildBranchesMoved moves) n = if recursive (n + countMoves_ moves recursive) n

//Collect parts of a UI and record their positions
collectNodes_ :: NodePath (NodePath UI -> Bool) Int UI -> (Maybe UI, NodeMoves, [(Int,UIChildChange)])
collectNodes_ path pred idx ui=:(UI type attr items)
	| pred path ui	= (Nothing, [], [(idx,InsertChild ui)])
	| otherwise 
		# (items, moves, inserts) = collectInItems idx 0 items
		= (Just (UI type attr items), moves, inserts)
where
	collectInItems idx i [] = ([],[],[])
	collectInItems idx i [item:items]
		# (mbItem, itemMoves, itemInserts) = collectNodes_ (path ++ [i]) pred idx item
		# (items, moves, inserts)          = collectInItems idx (i + 1) items
		= case mbItem of 
			Nothing   //The item itself was collected
				= (items, [(i,BranchMoved):moves], itemInserts ++ inserts)
			Just item //Maybe modified
				| itemMoves =:[] //If there are no moves in the branch, we don't need to add it
					= ([item:items], moves, itemInserts ++ inserts)
				| otherwise	
					= ([item:items], [(i,ChildBranchesMoved itemMoves):moves], itemInserts ++ inserts)

insertNodes_ :: NodePath [(Int,UIChildChange)] UI -> UI
insertNodes_ [] changes (UI type attr items) = UI type attr (foldl apply items changes)
where
	apply items (i,RemoveChild) = removeAt i items
	apply items (i,InsertChild ui) = insertAt i ui items
	apply items change = items

insertNodes_ [s:ss] changes (UI type attr items)
	| s < length items  = UI type attr (updateAt s (insertNodes_ ss changes (items !! s)) items)
	| otherwise 		= UI type attr items

layoutSubs_ :: (NodePath UI -> Bool) Layout -> Layout
layoutSubs_ pred layout = layout`
where
	layout` (change,s)
		| change=:(ReplaceUI _)
			# (change,eitherState) = layoutChange_ [] pred layout change (NL [])
			= (change,toJSON eitherState)
		| otherwise
			# (change,eitherState) = case fromMaybe (Right (NL [])) (fromJSON s) of
				(Left state) = appSnd Left (layout (change,state))
				(Right states) = layoutChange_ [] pred layout change states
			= (change,toJSON eitherState)

layoutChange_ :: NodePath (NodePath UI -> Bool) Layout UIChange NodeLayoutStates -> (UIChange,Either JSONNode NodeLayoutStates)
layoutChange_ path pred layout (ReplaceUI ui) states
	# (ui,eitherState) = layoutUI_ path pred layout ui
	= (ReplaceUI ui,eitherState)
layoutChange_ path pred layout (ChangeUI localChanges childChanges) (NL states)
	# (childChanges,states) = layoutChildChanges_ [] pred layout childChanges states
	= (ChangeUI localChanges childChanges, Right (NL states))
layoutChange_ path pred layout change states
	= (change,Right states)

layoutUI_ :: NodePath (NodePath UI -> Bool) Layout UI -> (UI,Either JSONNode NodeLayoutStates)
layoutUI_ path pred layout ui=:(UI type attr items)
	| pred path ui
		= case layout (ReplaceUI ui,JSONNull) of
			(ReplaceUI ui,state) = (ui,Left state)
			_                    = (ui,Right (NL [])) //Consider it a non-match if the layout function behaves flakey
	| otherwise
		# (items,states) = unzip [let (ui`,s) = layoutUI_ (path ++ [i]) pred layout ui in (ui`,(i,s)) \\ ui <- items & i <- [0..]]
		# states = filter (\(_,s) -> not (s =:(Right (NL [])))) states //Filter unnecessary state
		= (UI type attr items ,Right (NL states))

layoutChildChanges_ :: NodePath (NodePath UI -> Bool) Layout [(Int,UIChildChange)] [(Int,Either JSONNode NodeLayoutStates)]
                    -> (![(Int,UIChildChange)],![(Int,Either JSONNode NodeLayoutStates)])
layoutChildChanges_ path pred layout [] states = ([],states)
layoutChildChanges_ path pred layout [c=:(idx,_):cs] states
	# (statesBefore,statesAfter) = seek idx states
	# (c,statesAfter) = layoutChildChange_ path pred layout c statesAfter
	# (cs,statesAfter) = layoutChildChanges_ path pred layout cs statesAfter
	= ([c:cs],statesBefore ++ statesAfter)
where
	seek idx states = ([c \\ c <- states | fst c < idx], [c \\ c <- states | fst c >= idx])

	//Check if there is existing state for the change, in that case a layout was applied and we need
	//to pass the change and the state to that layout, otherwise we need to recursively adjust the change
	layoutChildChange_ path pred layout (i,ChangeChild change) states = case selectState i states of
		(Just (Left state),states) //Reapply the layout with the stored state
			# (change,state) = layout (change,state)
			= ((i,ChangeChild change),[(i,Left state):states])
		(Just (Right childStates),states) //Recursively adjust the change
			# (change,eitherStates) = layoutChange_ (path ++ [i]) pred layout change childStates
			= ((i,ChangeChild change),[(i,eitherStates):states])
		(Nothing,states) //Nothing to do
			= ((i,ChangeChild change),states) 
	layoutChildChange_ path pred layout (i,InsertChild ui) states
		# (ui,eitherState) = layoutUI_ (path ++ [i]) pred layout ui
		= ((i,InsertChild ui),[(i,eitherState):[(i + 1,s) \\ (i,s) <- states]]) //Also adjust the indices of the other states
	layoutChildChange_ path pred layout (idx,RemoveChild) states
		= ((idx,RemoveChild),[(i - 1, s) \\ (i,s) <- states | i <> idx]) //Remove the current state from the states and adjust the indices accordingly
	layoutChildChange_ path pred layout (idx,MoveChild nidx) states //Adjust the indices TODO
		= ((idx,MoveChild nidx),states)

	selectState idx states = case splitWith (((==) idx) o fst) states of
        ([(_,s):_],states) = (Just s,states)
        _                  = (Nothing,states)


//Common patterns
moveChildren :: NodePath (UI -> Bool) NodePath -> Layout
moveChildren container pred dst = moveSubs_ pred` (Just dst)
where
	pred` path ui = isSubPathOf_ path container && length path == length container + 1 && pred ui

layoutChildrenOf :: NodePath Layout -> Layout
layoutChildrenOf container layout = layoutSubs_ pred layout
where
	pred path ui = isSubPathOf_ path container && length path == length container + 1

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

//Select the first matching layout
selectLayout :: [(UI -> Bool,Layout)] -> Layout
selectLayout layouts = layout
where
	layout (change=:(ReplaceUI def),_) = case selectLayout def 0 layouts of
		Just (index,childLayout)
			# (change,state) = childLayout (change,JSONNull)
			= (change,JSONArray [JSONInt index,state])
		Nothing = (change,JSONNull)

	layout (change,JSONArray [JSONInt index,state])
		# (change,state) = (snd (layouts !! index)) (change,state)
		= (change,JSONArray [JSONInt index,state])

	layout (change,s) = (change,s)

	selectLayout def i [] = Nothing
	selectLayout def i [(pred,layout):ls]
		| pred def 	= Just (i,layout)
					= selectLayout def (i + 1) ls

conditionalLayout :: (UI -> Bool) Layout -> Layout
conditionalLayout pred condLayout = selectLayout [(pred,condLayout)]

