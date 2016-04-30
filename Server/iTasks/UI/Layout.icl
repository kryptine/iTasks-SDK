implementation module iTasks.UI.Layout

import StdTuple, StdList, StdBool, StdInt, StdOrdList, StdArray
import Data.Maybe, Data.Either, Text, Data.Tuple, Data.List, Data.Either, Data.Functor
import iTasks._Framework.Util, iTasks._Framework.HtmlUtil, iTasks.UI.Definition
import iTasks.API.Core.Types, iTasks.API.Core.TaskCombinators

from Data.Map as DM import qualified put, get, del, newMap, toList, fromList, alter

from StdFunc import o, const, id, flip
from iTasks._Framework.TaskState import :: TIMeta(..), :: TaskTree(..), :: DeferredJSON

//This type records where parts were removed from a ui tree
//Left -> the entire branch was removed
//Right -> somewhere in the branch something was removed 
:: NodeMoves = NM [(Int, Either () NodeMoves)] 

//This type records the states of layouts applied somewhere in a ui tree
:: NodeLayoutStates = NL [(Int,Either JSONNode NodeLayoutStates)]

//This type represents the structure of a ui tree, which needs to be remembered when flattening a tree
:: NodeSpine = NS [NodeSpine]

derive JSONEncode NodeMoves, NodeLayoutStates, NodeSpine
derive JSONDecode NodeMoves, NodeLayoutStates, NodeSpine

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
		
changeNodeType :: (UI -> UI) -> Layout
changeNodeType f = layout
where
	layout (ReplaceUI def,_) = (ReplaceUI (f def),JSONNull) 
	layout (change,s) = (change,s) //Other changes than replacing are not affected

changeNodeAttributes :: (UIAttributes -> UIAttributes) -> Layout
changeNodeAttributes f = layout
where
	layout (ReplaceUI (UI type attr items),_) = (ReplaceUI (UI type (f attr) items),JSONNull)
	layout (change,s) = (change,s)

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
	insertSub idx def (ReplaceUI container,s) =  (ReplaceUI (setChildren (insertAt idx def (getChildren container)) container),s)
	insertSub idx _ (ChangeUI local childChanges,s) = (ChangeUI local (insert idx childChanges),s)
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
		# moves = if (change=:(ReplaceUI _)) (NM []) (fromMaybe (NM []) (fromJSON s)) //On a replace, we reset the state
		# startIdx = maybe 0 last mbDst
		//Remove based on the predicate
		# (endIdx,change,moves,inserts) = removeAndAdjust_ [] pred startIdx change moves
		//If there is a destination path, adjust the change for these moves
		= case mbDst of
			Just dst = (insertAndAdjust_ (init dst) startIdx (endIdx - startIdx) inserts change, toJSON moves)
			Nothing  = (change, toJSON moves)

removeAndAdjust_ :: NodePath (NodePath UI -> Bool) Int UIChange NodeMoves -> (!Int,!UIChange,!NodeMoves,![(Int,UIChildChange)])
removeAndAdjust_ path pred targetIdx NoChange moves //Only adjust the targetIdx by counting the moved nodes
	= (targetIdx + countMoves_ moves True, NoChange, moves, [])
removeAndAdjust_ path pred targetIdx (ReplaceUI ui) moves //If the node is replaced, adjust the new ui and determine changes to the previously moved nodes
	//Remove all previously moved nodes
	# removals = repeatn (countMoves_ moves True) (targetIdx,RemoveChild)
	//Determine new moves in the replacement ui
	= case collectNodes_ path pred targetIdx ui of
		//If the predicate matches the root node don't change anything.
		//It is impossible to create an adjusted ReplaceUI instruction if the root node is removed
		(_,Nothing,_,_)                      = (targetIdx, ReplaceUI ui, moves, [])
		(targetIdx, Just ui, moves, inserts) = (targetIdx, ReplaceUI ui, moves, removals ++  inserts )
//For ChangeUI instructions we need to adjust the changes to the child branches
removeAndAdjust_ path pred targetIdx (ChangeUI localChanges childChanges) (NM moves) 
	//Adjust all child changes adjust
	//Make sure all moves and child changes are sorted appropriately
	# childChanges = sortBy (\x y -> fst x < fst y) childChanges
	# moves = sortBy (\x y -> fst x < fst y) moves
	# (targetIdx,childChanges, moves,inserts) = adjustChildChanges targetIdx 0 childChanges moves
	= (targetIdx,ChangeUI localChanges childChanges,NM moves,inserts)
where
	//Go through all child changes
	adjustChildChanges tidx numRem [] moves = (tidx,[],moves,[])
	adjustChildChanges tidx numRem [c=:(idx,_):cs] moves
		# (tidx,numRem,movesBefore,mbMove,movesAfter)   = seekTargetIndex idx tidx numRem moves
		# (tidx,mbC,mbMove,movesAfter,cInserts)  		= adjustChildChange tidx numRem c mbMove movesAfter
		//If the move is still 'active', adjust the offset for later changes
		# numRem                                        = numRem + maybe 0 (\m -> if ((snd m) =: Left _) 1 0) mbMove 
		# (tidx,cs,movesAfter,csInserts)                = adjustChildChanges tidx numRem cs movesAfter
		= (tidx,maybe cs (\c -> [c:cs]) mbC,movesBefore ++ maybeToList mbMove ++ movesAfter, cInserts ++ csInserts)

	//When there are branches with moves before the branch we are looking at we 'seek' the target index forward
	//Basically we increment the target index for all the removed nodes in the branches for which we don't have changes
	seekTargetIndex idx tidx numRem moves 
		# (before,equal,after) = split idx moves
		= (tidx + countMoves_ (NM before) True, numRem + countMoves_ (NM before) False, before, listToMaybe equal, after)
	where
		split idx [] = ([],[],[])
		split idx [(i,m):ms]
			| i < idx  = ([(i,m):before],equal,after)
			| i == idx = (before,[(i,m):equal],after)
			           = (before,equal,[(i,m):after])
		where
			(before,equal,after) = split idx ms

	//Adjust an individual child change
	//Known precondition: moves only holds moves with an index >= the index of the change

	//Replacements
	adjustChildChange targetIdx numRem (idx,ChangeChild change=:(ReplaceUI ui)) mbMove movesAfter = case mbMove of
		(Just (_,Left _))
			| pred (path ++ [idx]) ui //The replacement still matches, just replace in the target locatation
				= (targetIdx + 1, Nothing, Just (idx,Left ()), movesAfter,[(targetIdx,ChangeChild change)])
			| otherwise //The removed node should no longer be removed -> change the replacement to an insert instruction
				# inserts = [(targetIdx,RemoveChild)]
				# (targetIdx,mbUI,subMoves,subInserts) = collectNodes_ (path ++ [idx]) pred targetIdx ui
				| subMoves =:(NM []) //Nothing matched, no need to record anything
					= (targetIdx, Just (idx - numRem,InsertChild ui), Nothing, movesAfter, inserts)
				| otherwise
					= (targetIdx, Just (idx - numRem,InsertChild (fromJust mbUI)), Just (idx,Right subMoves), movesAfter, inserts ++ subInserts)
		(Just (_,Right subMoves))
			# inserts = repeatn (countMoves_ subMoves True) (targetIdx,RemoveChild) //Remove instructions for the replaced nodes
			# (targetIdx,mbUI,subMoves,subInserts) = collectNodes_ (path ++ [idx]) pred targetIdx ui
			| mbUI =:(Nothing) //The replacement UI matched, record the move 
				= (targetIdx, Just (idx - numRem, RemoveChild), Just (idx,Left ()), movesAfter, inserts ++ subInserts)
			| subMoves =:(NM []) //Nothing matched, no need to record anything
				= (targetIdx, Just (idx - numRem, ChangeChild (ReplaceUI ui)), Nothing, movesAfter, inserts)
			| otherwise	 //One or more sub nodes matched, we need to record the moves for this branch
				= (targetIdx, Just (idx - numRem, ChangeChild (ReplaceUI (fromJust mbUI))), Just (idx,Right subMoves), movesAfter, inserts ++ subInserts)
		_ //Previously the child did not match
			# (targetIdx,mbUI,subMoves,subInserts) = collectNodes_ (path ++ [idx]) pred targetIdx ui
			| mbUI =:(Nothing) //The inserted UI matched, record the move and remove the child
				= (targetIdx, Just (idx - numRem, RemoveChild), Just (idx,Left ()), movesAfter, subInserts)
			| subMoves =:(NM []) //Nothing matched, no need to record anything
				= (targetIdx, Just (idx - numRem, ChangeChild (ReplaceUI ui)), Nothing, movesAfter, [])
			| otherwise	 //One or more sub nodes matched, we need to record the moves for this branch
				= (targetIdx, Just (idx - numRem, ChangeChild (ReplaceUI (fromJust mbUI))), Just (idx,Right subMoves), movesAfter, subInserts)

	//Other changes
	adjustChildChange targetIdx numRem (idx,ChangeChild change) mbMove movesAfter = case mbMove of
		(Just (_,Left _)) //Redirect the change
			= (targetIdx + 1, Nothing, Just (idx,Left ()), movesAfter, [(targetIdx,ChangeChild change)])
		(Just (_,Right subMoves)) //Recursively adjust the move itself
			# (targetIdx,change,subMoves,subInserts) = removeAndAdjust_ (path ++ [idx]) pred targetIdx change subMoves
			= (targetIdx, Just (idx - numRem, ChangeChild change), Just (idx,Right subMoves), movesAfter, subInserts)
		_ //Nothing to do
			= (targetIdx,Just (idx - numRem,ChangeChild change), Nothing, movesAfter, [])

	adjustChildChange targetIdx numRem (idx,RemoveChild) mbMove movesAfter = case mbMove of
		(Just (_,Left _)) //Already removed, generate remove instructions for the insert location
			= (targetIdx, Nothing, Nothing, [(i - 1, m) \\ (i,m) <- movesAfter], [(targetIdx,RemoveChild)])
		(Just (_,Right subMoves))
			= (targetIdx,Just (idx - numRem, RemoveChild), Nothing, [(i - 1, m) \\ (i,m) <- movesAfter], repeatn (countMoves_ subMoves True) (targetIdx,RemoveChild))
		_ //Only adjust the indices of moves
			= (targetIdx,Just (idx - numRem, RemoveChild), Nothing, [(i - 1, m) \\ (i,m) <- movesAfter], [])

	adjustChildChange targetIdx numRem (idx, InsertChild ui) mbMove movesAfter
		# (targetIdx,mbUI,subMoves,subInserts) = collectNodes_ (path ++ [idx]) pred targetIdx ui
		# movesAfter = [(i + 1, m) \\ (i,m) <- maybeToList mbMove ++ movesAfter]
		# change = fmap (\ui -> (idx - numRem,InsertChild ui)) mbUI
		| mbUI =:(Nothing) //The inserted UI matched, record the move 
			= (targetIdx, change, Just (idx,Left ()), movesAfter, subInserts)
		| subMoves =:(NM []) //Nothing matched, no need to record anything
			= (targetIdx, change, Nothing, movesAfter, subInserts)
		| otherwise	 //One ore more sub nodes matched, we need to record the moves for this branch
			= (targetIdx, change, Just (idx,Right subMoves), movesAfter, subInserts)

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
countMoves_ (NM moves) recursive = foldr (\(_,m) n -> n + either (const 1) (\r -> if recursive (countMoves_ r recursive) 0) m) 0 moves

//Collect parts of a UI and record their positions
collectNodes_ :: NodePath (NodePath UI -> Bool) Int UI -> (Int, Maybe UI, NodeMoves, [(Int,UIChildChange)])
collectNodes_ path pred idx ui=:(UI type attr items)
	| pred path ui	= (idx + 1, Nothing, NM [], [(idx,InsertChild ui)])
	| otherwise 
		# (idx, items, moves, inserts) = collectInItems idx 0 items
		= (idx, Just (UI type attr items), NM moves, inserts)
where
	collectInItems idx i [] = (idx,[],[],[])
	collectInItems idx i [item:items]
		# (idx, mbItem, itemMoves, itemInserts) = collectNodes_ (path ++ [i]) pred idx item
		# (idx, items, moves, inserts)          = collectInItems idx (i + 1) items
		= case mbItem of 
			Nothing   //The item itself was collected
				= (idx, items, [(i,Left ()):moves], itemInserts ++ inserts)
			Just item //Maybe modified
				| itemMoves =:(NM []) //If there are no moves in the branch, we don't need to add it
					= (idx, [item:items], moves, itemInserts ++ inserts)
				| otherwise	
					= (idx, [item:items], [(i,Right itemMoves):moves], itemInserts ++ inserts)

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
		= ((i,InsertChild ui),[(i,eitherState):[(i + 1,s) \\ (i,s) <- states]]) //Aslo adjust the indices of the other states
	layoutChildChange_ path pred layout (idx,RemoveChild) states
		= ((idx,RemoveChild),[(i - 1, s) \\ (i,s) <- states | i <> idx]) //Remove the current state from the states and adjust the indices accordingly

	selectState idx states = case splitWith (((==) idx) o fst) states of
        ([(_,s):_],states) = (Just s,states)
        _                  = (Nothing,states)

copyAttributes :: NodePath NodePath -> Layout
copyAttributes src dst = layout //TODO: Also handle attribute updates in the src location, and partial replacements along the path
where
	layout (ReplaceUI ui,s) = case  selectAttr src ui of 
		Just attr = (ReplaceUI (addAttr attr dst ui),s)
		Nothing   = (ReplaceUI ui,s)
	layout (change,s) = (change,s)

	selectAttr [] (UI type attr items) = Just attr
	selectAttr [s:ss] (UI type attr items) 
		| s < length items  = selectAttr ss (items !! s)
							= Nothing

	addAttr extra [] (UI type attr items) = UI type (foldl (\m (k,v) -> 'DM'.put k v m) attr ('DM'.toList extra)) items
	addAttr extra [s:ss] (UI type attr items) 
		| s < length items = UI type attr (updateAt s (addAttr extra ss (items !! s)) items) 
						   = UI type attr items

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


//UTIL
getChildren :: UI -> [UI]
getChildren (UI type attr children) = children

setChildren :: [UI] UI -> UI
setChildren children (UI type attr _) = UI type attr children

// ######################### OBSOLETE ##################################
// ######################### OBSOLETE ##################################
// ######################### OBSOLETE ##################################
// ######################### OBSOLETE ##################################

/*
autoLayoutStep :: Layout ()
autoLayoutStep = layout 
where
	layout (UICompoundContent [UIEmpty:stepActions]) //Remove the empty element
		= UICompoundContent stepActions
	layout (UICompoundContent [UIForm stack=:{UIForm|attributes,controls,size}:stepActions])
		//Recognize special case of a complete empty interaction wrapped in a step as an actionset
		| isEmpty controls
			= UICompoundContent stepActions
    	//Promote to abstract container
		# stepActions = [a \\ UIAction a <- stepActions]
       	# (triggers,stepActions) = extractTriggers stepActions
        = addTriggersToUIDef triggers (UIBlock {UIBlock|autoLayoutForm stack & actions = stepActions ,size=size})
	layout (UICompoundContent [UIBlock sub=:{UIBlock|actions=[]}:stepActions])
		//If an abstract container without actions is placed under a step container, we add the actions
		# stepActions = [a \\ UIAction a <- stepActions]
    	# (triggers,stepActions) = extractTriggers stepActions
		= addTriggersToUIDef triggers (UIBlock {UIBlock|sub & actions = stepActions})
	layout (UICompoundContent [UIBlock sub=:{UIBlock|actions}:stepActions])
		= UIBlock sub
	layout (UICompoundContent [UIBlocks blocks origActions:stepActions])
		# stepActions = [a \\ UIAction a <- stepActions]
    	# (triggers,actions) = extractTriggers (origActions ++ stepActions)
		= addTriggersToUIDef triggers (UIBlock (autoLayoutBlocks blocks actions))
	layout (UICompoundContent [(UILayers [main:aux]):stepActions])
		# main = layout (UICompoundContent [main:stepActions])
		= UILayers [main:aux]
	layout def = def
*/
/*
autoLayoutParallel :: Layout ()
autoLayoutParallel = layout
where
	layout (change,s) = (change,s)
	layout (UICompoundContent [UICompoundContent defs,UICompoundContent parActions])
		# parActions = [a \\ UIAction a <- parActions]
    	# (triggers,parActions)  = extractTriggers parActions
		# (defs,layers) 		 = foldr collectLayers ([],[]) defs
    	# def = case defs of
        	[UIForm form]
            	# block = autoLayoutForm form
            	= addTriggersToUIDef triggers (UIBlock {UIBlock|block & actions = block.UIBlock.actions ++ parActions})
        	[UIBlock block]
            	= addTriggersToUIDef triggers (UIBlock {UIBlock|block & actions = block.UIBlock.actions ++ parActions})
        	[def=:(UIFinal _)]
            	= addTriggersToUIDef triggers def
        	_
            	| allForms defs
                	# form = (foldl mergeForms {UIForm|attributes='DM'.newMap,controls=[],size=defaultSizeOpts} defs)
                	| isEmpty parActions
                    	= UIForm form
                	# block = autoLayoutForm form
                	= addTriggersToUIDef triggers (UIBlock {UIBlock|block & actions = block.UIBlock.actions ++ parActions})
            	| otherwise
                	# (blocks,actions) = foldr collectBlocks ([],[]) defs
                	# def = case blocks of
                    	[block] = UIBlock {UIBlock|block & actions = block.UIBlock.actions ++ actions ++ parActions} //Not always a good idea :(
                    	_       = UIBlocks blocks (actions ++ parActions)
                	= addTriggersToUIDef triggers def
		= case layers of
	 		[] 	= def
			_ 	= UILayers [def:layers]
	where
    	oneDef [d]  = True
    	oneDef _    = False

    	allForms []             = True
    	allForms [UIForm _:fs]  = allForms fs
    	allForms _              = False

    	mergeForms form1 (UIForm form2)
        	= {UIForm
          		|attributes = mergeAttributes form1.UIForm.attributes form2.UIForm.attributes
          		,controls = form1.UIForm.controls ++ form2.UIForm.controls
          		,size = {UISizeOpts|form1.UIForm.size
                  & width = maybe form1.UIForm.size.UISizeOpts.width Just form2.UIForm.size.UISizeOpts.width
                  , height = maybe form1.UIForm.size.UISizeOpts.height Just form2.UIForm.size.UISizeOpts.height
                  }
          	  }

    	collectBlocks (UIForm form) (blocks,actions)
        	= ([autoLayoutForm form:blocks],actions)
    	collectBlocks (UIBlock block) (blocks,actions)
        	= ([block:blocks],actions)
    	collectBlocks (UIBlocks blocks2 actions2) (blocks1,actions1)
        	= (blocks2 ++ blocks1,actions2 ++ actions1)
    	collectBlocks _ (blocks,actions)
        	= (blocks,actions)

		collectLayers (UILayers [main:aux]) (defs,layers) = ([main:defs], aux ++layers)
		collectLayers d (defs,layers) 					= ([d:defs],layers)
*/
/**
* Overrule the title attribute with the title in the task meta data
*/
/*
autoLayoutSession :: Layout Arrangement
autoLayoutSession = arrangeLayout autoArrange
where
	layout (change,s) = (compactChangeDef change,s)
	layout (UILayers [main:aux])
		= UILayers [layout main:aux]
	layout UIEmpty
		= UIFinal (defaultPanel [])
	layout (UIForm stack)
    	= layout (UIBlock (autoLayoutForm stack))
	layout (UIBlock subui=:{UIBlock|attributes,content,actions,hotkeys,size})
    	# fullScreen = ('DM'.get SCREEN_ATTRIBUTE attributes === Just "full") || isNothing ('DM'.get "session" attributes)
    	# (panel,attributes,actions,panelkeys) = blockToPanel (if fullScreen {UIBlock|subui & attributes = 'DM'.del TITLE_ATTRIBUTE attributes} subui)
    	# panel = if fullScreen (setSize FlexSize FlexSize panel) ((setSize WrapSize WrapSize o setFramed True) panel)
		# (menu,menukeys,actions)	= actionsToMenus actions
		# items				        = [panel]
		# itemsOpts			        = {defaultItemsOpts items & direction = Vertical, halign = AlignCenter, valign= AlignMiddle}
		# hotkeys			        = case panelkeys ++ menukeys of [] = Nothing ; keys = Just keys
		= UIFinal (UIPanel defaultSizeOpts itemsOpts {UIPanelOpts|title = 'DM'.get TITLE_ATTRIBUTE attributes /*, menu = if (isEmpty menu) Nothing (Just menu) */, hotkeys = hotkeys,iconCls=Nothing,frame=False})
	layout (UIBlocks blocks actions)
    	= layout (UIBlock (autoLayoutBlocks blocks actions))
	layout (UIFinal control) = UIFinal control
	layout def = def
*/
/*
autoLayoutForm :: UIForm -> UIBlock
//Special case for choices
autoLayoutForm {UIForm|attributes,controls=[(c=:UIListChoice _ _ ,_)],size}
    = {UIBlock|attributes=attributes,content={UIItemsOpts|defaultItemsOpts ([fill c]) & direction=Vertical},actions=[],hotkeys=[],size=size}
autoLayoutForm {UIForm|attributes,controls=[(c=:UITree _ _ _ ,_)],size}
    = {UIBlock|attributes=attributes,content={UIItemsOpts|defaultItemsOpts ([fill c]) & direction=Vertical},actions=[],hotkeys=[],size=size}
autoLayoutForm {UIForm|attributes,controls=[(c=:UIGrid _ _ _ ,_)],size}
    = {UIBlock|attributes=attributes,content={UIItemsOpts|defaultItemsOpts ([fill c]) & direction=Vertical},actions=[],hotkeys=[],size=size}
//General case
autoLayoutForm {UIForm|attributes,controls,size}
	= {UIBlock|attributes=attributes,content={UIItemsOpts|defaultItemsOpts (decorateControls controls) & direction=Vertical},actions=[],hotkeys=[],size=size}
*/




mapLst :: (Bool a -> b) [a] -> [b]
mapLst f [] = []
mapLst f [x] = [f True x]
mapLst f [x:xs] = [f False x: mapLst f xs]
/*
//Add labels and icons to a set of controls if they have any of those attributes set
decorateControls :: [(UIControl,UIAttributes)] -> [UIControl]
decorateControls  controls = mapLst decorateControl controls
where
	mapLst f [] = []
	mapLst f [x] = [f True x]
	mapLst f [x:xs] = [f False x: mapLst f xs]
*/
/*
decorateControl :: Bool (!UIControl,!UIAttributes) -> UIControl
decorateControl last (control,attributes)
	# mbLabel 		= 'DM'.get LABEL_ATTRIBUTE attributes
	# mbPrefix 	 	= 'DM'.get PREFIX_ATTRIBUTE attributes
	# mbPostfix 	= 'DM'.get POSTFIX_ATTRIBUTE attributes
	# mbHint 		= 'DM'.get HINT_ATTRIBUTE attributes
	# mbHintType 	= 'DM'.get HINT_TYPE_ATTRIBUTE attributes
	# hasMargin		= hasMargin control
	# noMargins		= noMarginControl control
	= case (mbLabel,mbPrefix,mbPostfix,mbHint,mbHintType) of
		(Nothing,Nothing,Nothing,Nothing,Nothing)	//Just set margins
			| hasMargin	= control
						= if noMargins
							(setMargins 0 0 0 0 control)
							(if last (setMargins 5 5 5 5 control) (setMargins 5 5 0 5 control))

		_									//Add decoration													
			# control = row (labelCtrl mbLabel ++ prefixCtrl mbPrefix ++ [control] ++ postfixCtrl mbPostfix ++ iconCtrl control mbHint mbHintType)
			= if noMargins
				(setMargins 0 0 0 0 control)
				(if last (setMargins 5 5 5 5 control) (setMargins 5 5 0 5 control))
where
	row ctrls				= (setSize FlexSize WrapSize o setDirection Horizontal) (defaultContainer ctrls)
	
	labelCtrl (Just label)	= [setWidth (ExactSize 100) (stringDisplay label)]
	labelCtrl Nothing		= []

	postfixCtrl (Just postfix)	= [setLeftMargin 5 (setWidth (ExactSize 30) (stringDisplay postfix))]
	postfixCtrl Nothing		    = []

	prefixCtrl (Just prefix)	= [setRightMargin 5 (setWidth (ExactSize 30) (stringDisplay prefix))]
	prefixCtrl Nothing		    = []
	
    iconCtrl (UIEditCheckbox _ _) _ _ = []
	iconCtrl _ (Just msg) (Just "info") 	= icon "icon-hint" msg
	iconCtrl _ (Just msg) (Just "valid") 	= icon "icon-valid" msg
	iconCtrl _ (Just msg) (Just "warning")  = icon "icon-warning" msg
	iconCtrl _ (Just msg) (Just "invalid")	= icon "icon-invalid" msg
	iconCtrl _ _ _ 							= []
	
	icon cls tooltip		= [setLeftMargin 5 (UIIcon defaultFSizeOpts {UIIconOpts|iconCls = cls, tooltip = Just tooltip})]

	hasMargin control = isJust (getMargins control)

	//noMarginControl	(UIPanel _ _ _)			= True
	noMarginControl	(UIGrid _ _ _)			= True
	noMarginControl	(UITree _ _ _)			= True
	noMarginControl _						= False
*/
/*
autoLayoutBlocks :: [UIBlock] [UIAction] -> UIBlock
autoLayoutBlocks blocks actions = arrangeVertical blocks actions
*/


/*
arrangeStacked :: UIDirection [UIBlock] [UIAction] -> UIBlock
arrangeStacked direction blocks actions
    = foldl append {UIBlock|attributes='DM'.newMap,content={UIItemsOpts|defaultItemsOpts [] & direction=direction},hotkeys=[],size=defaultSizeOpts} blocks
where
    append ui1 ui2
        # (control,attributes,actions,hotkeys) = blockToControl ui2
        = {UIBlock|ui1 & content = {UIItemsOpts|ui1.UIBlock.content & items = ui1.UIBlock.content.UIItemsOpts.items ++ [control]}
                       , hotkeys = ui1.UIBlock.hotkeys ++ hotkeys
                       , attributes = mergeAttributes ui1.UIBlock.attributes attributes
                       }
*/


/*
arrangeWithSideBar :: !Int !UISide !Int !Bool -> UIBlocksCombinator
arrangeWithSideBar index side size resize = arrange
where
    arrange [] actions = autoLayoutBlocks [] actions
    arrange blocks actions
        | index >= length blocks = autoLayoutBlocks blocks actions
        # sidePart = blocks !! index
        # restPart = case removeAt index blocks of
            [ui] = ui
            uis  = autoLayoutBlocks uis []
        # (sideC,sideAt,sideAc,sideHK) = blockToControl sidePart
        # (restC,restAt,restAc,restHK) = blockToControl restPart
        # sideC = if (side === TopSide|| side === BottomSide) (setSize FlexSize (ExactSize size) sideC) (setSize (ExactSize size) FlexSize sideC)
        # restC = fill restC
        = {UIBlock|attributes=mergeAttributes restAt sideAt
                  ,content= {UIItemsOpts|defaultItemsOpts (if (side===TopSide || side === LeftSide) (if resize [sideC,UISplitter,restC] [sideC,restC]) (if resize [restC,UISplitter,sideC] [restC,sideC]))
                            &direction = if (side===TopSide || side === BottomSide) Vertical Horizontal
                            }
                  ,hotkeys = restHK ++ sideHK
                  ,size = defaultSizeOpts
                  }
*/
/*
instance tune ArrangeSplit
where
    tune (ArrangeSplit direction resize) t = tune (ApplyLayout layout) t
	where
		layout (ReplaceUI ui,()) = (ReplaceUI (arrangeBlocks (arrangeSplit direction resize) ui),())
		layout (change,s) = (change,s)
*/
/*
arrangeSplit :: !UIDirection !Bool -> UIBlocksCombinator
arrangeSplit direction resize = arrange
where
    arrange [] actions = autoLayoutBlocks [] actions
    arrange blocks actions
        # (bcontrols,_,bactions,bhotkeys) = unzip4 (map blockToPanel blocks)
        # controls = map fill bcontrols
        # controls = if resize (intersperse UISplitter controls) controls
        = {UIBlock|attributes='DM'.newMap
                  ,content = {UIItemsOpts|defaultItemsOpts controls & direction = direction}
                  //,actions = actions ++ flatten bactions
                  ,hotkeys = flatten bhotkeys
                  ,size = defaultSizeOpts
                  }
*/
/*
instance tune ArrangeCustom
where
    tune (ArrangeCustom f) t = tune (ApplyLayout layout) t
	where
		layout (ReplaceUI ui,()) = (ReplaceUI (arrangeBlocks f ui),())
		layout (change,s) = (change,s)
*/
/*
blockToControl :: UIBlock -> (UIControl,UIAttributes,[UIAction],[UIKeyAction])
blockToControl ui=:{UIBlock|attributes}
    = case ('DM'.get CONTAINER_ATTRIBUTE attributes) of
		(Just "panel")		= blockToPanel ui
		(Just "container")	= blockToContainer ui
        _                   = if (isNothing ('DM'.get TITLE_ATTRIBUTE attributes)) (blockToContainer ui) (blockToPanel ui)
*/
/*
blockToContainer :: UIBlock -> (UIControl,UIAttributes,[UIAction],[UIKeyAction])
blockToContainer {UIBlock|content=content=:{UIItemsOpts|items,direction},attributes,size}
	# actions = []
    //Add button actions
	# (buttons,hotkeys,actions)	= actionsToButtons actions	
	# (items,direction)		    = addButtonPanel attributes direction buttons items
    = (UIContainer sizeOpts {UIItemsOpts|content & items=items,direction=direction},attributes,actions,hotkeys)
where
	sizeOpts		= {UISizeOpts|size & width = Just FlexSize}
*/
/*
blockToPanel :: UIBlock -> (UIControl,UIAttributes,[UIAction],[UIKeyAction])
blockToPanel {UIBlock|content=content=:{UIItemsOpts|items,direction},attributes,size}
	# actions = []
    //Add button actions
	# (buttons,hotkeys,actions)	= actionsToButtons actions	
	# (items,direction)		    = addButtonPanel attributes direction buttons items
    = (UIContainer sizeOpts {UIItemsOpts|content & items=items,direction=direction} /*panelOpts*/,attributes`,actions,hotkeys)
where
	sizeOpts	= {UISizeOpts|size & width = Just FlexSize}
	panelOpts	= {UIPanelOpts|title = title,frame = False, hotkeys = Nothing, iconCls = iconCls}
	title		= 'DM'.get TITLE_ATTRIBUTE attributes	
	iconCls		= fmap (\icon -> "icon-" +++ icon) ('DM'.get ICON_ATTRIBUTE attributes)
    attributes` = ('DM'.del TITLE_ATTRIBUTE o 'DM'.del ICON_ATTRIBUTE) attributes
*/
/*
blockToTab :: UIBlock -> UITab
blockToTab {UIBlock|content=content=:{UIItemsOpts|items,direction},attributes,size}
	# actions = []
    //Check for tab close action
	# (close,actions)		        = actionsToCloseId actions
    //Add button actions
	# (buttons,buttonkeys,actions)	= actionsToButtons actions	
	# (items,direction)	    	    = addButtonPanel attributes direction buttons items
    //Add menu actions
	# (menus,menukeys,actions)	    = actionsToMenus actions
    = (UITab {UIItemsOpts|content&items=items,direction=direction} (tabOpts (buttonkeys++menukeys) menus close))
where
	tabOpts hotkeys menus close
        = {UITabOpts|title = title, menu = if (isEmpty menus) Nothing (Just menus)
          , hotkeys = if (isEmpty hotkeys) Nothing (Just hotkeys), focusTaskId = taskId, closeTaskId = close,iconCls=iconCls}

	taskId		= 'DM'.get TASK_ATTRIBUTE attributes
	title       = fromMaybe "Untitled" ('DM'.get TITLE_ATTRIBUTE attributes)
    iconCls     = fmap (\i -> "icon-" +++ i) ('DM'.get ICON_ATTRIBUTE attributes)
*/

/*
blockToWindow :: UIWindowType UIVAlign UIHAlign UIBlock -> UIWindow
blockToWindow windowType vpos hpos {UIBlock|content=content=:{UIItemsOpts|items,direction},attributes,size}
	# actions = []
    //Check for window close action
	# (close,actions)		        = actionsToCloseId actions
    //Add button actions
	# (buttons,buttonkeys,actions)	= actionsToButtons actions	
	# (items,direction)	    	    = addButtonPanel attributes direction buttons items
    //Add menu actions
	# (menus,menukeys,actions)	    = actionsToMenus actions
    = {UIWindow|sizeOpts=sizeOpts,itemsOpts={UIItemsOpts|content&items=items,direction=direction},windowOpts=windowOpts (buttonkeys++menukeys) menus close}
where
	sizeOpts	= {UISizeOpts|size & width = Just (fromMaybe WrapSize size.UISizeOpts.width), height = Just (fromMaybe WrapSize size.UISizeOpts.height)}
	windowOpts hotkeys menus close
        = {UIWindowOpts|windowType = windowType, title = title, closeTaskId = close, focusTaskId = Nothing
                      ,vpos = Just vpos, hpos = Just hpos, iconCls = iconCls}
	
    title		= 'DM'.get TITLE_ATTRIBUTE attributes	
    iconCls		= fmap (\icon -> "icon-" +++ icon) ('DM'.get ICON_ATTRIBUTE attributes)
*/

//Adds a button panel to a set of controls
//(not the prettiest code)
/*
addButtonPanel :: UIAttributes UIDirection [UIControl] [UIControl] -> (![UIControl],!UIDirection)
addButtonPanel attr direction [] items = (items,direction)
addButtonPanel attr direction buttons items
	= case ('DM'.get "buttonPosition" attr,direction) of
		(Nothing,Vertical)			= (items ++ [fillWidth (buttonPanel buttons)],Vertical)
		(Nothing,Horizontal)		= ([setDirection Horizontal (defaultContainer items),fillWidth (buttonPanel buttons)],Vertical)
		(Just "left",Vertical)		= ([wrapWidth (buttonPanel buttons),setDirection Vertical (defaultContainer items)],Horizontal)
		(Just "left",Horizontal)	= ([wrapWidth (buttonPanel buttons):items],Horizontal)
		(Just "right",Vertical)		= ([setDirection Vertical (defaultContainer items),wrapWidth (buttonPanel buttons)],Horizontal)
		(Just "right",Horizontal)	= (items ++ [wrapWidth (buttonPanel buttons)],Horizontal)	
		(Just "top",Vertical)		= ([fillWidth (buttonPanel buttons):items],Vertical)
		(Just "top",Horizontal)		= ([fillWidth (buttonPanel buttons),setDirection Horizontal (defaultContainer items)],Vertical)
		(Just "bottom",Vertical)	= (items ++ [fillWidth (buttonPanel buttons)],Vertical)
		(Just "bottom",Horizontal)	= ([setDirection Horizontal (defaultContainer items),fillWidth (buttonPanel buttons)],Vertical)
*/
//addTriggersToUIDef :: [(Trigger,String,String)] UI -> UI
//addTriggersToUIDef triggers (UIForm stack=:{UIForm|controls})
//    = UIForm {UIForm|stack & controls = [(addTriggersToControl triggers c,m)\\ (c,m) <- controls]}
//addTriggersToUIDef triggers (UIBlock subui)
    //= UIBlock (addTriggersToBlock triggers subui)
//addTriggersToUIDef triggers (UIBlocks blocks actions)
 //   = UIBlocks (map (addTriggersToBlock triggers) blocks) []
//addTriggersToUIDef triggers def = def


//addTriggersToControl :: [(Trigger,String,String)] UIControl -> UIControl
//Recursive cases
//addTriggersToControl triggers (UIContainer sOpts iOpts=:{UIItemsOpts|items})
 //   = UIContainer sOpts {UIItemsOpts|iOpts & items = map (addTriggersToControl triggers) items}
//addTriggersToControl triggers (UIPanel sOpts iOpts=:{UIItemsOpts|items} opts)
    //= UIPanel sOpts {UIItemsOpts|iOpts & items = map (addTriggersToControl triggers) items} opts
//addTriggersToControl triggers (UITabSet sOpts tOpts=:{UITabSetOpts|items})
    //= UITabSet sOpts {UITabSetOpts|tOpts & items = map (addTriggersToTab triggers) items}
//Single controls
//addTriggersToControl triggers control = foldr addTriggerToControl control triggers

//addTriggerToControl :: (Trigger,String,String) UIControl -> UIControl
//addTriggerToControl (DoubleClick,taskId,actionId) (UIGrid sOpts cOpts opts) = UIGrid sOpts cOpts {UIGridOpts|opts & doubleClickAction = Just (taskId,actionId)}
//addTriggerToControl (DoubleClick,taskId,actionId) (UITree sOpts cOpts opts) = UITree sOpts cOpts {UITreeOpts|opts & doubleClickAction = Just (taskId,actionId)}
//addTriggerToControl t c = c

//GUI combinators						
hjoin :: ![UI] -> UI
hjoin items = (setDirection Horizontal o setHalign AlignLeft o setValign AlignMiddle)
	          (uic UIContainer items)

vjoin :: ![UI] -> UI
vjoin items = (setDirection Vertical o setHalign AlignLeft o setValign AlignTop)
              (uic UIContainer items)
						
//Container operations
/*
addItemToUI :: (Maybe Int) UIControl UIControl -> UIControl
addItemToUI mbIndex item ctrl = case ctrl of
//	UIContainer sOpts iOpts=:{UIItemsOpts|items}    = UIContainer sOpts {UIItemsOpts|iOpts & items = add mbIndex item items}
//	UIPanel sOpts iOpts=:{UIItemsOpts|items} opts	= UIPanel sOpts {UIItemsOpts|iOpts & items = add mbIndex item items} opts
	_												= ctrl
where
	add Nothing item items		= items ++ [item]
	add (Just pos) item items	= take pos items ++ [item] ++ drop pos items
*/	


//getItemsOfUI :: UIControl -> [UIControl]
//getItemsOfUI (UIContainer _ {UIItemsOpts|items})	= items
//getItemsOfUI (UIPanel _ {UIItemsOpts|items} _)		= items
//getItemsOfUI ctrl									= [ctrl]
	
//setItemsOfUI :: [UIControl] UIControl -> UIControl
//setItemsOfUI items (UIContainer sOpts iOpts)	    = UIContainer sOpts {UIItemsOpts|iOpts & items = items}
//setItemsOfUI items (UIPanel sOpts iOpts opts)		= UIPanel sOpts {UIItemsOpts|iOpts & items = items} opts
//setItemsOfUI items ctrl								= ctrl

//Container for a set of horizontally layed out buttons
//buttonPanel	:: ![UIControl] -> UIControl	
//buttonPanel buttons
//	= (wrapHeight o setPadding 2 2 2 0 o setDirection Horizontal o setHalign AlignRight) (setBaseCls "buttonbar" (defaultContainer buttons))

/*
actionsToButtons :: ![UIAction] -> (![UIControl],![UIKeyAction],![UIAction])
actionsToButtons [] = ([],[],[])
actionsToButtons [a=:{taskId,action=action=:(Action name _),enabled}:as]
	# (buttons,hotkeys,actions)	= actionsToButtons as 
	= case split "/" name of
		//Action name consist of only one part -> make a button
		[name]
			= ([mkButton taskId action enabled:buttons],maybe hotkeys (\h -> [h:hotkeys]) (actionToHotkey a), actions)
		//Action name is "/" -> also make a button or we get a weird menu
		["",""]
			= ([mkButton taskId action enabled:buttons],maybe hotkeys (\h -> [h:hotkeys]) (actionToHotkey a), actions)
		//Action name consists of multiple parts -> pass through
		_		= (buttons,hotkeys,[a:actions])
where
	mkButton taskId action=:(Action actionId _) enabled
		= UIActionButton defaultSizeOpts {UIActionOpts|taskId = toString taskId,actionId=actionId}
			{UIButtonOpts|text = Just (actionName action), iconCls = (actionIcon action), disabled = not enabled}
*/			
/*
actionsToMenus :: ![UIAction] -> (![UIControl],![UIKeyAction],![UIAction])
actionsToMenus actions
	# (menus,hotkeys,actions) = makeMenus [] [] actions
	= (sortBy menuOrder menus, hotkeys, actions)
where
	makeMenus :: [UIControl] [UIKeyAction] [UIAction] -> ([UIControl],[UIKeyAction],[UIAction])
	makeMenus menus hotkeys []	= (menus,hotkeys,[])	
	makeMenus menus hotkeys [a=:{taskId,action,enabled}:as] = makeMenus (addToMenus (path action) taskId action enabled menus) (addToHotkeys taskId action enabled hotkeys) as

	path action = case (split "/" (actionName action)) of
		["":p]	= p
		p		= p
		
	addToMenus [main:item] taskId action enabled [] //Create menu
		= [createButton main item taskId action enabled]
	addToMenus [main:item] taskId action enabled [m=:(UIMenuButton sOpts opts):ms] //Add to existing menu if it exists
		| opts.UIMenuButtonOpts.text == Just main //Found!
			= [UIMenuButton sOpts {UIMenuButtonOpts|opts & menu = addToItems item taskId action enabled opts.UIMenuButtonOpts.menu}:ms]
		| otherwise
			= [m:addToMenus [main:item] taskId action enabled ms]
	addToMenus [main:item] taskId action enabled [m:ms]
		= [m:addToMenus [main:item] taskId action enabled ms]
	addToMenus _ taskId action enabled menus
		= menus
			
	addToItems [item:sub] taskId action enabled [] //Create item
		= [createItem item sub taskId action enabled]
	addToItems [item:sub] taskId action enabled [i:is]
		| itemText i == item
			| isEmpty sub	//Duplicate item (just add it)
				= [i,createItem item sub taskId action enabled:is]
			| otherwise		//Add to the found item
				= [addToItem sub taskId action enabled i:is]
		| otherwise
			= [i:addToItems [item:sub] taskId action enabled is]
	addToItems [] _ _ _ _
		= []

	itemText (UIActionMenuItem _ {UIButtonOpts|text})	= fromMaybe "" text
	itemText (UISubMenuItem {UIMenuButtonOpts|text})	= fromMaybe "" text
	itemText _					= ""
	
	createButton item [] taskId action enabled
		= UIActionButton defaultSizeOpts
			{UIActionOpts|taskId=taskId,actionId=actionName action}
			{UIButtonOpts|text=Just item,iconCls = actionIcon action, disabled = not enabled}
	createButton item sub taskId action enabled
		= UIMenuButton defaultSizeOpts
			{UIMenuButtonOpts
			|text = Just item
			,iconCls = actionIcon action //Just (icon item)
			,disabled	= if (isEmpty sub) (not enabled) False
			,menu = addToItems sub taskId action enabled []
			}
	createItem item [] taskId action enabled //Action item
		= UIActionMenuItem
			{UIActionOpts|taskId=taskId,actionId=actionName action}
			{UIButtonOpts|text=Just item,iconCls = actionIcon action, disabled = not enabled}
	createItem item sub taskId action enabled //Sub item
		= UISubMenuItem
				{ UIMenuButtonOpts
                | text = Just item
				, iconCls = actionIcon action
				, disabled = False
				, menu = addToItems sub taskId action enabled []
				}
		
	addToItem sub taskId action enabled item=:(UISubMenuItem opts=:{UIMenuButtonOpts|menu})
		= UISubMenuItem {UIMenuButtonOpts|opts & menu = addToItems sub taskId action enabled menu}

	addToHotkeys taskId action enabled hotkeys = case actionToHotkey {taskId=taskId,action=action,enabled=enabled} of
		Just hotkey = hotkeys ++ [hotkey]
		Nothing		= hotkeys
		
	menuOrder (UIMenuButton _ {UIMenuButtonOpts|text=Just m1}) (UIMenuButton _ {UIMenuButtonOpts|text=Just m2}) = m1 < m2
	menuOrder m1 m2 = False
*/
//Extract triggers from a list of actions
/*
extractTriggers :: ![UIAction] -> ([(Trigger,String,String)], [UIAction])
extractTriggers [] = ([],[])
extractTriggers [a=:{taskId,action=(Action name options)}:as]
    # (ts,as) = extractTriggers as
    = case [ t \\ ActionTrigger t <- options] of
        []          = (ts,[a:as])
        triggers    = ([(t,taskId,name) \\ t <- triggers] ++ ts, [{a & action= Action name (filter (not o isTrigger) options)}:as])
where
    isTrigger (ActionTrigger _) = True
    isTrigger _                 = False
*/
/*
actionsToCloseId :: ![UIAction] -> (!Maybe String, ![UIAction])
actionsToCloseId [] = (Nothing,[])
actionsToCloseId [{taskId,action=ActionClose,enabled}:as] = (if enabled (Just taskId) Nothing,as)

actionsToCloseId [a:as] = let (mbtask,as`) = actionsToCloseId as in (mbtask,[a:as`])
*/
/*
actionToHotkey :: UIAction -> Maybe UIKeyAction
actionToHotkey {taskId,action=Action actionId options,enabled=True}
	= case [key \\ ActionKey key <- options] of
		[key:_] = Just (key,{taskId=taskId,actionId=actionId})
		_		= Nothing
actionToHotkey _ = Nothing
*/

hasWindowContainerAttr :: UIAttributes -> Bool
hasWindowContainerAttr attributes = maybe False ((===) (JSONString "window")) ('DM'.get CONTAINER_ATTRIBUTE attributes)

hasPanelContainerAttr :: UIAttributes -> Bool
hasPanelContainerAttr attributes = maybe False ((===) (JSONString "panel")) ('DM'.get CONTAINER_ATTRIBUTE attributes)

hasContainerContainerAttr :: UIAttributes -> Bool
hasContainerContainerAttr attributes = maybe False ((===) (JSONString "container")) ('DM'.get CONTAINER_ATTRIBUTE attributes)

hasContainerAttr :: UIAttributes -> Bool
hasContainerAttr attributes = isJust ('DM'.get CONTAINER_ATTRIBUTE attributes) 

/*
singleControl :: UIDef -> Bool
singleControl  def = case uiDefControls def of
	[_]	= True
	_	= False
*/

mergeAttributes :: UIAttributes UIAttributes -> UIAttributes
mergeAttributes attr1 attr2
    = foldl setIfNotSet attr1 ('DM'.toList attr2)
where
    setIfNotSet attr (k,v)
        = maybe ('DM'.put k v attr) (const attr) ('DM'.get k attr)

//tweakUI :: (UIControl -> UIControl) UI -> UI
//tweakUI f (UIForm stack=:{UIForm|controls})
//	= UIForm {UIForm|stack & controls = [(f c,a) \\ (c,a) <- controls]}
//tweakUI f (UIBlock sub=:{UIBlock|content=content=:{UIItemsOpts|items}})
	//= UIBlock {UIBlock|sub & content = {UIItemsOpts|content & items = map f items}}
//tweakUI f (UI (UIControl control) attr items)
//    = UI (UIControl (f control)) attr items
//tweakUI f def = def

tweakAttr :: (UIAttributes -> UIAttributes) UI -> UI
//tweakAttr f (UIForm stack=:{UIForm|attributes})
//	= UIForm {UIForm|stack & attributes = f attributes}
//tweakAttr f (UIBlock sub=:{UIBlock|attributes})
//	= UIBlock {UIBlock|sub & attributes = f attributes}
tweakAttr f def = def

//tweakControls :: ([(UIControl,UIAttributes)] -> [(UIControl,UIAttributes)]) UI -> UI
//tweakControls f (UIForm stack=:{UIForm|controls})
//	= UIForm {UIForm|stack & controls = f controls}
//tweakControls f (UIBlock sub=:{UIBlock|content=content=:{UIItemsOpts|items}})
	//= UIBlock {UIBlock|sub & content = {UIItemsOpts|content & items = map fst (f [(c,'DM'.newMap) \\ c <- items])}}
//tweakControls f (UI (UIControl control) attr items)
//	= case f [(control,'DM'.newMap)] of
//		[(control,_):_] = UI (UIControl control) attr items
//		_ 				= UI (UIControl control) attr items
//tweakControls f def	= def

