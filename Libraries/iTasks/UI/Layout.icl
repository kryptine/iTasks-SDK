implementation module iTasks.UI.Layout

import StdTuple, StdList, StdBool, StdInt, StdOrdList, StdArray, StdMisc, StdString
import Data.Generics.GenLexOrd
import Data.Maybe, Data.Either, Text, Data.Tuple, Data.List, Data.Either, Data.Functor
import iTasks.Internal.Util, iTasks.Internal.HtmlUtil, iTasks.UI.Definition
import iTasks.Internal.Generic.Defaults 
import StdEnum
from Data.Map as DM import qualified put, get, del, newMap, toList, fromList, delList, alter, union, keys, unions, singleton, member
from Data.Set as DS import qualified newSet, insert, delete, toList, fromList
from Data.Tuple import appSnd

import Text.JSON

from StdFunc import o, const, id, flip
from iTasks.Internal.TaskState import :: TIMeta(..), :: TaskTree(..), :: DeferredJSON
from iTasks.Internal.TaskEval import :: TaskTime
from iTasks.WF.Combinators.Core import :: AttachmentStatus
import iTasks.WF.Definition
import Data.Generics.GenEq

//This type records the states of layouts applied somewhere in a ui tree
derive JSONEncode LayoutState, LayoutTree, MvUI, MvUIChild, LUI, LUIChanges, LUIEffects, LUIEffectStage, Set
derive JSONDecode LayoutState, LayoutTree, MvUI, MvUIChild, LUI, LUIChanges, LUIEffects, LUIEffectStage, Set

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
idLayout :: Layout 
idLayout = {Layout|apply=const (NoChange,LSNone),adjust=id,restore=const NoChange}

setUIType :: UIType -> Layout
setUIType type = ruleBasedLayout (setUITypeRule type)

setUITypeRef_ :: UIType -> Layout
setUITypeRef_ type = referenceLayout ref
where 
	ref (UI _ attr items) = UI type attr items

setUIAttributes :: UIAttributes -> Layout
setUIAttributes extraAttr = ruleBasedLayout (setUIAttributesRule extraAttr)

setUIAttributesRef_ :: UIAttributes -> Layout
setUIAttributesRef_ extraAttr = referenceLayout ref
where
	ref (UI type attr items) = UI type ('DM'.union extraAttr attr) items

delUIAttributes :: UIAttributeSelection -> Layout 
delUIAttributes selection = ruleBasedLayout (delUIAttributesRule selection)

delUIAttributesRef_ :: UIAttributeSelection -> Layout 
delUIAttributesRef_ selection = referenceLayout ref
where
	ref (UI type attr items) = UI type (foldl (\a k -> if (matchKey_ selection k) ('DM'.del k a) a) attr ('DM'.keys attr)) items

modifyUIAttributes :: UIAttributeSelection (UIAttributes -> UIAttributes) -> Layout
modifyUIAttributes selection modifier = ruleBasedLayout (modifyUIAttributesRule selection modifier)

modifyUIAttributesRef_ :: UIAttributeSelection (UIAttributes -> UIAttributes) -> Layout
modifyUIAttributesRef_ selection modifier = referenceLayout ref
where
	ref (UI type attr items) = UI type ('DM'.union (modifier selected) attr) items
	where
		selected = selectAttributesOLD selection attr

//Known use:
//-	copySubUIAttributes SelectAll [0] [] 
//- copySubUIAttributes (SelectKeys ["title"]) [0] []	
//- copySubUIAttributes (SelectKeys ["label","optional","mode"]) [1] [0]
//- copySubUIAttributes (SelectKeys [HINT_ATTRIBUTE,HINT_TYPE_ATTRIBUTE]) [1] [2]
//- copySubUIAttributes (SelectKeys [HINT_ATTRIBUTE,HINT_TYPE_ATTRIBUTE]) [1] [1]
copySubUIAttributes :: UIAttributeSelection UIPath UIPath -> Layout
copySubUIAttributes selection src dst = ruleBasedLayout (copySubUIAttributesRule selection src dst)

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

//Set attributes in 'new' if they are different than, or not found in 'old'
//Remove attributes that were in old, but are no longer in new
diffAttributes old new = [SetAttribute k v \\ (k,v) <- 'DM'.toList new | maybe True (\ov -> ov <> v) ('DM'.get k old)] 
					  ++ [DelAttribute k \\ k <- 'DM'.keys old | isNothing ('DM'.get k new)]

wrapUI :: UIType -> Layout
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

wrapUIRef_ :: UIType -> Layout
wrapUIRef_ type = referenceLayout ref
where
	ref ui = uic type [ui]

unwrapUI :: Layout
unwrapUI = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply ui=:(UI type attr [i:is]) = (ReplaceUI i, LSUnwrap ui)
	apply ui 					    = (NoChange, LSUnwrap ui)	

	adjust (NoChange,state) = (NoChange,state)
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

	//Crude restore...
	//As long as the UIChange type does not support moving elements up and down the tree we cannot do better
	restore (LSUnwrap ui) = ReplaceUI ui 

unwrapUIRef_ :: Layout
unwrapUIRef_ = referenceLayout ref
where
	ref (UI _ _ [ui:_]) = ui
	ref ui = ui

//Insert the element at the specified index.
//Only insert if there are at least as many children as the specified index
insertChildUI :: Int UI -> Layout
insertChildUI position insertion = ruleBasedLayout (insertChildUIRule position insertion)

insertChildUIRef_ :: Int UI -> Layout
insertChildUIRef_ idx insert = referenceLayout ref
where
	ref ui=:(UI type attr items)
		| idx >= 0 && idx <= length items = UI type attr (insertAt idx insert items)
										  = ui

removeSubUIs :: UISelection -> Layout
removeSubUIs selection = moveSubUIs` selection Nothing

removeSubUIsRef_ :: UISelection -> Layout
removeSubUIsRef_ selection = referenceLayout ref
where
	ref ui=:(UI type attr items) //Special case for the root node
	  | inUISelection selection [] ui = UI UIEmpty 'DM'.newMap []
							          = UI type attr (flatten [rem [i] x \\ x <- items & i <- [0..]])

	rem path ui=:(UI type attr items)
	  | inUISelection selection path ui = []
						      = [UI type attr (flatten [rem (path ++ [i]) x \\ x <- items & i <- [0..]])]

moveSubUIs :: UISelection UIPath Int -> Layout 
//moveSubUIs selection path pos = moveSubUIsRef_ selection path pos
moveSubUIs selection path pos = moveSubUIs` selection (Just (path,pos)) 

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

moveSubUIs` :: UISelection (Maybe (!UIPath,!Int)) -> Layout
moveSubUIs` selection mbDst = {Layout|apply=apply,adjust=adjust,restore=restore}
where
	apply ui
		//First construct the state and determine the change that removes all matching nodes
		# (rchange, state) = case applyRem selection [] ui of
			//We can't remove the root-level UI, therefore if it matches the selection we replace it with UIEmpty
			(RemoveChild,state)         = (ReplaceUI (UI UIEmpty 'DM'.newMap []),state) 
			(ChangeChild change, state) = (change,state)
		//If a destination was specified and it exists
		# (ichange, state) = applyIns mbDst [] state
		= (mergeUIChanges rchange ichange, LSRemoveSubUIs state)

	applyRem selection path ui=:(UI type attr items)
		| not (onDestinationPath path) && inUISelection selection path ui //Match
			= (RemoveChild, {toMvUI ui & matched = True})
		| otherwise
			# (change,children) = case applyRemChildren path 0 0 items of
				([],children) = (NoChange,children)
				(changes,children) = (ChangeUI [] changes, children)
			# state = {MvUI|type=type,attr=attr,matched=False,moved=False,deleted=False,dstChange=NoChange,children=children}
			= (ChangeChild change, state)
	where
		applyRemChildren path i n [] = ([],[])
		applyRemChildren path i n [item:items]
			# (change,item) = applyRem selection (path ++ [i]) item 
			# (changes,items) = applyRemChildren path (i + 1) (if item.MvUI.matched n (n + 1)) items
			= ([(n,change):changes], [MvUIItem item:items])

	applyIns Nothing path mvui = (NoChange,mvui)
	applyIns (Just dst) path mvui
		| destinationExists dst mvui
			# (changes,mvui) = collectDestinationChanges True [mvui]
			# (change,mvui)  = applyAtDestinationPath dst changes mvui
			= (change,mvui)
		| otherwise = (NoChange,mvui)

	adjust (change, LSRemoveSubUIs mvui)
		//Adjusting changes happens in two passes 
		//In the first pass we adjust the upstream change and remove or restore items that match, or no longer match the selection
		# (rchange, mvuis) = case adjustRem selection [] change mvui of
			(ChangeChild change,mvuis) = (change,mvuis)
			(RemoveChild,mvuis)        = (ReplaceUI (UI UIEmpty 'DM'.newMap []),mvuis) //If the root-level UI needs to be removed, replace it with UIEmpty
			(InsertChild ui,mvuis)     = (ReplaceUI ui,mvuis)           			   //If the root-level UI is going to be restored, replace it
		//In the second pass we update the nodes that were moved
		# (ichange, mvui) = adjustIns mbDst [] mvuis
		= (mergeUIChanges rchange ichange, LSRemoveSubUIs mvui)

	adjustRem selection path NoChange mvui=:{MvUI|deleted,matched,moved,children} 
		| deleted = (ChangeChild NoChange,[mvui])
		//Check if the node should have been removed by the layout
		# ui = fromMvUI mvui
		| not (onDestinationPath path) && inUISelection selection path ui
			| matched   = (ChangeChild NoChange, [mvui]) //Already removed, nothing to do
			| otherwise = (RemoveChild, [{MvUI|mvui & matched = True}]) //Remove it now
		| otherwise
			| matched //It should not be removed, restore it 
				# ui = fromMvUI mvui
				# (ChangeChild change, mvui`) = applyRem selection path ui //Replace the state, it is now possible that subnodes are matched
				= (InsertChild (applyUIChange change ui), [{MvUI|mvui & deleted = True}, mvui`])
			| otherwise 
				//EXPERIMENT: Recursively check children (may not be neccesary)
				# (schanges, children) = adjustRemSiblings selection path (\x -> True) children
				# change = case schanges of
					[] = NoChange
					_  = ChangeUI [] schanges
				= (ChangeChild change, [{MvUI|mvui & children = children}]) //Wasn't moved, nothing to do

	adjustRem selection path (ReplaceUI ui) mvui=:{MvUI|matched,moved,deleted}
		= case applyRem selection path ui of
			(ChangeChild change, mvui`) //The new ui does not match the selection
				| matched //This node was previously removed, we need to insert it
					= (InsertChild (applyUIChange change ui), [{MvUI|mvui & deleted = True}, mvui`])
				| otherwise //The ui existed, so we replace it with the adjusted version
					= (ChangeChild (ReplaceUI (applyUIChange change ui)), [{MvUI|mvui & deleted = True}, mvui`])
			(RemoveChild, mvui`) //The new ui matches the selection 
				| matched //The previous ui was also removed, so we don't have to do anything here, but we may have to update the target location
					 | moved
						= (ChangeChild NoChange, [{mvui & dstChange = ReplaceUI ui}]) //We don't apply the replace, but defer it to the target location
					 | otherwise
						= (ChangeChild NoChange, [{MvUI|mvui & deleted = True}, mvui`])
				| otherwise //The previous ui was not removed, so we remove it now
					= (RemoveChild, [{MvUI|mvui & deleted = True}, mvui`])

	adjustRem selection path change=:(ChangeUI attrChanges childChanges) mvui=:{MvUI|attr,matched,moved,dstChange,children}
		//Check if the change, when applied, would make this ui match the selection
		| not (onDestinationPath path) && inUISelection selection path (applyUIChange change (fromMvUI mvui))
			//Apply the changes to the state, but ignore the downstream changes because the node is hidden
			# attr         = foldl (flip applyUIAttributeChange) attr attrChanges
			//By passing SelectNone we make sure that all moved descendents are no longer removed
			# (_,children) = adjustRemChildChanges SelectNone path childChanges children 
			//If this item was already moved before, we need to apply the changge
			# dstChange = if moved (mergeUIChanges dstChange change) dstChange
			= (if matched (ChangeChild NoChange) RemoveChild, [{MvUI|mvui & attr = attr, matched = True, dstChange = dstChange, children = children}])
		| otherwise
			//Apply the changes to the state
			# attr                    = foldl (flip applyUIAttributeChange) attr attrChanges
			# (childChanges,children) = adjustRemChildChanges selection path childChanges children
			# mvui                    = {MvUI|mvui & attr = attr, children = children}
			| matched //If this ui was previously removed, we need to restore it
				= (InsertChild (fromMvUI mvui), [mvui])
			| otherwise
				= (ChangeChild (ChangeUI attrChanges childChanges), [mvui])

	adjustRemChildChanges selection path [] items = ([], items)
	adjustRemChildChanges selection path [(i,c):cs] items
		# (c, items) = adjustRemChildChange selection path i c items 
		# (cs, items) = adjustRemChildChanges selection path cs items
		= (c ++ cs, items)

	adjustRemChildChange selection path i (ChangeChild change) items
		| i >= 0 && i < numItems items //Check bounds
			//Recursively apply change 
			# (cchange, item) = adjustRem selection (path ++ [i]) change (getItem i items)
			# items           = setItem i item items
			# change = case cchange of 
				(ChangeChild NoChange)         = []
				(ChangeChild (ChangeUI [] [])) = []	
				_                              = [(adjustIndex i items, cchange)]
			= (change, items)
		| otherwise
			= ([],items)
	adjustRemChildChange selection path i (InsertChild ui) items
		| i >= 0 && i <= numItems items 
			# (cchange,item) = applyRem selection (path ++ [i]) ui
			# change = case cchange of 
				RemoveChild = []
				ChangeChild change = [(adjustIndex i items, InsertChild (applyUIChange change ui))]
			# items = insertItem i item items
			# (schanges, items) = adjustRemSiblings selection path (\x -> x > i) items
			= (change ++ schanges, items)
		| otherwise
			= ([],items)
	
	adjustRemChildChange selection path i (RemoveChild) items
		| i >= 0 && i < numItems items 
			# item=:{MvUI|matched} = getItem i items
			# change = if matched [] [(adjustIndex i items,RemoveChild)]
			# items = setItem i [{MvUI|item & deleted = True}] items
			# (schanges, items) = adjustRemSiblings selection path (\x -> x >= i) items
			= (change ++ schanges, items)
		| otherwise
			= ([],items)

	adjustRemChildChange selection path i (MoveChild d) items
		| i >= 0 && i < numItems items &&  d >= 0 && d < numItems items
			# item=:{MvUI|matched} = getItem i items //If the item was removed by the layout, we don't need to move it downstream
			# change = if matched [] [(adjustIndex i items,MoveChild (adjustIndex d items))]
			# items = moveItem i d items
			# (schanges, items) = adjustRemSiblings selection path (\x -> True) items //TODO: Improve the predicate, we don't have to check all...
			= (change ++ schanges, items) 
		| otherwise
			= ([],items)

	adjustRemSiblings selection path whichSiblings children = (changes, reverse items)
	where
        (changes, items) = adjust 0 [] children


        adjust :: !Int ![MvUIChild] ![MvUIChild] -> (![(!Int,!UIChildChange)],![MvUIChild])
		adjust i before [] = ([],before)
		adjust i before [MvUIItem item:children]
			| item.MvUI.deleted
				= adjust i [MvUIItem item : before] children //Ignore deleted branches
			| whichSiblings i
				# (cchange, items) = adjustRem selection (path ++ [i]) NoChange item
				# change = case cchange of
					(ChangeChild NoChange)         = []
					(ChangeChild (ChangeUI [] [])) = []
					(ChangeChild change)           = [(adjustIndex i (reverse before),cchange)]
					_                              = [] //FIXME: This function does not expect inserts for some reason...
				# (changes, children) = adjust (i + 1) (map MvUIItem (reverse items) ++ before) children
				= (change ++ changes, children)
			| otherwise
				= adjust (i + 1) [MvUIItem item : before] children
		adjust i before [child:children]
			= adjust i [child : before] children
			
	adjustIns Nothing path mvuis = (NoChange, removeDeleted mvuis)
	adjustIns (Just dst) path mvuis
		//First check if the previously inserted nodes are still where they were last time
		//If they are not, remove them if necessary
		# (dstExists,dstNew,correctionChange,mvuis) = adjustDestination (Just dst) mvuis
		//Then collect the changes to the destination and apply them in the right place
		| dstExists
			# (changes,mvui)      = collectDestinationChanges dstNew mvuis
			# (updateChange,mvui) = applyAtDestinationPath dst changes mvui
			= (mergeUIChanges correctionChange updateChange, mvui)
		| otherwise
			= (correctionChange, removeDeleted mvuis)

	//Check if a MvUIMoveDestination node is found where we expect it.
	//If it is not, we remove it from the state and create a change to correct the downstream UI 
	//We also return if we had to insert the exists and if we inserted the destination node for the first time
	adjustDestination dst [item=:{MvUI|deleted}:items] //Ignore the deleted items, we haven't removed them yet
		| deleted
			# (dstExist,dstNew,correctionChange,items) = adjustDestination dst items
			= (dstExist,dstNew,correctionChange,[item:items])
		| otherwise
			# (dstExist,dstNew,correctionChange,item) = adjustDestinationItem dst item
			= (dstExist,dstNew,correctionChange,[item:items])
		
	//We found the correct container, start looking for MvUIMoveDestination node
	adjustDestinationItem (Just ([], pos)) mvui=:{MvUI|children} //We found the correct container, start looking for MvUIMoveDestination node
		| pos < 0 = (False,False,NoChange,mvui) //Out of bounds...
		# dstExists = numItems children >= pos
		= case find 0 0 children of
			Nothing //Not found, check if there are enough items for the position
				= (dstExists, True, NoChange, mvui)
			Just (i,ai,n) //Found
				| i == pos //The inserted segment is where it should be!
				    = (True, False, NoChange, mvui)
				| i < pos  //Elements have been removed before the inserted segment
					| dstExists //If there are 'enough' items we can move elements from after the inserted segment to just before it
						# difference = pos - i
						# change = ChangeUI [] [(ai + n + j, MoveChild (ai + j)) \\ j <- [0 .. (difference - 1)]]
						# children = moveMoveDestinationTowardsEnd difference children
						= (True, False,change, {MvUI|mvui & children = children})
					| otherwise //If there are not enough elements, then the destination should not exist
						# change = ChangeUI [] (repeatn n (ai,RemoveChild))
						# children = [x \\ x <- children | not (x =:(MvUIMoveDestination _))]
					    = (False,False,change, {MvUI|mvui & children = children})
				| i > pos  //Elements have been inserted before the inserted segment
					# difference = i - pos
					# change = ChangeUI [] [(ai - 1 - j, MoveChild (ai + n - 1 - j)) \\ j <- [0 .. (difference - 1)]]
					# children = moveMoveDestinationTowardsBegin difference children
					= (True,False,change, {MvUI|mvui & children = children})
	where
		find i ai [] = Nothing
		find i ai [MvUIMoveDestination n:_] = Just (i,ai,n)
		find i ai [MvUIItem x:xs] = find (i + 1) (if x.MvUI.matched ai (ai + 1)) xs
		find i ai [_:xs] = find i ai xs

	//We are still searching for the destination
	adjustDestinationItem (Just ([s:ss], pos)) mvui=:{MvUI|children} 
		# (dstExists, dstNew, changes, children) = adjust 0 0 children
		# change = case changes of
			[] = NoChange
			_  = ChangeUI [] changes
		= (dstExists,dstNew,change, {MvUI|mvui & children = children})
	where
		adjust i ai [] = (False,False,[],[])
		adjust i ai [MvUIItem x:xs]
			| x.MvUI.deleted //Ignore deleted branches
				# (de,dn,cs,xs)   = adjust i ai xs
				= (de,dn,cs,[MvUIItem x:xs])
			| otherwise
				# (de,dn,c,x)     = adjustDestinationItem (if (i == s) (Just (ss,pos)) Nothing) x
				# (des,dns,cs,xs) = adjust (i + 1) (if x.MvUI.matched ai (ai + 1)) xs
				= case c of 
					NoChange = (de || des, dn || dns, cs, [MvUIItem x:xs])
					_        = (de || des, dn || dns, [(ai,ChangeChild c):cs], [MvUIItem x:xs])
		adjust i ai [MvUIMoveDestination n :xs] //This should not be here, create remove instructions
			# (de,dn,cs,xs)   = adjust i ai xs
			= (de,dn,repeatn n (ai,RemoveChild) ++ cs, xs)
		adjust i ai [x:xs] //Ignore other nodes
			# (de,dn,cs,xs)   = adjust i ai xs
			= (de,dn,cs,[x:xs])

	adjustDestinationItem Nothing mvui=:{MvUI|children}
		# (changes, children) = adjust 0 0 children //There should not be a destination node here, process children and remove it when we find one
		# change = case changes of
			[] = NoChange
			_  = ChangeUI [] changes
		= (False,False,change,{MvUI|mvui & children = children})
	where
		adjust i ai [] = ([],[])
		adjust i ai [MvUIItem x:xs]
			# (_,_,c,x) = adjustDestinationItem Nothing x
			# (cs,xs)   = adjust (i + 1) (if x.MvUI.matched ai (ai + 1)) xs
			= case c of 
				NoChange = (cs,[MvUIItem x:xs])
				_        = ([(ai,ChangeChild c):cs], [MvUIItem x:xs])
		adjust i ai [MvUIMoveDestination n :xs] //This should not be here, create remove instructions
			# (cs,xs)   = adjust i ai xs
			= (repeatn n (ai,RemoveChild) ++ cs, xs)
		adjust i ai [x:xs] //Ignore other nodes
			# (cs,xs)   = adjust i ai xs
			= (cs,[x:xs])

	removeDeleted mvuis = hd [{MvUI|x & children = removeDeletedItems children} \\ x=:{MvUI|deleted,children} <- mvuis | not deleted]
	where
		removeDeletedItems items = [app x \\ x <- items | not (del x)]

		app (MvUIItem mvui=:{MvUI|children}) = MvUIItem {MvUI|mvui & children = removeDeletedItems children}
		app x = x

		del (MvUIItem {MvUI|deleted}) = deleted
		del _ = False

	collectDestinationChanges dstNew mvuis
		# (_,changes,items) = collectc 0 (map MvUIItem mvuis)
		= case items of
			[MvUIItem mvui] = (changes,mvui)
			_               = abort "Broken algorithm, not all removed items have been collected :("
	where
		collect i mvui=:{MvUI|matched,moved,dstChange,children}
			| matched 
				| moved && not dstNew //Already moved, only apply the local changes
					= case dstChange of
						NoChange = (i + 1, [], mvui)
						_        = (i + 1, [(i,ChangeChild dstChange)],{MvUI|mvui & dstChange = NoChange})
				| otherwise //Newly removed node
					= (i + 1, [(i,InsertChild (fromMvUI mvui))],{MvUI|mvui & moved=True})
			| otherwise
				# (i,changes,children) = collectc i children
				= (i,changes,{MvUI|mvui & children=children})

		collectc i [] = (i,[],[])
		collectc i [MvUIItem x=:{MvUI|deleted}:xs]
			| deleted 
				# c = if dstNew [] (repeatn (countMoved x) (i,RemoveChild)) //If the destination still exist remove deleted children
				# (i,cs,xs) = collectc i xs
				= (i, c ++ cs, xs)
			| otherwise
				# (i,c,x) = collect i x
				# (i,cs,xs) = collectc i xs
				= (i, c ++ cs, [MvUIItem x:xs])
		collectc i [x:xs]
			# (i,cs,xs) = collectc i xs
			= (i,cs,[x:xs])

	applyAtDestinationPath ([],pos) changes mvui=:{MvUI|children}
		//Adjust the indices of the changes
		# numRemovedBeforePos = length [matched \\ MvUIItem {MvUI|matched} <- take pos children | matched]
		# changes = [(pos - numRemovedBeforePos + i,c) \\ (i,c) <- changes]
		//Determine how much the number of moved items changes in the destination
		# insertedAmountChange = length [c \\ c=:(_,InsertChild _) <- changes] - length  [c \\ c=:(_,RemoveChild) <- changes]
		# children = updateMoveDestination pos ((+) insertedAmountChange) children
		= (ChangeUI [] changes, {MvUI|mvui & children = children})
	applyAtDestinationPath ([s:ss],pos) changes mvui=:{MvUI|children}
		| s >= 0 && s < numItems children
			# (change,item) = applyAtDestinationPath (ss,pos) changes (getItem s children)
			= (ChangeUI [] [(adjustIndex s children,ChangeChild change)], {MvUI|mvui & children = setItem s [item] children})
		| otherwise
			= (NoChange,mvui)

	restore (LSRemoveSubUIs mvui)
		= ReplaceUI (fromMvUI mvui) //VERY CRUDE RESTORE..

	//UTIL FUNCTIONS 
	toMvUI :: UI -> MvUI 
	toMvUI (UI type attr items) = {MvUI|type=type,attr=attr,matched=False,moved=False,deleted=False,dstChange=NoChange,children=map (MvUIItem o toMvUI) items}

	fromMvUI :: MvUI -> UI
	fromMvUI {MvUI|type,attr,children} = UI type attr [fromMvUI mvui \\ MvUIItem mvui <- children]
	
	//Check if the destination for moving elements is this node or one of its descendents
	onDestinationPath path = maybe False (startsWith path o fst) mbDst
	where
		startsWith [] dst = True
		startsWith [p:ps] [d:ds] = if (p == d) (startsWith ps ds) False
		startsWith _ _ = False

	destinationExists ([],pos) {MvUI|children}
		= pos >= 0 && pos <= numItems children
	destinationExists ([s:ss],pos) {MvUI|children}
		| s >= 0 && s < numItems children = destinationExists (ss,pos) (getItem s children)
										  = False

	numItems xs = length [x \\ x=:(MvUIItem {MvUI|deleted}) <- xs | not deleted]

	getItem i [MvUIItem x=:{MvUI|deleted}:xs]
		| deleted = getItem i xs
		| i == 0  = x
				  = getItem (i - 1) xs
	getItem i [_:xs] = getItem i xs
		
	setItem i item [] = []
	setItem i item [MvUIItem x=:{MvUI|deleted}:xs]
		| deleted = [MvUIItem x:setItem i item xs]
		| i < 0  = [MvUIItem x:xs]
		| i == 0 = map MvUIItem item ++ xs
				 = [MvUIItem x:setItem (i - 1) item xs]
	setItem i item [x:xs] = [x:setItem i item xs]

	insertItem i item [] 
		| i == 0 = [MvUIItem item]
				 = []
	insertItem i item [MvUIItem x=:{MvUI|deleted}:xs]
		| deleted = [MvUIItem x:insertItem i item xs]
		| i < 0  = [MvUIItem x:xs]
		| i == 0 = [MvUIItem item, MvUIItem x:xs]
				 = [MvUIItem x:insertItem (i - 1) item xs]
	insertItem i item [x:xs] = [x:insertItem i item xs]
	
	moveItem i d xs = insertItem d (getItem i xs) (removeItem i xs)

	removeItem i [] = []
	removeItem i [MvUIItem x=:{MvUI|deleted}:xs]
		| deleted = [MvUIItem x:removeItem i xs]
	 	| i < 0   = [MvUIItem x:xs]
		| i == 0  = xs
				  = [MvUIItem x:removeItem (i - 1) xs]
	removeItem i [x:xs] = [x:removeItem i xs]

	updateMoveDestination 0 f [MvUIMoveDestination n:xs] = [MvUIMoveDestination (f n):xs]
	updateMoveDestination 0 f xs                         = [MvUIMoveDestination (f 0):xs]
	updateMoveDestination i f [MvUIItem x:xs]            = [MvUIItem x:updateMoveDestination (i - 1) f xs]
	updateMoveDestination i f [x:xs]                     = [x:updateMoveDestination i f xs]
	updateMoveDestination i f [] = []

    moveMoveDestinationTowardsEnd n children = children//trace_n "A" children //FIXME
    moveMoveDestinationTowardsBegin n children = children//trace_n "B" children

	countMoved {MvUI|moved,children} = (if moved 1 0) + sum [countMoved x \\ MvUIItem x <- children]

	adjustIndex s items = s + (offset s items)
	where
		offset n [] = n
		offset n [MvUIItem {MvUI|matched,deleted}:xs]
			| deleted = offset n xs //Ignore deleted items
			| n == 0    = 0 //Stop
						= offset (n - 1) xs - (if matched 1 0)
		offset n [MvUIMoveDestination num:xs] = offset n xs + num

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
noEffects = {overwrittenType = ESNotApplied, overwrittenAttributes = 'DM'.newMap, hiddenAttributes = 'DM'.newMap, additional = ESNotApplied, hidden = ESNotApplied}

//Initialize an LUI tree from a regular UI tree
initLUI :: Bool UI -> LUI
initLUI toBeInserted (UI type attr items) = LUINode type attr (map (initLUI toBeInserted) items) {noChanges & toBeInserted = toBeInserted} noEffects

toBeAdded :: Int LUI -> LUI
toBeAdded ruleId (LUINode type attr items changes effects) = LUINode type attr items changes {effects & additional = ESToBeApplied ruleId}

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
			# adjustedIndex = adjustIndex_ index Nothing items
			| index < 0 || adjustedIndex >= length items = lui
			= LUINode type attr (updateItem (applyUpstreamChange change) adjustedIndex items) changes effects
		_
			= lui
	applyUpstreamChildChange lui (index,RemoveChild) = case lui of
		(LUINode type attr items changes effects)
			# adjustedIndex = adjustIndex_ index Nothing items
			| index < 0 || adjustedIndex >= length items = lui
			= LUINode type attr (removeItem adjustedIndex items) changes effects
		_ = lui
	applyUpstreamChildChange lui (index,InsertChild ui) = case lui of
		(LUINode type attr items changes effects)
			# adjustedIndex = adjustIndex_ index Nothing items
			| index < 0 || adjustedIndex >= length items = lui
			= LUINode type attr (insertAt adjustedIndex (initLUI True ui) items) changes effects
		_ = lui
	applyUpstreamChildChange lui (index,MoveChild destination) = case lui of
		(LUINode type attr items changes effects)
			# shiftId = nextShiftID_ items
			# adjustedIndex = adjustIndex_ index Nothing items
			| index < 0 || adjustedIndex >= length items = lui
			= LUINode type attr (shiftItem shiftId adjustedIndex destination items) changes effects
		_ = lui

	//An index may point to the destination of a shifted child node. In that case we want to apply
	//the update to the node that will be shifted to that destination
	updateItem updateFunction index items = case items !! index of
		(LUIShiftDestination shiftId) = updateItem updateFunction (lookupShiftSource_ shiftId items) items
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
					= insertAt (adjustIndex_ destination Nothing items) (LUIShiftDestination prevShiftId) items
		//Regular node
		(LUINode type attr citems changes effects)
			//Mark the node as a shifted node
			# items = updateAt index (LUINode type attr citems {changes & toBeShifted = Just shiftId} effects) items
			//Record the destination
			= insertAt (adjustIndex_ destination Nothing items) (LUIShiftDestination shiftId) items
	where
		findSamePositionShift shiftId destination items = find 0 0 items
		where
			find i ai [] = Nothing
			find i ai [x=:(LUINode _ _ _ {toBeShifted=Just sourceId} _):xs]
				| sourceId == shiftId = if (ai == destination) (Just (ai,x)) Nothing
				                      = find (i + 1) ai xs
			find i ai [x:xs]
				| isInvisibleUpstream_ Nothing x = find (i + 1) ai xs
				                                 = find (i + 1) (ai + 1) xs

nextShiftID_ :: [LUI] -> Int
nextShiftID_ items = maximum [-1:map shiftID items] + 1
where
	shiftID (LUINode _ _ _ {toBeShifted=Just x} _) = x
	shiftID (LUIShiftDestination x) = x
	shiftID _ = -1

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

insertChildUIRule :: Int UI -> LayoutRule
insertChildUIRule position insertion = rule
where
	rule ruleId lui=:(LUINode type attr items changes=:{toBeReplaced=Just replacement} effects)
		= LUINode type attr items {changes & toBeReplaced=Just (rule ruleId replacement)} effects
	rule ruleId lui=:(LUINode type attr items changes effects)
		# index = adjustIndex_ position (Just ruleId) items
		//If the index is outside the range, don't add anything
		| index < 0 || index > length items = lui
		//If the index is at the end of the range, add the item
		| index == length items
			= LUINode type attr (undoAdditions ruleId items ++ [toBeAdded ruleId (initLUI False insertion)]) changes effects
		# selected = items !! index
		| getAdditional selected === ESToBeApplied ruleId || getAdditional selected === ESApplied ruleId 
			= lui
		| otherwise
			= LUINode type attr (insertAt index (toBeAdded ruleId (initLUI False insertion)) (undoAdditions ruleId items)) changes effects

	rule ruleId lui = lui

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

	getAdditional (LUINode _ _ _ _ {additional}) = additional
	getAdditional _ = ESNotApplied

//Utility functions shared by the layout rules:

//Adjust the index and length for additional nodes inserted by layout rules
adjustIndex_ :: Int (Maybe LID) [LUI] -> Int
adjustIndex_ index exceptAdditional items = scan index 0 items
where
	scan r i [] = i
	//Ignore removed, shifted and synthesized nodes
	scan r i [x:xs] | isInvisibleUpstream_ exceptAdditional x = scan r (i + 1) xs
	scan 0 i [_:xs] = i
	scan r i [_:xs] = scan (r - 1) (i + 1) xs

//Some nodes are not visible upstream, so we should not count them when using indexes
isInvisibleUpstream_ :: !(Maybe LID) !LUI -> Bool
isInvisibleUpstream_ _ (LUINode _ _ _ {toBeRemoved=True} _) = True
isInvisibleUpstream_ _ (LUINode _ _ _ {toBeShifted=(Just _)} _) = True
//When correcting the index to insert at for insertChildUI, the node for that specific rule should not be skipped
//TODO: Refactor this exception to the rule when all rules have been implemented and it is clear which other exceptions exist
isInvisibleUpstream_ exception (LUINode _ _ _ _ {additional})
	= case (exception,additional) of
		(_,ESNotApplied) = False
		(Just exceptionId,ESToBeApplied ruleId) = exceptionId <> ruleId
		(Just exceptionId,ESApplied ruleId) = exceptionId <> ruleId
		_ = True

isInvisibleUpstream_ _ _ = False

lookupShiftSource_ :: Int [LUI] -> Int
lookupShiftSource_ shiftId items = fromJust (findIndex isSource items)
where
	isSource (LUINode _ _ _ {toBeShifted = Just sourceId} _) = sourceId == shiftId
	isSource _ = False

selectNode_ :: UIPath LUI -> Maybe LUI 
selectNode_ [] lui = case lui of
	(LUINode _ _ _ {toBeRemoved=True} _)              = Nothing
	(LUINode _ _ _ {toBeReplaced=Just replacement} _) = selectNode_ [] replacement
	(LUINode _ _ _ _ _)                               = Just lui
	_                                                 = Nothing
selectNode_ [s:ss] (LUINode _ _ items _ _)
	# index = adjustIndex_ s Nothing items
	| index < 0 || index >= length items = Nothing
	# child = items !! index
	= case child of
		(LUINode _ _ _ {toBeReplaced=Just replacement} _) = selectNode_ ss replacement
		(LUINode _ _ _ _ _)                               = selectNode_ ss child
		(LUIShiftDestination shiftId)                     = selectNode_ ss (items !! (lookupShiftSource_ shiftId items))

updateNode_ :: UIPath (LUI -> LUI) LUI -> LUI 
updateNode_ [] update lui = case lui of
	(LUINode _ _ _ {toBeRemoved=True} _) = lui
	(LUINode _ _ _ {toBeReplaced=Just replacement} _) = updateNode_ [] update replacement
	(LUINode _ _ _ _ _)                               = update lui
	_                                                 = lui
updateNode_ [s:ss] update lui=:(LUINode type attr items changes effects)
	# index = adjustIndex_ s Nothing items
	| index < 0 || index >= length items = lui
	# child = items !! index
	# items = case child of
		(LUINode ctype cattr citems cchanges=:{toBeReplaced=Just replacement} ceffects)
			# replacement = updateNode_ ss update replacement
			= updateAt index (LUINode ctype cattr citems {cchanges & toBeReplaced = Just replacement} ceffects) items
		(LUINode _ _ _ _ _) 
			# child = updateNode_ ss update child
			= updateAt index child items
		(LUIShiftDestination shiftId)
			# sourceIndex = lookupShiftSource_ shiftId items
			# source = updateNode_ ss update (items !! sourceIndex)
			= updateAt sourceIndex source items
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

/*
* Once layout rules have annotated their effects, the change that has to be applied downstream
* can be extracted from the buffered structure. The result of doing this is both the combination of
* the ui change that has to be applied downstream, and a new version of the buffered tree that
* in which all pending changes and effects have been applied.
*/
extractDownstreamChange :: LUI -> (!UIChange,!LUI)
extractDownstreamChange (LUINode type attr items changes=:{toBeReplaced=Just replacement} effects)
	//When the UI is to be replaced, we need to determine the replacement UI with all effects applied
	# (ui,lui) = extractUIWithEffects replacement
	= (ReplaceUI ui, lui)
extractDownstreamChange lui=:(LUINode type attr items changes effects)
	//Check overwritten ui-types: There is no way to set a type downstream, so a full replace is needed
	| typeNeedsUpdate type effects
		# (ui,lui) = extractUIWithEffects lui
		= (ReplaceUI ui,lui)
	//Determine changes to attributes
	# (attributeChanges,attr,effects) = extractAttributeChanges changes attr effects
	//Determine changes to children
	# (childChanges,items) = extractChildChanges items
	# change = if (attributeChanges =: [] && childChanges =: [])
		NoChange
		(ChangeUI attributeChanges childChanges)
	= (change, LUINode type attr items (resetChanges changes) effects)
where
	typeNeedsUpdate type {overwrittenType=ESToBeApplied _} = True
	typeNeedsUpdate type {overwrittenType=ESToBeUpdated _ _} = True
	typeNeedsUpdate type {overwrittenType=ESToBeRemoved _} = True
	typeNeedsUpdate _ _ = False

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

	extractChildChanges items
		# (shifts,items) = extractShifts items
		# (insertsAndRemoves,items) = extractInsertsAndRemoves items
		= (shifts ++ insertsAndRemoves, items)
	where
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
				# (ui,x) = extractUIWithEffects x
				# (cs,xs) = extract (i + 1) xs
				= ([(i,InsertChild ui):cs],[x:xs])
			extract i [x=:(LUINode _ _ _ _ {additional=ESToBeApplied _}):xs]
				# (ui,x) = extractUIWithEffects x
				# (cs,xs) = extract (i + 1) xs
				= ([(i,InsertChild ui):cs],[x:xs])
			extract i [x=:(LUINode _ _ _ _ {additional=ESToBeRemoved _}):xs]
				# (cs,xs) = extract i xs
				= ([(i,RemoveChild):cs],xs)
			extract i [x:xs]
				# (c,x) = extractDownstreamChange x
				# (cs,xs) = extract (i + 1) xs
				= case c of
					NoChange = (cs,[x:xs])
					_        = ([(i,ChangeChild c):cs],[x:xs])

extractDownstreamChange lui = (NoChange,lui)

applyAttributeEffectsAsChanges_ effects=:{overwrittenAttributes,hiddenAttributes}
	= ([],effects)

extractUIWithEffects :: LUI -> (!UI,!LUI)
extractUIWithEffects (LUINode ltype lattr litems changes=:{toBeReplaced=Just replacement} effects)
	= extractUIWithEffects replacement
extractUIWithEffects (LUINode ltype lattr litems changes=:{setAttributes,delAttributes} effects=:{overwrittenType})
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
	# (items,litems) = unzip (map extractUIWithEffects litems)
	= (UI type attr items
	  ,LUINode ltype lattr litems noChanges (confirmAdditional effects)
	  )
where
	remove (LUINode _ _ _ {toBeRemoved} _) = toBeRemoved
	remove (LUINode _ _ _ {toBeReplaced=Nothing} {additional = ESToBeRemoved _}) = True
	remove _ = False

	collectShiftSources items = foldr collect ('DM'.newMap,[]) items
	where
		collect n=:(LUINode _ _ _ {toBeShifted=Just shiftID} _) (sources,items) = ('DM'.put shiftID n sources,items)
		collect n (sources,items) = (sources,[n:items])

	replaceShiftDestinations sources items = foldr replace [] items
	where
		replace (LUIShiftDestination shiftID) items = maybe items (\n -> [resetToBeShifted n:items]) ('DM'.get shiftID sources)
		replace n items = [n:items]

	confirmAdditional effects=:{additional=ESToBeApplied ruleId} = {effects & additional=ESApplied ruleId}
	confirmAdditional effects = effects

extractUIWithEffects _ = abort "extractUIWithEffects: can only extract UI from LUINodes"

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
revertEffects (LUINode type attr items changes effects=:{additional})
	//Remove existing additional nodes
	# additional = case additional of
		(ESApplied ruleId) = ESToBeRemoved ruleId
		_ 				   = additional
	//Remove newly added additional nodes
	# items = filter notNewAdditional items
	= LUINode type attr (map revertEffects items) changes {effects & additional = additional}
where
	notNewAdditional (LUINode _ _ _ _ effects=:{additional=ESToBeApplied _}) = False
	notNewAdditional _ = True

revertEffects lui = lui

import StdDebug

ruleBasedLayout :: LayoutRule -> Layout
ruleBasedLayout rule = {Layout|apply,adjust,restore}
where
	apply ui
		= appSnd LSRule (extractDownstreamChange (rule 0 (initLUI False ui)))
	adjust (change, LSRule lui)
		= appSnd LSRule (extractDownstreamChange (rule 0 (applyUpstreamChange change lui)))
	restore (LSRule lui)
	//	# (ui,lui) = extractUIWithEffects lui
	//	= ReplaceUI ui
		= fst (extractDownstreamChange (revertEffects lui)) 
