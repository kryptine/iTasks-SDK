implementation module iTasks.UI.Editor.Common

import StdBool, StdEnum, StdOrdList

import iTasks.UI.Definition, iTasks.UI.Editor
import Data.Tuple, Data.Error, Text, Text.JSON
import qualified Data.Map as DM

emptyEditor :: Editor a
emptyEditor = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI _ _ vst			    = (Ok (ui UIEmpty,newFieldMask),vst)
	onEdit _ _ val mask vst 	= (Ok (NoChange,mask),val,vst)
	onRefresh _ _ val mask vst  = (Ok (NoChange,mask),val,vst)

listEditor :: (Maybe ([a] -> a)) Bool Bool (Maybe ([a] -> String)) (Editor a) -> Editor [a]
listEditor add remove reorder count itemEditor = {Editor|genUI=genUI,onEdit=onEdit,onRefresh=onRefresh}
where
	genUI dp val vst=:{VSt|taskId,mode} = case genChildUIs dp 0 val [] vst of
		(Ok (items,masks),vst)
			//Add list structure editing buttons
			# items = if (not (mode =: View) && (remove || reorder)) [listItemUI taskId dp (length val) idx idx dx \\ dx <- items & idx <- [0..]] items
			//Add the add button
			# items = if (not (mode =: View) && add =: Just _) (items ++ [addItemControl val]) items
			= (Ok (uic UIContainer items,CompoundMask {fields=masks,state=toJSON (indexList val)}), vst)
		(Error e,vst)  = (Error e,vst)
	where			
		genChildUIs dp _ [] us vst = (Ok (unzip (reverse us)), vst)
		genChildUIs dp i [c:cs] us vst = case itemEditor.Editor.genUI (dp++[i]) c vst of
			(Ok (u,m),vst) = genChildUIs dp (i+1) cs [(u,m):us] vst
			(Error e,vst)  = (Error e,vst)

		addItemControl val
			# counter  	= maybe [] (\f -> [uia UITextView ('DM'.unions [widthAttr FlexSize, valueAttr (JSONString (f val))])]) count
			# button	= if (isJust add) [uia UIButton ('DM'.unions [iconClsAttr "icon-add",editAttrs taskId (editorId dp) (Just (JSONString "add"))])] []
			# attr      = 'DM'.unions [halignAttr AlignRight,heightAttr WrapSize,directionAttr Horizontal]
			= uiac UIContainer attr (counter ++ button)

	listItemUI taskId dp numItems idx id item
		# buttons	= (if reorder
			[uia UIButton ('DM'.unions [iconClsAttr "icon-up", enabledAttr (idx <> 0), editAttrs taskId (editorId dp) (Just (JSONString ("mup_" +++ toString id)))])
							  ,uia UIButton ('DM'.unions [iconClsAttr "icon-down", enabledAttr (idx <> numItems - 1), editAttrs taskId (editorId dp) (Just (JSONString ("mdn_" +++ toString id)))])
							  ] []) ++
							  (if remove
							  [uia UIButton ('DM'.unions [iconClsAttr "icon-remove",editAttrs taskId (editorId dp) (Just (JSONString ("rem_" +++ toString id)))])
							  ] [])
		# attr = 'DM'.unions [halignAttr AlignRight,heightAttr WrapSize,directionAttr Horizontal]
		= uiac UIContainer attr (if (reorder || remove) ([item] ++ buttons) [item])
			
	//Structural edits on the list
	onEdit dp ([],JSONString e) items (CompoundMask {fields=masks,state}) vst=:{VSt|taskId}
		# ids = fromMaybe [] (fromJSON state) //All item UI's have a unique id that is used in the data-paths of that UI
		# [op,id:_] = split "_" e
		# id = toInt id 
		# index = itemIndex id ids
		| op == "mup" && reorder
			| index < 1 || index >= (length items) = (Error "List move-up out of bounds",items,vst)
			= (Ok (ChangeUI [] [(index,MoveChild (index - 1))],CompoundMask {fields=(swap masks index),state=toJSON (swap ids index)}), (swap items index), vst)
		| op == "mdn" && reorder
			| index < 0 || index > (length items - 2) = (Error "List move-down out of bounds",items,vst)
			= (Ok (ChangeUI [] [(index,MoveChild (index + 1))],CompoundMask {fields=(swap masks (index + 1)),state=toJSON (swap ids (index + 1))}), (swap items (index + 1)), vst)
		| op == "rem" && remove
			| index < 0 || index >= (length items) = (Error "List remove out of bounds",items,vst)
			= (Ok (ChangeUI [] [(index,RemoveChild)],CompoundMask {fields=removeAt index masks,state=toJSON (removeAt index ids)}), (removeAt index items), vst)
		| op == "add" && add =: (Just _)
			# f = fromJust add
			# nx = f items
			# ni = length items
			# nid = nextId ids
			= case itemEditor.Editor.genUI (dp++[nid]) nx vst of
				(Error e,vst) = (Error e,items,vst)
				(Ok (ui,nm),vst)
					# nitems = items ++ [nx]
					# nmasks = masks ++ [nm]
					# nids = ids ++ [nid]
					# insert = [(ni,InsertChild (listItemUI taskId dp (ni + 1) ni nid ui))]
					# counter = maybe [] (\f -> [(ni + 1, ChangeChild (ChangeUI [] [(0,ChangeChild (ChangeUI [SetAttribute "value" (JSONString (f nitems))] []))]))]) count
					# prevdown = if (ni > 0) [(ni - 1,ChangeChild (ChangeUI [] [(2,ChangeChild (ChangeUI [SetAttribute "enabled" (JSONBool True)] []))]))] []
					# change = ChangeUI [] (insert ++ counter ++ prevdown)
					= (Ok (change,CompoundMask {fields=nmasks,state=toJSON nids}),nitems,vst)
		= (Ok (NoChange,CompoundMask {fields=masks,state=toJSON ids}),items,vst)
	where
		swap []	  _		= []
		swap list index
			| index == 0 			= list //prevent move first element up
			| index >= length list 	= list //prevent move last element down
			| otherwise				
				# f = list !! (index-1)
				# l = list !! (index)
				= updateAt (index-1) l (updateAt index f list)

	//Edits inside the list
	onEdit dp ([id:tp],e) items (CompoundMask {fields=masks,state}) vst
		# ids = fromMaybe [] (fromJSON state)
		# index = itemIndex id ids
		| index < 0 || index >= length items = (Error "List edit out of bounds",items,vst)
		| otherwise
			= case itemEditor.Editor.onEdit (dp ++ [id]) (tp,e) (items !! index) (masks !! index) vst of
				(Error e,nx,vst) 
					= (Error e, items,vst)
				(Ok (change,nm),nx,vst)
					= (Ok (childChange index change,CompoundMask {fields=updateAt index nm masks,state=state}), (updateAt index nx items),vst)
	where
		childChange i NoChange = NoChange
		childChange i change = ChangeUI [] [(i,ChangeChild (ChangeUI [] [(0,ChangeChild change)]))]

	//Very crude full replacement
	onRefresh dp new old mask vst = (Ok (NoChange,mask),old,vst) //TODO: Determine small UI change
/*
	onRefresh dp new old mask vst = case genUI dp new {vst & mode = Update} of
		(Ok (ui,mask),vst) = (Ok (ReplaceUI ui,mask),new,vst)
		(Error e,vst) = (Error e,old,vst)
*/

	nextId [] = 0
	nextId ids = maxList ids + 1

	itemIndex id ids = itemIndex` 0 id ids
	where
		itemIndex` _ _ [] = -1
		itemIndex` i id [x:xs] = if (id == x) i (itemIndex` (i + 1) id xs)
