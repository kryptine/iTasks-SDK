implementation module iTasks.UI.Editor.Common

import StdBool
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
			# items = if (not (mode =: View) && (remove || reorder)) [listItemUI (length val) idx dx \\ dx <- items & idx <- [0..]] items
			//Add the add button
			# items = if (not (mode =: View) && add =: Just _) (items ++ [addItemControl val]) items
			= (Ok (uic UIContainer items,CompoundMask masks), vst)
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

		listItemUI numItems idx item
			# buttons	= (if reorder
							  [uia UIButton ('DM'.unions [iconClsAttr "icon-up", enabledAttr (idx <> 0), editAttrs taskId (editorId dp) (Just (JSONString ("mup_" +++ toString idx)))])
							  ,uia UIButton ('DM'.unions [iconClsAttr "icon-down", enabledAttr (idx <> numItems - 1), editAttrs taskId (editorId dp) (Just (JSONString ("mdn_" +++ toString idx)))])
							  ] []) ++
							  (if remove
							  [uia UIButton ('DM'.unions [iconClsAttr "icon-remove",editAttrs taskId (editorId dp) (Just (JSONString ("rem_" +++ toString idx)))])
							  ] [])
			# attr = 'DM'.unions [halignAttr AlignRight,heightAttr WrapSize,directionAttr Horizontal]
			= uiac UIContainer attr (if (reorder || remove) ([item] ++ buttons) [item])
			
	//Structural edits on the list
	onEdit dp ([],JSONString e) items (CompoundMask masks) vst
		# split = split "_" e
		# index = toInt (last split)
		# (items,masks) = case hd split of	
			"mup" = if reorder (swap items index,swap masks index) (items,masks)
			"mdn" = if reorder (swap items (index+1),swap masks (index+1)) (items,masks)
			"rem" = if remove  (removeAt index items,removeAt index masks)(items,masks)
			"add" = maybe (items,masks) (\f -> (insertAt (length items) (f items) items, insertAt (length items) newFieldMask masks)) add
			_     = (items,masks)
		= (Ok (NoChange,CompoundMask masks),items,vst)
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
	onEdit dp ([i:tp],e) items (CompoundMask masks) vst
		| i < 0 || i >= length items 	= (Error "List edit out of bounds",items,vst)
		| otherwise
			= case itemEditor.Editor.onEdit (dp ++ [i]) (tp,e) (items !! i) (masks !! i) vst of
				(Error e,nx,vst) 
					= (Error e, items,vst)
				(Ok (change,nm),nx,vst)
					= (Ok (childChange i change,CompoundMask (updateAt i nm masks)), (updateAt i nx items),vst)
	where
		childChange i NoChange = NoChange
		childChange i change = ChangeUI [] [(i,ChangeChild change)]

	//Very crude full replacement
	onRefresh dp new old mask vst = case genUI dp new vst of
		(Ok (ui,mask),vst) = (Ok (ReplaceUI ui,mask),new,vst)
		(Error e,vst) = (Error e,old,vst)


