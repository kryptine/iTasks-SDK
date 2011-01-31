implementation module GenVisualize

import StdBool, StdChar, StdList, StdArray, StdTuple, StdMisc, StdMaybe, StdGeneric, StdEnum, StdFunc
import GenUpdate, GenVerify, TUIDiff, Util, Text, Html, JSON, TUIDefinition, Types

NEWLINE	:== "\n"		//The character sequence to use for new lines in text display visualization

mkVSt :: *VSt
mkVSt = {VSt| origVizType = VTextDisplay, vizType = VTextDisplay, idPrefix = "", currentPath = startDataPath, label = Nothing, 
		useLabels = False, selectedConsIndex = -1, optional = False, renderAsStatic = False, updateMask = [], verifyMask = [], updates = []}

//Wrapper functions
visualizeAsEditor :: String a UpdateMask VerifyMask -> [TUIDef] | gVisualize{|*|} a
visualizeAsEditor name x umask vmask
	# vst = {mkVSt & origVizType = VEditorDefinition, vizType  = VEditorDefinition, idPrefix = name, updateMask = [umask], verifyMask = [vmask]}
	# (defs,vst)	= gVisualize{|*|} (Just x) vst
	= coerceToTUIDefs defs	

visualizeAsHtmlDisplay :: a -> [HtmlTag] | gVisualize{|*|} a
visualizeAsHtmlDisplay x = flatten (coerceToHtml (fst (gVisualize{|*|} (Just x) {mkVSt & origVizType = VHtmlDisplay, vizType = VHtmlDisplay})))

visualizeAsTextDisplay :: a -> String | gVisualize{|*|} a
visualizeAsTextDisplay x = join " " (coerceToStrings (fst (gVisualize{|*|} (Just x) {mkVSt & origVizType = VTextDisplay, vizType = VTextDisplay})))

visualizeAsHtmlLabel :: a -> [HtmlTag] | gVisualize{|*|} a
visualizeAsHtmlLabel x =  flatten (coerceToHtml (fst (gVisualize{|*|} (Just x) {mkVSt & origVizType = VHtmlLabel, vizType = VHtmlLabel})))
	
visualizeAsTextLabel :: a -> String | gVisualize{|*|} a
visualizeAsTextLabel x = join " " (coerceToStrings (fst (gVisualize{|*|} (Just x) {mkVSt & origVizType = VTextLabel, vizType = VTextLabel})))

determineEditorUpdates :: String (a, UpdateMask, VerifyMask) (a, UpdateMask, VerifyMask) -> [TUIUpdate]	| gVisualize{|*|} a
determineEditorUpdates name (oval, oumask, ovmask) (nval, numask, nvmask)
	# oldviz = visualizeAsEditor name oval oumask ovmask
	# newviz = visualizeAsEditor name nval numask nvmask
	= diffEditorDefinitions startDataPath (hd oldviz) (hd newviz) 

//IDEAS:
// - ConstructorControl, should in same cases be a constructor container
// - HtmlContainer should really be an html control
// - Update instructions should be identified by path to make shuffles possible

//Generic visualizer
generic gVisualize a :: (Maybe a) *VSt -> ([Visualization], *VSt)

gVisualize{|UNIT|} _ vst
	= ([],vst)

gVisualize{|PAIR|} fx fy val vst
	# (x,y)		= case val of (Just (PAIR x y)) = (Just x, Just y) ; _ = (Nothing,Nothing)	
	# (vizx, vst)	= fx x vst
	# (vizy, vst)	= fy y vst
	= (vizx ++ vizy, vst)

gVisualize{|EITHER|} fx fy val vst
	= case val of
		Nothing				= fx Nothing vst
		Just (LEFT x)		= fx (Just x) vst
		Just (RIGHT y)	= fy (Just y) vst

gVisualize{|OBJECT of d|} fx val vst=:{vizType,idPrefix,label,currentPath,selectedConsIndex = oldSelectedConsIndex,useLabels,optional,renderAsStatic,updateMask,verifyMask}
	//For objects we only peek at the update & verify mask, but don't take it out of the state yet.
	//The masks are removed from the states when processing the CONS.
	# (cmu,_) = popMask updateMask
	# (cmv,_) = popMask verifyMask
	//ADT's with multiple constructors: Add the creation/updating of a control for choosing the constructor
	| d.gtd_num_conses > 1
		# x = case val of (Just (OBJECT x)) = (Just x); _ = Nothing
		= case vizType of 
			VEditorDefinition
				# (err,hnt)	= verifyElementStr cmu cmv
				# (items, vst=:{selectedConsIndex}) = fx x {vst & useLabels = False, optional = False}
				= ([TUIFragment (TUIConstructorControl {TUIConstructorControl
														| id = id
														, name = dp2s currentPath
														, fieldLabel = labelAttr useLabels label
														, consSelIdx = case cmu of (Touched _ _) = selectedConsIndex; _ = -1
														, consValues = [gdc.gcd_name \\ gdc <- d.gtd_conses]
														, items = case cmu of (Touched _ _) = (coerceToTUIDefs items); _ = []
														, staticDisplay = renderAsStatic
														, errorMsg = err
														, hintMsg = hnt
														})]
				  ,{VSt | vst & currentPath = stepDataPath currentPath, selectedConsIndex = oldSelectedConsIndex, useLabels = useLabels, optional = optional})					
			_
				# (viz,vst) = fx x vst
				= (viz,{VSt | vst & currentPath = stepDataPath currentPath})
	//Everything else, just strip of the OBJECT constructor and pass through
	| otherwise
		= case val of
			Just (OBJECT x)	= fx (Just x) vst
			Nothing			= fx Nothing vst			
where
	id		= dp2id idPrefix currentPath
	cId 	= (dp2id idPrefix currentPath) +++ "c"

gVisualize{|CONS of d|} fx val vst=:{vizType,idPrefix,currentPath,label,useLabels,optional,verifyMask,updateMask,renderAsStatic}
	# (cmu,um)	= popMask updateMask
	# (cmv,vm)	= popMask verifyMask
	# x			= case val of (Just (CONS x)) = (Just x); _ = Nothing
	= case vizType of
		VEditorDefinition	
			//records
			| not (isEmpty d.gcd_fields)
				# (err,hnt) = case cmu of
							(Untouched) = case cmv of
								(VMInvalid IsBlankError _)		= ("", "")
								(VMInvalid (ErrorMessage s) _)	= (s, "")
								(VMValid mbHnt _) 				= ("", toString mbHnt)
								(VMUntouched mbHnt _ _)			= ("", toString mbHnt)
							_				= case cmv of
								(VMInvalid err _) 				= (toString err, "")
								(VMValid mbHnt _)				= ("", toString mbHnt)
								(VMUntouched mbHnt _ _)			= ("", toString mbHnt)
				= case x of 
					//Create an empty record container that can be expanded later
					Nothing 
						= ([TUIFragment (TUIRecordContainer  {TUIRecordContainer | id = (dp2id idPrefix currentPath) +++ "-fs"
																				 , name = dp2s currentPath
																				 , title = label
																				 , items = []
																				 , optional = (optional && (not renderAsStatic))
																				 , hasValue = False
																				 , errorMsg = err
																				 , hintMsg = hnt})]
						  , {VSt|vst & currentPath = stepDataPath currentPath, optional = optional, updateMask = um, verifyMask = vm})
					_
						# (viz,vst) = fx x {VSt | vst & currentPath = shiftDataPath currentPath, useLabels = True, optional = False
													, updateMask = childMasks cmu, verifyMask = childMasks cmv}
						= ([TUIFragment (TUIRecordContainer {TUIRecordContainer | id = (dp2id idPrefix currentPath) +++ "-fs"
																				, name = dp2s currentPath
																				, title = label
																				, items = coerceToTUIDefs viz
																				, optional = (optional && (not renderAsStatic))
																				, hasValue = True
																				, errorMsg = err
																				, hintMsg = hnt})]
						 , {VSt|vst & currentPath = stepDataPath currentPath, optional = optional, useLabels = useLabels, updateMask = um, verifyMask = vm})
			
			//ADT's with multiple fields are essentially tuples
			| otherwise		
				# (viz,vst) = fx x {VSt | vst & currentPath = shiftDataPath currentPath, useLabels = False, updateMask = childMasks cmu, verifyMask = childMasks cmv}
				= (viz, {VSt | vst & currentPath = stepDataPath currentPath, optional = optional, selectedConsIndex= d.gcd_index, useLabels = useLabels, updateMask = um, verifyMask = vm})
		//Html display vizualization
		VHtmlDisplay
			= case val of
				Just (CONS x)
					# (viz,vst) = fx (Just x)
									{VSt | vst & label = Nothing, currentPath = shiftDataPath currentPath, updateMask = childMasks cmu, verifyMask = childMasks cmv}
					//Records
					| not (isEmpty d.gcd_fields) 
						= ([HtmlFragment [TableTag [] (flatten (coerceToHtml viz))]], {VSt|vst & currentPath = stepDataPath currentPath, updateMask = um, verifyMask = vm})
					//When there are multiple constructors, also show the name of the constructor
					| d.gcd_type_def.gtd_num_conses > 1
						= ([TextFragment d.gcd_name, TextFragment " " :viz], {VSt|vst & currentPath = stepDataPath currentPath, updateMask = um, verifyMask = vm})
					| otherwise
						= (viz, {VSt|vst & currentPath = stepDataPath currentPath, updateMask = um, verifyMask = vm})
				_
					= ([],{VSt|vst & currentPath = stepDataPath currentPath, updateMask = um, verifyMask = vm})
		//Other visualizations
		VHtmlLabel
			# (viz,vst) = fx x {VSt | vst & currentPath = shiftDataPath currentPath, updateMask = childMasks cmu, verifyMask = childMasks cmv}
			//For records only show the first field
			| not (isEmpty d.gcd_fields) 
				= ([hd viz], {VSt|vst & currentPath = stepDataPath currentPath, updateMask = um, verifyMask = vm})
			//When there are multiple constructors, also show the name of the constructor
			| d.gcd_type_def.gtd_num_conses > 1
				= ([TextFragment d.gcd_name,TextFragment " " :viz],{VSt|vst & currentPath = stepDataPath currentPath, updateMask = um, verifyMask = vm})
			| otherwise
				= (viz, {VSt|vst & currentPath = stepDataPath currentPath, updateMask = um, verifyMask = vm})
		VTextLabel
			//For records only show the first field
			# (viz,vst) = fx x {VSt | vst & currentPath = shiftDataPath currentPath, updateMask = childMasks cmu, verifyMask = childMasks cmv}
			| not (isEmpty d.gcd_fields) 
				= ([hd viz], {VSt|vst & currentPath = stepDataPath currentPath, updateMask = um, verifyMask = vm})
			//When there are multiple constructors, also show the name of the constructor
			| d.gcd_type_def.gtd_num_conses > 1
				= ([TextFragment d.gcd_name,TextFragment " " :viz],{VSt|vst & currentPath = stepDataPath currentPath, updateMask = um, verifyMask = vm})
			| otherwise
				= (viz, {VSt|vst & currentPath = stepDataPath currentPath, updateMask = um, verifyMask = vm})		
		VTextDisplay
			# (viz,vst) = fx x {VSt | vst & currentPath = shiftDataPath currentPath, updateMask = childMasks cmu, verifyMask = childMasks cmv}
			| not (isEmpty d.gcd_fields) 
				= (viz, {VSt|vst & currentPath = stepDataPath currentPath, updateMask = um, verifyMask = vm})
			//When there are multiple constructors, also show the name of the constructor
			| d.gcd_type_def.gtd_num_conses > 1
				= ([TextFragment d.gcd_name,TextFragment " " :viz],{VSt|vst & currentPath = stepDataPath currentPath, updateMask = um, verifyMask = vm})
			| otherwise
				= (viz, {VSt|vst & currentPath = stepDataPath currentPath, updateMask = um, verifyMask = vm})
		_	
			# (viz,vst) = fx x {VSt | vst & label = Nothing, currentPath = shiftDataPath currentPath, updateMask = childMasks cmu, verifyMask = childMasks cmv}
			= (viz,{VSt|vst & currentPath = stepDataPath currentPath, updateMask = um, verifyMask = vm})
where	
	id		= (dp2id idPrefix currentPath)
	fsid	= id +++ "-fs"
	cId		= id +++ "c"
	
gVisualize{|FIELD of d|} fx val vst=:{vizType,currentPath}
	# x = case val of (Just (FIELD x)) = (Just x) ; _ = Nothing
	= case vizType of
		VHtmlDisplay
			# (vizBody,vst) 	= fx x {VSt |vst & label = Nothing}
			= case vizBody of
			 [] = ([],vst)
			 _  = ([HtmlFragment [TrTag [] [ThTag [] [Text (formatLabel d.gfd_name),Text ": "],TdTag [] (flatten (coerceToHtml vizBody))]]],{VSt | vst & label = Nothing})
		VTextDisplay
			# (vizBody,vst) 	= fx x {VSt |vst & label = Just (formatLabel d.gfd_name)}
			= ([TextFragment (formatLabel d.gfd_name),TextFragment ": " : vizBody]++[TextFragment " "], {VSt | vst & label = Nothing})
		_
			# (vizBody,vst)		= fx x {VSt |vst & label = Just (formatLabel d.gfd_name)}
			= (vizBody, {VSt | vst & label = Nothing})
			
//***
gVisualize{|Int|} val vst=:{VSt | vizType,currentPath}
	= case vizType of
		VEditorDefinition
			# (ctl,vst) = visualizeBasicControl val vst
			= ([TUIFragment (TUIIntControl ctl)],vst)
		_	
			= ([TextFragment (toString val)], {VSt|vst & currentPath = stepDataPath currentPath})
		
gVisualize{|Real|}val vst=:{VSt | vizType,currentPath,updateMask}
	= case vizType of
		VEditorDefinition	
			# (ctl,vst) = visualizeBasicControl val vst
			= ([TUIFragment (TUIRealControl ctl)],vst)
		_					
			= ([TextFragment (toString val)], {VSt|vst & currentPath = stepDataPath currentPath})
		
gVisualize{|Char|} val vst=:{VSt | vizType,currentPath,updateMask}
	= case vizType of
		VEditorDefinition	
			# (ctl,vst) = visualizeBasicControl val vst
			= ([TUIFragment (TUICharControl ctl)],vst)
		_		
			= ([TextFragment (toString val)], {VSt|vst & currentPath = stepDataPath currentPath})

gVisualize{|String|} val vst=:{VSt | vizType,currentPath,updateMask}
	= case vizType of
		VEditorDefinition	
			# (ctl,vst) = visualizeBasicControl val vst
			= ([TUIFragment (TUIStringControl ctl)],vst)
		_			
			=	([TextFragment (toString val)]
				, {VSt|vst & currentPath = stepDataPath currentPath})

gVisualize{|Bool|} val vst=:{VSt | vizType,currentPath}
	= case vizType of
		VEditorDefinition
			# (ctl,vst) = visualizeBasicControl val vst	
			= ([TUIFragment (TUIBoolControl ctl)],vst)
		VHtmlDisplay
			= ([HtmlFragment [DivTag [ClassAttr ("bool-htmllabel-icon bool-htmllabel-icon-"+++(toLowerCase (toString val)))] [SpanTag [ClassAttr "bool-htmllabel-text"] [(Text (toString val))]]]]
				, {VSt|vst & currentPath = stepDataPath currentPath})
		VHtmlLabel
			= ([HtmlFragment [DivTag [ClassAttr ("bool-htmllabel-icon bool-htmllabel-icon-"+++(toLowerCase (toString val)))] [SpanTag [ClassAttr "bool-htmllabel-text"] [(Text (toString val))]]]]
				, {VSt|vst & currentPath = stepDataPath currentPath})
		_	
			= ([TextFragment (toString val)]
				, {VSt|vst & currentPath = stepDataPath currentPath})		
	
gVisualize{|Maybe|} fx val vst=:{vizType,idPrefix,currentPath,optional}
	= case vizType of
		VEditorDefinition
			= case val of
				Just (Just x)
					# (viz, vst) = fx (Just x) {VSt|vst & optional = True}
					= (viz, {VSt|vst & optional = optional, currentPath = stepDataPath currentPath})
				_
					# (viz, vst) = fx Nothing {VSt|vst & optional = True}
					= (viz, {VSt|vst & optional = optional, currentPath = stepDataPath currentPath})
		_			
			= case val of
				(Just Nothing)	= ([TextFragment "-"],vst)
				(Just (Just x))	= fx (Just x) vst
				Nothing				= ([],vst)

gVisualize{|Dynamic|} val vst
	= ([],vst)

gVisualize{|(,)|} f1 f2 val vst=:{vizType,idPrefix,currentPath,useLabels,label,optional,updateMask,verifyMask}
	# (cmu,um)	= popMask updateMask
	# (cmv,vm)	= popMask verifyMask
	# (v1,v2)	= case val of (Just (o1,o2)) = (Just o1, Just o2) ; _ = (Nothing,Nothing)
	= case vizType of
		VEditorDefinition
			# (viz1,vst) = f1 v1 {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing, updateMask = childMasks cmu, verifyMask = childMasks cmv}
			# (viz2,vst) = f2 v2 vst
			= ([TUIFragment (TUITupleContainer {TUITupleContainer | id=dp2id idPrefix currentPath, fieldLabel = label, optional = optional, items = map coerceToTUIDefs [viz1,viz2]})]		 
			  , {VSt|vst & currentPath = stepDataPath currentPath, useLabels = useLabels, updateMask = um, verifyMask = vm})		
		_
			# (viz1,vst) = f1 v1 {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing, updateMask = childMasks cmu, verifyMask = childMasks cmv}
			# (viz2,vst) = f2 v2 vst
			= (viz1 ++ separator viz2 ++ viz2,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = useLabels, updateMask = um, verifyMask = vm})
where
	separator v = case v of
		[] = []
		_
		   = case vizType of
				VHtmlDisplay	= []
				_				= [TextFragment ", "]

gVisualize{|(,,)|} f1 f2 f3 val vst=:{vizType,idPrefix,currentPath,useLabels, label,optional,updateMask,verifyMask}
	# (cmu,um)		= popMask updateMask
	# (cmv,vm)		= popMask verifyMask
	# (v1,v2,v3)	= case val of (Just (v1,v2,v3)) = (Just v1, Just v2, Just v3) ; _ = (Nothing,Nothing,Nothing)
	= case vizType of
		VEditorDefinition
			# oldLabels = useLabels
			# (viz1,vst) = f1 v1 {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing, updateMask = childMasks cmu, verifyMask = childMasks cmv}
			# (viz2,vst) = f2 v2 vst
			# (viz3,vst) = f3 v3 vst
			= ([TUIFragment (TUITupleContainer {TUITupleContainer | id=dp2id idPrefix currentPath, fieldLabel = label, optional = optional, items = map coerceToTUIDefs [viz1,viz2,viz3]})]			 
			  , {VSt|vst & currentPath = stepDataPath currentPath, useLabels=oldLabels, updateMask = um, verifyMask = vm})		
		_
			# (viz1,vst) = f1 v1 {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing, updateMask = childMasks cmu, verifyMask = childMasks cmv}
			# (viz2,vst) = f2 v2 vst
			# (viz3,vst) = f3 v3 vst
			= (viz1 ++ separator viz2 ++ viz2 ++ separator viz3 ++ viz3,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = useLabels, updateMask = um, verifyMask = vm})
where
	separator v = case v of
		[] = []
		_
		   = case vizType of
				VHtmlDisplay	= []
				_				= [TextFragment ", "]

gVisualize{|(,,,)|} f1 f2 f3 f4 val vst=:{vizType,idPrefix,currentPath,useLabels, label,optional,updateMask,verifyMask}
	# oldLabels		= useLabels
	# (cmu,um)		= popMask updateMask
	# (cmv,vm)		= popMask verifyMask
	# (v1,v2,v3,v4)	= case val of (Just (v1,v2,v3,v4)) = (Just v1, Just v2, Just v3,Just v4) ; _ = (Nothing,Nothing,Nothing,Nothing)
	= case vizType of
		VEditorDefinition
			# (viz1,vst) = f1 v1 {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing, updateMask = childMasks cmu, verifyMask = childMasks cmv}
			# (viz2,vst) = f2 v2 vst
			# (viz3,vst) = f3 v3 vst
			# (viz4,vst) = f4 v4 vst
			= ([TUIFragment (TUITupleContainer {TUITupleContainer | id=dp2id idPrefix currentPath, fieldLabel = label, optional = optional, items = map coerceToTUIDefs [viz1,viz2,viz3,viz4]})]			 
			  , {VSt|vst & currentPath = stepDataPath currentPath, useLabels = oldLabels, updateMask = um, verifyMask = vm})		
		_
			# (viz1,vst)	= f1 v1 {VSt| vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing, updateMask = childMasks cmu, verifyMask = childMasks cmv}
			# (viz2,vst)	= f2 v2 vst
			# (viz3,vst)	= f3 v3 vst
			# (viz4,vst)	= f4 v4 vst
			= (viz1 ++ separator viz2 ++ viz2 ++ separator viz3 ++ viz3 ++ separator viz4 ++ viz4,{VSt|vst & currentPath = stepDataPath currentPath, useLabels = useLabels, updateMask = um, verifyMask = vm})
where
	separator v = case v of
		[] = []
		_
		   = case vizType of
				VHtmlDisplay	= []
				_				= [TextFragment ", "]

gVisualize {|[]|} fx val vst=:{vizType,idPrefix,currentPath,useLabels,label,optional,renderAsStatic,updateMask,verifyMask}
	# (cmu, um) = popMask updateMask
	# (cmv, vm) = popMask verifyMask
	# x			= case val of (Just xl) = xl; _ = []
	= case vizType of
		VEditorDefinition
			# (err,hnt) = verifyElementStr cmu cmv 
			# (items,vst) = TUIDef fx x 0 {VSt | vst & currentPath = shiftDataPath currentPath, useLabels = False, label = Nothing, updateMask = childMasks cmu, verifyMask = childMasks cmv}
			= ([TUIFragment (TUIListContainer {TUIListContainer | items = items, optional = optional, name = name, id = id, fieldLabel = label, hideLabel = not useLabels, staticDisplay = renderAsStatic, errorMsg = err, hintMsg = hnt})],
			  {VSt | vst & currentPath = stepDataPath currentPath, updateMask = um, verifyMask = vm, useLabels = useLabels})	
		VHtmlDisplay
			= case x of
				[] 
					= ([HtmlFragment [UlTag [] [LiTag [ClassAttr "list-item-light"] [(Text "Empty list")]]]],{VSt | vst & currentPath = stepDataPath currentPath})
				_		
					# (items,vst) = staticDef fx x {VSt | vst & currentPath = shiftDataPath currentPath}
					= ([HtmlFragment [UlTag [] [(LiTag [ClassAttr (itemCls i)] (flatten (coerceToHtml x))) \\ x <- items & i <- [0..]]]],{VSt | vst & currentPath = stepDataPath currentPath})
		VTextDisplay
			= case x of
				[]
					= ([TextFragment "[]"],{VSt | vst & currentPath = stepDataPath currentPath})
				_
					# (items,vst) = staticDef fx x {VSt | vst & currentPath = shiftDataPath currentPath}
					= ([TextFragment ("["+++join ", " (flatten  [(coerceToStrings x) \\ x <-items])+++"]")],{VSt | vst & currentPath = stepDataPath currentPath})
		VHtmlLabel
			= case x of
				[] = ([HtmlFragment [(Text "Empty list")]],{VSt | vst & currentPath = stepDataPath currentPath})
				_ 
					# (items,vst) = staticDef fx x {VSt | vst & currentPath = shiftDataPath currentPath}
					= ([HtmlFragment (htmlLabel items)],{VSt | vst & currentPath = stepDataPath currentPath})
		_
			= ([],
			  {VSt | vst & currentPath = stepDataPath currentPath})
		
where
	id 			= dp2id idPrefix currentPath
	name 		= dp2s currentPath
	itemId idx 	= id+++"#"+++toString idx

	itemCls i
		| isEven i  = "list-item-light"
		| otherwise = "list-item-dark"

	TUIDef fx [] idx vst=:{VSt | optional}
		| renderAsStatic = ([],vst)
		| otherwise
			# (dx,vst)  = fx Nothing {VSt | vst & optional = True}
			= ([TUIListItemControl {TUIListItemControl | name = name, id=itemId idx, index = idx, items = coerceToTUIDefs dx}],{VSt | vst & optional = optional})
	TUIDef fx [x:xs] idx vst
		# (dx, vst) = fx (Just x) {VSt | vst & optional = False}
		# (dxs,vst) = TUIDef fx xs (inc idx) {VSt | vst & optional = optional}
		= ([TUIListItemControl {TUIListItemControl | name = name, id=itemId idx, index = idx, items = coerceToTUIDefs dx}:dxs],vst)
			
	staticDef fx []     vst = ([],vst)
	staticDef fx [x:xs] vst
		# (hx, vst) = fx (Just x) vst
		# (hxs,vst) = staticDef fx xs vst
		= ([hx:hxs],vst)
	
	htmlLabel [i] = (flatten (coerceToHtml i))
	htmlLabel [i:is] = (flatten (coerceToHtml i)) ++ [(Text ", ")] ++ htmlLabel is

//Functions (Don't visualize)
gVisualize{|(->)|} fx fy val vst=:{VSt | currentPath, updateMask, verifyMask}
	# (cmu,um) = popMask updateMask
	# (cmv,vm) = popMask verifyMask	
	= ([],{VSt | vst & currentPath = stepDataPath currentPath, updateMask = um, verifyMask = vm})
	
//Hidden type
gVisualize{|Hidden|} fx val vst=:{VSt | currentPath, updateMask, verifyMask}
	# (cmu,um) = popMask updateMask
	# (cmv,vm) = popMask verifyMask	
	= ([],{VSt | vst & currentPath = stepDataPath currentPath, updateMask = um, verifyMask = vm})

gVisualize{|Display|} fx val vst=:{VSt | origVizType, vizType, currentPath, renderAsStatic}
	# x	= case val of (Just (Display x)) = (Just x); _ = Nothing
	= case origVizType of
		VHtmlDisplay
			# (def,vst) = fx x vst
			= (def,{VSt | vst & currentPath = stepDataPath currentPath})
		_
			# (def,vst) = fx x {VSt | vst &  renderAsStatic = True}
			= (def,{VSt | vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})

gVisualize{|Editable|} fx val vst=:{VSt | vizType, currentPath, renderAsStatic}
	# x	= case val of (Just (Editable x)) = (Just x); _ = Nothing
	# (def,vst) = fx x {VSt | vst & renderAsStatic = False}
	= (def,{VSt | vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})

gVisualize{|VisualizationHint|} fx val vst=:{VSt | idPrefix, vizType, origVizType, currentPath, renderAsStatic,updateMask,verifyMask}
	# x	= case val of (Just (VHEditable x)) = (Just x); (Just (VHDisplay x)) = (Just x); _ = Nothing
	= case origVizType of
		VHtmlDisplay
			= case val of
				(Just (VHHidden _)) 
					# (cmu,um) = popMask updateMask
					# (cmv,vm) = popMask verifyMask	
					= ([],{VSt | vst & currentPath = stepDataPath currentPath, updateMask = um, verifyMask = vm})
				(Just (VHDisplay _))
					# (viz,vst) = fx x {vst & vizType = VHtmlDisplay}
					= (viz,{vst & currentPath = stepDataPath currentPath, vizType = vizType})
				_
					# (viz,vst) = fx x {vst & vizType = VHtmlDisplay}
					= (viz,{vst & currentPath = stepDataPath currentPath, vizType = vizType})
		VEditorDefinition
			= case val of
				(Just (VHHidden _))
					# (cmu,um) = popMask updateMask
					# (cmv,vm) = popMask verifyMask	
					= ([TUIFragment (TUIHiddenControl {TUIBasicControl | name = dp2s currentPath, id = dp2id idPrefix currentPath, value = "", fieldLabel = Nothing, staticDisplay = False, optional = True, errorMsg = "", hintMsg = ""})]
					  ,{VSt | vst & currentPath = stepDataPath currentPath, updateMask = um, verifyMask = vm})
				(Just (VHDisplay _))
					# (viz,vst) = fx x {vst & renderAsStatic = True}
					= (viz,{vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})
				(Just (VHEditable _))
					# (viz,vst) = fx x {vst & renderAsStatic = False}
					= (viz,{vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})			
		_
			= case val of
				(Just (VHHidden _))
					# (cmu,um) = popMask updateMask
					# (cmv,vm) = popMask verifyMask	
					= ([],{VSt | vst & currentPath = stepDataPath currentPath, updateMask = um, verifyMask = vm})
				(Just (VHDisplay _))
					# (viz,vst) = fx x {vst & renderAsStatic = True}
					= (viz,{vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})
				(Just (VHEditable _))
					# (viz,vst) = fx x {vst & renderAsStatic = False}
					= (viz,{vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})			

gVisualize{|Password|} val vst=:{VSt | vizType,currentPath}
	= case vizType of
		VEditorDefinition	
			# (ctl,vst) = visualizeBasicControl val vst
			= ([TUIFragment (TUIPasswordControl ctl)],vst)
		_					
			= ([TextFragment ("********")],{VSt | vst & currentPath = stepDataPath currentPath})

gVisualize{|Note|} val vst=:{vizType,label,idPrefix,currentPath,useLabels,optional,renderAsStatic,verifyMask,updateMask,updates}
	= case vizType of
		VEditorDefinition	
			# (ctl,vst) = visualizeBasicControl val vst
			= ([TUIFragment (TUINoteControl ctl)],vst)
		_					
			= ([HtmlFragment (flatten [[Text line,BrTag []] \\ line <- split "\n" (toString val)])]
					, {VSt|vst & currentPath = stepDataPath currentPath})

gVisualize{|Date|} val vst=:{VSt | vizType,currentPath}
	= case vizType of
		VEditorDefinition	
			# (ctl,vst) = visualizeBasicControl val vst
			= ([TUIFragment (TUIDateControl ctl)],vst)
		_					
			= ([TextFragment (toString val)],{VSt|vst & currentPath = stepDataPath currentPath})

gVisualize{|Time|} val vst=:{VSt | vizType,currentPath,updateMask,idPrefix}
	= case vizType of
		VEditorDefinition	
			# (ctl,vst) = visualizeBasicControl val vst
			= ([TUIFragment (TUITimeControl ctl)],vst)
		_					
			= ([TextFragment (toString val)],{VSt|vst & currentPath = stepDataPath currentPath})

gVisualize {|Document|} val vst=:{vizType, label, idPrefix, currentPath, optional, useLabels,renderAsStatic, updates, updateMask, verifyMask}
	# x	= case val of (Just x) = x; _ = {Document|documentId = "",name = "", mime = "", size = 0}
	= case vizType of
		VEditorDefinition
			# (cmu,um) = popMask updateMask
			# (cmv,vm) = popMask verifyMask
			# (err,hnt) = verifyElementStr cmu cmv
			= ([TUIFragment (TUIDocumentControl {TUIDocumentControl |id = id, name = dp2s currentPath, document = x, fieldLabel = label, optional = optional, staticDisplay = renderAsStatic, errorMsg = err, hintMsg = hnt})],
			  {VSt | vst & currentPath = stepDataPath currentPath, updateMask = um, verifyMask = vm})
		VHtmlDisplay
			= case val of
				(Nothing) = noDocument vst
				(Just document)
					| document.Document.size == 0
						= noDocument vst
					| otherwise
						# downLink = ATag [HrefAttr (buildLink document)
										  ,TargetAttr "_blank",IdAttr id
										  ,NameAttr "x-form-document-link"] [ImgTag [SrcAttr "skins/default/img/icons/page_white_put.png"]]
						# prevLink = ATag [HrefAttr "#", IdAttr id
										  ,NameAttr "x-form-document-preview-link" ] [ImgTag [SrcAttr "skins/default/img/icons/zoom.png"]]			
						= ([HtmlFragment [(Text ( document.Document.name +++" ("+++printByteSize document.Document.size+++") ")),RawText "&nbsp;",downLink,prevLink]],
					  		{VSt | vst & currentPath = stepDataPath currentPath})
		VTextDisplay
			= case val of
				(Nothing) = noDocument vst
				(Just document)
					| document.Document.size == 0 = noDocument vst
					| otherwise	= ([TextFragment document.Document.name],{VSt | vst & currentPath = stepDataPath currentPath})
where
	id = dp2id idPrefix currentPath
	fixReal r = (toReal (toInt (r*100.0)))/100.0
	
	printByteSize size
		| size >= 1048576 = toString (fixReal ((toReal size)/(toReal 1048576)))+++" Mbyte"
		| size >= 1024    = toString (fixReal ((toReal size)/(toReal 1024)))+++" Kbyte"
		| otherwise 	  = toString size +++ " byte"
	
	noDocument vst = ([TextFragment "No Document."],vst)
	buildLink document = "/services/json/documents/" +++ document.Document.documentId +++ "/download"

gVisualize{|FormButton|} val vst=:{vizType,label=fLabel,idPrefix,currentPath,useLabels,optional,renderAsStatic,updateMask,verifyMask,updates}
	# (cmu,um) = popMask updateMask
	# (cmv,vm) = popMask verifyMask
	= case vizType of
		VEditorDefinition
			# (err, hnt) = verifyElementStr cmu cmv
			= ([TUIFragment (TUIFormButtonControl {TUIButtonControl | label = label val, iconCls = icon val, name = dp2s currentPath, id = id, value = toString (pressed val), fieldLabel = labelAttr useLabels fLabel, optional = optional, staticDisplay = renderAsStatic, errorMsg = err, hintMsg = hnt})]
				, {VSt | vst & currentPath = stepDataPath currentPath, updateMask = um, verifyMask = vm})
		_
			= ([TextFragment (label val)]
				, {VSt | vst & currentPath = stepDataPath currentPath})
where
	id			= dp2id idPrefix currentPath
	
	label b		= case b of (Just b) = b.FormButton.label; _ = ""
	icon b		= case b of (Just b) = b.icon; _ = ""	
		
	pressed (Just b)	= case b.FormButton.state of
		Pressed		= True
		NotPressed	= False
	pressed Nothing		= False	

gVisualize{|Currency|} val vst=:{vizType,label,idPrefix,currentPath,useLabels,optional,renderAsStatic,verifyMask,updateMask,updates}
	# (cmu,um)	= popMask updateMask
	# (cmv,vm)	= popMask verifyMask
	# x			= value val cmu
	= case vizType of
		VEditorDefinition	
			# (err,hnt) = verifyElementStr cmu cmv
			= ([TUIFragment (TUICurrencyControl {TUICurrencyControl|id = id, name = dp2s currentPath
												, value = x, fieldLabel = labelAttr useLabels label
												, currencyLabel = curLabel val, optional = optional
												, staticDisplay = renderAsStatic
												, errorMsg = err, hintMsg = hnt})]
								, {VSt|vst & currentPath = stepDataPath currentPath, updateMask = um, verifyMask = vm})
		_					
			= ([TextFragment (toString val)], {VSt|vst & currentPath = stepDataPath currentPath})
where
	curLabel (Just (EUR _))	= "&euro;"
	curLabel (Just (GBP _))	= "&pound;"
	curLabel (Just (USD _))	= "$"
	curLabel (Just (JPY _)) = "&yen;"
	curLabel _				= "&euro;" //Use the default currency
	
	value Nothing um	= ""
	value (Just v) um	= case um of (Touched _ _) = (decFormat (toInt v)); _ = ""
	
	id = dp2id idPrefix currentPath

gVisualize{|User|} val vst=:{vizType,currentPath,updateMask}
	= case vizType of
		VEditorDefinition	
			# (ctl,vst) = visualizeBasicControl val vst
			= ([TUIFragment (TUIUserControl ctl)], vst)
		_					
			= ([TextFragment (toString val)]
				, {VSt|vst & currentPath = stepDataPath currentPath})

gVisualize{|Choice|} fx val vst=:{vizType,label,idPrefix,currentPath,useLabels,optional,renderAsStatic,verifyMask,updateMask,updates}
	# (cmu,um)	= popMask updateMask
	# (cmv,vm)	= popMask verifyMask
	# vst		= {VSt|vst & updateMask = um, verifyMask = vm}
	= case val of
		Nothing
			= ([TextFragment "Empty choice"],{VSt|vst & currentPath = stepDataPath currentPath})
		Just choice=:(Choice opts sel)
			= case vizType of
				VEditorDefinition
					# selection = case cmu of
						Touched _ _			= [sel]
						_					= []
					# (err,hnt)				= verifyElementStr cmu cmv
					# (optionLabels,vst)	= mkChoiceOptionLabels fx opts vst
					# id					= dp2id idPrefix currentPath
					= ([TUIFragment (TUIChoiceControl	{ TUIChoiceControl
														| name = id 					// names of checkbox groups have to be unique, ...
														, id = id
														, dataPath = dp2s currentPath	// ... so use data path field for events
														, fieldLabel = labelAttr useLabels label
														, optional = optional
														, staticDisplay = renderAsStatic
														, allowMultiple = False
														, options = optionLabels
														, selection = selection
														, errorMsg = err
														, hintMsg = hnt
														})]
					, {VSt|vst & currentPath = stepDataPath currentPath})
				_
					| sel >= 0 && sel < length opts
						# (vis,vst) = fx (Just (getChoice choice)) {VSt|vst & currentPath = shiftDataPath currentPath}
						= (vis,{VSt|vst & currentPath = stepDataPath currentPath})
					| otherwise
						= ([TextFragment "No item selected"],{VSt|vst & currentPath = stepDataPath currentPath})
	
gVisualize{|MultipleChoice|} fx val vst=:{vizType,label,idPrefix,currentPath,useLabels,optional,renderAsStatic,verifyMask,updateMask,updates}
	# (cmu,um)	= popMask updateMask
	# (cmv,vm)	= popMask verifyMask
	# vst		= {VSt|vst & updateMask = um, verifyMask = vm}
	= case val of
		Nothing
			= ([TextFragment "Empty multiple choice"],{VSt|vst & currentPath = stepDataPath currentPath})
		Just choice=:(MultipleChoice opts sel)
			= case vizType of
				VEditorDefinition	
					# (err,hnt)				= verifyElementStr cmu cmv
					# (optionLabels,vst)	= mkChoiceOptionLabels fx opts vst
					#id						= dp2id idPrefix currentPath
					= ([TUIFragment (TUIChoiceControl	{ TUIChoiceControl
														| name = id						// names of checkbox groups have to be unique,...
														, id = id
														, dataPath = dp2s currentPath	// ... so use data path field for events
														, fieldLabel = labelAttr useLabels label
														, staticDisplay = renderAsStatic
														, optional = optional
														, allowMultiple = True
														, options = optionLabels
														, selection = sel
														, errorMsg = err
														, hintMsg = hnt
														})]
					, {VSt|vst & currentPath = stepDataPath currentPath})
				_
					= case getChoices (fromJust val) of
						[]
							= ([TextFragment "No items selected"],{VSt|vst & currentPath = stepDataPath currentPath})
						choices
							# (vis,vst) = visualiseChoices choices [] {VSt | vst & currentPath = shiftDataPath currentPath}
							= (vis,{VSt|vst & currentPath = stepDataPath currentPath})
where	
	visualiseChoices [] acc vst = (acc,vst)
	visualiseChoices [choice:choices] acc vst
		# (vis,vst) = fx (Just choice) vst
		= visualiseChoices choices (acc ++ vis ++ (if (isEmpty choices) [] [TextFragment ", "])) vst

gVisualize{|Tree|} fx val vst=:{vizType,label,idPrefix,currentPath,useLabels,optional,renderAsStatic,verifyMask,updateMask,updates}
	# (cmu,um)	= popMask updateMask
	# (cmv,vm)	= popMask verifyMask
	# vst		= {VSt|vst & updateMask = um, verifyMask = vm}
	= case val of
		Nothing
			= ([TextFragment "Empty tree"],{VSt|vst & currentPath = stepDataPath currentPath})
		Just (Tree nodes sel)
			# vst = {vst & vizType = VTextLabel}
			# (tree,_,vst) = mkTree nodes 0 vst
			# vst = {vst & vizType = VEditorDefinition}
			= case vizType of
				VEditorDefinition
					# (err,hnt)				= verifyElementStr cmu cmv
					# id					= dp2id idPrefix currentPath
					= ([TUIFragment (TUITreeControl	{ TUITreeControl
													| name = dp2s currentPath
													, id = id
													, tuiTree = tree
													, selIndex = if (sel >= 0) (Just sel) Nothing
													, fieldLabel = labelAttr useLabels label
													, optional = optional
													, staticDisplay = renderAsStatic
													, errorMsg = err
													, hintMsg = hnt
													})]
					, {VSt|vst & currentPath = stepDataPath currentPath})
				_
					= ([TextFragment "tree"],{VSt|vst & currentPath = stepDataPath currentPath})
where
	mkTree [] idx vst
		= ([],idx,vst)
	mkTree [Node label nodes:r] idx vst
		# (children,idx,vst)	= mkTree nodes idx vst
		# (rtree,idx,vst)		= mkTree r idx vst
		= ([{id = Nothing, text = label, index = Nothing, leaf = False, children = Just children}:rtree],idx,vst)
	mkTree [Leaf v:r] idx vst
		# (leaf,vst)		= fx (Just v) vst
		# (rtree,idx`,vst)	= mkTree r (inc idx) vst
		= ([{id = Just (nodeId idx), text = join " " (coerceToStrings leaf), index = Just idx, leaf = True, children = Nothing}:rtree],idx`,vst)
	
	nodeId idx = (dp2id idPrefix currentPath) +++ "-" +++ toString idx
	
gVisualize{|Shared|} _ _ vst			= ([TextFragment "Reference to shared data"],vst)
gVisualize{|SharedReadOnly|} _ _ vst	= ([TextFragment "Read-Only reference to shared data"],vst)

derive gVisualize DateTime, Either, Void, UserDetails, Timestamp, Map, ProcessRef, EmailAddress, Action
derive bimap Maybe

//***** UTILITY FUNCTIONS *************************************************************************************************	

value2s :: !UpdateMask !(Maybe a) -> String | toString a
value2s (Touched _ _) (Just a) = toString a
value2s _ _ = ""

labelAttr :: !Bool !(Maybe String) -> Maybe String
labelAttr False	_		= Nothing
labelAttr True	Nothing	= Just ""
labelAttr True	l		= l 

formatLabel :: !String -> String
formatLabel label = {c \\ c <- [toUpper lname : addspace lnames]}
where
	[lname:lnames]		= [c \\ c <-: label]
	addspace []			= []
	addspace [c:cs]
		| c == '_'			= [' ':addspace cs]
		| isUpper c			= [' ',toLower c:addspace cs]
		| otherwise			= [c:addspace cs]

determineRemovals :: [Visualization] -> [Visualization]
determineRemovals editor = ([TUIUpdate (TUIRemove (fromJust (getId consid))) \\ consid <- (coerceToTUIDefs editor) | isJust (getId consid)])
	
determineAdditions :: String [Visualization] -> [Visualization]
determineAdditions consid editor = reverse [TUIUpdate (TUIAdd consid def) \\ def <- coerceToTUIDefs editor]

determineChildAdditions :: String [Visualization] -> [Visualization]
determineChildAdditions consid editor = [TUIUpdate (TUIAddTo consid def) \\ def <- coerceToTUIDefs editor]

visualizeBasicControl :: !(Maybe a) !*VSt -> (!TUIBasicControl, !*VSt) | toString a
visualizeBasicControl old vst=:{vizType,idPrefix,label,currentPath,updates,useLabels,optional,renderAsStatic,updateMask,verifyMask}
	# (cmu,um) = popMask updateMask
	# (cmv,vm) = popMask verifyMask
	# val = value2s cmu old
	# (err,hnt) = verifyElementStr cmu cmv
	= ({TUIBasicControl | name = dp2s currentPath, id = dp2id idPrefix currentPath, value = val, fieldLabel = labelAttr useLabels label, optional = optional, staticDisplay = renderAsStatic, errorMsg = err, hintMsg = hnt}
	  ,{VSt | vst & verifyMask = vm, updateMask = um, currentPath = stepDataPath currentPath})

mkChoiceOptionLabels fx options vst
	# vst			= {vst & vizType = VHtmlLabel}
	# (labels,vst)	= seqList (map mkOptionLabel options) vst
	= (labels, {vst & vizType = VEditorDefinition})
where
	mkOptionLabel option vst
		# (vis,vst) = fx (Just option) vst
		= (toString (SpanTag [ClassAttr "task-choice"] (flatten (coerceToHtml vis))),vst)

verifyElementStr :: !UpdateMask !VerifyMask -> (!String, !String)
verifyElementStr cmu cmv
	= case cmu of
		(Untouched) = case cmv of
			(VMValid mbHnt _) 				= ("",toString mbHnt)
			(VMUntouched mbHnt _ _) 		= ("",toString mbHnt)
			(VMInvalid IsBlankError _)		= ("","")
			(VMInvalid (ErrorMessage s) _)	= (s,"")
		_ = case cmv of
			(VMValid mbHnt _)				= ("",toString mbHnt)
			(VMUntouched mbHnt _ _) 		= ("",toString mbHnt)
			(VMInvalid err _)				= (toString err,"")
		
//*********************************************************************************************************************

//Coercion of visualizations
coerceToTUIDefs :: [Visualization] -> [TUIDef]
coerceToTUIDefs visualizations = [d \\ (TUIFragment d) <- visualizations]

coerceToTUIUpdates :: [Visualization] -> [TUIUpdate]
coerceToTUIUpdates []				  = []
coerceToTUIUpdates [(TUIUpdate u):vs] = [u:coerceToTUIUpdates vs]
coerceToTUIUpdates [(TUIFragment d):vs]
= case getId d of
	(Just id) 	= [(TUIReplace id d):coerceToTUIUpdates vs]
	Nothing		= coerceToTUIUpdates vs
coerceToTUIUpdates [v:vs]			= coerceToTUIUpdates vs

getId :: TUIDef -> Maybe TUIId
getId (TUIStringControl d)		= Just d.TUIBasicControl.id
getId (TUICharControl d)		= Just d.TUIBasicControl.id
getId (TUIIntControl d)			= Just d.TUIBasicControl.id
getId (TUIRealControl d)		= Just d.TUIBasicControl.id
getId (TUIBoolControl d)		= Just d.TUIBasicControl.id
getId (TUINoteControl d)		= Just d.TUIBasicControl.id
getId (TUIDateControl d)		= Just d.TUIBasicControl.id
getId (TUITimeControl d)		= Just d.TUIBasicControl.id
getId (TUICurrencyControl d)	= Just d.TUICurrencyControl.id
getId (TUIUserControl d)		= Just d.TUIBasicControl.id
getId (TUIPasswordControl d)	= Just d.TUIBasicControl.id
getId (TUIDocumentControl d)	= Just d.TUIDocumentControl.id
getId (TUIConstructorControl d)	= Just d.TUIConstructorControl.id
getId (TUIListItemControl d)	= Just d.TUIListItemControl.id
getId (TUITupleContainer d)		= Just d.TUITupleContainer.id
getId (TUIRecordContainer d)	= Just d.TUIRecordContainer.id
getId (TUIListContainer d)		= Just d.TUIListContainer.id

getId (TUILabel)				= Nothing
getId (TUICustom d)				= Nothing
getId _							= abort "unknown TUI Definition"

coerceToStrings :: [Visualization] -> [String]
coerceToStrings visualizations = [s \\ (TextFragment s) <- visualizations]

coerceToHtml :: [Visualization] -> [[HtmlTag]]
coerceToHtml visualizations = [coerce h \\h <- visualizations | coercable h]
where
	coerce (TextFragment s)		= [Text s]
	coerce (HtmlFragment h)		= h
	
	coercable (TextFragment _)	= True
	coercable (HtmlFragment _)	= True
	coercable _					= False
	
// VisualizationHints etc..
fromVisualizationHint :: !(VisualizationHint .a) -> .a
fromVisualizationHint (VHEditable a) = a
fromVisualizationHint (VHDisplay a) = a
fromVisualizationHint (VHHidden a) = a

toVisualizationHint :: !.a -> (VisualizationHint .a)
toVisualizationHint a = (VHEditable a)

fromEditable :: !(Editable .a) -> .a
fromEditable (Editable a) = a

toEditable :: !.a -> (Editable .a)
toEditable a = (Editable a)

fromDisplay :: !(Display .a) -> .a
fromDisplay (Display a) = a

toDisplay :: !.a -> (Display .a)
toDisplay a = (Display a)

fromHidden :: !(Hidden .a) -> .a
fromHidden (Hidden x) = x

toHidden :: !.a -> (Hidden .a)
toHidden x = (Hidden x)

(+++>) infixr 5	:: !a !String -> String | gVisualize{|*|} a
(+++>) a s = visualizeAsTextLabel a +++ s

(<+++) infixl 5	:: !String !a -> String | gVisualize{|*|} a
(<+++) s a = s +++ visualizeAsTextLabel a