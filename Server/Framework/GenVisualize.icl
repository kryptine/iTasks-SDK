implementation module GenVisualize

import StdBool, StdChar, StdList, StdArray, StdTuple, StdMisc, StdGeneric, StdEnum, StdFunc
import GenUpdate, GenVerify, TUIDiff, Util, Maybe, Functor, Text, HTML, JSON, TUIDefinition, Types, HtmlUtil

mkVSt :: *VSt
mkVSt = {VSt| origVizType = VTextDisplay, vizType = VTextDisplay, idPrefix = "", currentPath = startDataPath, label = Nothing, 
		useLabels = False, selectedConsIndex = -1, optional = False, renderAsStatic = False, verifyMask = [], headers = []}

//Wrapper functions
visualizeAsEditor :: String a VerifyMask -> [TUIDef] | gVisualize{|*|} a
visualizeAsEditor name x vmask
	# vst = {mkVSt & origVizType = VEditorDefinition, vizType  = VEditorDefinition, idPrefix = name, verifyMask = [vmask]}
	# (defs,vst) = gVisualize{|*|} (Just x) vst
	= coerceToTUIDefs defs	

visualizeAsHtmlDisplay :: a -> HtmlTag | gVisualize{|*|} a
visualizeAsHtmlDisplay x = html (coerceToHtml (fst (gVisualize{|*|} (Just x) {mkVSt & origVizType = VHtmlDisplay, vizType = VHtmlDisplay})))

visualizeAsTextDisplay :: a -> String | gVisualize{|*|} a
visualizeAsTextDisplay x = join " " (coerceToStrings (fst (gVisualize{|*|} (Just x) {mkVSt & origVizType = VTextDisplay, vizType = VTextDisplay})))

visualizeAsHtmlLabel :: a -> HtmlTag | gVisualize{|*|} a
visualizeAsHtmlLabel x = html (coerceToHtml (fst (gVisualize{|*|} (Just x) {mkVSt & origVizType = VHtmlLabel, vizType = VHtmlLabel})))
	
visualizeAsTextLabel :: a -> String | gVisualize{|*|} a
visualizeAsTextLabel x = join " " (coerceToStrings (fst (gVisualize{|*|} (Just x) {mkVSt & origVizType = VTextLabel, vizType = VTextLabel})))

determineEditorUpdates :: String (a,VerifyMask) (a,VerifyMask) ![DataPath] -> [TUIUpdate]	| gVisualize{|*|} a
determineEditorUpdates name (oval,ovmask) (nval,nvmask) alwaysUpdate
	# oldviz = visualizeAsEditor name oval ovmask
	# newviz = visualizeAsEditor name nval nvmask
	= case (oldviz,newviz) of
		([oldviz],[newviz])	= diffEditorDefinitions oldviz newviz alwaysUpdate
		_					= []
	
//IDEAS:
// - ConstructorControl, should in same cases be a constructor container
// - HtmlContainer should really be an html control
// - Update instructions should be identified by path to make shuffles possible

//Generic visualizer
generic gVisualize a :: (Maybe a) *VSt -> ([Visualization], *VSt)

gVisualize{|UNIT|} _ vst
	= ([],vst)
	
gVisualize{|FIELD of d|} fx val vst=:{vizType}
	# x = fmap fromFIELD val
	# (vis,vst) = case vizType of
		VHtmlDisplay
			# (vizBody,vst) 	= fx x {VSt |vst & label = Nothing}
			= case vizBody of
			 [] = ([],vst)
			 _  = ([HtmlFragment (TrTag [] [ThTag [] [Text (formatLabel d.gfd_name),Text ": "],TdTag [] (coerceToHtml vizBody)])],vst)
		VTextDisplay
			# (vizBody,vst) 	= fx x {VSt |vst & label = Just (formatLabel d.gfd_name)}
			= ([TextFragment (formatLabel d.gfd_name),TextFragment ": " : vizBody]++[TextFragment " "],vst)
		_
			# (vizBody,vst)		= fx x {VSt |vst & label = Just (formatLabel d.gfd_name)}
			= (vizBody,vst)
	= (vis,{VSt|vst & label = Nothing})
			
gVisualize{|OBJECT of d|} fx val vst=:{vizType,idPrefix,label,currentPath,selectedConsIndex = oldSelectedConsIndex,useLabels,optional,renderAsStatic,verifyMask}
	//For objects we only peek at the verify mask, but don't take it out of the state yet.
	//The masks are removed from the states when processing the CONS.
	# (cmv,_)	= popMask verifyMask
	# x			= fmap fromOBJECT val
	//ADT's with multiple constructors: Add the creation of a control for choosing the constructor
	| d.gtd_num_conses > 1
		= case vizType of 
			VEditorDefinition
				# (err,hnt)	= verifyElementStr cmv
				# (items, vst=:{selectedConsIndex}) = fx x {vst & useLabels = False, optional = False}
				= ([TUIFragment (TUIConstructorControl	{TUIConstructorControl
														| id = dp2id idPrefix currentPath
														, name = dp2s currentPath
														, fieldLabel = labelAttr useLabels label
														, consSelIdx = if (isTouched cmv) selectedConsIndex -1
														, consValues = [gdc.gcd_name \\ gdc <- d.gtd_conses]
														, items = if (isTouched cmv) (coerceToTUIDefs items) []
														, staticDisplay = renderAsStatic
														, optional = optional
														, errorMsg = err
														, hintMsg = hnt
														})]
				  ,{vst & currentPath = stepDataPath currentPath, selectedConsIndex = oldSelectedConsIndex, useLabels = useLabels, optional = optional})					
			_
				# (viz,vst) = fx x vst
				= (viz,{VSt|vst & currentPath = stepDataPath currentPath})
	//Everything else, just strip of the OBJECT constructor and pass through
	| otherwise = case val of
		= fx x vst
			
gVisualize{|CONS of d|} fx val vst=:{useLabels,optional} = visualizeCustom mkControl staticVis val False vst
where
	mkControl name id val _ label optional err hnt renderAsStatic vst
		# x = fmap fromCONS val
		# (vis,vst) = case isRecord d.gcd_type_def of
			False // normal ADT
				# (viz,vst) = fx x {vst & useLabels = False}
				= (coerceToTUIDefs viz, {VSt | vst & selectedConsIndex = d.gcd_index})
			True = case x of // record
				Nothing // Create an empty record container that can be expanded later
					= (recordContainer False [],vst)
				Just x
					# (viz,vst) = fx (Just x) {vst & useLabels = True, optional = False}
					=(recordContainer True viz,{vst & headers = recordHeaders d.gcd_fields})
		= (vis,{vst & optional = optional, useLabels = useLabels})
	where
		recordContainer hasValue viz = [TUIRecordContainer	{ TUIRecordContainer
															| id = id
															, name = name
															, title = label
															, items = coerceToTUIDefs viz
															, optional = (optional && (not renderAsStatic))
															, hasValue = hasValue
															}]
															
		recordHeaders fields = map (\{gfd_name} -> gfd_name) fields
		
	staticVis v _ _ vst=:{vizType}
		# x = fmap (\(CONS c) -> c) v
		# (viz,vst) = fx x {VSt|vst & label = Nothing}
		= case vizType of
			VHtmlDisplay
				//Records
				| not (isEmpty d.gcd_fields) 
					= ([HtmlFragment (TableTag [ClassAttr "viz-record"] (coerceToHtml viz))],vst)
				| otherwise
				= normalADTStaticViz viz vst
			VHtmlLabel
				//For records only show the first field
				| not (isEmpty d.gcd_fields)
					= ([hd viz],vst)
				| otherwise
					= normalADTStaticViz viz vst
			VTextLabel
				//For records only show the first field
				| not (isEmpty d.gcd_fields) 
					= ([hd viz],vst)
				| otherwise
					= normalADTStaticViz viz vst	
			VTextDisplay
				| not (isEmpty d.gcd_fields) 
					= (viz,vst)
				| otherwise
					= normalADTStaticViz viz vst
	where
		normalADTStaticViz viz vst
			//When there are multiple constructors, also show the name of the constructor
			| d.gcd_type_def.gtd_num_conses > 1
				= ([TextFragment d.gcd_name,TextFragment " " :viz],vst)
			| otherwise
				= (viz,vst)

gVisualize{|PAIR|} fx fy val vst
	# (x,y)			= (fmap fromPAIRX val, fmap fromPAIRY val)
	# (vizx, vst)	= fx x vst
	# (vizy, vst)	= fy y vst
	= (vizx ++ vizy, vst)

gVisualize{|EITHER|} fx fy val vst = case val of
		Nothing			= fx Nothing vst
		Just (LEFT x)	= fx (Just x) vst
		Just (RIGHT y)	= fy (Just y) vst
	
gVisualize{|Int|}		val vst = visualizeBasicControlSimple TUIIntControl val vst
gVisualize{|Real|}		val vst = visualizeBasicControlSimple TUIRealControl val vst
gVisualize{|Char|}		val vst = visualizeBasicControlSimple TUICharControl val vst
gVisualize{|String|}	val vst = visualizeBasicControlSimple TUIStringControl val vst
gVisualize{|Bool|}		val vst = visualizeBasicControl TUIBoolControl (toString,toHtml) val vst
where
	toHtml val _ = DivTag
		[ClassAttr ("bool-htmllabel-icon bool-htmllabel-icon-"+++(toLowerCase (toString val)))]
		[SpanTag [ClassAttr "bool-htmllabel-text"] [(Text (toString val))]]
gVisualize{|Password|}	val vst = visualizeBasicControl TUIPasswordControl (textOnly (const staticViz)) val vst
where
	staticViz = "********"
gVisualize{|Note|}		val vst = visualizeBasicControl TUINoteControl (toString,toHtml) val vst
where
	toHtml val _ = nl2br (toString val)
gVisualize{|Date|}		val vst = visualizeBasicControlSimple TUIDateControl val vst
gVisualize{|Time|}		val vst = visualizeBasicControlSimple TUITimeControl val vst
gVisualize{|User|}		val vst = visualizeBasicControlSimple TUIUserControl val vst
gVisualize{|Currency|}	val vst = visualizeControl mkControl (textOnly toString) val vst
where
	mkControl name id val label optional err hnt
		= TUICurrencyControl	{ TUICurrencyControl
								| id = id
								, name = name
								, value = toString (fmap (decFormat o toInt) val)
								, fieldLabel = label
								, currencyLabel = curLabel val
								, optional = optional
								, errorMsg = err
								, hintMsg = hnt}
		
	curLabel (Just (EUR _))	= "&euro;"
	curLabel (Just (GBP _))	= "&pound;"
	curLabel (Just (USD _))	= "$"
	curLabel (Just (JPY _)) = "&yen;"
	curLabel _				= "&euro;" //Use the default currency

gVisualize {|Document|}	val vst = visualizeControl mkControl (mkText,mkHtml) val vst
where
	mkControl name id val label optional err hnt
		# doc = case val of
			Just doc	= doc
			Nothing		= {Document|documentId = "",name = "", mime = "", size = 0}
		= TUIDocumentControl	{ TUIDocumentControl
								| id = id
								, name = name
								, document = doc
								, fieldLabel = label
								, optional = optional
								, errorMsg = err
								, hintMsg = hnt
								}
	mkText val = case val of
		Nothing								= noDocument
		Just document
			| document.Document.size == 0	= noDocument
			| otherwise						= document.Document.name
			
	mkHtml val id = case val of
		Nothing
			= html noDocument
		Just document
			| document.Document.size == 0
				= html noDocument
			| otherwise
				# downLink = ATag [HrefAttr (buildLink document)
								  ,TargetAttr "_blank",IdAttr id
								  ,NameAttr "x-form-document-link"] [ImgTag [SrcAttr "skins/default/img/icons/page_white_put.png"]]
				# prevLink = ATag [HrefAttr "#", IdAttr id
								  ,NameAttr "x-form-document-preview-link" ] [ImgTag [SrcAttr "skins/default/img/icons/zoom.png"]]			
				= html [Text ( document.Document.name +++" ("+++printByteSize document.Document.size+++") "),RawText "&nbsp;",downLink,prevLink]
			
	noDocument = "No Document"
	buildLink document = "/services/json/documents/" +++ document.Document.documentId +++ "/download"
	printByteSize size
		| size >= 1048576 = toString (fixReal ((toReal size)/(toReal 1048576)))+++" Mbyte"
		| size >= 1024    = toString (fixReal ((toReal size)/(toReal 1024)))+++" Kbyte"
		| otherwise 	  = toString size +++ " byte"
	fixReal r = (toReal (toInt (r*100.0)))/100.0
	
gVisualize{|FormButton|} val vst = visualizeControl mkControl (textOnly buttonLabel) val vst
where
	mkControl name id val label optional err hnt
		= TUIFormButtonControl	{ TUIButtonControl
								| label = buttonLabel val
								, iconCls = icon val
								, name = name
								, id = id
								, value = toString (pressed val)
								, fieldLabel = label
								, optional = optional
								, errorMsg = err
								, hintMsg = hnt}
								
	buttonLabel	b = toString ((fmap (\b -> b.FormButton.label)) b)
	icon		b = toString ((fmap (\b -> b.FormButton.icon)) b)
	pressed		b = case val of
		Just b = case b.FormButton.state of
			Pressed		= True
			NotPressed	= False
		Nothing			= False
		
gVisualize{|Choice|} fx val vst = visualizeCustom mkControl staticVis val True vst
where
	mkControl name id val touched label optional err hnt _ vst = case val of
		Nothing
			= ([TUIHtmlContainer	{ id = id
									, html = noSelection
									, fieldLabel = label
									}],vst)
		Just (Choice opts sel)
			# (children,vst) = childVisualizations fx opts (Just VHtmlLabel) vst
			= ([TUIChoiceControl	{ TUIChoiceControl
									| name = id					// names of checkbox groups have to be unique, ...
									, id = id
									, dataPath = name			// ... so use data path field for events
									, fieldLabel = label
									, optional = optional
									, allowMultiple = False
									, options = map mkOptionLabel children
									, selection = if (touched && sel >= 0 && sel < length opts) [sel] []
									, errorMsg = err
									, hintMsg = hnt
									}],vst)
								
	staticVis v touched _ vst = case (v,touched) of
		(Just choice,True)	= fx (Just (getChoice choice)) vst
		_					= ([TextFragment noSelection],vst)
	
	mkOptionLabel vis = toString (SpanTag [ClassAttr "task-choice"] (coerceToHtml vis))
	noSelection = "No item selected"

gVisualize{|MultipleChoice|} fx val vst = visualizeCustom mkControl staticVis val True vst
where
	mkControl name id val touched label optional err hnt _ vst = case val of
		Nothing
			= ([TUIHtmlContainer	{ id = id
									, html = empty
									, fieldLabel = label
									}],vst)
		Just (MultipleChoice opts sel)
			# (children,vst) = childVisualizations fx opts (Just VHtmlLabel) vst
			= ([TUIChoiceControl	{ TUIChoiceControl
								| name = id				// names of checkbox groups have to be unique,...
								, id = id
								, dataPath = name		// ... so use data path field for events
								, fieldLabel = label
								, optional = optional
								, allowMultiple = True
								, options = map mkOptionLabel children
								, selection = sel
								, errorMsg = err
								, hintMsg = hnt
								}],vst)
								
	staticVis v touched _ vst = case v of
		Just choice	= gVisualize{|* -> *|} fx (Just (getChoices choice)) vst
		Nothing		= ([TextFragment empty],vst)
	
	mkOptionLabel vis = toString (SpanTag [ClassAttr "task-choice"] (coerceToHtml vis))
	empty = "Empty multiple choice"

gVisualize{|Tree|} fx val vst = visualizeCustom mkControl staticVis val True vst
where
	mkControl name id val touched label optional err hnt _ vst = case val of
		Nothing
			= ([TUIHtmlContainer	{ id = id
									, html = empty
									, fieldLabel = label
									}],vst)
		Just (Tree nodes sel)
			# vst = {vst & vizType = VTextLabel}
			# (tree,_,vst) = mkTree nodes 0 vst
			# vst = {vst & vizType = VEditorDefinition}
			= ([TUITreeControl	{ TUITreeControl
								| name = name
								, id = id
								, tuiTree = tree
								, selIndex = if (touched && sel >= 0) (Just sel) Nothing
								, fieldLabel = label
								, optional = optional
								, errorMsg = err
								, hintMsg = hnt
								}],vst)
	where
		mkTree [] idx vst
			= ([],idx,vst)
		mkTree [Leaf v:r] idx vst
			# (leaf,vst)		= fx (Just v) vst
			# (rtree,idx`,vst)	= mkTree r (inc idx) vst
			= ([{id = Just (nodeId idx), text = join " " (coerceToStrings leaf), index = Just idx, leaf = True, children = Nothing}:rtree],idx`,vst)
		mkTree [Node label nodes:r] idx vst
			# (children,idx,vst)	= mkTree nodes idx vst
			# (rtree,idx,vst)		= mkTree r idx vst
			= ([{id = Nothing, text = label, index = Nothing, leaf = False, children = Just children}:rtree],idx,vst)
		nodeId idx = id +++ "-" +++ toString idx
		
	staticVis v touched _ vst=:{vizType}
		= case (v,touched) of
			(Just tree=:(Tree nodes sel),True)
				= case vizType of
					VHtmlDisplay
						= (\(html,_,vst) -> ([HtmlFragment (UlTag [] html)],vst)) (mkHtmlDisplay nodes 0 sel vst)
					_
						= fx (Just (getSelectedLeaf tree)) vst
			_
				= ([TextFragment empty],vst)
	where
		mkHtmlDisplay [] idx sel vst
			= ([],idx,vst)
		mkHtmlDisplay [Leaf v:r] idx sel vst
			# (leaf,vst)		= fx (Just v) vst
			# (rtree,idx`,vst)	= mkHtmlDisplay r (inc idx) sel vst
			= ([LiTag (itemCls idx) (coerceToHtml leaf):rtree],idx`,vst)
		where
			itemCls idx
				| idx == sel	= [ClassAttr "tree-list-item-selected"]
				| isEven idx	= [ClassAttr "list-item-light"]
				| otherwise		= [ClassAttr "list-item-dark"]
		mkHtmlDisplay [Node label nodes:r] idx sel vst
			# (children,idx,vst)	= mkHtmlDisplay nodes idx sel vst
			# (rtree,idx,vst)		= mkHtmlDisplay r idx sel vst
			= ([LiTag [ClassAttr "tree-list-node-header"] [Text label],UlTag ulClass children:rtree],idx,vst)
		where
			ulClass = [ClassAttr "tree-list"]
	
	empty = "Empty tree"

gVisualize{|Table|} fx val vst = visualizeCustom mkControl staticVis val True vst
where
	mkControl name id val touched label optional err hnt _ vst=:{VSt|currentPath,verifyMask} = case val of
		Nothing
			= ([TUIHtmlContainer	{ id = id
									, html = empty
									, fieldLabel = label
									}],vst)
		Just (Table rows)
			#( vis,vst=:{headers})	= gVisualize{|* -> *|} fx (Just rows) vst
			# (htm,vst)				= childVisualizations fx rows (Just VHtmlDisplay) {VSt|vst & currentPath = currentPath, verifyMask = childMasks (fst (popMask verifyMask))}
			= ([TUIGridControl	{ TUIGridControl
								| name = dp2s currentPath
								, id = id
								, columns = toTUICols headers
								, gridHtml = map getHtml htm
								, gridEditors = getEditors vis
								}],vst)
	where
		getHtml :: [Visualization] -> [String]
		getHtml [HtmlFragment (TableTag [ClassAttr "viz-record"] cols)] = map getHtml` cols
		where
			getHtml` :: HtmlTag -> String
			getHtml` (TrTag _ [_,TdTag _ htm])	= toString (html htm)
			getHtml` _							= abort "get table html: unexpected format of record table row"
		getHtml viz = [toString (html (coerceToHtml viz))]
		
		getEditors :: [Visualization] -> [[TUIDef]]
		getEditors [TUIFragment (TUIListContainer {TUIListContainer|items})] = map (getEditors` o stripListItem) items
		where
			stripListItem :: TUIDef -> [TUIDef]
			stripListItem (TUIListItemControl {TUIListItemControl|items}) = items
			stripListItem _ = abort "get table editors: list item container expected"
			
			getEditors` :: [TUIDef] -> [TUIDef]
			getEditors` tui = case tui of
				[TUIRecordContainer {TUIRecordContainer|items}]	= items
				tui												= tui
		getEditors _ = abort "get table editors: list container expected"
		
		toTUICols []		= [{header = ""}]
		toTUICols headers	= map toTUICol headers
		where
			toTUICol header = {header = (formatLabel header)}
			
	staticVis v touched _ vst = case (v,touched) of
		(Just (Table rows),True)	= gVisualize{|* -> *|} fx (Just rows) vst
		_							= ([TextFragment empty],vst)
	
	empty = "Empty table"
	
gVisualize {|[]|} fx val vst = visualizeCustom mkControl staticVis val False vst
where
	mkControl name id val touched label optional err hnt renderAsStatic vst=:{useLabels}
		# val = listValue val
		# (items,vst) = TUIDef val vst
		= ([TUIListContainer	{ TUIListContainer
								| items = items
								, optional = optional
								, name = name
								, id = id
								, fieldLabel = label
								, hideLabel = not useLabels
								, staticDisplay = renderAsStatic
								, errorMsg = err
								, hintMsg = hnt}],vst)
		where
			TUIDef items vst=:{optional,useLabels}
				# (itemsVis,vst)	= childVisualizations fx items Nothing {vst & optional = False, useLabels = False, label = Nothing}
				# vis				= [listItemControl name id idx (coerceToTUIDefs dx) \\ dx <- itemsVis & idx <- [0..]]
				# (vis,vst)			= case renderAsStatic of
					False
						# (dx,vst)  = fx Nothing {VSt|vst & optional = True}
						= (vis ++ [listItemControl name id (length vis) (coerceToTUIDefs dx)],vst)
					True
						= (vis,vst)
				= (vis,{vst & optional = optional, useLabels = useLabels})
						
			listItemControl name id idx defs
				= TUIListItemControl	{TUIListItemControl
										| name = name
										, id = id +++ "#" +++ toString idx
										, index = idx
										, items = defs
										}
							
	staticVis v _ _ vst=:{vizType}
		# v = listValue v
		# (items,vst) = childVisualizations fx v Nothing vst
		= case vizType of
			VHtmlDisplay = case v of
				[]	= ([HtmlFragment (UlTag [] [LiTag [ClassAttr "list-item-light"] [(Text empty)]])],vst)
				_	= ([HtmlFragment (UlTag [] [(LiTag [ClassAttr (itemCls i)] (coerceToHtml x)) \\ x <- items & i <- [0..]])],vst)
			VTextDisplay
					= ([TextFragment ("["+++join ", " (flatten  [(coerceToStrings x) \\ x <-items])+++"]")],vst)
			VHtmlLabel = case v of
				[]	= ([HtmlFragment (Text empty)],vst)
				_	= ([HtmlFragment (html (htmlLabel items))],vst)
			_		= ([],vst)
	where
		itemCls i
			| isEven i  = "list-item-light"
			| otherwise = "list-item-dark"
			
		htmlLabel [i]		= coerceToHtml i
		htmlLabel [i:is]	= coerceToHtml i ++ [(Text ", ")] ++ htmlLabel is
		
	listValue l = case val of
		Just l	= l
		Nothing	= []
		
	empty = "Empty list"
	
gVisualize{|(,)|} f1 f2 val vst = visualizeCustom (tupleMkControl visChildren) (tupleStaticVis visChildren) val False vst
where
	visChildren val combF vst=:{useLabels}
		# (v1,v2) = case val of
			Just (o1,o2)	= (Just o1, Just o2)
			Nothing		 	= (Nothing,Nothing)
		# (viz1,vst)		= f1 v1 {VSt| vst & useLabels = False, label = Nothing}
		# (viz2,vst)		= f2 v2 vst
		= (combF [viz1,viz2],{vst & useLabels = useLabels})
		
gVisualize{|(,,)|} f1 f2 f3 val vst = visualizeCustom (tupleMkControl visChildren) (tupleStaticVis visChildren) val False vst
where
	visChildren val combF vst=:{useLabels}
		# (v1,v2,v3) = case val of
			Just (o1,o2,o3)	= (Just o1, Just o2, Just o3)
			Nothing		 	= (Nothing,Nothing,Nothing)
		# (viz1,vst)		= f1 v1 {VSt| vst & useLabels = False, label = Nothing}
		# (viz2,vst)		= f2 v2 vst
		# (viz3,vst)		= f3 v3 vst
		= (combF [viz1,viz2,viz3],{vst & useLabels = useLabels})
		
gVisualize{|(,,,)|} f1 f2 f3 f4 val vst = visualizeCustom (tupleMkControl visChildren) (tupleStaticVis visChildren) val False vst
where
	visChildren val combF vst=:{useLabels}
		# (v1,v2,v3,v4) = case val of
			Just (o1,o2,o3,o4)	= (Just o1, Just o2, Just o3, Just o4)
			Nothing		 		= (Nothing,Nothing,Nothing,Nothing)
		# (viz1,vst)			= f1 v1 {VSt| vst & useLabels = False, label = Nothing}
		# (viz2,vst)			= f2 v2 vst
		# (viz3,vst)			= f3 v3 vst
		# (viz4,vst)			= f4 v4 vst
		= (combF [viz1,viz2,viz3,viz4],{vst & useLabels = useLabels})

// tuple util functions	
tupleMkControl visChildren _ id val _ label optional _ _ _ vst
		# (vis,vst) = visChildren val (map coerceToTUIDefs) vst
		= ([TUITupleContainer	{ TUITupleContainer
								| id = id
								, fieldLabel = label
								, optional = optional
								, items = vis}],vst)
								
tupleStaticVis visChildren v _ _ vst=:{useLabels} = visChildren v addSeparators vst
where
	addSeparators items = addSeparators` (filter (not o isEmpty) items) []
	
	addSeparators` [] acc = flatten (reverse acc)
	addSeparators` [viz:nextV] acc
		# addSep = case nextV of
			[[TextFragment _]:_]		= True
			[[HtmlFragment (Text _)]:_]	= True
			_							= False
		# acc = case addSep of
			True	= [separator,viz:acc]
			False	= [viz:acc]
		= addSeparators` nextV acc
	
	separator = [TextFragment ", "]
	
gVisualize{|Dynamic|}			_ vst	= noVisualization vst
gVisualize{|(->)|} _ _			_ vst	= noVisualization vst
gVisualize{|Shared|} _ _		_ vst	= noVisualization vst

gVisualize{|Maybe|} fx val vst=:{vizType,currentPath,optional}
	# vst = {VSt|vst & optional = True}
	# (viz,vst) = case vizType of
		VEditorDefinition = case val of
			Just (Just x)	= fx (Just x) vst
			_				= fx Nothing vst
		_ = case val of
			Just Nothing	= ([TextFragment "-"],vst)
			Just (Just x)	= fx (Just x) vst
			Nothing			= ([],vst)
	= (viz, {VSt|vst & optional = optional, currentPath = stepDataPath currentPath})
	
// wrapper types changing visualization behaviour
gVisualize{|Hidden|} fx val vst=:{VSt | currentPath, verifyMask}
	# (cmv,vm) = popMask verifyMask	
	= ([],{VSt | vst & currentPath = stepDataPath currentPath, verifyMask = vm})

gVisualize{|Display|} fx val vst=:{currentPath,renderAsStatic}
	# x	= fmap fromDisplay val
	# (def,vst) = fx x {VSt | vst &  renderAsStatic = True}
	= (def,{VSt | vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})

gVisualize{|Editable|} fx val vst=:{currentPath, renderAsStatic}
	# x	= fmap fromEditable val
	# (def,vst) = fx x {VSt | vst & renderAsStatic = False}
	= (def,{VSt | vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})

gVisualize{|VisualizationHint|} fx val vst=:{currentPath, idPrefix, vizType, origVizType}
	# x	= fmap fromVisualizationHint val
	= case val of
		(Just (VHHidden _))
			# (viz,vst) = gVisualize{|* -> *|} fx (fmap Hidden x) vst
			# viz = case origVizType of
				VEditorDefinition
					= [TUIFragment (TUIHiddenControl	{ TUIBasicControl
														| name = dp2s currentPath
														, id = dp2id idPrefix currentPath
														, value = ""
														, fieldLabel = Nothing
														, optional = True
														, errorMsg = ""
														, hintMsg = ""
														})]
				_
					= []
			= (viz,vst)
		(Just (VHDisplay _))
			# vizType` = case origVizType of
				VHtmlDisplay	= VHtmlDisplay
				_				= vizType
			# (viz,vst) = gVisualize{|* -> *|} fx (fmap Display x) {vst & vizType = vizType`}
			= (viz,{vst & vizType = vizType})
		(Just (VHEditable _))
			= gVisualize{|* -> *|} fx (fmap Editable x) vst		

derive gVisualize DateTime, Either, Void, UserDetails, Timestamp, Map, ProcessRef, EmailAddress, Action, TreeNode
derive bimap Maybe

//***** UTILITY FUNCTIONS *************************************************************************************************	
textOnly :: !((Maybe a) -> String) -> StaticVizFunctions a
textOnly textF = (textF,htmlF)
where
	htmlF v _ = html (textF v)

visualizeBasicControlSimple :: !(TUIBasicControl -> TUIDef) !(Maybe a) !*VSt -> *(![Visualization],!*VSt) | gVisualize{|*|} a & toString a
visualizeBasicControlSimple tuiType v vst = visualizeBasicControl tuiType (textOnly toString) v vst

visualizeBasicControl :: !(TUIBasicControl -> TUIDef) !(StaticVizFunctions a) !(Maybe a) !*VSt -> *(![Visualization],!*VSt) | gVisualize{|*|} a & toString a
visualizeBasicControl tuiType vizFuncs v vst = visualizeControl mkBasicControl vizFuncs v vst
where
	mkBasicControl name id val label optional err hnt
		= tuiType	{ TUIBasicControl
					| name = name
					, id = id
					, value = toString val
					, fieldLabel = label
					, optional = optional
					, errorMsg = err
					, hintMsg = hnt
					}

visualizeControl :: !(TUIVizFunction a) !(StaticVizFunctions a) !(Maybe a) !*VSt -> (![Visualization],!*VSt) | gVisualize{|*|} a
visualizeControl tuiF (strF,htmlF) v vst = visualizeCustom tuiF` staticF v True vst
where
	tuiF` name id v touched label optional err hnt _ vst
		# v = checkMask touched v
		= ([tuiF name id v label optional err hnt],vst)
	staticF v touched id vst=:{vizType}
		# v = checkMask touched v
		# vis = case vizType of
			VHtmlDisplay
				= [HtmlFragment (htmlF v id)]
			VHtmlLabel
				= [HtmlFragment (htmlF v id)]
			VTextDisplay
				= [TextFragment (strF v)]
			VTextLabel
				= [TextFragment (strF v)]
			_
				= abort "function for static visualizations called with VEditorDefinition"
		= (vis,vst)

	checkMask :: !Bool !(Maybe a) -> (Maybe a)
	checkMask False _	= Nothing
	checkMask _ val 	= val
	
visualizeCustom :: !(TUIVizFunctionCustom a) !(StaticVizFunctionCustom a) !(Maybe a) !Bool !*VSt -> *(![Visualization],!*VSt)
visualizeCustom tuiF staticF v staticHtmlContainer vst=:{vizType,origVizType,idPrefix,label,currentPath,useLabels,optional,renderAsStatic,verifyMask}
	# (cmv,vm)	= popMask verifyMask
	// only check mask if generating editor definition & not for labels
	# touched	= case origVizType of
		VEditorDefinition = case vizType of
			VTextLabel	= True
			VHtmlLabel	= True
			_			= isTouched cmv
		_				= True
	# id		= dp2id idPrefix currentPath
	# vst		= {VSt|vst & currentPath = shiftDataPath currentPath, verifyMask = childMasks cmv}
	# (vis,vst) = case vizType of
		VEditorDefinition
			# label = labelAttr useLabels label
			# (vis,vst) = case (renderAsStatic,staticHtmlContainer) of
				(True,True)
					# (vis,vst)	= staticF v touched id {vst & vizType = VHtmlDisplay}
					# vst		= {vst & vizType = vizType} 
					= ([TUIHtmlContainer	{ id = id
											, html = toString (html (coerceToHtml vis))
											, fieldLabel = label
											}],vst)
				_
					# (err,hnt) = verifyElementStr cmv
					= tuiF (dp2s currentPath) id v touched label optional err hnt renderAsStatic vst
				
			= (map TUIFragment vis,vst)
		_
			= staticF v touched id vst
	= (vis,{VSt|vst & currentPath = stepDataPath currentPath, verifyMask = vm})
	
noVisualization :: !*VSt -> *(![Visualization],!*VSt)
noVisualization vst=:{VSt|currentPath,verifyMask}
	# (_,vm) = popMask verifyMask
	= ([],{VSt|vst & currentPath = stepDataPath currentPath, verifyMask = vm})
	
childVisualizations :: !((Maybe a) -> .(*VSt -> *([Visualization],*VSt))) ![a] !(Maybe VisualizationType) !*VSt -> *(![[Visualization]],!*VSt)
childVisualizations fx children mbVizType vst=:{vizType}
	# vst = case mbVizType of
		Nothing	= vst
		Just vt	= {vst & vizType = vt}
	# (vis,vst)	= childVisualizations` children [] vst
	= (vis,{vst & vizType = vizType})
where
	childVisualizations` [] acc vst
		= (reverse acc,vst)
	childVisualizations` [child:children] acc vst
		# (childV,vst) = fx (Just child) vst
		= childVisualizations` children [childV:acc] vst

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

verifyElementStr :: !VerifyMask -> (!String, !String)
verifyElementStr cmv = case cmv of
	VMValid mbHnt _			= ("",toString mbHnt)
	VMUntouched mbHnt _ _	= ("",toString mbHnt)
	VMInvalid err _			= (toString err,"")

//*********************************************************************************************************************

//Coercion of visualizations
coerceToTUIDefs :: [Visualization] -> [TUIDef]
coerceToTUIDefs visualizations = [d \\ (TUIFragment d) <- visualizations]

coerceToTUIUpdates :: [Visualization] -> [TUIUpdate]
coerceToTUIUpdates []				  = []
coerceToTUIUpdates [(TUIUpdate u):vs] = [u:coerceToTUIUpdates vs]
coerceToTUIUpdates [(TUIFragment d):vs]
= case getTUIId d of
	(Just id) 	= [(TUIReplace id d):coerceToTUIUpdates vs]
	Nothing		= coerceToTUIUpdates vs
coerceToTUIUpdates [v:vs]			= coerceToTUIUpdates vs

coerceToStrings :: [Visualization] -> [String]
coerceToStrings visualizations = [s \\ (TextFragment s) <- visualizations]

coerceToHtml :: [Visualization] -> [HtmlTag]
coerceToHtml visualizations = [coerce h \\h <- visualizations | coercable h]
where
	coerce (TextFragment s)		= Text s
	coerce (HtmlFragment h)		= h
	
	coercable (TextFragment _)	= True
	coercable (HtmlFragment _)	= True
	coercable _					= False
	
(+++>) infixr 5	:: !a !String -> String | gVisualize{|*|} a
(+++>) a s = visualizeAsTextLabel a +++ s

(<+++) infixl 5	:: !String !a -> String | gVisualize{|*|} a
(<+++) s a = s +++ visualizeAsTextLabel a