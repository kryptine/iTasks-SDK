implementation module GenVisualize

import StdBool, StdChar, StdList, StdArray, StdTuple, StdMisc, StdGeneric, StdEnum, StdFunc
import GenUpdate, GenVerify, Util, Maybe, Functor, Text, HTML, JSON, TUIDefinition, SystemTypes, HtmlUtil

mkVSt :: *VSt
mkVSt = {VSt| origVizType = VTextDisplay, vizType = VTextDisplay, currentPath = startDataPath,
		selectedConsIndex = -1, optional = False, renderAsStatic = False, verifyMask = [], editEvent = Nothing, taskId = "",
		controlSize = (Auto,Auto,Nothing)}

visualizeAsEditor :: !a !TaskId !Int !VerifyMask !(Maybe (!DataPath,!JSONNode)) -> TUIDef | gVisualize{|*|} a
visualizeAsEditor x taskId idx vmask editEvent = visualizeAsEditorCustom gVisualize{|*|} x taskId idx vmask editEvent

visualizeAsHtmlDisplay :: !a -> HtmlTag | gVisualize{|*|} a
visualizeAsHtmlDisplay x = visualizeAsHtmlDisplayCustom gVisualize{|*|} x

visualizeAsTextDisplay :: !a -> String | gVisualize{|*|} a
visualizeAsTextDisplay x = visualizeAsTextDisplayCustom gVisualize{|*|} x

visualizeAsHtmlLabel :: !a -> HtmlTag | gVisualize{|*|} a
visualizeAsHtmlLabel x = visualizeAsHtmlLabelCustom gVisualize{|*|} x
	
visualizeAsTextLabel :: !a -> String | gVisualize{|*|} a
visualizeAsTextLabel x = visualizeAsTextLabelCustom gVisualize{|*|} x

// visualization wrapper functions with custom visualization function as first argument (used for child visualization inside of gVisualize)
visualizeAsEditorCustom :: !((Maybe a) -> .(*VSt -> (![Visualization], !*VSt))) !a !TaskId !Int !VerifyMask !(Maybe (!DataPath,!JSONNode)) -> TUIDef
visualizeAsEditorCustom vizF x taskId idx vmask editEvent
	# vst = {mkVSt & origVizType = VEditorDefinition, vizType  = VEditorDefinition, verifyMask = [vmask], editEvent = editEvent, taskId = taskId, currentPath = shiftDataPath (childDataPath emptyDataPath idx)}
	# (defs,vst) = vizF (Just x) vst
	= case coerceToTUIDefs defs of
		[tui]	= tui
		tuis	= {content = TUILayoutContainer (defaultLayoutContainer tuis), width = WrapContent 0, height = WrapContent 0, margins = Nothing}

visualizeAsHtmlDisplayCustom :: !((Maybe a) -> .(*VSt -> (![Visualization], !*VSt))) !a -> HtmlTag
visualizeAsHtmlDisplayCustom vizF x = html (coerceToHtml (fst (vizF (Just x) {mkVSt & origVizType = VHtmlDisplay, vizType = VHtmlDisplay})))

visualizeAsTextDisplayCustom :: !((Maybe a) -> .(*VSt -> (![Visualization], !*VSt))) !a -> String
visualizeAsTextDisplayCustom vizF x = join " " (coerceToStrings (fst (vizF (Just x) {mkVSt & origVizType = VTextDisplay, vizType = VTextDisplay})))

visualizeAsHtmlLabelCustom :: !((Maybe a) -> .(*VSt -> (![Visualization], !*VSt))) !a -> HtmlTag
visualizeAsHtmlLabelCustom vizF x = html (coerceToHtml (fst (vizF (Just x) {mkVSt & origVizType = VHtmlLabel, vizType = VHtmlLabel})))
	
visualizeAsTextLabelCustom :: !((Maybe a) -> .(*VSt -> (![Visualization], !*VSt))) !a -> String
visualizeAsTextLabelCustom vizF x = join " " (coerceToStrings (fst (vizF (Just x) {mkVSt & origVizType = VTextLabel, vizType = VTextLabel})))

//Generic visualizer
generic gVisualize a :: !(Maybe a) !*VSt -> (![Visualization], !*VSt)

gVisualize{|UNIT|} _ vst
	= ([],vst)
	
gVisualize{|FIELD of d|} fx val vst=:{vizType}
	# x = fmap fromFIELD val
	= case vizType of
		VEditorDefinition
			# (vizBody,vst=:{VSt|optional})	= fx x {VSt|vst & optional = False}
			# label							= {htmlDisplay (camelCaseToWords d.gfd_name +++ if optional "" "*" +++ ":") & width = Fixed 100}
			= ([TUIFragment {content = TUILayoutContainer {defaultLayoutContainer [label:coerceToTUIDefs vizBody] & orientation = Horizontal}, width = FillParent 1 ContentSize, height = (WrapContent 0), margins = Nothing}],{VSt|vst & optional = optional})
		VHtmlDisplay
			# (vizBody,vst) 				= fx x vst
			= case vizBody of
			 [] = ([],vst)
			 _  = ([HtmlFragment (TrTag [] [ThTag [] [Text (camelCaseToWords d.gfd_name),Text ": "],TdTag [] (coerceToHtml vizBody)])],vst)
		VTextDisplay
			# (vizBody,vst) 				= fx x vst
			= ([TextFragment (camelCaseToWords d.gfd_name),TextFragment ": " : vizBody]++[TextFragment " "],vst)
		_
			# (vizBody,vst)					= fx x vst
			= (vizBody,vst)
			
gVisualize{|OBJECT of d|} fx val vst=:{vizType,currentPath,selectedConsIndex = oldSelectedConsIndex,renderAsStatic,verifyMask,taskId,editEvent,controlSize}
	//For objects we only peek at the verify mask, but don't take it out of the state yet.
	//The masks are removed from the states when processing the CONS.
	# (cmv,vm)	= popMask verifyMask
	# x			= fmap fromOBJECT val
	//Record: just strip of the OBJECT constructor and pass through, record container is created when processing the CONS
	| isRecordType d
		= fx x vst
	= case vizType of
		VEditorDefinition
			//ADT with multiple constructors & not rendered static: Add the creation of a control for choosing the constructor
			| d.gtd_num_conses > 1 && not renderAsStatic
				# (items, vst=:{selectedConsIndex}) = fx x vst
				# content = if (isTouched cmv) (coerceToTUIDefs items) []
				= ([TUIFragment	{ content = TUILayoutContainer (defaultLayoutContainer
									[	addMsg (verifyElementStr cmv) (sizedControl controlSize (TUIComboControl [gdc.gcd_name \\ gdc <- d.gtd_conses])
												{ TUIControl
												| name			= dp2s currentPath
												, taskId		= taskId
												, value			= toJSON (if (isTouched cmv) (Just selectedConsIndex) Nothing)
												, eventValue	= eventValue currentPath editEvent
												})
									:	if (isEmpty content)
										[]
										[{ content	= TUILayoutContainer ({defaultLayoutContainer content & frame = True, baseCls = Just "x-constructor-panel"})
										,  width	= FillParent 1 ContentSize
										,  height	= WrapContent 0
										,  margins	= Just {sameMargins 0 & left = 12}
										}]
									])
								, width		= FillParent 1 ContentSize
								, height	= WrapContent 0
								, margins	= Nothing
								}
					]
				  ,{vst & currentPath = stepDataPath currentPath, selectedConsIndex = oldSelectedConsIndex})
			//ADT with one constructor: put content into container, if empty show cons name
			| otherwise
				# (vis,vst) = fx x vst
				# vis = case vis of
					[]	= if (isTouched cmv) [TUIFragment (htmlDisplay ((d.gtd_conses !! vst.selectedConsIndex).gcd_name))] []
					vis = [TUIFragment	{ content	= TUILayoutContainer (defaultLayoutContainer (coerceToTUIDefs vis))
										, width 	= FillParent 1 ContentSize
										, height	= Auto
										, margins	= Nothing
										}]
				= (vis,{vst & currentPath = stepDataPath currentPath, selectedConsIndex = oldSelectedConsIndex})
		_
			# (viz,vst) = fx x vst
			= (viz,{VSt|vst & currentPath = stepDataPath currentPath})
	
gVisualize{|CONS of d|} fx val vst=:{taskId,editEvent,currentPath,optional,controlSize} = visualizeCustomSimple mkControl staticVis val False vst
where
	mkControl name val _ _ renderAsStatic vst
		# x = fmap fromCONS val
		 = case isRecordCons d of
			False // normal ADT
				# (viz,vst)	= fx x vst
				# vst		= {VSt | vst & selectedConsIndex = d.gcd_index}
				= (coerceToTUIDefs viz,vst)
			True = case x of // record
				Nothing // Create checkbox to create record
					= ([checkbox False],vst)
				Just x
					# (viz,vst) = fx (Just x) vst
					= ([recordContainer viz],vst)
	where
		recordContainer viz =	{ content	= TUILayoutContainer (defaultLayoutContainer (if optional [checkbox True] [] ++ coerceToTUIDefs viz))
								, width		= FillParent 1 ContentSize
								, height	= (WrapContent 0)
								, margins	= Nothing
								}
											
		checkbox c = sizedControl controlSize TUIBoolControl
			{ name			= name
			, value			= toJSON c
			, taskId		= taskId
			, eventValue	= eventValue currentPath editEvent
			}
		
	staticVis v _ vst=:{vizType}
		# x = fmap (\(CONS c) -> c) v
		# (viz,vst) = fx x vst
		= case vizType of
			VHtmlDisplay
				//Records
				| isRecordCons d
					= ([HtmlFragment (TableTag [ClassAttr "viz-record"] (coerceToHtml viz))],vst)
				| otherwise
					= normalADTStaticViz viz vst
			VHtmlLabel
				//For records only show the first field
				| isRecordCons d
					= ([hd viz],vst)
				| otherwise
					= normalADTStaticViz viz vst
			VTextLabel
				//For records only show the first field
				| isRecordCons d 
					= ([hd viz],vst)
				| otherwise
					= normalADTStaticViz viz vst	
			VTextDisplay
				| isRecordCons d 
					= (viz,vst)
				| otherwise
					= normalADTStaticViz viz vst
	where
		normalADTStaticViz viz vst
			//If viz is empty, only show constructor name
			| isEmpty viz
				= ([TextFragment d.gcd_name],vst)
			//If there are multiple constructors, also show the name of the constructor
			| d.gcd_type_def.gtd_num_conses > 1
				= (intersperse (TextFragment " ") [TextFragment d.gcd_name:viz],vst)
			| otherwise
				= (intersperse (TextFragment " ") viz,vst)

gVisualize{|PAIR|} fx fy val vst
	# (x,y)			= (fmap fromPAIRX val, fmap fromPAIRY val)
	# (vizx, vst)	= fx x vst
	# (vizy, vst)	= fy y vst
	= (vizx ++ vizy, vst)

gVisualize{|EITHER|} fx fy val vst = case val of
		Nothing			= fx Nothing vst
		Just (LEFT x)	= fx (Just x) vst
		Just (RIGHT y)	= fy (Just y) vst

gVisualize{|Int|}		val vst = visualizeControlSimple TUIIntControl val vst
gVisualize{|Real|}		val vst = visualizeControlSimple TUIRealControl val vst
gVisualize{|Char|}		val vst = visualizeControlSimple TUICharControl val vst
gVisualize{|String|}	val vst = visualizeControlSimple TUIStringControl val vst
gVisualize{|Bool|}		val vst = visualizeControl TUIBoolControl (toString,toHtml) val vst
where
	toHtml :: !(Maybe Bool) -> HtmlTag
	toHtml val = DivTag
		[ClassAttr ("bool-htmllabel-icon bool-htmllabel-icon-"+++(toLowerCase (toString val)))]
		[SpanTag [ClassAttr "bool-htmllabel-text"] [(Text (toString val))]]
gVisualize{|Password|}	val vst = visualizeControl TUIPasswordControl (textOnly (const staticViz)) val vst
where
	staticViz = "********"
gVisualize{|Note|}		val vst = visualizeControl TUINoteControl (toString,toHtml) val vst
where
	toHtml :: !(Maybe Note) -> HtmlTag
	toHtml val = nl2br (toString val)
gVisualize{|Date|}		val vst = visualizeControlSimple TUIDateControl val vst
gVisualize{|Time|}		val vst = visualizeControlSimple TUITimeControl val vst
gVisualize{|User|}		val vst = visualizeControlSimple TUIUserControl val vst

gVisualize{|Currency|}	val vst = visualizeControl control (toString,RawText o toString o fmap (\c -> curLabel (Just c) +++ " " +++ toString c)) val vst
where
	control = TUICurrencyControl
		
	curLabel (Just (EUR _))	= "&euro;"
	curLabel (Just (GBP _))	= "&pound;"
	curLabel (Just (USD _))	= "$"
	curLabel (Just (JPY _)) = "&yen;"
	curLabel _				= "&euro;" //Use the default currency
	
gVisualize{|HtmlDisplay|} val vst = visualizeControl (TUIHtmlDisplay Nothing) (toString o fmap html2text,\mbD -> html (fmap RawText mbD)) (fmap fromHtmlDisplay val) vst

gVisualize {|Document|}	val vst = visualizeControl control (mkText,mkHtml) val vst
where
	control
		= TUIDocumentControl (fromMaybe {Document|documentId = "",name = "", mime = "", size = 0} val)
	mkText val = case val of
		Nothing								= noDocument
		Just document
			| document.Document.size == 0	= noDocument
			| otherwise						= document.Document.name
			
	mkHtml val = case val of
		Nothing
			= html noDocument
		Just document
			| document.Document.size == 0
				= html noDocument
			| otherwise
				# downLink = ATag [HrefAttr (buildLink document)
								  ,TargetAttr "_blank"
								  ,NameAttr "x-form-document-link"] [ImgTag [SrcAttr "skins/default/img/icons/page_white_put.png"]]
				# prevLink = ATag [HrefAttr "#"
								  ,NameAttr "x-form-document-preview-link" ] [ImgTag [SrcAttr "skins/default/img/icons/zoom.png"]]			
				= html [Text ( document.Document.name +++" ("+++printByteSize document.Document.size+++") "),RawText "&nbsp;",downLink,prevLink]
			
	noDocument = "No Document"
	buildLink document = "/services/json/documents/" +++ document.Document.documentId +++ "/download"
	printByteSize size
		| size >= 1048576 = toString (fixReal ((toReal size)/(toReal 1048576)))+++" Mbyte"
		| size >= 1024    = toString (fixReal ((toReal size)/(toReal 1024)))+++" Kbyte"
		| otherwise 	  = toString size +++ " byte"
	fixReal r = (toReal (toInt (r*100.0)))/100.0
	
gVisualize{|FormButton|} val vst = visualizeControl2 control (textOnly buttonLabel) (fmap (\b=:{state} -> (state,b)) val) vst
where
	control
		= TUIButtonControl	{ TUIButtonControl
							| label = buttonLabel val
							, iconCls = icon val
							}
								
	buttonLabel	b = toString ((fmap (\b -> b.FormButton.label)) b)
	icon		b = toString ((fmap (\b -> b.FormButton.icon)) b)

gVisualize{|RadioChoice|} fv _ val vst = visualizeControl2 (TUIChoiceControl (toChoice val)) (toString`,toHtml) (fmap (\r=:(RadioChoice _ mbSel) -> (maybe [] (\l -> [l]) mbSel,r)) val) vst
where
	toChoice Nothing						= {allowMultiple = False, options = []}
	toChoice (Just (RadioChoice options _))	= {allowMultiple = False, options = [visualizeAsTextLabelCustom fv v \\ (v,_) <- options]}
	
	toString` mbChoice	= fromMaybe "No item selected"			(getMbView (visualizeAsTextLabelCustom fv) mbChoice)
	toHtml mbChoice		= fromMaybe (Text "No item selected")	(getMbView (visualizeAsHtmlLabelCustom fv) mbChoice)
	
gVisualize{|ComboChoice|} fv _ val vst = visualizeControl2 (TUIComboControl (toChoice val)) (toString`,toHtml) (fmap (\c=:(ComboChoice _ mbSel) -> (mbSel,c)) val) vst
where
	toChoice Nothing						= []
	toChoice (Just (ComboChoice options _))	= [visualizeAsTextLabelCustom fv v \\ (v,_) <- options]
	
	toString` mbChoice	= fromMaybe "No item selected"			(getMbView (visualizeAsTextLabelCustom fv) mbChoice)
	toHtml mbChoice		= fromMaybe (Text "No item selected")	(getMbView (visualizeAsHtmlLabelCustom fv) mbChoice)
	
gVisualize{|TreeChoice|} fv _ val vst = visualizeControl2 (TUITreeControl (toTree val)) (toString`,toHtml) (fmap (\t=:(TreeChoice _ mbSel) -> (mbSel,t)) val) vst
where
	toTree Nothing								= []
	toTree (Just (TreeChoice (Tree nodes) _))	= fst (mkTree nodes 0)
	
	mkTree [] idx
		= ([],idx)
	mkTree [Leaf (v,_):r] idx
		# (rtree,idx`) 		= mkTree r (inc idx)
		= ([{text = visualizeAsTextLabelCustom fv v, index = Just idx, leaf = True, children = Nothing}:rtree],idx`)
	mkTree [Node label nodes:r] idx
		# (children,idx)	= mkTree nodes idx
		# (rtree,idx)		= mkTree r idx
		= ([{text = label, index = Nothing, leaf = False, children = Just children}:rtree],idx)
	
	toString` mbChoice	= fromMaybe "No item selected"			(getMbView (visualizeAsTextLabelCustom fv) mbChoice)
	
	toHtml mbChoice		= fromMaybe (Text "Empty tree")			(fmap (\(TreeChoice (Tree nodes) mbSel) -> html (fst (mkHtmlDisplay nodes 0 mbSel))) mbChoice)
	
	mkHtmlDisplay [] idx sel
		= ([],idx)
	mkHtmlDisplay [Leaf (v,_):r] idx sel
		# (rtree,idx`)	= mkHtmlDisplay r (inc idx) sel
		= ([LiTag (itemCls idx) [visualizeAsHtmlLabelCustom fv v]:rtree],idx`)
	where
		itemCls idx
			| Just idx == sel	= [ClassAttr "tree-list-item-selected"]
			| isEven idx		= [ClassAttr "list-item-light"]
			| otherwise			= [ClassAttr "list-item-dark"]
	mkHtmlDisplay [Node label nodes:r] idx sel
		# (children,idx)	= mkHtmlDisplay nodes idx sel
		# (rtree,idx)		= mkHtmlDisplay r idx sel
		= ([LiTag [ClassAttr "tree-list-node-header"] [Text label],UlTag ulClass children:rtree],idx)
	where
		ulClass = [ClassAttr "tree-list"]
	
getMbView f mbChoice = fmap f (maybe Nothing getMbSelectionView mbChoice)
	
gVisualize{|CheckMultiChoice|} fv _ val vst = visualizeControl2 (TUIChoiceControl (toChoice val)) (toString`,toHtml) (fmap (\r=:(CheckMultiChoice _ mbSel) -> (mbSel,r)) val) vst
where
	toChoice Nothing								= {allowMultiple = True, options = []}
	toChoice (Just (CheckMultiChoice options _))	= {allowMultiple = True, options = [toString (visualizeAsHtmlLabelCustom fv v) \\ (v,_) <- options]}
	
	toString` mbChoice	= visualizeAsTextLabelCustom (gVisualize{|* -> *|} fv) (maybe [] getSelectionViews mbChoice)
	toHtml mbChoice		= visualizeAsHtmlLabelCustom (gVisualize{|* -> *|} fv) (maybe [] getSelectionViews mbChoice)
	
gVisualize{|Table|} val vst = visualizeControl2 (TUIGridControl (toGrid val)) (toString`,toHtml) (fmap (\t=:(Table _ _ mbSel) -> (mbSel,t)) val) vst
where
	toGrid Nothing							= {cells = [], headers = []}
	toGrid (Just (Table headers cells _))	= {headers = headers, cells = map (map toString) cells}
	
	toString` _ = "Table"

	toHtml Nothing = Text "Empty table"
	toHtml (Just (Table headers cells mbSel)) = TableTag [] [TrTag [] [ThTag [] [Text header] \\ header <- headers]:map mkRow cells]
	mkRow row = TrTag [] [TdTag [] [cel] \\ cel <- row]
	
gVisualize {|[]|} fx val vst = visualizeCustomSimple mkControl staticVis val False vst
where
	mkControl name val touched verRes renderAsStatic vst=:{VSt|taskId}
		# val = listValue val
		# (items,vst) = TUIDef val vst
		= (addMsg verRes
			{ content	= TUIListContainer
							{ TUIListContainer
							| items = items
							, name = name
							, staticDisplay = renderAsStatic
							, taskId = taskId}
			, width		= Auto
			, height	= Auto
			, margins	= Nothing
			}
			,vst)
		where
			TUIDef items vst
				# (itemsVis,vst)	= childVisualizations fx items Nothing vst
				# vis				= [listItemControl idx (coerceToTUIDefs dx) \\ dx <- itemsVis & idx <- [0..]]
				# (vis,vst)			= case renderAsStatic of
					False
						# (dx,vst)  = fx Nothing vst
						= (vis ++ [listItemControl (length vis) (coerceToTUIDefs dx)],vst)
					True
						= (vis,vst)
				= (vis,vst)
						
			listItemControl idx defs
				=	{ content	= TUIListItem
									{ TUIListItem
									| index = idx
									, items = case defs of
										[def]	= def
										defs	= {content = TUILayoutContainer (defaultLayoutContainer defs), width = FillParent 1 ContentSize, height = (WrapContent 0), margins = Nothing}
									}
					, width		= Auto
					, height	= Auto
					, margins	= Nothing
					}
			
			addMsg verSt list = case verSt of
				NoMsg			= [list]
				HintMsg	msg		= addMsg` "x-hint-icon" msg list
				ErrorMsg msg	= addMsg` "x-invalid-icon" msg list
			addMsg` cls msg list = [	{ content	= TUILayoutContainer (defaultLayoutContainer [list,mkMessage cls msg])
										, width		= FillParent 1 ContentSize
										, height	= WrapContent 0
										, margins	= Nothing
										}]
			mkMessage cls msg =	htmlDisplay (DivTag [ClassAttr "list-msg-field"] [DivTag [ClassAttr cls] [Text msg]])
								
							
	staticVis v _ vst=:{vizType}
		# v = listValue v
		# (items,vst) = childVisualizations fx v Nothing vst
		= case vizType of
			VHtmlDisplay = case v of
				[]	= ([HtmlFragment (UlTag [] [LiTag [ClassAttr "list-item-light"] [(Text empty)]])],vst)
				_	= ([HtmlFragment (UlTag [] [(LiTag [ClassAttr (itemCls i)] (coerceToHtml x)) \\ x <- items & i <- [0..]])],vst)
			VHtmlLabel = case v of
				[]	= ([HtmlFragment (Text empty)],vst)
				_	= ([HtmlFragment (html (htmlLabel items))],vst)
			_
					= ([TextFragment ("["+++join ", " (flatten  [(coerceToStrings x) \\ x <-items])+++"]")],vst)
	where
		itemCls i
			| isEven i  = "list-item-light"
			| otherwise = "list-item-dark"
			
		htmlLabel [i]		= coerceToHtml i
		htmlLabel [i:is]	= coerceToHtml i ++ [(Text ", ")] ++ htmlLabel is
		
	listValue l = fromMaybe [] l
		
	empty = "Empty list"
	
gVisualize{|Dynamic|}			_ vst	= noVisualization vst
gVisualize{|(->)|} _ _			_ vst	= noVisualization vst

gVisualize{|Maybe|} fx val vst=:{vizType,currentPath}
	# vst = {VSt|vst & optional = True}
	# (viz,vst) = case vizType of
		VEditorDefinition = case val of
			Just (Just x)	= fx (Just x) vst
			_				= fx Nothing vst
		_ = case val of
			Just Nothing	= ([TextFragment "-"],vst)
			Just (Just x)	= fx (Just x) vst
			Nothing			= ([],vst)
	= (viz, {VSt|vst & optional = True, currentPath = stepDataPath currentPath})
	
// wrapper types changing visualization behaviour
gVisualize{|Hidden|} fx val vst=:{VSt | currentPath, verifyMask}
	# (_,vm) = popMask verifyMask	
	= ([],{VSt | vst & currentPath = stepDataPath currentPath, verifyMask = vm})

gVisualize{|Display|} fx val vst=:{currentPath,renderAsStatic}
	# (def,vst) = fx (fmap fromDisplay val) {VSt | vst &  renderAsStatic = True}
	= (def,{VSt | vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})

gVisualize{|Editable|} fx val vst=:{currentPath, renderAsStatic}
	# (def,vst) = fx (fmap fromEditable val) {VSt | vst & renderAsStatic = False}
	= (def,{VSt | vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})

gVisualize{|VisualizationHint|} fx val vst=:{currentPath, vizType, taskId}
	= case val of
		Just (VHHidden x)	= gVisualize{|* -> *|} fx (Just (Hidden x)) vst
		Just (VHDisplay x)	= gVisualize{|* -> *|} fx (Just (Display x)) vst
		Just (VHEditable x)	= gVisualize{|* -> *|} fx (Just (Editable x)) vst
		Nothing				= fx Nothing vst

gVisualize{|ControlSize|} fx val vst=:{controlSize}
	= case val of
		Nothing
			= fx Nothing vst
		(Just (ControlSize width height margins v))
			# (def,vst) = fx (Just v) {VSt | vst &  controlSize = (width,height,margins)}
			= (def,{VSt | vst & controlSize = controlSize})
			
gVisualize{|FillControlSize|} fx val vst=:{controlSize=controlSize=:(_,_,margins)}
	# (def,vst) = fx (fmap fromFillControlSize val) {vst & controlSize = (FillParent 1 ContentSize,FillParent 1 ContentSize,margins)}
	= (def,{vst & controlSize = controlSize})

gVisualize{|FillWControlSize|} fx val vst=:{controlSize=controlSize=:(_,height,margins)}
	# (def,vst) = fx (fmap fromFillWControlSize val) {vst & controlSize = (FillParent 1 ContentSize,height,margins)}
	= (def,{vst & controlSize = controlSize})
	
gVisualize{|FillHControlSize|} fx val vst=:{controlSize=controlSize=:(width,_,margins)}
	# (def,vst) = fx (fmap fromFillHControlSize val) {vst & controlSize = (width,FillParent 1 ContentSize,margins)}
	= (def,{vst & controlSize = controlSize})

gVisualize{|Void|} _ vst = noVisualization vst
gVisualize{|WorkflowTaskContainer|} _ vst = noVisualization vst
	
derive gVisualize DateTime, Either, (,), (,,), (,,,), UserDetails, Timestamp, Map, EmailAddress, Action, TreeNode, WorkflowDescription, ManagerProperties, RunningTaskStatus, TaskPriority, Session, Tree
derive bimap Maybe

//***** UTILITY FUNCTIONS *************************************************************************************************	
textOnly :: !((Maybe a) -> String) -> StaticVizFunctions a
textOnly textF = (textF,html o textF)

visualizeControlSimple :: !TUIControlType !(Maybe a) !*VSt -> *(![Visualization],!*VSt) | JSONEncode{|*|},toString a
visualizeControlSimple control v vst = visualizeControl2 control (textOnly toString) (fmap (\a -> (a,a)) v) vst

visualizeControl :: !TUIControlType !(StaticVizFunctions a) !(Maybe a) !*VSt -> *(![Visualization],!*VSt) | JSONEncode{|*|} a
visualizeControl control staticF v vst = visualizeControl2 control staticF (fmap (\a -> (a,a)) v) vst

visualizeControl2 :: !TUIControlType !(StaticVizFunctions b) !(Maybe (!a,!b)) !*VSt -> *(![Visualization],!*VSt) | JSONEncode{|*|} a
visualizeControl2 control (strF,htmlF) v vst=:{editEvent,currentPath,controlSize} = visualizeCustom tuiF staticF v True vst
where
	tuiF name v touched verRes _ vst=:{VSt|taskId}
		# v = checkMask touched v
		# viz = sizedControl controlSize control	{ TUIControl
													| name = name
													, value = toJSON v
													, eventValue = eventValue currentPath editEvent
													, taskId = taskId
													}
		= ([addMsg verRes viz],vst)
								
	staticF v touched vst=:{vizType}
		# v = checkMask touched v
		# vis = case vizType of
			VHtmlDisplay
				= [HtmlFragment (htmlF v)]
			VHtmlLabel
				= [HtmlFragment (htmlF v)]
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

visualizeCustomSimple :: !(TUIVizFunction a) !(StaticVizFunctionCustom a) !(Maybe a) !Bool !*VSt -> *(![Visualization],!*VSt)
visualizeCustomSimple tuiF staticF v staticHtmlContainer vst = visualizeCustom tuiF staticF (fmap (\a -> (a,a)) v) staticHtmlContainer vst

visualizeCustom :: !(TUIVizFunction a) !(StaticVizFunctionCustom b) !(Maybe (!a,!b)) !Bool !*VSt -> *(![Visualization],!*VSt)
visualizeCustom tuiF staticF v staticHtmlContainer vst=:{vizType,origVizType,currentPath,renderAsStatic,verifyMask}
	# (cmv,vm)	= popMask verifyMask
	// only check mask if generating editor definition & not for labels
	# touched	= case origVizType of
		VEditorDefinition = case vizType of
			VTextLabel	= True
			VHtmlLabel	= True
			_			= isTouched cmv
		_				= True
	# vst		= {VSt|vst & currentPath = shiftDataPath currentPath, verifyMask = childMasks cmv}
	# (vis,vst) = case vizType of
		VEditorDefinition
			# (vis,vst) = case (renderAsStatic,staticHtmlContainer) of
				(True,True)
					# (vis,vst)	= staticF (fmap snd v) touched {vst & vizType = VHtmlDisplay}
					# vst		= {vst & vizType = vizType} 
					= ([htmlDisplay (toString (html (coerceToHtml vis)))],vst)
				_
					= tuiF (dp2s currentPath) (fmap fst v) touched (verifyElementStr cmv) renderAsStatic vst
				
			= (map TUIFragment vis,vst)
		_
			= staticF (fmap snd v) touched vst
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

sizedControl :: !(!TUISize,!TUISize,!(Maybe TUIMargins)) !TUIControlType !TUIControl -> TUIDef
sizedControl (width,height,mbMargins) type control = {content = TUIControl type control, width = width, height = height, margins = mbMargins}

verifyElementStr :: !VerifyMask -> VerifyResult
verifyElementStr cmv = case cmv of
	VMValid mbHnt _			= maybe NoMsg HintMsg mbHnt
	VMUntouched mbHnt _ _	= maybe NoMsg HintMsg mbHnt
	VMInvalid err _			= ErrorMsg (toString err)
	
eventValue :: !DataPath !(Maybe (!DataPath,!JSONNode)) -> Maybe JSONNode
eventValue currentPath mbEvent = case mbEvent of
	Just (dp,val) | dp == currentPath	= Just val
	_									= Nothing

addMsg :: !VerifyResult !TUIDef -> TUIDef
addMsg verRes viz = case verRes of
		NoMsg			= viz
		HintMsg msg		= add "x-hint-icon" msg viz
		ErrorMsg msg	= add "x-invalid-icon" msg viz
where	
	add cls msg viz= {content = TUILayoutContainer {defaultLayoutContainer [viz,mkIcon cls msg] & orientation = Horizontal}, width = FillParent 1 ContentSize, height = WrapContent 0, margins = Nothing}
	mkIcon cls msg = { content	= TUIControl (TUIHtmlDisplay (Just msg))
									{ TUIControl
									| name			= ""
									, value			= JSONString (toString (DivTag [ClassAttr cls] []))
									, eventValue	= Nothing
									, taskId		= ""
									}
					, width		= WrapContent 0
					, height	= WrapContent 0
					, margins	= Nothing
					}

//*********************************************************************************************************************

//Coercion of visualizations
coerceToTUIDefs :: ![Visualization] -> [TUIDef]
coerceToTUIDefs visualizations = [d \\ (TUIFragment d) <- visualizations]

coerceToStrings :: ![Visualization] -> [String]
coerceToStrings visualizations = [s \\ (TextFragment s) <- visualizations]

coerceToHtml :: ![Visualization] -> [HtmlTag]
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