implementation module GenVisualize

import StdBool, StdChar, StdList, StdArray, StdTuple, StdMisc, StdGeneric, StdEnum, StdFunc, List, Generic
import GenUpdate, GenVerify, Util, Maybe, Functor, Text, HTML, JSON, TUIDefinition, SystemTypes, HtmlUtil

visualizeAsEditor :: !a !TaskId !Int !VerifyMask !(Maybe (!DataPath,!JSONNode)) -> TUIDef | gVisualizeEditor{|*|} a
visualizeAsEditor x taskId idx vmask editEvent
	# vst = {mkVSt & verifyMask = [vmask], editEvent = editEvent, taskId = taskId, currentPath = shiftDataPath (childDataPath emptyDataPath idx)}
	# (defs,vst) = gVisualizeEditor{|*|} (Just x) vst
	= case defs of
		[tui]   = tui
		tuis    = {content = TUILayoutContainer (defaultLayoutContainer tuis), width = WrapContent 0, height = WrapContent 0, margins = Nothing}

visualizeAsText :: !StaticVisualizationMode !a -> String | gVisualizeText{|*|} a
visualizeAsText mode v = concat (gVisualizeText{|*|} mode v)

visualizeAsHtml :: !StaticVisualizationMode !a -> HtmlTag | gVisualizeHtml{|*|} a
visualizeAsHtml mode v = html (gVisualizeHtml{|*|} mode v)

//Generic text visualizer
generic gVisualizeText a :: !StaticVisualizationMode !a -> [String]

gVisualizeText{|UNIT|} _ _ = []

gVisualizeText{|FIELD of d|} fx mode (FIELD x)
	# viz = fx mode x
	= case mode of
		AsDisplay	= [camelCaseToWords d.gfd_name, ": ": viz] ++ [" "]
		AsLabel	= viz
			
gVisualizeText{|OBJECT of d|} fx mode (OBJECT x) = fx mode x
	
gVisualizeText{|CONS of d|} fx mode (CONS x)
	# viz = fx mode x
	= case mode of
		AsLabel
			//For records only show the first field
			| isRecordCons d	= take 1 viz
			| otherwise			= normalADTStaticViz viz	
		AsDisplay
			| isRecordCons d	= viz
			| otherwise			= normalADTStaticViz viz
where
	normalADTStaticViz viz
		//If viz is empty, only show constructor name
		| isEmpty viz
			= [d.gcd_name]
		//If there are multiple constructors, also show the name of the constructor
		| d.gcd_type_def.gtd_num_conses > 1
			= intersperse " " [d.gcd_name:viz]
		//Otherwise show visualisation of fields separated by spaces
		| otherwise
			= intersperse " " viz

gVisualizeText{|PAIR|} fx fy mode (PAIR x y) = fx mode x ++ fy mode y

gVisualizeText{|EITHER|} fx fy mode either = case either of
	LEFT x	= fx mode x
	RIGHT y	= fy mode y

gVisualizeText{|Int|}			_ val				= [toString val]
gVisualizeText{|Real|}			_ val				= [toString val]
gVisualizeText{|Char|}			_ val				= [toString val]
gVisualizeText{|String|}		_ val				= [toString val]
gVisualizeText{|Bool|}			_ val				= [toString val]
gVisualizeText{|Password|}		_ val				= ["********"]
gVisualizeText{|Note|}			_ val				= [toString val]
gVisualizeText{|Date|}			_ val				= [toString val]
gVisualizeText{|Time|}			_ val				= [toString val]
gVisualizeText{|User|}			_ val				= [toString val]
gVisualizeText{|Currency|}		_ val				= [toString val]
gVisualizeText{|HtmlDisplay|}	_ val				= [html2text (toString val)]
gVisualizeText{|HtmlInclude|}	_ val				= ["Html include"]
gVisualizeText{|FormButton|}	_ val				= [val.FormButton.label]
gVisualizeText{|Document|}		_ val
	| val.Document.size == 0						= ["No Document"]
	| otherwise										= [val.Document.name]
gVisualizeText{|RadioChoice|}		fv _ mode val	= fromMaybe ["No item selected"] (fmap (\v -> fv mode v) (getMbSelectionView val))
gVisualizeText{|ComboChoice|}		fv _ mode val	= fromMaybe ["No item selected"] (fmap (\v -> fv mode v) (getMbSelectionView val))	
gVisualizeText{|TreeChoice|}		fv _ mode val	= fromMaybe ["No item selected"] (fmap (\v -> fv mode v) (getMbSelectionView val))
gVisualizeText{|CheckMultiChoice|}	fv _ _ val		= gVisualizeText{|* -> *|} fv  AsLabel (getSelectionViews val)
gVisualizeText{|Table|}	_ _							= ["Table"]
gVisualizeText {|[]|} fx  mode val					= ["[":  flatten (intersperse [", "] [fx mode x \\ x <- val])] ++ ["]"]
gVisualizeText{|Maybe|} fx mode val					= fromMaybe ["-"] (fmap (\v -> fx mode v) val)
gVisualizeText{|Hidden|} _ _ _						= []
gVisualizeText{|Display|} fx mode (Display val)		= fx mode val
gVisualizeText{|Editable|} fx mode(Editable val)	= fx mode val

gVisualizeText{|VisualizationHint|} fx mode val = case val of
	VHHidden x		= gVisualizeText{|* -> *|} fx mode (Hidden x)
	VHDisplay x		= gVisualizeText{|* -> *|} fx mode (Display x)
	VHEditable x	= gVisualizeText{|* -> *|} fx mode (Editable x)

gVisualizeText{|ControlSize|}		fx mode val = fx mode (fromControlSize val)
gVisualizeText{|FillControlSize|}	fx mode val = fx mode (fromFillControlSize val)
gVisualizeText{|FillWControlSize|}	fx mode val = fx mode (fromFillWControlSize val)
gVisualizeText{|FillHControlSize|}	fx mode val = fx mode (fromFillHControlSize val)

gVisualizeText{|Void|} _ _					= []
gVisualizeText{|Dynamic|} _ _				= []
gVisualizeText{|(->)|} _ _ _ _				= []
gVisualizeText{|WorkflowTaskContainer|} _ _	= []
	
derive gVisualizeText DateTime, Either, (,), (,,), (,,,), UserDetails, Timestamp, Map, EmailAddress, Action, TreeNode, WorkflowDescription, ManagerProperties, RunningTaskStatus, TaskPriority, Session, Tree

//Generic html visualizer
generic gVisualizeHtml a :: !StaticVisualizationMode !a -> [HtmlTag]

gVisualizeHtml{|UNIT|} _ _ = []
	
gVisualizeHtml{|FIELD of d|} fx mode (FIELD x)
	# viz = fx mode x
	= case mode of
		AsDisplay
			| isEmpty viz	= []
			| otherwise		= [TrTag [] [ThTag [] [Text (camelCaseToWords d.gfd_name),Text ": "],TdTag [] viz]]
		AsLabel	= viz
			
gVisualizeHtml{|OBJECT of d|} fx mode (OBJECT x) = fx mode x
	
gVisualizeHtml{|CONS of d|} fx mode (CONS x)
	# viz = fx mode x
	= case mode of
		AsLabel
			//For records only show the first field
			| isRecordCons d	= take 1 viz
			| otherwise			= normalADTStaticViz viz	
		AsDisplay
			| isRecordCons d	= [TableTag [ClassAttr "viz-record"] viz]
			| otherwise			= normalADTStaticViz viz
where
	normalADTStaticViz viz
		//If viz is empty, only show constructor name
		| isEmpty viz
			= [Text d.gcd_name]
		//If there are multiple constructors, also show the name of the constructor
		| d.gcd_type_def.gtd_num_conses > 1
			= intersperse (Text " ") [Text d.gcd_name:viz]
		//Otherwise show visualisation of fields separated by spaces
		| otherwise
			= intersperse (Text " ") viz

gVisualizeHtml{|PAIR|} fx fy mode (PAIR x y) = fx mode x ++ fy mode y

gVisualizeHtml{|EITHER|} fx fy mode either = case either of
	LEFT x	= fx mode x
	RIGHT y	= fy mode y

gVisualizeHtml{|Int|}			_ val				= [toHtmlText val]
gVisualizeHtml{|Real|}			_ val				= [toHtmlText val]
gVisualizeHtml{|Char|}			_ val				= [toHtmlText val]
gVisualizeHtml{|String|}		_ val				= [toHtmlText val]
gVisualizeHtml{|Bool|}			_ val				= [DivTag
														[ClassAttr ("bool-htmllabel-icon bool-htmllabel-icon-"+++(toLowerCase (toString val)))]
														[SpanTag [ClassAttr "bool-htmllabel-text"] [(Text (toString val))]]
													]
gVisualizeHtml{|Password|}		_ val				= [Text "********"]
gVisualizeHtml{|Note|}			_ val				= [nl2br (toString val)]
gVisualizeHtml{|Date|}			_ val				= [toHtmlText val]
gVisualizeHtml{|Time|}			_ val				= [toHtmlText val]
gVisualizeHtml{|User|}			_ val				= [toHtmlText val]
gVisualizeHtml{|Currency|}		_ val				= [RawText (curLabel (Just val) +++ " " +++ toString val)]
gVisualizeHtml{|HtmlDisplay|}	_ val				= [RawText (fromHtmlDisplay val)]
gVisualizeHtml{|HtmlInclude|}	_ (HtmlInclude url)	= [IframeTag [SrcAttr url, FrameborderAttr "0", WidthAttr "100%", HeightAttr "100%"] []]
gVisualizeHtml{|FormButton|}	_ val				= [Text val.FormButton.label]
gVisualizeHtml{|Document|}		_ val
	| val.Document.size == 0
		= [Text "No Document"]
	| otherwise
		# downLink = ATag [HrefAttr (buildLink val)
						  ,TargetAttr "_blank"
						  ,NameAttr "x-form-document-link"] [ImgTag [SrcAttr "skins/default/img/icons/page_white_put.png"]]
		# prevLink = ATag [HrefAttr "#"
						  ,NameAttr "x-form-document-preview-link" ] [ImgTag [SrcAttr "skins/default/img/icons/zoom.png"]]	
		= [Text ( val.Document.name +++" ("+++printByteSize val.Document.size+++") "),RawText "&nbsp;",downLink,prevLink]
where
	buildLink document = "/services/json/documents/" +++ document.Document.documentId +++ "/download"
	printByteSize size
		| size >= 1048576 = toString (fixReal ((toReal size)/(toReal 1048576)))+++" Mbyte"
		| size >= 1024    = toString (fixReal ((toReal size)/(toReal 1024)))+++" Kbyte"
		| otherwise 	  = toString size +++ " byte"
	fixReal r = (toReal (toInt (r*100.0)))/100.0
gVisualizeHtml{|RadioChoice|}		fv _ mode val	= fromMaybe [Text "No item selected"] (fmap (\v -> fv mode v) (getMbSelectionView val))
gVisualizeHtml{|ComboChoice|}		fv _ mode val	= fromMaybe [Text "No item selected"] (fmap (\v -> fv mode v) (getMbSelectionView val))	
gVisualizeHtml{|TreeChoice|} fv _ mode (TreeChoice (Tree nodes) mbSel) = fst (mkHtmlDisplay nodes 0 mbSel)
where	
	mkHtmlDisplay [] idx sel
		= ([],idx)
	mkHtmlDisplay [Leaf (v,_):r] idx sel
		# (rtree,idx`) = mkHtmlDisplay r (inc idx) sel
		= ([LiTag (itemCls idx) (fv AsLabel v):rtree],idx`)
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
gVisualizeHtml{|CheckMultiChoice|}	fv _ _ val		= gVisualizeHtml{|* -> *|} fv  AsLabel (getSelectionViews val)
gVisualizeHtml{|Table|}	_ val						= toHtml val
where
	toHtml (Table headers cells mbSel) = [TableTag [] [TrTag [] [ThTag [] [Text header] \\ header <- headers]:map mkRow cells]]
	mkRow row = TrTag [] [TdTag [] [cel] \\ cel <- row]
gVisualizeHtml {|[]|} fx  mode val
	# items = [fx mode x \\ x <- val]
	= case mode of
		AsDisplay = case val of
			[]	= [UlTag [] [LiTag [ClassAttr "list-item-light"] empty]]
			_	= [UlTag [] [LiTag [ClassAttr (itemCls i)] x \\ x <- items & i <- [0..]]]
		AsLabel = case val of
			[]	= empty
			_	= flatten (intersperse [Text ", "] items)
where
	itemCls i
			| isEven i  = "list-item-light"
			| otherwise = "list-item-dark"

	empty = [Text "Empty list"]
gVisualizeHtml{|Maybe|} fx mode val					= fromMaybe [Text "-"] (fmap (\v -> fx mode v) val)
gVisualizeHtml{|Hidden|} _ _ _						= []
gVisualizeHtml{|Display|} fx mode (Display val)		= fx mode val
gVisualizeHtml{|Editable|} fx mode(Editable val)	= fx mode val

gVisualizeHtml{|VisualizationHint|} fx mode val = case val of
	VHHidden x		= gVisualizeHtml{|* -> *|} fx mode (Hidden x)
	VHDisplay x		= gVisualizeHtml{|* -> *|} fx mode (Display x)
	VHEditable x	= gVisualizeHtml{|* -> *|} fx mode (Editable x)

gVisualizeHtml{|ControlSize|}		fx mode val = fx mode (fromControlSize val)
gVisualizeHtml{|FillControlSize|}	fx mode val = fx mode (fromFillControlSize val)
gVisualizeHtml{|FillWControlSize|}	fx mode val = fx mode (fromFillWControlSize val)
gVisualizeHtml{|FillHControlSize|}	fx mode val = fx mode (fromFillHControlSize val)

gVisualizeHtml{|Void|} _ _					= []
gVisualizeHtml{|Dynamic|} _ _				= []
gVisualizeHtml{|(->)|} _ _ _ _				= []
gVisualizeHtml{|WorkflowTaskContainer|} _ _	= []
	
derive gVisualizeHtml DateTime, Either, (,), (,,), (,,,), UserDetails, Timestamp, Map, EmailAddress, Action, TreeNode, WorkflowDescription, ManagerProperties, RunningTaskStatus, TaskPriority, Session, Tree

toHtmlText s = Text (toString s)

curLabel (Just (EUR _))	= "&euro;"
curLabel (Just (GBP _))	= "&pound;"
curLabel (Just (USD _))	= "$"
curLabel (Just (JPY _)) = "&yen;"
curLabel _				= "&euro;" //Use the default currency

mkVSt :: *VSt
mkVSt = {VSt| currentPath = startDataPath,
		selectedConsIndex = -1, optional = False, renderAsStatic = False, verifyMask = [], editEvent = Nothing, taskId = "",
		controlSize = (Auto,Auto,Nothing)}

//Generic visualizer
generic gVisualizeEditor a | gVisualizeText a, gVisualizeHtml a :: !(Maybe a) !*VSt -> (![TUIDef], !*VSt)

gVisualizeEditor{|UNIT|} _ vst
	= ([],vst)
	
gVisualizeEditor{|FIELD of d|} fx _ _ val vst
	# x = fmap fromFIELD val
	# (vizBody,vst=:{VSt|optional})	= fx x {VSt|vst & optional = False}
	# label							= {htmlDisplay (camelCaseToWords d.gfd_name +++ if optional "" "*" +++ ":") & width = Fixed 100}
	= ([{content = TUILayoutContainer {defaultLayoutContainer [label: vizBody] & orientation = Horizontal}, width = FillParent 1 ContentSize, height = (WrapContent 0), margins = Nothing}],{VSt|vst & optional = optional})
			
gVisualizeEditor{|OBJECT of d|} fx _ _ val vst=:{currentPath,selectedConsIndex = oldSelectedConsIndex,renderAsStatic,verifyMask,taskId,editEvent,controlSize}
	//For objects we only peek at the verify mask, but don't take it out of the state yet.
	//The masks are removed from the states when processing the CONS.
	# (cmv,vm)	= popMask verifyMask
	# x			= fmap fromOBJECT val
	//Record: just strip of the OBJECT constructor and pass through, record container is created when processing the CONS
	| isRecordType d
		= fx x vst
	//ADT with multiple constructors & not rendered static: Add the creation of a control for choosing the constructor
	| d.gtd_num_conses > 1 && not renderAsStatic
		# (items, vst=:{selectedConsIndex}) = fx x vst
		# content = if (isTouched cmv) items []
		= ([{ content = TUILayoutContainer (defaultLayoutContainer
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
	//ADT with one constructor or static render: put content into container, if empty show cons name
	| otherwise
		# (vis,vst) = fx x vst
		# vis = case vis of
			[]	= if (isTouched cmv) [(htmlDisplay ((d.gtd_conses !! vst.selectedConsIndex).gcd_name))] []
			vis = [{ content	= TUILayoutContainer (defaultLayoutContainer vis)
								, width 	= FillParent 1 ContentSize
								, height	= Auto
								, margins	= Nothing
								}]
		= (vis,{vst & currentPath = stepDataPath currentPath, selectedConsIndex = oldSelectedConsIndex})

gVisualizeEditor{|CONS of d|} fx _ _ val vst=:{taskId,editEvent,currentPath,optional,controlSize} = visualizeCustom mkControl val vst
where
	mkControl name val _ _ renderAsStatic vst
		# x = fmap fromCONS val
		 = case isRecordCons d of
			False // normal ADT
				# (viz,vst)	= fx x vst
				# vst		= {VSt | vst & selectedConsIndex = d.gcd_index}
				= (viz,vst)
			True = case x of // record
				Nothing // Create checkbox to create record
					= ([checkbox False],vst)
				Just x
					# (viz,vst) = fx (Just x) vst
					= ([recordContainer viz],vst)
	where
		recordContainer viz =	{ content	= TUILayoutContainer (defaultLayoutContainer (if optional [checkbox True] [] ++ viz))
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

gVisualizeEditor{|PAIR|} fx _ _ fy _ _ val vst
	# (x,y)			= (fmap fromPAIRX val, fmap fromPAIRY val)
	# (vizx, vst)	= fx x vst
	# (vizy, vst)	= fy y vst
	= (vizx ++ vizy, vst)

gVisualizeEditor{|EITHER|} fx _ _ fy _ _ val vst = case val of
		Nothing			= fx Nothing vst
		Just (LEFT x)	= fx (Just x) vst
		Just (RIGHT y)	= fy (Just y) vst

gVisualizeEditor{|Int|}		val vst = visualizeControlSimple TUIIntControl val vst
gVisualizeEditor{|Real|}		val vst = visualizeControlSimple TUIRealControl val vst
gVisualizeEditor{|Char|}		val vst = visualizeControlSimple TUICharControl val vst
gVisualizeEditor{|String|}	val vst = visualizeControlSimple TUIStringControl val vst
gVisualizeEditor{|Bool|}		val vst = visualizeControlSimple TUIBoolControl val vst
gVisualizeEditor{|Password|}	val vst = visualizeControlSimple TUIPasswordControl val vst
gVisualizeEditor{|Note|}		val vst = visualizeControlSimple TUINoteControl val vst
gVisualizeEditor{|Date|}		val vst = visualizeControlSimple TUIDateControl val vst
gVisualizeEditor{|Time|}		val vst = visualizeControlSimple TUITimeControl val vst
gVisualizeEditor{|User|}		val vst = visualizeControlSimple TUIUserControl val vst
gVisualizeEditor{|Currency|}	val vst = visualizeControlSimple TUICurrencyControl val vst
	
gVisualizeEditor{|HtmlDisplay|} val vst = visualizeControlSimple (TUIHtmlDisplay Nothing) (fmap fromHtmlDisplay val) vst
gVisualizeEditor{|HtmlInclude|} val vst = visualizeControlSimple TUIStringControl (fmap (\(HtmlInclude path) -> path) val) vst

gVisualizeEditor {|Document|}	val vst = visualizeControlSimple control val vst
where
	control = TUIDocumentControl (fromMaybe {Document|documentId = "",name = "", mime = "", size = 0} val)
	
gVisualizeEditor{|FormButton|} val vst = visualizeControl control (fmap (\b=:{state} -> (state,b)) val) gVisualizeHtml{|*|} vst
where
	control
		= TUIButtonControl	{ TUIButtonControl
							| label = buttonLabel val
							, iconCls = icon val
							}
								
	buttonLabel	b = toString ((fmap (\b -> b.FormButton.label)) b)
	icon		b = toString ((fmap (\b -> b.FormButton.icon)) b)

gVisualizeEditor{|RadioChoice|} _ gx hx _ _ hy val vst = visualizeControl (TUIChoiceControl (toChoice val)) (fmap (\r=:(RadioChoice _ mbSel) -> (maybe [] (\l -> [l]) mbSel, r)) val) (gVisualizeHtml{|* -> *|} (gVisualizeHtml{|* -> * -> *|} hx hy)) vst
where
	toChoice Nothing						= {allowMultiple = False, options = []}
	toChoice (Just (RadioChoice options _))	= {allowMultiple = False, options = [concat (gx AsLabel v) \\ (v,_) <- options]}
	
gVisualizeEditor{|ComboChoice|} _ gx hx _ _ hy val vst = visualizeControl (TUIComboControl (toChoice val)) (fmap (\c=:(ComboChoice _ mbSel) -> (mbSel,c)) val) (gVisualizeHtml{|* -> *|} (gVisualizeHtml{|* -> * -> *|} hx hy)) vst
where
	toChoice Nothing						= []
	toChoice (Just (ComboChoice options _))	= [concat (gx AsLabel v) \\ (v,_) <- options]
	
gVisualizeEditor{|TreeChoice|} _ gx hx _ _ hy val vst = visualizeControl (TUITreeControl (toTree val)) (fmap (\t=:(TreeChoice _ mbSel) -> (mbSel,t)) val) (gVisualizeHtml{|* -> *|} (gVisualizeHtml{|* -> * -> *|} hx hy)) vst
where
	toTree Nothing								= []
	toTree (Just (TreeChoice (Tree nodes) _))	= fst (mkTree nodes 0)
	
	mkTree [] idx
		= ([],idx)
	mkTree [Leaf (v,_):r] idx
		# (rtree,idx`) 		= mkTree r (inc idx)
		= ([{text = concat (gx AsLabel v), index = Just idx, leaf = True, children = Nothing}:rtree],idx`)
	mkTree [Node label nodes:r] idx
		# (children,idx)	= mkTree nodes idx
		# (rtree,idx)		= mkTree r idx
		= ([{text = label, index = Nothing, leaf = False, children = Just children}:rtree],idx)
	
getMbView f mbChoice = fmap f (maybe Nothing getMbSelectionView mbChoice)
	
gVisualizeEditor{|CheckMultiChoice|} _ _ hv _ _ hy val vst = visualizeControl (TUIChoiceControl (toChoice val)) (fmap (\r=:(CheckMultiChoice _ mbSel) -> (mbSel,r)) val) (gVisualizeHtml{|* -> *|} (gVisualizeHtml{|* -> * -> *|} hv hy)) vst
where
	toChoice Nothing								= {allowMultiple = True, options = []}
	toChoice (Just (CheckMultiChoice options _))	= {allowMultiple = True, options = [toString (html (hv AsLabel v)) \\ (v,_) <- options]}
	
gVisualizeEditor{|Table|} val vst = visualizeControl(TUIGridControl (toGrid val)) (fmap (\t=:(Table _ _ mbSel) -> (mbSel,t)) val) gVisualizeHtml{|*|} vst
where
	toGrid Nothing							= {cells = [], headers = []}
	toGrid (Just (Table headers cells _))	= {headers = headers, cells = map (map toString) cells}
	
gVisualizeEditor {|[]|} fx _ _ val vst = visualizeCustom mkControl val vst
where
	mkControl name val touched verRes renderAsStatic vst=:{VSt|taskId}
		# val = fromMaybe [] val
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
				# (itemsVis,vst)	= childVisualizations fx items vst
				# vis				= [listItemControl idx dx \\ dx <- itemsVis & idx <- [0..]]
				# (vis,vst) = case renderAsStatic of
					False
						# (dx,vst)  = fx Nothing vst
						= (vis ++ [listItemControl (length vis) dx],vst)
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

gVisualizeEditor{|Dynamic|}			_ vst	= noVisualization vst
gVisualizeEditor{|(->)|} _ _ _ _ _ _			_ vst	= noVisualization vst

gVisualizeEditor{|Maybe|} fx _ _ val vst=:{VSt|currentPath}
	# vst = {VSt|vst & optional = True}
	# (viz,vst) = case val of
		Just (Just x)	= fx (Just x) vst
		_				= fx Nothing vst
	= (viz, {VSt|vst & optional = True, currentPath = stepDataPath currentPath})
	
// wrapper types changing visualization behaviour
gVisualizeEditor{|Hidden|} fx _ _ val vst=:{VSt | currentPath, verifyMask}
	# (_,vm) = popMask verifyMask	
	= ([],{VSt | vst & currentPath = stepDataPath currentPath, verifyMask = vm})

gVisualizeEditor{|Display|} fx _ _ val vst=:{currentPath,renderAsStatic}
	# (def,vst) = fx (fmap fromDisplay val) {VSt | vst &  renderAsStatic = True}
	= (def,{VSt | vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})

gVisualizeEditor{|Editable|} fx _ _ val vst=:{currentPath, renderAsStatic}
	# (def,vst) = fx (fmap fromEditable val) {VSt | vst & renderAsStatic = False}
	= (def,{VSt | vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})

gVisualizeEditor{|VisualizationHint|} fx gx hx val vst=:{VSt|currentPath}
	= case val of
		Just (VHHidden x)	= gVisualizeEditor{|* -> *|} fx gx hx (Just (Hidden x)) vst
		Just (VHDisplay x)	= gVisualizeEditor{|* -> *|} fx gx hx (Just (Display x)) vst
		Just (VHEditable x)	= gVisualizeEditor{|* -> *|} fx gx hx (Just (Editable x)) vst
		Nothing				= fx Nothing vst

gVisualizeEditor{|ControlSize|} fx _ _ val vst=:{controlSize}
	= case val of
		Nothing
			= fx Nothing vst
		(Just (ControlSize width height margins v))
			# (def,vst) = fx (Just v) {VSt | vst &  controlSize = (width,height,margins)}
			= (def,{VSt | vst & controlSize = controlSize})
			
gVisualizeEditor{|FillControlSize|} fx _ _ val vst=:{controlSize=controlSize=:(_,_,margins)}
	# (def,vst) = fx (fmap fromFillControlSize val) {vst & controlSize = (FillParent 1 ContentSize,FillParent 1 ContentSize,margins)}
	= (def,{vst & controlSize = controlSize})

gVisualizeEditor{|FillWControlSize|} fx _ _ val vst=:{controlSize=controlSize=:(_,height,margins)}
	# (def,vst) = fx (fmap fromFillWControlSize val) {vst & controlSize = (FillParent 1 ContentSize,height,margins)}
	= (def,{vst & controlSize = controlSize})
	
gVisualizeEditor{|FillHControlSize|} fx _ _ val vst=:{controlSize=controlSize=:(width,_,margins)}
	# (def,vst) = fx (fmap fromFillHControlSize val) {vst & controlSize = (width,FillParent 1 ContentSize,margins)}
	= (def,{vst & controlSize = controlSize})

gVisualizeEditor{|Void|} _ vst = noVisualization vst
gVisualizeEditor{|WorkflowTaskContainer|} _ vst = noVisualization vst
	
derive gVisualizeEditor DateTime, Either, (,), (,,), (,,,), UserDetails, Timestamp, Map, EmailAddress, Action, TreeNode, WorkflowDescription, ManagerProperties, RunningTaskStatus, TaskPriority, Session, Tree
derive bimap Maybe

//***** UTILITY FUNCTIONS *************************************************************************************************	

visualizeControlSimple :: !TUIControlType !(Maybe a) !*VSt -> *(![TUIDef],!*VSt) | JSONEncode{|*|}, gVisualizeHtml{|*|} a
visualizeControlSimple control v vst = visualizeControl control (fmap (\v -> (v,v)) v) gVisualizeHtml{|*|} vst 

visualizeControl :: !TUIControlType !(Maybe (!a,b)) !(StaticVisualizationMode (Maybe b) -> [HtmlTag]) !*VSt -> *(![TUIDef], !*VSt) | JSONEncode{|*|} a
visualizeControl control v htmlF vst=:{editEvent,currentPath,controlSize} = visualizeCustom tuiF v vst
where
	tuiF name v touched verRes _ vst=:{VSt|taskId, renderAsStatic}
		| renderAsStatic
			= ([htmlDisplay (toString (html (htmlF AsDisplay (fmap snd v))))],vst)
		| otherwise
			# v = checkMask touched v
			# viz = sizedControl controlSize control	{ TUIControl
														| name = name
														, value = toJSON (fmap fst v)
														, eventValue = eventValue currentPath editEvent
														, taskId = taskId
														}
			= ([addMsg verRes viz],vst)
		
	checkMask :: !Bool !(Maybe a) -> (Maybe a)
	checkMask False _	= Nothing
	checkMask _ val 	= val

visualizeCustom :: !(TUIVizFunction a) !(Maybe a) !*VSt -> *(![TUIDef],!*VSt)
visualizeCustom tuiF v vst=:{currentPath,renderAsStatic,verifyMask}
	# (cmv,vm)	= popMask verifyMask
	// only check mask if generating editor definition & not for labels
	# touched	= isTouched cmv
	# vst		= {VSt|vst & currentPath = shiftDataPath currentPath, verifyMask = childMasks cmv}
	# (vis,vst) = tuiF (dp2s currentPath) v touched (verifyElementStr cmv) renderAsStatic vst
	= (vis,{VSt|vst & currentPath = stepDataPath currentPath, verifyMask = vm})
	
noVisualization :: !*VSt -> *(![TUIDef],!*VSt)
noVisualization vst=:{VSt|currentPath,verifyMask}
	# (_,vm) = popMask verifyMask
	= ([],{VSt|vst & currentPath = stepDataPath currentPath, verifyMask = vm})
	
childVisualizations :: !((Maybe a) -> .(*VSt -> *([TUIDef],*VSt))) ![a] !*VSt -> *(![[TUIDef]],!*VSt)
childVisualizations fx children vst = childVisualizations` children [] vst
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
	
(+++>) infixr 5	:: !a !String -> String | gVisualizeText{|*|} a
(+++>) a s = visualizeAsText AsLabel a +++ s

(<+++) infixl 5	:: !String !a -> String | gVisualizeText{|*|} a
(<+++) s a = s +++ visualizeAsText AsLabel a