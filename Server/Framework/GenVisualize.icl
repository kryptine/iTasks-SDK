implementation module GenVisualize

import StdBool, StdChar, StdList, StdArray, StdTuple, StdMisc, StdGeneric, StdEnum, StdFunc, List, Generic
import GenUpdate, GenVerify, Util, Maybe, Functor, Text, HTML, JSON, TUIDefinition, SystemTypes, HtmlUtil

visualizeAsEditor :: !a !TaskId !Int !VerifyMask !(Maybe (!DataPath,!JSONNode)) -> Maybe TUIDef | gVisualizeEditor{|*|} a
visualizeAsEditor x taskId idx vmask editEvent
	# vst			= {mkVSt & verifyMask = [vmask], editEvent = editEvent, taskId = Just taskId, currentPath = shiftDataPath (childDataPath emptyDataPath idx)}
	# (defs,vst)	= gVisualizeEditor{|*|} (Just x) vst
	= case defs of
		[]		= Nothing
		[tui]   = Just tui
		tuis    = Just (defaultDef (TUIContainer (defaultLayoutContainer tuis)))

visualizeAsDisplay :: !a -> Maybe TUIDef | gVisualizeEditor{|*|} a
visualizeAsDisplay x 
	# vst = {mkVSt & renderAsStatic = True}
	# (defs,vst)	= gVisualizeEditor{|*|} (Just x) vst
	= case defs of
		[]		= Nothing
		[tui]	= Just tui
		tuis	= Just (defaultDef (TUIContainer (defaultLayoutContainer tuis)))

visualizeAsText :: !StaticVisualizationMode !a -> String | gVisualizeText{|*|} a
visualizeAsText mode v = concat (gVisualizeText{|*|} mode v)

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

derive gVisualizeText DateTime, Either, (,), (,,), (,,,), UserDetails, Timestamp, Map, EmailAddress, Username, Action, TreeNode, ManagementMeta, TaskPriority, Tree

mkVSt :: *VSt
mkVSt = {VSt| currentPath = startDataPath,
		selectedConsIndex = -1, optional = False, renderAsStatic = False, verifyMask = [], editEvent = Nothing, taskId = Nothing,
		controlSize = (Nothing,Nothing,Nothing)}

//Generic visualizer
generic gVisualizeEditor a | gVisualizeText a :: !(Maybe a) !*VSt -> (![TUIDef], !*VSt)

gVisualizeEditor{|UNIT|} _ vst
	= ([],vst)
	
gVisualizeEditor{|FIELD of d|} fx _ val vst
	# x = fmap fromFIELD val
	# (vizBody,vst=:{VSt|optional})	= fx x {VSt|vst & optional = False}
	# label							= {stringDisplay (camelCaseToWords d.gfd_name +++ if optional "" "*" +++ ":") & width = Just (Fixed 100)}
	= ([{content = TUIContainer {TUIContainer|defaultLayoutContainer [label: vizBody] & direction = Horizontal}, width = Just (FillParent 1 ContentSize), height = Just (WrapContent 0), margins = Nothing}],{VSt|vst & optional = optional})
			
gVisualizeEditor{|OBJECT of d|} fx _ val vst=:{currentPath,selectedConsIndex = oldSelectedConsIndex,renderAsStatic,verifyMask,taskId,editEvent,controlSize}
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
		= ([{ content = TUIContainer (defaultLayoutContainer
							[	addMsg (verifyElementStr cmv) (sizedControl controlSize (TUIEditControl (TUIComboControl [gdc.gcd_name \\ gdc <- d.gtd_conses])
										{ TUIEditControl
										| name			= dp2s currentPath
										, taskId		= maybe "" id taskId
										, value			= toJSON (if (isTouched cmv) (Just selectedConsIndex) Nothing)
										, eventValue	= eventValue currentPath editEvent
										}))
							:	if (isEmpty content)
								[]
								[{ content	= TUIContainer (defaultLayoutContainer content /*& baseCls = Just "x-constructor-panel"*/)
								,  width	= Just (FillParent 1 ContentSize)
								,  height	= Just (WrapContent 0)
								,  margins	= leftMargin 10
								}]
							])
						, width		= Just (FillParent 1 ContentSize)
						, height	= Just (WrapContent 0)
						, margins	= Nothing
						}
			]
		  ,{vst & currentPath = stepDataPath currentPath, selectedConsIndex = oldSelectedConsIndex})
	//ADT with one constructor or static render: put content into container, if empty show cons name
	| otherwise
		# (vis,vst) = fx x vst
		# vis = case vis of
			[]	= if (isTouched cmv) [(stringDisplay ((d.gtd_conses !! vst.selectedConsIndex).gcd_name))] []
			vis = [{ content	= TUIContainer (defaultLayoutContainer vis)
								, width 	= Just (FillParent 1 ContentSize)
								, height	= Nothing
								, margins	= Nothing
								}]
		= (vis,{vst & currentPath = stepDataPath currentPath, selectedConsIndex = oldSelectedConsIndex})

gVisualizeEditor{|CONS of d|} fx _ val vst = visualizeCustom mkControl vst
where
	mkControl name _ _ vst=:{taskId,editEvent,currentPath,optional,controlSize,renderAsStatic}
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
		recordContainer viz =	{ content	= TUIContainer (defaultLayoutContainer (if optional [checkbox True] [] ++ viz))
								, width		= Just (FillParent 1 ContentSize)
								, height	= Just (WrapContent 0)
								, margins	= Nothing
								}
											
		checkbox c = sizedControl controlSize (TUIEditControl TUIBoolControl 
			{ name			= name
			, value			= toJSON c
			, taskId		= maybe "" id taskId
			, eventValue	= eventValue currentPath editEvent
			})

gVisualizeEditor{|PAIR|} fx _ fy _ val vst
	# (x,y)			= (fmap fromPAIRX val, fmap fromPAIRY val)
	# (vizx, vst)	= fx x vst
	# (vizy, vst)	= fy y vst
	= (vizx ++ vizy, vst)

gVisualizeEditor{|EITHER|} fx _ fy _ val vst = case val of
		Nothing			= fx Nothing vst
		Just (LEFT x)	= fx (Just x) vst
		Just (RIGHT y)	= fy (Just y) vst

gVisualizeEditor{|Int|}			val vst = visualizeControl TUIIntControl val vst
gVisualizeEditor{|Real|}		val vst = visualizeControl TUIRealControl val vst
gVisualizeEditor{|Char|}		val vst = visualizeControl TUICharControl val vst
gVisualizeEditor{|String|}		val vst = visualizeControl TUIStringControl val vst
gVisualizeEditor{|Bool|}		val vst = visualizeControl TUIBoolControl val vst
gVisualizeEditor{|Username|}	val vst = visualizeControl TUIStringControl val vst
gVisualizeEditor{|Password|}	val vst = visualizeControl TUIPasswordControl val vst
gVisualizeEditor{|Note|}		val vst = visualizeControl TUINoteControl val vst
gVisualizeEditor{|Date|}		val vst = visualizeControl TUIDateControl val vst
gVisualizeEditor{|Time|}		val vst = visualizeControl TUITimeControl val vst
gVisualizeEditor{|User|}		val vst = visualizeControl TUIUserControl val vst
gVisualizeEditor{|Currency|}	val vst = visualizeControl TUICurrencyControl val vst
gVisualizeEditor{|HtmlInclude|} val vst = visualizeControl TUIStringControl (fmap (\(HtmlInclude path) -> path) val) vst

gVisualizeEditor {|Document|}	val vst = visualizeControl control val vst
where
	control = TUIDocumentControl (fromMaybe {Document|documentId = "",name = "", mime = "", size = 0} val)
	
gVisualizeEditor{|FormButton|} val vst = visualizeControl control (fmap (\b=:{state} -> (state,b)) val) vst
where
	control
		= TUIButtonControl	{ TUIButtonControl
							| label = buttonLabel val
							, iconCls = icon val
							}
								
	buttonLabel	b = toString ((fmap (\b -> b.FormButton.label)) b)
	icon		b = toString ((fmap (\b -> b.FormButton.icon)) b)

gVisualizeEditor{|RadioChoice|} fx _ _ _ val vst = visualizeCustom mkControl vst
where
	mkControl name touched verRes vst=:{VSt|taskId,renderAsStatic}
		# (options,sel)		= maybe ([],-1) (\(RadioChoice options mbSel) -> (map fst options,fromMaybe -1 mbSel) ) val
		# (itemVis,vst)		= childVisualizations fx options {VSt|vst & renderAsStatic = True}
		# itemDefs			= [defaultDef (TUIRadioChoice {TUIRadioChoice| items = items, taskId = taskId, name = name, index = i, checked = i == sel}) \\ items <- itemVis & i <- [0..]]
		= ([defaultDef (TUIContainer (defaultLayoutContainer itemDefs))], vst)

gVisualizeEditor{|ComboChoice|} _ gx _ _ val vst = visualizeControl (TUIComboControl (toChoice val)) (fmap (\(ComboChoice _ mbSel) -> mbSel) val) vst
where
	toChoice Nothing						= []
	toChoice (Just (ComboChoice options _))	= [concat (gx AsLabel v) \\ (v,_) <- options]
	
gVisualizeEditor{|TreeChoice|} _ gx _ _ val vst = visualizeCustom tuiF vst
where
	tuiF name touched verRes vst=:{VSt|taskId,renderAsStatic,editEvent,currentPath,controlSize}
		//| renderAsStatic
		//	=	([sizedControl controlSize (TUIShowControl control {TUIShowControl| value = toJSON v})], vst)
		| otherwise
			# viz = sizedControl controlSize (TUIEditControl (TUITreeControl (toTree val))
													{ TUIEditControl
													| name = name
													, value = toJSON (fmap (\(TreeChoice _ mbSel) -> mbSel) (checkMask touched val))
													, eventValue = eventValue currentPath editEvent
													, taskId = fromMaybe "" taskId
													})
			= ([viz],vst)

	toTree Nothing								= []
	toTree (Just (TreeChoice (Tree nodes) _))	= fst (mkTree nodes 0)
	
	mkTree [] idx
		= ([],idx)
	mkTree [Leaf (v,_):r] idx
		# (rtree,idx`) 		= mkTree r (inc idx)
		= ([{text = concat (gx AsLabel v), value = idx, leaf = True, children = Nothing}:rtree],idx`)
	mkTree [Node (v,_) nodes:r] idx
		# (children,idx`)	= mkTree nodes (inc idx)
		# (rtree,idx`)		= mkTree r idx`
		= ([{text = concat (gx AsLabel v), value = idx, leaf = False, children = Just children}:rtree],idx`)
		
	checkMask :: !Bool !(Maybe a) -> (Maybe a)
	checkMask False _	= Nothing
	checkMask _ val 	= val

	
getMbView f mbChoice = fmap f (maybe Nothing getMbSelectionView mbChoice)

gVisualizeEditor{|CheckMultiChoice|} fx _ _ _ val vst = visualizeCustom mkControl vst
where
	mkControl name touched verRes vst=:{VSt|taskId,renderAsStatic}
		# (options,sel)		= maybe ([],[]) (\(CheckMultiChoice options sel) -> (map fst options,sel) ) val
		# (itemVis,vst)		= childVisualizations fx options {VSt|vst & renderAsStatic = True}
		# itemDefs			= [defaultDef (TUICheckChoice {TUICheckChoice| items = items, taskId = taskId, name = name, index = i, checked = isMember i sel}) \\ items <- itemVis & i <- [0..]]
		= ([defaultDef (TUIContainer (defaultLayoutContainer itemDefs))], vst)

gVisualizeEditor{|Table|} val vst = visualizeControl(TUIGridControl (toGrid val)) (fmap (\t=:(Table _ _ mbSel) -> (mbSel,t)) val) vst
where
	toGrid Nothing							= {cells = [], headers = []}
	toGrid (Just (Table headers cells _))	= {headers = headers, cells = map (map toString) cells}

gVisualizeEditor {|[]|} fx _ val vst = visualizeCustom mkControl vst
where
	mkControl name touched verRes vst=:{VSt|taskId,renderAsStatic}
		# val			= fromMaybe [] val
		# (items,vst)	= TUIDef val vst
		= (addMsg verRes
			{ content	= TUIListContainer
							{ TUIListContainer
							| items = items
							, name = Just name
							, taskId = if renderAsStatic Nothing taskId}
			, width		= Nothing
			, height	= Nothing
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
				=	{ TUIListItem
					| index = idx
					, items = case defs of
						[def]	= def
						defs	= {content = TUIContainer (defaultLayoutContainer defs), width = Just (FillParent 1 ContentSize), height = Just (WrapContent 0), margins = Nothing}
					}
					
			addMsg verSt list = case verSt of
				NoMsg			= [list]
				HintMsg	msg		= addMsg` "icon-hint" msg list
				ValidMsg msg	= addMsg` "icon-valid" msg list
				ErrorMsg msg	= addMsg` "icon-invalid" msg list
			
			addMsg` cls msg list = [	{ content	= TUIContainer (defaultLayoutContainer [list,mkMessage cls msg])
										, width		= Just (FillParent 1 ContentSize)
										, height	= Just (WrapContent 0)
										, margins	= Nothing
										}]
		
			mkMessage cls msg =	stringDisplay msg //(DivTag [ClassAttr "list-msg-field"] [DivTag [ClassAttr cls] [Text msg]])

gVisualizeEditor{|Dynamic|}			_ vst	= noVisualization vst
gVisualizeEditor{|(->)|} _ _ _ _	_ vst	= noVisualization vst

gVisualizeEditor{|Maybe|} fx _ val vst=:{VSt|currentPath}
	# vst = {VSt|vst & optional = True}
	# (viz,vst) = case val of
		Just (Just x)	= fx (Just x) vst
		_				= fx Nothing vst
	= (viz, {VSt|vst & optional = True, currentPath = stepDataPath currentPath})
	
// wrapper types changing visualization behaviour
gVisualizeEditor{|Hidden|} fx _ val vst=:{VSt | currentPath, verifyMask}
	# (_,vm) = popMask verifyMask	
	= ([],{VSt | vst & currentPath = stepDataPath currentPath, verifyMask = vm})

gVisualizeEditor{|Display|} fx _ val vst=:{currentPath,renderAsStatic}
	# (def,vst) = fx (fmap fromDisplay val) {VSt | vst &  renderAsStatic = True}
	= (def,{VSt | vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})

gVisualizeEditor{|Editable|} fx _ val vst=:{currentPath, renderAsStatic}
	# (def,vst) = fx (fmap fromEditable val) {VSt | vst & renderAsStatic = False}
	= (def,{VSt | vst & currentPath = stepDataPath currentPath, renderAsStatic = renderAsStatic})

gVisualizeEditor{|VisualizationHint|} fx gx val vst=:{VSt|currentPath}
	= case val of
		Just (VHHidden x)	= gVisualizeEditor{|* -> *|} fx gx (Just (Hidden x)) vst
		Just (VHDisplay x)	= gVisualizeEditor{|* -> *|} fx gx (Just (Display x)) vst
		Just (VHEditable x)	= gVisualizeEditor{|* -> *|} fx gx (Just (Editable x)) vst
		Nothing				= fx Nothing vst

gVisualizeEditor{|ControlSize|} fx _ val vst=:{controlSize}
	= case val of
		Nothing
			= fx Nothing vst
		(Just (ControlSize width height margins v))
			# (def,vst) = fx (Just v) {VSt | vst &  controlSize = (width,height,margins)}
			= (def,{VSt | vst & controlSize = controlSize})
			
gVisualizeEditor{|FillControlSize|} fx _ val vst=:{controlSize=controlSize=:(_,_,margins)}
	# (def,vst) = fx (fmap fromFillControlSize val) {vst & controlSize = (Just (FillParent 1 ContentSize),Just (FillParent 1 ContentSize),margins)}
	= (def,{vst & controlSize = controlSize})

gVisualizeEditor{|FillWControlSize|} fx _ val vst=:{controlSize=controlSize=:(_,height,margins)}
	# (def,vst) = fx (fmap fromFillWControlSize val) {vst & controlSize = (Just (FillParent 1 ContentSize),height,margins)}
	= (def,{vst & controlSize = controlSize})
	
gVisualizeEditor{|FillHControlSize|} fx _ val vst=:{controlSize=controlSize=:(width,_,margins)}
	# (def,vst) = fx (fmap fromFillHControlSize val) {vst & controlSize = (width,Just (FillParent 1 ContentSize),margins)}
	= (def,{vst & controlSize = controlSize})

gVisualizeEditor{|Void|} _ vst = noVisualization vst

/*
gVisualizeEditor{|DateTime|} val vst=:{VSt|currentPath}
	# (dateViz,timeViz,vst) = case val of
		Nothing
			# (dateViz,vst) = gVisualizeEditor{|*|} noDate vst
			# (timeViz,vst) = gVisualizeEditor{|*|} noTime vst
			= (dateViz,timeViz,vst)
		Just (DateTime date time)
			# (dateViz,vst) = gVisualizeEditor{|*|} (Just date) vst
			# (timeViz,vst) = gVisualizeEditor{|*|} (Just time) vst
			= (dateViz,timeViz,vst)
	= ([defaultDef (TUIContainer {TUIContainer|defaultLayoutContainer [hd dateViz, {hd timeViz & margins = leftMargin 5}] & direction = Horizontal})],vst)
where
	noDate :: Maybe Date
	noDate = Nothing
	noTime :: Maybe Time
	noTime = Nothing
*/	
derive gVisualizeEditor DateTime
	
derive gVisualizeEditor Either, (,), (,,), (,,,), UserDetails, Timestamp, Map, EmailAddress, Action, TreeNode, ManagementMeta, TaskPriority, Tree
derive bimap Maybe

//***** UTILITY FUNCTIONS *************************************************************************************************	

visualizeControl :: !TUIControlType !(Maybe a) !*VSt -> *(![TUIDef], !*VSt) | JSONEncode{|*|} a
visualizeControl control v vst = visualizeCustom tuiF vst
where
	tuiF name touched verRes vst=:{VSt|taskId, renderAsStatic,editEvent,currentPath,controlSize}
		| renderAsStatic
			=	([sizedControl controlSize (TUIShowControl control {TUIShowControl| value = toJSON v})], vst)
		| otherwise
			# v = checkMask touched v
			# viz = sizedControl controlSize (TUIEditControl control
													{ TUIEditControl
													| name = name
													, value = toJSON v
													, eventValue = eventValue currentPath editEvent
													, taskId = fromMaybe "" taskId
													})
			= ([addMsg verRes viz],vst)
		
	checkMask :: !Bool !(Maybe a) -> (Maybe a)
	checkMask False _	= Nothing
	checkMask _ val 	= val

visualizeCustom :: !TUIVizFunction !*VSt -> *(![TUIDef],!*VSt)
visualizeCustom tuiF vst=:{currentPath,renderAsStatic,verifyMask}
	# (cmv,vm)	= popMask verifyMask
	// only check mask if generating editor definition & not for labels
	# touched	= isTouched cmv
	# vst		= {VSt|vst & currentPath = shiftDataPath currentPath, verifyMask = childMasks cmv}
	# (vis,vst) = tuiF (dp2s currentPath) touched (verifyElementStr cmv) vst
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

sizedControl :: !(!Maybe TUISize,!Maybe TUISize,!Maybe TUIMargins) !TUIDefContent -> TUIDef
sizedControl (width,height,mbMargins) content = {content = content, width = width, height = height, margins = mbMargins}

verifyElementStr :: !VerifyMask -> VerifyResult
verifyElementStr cmv = case cmv of
	VMValid mbHnt _			= maybe NoMsg ValidMsg mbHnt
	VMUntouched mbHnt _ _	= maybe NoMsg HintMsg mbHnt
	VMInvalid err _			= ErrorMsg (toString err)
	
eventValue :: !DataPath !(Maybe (!DataPath,!JSONNode)) -> Maybe JSONNode
eventValue currentPath mbEvent = case mbEvent of
	Just (dp,val) | dp == currentPath	= Just val
	_									= Nothing

addMsg :: !VerifyResult !TUIDef -> TUIDef
addMsg verRes viz = case verRes of
		NoMsg			= viz
		HintMsg msg		= add "icon-hint" msg viz
		ValidMsg msg	= add "icon-valid" msg viz
		ErrorMsg msg	= add "icon-invalid" msg viz
where	
	add cls msg viz= {content = TUIContainer {TUIContainer|defaultLayoutContainer [viz,mkIcon cls msg] & direction = Horizontal}, width = Just (FillParent 1 ContentSize), height = Just (WrapContent 0), margins = Nothing}
	mkIcon cls msg = {defaultDef (TUIIcon {type = cls, tooltip = Just msg}) & margins = leftMargin 5}


//*********************************************************************************************************************
	
(+++>) infixr 5	:: !a !String -> String | gVisualizeText{|*|} a
(+++>) a s = visualizeAsText AsLabel a +++ s

(<+++) infixl 5	:: !String !a -> String | gVisualizeText{|*|} a
(<+++) s a = s +++ visualizeAsText AsLabel a