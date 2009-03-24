implementation module iDataFormlib

// Handy collection of Form's

import StdEnum, StdFunc, StdList, StdString, StdTuple
import iDataWidgets, iDataTrivial
import StdMaybe, GenBimap


EmptyBody :== SpanTag [] []
defpixel :== 100

// easy creation of an html page

mkHtml		:: !String ![HtmlTag] *HSt -> (!(!Bool,!String),HtmlTag,*HSt)
mkHtml s tags hst 			= ((False,""),simpleHtml s [] tags,hst)

mkHtmlExcep	:: !String !(!Bool,!String) ![HtmlTag] *HSt -> (!(!Bool,!String),HtmlTag,*HSt)
mkHtmlExcep s (exception,prefix) tags hst = ((exception,prefix),simpleHtml s [] tags,hst)

simpleHtml	:: !String ![HtmlAttr] ![HtmlTag] -> HtmlTag
simpleHtml s ba tags	 	= HtmlTag [] [header s, body tags]
where
	header s				= HeadTag [] [TitleTag [] [Text s]] 
	body tags				= BodyTag ba tags

mkHtmlB		:: !String ![HtmlAttr] ![HtmlTag] *HSt -> (!(!Bool,!String),HtmlTag,*HSt)
mkHtmlB s attr tags hst		= ((False,""), simpleHtml s attr tags,hst)

// operators for lay-out of html bodys ...

// Place two bodies next to each other

(<=>) infixl 5   :: [HtmlTag] [HtmlTag] -> [HtmlTag]
(<=>) b1 b2				= b1 ++ b2

(<.=.>) infixl 5 :: HtmlTag HtmlTag -> [HtmlTag]
(<.=.>) b1 b2				=  [b1] <=> [b2]

mkRowForm :: ![HtmlTag] -> [HtmlTag]
mkRowForm xs	 			= xs	//Just put the tags after each other

// Place second body below first

(<||>) infixl 4 :: [HtmlTag] [HtmlTag] -> [HtmlTag]	// Place a above b
(<||>) [] []				= []
(<||>) [] b2				= b2
(<||>) b1 []				= b1
(<||>) b1 b2				= [DivTag [] b1, DivTag [] b2]

(<.||.>) infixl 4:: HtmlTag HtmlTag -> [HtmlTag]			// Place a above b
(<.||.>) b1 b2				= [b1] <||> [b2]

mkColForm :: ![HtmlTag] -> [HtmlTag]
mkColForm xs 				= [DivTag [] [x] \\ x <- xs]

(<=|>) infixl 4	 :: [HtmlTag] [HtmlTag] -> [HtmlTag]		// Place a above b
(<=|>) b1 b2				= [DivTag [] [x1,x2] \\ x1 <- b1, x2 <- b1]

mkTable :: [[HtmlTag]] -> [HtmlTag]
mkTable table				= [TableTag []	(mktable table)]
where
	mktable table			= [TrTag [] (mkrow rows) \\ rows <- table]	
	mkrow   rows	 		= [TdTag [StyleAttr "vertical-align: top"] [row] \\ row <- rows]


// frequently used variants of mkViewForm

mkEditForm :: !(InIDataId d) !*HSt -> (Form d,!*HSt) | iData d
mkEditForm inIDataId hst
= mkViewForm inIDataId
	{ toForm = toViewId, updForm = const2, fromForm = const2, resetForm = Nothing } hst

mkSelfForm  :: !(InIDataId d) !(d -> d) !*HSt -> (Form d,!*HSt) | iData d
mkSelfForm inIDataId cbf hst
= mkViewForm inIDataId 
	{ toForm = toViewId, updForm = update, fromForm = const2, resetForm = Nothing } hst
where
	update b val
	| b.isChanged 			= cbf val
	| otherwise 			= val

mkStoreForm :: !(InIDataId d) !(d -> d) !*HSt -> (Form d,!*HSt) | iData d
mkStoreForm inIDataId cbf hst
= mkViewForm inIDataId
	{ toForm = toViewId, updForm = \_ v = cbf v, fromForm = const2, resetForm = Nothing } hst

mkApplyEditForm	:: !(InIDataId d) !d !*HSt -> (Form d,!*HSt) | iData d
mkApplyEditForm inIDataId inputval hst
= mkViewForm inIDataId
	{ toForm = toViewId, updForm = update, fromForm = const2, resetForm = Nothing } hst
where
	update b val
	| b.isChanged 			= val
	| otherwise 			= inputval

mkBimapEditor :: !(InIDataId d) !(Bimap d v) !*HSt -> (Form d,!*HSt) | iData v
mkBimapEditor inIDataId {map_to,map_from} hst
= mkViewForm inIDataId 
	{ toForm = toViewMap map_to, updForm = const2, fromForm = \_ v -> map_from v, resetForm = Nothing } hst 

mkSubStateForm :: !(InIDataId subState) !state !(subState state -> state) !*HSt -> (Bool,Form state,!*HSt) | iData subState
mkSubStateForm (init,formid) state upd hst
# (nsubState,hst)			= mkEditForm (init,subFormId formid "subst" subState) hst
# (commitBut,hst)			= FuncBut (Init,subnFormId formid "CommitBut" (HtmlButton "commit" False,id)) hst
# (cancelBut,hst)			= FuncBut (Init,subnFormId formid "CancelBut" (HtmlButton "cancel" False,id)) hst
# (nsubState,hst)			= if cancelBut.changed 
							     (mkEditForm (Set,setFormId formid subState) hst)
							     (nsubState,                                 hst)
= ( commitBut.changed
  ,	{ changed				= nsubState.changed || commitBut.changed || cancelBut.changed
	, value					= if commitBut.changed (upd nsubState.Form.value state) state
	, form					= [ DivTag [] nsubState.form
							  , BrTag []
							  , if commitBut.changed (DivTag [] [Text "Thanks for (re-)committing",BrTag [] ,BrTag []]) EmptyBody
							  , SpanTag [] commitBut.form
							  , SpanTag [] cancelBut.form
							  ]
	, inputs				= nsubState.inputs
	}
  , hst )
where
	subState				= formid.ival

mkShowHideForm :: !(InIDataId a) !*HSt -> (Form a,!*HSt) | iData a
mkShowHideForm (init,formid) hst 
| formid.mode == NoForm || formid.FormId.lifespan == LSTemp
	= mkEditForm (init,formid) hst
# (hiding,hst)				= mkStoreForm (Init,subFormId formid "ShowHideSore" True) id hst			// True == Hide
# (switch,hst)				= myfuncbut hiding.Form.value hst	
# hide 						= switch.Form.value hiding.Form.value
# (hiding,hst)				= mkStoreForm (Set,subFormId formid "ShowHideSore" True) (const hide) hst	// True == Hide
# (switch,hst)				= myfuncbut hiding.Form.value hst
| hide
	# (info,hst)			= mkEditForm (init,formid <@ NoForm) hst
	= ({info & form			= switch.form},hst)
| otherwise
	# (info,hst)			= mkEditForm (init,formid) hst
	= ({info & form			= switch.form ++ info.form},hst)
where
	mybut     hide			= HtmlButton (if hide "Show" "Hide") False	
	myfuncbut hide			= FuncBut (Init,subFormId formid "ShowHideBut" (mybut hide,not) <@ Edit)

// Form collection:

horlistForm :: !(InIDataId [a]) !*HSt -> (Form [a],!*HSt) | iData a
horlistForm inIDataId hSt	= layoutListForm (\f1 f2 -> f1 <=> f2) mkEditForm inIDataId hSt
			
vertlistForm :: !(InIDataId [a]) !*HSt -> (Form [a],!*HSt) | iData a
vertlistForm inIDataId hSt	= layoutListForm (\f1 f2 -> f1 <||> f2) mkEditForm inIDataId hSt


table_hv_Form :: !(InIDataId [[a]]) !*HSt -> (Form [[a]],!*HSt)							| iData a
table_hv_Form inIDataId hSt = layoutListForm (\f1 f2 -> f1 <||> f2) horlistForm inIDataId hSt

t2EditForm  :: !(InIDataId (a,b)) !*HSt -> ((Form a,Form b),!*HSt)						| iData a & iData b
t2EditForm (init,formid) hst
# (forma,hst)				= mkEditForm (init,subFormId formid "t21" a) hst 
# (formb,hst)				= mkEditForm (init,subFormId formid "t21" b) hst
= ((forma,formb),hst) 
where
	(a,b)					= formid.ival

t3EditForm  :: !(InIDataId (a,b,c)) !*HSt -> ((Form a,Form b,Form c),!*HSt)				| iData a & iData b & iData c
t3EditForm (init,formid) hst
# (forma,hst)				= mkEditForm (init,subFormId formid "t31" a) hst 
# (formb,hst)				= mkEditForm (init,subFormId formid "t32" b) hst
# (formc,hst)				= mkEditForm (init,subFormId formid "t33" c) hst
= ((forma,formb,formc),hst) 
where
	(a,b,c)					= formid.ival

t4EditForm  :: !(InIDataId (a,b,c,d)) !*HSt -> ((Form a,Form b,Form c,Form d),!*HSt)	| iData a & iData b & iData c & iData d
t4EditForm (init,formid) hst
# (forma,hst)				= mkEditForm (init,subFormId formid "t41" a) hst 
# (formb,hst)				= mkEditForm (init,subFormId formid "t42" b) hst
# (formc,hst)				= mkEditForm (init,subFormId formid "t43" c) hst
# (formd,hst) 				= mkEditForm (init,subFormId formid "t44" d) hst
= ((forma,formb,formc,formd),hst) 
where
	(a,b,c,d)				= formid.ival

simpleButton :: !String !String !(a -> a) !*HSt -> (Form (a -> a),!*HSt)
simpleButton id label fun hst
	= FuncBut (Init, nFormId id (HtmlButton label False,fun)) hst

counterForm :: !(InIDataId a) !*HSt -> (Form a,!*HSt) | +, -, one, iData a
counterForm inIDataId hst	= mkViewForm inIDataId bimap hst
where
	bimap					= { toForm		= toViewMap (\n -> (n,down,up))
							  , updForm		= updCounter`
							  , fromForm	= \_ (n,_,_) -> n
							  , resetForm	= Nothing
							  }
	updCounter` b val
	| b.isChanged 			= updCounter val
	| otherwise 			= val

	updCounter (n,HtmlButton _ True,_)= (n - one,down,up)
	updCounter (n,_,HtmlButton _ True)= (n + one,down,up)
	updCounter else 		= else

	(up,down)				= (HtmlButton "+" False,HtmlButton "-" False)

listForm :: !(InIDataId [a]) !*HSt -> (Form [a],!*HSt) | iData a
listForm inIDataId hSt		= layoutListForm (\f1 f2 -> [DivTag [] (f1 ++ f2)]) mkEditForm inIDataId hSt

layoutListForm :: !([HtmlTag] [HtmlTag] -> [HtmlTag]) 
                  !((InIDataId  a)   *HSt -> (Form  a,  *HSt))
                  ! (InIDataId [a]) !*HSt -> (Form [a],!*HSt) | iData a
layoutListForm layoutF formF (init,formid=:{mode}) hst 
# (store, hst)				= mkStoreForm (init,formid) id  hst			// enables to store list with different # elements
# (layout,hst)				= layoutListForm` 0 store.Form.value hst
# (store, hst)				= mkStoreForm (init,formid) (const layout.Form.value) hst
= (layout,hst)
where
	layoutListForm` n [] hst
		= ({ changed		= False
		   , value			= []
		   , form			= []
		   , inputs			= []
		   },hst)
	layoutListForm` n [x:xs] hst
		# (nxs,hst)			= layoutListForm` (n+1) xs hst
		# (nx, hst)			= formF (init,subFormId formid (toString (n+1)) x) hst
		= ({ changed		= nx.changed || nxs.changed
		   , value			= [nx.Form.value:nxs.Form.value]
		   , form			= layoutF nx.form nxs.form
		   , inputs			= nx.inputs ++ nxs.inputs
		   },hst)

FuncBut :: !(InIDataId (HtmlButton, a -> a)) !*HSt -> (Form (a -> a),!*HSt)
FuncBut (init,formid) hst	= FuncButNr 0 (init,formid) hst 

FuncButNr :: !Int !(InIDataId (HtmlButton, a -> a)) !*HSt -> (Form (a -> a),!*HSt)
FuncButNr i (init,formid) hst
= case formid.ival of
	(HtmlButton s True,cbf)	= FuncButNr i (init,setFormId formid (HtmlButton s False,cbf)) hst
	(button, cbf)			= mkViewForm (init,reuseFormId nformid id) hbimap hst
	where
		hbimap				= { toForm		= \init _ v -> toViewId init button v
							  , updForm		= const2
							  , fromForm	= \_ but -> case but of 
															HtmlButton _ True  -> cbf
															_		 -> id
							  , resetForm	= Just (const button)
							  }
		nformid				= case button of
								HtmlButton name _ -> formid <@ formid.id <+++ iDataIdSeparator <+++ i

TableFuncBut :: !(InIDataId [[(HtmlButton, a -> a)]]) !*HSt -> (Form (a -> a) ,!*HSt)
TableFuncBut inIDataId hSt
	= layoutIndexForm (\f1 f2 -> f1 <||> f2) 
		(layoutIndexForm (\f1 f2 -> f1 <=> f2) FuncButNr id (o)) 
			id (o) 0 inIDataId hSt

ListFuncBut2 :: !(InIDataId [(Mode,HtmlButton, a -> a)]) !*HSt -> (Form (a -> a),!*HSt)
ListFuncBut2 (init,formid) hst
	= ListFuncBut` 0 formid.ival hst 
where
	ListFuncBut` _ [] hst
	= ({ changed			= False
	   , value				= id
	   , form				= []
	   , inputs				= []
	   },hst)
	ListFuncBut` n [(bmode,but,func):xs] hst 
	# (rowfun,hst)			= ListFuncBut` (n+1) xs hst
	# (fun   ,hst)			= FuncButNr n (init,{formid & ival = (but,func)} <@ bmode) hst
	= ({ changed			= rowfun.changed || fun.changed
	   , value				= fun.Form.value o rowfun.Form.value
	   , form				= [DivTag [] (fun.form ++ rowfun.form) ]
	   , inputs				= fun.inputs ++ rowfun.inputs
	   },hst)

TableFuncBut2 :: !(InIDataId [[(Mode,HtmlButton, a -> a)]]) !*HSt -> (Form (a -> a) ,!*HSt)
TableFuncBut2 (init,formid) hSt
	= TableFuncBut2` 0 formid.ival hSt
where
	TableFuncBut2` n [] hSt 	
		= ({ changed		= False
		   , value			= id
		   , form			= []
		   , inputs			= []
		   },hSt)
	TableFuncBut2` n [x:xs] hSt 
	# (nx, hSt)				= ListFuncBut2 (init,subFormId formid (toString n) x) hSt
	# (nxs,hSt)				= TableFuncBut2` (n+1) xs hSt
	= ({ changed			= nx.changed || nxs.changed
	   , value				= nx.Form.value o nxs.Form.value
	   , form				= nx.form <||> nxs.form 
	   , inputs				= nx.inputs ++ nxs.inputs
	   },hSt)


//	Generalized form of ListFuncBut:
layoutIndexForm :: !([HtmlTag] [HtmlTag] -> [HtmlTag]) 
                   	!(Int (InIDataId x) *HSt -> (Form y,*HSt))
                   	 y (y y -> y) !Int !(InIDataId [x]) !*HSt -> (Form y,!*HSt)
layoutIndexForm layoutF formF r combineF n (init,formid) hSt
= case formid.ival of
	[]						= ({changed=False, value=r, form=[], inputs = []},hSt)
	[x:xs]
	# (xsF,hSt)				= layoutIndexForm layoutF formF r combineF (n+1) (init,setFormId formid xs) hSt
	# (xF, hSt)				= formF n (init,reuseFormId formid x) hSt
	= ({ changed			= xsF.changed || xF.changed
	   , value				= combineF xsF.Form.value xF.Form.value
	   , form				= layoutF xF.form xsF.form
	   , inputs				= xF.inputs ++ xsF.inputs
	   },hSt)

ListFuncBut :: !(InIDataId [(HtmlButton, a -> a)]) !*HSt -> (Form (a -> a),!*HSt)
ListFuncBut (init,formid) hSt
	= layoutIndexForm (\f1 f2 -> [DivTag [] (f1 ++ f2)]) FuncButNr id (o) 0 (init,formid) hSt


ListFuncCheckBox :: !(InIDataId [(HtmlCheckbox, Bool [Bool] a -> a)]) !*HSt -> (Form (a -> a,[Bool]),!*HSt)
ListFuncCheckBox (init,formid) hst 
# (check,hst)				= ListFuncCheckBox` formid.ival 0 hst
# (f,bools)					= check.Form.value
= ({ changed				= False
   , value					= (f bools,bools)
   , form					= check.form
   , inputs					= check.inputs
   },hst)
where
	ListFuncCheckBox` :: ![(HtmlCheckbox, Bool [Bool] a -> a)] !Int !*HSt -> (Form ([Bool] a -> a,[Bool]),!*HSt)
	ListFuncCheckBox` [] i hst
		= ({ changed			= False
		   , value				= (const2,[])
		   , form				= []
		   , inputs				= []
		   },hst)
	ListFuncCheckBox` [x:xs] i hst
		# (xform ,hst)			= FuncCheckBox (init, formid) x i hst
		# (xsform ,hst)			= ListFuncCheckBox` xs (inc i) hst
		# (xfun,xbool)			= xform.Form.value
		# (xsfun,xsbools)		= xsform.Form.value
		= ({ changed			= xform.changed || xsform.changed
		   , value				= (funcomp xfun xsfun, [xbool:xsbools])
		   , form				= xform.Form.form ++ xsform.Form.form
		   , inputs				= xform.inputs ++ xsform.inputs 
		   },hst)
	where
		funcomp f g			= \bools a -> g bools (f bools a)
		
		FuncCheckBox (init, formid) (checkbox, cbf) i hst
			= mkViewForm (init, nformid) bimap hst
		where
			bimap =	{ toForm 	= \init _ v -> toViewId init checkbox v
					, updForm	= \b v -> v
					, fromForm	= \b (HtmlCheckbox label val) -> if b.isChanged (cbf val,val) (const2,val)
					, resetForm	= Nothing
					}
			nformid = subFormId formid ("cb" +++ toString i) (const2,False)


FuncMenu :: !(InIDataId (Int,[(String, a -> a)])) !*HSt -> (Form (a -> a,Int),!*HSt)
FuncMenu (init,formid) hst	= mkViewForm (init,nformid) bimap hst
where
	nformid					= reuseFormId formid (id,calc index)
	(index,defs)			= formid.ival
	menulist				= HtmlSelect (map (\(x,y) -> (x,x)) defs) (fst (defs !! index))

	bimap =	{ toForm	 	= toViewMap (const menulist)
			, updForm		= const2
			, fromForm		= \b (HtmlSelect _ v) -> if b.isChanged (snd (defs!!(find v defs)),find v defs) (id,find v defs)
			, resetForm		= Nothing
			}

	calc index
	| abs index >= 0 && abs index < length defs
							= abs index
	| otherwise				= 0
	
	find val [(label,f):defs]
	| val == label 	= 0
	| otherwise		= find val defs


browseButtons :: !(InIDataId Int) !Int !Int !Int !*HSt -> (Form Int,!*HSt)
browseButtons (init,formid) step length nbuttuns hst
# (nindex,  hst)			= mkStoreForm (init,formid) id hst
# (calcnext,hst)			= browserForm nindex.Form.value hst
# (nindex,  hst)			= mkStoreForm (init,formid) calcnext.Form.value hst
# (shownext,hst)			= browserForm nindex.Form.value hst
= ({ changed				= calcnext.changed
   , value					= nindex.Form.value
   , form					= shownext.form
   , inputs					= shownext.inputs
   },hst)
where
	curindex				= formid.ival

	browserForm :: !Int *HSt -> (Form (Int -> Int),!*HSt) 
	browserForm index hst
		= ListFuncBut2 (init,reuseFormId formid (browserButtons index step length)) hst
	where
		browserButtons :: !Int !Int !Int -> [(Mode,HtmlButton,Int -> Int)]
		browserButtons init step length
		=	if (init - range >= 0) [(formid.mode,sbut "--", const (init - range))] [] 
							++
			take nbuttuns [(setmode i index,sbut (toString i),const i) \\ i <- [startval,startval+step .. length-1]] 
							++ 
			if (startval + range < length) [(formid.mode,sbut "++", const (startval + range))] []
		where
			range 			= nbuttuns * step
			start i j		= if (i < range) j (start (i-range) (j+range))
			startval 		= start init 0
			sbut s			= HtmlButton s False
			setmode i index
			| index <= i && i < index + step
							= Display
			| otherwise		= formid.mode

// scripts

openWindowScript ::  !String !Int !Int !Bool !Bool !Bool !Bool !Bool !Bool !HtmlTag -> HtmlTag
openWindowScript scriptname height width toolbar menubar scrollbars resizable location status html
= ScriptTag [TypeAttr "text/javascript"] [RawText ( 
			"function " +++ scriptname +++ 
			"{var OpenWindow = window.open(\"\", \"newwin\", \"" +++
					"height="      +++ toString height	+++
					",width="      +++ toString width 	+++
					",toolbar="    +++ yn toolbar		+++
					",menubar="    +++ yn menubar		+++
					",scrollbars=" +++ yn scrollbars	+++
					",resizable="  +++ yn resizable		+++
					",location="   +++ yn location		+++
					",status="     +++ yn status		+++ "\"); " +++
				"OpenWindow.document.write('" +++ (toString html) +++ "'); " +++
				"OpenWindow.document.close(); " +++
			"}" )]
where
	yn bool					= if bool "yes" "no" 

openNoticeScript ::  !String !Int !Int !HtmlTag -> HtmlTag
openNoticeScript scriptname height width html 
	= openWindowScript scriptname height width False False False False False False html

OnLoadException :: !(!Bool,String) -> [HtmlAttr]
OnLoadException (True,message) 	= [OnloadAttr ("alert('" +++ message +++ "')")]
OnLoadException _				= []

// refresh time in "minutes:seconds" Minutes should range from 0 to inifinity. Seconds should range from 0 to 59

autoRefresh :: !Int !Int -> HtmlTag
autoRefresh minutes seconds = ScriptTag [TypeAttr "text/javascript"] [ RawText (
				"\rvar limit=\"" +++ toString minutes +++ ":" +++ toString seconds +++ "\"" +++ ";\r" +++

				"if (document.images)" +++
				"{ var parselimit=limit.split(\":\");\r" +++
				"  parselimit=parselimit[0]*60+parselimit[1]*1" +++
				"};\r" +++
				"function beginrefresh()\r" +++
				"{ if (!document.images)\r" +++
				"  return;\r" +++
				"  if (parselimit==1)\r" +++
				"  window.location.reload();\r" +++
				"  else\r" +++
				"  { parselimit-=1\r" +++
				"    curmin=Math.floor(parselimit/60)\r" +++
				"    cursec=parselimit%60\r" +++
				"    if (curmin!=0)\r" +++
				"      curtime=curmin+\" minutes and \"+cursec+\" seconds left until page refresh!\"\r" +++
				"    else\r" +++
				"    curtime=cursec+\" seconds left until page refresh!\"\r" +++
				"    window.status=curtime\r" +++
				"    setTimeout(\"beginrefresh()\",1000)\r" +++
				"  }\r" +++
				"}\r" )]

// special objects ...

mediaPlayer :: !(Int,Int) Bool String -> HtmlTag
mediaPlayer (height,width) autostart filename
	= ObjectTag 
		[ ClassidAttr "CLSID:05589FA1-C356-11CE-BF01-00AA0055595A"
		, HeightAttr (toString height)
		, WidthAttr (toString width)
		] 
		[ ParamTag [ NameAttr "FileName",  ValueAttr filename ] []
		, ParamTag [ NameAttr "autostart", ValueAttr (toString autostart)] []
		]

// special forms

MailForm :: String Int Int -> HtmlTag
MailForm  mailaddress row col
	= FormTag
			[ActionAttr ("mailto:" +++ mailaddress), MethodAttr "post", EnctypeAttr "text/plain"] 
			(mkTable 	[ [BTag [] [Text "Name:"], InputTag [TypeAttr "text", NameAttr "uname", SizeAttr "20"] ]
						, [BTag [] [Text "Email:"], InputTag [TypeAttr "text", NameAttr "email", SizeAttr "20"] ]
						, [BTag [] [Text "Message:"], TextareaTag [NameAttr "message", RowsAttr (toString row), ColsAttr (toString col)] [] ]
						, [InputTag [TypeAttr "submit", NameAttr "submit", ValueAttr "Submit"],InputTag [TypeAttr "reset",  NameAttr "reset",  ValueAttr "Reset"]]
						])
	
MailApplicationLink :: String String String -> HtmlTag
MailApplicationLink mailaddress subject txtbody
	= ATag [HrefAttr ("mailto:" <+++ mailaddress <+++ "?subject=" <+++ subject <+++ "&body=" <+++ txtbody)] [Text mailaddress]
	
	
	
