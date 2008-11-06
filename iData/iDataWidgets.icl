implementation module iDataWidgets

import StdFunc, StdList, StdString, StdArray
import iDataForms, iDataFormlib, iDataTrivial, StdBimap


derive gForm	[], HtmlTag, HtmlAttr
derive gUpd		[], HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlDate, HtmlTime, RadioButton

derive gPrint	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlTextarea, HtmlDate, HtmlTime, RadioButton, RadioGroup/*, PullDownMenu*/, TextInput, TextArea, PasswordBox, RefreshTimer
derive gParse	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlTextarea, HtmlDate, HtmlTime, RadioButton, RadioGroup/*, PullDownMenu*/, TextInput, TextArea, PasswordBox, RefreshTimer
derive gerda 	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlTextarea, HtmlDate, HtmlTime, RadioButton, RadioGroup/*, PullDownMenu*/, TextInput, TextArea, PasswordBox, RefreshTimer
//derive read 	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlDate, HtmlTime, RadioButton, RadioGroup, PullDownMenu, TextInput, TextArea, PasswordBox, RefreshTimer
//derive write 	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlDate, HtmlTime, RadioButton, RadioGroup, PullDownMenu, TextInput, TextArea, PasswordBox, RefreshTimer

EmptyBody :== SpanTag [] []
defpixel :== 100
defsize	:== 10
layoutTableAtts	:== []	// default table attributes for arranging layout

// <-> works exactly the same as (,) and places its arguments next to each other, for compatibility with GEC's

gForm{|(<->)|} gHa gHb (init,formid) hst
# (na,hst)				= gHa (init,reuseFormId formid a) (incrHStCntr 1 hst)   	// one more for the now invisible <-> constructor 
# (nb,hst)				= gHb (init,reuseFormId formid b) hst
= (	{ changed			= na.changed || nb.changed 
	, value				= na.value <-> nb.value
	, form				= [SpanTag [] na.form, SpanTag [] nb.form]
	, inputs			= na.inputs ++ nb.inputs
	},hst)
where
	(a <-> b)			= formid.ival

// <|> works exactly the same as PAIR and places its arguments below each other, for compatibility with GEC's

gForm{|(<|>)|} gHa gHb (init,formid) hst 
# (na,hst)				= gHa (init,reuseFormId formid a) (incrHStCntr 1 hst)		// one more for the now invisible <|> constructor
# (nb,hst)				= gHb (init,reuseFormId formid b) hst
= (	{ changed			= na.changed || nb.changed 
	, value				= na.value <|> nb.value
	, form				= [DivTag [] na.form, DivTag [] nb.form]
	, inputs			= na.inputs ++ nb.inputs
	},hst)
where
	(a <|> b)			= formid.ival

// to switch between modes within a type ...

gForm{|DisplayMode|} gHa (init,formid) hst 	
= case formid.ival of
	(HideMode a)
		# (na,hst)		= gHa (init,reuseFormId formid a <@ Display) (incrHStCntr 1 hst)
		= (	{ changed	= na.changed 
			, value		= HideMode na.value
			, form		= []
			, inputs	= []
			},hst)
	(DisplayMode a)
		# (na,hst)		= gHa (init,reuseFormId formid a <@ Display) (incrHStCntr 1 hst)
		= (	{ changed	= False
			, value		= DisplayMode na.value
			, form		= na.form
			, inputs	= []
			},hst)
	(EditMode a) 
		# (na,hst)		= gHa (init,reuseFormId formid a <@ Edit) (incrHStCntr 1 hst)
		= (	{ changed	= na.changed
			, value		= EditMode na.value
			, form		= na.form
			, inputs	= na.inputs
			},hst)
	EmptyMode
		= (	{ changed	= False
			, value		= EmptyMode
			, form		= []
			, inputs	= []
			},incrHStCntr 1 hst)

gForm{|HtmlButton|} (init,formid) hst
	# (html,inputs,hst)	= mkButton (init,formid) l hst
	= ({ changed		= False
	   , value			= formid.ival
	   , form			= html
	   , inputs			= inputs
	   },hst)
where
	(HtmlButton l t)	= formid.ival

gForm{|HtmlCheckbox|} (init,formid =: {mode}) hst =:{cntr}
	# inputid			= formid.id +++ "-" +++ toString cntr
	= ({ changed		= False
	   , value			= formid.ival
	   , form			= [InputTag [ TypeAttr "checkbox"
	   								, NameAttr inputid
	   								, IdAttr inputid
	   								: if t [CheckedAttr] []
	   								]]
	   					   ++ if (isEmpty l) [] [LabelTag [ForAttr inputid] l]
	   , inputs			= [{inputid = inputid, formid = formid.id, updateon = if (mode == Edit ) OnChange OnSubmit}]
	   },setHStCntr (cntr + 1) hst)
where
	(HtmlCheckbox l t) = formid.ival

gForm{|HtmlSelect|} (init,formid) hst
	# (html,inputs,hst)	= mkSelect (init,formid) v o hst
	= ({ changed		= False
	   , value			= formid.ival
	   , form			= html
	   , inputs			= inputs
	   },hst)
where
	(HtmlSelect o v)	= formid.ival

gForm{|HtmlTextarea|} (init,formid =: {mode}) hst =:{cntr}
# inputid = formid.id +++ "-" +++ toString cntr
= (	{ changed			= False
	, value				= formid.ival
	, form				= [TextareaTag	[ NameAttr inputid
										, IdAttr inputid
										, RowsAttr (toString rows)
										]
										[RawText val]
						  ]
	, inputs			= [{inputid = inputid, formid = formid.id, updateon = (if (mode == Submit) OnSubmit OnChange)}]
	},setHStCntr (cntr + 1) hst)
where
	(HtmlTextarea rows val) = formid.ival

cleanString :: !String -> String
cleanString name = {clean_char c \\ c<-: name}
where
	clean_char '"' = ' '
	clean_char c   = c

gForm{|RadioButton|} (init,formid) hst =:{cntr} 
= case formid.ival of
	v=:(RBChecked name)
	= (	{ changed		= False
		, value			= v
		, form			= [InputTag (onMode formid.mode [] [] [DisabledAttr] [] ++
							[ TypeAttr			"radio"
							, ValueAttr			(cleanString name)
							, NameAttr			(encodeTriplet (formid.id,cntr,UpdS name))
							, CheckedAttr
							, IdAttr			(encodeInputId (formid.id,cntr,UpdS name))
							] ++ (callClean "click" formid.mode "" formid.lifespan False) )
						]
		, inputs		= []
		},incrHStCntr 1 hst)
	v=:(RBNotChecked name)
	= (	{ changed		= False
		, value			= v
		, form			= [InputTag (onMode formid.mode [] [] [DisabledAttr] [] ++
							[ TypeAttr			"radio"
							, ValueAttr			(cleanString name)
							, NameAttr			(encodeTriplet (formid.id,cntr,UpdS ""))
							, IdAttr			(encodeInputId (formid.id,cntr,UpdS name))
							] ++ (callClean "click" formid.mode "" formid.lifespan False) )
						]
		, inputs		= []
		},incrHStCntr 1 hst)

gForm{|RadioGroup|} (init, formid) hst
# (cntr, hst)			= getHStCntr hst
= case formid.ival of
	v=:(RadioGroup (sel, itemlist))
	= ( { changed		= False
		, value			= v
		, form			= flatten [([InputTag (
							[ TypeAttr			"radio"
							, ValueAttr			(toString i)
							, NameAttr			(encodeTriplet (formid.id, cntr, UpdI sel))
							, IdAttr			((encodeInputId (formid.id,cntr,UpdI sel)) <+++ "_" <+++ i)
							] ++ (callClean "change" formid.mode "" formid.lifespan False)
							  ++ (if (i == sel) [CheckedAttr] []) ++ (onMode formid.mode [] [] [DisabledAttr] []))
							, LabelTag [ForAttr ((encodeInputId (formid.id,cntr,UpdI sel)) <+++ "_" <+++ i)] [Text body], BrTag [] ] )
							\\ body <- itemlist & i <- [0..]
						  ]
		, inputs		= []
		},incrHStCntr 1 hst)


gForm{|TextInput|} (init,formid) hst 	
# (cntr,hst)			= getHStCntr hst
# (body,inputs,hst)		= mkInput(init,formid) v hst
= ({changed=False, value=formid.ival, form=body, inputs=inputs },incrHStCntr 2 hst)
where
	(v,updv)			= case formid.ival of
							(TI i) = (toString i,UpdI i)
							(TR r) = (toString r,UpdR r)
							(TS s) = (cleanString s,UpdS s)

gForm{|TextArea|} (init,formid) hst 
# (cntr,hst)			= getHStCntr hst
= (	{ changed			= False
	, value				= formid.ival
	, form				= [TextareaTag 	((onMode formid.mode [] [] [DisabledAttr] []) ++
											[ NameAttr 		(encodeTriplet (formid.id,cntr,UpdS string))
						  					, RowsAttr 		(toString (if (row == 0) 10 row))
						  					, ColsAttr 		(toString (if (col == 0) 50 col))
						  					, IdAttr		(encodeTriplet (formid.id,cntr,UpdS string))
						  					] ++ (callClean "change" formid.mode formid.id formid.lifespan False))
						  		[Text string]
						  ]
	, inputs			= []
	},incrHStCntr 1 hst)
where
	(TextArea row col string) = formid.ival

gUpd{|TextArea|}       (UpdSearch (UpdS name) 0) (TextArea r c s) 	= (UpdDone,                TextArea r c (urlDecode name))			// update button value
gUpd{|TextArea|}       (UpdSearch val cnt)       t					= (UpdSearch val (cnt - 1),t)										// continue search, don't change
gUpd{|TextArea|}       (UpdCreate l)             _					= (UpdCreate l,            TextArea defsize defsize "")				// create default value
gUpd{|TextArea|}       mode                      t					= (mode,                   t)										// don't change

gForm{|PasswordBox|} (init,formid) hst 	
= case formid.ival of
	(PasswordBox password) 
	# (body,hst)		= mkPswInput (init,formid) password (UpdS password) hst
	= ({ changed		= False
	   , value			= PasswordBox password
	   , form			= [body]
	   , inputs			= []
	   },incrHStCntr 1 hst)
where
	mkPswInput :: !(InIDataId d) String UpdValue !*HSt -> (!HtmlTag,!*HSt) 
	mkPswInput (init,formid=:{mode}) sval updval hst=:{cntr}
	| mode == Edit || mode == Submit
		= ( InputTag 	([ TypeAttr		"password"
						, ValueAttr		(cleanString sval)
						, NameAttr		(encodeTriplet (formid.id,cntr,updval))
						, IdAttr (encodeInputId (formid.id,cntr,updval))
						] ++ if (mode == Edit) (callClean "change" Edit "" formid.lifespan False) [] )
			
			,incrHStCntr 1 hst)
	| mode == Display
		= ( InputTag 	[ TypeAttr		"password"
						, ValueAttr		(cleanString sval)
						]
			,incrHStCntr 1 hst)
	= ( SpanTag [][] ,incrHStCntr 1 hst )

// time and date

import Time

getTimeAndDate :: !*HSt -> *(!(!HtmlTime,!HtmlDate),!*HSt)
getTimeAndDate hst=:{world = world=:{worldC}}
# (tm,worldC)				= localTime worldC
= ((HtmlTime tm.hour tm.min tm.sec,HtmlDate tm.mday tm.mon tm.year),{hst & world = {world & worldC = worldC}})

gForm {|HtmlTime|} (init,formid) hst
	= specialize (flip mkBimapEditor {map_to = toPullDown, map_from = fromPullDown}) (init,formid <@ nPage) hst
where
	nPage = if (formid.lifespan == Client) Client Page
	toPullDown (HtmlTime h m s)	= (hv,mv,sv)
	where
		hv					= HtmlSelect [(toString i,toString i) \\ i <- [0..23]] (toString h)
		mv					= HtmlSelect [(toString i,toString i) \\ i <- [0..59]] (toString m)
		sv					= HtmlSelect [(toString i,toString i) \\ i <- [0..59]] (toString s)

	fromPullDown (hv,mv,sv)	= HtmlTime (convert hv) (convert mv) (convert sv)
	where
		convert (HtmlSelect _ x) = toInt x

gForm {|HtmlDate|} (init,formid) hst 
	= specialize (flip mkBimapEditor {map_to = toPullDown, map_from = fromPullDown}) (init,formid <@ nPage) hst
where
	nPage = if (formid.lifespan == Client) Client Page
	toPullDown (HtmlDate d m y)	= (dv,mv,yv)
	where
		dv					= HtmlSelect [(toString i, toString i) \\ i <- [1..31]] (toString md)
		mv					= HtmlSelect [(toString i, toString i) \\ i <- [1..12]] (toString mm)
		yv					= HtmlSelect [(toString i, toString i) \\ i <- [1950..2015]] (toString my)

		my					= if (y >= 1950 && y <= 2015) y 2007
		md					= if (d >= 1    && d <= 31)   d 1
		mm					= if (m >= 1    && m <= 12)   m 1

	fromPullDown (dv,mv,yv)	= HtmlDate (convert dv) (convert mv) (convert yv)
	where
		convert (HtmlSelect _ x)= toInt x
		
		
gForm {|RefreshTimer|} (init,formid) hst = case formid.ival of
		RefreshTimer timeout
			# (cntr,hst)			= getHStCntr hst
			# triplet = encodeTriplet (formid.id,cntr,UpdS "timer")
			# inputid = encodeInputId (formid.id,cntr,UpdS "timer")
			# timedcode = "toClean(document.getElementById('" +++ inputid +++ "'),'" +++ triplet +++ "',true,false," +++ (IF_ClientTasks "true" "false") +++ ");"
			# script =  [RawText ("<input type=\"hidden\" id=\""+++ inputid +++"\" /><script type=\"text/javascript\">setTimeout(\"" +++ timedcode +++ "\"," +++ toString timeout +++ ");</script>")]
			= ({ changed = False, value = formid.ival, form = script, inputs = [] }, incrHStCntr 1 hst)


// Updates that have to be treated specially:

gUpd{|HtmlButton|}	(UpdSearch (UpdB b) 0) 	(HtmlButton s _)		= (UpdDone, HtmlButton s b)										// update value
gUpd{|HtmlButton|}	(UpdSearch val cntr)      v						= (UpdSearch val (cntr - 1),v)									// continue search, don't change
gUpd{|HtmlButton|}	(UpdCreate l)             _						= (UpdCreate l, HtmlButton "Press" False)						// create default value
gUpd{|HtmlButton|}	mode                      v						= (mode, v)														// don't change

gUpd{|HtmlCheckbox|}	(UpdSearch (UpdB b) 0) (HtmlCheckbox s _)	= (UpdDone, HtmlCheckbox s b)									// update value
gUpd{|HtmlCheckbox|}	(UpdSearch upd cntr)	v					= (UpdSearch upd (cntr - 1), v)									// continue search, don't change
gUpd{|HtmlCheckbox|}	(UpdCreate l)			_					= (UpdCreate l, HtmlCheckbox [] False)							// create default value
gUpd{|HtmlCheckbox|}	 mode					v					= (mode, v)														// don't change

gUpd{|HtmlSelect|} (UpdSearch (UpdS val) 0) (HtmlSelect options _)	= (UpdDone, HtmlSelect options val)
gUpd{|HtmlSelect|} (UpdSearch upd cntr)		 v						= (UpdSearch upd (cntr - 1), v)									// continue search, don't change
gUpd{|HtmlSelect|} (UpdCreate l)             _						= (UpdCreate l, HtmlSelect [] "error")							// create default value
gUpd{|HtmlSelect|} mode                      v						= (mode, v)														// don't change

gUpd{|HtmlTextarea|}	(UpdSearch (UpdS val) 0) (HtmlTextarea r _)	= (UpdDone, HtmlTextarea r val)									// update value
gUpd{|HtmlTextarea|}	(UpdSearch val cntr)      v					= (UpdSearch val (cntr - 1),v)									// continue search, don't change
gUpd{|HtmlTextarea|}	(UpdCreate l)             _					= (UpdCreate l, HtmlTextarea 5 "")								// create default value
gUpd{|HtmlTextarea|}	mode                      v					= (mode, v)														// don't change

gUpd{|RadioGroup|} 	(UpdSearch (UpdI sel) 0) (RadioGroup (index,itemlist)) 
																= (UpdDone,                RadioGroup (sel,itemlist))				// update integer value
gUpd{|RadioGroup|} 	(UpdSearch val cnt)       v					= (UpdSearch val (cnt - 1),v)										// continue search, don't change
gUpd{|RadioGroup|} 	(UpdCreate l)             _					= (UpdCreate l,            RadioGroup (0,["error"]))				// create default value
gUpd{|RadioGroup|}	mode                      v					= (mode,                   v)										// don't change

gUpd{|TextInput|}    (UpdSearch (UpdI ni) 0)   (TI i)			= (UpdDone,                TI ni)									// update integer value
gUpd{|TextInput|}    (UpdSearch (UpdR nr) 0)   (TR r)			= (UpdDone,                TR nr)									// update real    value
gUpd{|TextInput|}    (UpdSearch (UpdS ns) 0)   (TS s)			= (UpdDone,                TS ns)									// update string  value
gUpd{|TextInput|}    (UpdSearch val cnt)       i				= (UpdSearch val (cnt - 3),i)										// continue search, don't change
gUpd{|TextInput|}    (UpdCreate l)             _				= (UpdCreate l,            TS "")									// create default value
gUpd{|TextInput|}    mode                      i				= (mode,                   i)										// don't change

gUpd{|PasswordBox|}  (UpdSearch (UpdS name) 0) _				= (UpdDone,                PasswordBox name)						// update password value
gUpd{|PasswordBox|}  (UpdSearch val cnt)       b				= (UpdSearch val (cnt - 2),b)										// continue search, don't change
gUpd{|PasswordBox|}  (UpdCreate l)             _				= (UpdCreate l,            PasswordBox "")							// create default value
gUpd{|PasswordBox|}  mode                      b				= (mode,                   b)										// don't change

gUpd{|RefreshTimer|} (UpdSearch (UpdS name) 0) v				= (UpdDone,                v)										// We don't update
gUpd{|RefreshTimer|} (UpdSearch val cnt)       v				= (UpdSearch val (cnt - 1),v)										// continue search, don't change
gUpd{|RefreshTimer|} (UpdCreate l)             _				= (UpdCreate l,            RefreshTimer 0)							// create default value
gUpd{|RefreshTimer|} mode                      v				= (mode,                   v)										// don't change


// small utility stuf

instance toBool RadioButton where
	toBool (RBChecked _)					= True
	toBool _								= False


derive gEq PasswordBox, HtmlTime, HtmlDate
instance == PasswordBox where (==) pb1 pb2	= pb1 === pb2
instance == HtmlTime    where (==) ht1 ht2	= ht1 === ht2
instance == HtmlDate    where (==) hd1 hd2	= hd1 === hd2

instance == (DisplayMode a) | == a
where 
	(==) (DisplayMode a) (DisplayMode b)	= a == b
	(==) (EditMode a) (EditMode b)			= a == b
	(==) (HideMode a) (HideMode b)			= a == b
	(==) EmptyMode EmptyMode				= True
	(==) _ _								= False

derive gLexOrd HtmlTime, HtmlDate
instance + HtmlTime where (+) (HtmlTime h1 m1 s1) (HtmlTime h2 m2 s2)
											= HtmlTime (h1 + h2) (m1 + m2) (s1 + s2)
instance - HtmlTime where (-) (HtmlTime h1 m1 s1) (HtmlTime h2 m2 s2)
											= HtmlTime (h1 - h2) (m1 - m2) (s1 - s2)
instance < HtmlTime 
where 
	(<) (HtmlTime h1 m1 s1) (HtmlTime h2 m2 s2)
	| h1 <> h2 = h1 < h2
	| m1 <> m2 = m1 < m2
	= s1 < s2

instance < HtmlDate 
where 
	(<) (HtmlDate d1 m1 y1) (HtmlDate d2 m2 y2) 
	| y1 <> y2 = y1 < y2
	| m1 <> m2 = m1 < m2
	= d1 < d2

instance toString HtmlTime where
	toString (HtmlTime hrs min sec)				= toString hrs <+++ ":" <+++ min <+++ ":" <+++ sec
instance toString HtmlDate where
	toString (HtmlDate day month year)			= toString day <+++ "/" <+++ month <+++ "/" <+++ year

instance toInt HtmlSelect where
	toInt (HtmlSelect options val) = index val options
	where
		index val [] = -1
		index val [(l,v):lvs]
			| val == v	= 0
			| otherwise	= index val lvs

//TODO: is the HTML type still needed?
gForm {|HTML|} (init,formid ) hst	= specialize myeditor (Set,formid) hst
where
	myeditor (init,formid ) hst
	# (HTML bodytag)				= formid.ival
	= ({changed = False, form = bodytag, inputs = [], value = formid.ival},hst)

gUpd  {|HTML|} mode v				= (mode,v)

gPrint{|HTML|} (HTML x) st			= st <<- "XYX" 

gParse{|HTML|} st					= case gParse {|*|} st of
										Just "XYX" -> Just (HTML [EmptyBody])
										_          -> Just (HTML [EmptyBody])

gerda{|HTML|}  = abort "illegal gerda call for type HTML"
