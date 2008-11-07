implementation module iDataWidgets

import StdFunc, StdList, StdString, StdArray
import iDataForms, iDataFormlib, iDataTrivial, StdBimap


derive gForm	[], HtmlTag, HtmlAttr
derive gUpd		[], HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlDate, HtmlTime

derive gPrint	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlTextarea, HtmlPassword, HtmlDate, HtmlTime, RefreshTimer
derive gParse	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlTextarea, HtmlPassword, HtmlDate, HtmlTime, RefreshTimer
derive gerda 	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlTextarea, HtmlPassword, HtmlDate, HtmlTime, RefreshTimer
//derive read 	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlDate, HtmlTime, RadioButton, RadioGroup, PullDownMenu, TextInput, TextArea, PasswordBox, RefreshTimer
//derive write 	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlDate, HtmlTime, RadioButton, RadioGroup, PullDownMenu, TextInput, TextArea, PasswordBox, RefreshTimer


// <-> works exactly the same as (,) and places its arguments next to each other, for compatibility with GEC's
gForm{|(<->)|} gHa gHb (init,formid) hst
# (na,hst)				= gHa (init,reuseFormId formid a) (incrHStCntr 1 hst)   	// one more for the now invisible <-> constructor 
# (nb,hst)				= gHb (init,reuseFormId formid b) hst
= (	{ changed			= na.changed || nb.changed 
	, value				= na.Form.value <-> nb.Form.value
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
	, value				= na.Form.value <|> nb.Form.value
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
			, value		= HideMode na.Form.value
			, form		= []
			, inputs	= []
			},hst)
	(DisplayMode a)
		# (na,hst)		= gHa (init,reuseFormId formid a <@ Display) (incrHStCntr 1 hst)
		= (	{ changed	= False
			, value		= DisplayMode na.Form.value
			, form		= na.form
			, inputs	= []
			},hst)
	(EditMode a) 
		# (na,hst)		= gHa (init,reuseFormId formid a <@ Edit) (incrHStCntr 1 hst)
		= (	{ changed	= na.changed
			, value		= EditMode na.Form.value
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
	   , inputs			= [{formid = formid.id, inputid = cntr, updateon = if (mode == Edit ) OnChange OnSubmit}]
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
	, inputs			= [{formid = formid.id, inputid = cntr, updateon = (if (mode == Submit) OnSubmit OnChange)}]
	},setHStCntr (cntr + 1) hst)
where
	(HtmlTextarea rows val) = formid.ival

gForm{|HtmlPassword|} (init,formid =: {mode}) hst =: {cntr} 	
	#inputid = formid.id +++ "-" +++ toString cntr

	= ({ changed		= False
	   , value			= formid.ival
	   , form			= [InputTag	[ NameAttr inputid
	   								, IdAttr inputid
	   								, ValueAttr v
	   								]
	   					  ]
	   , inputs			= [{formid = formid.id, inputid = cntr, updateon = (if (mode == Submit) OnSubmit OnChange)}]
	   },setHStCntr (cntr + 1) hst)
where
	(HtmlPassword v)	= formid.ival

gForm {|HtmlTime|} (init,formid) hst
	= specialize (flip mkBimapEditor {map_to = toPullDown, map_from = fromPullDown}) (init,formid <@ nPage) hst
where
	nPage = if (formid.FormId.lifespan == Client) Client Page
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
	nPage = if (formid.FormId.lifespan == Client) Client Page
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

gUpd{|HtmlPassword|}  (UpdSearch (UpdS val) 0) _				= (UpdDone,                HtmlPassword val)						// update password value
gUpd{|HtmlPassword|}  (UpdSearch upd cntr)       v				= (UpdSearch upd (cntr - 1),v)										// continue search, don't change
gUpd{|HtmlPassword|}  (UpdCreate l)             _				= (UpdCreate l,            HtmlPassword "")							// create default value
gUpd{|HtmlPassword|}  mode                      v				= (mode,                   v)										// don't change

gUpd{|RefreshTimer|} (UpdSearch (UpdS name) 0) v				= (UpdDone,                v)										// We don't update
gUpd{|RefreshTimer|} (UpdSearch val cnt)       v				= (UpdSearch val (cnt - 1),v)										// continue search, don't change
gUpd{|RefreshTimer|} (UpdCreate l)             _				= (UpdCreate l,            RefreshTimer 0)							// create default value
gUpd{|RefreshTimer|} mode                      v				= (mode,                   v)										// don't change


// small utility stuf

derive gEq HtmlTime, HtmlDate

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

