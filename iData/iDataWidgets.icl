implementation module iDataWidgets

import StdFunc, StdList, StdString, StdArray
import iDataForms, iDataFormlib, iDataTrivial, GenBimap


derive gForm	[], HtmlTag, HtmlAttr
derive gUpd		[], HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlDate, HtmlTime

derive gPrint	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlTextarea, HtmlPassword, HtmlDate, HtmlTime, HtmlLabel, RefreshTimer
derive gParse	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlTextarea, HtmlPassword, HtmlDate, HtmlTime, HtmlLabel, RefreshTimer
derive gerda 	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlTextarea, HtmlPassword, HtmlDate, HtmlTime, HtmlLabel, RefreshTimer
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
	# (html,inputs,hst)	= mkButton (init,formid) "HtmlButton" l hst
	= ({ changed		= False
	   , value			= formid.ival
	   , form			= html
	   , inputs			= inputs
	   },hst)
where
	(HtmlButton l t)	= formid.ival

gForm{|HtmlCheckbox|} (init,formid =: {mode}) hst =:{cntr,prefix}
	# (html, inputs, hst) = mkCheckBox (init,formid) "HtmlCheckbox" l t hst
	= ({ changed		= False
	   , value			= formid.ival
	   , form			= html
	   , inputs			= inputs
	   },hst)
where
	(HtmlCheckbox l t) = formid.ival

gForm{|HtmlSelect|} (init,formid) hst
	# (html,inputs,hst)	= mkSelect (init,formid) "HtmlSelect" v o hst
	= ({ changed		= False
	   , value			= formid.ival
	   , form			= html
	   , inputs			= inputs
	   },hst)
where
	(HtmlSelect o v)	= formid.ival

gForm{|HtmlTextarea|} (init,formid =:{mode}) hst =:{cntr,prefix}
# inputid = prefix +++ formid.id +++ "-" +++ toString cntr
= (	{ changed			= False
	, value				= formid.ival
	, form				= [TextareaTag	[ NameAttr inputid
										, IdAttr inputid
										, RowsAttr (toString rows)
										: (if (mode == Display) [DisabledAttr] [])
										]
										[RawText val]
						  ]
	, inputs			= [{formid = formid.id, inputid = cntr, type = "HtmlTextarea", updateon = (if (mode == Submit) OnSubmit OnChange)}]
	},setHStCntr (cntr + 1) hst)
where
	(HtmlTextarea rows val) = formid.ival

gForm{|HtmlPassword|} (init,formid =: {mode}) hst =: {cntr,prefix} 	
	#inputid = prefix +++ formid.id +++ "-" +++ toString cntr

	= ({ changed		= False
	   , value			= formid.ival
	   , form			= [InputTag	[ NameAttr inputid
	   								, IdAttr inputid
	   								, ValueAttr v
	   								]
	   					  ]
	   , inputs			= [{formid = formid.id, inputid = cntr, type = "HtmlPassword", updateon = (if (mode == Submit) OnSubmit OnChange)}]
	   },setHStCntr (cntr + 1) hst)
where
	(HtmlPassword v)	= formid.ival

gForm {|HtmlTime|} (init,formid) hst
	= specialize (flip mkBimapEditor {map_to = toPullDown, map_from = fromPullDown}) (init,formid <@ nPage) hst
where
	nPage = if (formid.FormId.lifespan == LSClient) LSClient LSPage
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
	nPage = if (formid.FormId.lifespan == LSClient) LSClient LSPage
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
		
gForm {|HtmlLabel|} (init, formid) hst
	= ({ changed		= False
	   , value			= formid.ival
	   , form			= html
	   , inputs			= []
	   },hst)
where
	(HtmlLabel html)	= formid.ival
	
//TODO: FIX. toClean no longer exists
gForm {|RefreshTimer|} (init,formid) hst = case formid.ival of
		RefreshTimer timeout
			# (cntr,hst)			= getHStCntr hst
			# triplet = ""
			# inputid = ""
			# timedcode = "toClean(document.getElementById('" +++ inputid +++ "'),'" +++ triplet +++ "',true,false," +++ (IF_ClientTasks "true" "false") +++ ");"
			# script =  [RawText ("<input type=\"hidden\" id=\""+++ inputid +++"\" /><script type=\"text/javascript\">setTimeout(\"" +++ timedcode +++ "\"," +++ toString timeout +++ ");</script>")]
			= ({ changed = False, value = formid.ival, form = script, inputs = [] }, incrHStCntr 1 hst)


// Updates that have to be treated specially:

gUpd{|HtmlButton|}		(UpdSearch 0 upd) 		(HtmlButton s _)		= (UpdDone, HtmlButton s (upd == "click"))						// update value
gUpd{|HtmlButton|}		(UpdSearch cntr upd)	cur						= (UpdSearch (dec cntr) upd,cur)								// continue search, don't change
gUpd{|HtmlButton|}		(UpdCreate l)			_						= (UpdCreate l, HtmlButton "Press" False)						// create default value
gUpd{|HtmlButton|}		mode					cur						= (mode, cur)													// don't change

gUpd{|HtmlCheckbox|}	(UpdSearch 0 upd)		(HtmlCheckbox s _)		= (UpdDone, HtmlCheckbox s (upd == "checked"))					// update value
gUpd{|HtmlCheckbox|}	(UpdSearch cntr upd)	cur						= (UpdSearch (dec cntr) upd, cur)								// continue search, don't change
gUpd{|HtmlCheckbox|}	(UpdCreate l)			_						= (UpdCreate l, HtmlCheckbox [] False)							// create default value
gUpd{|HtmlCheckbox|}	 mode					cur						= (mode, cur)													// don't change

gUpd{|HtmlSelect|}		(UpdSearch 0 upd)		(HtmlSelect options _)	= (UpdDone, HtmlSelect options upd)								// TODO: Check if upd is in options
gUpd{|HtmlSelect|}		(UpdSearch cntr upd)	cur						= (UpdSearch (dec cntr) upd, cur)								// continue search, don't change
gUpd{|HtmlSelect|}		(UpdCreate l)			_						= (UpdCreate l, HtmlSelect [] "error")							// create default value
gUpd{|HtmlSelect|}		mode					cur						= (mode, cur)													// don't change

gUpd{|HtmlTextarea|}	(UpdSearch 0 upd) 		(HtmlTextarea r _)		= (UpdDone, HtmlTextarea r upd)									// update value
gUpd{|HtmlTextarea|}	(UpdSearch cntr upd)	cur						= (UpdSearch (dec cntr) upd, cur)								// continue search, don't change
gUpd{|HtmlTextarea|}	(UpdCreate l)			_						= (UpdCreate l, HtmlTextarea 5 "")								// create default value
gUpd{|HtmlTextarea|}	mode					cur						= (mode, cur)													// don't change

gUpd{|HtmlPassword|}	(UpdSearch 0 upd)		_						= (UpdDone, HtmlPassword upd)									// update password value
gUpd{|HtmlPassword|}	(UpdSearch cntr upd)	cur						= (UpdSearch (dec cntr) upd, cur)								// continue search, don't change
gUpd{|HtmlPassword|}	(UpdCreate l)			_						= (UpdCreate l, HtmlPassword "")								// create default value
gUpd{|HtmlPassword|}	mode					cur						= (mode, cur)													// don't change

gUpd{|RefreshTimer|}	(UpdSearch 0 upd)		cur						= (UpdDone, cur)												// We don't update
gUpd{|RefreshTimer|}	(UpdSearch cntr upd)	cur						= (UpdSearch (dec cntr) upd, cur)								// continue search, don't change
gUpd{|RefreshTimer|}	(UpdCreate l)			_						= (UpdCreate l, RefreshTimer 0)									// create default value
gUpd{|RefreshTimer|}	mode					cur						= (mode, cur)													// don't change

gUpd{|HtmlLabel|}		(UpdSearch 0 upd)		cur						= (UpdDone, cur)												// We don't update
gUpd{|HtmlLabel|}		(UpdSearch cntr upd)	cur						= (UpdSearch cntr upd, cur)										// continue search, don't change
gUpd{|HtmlLabel|}		(UpdCreate l)			_						= (UpdCreate l, HtmlLabel [])									// create default value
gUpd{|HtmlLabel|}		mode					cur						= (mode, cur)

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

