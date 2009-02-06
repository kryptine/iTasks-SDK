implementation module iDataWidgets

import StdFunc, StdList, StdString, StdArray
import iDataForms, iDataFormlib, iDataTrivial, GenBimap


derive gForm	HtmlTag, HtmlAttr
derive gUpd		HtmlTag, HtmlAttr, <->, <|>, DisplayMode

derive gPrint	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlRadiogroup, HtmlTextarea, HtmlPassword, HtmlDate, HtmlTime, HtmlLabel
derive gParse	HtmlTag, HtmlAttr, <->, <|>, DisplayMode, HtmlButton, HtmlCheckbox, HtmlSelect, HtmlRadiogroup, HtmlTextarea, HtmlPassword, HtmlDate, HtmlTime, HtmlLabel
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

gForm{|HtmlRadiogroup|} (init,formid =: {mode}) hst =: {cntr,prefix} 	
	#inputname = formid.id +++ "-" +++ toString cntr
	#inputid = prefix +++ inputname
	= ({ changed		= False
	   , value			= formid.ival
	   , form			= [TableTag [] 
	   						[TrTag [] [
	   								TdTag [] [InputTag [ NameAttr inputname
	   										 , IdAttr (inputid +++ "-" +++ toString i)
	   										 , ValueAttr (toString i)
	   										 , TypeAttr "radio"
	   										 :if (i == cur) [CheckedAttr] []]],
	   								TdTag [] [LabelTag [ForAttr (inputid +++ "-" +++ toString i)] label]
	   								]
	   						\\ label <- options & i <- [0..]
	   						]
	   					  ]
	   , inputs			= [{formid = formid.id, inputid = cntr, type = "HtmlRadiogroup", updateon = (if (mode == Submit) OnSubmit OnChange)}]
	   },setHStCntr (cntr + 1) hst)
where
	(HtmlRadiogroup options cur)	= formid.ival


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

gForm {|HtmlTime|} (init,formid =:{mode}) hst =:{cntr,prefix}
	# (html,inputs,hst)	= mkInput (init,formid) "HtmlTime" (toString formid.ival) hst
	= ({ changed		= False
	   , value			= formid.ival
	   , form			= html
	   , inputs			= inputs
	   },hst)

gForm {|HtmlDate|} (init,formid =:{mode}) hst =:{cntr,prefix}
	# (html,inputs,hst)	= mkInput (init,formid) "HtmlDate" (toString formid.ival) hst
	= ({ changed		= False
	   , value			= formid.ival
	   , form			= html
	   , inputs			= inputs
	   },hst)

gForm {|HtmlLabel|} (init, formid) hst
	= ({ changed		= False
	   , value			= formid.ival
	   , form			= html
	   , inputs			= []
	   },hst)
where
	(HtmlLabel html)	= formid.ival

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

gUpd{|HtmlRadiogroup|}	(UpdSearch 0 upd)		(HtmlRadiogroup options _)= (UpdDone, HtmlRadiogroup options (toInt upd))				// TODO: Check if upd is in options
gUpd{|HtmlRadiogroup|}	(UpdSearch cntr upd)	cur						= (UpdSearch (dec cntr) upd, cur)								// continue search, don't change
gUpd{|HtmlRadiogroup|}	(UpdCreate l)			_						= (UpdCreate l, HtmlRadiogroup [] 0)							// create default value
gUpd{|HtmlRadiogroup|}	mode					cur						= (mode, cur)													// don't change

gUpd{|HtmlTextarea|}	(UpdSearch 0 upd) 		(HtmlTextarea r _)		= (UpdDone, HtmlTextarea r upd)									// update value
gUpd{|HtmlTextarea|}	(UpdSearch cntr upd)	cur						= (UpdSearch (dec cntr) upd, cur)								// continue search, don't change
gUpd{|HtmlTextarea|}	(UpdCreate l)			_						= (UpdCreate l, HtmlTextarea 5 "")								// create default value
gUpd{|HtmlTextarea|}	mode					cur						= (mode, cur)													// don't change

gUpd{|HtmlPassword|}	(UpdSearch 0 upd)		_						= (UpdDone, HtmlPassword upd)									// update password value
gUpd{|HtmlPassword|}	(UpdSearch cntr upd)	cur						= (UpdSearch (dec cntr) upd, cur)								// continue search, don't change
gUpd{|HtmlPassword|}	(UpdCreate l)			_						= (UpdCreate l, HtmlPassword "")								// create default value
gUpd{|HtmlPassword|}	mode					cur						= (mode, cur)													// don't change

gUpd{|HtmlDate|}		(UpdSearch 0 upd) 		_						= (UpdDone, HtmlDate d m y)									// update value
where
	(m,d,y)	=  (toInt (upd %(0,1)),toInt (upd %(3,4)),toInt (upd %(6,9)))
gUpd{|HtmlDate|}		(UpdSearch cntr upd)	cur						= (UpdSearch (dec cntr) upd, cur)								// continue search, don't change
gUpd{|HtmlDate|}		(UpdCreate l)			_						= (UpdCreate l, HtmlDate 1 1 2000)								// create default value
gUpd{|HtmlDate|}		mode					cur						= (mode, cur)

gUpd{|HtmlTime|}		(UpdSearch 0 upd) 		_						= (UpdDone, HtmlTime h m s)									// update value
where
	(h,m,s)	=  (toInt (upd %(0,1)),toInt (upd %(3,4)),toInt (upd %(6,7)))
gUpd{|HtmlTime|}		(UpdSearch cntr upd)	cur						= (UpdSearch (dec cntr) upd, cur)								// continue search, don't change
gUpd{|HtmlTime|}		(UpdCreate l)			_						= (UpdCreate l, HtmlTime 12 0 0)								// create default value
gUpd{|HtmlTime|}		mode					cur						= (mode, cur)

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
	toString (HtmlTime hrs min sec)				= (pad 2 hrs) +++ ":" +++ (pad 2 min) +++ ":" +++ (pad 2 sec)
instance toString HtmlDate where
	toString (HtmlDate day month year)			= (pad 2 month) +++ "/" +++ (pad 2 day) +++ "/" +++ (pad 4 year)

pad :: Int Int -> String
pad len num = (createArray (len - size nums) '0' ) +++ nums
where 
	nums = toString num

instance toInt HtmlSelect where
	toInt (HtmlSelect options val) = index val options
	where
		index val [] = -1
		index val [(l,v):lvs]
			| val == v	= 0
			| otherwise	= index val lvs

instance toInt HtmlRadiogroup where
	toInt (HtmlRadiogroup _ val) = val
	
instance toBool HtmlButton where
	toBool (HtmlButton _ val) = val
