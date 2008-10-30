implementation module iDataButtons

import StdFunc, StdList, StdString, StdArray
import iDataFormlib, iDataHandler, iDataTrivial, StdBimap


derive gUpd  	(,), (,,), (,,,), (<->), <|>, HtmlDate, HtmlTime, DisplayMode/*, Button, CheckBox*/, RadioButton /*, PullDownMenu, TextInput , TextArea, PasswordBox*/
derive gPrint 	(,), (,,), (,,,), (<->), <|>, HtmlDate, HtmlTime, DisplayMode, Button, CheckBox, RadioButton, RadioGroup, PullDownMenu, TextInput, TextArea, PasswordBox, RefreshTimer
derive gParse 	(,), (,,), (,,,), (<->), <|>, HtmlDate, HtmlTime, DisplayMode, Button, CheckBox, RadioButton, RadioGroup, PullDownMenu, TextInput, TextArea, PasswordBox, RefreshTimer
derive gerda 	(,), (,,), (,,,), (<->), <|>, HtmlDate, HtmlTime, DisplayMode, Button, CheckBox, RadioButton, RadioGroup, PullDownMenu, TextInput, TextArea, PasswordBox, RefreshTimer
derive read 					  (<->), <|>, HtmlDate, HtmlTime, DisplayMode, Button, CheckBox, RadioButton, RadioGroup, PullDownMenu, TextInput, TextArea, PasswordBox, RefreshTimer
derive write 	  				  (<->), <|>, HtmlDate, HtmlTime, DisplayMode, Button, CheckBox, RadioButton, RadioGroup, PullDownMenu, TextInput, TextArea, PasswordBox, RefreshTimer

EmptyBody :== SpanTag [] []
defpixel :== 100
defsize	:== 10

gForm {|HTML|} (init,formid ) hst	= specialize myeditor (Set,formid) hst
where
	myeditor (init,formid ) hst
	# (HTML bodytag)				= formid.ival
	= ({changed = False, form = bodytag, value = formid.ival},hst)

gUpd  {|HTML|} mode v				= (mode,v)

gPrint{|HTML|} (HTML x) st			= st <<- "XYX" 

gParse{|HTML|} st					= case gParse {|*|} st of
										Just "XYX" -> Just (HTML [EmptyBody])
										_          -> Just (HTML [EmptyBody])

gerda{|HTML|}  = abort "illegal gerda call for type HTML"

// Tuples are placed next to each other, pairs below each other ...
layoutTableAtts	:== []	// default table attributes for arranging layout

gForm{|(,)|} gHa gHb (init,formid) hst
# (na,hst)				= gHa (init,reuseFormId formid a) (incrHStCntr 1 hst)   	// one more for the now invisible (,) constructor 
# (nb,hst)				= gHb (init,reuseFormId formid b) hst
= (	{ changed			= na.changed || nb.changed
	, value				= (na.value,nb.value)
	, form				= [SpanTag [] na.form, SpanTag [] nb.form]
	},hst)
where
	(a,b)				= formid.ival

gForm{|(,,)|} gHa gHb gHc (init,formid) hst
# (na,hst)				= gHa (init,reuseFormId formid a) (incrHStCntr 1 hst)   	// one more for the now invisible (,,) constructor 
# (nb,hst)				= gHb (init,reuseFormId formid b) hst
# (nc,hst)				= gHc (init,reuseFormId formid c) hst
= (	{ changed			= na.changed || nb.changed || nc.changed
	, value				= (na.value,nb.value,nc.value)
	, form				= [SpanTag [] na.form, SpanTag [] nb.form, SpanTag [] nc.form]
	},hst)
where
	(a,b,c)				= formid.ival

gForm{|(,,,)|} gHa gHb gHc gHd (init,formid) hst
# (na,hst)				= gHa (init,reuseFormId formid a) (incrHStCntr 1 hst)   	// one more for the now invisible (,,) constructor 
# (nb,hst)				= gHb (init,reuseFormId formid b) hst
# (nc,hst)				= gHc (init,reuseFormId formid c) hst
# (nd,hst)				= gHd (init,reuseFormId formid d) hst
= (	{ changed			= na.changed || nb.changed || nc.changed || nd.changed
	, value				= (na.value,nb.value,nc.value,nd.value)
	, form				= [SpanTag [] na.form, SpanTag [] nb.form, SpanTag [] nc.form, SpanTag [] nd.form]
	},hst)
where
	(a,b,c,d)			= formid.ival

// <-> works exactly the same as (,) and places its arguments next to each other, for compatibility with GEC's

gForm{|(<->)|} gHa gHb (init,formid) hst
# (na,hst)				= gHa (init,reuseFormId formid a) (incrHStCntr 1 hst)   	// one more for the now invisible <-> constructor 
# (nb,hst)				= gHb (init,reuseFormId formid b) hst
= (	{ changed			= na.changed || nb.changed 
	, value				= na.value <-> nb.value
	, form				= [SpanTag [] na.form, SpanTag [] nb.form]
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
			},hst)
	(DisplayMode a)
		# (na,hst)		= gHa (init,reuseFormId formid a <@ Display) (incrHStCntr 1 hst)
		= (	{ changed	= False
			, value		= DisplayMode na.value
			, form		= na.form
			},hst)
	(EditMode a) 
		# (na,hst)		= gHa (init,reuseFormId formid a <@ Edit) (incrHStCntr 1 hst)
		= (	{ changed	= na.changed
			, value		= EditMode na.value
			, form		= na.form
			},hst)
	EmptyMode
		= (	{ changed	= False
			, value		= EmptyMode
			, form		= [EmptyBody]
			},incrHStCntr 1 hst)

// Buttons to press
cleanString :: !String -> String
cleanString name = {clean_char c \\ c<-: name}
where
	clean_char '"' = ' '
	clean_char c   = c


gForm{|Button|} (init,formid) hst 
# (cntr,hst)			= getHStCntr hst
= case formid.ival of
	v=:(LButton size bname)
	= (	{ changed		= False
		, value			= v
		, form			= [InputTag (onMode formid.mode [] [] [DisabledAttr] [] ++
							[ TypeAttr		"button"
							, ValueAttr		(cleanString bname)
							, NameAttr		(encodeTriplet (formid.id,cntr,UpdS bname))
							, StyleAttr 	("width:" <+++ size)
							, IdAttr 		(encodeInputId (formid.id,cntr,UpdS bname))
							] ++ (callClean "click" Edit (encodeTriplet (formid.id,cntr,UpdS bname)) formid.lifespan True) )
							]
		},(incrHStCntr 1 hst))
	v=:(PButton (height,width) ref)
	= (	{ changed		= False
		, value			= v
		, form			= [InputTag (onMode formid.mode [] [] [DisabledAttr] [] ++
							[ TypeAttr		"image"
							, ValueAttr		(cleanString ref)
							, NameAttr		(encodeTriplet (formid.id,cntr,UpdS ref))
							, StyleAttr 	("width: " <+++ width <+++ "px; height: " <+++ height <+++ "px")
							, IdAttr		(encodeInputId (formid.id,cntr,UpdS ref))
							, SrcAttr		ref
							] ++ (callClean "click" Edit (encodeTriplet (formid.id,cntr,UpdS ref)) formid.lifespan True)
							)]
		},incrHStCntr 1 hst)
	Pressed
	= gForm {|*|} (init,(setFormId formid (LButton defpixel "??"))) hst // end user should reset button

gForm{|CheckBox|} (init,formid) hst 
# (cntr,hst)			= getHStCntr hst
= case formid.ival of
	v=:(CBChecked name) 
	= (	{ changed		= False
		, value			= v
		, form			= [InputTag (onMode formid.mode [] [] [DisabledAttr] [] ++
							[ TypeAttr		"checkbox"
							, ValueAttr		(cleanString name)
							, NameAttr		(encodeTriplet (formid.id,cntr,UpdS name))
							, CheckedAttr
							, IdAttr		(encodeInputId (formid.id,cntr,UpdS name))
						
							] ++ (callClean "click" formid.mode "" formid.lifespan False))
						]
		},incrHStCntr 1 hst)
	v=:(CBNotChecked name)
	= (	{ changed		= False
		, value			= v
		, form			= [InputTag (onMode formid.mode [] [] [DisabledAttr] [] ++
							[ TypeAttr		"checkbox"
							, ValueAttr		(cleanString name)
							, NameAttr		(encodeTriplet (formid.id,cntr,UpdS ""))
							, IdAttr		(encodeInputId (formid.id,cntr,UpdS name))
							] ++ (callClean "click" formid.mode "" formid.lifespan False))
						]
		},incrHStCntr 1 hst)

gForm{|RadioButton|} (init,formid) hst 
# (cntr,hst)			= getHStCntr hst
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
		},incrHStCntr 1 hst)


gForm{|PullDownMenu|} (init,formid) hst=:{submits}
# (cntr,hst)			= getHStCntr hst
= case formid.ival of
	v=:(PullDown (size,width) (menuindex,itemlist))
	= (	{ changed		= False
		, value			= v
		, form			= [SelectTag (onMode formid.mode [] [] [DisabledAttr] [] ++
							[ NameAttr			(selectorInpName +++ encodeString 
													(if (menuindex >= 0 && menuindex < length itemlist) (itemlist!!menuindex) ""))
							, SizeAttr			(toString size)
							, StyleAttr ("width:" <+++ width <+++ "px")
							, IdAttr (encodeInputId (formid.id,cntr,UpdS ""))
							] ++ (if submits [] (callClean "change" formid.mode "" formid.lifespan False))
							)
							[ OptionTag 
								[ ValueAttr (encodeTriplet (formid.id,cntr,UpdC (itemlist!!j)))
								: if (j == menuindex) [SelectedAttr] [] 
								]
								[Text elem]
								\\ elem <- itemlist & j <- [0..]
							]
						]
		},incrHStCntr 1 hst)

gForm{|TextInput|} (init,formid) hst 	
# (cntr,hst)			= getHStCntr hst
# (body,hst)			= mkInput(init,formid) v updv hst
= ({changed=False, value=formid.ival, form=[body]},incrHStCntr 2 hst)
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
	   },incrHStCntr 1 hst)
where
	mkPswInput :: !(InIDataId d) String UpdValue !*HSt -> (!HtmlTag,!*HSt) 
	mkPswInput (init,formid=:{mode}) sval updval hst=:{cntr,submits}
	| mode == Edit || mode == Submit
		= ( InputTag 	([ TypeAttr		"password"
						, ValueAttr		(cleanString sval)
						, NameAttr		(encodeTriplet (formid.id,cntr,updval))
						, IdAttr (encodeInputId (formid.id,cntr,updval))
						] ++ if (mode == Edit && not submits) (callClean "change" Edit "" formid.lifespan False) [] )
			
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

/*
import StdTime

getTimeAndDate :: !*HSt -> *(!(!HtmlTime,!HtmlDate),!*HSt)
getTimeAndDate hst
# (time,hst)				= accWorldHSt getCurrentTime hst
# (date,hst)				= accWorldHSt getCurrentDate hst
= ((HtmlTime time.hours time.minutes time.seconds,HtmlDate date.day date.month date.year),hst)
*/

gForm {|HtmlTime|} (init,formid) hst
	= specialize (flip mkBimapEditor {map_to = toPullDown, map_from = fromPullDown}) (init,formid <@ nPage) hst
where
	nPage = if (formid.lifespan == Client) Client Page
	toPullDown (HtmlTime h m s)	= (hv,mv,sv)
	where
		hv					= PullDown (1, defpixel/2) (h,[toString i \\ i <- [0..23]])
		mv					= PullDown (1, defpixel/2) (m,[toString i \\ i <- [0..59]])
		sv					= PullDown (1, defpixel/2) (s,[toString i \\ i <- [0..59]])

	fromPullDown (hv,mv,sv)	= HtmlTime (convert hv) (convert mv) (convert sv)
	where
		convert x			= toInt (toString x)

gForm {|HtmlDate|} (init,formid) hst 
	= specialize (flip mkBimapEditor {map_to = toPullDown, map_from = fromPullDown}) (init,formid <@ nPage) hst
where
	nPage = if (formid.lifespan == Client) Client Page
	toPullDown (HtmlDate d m y)	= (dv,mv,yv)
	where
		dv					= PullDown (1,  defpixel/2) (md-1,   [toString i \\ i <- [1..31]])
		mv					= PullDown (1,  defpixel/2) (mm-1,   [toString i \\ i <- [1..12]])
		yv					= PullDown (1,2*defpixel/3) (my-1950,[toString i \\ i <- [1950..2015]])

		my					= if (y >= 1950 && y <= 2015) y 2007
		md					= if (d >= 1    && d <= 31)   d 1
		mm					= if (m >= 1    && m <= 12)   m 1

	fromPullDown (dv,mv,yv)	= HtmlDate (convert dv) (convert mv) (convert yv)
	where
		convert x			= toInt (toString x)
		
		
gForm {|RefreshTimer|} (init,formid) hst = case formid.ival of
		RefreshTimer timeout
			# (cntr,hst)			= getHStCntr hst
			# triplet = encodeTriplet (formid.id,cntr,UpdS "timer")
			# inputid = encodeInputId (formid.id,cntr,UpdS "timer")
			# timedcode = "toClean(document.getElementById('" +++ inputid +++ "'),'" +++ triplet +++ "',true,false," +++ (IF_ClientTasks "true" "false") +++ ");"
			# script =  [RawText ("<input type=\"hidden\" id=\""+++ inputid +++"\" /><script type=\"text/javascript\">setTimeout(\"" +++ timedcode +++ "\"," +++ toString timeout +++ ");</script>")]
			= ({ changed = False, value = formid.ival, form = script}, incrHStCntr 1 hst)


// Updates that have to be treated specially:

gUpd{|PullDownMenu|} (UpdSearch (UpdC cname) 0) (PullDown dim (menuindex,itemlist)) 
																= (UpdDone,                PullDown dim (itemlist??cname,itemlist))	// update integer value
gUpd{|PullDownMenu|} (UpdSearch val cnt)       v				= (UpdSearch val (cnt - 1),v)										// continue search, don't change
gUpd{|PullDownMenu|} (UpdCreate l)             _				= (UpdCreate l,            PullDown (1,defpixel) (0,["error"]))		// create default value
gUpd{|PullDownMenu|} mode                      v				= (mode,                   v)										// don't change

gUpd{|Button|}       (UpdSearch (UpdS name) 0) _				= (UpdDone,                Pressed)									// update button value
gUpd{|Button|}       (UpdSearch val cnt)       b				= (UpdSearch val (cnt - 1),b)										// continue search, don't change
gUpd{|Button|}       (UpdCreate l)             _				= (UpdCreate l,            LButton defpixel "Press")				// create default value
gUpd{|Button|}       mode                      b				= (mode,                   b)										// don't change

gUpd{|CheckBox|}     (UpdSearch (UpdS name) 0) (CBChecked    s)	= (UpdDone,                CBNotChecked s)							// update CheckBox value
gUpd{|CheckBox|}     (UpdSearch (UpdS name) 0) (CBNotChecked s)	= (UpdDone,                CBChecked    s)							// update CheckBox value
gUpd{|CheckBox|}     (UpdSearch val cnt)       b				= (UpdSearch val (cnt - 1),b)										// continue search, don't change
gUpd{|CheckBox|}     (UpdCreate l)             _				= (UpdCreate l,            CBNotChecked "defaultCheckboxName")		// create default value
gUpd{|CheckBox|}     mode                      b				= (mode,                   b)										// don't change

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

instance toBool CheckBox where
	toBool (CBChecked _)					= True
	toBool _								= False

instance toBool Button where
	toBool Pressed							= True
	toBool _								= False

instance toInt PullDownMenu where
	toInt (PullDown _ (i,_))				= i

instance toString PullDownMenu where
	toString (PullDown _ (i,s))				= if (i>=0 && i <=length s) (s!!i) ""

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
