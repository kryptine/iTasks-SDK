implementation module iDataForms

import StdEnv, StdMaybe, ArgEnv
import iDataTrivial, iDataSettings, iDataState
import StdGeneric, GenParse, GenPrint, GenBimap
import Http, HttpUtil, HttpTextUtil
import HSt
import Html

derive bimap Form, FormId

gParse{|(->)|} gArg gRes _ 		= Nothing 
gPrint{|(->)|} gArg gRes _ _	= abort "functions can only be used with dynamic storage option!\n"

:: UpdMode	= UpdSearch Int String			// search for indicated postion and update it
			| UpdCreate [ConsPos]			// create new values if necessary
			| UpdDone						// and just copy the remaining stuff

//TODO: make nicely layed out version of list
derive gForm []
derive gUpd []

// The function mkViewForm is *the* magic main function 
// All idata end up in this function
// It does everything, a swiss army knife editor that makes coffee too ...
// TODO: Try to make it do just a little less :)

mkViewForm :: !(InIDataId d) !(HBimap d v) !*HSt -> (Form d,!*HSt) | iData v
mkViewForm (init,formid) bm=:{toForm, updForm, fromForm, resetForm} hst=:{HSt | prefix, request,states, nworld}
	| init == Const	&& formid.FormId.lifespan <> LSTemp
	= mkViewForm (init,{FormId| formid & lifespan = LSTemp}) bm hst			// constant i-data are never stored
	| init == Const															// constant i-data, no look up of previous value
	= calcnextView False Nothing states nworld				
	# (isupdated,view,states,nworld) = findFormInfo vformid states nworld 	// determine current view value in the state store
	= calcnextView isupdated view states nworld								// and calculate new i-data
where
	vformid					= reuseFormId formid (toForm init formid.ival Nothing)
	calcnextView isupdated view states nworld
		# (changedids,states)	= getUpdatedIds states
		# changed				= {isChanged = isupdated, changedId = changedids}
		# view					= toForm init formid.ival view				// map value to view domain, given previous view value
		# view					= updForm  changed view						// apply update function telling user if an update has taken place
		# newval				= fromForm changed view						// convert back to data domain	 
		# view					= case resetForm of							// optionally reset the view hereafter for next time
									Nothing 	-> view		 
									Just reset 	-> reset view
	
		| formid.mode == NoForm												// don't make a form at all
			# (states,nworld)	= replaceState` vformid view states nworld	// store new value into the store of states
			= ({ changed		= False
			   , value			= newval
			   , form			= []
			   , inputs			= []
			   }
			  , mkHSt prefix request states nworld)
	
		# (viewform,{states,nworld})										// make a form for it
								= mkForm (init,if (init == Const) vformid (reuseFormId formid view)) (mkHSt prefix request states nworld)
	
		| viewform.changed && not isupdated						 			// important: redo it all to handle the case that a user defined specialisation is updated !!
								= calcnextView True (Just viewform.Form.value) states nworld
	
		# (states,nworld)		= replaceState` vformid viewform.Form.value states nworld	// store new value into the store of states
	
		= (	{ changed			= isupdated
			, value				= newval
			, form				= viewform.form
			, inputs			= viewform.inputs
			}
		  ,mkHSt prefix request states nworld)

	replaceState` vformid view states nworld
		| init <> Const			= setState vformid view states nworld
		| otherwise				= (states,nworld)

	findFormInfo formid formStates nworld
		# (updateids,formStates) 					= getUpdatedIds formStates											// get list of updated id's
		| not (isMember formid.id updateids)	
			# (bool,justcurstate,formStates,nworld)	= getState formid formStates nworld									// the current form is not updated
			= (False,justcurstate,formStates,nworld)
		# (updates,formStates)	= getFormUpdates formid.id formStates													// get my updates
		= case (getState formid formStates nworld) of
				(False,Just currentState,formStates,nworld) -> (False, Just currentState,formStates,nworld) 			// yes, but update already handled
				(True, Just currentState,formStates,nworld) -> (updateState updates currentState formStates nworld)		// yes, handle update
				(_,    Nothing,formStates,nworld) 		   -> (False, Nothing,formStates,nworld) 		  				// cannot find previously stored state

	updateState updates currentState formStates nworld
		# allUpdates = [(inputid, value) \\ {FormUpdate | inputid,value} <- updates]
		# newState = applyUpdates (sortUpdates allUpdates) currentState
		= (True, Just newState, formStates,nworld)

	sortUpdates updates								= sortBy (\(i1,v1) (i2,v2) -> i2 < i1) updates						// updates need to be applied in descending order
																														// of input id's
	applyUpdates [] currentState 					= currentState
	applyUpdates [(pos,upd):updates] currentState	= applyUpdates updates (snd (gUpd{|*|} (UpdSearch pos upd) currentState))


// specialize has to be used if a programmer wants to specialize gForm.
// It remembers the current value of the index in the expression and creates an editor to show this value.
// The value might have been changed with this editor, so the value returned might differ from the value you started with!

specialize :: !((InIDataId a) *HSt -> (Form a,*HSt)) !(InIDataId a) !*HSt -> (!Form a,!*HSt) | gUpd {|*|} a
specialize editor (init,formid) hst=:{cntr = inidx,states = formStates, nworld}
	# nextidx					= incrIndex inidx formid.ival					// this value will be repesented differently, so increment counter 
	# (nv,hst) 					= editor (init,nformid) (setHStCntr 0 hst)
	= (nv,setHStCntr nextidx hst)
where
	nformid					= {formid & id = formid.id <+++ "_specialize_" <+++ inidx <+++ "_"}

	incrIndex :: Int v -> Int | gUpd {|*|} v
	incrIndex i v
		# (UpdSearch cntr _,v)	= gUpd {|*|} (UpdSearch -1 "0") v
		= i + (-1 - cntr)



/**
* gForm generats the html forms.
*/

generic gForm a :: !(InIDataId a) !*HSt -> *(Form a, !*HSt)	

gForm{|Int|} (init,formid=:{mode=Display}) hst
	= ({ changed		= False
	   , value			= formid.ival
	   , form			= [Text (toString formid.ival)]
	   , inputs			= []
	   },hst)
	   
gForm{|Int|} (init,formid) hst 	
	# (html,inputs,hst)			= mkInput (init,formid) "Int" (toString formid.ival) hst
	= ({ changed				= False
	   , value					= formid.ival
	   , form					= html
	   , inputs					= inputs
	   },hst)

gForm{|Real|} (init,formid=:{mode=Display}) hst
	= ({ changed		= False
	   , value			= formid.ival
	   , form			= [Text (toString formid.ival)]
	   , inputs			= []
	   },hst)	
gForm{|Real|} (init,formid) hst 	
	# (html,inputs,hst)			= mkInput (init,formid) "Real" (toString formid.ival) hst
	= ({ changed				= False
	   , value					= formid.ival
	   , form					= html
	   , inputs					= inputs
	   },hst)

gForm{|Bool|} (init,formid) hst
	# (html,inputs,hst)			= mkCheckBox (init,formid) "Bool" [] formid.ival hst
	= ({ changed				= False
	   , value					= formid.ival
	   , form					= html
	   , inputs					= inputs
	   },hst)

gForm{|String|} (init,formid=:{mode=Display}) hst
	= ({ changed		= False
	   , value			= formid.ival
	   , form			= [Text formid.ival]
	   , inputs			= []
	   },hst)

gForm{|String|} (init,formid) hst 	
	# (html,inputs,hst)			= mkInput (init,formid) "String" formid.ival hst
	= ({ changed				= False
	   , value					= formid.ival
	   , form					= html
	   , inputs					= inputs
	   },hst)

gForm{|UNIT|} _ hst
	= ({ changed				= False
	   , value					= UNIT
	   , form					= []
	   , inputs					= []
	   },hst)
   
gForm{|PAIR|} gHa gHb (init,formid) hst 
	# (na,hst)					= gHa (init, reuseFormId formid a) hst
	# (nb,hst)					= gHb (init, reuseFormId formid b) hst
	= ({ changed				= na.changed || nb.changed
	   , value					= PAIR na.Form.value nb.Form.value
	   , form					= na.form ++ nb.form
	   , inputs					= na.inputs ++ nb.inputs
	   },hst)
where
	(PAIR a b)					= formid.ival 

gForm{|EITHER|} gHa gHb (init,formid)  hst 
	= case formid.ival of
		(LEFT a)
			# (na,hst)			= gHa (init, reuseFormId formid a) hst
			= ({Form | na & value = LEFT na.Form.value},hst)
		(RIGHT b)
			# (nb,hst)			= gHb (init, reuseFormId formid b) hst
			= ({Form | nb & value = RIGHT nb.Form.value},hst)

gForm{|OBJECT|} gHo (init,formid) hst
	# (no,hst)					= gHo (init, reuseFormId formid o) hst
	= ({Form | no & value = OBJECT no.Form.value},hst)
where
	(OBJECT o) = formid.ival
	
gForm{|CONS of t|} gHc (init,formid) hst=:{cntr}
	| not (isEmpty t.gcd_fields) 		 
		# (nc,hst)				= gHc (init,reuseFormId formid c) (setHStCntr (cntr+1) hst) // don't display record constructor, but wrap the content in a table tag
		= ({ changed			= nc.changed
		   , value				= CONS nc.Form.value
		   , form				= [TableTag [ClassAttr "form-table"] nc.form]
		   , inputs				= nc.inputs
		   },hst)
	| t.gcd_type_def.gtd_num_conses == 1 
		# (nc,hst)				= gHc (init,reuseFormId formid c) (setHStCntr (cntr+1) hst) // don't display constructors that have no alternative
		= ({Form | nc & value = CONS nc.Form.value},hst)
	| t.gcd_name.[(size t.gcd_name) - 1] == '_' 										 	// don't display constructor names which end with an underscore
		# (nc,hst)				= gHc (init,reuseFormId formid c) (setHStCntr (cntr+1) hst) 
		= ({Form | nc & value = CONS nc.Form.value},hst)
				
	# (selHtml,selInputs,hst)	= mkSelect (init, formid) ("CONS:" +++ t.gcd_name) myname options hst
	# (nc,hst)					= gHc (init,reuseFormId formid c) hst
	= ({ changed				= nc.changed
	   , value					= CONS nc.Form.value
	   , form					= selHtml ++ nc.form
	   , inputs					= selInputs ++ nc.inputs
	   },hst)
where
	(CONS c)					= formid.ival
	myname						= t.gcd_name
	options						= map (\n -> (n.gcd_name,n.gcd_name)) t.gcd_type_def.gtd_conses

gForm{|FIELD of d |} gHx (init,formid) hst 
	# (nx,hst)					= gHx (init,reuseFormId formid x) hst
	| d.gfd_name.[(size d.gfd_name) - 1] == '_' 										 // don't display field names which end with an underscore
		= ({ changed	= False
		   , value		= formid.ival
		   , form		= []
		   , inputs		= []
		   },hst)
	| otherwise
		= ({ changed				= nx.changed
		   , value					= FIELD nx.Form.value
		   , form					= [TrTag [] [ThTag [] [Text fieldname],TdTag [] nx.form]]
		   , inputs					= nx.inputs
		   },hst)
where
	(FIELD x)				= formid.ival
	fieldname				= prettify d.gfd_name +++ ": "
	prettify name			= {c \\ c <- [toUpper lname : addspace lnames]}
	where
		[lname:lnames]		= [c \\ c <-: name]
		addspace []			= []
		addspace [c:cs]
		| isUpper c			= [' ',toLower c:addspace cs]
		| otherwise			= [c:addspace cs]

//gForms for the commonly used types

gForm{|(->)|} gHarg gHres (init,formid) hst 	
	= ({ changed = False, value = formid.ival, form = [], inputs = []},hst)


gForm{|(,)|} gHa gHb (init,formid) hst
# (na,hst)				= gHa (init,reuseFormId formid a) (incrHStCntr 1 hst)   	// one more for the now invisible (,) constructor 
# (nb,hst)				= gHb (init,reuseFormId formid b) hst
= (	{ changed			= na.changed || nb.changed
	, value				= (na.Form.value,nb.Form.value)
	, form				= [SpanTag [] na.form, SpanTag [] nb.form]
	, inputs			= na.inputs ++ nb.inputs
	},hst)
where
	(a,b)				= formid.ival

gForm{|(,,)|} gHa gHb gHc (init,formid) hst
# (na,hst)				= gHa (init,reuseFormId formid a) (incrHStCntr 1 hst)   	// one more for the now invisible (,,) constructor 
# (nb,hst)				= gHb (init,reuseFormId formid b) hst
# (nc,hst)				= gHc (init,reuseFormId formid c) hst
= (	{ changed			= na.changed || nb.changed || nc.changed
	, value				= (na.Form.value,nb.Form.value,nc.Form.value)
	, form				= [SpanTag [] na.form, SpanTag [] nb.form, SpanTag [] nc.form]
	, inputs			= na.inputs ++ nb.inputs ++ nc.inputs
	},hst)
where
	(a,b,c)				= formid.ival

gForm{|(,,,)|} gHa gHb gHc gHd (init,formid) hst
# (na,hst)				= gHa (init,reuseFormId formid a) (incrHStCntr 1 hst)   	// one more for the now invisible (,,) constructor 
# (nb,hst)				= gHb (init,reuseFormId formid b) hst
# (nc,hst)				= gHc (init,reuseFormId formid c) hst
# (nd,hst)				= gHd (init,reuseFormId formid d) hst
= (	{ changed			= na.changed || nb.changed || nc.changed || nd.changed
	, value				= (na.Form.value,nb.Form.value,nc.Form.value,nd.Form.value)
	, form				= [SpanTag [] na.form, SpanTag [] nb.form, SpanTag [] nc.form, SpanTag [] nd.form]
	, inputs			= na.inputs ++ nb.inputs ++ nc.inputs ++ nd.inputs
	},hst)
where
	(a,b,c,d)			= formid.ival

gForm{|Maybe|} gHa (init,formid) hst
= case formid.ival of
	Just a
		# (html,inputs,hst)		= mkCheckBox (init,formid) "Maybe" [] True hst
		# (na,hst)				= gHa (init,reuseFormId formid a) hst
		= ( { changed	= na.changed
			, value		= Just na.Form.value
			, form		= [TableTag [ ClassAttr "Maybe"]
								[TrTag [] [
									TdTag [] html,
									TdTag [] na.form
									]
								]
						  ]
			, inputs	= inputs ++ na.inputs
			},hst)
	Nothing
		# (html,inputs,hst)		= mkCheckBox (init,formid) "Maybe" [] False hst
		= ( { changed	= False
			, value		= Nothing
			, form		= html
			, inputs	= inputs
			}, hst)


derive gForm Void

// gUpd calculates a new value given the current value and a change in the value.
// If necessary it invents new default values (e.g. when switching from Nil to Cons)
// and leaves the rest untouched.

generic gUpd t :: UpdMode t -> (UpdMode,t)

gUpd{|Int|}		(UpdSearch 0 upd)		_	= (UpdDone, toInt upd)				// update integer value
gUpd{|Int|}		(UpdSearch cntr upd)    cur	= (UpdSearch (dec cntr) upd, cur)	// continue search, don't change
gUpd{|Int|}		(UpdCreate l)			_	= (UpdCreate l, 0)					// create default value
gUpd{|Int|}		mode 			  	    cur	= (mode,cur)						// don't change

gUpd{|Real|} 	(UpdSearch 0 upd)		_	= (UpdDone, toReal upd)				// update real value
gUpd{|Real|} 	(UpdSearch cntr upd)    cur = (UpdSearch (dec cntr) upd, cur)	// continue search, don't change
gUpd{|Real|} 	(UpdCreate l)			_	= (UpdCreate l, 0.0)				// create default value
gUpd{|Real|} 	mode 			  	    cur	= (mode,cur)						// don't change

gUpd{|Bool|}	(UpdSearch 0 upd) 		_	= (UpdDone, upd == "checked")		// update boolean value
gUpd{|Bool|}	(UpdSearch cntr upd)    cur = (UpdSearch (dec cntr) upd, cur)	// continue search, don't change
gUpd{|Bool|}	(UpdCreate l)			_	= (UpdCreate l, False)				// create default value
gUpd{|Bool|}	mode 			  	    cur	= (mode,cur)						// don't change

gUpd{|String|}	(UpdSearch 0 upd)		_	= (UpdDone, upd)					// update string value
gUpd{|String|}	(UpdSearch cntr upd)	cur	= (UpdSearch (dec cntr) upd, cur)	// continue search, don't change
gUpd{|String|}	(UpdCreate l)		   	_	= (UpdCreate l, "")					// create default value
gUpd{|String|}	mode 			  	 	cur	= (mode,cur)						// don't change

gUpd{|UNIT|}	mode					_	= (mode, UNIT)						// do nothing

gUpd{|PAIR|} gUpda gUpdb mode =:(UpdCreate l) _
	# (mode,a)					= gUpda mode (abort "PAIR a evaluated")
	# (mode,b)					= gUpdb mode (abort "PAIR b evaluated")
	= (mode,PAIR a b)
gUpd{|PAIR|} gUpda gUpdb mode (PAIR a b)
	# (mode,a)					= gUpda mode a
	# (mode,b)					= gUpdb mode b
	= (mode,PAIR a b)

gUpd{|EITHER|} gUpda gUpdb (UpdCreate [ConsLeft:cl]) _
	# (mode,a)					= gUpda (UpdCreate cl) (abort "LEFT a evaluated")
	= (mode,LEFT a)
gUpd{|EITHER|} gUpda gUpdb (UpdCreate [ConsRight:cl]) _
	# (mode,b)					= gUpdb (UpdCreate cl) (abort "RIGHT b evaluated")
	= (mode,RIGHT b)
gUpd{|EITHER|} gUpda gUpdb (UpdCreate []) _ 
	# (mode,b)					= gUpdb (UpdCreate []) (abort "Empty EITHER evaluated")
	= (mode,RIGHT b)
gUpd{|EITHER|} gUpda gUpdb mode (LEFT a)
	# (mode,a)					= gUpda mode a
	= (mode,LEFT a)
gUpd{|EITHER|} gUpda gUpdb mode (RIGHT b)
	# (mode,b)					= gUpdb mode b
	= (mode,RIGHT b)

gUpd{|OBJECT|} gUpdo (UpdCreate l) _ 										// invent new type
	# (mode,o)					= gUpdo (UpdCreate l) (abort "OBJECT evaluated")
	= (mode,OBJECT o)

gUpd{|OBJECT of typedes|} gUpdo (UpdSearch 0 upd) (OBJECT o) 				// constructor changed of this type
	# (mode,o)					= gUpdo (UpdCreate path) o
	= (UpdDone,OBJECT o)
	where
		path					= getConsPath (hd [cons \\ cons <- typedes.gtd_conses | cons.gcd_name == upd])

gUpd{|OBJECT|} gUpdo (UpdSearch cntr upd) (OBJECT o)						// search further
	# (mode,o)					= gUpdo (UpdSearch (dec cntr) upd) o
	= (mode,OBJECT o)
gUpd{|OBJECT|} gUpdo mode (OBJECT o)										// other cases
	# (mode,o)					= gUpdo mode o
	= (mode,OBJECT o)

gUpd{|CONS|} gUpdo (UpdCreate l) _ 											// invent new constructor ??
	# (mode,c)					= gUpdo (UpdCreate l) (abort "CONS evaluated")
	= (mode,CONS c)
gUpd{|CONS|} gUpdo mode (CONS c) 											// other cases
	# (mode,c)					= gUpdo mode c
	= (mode,CONS c)

gUpd{|FIELD|} gUpdx (UpdCreate l) _  										// invent new type 
	# (mode,x)					= gUpdx (UpdCreate l) (abort "Value of FIELD evaluated")
	= (mode,FIELD x)
gUpd{|FIELD|} gUpdx mode (FIELD x)											// other cases
	# (mode,x)					= gUpdx mode x
	= (mode,FIELD x)

gUpd{|(->)|} gUpdArg gUpdRes mode f
= (mode,f)	


gUpd{|Maybe|} gUpdx	(UpdSearch 0 upd) _											// if the checkbox is toggled
	| upd == "checked"
		# (_,x)	= gUpdx (UpdCreate []) (abort "Value of Maybe evaluated")
		= (UpdDone, Just x)		
	| otherwise
		= (UpdDone, Nothing)
		
gUpd{|Maybe|} gUpdx	(UpdSearch cntr upd)    Nothing		= (UpdSearch (dec cntr) upd, Nothing)	// continue search, don't change
gUpd{|Maybe|} gUpdx	(UpdSearch cntr upd)    (Just x)	
	# (mode, x)	= gUpdx (UpdSearch (dec cntr) upd) x											// continue search, don't change
	= (mode, Just x)
	
gUpd{|Maybe|} gUpdx	(UpdCreate l)			_			= (UpdCreate l, Nothing)				// create default value

gUpd{|Maybe|} gUpdx	mode 			  	    Nothing		= (mode, Nothing)						// don't change
gUpd{|Maybe|} gUpdx mode					(Just x)
	# (mode, x)	= gUpdx mode x							// don't change
	= (mode, Just x)
	
derive gUpd (,), (,,), (,,,), Void


// gForm: automatically derives a Html form for any Clean type
mkForm :: !(InIDataId a) *HSt -> *(Form a, !*HSt)	| gForm {|*|} a
mkForm (init, formid =: {issub}) hst =:{prefix}
	# (form, hst)	= gForm{|*|} (init, formid) hst
	| issub			= (form, hst) //Subforms are contained in the <form> tags of their parent
	| otherwise		= ({form &
						form = if (isEmpty form.form) [] [FormTag [IdAttr (prefix +++ formid.id)] form.form]
					   }, hst)
	
	
//The basic building blocks for creating inputs
mkInput :: !(InIDataId d) String String !*HSt -> ([HtmlTag], [InputDefinition],*HSt) 
mkInput (init,formid=:{mode}) type val hst=:{cntr,prefix} 
	| mode == Edit || mode == Submit
		# inputname = formid.id +++ "-" +++ toString cntr
		# inputid = prefix +++ inputname
		= ( [InputTag 	[ TypeAttr		"text"
						, ValueAttr		val
						, NameAttr		inputid
						, IdAttr 		inputid
						]]
		  , [{formid = formid.id, inputid = cntr, type = type, updateon = if (mode == Edit ) OnChange OnSubmit}]
		  , setHStCntr (cntr+1) hst)
	| mode == Display
		= ( [InputTag 	[ TypeAttr		"text"
						, ValueAttr		val
						, DisabledAttr
						]]
		  , []
		  , setHStCntr (cntr+1) hst)
	= ([], [], setHStCntr (cntr+1) hst)
	
	
mkButton :: !(InIDataId d) String String !*HSt -> ([HtmlTag],[InputDefinition],*HSt)
mkButton (init, formid =: {mode}) type label hst =: {cntr,prefix} 
	# inputname = formid.id +++ "-" +++ toString cntr
	# inputid = prefix +++ inputname
	= ( [ButtonTag	[ NameAttr	inputid
					, IdAttr	inputid
					, TypeAttr	"button"
					: if (mode == Display) [DisabledAttr] []
					] [Text label]]
	  , [{formid = formid.id, inputid = cntr, type = type, updateon = OnClick}]
	  , setHStCntr (cntr + 1) hst)

mkSelect :: !(InIDataId d) String String [(String,String)] !*HSt -> ([HtmlTag],[InputDefinition],*HSt)
mkSelect (init, formid=:{mode}) type val options hst =:{cntr,prefix}
	# inputname = formid.id +++ "-" +++ toString cntr
	# inputid = prefix +++ inputname
	= ( [SelectTag	[ NameAttr	inputid
					, IdAttr	inputid
					: if (mode == Display) [DisabledAttr] []
					] [OptionTag [ValueAttr value:if (value == val) [SelectedAttr] [] ] [Text label] \\ (label,value) <- options]]
	  , [{formid = formid.id, inputid = cntr, type = type, updateon = (if (mode == Edit) OnChange OnSubmit)}]
	  , setHStCntr (cntr + 1) hst)

mkCheckBox :: !(InIDataId d) String [HtmlTag] Bool !*HSt -> ([HtmlTag],[InputDefinition],*HSt)
mkCheckBox (init, formid=:{mode}) type label val hst =:{cntr,prefix}
	# inputname = formid.id +++ "-" +++ toString cntr
	# inputid = prefix +++ inputname
	= ( checkBoxForm inputid (InputTag	[ NameAttr	inputid
								, IdAttr	inputid
								, TypeAttr	"checkbox"
								: (if (mode == Display) [DisabledAttr] []) ++ (if val [CheckedAttr] [])
								]) label
	  , [{formid = formid.id, inputid = cntr, type = type, updateon = (if (mode == Edit) OnChange OnSubmit)}]
	  , setHStCntr (cntr + 1) hst)
where
	checkBoxForm inputid cb	[] = [cb]
	checkBoxForm inputid cb label = [TableTag [ClassAttr "id-checkbox"] [TrTag [] [TdTag [] [cb],TdTag [] [LabelTag [ForAttr inputid] label]]]]

// The following two functions are not an example of decent Clean programming, but it works thanks to lazy evaluation...
toHtml :: a -> HtmlTag | gForm {|*|} a
toHtml a
	# (na,_)	= mkForm (Set,mkFormId "__toHtml" a <@ Display) (mkHSt "" http_emptyRequest (mkFormStates [] []) dummy)
	= BodyTag [] na.form
where
	dummy = { world 	= abort "dummy world for toHtml!\n"
			, datafile	= abort "dummy datafile for toHtml!\n"
			}

toHtmlForm :: !(*HSt -> *(Form a,*HSt)) -> [HtmlTag] | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
toHtmlForm anyform 
	# (na,hst)	= anyform (mkHSt "" http_emptyRequest (mkFormStates [] []) (abort "illegal call to toHtmlForm!\n"))
	= na.form
where
	dummy = { world 	= abort "dummy world for toHtmlForm!\n"
			, datafile	= abort "dummy datafile for toHtmlForm!\n"
			}

createDefault :: a | gUpd{|*|} a 
createDefault = fromJust (snd (gUpd {|*|} (UpdSearch 0 "checked") Nothing))

