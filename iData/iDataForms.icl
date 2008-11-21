implementation module iDataForms

import StdArray, StdChar, StdList, StdStrictLists, StdString, StdTuple, StdFile, StdOrdList
import ArgEnv, StdMaybe
import iDataTrivial, iDataSettings, iDataState
import StdGeneric, GenParse, GenPrint
import Http, HttpUtil, HttpServer, HttpTextUtil
import Gerda
import StdBimap
import HSt
import Html

derive bimap Form, FormId

gParse{|(->)|} gArg gRes _ 		= Nothing 
gPrint{|(->)|} gArg gRes _ _	= abort "functions can only be used with dynamic storage option!\n"

:: UpdMode	= UpdSearch Int String			// search for indicated postion and update it
			| UpdCreate [ConsPos]			// create new values if necessary
			| UpdDone						// and just copy the remaining stuff


// The function mkViewForm is *the* magic main function 
// All idata end up in this function
// It does everything, a swiss army knife editor that makes coffee too ...
// TODO: Try to make it do just a little less :)

mkViewForm :: !(InIDataId d) !(HBimap d v) !*HSt -> (Form d,!*HSt) | iData v
mkViewForm (init,formid) bm=:{toForm, updForm, fromForm, resetForm} hst=:{request,states,world} 
	| init == Const	&& formid.FormId.lifespan <> Temp
	= mkViewForm (init,{FormId| formid & lifespan = Temp}) bm hst			// constant i-data are never stored
	| init == Const															// constant i-data, no look up of previous value
	= calcnextView False Nothing states world				
	# (isupdated,view,states,world) = findFormInfo vformid states world 	// determine current view value in the state store
	= calcnextView isupdated view states world								// and calculate new i-data
where
	vformid					= reuseFormId formid (toForm init formid.ival Nothing)
	calcnextView isupdated view states world
		# (changedids,states)	= getUpdatedIds states
		# changed				= {isChanged = isupdated, changedId = changedids}
		# view					= toForm init formid.ival view				// map value to view domain, given previous view value
		# view					= updForm  changed view						// apply update function telling user if an update has taken place
		# newval				= fromForm changed view						// convert back to data domain	 
		# view					= case resetForm of							// optionally reset the view hereafter for next time
									Nothing 	-> view		 
									Just reset 	-> reset view
	
		| formid.mode == NoForm												// don't make a form at all
			# (states,world)	= replaceState` vformid view states world	// store new value into the store of states
			= ({ changed		= False
			   , value			= newval
			   , form			= []
			   , inputs			= []
			   }
			  , mkHSt request states world)
	
		# (viewform,{states,world})											// make a form for it
								= mkForm (init,if (init == Const) vformid (reuseFormId formid view)) (mkHSt request states world)
	
		| viewform.changed && not isupdated						 			// important: redo it all to handle the case that a user defined specialisation is updated !!
								= calcnextView True (Just viewform.Form.value) states world
	
		# (states,world)		= replaceState` vformid viewform.Form.value states world	// store new value into the store of states
	
		= (	{ changed			= isupdated
			, value				= newval
			, form				= viewform.form
			, inputs			= viewform.inputs
			}
		  ,mkHSt request states world)

	replaceState` vformid view states world
		| init <> Const			= setState vformid view states world
		| otherwise				= (states,world)

	findFormInfo formid formStates world
		# (updateids,formStates) 					= getUpdatedIds formStates											// get list of updated id's
		| not (isMember formid.id updateids)		
			# (bool,justcurstate,formStates,world)	= getState formid formStates world									// the current form is not updated
			= (False,justcurstate,formStates,world)
		# (updates,formStates)	= getFormUpdates formid.id formStates													// get my updates
		= case (getState formid formStates world) of
				(False,Just currentState,formStates,world) -> (False, Just currentState,formStates,world) 				// yes, but update already handled
				(True, Just currentState,formStates,world) -> updateState updates currentState formStates world			// yes, handle update
				(_,    Nothing,formStates,world) 		   -> (False, Nothing,formStates,world) 		  				// cannot find previously stored state

	updateState updates currentState formStates world
		# allUpdates = [(inputid, value) \\ {FormUpdate | inputid,value} <- updates]
		# newState = applyUpdates (sortUpdates allUpdates) currentState
		= (True, Just newState, formStates,world)

	sortUpdates updates								= sortBy (\(i1,v1) (i2,v2) -> i2 < i1) updates						// updates need to be applied in descending order
																														// of input id's
	applyUpdates [] currentState 					= currentState
	applyUpdates [(pos,upd):updates] currentState	= applyUpdates updates (snd (gUpd{|*|} (UpdSearch pos upd) currentState))

// specialize has to be used if a programmer wants to specialize gForm.
// It remembers the current value of the index in the expression and creates an editor to show this value.
// The value might have been changed with this editor, so the value returned might differ from the value you started with!

specialize :: !((InIDataId a) *HSt -> (Form a,*HSt)) !(InIDataId a) !*HSt -> (!Form a,!*HSt) | gUpd {|*|} a
specialize editor (init,formid) hst=:{cntr = inidx,states = formStates,world}
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

gForm{|Int|} (init,formid) hst 	
	# (html,inputs,hst)			= mkInput (init,formid) (toString formid.ival) hst
	= ({ changed				= False
	   , value					= formid.ival
	   , form					= html
	   , inputs					= inputs
	   },hst)
	
gForm{|Real|} (init,formid) hst 	
	# (html,inputs,hst)			= mkInput (init,formid) (toString formid.ival) hst
	= ({ changed				= False
	   , value					= formid.ival
	   , form					= html
	   , inputs					= inputs
	   },hst)

gForm{|Bool|} (init,formid) hst
	# (html,inputs,hst)			= mkSelect (init,formid) (toString formid.ival) [("False","False"),("True","True")] hst
	= ({ changed				= False
	   , value					= formid.ival
	   , form					= html
	   , inputs					= inputs
	   },hst)

gForm{|String|} (init,formid) hst 	
	# (html,inputs,hst)			= mkInput (init,formid) formid.ival hst
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
		   , form				= [TableTag [] nc.form]
		   , inputs				= nc.inputs
		   },hst)
	| t.gcd_type_def.gtd_num_conses == 1 
		# (nc,hst)				= gHc (init,reuseFormId formid c) (setHStCntr (cntr+1) hst) // don't display constructors that have no alternative
		= ({Form | nc & value = CONS nc.Form.value},hst)
	| t.gcd_name.[(size t.gcd_name) - 1] == '_' 										 	// don't display constructor names which end with an underscore
		# (nc,hst)				= gHc (init,reuseFormId formid c) (setHStCntr (cntr+1) hst) 
		= ({Form | nc & value = CONS nc.Form.value},hst)
				
	# (selHtml,selInputs,hst)	= mkSelect (init, formid) myname options hst
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

derive gForm Maybe //TODO: Define the gForm for maybe with a checkbox

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

gUpd{|Bool|}	(UpdSearch 0 upd) 		_	= (UpdDone, upd == "True")			// update boolean value
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

derive gUpd (,)
derive gUpd (,,)
derive gUpd (,,,)
derive gUpd Maybe

// gForm: automatically derives a Html form for any Clean type
mkForm :: !(InIDataId a) *HSt -> *(Form a, !*HSt)	| gForm {|*|} a
mkForm (init, formid =: {issub}) hst
	# (form, hst)	= gForm{|*|} (init, formid) hst
	| issub			= (form, hst) //Subforms are contained in the <form> tags of their parent
	| otherwise		= ({form &
						form = [FormTag [IdAttr formid.id] form.form]
					   }, hst)
	
	
//The basic building blocks for creating inputs
mkInput :: !(InIDataId d) String !*HSt -> ([HtmlTag], [InputId],*HSt) 
mkInput (init,formid=:{mode}) val hst=:{cntr} 
	| mode == Edit || mode == Submit
		# inputid	= (formid.id +++ "-" +++ toString cntr)
		= ( [InputTag 	[ TypeAttr		"text"
						, ValueAttr		val
						, NameAttr		inputid
						, IdAttr 		inputid
						]]
		  , [{formid = formid.id, inputid = cntr, updateon = if (mode == Edit ) OnChange OnSubmit}]
		  , setHStCntr (cntr+1) hst)
	| mode == Display
		= ( [InputTag 	[ TypeAttr		"text"
						, ValueAttr		val
						, DisabledAttr
						]]
		  , []
		  , setHStCntr (cntr+1) hst)
	= ([], [], setHStCntr (cntr+1) hst)
	
	
mkButton :: !(InIDataId d) String !*HSt -> ([HtmlTag],[InputId],*HSt)
mkButton (init, formid =: {mode}) label hst =: {cntr} 
	# inputid = (formid.id +++ "-" +++ toString cntr)
	= ( [ButtonTag	[ NameAttr	inputid
					, IdAttr	inputid
					, TypeAttr	"button"
					: if (mode == Display) [DisabledAttr] []
					] [Text label]]
	  , [{formid = formid.id, inputid = cntr, updateon = OnClick}]
	  , setHStCntr (cntr + 1) hst)

mkSelect :: !(InIDataId d) String [(String,String)] !*HSt -> ([HtmlTag],[InputId],*HSt)
mkSelect (init, formid=:{mode}) val options hst =:{cntr}
	# inputid = (formid.id +++ "-" +++ toString cntr)
	= ( [SelectTag	[ NameAttr	inputid
					, IdAttr	inputid
					: if (mode == Display) [DisabledAttr] []
					] [OptionTag [ValueAttr value:if (value == val) [SelectedAttr] [] ] [Text label] \\ (label,value) <- options]]
	  , [{formid = formid.id, inputid = cntr, updateon = (if (mode == Edit) OnChange OnSubmit)}]
	  , setHStCntr (cntr + 1) hst)

// The following two functions are not an example of decent Clean programming, but it works thanks to lazy evaluation...
toHtml :: a -> HtmlTag | gForm {|*|} a
toHtml a
	# (na,_)	= mkForm (Set,mkFormId "__toHtml" a <@ Display) (mkHSt http_emptyRequest (mkFormStates [] []) dummy)
	= BodyTag [] na.form
where
	dummy = { worldC 	= abort "dummy world for toHtml!\n"
			, gerda	 	= abort "dummy gerda for toHtml!\n"
			, datafile	= abort "dummy datafile for toHtml!\n"
			}

toHtmlForm :: !(*HSt -> *(Form a,*HSt)) -> [HtmlTag] | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
toHtmlForm anyform 
	# (na,hst)	= anyform (mkHSt http_emptyRequest (mkFormStates [] []) (abort "illegal call to toHtmlForm!\n"))
	= na.form
where
	dummy = { worldC 	= abort "dummy world for toHtmlForm!\n"
			, gerda	 	= abort "dummy gerda for toHtmlForm!\n"
			, datafile	= abort "dummy datafile for toHtmlForm!\n"
			}

createDefault :: a | gUpd{|*|} a 
createDefault = fromJust (snd (gUpd {|*|} (UpdSearch 0 "Just") Nothing))

