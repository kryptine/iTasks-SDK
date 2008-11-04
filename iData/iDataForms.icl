implementation module iDataForms

import StdArray, StdChar, StdList, StdStrictLists, StdString, StdTuple, StdFile
import ArgEnv, StdMaybe
import iDataTrivial, iDataSettings, iDataState
import StdGeneric, GenParse, GenPrint
import Http, HttpUtil, HttpServer, HttpTextUtil
import Gerda
import StdBimap
import HSt
import Html

derive gPrint (,), (,,), (,,,), UpdValue
derive gParse (,), (,,), (,,,), UpdValue
derive gUpd		   (,,), (,,,)
derive gUpd Maybe

derive bimap Form, FormId

gParse{|(->)|} gArg gRes _ 		= Nothing 
gPrint{|(->)|} gArg gRes _ _	= abort "functions can only be used with dynamic storage option!\n"

:: UpdMode	= UpdSearch UpdValue Int		// search for indicated postion and update it
			| UpdCreate [ConsPos]			// create new values if necessary
			| UpdDone						// and just copy the remaining stuff


// The function mkViewForm is *the* magic main function 
// All idata end up in this function
// It does everything, a swiss army knife editor that makes coffee too ...
// TODO: Try to make it do just a little less :)

mkViewForm :: !(InIDataId d) !(HBimap d v) !*HSt -> (Form d,!*HSt) | iData v
mkViewForm (init,formid) bm=:{toForm, updForm, fromForm, resetForm} hst=:{request,states,world} 
| init == Const	&& formid.lifespan <> Temp
= mkViewForm (init,{formid & lifespan = Temp}) bm hst					// constant i-data are never stored
| init == Const															// constant i-data, no look up of previous value
= calcnextView False Nothing states world				
# (isupdated,view,states,world) = findFormInfo vformid states world 	// determine current view value in the state store
= calcnextView isupdated view states world								// and calculate new i-data
where
	vformid					= reuseFormId formid (toForm init formid.ival Nothing)

	calcnextView isupdated view states world
	# (changedids,states)	= getUpdateId states
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
							= calcnextView True (Just viewform.value) states world

	# (states,world)		= replaceState` vformid viewform.value states world	// store new value into the store of states

	= (	{ changed			= isupdated
		, value				= newval
		, form				= viewform.form
		, inputs			= viewform.inputs
		}
	  ,mkHSt request states world)

	replaceState` vformid view states world
	| init <> Const			= replaceState vformid view states world
	| otherwise				= (states,world)

//	findFormInfo :: FormId *FormStates *NWorld -> (Bool,Maybe a,*FormStates,*NWorld) | gUpd{|*|} a & gParse{|*|} a & TC a
	findFormInfo formid formStates world
	# (updateids,formStates) 					= getUpdateId formStates // get list of updated id's
	| not (isMember formid.id updateids)		
		# (bool,justcurstate,formStates,world)	= findState formid formStates world									// the current form is not updated
		= (False,justcurstate,formStates,world)
	# (alltriplets,formStates)	= getTriplets formid.id formStates		// get my update triplets
	= case (findState formid formStates world) of
			(False,Just currentState,formStates,world) -> (False, Just currentState,formStates,world) 				// yes, but update already handled
			(True, Just currentState,formStates,world) -> updateState alltriplets currentState formStates world		// yes, handle update
			(_,    Nothing,formStates,world) 		   -> (False, Nothing,formStates,world) 		  				// cannot find previously stored state


	updateState alltriplets currentState formStates world
	# allUpdates = [update \\ tripletupd <- alltriplets, (Just update) <- [examineTriplet tripletupd]]
	# newState = applyUpdates allUpdates currentState
	= (True,Just newState,formStates,world)

	applyUpdates [] currentState 					= currentState
	applyUpdates [(pos,upd):updates] currentState	= applyUpdates updates (snd (gUpd{|*|} (UpdSearch upd pos) currentState))

	examineTriplet :: TripletUpdate -> Maybe (Int,UpdValue)
	examineTriplet tripletupd
		= case parseTriplet tripletupd of
			((sid,pos,UpdC s), Just "") 								= (Just (pos,UpdC s) )
			((sid,pos,UpdC s), _) 										= (Just (pos,UpdC s) )
			(_,_)= case parseTriplet tripletupd of
					((sid,pos,UpdI i), Just ni) 						= (Just (pos,UpdI ni))
					((sid,pos,UpdI i), _) 								= (Just (pos,UpdI i) )
					(_,_) = case parseTriplet tripletupd of
							((sid,pos,UpdR r), Just nr) 				= (Just (pos,UpdR nr))
							((sid,pos,UpdR r), _) 						= (Just (pos,UpdR r) )
							(_,_) = case parseTriplet tripletupd of
									((sid,pos,UpdB b), Just nb) 		= (Just (pos,UpdB nb))
									((sid,pos,UpdB b), _) 				= (Just (pos,UpdB b) )
									(_,_) = case parseTriplet tripletupd of
										((sid,pos,UpdS s),	Just ns)	= (Just (pos,UpdS ns))
										_								= case  tripletupd of
																			((sid,pos,UpdS s), ns) -> (Just (pos, UpdS ns))
																			_ -> (Nothing			 )
	where
		parseTriplet :: TripletUpdate -> (Triplet,Maybe b) | gParse {|*|} b
		parseTriplet (triplet,update) = (triplet,parseString update)
		

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
	# (UpdSearch _ cnt,v)	= gUpd {|*|} (UpdSearch (UpdI 0) -1) v
	= i + (-1 - cnt)



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
	   , value					= PAIR na.value nb.value
	   , form					= na.form ++ nb.form
	   , inputs					= na.inputs ++ nb.inputs
	   },hst)
where
	(PAIR a b)					= formid.ival 

gForm{|EITHER|} gHa gHb (init,formid)  hst 
	= case formid.ival of
		(LEFT a)
			# (na,hst)			= gHa (init, reuseFormId formid a) hst
			= ({na & value = LEFT na.value},hst)
		(RIGHT b)
			# (nb,hst)			= gHb (init, reuseFormId formid b) hst
			= ({nb & value = RIGHT nb.value},hst)

gForm{|OBJECT|} gHo (init,formid) hst
	# (no,hst)					= gHo (init, reuseFormId formid o) hst
	= ({no & value = OBJECT no.value},hst)
where
	(OBJECT o) = formid.ival
	
gForm{|CONS of t|} gHc (init,formid) hst=:{cntr}
	| not (isEmpty t.gcd_fields) 		 
		# (nc,hst)				= gHc (init,reuseFormId formid c) (setHStCntr (cntr+1) hst) // don't display record constructor, but wrap the content in a table tag
		= ({ changed			= nc.changed
		   , value				= CONS nc.value
		   , form				= [TableTag [] nc.form]
		   , inputs				= nc.inputs
		   },hst)
	| t.gcd_type_def.gtd_num_conses == 1 
		# (nc,hst)				= gHc (init,reuseFormId formid c) (setHStCntr (cntr+1) hst) // don't display constructors that have no alternative
		= ({nc & value = CONS nc.value},hst)
	| t.gcd_name.[(size t.gcd_name) - 1] == '_' 										 	// don't display constructor names which end with an underscore
		# (nc,hst)				= gHc (init,reuseFormId formid c) (setHStCntr (cntr+1) hst) 
		= ({nc & value = CONS nc.value},hst)
				
	# (selHtml,selInputs,hst)	= mkSelect (init, formid) myname options hst
	# (nc,hst)					= gHc (init,reuseFormId formid c) hst
	= ({ changed				= nc.changed
	   , value					= CONS nc.value
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
		   , value					= FIELD nx.value
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


gForm{|(->)|} gHarg gHres (init,formid) hst 	
	= ({ changed = False, value = formid.ival, form = [], inputs = []},hst)

// gUpd calculates a new value given the current value and a change in the value.
// If necessary it invents new default values (e.g. when switching from Nil to Cons)
// and leaves the rest untouched.

generic gUpd t :: UpdMode t -> (UpdMode,t)

gUpd{|Int|} (UpdSearch (UpdI ni) 0) 	_ = (UpdDone,ni)					// update integer value
gUpd{|Int|} (UpdSearch val cnt)     	i = (UpdSearch val (dec cnt),i)		// continue search, don't change
gUpd{|Int|} (UpdCreate l)				_ = (UpdCreate l,0)					// create default value
gUpd{|Int|} mode 			  	    	i = (mode,i)						// don't change

gUpd{|Real|} (UpdSearch (UpdR nr) 0) 	_ = (UpdDone,nr)					// update real value
gUpd{|Real|} (UpdSearch val cnt)     	r = (UpdSearch val (dec cnt),r)		// continue search, don't change
gUpd{|Real|} (UpdCreate l)			 	_ = (UpdCreate l,0.0)				// create default value
gUpd{|Real|} mode 			  	     	r = (mode,r)						// don't change

gUpd{|Bool|} (UpdSearch (UpdC nb) 0) 	_ = (UpdDone,nb=="True")			// update boolean value
gUpd{|Bool|} (UpdSearch val cnt)     	b = (UpdSearch val (dec cnt),b)		// continue search, don't change
gUpd{|Bool|} (UpdCreate l)			 	_ = (UpdCreate l,False)				// create default value
gUpd{|Bool|} mode 			  	     	b = (mode,b)						// don't change

gUpd{|String|} (UpdSearch (UpdS ns) 0) 	_ = (UpdDone,ns)					// update string value
gUpd{|String|} (UpdSearch val cnt)     	s = (UpdSearch val (dec cnt),s)		// continue search, don't change
gUpd{|String|} (UpdCreate l)		   	_ = (UpdCreate l,"")				// create default value
gUpd{|String|} mode 			  	 	s = (mode,s)						// don't change

gUpd{|UNIT|} mode _			= (mode,UNIT)

gUpd{|PAIR|} gUpda gUpdb mode=:(UpdCreate l) _								// invent a pair
# (mode,a)					= gUpda mode (abort "PAIR a evaluated")
# (mode,b)					= gUpdb mode (abort "PAIR b evaluated")
= (mode,PAIR a b)
gUpd{|PAIR|} gUpda gUpdb mode (PAIR a b)									// pass mode to a and b in all other cases
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
gUpd{|OBJECT of typedes|} gUpdo (UpdSearch (UpdC cname) 0) (OBJECT o) 		// constructor changed of this type
# (mode,o)					= gUpdo (UpdCreate path) o
= (UpdDone,OBJECT o)
where
	path					= getConsPath (hd [cons \\ cons <- typedes.gtd_conses | cons.gcd_name == cname])
gUpd{|OBJECT|} gUpdo (UpdSearch val cnt) (OBJECT o)							// search further
# (mode,o)					= gUpdo (UpdSearch val (dec cnt)) o
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

// gForm: automatically derives a Html form for any Clean type
mkForm :: !(InIDataId a) *HSt -> *(Form a, !*HSt)	| gForm {|*|} a
mkForm (init,formid) hst = gForm{|*|} (init, formid) hst

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
		  , [{inputid = inputid, formid = formid.id, updateon = if (mode == Edit ) OnChange OnSubmit}]
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
mkButton (init, formid) val hst = ([],[],hst)

mkSelect :: !(InIDataId d) String [(String,String)] !*HSt -> ([HtmlTag],[InputId],*HSt)
mkSelect (init, formid=:{mode}) val options hst =:{cntr}
	# inputid = (formid.id +++ "-" +++ toString cntr)
	= ( [SelectTag	[ NameAttr	inputid
					, IdAttr	inputid
					: if (mode == Display) [DisabledAttr] []
					] [OptionTag [ValueAttr value:if (value == val) [SelectedAttr] [] ] [Text label] \\ (label,value) <- options]]
	  , [{inputid = inputid, formid = formid.id, updateon = (if (mode == Edit) OnChange OnSubmit)}]
	  , setHStCntr (cntr + 1) hst)

// The following two functions are not an example of decent Clean programming, but it works thanks to lazy evaluation...
toHtml :: a -> HtmlTag | gForm {|*|} a
toHtml a
# (na,_)						= mkForm (Set,mkFormId "__toHtml" a <@ Display) (mkHSt http_emptyRequest emptyFormStates dummy)
= BodyTag [] na.form
where
	dummy = { worldC 	= abort "dummy world for toHtml!\n"
			, gerda	 	= abort "dummy gerda for toHtml!\n"
			, datafile	= abort "dummy datafile for toHtml!\n"
			}

toHtmlForm :: !(*HSt -> *(Form a,*HSt)) -> [HtmlTag] | gForm{|*|}, gUpd{|*|}, gPrint{|*|}, gParse{|*|}, TC a
toHtmlForm anyform 
# (na,hst)						= anyform (mkHSt http_emptyRequest emptyFormStates (abort "illegal call to toHtmlForm!\n"))
= na.form
where
	dummy = { worldC 	= abort "dummy world for toHtmlForm!\n"
			, gerda	 	= abort "dummy gerda for toHtmlForm!\n"
			, datafile	= abort "dummy datafile for toHtmlForm!\n"
			}

createDefault :: a | gUpd{|*|} a 
createDefault					= fromJust (snd (gUpd {|*|} (UpdSearch (UpdC "Just") 0) Nothing))

