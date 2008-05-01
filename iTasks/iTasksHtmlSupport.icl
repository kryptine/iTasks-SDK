implementation module iTasksHtmlSupport

// *********************************************************************************************************************************
// This module contains some iTask combinators for html prompting
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import StdList, StdFunc
import iDataTrivial, iDataFormlib, iDataStylelib
import iTasksHandler, InternaliTasksCommon

/*
Prompting variants:
(?>>)			:: prompt as long as task is active but not finished
(!>>)			:: prompt when task is activated
(<<?)			:: as ?>>, except that prompt is displayed *after* task
(<<!)			:: as !>>, except that prompt is displayed *after* task

addHtml			:: add html code
*/

(?>>) infixr 5 	:: !HtmlCode !(Task a) 					-> Task a		| iCreate a
(?>>) prompt task = doTask
where
	doTask tst=:{html=ohtml,activated}
	| not activated						= (createDefault,tst)
	# (a,tst=:{activated,html=nhtml}) 	= task {tst & html = BT []}
	| activated 						= (a,{tst & html = ohtml})
	= (a,{tst & html = ohtml +|+ BT prompt +|+ nhtml})

(<<?) infixl 5 	:: !(Task a) !HtmlCode 					-> Task a		| iCreate a
(<<?) task prompt = doTask
where
	doTask tst=:{html=ohtml,activated}
	| not activated						= (createDefault,tst)
	# (a,tst=:{activated,html=nhtml}) 	= task {tst & html = BT []}
	| activated 						= (a,{tst & html = ohtml})
	= (a,{tst & html = ohtml +|+ nhtml +|+ BT prompt})

(!>>) infixr 5 :: !HtmlCode !(Task a) -> (Task a) | iCreate a
(!>>) prompt task = doTask
where
	doTask tst=:{html=ohtml,activated=myturn}
	| not myturn			= (createDefault,tst)
	# (a,tst=:{html=nhtml}) = task {tst & html = BT []}
	= (a,{tst & html = ohtml +|+ BT prompt +|+ nhtml})

(<<!) infixl 5 :: !(Task a) !HtmlCode -> (Task a) | iCreate a
(<<!) task prompt = doTask
where
	doTask tst=:{html=ohtml,activated=myturn}
	| not myturn			= (createDefault,tst)
	# (a,tst=:{html=nhtml}) = task {tst & html = BT []}
	= (a,{tst & html = ohtml +|+ nhtml +|+ BT prompt})


addHtml :: HtmlCode TSt -> TSt
addHtml bodytag  tst=:{activated, html}  
| not activated = tst						// not active, return default value
= {tst & html = html +|+ BT bodytag}		// active, so perform task or get its result


iTaskButton :: String -> Button
iTaskButton label = LButton defpixel label

mkTaskButtons :: !Bool !String !Int !TaskNr !Options ![String] *HSt -> ((Int,HtmlCode,String),*HSt)
mkTaskButtons vertical myid userId tasknr info btnnames hst
# btnsId			= iTaskId userId tasknr (myid <+++ "genBtns")
# myidx				= length btnnames
//| myidx == 1		= ((0,[],[]),hst)													// no task button if there is only one task to choose from
# (chosen,hst)		= SelectStore (myid,myidx) tasknr info id hst						// which choice was made in the past
# (buttons,hst)		= SelectButtons Init btnsId info (chosen,btnnames) hst				// create buttons
# (chosen,hst)		= SelectStore (myid,myidx) tasknr info  buttons.value hst			// maybe a new button was pressed
# (buttons,hst)		= SelectButtons Set btnsId info (chosen,btnnames) hst				// adjust look of that button
= ((chosen,buttons.form,btnnames!!chosen),hst)
where
	SelectButtons init id info (idx,btnnames) hst 
		= if vertical
			(TableFuncBut2 (init,pageFormId info id [[(mode idx n, but txt,\_ -> n)] \\ txt <- btnnames & n <- [0..]]) hst)
			(TableFuncBut2 (init,pageFormId info id [[(mode idx n, but txt,\_ -> n) \\ txt <- btnnames & n <- [0..]]]) hst)
	but i = iTaskButton i

	mode i j
	| i==j = Display
	= Edit

	SelectStore :: !(String,Int) !TaskNr !Options (Int -> Int) *HSt -> (Int,*HSt)
	SelectStore (myid,idx) tasknr info fun hst 
	# storeId 			= iTaskId userId tasknr (myid <+++ "BtnsS" <+++ idx)
	# (storeform,hst)	= mkStoreForm (Init,storageFormId info storeId 0) fun hst
	= (storeform.value,hst)
