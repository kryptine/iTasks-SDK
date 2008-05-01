implementation module iTasksLiftingCombinators

// *********************************************************************************************************************************
// Some iTasks combinators for lifting other domains to the iTask domain:
// *********************************************************************************************************************************
// iTask & iData Concept and Implementation: (c) 2006,2007,2008 - Rinus Plasmeijer
// *********************************************************************************************************************************
//
import iTasksHandler, iTasksEditors, iTasksBasicCombinators

(*=>) infix 4 :: !(TSt -> (!a,!TSt)) !(a -> Task b) -> (Task b)
(*=>) ftst b = doit
where
	doit tst
	# (a,tst) = ftst tst
	= b a tst

(*#>) infix 4 :: !(TSt -> TSt) !(Task a) -> Task a
(*#>) ftst b = doit
where
	doit tst
	# tst = ftst tst
	= b tst

appIData :: !(IDataFun a) -> (Task a) | iData a 
appIData idatafun = \tst -> mkTask "appIData" (appIData` idatafun) tst
where
	appIData` idata tst=:{tasknr,html,hst}
	# (idata,hst) 										= idatafun hst
	# (_,{tasknr,activated,html=ahtml,hst}) 			= editTaskLabel "appIDataDone" "Done" Void {tst & activated = True, html = BT [],hst = hst}	
	= (idata.value,{tst & tasknr = tasknr,activated = activated, html = html +|+ 
															(if activated (BT idata.form) (BT idata.form +|+ ahtml)), hst = hst})

appIData2 :: !(!String !*HSt -> *(!Form a,!*HSt)) -> (Task a) | iData a 
appIData2 idatafun = \tst -> mkTask "appIData" (appIData` idatafun) tst
where
	appIData` idata tst=:{tasknr,html,hst,userId}
	# taskId											= iTaskId userId tasknr "iData"
	# (idata,hst) 										= idatafun taskId hst
	# (_,{tasknr,activated,html=ahtml,hst}) 			= editTaskLabel "appIDataDone" "Done" Void {tst & activated = True, html = BT [],hst = hst}	
	= (idata.value,{tst & tasknr = tasknr,activated = activated, html = html +|+ 
															(if activated (BT idata.form) (BT idata.form +|+ ahtml)), hst = hst})

appHStOnce :: !String !(HSt -> (!a,!HSt)) -> (Task a) | iData a
appHStOnce label fun = Once label (liftHst fun)

appHSt :: !String !(!HSt -> (!a,!HSt)) -> (Task a) | iData a
appHSt label fun = mkTask label (liftHst fun)

liftHst :: !(*HSt -> *(.a,*HSt)) !*TSt -> *(.a,*TSt)
liftHst fun tst=:{hst}
# (form,hst) = fun hst
= (form,{tst & hst = hst})

appWorldOnce :: !String !(!*World -> *(!a,!*World)) -> (Task a) | iData a
appWorldOnce label fun = Once label (liftWorld fun)

appWorld :: !String !(*World -> *(!a,!*World)) -> (Task a) | iData a
appWorld label fun = mkTask label (liftWorld fun)

liftWorld :: !(*World -> !*(!a,!*World)) !*TSt -> !*(!a,!*TSt)
liftWorld fun tst=: {hst = hst=:{world = world=:{worldC}}}
# (fvalue,theWorld)	= fun worldC
= (fvalue,{tst & hst = {hst & world = {world & worldC = theWorld}}})	







