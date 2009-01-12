implementation module LiftingCombinators

import EditTasks, BasicCombinators

(*=>) infix 4 :: !(TSt -> (!a,!TSt)) !(a -> Task b) -> (Task b)
(*=>) ftst b = Task doit
where
	doit tst
	# (a,tst) = ftst tst
	= appTaskTSt (b a) tst

(*#>) infix 4 :: !(TSt -> TSt) !(Task a) -> Task a
(*#>) ftst b = Task doit
where
	doit tst
	# tst = ftst tst
	= appTaskTSt b tst

appIData :: !(IDataFun a) -> (Task a) | iData a 
appIData idatafun = Task ( \tst -> appTaskTSt (mkTask "appIData" (Task (appIData` idatafun))) tst)
where
	appIData` idata tst=:{tasknr,html,hst}
	# (idata,hst) 										= idatafun hst
	# (_,{tasknr,activated,html=ahtml,hst}) 			= appTaskTSt (editTaskLabel "appIDataDone" "Done" Void) {tst & activated = True, html = BT [] [],hst = hst}	
	= (idata.Form.value,{tst & tasknr = tasknr,activated = activated, html = html +|+ 
															(if activated (BT idata.form idata.inputs) (BT idata.form idata.inputs +|+ ahtml)), hst = hst})

appIData2 :: !(String *HSt -> *(!Form a,!*HSt)) -> (Task a) | iData a 
appIData2 idatafun = Task (\tst -> appTaskTSt (mkTask "appIData" (Task (appIData` idatafun))) tst)
where
	appIData` idata tst=:{tasknr,html,hst,userId}
	# taskId											= iTaskId userId tasknr "iData"
	# (idata,hst) 										= idatafun taskId hst
	# (_,{tasknr,activated,html=ahtml,hst}) 			= appTaskTSt (editTaskLabel "appIDataDone" "Done" Void) {tst & activated = True, html = BT [] [],hst = hst}	
	= (idata.Form.value,{tst & tasknr = tasknr,activated = activated, html = html +|+ 
															(if activated (BT idata.form idata.inputs) (BT idata.form idata.inputs +|+ ahtml)), hst = hst})

appHStOnce :: !String !(HSt -> (!a,!HSt)) -> (Task a) | iData a
appHStOnce label fun = Once label (Task (liftHst fun))

appHSt :: !String !(HSt -> (!a,!HSt)) -> (Task a) | iData a
appHSt label fun = mkTask label (Task (liftHst fun))

liftHst :: !(*HSt -> *(.a,*HSt)) !*TSt -> *(.a,*TSt)
liftHst fun tst=:{hst}
# (form,hst) = fun hst
= (form,{tst & hst = hst})

appWorldOnce :: !String !(*World -> *(!a,!*World)) -> (Task a) | iData a
appWorldOnce label fun = Once label (Task (liftWorld fun))

appWorld :: !String !(*World -> *(!a,!*World)) -> (Task a) | iData a
appWorld label fun = mkTask label (Task (liftWorld fun))

liftWorld :: !(*World -> *(!a,!*World)) !*TSt -> *(!a,!*TSt)
liftWorld f tst=: {hst = hst=:{nworld = nworld=:{world}}}
# (a,world)	= f world
= (a,{tst & hst = {hst & nworld = {nworld & world = world}}})	
