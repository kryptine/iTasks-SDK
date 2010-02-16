implementation module PublishSubscribeCombinators

import StoreTasks, Store, InteractionTasks, CoreCombinators, TSt, StdList, TUIDefinition

publishInformation :: question !(DBid a) -> Task (Action,a) | html question & iTask a 
publishInformation question dbid = iterateUntil (enterInformationA "Intial" [] [ActionOk,ActionCancel]) (publishUpd question dbid) finished
where
	publishUpd :: question (DBid a) (Action,a) -> Task (Action,a) | html question & iTask a
	publishUpd question dbid (act,val) = 
		updateInformationA question [] [(ActionIcon "Publish" "task-publish"),ActionFinish] val 
		>>= \(act,val) -> writeDB dbid val
		>>| return (act,val)
		
	finished (ActionCancel,_) = True
	finished (ActionFinish,_) = True
	finished _				= False

subscribe :: message !(DBid a) -> Task a | html message & iTask a
subscribe message dbid = mkInteractiveTask "Subscribe" subscribe`
where
	subscribe` tst=:{dataStore,world}
	# (mbVal,dstore,world) = loadValue dbid dataStore world
	# (val,tst) = case mbVal of
		Just val 
			= (val,{TSt | tst & dataStore = dstore, world = world})
		Nothing
			# (val,world) = defaultValue world
			= (val,{TSt | tst & dataStore = dstore, world = world})
	# (_,tst) = (makeSubscribeTask message (Just (visualizeAsHtmlDisplay val))) tst
	= (val,tst)
	
makeSubscribeTask :: message (Maybe [HtmlTag]) *TSt -> (!Action,!*TSt) | html message
makeSubscribeTask message context tst=:{taskNr}
	# taskId	= taskNrToString taskNr
	# editorId	= "tf-" +++ taskId
	# (updates,tst) = getUserUpdates tst
	| isEmpty updates
		# tst = setTUIDef (taskPanel taskId (html message) context Nothing (makeButtons editorId [ActionNext] [] True)) tst
		= (ActionCancel,{tst & activated = False})
	| otherwise
		= (ActionNext,{tst & activated = True})
