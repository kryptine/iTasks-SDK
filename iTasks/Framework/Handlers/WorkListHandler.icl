implementation module WorkListHandler

import StdEnv
import Http, Session
import Text
import JSON
import Time
import iDataForms
import TaskTree, TaskTreeFilters, InternaliTasksCommon

:: WorkList			= 	{ success		:: Bool
						, total			:: Int
						, worklist		:: [WorkListItem]
						}
						
:: WorkListItem 	= 	{ taskid		:: String 					// Task id of the work item
						, delegator		:: String 					// Id of the user who issued the work
						, processname	:: String					// Name given to the work process the task belongs to
				 		, subject		:: String 					// Name give to the task, which can be a short description of the work to do
				 		, priority		:: TaskPriority				// Priority of the task
				 		, timestamp		:: Int						// Time stamp when the task was issued
				 		, tree_path		:: [Bool]					// Path in the tree structure
				 		, tree_last		:: Bool						// Is this item the last of a set of siblings
				 		, tree_icon		:: String					// An icon name. The actual icon image is defined in the css.
				 													// Current possible values: editTask, orTask, andTask, conditionTask, timeTask, systemTask, finishedTask 
				  		}											// And also: andTaskMU, maybeTask
																	
derive JSONEncode WorkList, WorkListItem, TaskPriority

handleWorkListRequest :: !(LabeledTask a) !Int !HTTPRequest !Session *HSt -> (!HTTPResponse, !*HSt) | iData a
handleWorkListRequest mainTask mainUser request session hst
	# uid							= session.Session.userId
	# (toServer, htmlTree, maybeError, maybeProcessTable, maybeThreadTable,hst)	
									= calculateTaskTree uid False False False mainTask mainUser hst 	// Calculate the TaskTree given the id of the current user
	# workitems						= determineWorkItems uid htmlTree
	# worklist								= { success		= True
											  , total		= length workitems
											  , worklist	= workitems
											  }
	
	= ({http_emptyResponse & rsp_data = toJSON worklist}, hst)


determineWorkItems :: !UserId !HtmlTree -> [WorkListItem]
determineWorkItems uid tree = markLast (determineWorkItems` uid [] defaultDesc tree)
where
	determineWorkItems` uid path pdesc (desc @@: tree)
		# rest	= determineWorkItems` uid path desc tree
		| desc.taskWorkerId == uid
			# newitem =	{ taskid 		= desc.taskNrId
						, delegator		= toString desc.delegatorId
						, processname	= desc.workflowLabel
						, subject		= desc.taskLabel
						, priority		= desc.taskPriority
						, timestamp		= (\(Time i) -> i) desc.timeCreated
						, tree_path		= path
						, tree_last		= False
						, tree_icon		= "editTask"
						}
			= case rest of
				[]
					= [newitem]
				[x:xs]
					| x.taskid == desc.taskNrId	//'Merge' direct subnode when taskid is the same (e.g. a directly nested CondAnd node)
						= [ { x
							& delegator		= toString desc.delegatorId
							, processname	= desc.workflowLabel
							, subject		= desc.taskLabel
							, priority		= desc.taskPriority
							, timestamp		= (\(Time i) -> i) desc.timeCreated
							} :xs]
					| otherwise
						= [newitem,x:xs]
		| otherwise
			= rest

	determineWorkItems` uid path pdesc (CondAnd label nr trees) 
		| pdesc.taskWorkerId == uid
			# subpath = path ++ [False]
			# rest = markLast (flatten [[mkCondItem pdesc desc subpath: determineWorkItems` uid subpath pdesc tree] \\ (desc,tree) <- trees])
			= [	{ taskid 		= pdesc.taskNrId
				, delegator		= toString pdesc.delegatorId
				, processname	= pdesc.workflowLabel
				, subject		= pdesc.workflowLabel
				, priority		= pdesc.taskPriority
				, timestamp		= (\(Time i) -> i) pdesc.timeCreated
				, tree_path		= path
				, tree_last		= True
				, tree_icon		= label
				} : rest]
		| otherwise
			= flatten (map ((determineWorkItems` uid path pdesc) o snd) trees)

	determineWorkItems` uid path pdesc (tree1 +|+ tree2)
		# list1	= determineWorkItems` uid path pdesc tree1
		# list2 = determineWorkItems` uid path pdesc tree2
		= list1 ++ list2
		
	determineWorkItems` uid path pdesc (tree1 +-+ tree2)
		# list1	= determineWorkItems` uid path pdesc tree1
		# list2 = determineWorkItems` uid path pdesc tree2
		= list1 ++ list2	

	determineWorkItems` uid path pdesc (DivCode id tree) 
		= determineWorkItems` uid path pdesc tree
	
	determineWorkItems` uid path pdesc (TaskTrace traceinfo tree)
		= determineWorkItems` uid path pdesc tree
		
	determineWorkItems` uid path pdesc (BT html inputs)
		= []
		
	markLast :: [WorkListItem] -> [WorkListItem]
	markLast [] = []
	markLast items = reverse ((\[x:xs] -> [{x & tree_last = True}:xs]) (reverse items))
	
	mkCondItem :: TaskDescription CondAndDescription [Bool] -> WorkListItem
	mkCondItem pdesc cdesc path
		= 	{ taskid 		= cdesc.caTaskNrId
			, delegator		= toString pdesc.delegatorId
			, processname	= pdesc.workflowLabel
			, subject		= pdesc.taskLabel +++ " - Part " +++ (toString cdesc.caIndex)
			, priority		= pdesc.taskPriority
			, timestamp		= (\(Time i) -> i) pdesc.timeCreated
			, tree_path		= path
			, tree_last		= False
			, tree_icon		= if cdesc.caStatus "finishedTask" "editTask"
			}
	
	defaultDesc :: TaskDescription
	defaultDesc
		=	{ delegatorId	= 0								
			, taskWorkerId	= 0								
			, taskNrId		= ""								
			, processNr		= 0								
			, workflowLabel	= "Non-existing"							
			, taskLabel		= "Non-existing"								
			, timeCreated	= Time 0
			, taskPriority	= LowPriority
			, curStatus		= True
			}