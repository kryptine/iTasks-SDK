implementation module Task.Extensions

import iTasks
import qualified Data.Map as DM
import iTasks.UI.Definition
import iTasks.Extensions.DateTime

crudWith :: !d ![ChoiceOption r] [EnterOption r] [ViewOption r] [UpdateOption r r]
            !((f r) -> [r]) !(r (f r) -> f` w) !(r (f r) -> f` w)
            (sds () (f r) (f` w))
         -> Task () | toPrompt d & iTask r & iTask (f r) & iTask w & iTask (f` w) & RWShared sds
crudWith descr choiceOpts enterOpts viewOpts updateOpts toList putItem delItem sh = forever crud
where
  crud
    =  ( enterChoiceWithShared descr [ChooseFromGrid id:choiceOpts] (mapRead toList sh)
 	>&^ viewSharedInformation (Title "Selected") []) <<@ ApplyLayout (arrangeWithSideBar 1 RightSide 350 True)
    >>* [ OnAction (Action "New")    (always   newItem)
        , OnAction (Action "Edit")   (hasValue editItem)
        , OnAction (Action "Delete") (hasValue deleteItem)
        ]
  newItem
    =   enterInformation (Title "New item") enterOpts
    >>* [OnAction ActionOk (hasValue (\item -> upd (putItem item) sh @! ()))
		,OnAction ActionCancel (always (return ()))
		]

  editItem x
    =            updateInformation (Title "Edit item") updateOpts x
    >>* [OnAction ActionOk (hasValue (\item -> upd (delItem x) sh >>| upd (putItem item) sh @! ()))
		,OnAction ActionCancel (always (return ()))
		]
  deleteItem x
    =            upd (delItem x) sh
	@! ()

crud` :: !d !((f r) -> [r]) !(r (f r) -> f` w)  !(r (f r) -> f` w)
        (sds () (f r) (f` w))
     -> Task () | toPrompt d & iTask r & iTask (f r) & iTask w & iTask (f` w) & RWShared sds
crud` descr toList putItem delItem sh = crudWith descr [] [] [] [] toList putItem delItem sh

editStore :: String (Shared sds [a]) -> Task () | iTask a & Eq a & Ord a & RWShared sds
editStore prompt store
	= crud` (Title prompt) id (\item items -> sort [item:items]) (\item items -> removeMember item items) store

addToStore :: [a] !(Shared sds [a]) -> Task () | iTask a & RWShared sds
addToStore new store
	= upd (\content -> content ++ new) store @! ()

appendTitledTopLevelTask :: String (Task a) -> Task TaskId | iTask a
appendTitledTopLevelTask title task
	= get currentUser -&&- get currentDateTime
	>>- \(user,now) -> appendTopLevelTask ('DM'.fromList [ ("title", title)
                                          , ("createdBy",  toString (toUserConstraint user))
                                          , ("createdAt",  toString now)
                                          , ("createdFor", toString (toUserConstraint user))
                                          , ("priority",   toString 5):userAttr user]) False task
where
	userAttr (AuthenticatedUser uid _ _) = [("user", uid)]
    userAttr _                           = []

startTopLevelOnce :: (Task a) Action String (Task b) -> Task () | iTask a & iTask b
startTopLevelOnce viewTask action title flowTask
	=  	get taskInstancesForCurrentUser
	>>- \tasks -> case tasks of
		[{TaskInstance|instanceNo}:_]
			= workOn (TaskId instanceNo 0) <<@ ApplyLayout (setUIAttributes (sizeAttr FlexSize FlexSize))
			@! ()
		_   = 	viewTask
			>>* [OnAction action (always (
						appendTitledTopLevelTask title flowTask
					>>- \taskId -> (workOn taskId <<@ ApplyLayout (setUIAttributes (sizeAttr FlexSize FlexSize)))
					@! ()
				))]

maybeCancel :: String (Task a) -> Task (Maybe a) | iTask a
maybeCancel panic task
	= 	task >>* [OnValue (ifStable (return o Just))
	             ,OnAction (Action panic) (always (return Nothing))]

deadline :: Date (Task a) -> Task (Maybe a) | iTask a
deadline date task
	=	(task >>- return o Just)
		-||-
		(waitForDate date >>| return Nothing)

deadlineWith :: Date a (Task a) -> Task a | iTask a
deadlineWith date value task
	=	task
		-||-
		(waitForDate date >>| return value)
