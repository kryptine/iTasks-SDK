implementation module BugReport

import iTasks
import CommonDomain

:: Application =
	{ appNr			:: AppNr
	, name			:: String
	, versions		:: [String]
	, developers	:: [UserId]
	}
	
:: BugReport =
	{ application	:: DBRef Application
	, version		:: Maybe String
	, severity		:: BugSeverity
	, description	:: Note
	}
	
:: Bug =
	{ bugNr			:: BugNr
	, reportedAt	:: (Date,Time)
	, reportedBy	:: UserId
	, report		:: BugReport
	, status		:: BugStatus
	}
	
:: BugAnalysis =
	{ bug				:: DBRef Bug
	, cause				:: Note
	, affectedVersions	:: [String]
	}
	
:: AppNr :== Int
:: BugNr :== Int
:: BugSeverity = Low | Medium | High | Critical
:: BugStatus = Reported | Assigned | Reproduced | Resolved

derive gPrint     Application, BugReport, Bug, BugSeverity, BugStatus, BugAnalysis	
derive gParse	  Application, BugReport, Bug, BugSeverity, BugStatus, BugAnalysis
derive gVisualize Application, BugReport, Bug, BugSeverity, BugStatus, BugAnalysis
derive gUpdate	  Application, BugReport, Bug, BugSeverity, BugStatus, BugAnalysis

instance DB Application where
	databaseId					= mkDBid "Application"
	getItemId {appNr}			= DBRef appNr
	setItemId (DBRef appNr) app	= {app & appNr = appNr}
	
instance DB Bug where
	databaseId					= mkDBid "Bug"
	getItemId bug=:{bugNr}		= DBRef bugNr
	setItemId (DBRef bugNr) bug	= {bug & bugNr = bugNr}

bugReportExample :: [Workflow]
bugReportExample
	= [workflow "Examples/Miscellaneous/Bug report" reportBug]

reportBug :: Task Void
reportBug
	=	enterInitialReport
	>>= \report ->
		fileBugReport report
	>>= \bugnr ->
		case report.severity of
			Critical 
				= selectDeveloper report.application report.version
				>>= \assessor ->
					assessor @: 
					 ("Bug report assessment",
					  requestConfirmationAbout
					  	"Is this bug really critical?" report)
				>>= \confirmed -> if confirmed
						(resolveCriticalBug (DBRef bugnr))
						(resolveBug (DBRef bugnr))
			_ 
				= resolveBug (DBRef bugnr)
where		
	enterInitialReport :: Task BugReport
	enterInitialReport
		= enterInformation "Please describe the bug you have found"
		
	fileBugReport :: BugReport -> Task BugNr
	fileBugReport report
		=	dbCreateItem -&&- getCurrentUser
		>>= \(bug,(uid,name)) ->
			dbUpdateItem {bug & report = report, reportedBy = uid}
		>>| return bug.bugNr 

selectDeveloper :: (DBRef Application) (Maybe String) -> Task UserId
selectDeveloper application version
	=	dbReadItem application
	>>= \mbapp -> case mbapp of
		Nothing
			= getCurrentUser >>= \(uid,name) -> return uid
		Just app
			= selectLeastBusy app.developers
where
	selectLeastBusy :: [UserId] -> Task UserId
	selectLeastBusy []
		=	getCurrentUser >>= \(uid,name) -> return uid
	selectLeastBusy uids
		= 	allTasks [getNumTasksForUser uid \\ uid <- uids]
		>>= \activity -> 
			return (snd (minimum (zip (activity,uids))))
	where	
		minimum l = foldl min (hd l) (tl l) 
		
	getNumTasksForUser :: UserId -> Task Int
	getNumTasksForUser uid = return 42			//TODO: Use API function
	 
resolveBug :: (DBRef Bug) -> Task Void
resolveBug bugnr
	=	dbSafeReadItem bugnr
	>>= \bug ->
		analyzeBug bug
	>>|	developBugFix bugnr
	>>| mergeFixInMainLine bugnr
	>>| notifyReporter

resolveCriticalBug :: (DBRef Bug) -> Task Void
resolveCriticalBug bugnr
	=	dbSafeReadItem bugnr
	>>= \bug ->
		analyzeBug bug
	>>| developBugFix bugnr
	>>| makePatches defaultValue [] -&&- mergeFixInMainLine bugnr
	>>| notifyReporter
	
analyzeBug :: Bug -> Task BugAnalysis
analyzeBug bug
	=	determineCause bug -&&- determineAffectedVersions bug
	>>=	\(cause,versions) ->
		return {bug = getItemId bug, cause = cause, affectedVersions = versions}
where
	determineCause bug
		= enterInformationAbout "What is the cause of the following bug?" bug

	determineAffectedVersions bug
		=	dbSafeReadItem bug.report.application
		>>= \application ->
			enterMultipleChoiceAbout
				("Which versions of " <+++ application.Application.name <+++ " have been affected by this bug?")
				bug
				application.versions
				
		
developBugFix :: (DBRef Bug) -> Task Void
developBugFix bugnr = showMessage "TODO"

mergeFixInMainLine :: (DBRef Bug) -> Task Void
mergeFixInMainLine bugnr = showMessage "TODO"

makePatches :: (DBRef Application) [String] -> Task Void
makePatches application versions = showMessage "TODO"

notifyReporter :: Task Void
notifyReporter = showMessage "TODO"

dbSafeReadItem :: (DBRef a) -> Task a | iTask, DB a
dbSafeReadItem ref
	=	dbReadItem ref
	>>= \mbval = case mbval of
		Just val
			= return val
		Nothing
			=	dbReadAll
			>>= \all ->
				enterChoice ("Item " <+++ ref <+++ " could not be found. Please select an alternative.") all
