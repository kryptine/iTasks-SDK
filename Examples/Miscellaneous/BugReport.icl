implementation module BugReport

import iTasks
import CommonDomain

:: BugReport =
	{ application	:: String
	, version		:: Maybe String
	, occursAt		:: BugOccurance
	, severity		:: BugSeverity
	, description	:: Note
	}

:: BugSeverity	= Low | Medium | High | Critical	
:: BugOccurance	= Startup | Shutdown | Other Note


:: Application =
	{ appNr			:: AppNr
	, name			:: String
	, versions		:: [String]
	, developers	:: [UserId]
	}
	
:: Bug =
	{ bugNr			:: BugNr
	, reportedAt	:: (Date,Time)
	, reportedBy	:: UserId
	, report		:: BugReport
	, application	:: AppNr
	, analysis		:: Maybe BugAnalysis
	, status		:: BugStatus
	}
	
:: BugAnalysis =
	{ cause				:: Note
	, affectedVersions	:: [String]
	}
	
:: AppNr :== Int
:: BugNr :== Int

:: BugStatus = Reported | Assigned | Reproduced | Resolved

derive gPrint     Application, BugReport, Bug, BugSeverity, BugOccurance, BugStatus, BugAnalysis	
derive gParse	  Application, BugReport, Bug, BugSeverity, BugOccurance, BugStatus, BugAnalysis
derive gVisualize Application, BugReport, Bug, BugSeverity, BugOccurance, BugStatus, BugAnalysis
derive gUpdate	  Application, BugReport, Bug, BugSeverity, BugOccurance, BugStatus, BugAnalysis

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
	= [ workflow "Examples/Miscellaneous/Bug report (simple)" reportBugSimple
	  , workflow "Examples/Miscellaneous/Bug report (advanced)" reportBugAdvanced
	  ]
	  
reportBugSimple :: Task BugReport
reportBugSimple
	=	enterInformation "Please describe the bug you have found"
	>>=	\report ->
		assignByName "bas" "Bug fix" NormalPriority Nothing
			(showMessageAbout "The following bug has been reported" report)
	>>| return report

reportBugAdvanced :: Task Void
reportBugAdvanced
	=	enterInitialReport
	>>= \report ->
		fileBugReport report
	>>= \bugnr ->
		case report.severity of
			Critical 
				= selectDeveloper report.BugReport.application report.version
				>>= \assessor ->
					assessor @: 
					 ("Bug report assessment",
					  requestConfirmationAbout
					  	"Is this bug really critical?" report)
				>>= \confirmed ->
					selectDeveloper report.BugReport.application report.version
				>>= \developer -> if confirmed
						(assign developer HighPriority Nothing (resolveCriticalBug (DBRef bugnr) <<@ ("Critical bug resolvement " <+++ bugnr) ))
						(assign developer NormalPriority Nothing (resolveBug (DBRef bugnr) <<@ ("Bug resolvement " <+++ bugnr)))
			_ 
				=	selectDeveloper report.BugReport.application report.version 
				>>= \developer ->
					assign developer NormalPriority Nothing (resolveBug (DBRef bugnr) <<@ ("Bug resolvement " <+++ bugnr))
where		
	enterInitialReport :: Task BugReport
	enterInitialReport
		= enterInformation "Please describe the bug you have found"
		
	fileBugReport :: BugReport -> Task BugNr
	fileBugReport report
		=	dbCreateItem -&&- getCurrentUser
		>>= \(bug,user) ->
			dbUpdateItem {bug & report = report, reportedBy = user.User.userId}
		>>| return bug.bugNr 

selectDeveloper :: String (Maybe String) -> Task UserId
selectDeveloper application version
	=	findAppDevelopers application
	>>= \developers -> case developers of
		[]	= getCurrentUser >>= \user -> return user.User.userId
		_	= selectLeastBusy developers
where
	findAppDevelopers :: String -> Task [UserId]
	findAppDevelopers name
		=	dbReadAll
		>>= \apps -> case [app \\ app <- apps |app.Application.name == name] of
			[x] = return x.developers
			_	= return []
			
	selectLeastBusy :: [UserId] -> Task UserId
	selectLeastBusy []
		=	getCurrentUser >>= \user -> return user.User.userId
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
	>>= \bug ->
		developBugFix bug
	>>| mergeFixInMainLine bug
	>>| notifyReporter bug

resolveCriticalBug :: (DBRef Bug) -> Task Void
resolveCriticalBug bugnr
	=	dbSafeReadItem bugnr
	>>= \bug ->
		analyzeBug bug
	>>= \bug ->
		developBugFix bug
	>>| makePatches bug -&&- mergeFixInMainLine bug
	>>| notifyReporter bug
	
analyzeBug :: Bug -> Task Bug
analyzeBug bug
	=	determineCause bug -&&- determineAffectedVersions bug
	>>=	\(cause,versions) ->
		dbUpdateItem {bug & analysis = Just {cause = cause, affectedVersions = versions}}
where
	determineCause bug
		= enterInformationAbout "What is the cause of the following bug?" bug

	determineAffectedVersions bug
		=	dbSafeReadItem (DBRef bug.Bug.application)
		>>= \application ->
			case application.versions of
				[]	= return []
				_	=
					enterMultipleChoiceAbout
						("Which versions of " <+++ application.Application.name <+++ " have been affected by this bug?")
						bug
						application.versions
						
		
developBugFix :: Bug -> Task Void
developBugFix bug = showMessageAbout "Please implement a fix for the following bug:" bug

mergeFixInMainLine :: Bug -> Task Void
mergeFixInMainLine bug = showMessageAbout "Please merge the bugfix in the main line of version control" bug

makePatches :: Bug -> Task Void
makePatches bug =
	case bug.analysis of
		Nothing
			= return Void
		Just {affectedVersions = []}
			= return Void
		Just {affectedVersions = versions}
			= allTasks [showMessageAbout ("Please make a patch of bugfix " <+++ bug.bugNr <+++
								" for the following version of " <+++ bug.Bug.application)
								version
						\\ version <- versions
					   ]
			  >>| return Void
		
notifyReporter :: Bug -> Task Void
notifyReporter bug = notifyUser "The bug you reported has been fixed" bug.reportedBy

//UTIL:

dbSafeReadItem :: (DBRef a) -> Task a | iTask, DB a
dbSafeReadItem ref
	=	dbReadItem ref
	>>= \mbval = case mbval of
		Just val
			= return val
		Nothing
			=	dbReadAll
			>>= \all ->	
				case all of
					[]	= dbCreateItem
					all	= enterChoice ("Item " <+++ ref <+++ " could not be found. Please select an alternative.") all
