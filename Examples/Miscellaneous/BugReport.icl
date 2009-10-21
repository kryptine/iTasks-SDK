implementation module BugReport

import iTasks
import CommonDomain

:: BugReport =
	{ application	:: String
	, version		:: Maybe String
	, date			:: Date
	, occursAt		:: BugOccurance
	, severity		:: BugSeverity
	, description	:: Note
	}

:: BugSeverity	= Low | Medium | High | Critical	
:: BugOccurance	= Startup | Shutdown | Other Note

:: Bug =
	{ bugNr			:: BugNr
	, reportedAt	:: (Date,Time)
	, reportedBy	:: UserId
	, report		:: BugReport
	, application	:: AppNr
	, analysis		:: Maybe BugAnalysis
	}

:: BugAnalysis =
	{ cause				:: Note
	, affectedVersions	:: [String]
	}
	
:: Application =
	{ appNr			:: AppNr
	, name			:: String
	, versions		:: [String]
	, developers	:: [UserId]
	}
	
:: AppNr :== Int
:: BugNr :== Int

derive gPrint     Application, BugReport, Bug, BugSeverity, BugOccurance, BugAnalysis	
derive gParse	  Application, BugReport, Bug, BugSeverity, BugOccurance, BugAnalysis
derive gVisualize Application, BugReport, Bug, BugSeverity, BugOccurance, BugAnalysis
derive gUpdate	  Application, BugReport, Bug, BugSeverity, BugOccurance, BugAnalysis

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
			(showMessageAbout "The following bug has been reported, please fix it." report)
	>>| return report

reportBugAdvanced :: Task Void
reportBugAdvanced
	=	enterInitialReport
	>>= \report ->
		fileBugReport report
	>>= \bug ->
		case report.severity of
			Critical 
				=	(confirmCriticalBug bug
					>>= \critical ->
					assignBug bug critical)
			_
				=	assignBug bug False

enterInitialReport :: Task BugReport
enterInitialReport
=	enterInformation "Please describe the bug you have found"
	
fileBugReport :: BugReport -> Task Bug
fileBugReport report
	=	dbCreateItem -&&- getCurrentUser
	>>= \(bug,user) ->
		dbUpdateItem {bug & report = report, reportedBy = user.User.userId}

confirmCriticalBug :: Bug -> Task Bool
confirmCriticalBug bug
	=	selectDeveloper bug.report.BugReport.application bug.report.version
	>>= \assessor ->
		assign assessor HighPriority Nothing
			( "Bug report assessment"
			  @>>
			  requestConfirmationAbout "Is this bug really critical?" bug.report
			)

assignBug :: Bug Bool -> Task Void
assignBug bug critical
	=	selectDeveloper bug.report.BugReport.application bug.report.version
	>>=	\developer ->
		assign developer priority Nothing (subject @>> resolveBug bugid critical)
where
	bugid    = getItemId bug
	priority = if critical HighPriority NormalPriority
	subject  = if critical "Critical bug!" "Bug"
	
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
	 
resolveBug :: (DBRef Bug) Bool -> Task Void
resolveBug bugnr critical
	=	dbSafeReadItem bugnr
	>>= \bug ->
		analyzeBug bug
	>>= \bug ->
		developBugFix bug
	>>| if critical
		( makePatches bug -&&- mergeFixInMainLine bug
		  >>| notifyReporter bug)
		( mergeFixInMainLine bug
		  >>| notifyReporter bug)
	
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
