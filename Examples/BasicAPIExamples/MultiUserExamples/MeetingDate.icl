implementation module BasicAPIExamples.MultiUserExamples.MeetingDate

import iTasks

import iTasks.Extensions.Admin.UserAdmin
import iTasks.Extensions.Admin.WorkflowAdmin
import iTasks.Extensions.DateTime
import iTasks.UI.Editor.Common

wf :: String -> Workflow
wf a = workflow a "Plan a meeting" multiUserExample

Start :: *World -> *World
Start world 
	= startEngine multiUserExample world

multiUserExample
	=				set (map mkUserAccount players) userAccounts
	>>|				viewInformation "Login under one of the following names (password = login name)" [] 
						(foldl (+++) "" (map (\n -> n +++ ", ") players)) 
					-||-
					viewInformation "and then Select \"new\" to create a new Task..." [] ""
	>>|				loginAndManageWorkList "Meeting_4_3 Example" [workflow "Meeting date" "Determine meeting date" myExample] 
where
	mkUserAccount name  
		= { credentials = { username = Username name, password = Password name}, title = Nothing, roles = ["manager"] }


// -------------------------------------------------------------------------
// Enquire what a suitable a meeting date would be

players = ["bob","alice","carol","dave"]

myExample 
	= 					DefineMeetingPurpose
	>>= \purpose ->		SelectDatesToPropose
	>>= \dates ->		SelectAttendencees
	>>= \others -> 		AskOthers purpose others dates

:: DateOption
	 = 	{ date 		:: Date
		, hour 		:: Int
		, minute 	:: Int
		}
:: MeetingOption
	=	{ users		:: [String]
		, date		:: DateOption
		}

derive class iTask DateOption, MeetingOption

DefineMeetingPurpose :: Task String
DefineMeetingPurpose
	=	enterInformation "What is the purpose of the meeting?" []

SelectDatesToPropose :: Task [DateOption]
SelectDatesToPropose
	=	enterInformation "Select the date(s) and time you propose to meet..." []

SelectAttendencees :: Task [User]
SelectAttendencees
	=	enterMultipleChoiceWithShared ("Who do you want to invite for the meeting?") [ChooseFromCheckGroup id] users

AskOthers :: String [User] [DateOption] -> Task MeetingOption 
AskOthers purpose others dates
	=	withShared makeTable askAll
where
	makeTable :: [MeetingOption]
	makeTable 
		= [{users = [], date = date} \\ date <- dates]

	askAll :: (Shared [MeetingOption]) -> Task MeetingOption
	askAll table 
		= 			allTasks[(user, purpose) @: checkOptions (toString user) \\ user <- others]
		>>- \_ ->	enterChoiceWithShared "Select the date for the meeting:" [ChooseFromGrid id] table
		>>=	 		viewInformation "Date chosen:" []
	where
		checkOptions user  
			=				viewSharedInformation "Current Responses:" [] table
							||-
							enterMultipleChoice "Select the date(s) you can attend the meeting (ctrl alt):" [ChooseFromGrid (\i -> dates!!i)] [0..length dates - 1]
			>>=	\ids ->	upd (\table -> [{t & users = if (isMember j ids) [user:t.users] t.users} \\ j <- [0..] & t <- table]) table 



