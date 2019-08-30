implementation module BasicAPIExamples.MultiUserExamples.MeetingDate

import iTasks

import iTasks.Extensions.Admin.UserAdmin
import iTasks.Extensions.Admin.WorkflowAdmin
import iTasks.Extensions.DateTime

import Text

wf :: String -> Workflow
wf a = workflow a "Plan a meeting" myExample

main :: Task ()
main = multiUserExample @! ()

multiUserExample
	=				allTasks (map (createUser o mkUserAccount) players)
	>>|				(Hint "Login under one of the following names (password = login name)" @>> viewInformation []
						(join ", " players))
					-||-
					(Hint "and then Select \"new\" to create a new Task..." @>> viewInformation [] "")
	>>|				installWorkflows [wf "Meeting date"]
	>>|				loginAndManageWork "Meeting_4_3 Example" Nothing Nothing False
where
	mkUserAccount name
		= {UserAccount| credentials = {Credentials| username = Username name, password = Password name}, title = Nothing, roles = ["manager"] }


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
	=	Hint "What is the purpose of the meeting?" @>> enterInformation []

SelectDatesToPropose :: Task [DateOption]
SelectDatesToPropose
	=	Hint "Select the date(s) and time you propose to meet..." @>> enterInformation []

SelectAttendencees :: Task [User]
SelectAttendencees
	=	Hint "Who do you want to invite for the meeting?" @>> enterMultipleChoiceWithShared [ChooseFromCheckGroup id] users

AskOthers :: String [User] [DateOption] -> Task MeetingOption
AskOthers purpose others dates
	=	withShared makeTable askAll
where
	makeTable :: [MeetingOption]
	makeTable
		= [{users = [], date = date} \\ date <- dates]

	askAll table
		=   allTasks[(user, purpose) @: checkOptions (toString user) \\ user <- others]
		>-| (Hint "Select the date for the meeting:" @>> enterChoiceWithShared [ChooseFromGrid id] table)
		>>=	\result -> Hint "Date chosen:" @>> viewInformation [] result
	where
		checkOptions user
			=				(Hint "Current Responses:" @>> viewSharedInformation [] table)
							||-
							(Hint "Select the date(s) you can attend the meeting (ctrl alt):"
								@>> enterMultipleChoice [ChooseFromGrid (\i -> dates!!i)] [0..length dates - 1])

			>>=	\ids ->	upd (\table -> [{t & users = if (isMember j ids) [user:t.users] t.users} \\ j <- [0..] & t <- table]) table



