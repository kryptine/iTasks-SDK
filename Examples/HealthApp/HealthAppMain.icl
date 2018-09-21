module HealthAppMain

import iTasks

import HealthApp.Definition
import HealthApp.SDS
import HealthApp.Task

workflows = [ workflow "View medication for user" "Medication" viewMedication
			, workflow "Update address for user" "Address" updateAddess
			, workflow "Create new user" "Create user" createClient
			, workflow "Register new medication" "Medication" registerMedication
			, workflow "Create new appointment" "Appointment" createAppointment]

Start :: *World -> *World
Start world = doTasks loginTask world
where
	loginTask = installWorkflows workflows
		>>| loginAndManageWork "Please login to start your work"