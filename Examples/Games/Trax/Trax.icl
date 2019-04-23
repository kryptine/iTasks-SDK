module Trax

/**	This example implements the two-person tile game Trax.
	When creating a project, include the following paths:

	{Application}\Examples\iTasks\Games\

	To run the example playing as two persons, do the following:
	(a) first log in as root / root
	(b) select the 'Manage users' task
	(c) import a user community
	(d) logout
	(e) login as the key player who is going to invite another player
	(f) select the 'Trax' task
	(g) select a user to play Trax with
	(h) open the newly created task
	(i) in another browser( tab), login as the invited player and open the task received from the key player
	(j) have fun
*/

import Trax.UoD
import Trax.Tasks
import MultiUser.Tasks
import iTasks.Engine

Start :: *World -> *World
Start world
	= startMultiUserTasks [ workflow "Trax" "Play Trax" play_trax ] [] world
