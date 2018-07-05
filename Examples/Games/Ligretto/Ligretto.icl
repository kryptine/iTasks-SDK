module Ligretto

/**	This example implements a simplified version of the card game Ligretto.
	When creating a project, include the following paths:
	(i)  {Application}\Examples\iTasks\Games\
	(ii) {Application}\Examples\iTasks\Graphics\

	To run the example playing as two persons, do the following:
	(a) first log in as root / root
	(b) select the 'Manage users' task
	(c) import a user community
	(d) logout
	(e) login as the key player who is going to invite 1, 2, or 3 players
	(f) select the 'Ligretto' task
	(g) select 1, 2, or 3 users to play Ligretto with
	(h) open the newly created task
	(i) in other browser( tab)s, login as the invited player(s) and open the task received from the key player
	(j) have fun
*/

import Ligretto.Tasks
import MultiUser.Tasks

Start :: *World -> *World
Start world
	= StartMultiUserTasks [ workflow "Ligretto" "Play Ligretto" play_Ligretto ] [] world
