definition module Startup

import iDataSettings, StdBimap
import iTasksBasicCombinators

startTaskEngine :: !(Task a) !*World -> *World  	| iData a