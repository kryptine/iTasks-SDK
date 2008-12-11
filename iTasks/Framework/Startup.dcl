definition module Startup

import iDataSettings, StdBimap
import BasicCombinators

startTaskEngine :: !(Task a) !*World -> *World  	| iData a