module CEFP

import iTasks

import Chapter2, Chapter3, Chapter4, Chapter5, Chapter6, Chapter7

allFlows = 	flows2 ++ 
			flows3 ++
			flows4 ++
			flows5 ++
			flows6 ++
			flows7

Start :: *World -> *World
Start world = startEngine allFlows world

