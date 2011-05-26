module CEFP

// CEFP 2011
// This iTask applications shows all example workflows defined in the corresponding chapters
// as used for the CEFP 2011 summerschool 

import iTasks

import Chapter2, Chapter3, Chapter4, Chapter5, Chapter6, Chapter7, Chapter8, Chapter9, Chapter10

derive bimap (,), Maybe

allFlows = 	flows2 ++ 
			flows3 ++
			flows4 ++
			flows5 ++
			flows6 ++
			flows7 ++
			flows8 ++
			flows9 ++
			flows10

Start :: *World -> *World
Start world = startEngine allFlows world

