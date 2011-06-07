module CEFP

// CEFP 2011
// This iTask applications shows all example workflows defined in the corresponding chapters
// as used for the CEFP 2011 summerschool 

import iTasks

import Section2, Section3, Section4, Section5, Section6, Section7, Section8, Section9

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


