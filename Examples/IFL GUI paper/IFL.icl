module IFL

import iTasks

import Section2, Section3_1, Section3_2, Section3_3, Section3_4, Section3_5

Start :: *World -> *World
Start world = startEngine workflows world
where
	workflows = [section2, section3_1, section3_2, section3_3, section3_4, section3_5]