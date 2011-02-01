definition module PmPath

import StdFile, StdOverloaded//, StdString
import StdPathname

import PmTypes

MakeABCSystemPathname		:: !Pathname			-> Pathname
MakeObjSystemPathname		:: !Processor !Pathname	-> Pathname
