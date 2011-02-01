implementation module PmPath

import StdClass,StdString, StdChar, StdBool, StdChar,StdInt, StdMisc,StdArray;
import StdPathname

import Platform
import PmTypes

/* The name of the system directory */
SystemDir			:== "Clean System Files";

MakeABCSystemPathname :: !Pathname -> Pathname
MakeABCSystemPathname abcname
	= directory_name_plus_system_dir +++ sep +++ file +++ ".abc"
where
		directory_name_plus_system_dir
			| equal_suffix SystemDir dir
				= dir;
			| size dir > 0 && dir.[size dir - 1] == DirSeparator
				= dir +++ SystemDir;
			| otherwise
				= dir +++ sep +++ SystemDir;
		dir		= RemoveFilename abcname;
		sep		= toString DirSeparator;
		file	= RemovePath (RemoveSuffix abcname);

MakeObjSystemPathname :: !Processor !Pathname -> Pathname
MakeObjSystemPathname processor name
	= files_and_path (ProcessorSuffix processor)
/*
	| processor == CurrentProcessor
//		= files_and_path ".o";
		= files_and_path ".xo";
	| processor == MC68000
		= files_and_path ".obj0";
	| processor == MC68020
		= files_and_path ".obj1";
	| processor == MC68020_and_68881
		= files_and_path ".obj2";
		= abort ("MakeObjSystemPathname: " +++  toString processor +++ " : No such processor ");
*/
where
		files_and_path extension = directory_name_plus_system_dir +++ sep +++ file+++extension
		directory_name_plus_system_dir
			| equal_suffix SystemDir dir
				= dir;
			| size dir > 0 && dir.[size dir - 1] == DirSeparator
				= dir +++ SystemDir;
			| otherwise
				= dir +++ sep +++ SystemDir;
		dir		= RemoveFilename name;
		sep		= toString DirSeparator;
		file	= RemovePath (RemoveSuffix name);
