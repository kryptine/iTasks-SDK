implementation module GinCleanParser

import StdEnv
import Text

import Error

from general import :: Optional(..)
from checksupport import ::Heaps

from Heap		import	::Heap, newHeap
from hashtable	import	::HashTable, newHashTable, set_hte_mark
from predef		import	init_identifiers

import syntax
import scanner
import parse

parseModule :: !String !Bool *File -> ([ParsedDefinition], *File)
parseModule input iclmodule error 
# hash_table = newHashTable newHeap
# scanState = stringScanner input
# (ok, moduleKind, moduleName, scanState) = try_module_header iclmodule scanState
| not ok = ([], error)
# hash_table = set_hte_mark (if iclmodule 1 0) hash_table
# scanState = setUseLayout True scanState
# (_, scanState) = nextToken GeneralContext scanState
# parseContext = SetGlobalContext iclmodule
# parseState =
	{ ps_scanState = scanState
	, ps_error = { pea_file = error, pea_ok = True }
	, ps_flags = PS_SupportGenericsMask
	, ps_hash_table = hash_table
	}
# (defs,parseState) = wantDefinitions parseContext parseState
= (defs, parseState.ps_error.pea_file)

try_module_header :: !Bool !ScanState -> (!Bool,!ModuleKind,!String,!ScanState)
try_module_header is_icl_mod scanState
	# (token, scanState) = nextToken GeneralContext scanState
	| is_icl_mod && token == ImpModuleToken 
			= try_module_token MK_Module scanState
	| token == DefModuleToken
	  	= try_module_token MK_Module scanState
	= abort (toString token)

try_module_token :: !ModuleKind !ScanState -> (!Bool,!ModuleKind,!String,!ScanState)
try_module_token mod_type scanState
	# (token, scanState) = nextToken GeneralContext scanState
	| token == ModuleToken
		# (token, scanState) = nextToken GeneralContext scanState
		= try_module_name token mod_type scanState
		= (False, mod_type, "", tokenBack scanState)

try_module_name :: Token !ModuleKind !ScanState -> (Bool, !ModuleKind, !String, !ScanState)
try_module_name (IdentToken name) mod_type scanState
	= (True, mod_type, name, scanState)
try_module_name (UnderscoreIdentToken name) mod_type scanState
	= (True, mod_type, name, setUseUnderscoreIdents True scanState)
try_module_name token mod_type scanState
	= (False, mod_type, "", tokenBack scanState)

stringScanner :: !String -> ScanState
stringScanner input
	# lines			= split "\n" input   //TODO: dont use Text.split
	# inputStream	= foldr (\a b -> OldLine 0 (a +++ "\n") b) EmptyStream lines
	= ScanState	{	ss_input 		= Input
											{ inp_stream		= inputStream
								 			, inp_filename		= ""
								 			, inp_pos			= {fp_line = 1, fp_col = 0}
											, inp_tabsize		= 4
											}
					,	ss_offsides		=	[(1,False)] // to generate offsides between global definitions
					,	ss_scanOptions	=	0
					,	ss_tokenBuffer	=	Buffer0
					}

