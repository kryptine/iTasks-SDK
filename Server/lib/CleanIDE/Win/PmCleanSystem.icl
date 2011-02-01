implementation module PmCleanSystem

import StdEnv
import Directory

import ostoolbox 
import clCCall_12

import PmCompilerOptions
import PmPath
import PmTypes

import Platform
import StdPathname
import thread_message
import UtilIO
import UtilStrictLists

import linkargs
import WriteOptionsFile

maketempdir :: Pathname -> Pathname
maketempdir startupdir = startupdir +++ "\\Temp"

:: CompilerProcessHandlesAndId = {
	compiler_thread_id :: !Int,
	compiler_thread_handle :: !Int,
	compiler_process_handle :: !Int
   }

:: CompilerProcessIds :== [CompilerProcessHandlesAndId]

NoCompilerProcessIds :: CompilerProcessIds
NoCompilerProcessIds = []

instance == CompileOrCheckSyntax
where
//	(==) :: !CompileOrCheckSyntax !CompileOrCheckSyntax -> Bool
	(==) SyntaxCheck SyntaxCheck
		=	True
	(==) Compilation Compilation
		=	True
	(==) _ _
		=	False
		
ExitCleanCompiler :: !(!CompilingInfo,*env) -> (!CompilingInfo,*env)
ExitCleanCompiler prog=:(CompilingInfo (CompilerProcess compiler_thread_id compiler_thread_handle compiler_process_handle), ps)
	# wm_number=get_message_number;
	# r=send_string_to_thread compiler_thread_id compiler_process_handle wm_number ("exit\0")
	| /*trace_tn ("ExitCleanCompiler "+++toString r+++"\n") &&*/ r==0
		= prog
		= (CompilingInfo NoCompiler,ps);
ExitCleanCompiler prog
	= prog

instance == CodeGenerateAsmOrCode
where
//	(==) :: !CodeGenerateAsmOrCode !CodeGenerateAsmOrCode -> Bool
	(==) AsmGeneration AsmGeneration
		=	True
	(==) CodeGeneration CodeGeneration
		=	True
	(==) _ _
		=	False
		
instance == CompilerMsg
where
	(==) CompilerOK CompilerOK = True
	(==) SyntaxError SyntaxError = True
	(==) GlobalError GlobalError = True
	(==) _ _ = False
	
mangleCompiler2 ccstring` startupdir
	# (ccstring`,rem)			= splitOptions ccstring`
	# (opts,opts`)				= splitOptions rem
	# (shortOK,ccstring)		= GetShortPathName (startupdir +++ "\\" +++ ccstring` +++ "\0")
	| not shortOK
		# line				= "Error: Unable to get short path name '" +++ (startupdir +++ "\\" +++ ccstring`) +++ "'."
		= (False,line,"","","")
	# cocl = ccstring % (0, size ccstring - 2)
	# cocldir = RemoveFilename cocl
	= (True,cocl,cocldir,opts,opts`)

CompilePersistent ::
	!String !Bool !(WindowFun *env) !(WindowFun *env) !CompileOrCheckSyntax !Pathname
	!(List Pathname) !Bool !Bool !Bool !CompilerOptions !Pathname !CompilingInfo !*env
	-> (!CompilingInfo,!(!*env, !Pathname, !CompilerMsg))
	| FileEnv env
CompilePersistent
	cocl` write_module_times errwin typewin compileOrCheckSyntax path paths projectHeapProfiling
	projectTimeProfiling projectEagerOrDynamic co=:{CompilerOptions | listTypes}
	startupdir cstate env

	# tooltempdir = maketempdir startupdir
	# (cocl_ok,cocl,cocl_dir,cocl_startup,options)	= mangleCompiler2 cocl` startupdir	// platform dependant mangling...
	| not cocl_ok
		# env					= errwin [cocl] env
		= (cstate,(env,"",GlobalError))

	#	out_file_name		=  out_file_path tooltempdir dummy_slot
		errors_file_name	=  errors_file_path tooltempdir dummy_slot
	# cocl_arguments
		= options +++ write_module_times_string +++. CompileBuildCommand out_file_name errors_file_name compileOrCheckSyntax path paths
			projectHeapProfiling projectTimeProfiling projectEagerOrDynamic co
//	# cstate = NotCompiling
//	# cocl = startupdir+++toString dirseparator +++"cocl.exe";
	# (compile_ok,exitcode,(cstate,env)) = compile_with_cache cocl cocl_dir cocl_startup cocl_arguments (cstate,env);
	| not compile_ok
  		# env = errwin ["Error: Unable to run compiler: "+++cocl +++ " :"+++toString exitcode] env
  		= (cstate,(env,"",GlobalError))
	# (path,mess,env) = CompileHandleExitCode exitcode cocl tooltempdir dummy_slot  errwin typewin path listTypes env
	=	(cstate,(env,path,mess))
where
	dummy_slot = 0
	write_module_times_string = if write_module_times " -wmt " " "

CompileBuildCommand :: !String !String !CompileOrCheckSyntax !Pathname !(List Pathname) !Bool !Bool !Bool
				!CompilerOptions -> String
CompileBuildCommand out_file_name errors_file_name compileOrCheckSyntax path paths
					projectHeapProfiling projectTimeProfiling projectEagerOrDynamic co
	= MakeCompilerOptionsString
		compileOrCheckSyntax
		projectHeapProfiling
		projectTimeProfiling
		projectEagerOrDynamic
		co
		+++ (quoted_string path)
        +++ " -P " +++ quoted_string (ConcatenatePath paths)
		+++ " -RE "+++ quoted_string errors_file_name
		+++ " -RO "+++ quoted_string out_file_name;
		
CompileHandleExitCode :: !Int !String !String !Int !(WindowFun *env) !(WindowFun *env) !Pathname
				!ListTypes !*env -> (!Pathname,!CompilerMsg,!*env) | FileEnv env
CompileHandleExitCode exitcode cocl startupdir slot errwin typewin path
					listTypes ps
	# tooltempdir = startupdir
	#	out_file_name		=  out_file_path tooltempdir slot
		errors_file_name	=  errors_file_path tooltempdir slot
		((errors,errors_and_messages_not_empty,errors_and_messages),ps)
			= accFiles (ReadErrorsAndWarnings errors_file_name) ps
	| exitcode <> 0 && not errors_and_messages_not_empty =	// werkt dit ook voor persistent versie?
		( ""
		, GlobalError
		, errwin (	[  "Error: Compiler crashed: "+++cocl
					: if (errors == CompilerOK) ["Unable to open Errors file"] []
					]) ps
		)
	#	abcpath			= MakeABCSystemPathname path
		ps				= (if errors_and_messages_not_empty (errwin (StrictListToList errors_and_messages)) id) ps
		errors			= case exitcode of
							0	-> CompilerOK
							_	-> errors
     = (abcpath,errors,ps)

out_file_path :: String Int -> String
out_file_path tooltempdir slot
	=	file_path tooltempdir "out" slot

errors_file_path :: String Int -> String
errors_file_path tooltempdir slot
	=	file_path tooltempdir "errors" slot
	
file_path :: String String Int -> String
file_path dir base_name slot
	=	dir +++ DirSeparatorString +++ base_name +++ (if (slot == 0) "" (toString slot))

ConcatenatePath :: (List Pathname) -> String
/* old version
ConcatenatePath Nil             = ""
ConcatenatePath (path :! rest ) = path +++ ";" +++ ConcatenatePath rest
*/
ConcatenatePath ss
	# s = createArray (sSize ss) ';'
	= sUpdate 0 s ss
where
	sSize Nil = 0
	sSize (string :! Nil) = size string
	sSize (string :! rest) = size string + 1 + sSize rest
	
	sUpdate i s Nil = s
	sUpdate i s (string :! Nil)
		# (_,s) = sU (size string) i 0 s string
		= s
	sUpdate i s (string :! rest)
		# (i,s) = sU (size string) i 0 s string
		# i = inc i
		= sUpdate i s rest
	
	sU l i j s h
		| j >= l = (i,s)
		# s = update s i h.[j]
		= sU l (inc i) (inc j) s h

CodeGen	::	!String !(WindowFun *env) !CodeGenerateAsmOrCode !Pathname !Bool
			!CodeGenOptions !Processor !ApplicationOptions !Pathname !*env
			-> (!Pathname,!Bool,!*env) | FileEnv env
CodeGen cgen` wf genAsmOrCode path timeprofile cgo tp ao startupdir ps
	# tooltempdir = maketempdir startupdir
	# (cgen_ok,cgen,cgendir)		= mangleGenerator cgen` startupdir
	| not cgen_ok
		# ps				= wf [cgen] ps
		= ("",False,ps)

	#	objpath				= MakeObjSystemPathname tp path
		path_without_suffix	= RemoveSuffix path
		command				= cgen +++ MakeCodeGenOptionsString genAsmOrCode timeprofile cgo tp
//								+++ " " +++ (watcom_quoted_string path_without_suffix)
								+++ " " +++ (quoted_string path_without_suffix)

  		errorsfilename		= tooltempdir +++ DirSeparatorString +++ "errors"
		(didit,exit_code,_)	= CallProcess command [] cgendir "" "" errorsfilename 99
	| not didit
		= (objpath,False,wf [  "Error: Unable to run code generator: "+++cgen
//							, command
//							, startupdir
							] ps
		)
	#	((_, errors_not_empty, error_text),ps)	= accFiles (ReadErrorsAndWarnings errorsfilename) ps
		ps										= (if errors_not_empty 
													(wf (StrictListToList error_text)) 
													( if (exit_code <> 0)
														(wf ["Error: Code generator failed for '" +++ path +++ "' with exit code: "+++toString exit_code,(quoted_string path_without_suffix)])
														id
													)
												  ) ps
	=  (objpath,exit_code==0,ps)

:: StartedCodeGenerator = !{
	scg_thread_handle :: !Int,
	scg_std_error_handle :: !Int,
	scg_abc_path :: !{#Char},
	scg_path_without_suffix :: !{#Char},
	scg_errors_file_name :: !{#Char}
  }

start_code_generator ::	!String !(WindowFun *env) !Pathname !Int !Bool !CodeGenOptions !Processor !Pathname !*env
						-> (!Bool,!Int/*HANDLE*/,!StartedCodeGenerator,!*env) | FileEnv env
start_code_generator cgen` wf abc_path slot timeprofile cgo tp startupdir ps
	# tooltempdir = maketempdir startupdir
	# (cgen_ok,cgen,cgendir) = mangleGenerator cgen` startupdir
	| not cgen_ok
		# ps = wf [cgen] ps
		# scg = {scg_thread_handle=0,scg_std_error_handle=0,scg_abc_path="",scg_path_without_suffix="",scg_errors_file_name=""}
		= (False,0,scg,ps)
	# path_without_suffix = RemoveSuffix abc_path
	  command = cgen +++ MakeCodeGenOptionsString CodeGeneration timeprofile cgo tp +++ " " +++ (quoted_string path_without_suffix)
	  errors_file_name = errors_file_path tooltempdir slot
	  (didit,process_handle,thread_handle,std_error_handle,_) = start_process_with_redirected_std_error command cgendir errors_file_name 99
	| not didit
		# scg = {scg_thread_handle=0,scg_std_error_handle=0,scg_abc_path="",scg_path_without_suffix="",scg_errors_file_name=""}
		= (False,0,scg,wf ["Error: Unable to run code generator: "+++cgen] ps)
		# scg = {scg_thread_handle=thread_handle,scg_std_error_handle=std_error_handle,
				 scg_abc_path=abc_path,scg_path_without_suffix=path_without_suffix,scg_errors_file_name=errors_file_name}
		= (True,process_handle,scg,ps)

finish_code_generator :: !Int/*HANDLE*/ !StartedCodeGenerator !Int !(WindowFun *env) !*env -> (!Bool,!*env) | FileEnv env
finish_code_generator process_handle {scg_thread_handle,scg_std_error_handle,scg_abc_path,scg_path_without_suffix,scg_errors_file_name} exit_code wf ps
	# os = finish_process_with_redirected_std_error process_handle scg_thread_handle scg_std_error_handle 99
	| os<>99
		= undef
	# ((_, errors_not_empty, error_text),ps) = accFiles (ReadErrorsAndWarnings scg_errors_file_name) ps
	  ps = (if errors_not_empty 
				(wf (StrictListToList error_text)) 
					(if (exit_code <> 0)
						(wf ["Error: Code generator failed for '" +++ scg_abc_path +++ "' with exit code: "+++toString exit_code,(quoted_string scg_path_without_suffix)])
						id
					)
			 ) ps
	=  (exit_code==0,ps)

wait_for_finished_code_generator :: !{#Int} !*env -> (!Int,!Int,!*env);
wait_for_finished_code_generator handles ps
	# n_handles = size handles
	# (i,os) = WaitForMultipleObjects n_handles handles False INFINITE 99
	| i>=WAIT_OBJECT_0 && i<WAIT_OBJECT_0+n_handles
		# process_n = i-WAIT_OBJECT_0
		# (_,exit_code,os) = GetExitCodeProcess handles.[process_n] os
		| os<>99
			= undef
		= (process_n,exit_code,ps)
	| i>=WAIT_ABANDONED_0 && i<WAIT_ABANDONED_0+n_handles
		# process_n = i-WAIT_ABANDONED_0
		# (_,exit_code,os) = GetExitCodeProcess handles.[process_n] os
		| os<>99
			= undef
		= (process_n,exit_code,ps)
		= (-1,-1,ps)

mangleGenerator cgen` startupdir
	# (cgen`,opts)			= splitOptions cgen`
	# (shortOK,cgen)		= GetShortPathName (startupdir +++ "\\" +++ cgen` +++ "\0")
	| not shortOK
		# line				= "Error: Unable to get short path name '" +++ (startupdir +++ "\\" +++ cgen`) +++ "'."
		= (False,line,"")
	# cgencom = cgen % (0, size cgen - 2) +++ opts
	# cgendir = RemoveFilename (cgen % (0, size cgen - 2))
	= (True,cgencom,cgendir)

MakeCodeGenOptionsString genAsmOrCode timeprofile {ci,cs} tp
	= checkindex+++checkstack+++genasm
where
	checkindex	| ci = " -ci"; = ""
	checkstack	| cs = " -os"; = ""
	genasm		| genAsmOrCode == AsmGeneration
										= " -a"
										= ""
	
/* Links the given file:
*/

Link ::	!String !(WindowFun *env) !Pathname !ApplicationOptions
		!Pathname !(List Pathname) !(List Pathname) !(List Pathname) !Bool !Bool !Bool !Bool !String
		!Bool !String !Pathname !String !Processor !Bool !*env
		 -> (!*env,!Bool) | FileEnv env
Link linker` winfun path
		applicationOptions=:{ss,hs,initial_heap_size,profiling,heap_size_multiple,o,memoryProfilingMinimumHeapSize=minheap}
		optionspathname library_file_names object_file_names static_libraries static gen_relocs gen_linkmap
		link_resources resource_path gen_dll dll_syms startupdir dynlstr _ use_64_bit_processor ps
	# tooltempdir = maketempdir startupdir
	# (ok,linker,linkerdir)				= mangleLinker linker` startupdir
	| not ok
		# ps					= winfun [linker] ps
		= (ps,False)
	# flags						= ApplicationOptionsToFlags applicationOptions
	# optdirpath				= RemoveFilename optionspathname
	# ((ok,pd_optdirpath),ps)	= accFiles (pd_StringToPath optdirpath) ps
	| not ok
		= (winfun ["Linker error: Unable to understand path: "+++optdirpath] ps,False)
	# ((err,_),ps)				= accFiles (getDirectoryContents pd_optdirpath) ps
	# (err,ps)					= case err of
		DoesntExist		-> accFiles (createDirectory pd_optdirpath) ps
		err  			-> (err,ps)
	| err <> NoDirError
		= (winfun ["Linker error: Unable to access or create: "+++optdirpath] ps,False)
	# (options_file_ok,ps)		= accFiles (write_options_file optionspathname flags hs ss initial_heap_size heap_size_multiple minheap use_64_bit_processor) ps
	| not options_file_ok
		= (winfun ["Linker error: Could not write the options object file: "+++optionspathname] ps,False)
	# linkopts =
		{ exe_path					= path
		, res_path					= resource_path
		, open_console				= o <> NoConsole
		, static_link				= static
		, gen_relocs				= gen_relocs
		, gen_linkmap				= gen_linkmap
		, link_resources			= link_resources
		, object_paths				= optionspathname :! (RemoveDup object_file_names)
	  	, dynamic_libs				= RemoveDup library_file_names
	  	, static_libs				= RemoveDup static_libraries
	  	, stack_size				= ss
		, gen_dll					= gen_dll
		, dll_names					= dll_syms
		, dynamics_path				= startupdir +++. DirSeparatorString +++. dynlstr
	  	}
	# linkerpath					= RemoveFilename linker
//	# linkoptspath					= MakeFullPathname linkerpath "linkopts"
//	# linkerrspath					= MakeFullPathname linkerpath "linkerrs"
	# linkoptspath					= MakeFullPathname tooltempdir "linkopts"
	# linkerrspath					= MakeFullPathname tooltempdir "linkerrs"
	# (err,ps)						= accFiles (WriteLinkOpts linkoptspath linkopts) ps
	| isJust err
		= (winfun (fromJust err) ps,False)

	# linker = linker +++ " -I " +++ quoted_string linkoptspath +++ " -O " +++ quoted_string linkerrspath
	
	# (didit,exit_code,ost) = call_process linker linkerdir 99
	# diditall = if (ost == 99) didit didit
	| not diditall
		=	(winfun ["Error: Unable to run linker: "+++linker] ps, False)
	# link_ok = (exit_code==0) && (ost == 99)
	# ((err,link_errors),ps) = accFiles (ReadLinkErrors linkerrspath) ps
	| isJust err
		= (winfun (fromJust err) ps,False)
	# (errtext,errlines) = (link_errors, length link_errors)
	| errlines<>0
		= (winfun errtext ps,link_ok)
	=  (ps,link_ok)

mangleLinker linkstr` startupdir
	# (linkstr`,opts)		= splitOptions linkstr`
	# (shortOK,linkstr)		= GetShortPathName (startupdir +++ DirSeparatorString +++ linkstr` +++ "\0")
	| not shortOK
		# line				= "Error: Unable to get short path name '" +++ (startupdir +++ DirSeparatorString +++ linkstr`) +++ "'."
		= (False,line,"")
	# linkcom = linkstr % (0, size linkstr - 2) +++ opts
	# linkdir = RemoveFilename (linkstr % (0, size linkstr - 2))
	= (True,linkcom,linkdir)

splitOptions str
	| first_q >= len_str	= (str,"")
	= (first_str,last_str)
where
	first_str =  str%(0,dec first_q)
	last_str = str % (inc first_q, len_str)
	len_str = size str
	first_q			= FindQuoteChar str len_str 0
	FindQuoteChar str len pos	= FindChar ':' str len pos;
	FindChar	:: !Char !.String !.Int !Int -> Int;
	FindChar c line linelen pos
		| pos >= linelen		=  pos;
		| c ==  line.[pos]		=  pos;
								=  FindChar c line linelen (inc pos);

ReadErrorsAndWarnings :: !Pathname !*Files -> ((!CompilerMsg, !Bool, !(List String)), !*Files)
ReadErrorsAndWarnings path env
	#	(opened,file,env)	= fopen path FReadText env
	| not opened
		= ((CompilerOK,False,Nil),env)
	#	(errors,errors_and_warnings_read,errlist,file`) = ReadErrorAndWarningMessages file
		(_,env) = fclose file` env
	= ((errors,errors_and_warnings_read,errlist),env)
	
Strip "" = ""
Strip s
	#! last = dec (size s)
	#! char = s.[last]
	| char == '\n' || char == '\r'
		= Strip (s % (0,dec last))
	= s

ReadErrorAndWarningMessages :: !*File -> (!CompilerMsg,!Bool,!List String,!*File)
ReadErrorAndWarningMessages file
	#!	(string, file1)					= freadline file
		(eof,file2)						= fend file1
	| eof
		#!	not_empty_or_newline 		= (size string)<>0 && string.[0]<>'\n'
		= (SyntaxError,not_empty_or_newline,Strip string :! Nil,file2)
	#	(path_error,_,errlist,file3) = ReadErrorAndWarningMessages file2
	= (path_error,True,Strip string:!errlist,file3)

MakeCompilerOptionsString :: !CompileOrCheckSyntax !Bool !Bool !Bool !CompilerOptions -> String
MakeCompilerOptionsString compileOrCheckSyntax projectMemoryProfiling projectTimeProfiling projectEagerOrDynamic
			{neverMemoryProfile, neverTimeProfile,sa,gw,gc,listTypes,attr,reuseUniqueNodes,fusion}
	= options
where 
	memoryProfileSwitch
		| (not neverMemoryProfile && projectMemoryProfiling)
		|| projectEagerOrDynamic
			= " -desc"
			= ""
	timeProfileSwitch
		| not neverTimeProfile && projectTimeProfiling
			= " -pt"
			= ""
	dynamicLinkSwitch
		| projectEagerOrDynamic
			= " -exl -dynamics"
			= ""
	strictness
		| sa
			= ""
			= " -sa"
	warnings
		| gw
			= ""
			= " -w"
	comments
		| gc
			= " -d"
			= ""
	listtypes
		| listTypes == InferredTypes
			= " -lt"
		| listTypes == AllTypes
			= " -lat"
		| listTypes == StrictExportTypes
			= " -lset"
			= ""
	show_attr
		| attr
			= ""
			= " -lattr"
	checksyntax
		| compileOrCheckSyntax == SyntaxCheck
			= " -c"
			= ""
	reuse
		| reuseUniqueNodes
			= " -ou"
			= ""
	add_fusion_option s = if fusion (s+++" -fusion") s;
	
	options		= add_fusion_option (checksyntax +++ timeProfileSwitch +++ memoryProfileSwitch +++ dynamicLinkSwitch
									+++ strictness +++ warnings +++ comments +++listtypes+++show_attr+++reuse)
				  +++" "

start_compile_with_cache :: String Int String String String CompilerProcessIds *env -> (!Bool,!CompilerProcessIds,!*env)
start_compile_with_cache path slot directory startup_arguments arguments compiler_process_ids ps
	| slot<length compiler_process_ids
		# compiler_handles_and_id = compiler_process_ids !! slot
		= start_compile_with_cache2 path compiler_handles_and_id directory arguments compiler_process_ids ps
	# thread_id=get_current_thread_id;
	# begin_arguments=startup_arguments+++" -ide "+++int_to_hex thread_id;
	# (r,compiler_thread_id,compiler_thread_handle,compiler_process_handle) = start_compiler_process (path+++"\0") (directory+++"\0") (path+++" "+++begin_arguments+++"\0");
	| r==0
		= (False,compiler_process_ids,ps)
	# compiler_handles_and_id = {compiler_thread_id=compiler_thread_id,compiler_thread_handle=compiler_thread_handle,compiler_process_handle=compiler_process_handle}
	# compiler_process_ids = compiler_process_ids++[compiler_handles_and_id]
	= start_compile_with_cache2 path compiler_handles_and_id directory arguments compiler_process_ids ps
	
start_compile_with_cache2 :: {#.Char} CompilerProcessHandlesAndId {#.Char} {#.Char} CompilerProcessIds *env -> (!Bool,!CompilerProcessIds,!*env)
start_compile_with_cache2 path {compiler_thread_id,compiler_thread_handle,compiler_process_handle} directory arguments compiler_process_ids ps
	# wm_number=get_message_number
	# r=send_string_to_thread compiler_thread_id compiler_process_handle wm_number ("cocl "+++arguments+++"\0")
	| r==0
		= (False,compiler_process_ids,ps)
	= (True,compiler_process_ids,ps)
	
start_process_with_redirected_std_error :: !{#Char} !{#Char} !{#Char} !*OSToolbox -> (!Bool, !HANDLE, !HANDLE, !HANDLE, !*OSToolbox)
start_process_with_redirected_std_error command directory errors_file_name os
	| size command>0
		# (std_error_handle,os) = create_inheritable_file (errors_file_name+++"\0") os
		  (ok,process_information,os) = create_process (command+++."\0") (directory+++"\0") std_error_handle os
		| not ok
			# (_,os) = CloseHandle std_error_handle os
			= (False, 0, 0, 0, os)
			# process_handle = process_information.[PROCESS_INFORMATION_hProcess_int_offset]
			  thread_handle = process_information.[PROCESS_INFORMATION_hThread_int_offset]
			= (True,process_handle,thread_handle,std_error_handle,os)
		= (False, 0, 0, 0, os)

finish_process_with_redirected_std_error :: !HANDLE !HANDLE !HANDLE !*OSToolbox -> *OSToolbox
finish_process_with_redirected_std_error process_handle thread_handle std_error_handle os
	# (_,os) = CloseHandle std_error_handle os
	  (_,os) = CloseHandle thread_handle os
	  (_,os) = CloseHandle process_handle os
	= os
	
int_to_hex v
	= {hex_char i \\ i<-[0..7]};
where
		hex_char i
			# h=(v>>((7-i)<<2)) bitand 15;
			= toChar (if (h<10) (toInt '0'+h) ((toInt 'A'-10)+h));
			
InitCompilingInfo :: *CompilingInfo
//InitCompilingInfo = NotCompiling
InitCompilingInfo = CompilingInfo NoCompiler

compile_with_cache :: String String String String (CompilingInfo,*env) -> (!Bool,!Int,!(CompilingInfo,*env))
compile_with_cache path directory startup_arguments arguments prog=:(CompilingInfo NoCompiler, ps)
//	# startup_arguments = ""
	# thread_id=get_current_thread_id;
	# begin_arguments=startup_arguments+++" -ide "+++int_to_hex thread_id;
	# (r,compiler_thread_id,compiler_thread_handle,compiler_process_handle) = start_compiler_process (path+++"\0") (directory+++"\0") (path+++" "+++begin_arguments+++"\0");
	| r==0
		= (False,0,prog)
	# (ok,s) = compile_with_cache2 path directory arguments compiler_thread_id compiler_thread_handle compiler_process_handle;
	| ok
		# ci = CompilingInfo (CompilerProcess compiler_thread_id compiler_thread_handle compiler_process_handle)
		= (ok,s,(ci,ps));
		= (ok,s,prog);
compile_with_cache path directory startup_arguments arguments prog=:(CompilingInfo (CompilerProcess compiler_thread_id compiler_thread_handle compiler_process_handle),ps)
	# (ok,s) = compile_with_cache2 path directory arguments compiler_thread_id compiler_thread_handle compiler_process_handle
	| ok
		= (ok,s,prog)
	= (ok,s,(CompilingInfo NoCompiler,ps))
compile_with_cache path directory startup_arguments arguments prog=:(NotCompiling,ps)
    # command = quoted_string path +++ " " +++ arguments
	# (ok,exitcode, os4) = call_process command directory 99
	= (ok,exitcode,prog)
	
compile_with_cache2 :: {#.Char} {#.Char} {#.Char} Int Int Int -> (!Bool,!Int)
compile_with_cache2 path directory arguments compiler_thread_id compiler_thread_handle compiler_process_handle
	# wm_number=get_message_number
	# r=send_string_to_thread compiler_thread_id compiler_process_handle wm_number ("cocl "+++arguments+++"\0")
	| r==0
		= (False,0)
	# (r,a,s) =get_integers_from_thread_message wm_number compiler_thread_handle
	| r==0
		= (False,s)
	= (True,s)

CallProcess :: !String [(!String,!String)] !String !String !String !String !*OSToolbox -> (!Bool, !Int, !*OSToolbox)
CallProcess command environment directory stdin stdout stderr os 
	| size command > 0
		#	(commandptr,os)		=  winMakeCString command os
		    envstring			=  MakeEnvironmentString environment
			(envptr,os)			=  case (size envstring == 0) of
										True	-> (0, os)
										false	-> (winMakeCString envstring os)
		    (dirptr, os)		=  case (size directory == 0) of
										True	-> (0, os)
										false	-> (winMakeCString directory os)
			(inptr,  os)		=  case (size stdin  == 0) of
										True	-> (0, os)
										false	-> (winMakeCString stdin  os)
			(outptr, os)		=  case (size stdout == 0) of
										True	-> (0, os)
										false	-> (winMakeCString stdout os)
			(errptr, os)		=  case (size stderr == 0) of
										True	-> (0, os)
										false	-> (winMakeCString stderr os)
		    (success, exitcode, os) 
								=  winCallProcess commandptr envptr dirptr inptr outptr errptr os
			os					=  winReleaseCString commandptr os
			os					=  case (envptr == 0) of
										True	-> os
										false	->  (winReleaseCString envptr os)
			os					=  case (dirptr == 0) of
										True	-> os
										false	->  (winReleaseCString dirptr os)
			os					=  case (envptr == 0) of
										True	-> os
										false	-> (winReleaseCString inptr os)
			os					=  case (envptr == 0) of
										True	-> os
										false	-> (winReleaseCString outptr os)
//error
			os					=  case (envptr == 0) of
										True	-> os
										false	-> (winReleaseCString errptr os)
		= (success, exitcode, os)
	= (False, -1, os)
where
	MakeEnvironmentString [] = ""
    MakeEnvironmentString [ (name, value):rest ] = name +++ "=" +++ value +++ "\0" +++ MakeEnvironmentString rest

call_process :: !String !String !*OSToolbox -> (!Bool, !Int, !*OSToolbox)
call_process command directory os 
	| size command > 0
		#	(commandptr,os) = winMakeCString command os
		    (dirptr, os)	= if (size directory == 0)
									(0, os)
									(winMakeCString directory os)
		    (success, exitcode, os) = win_create_process commandptr dirptr os
			os = winReleaseCString commandptr os
			os = if (dirptr == 0)
					os
					(winReleaseCString dirptr os)
		= (success, exitcode, os)
	= (False, -1, os)

win_create_process ::  !CSTR !CSTR !*OSToolbox -> (!Bool, !Int, !*OSToolbox)
win_create_process _ _ _
	= code {
		ccall win_create_process "II:VII:I"
	}


wait_message :: !Int -> Int;
wait_message r = code {
	ccall WaitMessage@0 "P:V:I"
}

GENERIC_WRITE:==0x40000000;
CREATE_ALWAYS:==2;
FILE_ATTRIBUTE_NORMAL:==0x00000080;  

SECURITY_ATTRIBUTES_nLength_int_offset:==0;
SECURITY_ATTRIBUTES_bInheritHandle_int_offset:==2;

SECURITY_ATTRIBUTES_size_int:==3;
SECURITY_ATTRIBUTES_size_bytes_32:==12;
SECURITY_ATTRIBUTES_size_bytes_64:==24;
SECURITY_ATTRIBUTES_size_bytes :== IF_INT_64_OR_32 SECURITY_ATTRIBUTES_size_bytes_64 SECURITY_ATTRIBUTES_size_bytes_32;

:: HANDLE:==Int;

create_inheritable_file :: !{#Char} !*OSToolbox -> (!HANDLE,!*OSToolbox);
create_inheritable_file file_name os
	# security_attributes = {createArray SECURITY_ATTRIBUTES_size_int 0 &
	  							[SECURITY_ATTRIBUTES_nLength_int_offset] = SECURITY_ATTRIBUTES_size_bytes,
	  							[SECURITY_ATTRIBUTES_bInheritHandle_int_offset] = 1};
	= IF_INT_64_OR_32
		(CreateFile file_name GENERIC_WRITE 0 security_attributes CREATE_ALWAYS FILE_ATTRIBUTE_NORMAL 0 os)
		(CreateFile_32 file_name GENERIC_WRITE 0 security_attributes CREATE_ALWAYS FILE_ATTRIBUTE_NORMAL 0 os);

create_process :: !*{#Char} !{#Char} !HANDLE !*OSToolbox -> (!Bool,!{#Int},!*OSToolbox)
create_process command_line current_directory std_error_handle os
	# startup_info = {createArray STARTUPINFO_size_int 0 &
	  					[STARTUPINFO_cb_int_offset] = STARTUPINFO_size_bytes,
						[IF_INT_64_OR_32 STARTUPINFO_dwFlags_int_h_offset_64 STARTUPINFO_dwFlags_int_offset_32]
							= IF_INT_64_OR_32 (STARTF_USESTDHANDLES<<32) STARTF_USESTDHANDLES,
						[STARTUPINFO_hStdError_int_offset] = std_error_handle};
	  process_information = createArray PROCESS_INFORMATION_size_int 0
	  (ok,os) = IF_INT_64_OR_32
	  				(CreateProcess 0 command_line 0 0 True DETACHED_PROCESS 0 current_directory startup_info process_information os)
	  				(CreateProcess_32 0 command_line 0 0 True DETACHED_PROCESS 0 current_directory startup_info process_information os)
	= (ok,process_information,os)

CreateFile_32 :: !{#Char} !Int !Int !{#Int} !Int !Int !HANDLE !*OSToolbox -> (!HANDLE,!*OSToolbox);
CreateFile_32 fileName desiredAccess shareMode lpSecurityAttributes creationDisposition flagsAndAttributes templateFile os
	= code {
		ccall CreateFileA@28 "PsIIAIII:I:I"
	}

CreateFile :: !{#Char} !Int !Int !{#Int} !Int !Int !HANDLE !*OSToolbox -> (!HANDLE,!*OSToolbox);
CreateFile fileName desiredAccess shareMode lpSecurityAttributes creationDisposition flagsAndAttributes templateFile os
	= code {
		ccall CreateFileA@28 "PsIIAIIp:I:I"
	}

:: LPCTSTR:==Int;
:: LPSECURITY_ATTRIBUTES:==Int;
:: LPVOID:==Int;
:: LPSTARTUPINFO:==Int;
:: LPPROCESS_INFORMATION:==Int;

STARTF_USESTDHANDLES:==0x00000100;

STARTUPINFO_size_int_32:==17;
STARTUPINFO_size_bytes_32:==68;

STARTUPINFO_size_int_64:==13;
STARTUPINFO_size_bytes_64:==104;

STARTUPINFO_size_int :== IF_INT_64_OR_32 STARTUPINFO_size_int_64 STARTUPINFO_size_int_32;
STARTUPINFO_size_bytes :== IF_INT_64_OR_32 STARTUPINFO_size_bytes_64 STARTUPINFO_size_bytes_32;

STARTUPINFO_cb_int_offset:==0;

STARTUPINFO_dwFlags_int_offset_32:==11;
STARTUPINFO_hStdError_int_offset_32:==16;

STARTUPINFO_dwFlags_int_h_offset_64:==7;
STARTUPINFO_hStdError_int_offset_64:==12;

STARTUPINFO_hStdError_int_offset :== IF_INT_64_OR_32 STARTUPINFO_hStdError_int_offset_64 STARTUPINFO_hStdError_int_offset_32;

PROCESS_INFORMATION_size_int_32:==4;

PROCESS_INFORMATION_size_int_64:==3;

PROCESS_INFORMATION_size_int :== IF_INT_64_OR_32 PROCESS_INFORMATION_size_int_64 PROCESS_INFORMATION_size_int_32;

PROCESS_INFORMATION_hProcess_int_offset:==0;
PROCESS_INFORMATION_hThread_int_offset:==1;

DETACHED_PROCESS:==8;

CreateProcess_32 :: !LPCTSTR !*{#Char} !LPSECURITY_ATTRIBUTES !LPSECURITY_ATTRIBUTES !Bool !Int !LPVOID
					!{#Char} !{#Int} !{#Int} !*OSToolbox -> (!Bool,!*OSToolbox)
CreateProcess_32 lpApplicationName commandLine lpProcessAttributes lpThreadAttributes inheritHandles creationFlags lpEnvironment
					currentDirectory lpStartupInfo lpProcessInformation os
	= code {
		ccall CreateProcessA@40 "PIsIIIIIsAA:I:I"
	}

CreateProcess :: !LPCTSTR !*{#Char} !LPSECURITY_ATTRIBUTES !LPSECURITY_ATTRIBUTES !Bool !Int !LPVOID
					!{#Char} !{#Int} !{#Int} !*OSToolbox -> (!Bool,!*OSToolbox)
CreateProcess lpApplicationName commandLine lpProcessAttributes lpThreadAttributes inheritHandles creationFlags lpEnvironment
					currentDirectory lpStartupInfo lpProcessInformation os
	= code {
		ccall CreateProcessA@40 "PpsppIIpsAA:I:I"
	}

INFINITE:==0xFFFFFFFF

WaitForSingleObject :: !HANDLE !Int !*OSToolbox -> (!Int,!*OSToolbox);
WaitForSingleObject handle milliseconds os
	= code {
		ccall WaitForSingleObject@8 "PpI:I:I"
	}

WAIT_OBJECT_0:==0;
WAIT_ABANDONED_0:==0x80;
WAIT_TIMEOUT:==258;
WAIT_FAILED:==0xFFFFFFFF;

WaitForMultipleObjects :: !Int !{#Int} !Bool !Int !*OSToolbox -> (!Int,!*OSToolbox);
WaitForMultipleObjects n_handles handles waitAll milliseconds os
	= code {
		ccall WaitForMultipleObjects@16 "PIAII:I:I"
	}

GetExitCodeProcess :: !HANDLE !*OSToolbox -> (!Bool,!Int,!*OSToolbox);
GetExitCodeProcess process os
	= code {
		ccall GetExitCodeProcess@8 "PI:II:I"
	}

CloseHandle :: !HANDLE !*OSToolbox -> (!Bool,!*OSToolbox);
CloseHandle object os
	= code {
		ccall CloseHandle@4 "PI:I:I"
	}
