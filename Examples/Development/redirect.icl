implementation module redirect;

import StdEnv;

call_process_with_redirected_std_out_and_error :: !{#Char} !{#Char} !{#Char} !{#Char} !*World -> (!(!Bool, !Int), !*World);
call_process_with_redirected_std_out_and_error command directory out_file_name errors_file_name world
	# (b,i,os) = create_process_with_redirected_std_out_and_error command directory out_file_name errors_file_name 0;
	= ((b,i),world);


:: OSToolbox:==Int;

Start
	= create_process_with_redirected_std_out_and_error
		"D:\\John\\CleanPrograms\\fsieve.exe -con"
		"D:\\John\\CleanPrograms"
		"D:\\John\\CleanPrograms\\ooo"
		"D:\\John\\CleanPrograms\\eee"
		0;

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

create_process_with_redirected_std_out_and_error :: !{#Char} !{#Char} !{#Char} !{#Char} !*OSToolbox -> (!Bool, !Int, !*OSToolbox);
create_process_with_redirected_std_out_and_error command directory out_file_name errors_file_name os
	| size command>0
		# (std_out_handle,os) = create_inheritable_file (out_file_name+++"\0") os;
		# (std_error_handle,os) = create_inheritable_file (errors_file_name+++"\0") os;
		  (ok,process_information,os) = create_process (command+++."\0") (directory+++"\0") std_out_handle std_error_handle os;
		| not ok
			# (_,os) = CloseHandle std_out_handle os;
			# (_,os) = CloseHandle std_error_handle os;
			= (False, -1, os);
			# process_handle = process_information.[PROCESS_INFORMATION_hProcess_int_offset];
			  thread_handle = process_information.[PROCESS_INFORMATION_hThread_int_offset];
			  (_,os) = WaitForSingleObject process_handle INFINITE os;
			  (_,exit_code,os) = GetExitCodeProcess process_handle os;
			  (_,os) = CloseHandle std_out_handle os;
			  (_,os) = CloseHandle std_error_handle os;
			  (_,os) = CloseHandle thread_handle os;
			  (_,os) = CloseHandle process_handle os;
			= (True, exit_code, os);
		= (False, -1, os);

create_inheritable_file :: !{#Char} !*OSToolbox -> (!HANDLE,!*OSToolbox);
create_inheritable_file file_name os
	# security_attributes = {createArray SECURITY_ATTRIBUTES_size_int 0 &
	  							[SECURITY_ATTRIBUTES_nLength_int_offset] = SECURITY_ATTRIBUTES_size_bytes,
	  							[SECURITY_ATTRIBUTES_bInheritHandle_int_offset] = 1};
	= IF_INT_64_OR_32
		(CreateFile file_name GENERIC_WRITE 0 security_attributes CREATE_ALWAYS FILE_ATTRIBUTE_NORMAL 0 os)
		(CreateFile_32 file_name GENERIC_WRITE 0 security_attributes CREATE_ALWAYS FILE_ATTRIBUTE_NORMAL 0 os);

create_process :: !*{#Char} !{#Char} !HANDLE !HANDLE !*OSToolbox -> (!Bool,!{#Int},!*OSToolbox);
create_process command_line current_directory std_out_handle std_error_handle os
	# startup_info = {createArray STARTUPINFO_size_int 0 &
	  					[STARTUPINFO_cb_int_offset] = STARTUPINFO_size_bytes,
						[IF_INT_64_OR_32 STARTUPINFO_dwFlags_int_h_offset_64 STARTUPINFO_dwFlags_int_offset_32]
							= IF_INT_64_OR_32 (STARTF_USESTDHANDLES<<32) STARTF_USESTDHANDLES,
						[STARTUPINFO_hStdOut_int_offset] = std_out_handle,
						[STARTUPINFO_hStdError_int_offset] = std_error_handle};
	  process_information = createArray PROCESS_INFORMATION_size_int 0;
	  (ok,os) = IF_INT_64_OR_32
	  				(CreateProcess 0 command_line 0 0 True DETACHED_PROCESS 0 current_directory startup_info process_information os)
	  				(CreateProcess_32 0 command_line 0 0 True DETACHED_PROCESS 0 current_directory startup_info process_information os);
	= (ok,process_information,os);

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
STARTUPINFO_hStdOut_int_offset_32:==15;
STARTUPINFO_hStdError_int_offset_32:==16;

STARTUPINFO_dwFlags_int_h_offset_64:==7;
STARTUPINFO_hStdOut_int_offset_64:==11;
STARTUPINFO_hStdError_int_offset_64:==12;

STARTUPINFO_hStdOut_int_offset :== IF_INT_64_OR_32 STARTUPINFO_hStdOut_int_offset_64 STARTUPINFO_hStdOut_int_offset_32;
STARTUPINFO_hStdError_int_offset :== IF_INT_64_OR_32 STARTUPINFO_hStdError_int_offset_64 STARTUPINFO_hStdError_int_offset_32;

PROCESS_INFORMATION_size_int_32:==4;

PROCESS_INFORMATION_size_int_64:==3;

PROCESS_INFORMATION_size_int :== IF_INT_64_OR_32 PROCESS_INFORMATION_size_int_64 PROCESS_INFORMATION_size_int_32;

PROCESS_INFORMATION_hProcess_int_offset:==0;
PROCESS_INFORMATION_hThread_int_offset:==1;

DETACHED_PROCESS:==8;

CreateProcess_32 :: !LPCTSTR !*{#Char} !LPSECURITY_ATTRIBUTES !LPSECURITY_ATTRIBUTES !Bool !Int !LPVOID
					!{#Char} !{#Int} !{#Int} !*OSToolbox -> (!Bool,!*OSToolbox);
CreateProcess_32 lpApplicationName commandLine lpProcessAttributes lpThreadAttributes inheritHandles creationFlags lpEnvironment
					currentDirectory lpStartupInfo lpProcessInformation os
	= code {
		ccall CreateProcessA@40 "PIsIIIIIsAA:I:I"
	}

CreateProcess :: !LPCTSTR !*{#Char} !LPSECURITY_ATTRIBUTES !LPSECURITY_ATTRIBUTES !Bool !Int !LPVOID
					!{#Char} !{#Int} !{#Int} !*OSToolbox -> (!Bool,!*OSToolbox);
CreateProcess lpApplicationName commandLine lpProcessAttributes lpThreadAttributes inheritHandles creationFlags lpEnvironment
					currentDirectory lpStartupInfo lpProcessInformation os
	= code {
		ccall CreateProcessA@40 "PpsppIIpsAA:I:I"
	}

INFINITE:==0xFFFFFFFF;

WaitForSingleObject :: !HANDLE !Int !*OSToolbox -> (!Int,!*OSToolbox);
WaitForSingleObject handle milliseconds os
	= code {
		ccall WaitForSingleObject@8 "PpI:I:I"
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
