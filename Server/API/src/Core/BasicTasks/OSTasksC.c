#include "..\..\..\..\..\..\Libraries\ObjectIO\OS Windows\Windows_C_12\util_121.h"

Bool WinIsDirectory(CLEAN_STRING name) {
	WIN32_FIND_DATA find_data;

	HANDLE handle = FindFirstFile(cstring(name), &find_data);

	if (handle != INVALID_HANDLE_VALUE) {
		FindClose(handle);
		return find_data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY ? TRUE : FALSE;
	}
	else
		return FALSE;
}

Bool WinCreateDirectory (CLEAN_STRING name) {
	return CreateDirectory(cstring (name), NULL) ? TRUE : FALSE;
}

void WinCallExecutable (PSTR commandline,
					 OS ios,
					 Bool * success,
					 HANDLE *handle,
					 OS * oos
				    )
{
	 PSTR env = NULL;
	 PSTR dir = NULL;
	 PSTR in = NULL;
	 PSTR out = NULL;
	 PSTR err = NULL;

	SECURITY_ATTRIBUTES sa;
	STARTUPINFO si;
	PROCESS_INFORMATION pi;
	BOOL fsuccess;
	char *ep;
	HANDLE infile;
	HANDLE outfile;
	HANDLE errfile;

	*success = FALSE;
	*oos = ios;
	*handle = NULL;

	if (env != NULL)
	{
		ep = env;
		while (rstrlen (ep) != 0)
		{
			ep += rstrlen (ep) + 1;
		}
	}

	sa.nLength = sizeof (SECURITY_ATTRIBUTES);
	sa.bInheritHandle = TRUE;
	sa.lpSecurityDescriptor = NULL;

	if (in != NULL)
	{
		infile = CreateFile (in,
							 GENERIC_READ,
							 FILE_SHARE_READ | FILE_SHARE_WRITE,
							 &sa,
							 OPEN_EXISTING,
							 FILE_ATTRIBUTE_NORMAL,
							 NULL
			);
		if (infile == INVALID_HANDLE_VALUE)
		{
			rprintf ("infile creation failed\n");
			return;
		}
		rprintf ("redirection of input ok\n");
	}
	else
	{
		rprintf ("in == NULL\n");
		infile = GetStdHandle(STD_INPUT_HANDLE);
	}

	if (out != NULL)
	{
		outfile = CreateFile (out,
							  GENERIC_WRITE,
							  0,
							  &sa,
							  CREATE_ALWAYS,
							  FILE_ATTRIBUTE_NORMAL,
							  NULL
			);
		if (outfile == INVALID_HANDLE_VALUE)
		{
			rprintf ("outfile creation failed\n");
			return;
		}
		rprintf ("redirection of output ok\n");

	}
	else
	{
		rprintf ("out == NULL\n");
		outfile = GetStdHandle(STD_OUTPUT_HANDLE);
	}

	if (err != NULL)
	{
		errfile = CreateFile (err,
							  GENERIC_WRITE,
							  0,
							  &sa,
							  CREATE_ALWAYS,
							  FILE_ATTRIBUTE_NORMAL,
							  NULL
			);
		if (errfile == INVALID_HANDLE_VALUE)
		{
			rprintf ("errfile creation failed\n");
			return;
		}

		rprintf ("redirection of errors ok\n");
	}
	else
	{
		rprintf ("err == NULL\n");
		errfile = GetStdHandle(STD_ERROR_HANDLE);
	}

	si.cb = sizeof (STARTUPINFO);
	si.lpReserved = NULL;
	si.lpReserved2 = NULL;
	si.cbReserved2 = 0;
	si.lpDesktop = NULL;
	si.lpTitle = NULL;
	si.dwFlags = STARTF_USESTDHANDLES;
	si.hStdInput = infile;
	si.hStdOutput = outfile;
	si.hStdError = errfile;

	fsuccess =
		CreateProcess (NULL,				/* pointer to name of executable module		*/
					   commandline,			/* pointer to command line string			*/
					   NULL,				/* pointer to process security attributes	*/
					   NULL,				/* pointer to thread security attributes	*/
					   TRUE,				/* handle inheritance flag					*/
					   DETACHED_PROCESS,	/* creation flags							*/
					   env,					/* pointer to new environment block			*/
					   dir,					/* pointer to current directory name		*/
					   &si,					/* pointer to STARTUPINFO					*/
					   &pi					/* pointer to PROCESS_INFORMATION			*/
		);
	if (fsuccess)
	{
		rprintf ("WCP: success\n");
		*handle = pi.hProcess;
		//WaitForSingleObject (pi.hProcess, INFINITE);
		//GetExitCodeProcess (pi.hProcess, (unsigned long *) exitcode);
		*success = TRUE;
	}
	else
	{
		rprintf ("WCP: failure %d\n", (int) GetLastError ());
		*success = FALSE;
	}

	if (in != NULL && infile != NULL)
	{
		if (CloseHandle (infile))
			rprintf ("closing infile ok\n");
		else
			rprintf ("closing infile failed\n");
	}
	else
		rprintf ("no need to close and reset input\n");

	if (out != NULL && outfile != NULL)
	{
		if (CloseHandle (outfile))
			rprintf ("closing outfile ok\n");
		else
			rprintf ("closing outfile failed\n");
	}
	else
		rprintf ("no need to close and reset output\n");

	if (err != NULL && errfile != NULL)
	{
		if (CloseHandle (errfile))
			rprintf ("closing errfile ok\n");
		else
			rprintf ("closing errfile failed\n");
	}
	else
		rprintf ("no need to close and reset errput\n");

	rprintf ("WCP: returning\n");
	/* *oos = ios; */
}	/* WinCallProcess */

void WinCheckProcess (HANDLE handle, OS ios, Bool* active, LPDWORD exitCode, OS * oos) {
	*oos = ios;
	GetExitCodeProcess(handle, exitCode);
	*active = *exitCode == STILL_ACTIVE ? TRUE : FALSE;
}

