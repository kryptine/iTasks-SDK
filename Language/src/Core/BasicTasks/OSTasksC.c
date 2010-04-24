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
