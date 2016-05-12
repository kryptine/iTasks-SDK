This folder contains patched versions of external libraries.
The necessity for this folder comes from the fact that the clean compiler used to make
iTasks applications has modifications which require library changes, but we can't change
them upstream because then they will no longer work with the vanilla clean 2.4 compiler.

Always make sure the paths to this folder and its subfolders are the topmost entries in
environments and project files.

Sorry for the inconvenience.