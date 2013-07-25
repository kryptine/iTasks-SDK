Running iTasks on Linux or a Mac is possible, but it takes a little more work

You should be able to get it working if you follow these steps:

 1. Install clean. Download the clean 2.4 distribution for your OS, unzip it in
    your home directory (e.g. in ~/clean) and run make inside that directory.

 2. Make sure the clean binaries are in your path by editing your .profile,
    .bashrc or whatever config file your shell uses.  Add ~/clean and
    ~/clean/bin to your path (assumming you installed clean in ~/clean). You
    can check by running "clm" (the commandline clean build tool for unix).

 3. Get the latest Clean compiler with iTasks specific modifications from
    SVN (https://svn.cs.ru.nl/repos/clean-compiler/branches/itask):

      "svn co https://svn.cs.ru.nl/repos/clean-compiler/branches/itask clean-compiler-itask"

    and build it by running the build script for your OS from within the
    checkout. For example ./unix/make.linux64.sh (for 64 bit linux). This will
    build an executable called "cocl" (the Clean compiler).

 4. Get the latest Clean code generator from SVN
    (https://svn.cs.ru.nl/repos/clean-code-generator/trunk/) and build it by
    executing the make file for your platform. E.g. make -f Makefile.linux64
    This will create a new executable called "cg".

 5. Move or copy the cocl and cg executables to ~/clean/lib/exe/ and execute
    "make" in ~/clean

 6. Get the latest iTasks from Subversion (SVN)
    (https://svn.cs.ru.nl/repos/iTask-system/trunk) and check it out to
    ~/clean/iTasks-SDK from within the ~/clean directory

 7. Compile the latest graph_copy for your platform. It can be found in
    iTasks-SDK/Server/lib/graph_copy. Build it using "make -f Makefile.linux64"
    if you're compiling for Linux 64.

 8. Copy the contents of iTasks-SDK/Server/iTasks(Linux64|OSX).env (depending
    on your platoform) to your ~/clean/IDEEnvs file. If IDEEnvs does not exist,
    create it with the following contents and copy the contents of the .env
    file below it:

      Version: 1.0
      Environments

 9. Get the latest Clean IDE from SVN
    (https://svn.cs.ru.nl/repos/clean-ide/trunk/) and navigate to the
    BatchBuild directory. Execute the make script for your platform. E.g.
    ./make.linux.sh Copy the resulting BatchBuild executable to ~/clean

If in this step or the next you get an error about a _return_code symbol, edit
Unix/set_return_code.icl and replace all occurences of _return_code with
return_code

10. At this point, Linux users must follow the instructions at the bottom of
    this README in order for everything to work.

11. Navigate to the root of your Clean IDE checkout and execute the following
    command to build CPM for your platform:

      BatchBuild CpmLinux.prj

    Replace CpmLinux.prj with CpmMacOSX.prj if you are building for Mac OS X.
    Copy the resulting "cpm" executable to ~/clean as well.

12. You are now ready to compile your iTasks program. The best way to do so is
    by using CPM. To build all project files in the current directory, simply
    type

      cpm make

    To build a specific project file, type

      cpm project MyProject.prj build

    CPM has a built-in help system as well.

If you get an error similar to the following

  "Link error: File: 'wsock_library' not found."

comment out

  library "wsock_library"

in ~/clean/lib/TCPIP/ostcp.icl

N.B. If you execute "make" in ~/clean after this process, many of the files you
have manually copied to your Clean installation will be overwritten. To prevent
this, also copy files to ~/clean/exe, ~/clean/StdEnv and ~/clean/data


== Linux specific instructions ==

A.  Get the latest clean-tools from SVN
    (https://svn.cs.ru.nl/repos/clean-tools/trunk/) and navigate to the
    elf_tools directory. Modify linker.icl and change the occurence of
    "set_return_code" to "set_return_code_world". Next, build a new linker with
    the following command:

      "clm -nt -nr -h 200M -s 100M -I ai64 -IL ArgEnv -IL Dynamics -I ../../clean-ide/Unix linker -o linker"

    Depending on where you checked out your copy of the Clean IDE, you might
    need to modify the relative path to clean-ide/Unix.

    Copy the resulting linker executable to ~/clean/lib/exe

B.  Get the latest clean-libraries from SVN
    (https://svn.cs.ru.nl/repos/clean-libraries/trunk) and navigate to the
    Libraries/StdEnv/StdEnv\ Changed\ Files directory. Copy
    _SystemStrictList.icl to ~/clean/lib/StdEnv and copy _system.abc to
    ~/clean/lib/StdEnv/Clean\ System\ Files. Remove _SystemStrictList.o and
    _SystemStrictList.abc

C.  Get the latest Clean RTS from SVN
    (https://svn.cs.ru.nl/repos/clean-run-time-system/trunk/) and execute
    ./make_astartup.csh. Copy the resulting linux64/_startup.o to
    ~/clean/lib/StdEnv/Clean\ System\ Files

D.  Go to iTasks-SDK/Server/lib/graph_copy and remove all .o files by executing
    "rm -f *.o" and rebuild graph_copy: make -f Makefile.linux64
