Running iTasks on Linux or a Mac is possible, but it takes a little more work

You should be able to get it working if you follow these steps:

1. Install clean. Download the clean 2.4 distribution for your OS, unzip it in
   your home directory (e.g. in ~/clean) and run make inside that directory.

2. Make sure the clean binaries are in your path by editing your .profile,
   .bashrc or whatever config file your shell uses.  Add ~/clean/bin to your
   path (assumming you installed clean in ~/clean). You can check by running
   "clm" (the commandline clean build tool for unix).

3. Get the latest iTasks from subversion
   (https://svn.cs.ru.nl/repos/iTask-system/trunk) and check it out to
   ~/clean/iTasks-SDK (e.g.

     "svn co https://svn.cs.ru.nl/repos/iTask-system/trunk iTasks-SDK"

   from within the ~/clean directory

4. Get the latest clean compiler with iTasks specific modifications from
   subversion (https://svn.cs.ru.nl/repos/clean-compiler/branches/itask) and
   build it by running the build script for your OS from within the checkout.
   For example ./unix/make.linux.sh (for 32bit linux). This will build an
   executable called "cocl" (the clean compiler). You must copy or move this
   cocl to ~/clean/lib/exe/

5. You need to install the latest graph_copy library from subversion and build
   it. This library provides fast serialization of arbitrary Clean values. You
   can find it in
   https://svn.cs.ru.nl/repos/clean-libraries/trunk/Libraries/graph_copy. Check
   it out to ~/clean/lib/. This library contains C code that needs to be
   compiled. Remove all .o files from ~/clean/lib/graph_copy and then run the
   makefile for your platform (e.g. "make -f Makefile.linux"). To make the
   library work with iTasks you need to comment out two double imports that
   currently break on Unix platforms. In dynamic_string.icl you need to comment
   out lines 6 and 7 such that they contain:

     //import code from "copy_graph_to_string_interface."
     //import code from "copy_graph_to_string."

6. You need to patch some of the standard clean libraries that came with the
   Clean distribution. You can find the patches files in iTasks-SDK/Compiler/
   Copy the following:

     cp ~/clean/iTasks-SDK/Compiler/StdGeneric.dcl ~/clean/lib/StdEnv/
     cp ~/clean/iTasks-SDK/Compiler/StdGeneric.icl ~/clean/lib/StdEnv/
     cp ~/clean/iTasks-SDK/Compiler/_SystemDynamic.dcl ~/clean/lib/Dynamics/
     cp ~/clean/iTasks-SDK/Compiler/_SystemDynamic.icl ~/clean/lib/Dynamics/
     cp ~/clean/iTasks-SDK/Compiler/TCPChannels.dcl ~/clean/lib/TCPIP/
     cp ~/clean/iTasks-SDK/Compiler/TCPChannels.icl ~/clean/lib/TCPIP/

7. You are now ready to compile your iTasks program. You do this by running clm
   with the options -nt -mv -dynamics -h 80M and a series of -I options to all
   necessary libraries. On linux you also need the -no-opt-link option. It is
   easiest to do so with a small script or Makefile because the list of
   libraries is quite long. The content of your script could be something like
   this (for a program myprogram.icl on linux32)

#!/bin/sh
CLEAN_HOME=$HOME/clean
ITASKS_SDK=$CLEAN_HOME/iTasks-SDK
clm -h 80M -no-opt-link -nt -mv -dynamics\
    -I $CLEAN_HOME/lib/graph_copy\
    -I $CLEAN_HOME/lib/Dynamics\
    -I $CLEAN_HOME/lib/StdEnv\
    -I $CLEAN_HOME/lib/TCPIP\
    -I $ITASKS_SDK/Server\
    -I $ITASKS_SDK/Server/Framework\
    -I $ITASKS_SDK/Server/Framework/ClientSupport\
    -I $ITASKS_SDK/Server/API\
    -I $ITASKS_SDK/Server/API/Core\
    -I $ITASKS_SDK/Server/API/Common\
    -I $ITASKS_SDK/Server/API/Extensions\
    -I $ITASKS_SDK/Server/API/Extensions/Admin\
    -I $ITASKS_SDK/Server/lib/Platform/OS-Independent\
    -I $ITASKS_SDK/Server/lib/Platform/OS-Independent/Data\
    -I $ITASKS_SDK/Server/lib/Platform/OS-Independent/Math\
    -I $ITASKS_SDK/Server/lib/Platform/OS-Independent/Text\
    -I $ITASKS_SDK/Server/lib/Platform/OS-Independent/Text/Encodings\
    -I $ITASKS_SDK/Server/lib/Platform/OS-Independent/Internet\
    -I $ITASKS_SDK/Server/lib/Platform/OS-Independent/System\
    -I $ITASKS_SDK/Server/lib/Platform/OS-Posix/System\
    -I $ITASKS_SDK/Server/lib/Platform/OS-Linux/System\
    -I $ITASKS_SDK/Server/lib/Platform/OS-Linux-32/System\
    -I $ITASKS_SDK/Server/lib/Http\
    -I $ITASKS_SDK/Server/lib\
    myprogram -o myprogram

8. Pray everything works and have fun!

9. If you get an error similar to the following

     "Link error: File: 'wsock_library' not found."

   comment out

     library "wsock_library"

   in ~/clean/lib/TCPIP/ostcp.icl
