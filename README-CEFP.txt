This package is the iTasks Software Development Kit (SDK) prerelease for CEFP 2011.

If you are impatient and want to get started right away, please continue with
the instructions at the bottom of this document.
This SDK consists of multiple libraries, tools and documents that are needed to
create iTasks applications.
They are divided over multiple folders as follows:

- Server   : Here you find all the Clean libraries that are needed to build iTasks
             server applications. As a workflow programmer you will need these libraries
             in combination with the Clean IDE and build system to program workflow management
             applications.
             The "Server" folder is divided in an "API" and "Framework" folder. In API you find
             the modules that make up the iTasks workflow definition language. You can use these
             to specify your workflows with combinators. The "Framework" folder contains all the
             magic that is needed to turn workflow specifications into executable systems. As a
             workflow programmer you will not need to get to know these libraries.

- Client   : To let end-users interact with the workflow management systems you create with
             the server libraries, they need a client application. In this folder you find the
             default AJAX web client which your server application will serve at run time.

- Examples : Example workflow suites.

- Tools    : Various build and code generation tools.

=== Getting started (CEFP examples suite) ===

The best way to get to know the iTasks SDK is by building and running the CEFP examples suite.
To build this suite do the following steps:
- Make sure that this SDK is placed in the folder of the Clean 2.3 distribution (the one that contains "CleanIDE.exe")
  and is called "iTasks-SDK".
- Copy the file "iTasks-SDK/Compiler/_SystemArray-patched.icl" to "Libraries/StdEnv/_SystemArray.icl"
- Start the CleanIDE
- Import the "iTasks" environment by choosing "Environment" -> "Import..." from the menu
  and selecting the "iTasks-SDK/Server/iTasks.env" file. 
- Open the "iTasks-SDK/Examples/CEFP2011/CEFP.prj" Clean project. ("File" -> "Open...")
- You can now build the project by choosing ("Project" -> "Update and Run") from the menu.
- The CEFP.exe server is started automatically which you can access at "http://localhost/"
- Further instructions for setting up are given by the server 
