This repository holds the iTasks Software Development Kit (SDK).

=== What is it ? ==
The iTask Software Development Kit enables you to write multi-user web applications.
This can vary from a simple web application for filling in a web-form, a dedicated workflow system, an email application, or any application to support
workers in an organizarion to work together on the internet accomplishing a common goal.
So, it is a kit for developing multi-user software systems.

Applications are defined on a high level of abstraction, called "Task Oriented Programming" (TOP).
In this new style of programming, one defines the tasks humans and machines have to do.
One can abstract from low level technical stuf, like the communication between browsers and the server, the generation and handling of user interfaces,
the storage of information on disk or in databases.

TOP can be seen as an Embedded Domain Specific language, realized with a Combinator Library programmed in the host language Clean.

=== Setup ===

= First install the Clean 2.4 32-bit distribution on your machine (download it from the Clean site: http://wiki.clean.cs.ru.nl/Download_Clean). 
There are version for the PC, Mac, and Linux.

= Preparation of the Clean IDE for iTasks =
- Make sure that this SDK is placed in the folder of the Clean 2.4 32-bit distribution (the one that contains "CleanIDE.exe")
  and is called "iTasks-SDK".
- You need to use the "iTasks-SDK/Binaries/<OS version>/CleanIDE.exe", which must be in the same folder as the regular CleanIDE.exe. 
  If you want to use both, rename "iTasks-SDK/Binaries/<Windows version>/CleanIDE.exe" and move it in the same folder as CleanIDE.exe.
- Start the CleanIDE
- Import the "iTasks" environment by choosing "Environment" -> "Import..." from the menu
  and selecting the "iTasks-SDK/Server/iTasks-<OS version>.env" environment file.
- Set your default heap size for projects to 8M by choosing "Project" -> "Project defaults..." from the menu
- Copy the folder "iTasks-SDK/Patches/Sapl" into "Libraries/StdEnv/".
  The compilation process creates a directory called "sapl" which contains all the necessary SAPL files;
  this directory will be used by the client side execution infrastucture of the iTask toolkit.
- Copy the file "iTasks-SDS/Patches/_system.abc" into "Libraries/StdEnv/Clean System Files/". This is adds the
  predefined type () which is used for tasks without a result


= Building examples =
The most up-to-date example suite to run at the moment is the examples collection for the CEFP Summerschool.
- Open the "iTasks-SDK/Examples/BasicAPIExamples.prj" Clean project. ("File" -> "Open...")
- You build the project by choosing ("Project" -> "Update and Run") from the menu.
- A BasicAPIExamples.exe server is started automatically which you can access at "http://localhost/"
- Further instructions for setting up are given by the server 

=== Content of the repository ===
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

- Examples : Example task definitions.

- Tools    : Various build and code generation tools.
