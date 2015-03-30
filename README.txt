This repository holds the iTasks Software Development Kit (SDK).

=== What is it ? ==
The iTask Software Development Kit enables you to write multi-user web applications.
This can vary from a simple web application for filling in a web-form, a dedicated workflow system, an email application, or any application to support
workers in an organization to work together on the internet accomplishing a common goal.
So, it is a kit for developing multi-user software systems.

Applications are defined on a high level of abstraction, called "Task Oriented Programming" (TOP).
In this new style of programming, one defines the tasks humans and machines have to do.
One can abstract from low level technical stuff, like the communication between browsers and the server, the generation and handling of user interfaces,
the storage of information on disk or in databases.

TOP can be seen as an Embedded Domain Specific language, realized with a Combinator Library programmed in the host language Clean.

=== Installation ===

Please read Install/<OS version>/INSTALL.txt in the Installation directory.

=== Content of the repository ===
This SDK consists of multiple libraries, tools and documents that are needed to
create iTasks applications.
They are divided over multiple folders as follows:

- Server        : Here you find all the Clean libraries that are needed to build iTasks
                  server applications. As a workflow programmer you will need these libraries
                  in combination with the Clean IDE and build system to program workflow management
                  applications.
                  The "Server" folder is divided in an "API" and "Framework" folder. In API you find
                  the modules that make up the iTasks workflow definition language. You can use these
                  to specify your workflows with combinators. The "Framework" folder contains all the
                  magic that is needed to turn workflow specifications into executable systems. As a
                  workflow programmer you will not need to get to know these libraries.

- Client        : To let end-users interact with the workflow management systems you create with
                  the server libraries, they need a client application. In this folder you find the
                  default AJAX web client which your server application will serve at run time.

- Documentation : Documentation and example task definitions.

- Tools         : Various build and code generation tools.


=== Practical tips ===

- You can use your own images in your iTasks application by creating a WebPublic
  directory next to your exectuable. Any file and folder contained therein can
  be accessed via hostname/fileordir.ext
