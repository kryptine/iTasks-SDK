This repository contains the iTask framework for developing Task-Oriented Programs

# What is iTasks ?

The iTask framework enables you to write multi-user web applications using a task-oriented style of programming.
Applications range from simple web applications for filling out web-forms, to large workflow support systems. Essentially any application to support people in an organization to work together using the internet.

Applications are defined on a high level of abstraction, called "Task Oriented Programming" (TOP).
In this new style of programming, one defines the tasks humans and machines have to do.
One can abstract from low level technical stuff, like the communication between browsers and the server, the generation and handling of user interfaces,
the storage of information on disk or in databases.

TOP can be seen as an Embedded Domain Specific language, realized with a Combinator Library programmed in the host language Clean.

## Set up

### Step 1: Clean and dependencies

To use the framework you need to have a recent install of the Clean language. The easiest way to obtain this is to get the `clean-bundle-itasks` nightly build package. This contains everything you need to get started. This bundle can be downloaded from:

[ftp://ftp.cs.ru.nl/pub/Clean/builds/&lt;OS version&gt;/clean-bundle-itasks-&lt;OS version&gt;-latest.[zip|tgz]](ftp://ftp.cs.ru.nl/pub/Clean/builds/<OS version>/clean-bundle-itasks-linux-64-latest.tgz)

Alternatively you can also download a `clean-base` package from the same ftp-site and add the following packages `clean-lib-generics` `clean-lib-dynamics` `clean-lib-platform` `clean-lib-sapl` `clean-lib-graphcopy` and `clean-lib-tcpip`.

### Step 2: Configuring CleanIDE or Cpm

Given that you are reading this README file, you have probably cloned this repository directly from:

https://gitlab.science.ru.nl/clean-and-itasks/iTasks-SDK

If you also have a packaged version of iTasks installed in your `$CLEAN_HOME/Libraries` folder (e.g. the one from the `clean-bundle-itasks`) repository you need to make sure that you are using the checked out version instead of the bundled version. You can put your checkout everywhere you like, but to get started quickly we provide a convenient `iTasks git` environment for use with the CleanIDE/Cpm. You can find this environment in `Config/<OS version>/iTasks-git.env`. This environment expects your git checkout to be located in `$CLEAN_HOME/Development/iTasks-SDK`. 

## Organisation of this repository

This repository follows the structure of a Clean system install.

The top-level folders are:

- `Libraries` : Here you find the iTask framework Clean library. 
- `Tools`: These contain additional tools you need to create iTask applications. Currently there is only one tool you really need: the web resource collector. This program is essentially a linker, but for static web resources such as images, html/css and javascript files. In the `Libraries` folder exist special `WebPublic` folders with these resources. At compile time, they are collected based on which parts of the framework is used by your application.
- `Examples`: In this folder you can find example programs that demonstrate the iTask framework.
- `Tests`: In this folder you can find test programs that are used to automatically and interactively test the framework
- `Config`: Here you can find the environment definitions for the CleanIDE/Cpm. These are organized in subfolders for each OS/Platform because paths are different on each target.
- `Documentation`: Here you can find supplemental documentation. A good starting point is the iTask guide (`Documentation/GUIDE.md`).

