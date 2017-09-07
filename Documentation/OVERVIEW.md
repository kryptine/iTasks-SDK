# iTasks Overview

## Introduction
The iTasks Framework is not your average codebase. It is an implementation of a new paradigm in a programming language you will likely never have programmed in before.
So if you are a unfamiliar with Clean, TOP, iTasks or all of the above, you may get a little lost trying to wrap your head around things.
This document is intended to help you find your way around the  library (and the toolchain around it), and to answer some questions we expect you might have.

## Context

The goal of the iTasks framework is to facilitate the development of programs following the "Task-Oriented Programming (TOP)" paradigm. Stated somewhat simplisticly that means that you write programs by  describing the task, as a composition of subtasks, that the program is intended to support. Because all programs manipulate data in some way, you don't only specify tasks, but also the data associated with those tasks. Everything else that is typically part of a program: data persistence, data communication infrastructure, user-interface design is considered optional. A TOP framework (iTasks in this case) should provide generic solutions for those aspects. That way you can choose to invest in carefully designed solutions only for those tasks where it adds value.

The TOP paradigm and the iTask framework have evolved together. The framework in its current form is something completely different than what was described in the first research paper about iTasks in 2007. In those years the main purpose of the library was to experiment with new concepts, refine the TOP paradigm, explore alternative designs, and to create of proof-of-concept applications. In recent years the focus has shifted from proof-of-concepts to demonstration and facilitation of the TOP paradigm for real-world use.  As you explore the code, you may still find some artifacts from early experiments, but we are continuously refactoring and cleaning up the codebase to consolidate them into a coherent overall architecture.  

## Principles

The primary principle of TOP is that programming should be reduced to the specification of tasks, their relations and the information that flows between them.

Every design decision in the iTask framework should ultimately be judged against that principle. This means that:

- The embedded language to model tasks and information flow must be rich enough to describe a wide range of applications
- An executable program must be producable from this specification alone.
- The generic solutions that supplement the TOP specifications must be "acceptable". The quality attributes should be such that for roughly 80% of a program the generic solutions should be "good enough".
- All other development activities other than writing TOP specifications should be minimised. It is a bit disappointing if all the time you saved by creating a concise iTasks program that perfectly captures the task you want to support with your program is lost on figuring out how to deploy your application to a production environment.   

## Constraints

The iTask framework is an open-source system developed in an academic setting by a small team. For none of the people that work on it, it is their primary task. It is not a product backed by a company with customers who need iTasks-based applications to run their daily operations. In practice, this means that you cannot expect support reliably (we do want to help everyone, but can't always afford to) and that what gets worked on is primary dictated by the personal agenda's and interests of the contributors.

For us, the researchers that work on it, it is a platform for experimenting with new ideas and to demonstrate results with. This means that attention to features and quality attributes is distributed differently than in libraries intended for use in production systems. There are a lot of cool advanced techniques used in the framework, but a mundane (but useful) features that you might expect are sometimes missing. Think of iTasks being something akin to a F1 race car. They are great for experimenting with and showing off advances in automotive technology, but they are lousy cars for daily use. No room for passengers or groceries, no lights and don't even think of parallel parking.

## Software Architecture

The iTasks _framework_ is implementated as a _library_ in the programming language Clean. This means that an iTasks program is "just" a Clean program. But, because it is a framework, is a program that will call your program. So generally all iTasks programs will have a `Start` expression (think `main` in C) that looks something like this: `Start world = startEngine myTaskExpression world`. The `startEngine` runs the iTask framework which is parameterized with your program expressed by `myTaskExpression`.

How you specify that task expression we'll ignore for now. The necessary concepts are explained in the next section. We first look at what iTasks does with that expression. To summarize: it generates a complete interactive (multi-user) web-application that let's you perform the task you specified. Many web applications consist of code written different languages. Typically 'frontend' code in javascript (and html fragments) and backend code in a mainstream language of choice. iTasks programs are _single source_ specifications. This does not mean that you are not allowed to divide your program into modules, but that a full web application is generated from the Clean program you write. The "client-side" javascript code of the web-applications is generated from the same Clean source code as the web server program. In fact, to explain the overall architecture of an iTasks program, let's first look at the artifacts that are created when you compile an iTask program.

When you compile an iTask program, say a Clean module called `HelloWorld.icl`, you will get three things:

- An executable  e.g. `HelloWorld.exe` on Windows. This is the application server that will coordinate and execute the task(s) you specified in your program. It exposes the GUI of your application through an HTTP web service (by default on port 80 on Windows and 8080 on Linux and Mac).
- A folder with static web resources called `HelloWorld-www`. This contains all the static HTML, Javascript, CSS and images that the application needs. Most importantly it contains the `index.html` landing page that will load the `itask-*.js` libraries that make up the client-side runtime system. If your application uses iTasks extension libraries that integrate with other Javascript libraries, things like Google Maps for example, the additional scripts necessary for that will also be added to this folder during compilation.
- A folder with partially compiled client-side code called `HelloWorld-sapl`. This folder contains the code of your application in an intermediate language called SAPL. This language is compiled just-in-time to Javascript and downloaded only when needed by tasks that use client-side computation.

TODO: Explain main components: Server executable with two services: ui and static content, client runtime with interpreter for gui's

## iTasks Concepts

In the next section we give an overview of how the codebase is organised. To make sense of this organisation you need to be familiar with the main concepts used in the library.

### Tasks

Tasks are the key concepts in TOP.  In iTasks they are represented by expressions of type `Task a` . A task is a  _specification_ of how a certain task is supported by the software. It does represent the execution of that task. To actually do something useful, a task needs to be instantiated. When a task is instantiated the iTask framework will manage the _state_ of that specific instance. Every time a user uses the program, by accessing it with a web browser, a new instance of the main task is created for that user.

Tasks are specified by composing tasks from other tasks using functions (operators) called _combinators_. This can only work if there are also predefined tasks to use as smallest building blocks. The iTasks framework therefore provides a set of such builtin tasks. These are broadly divided in two categories. Tasks in which a user interacts with the software, and fully automated tasks.

### Shared Data Sources

Every information system stores and accesses data that is often shared between different users in some way or another. The choices you make on how and where you store data have a big influence on various quality attributes of your programs. However, when analysing workflows and describing them as a composition of tasks, how data is stored and shared is not important. You simply want to specify what information is shared between tasks. For this reason, iTasks uses a single abstraction for every kind of stateful shared data. This abstraction is the notion of _shared data sources_.  So whether you retrieve data from a database, a flat file, memory or a web service, in the tasks specifications you just access an abstract shared data source.

### Editors

For tasks in which a user interactively performs the task with the help of your program, you need a user interface. In iTasks we use the concept of an _Editor_ to specify user interfaces for tasks. An `Editor a` is a set of functions that define a complete GUI for editing some value of type `a`. It contains the code to render the GUI initially, to handle events, and to synchronize changes to the value between the application (that contains a webserver) and the user's web browser.

Like everything in the iTask framework, editors are composable. There are builtin editors, for example a `textField` editor of type `Editor String` and a `checkbox` editor of type `Editor Bool`. With these building blocks you can construct GUIs using combinators.

One of the nice features of iTasks is that it provides a type-generic function which can be used to derive an editor for _any_ type. This means that you can always view, enter or update information, even when you don't explicitly design a GUI to do so.

### Layouts

By specifying editors, you can program (or derive) the GUI's of individual tasks in which a user interacts with  your program. But because tasks are composable, a task can also be composed of any number of tasks that can be done in parallel. The GUI for that task composition also has to be the composition of the GUI's of the tasks in the composition. A _Layout_ specifies how these GUI's are combined into a single GUI. It divides the screen real estate and sometimes removes or adds parts.

## Overview of the Codebase

The main concerns that an iTask programmer has to think about are _the application's workflow_, _the application's use of shared data_, and often to some extent _user interface_ design. These concerns are reflected in the top level of the module hierarchy which is as follows

- `iTasks`
  - `WF`
  - `SDS`
  - `UI`
  - `Internal`
  - `Extensions`
  - `Util`

Everything related to the specification of tasks can be found in the `iTasks.WF` namespace. It contains all the types, builtin tasks and combinators you need to specify workflows in an iTask application.

The modules in `iTasks.SDS` contain everything you need to define shared datasources. You can create volatile shared sources in memory, or create adapters to various forms of persistent storage. Additionally you can compose sources using combinators.

User interface specification is done using the modules in `iTasks.UI`. Here you can find everything for creating custom editors and specifying layouts. 

The `iTasks.WF`, `iTasks.SDS`, and `iTasks.UI` modules are what you need to write iTask programs. But there are three more submodules in the `iTasks` namespace. The `iTasks.Internal` namespace contains all the machinery to make the first three work. You should never have to import anything from this namespace, unless you are working on the iTask framework itself. The `iTasks.Extensions` namespace contains a bunch of useful libraries that you can import individually. Finally, the `iTasks.Util` namespace contains general utility libraries that are not specific to iTasks and should eventually be migrated to the [Clean Platform](https://gitlab.science.ru.nl/clean-and-itasks/clean-platform) library.

If you want to learn more about the modules you can look at the documentation comment blocks in the definition modules. When there is more to say about a module (or a whole namespace) that would just pollute the definition modules, you can look for markdown files in the `Documentation` folder. These files are structured to reflect the structure of the source code.

