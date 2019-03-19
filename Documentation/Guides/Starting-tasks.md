# Starting Tasks #

## Introduction ##
For many cases the "standard" way of starting tasks using `doTasks` is all you need.
For example, when you create an iTask program with a single simple task such as this:
```Clean
Start world = doTasks (viewInformation () [] "Hello, world") world
```
your task will make your task available to be started at the URL path `/` of the built-in web-server.
For each client that requests this URL, the server creates an instance of that task for that client.

While this is the common way of starting tasks, it is not the only way.
You can specify different tasks to be started through different URL paths, or you can specify that certain tasks should be started when the iTask program is (re)started.
In this guide we will show you how to do this.

## Starting different tasks in one application ##
If we want to start different tasks by accessing different URLs we need to specify which URLs correspond to which tasks. This also possible with `doTasks`.
The `doTasks` function is overloaded. It can be used with everything that is `Startable`.
There is an instance of `Startable` for `Task a` which is used in the simple case.
If we want to use `doTasks` with multiple tasks that can have different types, we have to create a value that
wraps those tasks and groups them together.
We do this as follows:
```Clean
taska = viewInformation () [] "Hello, world"
taskb = viewInformation () [] 42

Start world = doTasks
	[onRequest "/ta" taska
    ,onRequest "/tb" taskb
	] world
```

The `onRequest` function takes a task and wraps it together with the URL path as a `StartableTask` value.
If you need the information from the HTTP request to compute the task, you can use `onRequestFromRequest`, which takes a function of type `HTTPRequest -> Task a` instead.
There is an instance of `Startable` for `[StartableTask]`.
This makes it possible to specify a heterogeneous list of tasks with different types that can be started by requesting different URLs. In most cases you don't need the HTTP request, but it can be useful.

## Starting tasks when the application starts ##
Some applications contain tasks that don't need any user interaction and should be started immediately.
For those cases there is the `onStartup` function which also wraps a task as `StartableTask` value.
Tasks that are wrapped this way are created when the iTask program starts and are not directly bound to any user or client.
The following example shows a combination of an initialization task that is started when the application is started and another that can be started through the web.

```Clean
Start world = doTasks
	[onStartup initDatabase 
	,onRequest "/" browseDatabase
	] world
```
These tasks are not directly attached to a client when they are created. For a database initialization this may not be necessary, but sometimes you may want to attach them later.
In those cases you can use `onStartupWithAttributes`. This version takes another argument of type `TaskAttributes`. It lets you set the initial meta-data of the tasks, so you can identify the task instance.
This makes it possible to find and attach the task later.

## Advanced patterns ##
If you create frameworks for similar types of applications you can also create an instance of `Startable` for a type of your own. This lets you abstract from tasks that are always available in a certain type of application.
For example, the `iTasks.Extensions.Admin.WorkflowAdmin` extension defines the type `WorkflowCollection` which is `Startable`. Values of this type contain a set of tasks with names and descriptions.
This way, when you create an application using `WorkflowCollection` it initializes a database with those wrapped tasks, called workflows, on startup. It also adds a generic task that lets you login and choose which of those workflows to work on.

