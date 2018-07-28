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
If we want to start different tasks by accessing different URLs we need to specify which URLs correspond to which tasks.
The `doTask` function is overloaded. It can be used with everything that is `Startable`.
There is an instance of `Startable` for `Task a` which is used in the simple case.
If we want to use `doTasks` with multiple tasks that can have different types we have to create a value that
wraps those tasks and groups them together.
We can do this as follows:
```Clean
taska = viewInformation () [] "Hello, world"
taskb = viewInformation () [] 42

Start world = doTasks
	[onRequest "/ta" (const taska)
    ,onRequest "/tb" (const taskb)
	]
```
The `onRequest` function takes a function that computes a task from an HTTP request (hence the `const` in the example) and wraps it together with the URL path as a `StartableTask` value.
There is an instance of `Startable` for `[StartableTask]`.
This makes it possible to specify a heterogeneous list of tasks with different types that can be started by requesting different URLs. In most cases you don't need the HTTP request, but it can be useful.

## Starting tasks when the application starts ##
Some applications contain tasks that don't need any user interaction and should be started immediately.
For those cases there is the `onStartup` function which also wraps a task as `StartableTask` value.
Tasks that are wrapped this way are created when the iTask program starts and are not directly bound to any user or client. The second argument is a set of `TaskAttributes`.
This is initial meta-data that you can use to make it possible to identify the task instance.
Because these tasks are not directly attached to a client when they are created, this makes it possible to find and attach them later.
The following example shows a combination of an initialization task that is started when the application is started and another that can be started through the web.

```Clean
Start world = doTasks
	[onStartup initDatabase defaultValue
	,onRequest "/" (const browseDatabase)
	]
```
