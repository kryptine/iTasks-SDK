# Building an iTask project # 

iTask programs are just regular Clean programs that happen to use the iTask framework. Compiling an iTask program does require some specific project configuration settings though. Luckily you can easily get started by using a project template.

In this guide we'll walk you through the steps to build your first iTask project. We assume you have already installed Clean and the iTask framework.

## Building on Windows ##
On windows, you can use the Clean IDE to build your iTask programs.

### Step 1: Creating a main module ###
Open the Clean IDE and use `File -> New File` from the main menu to create a new file called `myprogram.icl`.
Write the following code in the new file (the yellow window).

```Clean
module myprogram
import iTasks

Start world = doTasks (viewInformation (Title "Hello") [] "Hello, world") world
```
Save the file using `File -> Save myprogram.icl` from the main menu.

### Step 2: Creating a project file ###
You now need to create a project file. A project file contains all search paths and compilation settings for building a Clean program.
The iTask framework provides a template with default settings for an iTask project. You can create your own project file using this template by choosing `File -> New Project Using Template...` from the main menu.
You are then asked to select a project template (.prt) file. You can find the `iTasks.prt` template file in the `Config` subdirectory of your Clean system.
After selecting the template file you are asked to choose a name for the project file. By default it will be `myprogram.prj` (for the module `myprogram.icl`) which you don't have to change.

### Step 3: Building and running your project ###
You can build and run your program by choosing `Project -> Update and Run` from the main menu. After compilation is completed you should see a console window displaying an URL that you can open in your web browser.

## Building on Linux or Mac ##
On Linux and macOS you can use a command line tool called `cpm` to create and build Clean projects.

### Step 1: Creating a main module ###
First create a Clean source module with a minimal program. Write the following code to a file called `myprogram.icl`.

```Clean
module myprogram
import iTasks

Start world = doTasks (viewInformation (Title "Hello") [] "Hello, world") world
```

### Step 2: Creating a project file ###
Using the `cpm` tool we can create a new project file. A project file contains all search paths and compilation settings for building a Clean program.
The iTask framework provides a template with default settings for an iTask project. You can create your own project file using this template with the following command:

```
cpm project myprogram create $CLEAN_HOME/etc/iTasks.prt
```
This will create a file called `myprogram.prj` which is the project file for your program.

### Step3: Building your project ###
To build your program, you can again use `cpm`:

```
cpm myprogram.prj
```

This will create a number of output files in your current directory. The most important one is `myprogram`, the executable program. There will also be additional resources that are needed at runtime: A bytecode version of your program: `myprogram.bc`, a version of that bytecode for the browser: `myprogram.pbc` and a directory with all the public web assets your program needs: `myprogram-www`.

### Step 4: Running your program ###
Simply run the `myprogram` executable and use a web browser to open the URL that is displayed.

```
./myprogram
```

