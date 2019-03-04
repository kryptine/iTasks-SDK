# Distributed iTasks example

## Set-up

### Linux

1. Get `cclinker` from: [https://gitlab.science.ru.nl/_arjan/cclinker](https://gitlab.science.ru.nl/_arjan/cclinker)
	* Compile it (see README.md)
	* copy `cclinker` and `cclinker.config` to `lib/exe` folder of the Clean compiler.
2. Change the `IDEEnvs` file of Clean compiler (`etc/IDEnvs`):
	* Append to the EnvironmentCompiler line: `-desc -exl -d` to enable that the compiler stores function names in the executable.
	* Change `lib\exe\linker` to `lib\exe\cclinker` in the EnvironmentLinker line.
4.  Now you can compile examples.prj with cpm.

### Windows

1. Change the `IDEEnvs` file of Clean compiler (`Config/IDEnvs`):
	* Append to the EnvironmentCompiler line: `-desc -exl -d` to enable that the compiler stores function names in the executable.
2. Open the examples.prj file with Clean-IDE and compile the project.
