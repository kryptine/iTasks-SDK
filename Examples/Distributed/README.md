# Distributed iTasks example

## Set-up

### Linux

1. Get `cclinker` from: [https://gitlab.science.ru.nl/_arjan/cclinker](https://gitlab.science.ru.nl/_arjan/cclinker)
	* Compile it (see README.md)
	* copy `cclinker` and `cclinker.config` to `lib/exe` folder of the Clean compiler.
2. Change the `IDEEnvs` file of Clean compiler (`etc/IDEnvs`):
	* Append to the EnvironmentCompiler line: `-desc -exl -d` to enable that the compiler stores function names in the executable.
	* Change `lib\exe\linker` to `lib\exe\cclinker` in the EnvironmentLinker line.
3. Compile the code-generator and copy the new cg version to the Clean compiler.
	* Get the source: `svn checkout https://svn.cs.ru.nl/repos/clean-code-generator/trunk/ code-generator`
	* Apply patch: `(cd code-generator; sed -i 's/\(!rts_got_flag\)/\(pic_flag \&\& !rts_got_flag\)/g' cgaas.c)`
	* Compile: `(cd code-generator; make -f Makefile.linux64)`
	* Replace cg: (`cp code-generator\cg \path\to\clean\lib\exe\cg`)
	* Remove all the .abc files compiled with the old code-generator.
4.  Now you can compile examples.prj with cpm.

### Windows

1. Change the `IDEEnvs` file of Clean compiler (`Config/IDEnvs`):
	* Append to the EnvironmentCompiler line: `-desc -exl -d` to enable that the compiler stores function names in the executable.
2. Open the examples.prj file with Clean-IDE and compile the project.
