#!/bin/bash
set -e

#Update IDEEnvs
if [ -e "$CLEAN_HOME"/etc/IDEEnvs ]; then
	trap 'mv -v "$CLEAN_HOME"/etc/IDEEnvs{.bak,}' EXIT
	cp -v "$CLEAN_HOME"/etc/IDEEnvs{,.bak}
	sed -i.bak "s|{Application}/lib/iTasks|$(pwd)/Libraries|g" "$CLEAN_HOME"/etc/IDEEnvs
	sed -i.bak 's#EnvironmentLinker:	lib/exe/linker#&:-lmysqlclient -lsqlite3#g' "$CLEAN_HOME"/etc/IDEEnvs
fi

#Create BasicAPIExamples
(
	cd Examples
	cp CreateBasicAPIExamples.prj{.default,}
	cpm CreateBasicAPIExamples.prj
	rm CreateBasicAPIExamples.prj
	./CreateBasicAPIExamples.exe > BasicAPIExamples.icl
)

#Try to compile everything
find . -name "*.prj.default" | while read f; do
		cp "$f" "$(dirname $f)/$(basename -s .prj.default $f)".prj
	done
#Without generic fusion
find . -name "*.prj" -exec dirname {} \; | sort -u | xargs -I{} sh -c\
	"cd {}; cpm make"
#With generic fusion
find . -name "*.prj" -exec dirname {} \; | sort -u | xargs -I{} sh -c\
	"cd {}; sed -i 's/GenericFusion:	False/GenericFusion: True/g' *.prj &&
	sed -i 's|EnvironmentCompiler:	lib/exe/cocl:|&:-h 4000m -s 100m|g' *.prj && cpm make"
		

#Run the unit tests
find Tests/Unit -type f -perm 755 | xargs -n 1 cleantest -r
