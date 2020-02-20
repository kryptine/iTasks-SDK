#!/bin/bash
set -e

#Update IDEEnvs
if [ -w "$CLEAN_HOME"/etc/IDEEnvs ]; then
	cp -v "$CLEAN_HOME"/etc/IDEEnvs{,.bak2}
	trap 'mv -v "$CLEAN_HOME"/etc/IDEEnvs{.bak2,}' EXIT
	sed -n -i.bak '/EnvironmentName:\tiTasks/q;p' "$CLEAN_HOME"/etc/IDEEnvs
	tail -n +3 Config/linux-x64/iTasks.env >> "$CLEAN_HOME"/etc/IDEEnvs
	sed -i.bak "s|{Application}/lib/iTasks|$(pwd)/Libraries|g" "$CLEAN_HOME"/etc/IDEEnvs
	sed -i.bak 's#EnvironmentLinker:\s\+/usr/bin/gcc::#& -lmysqlclient -lsqlite3 #g' "$CLEAN_HOME"/etc/IDEEnvs
	sed -i.bak 's|EnvironmentCompiler:\s\+lib/exe/cocl-itasks:|&-h 2048m|g' "$CLEAN_HOME"/etc/IDEEnvs
fi

#Create BasicAPIExamples
(
	cd Examples
	cp CreateBasicAPIExamples.prj{.default,}
	cpm CreateBasicAPIExamples.prj
	rm CreateBasicAPIExamples.prj
	./CreateBasicAPIExamples.exe > BasicAPIExamples.icl
)

#Try to compile all modules
if [ $(uname) = "Linux" ]; then
	errors="$(
		cd Examples
		cp BasicAPIExamples.prj{.default,}
		find ../Libraries/ -name "*.dcl" -exec head -n 1 {} \; \
			|  sed 's/definition module //g' \
			|  xargs cpm project BasicAPIExamples.prj compile \
			|& grep -i 'Error \[.*\.[di]cl')"
	echo "$errors" >&2
	[ -z "$errors" ]
fi

#Try to compile everything
find . -name "*.prj.default" | while read f; do
		cp "$f" "$(dirname $f)/$(basename -s .prj.default $f)".prj
	done
#Without generic fusion
find . -name "*.prj" -exec dirname {} \; | sort -u | xargs -I{} sh -c\
	"cd {}; cpm make"
#With generic fusion
find . -name "*.prj" -not -name "IncidoneCCC.prj" -not -name "examples.prj" -not -name "RemoteShareExamples.prj" -exec dirname {} \; | sort -u | xargs -I{} sh -c\
	"cd {}; sed -i.bak 's/GenericFusion:	False/GenericFusion: True/g' *.prj && cpm make"

#Run the unit tests
find Tests/Unit -name "*.prj.default" | sed "s/.prj.default//" | xargs -n 1 -I@ cleantest -f human --junit @-junit.xml -r @
