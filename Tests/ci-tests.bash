#!/bin/bash
set -e
trap 'mv -v /opt/clean/etc/IDEEnvs{.bak,}' EXIT
cp -v /opt/clean/etc/IDEEnvs{,.bak}
sed -i "s|{Application}/lib/iTasks|$(pwd)/Libraries|g" /opt/clean/etc/IDEEnvs

#Create BasicAPIExamples
(
	cd Examples
	cp CreateBasicAPIExamples.prj{.default,}
	cpm CreateBasicAPIExamples.prj
	rm CreateBasicAPIExamples.prj
	./CreateBasicAPIExamples > BasicAPIExamples.icl
)

#Try to compile everything
find . -name "*.prj.default" | while read f; do
		cp "$f" "$(dirname $f)/$(basename -s .prj.default $f)".prj
	done
find . -name "*.prj" | xargs dirname | sort -u | xargs -I{} sh -c "cd {}; cpm make; rm *.prj"

( cd Tools; ./RunUnitTestsForCI; )
