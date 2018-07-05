#!/bin/bash
set -e

if [ -e "$CLEAN_HOME"/etc/IDEEnvs ]; then
	trap 'mv -v "$CLEAN_HOME"/etc/IDEEnvs{.bak,}' EXIT
	cp -v "$CLEAN_HOME"/etc/IDEEnvs{,.bak}
	sed -i "s|{Application}/lib/iTasks|$(pwd)/Libraries|g" "$CLEAN_HOME"/etc/IDEEnvs
	sed -i 's#EnvironmentLinker:	lib/exe/linker#&:-lmysqlclient -lsqlite3#g' "$CLEAN_HOME"/etc/IDEEnvs
fi

#Try to compile everything
find . -name "*.prj.default" | while read f; do
		cp "$f" "$(dirname $f)/$(basename -s .prj.default $f)".prj
	done
find . -name "*.prj" -exec dirname {} \; | sort -u | xargs -I{} sh -c "cd {}; cpm make"

#Run the unit tests
find Tests/Unit -type f -perm 755 | xargs -n 1 cleantest -r
