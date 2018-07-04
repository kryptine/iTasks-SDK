#!/bin/bash
set -e
if [ -e /opt/clean/etc/IDEEnvs ]; then
	trap 'mv -v /opt/clean/etc/IDEEnvs{.bak,}' EXIT
	cp -v /opt/clean/etc/IDEEnvs{,.bak}
	sed -i "s|{Application}/lib/iTasks|$(pwd)/Libraries|g" /opt/clean/etc/IDEEnvs
	sed -i 's#EnvironmentLinker:	lib/exe/linker#&:-lmysqlclient -lsqlite3#g' /opt/clean/etc/IDEEnvs
fi

#Try to compile everything
find . -name "*.prj.default" | while read f; do
		cp "$f" "$(dirname $f)/$(basename -s .prj.default $f)".prj
	done
find . -name "*.prj" -exec dirname {} \; | sort -u | xargs -I{} sh -c "cd {}; cpm make"

#Run the unit tests
find Tests/Unit -type f -perm 755 -exec cleantest -r {} \;
