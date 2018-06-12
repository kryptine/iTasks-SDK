#!/bin/bash
set -e
trap 'mv -v /opt/clean/etc/IDEEnvs{.bak,}' EXIT
cp -v /opt/clean/etc/IDEEnvs{,.bak}
sed -i "s|{Application}/lib/iTasks|$(pwd)/Libraries|g" /opt/clean/etc/IDEEnvs
find . -name "*.prj" | xargs dirname | sort -u | xargs -I{} sh -c "cd {}; cpm make"
