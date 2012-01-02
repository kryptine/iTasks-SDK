#!/bin/sh
sencha create jsb -a http://localhost:8082/ -p app.jsb3
sencha build -p app.jsb3 -d .
