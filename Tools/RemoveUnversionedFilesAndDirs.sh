#!/bin/bash
cd ..
svn status | grep "^\?" | cut -c9- | tr '\n' '\0' | xargs -0 rm -rf
