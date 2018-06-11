#!/bin/bash
set -e

CLMFLAGS="-dynamics -h 200m -s 10m"
CLMLIBS="-IL Dynamics -IL GraphCopy -IL Sapl -IL TCPIP -IL Platform -IL Platform/Deprecated/StdLib -I $(pwd)/Libraries"
CLM="clm $CLMFLAGS $CLMLIBS"

( cd Examples; $CLM BasicAPIExamples; )
( cd Examples/Games/Ligretto; $CLM -I .. -I ../../Graphics Ligretto; )
( cd Examples/Games/Trax; $CLM -I .. Trax; )
( cd Examples/GIS; $CLM LeafletMapExample; )
( cd Examples/Graphics/BasicImagesExamples; for i in *.prj.default; do $CLM -I .. $(basename -s .prj.default $i); done; )
( cd Examples/Applications/TheTaxMan; $CLM TheTaxMan; )
( cd Examples/Applications/Incidone; $CLM -l -l sqlite3 -l -l mysqlclient IncidoneCCC; )
#( cd Examples/Applications/ShipAdventure; $CLM main; )
