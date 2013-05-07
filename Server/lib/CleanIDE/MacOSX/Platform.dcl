definition module Platform

PlatformDependant win mac :== win

IF_MACOSX macosx not_macosx :== macosx

DirSeparator:=='/'
DirSeparatorString:=="/"

application_path :: !{#Char} -> {#Char}
