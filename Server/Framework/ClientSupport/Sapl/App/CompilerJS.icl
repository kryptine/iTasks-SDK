module CompilerJS

import StdEnv, CodeGeneratorJS, StringAppender

readLines con a 
	# (line, con) = freadline con
	= if (size line > 0) (readLines con (a <++ line)) (toString a, con)

Start world 
	# (con, world) = stdio world
	# (source, con) = readLines con newAppender
	# con = case generateJS source of
		Ok (a, _) = fwrites (toString a) con
		Error msg = fwrites msg stderr 
	= fclose con world