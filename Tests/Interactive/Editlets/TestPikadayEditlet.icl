module TestPikadayEditlet

import iTasks.Extensions.Form.Pikaday

test :: Task Date
test = testEditorWithShare pikadayDateField defaultValue Update

Start world = startEngine test world
