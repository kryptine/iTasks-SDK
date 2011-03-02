implementation module GinBindings

import StdBool
import Maybe
import GenEq

import GinSyntax
import GinParser
import GinAbstractSyntax

getNodeBinding :: GIdentifier Bindings -> GParseState NodeBinding
getNodeBinding name [] = parseError ("Node binding " +++ name +++ " not found")
getNodeBinding name [b:bs] = case b of
	NodeBinding sb | sb.NodeBinding.declaration.GDeclaration.name == name = ret sb
	otherwise = getNodeBinding name bs

getParallelBinding :: GIdentifier GIdentifier Bindings -> GParseState ParallelBinding
getParallelBinding split merge [] = parseError ("Invalid split/merge combination (" +++ split +++ "," +++ merge +++ ")")
getParallelBinding split merge [b:bs] = case b of
	ParallelBinding pb | pb.ParallelBinding.split.GDeclaration.name == split 
						 && pb.ParallelBinding.merge.GDeclaration.name == merge = ret pb
	otherwise = getParallelBinding split merge bs					

getBranchType :: GIdentifier Bindings -> GParseState BranchType
getBranchType name [] = parseError ("Node binding " +++ name  +++ " not found")
getBranchType name [b:bs] = case b of
	NodeBinding sb     | sb.NodeBinding.declaration.GDeclaration.name == name = ret BTSingle
	ParallelBinding pb | pb.ParallelBinding.split.GDeclaration.name   == name = ret BTSplit
	ParallelBinding pb | pb.ParallelBinding.merge.GDeclaration.name   == name = ret BTMerge
	otherwise = getBranchType name bs

/*
importDeclarations :: ModuleBindings -> GImport
importDeclarations mb = { GImport 
                        | name = mb.ModuleBindings.name
                        , types = mb.ModuleBindings.types
                        , declarations = filterDup (flatten (map imp mb.bindings))
                        } where
	imp :: Binding -> [GDeclaration]
	imp (NodeBinding d)      = [d.NodeBinding.declaration]
	imp (ParallelBinding pb) = [pb.split, pb.merge]
	
	filterDup              :: [a] -> [a] | gEq{|*|} a
	filterDup []           =  []
	filterDup [x:xs]       =  [x : filterDup (filter (\y = not (x === y)) xs)]
*/
mkGDefinitionBinding :: GDefinition -> Binding
mkGDefinitionBinding gdef = 
	NodeBinding { NodeBinding 
                | declaration = gdef.GDefinition.declaration
				, parameterMap = NBPrefixApp
				}

getModuleDeclarations :: ModuleBindings -> [GDeclaration]
getModuleDeclarations mb = getDeclarations mb.ModuleBindings.bindings

getDeclarations :: Bindings -> [GDeclaration]
getDeclarations bindings = flatten (map get bindings)
where
	get :: Binding -> [GDeclaration]
	get (NodeBinding nb) = [nb.NodeBinding.declaration]
	get (ParallelBinding pb) = [ pb.ParallelBinding.split
							   , pb.ParallelBinding.merge ]

