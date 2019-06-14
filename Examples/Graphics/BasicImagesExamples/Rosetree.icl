module Rosetree

import iTasks.Engine
import iTasks.WF.Tasks.Interaction
import iTasks.UI.Prompt
import iTasks.Extensions.SVG.SVGEditor
import StdFunc, StdList, StdTuple

//	shorthand definitions for the used fonts in these examples
arial			= normalFontDef "Arial"

//	shorthand definitions for the used colours in these examples
white			= toSVGColor "white"

Start :: *World -> *World
Start world
	= doTasks (viewInformation "Rose tree" [ViewUsing id (fromSVGEditor
															{ initView    = id
															, renderImage = const roses
															, updModel    = \_ v = v
															})] 0) world


/** roses model tags = image:
	@image shows the use of @tags to display an arbitrary rose tree structure.
*/
roses :: m *TagSource -> Image m
roses _ tags = margin (px 10.0) (fst (show_rose show_my_node my_rose_tree tags))
where
	show_my_node txt ts
		= (margin (px zero,px zero,px bottom,px zero) (
		      overlay [(AtMiddleX,AtMiddleY)] []
		          [text font txt]
		          (Host (rect (textxspan font txt + textxspan font "MM") (px (height + text_y_margin)) <@< {fill = white})))
		  , ts
		  )
	where
		font          = arial height
		height        = 10.0
		text_y_margin = 5.0
		bottom        = 5.0

/**	show_rose show_node (Rose r []):
		is the function that only displays @r, using @show_node.
	show_rose show_node (Rose r rs):
		is the function that display @r, using @show_node, and below and to the right of it,
		shows the children nodes @rs.
*/
show_rose :: (a -> St *TagSource (Image m)) (Rose a) -> St *TagSource (Image m)
show_rose show_node (Rose r [])
	= show_node r
show_rose show_node (Rose r rs)
	= \[(t1,ut1), (t2,ut2) : ts] ->
		let (image,  ts1) = show_node r ts
		    (images, ts2) = seqList (map (show_rose show_node) rs) ts1
		 in ( above (repeat AtLeft) [] Nothing []
		          [ image
		          , beside (repeat AtTop) [] Nothing []
		                   [ yline (imageyspan t1 - imageyspan t2)
		                   , tag ut1
		                        (grid (Columns 2) (ColumnMajor,LeftToRight,TopToBottom) [] [] [] []
		                              (repeatn (length rs) (xline (px 10.0)) ++ init images ++ [tag ut2 (last images)])
		                              NoHost
		                        )
		                   ] NoHost
		          ] NoHost
		    , ts2
		    )


:: Rose a = Rose a [Rose a]

/**	my_rose_tree:
	is an example of a rose tree, using the chapters from the Clean 2.2 Language Report.
*/
my_rose_tree :: Rose String
my_rose_tree = Rose "Clean 2.2 Language Report"
                  [Rose "BASIC SEMANTICS"
                      [Rose "Graph Rewriting"
                          [Rose "A Small Example" []]
                      ,Rose "Global Graphs" []
                      ]
                  ,Rose "MODULES AND SCOPES"
                      [Rose "Identifiers, Scopes and Name Spaces"
                          [Rose "Naming Conventions of Identifiers" []
                          ,Rose "Scopes and Name Spaces" []
                          ,Rose "Nesting of Scopes" []
                          ]
                      ,Rose "Modular Structure of Clean Programs" []
                      ,Rose "Implementation Modules"
                          [Rose "The Main or Start Module" []
                          ,Rose "Scope of Global Definitions in Implementation Modules" []
                          ,Rose "Begin and End of a Definition: the Layout Rule" []
                          ]
                      ,Rose "Definition Modules" []
                      ,Rose "Importing Definitions"
                          [Rose "Explicit Imports of Definitions" []
                          ,Rose "Implicit Imports of Definitions" []
                          ]
                      ,Rose "System Definition and Implementation Modules" []
                      ]
                  ,Rose "DEFINING FUNCTIONS AND CONSTANTS"
                      [Rose "Functions" []
                      ,Rose "Patterns" []
                      ,Rose "Guards" []
                      ,Rose "Expressions"
                          [Rose "Lambda Abstraction" []
                          ,Rose "Case Expression and Conditional Expression" []
                          ]
                      ,Rose "Local Definitions"
                          [Rose "Let Expression: Local Definitions in Expressions" []
                          ,Rose "Where Block: Local Definitions in a Function Alternative" []
                          ,Rose "With Block: Local Definitions in a Guarded Alternative" []
                          ,Rose "Let-Before Expression: Local Constants defined between Guards" []
                          ]
                      ,Rose "Defining Constants"
                          [Rose "Selectors" []]
                      ,Rose "Typing Functions"
                          [Rose "Typing Curried Functions" []
                          ,Rose "Typing Operators" []
                          ,Rose "Typing Partial Functions" []
                          ,Rose "Explicit use of the Universal Quantifier in Function Types" []
                          ,Rose "Functions with Strict Arguments" []
                          ]
                      ]
                  ,Rose "PREDEFINED TYPES"
                      [Rose "Basic Types: Int, Real, Char and Bool"
                          [Rose "Creating Constant Values of Basic Types" []
                          ,Rose "Patterns of Basic Types" []
                          ]
                      ,Rose "Lists"
                          [Rose "Creating Lists" []
                          ,Rose "List Patterns" []
                          ]
                      ,Rose "Tuples"
                          [Rose "Creating Tuples" []
                          ,Rose "Tuple Patterns" []
                          ]
                      ,Rose "Arrays"
                          [Rose "Creating Arrays and Selection of field Elements" []
                          ,Rose "Array Patterns" []
                          ]
                      ,Rose "Predefined Type Constructors" []
                      ,Rose "Arrow Types" []
                      ,Rose "Predefined Abstract Types" []
                      ]
                    ,Rose "DEFINING NEW TYPES"
                      [Rose "Defining Algebraic Data Types"
                          [
                           Rose "Using Constructors in Patterns" []
                          ,Rose "Using Higher Order Types" []
                          ,Rose "Defining Algebraic Data Types with Existentially Quantified Variables" []
                          ,Rose "Defining Algebraic Data Types with Universally Quantified Variables" []
                          ,Rose "Strictness Annotations in Type Definitions" []
                          ,Rose "Semantic Restrictions on Algebraic Data Types" []
                          ]
                      ,Rose "Defining Record Types"
                          [Rose "Creating Records and Selection of Record Fields" []
                          ,Rose "Record Patterns" []
                          ]
                      ,Rose "Defining Synomym Types" []
                      ,Rose "Defining Abstract Data Types"
                          [Rose "Defining Abstract Data Types with Synonym Type Definition" []]
                      ]
                    ,Rose "OVERLOADING"
                      [Rose "Type Classes" []
                      ,Rose "Functions Defined in Terms of Overloaded Functions" []
                      ,Rose "Instances of Type Classes Defined in Terms of Overloaded Functions" []
                      ,Rose "Type Constructor Classes" []
                      ,Rose "Overlapping Instances" []
                      ,Rose "Internal Overloading" []
                      ,Rose "Defining Derived Members in a Class" []
                      ,Rose "A Shorthand for Defining Overloaded Functions" []
                      ,Rose "Classes Defined in Terms of Other Classes" []
                      ,Rose "Exporting Type Classes" []
                      ,Rose "Semantic Restrictions on Type Classes" []
                      ]
                  ,Rose "GENERIC PROGRAMMING"
                      [Rose "Basic Ideas Behing Generic Programming" []
                      ,Rose "Defining Generic Functions" []
                      ,Rose "Deriving Generic Functions" []
                      ,Rose "Applying Generic Functions" []
                      ,Rose "Using Constructor Information" []
                      ,Rose "Generic Functions and Uniqueness Typing" []
                      ,Rose "Exporting Generic Functions" []
                      ]
                  ,Rose "DYNAMICS"
                      [Rose "Packing Expressions into a Dynamic"
                          [Rose "Packing Abstract Data Types" []
                          ,Rose "Packing Overloaded Functions" []
                          ,Rose "Packing Expressions of Unique Type" []
                          ,Rose "Packing Arguments of Unknown Type" []
                          ,Rose "Using Dynamic Typing to Defeat the Static Type System" []
                          ]
                      ,Rose "Unpacking Dynamics Using a Dynamic Pattern Match"
                          [Rose "Unpacking Abstract Data Types" []
                          ,Rose "Unpacking of Overloaded Functions" []
                          ,Rose "Unpacking Expressions of Unique Type" []
                          ,Rose "Checking and Unifying Types Schemes using Type Pattern Variables" []
                          ,Rose "Checking and Unifying Unknown Types using Overloaded Type Variables" []
                          ]
                      ,Rose "Type Safe Communication using Dynamics" []
                      ,Rose "Architecture of the implementation" []
                      ,Rose "Semantic Restrictions on Dynamics" []
                      ]
                  ,Rose "UNIQUENESS TYPING"
                      [Rose "Basic Ideas behind Uniqueness Typing" []
                      ,Rose "Attribute Propagation" []
                      ,Rose "Defining New Types with Uniqueness Attributes" []
                      ,Rose "Uniqueness and Sharing"
                          [Rose "Higher Order Uniqueness Typing" []
                          ,Rose "Uniqueness Type Coercions" []
                          ]
                      ,Rose "Combining Uniqueness Typing and Overloading"
                          [Rose "Constructor Classes" []]
                      ,Rose "Higher-Order Type Definitions" []
                      ,Rose "Destructive Updates using Uniqueness Typing" []
                      ]
                  ,Rose "STRICTNESS, MACROS AND EFFICIENCY"
                      [Rose "Annotations to Change Lazy Evaluation into Strict Evaluation"
                          [Rose "Advantages and Disadvantages of Lazy versus Strict Evaluation" []
                          ,Rose "Strict and Lazy Context" []
                          ,Rose "Space Consumption in Strict and Lazy Context" []
                          ,Rose "Time Consumption in Strict and Lazy Context" []
                          ,Rose "Changing Lazy into Strict Evaluation" []
                          ]
                      ,Rose "Defining Graphs on the Global Level" []
                      ,Rose "Defining Macros" []
                      ,Rose "Efficiency Tips" []
                      ]
                  ,Rose "FOREIGN LANGUAGE INTERFACE"
                      [Rose "Foreign Export" []
                      ,Rose "Using ABC instructions" []
                      ]
                  ]
