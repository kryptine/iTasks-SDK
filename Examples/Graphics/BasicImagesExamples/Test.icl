module Test

import Data.Map
import qualified Data.Map as DM
import qualified Data.Set as DS
import StdArray, StdBool, StdEnum, StdFunc, StdInt, StdMisc, StdReal, StdString, StdTuple
from   StdList import ++, flatten, init, last, instance length [], map, repeat, repeatn, removeDup
import Graphics.Scalable.Image
import Graphics.Scalable.Internal.Types
import Graphics.Scalable.Internal.Image`
import printstuff

tagsource :: *TagSource
tagsource = [(ImageTagUser i "FX",ImageTagUser i "FX") \\ i <- [0..]]

newImgTables :: ImgTables m
newImgTables
  = {ImgTables | imgEventhandlers = 'DM'.newMap
               , imgNewFonts      = 'DS'.newSet
               , imgNewTexts      = 'DM'.newMap
               , imgMasks         = 'DM'.newMap
               , imgLineMarkers   = 'DM'.newMap
               , imgPaths         = 'DM'.newMap
               , imgSpans         = 'DM'.newMap
               , imgGrids         = 'DM'.newMap
               , imgTags          = 'DM'.newMap
               , imgUniqIds       = 0
    }

addNewFontSpans :: !ImgFonts !FontSpans -> FontSpans
addNewFontSpans new_fonts cur_fonts
	= seq ['DM'.put new_font (new_font.FontDef.fontysize * 0.25) \\ new_font <- 'DS'.toList new_fonts] cur_fonts

addNewTextsSpans :: !ImgTexts !TextSpans -> TextSpans
addNewTextsSpans new_txts cur_txts
	= seq ['DM'.alter (merge ('DM'.fromList [(txt,toReal (10*size txt)) \\ txt <- 'DS'.toList txts])) fontdef \\ (fontdef,txts) <- 'DM'.toList new_txts] cur_txts
where
	merge :: !(Map String TextSpan) !(Maybe (Map String TextSpan)) -> Maybe (Map String TextSpan)
	merge ws` (Just ws) = Just ('DM'.union ws` ws)
	merge ws` nothing   = Just ws`

Start
  #! (img,{ImgTables | imgEventhandlers=es,imgNewFonts=new_fonts,imgNewTexts=new_txts,imgMasks=masks,imgLineMarkers=markers,imgPaths=paths,imgSpans=spans,imgGrids=grids,imgTags=tags})
                  = toImg (test_image tagsource) newMap newMap newImgTables
  #! font_spans   = addNewFontSpans new_fonts 'DM'.newMap
  #! text_spans   = addNewTextsSpans new_txts 'DM'.newMap
  = case resolve_all_spans tags font_spans text_spans img masks markers paths spans grids of
      Error error = abort error
      Ok (img,masks,markers,paths,spans,grids)
                  = ( toString (length ('DM'.toList paths)) +++ " paths"
                    , toString (length ('DM'.toList spans)) +++ " spans"
                    , toString (length ('DM'.toList grids)) +++ " grids", '\n'
                    , map (\(t,no) -> toString (t,no)) ('DM'.toList tags), '\n'
                    , map (\a -> toString a <+ "\n") ('DM'.toList spans)//, '\n'
                    //, indentImg "" img
                    )

font         = normalFontDef "Times" 16.0
txt          = "Hello World!" // 12 chars, 120.0 px wide
lucida       = normalFontDef "Lucida Console"
times        = normalFontDef "Times New Roman"
arial        = normalFontDef "Arial"
arial_narrow = normalFontDef "Arial Narrow"
none         = toSVGColor "none"
white        = toSVGColor "white"

test_image = test_image2

test_image1 tags
	= pair (arrow, arrow`) tags
where
	arrow  = polygon [(px zero,px -10.0),(px 55.0,px -10.0),(px 50.0,px -30.0),(px 85.0,px zero)
	                 ,(px 50.0,px  30.0),(px 55.0,px  10.0),(px zero,px  10.0)
	                 ]
	arrow` = polygon [(px -10.0,px zero),(px -10.0,px 55.0),(px -30.0,px 50.0),(px zero,px 85.0)
	                 ,(px  30.0,px 50.0),(px  10.0,px 55.0),(px  10.0,px zero)
	                 ]

/**	pair (img1,img2) tags = image:
	@image uses @tags to put @img1 and @img2 beside each other on a host that fits their width and height.
*/
pair :: (Image m,Image m) *TagSource -> Image m
pair (img1,img2) [(t1,ut1),(t2,ut2):tags]
	= beside [] [] Nothing []
	      [ overlay [(AtMiddleX,AtMiddleY)] [] [tag ut1 img1] host
	      , overlay [(AtMiddleX,AtMiddleY)] [] [tag ut2 img2] host
	      ] NoHost
where
	(w1,h1) = (imagexspan t1,imageyspan t1)
	(w2,h2) = (imagexspan t2,imageyspan t2)
	host    = Host (rect (maxSpan [w1,w2]) (maxSpan [h1,h2]) <@< {fill = none})


/** roses model tags = image:
	@image shows the use of @tags to display an arbitrary rose tree structure.
*/
test_image2 tags
	= margin (px 10.0) (fst (show_rose show_my_node my_rose_tree tags))
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
/*                  ,Rose "MODULES AND SCOPES"
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
                          [Rose "Using Constructors in Patterns" []
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
*/                  ]

test_image3 tags
	= margin (px zero,px 100.0,px zero,px zero) (
	    grid (Columns 3) (RowMajor,LeftToRight,TopToBottom) (updateAt` 6 (AtLeft,AtMiddleY) (repeat (AtLeft,AtTop))) [] [] []
	       [ above [] [] Nothing [] [empty  (px 200.0) (px 100.0),                   txts ["empty (px 200.0) (px 100.0)"]] NoHost
	       , above [] [] Nothing [] [margin (px zero,px 5.0,px zero,px 5.0) (rect (px 200.0) (px 100.0))
	                                                                               , txts ["rect (px 200.0) (px 100.0)"]] NoHost
	       , above [] [] Nothing [] [rect   (px 200.0) (px 100.0) <@< {fill = none}, txts ["rect (px 200.0) (px 100.0)"
	                                                                                      ,"<@< {fill = toSVGColor \"none\"}"
	                                                                                      ]] NoHost
	       , above [] [] Nothing [] [circle  (px 100.0),                             txts ["circle (px 100.0)"]] NoHost
	       , above [] [] Nothing [] [ellipse (px 200.0) (px 100.0),                  txts ["ellipse (px 200.0) (px 100.0)"]] NoHost
	       , above [] [] Nothing [] [overlay [] []
	                                    [text (times 100.0) "Hey World!"]
	                                    (Host (empty (px 200.0) (px 100.0))),        txts ["text (normalFontDef \"Times New Roman\" 100.0) \"Hey World!\""]] NoHost
	       , above [] [] Nothing [] [xline (px 200.0),                               txts ["xline (px 200.0)"]] NoHost
	       , above [AtMiddleX]
	                  [] Nothing [] [yline (px 100.0),                               txts ["yline (px 100.0)"]] NoHost
	       , above [] [] Nothing [] [line (px 200.0) (px -100.0),                    txts ["line (px 200.0) (px -100.0)"]] NoHost
	       , above [] [] Nothing [] [line (px 200.0) (px  100.0),                    txts ["line (px 200.0) (px  100.0)"]] NoHost
	       , above [] [] Nothing [] [polygon  offsets,                               txts ["polygon Nothing" : offsetsts]] NoHost
	       , above [] [] Nothing [] [polyline offsets,                               txts ["polyline Nothing": offsetsts]] NoHost
	       ] NoHost
	  )
where
	txts lines	= margin (px 5.0,px 10.0,px 10.0,px 10.0) (above [] [] Nothing [] (map (text (lucida 10.0)) lines) NoHost)
	offsets		= [(zero,zero),(px 200.0,px 100.0),(px 200.0,zero),(zero,px 100.0)]
	offsetst	= ["(zero,    zero    )"
	              ,"(px 200.0,px 100.0)"
	              ,"(px 200.0,zero    )"
	              ,"(zero,    px 100.0)]"
	              ]
	offsetsts	= ["        " +++ s +++ t \\ t <- offsetst & s <- ["[" : repeat ","]]

	updateAt` i a as = [  if (j==i) a x
	                   \\ j <- [0..]
	                    & x <- as
	                   ]


/** clean model tags = image:
	@image is inspired by an image that was displayed by Marc Schoolderman during the lab session of friday afternoon, may 22 2015.
*/
test_image4 tags
	= overlay (repeat (AtMiddleX,AtMiddleY)) []
	     [ star 31 (r_in,r_out)
	     , circle (px r_in *. 1.6) <@< {strokewidth = px bandwidth} <@< {stroke = white}
	     , rotate (rad (pi * 0.25)) (circular (px r_in *. 0.8) (2.0 * pi) (repeatn 4 (circle (px bandwidth *. 0.8))))
	     , rotate (rad (pi * 0.32)) (circular (px zero)        (2.0 * pi) (map (arctext (px r_in *. 1.10) (0.4 * pi) narrowfont) ["NO VIRUSES","NO SPYWARE","NO VIRUSES","NO SPYWARE"]))
	     , above (repeat AtMiddleX) [] Nothing [] (map (((>@>) {fill = white}) o ((>@>) {stroke = white}) o (text bigfont)) ["100%", "CLEAN"]) NoHost
	     ] NoHost
where
	r_out      = 100.0
	r_in       = 90.0
	bandwidth  = r_in * 0.2
	bigfont    = {arial (r_in * 0.35) & fontweight = "bolder"}
	narrowfont = arial_narrow (r_in * 0.22)

/**	star n (r_in,r_out) = image:
	@image displays a star shaped image that has @n rays. The inner radius is @r_in. The rays extend to @r_out.
*/
star :: Int (Real,Real) -> Image m
star n (r_in,r_out)
	= polygon (flatten
	     [  [(px r_out *. (cos (angle * (toReal outer_corner))), px r_out *. (sin (angle * (toReal outer_corner))))
	        ,(px r_in  *. (cos (angle * (toReal inner_corner))), px r_in  *. (sin (angle * (toReal inner_corner))))
	        ]
	     \\ outer_corner <- [0, 2 .. 2*n], let inner_corner = outer_corner+1
	     ])
where
	angle = pi / (toReal n)

/**	arctext r a font txt = image:
	@image displays the content of @txt along an arc of radius @r, starting at angle @a, using @font.
*/
arctext :: Span Real FontDef String -> Image m
arctext r a font txt
	= circular r a [rotate (rad pi) (text font (toString c)) \\ c <-: txt]

/**	circular r a imgs = image:
	displays @imgs along an arc of radius @r, starting at angle @a.
*/
circular :: !Span !Real ![Image m] -> Image m
circular r a imgs
  #! n      = length imgs
  #! sign_a = toReal (sign a)
  #! a`     = normalize (rad a)
  #! alpha  = (toRad a`) / (toReal n)
  = overlay (repeat (AtMiddleX,AtMiddleY))
        [(~r *. cos angle,~r *. sin angle) \\ i <- [0.0, sign_a ..], angle <- [i*alpha - 0.5*pi]]
        [rotate (rad (i*alpha)) img \\ i <- [0.0, sign_a ..] & img <- imgs]
        (Host (empty (r *. 2) (r *. 2)))              // BUG: using NoHost creates incorrect image (offset to left)


test_image5 tags=:[(t0,ut0),(t1,ut1):_]
	= tag ut0 (
	     grid (Rows 1) (RowMajor,LeftToRight,TopToBottom) [] [px 300.0,px 300.0] [] []
	        [rect (columnspan t0 0) (px 100.0)
	        ,rect (columnspan t0 1) (px 200.0)
	        ] (Host (empty (px 400.0) (px 400.0)))
	  )

test_image6 tags
	= overlay [] [(px 10.0,px 10.0)]
			[polyline [(px 0.0,px 0.0),(px 25.0,px 25.0),(px 50.0,px 0.0),(px 75.0,px 25.0),(px 100.0,px 0.0),(px 85.0,px 100.0),(px 15.0,px 100.0),(px 0.0,px 0.0)]
			    <@< {stroke = white} <@< {strokewidth = px 3.0}
			]
			(Host (rect (px 120.0) (px 120.0)))
