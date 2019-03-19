# Laying out Task Compositions #

## Introduction ##
In this guide we introduce you to the different task layout modifiers.
We assume you are already familiar with writing programs with iTasks.

All combinators are of the type `:: Layout`.
It is not advised to build your own layouts but you can create them using combinators.
However, creating your own layout rules is not in the scope of this tutorial.

Layout modifications can be applied using the `tune` class and the convenient `<<@` infix operator.
For example, to place two tasks horizontally instead of the vertical default:

```clean
task1 -||- task2 <<@ ArrangeHorizontal
```

Not all layout modifications have instances for `tune`.
In those cases you can use the layout directly with the `ApplyLayout` type as follows:
```clean
task1 <<@ ApplyLayout somelayout
```

## Menu bars ##
The function signature for the menubar decorator is as follows:
```clean
arrangeAsMenu :: [[Int]] -> Layout

:: ArrangeAsMenu = ArrangeAsMenu [[Int]]
```

When the task is arranged as a menu it will convert all actions starting with a `/` to a menu action.
It is possible to combine menu actions with regular actions.
The regular actions will be converted to a buttonbar as usual.
The first and only argument contains the list of separators that should be added to the menu.
The following code shows an example of a menu:

```clean
task = (programTask
	>^* [OnAction (Action "/File/New")     $ do_something
		,OnAction (Action "/File/Open...") $ do_something
		,OnAction (Action "/File/Quit")    $ do_something
		,OnAction (Action "/Edit/Cut")     $ do_something
		,OnAction (Action "/Edit/Copy")    $ do_something
		,OnAction (Action "/Edit/Paste")   $ do_something
		,OnAction (Action "/Help")         $ do_something
		]
	) <<@ ArrangeAsMenu [[0,2]]
```

## Sidebar ##
The function signature for the sidebar decorator is as follows:
```clean
arrangeWithSideBar :: !Int !UISide !Int !Bool -> Layout

:: ArrangeWithSideBar = ArrangeWithSideBar Int UISide Int Bool
```

The first parameter is the index of the task that should become the sidebar.
Most of the time this is `0` or `1` but if you apply it to an `allTasks` for example it can be higher.
The location of the sidebar and the size follow ended by the flag for resizability.
For example, if you want to provide an interface for the user to update named strings it can be done as follows.

```clean
t :: Task String
t = (enterChoice "Pick an item to edit" [ChooseFromList id] choices
	>&> \sh->whileUnchanged sh \msh->case msh of
		Nothing = viewInformation () [] "No choice has been made"
		Just s = updateSharedInformation ("Upd " +++ s) [] (sharedStore s "")
	) <<@ ArrangeWithSideBar 0 LeftSide 150 True
where
	choices = ["string" +++ toString i\\i<-[0..20]]
```

## Panels ##
In some cases, such as when you apply a title, the user interface is automatically lifted to a panel.
However, this can also be done manually with the option of making the panel fullscreenable.

```clean
toPanel :: Bool -> Layout
:: InPanel = InPanel Bool
```

If the boolean flag is set to true, the panel includes a small icon (![](Libraries/iTasks/UI/WebPublic/css/icons/fullscreen.png)) on the bottom right that, when clicked, makes the panel full screen.
If the small icon is clicked again, the panel shrinks back to the original size again.

## Conclusion ##

In this guide we have shown how you how to decorate tasks with layout combinators so that you can change the way they appear in the client.
Not all combinators are covered, but they can be found in `iTasks.UI.Layout.Common`.
