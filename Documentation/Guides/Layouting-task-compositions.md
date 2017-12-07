##### Layouting Task Compositions

## Introduction
In this guide we introduce you to the different task layout modifiers.
We assume you are already familiar with writing programs with iTasks.

## Sidebar
The function signature for the sidebar decorator is as follows:
```clean
arrangeWithSideBar :: !Int !UISide !Int !Bool -> Layout
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

## Conclusion

In this guide we have shown how you how to decorate tasks with layout combinators so that you can change the way they appear in the client.
Not all combinators are covered, but they can be found in `iTasks.UI.Layout.Common`.
