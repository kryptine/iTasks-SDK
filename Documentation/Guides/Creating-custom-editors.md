##### Creating Custom Editors

## Introduction
In this guide we assume you are already familiar with writing programs with iTasks and have used the common task definitions for user interaction such as `updateInformation` and `viewInformation`, but find that the automagically generated GUI's are not working for your application.

In this guide we'll walk you through the process of creating an `Editor`. This is the construct that is used by tasks like `updateInformation` to render the GUI and handle user events. Editors are typed GUI building blocks that can be composed to create any GUI you like. Editors abstract away all UI concerns into a single opaque value. These building blocks define how the GUI is rendered, how events are handled and how the GUI is synchronized with the (task) value it represents.

The remainder of this document is structured as follows: First we'll look at how a custom editor can be applied in an interaction task. We'll then look at how we can configure the builtin editors of the iTask framework for basic types. We end by looking at how we can use editor combinators to build editors for more complex data structures such as records and Algebraic data types.

## Using custom editors in tasks

The first thing you need to know to work with custom editors is how you can use them in a task. Let's look at a very simple task we'll use as example:

```Clean
myTask :: Task Int
myTask = updateInformation "Change the magic number" [] 42
```

When rendered, this will give you a simple input box that allows you to update the value. But what if we wanted to use a slider component to edit the integer? We can do this with the following code:

```Clean
myTask :: Task Int
myTask = updateInformation "Change the magic number"
           [UpdateUsing (\x -> x) (\_ x -> x) slider] 42
```

The GUI will now be rendered as a nice slider. Let's look a little closer at what's going on here.

1. Adding an editor to the task. The second parameter of `updateInformation` is a list of `UpdateOption` values that allow us to change its default behavior. In this case we are using the `UpdateUsing` constructor that is defined like this:

   ```Clean
   ...
   | E.v: UpdateUsing  (a -> v) (a v -> b) (Editor v) & iTask v
   ...
   ```

   The option consists of three parts. The last is the most important one. We need to specify some value of type `Editor v`. The other two functions define how to map between the task value and the value that is being edited (of type ` v`). When we look at editor combinators later on we'll see that the mapping to the  `v` domain is not strictly necessary. By passing the identity mapping `(\x -> x) (\x _ -> x)` we stay in the same domain as the task value, so the editor we are using needs to be of type `Editor Int`. 

2. So when using  `UpdateUsing` (or `EnterUsing`,`ViewUsing` etc.) we pass a value of type `Editor v`. If we are lazy we can simply call `gEditor{|*|}` to create a the generic version that is used by default, but the goal in this guide is to specify a custom GUI. In this example we therefore use the `slider` function, which is a builtin function that returns a value of type `Editor Int`.

So to use a custom task UI we need to do two things: We need to specify an editor of the right type, and then pass it in the option list of the interaction task we are using. In the next section we'll look at the builtin editors and how we can use them to replace the generic editor function.

## Using the builtin editors

The iTask framework provides a number of builtin UI components that it can render in a browser. These are the lowest level building blocks with which all iTasks GUI's are constructed.

In the example of the previous section we have seen the `slider` editor. This editor is one of the builtin componentens in the `iTasks.UI.Editor.Controls` module. All builtin editors have no arguments, but can dynamically be configured by setting attributes. For example if we wanted to set the maximum value of the slider, we would write `slider <<@ maxAttr 42`. The tuning combinators `<<@` or `@>>` are used to set attributes on editors. This pattern is used to make it easy to create editors without the need to specify all attributes in advance. In many cases, it is not necessary to deviate from the default values of the configurable attributes. Forcing a programmer to specify them all makes our GUI code too verbose. The price we pay for this convenience is that we lose some type safety. We dynamically set arbitrary attributes on editors, whether the UI rendering code uses them or not. 

## Composing editors

Creating editors for basic values is useful, but more often we want to construct editors for composite datastructures such as records. Let's expand the slider example to show how you can compose editors.

```Clean
myTask4 :: Task Int
myTask4 = updateInformation "Change the magic number"
    [UpdateUsing (\x -> ("Mylabel",x)) (\_ (_,x) -> x) editor] 42
where
    editor :: Editor (String,Int)
    editor = (container2 label slider) <<@ directionAttr Horizontal
```

When you run this example, you'll see the same slider as before, but this time with a label "Mylabel" to the left of it. There are a few things going on here. The first new thing is the `label` builtin editor that we are using. This is an editor of type `String` and simply displays its value. The next new thing is the `container2` combinator. This is where the composition happens. This combinator takes two editors and puts them together in a container. The values are combined into a tuple, so the type of the combined editor in this case is `(String,Int)`.  There are combinators for different kinds of containers, such as `panel`, `window` or `tabset`. Because grouping editors with these combinators creates tuples, we need different versions of each depending on how many items we group together. In this case we are using `container2` to group two editors. The last thing we are doing in this example is providing the actual label. We are using the model-to-view mapping of `UpdateUsing` to add the static label to the value.

If we want to create more complex editors, it will quickly become very messy if we pass all labels and other static elements in the mapping of `UpdateUsing`. Luckily, there is a way we can embed this mapping in the editor itself. Let's take a look:

```Clean
myTask5 :: Task Int
myTask5 = updateInformation "Change the magic number"
    [UpdateUsing (\x -> x) (\_ x -> x) editor] 42
where
    editor :: Editor Int
    editor = bijectEditorValue (\x -> ("Mylabel",x)) snd
        (panel2 label slider <<@ directionAttr Horizontal)
```

In this revision, we have used a new combinator from `iTasks.UI.Editor.Modifiers`: The `bijectEditorValue` combinator. With this function we can change the type of the editor. In this case the two functions `(\x -> "Mylabel",x))` and `snd` define a bijection between the domain of the composed editor (of type `(String,Int)`) and the domain we would like our editor to work on (of type `Int`).

We now have a composed editor with a nice label, but it's still of type `Editor Int`, so we can use it as drop-in replacement for our original editor.

In the next example, we'll take it one step further and create a nice little form for a custom record:

```Clean
:: MyRecord =
    { foo :: String 
    , bar :: Int
    }
derive class iTask MyRecord

myTask6 :: Task MyRecord
myTask6 = enterInformation "Enter your data" [EnterUsing id editor]
where
    editor = bijectEditorValue (\{foo,bar} -> (foo,bar)) (\(foo,bar) -> {foo=foo,bar=bar})
                (panel2
                    (row "Footastic:" passwordField)
                    (row "Barmagic:" slider)
                ) <<@ heightAttr WrapSize
    row l e = bijectEditorValue (\x -> (l,x)) snd
                ((container2 label e) <<@ directionAttr Horizontal)
```

This example is a little more complex, but uses only things we have already seen. By constructing editors from the basic building blocks and transforming the value domain of the editors, we can construct any kind of GUI we like.

## Creating dynamic editors

One of the nice features of the generic editors is that they work for any type of data structure. You can easily create editors for recursive data types that have values of arbitrary size, not just static forms.

When you choose between different constructors of an ADT, the editor for the fields of the ADT depends on the selected constructor. You can create similar behaviour in your custom editors with the `containerc`/`panelc`/… combinators. The following example shows how this is done for a custom list type `MyList a`.

```
::: MyList a = MyNil | MyCons a (MyList a)
derive class iTask MyList

myTask7 :: Task (MyList String)
myTask7 = enterInformation "Enter the list" [EnterUsing id editor]
where
    editor = injectEditorValue (\x -> (0,x)) (Ok o snd)
        (containerc (chooseWithDropdown ["Nil","Cons"])
            [(const MyNil, emptyEditor)
            ,(const (MyCons gDefault{|*|} MyNil), consEditor)
            ] <<@ heightAttr WrapSize)

    consEditor = bijectEditorValue (\(MyCons x xs) -> (x,xs)) (\(x,xs) -> MyCons x xs)
        (container2 gEditor{|*|} editor)
```

There is quite a lot going on in this example, but the central combinator making this work is `containerc`. This combinator groups an editor for making a selection (of type `Editor Int`) with a list of editors. Based on the value of the selection editor an from the list is selected to edit the value.

The type of the list of possible editors is not `[Editor a]` but `[(a -> a, Editor a)]`. The function in the tuple is applied whenever the selection changes. This function let's the underlying value be changed based on what we choose. So in this example, if we choose `"Nil"` in our selector dropdown, the `const MyNil` function makes sure that the value is changed to `MyNil`. If we would not do this, we would only change to using another editor for the same value. When changing from `"Cons"` to `"Nil"` we would simply use the first editor (`emptyEditor`) for the `MyCons` value. This is not what you would expect. Therefore the function let's you modify the editor's value to be consistent with the selection. An additional benefit is that we can safely use partial functions when mapping to the alternative editors. In `consEditor` we can safely map between a tuple and the `MyCons` constructor with lambda's because we know that this editor will not be used with the `MyNil` constructor.

This example shows that with these combinators you can also make dynamic editors for recursive types. You can even plug in in the generic editors to create editors for higher order-types.

## Conclusion

In this guide we have shown how you can fully customize the GUI of your iTask tasks by creating editors. We have not covered all builtin editors and combinators, but just enough to get you started. You can look at the  documentation of `iTasks.UI.Editor.Controls`, `iTasks.UI.Editor.Containers` and `iTasks.UI.Editor.Modifiers` to find out all possibilities.

Although editors give you full control over the GUI of interaction tasks, they don't define full GUI's. Tasks can be composed, so the GUIs of task compositions are created from their parts. As with everything in iTasks this happens automatically by default, but if you also want to customize this process, the "Layouting Task Compositions" guide will get you started.
