# Playground

- [Playground](#playground)
  - [Basic usage](#basic-usage)
    - [Do it](#do-it)
    - [Print it](#print-it)
    - [Do it and go](#do-it-and-go)
    - [Inspect it](#inspect-it)
    - [Basic inspect it](#basic-inspect-it)
    - [Debug it ](#debug-it )
    - [Profile it](#profile-it)
    - [Code search](#code-search)
  - [Some Advices](#some-advices)
    - [Doing vs. Printing](#doing-vs-printing)
  - [Advance usage](#advance-usage)

Playground is a tool that is most often used for typing Pharo expressions and executing them but it can be used as a scratchpad area where fragments of Pharo code can be entered, stored, edited, and evaluated.

You can open it with the shortcut `cmd/ctrl + o + w` or using the menu entry from the menubar: 

![open playground from the image](Playground_open_from_image.png)

## Basic usage
The Playground can be used to quickly execute Pharo code. You write something and then can do operations on the code, such as:

### Do it 
Shortcut: `⌘D` It Execute the code/expression

![Do it command on playground](Playground_do_it.png)

### Print it
Shortcut: `cmd/ctrl + p` It Compiles the expression, executes it, sends the message `printString` to the result, and displays the resulting string.

![Print it command on playground](Playground_print_it.png)
![Print it result on playground](Playground_print_result.png)

### Do it and go
Shortcut: `cmd/ctrl + g` :  Execute the code and additionally opens a navigable inspector on the side of the playground. It allows us to navigate the object structure.

![Do it and go on the playground](Playground_do_it_and_go.png)

### Inspect it
Shortcut: `cmd/ctrl + i` : Execute the code and additionally opens a navigable inspector outside of the playground. It allows us to navigate the object structure.

![Inspect on Time now](Playground_inspect_on_Time_now.png)

> The title tells us that `11:55:11.561177 am` is an instance of the class `Time`. The top panel allows us to browse the instance variables of an object and their values. The bottom panel can be used to write expressions to send messages to the object. 

### Basic inspect it
Shortcut: `cmd/ctrl + shift + i` : Opens a minimal inspector on the result of the expression. (Might be useful if you work on the Inspector for example)
### Debug it
Shortcut: `cmd/ctrl + shift + d` : Opens a debugger on the code with the context at the start of the snippet.

![Debug it on Time now](Playground_debug_it.png)

>The Debugger is a tool that not only lets you inspect the run-time stack of your program when an error is raised, but it also enables you to interact with all of the objects of your application, including the source code. In many cases you can modify your source code from the debugger and continue executing. The debugger is especially effective as a tool to support test-first development in tandem with SUnit.

### Profile it
Profiles the code with the Pharo profile tool which shows how much time is spent for each message sent.
### Code search
offers several options provided by System Browser, such as browsing the source code of an expression, searching for senders and implementors, and so on.

You can also use the playground for typing any text that you would like to remember, such as to-do lists or instructions for anyone who will use your image.

![Text on the playground](Playground_with_only_text.png)

## Some Advices
### Doing vs. Printing
The diference between this two actions is visual. try to execute something like `1+5` with `Do it`. Maybe you think that nothing happened because there isn't a visual cue about the execution but in fact something did happen. You sent the message `+` with argument `5` to the number `1`.

Now try to do it with Print it, the same will happen but also you will see the result printed just aside:

![Print the sum using the playground](Playground_print_sum.png)

That could be useful if you want to understand the result value.

## Advance usage

>TODO
