# Playground
Is a window into which you can type input. It can be used for any purpose, 
but is most often used for typing Pharo expressions and executing them via Do it.

You can open it with the shortcut `⌘OW`or using the menu in the image: 

![open playground from the image](playground_open_from_image.png)

## Basic usage
We can use Playground to quickly execute Pharo code, just writing something and doing some operations on the code, such as:

1. __Do it__ or using the shortcut `⌘D`: Execute the code

![Do it command on playground](playground_do_it.png)

2. __Print it__ or using the shortcut `⌘P`: Compiles the expression, executes it, sends the message printString to the result, and displays the resulting string.

![Print it command on playground](playground_print_it.png)
![Print it result on playground](playground_print_result.png)

3. __Do it and go__ `⌘G` :  additionally opens a navigable inspector on the side of the playground. It allows us to navigate the object structure.

![Do it and go on the playground](playgorund_do_it_and_go.png)

4. __Inspect it__ `⌘I` : If you want to understand not only the result value but the result: The inspector is an extremely useful tool that allows you to browse and interact with any object in the system.

![Inspect on Time now](playground_inspect_on_Time_now.png)

The title tells us that `11:55:11.561177 am` is an instance of the class `Time`. The top panel allows us to browse the instance variables of an object and their values. The bottom panel can be used to write expressions to send messages to the object. 

5. __Basic inspect it__ `⇧⌘I` : Opens the classic inspector that offers a more minimal GUI and live updates of changes to the object.
6. __Debug it__ `⇧⌘D` : Opens the debugger on the code.
7. __Profile it__ : Profiles the code with the Pharo profile tool which shows how much time is spent for each message sent.
8. __Code search__ offers several options provided by System Browser, such as browsing the source code of an expression, searching for senders and implementors, and so on.


You can also use the playground for typing any text that you would like to remember, such as to-do lists or instructions for anyone who will use your image.

![Text on the playground](Playgorund_with_only_text.png)

## Some Advices
### Doing vs. Printing
The diference between this two actions is visual. try to execute something like `1+5` with `Do it`. Maybe you think that nothing happened because there isn't a visual clue about the execution but in fact something did happen. You send the message `+` with argument `5` to the number `1`.

Now try to do it with Print it, the same will happen but also you will see the result printed just aside:

![Print the sum using the playground](playground_print_sum.png)

That could be useful if you want to understand the result value.



