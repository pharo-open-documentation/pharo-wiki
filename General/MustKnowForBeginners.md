# Must know for beginners

This page includes knowledge about thing the Pharo community think important to know as a Pharo beginner.


- [Navigate into the code](#navigate-into-the-code)
  * [Browse, Senders and Implementors](#browse-senders-and-implementors)
  * [Method source with it](#method-source-with-it)
- [Interrupt the Pharo process](#interrupt-the-pharo-process)
- [Debugging facilities](#debugging-facilities)

## Navigate into the code

Pharo offers a lot of ways to navigate into the code, and most of those are used daily by Pharo developers. This section will highlight the most important ones.

### Browse, Senders and Implementors

When reading code we often need to open an entity or to check who is using an entity. To do that, Pharo has different commands:
* **Browse** (`CMD/CTRL + b`): This command will open a new system browser on the class you are currently focusing
* **Senders/References** (`CMD/CTRL + n`): This command will open a message browser with all the methods calling the method/symbol you are currently focusing, or, in the case where you are focusing a class, the references to this class. 
* **Implementors** (`CMD/CTRL + m`):  This command will open a message browser with all the methods whose name is the name of the method/symbol you are currently focusing, or, in the case where you are focusing a class, the references to this class like `browse`. 

Alternatively, it is possible to navigate the code using the mouse and the keyboard. In most text editors of Pharo you can use the shortcut `CMD + (Shift +) click` on OSX or `Alt + (Shift +) right click` on Windows/Linux to browse senders, implementors, classes and references. 

Using `CMD + click` on OSX or `Alt + right click` on Windows/Linux allows one to:
- Browse the implementors when you click on a method.
- Browse the class when you click on a class.
- Browse the class or implementors of a method represented by a symbol when you click on a symbol. 

Using `CMD + (Shift +) click` on OSX or `Alt + (Shift +) right click` on Windows/Linux allows one to:
- Browse the senders when you click on a method.
- Browse the references to a class when you click on a class.
- Browse the references to a class or senders of a method represented by a symbol when you click on a symbol. 

> Warning: The click shortcuts on symbols are only working starting from Pharo 9. In previous version, it will browse the ByteSymbol class. 

### Method source with it

It often happens that we want to see how a feature is done, or we want to edit something but we do not know *where* is the implementation. If we have the name of a menu for example we can use it to find every places in the image were the name of the menu is written. 

To do that we need to open a playground and type the text (or select some text in an editor) then select on a right click `Code search...` then `Method source with it`.

This will open a message browser with all the methods/class comments containing the string of code we are looking for.

## Interrupt the Pharo process

Pharo currently run in one native thread. If you launch a method taking a lot of time to run, or if you have an infinite loop in your code, you might want to interrupt the process. 

It is possible to do that in Pharo with the shortcut `CMD/ALT + .`.

> This feature will work in most cases. However, sometimes it might not work, e.g. when a faulty code will have fill up too much memory, or when a process is executing outside of the VM (FFI calls).

## Debugging facilities

Pharo includes multiple ways to help with debugging. This section gives some of them.

`Object` implements multiple debugging messages that are useful. They can be sent to any objects in your code:

- `#halt` : This method will stop the execution of your code when it is called and open a debugger. (/!\ Do not use it in a code called multiple times in a fork, or system critical loops or you cqn regret it by breaking your image. In this case, it is advise to not save your image in this kind of unstable state)
- `#haltOnce` : This method stops the execution and raise a debugger only the first time it is called. To enable it once more, use `Menubar -> Debbuging -> Enable all break/inspect once`.
- `#haltIf:` : This method takes a block as parameter and will stop the execution and open a debugger if the block as parameter returns `true`.
- `#haltOnCount:` : This method takes a number as argument and will stop the execution and open a debugger when it will be called at least the number of time given as argument. Reset the counter via `Menubar -> Debbuging -> Reset Counters`.
- `#inspect` : This message can be called on any object and will open an inspector for the receiver.
- `#inspectOnce` : This message works like #haltOnce for inspecting.

**Example**

``` Smalltalk

myMethod
	
	| temp |
	self doSomething.

	"If you think an infinite loop might happen here, #haltOnCount: might help"
	self haltOnCount: 1000.

	temp := self doSomethingElse.
	
	"Inspect the first value of #temp"	
	temp inspectOnce.

	"Debug only in certain conditions"
	self  haltIf: [ temp isKindOf: ObjectIShouldNotGetHere ].

	1 to: 1000 do: [ :e | 
	
	"I want to stop only one time here to check something."
	self haltOnce.

	self doSomethingWith: e ]

```

