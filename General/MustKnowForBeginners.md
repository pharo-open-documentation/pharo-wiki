# Must know for beginners

This page includes knowledge about thing the Pharo community think important to know as a Pharo beginner.


- [Navigate into the code](#navigate-into-the-code)
  * [Browse, Senders and Implementors](#browse-senders-and-implementors)
  * [Method source with it](#method-source-with-it)
- [Interupt the Pharo process](#interupt-the-pharo-process)
- [Debuging facilities](#debuging-facilities)

## Navigate into the code

Pharo offers a lot of way to navigate into the code, and most of those are used daily by Pharo developers. This section will highlight the most important ones.

### Browse, Senders and Implementors

When reading code we often need to open an entity or to check who is using an entity. To do that, Pharo has different commands:
* **Browse** (`CMD/CTRL + b`): This command will open a new system browser on the class you are currently focusing
* **Senders** (`CMD/CTRL + n`): This command will open a message browser with all the methods calling the method/symbol you are currently focusing. 
* **Implementors** (`CMD/CTRL + m`):  This command will open a message browser with all the methods whose name is the name of the method/symbol you are currently focusing. 

### Method source with it

It often happens that we want to see how a feature is done, or we want to edit something but we do not know *where* is the implementation. If we have the name of a menu for example we can use it to find every places in the image were the name of the menu is written. 

To do that we need to open a playground and type the text (or select some text in an editor) then select on a right click `Code search...` then `Method source with it`.

This will open a message browser with all the methods/class comments containing the string of code we are looking for.

## Interupt the Pharo process

Pharo currently run in one native thread. If you launch a method taking a lot of time to run, or if you have an infinite loop in your code, you might want to interupt the process. 

It is possible to do that in Pharo with the shortcut `CMD/CTRL + .`.

> This feature will work in most cases but sometime it might not work because a faulty code will have fill up too much the memory.

## Debuging facilities

Pharo includes multiple way to help with debugging. This section will explains some of them.

`Object` implements multiple debugging messages that are useful. They can be sent to any objects in your code.

- `#halt` : This method will stop the executing of your code when it is call and open a debugger. (/!\ Do not use it in a code call multiple times in a fork or you might break your image)
- `#haltOnce` : This method will stop the execution and raise a debugger the first time it is called. To enable it once more, use `Menubar -> Debbuging -> Enable all break/inspect once`.
- `#haltIf:` : This method takes a block as parameter and will stop the execution and open a debugger if the block as parameter returns `true`.
- `#haltOnCount:` : This method takes a number as argument and will stop the execution and open a debugger when it will be called at least the number of time given as argument. Reset the counter via `Menubar -> Debbuging -> Reset Counters`.
- `#inspect` : This message can be called on any object and will open an inspector for the receiver.
- `#inspectOnce` : This message works like #haltOnce for inspecting.

**Example**

``` Smalltalk

myMethod
	
	| temp |
	self doSomething.

	"If you think an infinit loop might happen here, #haltOnCount: might help"
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

