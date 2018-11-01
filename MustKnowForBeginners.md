# Must know for beginners

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

