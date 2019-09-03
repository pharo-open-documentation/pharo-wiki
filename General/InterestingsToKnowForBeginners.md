# Interesting things to know for beginners

This page will expose different things that might be interesting to know for beginners. Tools, API, shortcuts...

The goal is to ease the use of Pharo for beginners.

- [API](#api)
- [Evaluatable objects (Blocks and Symbols)](#evaluatable-objects--blocks-and-symbols-)
- [Tools](#tools)
  * [Calypso System Browser](#calypso)
  * [Dependency Analyzer](#dependency-analyzer)
  
- [Flags](#flags)
- [Useful pragmas](#useful-pragmas)
  * [Modify IDE](#modify-ide)
  * [Influence execution of methods](#influence-execution-of-methods)
  * [Influence Debugger](#influence-debugger)

## API

The Pharo API is extensive. In this section we will explain various methods existing in Pharo that have proven to be useful in our experience.

**String class>>#streamContents:**

String concatenation can be costly because it will create a new string instance and copy the content of the two strings at each concatenation. To make it more efficient, Pharo includes a method to create a String via a stream:

```Smalltalk
'This is a ', 'strong concatenation ' , 'that is not really' , ' ' , ' efficient'.

String streamContents: [ :aStream | aStream << 'This is a '<< 'strong concatenation ' << 'that is ' << ' ' << ' efficient' ]
```

> TODO

## Evaluatable objects (Blocks and Symbols)
Pharo's Blocks and Symbols have an interesting property: they are polymorphic through `#value:` and `#cull:` methods.

For example, the two code snippets below are equivalent:

```Smalltalk
#class value: Object new
```

```
[ :o | o class ] value: Object new
```

They both return the class of an object.

This means that for methods taking a one-argument block as a parameter, this block can be replaced by a symbol. The effect is that the unary method named by the symbol will be executed with the object as receiver.

The difference between `#value:` and `#cull:` is that `#value:` requires a parameter when `#cull:` can work with or without a parameter.

The common usage of this feature is via iterators. For example, if you want to collect the result of the `name` method of all objects in a collection you can either do:

```
objectCollection collect: [ :o | o name ].
```

or

```
objectCollecton collect: #name
```

The second way is shorter and often more readable.

## Tools

> TODO

### Calypso (System browser)

- Display the differences between two methods
	* Click on a method
	* Hold down the SHIFT-key
	* Click on the second method
	* Click on the tab Diff to see the differences
	
- Editing 2 methods side by side
	* Click on a method
	* Hold down the CTRL-key
	* Click on the second method
	* Two tabs are displayed with the source code of the two methods.

### Dependency Analyzer

> TODO

## Flags

It is possible to "flag" methods in Pharo in order to be able to retrieve them later. This can be done by sending the `#flag:` message to self with a symbol as parameter which is the flag to be used.

Commonly used flags are:
- `#todo` to mark code that needs work to be done.
- `#hack` to mark hackish code.
- `#broken` to mark broken code.
- `#clean` to mark code that needs cleaning.
- `#toCheck` to mark code that should be checked later.
- `#pharoX` to mark code present for compatibility with `X` being the version of Pharo.

Flags will not modify the execution of a method. It is just a way to tag it.

For example, the method below will return `42` if executed as if there was no call to `#flag:` method.

```
myUnfinishedMethod
	self flag: #TODO. "Some process to be done here."
	^ 42
```

To retrieve methods for which you set flag(s), just search for senders of your flag. For example, to retrieve methods flagged with `#todo`, either inspect the result of the following script: `#todo senders` or just select the symbol and press Meta+N (shortcut for "Browse senders").

## Useful pragmas

Pragmas allow one to tag methods. They are similar to Java's annotations. Some pragmas allow the developer to modify the IDE or to influence execution of methods. This section presents some of these useful pragmas.

### Modify IDE

- `<script>` for class-side methods, adds a button next to the method name in the IDE. This button, when clicked, executes the method.
- `<script: 'Smalltalk code to run'>` is similar to `<script>` but the button executes the Smalltalk code held by the string provided as parameter to the pragma.
- `<sampleInstance>` for class-side methods, adds a button next to the method name in the IDE. When clicked, the method is executed and an inspector is opened on its result.
- `<worldMenu>` for class-side methods, add a menu entry in the Pharo menu. The method with this pragma should take a builder as parameter. You can browse the senders of the `worldMenu` pragma to find the API.
- `<systemsettings>` for class-side methods, add an entry in the Pharo settings. The method with this pragma should take a builder as parameter. You can browse the senders of the `systemsettings` pragma to find the API.
- `<haltOrBreakpointForTesting>` should be used in methods using breakpoints that should not be listed in the Breakpoint browser. For example, it can be used in examples using breakpoints to highlight a feature of the example.

### Influence execution of methods

- `<expectedFailure>` for instance-side methods of subclasses of `TestCase`, will make the test green when it is failing. But, althrough we expect that the test fails, a success of the test (i.e. not assertion missed) will show as a failure.

### Influence Debugger
- `<debuggerCompleteToSender>` allows one to tell the debugger to open itself on the method that sends the message corresponging to the method holding `<debuggerCompleteToSender>` pragma.

For example, let us consider these two methods:
```st
foo
	<debuggerCompleteToSender>
	Error signal: 'foo'
```

```st
fooCaller
	self foo
```

Because of the `<debuggerCompleteToSender>`, when `#fooCaller` is executed, the debugger will show `#fooCaller` method source code.
If the pragma was not there, the debugger would show `#foo` method source code.
