# Dynamic variables

Pharo contains a system of dynamic varibales which are global variables whose value can be changed during the execution of a process.

> Be careful when you use these variables. You should carefuly think about there usefulness. They should not be used just as a global variable to make access to it easier in the code.

An example of use of a dynamic variable can be for a logger system. In such a system we can imagine that we have a default logger thaht will be used to record logs, but in some specific cases we might prefer to use a custom logger. In that case a Dynamic variable might do the work. If you wish to see it at work you can check this [logger project using it](https://github.com/jecisc/TinyLogger)

## Create a new Dynamic variable

To create a new Dynamic variable you just need to create a new subclass of `DynamicVariable`:

```Smalltalk
DynamicVariable subclass: #MyVariable
	slots: {  }
	classVariables: {  }
	package: 'MyProject'
```

## Add a default value

It is possible to asign a default value to your dynamic variable. To do so, you need to add a `default` method.

```Smalltalk
MyVariable>>default
  ^ MyDefaultObject
 ```
 
## Use your dynamic variable

To use your variable, just send the `value` message to it. If you have no default value, do not forget to manage the case where it can be nil if it can happen.

```Smalltalk
MyVariable value doSomething
```

## Change the value of the variable in a process

If you need to change the value of the variable for the execution of a specific process, just use the `value:during:` message.

```Smalltalk
MyVarible value: MyNewObject during: [
  self doAnActionS=UsingTheVariable.
]
```

## Example

Here is the way Dynamic variables are used in the `TinyLogger` project quoted above.

```Smalltalk
DynamicVariable subclass: #TinyCurrentLogger
	slots: {  }
	classVariables: {  }
	package: 'TinyLogger-Core'
```

```Smalltalk
TinyCurrentLogger>>default
	^ TinyLogger default
```

```Smalltalk
Object>>record: aString
	TinyCurrentLogger value record: aString
```

```Smalltalk
Object>>execute: aBlock recordedAs: aString
	TinyCurrentLogger value execute: aBlock recordedAs: aString
```

```Smalltalk
	TinyCurrentLogger value: (TinyLogger new addTranscriptLogger; yourself) during: [ 
		'test' record.
		TinyCurrentLogger value record: 'Test2'
	]
```


