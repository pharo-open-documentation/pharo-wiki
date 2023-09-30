# Цікаве для початківців

У цьому розділі ми розповімо про різні речі, які можуть бути цікавими для початківців. Інструменти, API, ярлики...

Метою є спростити використання і вивчення Pharo.

- [Прикладний програмний інтерфейс (API)](#прикладний-програмний-інтерфейс-api)
- [Обчислювані обʼєкти (блоки та символи)](#обчислювані-обʼєкти-блоки-та-символи)
- [Інструменти](#інструменти)
  * [Системний браузер Каліпсо](#системний-браузер-каліпсо)
- [Ярлики](#ярлики)
- [Корисні прагми](#корисні-прагми)
  * [Зміна інтегрованого середовища розробки (IDE)](#зміна-інтегрованого-середовища-розробки-ide)
  * [Вплив на виконання методів](#вплив-на-виконання-методів)
  * [Вплив на зневаджувач](#вплив-на-зневаджувач)

## Прикладний програмний інтерфейс (API)

Прикладний програмний інтерфейс (API) Pharo дуже багатий. У цьому розділі ми поговоримо про різноманітні методи Pharo, які виявилися корисними з нашого досвіду.

**String class>>#streamContents:**

Конкатенація рядків може бути дороговартісною, оскільки вона створює новий екземпляр рядка і копіює вміст двох рядків при кожній конкатенації. Щоб зробити це більш ефективним, Pharo включає метод створення рядка за допомогою потоку:

```st
'This is a ', 'string concatenation ' , 'that is not really ' , 'efficient'.

String streamContents: [ :aStream |
    aStream
        << 'This is a '
        << 'string concatenation '
        << 'that is '
        << 'efficient' ]
```

## Обчислювані обʼєкти (блоки та символи)
Pharo's Blocks and Symbols have an interesting property: they are polymorphic through `#value:` and `#cull:` methods.

For example, the two code snippets below are equivalent:

```Smalltalk
#class value: Object new
```

```
[ :o | o class ] value: Object new
```

They both return the class of an object.

This means that for methods taking a one-argument block as a parameter, this block can be replaced by a symbol. The effect is that the unary method named by the symbol will be executed with the object as the receiver.

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

## Інструменти

### Системний браузер Каліпсо

- Display the differences between two methods
	* Click on a method
	* Hold down the `SHIFT` key
	* Click on the second method
	* Click on the tab Diff to see the differences
	
- Editing multiple methods side by side
	* Click on a method
	* Hold down the `CTRL` key
	* Click on all the methods you want to edit. 
	* Tabs are displayed with the source code of the methods selected.

## Ярлики

It is possible to "flag" methods in Pharo in order to be able to retrieve them later. This can be done by sending the `#flag:` message to self with a symbol as parameter which is the flag to be used.

Commonly used flags are:
- `#todo` to mark code that needs work to be done.
- `#hack` to mark hackish code.
- `#broken` to mark broken code.
- `#clean` to mark code that needs cleaning.
- `#toCheck` to mark code that should be checked later.
- `#pharoX` to mark code present for compatibility with `X` being the version of Pharo.

Flags will not modify the execution of a method. Flags are just used to tag methods.

For example, the method below will return `42` if executed as if there was no call to `#flag:` method.

```
myUnfinishedMethod
	self flag: #TODO. "Some process to be done here."
	^ 42
```

To retrieve methods for which you set flag(s), just search for senders of your flag. For example, to retrieve methods flagged with `#todo`, either inspect the result of the following script: `#todo senders` or just select the symbol and press Meta+N (shortcut for "Browse senders").

## Корисні прагми

Pragmas allow one to tag methods. They are similar to Java's annotations. Some pragmas allow the developer to modify the IDE or to influence execution of methods. This section presents some of these useful pragmas.

### Зміна інтегрованого середовища розробки (IDE)

- `<script>` for class-side methods, adds a button next to the method name in the IDE. This button, when clicked, executes the method.
- `<script: 'Smalltalk code to run'>` is similar to `<script>` but the button executes the Smalltalk code held by the string provided as parameter to the pragma.
- `<sampleInstance>` for class-side methods, adds a button next to the method name in the IDE. When clicked, the method is executed and an inspector is opened on its result.
- `<worldMenu>` for class-side methods, add a menu entry in the Pharo menu. The method with this pragma should take a builder as a parameter. You can browse the senders of the `worldMenu` pragma to find the API.
- `<systemsettings>` for class-side methods, add an entry in the Pharo settings. The method with this pragma should take a builder as a parameter. You can browse the senders of the `systemsettings` pragma to find the API.
- `<haltOrBreakpointForTesting>` should be used in methods using breakpoints that should not be listed in the Breakpoint browser. For example, it can be used in examples using breakpoints to highlight a feature of the example.

### Вплив на виконання методів

- `<expectedFailure>` for instance-side methods of subclasses of `TestCase`, will make the test green when it is failing. But, although we expect that the test will fail, a success of the test (i.e. not assertion missed) will show as a failure.

### Вплив на зневаджувач
- `<debuggerCompleteToSender>` allows one to tell the debugger to open itself on the method that sends the message corresponding to the method holding the `<debuggerCompleteToSender>` pragma.

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
