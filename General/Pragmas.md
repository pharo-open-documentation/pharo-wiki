# Pragmas

## Description

Pragmas in Pharo are annotations on `CompiledMethods`. They are used to attach to the methods additional properties to make those methods easily collectable through reflectivity and to easy the creation of special handlings.

Pragmas are part of Pharo's syntax and are declared like this: `<aPragma>`. They should be placed at the beginning of a method, after the method selector.

This documentation will illustrate the use of pragmas by describing how to use them to annotate an application's parameters and generate a documentation from those parameters comments.

## How to declare a new pragma

To declare a new pragma, you do not need to create any class or anything, you only need to add you pragma to one method.

For example, to annotate an application parameter you can do it like this:

```Smalltalk
isInAdminMode
  "If this parameter is true, the application will allows the user to access all the administration panels."
  
  <applicationParameter>
  ^ isAdminMode
```

Pragmas can also take literals as parameter to configure their future handling.

In our example we might want to declare each parameters are optional.

```Smalltalk
isInAdminMode
  "If this parameter is true, the application will allows the user to access all the administration panels."
  
  <applicationParameterOptional: false>
  ^ isAdminMode
```

## Collect pragmas

Once defined, we need to be able to collect the pragmas either to modify them during the development or to use them to handle a feature of the application.

We will explore those parts in this sections.

### Find pragmas in the IDE

During the development of the application, the developer might need to browse the currents senders of a pragmas. This cas be done in the same way we browse method selectors in Pharo.

You can:
- Select a symbole which will be the name of the pragma and type `CMD + n` or `CTRL + n`.
- Select a symbole which will be the name of the pragma and right click on it then select `Browse senders` in the search menu.
- Type the name of the pragma in the Spotter (`SHIFT + ENTER`) followed by `#p`.
- Use the `Finder` (`Menubar -> Tools -> Finder`) and select the `Pragmas` mode.
- Send the message `senders` to a symbole which is the name of the pragma.

### Get the pragmas of a CompiledMethod

If you have a compile method you can directly ask it tis pragmas via the `#pragmas` method.

For example you can get all methods with a pragma like this:

```Smalltalk
SystemNavigation new browseAllSelect: [ :method | method pragmas isNotEmpty]
```

### Collect pragmas in a hierarchy

When you want to use the pagmas in you application you have two ways to collect them. If you want to find the pragmas in one class or one hierarchy of class, you can use the `Pragma` class to collect them.

The Pragma class implements different way to get to pragmas depending on what you want.

You can use:
- `allNamed: aSymbol from: aSubClass to: aSuperClass`
- `allNamed: aSymbol from: aSubClass to: aSuperClass sortedByArgument: anInteger`
- `allNamed: aSymbol in: aClass`
- `allNamed: aSymbol in: aClass sortedByArgument: anInteger`
...

In our example, we might want to use this way of accessing our pragmas if all parameters are defined in a configuration object.

In that case we can access them this way:

```Smalltalk
Pragma allNamed: #applicationParameterOptional: in: MyConfiguration
```

Or if we have a hierarchy of configurations:

```Smalltalk
Pragma allNamed: #applicationParameterOptional: from: MyConfiguration to: MyAbstractConfiguration
```

### Collect pragmas in the image

The second way to collect the pragmas to handle them is to use the PragmaCollector. This class can be configured with a query and will return all the pragmas matching the query.

To configure the PragmaCollector we need to send implement a filter block. If we want to collect the primitive pragmas:

```Smalltalk
PragmaCollector filter: [:prg | prg keyword = 'primitive:' ]
```

Once configured we can execute the collection of the pragmas by sending the method #reset to the pragma collector:

```Smalltalk
(PragmaCollector filter: [:prg | prg keyword = 'primitive:' ]) reset
```

Then you can iterate on the result using #do:, #collect:, #reject:, etc or using them directly through #collected. 

In our example we might want to access them this way:

```Smalltalk
(PragmaCollector filter: [:prg | prg keyword = 'applicationParameterOptional:' ]) reset collected
```

Or we can select only non optional parameters this way:

```Smalltalk
(PragmaCollector filter: [:prg | prg keyword = 'applicationParameterOptional:' and: [ (prg argumentAt: 1) not ] ]) reset collected
```

## Act on collected pragmas

Once collected, we need to act on pragmas.

For that, the pragmas can answer diverse messages to get their context. They can answer to:
- `#method` : Returns the CompiledMethod in which the pragmas is.
- `#arguments` : Returns the arguements of the pragmas.
- `#argumentAt:` : Returns the value of the n-th arguement of the pragma instance.
- `#keyword` : Returns the pragma selector.
- `#methodClass` : Returns the class in which the method containing the pragma is.
- Etc

With those informations we can them build our handler.

The next snippet will collect the senders of our example pragma and generate a user documentation about them:

```Smalltalk
String
	streamContents: [ :stream | 
		stream
			<< 'Documentation';
			lf;
			lf.
		(PragmaCollector filter: [ :prg | prg keyword = 'applicationParameterOptional:' ]) reset
			do: [ :pragma | 
				stream
					<< '- ';
					<< pragma methodSelector.
				(pragma argumentAt: 1) ifTrue: [ stream << ' (Optional)' ].
				stream
					<< ': ';
					<< pragma method comment;
					lf ] ]
```

## Examples of pragma usage

Pragmas are used for many things in Pharo and you can find examples by browsing Pharo code.

For example, the setting framework is build with pragmas. The pragma `<systemSetting>` is used to register new settings the the setting browser.

And other example can be found in the world menu registration and is explain if the page [Menubar and Wrold menu](Menubar.md).

### Pragmas used in the IDE

Some pragmas are interesting to know in Pharo IDE. You can find information on them in [this page](InterestingsToKnowForBeginners.md#useful-pragmas)

## See also

- [Pragmas: Literal Messages as Powerful Method Annotations](https://rmod.inria.fr/archives/papers/Duca16a-Pragmas-IWST.pdf)
