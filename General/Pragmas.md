# Pragmas

- [Description](#description)
- [How to declare a new pragma](#how-to-declare-a-new-pragma)
- [Collect pragmas](#collect-pragmas)
  * [Find pragmas in the IDE](#find-pragmas-in-the-ide)
  * [Get the pragmas of a CompiledMethod](#get-the-pragmas-of-a-compiledmethod)
  * [Collect pragmas in a hierarchy](#collect-pragmas-in-a-hierarchy)
  * [Collect pragmas in the image](#collect-pragmas-in-the-image)
- [Act on collected pragmas](#act-on-collected-pragmas)
- [Examples of pragma usage](#examples-of-pragma-usage)
  * [Pragmas used in the IDE](#pragmas-used-in-the-ide)
- [See also](#see-also)

## Description

Pragmas in Pharo are annotations on `CompiledMethods`. They are used to attach additional properties to the methods to make those methods easily collectable through reflectivity and to ease the creation of special handlings.

Pragmas are part of Pharo's syntax and are declared like this: `<aPragma>`. They should be placed at the beginning of a method, after the method selector.

This documentation illustrates the use of pragmas by describing how to use them to annotate an application's parameters and generate documentation from those parameters comments.

## How to declare a new pragma

To declare a new pragma, you only need to add it to, at least, one method in the system.

For example, annotating an application parameter can be done as follow:

```Smalltalk
isInAdminMode
  "If this parameter is true, the application will allow one to access all the administration panels."
  
  <applicationParameter>
  ^ isAdminMode
```

Pragmas can take literals as a parameter in order to configure their future handling.

In our example, we might want to declare which parameters are optional.

```Smalltalk
isInAdminMode
  "If this parameter is true, the application will allow the user to access all the administration panels."
  
  <applicationParameterOptional: false>
  ^ isAdminMode
```

## Collect pragmas

Once defined, we need to be able to collect the pragmas either to modify them during the development or to use them to handle a feature of the application.

We will explore these possibilities in the following sections.

### Find pragmas in the IDE

During the development of the application, the developer might need to browse the currents senders of a pragma. This can be done in the same way we browse method selectors in Pharo.

You can:
- Select a symbol is the name of the pragma and type `CMD + n` (or `CTRL + n` depending on your operating system).
- Select a symbol which is the name of the pragma and right click on it then select `Browse senders` in the search menu.
- Type the name of the pragma in the Spotter (`SHIFT + ENTER` to open Spotter) followed by `#p`.
- Use the `Finder` (accessible via the menu through path: `Menubar -> Tools -> Finder`) and select the `Pragmas` mode.
- Send the message `senders` to a symbol which is the name of the pragma.

### Get the pragmas of a CompiledMethod

If you have a compile method you can directly ask it his pragmas via the `#pragmas` method.

For example, you can get all methods with a pragma like this:

```Smalltalk
SystemNavigation new browseAllSelect: [ :method | method pragmas isNotEmpty]
```

### Collect pragmas in a hierarchy

When you want to use the pragmas in your application you have two ways to collect them. If you want to find the pragmas in one class or one hierarchy of class, you can use the `Pragma` class to collect them.

The Pragma class implements different ways to get to pragmas depending on your requirements.

You can use:
- `allNamed: aSymbol from: aSubClass to: aSuperClass`
- `allNamed: aSymbol from: aSubClass to: aSuperClass sortedByArgument: anInteger`
- `allNamed: aSymbol in: aClass`
- `allNamed: aSymbol in: aClass sortedByArgument: anInteger`
...

In our example, we might want to use this way of accessing our pragmas if all parameters are defined in a configuration object.

In that case, we can access them this way:

```Smalltalk
Pragma allNamed: #applicationParameterOptional: in: MyConfiguration
```

Or if we have a hierarchy of configurations:

```Smalltalk
Pragma allNamed: #applicationParameterOptional: from: MyConfiguration to: MyAbstractConfiguration
```

### Collect pragmas in the image

You can also use the Pragma class to collect pramgas in the entire image by using:
- `allNamed: aSymbol `
  
A second, now less reccomended way to collect pragmas, is to use the PragmaCollector. This class can be configured with a query and will return all the pragmas matching the query.

To configure the PragmaCollector we need to send implement a filter block. If we want to collect the `primitive:` pragmas:

```Smalltalk
PragmaCollector filter: [:prg | prg selector = #'primitive:' ]
```

Once configured we can execute the collection of the pragmas by sending the method #reset to the pragma collector:

```Smalltalk
(PragmaCollector filter: [:prg | prg selector = #'primitive:' ]) reset
```

Then you can iterate on the result using `#do:`, `#collect:`, `#reject:`, etc or use them directly through `#collected`. 

In our example we might want to access them this way:

```Smalltalk
(PragmaCollector filter: [:prg | prg selector = #'applicationParameterOptional:' ]) reset collected
```

Or we can select only non optional parameters this way:

```Smalltalk
(PragmaCollector filter: [:prg | prg selector = #'applicationParameterOptional:' and: [ (prg argumentAt: 1) not ] ]) reset collected
```

## Act on collected pragmas

Once collected, we need use the pragmas to implement the feature we want.

For that, the pragmas can answer diverse messages to get their context. For example, they can answer to:
- `#method` : Returns the CompiledMethod in which the pragmas is.
- `#arguments` : Returns the arguments of the pragmas.
- `#argumentAt:` : Returns the value of the n-th argument of the pragma instance.
- `#argumentNamed:`: Returns the value of the arguemnts following a subpart of the pragma selector. This is available since Pharo 8.
- `#selector` : Returns the pragma selector.
- `#methodClass` : Returns the class in which the method containing the pragma is.

With that information, we can then build our handler.

The next snippet will collect the senders of our example pragma and generate user documentation about them:

```Smalltalk
String
    streamContents: [ :stream | 
        stream
            << 'Documentation';
            lf;
            lf.
        (PragmaCollector filter: [ :prg | prg selector = 'applicationParameterOptional:' ]) reset
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

Here is an example on how to use `#argumentNamed:`:

```Smalltalk
(PragmaCollector filter: [:prg | prg selector = #'loaderNamed:priority:' and: [ (prg argumentNamed: #priority) > 0 ] ]) reset collected
```

## Examples of pragma usage

Pragmas are used for many things in Pharo and you can find examples of their usage by browsing Pharo code using them.

For example, the setting framework is built with pragmas. The pragma `<systemSetting>` is used to register new settings the setting browser.

And other examples can be found in the world menu registration and is explain if the page [Menubar and World menu](MenuBar.md).

### Pragmas used in the IDE

Some pragmas are interesting to know in Pharo IDE. You can find information on them on [this page](InterestingsToKnowForBeginners.md#useful-pragmas)

## See also

- [Pragmas: Literal Messages as Powerful Method Annotations](https://rmod.inria.fr/archives/papers/Duca16a-Pragmas-IWST.pdf)
