# Extensions

- [Extensions](#extensions)
  * [Use of Extension methods](#use-of-extension-methods)
  * [Add a new extension method](#add-a-new-extension-method)
    + [Define an extension method in Pharo before 7](#define-an-extension-method-in-pharo-before-7)
    + [Define an exrension method since Pharo 7](#define-an-exrension-method-since-pharo-7)
    + [Programaticaly](#programaticaly)
  * [Find methods currently extending a class](#find-methods-currently-extending-a-class)
    + [In Pharo before 7](#in-pharo-before-7)
    + [Since Pharo 7](#since-pharo-7)
    + [Programatically](#programatically)
  * [Warnings](#warnings)

Pharo includes a system of extensions methods. This feature allows the developer to add behavior (but not state) to existing objects in Pharo. 

There is no syntactic difference between calling an extension method and calling a method declared in the class. The main difference between methods and extension methods is that extension methods are stored in antoher package than the class it is implemented on.

## Use of Extension methods

Extension methods are useful when we want to use some behavior on an object but it does not make sense to package it directly with the objct. For example we might want to ask to a model object informations related to its display in a GUI, but to keep clean layers we do not want display informations stored in the code package of our application. In that cas we can add those methods to the model object but package those methods in a GUI package.

## Add a new extension method

Internaly, an extension method is a method which protocol has a patter `*MyPackageExtendingMyClass`. 

Here are more detailed informations about adding an extension method via Pharo's tooling.

### Define an extension method in Pharo before 7

In the Pharo versions based on Nautilus system browser, extension methods are created when we catogorize methods on a protocol beginning by a `*`.

![Add an extension method via Nautilus 1](ExtensionsNautilusAddExtension1.png?raw=true "Add an extension method via Nautilus 1")

![Add an extension method via Nautilus 2](ExtensionsNautilusAddExtension2.png?raw=true "Add an extension method via Nautilus 2")

### Define an exrension method since Pharo 7

Since Pharo 7 the default system browser is Calypso. To add an extension method in Calypso you need to use the "Move to package" menu entry.

![Add an extension method via Calypso](ExtensionsCalypsoAddExtension.png?raw=true "Add an extension method via Calypso")

If you already have extensions in this package, you can also select the package in the list of extending package in the protocol pane before adding a method.

### Programaticaly

```Smalltalk
  ExistingClass compile: 'myMethod
  ^ true' classified: '*MyPackage'
```

## Find methods currently extending a class

To find which methods are extensions and in which package it they are you have two ways.

The first is to browse a class an check the protocols pane. (See concrete examples bellow)

The second solution is to look at a package and check the bottom of the class pane. The last classes will appear in grey and will be the classes extended by the package selected.

### In Pharo before 7

![See an extension method via Nautilus](ExtensionsNautilusSeeExtensions.png?raw=true "See an extension method via Nautilus")

### Since Pharo 7

![See an extension method via Calypso](ExtensionsCalypsoSeeExtensions.png?raw=true "See an extension method via Calypso")

### Programatically 

To find extension methods programmaticaly you can do something like:

```Smalltalk
(Number methods select: #isExtension) groupedBy: #package
```

## Warnings

One thing developers should take into account when using extensions is to have clean dependencies between its packages. A package extending classes from another package should be loaded *after* this package.

For example, if my package `MyProject-GUI` extends `MyProject-Model`, `MyProject-Model` should always be loaded *before* `MyProject-GUI` is. In order to ensure this, we recommand you to read the dependencies sections in the [baseline guide](General/Baselines.md).
