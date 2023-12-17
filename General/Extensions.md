# Extensions

- [Extensions](#extensions)
  - [Use of Extension methods](#use-of-extension-methods)
  - [Add a new extension method](#add-a-new-extension-method)
    - [Define an extension method in versions prior to Pharo 7](#define-an-extension-method-in-versions-prior-to-pharo-7)
    - [Define an extension method since Pharo 7](#define-an-extension-method-since-pharo-7)
    - [Define an extension method programmatically](#define-an-extension-method-programmatically)
  - [Find methods currently extending a class](#find-methods-currently-extending-a-class)
    - [In versions prior to Pharo 7](#in-versions-prior-to-pharo-7)
    - [Since Pharo 7](#since-pharo-7)
    - [Find extensions programmatically](#find-extensions-programmatically)
  - [Package loading order is important](#package-loading-order-is-important)

Pharo includes a system of extension methods. This feature allows the developer to add behavior (but not state) to existing objects in Pharo. 

> Note that it is only possible to add methods (behavior) as extension and not variables (state)

There is no syntactic difference between calling an extension method and calling a method declared in the class. The main difference between methods and extension methods is that extension methods are stored in a different package than the class they are implemented on.

## Use of Extension methods

Extension methods are useful when we want to use some behavior on an object, but it does not make sense to package it directly with the object. For example we might want to ask to a model object for information related to its display in a GUI, but to keep layers as cohesive as possible, we do not want display information stored in the code package of our application. In this case, we can add those methods to the model object but package those methods in a GUI package.

Another case might be when you want to enhance the behavior of a package that is simply not your own. You could add the methods to the class in the different package, but committing those changes to source control is not practical (i.e., you don't want to make a branch of the package, and doing a pull request would take a long time, etc.). With extension methods, the changes you make to the foreign package are stored in your own package, so they belong to you. 

## Add a new extension method

Internally, an extension method is a method whose protocol has a pattern `*MyPackageExtendingMyClass`. 

Here is more detailed information about adding an extension method via Pharo's tooling.

### Define an extension method in versions prior to Pharo 7

In the Pharo versions based on Nautilus system browser, extension methods are created when we categorize methods on a protocol beginning by a `*`.

![Add an extension method via Nautilus 1](img/Extensions_Image_NautilusAddExtension1.png?raw=true "Add an extension method via Nautilus 1")

![Add an extension method via Nautilus 2](img/Extensions_Image_NautilusAddExtension2.png?raw=true "Add an extension method via Nautilus 2")

### Define an extension method since Pharo 7

Since Pharo 7 the default system browser is Calypso. To add an extension method in Calypso you need to use the "Move to package" menu entry.

![Add an extension method via Calypso](img/Extensions_Image_CalypsoAddExtension.png?raw=true "Add an extension method via Calypso")

If you already have extensions in this package, you can also select the package in the list of extending packages in the protocol pane before adding a method.

### Define an extension method programmatically

```Smalltalk
ExistingClass compile: 'myMethod
	^ true' classified: '*MyPackage'
```

## Find methods currently extending a class

There are two ways to find which methods are extensions where they are located:

- Browse a class and check the protocols pane. (See examples below)

- Look at a package and check the bottom of the class pane. The last classes will appear in grey and will be the classes extended in that package.

### In versions prior to Pharo 7

![See an extension method via Nautilus](img/Extensions_Image_NautilusSeeExtensions.png?raw=true "See an extension method via Nautilus")

### Since Pharo 7

![See an extension method via Calypso](img/Extensions_Image_CalypsoSeeExtensions.png?raw=true "See an extension method via Calypso")

### Find extensions programmatically

To find extension methods programmatically you can do something like:

```Smalltalk
(Number methods select: #isExtension) groupedBy: #package
```

> **TODO: Add example of refactoring to an extension?**


## Package loading order is important

One thing developers should take into account when using extensions is to have clean dependencies between its packages. A package extending classes from another package should be loaded *after* this package.

For example, if my package `MyProject-GUI` extends `MyProject-Model`, `MyProject-Model` should always be loaded *before* `MyProject-GUI` is. For more details, see the dependencies sections in the [baseline guide](Baselines.md).
