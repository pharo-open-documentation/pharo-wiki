# Extensions

Pharo includes a system of extensions methods. This feature allows the developer to add behavior (but not state) to existing objects in Pharo. 

There is no syntactic difference between calling an extension method and calling a method declared in the class. The main difference between methods and extension methods is that extension methods are stored in antoher package than the class it is implemented on.

## What is the use of Extension methods?

Extension methods are useful when we want to use some behavior on an object but it does not make sense to package it directly with the objct. For example we might want to ask to a model object informations related to its display in a GUI, but to keep clean layers we do not want display informations stored in the code package of our application. In that cas we can add those methods to the model object but package those methods in a GUI package.

## How to add a new extension method?

Internaly, an extension method is a method which protocol has a patter `*MyPackageExtendingMyClass`. 

Here are more detailed informations about adding an extension method via Pharo's tooling.

### Define an extension method in Pharo < 7

In the Pharo versions based on Nautilus system browser, extension methods are created when we catogorize methods on a protocol beginning by a `*`.

> TODO

### Define an exrension method since Pharo 7

> TODO

## Find methods currently extending a class

> TODO

### In Pharo < 7

> TODO

### Since Pharo 7

> TODO

### Programatically 

> TODO

## Warnings

One thing developers should take into account when using extensions is to have clean dependencies between its packages. A package extending classes from another package should be loaded *after* this package.

For example, if my package `MyProject-GUI` extends `MyProject-Model`, `MyProject-Model` should always be loaded *before* `MyProject-GUI` is. In order to ensure this, we recommand you to read the dependencies sections in the [baseline guide](General/Baselines.md).
