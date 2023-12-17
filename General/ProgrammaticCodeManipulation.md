# Programmatic Code Manipulation

In this article, we will present some code snippets that demonstrate how we can manipulate the source code (packages, classes, methods, variables, traits, etc.) programmatically, without using the SystemBrowser. Those techniques can be very useful for reflective operations, code generation, testing, etc.

This article explains only those operations that **modify** the code. We will write a separate article on **querrying code** (looking for method senders, class references, pragma usages, etc.).

## Packages

### Creating a new package

### Removing a package

### Creating a package without installing it

_(how to create an instance of a package without installing it into the image? - e.g., for testing)_

## Classes

### Creating a new class

To create a new class, you can use the same code that appears in SystemBrowser. Here is an example with **Fluid** class definition.

```st
builder := Object << #MyClass	slots: { };	package: 'MyPackage'.	class := builder install.
```

The first statement will return an instance of `FluidClassBuilder`. The second statement will build the class, install it into the image, and assign it to the variable. If `MyPackage` does not exist, it will be created.

### Creating an anonymous class

### Removing a class

## Methods

### Adding a method to a class

To add a new method to a class, you can simply write its source code (including method name and arguments) in a string and send it as argument of a `compile:` method of your class. Here is an example of adding the method `printOn:` to `MyClass` (this is a silly implementation that will print a class name followed by a random number).

```st
sourceCode := 'printOn: aStream	"Print the object of this class"	aStream	    nextPutAll: self class name;	    space;	    nextPutAll: (Random new nextInteger: 100) asString'.

MyClass compile: sourceCode.
```

You can also specify a protocol:

```st
MyClass compile: sourceCode classified: 'printing'.
```

Finally, to avoid the error when generating a code in a fresh image, you should specify the author name. It can be anything, so in this case, we use `'Dummy Name'`

```st
Author 
    useAuthor: 'Dummy Name'
    during: [ MyClass compile: sourceCode ].
```

### Removing a method from a class

## Variables

## Traits

### Adding trait to a class

### Removing trait from a class

### Creating a new trait

### Adding methods to a trait

## Useful links

- A toolkit to generate Pharo code: <https://github.com/juliendelplanque/PharoCodeGenerator>