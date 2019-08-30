# Refactoring in Pharo 8.0

## Introduction

Pharo offers many automatic refactoring tools. This tutorial presents what refactorings are, when and how to apply them and to show any possible inconvenience before using them.

## What is refactoring?

Martin Fowler defines refactoring as: "... It is a disciplined technique to restructure an existing body of code, altering its internal structure without changing its external behavior." This means that before carrying out any type of refactoring in the code it is important to have complete test coverage to demonstrate that the code behavior does not change during the refactoring process.

## The most popular refactorings supported by Pharo 8.0

| Refactoring | Supported in |
| ---------- | ---------- |
| [Rename](#rename)   | Package, Class, Method, Instance and Temporary Variable |
| [Extract](#extract)   | Method and Temporary Variable  |
| [Remove](#remove)| Package, Class, Method and Instance Variable |
| [Copy](#copy) | Class |
| [Move](#move) | Class and method |
| [Inline](#inline) | Method and Temporary Variable |
| [Push up / Push down](#push-up-and-push-down) | Method and Instance Variable |
| [Replace](#replace) | Method |
| [Other refactorings](#other-refactorings) |  Class, Method, Instance and Temporary Variable |

# Rename

Renaming seems like a trivial refactoring however it can lead to many complications because this action can lead to involuntary changes of elements not related to the same name. The Pharo's rename tool has can minimize these errors.

## When should we rename?

1. When the name is not descriptive enough
2. The name of the class / method / variable does not match what it really is.
3. Something new was introduced that requires the existing code to have a new more specific name.
 
> This refactoring can be done for classes, methods and variables.
>
>1. Select the class / variable / method 
>2. Press **Meta+R** or from the menu, select **Rename**.
>3. Fill the input of dialog with the new name and press **Ok** button.

[![Pharo - Rename refactoring](https://img.youtube.com/vi/tHdt11WDq2o/0.jpg)](http://www.youtube.com/watch?v=tHdt11WDq2o)

# Extract

The extraction Pharo's tools allow developers to reshape their code when they have duplicate code or when they want to change the design.

Pharo can perform this refactoring to extract methods and extract temporal variables.

## Extract temporary variable

#### Before
``` Smalltalk
Example >> method
    | a b c|
    a := 1.
    b := a + list size.
    c := b + list size.
```
#### After
Selecting `list size` on source code
``` Smalltalk
Example >> method
    | a b c d|
    a := 1.
    d := list size.
    b := a + d.
    c := b + d.
```

>1. Select the section of source code you want to convert in temporary variable.
>2. Press **Meta+T** or from the menu, select **Source code**.
>3. Select **Extract temp** option.
>4. Fill dialog's input with the name of the variable and press **Ok** button.

## Extract method

#### Before
``` Smalltalk
Example >> method
    | a b c d|
    a := 1.
    b := 2.
    c := a + b.
    d := a + c.
```
#### After
Selecting `a + b` on source code
``` Smalltalk
Example >> method
    | a b c d|
    a := 1.
    b := 2.
    c := self add: a to: b.
    d := self add: a to: c.

Example >> add: a to: b
    ^ a + b
```

>1. Seleccionar la seccion del codigo fuente que desea extraer.
>2. Press **Meta+T** or from the menu, select **Source code**.
>3. Select **Extract method** option.
>4. Fill the input of dialog with the name of method and press **Ok** button.

[![Pharo - Rename refactoring](https://img.youtube.com/vi/LfyN3bo4b0A/0.jpg)](http://www.youtube.com/watch?v=LfyN3bo4b0A)

# Remove

Sometimes when the code is refactored it can end with code that is no longer used, or that should no longer be used. Therefore, it is necessary to eliminate all the unused code since this affects the developers who work and try to understand the source code.

>This refactoring can be done for packages, classes, methods and variables.
>
>1. Select the package / class / variable / method
>2. Press **Meta+X**

# Copy

Many times we want to change or add some functionalities to our code, however this entails major changes which makes it dangerous to do it on the original class. That is why it may be necessary to copy the classes to have a backup in case you want to reverse the changes.

>Currently this tool only applies to classes and to use it you just have to follow the following steps:
>
>1. Select class
>2. Press **Meta+C** or from the menu, select **Copy**.

[![Pharo - Rename refactoring](https://img.youtube.com/vi/wgeVyFltKXY/0.jpg)](http://www.youtube.com/watch?v=wgeVyFltKXY)

# Move

Move allows you to move classes and methods from one package to another, and move methods to class / instance side or even move them to another class.

## Move to package

> ### Class
>
>1. Select class or classes that you want to move.
>2. Press **Meta+MC** or from the menu, select **Refactorings** and then select **Move to package** option.
>3. Select the package and press **Ok** button

> ### Method
>
>1. Select the method or methods to be moved.
>2. Select **Refactorings** and then select **Move to package** option.
>3. Select the package and press **Ok** button

## Move to class

>1. Select the method or methods to be moved.
>2. Select **Refactorings** and then select **Move to another class** option.
>3. Select the new class for method(s) and press **Ok** button

## Move to class side

>1. Select the method or methods to be moved.
>2. Press **Meta+TC**, select **Refactorings** and then select **Move to class side** option.

## Move to instance side

>1. Select the method or methods to be moved.
>2. Press **Meta+TI**, select **Refactorings** and then select **Move to instance side** option.

# Inline

Inline refactoring allows you to reverse extract refactoring of a method or temporary variable.

## Inline temp

Inline variables replaces the use of redundant variables with its initializer.

#### Before
``` Smalltalk
Example >> method
    | number b |
    number := anotherClass value.
    b := 3 + number.
```
#### After
Selecting 'number' temp on source code
``` Smalltalk
Example >> method
    | b |
    b := 3 + anotherClass value.
```

>1. Select the temporary variable in the source code
>2. Press **Meta+T** or from the menu, select **Source code**.
>3. Select **Inline temp** option.

## Inline method

The results of inline method place methods's body in the body of the method where we select its call.

#### Before
``` Smalltalk
Example >> method
    | a b c d|
    a := 1.
    b := 2.
    c := self add: a to: b.
    d := self add: a to: c.
    
Example >> add: a to: b
    ^ a + b
```
#### After
Selecting 'self add: a to: b.' on source code
``` Smalltalk
Example >> method
    | a b c d|
    a := 1.
    b := 2.
    c := a + b.
    d := a + c.
```

>1. Select the call to method in the source code.
>2. Press **Meta+T** or from the menu, select** Source code**.
>3. Select **Inline method** option.

## Inline senders

This refactoring replaces all calls to the method selected from its class by the body of its implementation, in addition to the method itself being eliminated.

>1. Select the method
>2. From the menu, select **Refactorings**.
>3. Select **Inline senders** option.

# Push up and Push down

## Push up method

Gets rid of duplicate code. If you need to make changes to a method, it’s better to do so in a single place than have to search for all duplicates of the method in subclasses; this refactoring technique can also be used if, for some reason, a subclass redefines a superclass method but performs what’s essentially the same work.

>1. Select the method
>2. Select **Meta+PU** or from the menu, select **Refactorings** and then select **Push up** option.

[![Pharo - Rename refactoring](https://img.youtube.com/vi/nZiLNx4Y6tA/0.jpg)](http://www.youtube.com/watch?v=nZiLNx4Y6tA)

## Push down method

Improves class coherence. A method is located where you expect to see it. For example if you see that a method is needed by more than one subclass, but not all of them, it may be useful to create an intermediate subclass and move the method to it. This allows avoiding the code duplication that would result from pushing a method down to all subclasses.

>1. Select the method
>2. Select **Meta+PD** or from the menu, select **Refactorings** and then select **Push down** option.

[![Pharo - Rename refactoring](https://img.youtube.com/vi/F_JjZfZzCqY/0.jpg)](http://www.youtube.com/watch?v=F_JjZfZzCqY)

In the same way as in the methods you can perform the push up and push down refactorings on the instance variables

## Push up variable

>1. Select the variable
>2. From the menu select **Push up** option.

## Push down variable

>1. Select the variable.
>2. From the menu select **Push down** option.

# Replace

## Replace senders

Replace senders helps us to change the senders of the selected method to the name of another method that we want, considering that the method with which we replace it must have the same number of arguments as the original.

>1. Select the method.
>2. From the menu, select **Refactorings**.
>3. Select **Replace senders** option.

[![Pharo - Rename refactoring](https://img.youtube.com/vi/CJjZYugsqZg/0.jpg)](http://www.youtube.com/watch?v=CJjZYugsqZg)

## Find and Replace method

Find and replace helps us when we have duplicate code, if this refactoring consists in selecting a method and looking for the occurrences of your body in a range of methods that is selected.

#### Before
``` Smalltalk
Example >> textInputOn: html withName: aString andSymbol: aSymbol
    html text: aString.
    html textInput on: aSymbol of: self contact.
    html break

Example >> renderContentOn: html
    html
	form: [ html text: 'Name:'.
  	    html textInput on: #name of: self contact.
	    html break.
	    html text: 'Email address:'.
	    html textInput on: #emailAddress of: self contact.
	    html break.
	    html text: 'Example:'.
	    html textInput on: #example of: self contact.
	    html break]
```
#### After
``` Smalltalk
Example >> textInputOn: html withName: aString andSymbol: aSymbol
    html text: aString.
    html textInput on: aSymbol of: self contact.
    html break

Example >> renderContentOn: html
    html
	form: [ self textInputOn: html withName: 'Name:' andSymbol: #name.
   	    self textInputextInputOnt: html withName: 'Email address:' andSymbol: #emailAddress.
	    self textInputOn: html withName: 'Example:' andSymbol: #example ]
```

>1. Select the method
>2. From the menu, select **Refactorings**.
>3. Select **Find and replace** option.

[![Pharo - Rename refactoring](https://img.youtube.com/vi/yxOn3tdh1wU/0.jpg)](http://www.youtube.com/watch?v=yxOn3tdh1wU)

# Other refactorings

## Generate accessors

This refactoring generates the get and set messages of the instance variables, can be used from the class and the variables.

#### Before
``` Smalltalk
Object subclass: #Example
    instanceVariableNames: 'a'
    classVariableNames: ''
    package: 'Refactoring-Example'
```
#### After
``` Smalltalk
Object subclass: #Example
    instanceVariableNames: 'a'
    classVariableNames: ''
    package: 'Refactoring-Example'
    
Example >> a
    ^ a

Example >> a: anObject
    a := anObject</pre>
```

>### Class
>
>1. Select the class that you want generate accessors
>2. Select **Meta+GA** or from the menu, select **Generate accessors** option

>### Variables
>
>1. Select the variable(s) that you want generate accessors
>2. From the menu, select **Generate accessors** option

[![Pharo - Rename refactoring](https://img.youtube.com/vi/LMnv8HDNE-4/0.jpg)](http://www.youtube.com/watch?v=LMnv8HDNE-4)

## New subclass

Generate a new subclass of a class. 

#### Before
``` Smalltalk
Object subclass: #MyA
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Refactoring-Example'

MyA subclass: #MyB
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Refactoring-Example'</pre>
```
#### After
Selecting the `MyA` class
``` Smalltalk
Object subclass: #MyA
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Refactoring-Example'
    
MyA subclass: #MyB
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Refactoring-Example'
    
MyA subclass: #MyC
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Refactoring-Example'
```

>1. Select class.
>2. From the menu, select **Refactorings** and then select **New subclass** option.
>3. Fill the dialog's input with new class's name

[![Pharo - Rename refactoring](https://img.youtube.com/vi/PLqO6s46p_o/0.jpg)](http://www.youtube.com/watch?v=PLqO6s46p_o)

## Insert subclass

Generate a new subclass of a class between the class and its subclasses. This refactoring is useful when we have to specialize some classes.

#### Before
``` Smalltalk
Object subclass: #MyA
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Refactoring-Example'

MyA subclass: #MyB
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Refactoring-Example'
```
#### After
Selecting the `MyA` class
``` Smalltalk
Object subclass: #MyA
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Refactoring-Example'
    
MyA subclass: #MyC
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Refactoring-Example'
    
MyC subclass: #MyB
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Refactoring-Example'
```

>1. Select class.
>2. From the menu, select **Refactorings** and then select **Insert subclass** option.
>3. Fill the dialog's input with new class's name

[![Pharo - Rename refactoring](https://img.youtube.com/vi/vflwFbwY58A/0.jpg)](http://www.youtube.com/watch?v=vflwFbwY58A)

## Insert superclass

Generate a new superclass of a class between the class and its superclass. This refactoring is useful when we must generalize to avoid the redundancy of the classes.

#### Before
``` Smalltalk
Object subclass: #MyA
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Refactoring-Example'

MyA subclass: #MyB
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Refactoring-Example'
```
#### After
Selecting the `MyB` class
``` Smalltalk
Object subclass: #MyA
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Refactoring-Example'
    
MyA subclass: #MyC
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Refactoring-Example'
    
MyC subclass: #MyB
    instanceVariableNames: ''
    classVariableNames: ''
    package: 'Refactoring-Example'
```

>1. Select class.
>2. From the menu, select **Refactorings** and then select **Insert subclass** option.
>3. Fill the dialog's input with new class's name

[![Pharo - Rename refactoring](https://img.youtube.com/vi/b1wpkuxYwHc/0.jpg)](http://www.youtube.com/watch?v=b1wpkuxYwHc)

## Jump to test class

This seems trivial, however this command offers us a shortcut to automatically create a class for our tests or go to tests class.

>1. Select class.
>2. Press **Meta+GJ** or from the menu, select **Jump to test class**.

[![Pharo - Rename refactoring](https://img.youtube.com/vi/IC6MawvjzwE/0.jpg)](http://www.youtube.com/watch?v=IC6MawvjzwE)


##  Abstract instance variables

This refactoring generate accessors methods if they don't exist and replace every direct access to class variables with accessor methods.

#### Before
``` Smalltalk
Object subclass: #MyA
    instanceVariableNames: 'a'
    classVariableNames: ''
    package: 'Refactoring-Example'

MyA >> method
    |result|
    a := self initializeVariable.
    result := a value.
```
#### After
Selecting the `a` variable
``` Smalltalk
Object subclass: #MyA
    instanceVariableNames: 'a'
    classVariableNames: ''
    package: 'Refactoring-Example'
    
Example >> a
    ^ a

Example >> a: anObject
    a := anObject

MyA >> method
    |result|
    self a: self initializeVariable.
    result := self a value.'</pre>
```

>1. Select variable.
>2. From the menu, select **Abstract instance variables** option.

[![Pharo - Rename refactoring](https://img.youtube.com/vi/cRBICE66v7g/0.jpg)](http://www.youtube.com/watch?v=cRBICE66v7g)
