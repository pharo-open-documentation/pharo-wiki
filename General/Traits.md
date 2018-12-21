# Traits

- [Traits](#traits)
  * [Description](#description)
  * [Create and use a new Trait](#create-and-use-a-new-trait)
  * [Abstract methods](#abstract-methods)
  * [Stateful traits](#stateful-traits)
  * [Traits initialization](#traits-initialization)
  * [Customize method received from a Trait](#customize-method-received-from-a-trait)
  * [Trait composition](#trait-composition)
  * [Conflicts](#conflicts)

## Description

Traits are pure units of behavior that can be composed to form classes or other traits. The trait composition mechanism is an alternative to multiple or mixin inheritance in which the composer has full control over the trait composition. The result enables more reuse than single inheritance without introducing the drawbacks of multiple or mixin inheritance.

Since Pharo 7 Traits are modular and not tight the the Kernel. So it would be possible to have multiple implementations.

## Create and use a new Trait

Creation of a new Trait is close to the creation of a new class. It is done in a programatic way:

```Smalltalk
Trait named: #TNameOfMyTrait
	uses: {}
	package: 'MyPackage'
```

Concrete example:

```Smalltalk
Trait named: #FamixTWithEnumValues
	 uses: {}
	 slots: {}
	 category: 'Famix-Traits-EnumValue'
```

This will create a new Trait called `TNameOfMyTrait` stored in `MyPackage`.

Then you add a new method to the Trait, just as you would implement a method in a class. All classes using this trait will be able to use methods created in the Traits if the do not override it.

Tu use your Trait you just need to declare it in the class declaration as parameter of the #uses: keyword. 

```Smalltalk
MySuperClass subclass: #MyClass
	uses: TNameOfMyTrait
	slots: {  }
	classVariables: {  }
	package: 'MyPackage'
```

Concrete example:

```Smalltalk
FAMIXType subclass: #FAMIXEnum
	uses: FamixTWithEnumValues
	slots: {  }
	classVariables: {  }
	package: 'Famix-Compatibility-Entities'
```

You can also use multiple Traits with your class with the #+ message.

```Smalltalk
MySuperClass subclass: #MyClass
	uses: TNameOfMyTrait + TNameOfMySecondTrait
	slots: {  }
	classVariables: {  }
	package: 'MyPackage'
```

## Abstract methods

Sometime we want to call a methods that should not be implemented on a Trait but on the class using the Trait. To manage this a Trait can declare method that users should define by calling sending the message #explicitRequirement. 

```Smalltalk
TMyTrait>>addButton: aButton
	self buttons add: aButton
```

```Smalltalk
TMyTrait>>buttons
	^ self explicitRequirement
```

## Stateful traits

Since Pharo 7, it is possible to add instance variables or a slot to Traits. This will make you trait a stateful trait. 

Examples:

```Smalltalk
Trait named: #MDLWithConfigurableRightPanel
	 uses: {}
	 slots: { #panelComponent. #toolbar }
	 category: 'MaterialDesignLite-Extensions'
```

```Smalltalk
Trait named: #FamixTWithEnumValues
	 uses: {}
	 slots: { #enumValues => FMMany type: #FamixTEnumValue opposite: #parentEnum }
	 category: 'Famix-Traits-EnumValue'
```

## Traits initialization

Traits do not include a way to initialize classes using them, it relies more on conventions.

One way to manage this might be to implement a method named `initializeTMyTraitName` on each traits needing an initialization and to call all those methods on the class using them.

In case of trait composition (See [Trait composition](#trait-composition)), a trait composed of other traits can also implement a initialize method calling the one of the Traits it includes.

## Customize method received from a Trait

### Reject some methods received from the trait
In some case it is needed to reject a method of a Trait. For example if a Trait uses a cache but you know you don't need it, you can remove this method using `-`.

```Smalltalk
TestCase subclass: #StackTest
	uses: TEmptyTest - {#testIfNotEmptyifEmpty. #testIfEmpty. #testNotEmpty} + (TCloneTest - {#testCopyNonEmpty})
	slots: { #empty. #nonEmpty }
	classVariables: {  }
	package: 'Collections-Tests-Stack'
```

### Alias some methods received from the trait
It is possible to alias some methods received from a trait. If, for example you alias `#aliasedMethod` with `#methodAlias` as shown below, your class will hold both `#methodAlias` and `#aliasedMethod`.

```
Object subclass: #MyObjectUsingTraitByAliasingMethod
	uses: TTraitToBeUsed @ { #methodAlias -> #aliasedMethod }
	slots: {  }
	classVariables: {  }
	package: 'TestTraitAliasing'
```

## Trait composition

Traits are composabls, this mean that you can have Traits using other traits. It is done in the same way than class using a Trait:

```Smalltalk
Trait named: TMyComposedTrait
	uses: TMyFirstTrait + TMySecondTrait
	category: 'MyPackage'
```

Example:

```Smalltalk
Trait named: #EpTEventVisitor
	 uses: EpTCodeChangeVisitor
	 category: 'Epicea-Visitors'
```

## Conflicts

Two kinds of *conflicts* can happen with methods implemented on Traits.

1. A method is present on a used Trait, but the class using this Trait also implements this method. In that case, the method lookup will select the method from the class. It is an equivalent of an override of method.

2. Two traits implementing the same method are used. In that case, if the method is called it will raise an error `traitConflict`.

A way to solve both cases is to use method aliasing and to remove the conflicting method:
```
Object subclass: #MyObjectUsingTraitByAliasingMethod
	uses: TTraitToBeUsed @ { #methodAlias -> #conflictingMethod } - { #conflictingMethod }
	slots: {  }
	classVariables: {  }
	package: 'TestTraitAliasing'
```

Another way to solve case 2 is to implement a method on the class using the trait in order to chose the behavior wanted.
