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

> TODO

## Customize method received from a Trait

In some case it is needed to reject a method of a Trait. For example if a Trait uses a cache but you know you don't need it, you can remove this method using `-`.

```Smalltalk
TestCase subclass: #StackTest
	uses: TEmptyTest - {#testIfNotEmptyifEmpty. #testIfEmpty. #testNotEmpty} + (TCloneTest - {#testCopyNonEmpty})
	slots: { #empty. #nonEmpty }
	classVariables: {  }
	package: 'Collections-Tests-Stack'
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

> TODO Explain what happens if we use two traits with the same mehtod or if we implement a method in the class
