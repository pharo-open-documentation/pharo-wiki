# Traits

## Description

Traits are pure units of behavior that can be composed to form classes or other traits. The trait composition mechanism is an alternative to multiple or mixin inheritance in which the composer has full control over the trait composition. The result enables more reuse than single inheritance without introducing the drawbacks of multiple or mixin inheritance.

Since Pharo 7 Traits are modular and not tight the the Kernel. So it would be possible to have multiple implementations.

## Add a new Trait

Creation of a new Trait is close to the creation of a new class. It is done in a programatic way:

```Smalltalk
Trait named: #TNameOfMyTrait
	uses: {}
	package: 'MyPackage'
```

This will create a new Trait called `TNameOfMyTrait` stored in `MyPackage`

### Add a new variable or slot

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

### Add a new method

To add a new method to the Trait you just need to implement the method as if you were implementing a method on a class.

### Explicit requirement

Sometime we want to call a methods that should not be implemented on a Trait but on the method using the Trait. To manage this a Trait can declare method that users should define by calling sending the message #explicitRequirement. 

```Smalltalk
TMyTrait>>addButton: aButton
	self buttons add: aButton
```

```Smalltalk
TMyTrait>>buttons
	^ self explicitRequirement
```

## Use a Trait

Tu use a Trait you just need to declare it in the class declaration as parameter of the #uses: keyword. 

```Smalltalk
FAMIXType subclass: #FAMIXEnum
	uses: FamixTWithEnumValues
	slots: {  }
	classVariables: {  }
	package: 'Famix-Compatibility-Entities'
```

### Reject some method

In some case it is needed to reject a method of a Trait. For example if a Trait uses a cache but you know you don't need it, you can remove this method using `-`.

```Smalltalk
TestCase subclass: #StackTest
	uses: TEmptyTest - {#testIfNotEmptyifEmpty. #testIfEmpty. #testNotEmpty} + (TCloneTest - {#testCopyNonEmpty})
	slots: { #empty. #nonEmpty }
	classVariables: {  }
	package: 'Collections-Tests-Stack'
```

### Trait using Traits

Itis possible to use Traits to compose a new Trait:

```Smalltalk
Trait named: #EpTEventVisitor
	 uses: EpTCodeChangeVisitor
	 category: 'Epicea-Visitors'
```
