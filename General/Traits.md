# Traits

- [Description](#description)
- [Create and use a new Trait](#create-and-use-a-new-trait)
- [Abstract methods](#abstract-methods)
- [Stateful traits](#stateful-traits)
- [Traits initialization](#traits-initialization)
- [Customize method received from a Trait](#customize-method-received-from-a-trait)
  * [Reject some methods received from the trait](#reject-some-methods-received-from-the-trait)
  * [Alias some methods received from the trait](#alias-some-methods-received-from-the-trait)
- [Customize instance variables received from a (stateful) Trait](#customize-instance-variables-received-from-a-stateful-trait)
  * [Reject some instance variables received from the trait](#reject-some-instance-variables-received-from-the-trait)
  * [Alias some instance variables received from the trait](#alias-some-instance-variables-received-from-the-trait)
- [Trait composition](#trait-composition)
- [Conflicts](#conflicts)

## Description

Traits are pure units of behavior that can be composed to form classes or other traits. The trait composition mechanism is an alternative to multiple or mixin inheritance in which the composer has full control over the trait composition. It enables more reuse than single inheritance without introducing the drawbacks of multiple or mixin inheritance.

Pharo 7's Traits are modular and not tied to the Kernel. So it would be possible to have multiple implementations.

## Create and use a new Trait

Creation of a new Trait is close to the creation of a new class. It is done in a programatic way:

```Smalltalk
Trait named: #TNameOfMyTrait
	uses: {}
	package: 'MyPackage'
```

This will create a new Trait called `TNameOfMyTrait` stored in `MyPackage`.

Concrete example:

```Smalltalk
Trait named: #FamixTWithEnumValues
	uses: {}
	slots: {}
	package: 'Famix-Traits-EnumValue'
```

> Calypso provides a menu entry to create traits. To access it, right-click on the classes list (with no class or trait selected) and select "New trait".

> **Note**: In Pharo < 8, the message #named:uses:slots:package: should be replaced by #named:uses:slots:category:.  

Then you add a new method to the Trait, just as you would implement a method in a class. All classes using this trait will be able to use methods created in the Traits except if for methods overriden by the class.

To use your Trait you just need to declare it in the class declaration as parameter of the `#uses:` keyword. 

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

You can also use multiple Traits with your class with the `#+` message.

```Smalltalk
MySuperClass subclass: #MyClass
	uses: TNameOfMyTrait + TNameOfMySecondTrait
	slots: {  }
	classVariables: {  }
	package: 'MyPackage'
```

## Abstract methods

We might need to call a method for which the implementation will be specific to the class using the trait. To manage this case, a Trait can hold methods that explicitely declare that user should define it. These methods contain a call to `#explicitRequirement` message.

```Smalltalk
TMyTrait>>addButton: aButton
	self buttons add: aButton
```

```Smalltalk
TMyTrait>>buttons
	^ self explicitRequirement
```

> Some Pharo developers create Traits with all their methods calling `#explicitRequirement` message. Doing this kind of simulate an interface (as Java's interfaces). Users of one of these traits thus declare that they support the interface it defines and override all methods defined by the trait.

## Stateful traits

Since Pharo 7, it is possible to add instance variables or a slot to Traits. This will make you trait a stateful trait. 

Examples:

```Smalltalk
Trait named: #MDLWithConfigurableRightPanel
	uses: {}
	slots: { #panelComponent. #toolbar }
	package: 'MaterialDesignLite-Extensions'
```

```Smalltalk
Trait named: #FamixTWithEnumValues
	uses: {}
	slots: { #enumValues => FMMany type: #FamixTEnumValue opposite: #parentEnum }
	package: 'Famix-Traits-EnumValue'
```

## Traits initialization

Traits do not include a way to initialize classes using them, it relies on conventions.

One way to manage this might be to implement a method named `initializeTMyTraitName` on each traits needing an initialization and to call all those methods on the class using them.

In case of trait composition (See [Trait composition](#trait-composition)), a trait composed of other traits can also implement a initialize method calling the one of the Traits it includes.

## Customize method received from a Trait
When a class uses a trait, it is possible for it to reject or alias some methods.

### Reject some methods received from the trait
In some case it is needed to reject a method of a Trait. It can be achieved using `#-` message.

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

## Customize instance variables received from a (stateful) Trait
When a class uses a trait, it is possible for it to reject or alias some instance variables.

### Reject some instance variables received from the trait
In some case it is needed to reject an instance variable of a Trait. It can be achieved using `#--` message. It works similarly to methods rejecting explaining in previous section.

```
Object subclass: #MyObjectUsingTraitByRejectingInstVar
	uses: TTraitToBeUsed asTraitComposition -- #instVarNameToRemove
	slots: {  }
	classVariables: {  }
	package: 'TestTraitAliasing'
```

> `#asTraitComposition` needs to sent to the trait because `#--` message is not understood by trait but by trait composition.

### Alias some instance variables received from the trait
It is possible to alias some instance variables received from a trait. If, for example you alias `#aliasedInstVar` with `#instVarAlias` as shown below, your class will hold both `#instVarAlias` and `#aliasedInstVar`.

```
Object subclass: #MyObjectUsingTraitByAliasingInstVar
	uses: (TTraitToBeUsed >> { #instVarAlias -> #aliasedInstVar })
	slots: {  }
	classVariables: {  }
	package: 'TestTraitAliasing'
```

## Trait composition

Traits are composable, this mean that you can have Traits using other traits. It is done in the same way than class using a Trait:

```Smalltalk
Trait named: TMyComposedTrait
	uses: TMyFirstTrait + TMySecondTrait
	package: 'MyPackage'
```

Example:

```Smalltalk
Trait named: #EpTEventVisitor
	uses: EpTCodeChangeVisitor
	package: 'Epicea-Visitors'
```
## Conflicts

Two kinds of *conflicts* can happen with methods implemented on Traits.

1. A method is present on a used Trait, but the class using this Trait also implements this method. In that case, the method lookup will select the method from the class. It is an equivalent of an override of method.

2. Two traits implementing the same method are used. In that case, if the method is called it will raise an error `traitConflict`.

A way to solve both cases is to use method aliasing and to remove the conflicting method:
```Smalltalk
Object subclass: #MyObjectUsingTraitByAliasingMethod
	uses: TTraitToBeUsed @ { #methodAlias -> #conflictingMethod } - { #conflictingMethod }
	slots: {  }
	classVariables: {  }
	package: 'TestTraitAliasing'
```

Another way to solve case 2. is to implement a method on the class using the trait in order to chose the behavior wanted.
