# Pharo vocabulary
In Pharo, there is some specific vocabulary that is used inside the community to designate concepts provided by the language.
This page aims to provide disambiguation for the words belonging to this vocabulary.

## Browser
The browser designate the tool for browsing and editing packages, classes and methods. In Pharo 6.1 and greater, the browser is Calypso.

## Candidates
In the context of a method-call the candidates are the potential classes in the system that can receive the call. This list is computed from a static analysis of the source code.

## Class-side

## Context

## Debugger

## Dispatch
Dispatch is a technique used in object-oriented programming to delegate behavior using polymorphism. The goal is to define a concept that will changed depending on the context. For example we can have an application with commands. When we need to execute the command, instead of creating a switch/case, each object will implément their own behavior on a method of the same name and we just need to call this method. 

## DoesNotUnderstand (DNU)
This name is used to designate the error that arise when a message is sent to an object but this object does not understand it. It also happen that people use the "DNU" shortcut.

## Iceberg

## Implementors
For a given selector in the system, implementors are classes that have a method with this selector (they implement the selector).

## Inspector

## Instance

## Instance-side

## Late binding

## Message

## Message-send

## Monticello

## Object

## Playground / Workspace

## Polymorphism

## Pragma
Pragmas are annotations on methods. They are used to attach additional properties to the methods to make those methods easily collectable through reflectivity and to ease the creation of special handlings.
A page dedicated to pragmas is available [here](https://github.com/pharo-open-documentation/pharo-wiki/blob/master/General/Pragmas.md).

## Primitive
A primitive is a method for which the source code is not written in Smalltalk but directly in C code. From the inside of the image, those methods contain a `<primitive:>` pragma.

Some methods holding this primitive pragma also contain source code written in Smalltalk. For these methods, first the C version of the method is tried but if the implementation in C is not provided by the VM for some reason, Pharo falls back to the smalltalk implementation.

## Protocol
In Pharo, each method belongs to a protocol. A protocol has a name and allows one to group methods that are related together. For example, the protocol `accessing` is widely used in the system to hold accessor and mutator methods.

## Receiver
The receiver of a message send is the instance of a class implementing the method corresponding to the message sent receiving this message.

## Selector
A selector corresponds to the signature of a method in the system. Since Pharo is not statically typed, the selector of a method is simply a Symbol. On a CompiledMethod, it is possible to send `#selector` message to retrieve this Symbol.

## Self

## Senders

## Slot
Instance variables are reified in Pharo, which means they are real objects that can be manipulated as any other object.
Because of that, the name *slot* has been given to the general concept of instance variable in Pharo.
In practice, most of the slots in classes of the system are `InstanceVariableSlot`.
One can create a new kind of slot by subclassing `Slot` class (see [this blogpost](https://medium.com/@juliendelplanque/typed-slots-for-pharo-98ba5d5aafbe) as an example).

## Smalltalk

## Squeak

## Super
