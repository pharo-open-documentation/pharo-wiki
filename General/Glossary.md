# Pharo vocabulary
In Pharo, there is some specific vocabulary that is used inside the community to designate concepts provided by the language.
This page aims to provide disambiguation for the words belonging to this vocabulary.

## Binary message
A binary message is a (message)[#message] composed of special character(s) where two objects are involved (the (receiver)[#receiver] and one argument), which is the reason it is call binary. This is mostly used for arithmetic, comparison, and logical operations. For example, `+` is a binary message involving the receiver and the argument in parameter.

## Bootstrap

## Browser
The browser designate the tool for browsing and editing packages, classes and methods. In Pharo 6.1 and greater, the browser is Calypso.

## Candidates
In the context of a method-call the candidates are the potential classes in the system that can receive the call. This list is computed from a static analysis of the source code.

## Changes (file)
The changesfile logs of all source code modifications (especially all thechanges you did while programming) since the (sources file)[#sources] was generated. This facilitates a per method history for diffs or reverting. It means that even if you did not manage to save the image file on a crash or you just forgot, you can recover your changes from this file. A changes file is always coupled with a image file. They work in pair.

> Note: Since Pharo 5 Pharo contains a project called Epicea implementing a new logging system. The long term goal of Epicea is to replace the changes file, but we are not yet at this stage.

## Class-side

## Cog
Cog is the name of the (virtual machine)[#virtual-machine] used by Pharo. Cog is a virtual machine used by multiple programming languges such as Pharo, (Squeak)[#squeak], Newspeak and Cuis.

## Context

## Debugger

## Dispatch
Dispatch is a technique used in object-oriented programming to delegate behavior using polymorphism. The goal is to define a concept that will changed depending on the context. For example we can have an application with commands. When we need to execute the command, instead of creating a switch/case, each object will implément their own behavior on a method of the same name and we just need to call this method. 

## DNU
See (DoesNotUnderstand)[#doesnotunderstand].

## DoesNotUnderstand
This name is used to designate the error that arise when a message is sent to an object but this object does not understand it. It also happen that people use the "DNU" shortcut.

## Iceberg

## Image
A Pharo image is a snapshot of the system memory of the Pharo system at a given moment. It is called image since it is a snapshot. This is the file where all objects are stored and as such it’s a cross plat-form format. An image file contains the live state of all objects of the system (including classes and compiled methods, since they are objects too) at a givenpoint. An image is a virtual object container. 

## Implementors
For a given selector in the system, implementors are classes that have a method with this selector (they implement the selector). For example the implementors of the method `#banana` are all the classes containing a method named `#banana`.

## Inspector

## Instance

## Instance-side

## Keyword message
A keyword message is a (message)[#message] where two or more objects are involved (the (receiver)[#receiver] and the arguments). A message is composed of alphanumeric characters. The arugments are injected inside the message selector and must be proceeded by a colon (`:`). For example, `between:and:` is a keyword message with a receiver and two arguments. It can be used like this: `13 between: 12 and: 14`.

## Late binding

## Lookup
Method lookup is the name of the technique Pharo uses to find the method to execute when an object receive a (message)[#message]. it proceeds as follows: When a message is sent, methods in the (receiver)[#receiver]'s class are searched for a matching method. If no match is found, the superclass is searched, and so on up the superclass chain. In the end if no method is found, the object call the method (`#doesNotUnderstood:`)[#doesnotunderstood] with the message as parameter.

## Message
A message in Pharo represent an interaction between two objects. A (sender)[#sender] will send a message to a (receiver)[#receiver] which will start a (lookup)[#lookup] from the receiver. A message is composed of two things that are a selector, which is the name of the method to lookup, and argument values which are the values of each arguments of the method to execute once find by the lookup. Three kinds of messages exists: (unary messages)[#unary-message], (binary messages)[#binary-message] and (keyword messages)[#keyword-message].

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
Self is the (receiver)[#receiver] of a (message)[#message] an object send to itself. This means that the (lookup)[#lookup] of a method sent will start by the sender of this message then, if not found, it will proceed with its superclass.

## Sender
The sender of a (message)[#message] is the object that will send a message to another object. In case the (receiver)[#receiver] of a message is (self)[#self], then the sender is equal to the receiver.

## Senders
For a given selector in the system, senders are the methods sending a message with this selector. For example, the senders of the method `#banana` are all the methods sending the message `#banana` in their code.

## Sista 
Sista is the name of a (virtual machine)[#virtual-machine] with adaptive optimisations such as speculative inlining. It has better performances but is still currently under developement. It is not yet production ready.

## Slang
The term "Slang" refers to a subset of the Smalltalk language and objects that can directly translated to C (or other language, such as Javascript). It is mainly used to develop the (Cog)[#cog] virtual machine. It allows to develop the virtual machine in Smalltalk and generate it in C.

## Slot
Instance variables are reified in Pharo, which means they are real objects that can be manipulated as any other object.
Because of that, the name *slot* has been given to the general concept of instance variable in Pharo.
In practice, most of the slots in classes of the system are `InstanceVariableSlot`.
One can create a new kind of slot by subclassing `Slot` class (see [this blogpost](https://medium.com/@juliendelplanque/typed-slots-for-pharo-98ba5d5aafbe) as an example).

## Smalltalk
Smalltalk is an object-oriented, dynamically typed reflective programming language with multiple implementations. Some implemenations includes (Squeak)[#squeak], VisualWorks, Gemstone, VA Smalltalk and many others. Pharo started as an implementation of Smalltalk and is still very close to it.

## Sources (file)
The sources file contains source code of Pharo. Sources file is important because the image file format stores only objects including compiled methods and their bytecode and not their source code. Originally, a new sources file was generated once per major release of Pharo. Now Pharo changed its production mechanisme to be (bootstraped)[#bootstrap] and generates a new sources file for each Pharo image produced.

## Spur
Spur is the name of the current object representation in memory of Pharo used by the (virtual machine)[#virtual-machine]. This representation is used since Pharo 5 and was introduced to improve performances (by 30% compared to the previous implementation) and prepare Pharo to run in 64bits.

## Squeak
Suqeak is a dialect of (Smalltalk)[#smalltalk]. Pharo started as a fork of Squeak and both still share the same (virtual machine)[#virtual-machine].

## Super
Super is the (receiver)[#receiver] of a (message)[#message] an object send to itself, but on the contrary of (self)[#self], the method lookup will start in the superclass of a receiver. Super is a unique way for the class to add behavior
to the inherited methods and not just replace them.

## Unary message
An unary message is a (message)[#message] where only the (receiver)[#receiver] of the message will be involved, which is the reason it is call unary. This means that the message will have no argument. For example, `#factorial` is an unary message that will only involve the number receiving the message.

## VM
See (Virtual Machine)[#virtual-machine].

## Virtual Machine
The virtual machine of Pharo is the execution engine. It takes Pharo bytcode that is generated each time user compiles a piece of code, converts it to machine code and executes it. The VM is different for each platform running Pharo (windows, OSX, linux, arm, 32/64bits) and allows the other part of pharo to be platform independant.
