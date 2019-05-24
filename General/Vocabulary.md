# Pharo vocabulary
In Pharo, there is some specific vocabulary that is used inside the community to designate concepts provided by the language.
This page aims to provide disambiguation for the words belonging to this vocabulary.

## Class-side

## Dispatch

## DoesNotUnderstand (DNU)
This name is used to designate the error that arise when a message is sent to an object but this object does not understand it. It also happen that people use the "DNU" shortcut.

## Instance-side

## Late binding

## Message

## Message-send

## Playground / Workspace

## Pragma

## Protocol
In Pharo, each method belongs to a protocol. A protocol has a name and allows one to group methods that are related together. For example, the protocol `accessing` is widely used in the system to hold accessor and mutator methods.

## Selector
A selector corresponds to the signature of a method in the system. Since Pharo is not statically typed, the selector of a method is simply a Symbol. On a CompiledMethod, it is possible to send `#selector` message to retrieve this Symbol.

## Slot
Instance variables are reified in Pharo, which means they are real objects that can be manipulated as any other object.
Because of that, the name *slot* has been given to the general concept of instance variable in Pharo.
In practice, most of the slots in classes of the system are `InstanceVariableSlot`.
One can create a new kind of slot by subclassing `Slot` class (see [this blogpost](https://medium.com/@juliendelplanque/typed-slots-for-pharo-98ba5d5aafbe) as an example).
