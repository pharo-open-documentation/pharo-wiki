# Announcer
`Announcer` and `Announcement` classes provide an implementation of the [observer design pattern](https://en.wikipedia.org/wiki/Observer_pattern) in Pharo.
As written on the wikipedia page, the observer design pattern (and thus `Announcers` and `Announcements`) aims to address the following problems:

> - A one-to-many dependency between objects should be defined without making the objects tightly coupled.
> - It should be ensured that when one object changes state an open-ended number of dependent objects are updated automatically.
> - It should be possible that one object can notify an open-ended number of other objects.

This page provide a quick tutorial to get started with them.

- [Getting started](#getting-started)
- [Defining new kind of Announcement](#defining-new-kind-of-announcement)
- [Using announcer for your subject object](#using-announcer-for-your-subject-object)

## Getting started
The announcer class implements a mechanism to:
1. Allow objects (the observers) to subscribe to announcements;
2. Manage objects subscriptions; and
3. Allow the object holding the announcer (the subject) to announce events.

For example, there is an announcer available to let objects listen to announcements (events) concerning the system: `SystemAnnouncer uniqueInstance`.
Such announcements include:
- `ClassAdded`
- `ClassRemoved`
- `MethodModified`
- etc.

If one wants to be notified when a class is removed, it is possible to subscribe an object to this specific announcement as follow:

First, let's create the method `#whenClassAdded:` for the object that will listen to `ClassAdded` announcement.
This method will be the hook called when such announcement has been announced.
It takes the announcement sent by the subject as parameter.

```Smalltalk
MyObjectListeningToClassAdded>>#whenClassAdded: aClassAdded
  Transcript
    show: aClassAdded classAffected name;
    show: ' has been added.';
    cr
```

Then, we subscribe an instance of our object to the announcer.
When doing that, it is needed to specify
1. Which kind of announcements (its class) our object listen to.
2. Which object list to this kind of announcements.
3. Which method to call when the annoncement is sent.

The previous step can be achieved as follow:

```Smalltalk
SystemAnnouncer uniqueInstance
  when: ClassAdded send: #whenClassAdded: to: instanceOfMyObjectListeningToClassAdded
```

> Note: the SystemAnnouncer is a bit special because it is a subclass of `Announcer`.
> Usually, when creating you own announcer, you do not subclass `Announcer` but rather use it directly by storing an instance in a dedicated instance variable.

Once the code above has been executed, a message is printed in the Transcript each time a class has been added.
To unsubscribe an object from an announcer, simply call `#unsubscribe:` method on the announcer.

```Smalltalk
SystemAnnouncer uniqueInstance
  unsubscribe: instanceOfMyObjectListeningToClassAdded
```

## Defining new kind of Announcement
Creating a new kind of announcement to fit your needs is easy, just subclass `Announcement` class.
For example, let's say you have an object that has a color. You want to allow observers to listen to color changes of the object.
To do that, let's create a `ColorChangeAnnouncement` class:

```
Announcement subclass: #ColorChangeAnnouncement
	slots: { #newColor }
	classVariables: {  }
	package: 'MyPackage'
```

Your new kind of announcement aims to hold all the information you need.
In our example, `ColorChangeAnnouncement` hold the new color in `#newColor` instance variable.

## Using announcer for your subject object
The common pattern to use an `Announcer` in your subject object is to:
1. Create an `#announcer` instance variable which will contain the instance of `Announcer`.
2. Create an accessor method for `#announcer` instance variable that use lazy-initialization (so no announcer is created if no announcement is sent and no one wants to subscribe to events of the subject).
3. The subject object send announcements through its announcer.

As an example, let's implement the object changing its color discussed in previous section"

```
Object subclass: #MyObjectChangeColor
	slots: { #announcer. #color }
	classVariables: {  }
	package: 'MyPackage'
```

Then we create the accessor for `#announcer` with lazy-initialisation.

```
MyObjectChangeColor>>#announcer
  ^ announcer ifNil: [ announcer := Announcer new ]
```

And when the color of the object is updated, we announce it:

```
MyObjectChangeColor>>#color: aColor
  color := aColor.
  self announcer
    announce: (ColorChangeAnnouncement new
                newColor: aColor;
                yourself)
```
