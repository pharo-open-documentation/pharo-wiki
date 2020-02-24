# Sessions Management

 * [Description](#description)
 * [Quick start](#quick-start)
 * [Register a class to the session manager](#register-a-class-to-the-session-manager)
 * [Start-up, Shut-down and Saving actions](#start-up--shut-down-and-saving-actions)

## Description

Pharo images have a concept of **Session**. A session starts when one open a Pharo image and stop when one close it.
It is possible to hook your project to those sessions to perform actions such as executing code at startup, shutdown, image saving...

## Quick start

Here is a quick start to show how to set a class variable to the home folder of a user at startup and removing this value at shut down. 

You need to do those actions in a `#startUp:` method and `#shutDown:` method on the class side of your class. 

```Smalltalk
MyClass class>>startUp: isImageStarting
		isImageStarting
			ifTrue: [ MyVariable := FileLocator home asFileReference ]
```

```Smalltalk
MyClass class>>shutDown: isImageQuitting
		isImageQuitting
			ifTrue: [ MyVarible := nil ]
```

Then you need to register the class in the `SessionManager` of Pharo. This is usually done in the `#initialize` method (class-side).

```Smalltalk
MyClass class>>initialize
  SessionManager default registerUserClassNamed: self name.
  self startUp: true. "Here we execute it to set up the value while loading the project."
```

## Register a class to the session manager

It is possible to register classes to the `SessionManager` of Pharo in order to be able to execute code at start-up, shut-down or when we save a Pharo image.

The simplest way to do that is to add this in the class initialization method of the class you want to register.

```Smalltalk
MyClass class>>initialize
    SessionManager default registerUserClassNamed: self name
```
  
Sometimes, the order of execution of start-up/shut-down actions might matter. To manage this, it is possible to set a priority.
  
```Smalltalk
MyClass class>>initialize
SessionManager default registerUserClassNamed: self name atPriority: 60
```
  
The actions registered using `#registerUserClassNamed:[atPriority:]` will have a low priority in the startup/shutdown list.
It is possible to further refine the priorities using categories. 

The SessionManager has categories. `User` category one is one of them. Categories have priority between each others.
By default the system comes with those categories (sorted by priority from the highest to the lowest one):

- System (`#registerSystemClassNamed:[atPriority:]`)
- Network (`#registerNetworkClassNamed:[atPriority:]`)
- Graphical User Interface (`#registerUserGUINamed:[atPriority:]`)
- Tools (`#registerToolClassNamed:[atPriority:]`)
- User (`#registerUserClassNamed:[atPriority:]`)

It is possible to create a new category:

```Smalltalk
SessionManager default createCategory: aCategoryName.
SessionManager default createCategory: aCategoryName after: anotherCategoryName.
```

Then one can register classes in you new custom category:

```Smalltalk
SessionManager default register: (ClassSessionHandler forClassNamed: self name) inCategory: (SessionManager default categoryNamed: 'MyCategory').
SessionManager default register: (ClassSessionHandler forClassNamed: self name) inCategory: (SessionManager default categoryNamed: 'MyCategory') atPriority: 30.
```

## Start-up, Shut-down and Saving actions

Once a class is registered in the `SessionManager`, it is possible to add start-up and shut-down actions that will be executed when we open, close or save an image.

To register a start-up action, you need to implement a #startUp: method in the class side of your registered class.
This method is called in two situations: when you launch an image and when after you save an image.
This method takes a boolean as parameter. The value of this parameter is true when we start the image and false when we save it. 

Example:

```Smalltalk
MyClass class>>startUp: isImageStarting
	Transcript traceCr:
		(isImageStarting
			ifTrue: [ 'Image is starting' ]
			ifFalse: [ 'Image was saved' ])
```

To register a shut-down action, you need to implement a #shutDown: method in the class side of your registered class.
This method is called in two situations: when you quit an image and when you save an image.
This method takes a boolean as parameter. The value of this parameter is true when we quit the image and false when we save it. 

Example:

```Smalltalk
MyClass class>>shutDown: isImageQuitting
	Transcript traceCr:
		(isImageQuitting
			ifTrue: [ 'Image is quitting' ]
			ifFalse: [ 'Image is been saved' ])
```













