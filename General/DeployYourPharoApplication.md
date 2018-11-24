# How to deploy a Pharo application

>TODO

## Clean your image before deployment

In this section we will explain divers way to clean an image for deployment. All steps are not necessary and we will explain the benefits in each subsection.

### Clean up the system

A first step is to launch a cleanup of the image. Pharo already contains a system to cleanup divers part of the system by calling the `#cleanUp(:)` method of every class implementing it.

It can be done with this snippet:

```Smalltalk
  Smalltalk cleanUp: true except: {} confirming: false
```

The first parameter should be a boolean. If it's value is `true` it will launch a more aggresive cleanup and this will destroy resources, change sets, etc.
The second parameter allows the developper to exclude some classes of the cleanup by passing an array containing all the classes that should not execute their cleanup.

### Ensure logins are removed from the image

During the deployment of an application it might be a good idea to remove all login informations from the image.

In order to do that you can execute this script:

```Smalltalk
| store |

"Remove all repositories from Monticello VCS"
MCRepositoryGroup allSubInstancesDo: [ :group | group repositories do: [ :repo | group removeRepository: repo ] ].

"Remove projects and credentials from Git VCS (Tested in Pharo 7)"
IceRepository registry removeAll.
store := IceCredentialStore current.
store allCredentials do: [ :each | each removeFrom: store ]
```

With this step done all the credentials should be removed from the image if you do not use an external project from Pharo that store some credentials.

### Close all windows

While deploying an application you might want to ensure every Morph is closed (to remove the Pharo welcome window for example).

This can directly be done via the command:

```Smalltalk
World closeAllWindowsDiscardingChanges
```

### Disable deprecation warning

Pharo contains a deprecation warning system to help the developers to keep their code up to date but in productions those warnings should be removed so that the user will not be bothered by them in case a deprecated method is called.

You can do that via:

```Smalltalk
Deprecation
		raiseWarning: false;
		showWarning: false
```

### Enable the run without sources and changes files

> TODO

### Disable creation of STDio files on Windows

> TODO

### Remove tests and example packages

> TODO

### Disable Monticello cache

> TODO

### Disable Epicea

> TODO

### Garbage collect

> TODO

## Block access to the sources

> TODO

### Force omission of startup preferences

> TODO

### Protect command lines by a password

> TODO

### Remove the decompiler

> TODO

### Disable global shortcuts

> TODO

### Disable WorldMenu and MenuBar

> TODO

### Disable progress bar interrupt button

> TODO

### Disable process interruption button

> TODO

### Disable drag and drop in Pharo

> TODO

### Disable Morph's Halos

> TODO

### Hide Taskbar

> TODO

### Disable Debuggeur

> TODO

### Open a morph in full screen

> TODO

## Change the logo and window title of the application

> TODO

## Sign your application on Windows and OSX

> TODO

## Deploy a Seaside application with Nginx

> TODO Don't forget to talk about the server mode of the world state
