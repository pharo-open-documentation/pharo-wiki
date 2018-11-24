# How to deploy a Pharo application

>TODO - Introduction - Explain that this guide was written from Pharo 7

- [How to deploy a Pharo application](#how-to-deploy-a-pharo-application)
  * [Clean your image before deployment](#clean-your-image-before-deployment)
  * [Sources obfuscation](#sources-obfuscation)
  * [Change the logo and window title of the application](#change-the-logo-and-window-title-of-the-application)
  * [Sign your application on Windows and OSX](#sign-your-application-on-windows-and-osx)
  * [Deploy a Seaside application with Nginx](#deploy-a-seaside-application-with-nginx)

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

It is possible to run a Pharo image without `.changes` or `.sources` files.

To do that you can execute:

```Smalltalk
NoChangesLog install.
NoPharoFilesOpener install
```

Note that FFI needs the sources to work properly. In Pharo 7 a pluggin to remove this need was introduced.
If your application uses FFI calls you will need to execute:

```Smalltalk
FFICompilerPlugin install
```

### Disable creation of STDio files on Windows

On Windows there is two way to launch a Pharo image. The first uses the `Pharo.exe` executable and the second `PharoConsole.exe`. The difference between those two methods is that the former will be launched without a terminal and will create files to write STDio outputs. The second will open a terminal with Pharo to manage STDio.

It is possible to disable the writing of the STDio files on Windows with this code:

```Smalltalk
Stdio useNullStreams
```

### Remove tests and example packages

In production examples and tests packages are useless. You can find bellow a script to unload them. 

> Be careful, this script is based on a heuristic. If the naming convensions or dependencies are not well managed this might break your application. Please test you application after using such a script.

```Smalltalk
| substrings |
substrings := #('Test' 'Example' 'Mock' 'Demo').

RPackageOrganizer default packages
		select: [ :p | substrings anySatisfy: [ :aString | p name includesSubstring: aString ] ]
		thenDo: #removeFromSystem
```

### Disable Monticello cache

Monticello uses by default a cache when it is used. It is possible to disable this cache with this script:

```Smalltalk
MCCacheRepository uniqueInstance disable
```

### Disable Epicea

Pharo contains a record system to recover changes: *Epicea*. This system log a lot of events on disk.

It is possible to disable Epicea like this:

```Smalltalk
EpMonitor reset
```

### Garbage collect

As last step of the deployment I would recommand the user to launch a full garbage collection of the system to clean all dead instances from the image:

```Smalltalk
5 timesRepeat: [ Smalltalk garbageCollect ]
```

### Delete pharo-local folder

## Sources obfuscation
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
