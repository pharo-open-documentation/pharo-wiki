# How to deploy a Pharo application

This guide has for vocation to help developpers deploying a Pharo application. If something is missing in your point of view, do not hesitate to open an issue.

**This guide was first written for Pharo 7. Some part will not work in Pharo 6 and earlier.**

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

Pharo works with a local folder containing caches. This folder is called `pharo-local` and is next to the image since Pharo 7. It is recommanded to delete this folder, or to not include this folder in the distribution.

## Sources obfuscation

When deploying a commercial application on the customer's infrastructure, we might want to protect our code. This section will give some advice on how to protect the source code of your application.

> WARNING: What will be describe here is not a perfect solution. We are aware it will still have some weakness, but at least it will make it much harder to get the source code of the application.

This section works with the previous section aswell. We recommand to:
* Run without the sources file
* Run without the changes file
* Disable Epicea
* Remove the loggins of your image

### Force omission of startup preferences

At launch Pharo try to load preferences. Since the user can execute Smalltalk code via those proferences, we recommand to disable the preference mechanism with this code:

```Smalltalk
PharoCommandLineHandler forcePreferencesOmission: true
```

### Protect command lines by a password

Since the user can intereact with Pharo via command line, we recommand to protect command lines with a password.

The password is not be saved in clear. It is hashed using pepper and iterations.

If you wish to define *application* command lines who does not need a password protection, implement the method `requireDeploymentPassword` on the class side of your command lines to return `false`.

```Smalltalk
"Enable password protection"
CommandLinePasswordManager protectCommandLinesByPasswordWith: 'PharoPassword'.

"You can also customize the pepper and number of iterations for the hashing of the password."
CommandLinePasswordManager protectCommandLinesByPasswordWith: 'PharoPassword' pepper: 'SomePepper' numberOfHashIterations: 10.

"Remove password protection"
CommandLinePasswordManager removePasswordProtection.
```

### Remove the decompiler

Without the sources and changes files the user does not have the source code shipped with the application, but he still has the byte code of the application. To make it harder to exploit if he succeed to get a part of the byte code, you can unload the decompiler from the image with the piece of code:

```Smalltalk
RPackageOrganizer default packages
	select: [ :p | p name includesSubstring: 'Flashback' ]
	thenDo: #removeFromSystem
```

### Disable global shortcuts

If the customer has access to the Pharo image it is recommanded to disable global shortcuts that can help to open tools. 

It is doable this way:

```Smalltalk
(KMRepository default globalCategories flatCollect: [ :each | each allEntries keymaps ]) do: #disable
```

### Disable WorldMenu, Menubar and Taskbar

To block the access to the tools it is possible to disable the world menu, the taskbar and the menu bar from Pharo with this piece of code:

```Smalltalk
"Disable world menu"
WorldState desktopMenuPragmaKeyword: ''.

"Disable taskbar"
TaskbarMorph showTaskbar: false.

"Disable Menubar"
MenubarMorph showMenubar: false.
```

### Disable progress bar interrupt button

If you show progresses in your application via a progress bar, the user can clic on the red cross to stop the action and open a debugger.

It is possible to remove this possibility executing:

```Smalltalk
JobProgressBarMorph isInterruptable: false
```

### Disable process interruption button

In Pharo, it is possible to disable the current process via the `cmd + .` shortcut. This feature can be disabled:

```Smalltalk
UserInterruptHandler cmdDotEnabled: false
```

### Disable drag and drop in Pharo

It is possible to drop files in Pharo to install code in it. It is recommanded to disable this feature to block users to inject code into the application. Since there is no setting to do that, you can recompile a part of the Pharo image to block it this way:

```Smalltlak
Author
	useAuthor: 'Deployment'
	during: [ WorldMorph
			compile:
				'wantsDropFiles: arg
	^ false' ]
```

### Disable Morph's Halos

To remove the option to open Halos around the Morphs of Pharo you can execute:

```Smalltalk
Morph halosEnabled: false
```

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
