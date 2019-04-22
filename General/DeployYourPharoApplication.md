# How to deploy a Pharo application

*This guide has for purpose to help developpers deploying a Pharo application. If something is missing to you, do not hesitate to open an issue.*

**This guide is first written for Pharo 7. Some parts will not work in Pharo 6 and earlier.**

- [How to deploy a Pharo application](#how-to-deploy-a-pharo-application)
  * [Cruiser: A tool for app deployment?](#cruiser)
  * [Clean your image before deployment](#clean-your-image-before-deployment)
  * [Sources obfuscation](#sources-obfuscation)
  * [Change the logo and window title of the application](#change-the-logo-and-window-title-of-the-application)
  * [Sign your application on Windows and OSX](#sign-your-application-on-windows-and-osx)
  * [Deploy a Seaside application with Nginx](#deploy-a-seaside-application-with-nginx)

## Cruiser




## Clean your image before deployment

In this section we explain various ways to clean an image for deployment. All steps are not necessary and the benefits are given in each subsection.

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
"Disable world menu and menubar"
WorldState desktopMenuPragmaKeyword: ''.

"Disable Menubar only"
MenubarMorph showMenubar: false.

"Disable WorldMenu only"
PasteUpMorph shouldShowWorldMenu: false.

"Disable taskbar"
TaskbarMorph showTaskbar: false.
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

```Smalltalk
WorldMorph allowDropFiles: false
```

### Disable Morph's Halos

To remove the option to open Halos around the Morphs of Pharo you can execute:

```Smalltalk
Morph halosEnabled: false
```

### Disable Debuggeur

#### Pharo 8

Since Pharo 8, Pharo comes with new debuggers you can choose from.

When deploying a private application you don't want the user to get a bug and access to the code through it. For that you can use the `NoDebugger`:

```Smalltalk
NoDebugger beCurrent
```

#### Pharo < 8 or specific debuggers

In case you are using Pharo < 8 or you want a special handling of the bug you can create your own debugger.

For that you need to create a object and implement a class side method called `#openOn:withFullView:andNotification:`.

For example for a `NoDebugger`:

```Smalltalk
NoDebugger class>>openOn: aDebugSession withFullView: aBool andNotification: aString
	"Do nothing"
```

For a debugger exporting in a file the error:

```Smalltalk
openOn: aDebugSession withFullView: aBool andNotification: aString
	'errors.log'
		ensureCreateFile;
		writeStreamDo: [ :s |
			s
				setToEnd;
				<< 'ERROR. Here is the stack:';
				<< OSPlatform current lineEnding.
			aDebugSession interruptedContext shortDebugStackOn: s ]
```

You then need to register the debugger:

```Smalltalk
Smalltalk tools register: NoDebugger as: #debugger
```

### Open a morph in full screen

When deploying the application, while there is no real headless mode in Pharo or when the application contains a user interface embedded we can open a Morph in full screen to ensure the user cannot access to content behind it. 

To do that we can create a Spec application (in case of headless application it can just contains a logo and a `quit` button) and open it in full screen with this command:

```Smalltalk
MyPresenter new openWorldWithSpec
```

## Change the logo and window title of the application

In Windows it is possible to change the title and the logo of the Pharo application. 

To do that you can execute:

```Smalltalk
DisplayScreen setWindowTitle: 'MyApplication'.

DisplayScreen hostWindowIcon: (FileLocator imageDirectory / 'testLogo.ico') asFileReference fullName.
```

## Sign your application on Windows and OSX

> TODO

## Deploy a Seaside application

This section will cover a specific kind of deployment: the deployment of a web application with Seaside.

### Prepare the image

It is recommanded to prepare the image for deployment. This section will cover some possible configurations you can apply to your image.

> Note: This is section only contains suggestions and it might be missing interesting options. If you have an idea of missing section do not hesitate to open an issue.

#### Set server mode

When deploying an application as a server, it is possible to change a setting to slow down the rendering cycle of the image and increase performances. It can be done like this:

```Smalltalk
WorldState serverMode: true
```

#### Unregister applications

When building your seaside image it is possible that some Seaside application got registered (demos for examples). 

It is possible to unregister them like this:

```Smalltalk
applicationsToUnregister := WAAdmin defaultDispatcher handlers keys \ #('myApplication' 'files' 'myHandler' 'config').
applicationsToUnregister do: [ :appName | WAAdmin defaultDispatcher unregister: (WAAdmin defaultDispatcher handlerAt: appName) ]
```

#### Set default application

Applications must have a name in Seaside and the name should be in the URL. However, it is possible to define a default application which will be selected in case no application name is in the URL. 

```Smalltalk
WAAdmin defaultDispatcher defaultName: 'myApplicationName'
```

#### Disable development toolbar

In case you load a version of Seaside containing development tools, your application will come with a development toolbar. 

It is possible to remove it by executing:

```Smalltalk
WAAdmin applicationDefaults removeParent: WADevelopmentConfiguration instance ].
```

Then you need to initialize you application. 

#### Protect configuration by a password

In case you keep the configuration application available to the user, you might want to protect it by a password. It wan be done like this:

```Smalltalk
| application |
application := WAAdmin defaultDispatcher handlerAt: 'config'.
application configuration addParent: WAAuthConfiguration instance.
application
	preferenceAt: #login put: 'admin';
	preferenceAt: #passwordHash put: (GRPlatform current secureHashFor: 'seasideiscool').
application addFilter: WAAuthenticationFilter new
```

### Deploy with a Ngnix server

This section will cover the configuration needed to deploy an image with a nginx server.

#### Launch the image

In general I (the author of this decumentation) use a Jenkins to generate my image and then I have a script to deploy the image.

My script looks like this:

```bash
#!/usr/bin/env bash
set -vx

# Where to deploy the application
export DEST=/srv/app/mdl
# Pharo version for the deployment
export PHARO=61
# Location of the zip containing the archive
export ARCHIVE_LOCATION=/var/lib/jenkins/workspace/MaterialDesignLite/PHARO/$PHARO/VERSION/master/

# To launch an image I uses a screen, else the image will close with my ssh session. Here I ensure the session used is closed.
screen -S mdl -X quit

# Remove the old version
rm -rf $DEST/{MaterialDesignLite.*,pharo*,*.sources,Pharo*}

# Copy the application
cp ${ARCHIVE_LOCATION}MaterialDesignLite.zip $DEST/
cd $DEST

# Get a VM and unzip the application
wget --quiet -O - get.pharo.org/vm${PHARO} | bash
unzip MaterialDesignLite.zip

# Launch the application and initialize it on a free and open port
./pharo MaterialDesignLite.image eval --save "
MDLDemo initializeAs: 'mdl'.

WAAdmin defaultDispatcher defaultName: 'mdl'.

ZnZincServerAdaptor startOn: 8088"

#Launch the image in a screen
screen -Sdm mdl ./pharo MaterialDesignLite.image --no-quit
```
#### HTTP deployment

Now that the image is launched we need to dispatch it via nginx.

In order to do that over HTTP I uses this configuration:

```nginx
server {
  listen 80; #Since it's a web application, listen port 80
  listen [::]:80; #Same for IPv6
  server_name {Domaine name. Example mysite.com}; #Set your domaine name
  server_tokens off;  #Do not display nginx version for security reasons

  access_log /var/log/nginx/{log name}.log; #loging
  error_log /var/log/nginx/{error log name}.log; #error loging

  root {Path to the root. For example /srv/myApp/};

  location = / {
    try_files $uri $uri/index.html @proxy;
  }

  #use a proxy for your seaside application
  location @proxy {
    rewrite ^ /{Seaside application name. For example TelescopeDemo}$1 last;
  }

  location /{Seaside application name. For example TelescopeDemo} {
    proxy_set_header Host $host;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_pass http://127.0.0.1:{Port on which your ZincServerAdaptor listen. For example 8080};
  }

  # This is for the file libraries
  location /files {
    proxy_set_header Host $host;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_pass http://127.0.0.1:{Port on which your ZincServerAdaptor listen. For example 8080};
  }

}
```
#### HTTPS deployment

The previous option works but is not secured. It is recommanded to generate a TLS certificate (with `let's encrypt` via `certbot` for example) and to deploy over HTTPS.

Then the configuration will look like this:

```nginx
server {
  listen 80; #Since it's a web application, listen port 80
  listen [::]:80; #Same for IPv6
  server_name {Domaine name. Example mysite.com}; #Set your domaine name
  server_tokens off;  #Do not display nginx version for security reasons
  return 301 https://$server_name$request_uri; #Redirect HTTP -> HTTPS
}

server {
  listen 443 ssl http2; #Listen to port 443 for HTTPS
  listen [::]:443 ssl http2; #Same for IPv6
  server_name {Domaine name. Example mysite.com}; #Set your domaine name
  server_tokens off;  #Do not display nginx version for security reasons
  ssl_certificate {path to your public certificate key}.pem;
  ssl_certificate_key {path to your private certificate key}.pem;

  access_log /var/log/nginx/{log name}.log; #loging
  error_log /var/log/nginx/{error log name}.log; #error loging

  root {Path to the root. For example /srv/myApp/};

  location = / {
    try_files $uri $uri/index.html @proxy;
  }

  #use a proxy for your seaside application
  location @proxy {
    rewrite ^ /{Seaside application name. For example TelescopeDemo}$1 last;
  }

  location /{Seaside application name. For example TelescopeDemo} {
    proxy_set_header Host $host;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_pass http://127.0.0.1:{Port on which your ZincServerAdaptor listen. For example 8080};
  }

  # This is for the file libraries
  location /files {
    proxy_set_header Host $host;
    proxy_set_header X-Real-IP $remote_addr;
    proxy_pass http://127.0.0.1:{Port on which your ZincServerAdaptor listen. For example 8080};
  }
}
```

