# Baselines

Pharo projects often requires a configuration to declare the how they should be loaded. This configuration is done via **Baselines**. A baseline defines the packages of the project, their dependencies to each other and to extermal projects and independant sub-groups that can be loaded.

In this documentation we will first explain how to write a baseline, then we will explain how to load a project from its baseline.

> **TODO: Add documentation about #includes: (Includes allow to say "When XXX is loaded, I must be loaded before it". It's really useful when we define spec for specific attributes)**

- [Baselines](#baselines)
  * [How to define Baselines](#how-to-define-baselines)
    + [Define packages forming your project](#define-packages-forming-your-project)
    + [Define external dependencies](#define-external-dependencies)
      - [To other remote git projects](#to-other-remote-git-projects)
        * [Depends on the same project with different groups](#depends-on-the-same-project-with-different-groups)
      - [To a local git project](#to-a-local-git-project)
      - [To smalltalkhub projects](#to-smalltalkhub-projects)
    + [Groups](#groups)
      - [The default group](#the-default-group)
    + [Pre/post load actions](#pre-post-load-actions)
    + [Loads different packages depending on the Pharo version](#loads-different-packages-depending-on-the-pharo-version)
    + [Define custom attributes](#define-custom-attributes)
    + [Loading types](#loading-types)
      - [Linear loading](#linear-loading)
      - [Atomic loading](#atomic-loading)
    + [Full example](#full-example)
  * [How to load a git project using its baseline](#how-to-load-a-git-project-using-its-baseline)
    + [From Iceberg](#from-iceberg)
    + [From the playground](#from-the-playground)

## How to define Baselines

The first step to create a baseline is to create a new subclass of `BaselineOf` (in the following code snippet, `{MyProject}` is to be substituted by the name of your project):

```Smalltalk
BaselineOf subclass: #BaselineOf{MyProject}
	slots: {  }
	classVariables: {  }
	package: 'BaselineOf{MyProject}'
``` 

Then, you will need to create a method that will define the spec of the project for the commit it will be included in.

```Smalltalk
baseline: spec
	<baseline>
	spec
		for: #common
		do: [
			"The main code of the baseline will go here"
		 ]
```

> The name of this method is not constrained to be `#baseline:`, it is the `<baseline>` pragma which specify that the method defines the spec of the project. However, it is common place to name this method `#baseline:`.

### Define packages forming your project

To define packages of the project, just send the message `#package:` to the spec with the name of the package as argument.

```Smalltalk
baseline: spec
	<baseline>
	spec
		for: #common
		do: [
			"Packages"
			spec
				package: 'MyProject';
				package: 'MyProject-Tests';
				package: 'MyProject-Gui';
				package: 'MyProject-Gui-Tests';
				package: 'MyProject-Examples' ]
```

Defining packages is not enough to load them because some of them might depend on other packages/projects. For example, `MyProject-Tests` needs to be loaded after `MyProject`.

To manage dependencies external to a project, see section [Define external dependencies](#define-external-dependencies).

To manage dependencies between the packages of your project, you can use the message `#package:with:` to give more informations to the spec.

```Smalltalk
baseline: spec
	<baseline>
	spec
		for: #common
		do: [
			"Packages"
			spec
				package: 'MyProject';
				package: 'MyProject-Tests' with: [ spec requires: #('MyProject') ];
				package: 'MyProject-Gui' with: [ spec requires: #('MyProject') ];
				package: 'MyProject-Gui-Tests' with: [ spec requires: #('MyProject-Tests') ];
				package: 'MyProject-Examples' with: [ spec requires: #('MyProject-Gui') ] ]
```

### Define external dependencies

Defining external dependencies can be done in different ways depending on where the dependency is hosted. 

> For improved readability I recommand to extract the definition of dependencies to external methods.

#### To other remote git projects

To depend on a git project hosted project you can use the method #baseline:with:.

```Smalltalk
	spec
		baseline: '{BaselineName}'
		with: [ spec repository: '{prefix}://{owner}/{projectName}:{version}/{subfolder}' ]
```

This snippet should be configured with:

* The `{BaselineName}`: The name of the baseline to load (For example it would be `'MaterialDesignLite'` to load the `BaselineOfMaterialDesignLite`)
* The `{prefix}`: This one will be specific to the host. It can be:
	* `github` for github
	* `bitbucket` for bitbucket
	* `gitlab` for gitlab
* The `{owner}`: Name of the user or organisation hosting the project
* The `{projectName}`: Name of the project
* The `{version}`: This parameter is optional (it will take master by default). It can be: the name of a branch, a tag like `'v1.2.0'` or `'v1.x.x'`, or a the SHA of a commit
* The `{subfolder}`: This parameter is optional in case the code is at the root of the project. It should point to the subfolder containing the code.

Example:

```Smalltalk
	spec
		baseline: 'MaterialDesignLite' 
		with: [ spec repository: 'github://DuneSt/MaterialDesignLite:v1.x.x/src']
```

This snippet can also be personalized to load only a specific group of the dependency like this:

```Smalltalk
	spec
		baseline: 'MaterialDesignLite'
		with: [ 
			spec 
				loads: #('Extensions');
				repository: 'github://DuneSt/MaterialDesignLite:v1.x.x/src'
		]
```

Once the dependency is defined, you just need to add the `BaselineName` to the list of the required dependencies of the package depending on it. 

Example:


```Smalltalk
baseline: spec
	<baseline>
	spec
		for: #common
		do: [
			"Dependencies"
			self materialDesignLite: spec.

			"Packages"
			spec
				package: 'MyProject';
				package: 'MyProject-Tests' with: [ spec requires: #('MyProject') ];
				package: 'MyProject-Gui' with: [ spec requires: #('MyProject' 'MaterialDesignLite') ];
				package: 'MyProject-Gui-Tests' with: [ spec requires: #('MyProject-Tests') ];
				package: 'MyProject-Examples' with: [ spec requires: #('MyProject-Gui') ] ].
```

```Smalltalk
materialDesignLite: spec
	spec
		baseline: 'MaterialDesignLite'
		with: [
			spec  
				loads: #('Extensions');
				repository: 'github://DuneSt/MaterialDesignLite:v1.x.x/src'
		]
```

##### Depends on the same project with different groups

Sometimes, you might want to depend on a project, but two packages will depend on different groups of this external project. 

For that case you can use the message `#project:copyFrom:with:` to create a new dependency spec.

```Smalltalk
materialDesignLite: spec
	spec
		baseline: 'MaterialDesignLite' with: [ spec repository: 'github://DuneSt/MaterialDesignLite:v1.x.x/src' ];
		project: 'MaterialDesignLiteExtensions' copyFrom: 'MaterialDesignLite' with: [ spec loads: #('Extensions') ]
```

Then you can use the new project name in the specification of dependencies.

Example:


```Smalltalk
baseline: spec
	<baseline>
	spec
		for: #common
		do: [
			"Dependencies"
			self materialDesignLite: spec.

			"Packages"
			spec
				package: 'MyProject';
				package: 'MyProject-Tests' with: [ spec requires: #('MyProject') ];
				package: 'MyProject-Gui' with: [ spec requires: #('MyProject' 'MaterialDesignLiteExtensions') ];
				package: 'MyProject-Gui-Tests' with: [ spec requires: #('MyProject-Tests' 'MaterialDesignLite' "We load the version containing MDL tests for our tests only") ];
				package: 'MyProject-Examples' with: [ spec requires: #('MyProject-Gui') ] ].
```

```Smalltalk
materialDesignLite: spec
	spec
		baseline: 'MaterialDesignLite' with: [ spec repository: 'github://DuneSt/MaterialDesignLite:v1.x.x/src' ];
		project: 'MaterialDesignLiteExtensions' copyFrom: 'MaterialDesignLite' with: [ spec loads: #('Extensions') ]
```

#### To a local git project

Sometimes, we do not have access to internet and we want to define dependencies to local git repositories. 

Those works like in the previous section but with this repository format:

```Smalltalk
	spec
		baseline: 'MaterialDesignLite'
		with: [ spec repository: 'gitlocal://full/path/to/repository' ]
```

#### To smalltalkhub projects

Depending on a [Smalltalkhub](http://smalltalkhub.com) project is done via `#project:with`.

```Smalltalk
	spec
		project: '{DependencyName}'
		with: [ spec
				className: #ConfigurationOf{ConfigurationName};
				versionString: #'{Version}';
				repository: 'http://smalltalkhub.com/mc/{owner}/{repositoryName}/main/' ]
```

The snippet should be configured with:

* The `{DependencyName}`: It can be anything different from your packages, groups and other dependencies names. It will be used to define dependency to this project in your packages/groups
* The `{ConfigurationName}`: It is the name of the configuration you wish to reference
* The `{Version}`: Name of the version you wish to reference. It can be something like `'development'`, `'stable'`, `'release1'`, `'1.2.6'`, `'1.0-baseline'`, etc.
* The `{owner}`: Name of the team or user hosting the project
* The `{repositoryName}`: Name of the repository on SmalltalkHub

Example:

```Smalltalk
	spec
		project: 'Magritte3'
		with: [ spec
				className: #ConfigurationOfMagritte3;
				versionString: #'release3';
				repository: 'http://smalltalkhub.com/mc/Magritte/Magritte3/main/' ]
```

As for git hosted repositories, you can reference a specific group:

```Smalltalk
	spec
		project: 'Magritte3'
		with: [ spec
				className: #ConfigurationOfMagritte3;
				versionString: #'release3';
				loads: #('Seaside');
				repository: 'http://smalltalkhub.com/mc/Magritte/Magritte3/main/' ]
```

You can now use the dependencies names to add the project as dependency of your packages.

```Smalltalk
baseline: spec
	<baseline>
	spec
		for: #common
		do: [
			"Dependencies"
			self magritte3: spec.

			"Packages"
			spec
				package: 'MyProject';
				package: 'MyProject-Tests' with: [ spec requires: #('MyProject') ];
				package: 'MyProject-Gui' with: [ spec requires: #('MyProject' 'Magritte3') ];
				package: 'MyProject-Gui-Tests' with: [ spec requires: #('MyProject-Tests') ];
				package: 'MyProject-Examples' with: [ spec requires: #('MyProject-Gui') ] ].
```

```Smalltalk
magritte3: spec
	spec
		project: 'Magritte3'
		with: [ spec
				className: #ConfigurationOfMagritte3;
				versionString: #'release3';
				loads: #('Seaside');
				repository: 'http://smalltalkhub.com/mc/Magritte/Magritte3/main/' ]

```

### Groups

We do not want to load the full project all the time. In some case we might want to loads a sub part of the project for different reasons:
* We might want to load only the model of a project without the UI to build an alternative UI
* We might want to be able to load it without the tests and examples
* The project might have some optional modules
* Etc.

To manage this, baselines have the concept of `Group`. A group is a loadable spec containing only a sub part of the project.

They can be declared with the `#group:with:` message. The second parameter will define the content of the group. The content can either be a package name, a dependency name or even another group name.

Example:

```Smalltalk
baseline: spec
	<baseline>
	spec
		for: #common
		do: [
			"Packages"
			spec
				package: 'MyProject';
				package: 'MyProject-Tests' with: [ spec requires: #('MyProject') ];
				package: 'MyProject-Gui' with: [ spec requires: #('MyProject') ];
				package: 'MyProject-Gui-Tests' with: [ spec requires: #('MyProject-Tests') ];
				package: 'MyProject-Examples' with: [ spec requires: #('MyProject-Gui') ] ].

			"Groups"
			spec
				group: 'Model' with: #('MyProject');
				group: 'Tests' with: #('MyProject-Tests' 'MyProject-Gui-Tests');
				group: 'Gui' with: #('MyProject-Gui');
				group: 'Example' with: #('MyProject-Examples');
				group: 'All' with: #('Model' 'Tests' 'Gui' 'Example')
```

#### The default group

By default, each baseline has a group named `'default'`. This group includes all the packages and the dependencies declared in the baseline.

When using the message `#load` with Metacello, or when you do not specify the group of a dependency, it will load the "default" group.

This group can be redefined by the user to change what will be loaded by default in a project.

Example:

```Smalltalk
baseline: spec
	<baseline>
	spec
		for: #common
		do: [
			"Packages"
			spec
				package: 'MyProject';
				package: 'MyProject-Tests' with: [ spec requires: #('MyProject') ];
				package: 'MyProject-Gui' with: [ spec requires: #('MyProject') ];
				package: 'MyProject-Gui-Tests' with: [ spec requires: #('MyProject-Tests') ];
				package: 'MyProject-Examples' with: [ spec requires: #('MyProject-Gui') ] ].

			"Groups"
			spec
				group: 'default' with: #('Model' 'Gui');
				group: 'Model' with: #('MyProject');
				group: 'Tests' with: #('MyProject-Tests' 'MyProject-Gui-Tests');
				group: 'Gui' with: #('MyProject-Gui');
				group: 'Example' with: #('MyProject-Examples');
				group: 'All' with: #('Model' 'Tests' 'Gui' 'Example')
```

### Pre/post load actions

Baselines allow to have some hooks to execute some code when loading a project. 

Those hooks are:

* `#preLoadDoIt:` : This hook is executed after the code and dependencies are resolved and fetched and before the code is loaded.
* `#postLoadDoIt:` : This hook is executed when the project finished to load.

Those methods take a symbol as parameter. This symbol should be the name of a method of the baseline that should be executed by the hook. 

Those methods take two optional parameters:

* A Metacello loader containing informations on the current project to load
* A Metacello spec containing the informations on the project spec

Example:

```Smalltalk
baseline: spec
	<baseline>
	spec
		for: #common
		do: [
			spec preLoadDoIt: #'preload:package:'.
			spec postLoadDoIt: #'postload:package:'.

			"Packages"
			spec
				package: 'MyProject';
				package: 'MyProject-Tests' with: [ spec requires: #('MyProject') ];
				package: 'MyProject-Gui' with: [ spec requires: #('MyProject') ];
				package: 'MyProject-Gui-Tests' with: [ spec requires: #('MyProject-Tests') ];
				package: 'MyProject-Examples' with: [ spec requires: #('MyProject-Gui') ] ]
```
```Smalltalk
preload: loader package: packageSpec

	Trascript crLog: 'The fetch was finished. Now let's load the project'

```
```Smalltalk
postload: loader package: packageSpec

	Trascript crLog: 'Project loaded!'

```

### Loads different packages depending on the Pharo version

It might be useful to load some packages only in specific Pharo versions. For example if we have a compatibility package for Pharo 6, we do not want to load it in pharo 7.

This is possible with the different spec attributes. 

Until here we defined everything in a spec for `#common`. This spec will impact every pharo versions. But it's possible to define spec only for some Pharo versions or even other Smalltalks. 

In order to do that we can add in the baseline a special #for:do: command taking as parameter a specific attribute.

Every Pharo version contains some default attributes. For a Pharo version X.Y we have:
* `#pharo`
* `#pharoX.x`
* `#pharoX.Y.x`

For example for Pharo 6.1:
* `#pharo`
* `#pharo6.x`
* `#pharo6.1.x`

Those attributes can be used to define a spec that will be executed only in the images containing the corresponding tags.

Example: 

```Smalltalk
baseline: spec
	<baseline>
	spec
		for: #common
		do: [
			"Packages"
			spec
				package: 'MyProject';
				package: 'MyProject-Tests' with: [ spec requires: #('MyProject') ];
				package: 'MyProject-Gui' with: [ spec requires: #('MyProject') ];
				package: 'MyProject-Gui-Tests' with: [ spec requires: #('MyProject-Tests') ];
				package: 'MyProject-Examples' with: [ spec requires: #('MyProject-Gui') ] ].

	spec
		for: #'pharo6.x'
		do: [ spec
				package: 'MyProject' with: [ spec requires: #('MyProject-Pharo6') ];
				package: 'MyProject-Pharo6' ].
	spec
		for: #(#'pharo3.x' #'pharo4.x' #'pharo5.x' #'pharo6.x')
		do: [ spec
				package: 'MyProject' with: [ spec requires: #('MyProject-Pharo3To6') ];
				package: 'MyProject-Pharo3To6' ] ]
```

### Define custom attributes

On top of attributes from Pharo, it is also possible to define our own attributes. 

To do that we can override the method `#customProjectAttributes` to return the custom attributes depending on the environment.

For example:

```Smalltalk
customProjectAttributes
	Smalltalk os isMacOS ifTrue: [ ^ #(#MacOS) ].
	Smalltalk os isUnix ifTrue: [ ^ #(#Unix) ].
	Smalltalk os isWindows ifTrue: [ ^ #(#Windows) ]
```

Then they can be used in the baseline.

```Smalltalk
baseline: spec
	<baseline>
	spec
		for: #common
		do: [
			"Packages"
			spec
				package: 'MyProject';
				package: 'MyProject-Tests' with: [ spec requires: #('MyProject') ];
				package: 'MyProject-Gui' with: [ spec requires: #('MyProject') ];
				package: 'MyProject-Gui-Tests' with: [ spec requires: #('MyProject-Tests') ];
				package: 'MyProject-Examples' with: [ spec requires: #('MyProject-Gui') ] ].

	spec
		for: #(#'MacOS' #'Unix') do: [ 
			self osSubprocess: spec.
			spec package: 'MyProject' with: [ spec requires: #('OSSubprocess') ] ];
		for: #'Windows' do: [ 
			self processWrapper: spec.
			spec package: 'MyProject' with: [ spec requires: #('ProcessWrapper') ] ]
```
```Smalltalk
osSubprocess: spec
	spec 
		baseline: 'OSSubprocess' 
		with: [ spec repository: 'github://pharo-contributions/OSSubprocess:v1.0.1/repository' ]
```
```Smalltalk
processWrapper: spec
	spec 
		configuration: 'ProcessWrapper' 
		with: [ 
			spec
				versionString: '1.2';
				repository: 'http://smalltalkhub.com/mc/hernan/ProcessWrapper/main' ]				
```

### Loading types

Baselines support different loading types. The loading types will define how Metacello will load the project. 

#### Linear loading

This type is the default one. If you change nothing to the baseline, it will use it.

When the load type is linear, the packages will be loaded one by one with their requirement loaded before them.

#### Atomic loading 

This load type will force Metacello to load the full project in an atomic load. This is useful when a project has cyclic dependencies that cannot be cut. For example it's useful to load Pharo's Kernel and Collections since they depends on each other.

To define the loading type as atomic you need to override the method #project:

```Smalltalk
project
	^ super project
		loadType: #atomic;
		youself
```

### Full example

Here is an example with everything presented:


```Smalltalk
"baseline"
baseline: spec
	<baseline>
	spec
		for: #common
		do: [
			spec preLoadDoIt: #'preload:package:'.
			spec postLoadDoIt: #'postload:package:'.

			"Dependencies"
			self materialDesignLite: spec.

			"Packages"
			spec
				package: 'MyProject';
				package: 'MyProject-Tests' with: [ spec requires: #('MyProject') ];
				package: 'MyProject-Gui' with: [ spec requires: #('MyProject' 'MaterialDesignLiteExtensions' 'Magritte3') ];
				package: 'MyProject-Gui-Tests' with: [ spec requires: #('MyProject-Tests' 'MaterialDesignLite' "We load the version containing MDL tests for our tests only") ];
				package: 'MyProject-Examples' with: [ spec requires: #('MyProject-Gui') ].

			"Groups"
			spec
				group: 'Model' with: #('MyProject');
				group: 'Tests' with: #('MyProject-Tests' 'MyProject-Gui-Tests');
				group: 'Gui' with: #('MyProject-Gui');
				group: 'Example' with: #('MyProject-Examples');
				group: 'All' with: #('Model' 'Tests' 'Gui' 'Example') ].

	spec
		for: #'pharo6.x'
		do: [ spec
				package: 'MyProject' with: [ spec requires: #('MyProject-Pharo6') ];
				package: 'MyProject-Pharo6' ].

	spec
		for: #(#'pharo3.x' #'pharo4.x' #'pharo5.x' #'pharo6.x')
		do: [ spec
				package: 'MyProject' with: [ spec requires: #('MyProject-Pharo3To6') ];
				package: 'MyProject-Pharo3To6' ] ].

	spec
		for: #(#'MacOS' #'Unix') do: [ 
			self osSubprocess: spec.
			spec package: 'MyProject' with: [ spec requires: #('OSSubprocess') ] ].

	spec
		for: #'Windows' do: [ 
			self processWrapper: spec.
			spec package: 'MyProject' with: [ spec requires: #('ProcessWrapper') ] ]
```
```Smalltalk
"dependencies"
materialDesignLite: spec
	spec
		baseline: 'MaterialDesignLite' with: [ spec repository: 'github://DuneSt/MaterialDesignLite:v1.x.x/src' ];
		project: 'MaterialDesignLiteExtensions' copyFrom: 'MaterialDesignLite' with: [ spec loads: #('Extensions') ]
```
```Smalltalk
"dependencies"
magritte3: spec
	spec
		project: 'Magritte3'
		with: [ spec
				className: #ConfigurationOfMagritte3;
				versionString: #'release3';
				loads: #('Seaside');
				repository: 'http://smalltalkhub.com/mc/Magritte/Magritte3/main/' ]

```
```Smalltalk
"dependencies"
osSubprocess: spec
	spec 
		baseline: 'OSSubprocess' 
		with: [ spec repository: 'github://pharo-contributions/OSSubprocess:v1.0.1/repository' ]
```
```Smalltalk
"dependencies"
processWrapper: spec
	spec 
		configuration: 'ProcessWrapper' 
		with: [ 
			spec
				versionString: '1.2';
				repository: 'http://smalltalkhub.com/mc/hernan/ProcessWrapper/main' ]				
```
```Smalltalk
"accessing"
customProjectAttributes
	Smalltalk os isMacOS ifTrue: [ ^ #(#MacOS) ].
	Smalltalk os isUnix ifTrue: [ ^ #(#Unix) ].
	Smalltalk os isWindows ifTrue: [ ^ #(#Windows) ]
```
```Smalltalk
"actions"
preload: loader package: packageSpec

	Trascript crLog: 'The fetch was finished. Now let's load the project'

```
```Smalltalk
"actions"
postload: loader package: packageSpec

	Trascript crLog: 'Project loaded!'

```
```Smalltalk
"accessing"
project
	^ super project
		loadType: #atomic;
		youself
```

## How to load a git project using its baseline

When you have a project with a *Baseline*, it is possible to load it in a Pharo image if the project is compatible with the Pharo version.

This section will explain how to load a project via its baseline.

### From the playground

The first way to load a project is to create a *Metacello* request programmaticaly and to execute it. This request looks like this:

```Smalltalk
Metacello new
	githubUser: 'DuneSt' project: 'MaterialDesignLite' commitish: 'v1.x.x' path: 'src';
	baseline: 'MaterialDesignLite';
	load
```

We can note three steps to load a project this way:
1. Create a new Metacello request
2. Parametrize it (declare the repository of the project, specify the version, the baseline, optional options...)
3. Launch the loading

To parametrize the request some options are necessary and some are optional. We will cover in the next two sub sections how to parametrize the loading of a project hosted via Monticello and Git, then we will detail optional options.

#### Project managed with Git

To load a project from git you need to execute an expression like this:

```Smalltalk
Metacello new
	repository: {repository};
	baseline: {baselineName};
	load
```

This command has two parameter:
- `repository` defining the location of the git project, the version of the project to load and the subdirectory in which the project is stored.
- `baselineName` is the name of the baseline to load. For example to load the `MaterialDesignLite` project, the baseline name is `MaterialDesignLite` to load the project with `BaselineOfMaterialDesignLite`.

The repository parameter is a string that can takes different form in case we have a local project or a project hosted remotely.

##### Project from github/gitlab/bitbucket

The repository parameter to load a project from github/gitlab/bitbucket takes this form:

`{prefix}://{owner}/{projectName}:{version}/{subFolder}`

This snippet should be configured with:

* The `{prefix}`: This one will be specific to the host. It can be:
	* `github` for github
	* `bitbucket` for bitbucket
	* `gitlab` for gitlab
* The `{owner}`: Name of the user or organisation hosting the project
* The `{projectName}`: Name of the project
* The `{version}`: This parameter is optional (it will take master by default). It can be: the name of a branch, a tag like `'v1.2.0'` or `'v1.x.x'`, or a the SHA of a commit
* The `{subfolder}`: This parameter is optional in case the code is at the root of the project. It should point to the subfolder containing the code.

Examples:

```Smalltalk
Metacello new
	repository: 'github://DuneSt/MaterialDesignLite:v1.x.x/src';
	baseline: 'MaterialDesignLite';
	load
```

Metacello also comes with some sytaxic suggar to define the repository to github or bitbucket:
* *Github*: `Metacello>>githubUser:project:commitish:path:`
* *Bitbucket*: `Metacello>>bitbucketUser:project:commitish:path:`

Example: 

```Smalltalk
Metacello new
	githubUser: 'DuneSt' project: 'MaterialDesignLite' commitish: 'master' path: 'src';
	baseline: 'MaterialDesignLite';
	onWarningLog;
	load
```

##### Project from local repository

To load a project from a local repository you can this form to declare the repository:

`'{prefix}://{full/path/to/repository}/{subFolder}'`

This snippet should be configured with:

* The `{prefix}`: This one will be specific to the file format:
	* `filetree` for a Filetree project
	* `tonel` for a Tonel project
* The `{full/path/to/repository}`: is the path to the project on the file system
* The `{subfolder}`: This parameter is optional in case the code is at the root of the project. It should point to the subfolder containing the code.

Example:

```Smalltalk
Metacello new
	repository: 'tonel://C:\Users\Cyril\GitRepositories\Native-Browser\src';
	baseline: 'NativeBrowser';
	load
```

#### Project managed with Monticello
**TODO**
#### Loading groups
**TODO**
#### Conflict and Upgrade resolution
**TODO**
#### Log warnings
**TODO**

### From Iceberg
**TODO**
### Metacello lock feature
**TODO**
### Metacello get feature
**TODO**
### Metacello record feature
**TODO**
