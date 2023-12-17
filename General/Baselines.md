# Baselines

Pharo projects often require a configuration to declare how they should be loaded. This configuration is done via **Baselines**. A baseline defines the packages of the project, their dependencies to each other and to external projects and independent sub-groups that can be loaded.

Adding a baseline to a project has some advantages:
- It makes it easier to load the project
- It makes it easier for others to contribute to your project
- It allows the users of the project to be unaware of the project's dependencies
- It makes explicit the dependencies of the project
- It ensures that packages and dependencies of the project are loaded in the right order

This documentation explains how to write a baseline and how to load the project described by this baseline.

- [Baselines](#baselines)
	- [How to define Baselines](#how-to-define-baselines)
		- [Define packages forming your project](#define-packages-forming-your-project)
		- [Define external dependencies](#define-external-dependencies)
			- [To other remote git projects](#to-other-remote-git-projects)
				- [Depend on a subset of a git project](#depend-on-a-subset-of-a-git-project)
				- [Depends on the same project with different groups](#depends-on-the-same-project-with-different-groups)
			- [To a local git project](#to-a-local-git-project)
			- [To smalltalkhub projects](#to-smalltalkhub-projects)
		- [Groups](#groups)
			- [The default group](#the-default-group)
		- [Pre/post load actions](#prepost-load-actions)
		- [Loads different packages depending on the Pharo version](#loads-different-packages-depending-on-the-pharo-version)
		- [Define custom attributes](#define-custom-attributes)
		- [Loading types](#loading-types)
			- [Linear loading](#linear-loading)
			- [Atomic loading](#atomic-loading)
		- [Full example](#full-example)
	- [How to load a git project using its baseline](#how-to-load-a-git-project-using-its-baseline)
		- [From the playground](#from-the-playground)
			- [Project managed with Git](#project-managed-with-git)
				- [Project from github/gitlab/bitbucket](#project-from-githubgitlabbitbucket)
				- [Project from local repository](#project-from-local-repository)
			- [Project managed with Smalltalkhub](#project-managed-with-smalltalkhub)
			- [Project without repository](#project-without-repository)
			- [Loading groups](#loading-groups)
			- [Conflict, Upgrade and Downgrade resolution](#conflict-upgrade-and-downgrade-resolution)
			- [Manage warnings](#manage-warnings)
		- [From Iceberg](#from-iceberg)
	- [Other features](#other-features)
		- [Metacello lock feature](#metacello-lock-feature)
		- [Metacello get feature](#metacello-get-feature)
		- [Metacello fetch feature](#metacello-fetch-feature)
		- [Metacello record feature](#metacello-record-feature)
		- [Metacello listing feature](#metacello-listing-feature)
	- [See also](#see-also)

## How to define Baselines

The first step to create a baseline is to create a new subclass of `BaselineOf`. In the following example, `MyProject` is to be substituted by the name of your project:

```Smalltalk
BaselineOf subclass: #BaselineOfMyProject
	slots: {  }
	classVariables: {  }
	package: 'BaselineOfMyProject'
```

This class should be in a package separated from other packages' projects. The package holding the baseline **must** have the same name as the baseline. To summarize, `BaselineOfMyProject` class is in the `BaselineOfMyProject` package.

Then, create a method that defines the spec of the project for the commit it will be included in.

```Smalltalk
baseline: spec
	<baseline>
	spec
		for: #common
		do: [
			"The main code of the baseline will go here"
		]
```

> The name of this method does not have to be `#baseline:`; however, that is the name that is commonly used. In fact, it is the `<baseline>` pragma which specifies that the method defines the spec of the project.

If your project is stored using a metadataless format (Tonel or FileTree metadataless), which is the default since Pharo 6, you need to add this method to your baseline:

```Smalltalk
projectClass
	^ MetacelloCypressBaselineProject
```

Or, if the project should be loadable in Pharo < 6.1, use this version:

```Smalltalk
projectClass
	^ [ self class environment at: #MetacelloCypressBaselineProject ]
	on: NotFound
	do: [ super projectClass ]
```
> The method is common to all projets using the metadataless format and the class return does not depend on the name of your baseline.

This will allow Metacello to be able to update your project and is needed because the default project class of Metacello used metadata to know if an update was needed.

### Define packages forming your project

To define the packages of the project, send the message `#package:` to the spec with the name of the package as argument.

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

> Note: Packages are the most atomic entities managed by the baseline. It is not possible to declare entities at the package-tag granularity.

Defining packages is not enough to load them, because some of them might depend on other packages/projects. For example, `MyProject-Tests` needs to be loaded after `MyProject`.

To manage dependencies that are external to a project, see section *[Define external dependencies](#define-external-dependencies)*.

For dependencies between the packages of your project, you can use the message `#package:with:` to give more information to the spec.

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

The method `#requires:` will define the list of dependencies of a specific package.

Another way to declare requirements is to use the method `#includes:`. This method takes a collection of declarations as a parameter and will notify Metacello that all of them should include the package if they are loaded. This is helpful when defining platform-specific requirements, in case we want one of our packages to come with a platform-dependant package, which is depending on this package. See example in section *[Loads different packages depending on the Pharo version](#loads-different-packages-depending-on-the-pharo-version)*.

### Define external dependencies

Defining external dependencies can be done in different ways depending on where the dependency is hosted.

> To improve readability, I recommend extracting the definitions of dependencies into separate methods.

#### To other remote git projects

To depend on a git project, you can use the method `#baseline:with:`.

```Smalltalk
spec
	baseline: '{BaselineName}'
	with: [ spec repository: '{prefix}://{url}:{owner}/{projectName}:{version}/{subfolder}' ]
```

This snippet should be configured with:

- `{BaselineName}`: The name of the baseline to load (e.g, `'MaterialDesignLite'` to load `BaselineOfMaterialDesignLite`)
- `{prefix}`: This is host-specific:
  - `github` for github
  - `bitbucket` for bitbucket
  - `gitlab` for gitlab
  - `git` for others (and {url} is thus mandatory)
- `{url}`: Base url to the git host. Mandatory when prefix `git` is used, optional for other prefixes (can be useful for self hosted gitlab for example)
- `{owner}`: Name of the user or organization hosting the project
- `{projectName}`: Name of the project
- `{version}`: This parameter is optional (defaults to master). It can be the name of a branch, a tag like `'v1.2.0'` or `'v1.x.x'`, or a the SHA of a commit
- `{subfolder}`: This parameter is optional in case the code is not at the root of the project. It should point to the sub-folder containing the code

Example:

```Smalltalk
spec
	baseline: 'MaterialDesignLite'
	with: [ spec repository: 'github://DuneSt/MaterialDesignLite:v1.x.x/src']
```

##### Depend on a subset of a git project

Some projects can defines `groups` in their baselines. They are subsets of the project that can be loaded independently.
The previous snippet can also be customized to load only a specific group of the dependency like this:


```Smalltalk
spec
	baseline: 'MaterialDesignLite'
	with: [
		spec
		loads: #('Extensions');
		repository: 'github://DuneSt/MaterialDesignLite:v1.x.x/src'
	]
```

Once the dependency is defined, add `BaselineName` to the list of the required dependencies of the package depending on it.

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

In some cases your project might depend on an external project, but two packages of your project depend on different groups of this external project.

You can use the message `#project:copyFrom:with:` to create a new dependency spec.

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

Sometimes we do not have access to a network, so we want to define dependencies to local git repositories.

This works like in the previous section, but with this repository format:

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

- `{DependencyName}`: It can be anything from your packages, groups and other dependencies names. It will be used to define dependency to this project in your packages/groups
- `{ConfigurationName}`: It is the name of the configuration you wish to reference
- `{Version}`: Name of the version you wish to reference. It can be something like `'development'`, `'stable'`, `'release1'`, `'1.2.6'`, `'1.0-baseline'`, etc.
- `{owner}`: Name of the team or user hosting the project
- `{repositoryName}`: Name of the repository on SmalltalkHub

Example:

```Smalltalk
spec
	project: 'Magritte3'
	with: [ spec
		className: #ConfigurationOfMagritte3;
		versionString: #'release3';
		repository: 'http://smalltalkhub.com/mc/Magritte/Magritte3/main/' ]
```

As for git-hosted repositories, you can ask for a specific group:

```Smalltalk
spec
	project: 'Magritte3'
	with: [ spec
		className: #ConfigurationOfMagritte3;
		versionString: #'release3';
		loads: #('Seaside');
		repository: 'http://smalltalkhub.com/mc/Magritte/Magritte3/main/' ]
```

You can now use the dependency names to add the project as a dependency of your packages.

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

Sometimes we don't want to load the full project, but just a sub part, e.g.:

- Only the model of a project is needed without the UI (for example to build an alternative UI)
- Only the core of the project is needed without the tests and examples
- The project has some optional modules
- etc.

To manage such cases, baselines have the concept of a `Group`. A group is a loadable spec containing only a sub part of the project.

They can be declared with the `#group:with:` message. The second parameter defines the content of the group. The content can either be a package name, a dependency name, or even another group name.

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

> To load a project with a given group, you can check the section [loading groups](#loading-groups).

#### The default group

Each baseline has a default group named `'default'`. This group includes all the packages and the dependencies declared in the baseline.

When using the message `#load` with Metacello, or when not specifying the group of a dependency, it will load the "default" group.

This group can be redefined to change what will be loaded by default in a project.

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

Baselines provide some hooks to execute some code when loading a project.

Those hooks are:

- `#preLoadDoIt:` which is executed after the code and dependencies are resolved and fetched, but before the code is loaded.
- `#postLoadDoIt:` which is executed when the project finishes loading.

Those methods take a symbol as parameter, which should be the name of a method of the baseline that should be executed by the hook.

Those methods can take two optional parameters:

- A Metacello loader containing information on the current project to load
- A Metacello spec containing information on the project spec

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

	Transcript crLog: 'The fetch was finished. Now let''s load the project'
```

```Smalltalk
postload: loader package: packageSpec

	Transcript crLog: 'Project loaded!'
```

### Loads different packages depending on the Pharo version

It might be useful to load some packages in specific Pharo versions only. For example, if we have a compatibility package for Pharo 6, we do not want to load it in Pharo 7.

This is possible with the different spec attributes.

Up until now we defined everything in a spec for `#common`, which applies to all versions of Pharo. But it's possible to define a spec for specific Pharo versions or even other Smalltalk environments.

We can add in the baseline a special `#for:do:` command taking as parameter a specific attribute.

Every Pharo version contains some default attributes. For a Pharo version X.Y we have:

- `#pharo`
- `#pharoX.x`
- `#pharoX.Y.x`

For example for Pharo 6.1:

- `#pharo`
- `#pharo6.x`
- `#pharo6.1.x`

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

The `#includes:` method explained in section *[Define packages forming your project](#define-packages-forming-your-project)* is often useful when dealing with platform-specific requirements. Imagine your package `MyProject` will work in Pharo 6 only if `MyProject-Pharo6` is present, but `MyProject-Pharo6` depends on `MyProject`. This can be resolved like this:

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
					package: 'MyProject' with: [ spec includes: #('MyProject-Pharo6') ];
					package: 'MyProject-Pharo6' with: [ spec requires: #('MyProject') ] ].
			spec
				for: #(#'pharo3.x' #'pharo4.x' #'pharo5.x' #'pharo6.x')
				do: [ spec
					package: 'MyProject' with: [ spec requires: #('MyProject-Pharo3To6') ];
					package: 'MyProject-Pharo3To6' ] ]
```

### Define custom attributes

On top of attributes from Pharo, it's also possible to define our own attributes.

We override the method `#customProjectAttributes` to return the custom attributes depending on the environment.

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

Baselines support different loading types. The loading types define how Metacello loads the project.

#### Linear loading

By default, a baseline uses linear loading, which means packages are loaded one by one with their requirements loaded before them.

#### Atomic loading

This load type forces Metacello to load the full project in an atomic load. This is useful when a project has cyclic dependencies that cannot be resolved. For example it's useful to do an atomic load of Pharo's Kernel and Collections, since they depend on each other.

To define atomic loading, override the method `#project`:

```Smalltalk
project
	^ super project
		loadType: #atomic;
		yourself
```

### Full example

Here is an example with all previous features illustrated:

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
					package: 'MyProject' with: [ spec includes: #('MyProject-Pharo6') ];
					package: 'MyProject-Pharo6' with: [ spec requires: #('MyProject') ] ].

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
projectClass
	^ MetacelloCypressBaselineProject
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

	Transcript crLog: 'The fetch was finished. Now let''s load the project'
```

```Smalltalk
"actions"
postload: loader package: packageSpec

	Transcript crLog: 'Project loaded!'
```

```Smalltalk
"accessing"
project
	^ super project
		loadType: #atomic;
		yourself
```

## How to load a git project using its baseline

When you have a project with a *Baseline*, it is possible to load it in a Pharo image if the project is compatible with the Pharo version.

Here we explain how to load a git project via its baseline.

### From the playground

The first way to load a project is to create a *Metacello* request programmatically and to execute it. This request looks like this:

```Smalltalk
Metacello new
	githubUser: 'DuneSt' project: 'MaterialDesignLite' commitish: 'v1.x.x' path: 'src';
	baseline: 'MaterialDesignLite';
	load
```

Note the three steps:

1. Create a new Metacello request
2. Configure it (declare the repository of the project, specify the version, the baseline, optional options...)
3. Launch the loading

To configure the request, some options are necessary and some are optional. We cover in the next two sub sections how to configure the loading of a project hosted via Monticello and git, and then we detail optional parameters.

#### Project managed with Git

To load a project from git you need to execute an expression like this:

```Smalltalk
Metacello new
	repository: {repository};
	baseline: {baselineName};
	load
```

This command has two parameters:

- `repository` defining the location of the git project, the version of the project to load and the subdirectory in which the project is stored.
- `baselineName` is the name of the baseline to load. For example to load the `MaterialDesignLite` project, the baseline name is `MaterialDesignLite` to load the project with `BaselineOfMaterialDesignLite`.

The repository parameter is a string that can take different forms depending on if the project is local or hosted remotely.

##### Project from github/gitlab/bitbucket

The repository parameter to load a project from github/gitlab/bitbucket takes this form:

`{prefix}://{optionalHostname}:{owner}/{projectName}:{version}/{subFolder}`

This snippet should be configured with:

- `{prefix}`: This is host-specific:
  - `github` for github
  - `bitbucket` for bitbucket
  - `gitlab` for gitlab
- `{optionalHostname}`: Optional server host, for private git servers
- `{owner}`: Name of the user or organization hosting the project
- `{projectName}`: Name of the project
- `{version}`: This parameter is optional, and it defaults to master. It can be the name of a branch, a tag like `'v1.2.0'` or `'v1.x.x'`, or a the SHA of a commit
- `{subfolder}`: This parameter is optional in case the code is at the root of the project. It should point to the sub-folder containing the code.

Example: loading from Github.

```Smalltalk
Metacello new
	repository: 'github://DuneSt/MaterialDesignLite:v1.x.x/src';
	baseline: 'MaterialDesignLite';
	load
```

Example: loading from a private Gitlab host.

```Smalltalk
Metacello new 
	baseline: 'Ghost';
	repository: 'gitlab://gitlab.inria.fr:RMOD/Ghost';
	load
```

Metacello also comes with some syntactic sugar to define the repository to github or bitbucket:

- *Github*: `Metacello>>githubUser:project:commitish:path:`
- *Bitbucket*: `Metacello>>bitbucketUser:project:commitish:path:`

Example:

```Smalltalk
Metacello new
	githubUser: 'DuneSt' project: 'MaterialDesignLite' commitish: 'master' path: 'src';
	baseline: 'MaterialDesignLite';
	load
```

##### Project from local repository

To load a project from a local repository you can use this form to declare the repository:

`'{prefix}://{full/path/to/repository}/{subFolder}'`

This snippet should be configured with:

- `{prefix}`: This is specific to the file format:
  - `filetree` for a Filetree project
  - `tonel` for a Tonel project
- `{full/path/to/repository}`: is the path to the project on the file system
- `{subfolder}`: This parameter is optional in case the code is at the root of the project. It should point to the subfolder containing the code.

Example:

```Smalltalk
Metacello new
	repository: 'tonel://C:\Users\Cyril\GitRepositories\Native-Browser\src';
	baseline: 'NativeBrowser';
	load
```

#### Project managed with Smalltalkhub

To load a project from Smalltalkhub you need to execute an expression like this:

```Smalltalk
Metacello new
	repository: 'http://smalltalkhub.com/mc/{owner}/{repositoryName}/main';
	configuration: {configurationName};
	version: {version};
	load
```

This command has two parameters:

- `owner`: Name of the team or user hosting the project
- `repositoryName`: Name of the repository on SmalltalkHub
- `configurationName` is the name of the configuration to load. For example to load the `MaterialDesignLite` project, the baseline name is `MaterialDesignLite` to load the project with `BaselineOfMaterialDesignLite`.
- `{version}`: Name of the version you wish to reference. It can be something like `'development'`, `'stable'`, `'release1'`, `'1.2.6'`, `'1.0-baseline'`, etc.

Example:

```Smalltalk
Metacello new
	repository: 'http://smalltalkhub.com/mc/Seaside/Seaside31/main';
	configuration: 'Seaside3';
	version: #stable;
	load
```

You can also use `Metacello>>smalltalkhubUser:project:`:

```Smalltalk
Metacello new
	smalltalkhubUser: 'Seaside' project: 'Seaside31';
	configuration: 'Seaside3';
	version: #stable;
	load
```

#### Project without repository

It is possible to use Metacello without specifying any repository. This can be useful for defining all project dependencies in a baseline and then loading them with Metacello.

```Smalltalk
Metacello new
	baseline: #TinyBlog;
	load
```

#### Loading groups

Sometimes we want to load only specific groups of a project. This can be done be replacing the `load` selector by `load:`.

The `load:` selector takes a string or a collection of strings as parameter. Each string represents a group name from the baseline.

Examples:

```Smalltalk
Metacello new
	githubUser: 'DuneSt' project: 'MaterialDesignLite' commitish: 'master' path: 'src';
	baseline: 'MaterialDesignLite';
	load: 'Extensions'
```

```Smalltalk
Metacello new
	githubUser: 'DuneSt' project: 'MaterialDesignLite' commitish: 'master' path: 'src';
	baseline: 'MaterialDesignLite';
	load: #('Extensions' 'Widgets')
```

#### Conflict, Upgrade and Downgrade resolution

Sometimes there can be conflicts, updates or downgrades while loading a project.

For example, imagine in an image the project `ProjA` at version v1.0.0. We want to load our project `ProjB` that depends on `ProjA` version v2.0.0., `ProjC` version v1.0.0, and `ProjD` that loads also `ProjC` version v2.0.0.

If we load `ProjB` in those conditions, we will have two problems:

- The update of `ProjA` from v1.0.0 to v2.0.0
- A conflict between `ProjC` v1.0.0 and v2.0.0

To manage conflicts we can use the options `onConflict:`, `onUpgrade:` and `onDowngrade:`.

Example:

```Smalltalk
Metacello new
	githubUser: 'DuneSt' project: 'MaterialDesignLite' commitish: 'master' path: 'src';
	baseline: 'MaterialDesignLite';
	onConflict: [ :ex | ex useIncoming ];
	onUpgrade: [ :ex | ex useIncoming ];
	onDowngrade: [ :ex | ex useLoaded ];
	load
```

A last conflicting situation happens if Pharo includes a project in the default distribution and you want to load a new version. To manage this case you have the `ignoreImage` option.

```Smalltalk
Metacello new
	githubUser: 'DuneSt' project: 'MaterialDesignLite' commitish: 'master' path: 'src';
	baseline: 'MaterialDesignLite';
	onConflict: [ :ex | ex useIncoming ];
	onUpgrade: [ :ex | ex useIncoming ];
	onDowngrade: [ :ex | ex useLoaded ];
	ignoreImage;
	load
```

Here is a last example of conflict management. The Pharo community was previously on a version control system called Monticello. Most of the community has now migrated to GitHub. Some of the projects exist on Smalltalkhub (managed with Monticello) and on GitHub. It's not unusual to have conflict between the two.

Here is a little script that loads the version managed with git when the project name is the same:

```Smalltalk
Metacello new
	githubUser: 'DuneSt' project: 'MaterialDesignLite' commitish: 'master' path: 'src';
	baseline: 'MaterialDesignLite';
	onConflict: [ :ex :a :b | a projectName = b projectName ifTrue: [ a projectSpec isBaselineOfProjectSpec ifTrue: [ ex useLoaded ] ifFalse: [ ex useIncoming ] ] ifFalse: [ ex resume ] ];
	load
```

#### Manage warnings

In some cases a project has problems during the loading, for example, if a package loaded is missing a dependency. When this happen, Metacello will raise a warning. Most of the time the projects can still work, at least partially. If you do not want Metacello to open a warning, you can log them instead. To enable this option you can use the `onWarningLog` or `onWarning:` options.

Examples:

```Smalltalk
Metacello new
	githubUser: 'DuneSt' project: 'MaterialDesignLite' commitish: 'master' path: 'src';
	baseline: 'MaterialDesignLite';
	onWarning: [ :ex | Transcript crShow: ex ];
	load
```

```Smalltalk
Metacello new
	githubUser: 'DuneSt' project: 'MaterialDesignLite' commitish: 'master' path: 'src';
	baseline: 'MaterialDesignLite';
	onWarningLog;
	load
```

### From Iceberg

In Pharo 7 a new tool to manage git repositories was introduced: *Iceberg*. This tool allows the developer to load a project via a user interface.

The first step is to add your git project to Iceberg. Then right-click on the project name to access a `Metacello` sub-menu to load the project.

![Interface of Iceberg to load a project](img/Baselines_Image_LoadBaselineViaIceberg.png?raw=true "Interface of Iceberg to load a project")

## Other features

This section covers other features of baselines and Metacello.

### Metacello lock feature

In the normal course of loading and upgrading, you want the correct version of dependent projects loaded. However, there are some special cases where automatic upgrading isn't desirable:

- You may be actively developing a particular version of a project and you don't want the project upgraded (or downgraded) out from under you.
- You may be working with a git checkout of a project and you want to continue using the git checkout.
- You may not want to have particular projects upgraded automatically.

The `lock` command forces a particular version.

Example:

```Smalltalk
Metacello new
	githubUser: 'DuneSt' project: 'MaterialDesignLite' commitish: 'v1.1.0' path: 'src';
	baseline: 'MaterialDesignLite';
	lock
```

This example will lock the project MaterialDesignLite to version v1.1.0.

You can check the list of locked projects via those snippets:

```Smalltalk
Metacello registry locked. "Return the list of locked projects from the Metacello registry"

Metacello image locked. "Return the list of locked projects loaded in the image."
```

If you wish to unlock a project, you can use the `unlock` message.

Example:

```Smalltalk
Metacello new
	githubUser: 'DuneSt' project: 'MaterialDesignLite' commitish: 'v1.1.0' path: 'src';
	baseline: 'MaterialDesignLite';
	unlock
```

During the loading of a project you can also do some specific actions when you encounter a lock. For this you can use the `onLock:` message.

```Smalltalk
Metacello new
	githubUser: 'DuneSt' project: 'MaterialDesignLite' commitish: 'v1.1.0' path: 'src';
	baseline: 'MaterialDesignLite';
	onLock: [ :ex :loaded :incoming | loaded baseName = 'myProject' ifTrue: [ ex break ] ifFalse: [ ex honor ] ];
	load
```

### Metacello get feature

Metacello includes a command to load the baseline of a project into the image. This is useful in two cases:

- You want to load only the baseline of the project
- You already have an obsolete baseline in your image and you want to update it before loading the project

To do that you can use the `get` command.

Example:

```Smalltalk
Metacello new
	githubUser: 'DuneSt' project: 'MaterialDesignLite' commitish: 'v1.x.x' path: 'src';
	baseline: 'MaterialDesignLite';
	get
```

### Metacello fetch feature

The fetch command downloads all of the packages without loading them. This includes the packages of the project but also their dependencies.

Example:

```Smalltalk
Metacello new
	githubUser: 'DuneSt' project: 'MaterialDesignLite' commitish: 'v1.x.x' path: 'src';
	baseline: 'MaterialDesignLite';
	fetch
```

You can also specify a group:

```Smalltalk
Metacello new
	githubUser: 'DuneSt' project: 'MaterialDesignLite' commitish: 'v1.x.x' path: 'src';
	baseline: 'MaterialDesignLite';
	fetch: #('Extensions' 'Widgets')
```

The fetch command duplicates what the load command would do, which means if a package is already loaded in the image, it will not be fetched. To fetch packages regardless of what is loaded in the image, use the `ignoreImage` option:

```Smalltalk
Metacello new
	githubUser: 'DuneSt' project: 'MaterialDesignLite' commitish: 'v1.x.x' path: 'src';
	baseline: 'MaterialDesignLite';
	ignoreImage;
	fetch
```

### Metacello record feature

The `record` command performs the same function as the `fetch` command, without actually downloading any files. As a consequence, it can give you a quick report of what packages will be loaded into your image. The recording will be produced in the `Transcript` (cmd + o + t).

Example:

```Smalltalk
Metacello new
	githubUser: 'DuneSt' project: 'MaterialDesignLite' commitish: 'v1.x.x' path: 'src';
	baseline: 'MaterialDesignLite';
	record
```

### Metacello listing feature

The `list` command lists projects in the image or Metacello registry:

```Smalltalk
Metacello image
	baseline: [ :spec | true "spec name beginsWith: 'Seaside'" ];
	list
  
Metacello registry
	baseline: [ :spec | true "spec name beginsWith: 'Seaside'" ];
	list
```

This command needs to be inspected to see the list.

## See also

- Deep into pharo: [Chapter 9, Managing Projects with Metacello](http://files.pharo.org/books-pdfs/deep-into-pharo/2013-DeepIntoPharo-EN.pdf)
- [Metacello documentation](https://github.com/Metacello/metacello/tree/master/docs)
