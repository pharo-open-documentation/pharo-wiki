# Baselines

- [Baselines](#baselines)
  * [How to define Baselines](#how-to-define-baselines)
    + [Define packages forming your project](#define-packages-forming-your-project)
    + [Define external dependencies](#define-external-dependencies)
      - [To other remote git projects](#to-other-remote-git-projects)
        * [Depends on the same project with different groups](#depends-on-the-same-project-with-different-groups)
      - [To a local git project](#to-a-local-git-project)
      - [To smalltalkhub projects](#to-smalltalkhub-projects)
    + [Groups](#groups)
    + [Pre/post load actions](#pre-post-load-actions)
    + [Loads different packages depending on the Pharo version](#loads-different-packages-depending-on-the-pharo-version)
    + [Define custom attributes](#define-custom-attributes)
    + [Different loading kinds](#different-loading-kinds)
  * [How to load a git project using its baseline](#how-to-load-a-git-project-using-its-baseline)
    + [From Iceberg](#from-iceberg)
    + [From the playground](#from-the-playground)

## How to define Baselines
To be done, but a page like this would be really nice.

Baseline are a way to manage projects in Pharo. A baseline defines the packages of the project, their dependencies and independant sub-groups that can be loaded.

The first step to create a baseline is to create a new subclass of `BaselineOf`:

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

### Define packages forming your project

To define packages of the project, just send the message #package: to the spec with the name of the package as argument.

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

To manage dependencies inside a project, see section [Define external dependencies](#define-external-dependencies).

To manage dependencies between the packages of your project, you can use the message #package:with: to give more informations to the spec.

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

*Note: For lisibility I recommand to extract the definition of dependencies to external methods.*

#### To other remote git projects

To depend on a git project hosted project you can use the method #baseline:with:.

```Smalltalk
	spec
		baseline: '{BaselineName}'
		with: [ spec repository: '{prefix}://{owner}/{projectName}:{version}/{subfolder}' ]
```

This snippet should be configured with:

* The BaselineName: The name of the baseline to load (For example it would be 'MaterialDesignLite' to load the BaselineOfMaterialDesignLite)
* The prefix: This one will be specific to the host. It can be:
	* `github` for github
	* `bitbucket` for bitbucket
	* `gitlab` for gitlab
* The owner: Name of the user or organisation hosting the project
* The projectName: Name of the project
* The version: This parameter is optional (it will take master by default). It can be: the name of a branch, a tag like v1.2.0 or v1.x.x, or a the SHA of a commit
* The subfolder: This parameter is optional in case the code is at the root of the project. It should point to the subfolder containing the code.

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

Some time you might want to depend on a project, but two packages will depend on different groups of this external project. 

For that case you can use the message #project:copyFrom:with: to create a new dependency spec.

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

===================================================
materialDesignLite: spec

	spec
		baseline: 'MaterialDesignLite' with: [ spec repository: 'github://DuneSt/MaterialDesignLite:v1.x.x/src' ];
		project: 'MaterialDesignLiteExtensions' copyFrom: 'MaterialDesignLite' with: [ spec loads: #('Extensions') ]

```

#### To a local git project

Sometime we do not have access to internet and we want to define dependencies to local git repositories. 

Those works like in the previous section but with this repository format:

```Smalltalk
	spec
		baseline: 'MaterialDesignLite'
		with: [ spec repository: 'gitlocal://full/path/to/repository' ]
```

#### To smalltalkhub projects
**TODO**

### Groups
**TODO**
### Pre/post load actions
**TODO**
### Loads different packages depending on the Pharo version
**TODO**
### Define custom attributes
**TODO**
### Different loading kinds
**TODO**
## How to load a git project using its baseline
**TODO**
### From Iceberg
**TODO**
### From the playground
**TODO**