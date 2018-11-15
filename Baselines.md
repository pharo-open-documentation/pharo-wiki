# Baselines

- [Baselines](#baselines)
  * [How to define Baselines](#how-to-define-baselines)
    + [Define packages forming your project](#define-packages-forming-your-project)
    + [Define external dependencies](#define-external-dependencies)
      - [To other github projects](#to-other-github-projects)
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

**TODO**

#### To other github projects
**TODO**
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