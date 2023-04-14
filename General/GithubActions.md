# Setting up your continuous integration via GitHub Actions

Previously, the Pharo community was heavily relying on Travis CI for the continuous integration of projects hosted on GitHub.
But Travis becoming pay-to-use service, the community created some tooling to manage the CI via GitHub Actions. 

This guide will get you through the process to set up your own integration.

> Note: In order to do anything for the CI of your project, you will need a `Baseline` to manage its dependencies. If it is not the case yet, you can check the [guide on baselines](Baselines.md).

- [Setting up your continuous integration via Github Actions](#setting-up-your-continuous-integration-via-github-actions)
  * [Simple case: Run tests on master branch](#simple-case-run-tests-on-the-master-branch)
  * [SmalltalkCI options](#smalltalkci-options)
    + [Load spec options](#load-spec-options)
    + [Testing options](#testing-options)
    + [Code Coverage](#code-coverage)
    + [Using custom scripts](#using-custom-scripts)
  * [Testing multiple versions of Pharo at once](#testing-multiple-versions-of-pharo-at-once)
  * [Testing on multiple OS](#testing-on-multiple-os)
  * [Manage the workflow target (branchs, PR, ...)](#manage-the-workflow-target-branchs-pr-)
    + [Target branches](#target-branches)
    + [Target pull requests](#target-pull-requests)
    + [Target releases](#target-releases)
    + [Target scheduled workflow](#target-scheduled-workflow)
    + [Other targets](#other-targets)
  * [Managing multiple workflows and SmalltalkCI configurations](#managing-multiple-workflows-and-smalltalkci-configurations)
  * [Continuous releases](#continuous-releases)
  * [Save releases artifacts](#save-releases-artifacts)
  * [Depending on resources of your repository with GitBridge](#depending-on-the-resources-of-your-repository-with-gitbridge)
  * [Add your build artifacts to PharoLauncher](#add-your-build-artifacts-to-pharolauncher)
  * [External ressources](#external-ressources)

## Simple case: Run tests on the master branch

Let's start simple! This section will explain how to set up a workflow to launch the tests of the project on every commit done on the master branch.

To load our Pharo project in the CI and execute the tests, we'll use [SmalltalkCI](https://github.com/hpi-swa/smalltalkCI).
This project needs a configuration file to fetch information on your project. This file should be at the root of your project under the name of `.smalltalk.ston`. 

Here is the most simple configuration:

```ston
SmalltalkCISpec {
  #loading : [
    SCIMetacelloLoadSpec {
      #baseline : 'MyProject',
      #directory : 'src'
    }
  ]
}
```

This configuration tells smalltalkCI two things:
- The baseline to use to load the project is BaselineOf`MyProject`
- The sources of the project are in /src

Now that smalltalkCI configuration file is created, we just need to define your GitHub workflow file.
This file should be located in `.github/workflows/` folder. Let's call ours `testing.yml`.

```yml
name: CI

on:
  push:
    branches:
      - 'master'

jobs:
  build:
    runs-on: ubuntu-latest
    env:
      GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
    steps:
      - uses: actions/checkout@v3
      - uses: hpi-swa/setup-smalltalkCI@v1
        with:
          smalltalk-image: Pharo64-10
      - run: smalltalkci -s ${{ matrix.smalltalk }}
        shell: bash
        timeout-minutes: 15
```

Let's see a little about what is happening here. 

The #name parameter allows one to give a name to the workflow. This is useful because you might have more than one workflow for your project. 
For example:

![Example of different workflows](GitHubActions_workflows.png)

In this image, we can see a CI that has 3 different workflows:
- CI
- continuous
- Release

Then we have:  

```yml
on:
  push:
    branches:
      - 'master'
```

This is used to define when the workflow should enter into action. In our case, we want it when we `push` on `master` branch.

Then we have:

```yml
    runs-on: ubuntu-latest
```

With this, the workflow will run in the latest version of Ubuntu.


```yml
    env:
      GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
```

This step adds en environment variable needed by smalltalkCI.

Last but not least, we have the actions to execute:

```yml
    steps:
      - uses: actions/checkout@v3
```

Using the "checkout" action to checkout the project on the CI worker.  

```yml
      - uses: hpi-swa/setup-smalltalkCI@v1
        with:
          smalltalk-image: Pharo64-10
```

Using the "setup-SmalltalkCI" action to prepare the setup.

```yml
      - run: smalltalkci -s ${{ matrix.smalltalk }}
        shell: bash
        timeout-minutes: 15
```

Loads the project and executes the tests of the project with a 15min timeout.  

This timeout can be increased in case your project tests are longer.  

Once you commit these two files, your project will have a working CI !  
Each time you commit to master, a new build should happen.  
You will be able to see the resulting build in the `Actions` tab in your project.

## SmalltalkCI options

We have seen in the previous section that [SmalltalkCI](https://github.com/hpi-swa/smalltalkCI) relies on a configuration file. 
We did a simple one to start, but there are more options available. We will cover them in this section.  
The full and up-to-date list is available in the [SmalltalkCI's README file](https://github.com/hpi-swa/smalltalkCI/blob/master/README.md).

### Load spec options

In the previous example, we declared a loading spec with a baseline and a code directory. But it is also possible to add more options:

```ston
SCIMetacelloLoadSpec {
  #baseline : 'MyProject',                            // Define MC Baseline
  #directory : 'src',                                 // Path to packages
  #failOn : [ #OCUndeclaredVariableWarning ],         // Fail build on provided list of Warnings
  #ignoreImage : true,                                // If true, Metacello will force a load of a package, overriding a previously existing one
  #load : [ 'default' ],                              // Define some specific groups to load
  #onConflict : #useIncoming,                         // When there is a conflict between loaded sources and incoming sources (can be #useIncoming|#useLoaded)
  #onUpgrade : #useIncoming,                          // When loaded sources are an older version than incoming sources (can be #useIncoming|#useLoaded)
  #onWarningLog : true,                               // Log Warning messages to Transcript
  #platforms : [ #squeak, #pharo, #gemstone ],        // Define compatible platforms
  #usernameEnvVar : 'GITHUB_USER',                    // Environment variable containing the username used for authentication
  #passwordEnvVar : 'GITHUB_TOKEN',                   // Environment variable containing the password used for authentication
  #registerInIceberg : true                           // Pharo Only | Register the tested repository in Iceberg (false by default)
}
```
### Testing options
It is also possible to customize the testing of the project:

```ston
SmalltalkCISpec {
  ...
  #testing : {
    // Include specific TestCases
    #include : {
      #classes : [ #AnotherProjectTestCase ],
      #categories : [ 'AnotherProject-Tests' ],
      #packages : [ 'AnotherProject.*' ],
      #projects : [ 'BaselineOfMyProject' ]
    },

    // Exclude specific TestCases from testing
    #exclude : {
      #classes : [ #AnotherProjectTestCase ],
      #categories : [ 'AnotherProject-Tests' ],
      #packages : [ 'AnotherProject.*' ],
      #projects : [ 'ConfigurationOfMyOtherProject' ]
    },

    #allTestCases : true, // Run all TestCases in image

    // Define TestCases explicitly
    #classes : [ #MyProjectTestCase ],
    #categories : [ 'MyProject-*' ],
    #packages : [ 'MyProject.*' ],
    #projects : [ 'BaselineOfMyProject' ],

    // Other options
    #defaultTimeout : 30, // In seconds (Squeak, and Pharo 6 or later)
    #failOnSCIDeprecationWarnings : false, // Fail if a deprecated smalltalkCI API is used
    #failOnZeroTests : false, // Pass builds that did not run any tests
    #hidePassingTests : true, // Hide passing tests when printing to stdout
    #serializeError: false // (default: true) Disable serialization of failing test case (e.g. with Fuel in Pharo)
  }
}
```

### Code Coverage

It is possible to see the coverage of our project via SmalltalkCI with [Coveralls](https://coveralls.io/).

To enable this, you need to add a "coverage" section in your configuration:

```ston
SmalltalkCISpec {
  ...
  #testing : {
    ...
    #coverage : {
      #packages : [ 'SomePackage', 'SomePack*' ],
      #classes : [ #ClassToCover ],
      #categories : [ 'SomeClassCategory', 'SomeClassCat*' ],
      #format : #coveralls
    }
  }
}
```

For example our project could look like:

```ston
SmalltalkCISpec {
  #loading : [
    SCIMetacelloLoadSpec {
      #baseline : 'Myproject',
      #directory : 'src'
    }
  ],
  #testing : {
    #coverage : {
      #packages : [ 'MyProject-*' ]
    }
  }
}
```

This configuration will execute all the tests loaded by the `BaselineOfMyProject` and build the coverage of all packages starting by `MyProject-` for [Coveralls](https://coveralls.io/).
If you enabled your repository in coveralls ([https://coveralls.io/repos/new](https://coveralls.io/repos/new)), the results would be uploaded automatically.

![Coveralls screenshot](GithubActions_coveralls.png)

For more information, check: [SmalltalkCI guide on coverage](https://github.com/hpi-swa/smalltalkCI/blob/master/docs/COVERAGE.md).

### Using custom scripts

SmalltalkCI offers some hooks to be able to run some Pharo scripts at different moments of the project loading and testing.
Four hooks are available:
- `preLoading` : Executed before loading the project
- `postLoading` : Executed after loading the project
- `preTesting` : Executed before testing the project
- `postTesting` : Executed after testing the project

Those parameters can take 3 different parameters. 

The first one is a single script:

```ston
SmalltalkCISpec {
  #preLoading : 'scripts/preLoading.st',
  #loading : [
    SCIMetacelloLoadSpec {
      #baseline : 'Myproject',
      #directory : 'src'
    }
  ]
}
```

The second one is a collection of scripts:

```ston
SmalltalkCISpec {
  #loading : [
    SCIMetacelloLoadSpec {
      #baseline : 'Myproject',
      #directory : 'src'
    }
  ],
  #postLoading : [
    'scripts/postLoading1.st',
    'scripts/postLoading2.st'
  ]
}
```

And the third one is an instance of SCICustomScript that allows running script only on certain platforms:

```ston
SmalltalkCISpec {
  #preLoading : 'scripts/preLoading.st',
  #loading : [
    SCIMetacelloLoadSpec {
      #baseline : 'Myproject',
      #directory : 'src'
    }
  ],
  #postLoading : [
    SCICustomScript {
      #path : 'scripts/postLoadingSqueak.st',
      #platforms : [ #squeak ]
    },
    SCICustomScript {
      #path : 'scripts/postLoadingPharo.st',
      #platforms : [ #pharo ]
    }
  ]
}
```

Here is a full example:

```ston
SmalltalkCISpec {
  #preLoading : 'scripts/preLoading.st',
  #loading : [
    SCIMetacelloLoadSpec {
      #baseline : 'Myproject',
      #directory : 'src'
    }
  ],
  #postLoading : [
    'scripts/postLoading1.st',
    'scripts/postLoading2.st'
  ],
  #preTesting : SCICustomScript {
    #path : 'scripts/preTesting.st',
    #platforms : [ #squeak, #pharo, #gemstone ]
  },
  #testing : ...,
  #postTesting : [
    SCICustomScript {
      #path : 'scripts/postTestingSqueak.st',
      #platforms : [ #squeak ]
    },
    SCICustomScript {
      #path : 'scripts/postTestingPharo.st',
      #platforms : [ #pharo ]
    }
  ]
}
```

For more informations on SmalltalkCI in general check: [SmalltalkCI documentation](https://github.com/hpi-swa/smalltalkCI).

## Testing multiple versions of Pharo at once

It is rare to have a project working only on one version of Pharo. Thus, it is often useful to have our workflows run on multiple versions. 

This can be achieved this way:

```yml
name: CI

on:
  push:
    branches:
      - 'master'

jobs:
  build:
    runs-on: ubuntu-latest
    env:
      GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
    strategy:
      matrix:
        smalltalk: [ Pharo64-9.0, Pharo64-10, Pharo64-11 ]
    name: ${{ matrix.smalltalk }}
    steps:
      - uses: actions/checkout@v3
      - uses: hpi-swa/setup-smalltalkCI@v1
        with:
          smalltalk-image: ${{ matrix.smalltalk }}
      - run: smalltalkci -s ${{ matrix.smalltalk }}
        shell: bash
        timeout-minutes: 15
```

Here we declared a matrix with multiple versions of Pharo as axes:

```yml
    strategy:
      matrix:
        smalltalk: [ Pharo64-9.0, Pharo64-10, Pharo64-11 ]
```

Each axe will use the name of the Pharo version as name:

```yml
    name: ${{ matrix.smalltalk }}
```

But this could also be another name, such as:

```yml
    name: MyProject-${{ matrix.smalltalk }}
```

Then the builds would be: `MyProject-Pharo64-9.0`, `MyProject-Pharo64-10` and `MyProject-Pharo64-11`.

The names will then be used to display the different builds on Github:

![Screenshot of matrix](GithubActions_matrix.png)

The list of possible Pharo versions are:

| 64bits           | 32bits           |
| ---------------- | ---------------- |
| `Pharo64-alpha`  | `Pharo32-alpha`  |
| `Pharo64-stable` | `Pharo32-stable` |
| `Pharo64-11`     | `Pharo32-11`     |
| `Pharo64-10`     | `Pharo32-10`     |
| `Pharo64-9.0`    | `Pharo32-9.0`    |
| `Pharo64-8.0`    | `Pharo32-8.0`    |
| `Pharo64-7.0`    | `Pharo32-7.0`    |
| `Pharo64-6.1`    | `Pharo32-6.1`    |
| `Pharo64-6.0`    | `Pharo32-6.0`    |
|                  | `Pharo32-5.0`    |
|                  | `Pharo32-4.0`    |
|                  | `Pharo32-3.0`    |

> Note: This list is from February 2023. More versions will be added in the future

## Testing on multiple OS

Usually, our Pharo projects are not influenced by the OS on which it runs. But in some cases, it can be important to test on multiple OS.

This can be achieved this way: 

```yml
name: CI

on:
  push:
    branches:
      - 'master'

jobs:
  build:
    strategy:
      matrix:
        os: [ macos-latest, windows-latest, ubuntu-latest]
        smalltalk: [ Pharo64-9.0, Pharo64-10]
    runs-on: ${{ matrix.os }}
    name: ${{ matrix.smalltalk }} on ${{ matrix.os }}
    env:
      GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
    steps:
      - uses: actions/checkout@v3
      - uses: hpi-swa/setup-smalltalkCI@v1
        with:
          smalltalk-image: ${{ matrix.smalltalk }}
      - run: smalltalkci -s ${{ matrix.smalltalk }}
        shell: bash
        timeout-minutes: 15
```

As we can see here:

```yml
        os: [ macos-latest, windows-latest, ubuntu-latest]
    runs-on: ${{ matrix.os }}
```

We declare that our project will be run on the latest macos, windows and ubuntu to covers the 3 major OS. 

To distinguish the builds, we use the Pharo version and the OS name to name our builds:

```yml
    name: ${{ matrix.smalltalk }} on ${{ matrix.os }}
```

![Screenshot of ci matrix](GithubActions_os.png)

You can find more information on the available OS here: [https://docs.github.com/en/actions/using-github-hosted-runners/about-github-hosted-runners](https://docs.github.com/en/actions/using-github-hosted-runners/about-github-hosted-runners)

## Manage the workflow target (branchs, PR, ...)

In our simple example at the beginning, we were launching the CI on the master branch, but you will often need to execute the CI on multiple branches.

### Target branches

It is possible to target multiple branches at once. For example:

```yml
on:
  push:
    branches:
      - 'master'
      - 'development'
      - 'feature/**
```

This will launch your workflow for every commit on `master`, `development` or any branch starting with `feature/`. 

You can also target all branches in two ways. The first is by not specifying any:  

```yml
on: [ push ]
```

The second is to use `'**'`:

```yml
on:
  push:
    branches:
      - '**'
```

This second way of declaring "all branches" is useful when you want to execute your workflow on all branches except some of them.
In that case, you can use:

```yml
on:
  push:
    branches:
      - '**'
      - '!doc/**'
```

This template will launch the CI for every commit on any branch except the ones starting with `doc/`.

### Target pull requests

It is also possible to target others kinds of events.
Particularly pull requests made to your project:

```yml
on: [ push, pull_request ]
```

And be even more precise on the kinds of PR that should execute a workflow:  
```yml
on:
  pull_request:
    types: [assigned, opened, synchronize, reopened]
```


Note that this can be done by addition of other targets:

```yml
on:
  push:
    branches:
      - '**'
      - '!master'
  pull_request:
    types: [assigned, opened, synchronize, reopened]
```

Here the workflow runs on every pull request and every commit of any branch except `master`.

### Target releases

It is also sometimes useful to have a workflow targeting releases. We'll exploit that later in this documentation.
In that case you can use:

```yml
on:
  release:
    types: [created, edited]
```

### Target scheduled workflow

In some cases, it might be useful to target the workflow at specific times. 
For example, it happens that some projects are "meta project". Their goal is to load a bunch of other projects. In that case, it's rare that a commit is made direcly in them.
But we want to run the CI on a regular basis to be sure our project still works. In that case, we can use a cron to schedule the workflow:

```yml
on:
  push:
    branches:
      - 'master'
  schedule:
    - cron:  '0 0 * * *'
```

Some utils will help you to configure the parameter of this target such as [Crontab.guru](https://crontab.guru/).

### Other targets

GitHub actions have way more options than the options presented here. The most useful ones were presented here, but you can find more information in [Github documentation](https://docs.github.com/en/actions/using-workflows/workflow-syntax-for-github-actions#on)

## Managing multiple workflows and SmalltalkCI configurations

Until now, we managed only one workflow file, but it is possible to manage multiple of them.

To demonstrate this, let's imagine that our project has two groups in its configuration.
The first group loads the core of the project and is the default group of our baseline. 
A second group loads the full projects with additional features. 

We would now like to have two workflows:
- A first workflow launched on all branches and PR to test the core in Pharo 9, 10 and 11
- A second workflow launched on master branch and PR to test the full project in Pharo 11

The first step is to get two smalltalkCI configurations.

A first one will be the default `.smalltalk.ston`.

```ston
SmalltalkCISpec {
  #loading : [
    SCIMetacelloLoadSpec {
      #baseline : 'MyProject',
      #directory : 'src'
    }
  ]
}
```

And a second one loads the group `all` and is named `.smalltalkFull.ston`.

```ston
SmalltalkCISpec {
  #loading : [
    SCIMetacelloLoadSpec {
      #baseline : 'MyProject',
      #directory : 'src',
      #load : [ 'all' ]
    }
  ]
}
```

Now that we have our two configurations, we can create our two workflow files. 

The first one is close to what we have seen until here in this documentation:

```yml
name: CI Core

on: [ push, pull_request ]

jobs:
  build:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        smalltalk: [ Pharo64-9.0, Pharo64-10, Pharo64-11 ]
    name: ${{ matrix.smalltalk }}
    env:
      GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
    steps:
      - uses: actions/checkout@v3
      - uses: hpi-swa/setup-smalltalkCI@v1
        with:
          smalltalk-image: ${{ matrix.smalltalk }}
      - run: smalltalkci -s ${{ matrix.smalltalk }}
        shell: bash
        timeout-minutes: 15
```

In the second, we change the targets, and we give one more parameter to the smalltalkCI launch command to specify the path to our specific Smalltalk configuration.

```yml
name: CI Full

on:
  push:
    branches:
      - 'master'
  pull_request:
    types: [assigned, opened, synchronize, reopened]

jobs:
  build:
    runs-on: ubuntu-latest
    name: ${{ matrix.smalltalk }}
    env:
      GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
    steps:
      - uses: actions/checkout@v3
      - uses: hpi-swa/setup-smalltalkCI@v1
        with:
          smalltalk-image: Pharo64-11
      - run: smalltalkci -s ${{ matrix.smalltalk }} .smalltalkFull.ston
        shell: bash
        timeout-minutes: 15
```

Each file uses a different configuration file.
The first workflow file is named `CI` uses the default file `smalltalk.ston` implicitly.
The second workflow file is named `CI full` uses `smalltalkFull.ston` explicitly.


```yml
      - run: smalltalkci -s ${{ matrix.smalltalk }} .smalltalkFull.ston
        shell: bash
        timeout-minutes: 15
```

> Note: If we wanted to run on the same targets with two smalltalkCI configurations, we could also have used another matrix axis and avoid needing of different workflows.

Example:

```yml
name: CI

on: [ push, pull_request ]

jobs:
  build:
    runs-on: ubuntu-latest
    env:
      GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
    strategy:
      matrix:
        smalltalk: [ Pharo64-9.0, Pharo64-10, Pharo64-11 ]
        config: [ .smalltalk.ston, .smalltalkFull.ston ]
    name: ${{ matrix.smalltalk }} - ${{ matrix.config }}
    steps:
      - uses: actions/checkout@v3
      - uses: hpi-swa/setup-smalltalkCI@v1
        with:
          smalltalk-image: ${{ matrix.smalltalk }}
      - run: smalltalkci -s ${{ matrix.smalltalk }} ${{ matrix.config }}
        shell: bash
        timeout-minutes: 15
```

Having multiple workflows can have other usages that we explore in the next sections.

## Continuous releases

Until now, we have used Github actions to test our project.  
But Github Actions has more features.  
For instance, we are able to realease our projects.  

In this section, we see how to save the result of the builds of our master branch in a github release.

To do that, we can first remove the master branch from the targets of our test workflow because we will handle the master branch in another workflow.

```yml
on:
  push:
    branches:
      - '**'
      - '!master'
  pull_request:
    types: [assigned, opened, synchronize, reopened]
```

Then we will create another workflow called `.github/workflows/continuous.yml`.

```yml
name: continuous

on:
  push:
    branches:
      - 'master'

jobs:
  build:
    runs-on: ubuntu-latest
    env:
      PROJECT_NAME: MyProject-${{ matrix.smalltalk }}
    strategy:
      matrix:
        smalltalk: [ Pharo64-10, Pharo64-11 ]
    name: ${{ matrix.smalltalk }}
    env:
      GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
    steps:
      - uses: actions/checkout@v3
      - uses: hpi-swa/setup-smalltalkCI@v1
        with:
          smalltalk-version: ${{ matrix.smalltalk }}
      - run: smalltalkci -s ${{ matrix.smalltalk }}
        shell: bash
        timeout-minutes: 15
        
      # Here we zip the result of the build to be able to keep the artefacts
      - name: package
        run: |
          mv /home/runner/.smalltalkCI/_builds/* .
          mv TravisCI.image $PROJECT_NAME.image
          mv TravisCI.changes $PROJECT_NAME.changes
          echo ${${{ matrix.smalltalk }}} | sed -e 's/.*\-//g ; s/\..*//g ; s/$/0/' > pharo.version
          zip -r $PROJECT_NAME.zip $PROJECT_NAME.image $PROJECT_NAME.changes *.sources pharo.version
          ls
        
      #Save the artefact of the build under "continuous" tag
      - name: Update release
        uses: johnwbyrd/update-release@v1.0.0
        with:
          release: 'continuous'
          token: ${{ secrets.GITHUB_TOKEN }}
          files: ${{ env.PROJECT_NAME }}.zip
```

This workflow starts to like what we have seen until now. It checkout our project, install smalltalkCI and runs it. 
But it also adds three steps to the process. 

The first step is to give a name to our project. We will use that name to name our build artifact:

```yml
    env:
      PROJECT_NAME: MyProject-${{ matrix.smalltalk }}
```

In the second step, we rename the image produced by SmalltalkCI. We generate a version file that is useful in tools such as the `PharoLauncher`. And we zip the files in the archive to save.

```yml
      # Here we zip the result of the build to be able to keep the artefacts
      - name: package
        run: |
          mv /home/runner/.smalltalkCI/_builds/* .
          mv TravisCI.image $PROJECT_NAME.image
          mv TravisCI.changes $PROJECT_NAME.changes
          echo ${${{ matrix.smalltalk }}} | sed -e 's/.*\-//g ; s/\..*//g ; s/$/0/' > pharo.version
          zip -r $PROJECT_NAME.zip $PROJECT_NAME.image $PROJECT_NAME.changes *.sources pharo.version
          ls
```

The last step is to publish the artifact in the continuous tag. For that we are using the action `johnwbyrd/update-release@v1.0.0`.

```yml
      #Save the artefact of the build under "continuous" tag
      - name: Update release
        uses: johnwbyrd/update-release@v1.0.0
        with:
          release: 'continuous'
          token: ${{ secrets.GITHUB_TOKEN }}
          files: ${{ env.PROJECT_NAME }}.zip
```

> Note: The name of the continuous release can be changed

Once this is done, each commit on master will result in updating the assets of the `continuous` release on GitHub to save the latest one.

![Screenshot of continuous release](GithubActions_continuous.png)

## Save releases artifacts

We have seen how to save the last artifact of a branch in a `continuous` release, but another case needs to be taken into account: the real releases.

It is useful when we release a project to save a Pharo image of this project with the version of the release.

This can be done with a new workflow file that will target releases:

```yml
name: Release

on:
  release:
    types: [created, edited]

jobs:
  build:
    runs-on: ubuntu-latest
    env:
      PROJECT_NAME: MyProject-${{ matrix.smalltalk }}
      GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
    strategy:
      matrix:
        smalltalk: [ Pharo64-10, Pharo64-11 ]
    name: ${{ matrix.smalltalk }}
    steps:
      - uses: actions/checkout@v3
      - uses: hpi-swa/setup-smalltalkCI@v1
        with:
          smalltalk-version: ${{ matrix.smalltalk }}
      - run: smalltalkci -s ${{ matrix.smalltalk }}
        shell: bash
        timeout-minutes: 15
        
      # Here we zip the result of the build to be able to keep the artefacts
      - name: package
        run: |
          mv /home/runner/.smalltalkCI/_builds/* .
          mv TravisCI.image $PROJECT_NAME.image
          mv TravisCI.changes $PROJECT_NAME.changes
          echo ${${{ matrix.smalltalk }}} | sed -e 's/.*\-//g ; s/\..*//g ; s/$/0/' > pharo.version
          zip -r $PROJECT_NAME.zip $PROJECT_NAME.image $PROJECT_NAME.changes *.sources pharo.version
          ls
        
      - name: Release
        uses: softprops/action-gh-release@v1
        with:
          files: ${{ env.PROJECT_NAME }}.zip
```

This file looks a lot like the one we wrote in the previous section.

A few changes are to be noted. The is the name of the workflow to identify it easily in the Actions tab of Github.
The second is the target to build only on releases.

```yml
on:
  release:
    types: [created, edited]
```

And the last change is the action used after creating our Pharo archive.

```yml
      - name: Release
        uses: softprops/action-gh-release@v1
        with:
          files: ${{ env.PROJECT_NAME }}.zip
```

This action will add the files matching the $files: property to the assets of the release.

![Screenshot of releases](GithubActions_releases.png)

## Depending on the resources of your repository with GitBridge

It happens in some projects that we need resources to do some tests. In the past, it was common to save those resources as a string or a byte array directly in a method of the project.
This makes can have multiple drawbacks like making the management of the project harder, dropping Pharo's performances in code management, no real versioning of those resources…

Now that we can store our projects on git, an alternative is possible: Save your resources in your git repository and use file references in Pharo to access them.

To help with that, the project [GitBridge](https://github.com/jecisc/GitBridge) was created. 
This project helps one to access resources from the git repository and information from git directly from the Pharo image.

This project can be added as a dependency of your project with this spec:

```Smalltalk
    spec
    	baseline: 'GitBridge'
    	with: [ spec repository: 'github://jecisc/GitBridge:v1.x.x/src' ]
```

In order to create a GitBridge to your project, you first need to subclass `GitBridge` and to store your bridge in a package of your project.

```Smalltalk
GitBridge subclass: #MyProjectBridge
	slots: {  }
	classVariables: {  }
	package: 'MyProject'
```

This new bridge needs a class initialization like this one:

```Smalltalk
MyProjectBridge class>>initialize
	SessionManager default registerSystemClassNamed: self name
```

This will allow the bridge to reset some cache at the image startup.

Now that your bridge is created, if it finds an Iceberg repository associated with its local clone containing the package in which the bridge is defined, you will be able to use the bridge to access some resources.

For example, you can get a file reference to the git folder like this:

```Smalltalk
MyProjectBridge root
```

And this allows you to access your test resources.

Once your project is using `GitBridge`, you just need to be sure of two things in order for the CI to work. 
The first is to have the option `#registerInIceberg` to true in your smalltalkCI configuration.

```ston
SmalltalkCISpec {
  #loading : [
    SCIMetacelloLoadSpec {
      #baseline : 'MyProject',
      #directory : 'src',
      #registerInIceberg : true
    }
  ]
}
```

And the second is to add a parameter to the checkout action of your workflow file in order to fetch the full history of git:

```yml
  steps:
    - uses: actions/checkout@v3
      with:
        fetch-depth: '0'
```

Once those steps are set up, your tests should be able to run and fetch resources from your git repository without trouble.
For more information, you can look at the [documentation of GitBridge](https://github.com/jecisc/GitBridge).

## Add your build artifacts to PharoLauncher

[Pharo launcher](https://pharo-project.github.io/pharo-launcher//) is a great tool to manage Pharo images, and here we are going to explain how to be able to get images of your project from it. 

> Note: In order to do this, you will need to have a continuous release and/or a release workflow setup as we explained earlier in this documentation.

Pharo launcher comes with a default set of sources to fetch Pharo images. It also allows one to add its own sources, and we will show here how to add your project as one of your own source.

It is doable thanks to the `mysources.list` of pharo-launcher file located in the `PhLUserTemplateSources sourcesFile` folder. If present, this file defines additional template sources beyond the official list of templates. At this time, there is no UI to add them.

To find the right folder:

* Open the Pharo Launcher
* Open a Playground (Ctrl + O, Ctrl + W)
* Execute `PhLUserTemplateSources sourcesFile`

You can now edit `mysources.list` in this folder to add the images of your project you wish to have in your Pharo launcher.

Here is how to add images from your continuous release:

```st
[
	PhLTemplateSource {
		#type : #URLGroup,
		#name : 'MyProject',
		#templates : [
			PhLTemplateSource {
				#type : #URL,
				#name : 'MyProject Pharo 10 - master',
				#url : 'https://github.com/MY_USERNAME/MY_PROJET_NAME/releases/download/continuous/MyProject-Pharo64-10.zip'
			},
			PhLTemplateSource {
				#type : #URL,
				#name : 'MyProject 11 - master',
				#url : 'https://github.com/MY_USERNAME/MY_PROJET_NAME/releases/download/continuous/MyProject-Pharo64-11.zip'
			}
		]
	}
]
```

And here is how to add images from a specific release, lets say `v1.4.3`:

```st
[
	PhLTemplateSource {
		#type : #URLGroup,
		#name : 'MyProject',
		#templates : [
			PhLTemplateSource {
				#type : #URL,
				#name : 'MyProject Pharo 10 - v1.4.3',
				#url : 'https://github.com/MY_USERNAME/MY_PROJET_NAME/releases/download/v1.4.3/MyProject-Pharo64-10.zip'
			},
			PhLTemplateSource {
				#type : #URL,
				#name : 'MyProject Pharo 11 - v1.4.3',
				#url : 'https://github.com/MY_USERNAME/MY_PROJET_NAME/releases/download/v1.4.3/MyProject-Pharo64-11.zip'
			}
		]
	}
]
```

You can then adapt those sources to what you need. Once it is done, you can click on the `New` button of the launcher and see a new source named `MyProject`.

## External ressources

Here are some resources on GitHub actions and Pharo:
- [https://badetitou.fr/misc/2020/11/30/Testing-pharo-with-github-actions/](https://badetitou.fr/misc/2020/11/30/Testing-pharo-with-github-actions/)
- [https://badetitou.fr/misc/2022/10/27/test-your-moose-code-using-ci/](https://badetitou.fr/misc/2022/10/27/test-your-moose-code-using-ci/)
- [https://modularmoose.org/2021/07/19/automatic-metamodel-documentation-generation.html](https://modularmoose.org/2021/07/19/automatic-metamodel-documentation-generation.html)

Do not hesitate to do a PR to add more resources ;)
