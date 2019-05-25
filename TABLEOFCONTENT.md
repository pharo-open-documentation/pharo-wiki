# Documentation Full Index

- [Pharo wiki manifest](./MANIFEST.md#pharo-wiki-manifest)

- [Baselines](./General/Baselines.md#baselines)
  * [How to define Baselines](./General/Baselines.md#how-to-define-baselines)
  * [How to load a git project using its baseline](./General/Baselines.md#how-to-load-a-git-project-using-its-baseline)
  * [Other features](./General/Baselines.md#other-features)
  * [See also](./General/Baselines.md#see-also)

- [Setting up a new project](./General/SettingUpANewProject.md#setting-up-a-new-project)
  * [Step 0. Download and install Pharo Launcher](./General/SettingUpANewProject.md#step-0.-download-and-install-pharo-launcher)
  * [Step 1. Get a fresh Pharo image](./General/SettingUpANewProject.md#step-1.-get-a-fresh-pharo-image)
  * [Step 2. Create a package and a class](./General/SettingUpANewProject.md#step-2.-create-a-package-and-a-class)
  * [Step 3. Write some failing tests](./General/SettingUpANewProject.md#step-3.-write-some-failing-tests)
  * [Step 4. Add methods to make tests green](./General/SettingUpANewProject.md#step-4.-add-methods-to-make-tests-green)
  * [Step 5. Create repository on GitHub](./General/SettingUpANewProject.md#step-5.-create-repository-on-github)
  * [Step 6. Load the repository into your image](./General/SettingUpANewProject.md#step-6.-load-the-repository-into-your-image)
  * [Step 7. Add your package and do the first commit](./General/SettingUpANewProject.md#step-7.-add-your-package-and-do-the-first-commit)
  * [Step 8. Create a baseline and a Metacello script](./General/SettingUpANewProject.md#step-8.-create-a-baseline-and-a-metacello-script)
  * [Step 9. Set up the continuous integration (CI)](./General/SettingUpANewProject.md#step-9.-set-up-the-continuous-integration-(ci))
  * [Step 10. Add badges to your README.md](./General/SettingUpANewProject.md#step-10.-add-badges-to-your-readme.md)

- [Coding conventions](./General/CodingConventions.md#coding-conventions)
  * [Tests conventions](./General/CodingConventions.md#tests-conventions)
  * [Protocols conventions](./General/CodingConventions.md#protocols-conventions)

- [Must know for beginners](./General/MustKnowForBeginners.md#must-know-for-beginners)
  * [Navigate into the code](./General/MustKnowForBeginners.md#navigate-into-the-code)
  * [Interrupt the Pharo process](./General/MustKnowForBeginners.md#interrupt-the-pharo-process)
  * [Debugging facilities](./General/MustKnowForBeginners.md#debugging-facilities)

- [Source Code Export formats](./General/ExportFormats.md#source-code-export-formats)
  * [Comparisons](./General/ExportFormats.md#comparisons)
  * [Tonel ](./General/ExportFormats.md#tonel-)
  * [FileTree metadata less ](./General/ExportFormats.md#filetree-metadata-less-)
  * [FileTree metadata full](./General/ExportFormats.md#filetree-metadata-full)

- [Traits](./General/Traits.md#traits)
  * [Description](./General/Traits.md#description)
  * [Create and use a new Trait](./General/Traits.md#create-and-use-a-new-trait)
  * [Abstract methods](./General/Traits.md#abstract-methods)
  * [Stateful traits](./General/Traits.md#stateful-traits)
  * [Traits initialization](./General/Traits.md#traits-initialization)
  * [Customize method received from a Trait](./General/Traits.md#customize-method-received-from-a-trait)
  * [Customize instance variables received from a (stateful) Trait](./General/Traits.md#customize-instance-variables-received-from-a-(stateful)-trait)
  * [Trait composition](./General/Traits.md#trait-composition)
  * [Conflicts](./General/Traits.md#conflicts)

- [Files](./General/Files.md#files)

- [Menubar and World menu](./General/MenuBar.md#menubar-and-world-menu)
  * [Disable the Menubar/WorldMenu](./General/MenuBar.md#disable-the-menubar/worldmenu)
  * [Add your own entries](./General/MenuBar.md#add-your-own-entries)
  * [Change the menu](./General/MenuBar.md#change-the-menu)

- [Interesting things to know for beginners](./General/InterestingsToKnowForBeginners.md#interesting-things-to-know-for-beginners)
  * [API](./General/InterestingsToKnowForBeginners.md#api)
  * [Evaluatable objects (Blocks and Symbols)](./General/InterestingsToKnowForBeginners.md#evaluatable-objects-(blocks-and-symbols))
  * [Tools](./General/InterestingsToKnowForBeginners.md#tools)
  * [Flags](./General/InterestingsToKnowForBeginners.md#flags)
  * [Useful pragmas](./General/InterestingsToKnowForBeginners.md#useful-pragmas)

- [Extensions](./General/Extensions.md#extensions)
  * [Use of Extension methods](./General/Extensions.md#use-of-extension-methods)
  * [Add a new extension method](./General/Extensions.md#add-a-new-extension-method)
  * [Find methods currently extending a class](./General/Extensions.md#find-methods-currently-extending-a-class)
  * [Package loading order is important](./General/Extensions.md#package-loading-order-is-important)

- [Progress bar](./General/ProgressBar.md#progress-bar)
  * [Progress bar on iterators](./General/ProgressBar.md#progress-bar-on-iterators)
  * [Use Jobs](./General/ProgressBar.md#use-jobs)

- [Profiling](./General/Profiling.md#profiling)
  * [Time Profiling](./General/Profiling.md#time-profiling)
  * [Virtual Machine Execution Time Profiling](./General/Profiling.md#virtual-machine-execution-time-profiling)
  * [Space Profiling](./General/Profiling.md#space-profiling)

- [Pragmas](./General/Pragmas.md#pragmas)
  * [Description](./General/Pragmas.md#description)
  * [How to declare a new pragma](./General/Pragmas.md#how-to-declare-a-new-pragma)
  * [Collect pragmas](./General/Pragmas.md#collect-pragmas)
  * [Act on collected pragmas](./General/Pragmas.md#act-on-collected-pragmas)
  * [Examples of pragma usage](./General/Pragmas.md#examples-of-pragma-usage)
  * [See also](./General/Pragmas.md#see-also)

- [How to deploy a Pharo application](./General/DeployYourPharoApplication.md#how-to-deploy-a-pharo-application)
  * [Cruiser](./General/DeployYourPharoApplication.md#cruiser)
  * [Clean your image before deployment](./General/DeployYourPharoApplication.md#clean-your-image-before-deployment)
  * [Sources obfuscation](./General/DeployYourPharoApplication.md#sources-obfuscation)
  * [Change the logo and window title of the application](./General/DeployYourPharoApplication.md#change-the-logo-and-window-title-of-the-application)
  * [Sign your application on Windows and OSX](./General/DeployYourPharoApplication.md#sign-your-application-on-windows-and-osx)
  * [Deploy a Seaside application](./General/DeployYourPharoApplication.md#deploy-a-seaside-application)
- [Where to deploy the application](./General/DeployYourPharoApplication.md#where-to-deploy-the-application)
- [Pharo version for the deployment](./General/DeployYourPharoApplication.md#pharo-version-for-the-deployment)
- [Location of the zip containing the archive](./General/DeployYourPharoApplication.md#location-of-the-zip-containing-the-archive)
- [To launch an image I uses a screen, else the image will close with my ssh session. Here I ensure the session used is closed.](./General/DeployYourPharoApplication.md#to-launch-an-image-i-uses-a-screen,-else-the-image-will-close-with-my-ssh-session.-here-i-ensure-the-session-used-is-closed.)
- [Remove the old version](./General/DeployYourPharoApplication.md#remove-the-old-version)
- [Copy the application](./General/DeployYourPharoApplication.md#copy-the-application)
- [Get a VM and unzip the application](./General/DeployYourPharoApplication.md#get-a-vm-and-unzip-the-application)
- [Launch the application and initialize it on a free and open port](./General/DeployYourPharoApplication.md#launch-the-application-and-initialize-it-on-a-free-and-open-port)

- [Cool Snippets](./General/CoolSnippets.md#cool-snippets)
  * [Download a file with a progress bar](./General/CoolSnippets.md#download-a-file-with-a-progress-bar)
  * [Bench and profile a project from the tests](./General/CoolSnippets.md#bench-and-profile-a-project-from-the-tests)
  * [Automatic transformation of Pharo's methods source code](./General/CoolSnippets.md#automatic-transformation-of-pharo's-methods-source-code)
  * [Browse all available icons](./General/CoolSnippets.md#browse-all-available-icons)
  * [Rename programatically methods](./General/CoolSnippets.md#rename-programatically-methods)
  * [Get all senders/implementors of a selector](./General/CoolSnippets.md#get-all-senders/implementors-of-a-selector)

- [Arff support in Pharo](./ExternalProjects/Export/Arff.md#arff-support-in-pharo)

- [ESCP support in Pharo](./ExternalProjects/Export/ESCP.md#escp-support-in-pharo)

- [CSV support in Pharo](./ExternalProjects/Export/CSV.md#csv-support-in-pharo)
  * [NeoCSV](./ExternalProjects/Export/CSV.md#neocsv)

- [HTML support in Pharo](./ExternalProjects/Export/HTML.md#html-support-in-pharo)

- [XML support in Pharo](./ExternalProjects/Export/XML.md#xml-support-in-pharo)

- [JSON support in Pharo](./ExternalProjects/Export/JSON.md#json-support-in-pharo)
  * [STONJSON](./ExternalProjects/Export/JSON.md#stonjson)
  * [NeoJSON](./ExternalProjects/Export/JSON.md#neojson)
  * [STONJSON v.s. NeoJSON](./ExternalProjects/Export/JSON.md#stonjson-v.s.-neojson)
  * [JSON Schema](./ExternalProjects/Export/JSON.md#json-schema)

- [DataFrame](./ExternalProjects/DataStructures/DataFrame.md#dataframe)

- [Migration from Pharo 6.1 to Pharo 7.0 guideline](./Migration/MigrationToPharo7.md#migration-from-pharo-6.1-to-pharo-7.0-guideline)
  * [Pharo 7 file streams guideline](./Migration/MigrationToPharo7.md#pharo-7-file-streams-guideline)

- [Web browser](./PharoProjects/WebBrowser.md#web-browser)

- [Metalinks](./PharoProjects/Metalinks.md#metalinks)

- [Basic interactions with the OS](./PharoProjects/OS.md#basic-interactions-with-the-os)
  * [Execute shell command](./PharoProjects/OS.md#execute-shell-command)
  * [Write environment variable](./PharoProjects/OS.md#write-environment-variable)
  * [Read environment variable](./PharoProjects/OS.md#read-environment-variable)

- [Numbers](./PharoProjects/Numbers.md#numbers)
  * [Numbers are not primitive](./PharoProjects/Numbers.md#numbers-are-not-primitive)
  * [Scaled decimals](./PharoProjects/Numbers.md#scaled-decimals)
  * [Special numbers](./PharoProjects/Numbers.md#special-numbers)
  * [Parse number](./PharoProjects/Numbers.md#parse-number)

- [Rich text in Pharo](./PharoProjects/RichText.md#rich-text-in-pharo)

- [Cursor](./PharoProjects/Cursor.md#cursor)

- [Announcer](./PharoProjects/Announcer.md#announcer)
  * [Getting started](./PharoProjects/Announcer.md#getting-started)
  * [Defining new kind of Announcement](./PharoProjects/Announcer.md#defining-new-kind-of-announcement)
  * [Using announcer for your subject object](./PharoProjects/Announcer.md#using-announcer-for-your-subject-object)

- [Objects serialization support](./PharoProjects/ObjectsSerialization.md#objects-serialization-support)
  * [STON](./PharoProjects/ObjectsSerialization.md#ston)
  * [Fuel](./PharoProjects/ObjectsSerialization.md#fuel)
  * [STON v.s. Fuel](./PharoProjects/ObjectsSerialization.md#ston-v.s.-fuel)

- [Dynamic variables](./PharoProjects/DynamicVariables.md#dynamic-variables)
  * [Create a new dynamic variable](./PharoProjects/DynamicVariables.md#create-a-new-dynamic-variable)
  * [Add a default value](./PharoProjects/DynamicVariables.md#add-a-default-value)
  * [Use your dynamic variable](./PharoProjects/DynamicVariables.md#use-your-dynamic-variable)
  * [Change the value of the variable in a process](./PharoProjects/DynamicVariables.md#change-the-value-of-the-variable-in-a-process)
  * [Example](./PharoProjects/DynamicVariables.md#example)

- [Contribution guidlines](./CONTRIBUTION.md#contribution-guidlines)
  * [Suggest new pages](./CONTRIBUTION.md#suggest-new-pages)
  * [Entries structure](./CONTRIBUTION.md#entries-structure)
  * [Edit a page](./CONTRIBUTION.md#edit-a-page)
  * [Add a new page](./CONTRIBUTION.md#add-a-new-page)
  * [Deleting, Moving or Renaming a page](./CONTRIBUTION.md#deleting,-moving-or-renaming-a-page)
  * [Review content](./CONTRIBUTION.md#review-content)

- [Documentation Full Index](./TABLEOFCONTENT.md#documentation-full-index)

- [Pharo wiki](./README.md#pharo-wiki)
  * [Contents](./README.md#contents)
  * [Beginners](./README.md#beginners)
  * [General](./README.md#general)
  * [Pharo projects](./README.md#pharo-projects)
  * [External projects](./README.md#external-projects)
  * [Migration guidelines](./README.md#migration-guidelines)
