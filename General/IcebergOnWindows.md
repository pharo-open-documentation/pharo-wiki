### Long file names in Windows

Its error message goes by, “Failed to stat file ‘filename’: The filename or extension is too long”.

The Windows problem with long filenames is a constraint of [libgit](https://libgit2.org/libgit2/#HEAD) library that is used in Pharo (Iceberg accesses git through this exact library). It occurs because FileTree format tends to have long paths and was one of the main drivers in creating the Tonel format. This error can occur, for example, when you are trying to clone your Pharo fork on Github into Pharo local. There are two possible ways to fix it:

1. Make path to Pharo Image short.
2. Convert repo to Tonel format — in Iceberg there is a menu item under one of the Extras menus.

Here’s more about them:

## Make path to the Pharo Image shorter

If you are simply using a standalone Pharo [download](http://pharo.org/download/#standalone), you can just move your image and VM a few folders up. However, if you are using [Pharo Launcher](http://pharo.org/download/), you have to access the settings, to change where it stores images it will create in the future, as well as move all the existing images and VMs up. Here’s how to do it.

In Pharo Launcher, in the bottom right corner you can see a button ‘open settings’ — click on it.

In the settings, if you expand the ‘Pharo Launcher’ tab you can see the following options: 1. Location of template sources file. 2. Template sources URL. 3. Location of your images. 4. VMs directory.

In the first, third and fourth options, change directories without disrupting the dependencies (images in ‘images’, template sources one folder up, etc). When you change the paths, the upper right corner of each of the text boxes will be yellow, which means unsaved changes (that is also the logic inside Pharo). You have to position your cursor in each of the text boxes and press (ctrl+s) to save the changes.

Don’t forget that this section only means images and VMs it will create in the future, so you should also manually move your already existing images and VMs to the same folder you listed. 

Now, everything should work!

#### 2. Convert the repository to the Tonel format

Note re FileTree/Tonel: A FileTree represents a hierarchy of files. [Tonel](https://github.com/pharo-vcs/tonel) is a file-per-class format — class and all methods are stored in a single file, which is faster than having a file for each method; it also prevents some trouble with git on Windows due to number of files and subdirectory depth.

If you are using an older version of Pharo, you need to download Tonel using the code below (in Pharo 7 and 8 it comes by default):

```Smalltalk
Metacello new
  baseline: 'Tonel';
  repository: 'github://pharo-vcs/tonel:v1.0.12';
  load.
```

(In Pharo 7 and 8 you don’t need any kind of script to convert to Tonel — there’s a UI option available. But in Pharo 6.1 you need to upgrade Iceberg first, and Tonel will come with it. Link with the script [here](https://github.com/pharo-vcs/iceberg/#for-pharo-61)).

With Tonel loaded, if you go to Iceberg repositories and right-click on the specific repository, you can go to ‘extra’ and select ‘convert repository sources to tonel’. It will migrate the code for you.

However, this option is only possible if you repository isn’t already in the Tonel format. So if your problem is, like mine, cloning the Pharo fork (Pharo repo is partially tonel, partially filetree), then the only option that will actually solve it, is the first one — shortening the path.
