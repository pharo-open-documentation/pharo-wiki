# Source Code Export formats

Pharo stores its source code in memory (by default) or in a snapshot of the memory when we save a Pharo image. This makes it complicated to manage source code in the long term or to collaborate with others.

In order to be able to save some source code in readable files, Pharo includes some textual export formats. This page will explain the main export formats used to save code in VCS such as git.

This page will present formats from the most recommended for new projects to the less recommended.

- [Comparisons](#comparisons)
  * [Overview](#overview)
  * [Supported Pharo versions](#supported-pharo-versions)
- [Tonel](#tonel)
  * [Tonel Pros and Cons](#tonel-pros-and-cons)
  * [Tonel supported Pharo versions](#tonel-supported-pharo-versions)
  * [Tonel versions](#tonel-versions)
- [FileTree metadata less](#filetree-metadata-less)
  * [FileTree metadata less Pros and Cons](#filetree-metadata-less-pros-and-cons)
  * [FileTree metadata less supported Pharo versions](#filetree-metadata-less-supported-pharo-versions)
- [FileTree metadata full](#filetree-metadata-full)
  * [FileTree metadata full Pros and Cons](#filetree-metadata-full-pros-and-cons)
  * [FileTree metadata full supported Pharo versions](#filetree-metadata-full-supported-pharo-versions)


## Comparisons

### Overview 

|| Tonel | FileTree metadata less | FileTree metadata full |
| ------------- | ------------- |------------- |------------- |
| Work out of the box since Pharo | 6.1 | 4 | 3 |
| Older version supported after update | 4 | 4 | 3 |
| Export granularity | Class | Method | Method |
| Works well on windows | :white_check_mark: | :x: | :x: |
| Easy to merge two branches | :white_check_mark: | :white_check_mark: | :x: |
| Takes a reasonable space on file system | :white_check_mark: | :x: | :x: |
| Works with Iceberg | :white_check_mark: | :white_check_mark: | :x: |
| Works with Monticello | :white_check_mark: | :white_check_mark: | :white_check_mark: |

### Supported Pharo versions

| Pharo Version | FileTree metadata full | FileTree metadata less | Tonel |
| ------------- | ------------- |------------- |------------- |
3 |  :white_check_mark: | :x: | :x: |
4 |  :white_check_mark: |  :white_check_mark: | Update Metacello + Install Tonel |
5 |  :white_check_mark: |  :white_check_mark: | Update Metacello + Install Tonel |
6.0 |  :white_check_mark: |  :white_check_mark: | Update Metacello + Install Tonel |
6.1 | :white_check_mark: | :white_check_mark: | :white_check_mark: |
7 | :white_check_mark: | :white_check_mark: | :white_check_mark: |
8 | :white_check_mark: | :white_check_mark: | :white_check_mark: |

## Tonel 

Tonel ([https://github.com/pharo-vcs/tonel](https://github.com/pharo-vcs/tonel)) is an export format introduced in Pharo 6.1. 

It creates one file by class or extended class and works well on Windows (compared to file tree).

### Tonel Pros and Cons

Pros
- Works well on Windows 
- Not much space lost because of small files
- Export readable files
- No problem during merge because of metadata

Cons
- Works out of the box since Pharo 6.1.
- Since the format is more recent than the others, some edges cases might cause trouble. (It rarely happens)

### Tonel supported Pharo versions

Tonel works out of the box in Pharo 6.1 and higher. But it is possible to make it work in Pharo 4 to 6.0 by updating Metacello and installing Tonel.

```Smalltalk
Metacello new
	baseline: 'Metacello';
	repository: 'github://Metacello/metacello:pharo-6.1_dev/repository';
	get.
Metacello new
	baseline: 'Metacello';
	repository: 'github://Metacello/metacello:pharo-6.1_dev/repository';
	onConflict: [:ex | ex allow];
	load.
Metacello new 
	repository: 'github://pharo-vcs/tonel';
	baseline: 'Tonel';
	load.
```

### Tonel versions

Tonel got multiple versions over the years, each tweaking the export format:
- version 1.0: original tonel export format
- version 2.0: this version ensure that in the metadatas the keys are symbols and the values are strings. This change happened in order to be compatible with Gemstone. Note that this format was never used as a default export format of Pharo
- version 3.0: this version has the changes of the 2.0 but it also adds the properties `package` and `tag` to replace the `category` property for class definitions. This is to remove same ambiguity in the class definitions. The `category` property is kept by default for backward compatibility, but the TonelWriter can be configured to not export this property. This format can also be improved to export more metadata. For example, it is planned to export a property `deprecatedAliases` to manage some class deprecations in the future.

If you want to change your export format and convert all the files of a repository at once to avoid to have multiple PR with format changes you can use the following script.
Take a Pharo 12 or Pharo 13 images and run the following script. It will convert all the loaded packages in your image so you may load extra packages to prepare such operation. 

```st
| projectName repository |
projectName := 'ProjectNameInIceberg'.
repository := IceRepository repositories detect: [ :repo | repo name = projectName ].
repository workingCopy packages do: [ :pkg |
IceLibgitTonelWriter forInternalStoreFileOut: pkg latestVersion mcVersion on: repository ]
```

**NOTE: you must ensure all your project packages are loaded in the image (check in the Repository | Packages window, and right click load any that aren't).**

Once you have run this script, use the "Repository, Project | Extra | Open in native file browser" menu option to open an OS terminal and then run:
```
git commit -a -m "Update tonel formal to V3"
git push
```

Since Pharo 12, it is also possible to indicate in a Tonel project which version of Tonel to use to export some code. The file to update is the `.properties` file that is in the source folder and it should look like this:

```
{
    #format : #tonel,
    #version: #'1.0' //could be 2.0 or 3.0
}
```

This is useful for example if a project has contributors on P11 using v1 format and contributors in P12 using v3 format since P11 is unable to export Tonel v3 format. 

## FileTree metadata less 

FileTree ([https://github.com/dalehenrich/filetree](https://github.com/dalehenrich/filetree)) is the first export format that was integrated in the goal to use Pharo with Git. The first version had a lot of metadata (see section Filetree metadata full), the second was a new version with metadata less format. 

This format exports one file per method. This can cause trouble with Windows because Windows has a maximum path and file name length of 255 characters. This limit can easily be reached with FileTree. 

Another problem of the FileTree format is that most Pharo's methods are short and creating one file by method waste a lot of space because the physical representation of files on a drive has a minimal size (4 KiB on NTFS file systems) that is reached by most of Pharo's method. 

Its advantage compared to FileTree metadata full is that there is no problem during the merge of the source code thanks to the absence of metadata.

### FileTree metadata less Pros and Cons 

Pros
- Easy to browse the history of a method
- Present since Pharo 4 out of the box
- No problem during merge because of metadata

Cons
- Cause trouble on Windows
- Waste space on the file system

### FileTree metadata less supported Pharo versions

FileTree metadata less is supported out of the box since Pharo 4.0 and cannot easily be used in older Pharo versions. 

## FileTree metadata full

FileTree metadata full ([https://github.com/dalehenrich/filetree](https://github.com/dalehenrich/filetree) )is the ancestor of FileTree metadata less and works in the same way but also export a lot of metadata relating to the commits such has a commit message (different from the one of the VCS), the timestamp of the method compilation, etc.

This simplifies the tooling because the tools can get some information from the metadata instead of the VCS but it also creates a lot of trouble while merging two branches using this format.

Merges can be eased via this project: [https://github.com/ThierryGoubier/GitFileTree-MergeDriver](https://github.com/ThierryGoubier/GitFileTree-MergeDriver)

This format can be used via Monticello but not via Iceberg. Iceberg will ignore the format and export in FileTree metadata less.

### FileTree metadata full Pros and Cons

Pros
- Easy to browse the history of a method
- Present in most Pharo versions out of the box

Cons
- Cause trouble on Windows
- Waste space on the file system
- Hard to merge two branches because of metadata
- Does not work with Iceberg, the default git tool since Pharo 7

### FileTree metadata full supported Pharo versions

This format works out of the box in most Pharo versions.
