# Export formats

Pharo store its code in memory by default or in a snapshot of the memory when we save a Pharo image. This make it complicated to manage code in the long terme or to collaborate with others.

In order to be able to save some code in readable files, Pharo includes some textual export formats. This page will explain the main export formats used to save code in VCS such as git.

This page will present formats from the most recommanded for new projects to the less recommanded.

- [Comparisons](#comparisons)
  * [Overview](#overview)
  * [Supported Pharo versions](#supported-pharo-versions)
- [Tonel](#tonel)
  * [Tonel Pros and Cons](#tonel-pros-and-cons)
  * [Tonel supported Pharo versions](#tonel-supported-pharo-versions)
- [FileTree metadata less](#filetree-metadata-less)
- [FileTree metadata less Pros and Cons](#filetree-metadata-less-pros-and-cons)
  * [FileTree metadata less supported Pharo versions](#filetree-metadata-less-supported-pharo-versions)
- [FileTree metadata full](#filetree-metadata-full)
  * [FileTree metadata full Pros and Cons](#filetree-metadata-full-pros-and-cons)
  * [FileTree metadata full supported Pharo versions](#filetree-metadata-full-supported-pharo-versions)


## Comparisons

### Overview 

|| Tonel | FileTree metadata less | FileTree metadata full |
| ------------- | ------------- |------------- |------------- |
| Work out of the box since Pharo | 6.1 | 4 | 3 |
| Export granularity | Class | Method | Method |
| Works well on windows | :white_check_mark: | :x: | :x: |
| Easy to merge two branches | :white_check_mark: | :white_check_mark: | :x: |
| Takes a reasonable space on file system | :white_check_mark: | :x: | :x: |
| Works with Iceberg | :white_check_mark: | :white_check_mark: | :x: |
| Works with Monticello | :white_check_mark: | :white_check_mark: | :white_check_mark: |

### Supported Pharo versions

| Pharo Version | FileTree metadata full | FileTree metadata less | Tonel |
| ------------- | ------------- |------------- |------------- |
3 |  :white_check_mark: | Update Metacello? | :x: |
4 |  :white_check_mark: |  :white_check_mark: | Update Metacello + Install Tonel |
5 |  :white_check_mark: |  :white_check_mark: | Update Metacello + Install Tonel |
6.0 |  :white_check_mark: |  :white_check_mark: | Update Metacello + Install Tonel |
6.1 | :white_check_mark: | :white_check_mark: | :white_check_mark: |
7 | :white_check_mark: | :white_check_mark: | :white_check_mark: |
8 | :white_check_mark: | :white_check_mark: | :white_check_mark: |

## Tonel 

Tonel ([https://github.com/pharo-vcs/tonel](https://github.com/pharo-vcs/tonel)) is an export format introduced in Pharo 6.1. 

It creates one file by class or extented class and works well on windows (compared to file tree).

### Tonel Pros and Cons

Pros
- Works well on windows 
- Not much space lost because of small files
- Export readable files
- No problem during merge because of metadata

Cons
- Work out of the box only since Pharo 6.1
- Since the format is more recent than the others, some edges cases might cause trouble. (It rarely happens)

### Tonel supported Pharo versions

Tonel works out of the box in Pharo 6.1 and higher. But it is possible to make it work in Pharo 4 to 6.0 by updating Metacello and installing Tonel.

```Smalltalk
Metacello new
  baseline: 'Metacello';
  repository: 'github://Metacello/metacello:master/repository';
  get.
Metacello new
  baseline: 'Metacello';
  repository: 'github://Metacello/metacello:master/repository';
  onConflict: [:ex | ex allow];
  load.
Metacello new 
	repository: 'github://pharo-vcs/tonel';
	baseline: 'Tonel';
	load.
```

## FileTree metadata less 

FileTree ([https://github.com/dalehenrich/filetree](https://github.com/dalehenrich/filetree)) is the first export format planned to use Pharo with Git that was integrated. It had a first version with a lot of metadata (see section Filetree metadata full) then a new version with a metadata less format. 

This format exports one file per methods. This can cause trouble with Windows because Windows has a maximum path and file name length of 255 characters. This limit can easily be reached with FileTree. 

Another problem of the FileTree format is that most Pharo's methods are short and creating one file by method waste a lot of space because the physical representation of files on a drive has a minimal size (4 KiB on NTFS file systems) that is reached by most of Pharo's method. 

Its advantage compared to FileTree metadata full is that there is no problem during merge because of metadata.

## FileTree metadata less Pros and Cons 

Pros
- Easy to browse the history of a method
- Present since Pharo 4 out of the box
- No problem during merge because of metadata

Cons
- Cause trouble on Windows
- Waste space on the file system

### FileTree metadata less supported Pharo versions

FileTree metadata less is supported out of the box since Pharo 4.0. 

It might be possible to use it in older Pharo version by updating Metacello.

## FileTree metadata full

FileTree metadata full ([https://github.com/dalehenrich/filetree](https://github.com/dalehenrich/filetree))is the ancestor of FileTree metadata less and works in the same way but also export a lot of metadata relatif to the commits such has a commit message (different from the one of the VCS), timestamp of the method compilation, etc.

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
- Does not work with Iceberg, the default git tool in Pharo

### FileTree metadata full supported Pharo versions

This format works out of the box in most Pharo versions.
