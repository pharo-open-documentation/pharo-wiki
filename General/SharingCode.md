# Sharing Pharo code

While collaborating it is important to be able to share code. This page is here to describe multiple ways to share code depending on your needs.

## Versions control system

The first way to share code, and the most lasting and stable one is to use a versions control system (VCS).

A VCS is a tool to version and manage documents, in our case: code. Those tools allozs one to store the code source of an application, to browse previous versions and to revert changes.

Pharo environment comes with two VCS integrations:
- Iceberg (Since Pharo 7) - A [Git](https://git-scm.com/) client
- Monticello - A smalltalk specialized VCS

### Iceberg 

Iceberg is a set of tools that allow one to handle [git](https://git-scm.com/) repositories directly from a Pharo image.

Via this tool one can commit versions of its code, use branch system and do much more. 

Since Iceberg and git are a big subject, further documentation can be [found here](SettingUpANewProject.md).

### Monticello 

Monticello is a distributed, optimistic, concurrent, versioning system for Pharo code. It is a tool specific to some [Smalltalk](Glossary.md#smalltalk) implementations such as Pharo, Squeak and Cuis.

Since Pharo 7 it is recommanded to use Iceberg over Monticello since mantenance is centered around Iceberg and git is much more used worldwide than Monticello.

You can find a tutorial on Monticello usage in the Chapter 8 of the «Updated Pharo by example» book [(http://books.pharo.org/updated-pharo-by-example/pdf/2018-09-29-UpdatedPharoByExample.pdf)](http://books.pharo.org/updated-pharo-by-example/pdf/2018-09-29-UpdatedPharoByExample.pdf).

## File out 

## Sharing scripts

### ST files 

### Remote playground 

## Fuel 