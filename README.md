# Pharo wiki

Wiki for the [Pharo](https://pharo.org/features) programming language and development environment.

The website is published at [pharo-open-documentation.github.io/pharo-wiki](https://pharo-open-documentation.github.io/pharo-wiki/).

The wiki's pages and images live in the `docs/` folder. Their old paths at the root of this repository are symlinks to it, kept so that existing GitHub links keep working. See [Why the root symlinks exist](#why-the-root-symlinks-exist).

## Contribution guide

  * [Suggest new pages](#suggest-new-pages)
  * [Entries structure](#entries-structure)
  * [Edit a page](#edit-a-page)
  * [Add a new page](#add-a-new-page)
  * [Build the website locally](#build-the-website-locally)
  * [Why the root symlinks exist](#why-the-root-symlinks-exist)
  * [Deleting, Moving or Renaming a page](#deleting-moving-or-renaming-a-page)
    + [If the content is obsolete](#if-the-content-is-obsolete)
    + [If we want to move a page](#if-we-want-to-move-a-page)
    + [If we want to split a page](#if-we-want-to-split-a-page)
    + [If we want to rename a page](#if-we-want-to-rename-a-page)
  * [Review content](#review-content)

## Suggest new pages

To suggest pages that could fit the wiki, the recommended way is to open an issue containing a summary of the page, the section in which it should be (General, Internal projects of Pharo or External projects of Pharo) and optionaly links to already existing documentation on the projects.

On the language and environment part of the wiki we accept to document a part of the system that is already documented outside the wiki at the condition that we reference in a *See also* section those documentation. The goal is to have a centralized information and to provide alternative ways of explaining.

If you wish to add the new entry yourself, see section [Add a new page](#add-a-new-page).

## Entries structure

The entries structure is free but we still have some conventions. 

* Documentation on external projects should mostly contains small example, comparaisons between multiple projects, links to official documentations and repositories. We can accept documentation on the external projects in case there is none by the official maintainer.
* Long entries do not need a table of contents. The website builds one automatically in its right-hand column.
* References to external/decentralized documentation should be in a *See also* section at the end of the entry.
* Smalltalk code snippets should use *tabulation* for source code indentation.
* Smalltalk code snippets should be highlighted using this format:


<pre>```Smalltalk
 Code
```</pre>

## Edit a page

If you see some typo, wrong informations or missing informations you can report it in the issues or you can also edit the document yourself and create a pull request.

Creating a PR is as easy as clicking on the Edit (pen) button, updating the document and propose the changes.

![Gif showing how to edit a page](Resources/EditPage.gif)

## Add a new page

In order to add a page you can just go into the folder of the section where the page should be, inside `docs/`, and use the "Create new file" button.

![Gif showing how to add a new page](Resources/CreatePage.gif)

If you need to integrate images in your entry, add them next to the Markdown file with an name following this pattern: `<NameOfThePage>_Image_<Description>.extension`. For example for an image illustrating `Iceberg` in a page called `VCS.md`, the image should be named `VCS_Image_IcebergIllustration.png`.

Once the page is created you need to add it to the [docs/README.md](docs/README.md) in the right section, and to the `nav` of `mkdocs.yml` so that it appears on the website.

> Note: If you have contents to start a page but don't have the time or the knowledge to finish it, it is advised to propose a pull request with the content you have already. Then, one can open issues with the missing sections pointing to the concerned page.

An entry in docs/README.md can also have a one short sentense explaining the purpose of the page.

The pages focus mainly on the latest stable Pharo version. This does not mean we refuse contribution on older or development version of Pharo, but those should be complement of the current stable version.

## Why the root symlinks exist

The pages used to sit at the root of the repository, and their GitHub links (such as `General/Traits.md`) are shared across the web. The website needs the sources in their own folder: MkDocs and Zensical refuse the repository root as the source folder, because the configuration file lives there too. So the sources moved to `docs/`.

The old paths still have to work for wiki content, so each page and image has a symlink to its new place in `docs/`. Each section folder at the root is a real folder that holds one symlink per page and image.

Technical files are not symlinked. This README, the license and the CI configuration stay at the root, and `MANIFEST.md` and `CONTRIBUTION.md` no longer have root paths.

Folder symlinks would be shorter, but GitHub does not follow a symlinked folder in `blob` or `raw` URLs. With `General` as a folder symlink, `github.com/.../blob/master/General/Traits.md` returned 404, while a file symlink works. That is why every file has its own symlink.

Git stores symlinks as links, so history follows the page in `docs/`. The symlinks only serve old paths: a new page needs no root symlink, and a page that is moved or renamed inside `docs/` needs its root symlink updated. The checkout caveat on Windows is explained in [Build the website locally](#build-the-website-locally).

## Build the website locally

The website is built with [Zensical](https://zensical.org/) from the `mkdocs.yml` configuration. The pages and images live in `docs/`. The folders and files at the root of the repository are symlinks to `docs/`, kept so that the old GitHub paths keep working.

To preview your changes:

```bash
python3 -m venv .venv
source .venv/bin/activate
pip install -r requirements-docs.txt
zensical serve
```

Before opening a pull request, run the same strict build as the CI. It fails on broken links, missing pages and broken anchors:

```bash
zensical build --strict
```

New pages do not need a symlink at the root. If you move or rename a page or image inside `docs/`, update its root symlink too, otherwise the old GitHub path breaks.

On Windows, the root symlinks only work when developer mode is enabled and git is configured with `core.symlinks=true`. Otherwise git checks them out as plain text files containing the target path. Editing inside `docs/` is always safe.

## Deleting, Moving or Renaming a page

**/!\\ We never delete a page. /!\\**

This is a rule in this wiki. What is more annoying than a 404 when we look for documentation?

Instead we have different strategies depending on why we want to delete a page.

### If the content is obsolete

If the content is obsolete, we do not delete the page but we add a note (using `> OBSOLETE : bla`) at the beginning explaining why the guide is obsolete. The guide can be removed from docs/README.md. 

### If we want to move a page

If we want to move a page, we can copy it to the new location and let the old page at the original location with an explanation and a link to the new page.

### If we want to split a page

Sometimes a page is too big and would win to be splited. In that case the content can be extracted in multiple pages but the original page should stay and contains an index of the new pages.

### If we want to rename a page

If we want to rename a page, we can copy it with the new name and let the old page at the original location with an explanation and a link to the new page.

## Review content

It is really helpful to get reviews of the wiki's content. This can be achieved in multiple ways:
- Review a Pull Request. Comments, approvals and changes request on pull request will help the intergation of new contents.
- Open issues on existing entries.
- Create a Pull Request to propose enhancements to an existing entry.
