# Menubar and World menu

In Pharo, to open tools or execute actions a Menubar (At the top of the screen) and a WorldMenu (Appears on a left click in the world) are present.

This page will document sone aspects of those features. 

## Disable the Menubar/WorldMenu

In some case, we might want to hide the menubar and/or the world menu. For example, when releasing a new verison of a project to deploy, the development tools needs to be hidden. 

It is possible to do so with some settings.

*Disable Menubar*

This option is named `Show menubar` in the setting browser and is equivalent of executing:

```Smalltalk
MenubarMorph showMenubar: false
```

*Disable the WorldMenu*

It is not possible to disable this feature alone (without disabling the Menubar) in Pharo <= 7. 

In Pharo 8 it is possible to disable the WorldMenu alone with the setting named `Show world menu` or by executing:

```Smalltalk
PasteUpMorph shouldShowWorldMenu: false
```

* Disable WorldMenu and Menubar*

To disable both the menubar and the world menu it is possible to disable both of them individually, or it is also possible to change the setting `World menu pragma` to set an empty pragma. programmatically, it can be done this way:

```Smalltalk
WorldState desktopMenuPragmaKeyword: ''
```

## Add your own entries

## Change the menu
