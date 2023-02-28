# TOlfAboutDialog Delphi Component

[Cette page en français.](LISEZMOI.md)

To have standard "About" dialogs in VCL and FireMonkey projects.

This code repository contains a project developed in Object Pascal language under Delphi. You don't know what Delphi is and where to download it ? You'll learn more [on this web site](https://delphi-resources.developpeur-pascal.fr/).

The first steps of creating those components were live coded on [Twitch](https://www.twitch.tv/patrickpremartin). Replays are available on demand at [Serial Streameur](https://serialstreameur.fr/boite-de-dialogue-a-propos-composant-delphi-vcl-et-fmx.html).

If you are interested in components creation or behind the code for TOlfAboutDialog, look at [Developpeur Pascal blog](https://developpeur-pascal.fr/boite-de-dialogue-a-propos-de.html).

## Contents of the project

In the /packages folder you will find the sources of the packages to compile and install to use the TOlfAboutDialog components in your VCL or FireMonkey projects.

In the /sources folder you will find the sources of the components and their dependencies.

In the /samples folder you will find demo projects of using these components as components or as live code. You can use them as inspiration for your own projects.

The TODO.md file contains planned changes for a future release.

The CHANGELOG folder contains changes made to the project versions. If you are upgrading components, take a look at it to see what has happened since your previous installation.

## Install

To download this project you better should use "git" command but you also can download a ZIP from [its GitHub repository](https://github.com/DeveloppeurPascal/AboutDialog-Delphi-Component).

**Warning :** if the project has submodules dependencies they wont be in the ZIP file. You'll have to download them manually.

The sources of the component packages can be found in the /packages folder. There you will find a VCL version and a FireMonkey version depending on your RAD Studio or Delphi release.

You do not have to install both components. If you only do VCL projects, install only the VCL package. If you only do FireMonkey projects, install only the FMX package.

The sources of the components are in the /sources folder. If you just want to manipulate them by code you do not need to install the packages. Just use the source files in your projects.

To install a component package:
- open the package source in the Delphi IDE or RAD Studio
- uninstall the package if it was already installed (right click, then "uninstall" on the package from the project manager)
- compile the package
- install the package (right click, then "install" on the package from the project manager)
- add the sources of the components in your sources folder or the path to the /sources folder in the default search path in the IDE options (preferable) or your project options (for each project)

## Dependencies

This project depends on :

* none

## How to ask a new feature, report a bug or a security issue ?

If you want an answer from the project owner the best way to ask for a new feature or report a bug is to go to [the GitHub repository](https://github.com/DeveloppeurPascal/AboutDialog-Delphi-Component) and [open a new issue](https://github.com/DeveloppeurPascal/AboutDialog-Delphi-Component/issues).

If you found a security issue please don't report it publicly before a patch is available. Explain the case by [sending a private message to the author](https://developpeur-pascal.fr/nous-contacter.php).

You also can fork the repository and contribute by submitting pull requests if you want to help. Please read the [CONTRIBUTING.md](CONTRIBUTING.md) file.

## Dual licensing model

This project is distributed under [AGPL 3.0 or later](https://choosealicense.com/licenses/agpl-3.0/) license.

If you want to use it or a part of it in your projects but don't want to share the sources or don't want to distribute your project under the same license you can buy the right to use it under the [Apache License 2.0](https://choosealicense.com/licenses/apache-2.0/) or a dedicated license ([contact the author](https://developpeur-pascal.fr/nous-contacter.php) to explain your needs).

## Support the project and its author

If you think this project is useful and want to support it, please make a donation to [its author](https://github.com/DeveloppeurPascal). It will help to maintain the code and binaries.

You can use one of those services :

* [GitHub Sponsors](https://github.com/sponsors/DeveloppeurPascal)
* [Liberapay](https://liberapay.com/PatrickPremartin)
* [Patreon](https://www.patreon.com/patrickpremartin)
* [Paypal](https://www.paypal.com/paypalme/patrickpremartin)

or if you speack french you can [subscribe to Zone Abo](https://zone-abo.fr/nos-abonnements.php) on a monthly or yearly basis and get a lot of resources as videos and articles.
