# "About" dialog box as a Delphi Component

[Cette page en français.](LISEZMOI.md)

To have standard "About" dialogs in VCL and FireMonkey projects.

This code repository contains a project developed in Object Pascal language under Delphi. You don't know what Delphi is and where to download it ? You'll learn more [on this web site](https://delphi-resources.developpeur-pascal.fr/).

The first steps of creating those components were live coded on [Twitch](https://www.twitch.tv/patrickpremartin). Replays are available on demand at [Serial Streameur](https://serialstreameur.fr/boite-de-dialogue-a-propos-composant-delphi-vcl-et-fmx.html).

If you are interested in components creation or behind the code for TOlfAboutDialog, look at [Developpeur Pascal blog](https://developpeur-pascal.fr/boite-de-dialogue-a-propos-de.html).

## Talks and conferences

### Twitch

Follow my development streams of software, video games, mobile applications and websites on [my Twitch channel](https://www.twitch.tv/patrickpremartin) or as replays on [Serial Streameur](https://serialstreameur.fr/boite-de-dialogue-a-propos-composant-delphi-vcl-et-fmx.html) mostly in French.

## Contents of the project

In the /packages folder you will find the sources of the packages to compile and install to use the TOlfAboutDialog components in your VCL or FireMonkey projects.

In the /src folder you will find the sources of the components and their dependencies.

In the /samples folder you will find demo projects of using these components as components or as live code. You can use them as inspiration for your own projects.

The CHANGELOG folder contains changes made to the project versions. If you are upgrading components, take a look at it to see what has happened since your previous installation.

## Install

To download this project you better should use "git" command but you also can download a ZIP from [its GitHub repository](https://github.com/DeveloppeurPascal/AboutDialog-Delphi-Component).

**Warning :** if the project has submodules dependencies they wont be in the ZIP file. You'll have to download them manually.

The sources of the component packages can be found in the /packages folder. There you will find a VCL version and a FireMonkey version depending on your RAD Studio or Delphi release.

You do not have to install both components. If you only do VCL projects, install only the VCL package. If you only do FireMonkey projects, install only the FMX package.

The sources of the components are in the /src folder. If you just want to manipulate them by code you do not need to install the packages. Just use the source files in your projects.

To install a component package:
- open the package source in the Delphi IDE or RAD Studio
- uninstall the package if it was already installed (right click, then "uninstall" on the package from the project manager)
- compile the package
- install the package (right click, then "install" on the package from the project manager)
- add the sources of the components in your sources folder or the path to the /src folder in the default search path in the IDE options (preferable) or your project options (for each project)

## Dependencies

This project depends on :

* [DeveloppeurPascal/librairies](https://github.com/DeveloppeurPascal/librairies) is used by the sample projects and must be in the folder ./samples/lib-externes/librairies

## Compatibility

As an [Embarcadero MVP](https://www.embarcadero.com/resources/partners/mvp-directory), I benefit from the latest versions of [Delphi](https://www.embarcadero.com/products/delphi) and [C++ Builder](https://www.embarcadero.com/products/cbuilder) in [RAD Studio](https://www.embarcadero.com/products/rad-studio) as soon as they are released. I therefore work with these versions.

Normally, my libraries and components should also run on at least the current version of [Delphi Community Edition](https://www.embarcadero.com/products/delphi/starter).

There's no guarantee of compatibility with earlier versions, even though I try to keep my code clean and avoid using too many of the new ways of writing in it (type inference, inline var and multiline strings).

If you detect any anomalies on earlier versions, please don't hesitate to [report them](https://github.com/DeveloppeurPascal/AboutDialog-Delphi-Component/issues) so that I can test and try to correct or provide a workaround.

## License to use this code repository and its contents

This source code is distributed under the [AGPL 3.0 or later license](https://choosealicense.com/licenses/agpl-3.0/).

You are generally free to use the contents of this code repository anywhere, provided that:
* you mention it in your projects
* distribute the modifications made to the files supplied in this project under the AGPL license (leaving the original copyright notices (author, link to this repository, license) which must be supplemented by your own)
* to distribute the source code of your creations under the AGPL license.

If this license doesn't suit your needs, you can purchase the right to use this project under the [Apache License 2.0](https://choosealicense.com/licenses/apache-2.0/) or a dedicated commercial license ([contact the author](https://developpeur-pascal.fr/nous-contacter.php) to explain your needs).

These source codes are provided as is, without warranty of any kind.

Certain elements included in this repository may be subject to third-party usage rights (images, sounds, etc.). They are not reusable in your projects unless otherwise stated.

## How to ask a new feature, report a bug or a security issue ?

If you want an answer from the project owner the best way to ask for a new feature or report a bug is to go to [the GitHub repository](https://github.com/DeveloppeurPascal/AboutDialog-Delphi-Component) and [open a new issue](https://github.com/DeveloppeurPascal/AboutDialog-Delphi-Component/issues).

If you found a security issue please don't report it publicly before a patch is available. Explain the case by [sending a private message to the author](https://developpeur-pascal.fr/nous-contacter.php).

You also can fork the repository and contribute by submitting pull requests if you want to help. Please read the [CONTRIBUTING.md](CONTRIBUTING.md) file.

## Support the project and its author

If you think this project is useful and want to support it, please make a donation to [its author](https://github.com/DeveloppeurPascal). It will help to maintain the code and binaries.

You can use one of those services :

* [GitHub Sponsors](https://github.com/sponsors/DeveloppeurPascal)
* Ko-fi [in French](https://ko-fi.com/patrick_premartin_fr) or [in English](https://ko-fi.com/patrick_premartin_en)
* [Patreon](https://www.patreon.com/patrickpremartin)
* [Liberapay](https://liberapay.com/PatrickPremartin)
* [Paypal](https://www.paypal.com/paypalme/patrickpremartin)

or if you speack french you can [subscribe to Zone Abo](https://zone-abo.fr/nos-abonnements.php) on a monthly or yearly basis and get a lot of resources as videos and articles.
