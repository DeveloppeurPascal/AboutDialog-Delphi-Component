# Version du 2022-07-18 - v1 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

* réorganisation de l'arborescence du projet
* déplacement des packages dans le dossier /packages/11_1-Alexandria
* déplacement des composants et fiches liées dans /sources (fusionable avec les sources des autres projets de composants et librairies Olf Software)
* déplacement des exemples dans les dossiers /samples/fmx et /samples/vcl
* finalisation d'une première version diffusable du projet
* ajout d'une documentation de base dans README.md
* mise à jour de la TODO liste dans TODO.md
* regroupement des historiques de mise à jour dans CHANGELOG.md
* ajout d'un groupe de projets pour regrouper les paquets par version de Delphi
* ajout d'infos de copyright en entête de chaque fichier Pascal (projets, paquets et unités)
* ajout d'une page d'installation en tant que fichier INSTALL.MD
* mise en production du projet sur https://dialogueapropos.developpeur-pascal.fr/

## Pour la VCL

* renommage des fichiers du composant en Olf.VCL.AboutDialogxxx pour éviter les conflits avec des fichiers existants sur les projets l'utilisant
* renommage du composant en TOlfAboutDialog
* renommage de la fiche (non visible de l'extérieur du composant) en TOlfAboutDialogForm
* Le libellé du paquet de composants est désormais "Olf Software - AboutDialog - VCL" dans le menu "Composants / Installer des packages" de l'IDE

## Pour FireMonkey

* renommage des fichiers du composant en Olf.FMX.AboutDialogxxx pour éviter les conflits avec des fichiers existants sur les projets l'utilisant
* renommage du composant en TOlfAboutDialog
* renommage de la fiche (non visible de l'extérieur du composant) en TOlfAboutDialogForm
* Le libellé du paquet de composants est désormais "Olf Software - AboutDialog - FMX" dans le menu "Composants / Installer des packages" de l'IDE
