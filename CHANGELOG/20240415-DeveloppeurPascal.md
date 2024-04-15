# 20240415 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

* mise à jour des librairies (dépendance utilisée dans les projets d'exemples)
* basculement des sources du sous-dossier /sources ver le sous-dossier /src pour se conformer aux habitudes des autres projets de composants et librairies
* projets de démo mis à jour en Delphi 12 Athens avec les bons chemins pour les fichiers du composant
* création du paquet de composants pour Delphi 10.4 Sydney
* création du paquet de composants pour Delphi 11 Alexandria
* création du paquet de composants pour Delphi 12 Athens

* mise à niveau des README FR/EN par rapport à la licence, au titre du projet et à la nouvelle arborescence de fichiers
* transfert des éléments du TODO vers GitHub Issues

* tri des TODO à faire ou abandonner

* changement de la présentation le la liste des unités dans les USES de la librairie pour simplifier les git suivants en cas d'ajout ou de retrait de dépendances
* ajout du prérenseignement des champs manquants du composant VCL dans son constructeur
* ajout du prérenseignement des champs manquants du composant FMX dans son constructeur
* prérenseignement du titre de la boite de dialogue à partir de celui de son propriétaire si elle n'en a pas et que le propriétaire est une fiche
* modification de la forme du curseur de la souris sur le lien "url" uniquement s'il est clicable
* modification du style (couleur+souligné) du lien "url" selon qu'il est clicable ou pas

* ajout d'événements sur le composant et la boite de dialogue afin de préparer l'accès à de futures modifications (API permettant de manipuler le contenu affiché à l'écran) : onBeforeExecute (#29), onAfterExecute (#30), onFormCreate (#11), onFormActivate (#13), onFormShow (#12), onFormClose (#14)
* modification des programmes VCL/FMX d'exemple afin de tester ces changements
