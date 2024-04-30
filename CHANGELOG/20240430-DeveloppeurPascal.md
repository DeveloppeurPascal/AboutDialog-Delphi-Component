# 20240430 - [DeveloppeurPascal](https://github.com/DeveloppeurPascal)

* ajout de "auto" dans la liste des langues disponibles pour permettre à la librairie de détecter la langue actuelle de l'ordinateur (sur les plateformes supportées)et activer la bonne sélection de façon automatique. (utilisation de GetCurrentLanguage() et GetCurrentLanguageISOCode() copiées depuis l'unité Olf.RTL.Language du dépôt [DeveloppeurPascal/Librairies](https://github.com/DeveloppeurPascal/librairies))
* traduction automatique de la fenêtre lors d'un changement de langue une fois qu'elle est affichée ou chargée (par exemple depuis l'un des événements disponibles dessus)
* mise à jour des programmes d'exemples pour tester ces deux fonctionnalités
