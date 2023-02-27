# Composant Delphi TOlfAboutDialog (en français)

[This page in english.](README.md)

Pour avoir des boites de dialogue "A propos" standard dans les projets VCL et FireMonkey.

Ce dépôt de code contient un projet développé en langage Pascal Objet sous Delphi. Vous ne savez pas ce qu'est Dephi ni où le télécharger ? Vous en saurez plus [sur ce site web](https://delphi-resources.developpeur-pascal.fr/).

Les premières étapes de la création de ces composants ont été codées en direct sur [Twitch](https://www.twitch.tv/patrickpremartin). Des rediffusions sont disponibles à la demande sur [Serial Streameur](https://serialstreameur.fr/boite-de-dialogue-a-propos-composant-delphi-vcl-et-fmx.html).

Si vous êtes intéressé par la création de composants ou par le code de TOlfAboutDialog, consultez le [blog Developpeur Pascal](https://developpeur-pascal.fr/boite-de-dialogue-a-propos-de.html).

## Contenu du projet

Dans le dossier /packages vous trouverez les sources des paquets à compiler puis installer pour utiliser les composants TOlfAboutDialog dans vos projets VCL ou FireMonkey.

Dans le dossier /sources vous trouverez les sources des composants et leurs dépendances.

Dans le dossier /samples vous trouverez des projets de démo d'utilisation de ces composants comme composants ou en direct sous forme de code. Vous pouvez vous en inspirer pour vos projets.

Le fichier TODO.md contient les modifications prévues pour une version ultérieure.

Le dossier CHANGELOG contient les modifications faites sur les versions du projet. Si vous faites une mise à jour des composants, jetez y un oeil pour savoir ce qu'il s'est passé depuis votre précédente installation.

## Installation

Pour télécharger ce projet il est recommandé de passer par "git" mais vous pouvez aussi télécharger un ZIP directement depuis [son dépôt GitHub](https://github.com/DeveloppeurPascal/AboutDialog-Delphi-Component).

**Attention :** si le projet utilise des dépendances sous forme de sous modules ils seront absents du fichier ZIP. Vous devrez les télécharger à la main.

Les sources des paquets de composants se trouvent dans le dossier /packages. Vous y trouverez une version VCL et une version FireMonkey en fonction de votre version de l'environnement de développement RAD Studio ou Delphi.

Vous n'êtes pas obligés d'installer les deux versions. Si vous ne faites que des projets VCL n'installez que le paquet VCL. Si vous ne faites que des projets FireMonkey n'installez que le paquet FMX.

Les sources des composants se trouvent dans le dossier /sources. Si vous voulez juste les manipuler par code vous n'avez pas besoin d'installer les paquets. Utilisez simplement les fichiers sources dans vos projets.

Pour installer un paquet de composant :
- ouvrez le source du paquet dans l'IDE de Delphi ou RAD Studio
- désinstallez le paquet s'il était déjà installé (clic droit, puis "désinstaller" sur le paquet depuis le gestionnaire de projets)
- compilez le paquet
- installez le paquet (clic droit, puis "installer" sur le paquet depuis le gestionnaire de projets)
- ajoutez les sources des composants dans votre dossier de sources ou le chemin vers le dossier /sources dans le chemin de recherche par défaut dans les options de l'environnement (préférable) ou les options de vos projets (pour chaque projet)

## Dépendances

Ce dépôt de code dépend des dépôts suivants :

* aucune

## Comment demander une nouvelle fonctionnalité, signaler un bogue ou une faille de sécurité ?

Si vous voulez une réponse du propriétaire de ce dépôt la meilleure façon de procéder pour demander une nouvelle fonctionnalité ou signaler une anomalie est d'aller sur [le dépôt de code sur GitHub](https://github.com/DeveloppeurPascal/AboutDialog-Delphi-Component) et [d'ouvrir un ticket](https://github.com/DeveloppeurPascal/AboutDialog-Delphi-Component/issues).

Si vous avez trouvé une faille de sécurité n'en parlez pas en public avant qu'un correctif n'ait été déployé ou soit disponible. [Contactez l'auteur du dépôt en privé](https://developpeur-pascal.fr/nous-contacter.php) pour expliquer votre trouvaille.

Vous pouvez aussi cloner ce dépôt de code et participer à ses évolutions en soumettant vos modifications si vous le désirez. Lisez les explications dans le fichier [CONTRIBUTING.md](CONTRIBUTING.md).

## Modèle de licence double

Ce projet est distribué sous licence [AGPL 3.0 ou ultérieure] (https://choosealicense.com/licenses/agpl-3.0/).

Si vous voulez l'utiliser en totalité ou en partie dans vos projets mais ne voulez pas en partager les sources ou ne voulez pas distribuer votre projet sous la même licence, vous pouvez acheter le droit de l'utiliser sous la licence [Apache License 2.0](https://choosealicense.com/licenses/apache-2.0/) ou une licence dédiée ([contactez l'auteur](https://developpeur-pascal.fr/nous-contacter.php) pour discuter de vos besoins).

## Supportez ce projet et son auteur

Si vous trouvez ce dépôt de code utile et voulez le montrer, merci de faire une donation [à son auteur](https://github.com/DeveloppeurPascal). Ca aidera à maintenir le projet (codes sources et binaires).

Vous pouvez utiliser l'un de ces services :

* [GitHub Sponsors](https://github.com/sponsors/DeveloppeurPascal)
* [Liberapay](https://liberapay.com/PatrickPremartin)
* [Patreon](https://www.patreon.com/patrickpremartin)
* [Paypal](https://www.paypal.com/paypalme/patrickpremartin)

ou si vous parlez français vous pouvez [vous abonner à Zone Abo](https://zone-abo.fr/nos-abonnements.php) sur une base mensuelle ou annuelle et avoir en plus accès à de nombreuses ressources en ligne (vidéos et articles).
