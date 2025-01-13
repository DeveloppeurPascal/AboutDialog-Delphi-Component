# Boite de dialogue "à propos" sous forme de composant Delphi

[This page in English.](README.md)

Pour avoir des boites de dialogue "A propos" standard dans les projets VCL et FireMonkey.

Ce dépôt de code contient un projet développé en langage Pascal Objet sous Delphi. Vous ne savez pas ce qu'est Dephi ni où le télécharger ? Vous en saurez plus [sur ce site web](https://delphi-resources.developpeur-pascal.fr/).

Les premières étapes de la création de ces composants ont été codées en direct sur [Twitch](https://www.twitch.tv/patrickpremartin). Des rediffusions sont disponibles à la demande sur [Serial Streameur](https://serialstreameur.fr/boite-de-dialogue-a-propos-composant-delphi-vcl-et-fmx.html).

Si vous êtes intéressé par la création de composants ou par le code de TOlfAboutDialog, consultez le [blog Developpeur Pascal](https://developpeur-pascal.fr/boite-de-dialogue-a-propos-de.html).

## Présentations et conférences

### Twitch

Suivez mes streams de développement de logiciels, jeux vidéo, applications mobiles et sites web sur [ma chaîne Twitch](https://www.twitch.tv/patrickpremartin) ou en rediffusion sur [Serial Streameur](https://serialstreameur.fr/boite-de-dialogue-a-propos-composant-delphi-vcl-et-fmx.html) la plupart du temps en français.

## Contenu du projet

Dans le dossier /packages vous trouverez les sources des paquets à compiler puis installer pour utiliser les composants TOlfAboutDialog dans vos projets VCL ou FireMonkey.

Dans le dossier /src vous trouverez les sources des composants et leurs dépendances.

Dans le dossier /samples vous trouverez des projets de démo d'utilisation de ces composants comme composants ou en direct sous forme de code. Vous pouvez vous en inspirer pour vos projets.

Le dossier CHANGELOG contient les modifications faites sur les versions du projet. Si vous faites une mise à jour des composants, jetez y un oeil pour savoir ce qu'il s'est passé depuis votre précédente installation.

## Installation

Pour télécharger ce projet il est recommandé de passer par "git" mais vous pouvez aussi télécharger un ZIP directement depuis [son dépôt GitHub](https://github.com/DeveloppeurPascal/AboutDialog-Delphi-Component).

**Attention :** si le projet utilise des dépendances sous forme de sous modules ils seront absents du fichier ZIP. Vous devrez les télécharger à la main.

Les sources des paquets de composants se trouvent dans le dossier /packages. Vous y trouverez une version VCL et une version FireMonkey en fonction de votre version de l'environnement de développement RAD Studio ou Delphi.

Vous n'êtes pas obligés d'installer les deux versions. Si vous ne faites que des projets VCL n'installez que le paquet VCL. Si vous ne faites que des projets FireMonkey n'installez que le paquet FMX.

Les sources des composants se trouvent dans le dossier /src. Si vous voulez juste les manipuler par code vous n'avez pas besoin d'installer les paquets. Utilisez simplement les fichiers sources dans vos projets.

Pour installer un paquet de composant :
- ouvrez le source du paquet dans l'IDE de Delphi ou RAD Studio
- désinstallez le paquet s'il était déjà installé (clic droit, puis "désinstaller" sur le paquet depuis le gestionnaire de projets)
- compilez le paquet
- installez le paquet (clic droit, puis "installer" sur le paquet depuis le gestionnaire de projets)
- ajoutez les sources des composants dans votre dossier de sources ou le chemin vers le dossier /src dans le chemin de recherche par défaut dans les options de l'environnement (préférable) ou les options de vos projets (pour chaque projet)

## Dépendances

Ce dépôt de code dépend des dépôts suivants :

* [DeveloppeurPascal/librairies](https://github.com/DeveloppeurPascal/librairies) est utlisé par les projets de démo et doit être présent dans le dossier ./samples/lib-externes/librairies

## Compatibilité

En tant que [MVP Embarcadero](https://www.embarcadero.com/resources/partners/mvp-directory) je bénéficie dès qu'elles sortent des dernières versions de [Delphi](https://www.embarcadero.com/products/delphi) et [C++ Builder](https://www.embarcadero.com/products/cbuilder) dans [RAD Studio](https://www.embarcadero.com/products/rad-studio). C'est donc dans ces versions que je travaille.

Normalement mes librairies et composants doivent aussi fonctionner au moins sur la version en cours de [Delphi Community Edition](https://www.embarcadero.com/products/delphi/starter).

Aucune garantie de compatibilité avec des versions antérieures n'est fournie même si je m'efforce de faire du code propre et ne pas trop utiliser les nouvelles façons d'écrire dedans (type inference, inline var et multilines strings).

Si vous détectez des anomalies sur des versions antérieures n'hésitez pas à [les rapporter](https://github.com/DeveloppeurPascal/AboutDialog-Delphi-Component/issues) pour que je teste et tente de corriger ou fournir un contournement.

## Licence d'utilisation de ce dépôt de code et de son contenu

Ces codes sources sont distribués sous licence [AGPL 3.0 ou ultérieure](https://choosealicense.com/licenses/agpl-3.0/).

Vous êtes globalement libre d'utiliser le contenu de ce dépôt de code n'importe où à condition :
* d'en faire mention dans vos projets
* de diffuser les modifications apportées aux fichiers fournis dans ce projet sous licence AGPL (en y laissant les mentions de copyright d'origine (auteur, lien vers ce dépôt, licence) obligatoirement complétées par les vôtres)
* de diffuser les codes sources de vos créations sous licence AGPL

Si cette licence ne convient pas à vos besoins vous pouvez acheter un droit d'utilisation de ce projet sous la licence [Apache License 2.0](https://choosealicense.com/licenses/apache-2.0/) ou une licence commerciale dédiée ([contactez l'auteur](https://developpeur-pascal.fr/nous-contacter.php) pour discuter de vos besoins).

Ces codes sources sont fournis en l'état sans garantie d'aucune sorte.

Certains éléments inclus dans ce dépôt peuvent dépendre de droits d'utilisation de tiers (images, sons, ...). Ils ne sont pas réutilisables dans vos projets sauf mention contraire.

## Comment demander une nouvelle fonctionnalité, signaler un bogue ou une faille de sécurité ?

Si vous voulez une réponse du propriétaire de ce dépôt la meilleure façon de procéder pour demander une nouvelle fonctionnalité ou signaler une anomalie est d'aller sur [le dépôt de code sur GitHub](https://github.com/DeveloppeurPascal/AboutDialog-Delphi-Component) et [d'ouvrir un ticket](https://github.com/DeveloppeurPascal/AboutDialog-Delphi-Component/issues).

Si vous avez trouvé une faille de sécurité n'en parlez pas en public avant qu'un correctif n'ait été déployé ou soit disponible. [Contactez l'auteur du dépôt en privé](https://developpeur-pascal.fr/nous-contacter.php) pour expliquer votre trouvaille.

Vous pouvez aussi cloner ce dépôt de code et participer à ses évolutions en soumettant vos modifications si vous le désirez. Lisez les explications dans le fichier [CONTRIBUTING.md](CONTRIBUTING.md).

## Supportez ce projet et son auteur

Si vous trouvez ce dépôt de code utile et voulez le montrer, merci de faire une donation [à son auteur](https://github.com/DeveloppeurPascal). Ca aidera à maintenir le projet (codes sources et binaires).

Vous pouvez utiliser l'un de ces services :

* [GitHub Sponsors](https://github.com/sponsors/DeveloppeurPascal)
* Ko-fi [en français](https://ko-fi.com/patrick_premartin_fr) ou [en anglais](https://ko-fi.com/patrick_premartin_en)
* [Patreon](https://www.patreon.com/patrickpremartin)
* [Liberapay](https://liberapay.com/PatrickPremartin)
* [Paypal](https://www.paypal.com/paypalme/patrickpremartin)

ou si vous parlez français vous pouvez [vous abonner à Zone Abo](https://zone-abo.fr/nos-abonnements.php) sur une base mensuelle ou annuelle et avoir en plus accès à de nombreuses ressources en ligne (vidéos et articles).
