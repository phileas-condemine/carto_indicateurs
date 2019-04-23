Bienvenue sur le Portail des Indicateurs de Santé.

# Objectif
Cette application s'adresse à tout utilisateur désireux d'indentifier des indicateurs de santé parmis les 18,885 indicateurs actuellement recensés.
Pour cela nous proposons deux critères d'exploration : 
- les mots clefs via la barre de recherche
- des catégories (dits tags). Ces catégories sont générées grâce à un procédé d'automatisation (machine learning), elles peuvent donc être fausses ! N'hésitez pas à faire remonter vos observations et remarques dans les issues de ce repo.

# To do
* column filters devraient ne contenir que les observations restantes, droplevels ?
* ajouter du contenu dans les modalDialog des valuebox.es
* ajuster les colonnes trop longues comme nom du producteur avec acronyme... remplacer par acronyme
* tooltip sur tableau par ligne https://datatables.net/forums/discussion/32240/how-to-implement-a-popup-tooltip-on-a-datatables-cell-that-displays-all-data
* élargir le side bar pour mieux voir la liste des tags
* proposer un tutoriel pour décrouvrir les tags selon 4 thématiques avec définitions et approche visuelle
* faire une vidéo de démonstration de navigation dans l'application


* chargement de l'app trop long
* bsToolTip pour sache qu'on peut cliquer sur valuebox
* bsToolTip pour ajouter les tags de l'indicateur aux filtres


- quand on arrive on veut juste un minitext ! On explique la vocation de l'appli, l'organisation d'1 ligne par indicateur. La table en bas c'est quoi ?
- doc modaldialog ajouter easyexit ie on quitte en cliquant dans la zone grise.
- Quand on clique sur valuebox il ne se passe rien... on veut une  réaction. par exemple si nb bases ou nb prod on affiche la liste des noms. s'il y en a trop on doit afficher qqch d'autre...
  * valuebox du producteur majoritaire => afficher la fiche du producteur 
  * valuebox du nombre de bases => afficher les noms des bases
  * valuebox du nombre de tags dans la recherche actuelle. => afficher les noms des tags
  * pour les autres valueboxes on affiche quoi ?
  * changer le curseur sur les lignes du tableau ? mettre une main avec doigt tendu ?
  * dans le modaldialog à côté des tags il faut préciser "ajouter le tag pour affiner ma recherche". Lorsqu'on clique sur la page, quitter automatiquement le modaldialog. supprimer cette fonctionnalité d'ajout de tag ? mettre dans le tableau directement ?

[A discuter] pourquoi des traits autour des filter="top" => contours du tableau
[A discuter] détourer (contraste de couleurs) toute la zone utile y compris les noms de colonnes
[A discuter] supprimer l'ajout des tags ? remplacer par le bouton de lien vers le site du producteur
[A discuter] dans le modaldialog mettre sur une column et pas 2 mais dans une table avec 2 colonnes [nom : valeur] et rétrecir le modaldialog ! 




# Stack technique 
Cette application est développée en R Shiny. 
Vous pouvez accéder au code source de l'application sur le dépôt <a href="https://github.com/phileas-condemine/carto_indicateurs">Github</a>.