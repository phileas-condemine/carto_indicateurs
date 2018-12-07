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
* chargement de l'app trop long
* bsToolTip pour qu'on comprenne mieux les inputs
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

# Etat d'avancement
- [V0.4] 
  * maintenir les 'en-têtes' des colonnes lorsqu'on scrolle => fixedheaders
  * bug avec la "recherche par mot clef"
  * persistence de la recherche par mot clef lorsqu'on change les colonnes https://dev.to/awwsmm/reactive-datatables-in-r-with-persistent-filters-l26
  * click sur valuebox https://stackoverflow.com/questions/34413137/use-href-infobox-as-actionbutton
  * [Inutile] leftsidebar menus déroulant avec tags par thématiques en utilisant shinydashboardPlus

  
- [V0.3]
  * aide et github directement sur des liens sur le header, on gagne un clic. on a envie de cliquer sur "référence" mais ce n'est pas un lien.
  * [OK] dans la sidebar ou dans le header les barres de recherche text+tag
  * [Non => dans la sidebar aussi] pagination à côté du choix previous-next
- [V0.2] Implémentation des idées remontées par Antoine Augusti : 
  * "Il semblerait qu’un clic sur les tags du bas filtre par ce tag mais tu as pas de retour de l’UI. Tu pourrais avoir un bandeau / une flash notif pour dire que tu as rajouté ce tag aux filtres"
  * "Quand j’arrive sur l’accueil et que je connais rien au domaine, je comprends pas du tout ce que c’est. Je sais pas si c’est normal. Sinon, quelques paragraphes d’explications feraient du bien"
  * "On peut filtrer les colonnes, mais quand tu connais pas les valeurs possibles c’est pas facile" => pas encore géré.
  * "Ce que tu décris est la procédure pour accéder aux données. Je pense que le mettre en titre aiderait à savoir que c’est ça. Le centrage rend la lecture difficile"
  * "Pourquoi des parenthèses pour le producteur majoritaire ?"
  * "Tu gères pas le cas où il n’y a pas besoin du pluriel (pas dramatique)"
  * "J’aimerais bien pouvoir changer le nombre d’entrées dans ton tableau + télécharger les données"
- [V0.1] Ajout de fonctionnalités grâce aux idées récoltées sur le slack GRrr. Merci à Romain Lesur et Paul-Antoine Chevalier.
  * filter="top" pour rechercher dans les colonnes
  * valueBoxes pour suivre quelques métriques intéressantes sur la recherche actuelle
- [V0.0] Publication le 05/10/2018 : Ouverture en alpha pour recenser (par crowd sourcing) des idées d'amélioration de l'expérience-utilisateur ou du design de l'application... ou autre !
  * Ajout d'un lien vers la documentation sur github avec le bouton d'aide en haut à droite.





# Stack technique 
Cette application est développée en R Shiny. 
Vous pouvez accéder au code source de l'application sur le dépôt <a href="https://github.com/phileas-condemine/carto_indicateurs">Github</a>.