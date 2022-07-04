FrameWork Extended


Le FrameWork Extended est un jeu de composants DELPHI et LAZARUS permettant de gérer des listes de données (TDBListView), des champs numériques (TExtDBNumEdit), d'affecter des couleurs à des champs (TextDBColorCombo et TExtColorCombo), de naviguer dans les données en pouvant déplacer les enregistrements et se rendre à un enregistrement donné (TExtDBNavigator). 

Les composants de champs Extended comme TFWDBEdit, TextNumEdit, TextColorCombo et TFWDBLookupCombo ainsi que le composant de grille TFWDBGrid permettent d'affecter une couleur d'écriture et de lecture seule, une couleur de Focus, et une couleur pour le Label associé pour les composants champs.

Les composants commençant pas TFW ne sont que des améliorations visuelles de composants surchargés.

Le composant de libellé TFWLabel est associé aux composants champs et possède une couleur de survol à la souris.

Le composant TExtFileCopy permet de copier d'un répertoire vers un autre en gardant ou pas la structure des sous-dossiers. Il peut être associé au composant TTraduceFile permettant de traduire la copie vers un autre type de fichier image.

Des composants boutons dans la palette FWButtons chargent une image particulière sans la sauver dans la conception afin de ne pas dupliquer des images sur chaque utilisation des boutons.

Composant  TExtDBNumEdit

Pour utiliser le composant  TExtDBNumEdit il faut :
Affecter un format au champ du composant
Affecter un Datasource possèdant un Dataset dans la propriété Datasource
Affecter DataField nommant le champ dans la base de données

Composant   TExtColorCombo

Le composant liste de couleurs est utilisable directement. Il peut être sauvé dans un fichier INI.

Composant   TExtDBColorCombo

Pour utiliser le composant  liste de couleurs pou un champ donné il faut :
Affecter un Datasource possèdant un Dataset dans la propriété Datasource
Affecter DataField nommant le champ dans la base de données

Composant  TDBListView

Le composant de liste liée aux données charge une liste au fur et à mesure du déplacement dans celle-ci.
Pour utiliser la liste il faut :
Pour utiliser le composant  liste de couleurs pou un champ donné il faut :
Affecter un Datasource possèdant un Dataset dans la propriété Datasource
Affecter DataFieldsDisplay nommant le ou les champs à afficher
Créer un Header avec la description de chaque champ


Composant  TExtDBNavigator

Le composant de navigation dans les données permet en plus de la navigation et de l'édition de rafraichir les données, de revenir à enregistrement précédemment sélectionné, de déplacer un enregistrement trié.
Pour utiliser la liste il faut :
Pour utiliser le composant  liste de couleurs pou un champ donné il faut :
Affecter un Datasource possèdant un Dataset dans la propriété Datasource
Eventuellement affecter la propriété SortField

Composant TOnFormInfoIni 

Le composant de sauvegarde des composants visuels dans le fichier INI permet de sauver des couleurs, des textes, des mémos, des nombres, la taille des panneaux, la position de la fenêtre,  etc.