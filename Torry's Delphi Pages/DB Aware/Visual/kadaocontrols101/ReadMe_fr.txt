KADao Controls README
---------------------

Contenu:
 1. Description
 2. Installation
 3. Garantie
 4. Copyrigh
 5. Traduction


1 - DESCRIPTTION
----------------
KADao Controls est un pack de composants GRATUIT permettant d'ameliorer 
le travail avec KADao.
KADao Controls ne peut fonctionner que si la version 7.0 ou ulterieure
de KADAO est installee.
KADao Controls comprend les composants suivants :
  KADaoDBGrid
  KADaoSortByButton
  KADaoFilterByButton
  KADaoFindButton
  KADaoSeekButton
  KADaoSelectIndexButton
  KADaoCVFButton
  KADaoDateTime
  KADaoExportButton
  KADaoSearch

*************************************************************************
KADaoDBGrid
-----------
KADaoDBGrid est un descendant standard de DBGrid. Vous pouvez cependant 
le rendre descendant de votre dbGrid favori en suivant la procedure suivante :
Ouvrez le fichier KADaoDBGrid.pas et modifiez la ligne
TKADaoDBGrid = class(TDBGrid)
en
TKADaoDBGrid = class(MYFavoriteDBGrid)

KADaoDBGrid a un menu special qui lui est attache. Cliquez simplement sur
la fleche rouge situee dans le coin supérieur gauche de la grille pour faire 
apparaitre ce menu.

Le menu contient les commandes suivantes :
- Select Index
Permet a l'utilisateur de changer l'index courant (KADaoTable.TableType 
doit etre StandartTable).
- Sort
Permet a l'utilisateur de trier la table (KADaoTable.TableType doit etre
DynasetTable).
- Filter
Permet a l'utilisateur de filtrer la table (KADaoTable.TableType doit etre
DynasetTable) 
- Find
Permet a l'utilisateur de chercher des donnees specifiques 
(KADaoTable.TableType doit etre DynasetTable).
- Seek
Permet a l'utilisateur de chercher des donnees specifiques 
(KADaoTable.TableType doit etre StandartTable).
- Quick Find
Permet a l'utilisateur de chercher des donnees specifiques basees sur la 
colonne actuellement selectionnee dans la grille (KADaoTable.TableType doit 
etre DynasetTable).
- Quick Seek
Permet a l'utilisateur de chercher des donnees specifiques basees sur la 
colonne actuellement selectionnee dans la grille (KADaoTable.TableType doit 
etre StandartTable) 
- Show Memos
Montre le contenu du Memo dans la grille
- Auto Size Columns
Redimensionne toutes les colonnes en ajustant au mieux la largeur de chacune
(si la table est volumineuse et entierement calculee, ne pas utiliser).
- Fast Lookup
Positionne la propriete CacheLookups a True/False.
- Frame Index Fields
Cree une bordure rouge pour les champs faisant partie d'un index (ne 
fonctionne que si un index est selectionne).
- Choose Visible Fields
Permet a l'utilisateur de selectionner les champs a visualiser dans la grille.
- Save as
Permet l'export au format Excel, Paradox, DBF, TXT, HTML etc.
- Select All
Permet de selectionner tous les enregistrements dans la grille.
- DeSelect All	
Permet de deselectionner tous les enregistrements actuellement selectionnes 
dans la grille.
- Cut
Coupe les enregistrements dans le presse-papier.
- Copy
Copie les enregistrements dans le presse-papier.
- Paste
Colle les enregistrements depuis le presse-papier (format MSOffice entierement
supporte).
- Office-compatible Clipboard
Active/desactive le format MSOffice pour le presse-papier.

Proprietes specifiques
----------------------
- Property ShowGridMenu: Boolean
Active / desactive la possibilite d'utiliser le menu attache.

Menu
----
- Property MenuTitles: TStringList 
Contient les titres des menus. Vous pouvez changer le titre des menus simplement
en changeant la propriete texte de cette propriete. Si vous placez le caractere
"!" au debut du titre, le menu sera invisible.
- Property FrameIndexFields: Boolean
Enables/disables ability to irame index fields 
- Property AutoSizeColuns: Boolean     
Si True, redimensionne toutes les colonnes avec un ajustage optimise, si false 
restaure les valeurs par defaut de DBGrid.
- Property FindFieldsOnPaste: Boolean
Si True tous les champs a coller sont cherches dans la table cible, sinon le
collage est base sur l'ordre des colonnes.
- Poperty OfficeClipboardFormat: Boolean
Active / desactive le format MSOffice pour le presse-papier.

********************************************************************************
KADaoSortByButton
KADaoFilterByButton
KADaoFindButton
KADaoSeekButton
KADaoSelectIndexButton
KADaoCVFButton
KADaoDateTime
KADaoExportButton
---------------------
Tous ces bouttons sont attaches a la meme function dans la grille KADaoDBGrid.
Affectez simplement a la propriete DataSource une source de donnees attachee a
une table kadao.
Chaque boutton a une propriete CallBefore. Si cette propriete est a true, la 
fonction user-attached du boutton est appelee en premier lieu (avant celle 
codee en dur). Si cette propriete est a false, la fonction codee en dur est 
executee avant la fonction user-attached.
Vous pouvez parametrer la propriete Visible du boutton a false. Si vous 
souhaitez utiliser les fonctionnalites de ce boutton alors qu'il n'est pas 
visible, appelez simplement la fonction XXXButton.Click dans votre code.


KADaoDateTime
-------------
Composant specifique pour effectuer des conversions DateTime.
Affecter la propriete DateTimeString a une chaine de caracteres et vous 
obtiendrez
1. Date as TDate
2. DateTime as TDateTime
3. SQLString - utile pour inclure du texte dans une requete SQL
4. SQLField  - utile pour inclure du texte dans une requete SQL

*****************************************************************************

KADaoSearch
-----------
KADaoSearch est une fonction prete a l'emploi permettant des recherches basee
sur un champ unique.
Affectez simplement la propriete DataSource a une source de donnees attachee 
a une table kadao.
Dans la version 2.0, une nouvelle propriete IncrementalSearch a ete ajoutee.
A true, la recherche est effectuee au fur et a mesure de votre saisie.


2 - INSTALLATION
----------------
Ouvrir KADaoControls.dpk et cliquez sur le boutton Install. C'est tout.

Desole de n'assurer aucun support par E-mail. Ces composants sont vraiment 
tres faciles a utiliser.
Je suis desormais tres occupe.
Les reports de bugs sont toujours les bienvenus :-)
LONGUE VIE AU SOURCE LIBRE !


3 - Garantie
------------
COMPONENTS ARE SUPPLIED "AS IS" WITHOUT WARRANTY OF ANY KIND. THE AUTHOR
DISCLAIMS ALL WARRANTIES, EXPRESSED OR IMPLIED, INCLUDING, WITHOUT LIMITATION,
THE WARRANTIES OF MERCHANTABILITY AND OF FITNESS FOR ANY PURPOSE. THE AUTHOR
ASSUMES NO LIABILITY FOR DAMAGES, DIRECT OR CONSEQUENTIAL, WHICH MAY RESULT
FROM THE USE OF COMPONENTS.
USE THIS COMPONENTS AT YOUR OWN RISK


4 - COPYRIGHT
-------------
Copyrigh (c) 2001-2002 By KirilAntonov, Sofia, Bulgaria. All Rights Reserved.
Sites : 
- delphi.pari.bg
- www.delphi.pari.bg
- www.kadao.8m.com
- www.delphiwarrior.freeservers.com


5 - Traduction du ficher README.TXT
-----------------------------------
La traduction de ce fichier en francais a ete assuree par Remy Mallard
remy.mallard@wanadoo.fr

