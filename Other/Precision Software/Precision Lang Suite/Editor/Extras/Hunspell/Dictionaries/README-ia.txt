Iste dictionario orthographic pro MySpell e OpenOffice.org per Ove Hanebring 
es publicate sub le licentia de GNU LGPL.

Iste obra se basa sur le dictionario pro Ispell, elaborate per Peter Kleiweg. 
Le base de parolas es le mesme: "Interlingua-English Dictionary" de IALA + le 
"Addenda al lista de martio 1997 del vocabulos de Interlingua" de Piet Cleij,
ma le regulas de derivation (le file "ia.aff") ha essite completemente revidite.

Commentarios e suggestiones es benvenite a <ove.hanebring@comhem.se>


Instructiones de installation (Windows)
=======================================

OpenOffice.org 1.0.1 non regognosce interlingua como un lingua, dunque tu debe 
installar lo sub un altere nomine. Personalmente, io lo appella "italiano", ma 
si tu anque scribe textos in ille lingua, tu pote usar qualcunque altere lingua 
que es supportate per OpenOffice.org.


1. Copia ia.dic e ia.aff a C:\Program Files\OpenOffice.org1.0.1\share\dict\ooo


2. Redige le file dictionary.lst, que se trova in le mesme loco, e adde iste 
linea:
DICT it IT ia

Isto significa que le files ia.dic/.aff es usate pro le lingua "italiano".
Si tu vole appellar lo "svedese" in loco de "italiano" le line debe esser:
DICT sv SE ia


3. (Optional) Si tu desira interlingua como lingua standard pro nove documentos:
3.a Lancea OpenOffice.org, e selige Tools/Options...
3.b Selige Language Settings/Languages in le parte sinistre del cassa de dialogo.
3.c Sub "Default Languages for documents", selige "Italian (Italy)".
