Informationen zu Simons Komponentensammlung
-------------------------------------------
Herstellung und Vertrieb:

p.i.c.s.
Simon Reinhardt
Berner Str. 2
53117 Bonn

eMail		: reinhardt@picsoft.de
Internet	: http://www.picsoft.de

Version		: 1.53


Beschreibung:
-------------

Die Komponentensammlung wurde mit Delphi 5 (Delphi 1) erstellt und bearbeitet. Sie kann auch mit Delphi 6 und Delphi 7 kompiliert werden, es fehlen allerdings Funktionen, die erst mit diesen Delphiversionen hinzugekommen sind. Das Komponentenpackage (.dpk) wurde mit Delphi 3 erstellt und muß in allen höheren Versionen nach dem Öffnen umgewandelt werden. Die IDE erledigt das nach Rückfrage selbstätig.

Das ZIP-Archiv "Simons.zip" enthält alle Dateien für die Komponentensammlung. Nach dem Entpacken des Archivs (dabei die Pfadnamen wiederherstellen!) findet man die Dateien in drei 
Ordnern vor.

Der Ordner "Units" enthält alle Dateien der Komponentensammlung, die für die Installation in den Delphi-IDEs benötigt werden.

Im Ordner "Doc" finden sich Kurzbeschreibungen aller Komponenten im Textformat (*.txt), sowie eine 32Bit-und eine 16Bit-Windows-Hilfedatei (*.hlp).

Der Ordner "Demo" schließlich enthält eine mit Delphi 5 kompilierte Exedatei, die die Funktionen aller Komponeten demonstriert, sowie die entsprechenden Quelldateien für Delphi 1 (D1_Demo.dpr), Delphi 2 (D2_Demo.dpr) und Delphi 3 bis 5 (D5_Demo.dpr).

Der Abschnitt "Installation" dieser Datei beschreibt die Installation der Komponenten in den verschiedenen Delphi-IDEs.


Komponentenliste:
-----------------

TAudioTools, TButtonPanel, TCSVOpenDialog, TCSVSaveDialog, TEnhancedCheckbox, TEnhancedEdit, TExtOpenDialog, TExtSaveDialog, TIniList, TLEDButton, TLEDDisplay, TLEDMeter, TNumericEdit, TOvalButton, TScrewPanel, TSliderEdit, TSliderOpenDialog, TSliderSaveDialog, TSRCalendar, TSRCeckBox, TSRClock, TSRColorButton, TSRFontComboBox, TSRFontListBox, TSRGradient, TSRLabel, TSRValueEdit und TSRWavePlayer


Installation:
-------------

Die Komponentensammlung wurde mit Delphi 5 (Delphi 1) erstellt und bearbeitet. Sie kann auch mit höheren Delphiversionen kompiliert werden, es fehlen allerdings Funktionen, die erst mit diesen Delphiversionen hinzugekommen sind. Das Komponentenpackage "Simons.dpk" wurde mit Delphi 3 erstellt und muß in allen höheren Versionen nach dem Öffnen umgewandelt werden. Die IDE erledigt das nach Rückfrage selbstätig. Ab Delphi 2005 muß das Komponentenpackage "Simons_2005.dpk" kompiliert werden.

Das ZIP-Archiv "Simons.zip" enthält alle Dateien für die Komponentensammlung. Nach dem Entpacken des Archivs (dabei die Pfadnamen wiederherstellen!) findet man die Dateien in drei Ordnern vor.

Der Ordner "Units" enthält alle Dateien der Komponentensammlung, die für die Installation in den Delphi-IDEs benötigt werden.

Im Ordner "Doc" finden sich Kurzbeschreibungen aller Komponenten im Textformat (*.txt), sowie eine 32 Bit-und eine 16 Bit-Windows-Hilfedatei (*.hlp).

Der Ordner "Demo" schließlich enthält jeweils eine mit Delphi 1 und Delphi 5 kompilierte Exedatei, die die Funktionen aller Komponeten demonstriert, sowie die entsprechenden Quelldateien für Delphi 1 (D1_Demo.dpr), Delphi 2 (D2_Demo.dpr) und Delphi 3 bis 5 (D5_Demo.dpr).


In Delphi 1:
1. Alle Dateien aus dem Archivordner "Units" in ein Verzeichnis entpacken, das über den Suchpfad der Komponentenbibliothek zugänglich sein muß.
2. Menü Optionen: Komponente installieren
3. Schaltfläche Hinzufügen / Durchsuchen
4. Die Unit SimonReg.pas im Komponenten-Verzeichnis suchen und mit OK bestätigen.
5. Die Komponenten erscheinen in der Komponenten-Gruppe "Simon"

In Delphi 2:
1. Alle Dateien aus dem Archivordner "Units" in ein Verzeichnis entpacken, das über den Suchpfad der Komponentenbibliothek zugänglich sein muß.
2. Menü Komponente: installieren
3. Schaltfläche Hinzufügen / Durchsuchen
4. Die Unit SimonReg.pas im Komponenten-Verzeichnis suchen und mit OK bestätigen.
5. Die Komponenten erscheinen in der Komponenten-Gruppe "Simon"

Delphi 3 bis Delphi 8:
1. Alle Dateien aus dem Archivordner "Units" in ein Verzeichnis entpacken, das über den Suchpfad der Komponentenbibliothek zugänglich sein muß.
2. Menü Datei: öffnen.
3. Im Öffnendiaog als Dateityp "Delphi-Quell-Package (*.dpk) auswählen.
4. Das Package Simons.dpk im Komponenten-Verzeichnis suchen und öffnen.
   (Fehlermeldungen bei höheren Delphiversionen sind einfach zu ignorieren)
5. Das Package Simons.dpk kompilieren.
6. Die Komponenten erscheinen in der Komponenten-Gruppe "Simon"

Delphi 2005:
1. Alle Dateien aus dem Archivordner "Units" in ein Verzeichnis entpacken, das über den Suchpfad der Komponentenbibliothek zugänglich sein muß.
2. Menü Datei: öffnen.
3. Im Öffnendiaog als Dateityp "Delphi-Quell-Package (*.dpk) auswählen.
4. Das Package Simons_2005.dpk im Komponenten-Verzeichnis suchen und öffnen.
5. Das Package Simons_2005.dpk kompilieren.
6. Die Komponenten erscheinen in der Komponenten-Gruppe "Simon"


Vielen Dank an:
---------------

U. Conrad für die Ergänzung der FSingleLED-Eigenschaft von TLEDMeter.

Matthias Frey für die Korrektur der Advents-Berechnung in der Komponente TSRCalendar.

Peter Haas für die ConvertStrToDateTime und InitLocale-Funktionen

Andreas Hörstemeier für die Routinen aus der Unit TimeFunc aus seiner TMoon-Komponente, die ich für die TSRCalendar-Komponente genutzt habe. Andreas hat die Routinen aus dem Buch "Astronomical Algorithms" von Jean Meeus.

Torsten Hunnenberg für die Anpassung an Delphi 2005.

Christoph Kremer für die Die GetWeekOfYear-Funktion der Komponente TSRCalendar, die die Wochennummer nach DIN 1355 ermittelt.

Marco Lange für die Korrektur der OnMouseEnter- und OnMouseLeave-Ereignisse von TOvalButton. 

Edmund Matzke für die Korrektur der Schleswig-Holsteinischen Feiertage in TSRCalendar.

Markus Pinl für die Fehlerkorrektur in TSRColorButton und die Beisteuerung der BorderColor-Eigenschaft!

Jürgen Probst für die Erweiterung der TSRGradient-Komponente um neue Zeichnestile.

Brendan Rempel, der so nett war, die Komponente TRoundButton als Public Domain zu veröffentlichen. Auf Basis dieser Komponente habe ich TOvalButton entwickelt.

Robert Rossmair für seine rrColors-Unit!


Einsatzbereich und Lizenzbestimmungen:
--------------------------------------

Die Komponenten lassen sich mit allen bisher erschienenen Delphiversionen kompilieren. In älteren Delphiversionen sind aber u.U. nicht alle Eigenschaften und Ereignisse der Komponenten vorhanden. Die Komponenten TAudioTools, TSRClock und alle erweiterten Open- und Save-Dialoge existieren nur für die 32-Bit-Delphiversionen ab 2.00.

Diese Komponentensammlung ist Public Domain, das Urheberrecht liegt aber beim Autor. Fragen und Verbesserungsvorschläge sind immer willkommen. Sie darf frei benutzt und kopiert werden. Einzige Voraussetzung für die Weitergabe der Komponentensuite ist die vollständige Weitergabe aller dazugehörigen unveränderten Dateien. 



(C) 2005 Simon Reinhardt