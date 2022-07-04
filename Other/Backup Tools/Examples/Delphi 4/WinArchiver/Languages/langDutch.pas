unit langDutch;

interface

procedure SetLanguage;

implementation
uses unTranslation;

procedure SetLanguage;
begin
  // Misc strings
  AddStr(   1, 'Kies "Nieuw" voor nieuw of "Open" om een archief te openen' );
  AddStr(   2, 'OK' );
  AddStr(   3, 'Annuleer' );
  AddStr(   4, '&Help' );
  // unit fmAboutBox
  AddStr( 500, 'Over' );
  AddStr( 501, 'by Morgan Martinet (C)1998' );
  AddStr( 502, 'Deze componenten zijn freeware.' );
  AddStr( 503, 'Copyright (C) 1998 by NOMSSI NZALI Jacques H. C.' );
  AddStr( 504, 'pasZLib library:' );
  AddStr( 505, 'mmm@imaginet.fr of mmm@mcom.fr' );
  AddStr( 506, 'BlowFish Implementatie door Greg Carter, CRYPTOCard' );
  AddStr( 507, 'SFX-code door Oliver Buschjost' );
  AddStr( 508, 'Web site:' );
  // unit fmTiming
  AddStr( 600, 'Verstreken tijd :' );
  AddStr( 601, 'Overblijvende tijd :' );
  // unit fmMain
  AddStr( 700, 'Nieuw...' );
  AddStr( 701, 'Open...' );
  AddStr( 702, '&Toevoegen...' );
  AddStr( 703, 'T&erugzetten...' );
  AddStr( 704, '&Verwijder...' );
  AddStr( 705, '&Annuleer' );
  AddStr( 706, 'Nieuw archief' );
  AddStr( 707, 'Open archief' );
  AddStr( 708, 'Bestanden toevoegen...' );
  AddStr( 709, 'Bestanden terugzetten...' );
  AddStr( 710, 'Bestanden verwijderen...' );
  AddStr( 711, 'Archiefbestanden (*.mmm)|*.mmm|SFX-archieven (*.exe)|*.exe|Alle bestanden (*.*)|*.*' );
  AddStr( 712, 'Archiefbestanden (*.mmm)|*.mmm|Alle bestanden (*.*)|*.*' );
  AddStr( 713, 'Open een bestaand archief' );
  AddStr( 714, 'Maak een nieuw archief' );
  AddStr( 715, 'Open één segment van een archief' );
  AddStr( 718, '%d bestand(en), %s' );
  AddStr( 720, 'Het bestand "%s" bestaat al' );
  AddStr( 721, 'Wilt u dit archief resetten?' );
  AddStr( 722, 'Wilt u dit archief verwijderen?' );
  AddStr( 723, '%.0n Byte' );
  AddStr( 724, '%.0n Bytes' );
  AddStr( 725, '%.0n Kb' );
  AddStr( 726, '%.0n Mb' );
  AddStr( 727, '%d Bestand(en) geselecteerd, %s' );
  AddStr( 729, 'Niet beschikbaar' );
  AddStr( 730, 'hernoem het huidige archief naar:' );
  AddStr( 731, 'kon het archief niet hernoemen naar "%s"!' );
  AddStr( 732, 'SFX-configuratie' );
  AddStr( 733, 'Maak een zelf-uitpakkend (SFX) archief' );
  AddStr( 734, 'Maak' );
  AddStr( 735, 'Kon geen zelf-uitpakkend archief maken!' );
  AddStr( 736, 'Geef arc&hiefcommentaar' );
  AddStr( 737, 'Archiefcommentaar' );
  AddStr( 738, 'Een actie is bezig. Wacht tot het einde of klik op Annuleer.' );
  AddStr( 739, 'U heeft een bestand uitgevoerd.  Sluit dit af en probeer opnieuw.' );
  AddStr( 740, 'U moet eerst een archief openen of maken.' );
  AddStr( 741, 'Kon geen geassocieerd programma vinden voor %s' );
  AddStr( 742, 'Naam' );
  AddStr( 743, 'Datum' );
  AddStr( 744, 'Tijd' );
  AddStr( 745, 'Grootte' );
  AddStr( 746, 'Ratio' );
  AddStr( 747, 'Ingepakt' );
  AddStr( 748, 'Seg#' );
  AddStr( 749, 'Pad' );
  AddStr( 750, '&Bestand' );
  AddStr( 751, '&Acties' );
  AddStr( 752, '&Opties' );
  AddStr( 753, '&Help' );
  AddStr( 754, '&Nieuw archief...' );
  AddStr( 755, '&Open archief...' );
  AddStr( 756, 'Open &segment...' );
  AddStr( 757, '&Sluit archief' );
  AddStr( 758, '&Informatie...' );
  AddStr( 759, 'Her&noem Archief' );
  AddStr( 760, '&Reset archief' );
  AddStr( 761, 'Verwij&der archief' );
  AddStr( 762, '&Quit' );
  AddStr( 763, '&Toon...' );
  AddStr( 764, '&Selecteer alles' );
  AddStr( 765, '&Maak .EXE bestand' );
  AddStr( 766, 'Geef archiefcommentaar...' );
  AddStr( 767, '&SFX-configuratie...' );
  AddStr( 769, '&Over...' );
  AddStr( 770, 'Maak een nieuw archief' );
  AddStr( 771, 'Open een bestaand archief' );
  AddStr( 772, 'Open één segment van een archief' );
  AddStr( 773, 'Sluit dit archief' );
  AddStr( 774, 'Toon informatie over dit archief' );
  AddStr( 775, 'Hernoem het huidige archief...' );
  AddStr( 776, 'Reset de inhoud van het archief' );
  AddStr( 777, 'Verwijder archief' );
  AddStr( 778, 'Sluit de toepassing af' );
  AddStr( 781, 'Voeg bestanden toe aan het archief' );
  AddStr( 782, 'Bestanden terugzetten uit het archief' );
  AddStr( 783, 'Verwijder bestanden uit het archief' );
  AddStr( 784, 'Toon bestanden' );
  AddStr( 785, 'Selecteer alle bestanden van het archief' );
  AddStr( 786, 'Maak een zelf-uitpakkend archief' );
  AddStr( 787, 'Geef commentaar voor het huidige archief' );
  AddStr( 788, 'Verander de configuratie' );
  AddStr( 789, 'Verander de configuratie van het SFX-bestand' );
  AddStr( 790, 'Over deze applicatie' );
  AddStr( 798, '&Configuratie...' );
  AddStr( 799, '%s bestand' );
  AddStr( 800, 'Sluiten archief...' );
  AddStr( 801, 'Selecteer gee&n' );
  AddStr( 802, 'Select&ie omkeren' );
  AddStr( 803, 'Root' );
  AddStr( 804, 'Toon boomstructuur' );
  AddStr( 805, 'Grote iconen' );
  AddStr( 806, 'Kleine iconen' );
  AddStr( 807, 'Lijst' );
  AddStr( 808, 'Details' );
  AddStr( 809, 'Volledig uitklappen' );
  AddStr( 810, 'Volledig inklappen' );
  AddStr( 811, 'Bestandenlijst schoonmaken' );
  AddStr( 812, 'Creatie bestandenlijst' );
  AddStr( 813, 'Sorteren bestandenlijst' );
  AddStr( 814, 'Het archief %s bestaat niet!' );
  AddStr( 815, 'Controlee&r integriteit' );
  AddStr( 816, 'Controleer integriteit van het huidige archief' );
  AddStr( 817, '&Toon laatste uitvoer...' );
  AddStr( 818, 'Toon uitvoer van de laatste operatie' );
  AddStr( 819, 'Installeren' );
  AddStr( 820, 'Pak de inhoud uit, en voer daarna de installatie uit' );
  AddStr( 821, '&Lettertype...' );
  AddStr( 822, '&Sorteer' );
  AddStr( 823, '&Originele volgorde' );
  AddStr( 824, 'Vervander het huidige lettertype' );
  AddStr( 825, 'Selecteer een sorteervolgorde' );
  AddStr( 826, 'F&ilters...' );
  AddStr( 827, 'Laat u filters definiëren om toe te voegen bestanden te selecteren' );
  // unit fmAdd and fmAddDropedFiles
  AddStr( 900, 'Toevoegen uit' );
  AddStr( 901, 'Naam:' );
  AddStr( 902, 'Mappen' );
  AddStr( 903, 'Submappen opnemen' );
  AddStr( 904, 'Huidig pad ook opnemen' );
  AddStr( 905, 'Lege mappen opslaan' );
  AddStr( 906, 'Pad opslaan:' );
  AddStr( 907, 'Bestanden coderen?' );
  AddStr( 908, 'Compressieniveau:' );
  AddStr( 909, 'Toevoegen' );
  AddStr( 910, 'Geen'+#13+
               'Volledig'+#13+
               'Relatief' );
  AddStr( 911, 'Maximaal (langzaamst)'+#13+
               'Normaal'+#13+
               'Snel'+#13+
               'Supersnel'+#13+
               'Geen' );
  AddStr( 912, 'Toevoegen gedropte bestanden' );
  AddStr( 913, 'Toevoegen onderdelen' );
  AddStr( 914, 'Filter:' );
  AddStr( 915, 'toevoegen aan huidige map?' );
  AddStr( 916, 'Bestanden filteren?' );
  // unit fmConfiguration
  AddStr( 1000, 'Configuratie' );
  AddStr( 1001, 'Schijfoverbrugging' );
  AddStr( 1002, 'Creatie archief' );
  AddStr( 1003, 'Opties' );
  AddStr( 1004, 'Archief splitsen' );
  AddStr( 1005, 'Maximumgrootte van een archiefsegment:' );
  AddStr( 1006, '720 Kb'+#13+
                '1,44 Mb'+#13+
                'Overig (Kb):' );
  AddStr( 1007, 'Comprimeren archief' );
  AddStr( 1008, 'Coderen archief' );
  AddStr( 1009, 'Bestendig archief' );
  AddStr( 1010, 'Alleen-lezen' );
  AddStr( 1011, 'Creatie SFX-archief' );
  AddStr( 1014, 'Blokgrootte' );
  AddStr( 1015, 'Voorbehouden ruimte' );
  AddStr( 1016, 'Kb' );
  AddStr( 1017, 'Taal:' );
  AddStr( 1018, 'Automatisch'+#13+
                'Engels'+#13+
                'Frans'+#13+
                'Chinees'+#13+
                'Portugees'+#13+
                'Duits'+#13+
                'Italiaans'+#13+
                'Russisch'+#13+
                'Spaans'+#13+
                'Deens'+#13+
                'Nederlands'+#13+
                'Tsjechisch'
                );
  AddStr( 1019, 'Toon lege mappen' );
  AddStr( 1020, 'Toon boomstructuur' );
  // unit fmCreateFolder
  AddStr( 1100, 'Huidige map:' );
  AddStr( 1101, 'Naam:' );
  // unit fmDelete
  AddStr( 1200, 'Verwijder' );
  AddStr( 1201, 'Bestanden' );
  AddStr( 1202, 'Voll&edig Archief'+#13+
                'Ge&selecteerde bestanden'+#13+
                '&Bestanden:' );
  // unit fmEnterCryptKey
  AddStr( 1300, 'Systeembericht' );
  AddStr( 1301, 'Verberg wachtwoord?' );
  // unit fmExtract
  AddStr( 1400, 'Terugzetten' );
  AddStr( 1401, 'Terugzetten naar:' );
  AddStr( 1402, 'Bestanden' );
  AddStr( 1403, 'Ge&selecteerde bestanden'+#13+
                '&Alle bestanden'+#13+
                'Bes&tanden:' );
  AddStr( 1404, 'Gebr&uik mapnamen' );
  AddStr( 1405, 'Overschrijf bestaande bestanden'+#13+
                'Bestaande bestanden overslaan'+#13+
                'Bijwerken nieuwe bestanden'+#13+
                'Vraag bevestiging'+#13+
                'Zet alleen bestaande bestanden terug'+#13+
                'Bijwerken bestaande bestanden' );
  AddStr( 1406, 'Mappen / Stations' );
  AddStr( 1407, 'Nieuwe map...' );
  // unit fmHelpOnSFX
  AddStr( 1500, 'De volgende woorden mogen worden gebruikt in de "Opdrachtregel"- en'+#13+
                '"Standaardpad voor terugzetten"-velden:' );
  AddStr( 1501, 'zal worden vervangen door de tijdelijke directory'+#13+
                '(vaak ''C:\windows\temp'' of ''C:\win95\temp'' of ''C:\temp'')' );
  AddStr( 1502, 'zal worden vervangen door de Windowsdirectory'+#13+
                '(vaak ''C:\windows'' of ''C:\win95'')' );
  AddStr( 1503, 'zal worden vervangen door de Systeemdirectory'+#13+
                '(vaak ''C:\windows\system'' of ''C:\win95\system'')' );
  AddStr( 1504, 'zal worden vervangen door de Programmadirectory'+#13+
                '(vaak ''C:\Program Files'' [afhankelijk van de taal'+#13+
                'van de geïnstalleerde Windows])' );
  AddStr( 1505, 'zal worden vervangen door de directory waarnaar de '+#13+
                'bestanden werden teruggezet (alleen voor de "Opdrachtregel"- of'+#13+
                '"Argumenten"-velden)' );
  AddStr( 1506, 'Voorbeeld:' );
  AddStr( 1507, '<PF>MijnBedrijf\MijnSpullen' );
  // unit fmInformation
  AddStr( 1600, 'Pad:' );
  AddStr( 1601, 'Naam:' );
  AddStr( 1602, 'Bestandsgrootte:' );
  AddStr( 1603, 'Bestanden:' );
  AddStr( 1604, 'Comprimering:' );
  AddStr( 1605, 'Datum/Tijd:' );
  AddStr( 1606, 'Segment:' );
  AddStr( 1607, 'Kenmerken' );
  AddStr( 1608, 'Gecodeerd' );
  AddStr( 1609, 'Gecomprimeerd' );
  AddStr( 1610, 'Bestendig' );
  AddStr( 1611, 'Alleen-lezen' );
  AddStr( 1612, 'Laatste segment' );
  AddStr( 1613, 'Informatie' );
  // unit fmSFXComments
  AddStr( 1700, 'Commentaar' );
  AddStr( 1701, 'Commentaar tonen bij openen van SFX-archief' );
  AddStr( 1702, 'Commentaar tonen na het uitpakken van bestanden uit het SFX-archief' );
  AddStr( 1703, 'Commentaar verwijderen' );
  // unit fmSFXConfig
  AddStr( 1800, 'SFX-configuratie' );
  AddStr( 1801, 'Bestand uitvoeren na terugzetten?' );
  AddStr( 1802, 'Kiest de gebruiker bestanden om terug te zetten?' );
  AddStr( 1803, 'Kiest de gebruiker de overschrijfmodus?');
  AddStr( 1804, 'Weer te geven tekst:' );
  AddStr( 1805, 'Opdrachtregel:' );
  AddStr( 1806, 'Argumenten:' );
  AddStr( 1807, 'Standaardpad voor terugzetten:' );
  AddStr( 1808, 'Overschrijfmodus:' );
  AddStr( 1809, 'Commentaar...' );
  AddStr( 1810, 'Vraag bevestiging'+#13+
                'Overschrijf bestaande bestanden'+#13+
                'Sla bestaande bestanden over'+#13+
                'Overschrijf alleen als nieuwer'+#13+
                'Zet alleen bestaande bestanden terug'+#13+
                'Zet alleen bestanden terug als ze al bestaan en nieuwer zijn.' );
  AddStr( 1811, 'Is het de gebruiker toegestaan om het bestand niet uit te voeren?' );
  // unit fmTextViewer
  AddStr( 1900, 'Toon: %s' );
  AddStr( 1901, 'Kopiëren naar &klembord' );
  AddStr( 1902, '&Lettertype' );
  // unit fmView
  AddStr( 2000, 'Toon: %s' );
  AddStr( 2001, 'Gebruiken' );
  AddStr( 2002, 'Tonen' );
  AddStr( 2003, 'Ge&associeerd programma (%s)'+#13+
                '&Interne ASCII Tekstviewer' );
  // unit fmLastOutput
  AddStr( 2100, 'Toon laatste uitvoer' );
  // unit fmFilters
  AddStr( 2200, 'Filters' );
  AddStr( 2202, 'Toevoegen' );
  AddStr( 2203, 'Bewerk' );
  AddStr( 2204, 'Verwijder' );
  AddStr( 2205, 'Alles leegmaken' );
  AddStr( 2206, 'Filtersoort' );
{}{}  AddStr( 2207, 'Nodig'+#13+
                'Uitsluiten' );
  AddStr( 2208, 'Filter bewerken:' );
  AddStr( 2209, 'Nieuwe filter:' );
end;

end.
