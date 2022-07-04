unit langDanish;

interface

procedure SetLanguage;

implementation
uses unTranslation;

procedure SetLanguage;
begin
  // Misc strings
  AddStr(   1, 'Vælg "Ny" for at oprette eller "Åben" for at åbne et arkiv' );
  AddStr(   2, 'OK' );
  AddStr(   3, 'Annuller' );
  AddStr(   4, '&Hjælp' );
  // unit fmAboutBox
  AddStr( 500, 'Om' );
  AddStr( 501, 'by Morgan Martinet (C)1998' );
  AddStr( 502, 'These Components are Freeware.' );
  AddStr( 503, 'Copyright (C) 1998 by NOMSSI NZALI Jacques H. C.' );
  AddStr( 504, 'pasZLib bibliotek:' );
  AddStr( 505, 'mmm@imaginet.fr or mmm@mcom.fr' );
  AddStr( 506, 'BlowFish Implementering af Greg Carter, CRYPTOCard' );
  AddStr( 507, 'SFX code af Oliver Buschjost' );
  AddStr( 508, 'Web side:' );
  // unit fmTiming
  AddStr( 600, 'Brugt tid :' );
  AddStr( 601, 'Resterende tid :' );
  // unit fmMain
  AddStr( 700, 'Ny...' );
  AddStr( 701, 'Åben...' );
  AddStr( 702, '&Tilføj...' );
  AddStr( 703, '&Udtræk...' );
  AddStr( 704, '&Slet...' );
  AddStr( 705, '&Fortryd' );
  AddStr( 706, 'Nyt Arkiv' );
  AddStr( 707, 'Åben Arkiv' );
  AddStr( 708, 'Tilføj filer...' );
  AddStr( 709, 'Udtræk files...' );
  AddStr( 710, 'Slet filer...' );
  AddStr( 711, 'Arkiv filer (*.mmm)|*.mmm|SFX Arkiver (*.exe)|*.exe|Alle filer (*.*)|*.*' );
  AddStr( 712, 'Arkiv files (*.mmm)|*.mmm|Alle filer (*.*)|*.*' );
  AddStr( 713, 'Åben et eksisterende arkiv' );
  AddStr( 714, 'Lav et nyt arkiv' );
  AddStr( 715, 'Åben en del af at Arkiv' );
  AddStr( 718, '%d file(s), %s' );
  AddStr( 720, 'Filen "%s", eksistere allerede' );
  AddStr( 721, 'Ønsker du at nulstille dette arkiv ?' );
  AddStr( 722, 'Ønsker du at slette dette arkiv ?' );
  AddStr( 723, '%.0n Byte' );
  AddStr( 724, '%.0n Bytes' );
  AddStr( 725, '%.0n Kb' );
  AddStr( 726, '%.0n Mb' );
  AddStr( 727, 'valgte %d fil(er), %s' );
  AddStr( 729, 'Ikke tilgængelig' );
  AddStr( 730, 'Omdøb nuværende arkiv til:' );
  AddStr( 731, 'Kunne ikke omdøbe arkivet til "%s" !' );
  AddStr( 732, 'SFX Konfiguration ' );
  AddStr( 733, 'Lav et selvudpakkende arkiv' );
  AddStr( 734, 'Opret' );
  AddStr( 735, 'Kunne ikke lave et selvudpakkende arkiv  !' );
  AddStr( 736, 'Set Ark&iv kommentar' );
  AddStr( 737, 'Arkiv kommentar' );
  AddStr( 738, 'En operation er i gang. Vent til den er færdig eller Klik på Annuller.' );
  AddStr( 739, 'Du har kørt en fil. Afslut før du prøver igen.' );
  AddStr( 740, 'Du skan først åbne eller lave et arkiv.' );
  AddStr( 741, 'Kunne ikke finde det associerede program for %s' );
  AddStr( 742, 'Navn' );
  AddStr( 743, 'Dato' );
  AddStr( 744, 'Tid' );
  AddStr( 745, 'Størrelse' );
  AddStr( 746, 'Forhold' );
  AddStr( 747, 'Pakket' );
  AddStr( 748, 'Segment#' );
  AddStr( 749, 'Sti' );
  AddStr( 750, '&Fil' );
  AddStr( 751, '&Handlinger' );
  AddStr( 752, '&Optioner' );
  AddStr( 753, '&Hælp' );
  AddStr( 754, '&Nyt Arkiv...' );
  AddStr( 755, '&Åben Arkiv...' );
  AddStr( 756, 'Åben &segmentere...' );
  AddStr( 757, '&Luk Arkiv' );
  AddStr( 758, '&Information...' );
  AddStr( 759, 'Om&døb Arkiv' );
  AddStr( 760, '&Nulstil arkiv' );
  AddStr( 761, '&Slet arkiv' );
  AddStr( 762, '&Afslut' );
  AddStr( 763, '&Vis...' );
  AddStr( 764, '&Vælg alt' );
  AddStr( 765, '&Lav .EXE fil' );
  AddStr( 766, 'Set Arkiv kommentar...' );
  AddStr( 767, '&SFX Konfiguration...' );
  AddStr( 769, '&Om...' );
  AddStr( 770, 'Lav et nyt arkiv' );
  AddStr( 771, 'Åben et eksisterende arkiv' );
  AddStr( 772, 'Åben en del af at Arkiv' );
  AddStr( 773, 'Luk dette arkiv' );
  AddStr( 774, 'Vis informationer om dette arkiv' );
  AddStr( 775, 'Omdøb nuværende arkiv...' );
  AddStr( 776, 'Nulstil indholdet af arkivet' );
  AddStr( 777, 'Slet arkiv' );
  AddStr( 778, 'Afslut programmet' );
  AddStr( 781, 'Tilføj filer til arkivet' );
  AddStr( 782, 'Udtræk filer fra arkivet' );
  AddStr( 783, 'Slet filer fra arkivet' );
  AddStr( 784, 'Vis filer' );
  AddStr( 785, 'Vælg alle filer i arkivet' );
  AddStr( 786, 'Lav et selvudpakkende arkiv' );
  AddStr( 787, 'Definer en kommentar til nuværende arkiv' );
  AddStr( 788, 'Ændre Konfigurationen' );
  AddStr( 789, 'Ændre Konfigurationen af SFX fremstilling' );
  AddStr( 790, 'Om dette program' );
  AddStr( 798, '&Konfiguration...' );
  AddStr( 799, '%s Fil' );
  AddStr( 800, 'Lukker arkiv...' );
  AddStr( 801, 'Vælg &ingen' );
  AddStr( 802, '&Inverter valg' );
  AddStr( 803, 'Rod' );
  AddStr( 804, 'Træ oversigt' );
  AddStr( 805, 'Store ikoner' );
  AddStr( 806, 'Små ikoner' );
  AddStr( 807, 'List' );
  AddStr( 808, 'Detaljer' );
  AddStr( 809, 'Fuld udvidelse' );
  AddStr( 810, 'Fuld sammentrækning' );
  AddStr( 811, 'Nulstil fil list' );
  AddStr( 812, 'Bygger fil list' );
  AddStr( 813, 'Sortere files list' );
  AddStr( 814, 'Arkivet %s eksistere ikke !' );
  AddStr( 815, 'Chec&k integritet' );
  AddStr( 816, 'Check integritet af nuværende arkiv' );
  AddStr( 817, '&Vis sidste Output...' );
  AddStr( 818, 'Vis output af sidste operation' );
  AddStr( 819, 'Installere' );
  AddStr( 820, 'Udtrak indholdet og kør installations programmet' );
  AddStr( 821, '&Font...' );
  AddStr( 822, '&Sorter' );
  AddStr( 823, '&Original rækkefølge' );
  AddStr( 824, 'Ændre nuværende font' );
  AddStr( 825, 'Vælg sorterings rækkefølge' );
{*}  AddStr( 826, 'F&ilters...' );
{*}  AddStr( 827, 'Lets you define filters to select files to be added' );
  // unit fmAdd and fmAddDropedFiles
  AddStr( 900, 'Tilføj fra' );
  AddStr( 901, 'Navn :' );
  AddStr( 902, 'Foldere' );
  AddStr( 903, 'Resurse foldere' );
  AddStr( 904, 'Inkluder nuværende sti' );
  AddStr( 905, 'Gem tomme foldere' );
  AddStr( 906, 'Gem sti :' );
  AddStr( 907, 'Krypter filer ?' );
  AddStr( 908, 'Kompressions styrke:' );
  AddStr( 909, 'Tilføj' );
  AddStr( 910, 'Ingen'+#13+
               'Alt'+#13+
               'Relativ' );
  AddStr( 911, 'Maximum (langsomst)'+#13+
               'Normal'+#13+
               'Hurtig'+#13+
               'Meget hurtig'+#13+
               'Ingen' );
  AddStr( 912, 'Tilføj Droppede Filer' );
  AddStr( 913, 'Tilføj' );
  AddStr( 914, 'Filter :' );
  AddStr( 915, 'Tilføj til nuværende folder ?' );
{*}AddStr( 916, 'Filter files ?' );
  // unit fmConfiguration
  AddStr( 1000, 'Konfiguration' );
  AddStr( 1001, 'Disk overlapning' );
  AddStr( 1002, 'Arkiv creation' );
  AddStr( 1003, 'Optioner' );
  AddStr( 1004, 'Del arkiv' );
  AddStr( 1005, 'Maximum størrelse af et arkiv segment:' );
  AddStr( 1006, '720 Kb'+#13+
                '1,44 Mb'+#13+
                'Andre (Kb):' );
  AddStr( 1007, 'Komprimere arkiv' );
  AddStr( 1008, 'Krypter arkiv' );
  AddStr( 1009, 'Fast arkiv' );
  AddStr( 1010, 'Skrivebeskyttet' );
  AddStr( 1011, 'Lav SFX ariv' );
  AddStr( 1014, 'Blok størrelse' );
  AddStr( 1015, 'Reserveret plads' );
  AddStr( 1016, 'Kb' );
  AddStr( 1017, 'Sprog:' );
  AddStr( 1018, 'Automatisk'+#13+
                'English'+#13+
                'French'+#13+
                'Chinese'+#13+
                'Portuguese'+#13+
                'German'+#13+
                'Italian'+#13+
                'Russian'+#13+
	        'Spanish'+#13+
{*}             'Danish'+#13+
                'Dutch'+#13+
                'Czech'
                );
  AddStr( 1019, 'Vis tomme foldere' );
  AddStr( 1020, 'Vis Træ oversigt' );
  // unit fmCreateFolder
  AddStr( 1100, 'Nuværende folder:' );
  AddStr( 1101, 'Navn:' );
  // unit fmDelete
  AddStr( 1200, 'Slet' );
  AddStr( 1201, 'Filer' );
  AddStr( 1202, '&Hele arkivet'+#13+
                '&Valgte Filer'+#13+
                '&Files:' );
  // unit fmEnterCryptKey
  AddStr( 1300, 'System meddelelse' );
  AddStr( 1301, 'Gem password ?' );
  // unit fmExtract
  AddStr( 1400, 'Udtræk' );
  AddStr( 1401, 'Udtræk til:' );
  AddStr( 1402, 'Filer' );
  AddStr( 1403, '&Valgte filer'+#13+
                '&Alle filer'+#13+
                'F&iler:' );
  AddStr( 1404, '&Brug folder navne' );
  AddStr( 1405, 'Overskriv eksisterende filer'+#13+
                'Skip eksisterende filer'+#13+
                'Opdatere nye filey'+#13+
                'Spørg om bekræftelse'+#13+
                'Genskab kun eksisterende filer'+#13+
                'Opdater eksisterende filer' );
  AddStr( 1406, 'Folders / Drives' );
  AddStr( 1407, 'Ny Folder...' );
  // unit fmHelpOnSFX
  AddStr( 1500, 'Følgende nøgleord må bruges i "Kommando linjen" og'+#13+
                '"Default Extract Path" fields :' );
  AddStr( 1501, 'vil blive erstattet af Temp biblioteket'+#13+
                '(tit ''c:\windows\temp'' eller ''c:\win95\temp'' eller ''c:\temp'')' );
  AddStr( 1502, 'will be replaced with the Windows Directory'+#13+
                '(often ''c:\windows'' or ''c:\win95'')' );
  AddStr( 1503, 'vil blive erstattet af med system biblioteket'+#13+
                '(tit ''c:\windows\system'' eller ''c:\win95\system'')' );
  AddStr( 1504, 'vil blive erstattet af med ProgramFiles biblioteket'+#13+
                '(tit ''c:\Program Files'' [afhængig af sprog '+#13+
                'tit den installerede windows])' );
  AddStr( 1505, 'vil blive erstattet af med biblioteket hvor filerne'+#13+
                'var udpakket til (kun for "Kommando Linjen" eller'+#13+
                '"Argument" felterne)' );
  AddStr( 1506, 'Eksempel:' );
  AddStr( 1507, '<PF>MyCompany\MyStuff' );
  // unit fmInformation
  AddStr( 1600, 'Sti:' );
  AddStr( 1601, 'Navn:' );
  AddStr( 1602, 'Fil Størrelse:' );
  AddStr( 1603, 'Filer:' );
  AddStr( 1604, 'Kompression:' );
  AddStr( 1605, 'Dato/Tid:' );
  AddStr( 1606, 'Segment:' );
  AddStr( 1607, 'Attributter' );
  AddStr( 1608, 'Krypteret' );
  AddStr( 1609, 'Komprimeret' );
  AddStr( 1610, 'Solid' );
  AddStr( 1611, 'Skrivebeskyttet' );
  AddStr( 1612, 'Sidste segment' );
  AddStr( 1613, 'Information' );
  // unit fmSFXComments
  AddStr( 1700, 'Kommentar' );
  AddStr( 1701, 'Kommentar vist når SFX Arkivet åbnes' );
  AddStr( 1702, 'Kommentar vist efter udpakning af filerne gemt i SFX Arkivet' );
  AddStr( 1703, 'Slet Kommentar');
  // unit fmSFXConfig
  AddStr( 1800, 'SFX Konfiguration' );
  AddStr( 1801, 'Udfør fil efter udpakning ?' );
  AddStr( 1802, 'Bruger vælger filer som udpakkes ?' );
  AddStr( 1803, 'Bruger vælger overskrivnings tilstand ?');
  AddStr( 1804, 'Overskrift:' );
  AddStr( 1805, 'Command line:' );
  AddStr( 1806, 'Argument:' );
  AddStr( 1807, 'Standart udpaknings sti:' );
  AddStr( 1808, 'Overskrivnings tilstand:' );
  AddStr( 1809, 'Kommentar...' );
  AddStr( 1810, 'Spørg om bekræftelse'+#13+
                'Overskriv eksisterende filer'+#13+
                'Skip eksisterende filer'+#13+
                'Overskriv kun hvis nyere '+#13+
                'Gendan kun eksisterende filer'+#13+
                'Udpak kun filer hvis de allerede eksistere og er nyere' );
  AddStr( 1811, 'Brugeren tilod ikke at køre filen ?' );
  // unit fmTextViewer
  AddStr( 1900, 'Vis: %s' );
  AddStr( 1901, '&Clipboard kopier' );
  AddStr( 1902, '&Font' );
  // unit fmView
  AddStr( 2000, 'Vis : %s' );
  AddStr( 2001, 'Bruger' );
  AddStr( 2002, 'Vis' );
  AddStr( 2003, '&Associeret program (%s)'+#13+
                '&Intern ASCII tekst viser' );
  // unit fmLastOutput
  AddStr( 2100, 'Vis sidste output' );
  // unit fmFilters
{*}  AddStr( 2200, 'Filters' );
{*}  AddStr( 2202, 'Add' );
{*}  AddStr( 2203, 'Edit' );
{*}  AddStr( 2204, 'Delete' );
{*}  AddStr( 2205, 'Clear all' );
{*}  AddStr( 2206, 'Kind of filter' );
{*}  AddStr( 2207, 'Require'+#13+
{*}                'Exclude' );
{*}  AddStr( 2208, 'Edit filter:' );
{*}  AddStr( 2209, 'New filter:' );
end;

end.
