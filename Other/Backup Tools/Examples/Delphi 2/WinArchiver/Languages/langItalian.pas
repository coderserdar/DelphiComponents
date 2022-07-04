unit langItalian;

interface

procedure SetLanguage;

implementation
uses unTranslation;

procedure SetLanguage;
begin
  // Misc strings
  AddStr(   1, 'Scegliere "Nuovo" per creare oppure "Apri" per aprire un archivio' );
  AddStr(   2, 'OK' );
  AddStr(   3, 'Cancel' );
  AddStr(   4, '&Help' );
  // unit fmAboutBox
  AddStr( 500, 'About' );
  AddStr( 501, 'by Morgan Martinet (C)1998' );
  AddStr( 502, 'Questi componenti sono Freeware.' );
  AddStr( 503, 'Copyright (C) 1998 by NOMSSI NZALI Jacques H. C.' );
  AddStr( 504, 'pasZLib library:' );
  AddStr( 505, 'mmm@imaginet.fr or mmm@mcom.fr' );
  AddStr( 506, 'BlowFish Implementation provided by Greg Carter, CRYPTOCard' );
  AddStr( 507, 'SFX code by Oliver Buschjost' );
  AddStr( 508, 'Web site:' );
  // unit fmTiming
  AddStr( 600, 'Tempo trascorso :' );
  AddStr( 601, 'Tempo rimanente :' );
  // unit fmMain
  AddStr( 700, 'Nuovo...' );
  AddStr( 701, 'Apri...' );
  AddStr( 702, '&Aggiungi...' );
  AddStr( 703, '&Estrazione...' );
  AddStr( 704, '&Cancella...' );
  AddStr( 705, '&Abort' );
  AddStr( 706, 'Nuovo Archivio' );
  AddStr( 707, 'Apri Archivio' );
  AddStr( 708, 'Aggiungi files...' );
  AddStr( 709, 'Estrai files...' );
  AddStr( 710, 'Cancella files...' );
  AddStr( 711, 'Archivia files (*.mmm)|*.mmm|SFX Archives (*.exe)|*.exe|Tutti i files (*.*)|*.*' );
  AddStr( 712, 'Archivia files (*.mmm)|*.mmm|Tutti i files (*.*)|*.*' );
  AddStr( 713, 'Apri un archivio esistente' );
  AddStr( 714, 'Crea un nuovo archivio' );
  AddStr( 715, 'Apri un segmento di un archivio' );
  AddStr( 718, '%d file(s), %s' );
  AddStr( 720, 'Il file "%s", esiste già' );
  AddStr( 721, 'Vuoi resettare questo archivio ?' );
  AddStr( 722, 'Vuoi cancellare questo archivio ?' );
  AddStr( 723, '%.0n Byte' );
  AddStr( 724, '%.0n Bytes' );
  AddStr( 725, '%.0n Kb' );
  AddStr( 726, '%.0n Mb' );
  AddStr( 727, 'selezionato %d file(s), %s' );
  AddStr( 729, 'Non disponibile' );
  AddStr( 730, 'Rinominare l''archivio corrente in:' );
  AddStr( 731, 'Non si riesce a rinominare l''arhcivio in "%s" !' );
  AddStr( 732, 'SFX Configurazione' );
  AddStr( 733, 'Costruisci un archivio di auto-estrazione' );
  AddStr( 734, 'Costruisci' );
  AddStr( 735, 'Non si riesce a costruire un archivio di auto-estrazione !' );

  AddStr( 736, 'Commento dell''archivio' );
  AddStr( 737, 'Commento dell''archivio' );
  AddStr( 738, 'Un''operazione è in corso. Attendere fino alla fine o cliccare su Abort.' );
  AddStr( 739, 'Hai eseguito un file. Terminare e riprovare di nuovo.' );
  AddStr( 740, 'Devi prima aprire o creare un archivio.' );
  AddStr( 741, 'Non si riesce a trovare il programma associato a %s' );
  AddStr( 742, 'Nome' );
  AddStr( 743, 'Data' );
  AddStr( 744, 'Ora' );
  AddStr( 745, 'Ampiezza' );
  AddStr( 746, 'Ratio' );
  AddStr( 747, 'Compresso' );
  AddStr( 748, 'Seg#' );
  AddStr( 749, 'Path' );
  AddStr( 750, '&File' );
  AddStr( 751, '&Azioni' );
  AddStr( 752, '&Opzioni' );
  AddStr( 753, '&Help' );
  AddStr( 754, '&Nuovo archivio...' );
  AddStr( 755, '&Apri archivio...' );
  AddStr( 756, 'Apri &segmento...' );
  AddStr( 757, '&Chiudi archivio' );
  AddStr( 758, '&Informazioni...' );
  AddStr( 759, 'Ri&nomina Archivio' );
  AddStr( 760, '&Reset archivio' );
  AddStr( 761, '&Cancella archivio' );
  AddStr( 762, '&Quit' );
  AddStr( 763, '&Vsualizza...' );
  AddStr( 764, '&Seleziona tutti' );
  AddStr( 765, '&Crea .EXE file' );
  AddStr( 766, 'Commento archivio...' );
  AddStr( 767, '&SFX Configurazione...' );
  AddStr( 769, '&About...' );
  AddStr( 770, 'Crea un nuovo archivio' );
  AddStr( 771, 'Apri un archivio esistente' );
  AddStr( 772, 'Apri un segmento di un archivio' );
  AddStr( 773, 'Chiude questo archivio' );
  AddStr( 774, 'Visualizza informazioni su questo archivio' );
  AddStr( 775, 'Rinomina l''archivio corrente...' );
  AddStr( 776, 'Reset del contenuto dell''archivio' );
  AddStr( 777, 'Cancella archivio' );
  AddStr( 778, 'Esci dall''applicazione' );
  AddStr( 781, 'Aggiungi files all''archivio' );
  AddStr( 782, 'Estrai files dall''archivio' );
  AddStr( 783, 'Cancella files dall''archivio' );
  AddStr( 784, 'Visualizza files' );
  AddStr( 785, 'Seleziona tutti i files dall''archivio' );
  AddStr( 786, 'Costruisci un archivio autoestraente' );
  AddStr( 787, 'Definisci un commento per l''archivio corrente' );
  AddStr( 788, 'Cambia la configurazione' );
  AddStr( 789, 'Cambia la configurazione della creazione SFX' );
  AddStr( 790, 'Riguardo questa applicazione' );
  AddStr( 798, '&Configurazione...' );
  AddStr( 799, '%s File' );
  AddStr( 800, 'Chiusura dell''archivio...' );
  AddStr( 801, 'Seleziona &nessuno' );
  AddStr( 802, '&Inverti selezione' );
  AddStr( 803, 'Root' );
  AddStr( 804, 'Vista ad albero' );
  AddStr( 805, 'Icone grandi' );
  AddStr( 806, 'Icone piccole' );
  AddStr( 807, 'Elenco' );
  AddStr( 808, 'Dettaglio' );
  AddStr( 809, 'Espansione completa' );
  AddStr( 810, 'Collassamento pieno' );
  AddStr( 811, 'Pulizia elenco files' );
  AddStr( 812, 'Costruzione elenco files' );
  AddStr( 813, 'Ordinamento elenco files' );
  AddStr( 814, 'L''archivio %s non esiste !' );
  AddStr( 815, 'Chec&k integrità' );
  AddStr( 816, 'Check integrità dell''archivio corrente' );
  AddStr( 817, '&Visualizza l''ultimo Output...' );
  AddStr( 818, 'Visualizza l''output dell''ultima operazione' );
  AddStr( 819, 'Installazione' );
  AddStr( 820, 'Estrazione del contenuto ed esecuzione del programma di installazione' );
  AddStr( 821, '&Font...' );
  AddStr( 822, '&Ordinamento' );
  AddStr( 823, '&Ordine originale' );
  AddStr( 824, 'Cambia il font corrente' );
  AddStr( 825, 'Seleziona un criterio di ordinamento' );
  // unit fmAdd and fmAddDropedFiles
  AddStr( 900, 'Aggiungi da' );
  AddStr( 901, 'Nome :' );
  AddStr( 902, 'Cartelle' );
  AddStr( 903, 'Recursivamente nelle cartelle' );
  AddStr( 904, 'Includi il path corrente' );
  AddStr( 905, 'Registra cartelle vuote' );
  AddStr( 906, 'Path registrazione :' );
  AddStr( 907, 'Encrypt files ?' );
  AddStr( 908, 'Livello di compressione:' );
  AddStr( 909, 'Aggiungi' );
  AddStr( 910, 'Nessuno'+#13+
               'Intero'+#13+
               'Relativo' );
  AddStr( 911, 'Massimo (il più lento)'+#13+
               'Normale'+#13+
               'Veloce'+#13+
               'Super Veloce'+#13+
               'Nessuno' );
  AddStr( 912, 'Aggiungi files cancellati' );
  AddStr( 913, 'Aggiungi campi' );
  AddStr( 914, 'Filtro :' );
  AddStr( 915, 'Aggiungi alla cartella corrente ?' );
  // unit fmConfiguration
  AddStr( 1000, 'Configurazione' );
  AddStr( 1001, 'Disk spanning' );
  AddStr( 1002, 'Creazione dell''archivio' );
  AddStr( 1003, 'Opzioni' );
  AddStr( 1004, 'Split dell''archivio' );
  AddStr( 1005, 'Massima ampiezza di un segmento dell''archivio:' );
  AddStr( 1006, '720 Kb'+#13+
                '1,44 Mb'+#13+
                'Other (Kb):' );

  AddStr( 1007, 'Compressione archivio' );
  AddStr( 1008, 'Criptazione archivio' );
  AddStr( 1009, 'Consolidamento archivio' );
  AddStr( 1010, 'Sola lettura' );
  AddStr( 1011, 'Creazione archivio SFX' );
  AddStr( 1014, 'Ampiezza Blocco' );
  AddStr( 1015, 'Spazio riservato' );
  AddStr( 1016, 'Kb' );
  AddStr( 1017, 'Lingua:' );
  AddStr( 1018, 'Automatico'+#13+
                'Inglese'+#13+
                'Francese'+#13+
                'Cinese'+#13+
                'Portoghese'+#13+
                'Tedesco'+#13+
                'Italiano'+#13+
                'Russo'+#13+
                'Spagnolo' );
  AddStr( 1019, 'Mostra cartelle vuote' );
  AddStr( 1020, 'Mostra vista ad albero' );
  // unit fmCreateFolder
  AddStr( 1100, 'Cartella corrente:' );
  AddStr( 1101, 'Nome:' );
  // unit fmDelete
  AddStr( 1200, 'Cancella' );
  AddStr( 1201, 'Files' );
  AddStr( 1202, '&Tutto l''archivio'+#13+
                'Files &Selezionati'+#13+
                '&Files:' );
  // unit fmEnterCryptKey
  AddStr( 1300, 'Messaggio di sistema' );
  AddStr( 1301, 'Nascondere la password ?' );
  // unit fmExtract
  AddStr( 1400, 'Estrazione' );
  AddStr( 1401, 'Estrazione in:' );
  AddStr( 1402, 'Files' );
  AddStr( 1403, 'Files &Selezionati'+#13+
                '&Tutti i files'+#13+
                'F&iles:' );
  AddStr( 1404, '&Usa i nomi delle cartelle' );
  AddStr( 1405, 'Sovrascrivere i files esistenti'+#13+
                'Saltare i files esistenti'+#13+
                'Aggiorna i nuovi files'+#13+
                'Chiedere conferma'+#13+
                'Ripristina solo i files esistenti'+#13+
                'Aggiorna i files esistenti' );
  AddStr( 1406, 'Cartelle / Drives' );
  AddStr( 1407, 'Nuova cartella...' );
  // unit fmHelpOnSFX
  AddStr( 1500, 'Le seguenti parole chiave possono essere usate nella "Linea di Comando" e'+#13+
                'nei campi "Default Extract Path" :' );
  AddStr( 1501, 'sarà rimpiazzato con la directory Temp'+#13+
                '(solitamente ''c:\windows\temp'' or ''c:\win95\temp'' or ''c:\temp'')' );
  AddStr( 1502, 'sarà rimpiazzato con la directory Windows'+#13+
                '(solitamente ''c:\windows'' or ''c:\win95'')' );
  AddStr( 1503, 'sarà rimpiazzato con la directory di systema'+#13+
                '(solitamente ''c:\windows\system'' or ''c:\win95\system'')' );
  AddStr( 1504, 'sarà rimpiazzato con la directory dei programmi'+#13+
                '(solitamente ''c:\Programmi'' [dipende dal linguaggio'+#13+
                'della versione di Windows installata])' );
  AddStr( 1505, 'sarà rimpiazzato con la directory dove i files'+#13+
                'furono estratti (vale solo per la "Linea di Comando" o'+#13+
                'i campi "Arguments")' );
  AddStr( 1506, 'Esempio:' );
  AddStr( 1507, '<PF>MiaSocietà\Mioufficio' );
  // unit fmInformation
  AddStr( 1600, 'Indirizzo:' );
  AddStr( 1601, 'Nome:' );
  AddStr( 1602, 'Ampiezza File:' );
  AddStr( 1603, 'Files:' );
  AddStr( 1604, 'Compressione:' );
  AddStr( 1605, 'Data/Ora:' );
  AddStr( 1606, 'Segmento:' );
  AddStr( 1607, 'Attributi' );
  AddStr( 1608, 'Criptato' );
  AddStr( 1609, 'Compresso' );
  AddStr( 1610, 'Consolidato' );
  AddStr( 1611, 'Sola lettura' );
  AddStr( 1612, 'Segmento finale' );
  AddStr( 1613, 'Informazioni' );
  // unit fmSFXComments
  AddStr( 1700, 'Commenti' );
  AddStr( 1701, 'Commenti visualizzati all''apertura dell''archivio SFX' );
  AddStr( 1702, 'Commenti visualizzati dopo l''estrazione dei files registrati nell''archivio SFX' );
  AddStr( 1703, 'Togli i commenti' );
  // unit fmSFXConfig
  AddStr( 1800, 'Configurazione SFX' );
  AddStr( 1801, 'Esegui il file dopo l''estrazione ?' );
  AddStr( 1802, 'L''utente deve scegliere i files da estrarre ?' );
  AddStr( 1803, 'L''utente può scegliere la modalità di sovrascrittura ?');
  AddStr( 1804, 'Descrizione:' );
  AddStr( 1805, 'Linea di comando:' );
  AddStr( 1806, 'Argomenti:' );
  AddStr( 1807, 'Indirizzo di estrazione di default:' );
  AddStr( 1808, 'Modalità di sovrascrittura:' );
  AddStr( 1809, 'Commenti...' );
  AddStr( 1810, 'Chiedere conferma'+#13+
                'Sovrascrivere i files esistenti'+#13+
                'Saltare i files esistenti'+#13+
                'Sovrascrivere solo se più recenti'+#13+
                'Ripristina solo i files esistenti'+#13+
                'Estrarre il file solo se esiste già ed è più recente' );
  AddStr( 1811, 'L''utente non è abilitato ad eseguire il file ?' );
  // unit fmTextViewer
  AddStr( 1900, 'Visualizza: %s' );
  AddStr( 1901, 'Copia nella &Clipboard' );
  AddStr( 1902, '&Font' );
  // unit fmView
  AddStr( 2000, 'Visualizza : %s' );
  AddStr( 2001, 'Usando' );
  AddStr( 2002, 'Visualizza' );
  AddStr( 2003, 'Programma &Associato (%s)'+#13+
                'Visualizzatore &Interno di testo ASCII' );
  // unit fmLastOutput
  AddStr( 2100, 'Visualizza l''ultimo output' );
end;

end.
