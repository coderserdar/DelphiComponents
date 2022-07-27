{*************************************************************************}
{
   LsConsts.pas
   ------------
     version : 2.886
     Updated : 12/5/2005
 =========================================================================
 ResourceStrings for the following languages:
   - English (Default)
   - Chinese_Tra (Chinese Tradition)
   - Chinese_Sim (Chinese Simplified)
   - Dutch
   - French
   - German
   - Italian
   - Japanese
   - Korean
   - Polish
   - Brazillian Portuguese
   - Slovenian
   - Slovak
   - Spanish
   - Swedish
   - Turkish

 ACKNOWLEDGMENTS
 ===============
   I would like thank Andreas Roth, Bernd Ohse, Bernard Bourguignon,
   Ferruccio Accalai, David Abdaleon, Alberto Meyer, Sam Francke, Zenon
   Mieszkuniec, Martin Berta, Scarfman Lin, Yoshihiro Sugahara,
   Matjaz Prtenjak, Olle Johansson, Do-wan Kim and Yasar Ozanlagan for
   their German, French, Italian, Spanish, Brazillian-Portuguese, Dutch,
   Polish, Slovak, Chinese(Traditional and Simplified), Japanese, Slovenian,
   Swedish, Korean and Turkish language translations.
}
{*************************************************************************}

unit LsConsts;

interface

ResourceString

//------------------------------------------
//               English
//------------------------------------------
  //872<
  //LsFileListview28 Column Caption
  ENGLISH_ColIdName   = 'Name';
  ENGLISH_ColIdSize   = 'Size';
  ENGLISH_ColIdType   = 'Type';
  ENGLISH_ColIdDate   = 'Modified';
  ENGLISH_ColIdAttr   = 'Attr';
  ENGLISH_ColIdHdSize = 'Disk Size';
  ENGLISH_ColIdFSpace = 'Free Space';
  //Drive Type
  ENGLISH_DrvTypeStr0 = 'Unknown';
  ENGLISH_DrvTypeStr1 = 'Not exist';
  ENGLISH_DrvTypeStr2 = 'Removable Disk';
  ENGLISH_DrvTypeStr3 = 'Fixed Disk';
  ENGLISH_DrvTypeStr4 = 'Network Disk';
  ENGLISH_DrvTypeStr5 = 'CD-ROM Disk';
  ENGLISH_DrvTypeStr6 = 'RAM Disk';
  //LsFileListview28Popup MenuItem Caption
  ENGLISH_LvItemID_0  = '&Open';
  ENGLISH_LvItemID_1  = '&View';
  ENGLISH_LvItemID_3  = 'Se&nd To';
  ENGLISH_LvItemID_5  = 'Cu&t';
  ENGLISH_LvItemID_6  = '&Copy';
  ENGLISH_LvItemID_7  = '&Paste';
  ENGLISH_LvItemID_9  = '&Rename';
  ENGLISH_LvItemID_10 = '&Delete';
  ENGLISH_LvItemID_12 = '&Properties';
  ENGLISH_LvItemID_14 = '&New Folder...';
  ENGLISH_LvItemID_30 = 'Any Folder ...';
  ENGLISH_LvItemID_31 = 'Clipboard as Contents';
  ENGLISH_LvItemID_32 = 'Clipboard as FileName';
  ENGLISH_LvItemID_33 = 'DeskTop as ShortCut';
  //LsDirTree21Popup MenuItem Caption
  ENGLISH_TvItemID_0  = '&New Folder';
  ENGLISH_TvItemID_1  = '&Rename Folder';
  ENGLISH_TvItemID_2  = '&Delete Folder';
  ENGLISH_TvItemID_4  = 'Cu&t';
  ENGLISH_TvItemID_5  = '&Copy';
  ENGLISH_TvItemID_6  = '&Paste';
  ENGLISH_TvItemID_8  = 'Tree &Size';
  ENGLISH_TvItemID_9  = '&Folder Contents';
  ENGLISH_TvItemID_11 = '&Map Network Drive ...';
  ENGLISH_TvItemID_12 = 'Dis&Connect Network Drive';
  ENGLISH_TvItemID_14 = '&Property';
  //Error words
  ENGLISH_ewDrive = 'Drive ';  //873
  ENGLISH_ewFolder = 'Folder "';  //873
  ENGLISH_ewError = 'ERROR - ';
  ENGLISH_ewFile = ' file ';
  ENGLISH_ewFrom = 'From  : ';
  ENGLISH_ewTo = 'To     : ';
  ENGLISH_ewCancel = 'Cancel';
  ENGLISH_ewBrowse = 'Browse';
  ENGLISH_ewReadOnly = 'ReadOnly';
  ENGLISH_ewArchive = 'Archive';
  ENGLISH_ewHidden = 'Hidden';
  ENGLISH_ewSystem = 'System';
  //Error strings
  ENGLISH_esCannot = 'ERROR - Can''t ';
  ENGLISH_esSpecifyDir = 'Please specify a directory';
  ENGLISH_esInvalidDrvID = 'Not a valid Drive ID';
  ENGLISH_esDrvNotReady = 'There is no disk in Drive or Drive is not ready';
  ENGLISH_esExists = 'already exists !';
  ENGLISH_esInvalidDirName = 'Error - Invalid Directory Name';
  ENGLISH_esConfirmRename = 'Do you want to rename the selected folder';
  ENGLISH_esCannotAddDrv = 'Can''t add drives';
  ENGLISH_esNewFolder = 'New Folder';
  ENGLISH_esInvalidChars = 'Folder Name contains invalid characters';
  ENGLISH_esNotFound = 'not found';
  ENGLISH_esFilesIn = 'Files in ';
  ENGLISH_esFileOpFailed = 'File operation failed';
  ENGLISH_esReadOnly = 'It''s ReadOnly';
  ENGLISH_esNoFileSelected = 'No file(s) selected';
  ENGLISH_esSendToFolder = 'Send to any folder';
  ENGLISH_esSendToPath = 'Please enter the send_to path';
  ENGLISH_esPersistSaveError = 'Error - PersistFile.Save failed';
  ENGLISH_esSetAttr = 'Set Attributes';
  ENGLISH_esTreeSize = 'TREE SIZE';  //873
  ENGLISH_esAllSubDir = 'and all its Sub-Folders ';  //873
  //>872

//------------------------------------------
//               Chinese Tradition
//------------------------------------------
  //879<
  //LsFileListview28 Column Caption
  CHINESE_Tra_ColIdName   = '¦WºÙ';
  CHINESE_Tra_ColIdSize   = '¤j¤p';
  CHINESE_Tra_ColIdType   = 'Ãþ«¬';
  CHINESE_Tra_ColIdDate   = '¤w­×§ï';
  CHINESE_Tra_ColIdAttr   = 'ÄÝ©Ê';
  CHINESE_Tra_ColIdHdSize = 'ºÏºÐ®e¶q';
  CHINESE_Tra_ColIdFSpace = '³Ñ¾lªÅ¶¡';
  //Drive Type
  CHINESE_Tra_DrvTypeStr0 = '¤£©ú';
  CHINESE_Tra_DrvTypeStr1 = '¤£¦s¦b';
  CHINESE_Tra_DrvTypeStr2 = '¥i²¾°£¦¡ºÏºÐ';
  CHINESE_Tra_DrvTypeStr3 = '¥»¾÷ºÏºÐ';
  CHINESE_Tra_DrvTypeStr4 = 'ºô¸ôºÏºÐ¾÷';
  CHINESE_Tra_DrvTypeStr5 = '¥úºÐ¾÷';
  CHINESE_Tra_DrvTypeStr6 = 'µêÀÀºÏºÐ¾÷';
  //LsFileListview28Popup MenuItem Caption
  CHINESE_Tra_LvItemID_0  = '¶}±Ò(&O)';
  CHINESE_Tra_LvItemID_1  = 'ÀËµø(&V)';
  CHINESE_Tra_LvItemID_3  = '¶Ç°e¨ì(&N)';
  CHINESE_Tra_LvItemID_5  = '°Å¤U(&T)';
  CHINESE_Tra_LvItemID_6  = '½Æ»s(&C)';
  CHINESE_Tra_LvItemID_7  = '¶K¤W(&P)';
  CHINESE_Tra_LvItemID_9  = '§ó¦W(&R)';
  CHINESE_Tra_LvItemID_10 = '§R°£(&D)';
  CHINESE_Tra_LvItemID_12 = '¤º®e(&P)';
  CHINESE_Tra_LvItemID_14 = '·s¼W¸ê®Æ§¨(&N)...';
  CHINESE_Tra_LvItemID_30 = '¨ä¥L¸ê®Æ§¨ ...';
  CHINESE_Tra_LvItemID_31 = '½Æ»s¤º®e¨ì°Å¶KÃ¯';
  CHINESE_Tra_LvItemID_32 = '½Æ»sÀÉ¦W¨ì°Å¶KÃ¯';
  CHINESE_Tra_LvItemID_33 = '¦b®à­±«Ø¥ß±¶®|';
  //LsDirTree21Popup MenuItem Caption
  CHINESE_Tra_TvItemID_0  = '·s¼W¤l¸ê®Æ§¨(&N)';
  CHINESE_Tra_TvItemID_1  = '§ó¦W¸ê®Æ§¨(&R)';
  CHINESE_Tra_TvItemID_2  = '§R°£¸ê®Æ§¨(&D)';
  CHINESE_Tra_TvItemID_4  = '°Å¤U(&T)';
  CHINESE_Tra_TvItemID_5  = '½Æ»s(&C)';
  CHINESE_Tra_TvItemID_6  = '¶K¤W(&P)';
  CHINESE_Tra_TvItemID_8  = '¸ê®Æ§¨¤j¤p(&S)';
  CHINESE_Tra_TvItemID_9  = '¸ê®Æ§¨¤º®e(&F)';
  CHINESE_Tra_TvItemID_11 = '³s½uºô¸ôºÏºÐ¾÷(&M)';
  CHINESE_Tra_TvItemID_12 = '¤¤Â_ºô¸ôºÏºÐ¾÷(&C)';
  CHINESE_Tra_TvItemID_14 = '¤º®e(&P)';
  //Error words
  CHINESE_Tra_ewDrive = 'ºÏºÐ ';  //873
  CHINESE_Tra_ewFolder = '¸ê®Æ§¨ "';  //873
  CHINESE_Tra_ewError = '¿ù»~ - ';
  CHINESE_Tra_ewFile = ' ÀÉ®× ';
  CHINESE_Tra_ewFrom = '±q  : ';
  CHINESE_Tra_ewTo = '¨ì     : ';
  CHINESE_Tra_ewCancel = '¨ú®ø';
  CHINESE_Tra_ewBrowse = 'ÂsÄý';
  CHINESE_Tra_ewReadOnly = '°ßÅª';
  CHINESE_Tra_ewArchive = '«O¦s';
  CHINESE_Tra_ewHidden = 'ÁôÂÃ';
  CHINESE_Tra_ewSystem = '¨t²Î';
  //Error strings
  CHINESE_Tra_esCannot = '¿ù»~- µLªk ';
  CHINESE_Tra_esSpecifyDir = '½Ð«ü©w¤@­Ó¸ô®|';
  CHINESE_Tra_esInvalidDrvID = '¤£¬O¦³®ÄªººÏºÐ¾÷¥N¸¹';
  CHINESE_Tra_esDrvNotReady = 'ºÏºÐ¾÷¤º¨S¦³ºÐ¤ù¡A©ÎºÏºÐ¾÷©|¥¼«Ý¾÷';
  CHINESE_Tra_esExists = '¤w¸g¦s¦b !';
  CHINESE_Tra_esInvalidDirName = '¿ù»~ - µL®Äªº¸ô®|¦WºÙ';
  CHINESE_Tra_esConfirmRename = '§A­n§ó°Ê³o­Ó¸ê®Æ§¨ªº¦WºÙ¶Ü';
  CHINESE_Tra_esCannotAddDrv = 'µLªk¥[¤JºÏºÐ¾÷';
  CHINESE_Tra_esNewFolder = '·s¼W¸ê®Æ§¨';
  CHINESE_Tra_esInvalidChars = '¸ê®Æ§¨¦WºÙ¤¤§t¦³µL®Ä¦r¤¸';
  CHINESE_Tra_esNotFound = '§ä¤£¨ì';
  CHINESE_Tra_esFilesIn = 'ÀÉ®×¦b ';
  CHINESE_Tra_esFileOpFailed = 'ÀÉ®×§@·~¥¢±Ñ';
  CHINESE_Tra_esReadOnly = '³o¬O°ßÅªªº';
  CHINESE_Tra_esNoFileSelected = '¨S¦³¿ï¨ìÀÉ®×';
  CHINESE_Tra_esSendToFolder = '¶Ç°e¨ì¨ä¥L¸ê®Æ§¨';
  CHINESE_Tra_esSendToPath = '½Ð¿é¤J¶Ç°eªº¸ô®|';
  CHINESE_Tra_esPersistSaveError = '¿ù»~ - PersistFile.Save ¥¢±Ñ';
  CHINESE_Tra_esSetAttr = '³]©wÀÉ®×ÄÝ©Ê';
  CHINESE_Tra_esTreeSize = '¸ê®Æ§¨¤º®e¤j¤p';  //873
  CHINESE_Tra_esAllSubDir = '»P¨ä¥þ³¡ªº¤l¸ê®Æ§¨ ';  //873
  //>879

//------------------------------------------
//               Chinese Simplified
//------------------------------------------
  //879<
  //LsFileListview28 Column Caption
  CHINESE_Sim_ColIdName   = 'Ãû³Æ';
  CHINESE_Sim_ColIdSize   = '´óÐ¡';
  CHINESE_Sim_ColIdType   = 'ÀàÐÍ';
  CHINESE_Sim_ColIdDate   = 'ÒÑÐÞ¸Ä';
  CHINESE_Sim_ColIdAttr   = 'ÊôÐÔ';
  CHINESE_Sim_ColIdHdSize = '´ÅÅÌÈÝÁ¿';
  CHINESE_Sim_ColIdFSpace = 'Ê£Óà¿Õ¼ä';
  //Drive Type
  CHINESE_Sim_DrvTypeStr0 = '²»Ã÷';
  CHINESE_Sim_DrvTypeStr1 = '²»´æÔÚ';
  CHINESE_Sim_DrvTypeStr2 = '¿ÉÒÆ³ýÊ½´ÅÅÌ';
  CHINESE_Sim_DrvTypeStr3 = '±¾»ú´ÅÅÌ';
  CHINESE_Sim_DrvTypeStr4 = 'ÍøÂç´ÅÅÌ»ú';
  CHINESE_Sim_DrvTypeStr5 = '¹âµú»ú';
  CHINESE_Sim_DrvTypeStr6 = 'ÐéÅÌ»ú';
  //LsFileListview28Popup MenuItem Caption
  CHINESE_Sim_LvItemID_0  = '¿ªÆô(&O)';
  CHINESE_Sim_LvItemID_1  = '¼ìÊÓ(&V)';
  CHINESE_Sim_LvItemID_3  = '´«ËÍµ½(&N)';
  CHINESE_Sim_LvItemID_5  = '¼ôÏÂ(&T)';
  CHINESE_Sim_LvItemID_6  = '¸´ÖÆ(&C)';
  CHINESE_Sim_LvItemID_7  = 'ÌùÉÏ(&P)';
  CHINESE_Sim_LvItemID_9  = '¸üÃû(&R)';
  CHINESE_Sim_LvItemID_10 = 'É¾³ý(&D)';
  CHINESE_Sim_LvItemID_12 = 'ÄÚÈÝ(&P)';
  CHINESE_Sim_LvItemID_14 = 'ÐÂÔö×ÊÁÏ¼Ð(&N)...';
  CHINESE_Sim_LvItemID_30 = 'ÆäËû×ÊÁÏ¼Ð ...';
  CHINESE_Sim_LvItemID_31 = '¸´ÖÆÄÚÈÝµ½¼ôÌù²¾';
  CHINESE_Sim_LvItemID_32 = '¸´ÖÆµµÃûµ½¼ôÌù²¾';
  CHINESE_Sim_LvItemID_33 = 'ÔÚ×ÀÃæ´´½¨½Ý¾¶';
  //LsDirTree21Popup MenuItem Caption
  CHINESE_Sim_TvItemID_0  = 'ÐÂÔö×Ó×ÊÁÏ¼Ð(&N)';
  CHINESE_Sim_TvItemID_1  = '¸üÃû×ÊÁÏ¼Ð(&R)';
  CHINESE_Sim_TvItemID_2  = 'É¾³ý×ÊÁÏ¼Ð(&D)';
  CHINESE_Sim_TvItemID_4  = '¼ôÏÂ(&T)';
  CHINESE_Sim_TvItemID_5  = '¸´ÖÆ(&C)';
  CHINESE_Sim_TvItemID_6  = 'ÌùÉÏ(&P)';
  CHINESE_Sim_TvItemID_8  = '×ÊÁÏ¼Ð´óÐ¡(&S)';
  CHINESE_Sim_TvItemID_9  = '×ÊÁÏ¼ÐÄÚÈÝ(&F)';
  CHINESE_Sim_TvItemID_11 = 'Á¬»úÍøÂç´ÅÅÌ»ú(&M)';
  CHINESE_Sim_TvItemID_12 = 'ÖÐ¶ÏÍøÂç´ÅÅÌ»ú(&C)';
  CHINESE_Sim_TvItemID_14 = 'ÄÚÈÝ(&P)';
  //Error words
  CHINESE_Sim_ewDrive = '´ÅÅÌ ';  //873
  CHINESE_Sim_ewFolder = '×ÊÁÏ¼Ð "';  //873
  CHINESE_Sim_ewError = '´íÎó - ';
  CHINESE_Sim_ewFile = ' ÎÄ¼þ ';
  CHINESE_Sim_ewFrom = '´Ó  : ';
  CHINESE_Sim_ewTo = 'µ½     : ';
  CHINESE_Sim_ewCancel = 'È¡Ïû';
  CHINESE_Sim_ewBrowse = 'ä¯ÀÀ';
  CHINESE_Sim_ewReadOnly = 'Î¨¶Á';
  CHINESE_Sim_ewArchive = '±£´æ';
  CHINESE_Sim_ewHidden = 'Òþ²Ø';
  CHINESE_Sim_ewSystem = 'ÏµÍ³';
  //Error strings
  CHINESE_Sim_esCannot = '´íÎó- ÎÞ·¨ ';
  CHINESE_Sim_esSpecifyDir = 'ÇëÖ¸¶¨Ò»¸öÂ·¾¶';
  CHINESE_Sim_esInvalidDrvID = '²»ÊÇÓÐÐ§µÄ´ÅÅÌ»ú´úºÅ';
  CHINESE_Sim_esDrvNotReady = '´ÅÅÌ»úÄÚÃ»ÓÐµúÆ¬£¬»ò´ÅÅÌ»úÉÐÎ´´ý»ú';
  CHINESE_Sim_esExists = 'ÒÑ¾­´æÔÚ !';
  CHINESE_Sim_esInvalidDirName = '´íÎó - ÎÞÐ§µÄÂ·¾¶Ãû³Æ';
  CHINESE_Sim_esConfirmRename = 'ÄãÒª¸ü¶¯Õâ¸ö×ÊÁÏ¼ÐµÄÃû³ÆÂð';
  CHINESE_Sim_esCannotAddDrv = 'ÎÞ·¨¼ÓÈë´ÅÅÌ»ú';
  CHINESE_Sim_esNewFolder = 'ÐÂÔö×ÊÁÏ¼Ð';
  CHINESE_Sim_esInvalidChars = '×ÊÁÏ¼ÐÃû³ÆÖÐº¬ÓÐÎÞÐ§×Ö·û';
  CHINESE_Sim_esNotFound = 'ÕÒ²»µ½';
  CHINESE_Sim_esFilesIn = 'ÎÄ¼þÔÚ ';
  CHINESE_Sim_esFileOpFailed = 'ÎÄ¼þ×÷ÒµÊ§°Ü';
  CHINESE_Sim_esReadOnly = 'ÕâÊÇÎ¨¶ÁµÄ';
  CHINESE_Sim_esNoFileSelected = 'Ã»ÓÐÑ¡µ½ÎÄ¼þ';
  CHINESE_Sim_esSendToFolder = '´«ËÍµ½ÆäËû×ÊÁÏ¼Ð';
  CHINESE_Sim_esSendToPath = 'ÇëÊäÈë´«ËÍµÄÂ·¾¶';
  CHINESE_Sim_esPersistSaveError = '´íÎó - PersistFile.Save Ê§°Ü';
  CHINESE_Sim_esSetAttr = 'Éè¶¨ÎÄ¼þÊôÐÔ';
  CHINESE_Sim_esTreeSize = '×ÊÁÏ¼ÐÄÚÈÝ´óÐ¡';  //873
  CHINESE_Sim_esAllSubDir = 'ÓëÆäÈ«²¿µÄ×Ó×ÊÁÏ¼Ð ';  //873
  //>879

//------------------------------------------
//               Dutch
//------------------------------------------
  //875<
  //LsFileListview28 Column Caption
  DUTCH_ColIdName   = 'Naam';
  DUTCH_ColIdSize   = 'Grootte';
  DUTCH_ColIdType   = 'Type';
  DUTCH_ColIdDate   = 'Gewijzigd op';
  DUTCH_ColIdAttr   = 'Attr';
  DUTCH_ColIdHdSize = 'Schijf groote';
  DUTCH_ColIdFSpace = 'Vrije ruimte';
  //Drive Type
  DUTCH_DrvTypeStr0 = 'Onbekend';
  DUTCH_DrvTypeStr1 = 'Bestaat niet';
  DUTCH_DrvTypeStr2 = 'Verwijderbare schijf';
  DUTCH_DrvTypeStr3 = 'Vaste schijf';
  DUTCH_DrvTypeStr4 = 'Netwerk schijf';
  DUTCH_DrvTypeStr5 = 'CD-ROM';
  DUTCH_DrvTypeStr6 = 'RAM schijf';
  //LsFileListview28Popup MenuItem Caption
  DUTCH_LvItemID_0  = '&Open';
  DUTCH_LvItemID_1  = '&Beeld';
  DUTCH_LvItemID_3  = '&Versturen naar';
  DUTCH_LvItemID_5  = '&Knippen';
  DUTCH_LvItemID_6  = 'K&opiëren';
  DUTCH_LvItemID_7  = '&Plakken';
  DUTCH_LvItemID_9  = '&Naam wijzigen';
  DUTCH_LvItemID_10 = '&Verwijderen';
  DUTCH_LvItemID_12 = '&Eigenschappen';
  DUTCH_LvItemID_14 = '&Nieuwe map ...';
  DUTCH_LvItemID_30 = 'Elke map ...';
  DUTCH_LvItemID_31 = 'Klembord als inhoud';
  DUTCH_LvItemID_32 = 'Klembord als bestandsnaam';
  DUTCH_LvItemID_33 = 'Bureaublad als snelkoppeling';
  //LsDirTree21Popup MenuItem Caption
  DUTCH_TvItemID_0  = '&Nieuwe map';
  DUTCH_TvItemID_1  = '&Geef map andere naam';
  DUTCH_TvItemID_2  = '&Verwijer map';
  DUTCH_TvItemID_4  = 'Knippen';
  DUTCH_TvItemID_5  = '&Kopiëren';
  DUTCH_TvItemID_6  = '&Plakken';
  DUTCH_TvItemID_8  = 'Map grootte';
  DUTCH_TvItemID_9  = '&Map inhoud';
  DUTCH_TvItemID_11 = '&Map netwerk schijf ...';
  DUTCH_TvItemID_12 = 'Verbreek verbinding met netwerk schijf';
  DUTCH_TvItemID_14 = '&Eigenschap';
  //Error words
  DUTCH_ewDrive = 'Schijf ';  //873
  DUTCH_ewFolder = 'Map "';  //873
  DUTCH_ewError = 'FOUT - ';
  DUTCH_ewFile = ' bestand ';
  DUTCH_ewFrom = 'Van  : ';
  DUTCH_ewTo = 'Naar     : ';
  DUTCH_ewCancel = 'Afbreken';
  DUTCH_ewBrowse = 'Verkennen';
  DUTCH_ewReadOnly = 'Alleen lezen';
  DUTCH_ewArchive = 'Archief';
  DUTCH_ewHidden = 'Verborgen';
  DUTCH_ewSystem = 'Systeem';
  //Error strings
  DUTCH_esCannot = 'FOUT - kan niet ';
  DUTCH_esSpecifyDir = 'Geef svp directorie op';
  DUTCH_esInvalidDrvID = 'Geen geldige drive ID';
  DUTCH_esDrvNotReady = 'Er is geen disk in de drive of de drive is niet klaar';
  DUTCH_esExists = 'bestaat al !';
  DUTCH_esInvalidDirName = 'FOUT - verkeerde directorie naam';
  DUTCH_esConfirmRename = 'Wilt u de geselecteerde map een ander naam geven';
  DUTCH_esCannotAddDrv = 'Kan geen schijven toevoegen';
  DUTCH_esNewFolder = 'Nieuwe map';
  DUTCH_esInvalidChars = 'De map naam bevat ongeldige letters';
  DUTCH_esNotFound = 'niet gevonden';
  DUTCH_esFilesIn = 'Bestanden in ';
  DUTCH_esFileOpFailed = 'Bestands operatie mislukte';
  DUTCH_esReadOnly = 'Het is aleen-lezen';
  DUTCH_esNoFileSelected = 'Geen bestand(en) geselecteerd';
  DUTCH_esSendToFolder = 'Stuur naar welke map dan ook';
  DUTCH_esSendToPath = 'Geef svp het doel pad';
  DUTCH_esPersistSaveError = 'ERROR - herhaalde bestand.opslaan mislukte';
  DUTCH_esSetAttr = 'Zet attributen';
  DUTCH_esTreeSize = 'BOOM GROOTTE';  //873
  DUTCH_esAllSubDir = 'en alle-sub-mappen ';  //873
  //>875

//------------------------------------------
//               French
//------------------------------------------
  //872<
  //LsFileListview28 Column Caption
  FRENCH_ColIdName    = 'Nom';
  FRENCH_ColIdSize    = 'Taille';
  FRENCH_ColIdType    = 'Type';
  FRENCH_ColIdDate    = 'Date';
  FRENCH_ColIdAttr    = 'Attributs';
  FRENCH_ColIdHdSize  = 'Taille Disque';
  FRENCH_ColIdFSpace  = 'Espace libre';
  //Drive Type
  FRENCH_DrvTypeStr0  = 'Inconnu';
  FRENCH_DrvTypeStr1  = 'N''existe pas';
  FRENCH_DrvTypeStr2  = 'Disque amovible';
  FRENCH_DrvTypeStr3  = 'Disque Fixe';
  FRENCH_DrvTypeStr4  = 'Disque Réseau';
  FRENCH_DrvTypeStr5  = 'CD-ROM';
  FRENCH_DrvTypeStr6  = 'RAM Disk';
  //LsFileListview28Popup MenuItem Caption
  FRENCH_LvItemID_0   = '&Ouvrir';
  FRENCH_LvItemID_1   = '&Voir';
  FRENCH_LvItemID_3   = 'Co&pier dans';
  FRENCH_LvItemID_5   = 'C&ouper';
  FRENCH_LvItemID_6   = '&Copier';
  FRENCH_LvItemID_7   = 'Co&ller';
  FRENCH_LvItemID_9   = '&Renommer';
  FRENCH_LvItemID_10  = '&Supprimer';
  FRENCH_LvItemID_12  = '&Propriétés';
  FRENCH_LvItemID_14  = '&Nouveau Dossier ...';
  FRENCH_LvItemID_30  = 'le Dossier ...';  //873
  FRENCH_LvItemID_31  = 'Contenu dans le Presse-Papier';
  FRENCH_LvItemID_32  = 'Nom du fichier  dans le Presse-Papier';  //873
  FRENCH_LvItemID_33  = 'Comme Raccourci sur le Bureau';
  //LsDirTree21Popup MenuItem Caption
  FRENCH_TvItemID_0   = '&Nouveau Dossier';
  FRENCH_TvItemID_1   = '&Renommer le Dossier';
  FRENCH_TvItemID_2   = '&Effacer le Dossier';
  FRENCH_TvItemID_4   = 'Cou&per';
  FRENCH_TvItemID_5   = '&Copier';
  FRENCH_TvItemID_6   = 'Co&ller';
  FRENCH_TvItemID_8   = 'Taille de l''&arborescence';
  FRENCH_TvItemID_9   = '&Contenu du Dossier';
  FRENCH_TvItemID_11  = '&Connecter Lecteur Réseau ...';
  FRENCH_TvItemID_12  = '&Déconnecter Lecteur Réseau';
  FRENCH_TvItemID_14  = '&Propriétés';
  //Error words
  FRENCH_ewDrive = 'Lecteur ';  //873
  FRENCH_ewFolder = 'Dossier "';  //873
  FRENCH_ewError = 'ERREUR - ';
  FRENCH_ewFile = ' le fichier  ';
  FRENCH_ewFrom = 'de  : ';
  FRENCH_ewTo = 'à     : ';
  FRENCH_ewCancel = 'Annule';
  FRENCH_ewBrowse = 'Parcourir';  //873
  FRENCH_ewReadOnly = 'Lecture Seule';
  FRENCH_ewArchive = 'Archive';  //873
  FRENCH_ewHidden = 'Caché';
  FRENCH_ewSystem = 'Système';
  //Error strings
  FRENCH_esCannot = 'ERREUR - Impossible de ';
  FRENCH_esSpecifyDir = 'SVP choisissez un répertoire';
  FRENCH_esInvalidDrvID = 'ID de lecteur non valide';
  FRENCH_esDrvNotReady = 'Il n''y a pas de disque dans le lecteur' +
                          'ou le lecteur n''est pas prêt.';
  FRENCH_esExists = '" existe déjà !';
  FRENCH_esInvalidDirName = 'Erreur - Nom de répertoire invalide';
  FRENCH_esConfirmRename = 'Voulez-vous renommer le dossier sélectionné';
  FRENCH_esCannotAddDrv = 'Impossible d''ajouter des lecteurs';
  FRENCH_esNewFolder = 'Nouveau Dossier';
  FRENCH_esInvalidChars = 'Le nom de dossier contient des caractères interdits';
  FRENCH_esNotFound = 'pas trouvé';
  FRENCH_esFilesIn = 'Fichiers dans ';
  FRENCH_esFileOpFailed = 'L''opération sur le fichier a échoué';
  FRENCH_esReadOnly = 'Lecteur en Lecture Seule';  //873
  FRENCH_esNoFileSelected = 'Pas de fichier sélectionné';
  FRENCH_esSendToFolder = 'Copier dans le dossier...';
  FRENCH_esSendToPath = 'Veuillez donner le chemin de destination';
  FRENCH_esPersistSaveError = 'Erreur - Fichier protégé.L''enregistrement à échoué';
  FRENCH_esSetAttr = 'Donnez les Attributs';
  FRENCH_esTreeSize = 'Taille de l''arbre';  //873
  FRENCH_esAllSubDir = 'et de tous ses sous-dossiers ';  //873
  //>872

//------------------------------------------
//               German
//------------------------------------------
  //872<
  //LsFileListview28 Column Caption
  GERMAN_ColIdName    = 'Dateiname';
  GERMAN_ColIdSize    = 'Größe';
  GERMAN_ColIdType    = 'Typ';
  GERMAN_ColIdDate    = 'Geändert am';
  GERMAN_ColIdAttr    = 'Attribute';
  GERMAN_ColIdHdSize  = 'Gesamtgröße';
  GERMAN_ColIdFSpace  = 'Freier Speicher';
  //Drive Type
  GERMAN_DrvTypeStr0  = 'Unbekannt';
  GERMAN_DrvTypeStr1  = 'Nicht existent';
  GERMAN_DrvTypeStr2  = 'Disketten-Laufwerk';
  GERMAN_DrvTypeStr3  = 'Lokales Laufwerk';
  GERMAN_DrvTypeStr4  = 'Netz-Laufwerk';
  GERMAN_DrvTypeStr5  = 'CD-ROM-Laufwerk';
  GERMAN_DrvTypeStr6  = 'RAM-Laufwerk';
  //LsFileListview28Popup MenuItem Caption
  GERMAN_LvItemID_0   =  'Ö&ffnen';
  GERMAN_LvItemID_1   =  'An&zeigen';
  GERMAN_LvItemID_3   =  'Se&nden an';
  GERMAN_LvItemID_5   =  '&Ausschneiden';
  GERMAN_LvItemID_6   =  '&Kopieren';
  GERMAN_LvItemID_7   =  '&Einfügen';
  GERMAN_LvItemID_9   =  '&Umbenennen';
  GERMAN_LvItemID_10  =  '&Löschen';
  GERMAN_LvItemID_12  =  'Eigen&schaften';
  GERMAN_LvItemID_14  =  'Neuer &Ordner';
  GERMAN_LvItemID_30  =  'Ordne&r...';
  GERMAN_LvItenID_31  =  '&Datei(en) an Zwischenablage';
  GERMAN_LvItemID_32  =  'Dateiname(n) an Z&wischenablage';
  GERMAN_LvItemID_33  =  '&Verknüpfung auf den Desktop';
  //LsDirTree21Popup MenuItem Caption
  GERMAN_TvItemID_0   = '&Neuer Ordner';
  GERMAN_TvItemID_1   = '&Ordner umbenennen';
  GERMAN_TvItemID_2   = 'Ordner &löschen';
  GERMAN_TvItemID_4   = '&Ausschneiden';
  GERMAN_TvItemID_5   = '&Kopieren';
  GERMAN_TvItemID_6   = '&Einfügen';
  GERMAN_TvItemID_8   = 'Ordner&größe';
  GERMAN_TvItemID_9   = 'Ordner&inhalt';
  GERMAN_TvItemID_11  = 'Netzlaufwerk &verbinden...';
  GERMAN_TvItemID_12  = 'Netzlaufwerk &trennen...';
  GERMAN_TvItemID_14  = 'Eigen&schaften';
  //Error words
  GERMAN_ewDrive = 'Laufwerk ';  //873
  GERMAN_ewFolder = 'Ordner "';  //873
  GERMAN_ewError = 'Fehler - ';
  GERMAN_ewFile = '" ist fehlgeschlagen mit Datei ';
  GERMAN_ewFrom = 'Von  : ';
  GERMAN_ewTo = 'Nach : ';
  GERMAN_ewCancel = '&Abbrechen';
  GERMAN_ewBrowse = 'Ordner &wählen';
  GERMAN_ewReadOnly = 'Schreibgeschützt';
  GERMAN_ewArchive = 'Archiv';
  GERMAN_ewHidden = 'Versteckt';
  GERMAN_ewSystem = 'System';
  //Error strings
  GERMAN_esCannot = 'Fehler - Dateioperation "';
  GERMAN_esSpecifyDir = 'Bitte wählen Sie ein Verzeichnis:';
  GERMAN_esInvalidDrvID = 'Keine gültige Laufwerksbezeichnung!';
  GERMAN_esDrvNotReady = 'Kein Datenträger im Laufwerk ' +
                          'oder Laufwerk nicht bereit!';
  GERMAN_esExists = ' existiert bereits!';
  GERMAN_esInvalidDirName = 'Fehler - Ungültiger Verzeichnisname:';
  GERMAN_esConfirmRename = 'Wollen Sie den markierten Ordner umbenennen';
  GERMAN_esCannotAddDrv = 'Hinzufügen von Laufwerken nicht möglich!';
  GERMAN_esNewFolder = 'Neuer Ordner';
  GERMAN_esInvalidChars = 'Ordnername enthält ungültige Zeichen!';
  GERMAN_esNotFound = ' nicht gefunden!';
  GERMAN_esFilesIn = 'Dateien in ';
  GERMAN_esFileOpFailed = 'Dateioperation ist fehlgeschlagen!';
  GERMAN_esReadOnly = 'Die Datei ist schreibgeschützt!';
  GERMAN_esNoFileSelected = 'Keine Datei(en) markiert!';
  GERMAN_esSendToFolder = 'An Ordner senden...';
  GERMAN_esSendToPath = 'Bitte geben Sie den Zielpfad ein:';
  GERMAN_esPersistSaveError = 'Verknüpfung konnte nicht erstellt werden!';
  GERMAN_esSetAttr = 'Attribute setzen...';
  GERMAN_esTreeSize = 'BAUMGRÖßE';  //873
  GERMAN_esAllSubDir = 'und alle untergeordneten Ordner ';  //873
  //>872

//------------------------------------------
//               Italiano
//------------------------------------------
  //874<
  //LsFileListview28 Column Caaption
  ITALIAN_ColIdName   = 'Nome';
  ITALIAN_ColIdSize   = 'Capacità';
  ITALIAN_ColIdType   = 'Tipo';
  ITALIAN_ColIdDate   = 'Modificato';
  ITALIAN_ColIdAttr   = 'Attributi';
  ITALIAN_ColIdHdSize = 'Capacità disco';
  ITALIAN_ColIdFSpace = 'Disponibile';
  //Drive Type
  ITALIAN_DrvTypeStr0 = 'Sconosciuto';
  ITALIAN_DrvTypeStr1 = 'Inesistente';
  ITALIAN_DrvTypeStr2 = 'Disco rimovibile';
  ITALIAN_DrvTypeStr3 = 'Disco locale';
  ITALIAN_DrvTypeStr4 = 'Disco di rete';
  ITALIAN_DrvTypeStr5 = 'Disco CD-ROM';
  ITALIAN_DrvTypeStr6 = 'Disco RAM';
  //LsFileListview28Popup MenuItem Caption
  ITALIAN_LvItemID_0  = '&Apri';
  ITALIAN_LvItemID_1  = '&An&teprima';
  ITALIAN_LvItemID_3  = 'In&via a';
  ITALIAN_LvItemID_5  = 'Tagl&ia';
  ITALIAN_LvItemID_6  = '&Copia';
  ITALIAN_LvItemID_7  = '&Incolla';
  ITALIAN_LvItemID_9  = '&Rin&omina';
  ITALIAN_LvItemID_10 = '&Elimina';
  ITALIAN_LvItemID_12 = '&Proprietà';
  ITALIAN_LvItemID_14 = '&Nuova Cartella...';
  ITALIAN_LvItemID_30 = 'Cartella ...';
  ITALIAN_LvItemID_31 = 'Copia il contenuto negli appunti';
  ITALIAN_LvItemID_32 = 'Copia il nome negli appunti';
  ITALIAN_LvItemID_33 = 'Collegamento al Desktop';
  //LsDirTree21Popup MenuItem Caption
  ITALIAN_TvItemID_0  = '&Nuova Cartella';
  ITALIAN_TvItemID_1  = '&Rinomina Cartella';
  ITALIAN_TvItemID_2  = '&Elimina Cartella';
  ITALIAN_TvItemID_4  = 'Ta&glia';
  ITALIAN_TvItemID_5  = '&Copia';
  ITALIAN_TvItemID_6  = '&Incolla';
  ITALIAN_TvItemID_8  = '&Dimensioni elenco';
  ITALIAN_TvItemID_9  = 'Co&ntenuto Cartella';
  ITALIAN_TvItemID_11 = 'Connetti &unità di rete...';
  ITALIAN_TvItemID_12 = 'Dis&connetti unità di rete';
  ITALIAN_TvItemID_14 = '&Prorietà';
  //Error words
  ITALIAN_ewDrive = 'Unità ';
  ITALIAN_ewFolder = 'Cartella "';
  ITALIAN_ewError = 'ERRORE - ';
  ITALIAN_ewFile = ' file ';
  ITALIAN_ewFrom = 'Da  : ';
  ITALIAN_ewTo = 'A     : ';
  ITALIAN_ewCancel = 'Annulla';
  ITALIAN_ewBrowse = 'Esplora';
  ITALIAN_ewReadOnly = 'Sola lettura';
  ITALIAN_ewArchive = 'Archivio';
  ITALIAN_ewHidden = 'Nascosto';
  ITALIAN_ewSystem = 'Sistema';
  //Error strings
  ITALIAN_esCannot = 'ERRORE - Impossibile ';
  ITALIAN_esSpecifyDir = 'Specificare una cartella';
  ITALIAN_esInvalidDrvID = 'Lettera di unità non valida';
  ITALIAN_esDrvNotReady = 'Nessun disco nell''unità o unità non pronta';
  ITALIAN_esExists = 'già esistente !';
  ITALIAN_esInvalidDirName = 'Errore - Nome cartella non valido';
  ITALIAN_esConfirmRename = 'Vuoi rinominare la cartella selezionata';
  ITALIAN_esCannotAddDrv = 'Impossibile aggiungere unità';
  ITALIAN_esNewFolder = 'Nuova cartella';
  ITALIAN_esInvalidChars = 'Il nome della cartella contiene un carattere non valido';
  ITALIAN_esNotFound = 'non trovato';
  ITALIAN_esFilesIn = 'Files in ';
  ITALIAN_esFileOpFailed = 'Operazione fallita';
  ITALIAN_esReadOnly = 'E'' in sola lettura';
  ITALIAN_esNoFileSelected = 'Nessun file selezionato';
  ITALIAN_esSendToFolder = 'Invia alla cartella';
  ITALIAN_esSendToPath = 'Inserire il percorso di destinazione';
  ITALIAN_esPersistSaveError = 'Errore - PersistFile.Save fallito';
  ITALIAN_esSetAttr = 'Fissa attributi';
  ITALIAN_esTreeSize = 'Capacità';  //873
  ITALIAN_esAllSubDir = 'e tutte le sotto-cartelle ';
  //>874

//------------------------------------------
//               Japanese
//------------------------------------------
  //880<
  //LsFileListview28 Column Caption
  JAPANESE_ColIdName   = '–¼‘O';
  JAPANESE_ColIdSize   = 'ƒTƒCƒY';
  JAPANESE_ColIdType   = 'Ží—Þ';
  JAPANESE_ColIdDate   = 'XV“úŽž';
  JAPANESE_ColIdAttr   = '‘®«';
  JAPANESE_ColIdHdSize = '‡ŒvƒTƒCƒY';
  JAPANESE_ColIdFSpace = '‹ó‚«—Ìˆæ';
  //Drive Type
  JAPANESE_DrvTypeStr0 = '•s–¾';
  JAPANESE_DrvTypeStr1 = '‘¶Ý‚µ‚Ü‚¹‚ñ';
  JAPANESE_DrvTypeStr2 = 'ƒŠƒ€[ƒoƒuƒ‹ƒfƒoƒCƒX';
  JAPANESE_DrvTypeStr3 = 'ƒn[ƒhƒfƒBƒXƒN';
  JAPANESE_DrvTypeStr4 = 'ƒlƒbƒgƒ[ƒNƒhƒ‰ƒCƒu';
  JAPANESE_DrvTypeStr5 = 'CD-ROM ƒhƒ‰ƒCƒu';
  JAPANESE_DrvTypeStr6 = 'RAM Disk';
  //LsFileListview28Popup MenuItem Caption
  JAPANESE_LvItemID_0  = 'ŠJ‚­(&O)';
  JAPANESE_LvItemID_1  = 'Œ©‚é(&V)';
  JAPANESE_LvItemID_3  = '‘—‚é(&n)';
  JAPANESE_LvItemID_5  = 'Ø‚èŽæ‚è(&t)';
  JAPANESE_LvItemID_6  = 'ƒRƒs[(&C)';
  JAPANESE_LvItemID_7  = '“\‚è•t‚¯(&P)';
  JAPANESE_LvItemID_9  = '–¼‘O‚Ì•ÏX(&R)';
  JAPANESE_LvItemID_10 = 'íœ(&D)';
  JAPANESE_LvItemID_12 = 'ƒvƒƒpƒeƒB(&P)';
  JAPANESE_LvItemID_14 = 'V‹KƒtƒHƒ‹ƒ_ì¬(&N)';
  JAPANESE_LvItemID_30 = 'ƒtƒHƒ‹ƒ_‚ÌŽw’è ...';
  JAPANESE_LvItemID_31 = 'ƒtƒ@ƒCƒ‹‚Ì“à—e‚ðƒNƒŠƒbƒvƒ{[ƒh‚ÖƒRƒs[';
  JAPANESE_LvItemID_32 = 'ƒtƒ@ƒCƒ‹–¼‚ðƒNƒŠƒbƒvƒ{[ƒh‚ÖƒRƒs[';
  JAPANESE_LvItemID_33 = 'ƒfƒXƒNƒgƒbƒv‚ÖƒVƒ‡[ƒgƒJƒbƒg‚ðì¬';
  //LsDirTree21Popup MenuItem Caption
  JAPANESE_TvItemID_0  = 'V‹KƒtƒHƒ‹ƒ_ì¬(&N)';
  JAPANESE_TvItemID_1  = 'ƒtƒHƒ‹ƒ_–¼•ÏX(&R)';
  JAPANESE_TvItemID_2  = 'ƒtƒHƒ‹ƒ_íœ(&D)';
  JAPANESE_TvItemID_4  = 'Ø‚èŽæ‚è(&t)';
  JAPANESE_TvItemID_5  = 'ƒRƒs[(&C)';
  JAPANESE_TvItemID_6  = '“\‚è•t‚¯(&P)';
  JAPANESE_TvItemID_8  = 'ƒtƒHƒ‹ƒ_ƒTƒCƒY(&S)';
  JAPANESE_TvItemID_9  = 'ƒtƒHƒ‹ƒ_“à—e(&F)';
  JAPANESE_TvItemID_11 = 'ƒlƒbƒgƒ[ƒNƒhƒ‰ƒCƒu‚ÌŠ„‚è“–‚Ä(&M)';
  JAPANESE_TvItemID_12 = 'ƒlƒbƒgƒ[ƒNƒhƒ‰ƒCƒu‚ÌØ’f(&C)';
  JAPANESE_TvItemID_14 = 'ƒvƒƒpƒeƒB(&P)';
  //Error words
  JAPANESE_ewDrive = 'ƒhƒ‰ƒCƒu ';  //873
  JAPANESE_ewFolder = 'ƒtƒHƒ‹ƒ_ "';  //873
  JAPANESE_ewError = 'ƒGƒ‰[ - ';
  JAPANESE_ewFile = ' ƒtƒ@ƒCƒ‹ ';
  JAPANESE_ewFrom = '‚©‚ç  : ';
  JAPANESE_ewTo = '‚Ö     : ';
  JAPANESE_ewCancel = 'ƒLƒƒƒ“ƒZƒ‹';
  JAPANESE_ewBrowse = '...ŽQÆ';
  JAPANESE_ewReadOnly = '“Ç‚ÝŽæ‚èê—p';
  JAPANESE_ewArchive = 'ƒA[ƒJƒCƒu';
  JAPANESE_ewHidden = '‰B‚µƒtƒ@ƒCƒ‹';
  JAPANESE_ewSystem = 'ƒVƒXƒeƒ€ƒtƒ@ƒCƒ‹';
  //Error strings
  JAPANESE_esCannot = 'ƒGƒ‰[ - ŽÀso—ˆ‚Ü‚¹‚ñ ';
  JAPANESE_esSpecifyDir = 'ƒfƒBƒŒƒNƒgƒŠ‚ðŽw’è‚µ‚Ä‰º‚³‚¢';
  JAPANESE_esInvalidDrvID = '•s³‚È ƒhƒ‰ƒCƒuID ‚Å‚·';
  JAPANESE_esDrvNotReady = 'ƒfƒBƒXƒN‚ª‘}“ü‚³‚ê‚Ä‚¢‚È‚¢‚©Aƒhƒ‰ƒCƒu‚Ì€”õ‚ªo—ˆ‚Ä‚¢‚Ü‚¹‚ñ';
  JAPANESE_esExists = 'Šù‚É“¯–¼‚Ì‚à‚Ì‚ª‘¶Ý‚µ‚Ä‚¢‚Ü‚· !';
  JAPANESE_esInvalidDirName = 'ƒGƒ‰[ - •s³‚ÈƒfƒBƒŒƒNƒgƒŠ–¼‚Å‚·';
  JAPANESE_esConfirmRename = '‘I‘ð‚µ‚½ƒtƒHƒ‹ƒ_‚Ì–¼‘O‚ð•ÏX‚µ‚Ü‚·‚©H';
  JAPANESE_esCannotAddDrv = 'ƒhƒ‰ƒCƒu‚ð’Ç‰Á‚Å‚«‚Ü‚¹‚ñ';
  JAPANESE_esNewFolder = 'V‚µ‚¢ƒtƒHƒ‹ƒ_';
  JAPANESE_esInvalidChars = 'ƒtƒHƒ‹ƒ_–¼‚É•s³‚È•¶Žš‚ªŠÜ‚Ü‚ê‚Ä‚¢‚Ü‚·';
  JAPANESE_esNotFound = 'Œ©‚Â‚©‚è‚Ü‚¹‚ñ';
  JAPANESE_esFilesIn = 'ƒtƒ@ƒCƒ‹ in ';
  JAPANESE_esFileOpFailed = 'ƒtƒ@ƒCƒ‹‚Ì‘€ì‚ÉŽ¸”s‚µ‚Ü‚µ‚½';
  JAPANESE_esReadOnly = '“Ç‚ÝŽæ‚èê—p‚Å‚·';
  JAPANESE_esNoFileSelected = 'ƒtƒ@ƒCƒ‹‚ª‘I‘ð‚³‚ê‚Ä‚¢‚Ü‚¹‚ñ';
  JAPANESE_esSendToFolder = 'Žw’è‚ÌƒtƒHƒ‹ƒ_‚Öu‘—‚év';
  JAPANESE_esSendToPath = 'u‘—‚évæ‚ÌƒpƒX‚ð“ü—Í‚µ‚Ä‚­‚¾‚³‚¢';
  JAPANESE_esPersistSaveError = 'ƒGƒ‰[ - ƒVƒ‡[ƒgƒJƒbƒg‚Ìì¬Ž¸”s';
  JAPANESE_esSetAttr = '‘®«•ÏX';
  JAPANESE_esTreeSize = 'ƒtƒHƒ‹ƒ_ƒTƒCƒY';
  JAPANESE_esAllSubDir = '‚ÆˆÈ‰º‘S‚Ä‚ÌƒTƒuƒtƒHƒ‹ƒ_‚ÌƒTƒCƒY ';
  //>880
//------------------------------------------
//               Korean
//------------------------------------------
  //885<
  //LsFileListview28 Column Caption
  KOREAN_ColIdName   = 'ÀÌ¸§';
  KOREAN_ColIdSize   = 'Å©±â';
  KOREAN_ColIdType   = 'Á¾·ù';
  KOREAN_ColIdDate   = '¼öÁ¤ÇÑ ³¯Â¥';
  KOREAN_ColIdAttr   = '¼Ó¼º';
  KOREAN_ColIdHdSize = 'µð½ºÅ© Å©±â';
  KOREAN_ColIdFSpace = '¿©À¯ °ø°£';
  //Drive Type
  KOREAN_DrvTypeStr0 = '¸ð¸§';
  KOREAN_DrvTypeStr1 = 'Á¸ÀçÇÏÁö ¾ÊÀ½';
  KOREAN_DrvTypeStr2 = 'Á¦°Å°¡´É µð½ºÅ© µå¶óÀÌºê';
  KOREAN_DrvTypeStr3 = '°íÁ¤ µð½ºÅ© µå¶óÀÌºê';
  KOREAN_DrvTypeStr4 = '³×Æ®¿÷ µð½ºÅ© µå¶óÀÌºê';
  KOREAN_DrvTypeStr5 = 'CD-ROM µð½ºÅ© µå¶óÀÌºê';
  KOREAN_DrvTypeStr6 = 'RAM µð½ºÅ© µå¶óÀÌºê';
  //LsFileListview28Popup MenuItem Caption
  KOREAN_LvItemID_0  = '¿­±â(&O)';
  KOREAN_LvItemID_1  = 'º¸±â(&V)';
  KOREAN_LvItemID_3  = 'º¸³»±â(&S)';
  KOREAN_LvItemID_5  = 'ÀÚ¸£±â(&X)';
  KOREAN_LvItemID_6  = 'º¹»ç(&C)';
  KOREAN_LvItemID_7  = 'ºÙÀÌ±â(&P)';
  KOREAN_LvItemID_9  = 'ÀÌ¸§ º¯°æ(&R)';
  KOREAN_LvItemID_10 = 'Áö¿ì±â(&D)';
  KOREAN_LvItemID_12 = 'Á¤º¸(&P)';
  KOREAN_LvItemID_14 = '»õ Æú´õ(&N)...';
  KOREAN_LvItemID_30 = 'Æú´õ·Î(&O)...';
  KOREAN_LvItemID_31 = 'Å¬¸³º¸µå·Î ³»¿ë º¹»ç(&K)';
  KOREAN_LvItemID_32 = 'Å¬¸³º¸µå·Î ÆÄÀÏ ÀÌ¸§ º¹»ç(&L)';
  KOREAN_LvItemID_33 = '¹ÙÅÁÈ­¸é¿¡ ¹Ù·Î°¡±â·Î ¸¸µé±â(&S)';
  //LsDirTree21Popup MenuItem Caption
  KOREAN_TvItemID_0  = '»õ Æú´õ(&N)';
  KOREAN_TvItemID_1  = 'Æú´õ ÀÌ¸§ ¹Ù²Ù±â(&R)';
  KOREAN_TvItemID_2  = 'Æú´õ Áö¿ì±â(&D)';
  KOREAN_TvItemID_4  = 'ÀÚ¸£±â(&T)';
  KOREAN_TvItemID_5  = 'º¹»ç(&C)';
  KOREAN_TvItemID_6  = 'ºÙÀÌ±â(&P)';
  KOREAN_TvItemID_8  = 'Æ®¸® Å©±â(&S)';
  KOREAN_TvItemID_9  = 'Æú´õ ³»¿ë(&F)';
  KOREAN_TvItemID_11 = '³×Æ®¿÷ µå¶óÀÌºê ¸¸µé±â(&N)...';
  KOREAN_TvItemID_12 = '³×Æ®¿÷ µå¶óÀÌºê ¿¬°á²÷±â(&U)';
  KOREAN_TvItemID_14 = 'Á¤º¸(&P)';
  //Error words
  KOREAN_ewDrive = 'µå¶óÀÌºê ';  //873
  KOREAN_ewFolder = 'Æú´õ "';  //873
  KOREAN_ewError = '¿¡·¯ - ';
  KOREAN_ewFile = ' ÆÄÀÏ ';
  KOREAN_ewFrom = '¿¡¼­  : ';
  KOREAN_ewTo = '(À¸)·Î     : ';
  KOREAN_ewCancel = 'Ãë¼Ò';
  KOREAN_ewBrowse = '¿­¶÷';
  KOREAN_ewReadOnly = 'ÀÐ±âÀü¿ë';
  KOREAN_ewArchive = 'º¸°ü';
  KOREAN_ewHidden = '¼û±è';
  KOREAN_ewSystem = '½Ã½ºÅÛ';
  //Error strings
  KOREAN_esCannot = '¿¡·¯ - ÇÒ¼ö ¾øÀ½ ';
  KOREAN_esSpecifyDir = 'µð·ºÅä¸®¸¦ ¾Ë·ÁÁÖ¼Å¿ä';
  KOREAN_esInvalidDrvID = '¸ÂÁö ¾Ê´Â µå¶óÀÌºê ID';
  KOREAN_esDrvNotReady = 'µð½ºÅ©°¡ ¾ø°Å³ª ÁØºñµÇÁö ¾Ê¾Ò½À´Ï´Ù';
  KOREAN_esExists = 'ÀÌ¹Ì Á¸ÀçÇÕ´Ï´Ù !';
  KOREAN_esInvalidDirName = '¿¡·¯ - Àß¸øµÈ µð·ºÅä¸® ÀÌ¸§ÀÔ´Ï´Ù';
  KOREAN_esConfirmRename = 'Æú´õÀÇ ÀÌ¸§À» ¹Ù²Ù½Ã°Ú½À´Ï±î -';
  KOREAN_esCannotAddDrv = 'µå¶óÀÌºê¸¦ Ãß°¡ÇÒ ¼ö ¾ø½À´Ï´Ù';
  KOREAN_esNewFolder = '»õ Æú´õ';
  KOREAN_esInvalidChars = 'Æú´õ ÀÌ¸§¿¡ Àß¸øµÈ ¹®ÀÚ°¡ ÀÖ½À´Ï´Ù';
  KOREAN_esNotFound = 'Ã£À» ¼ö ¾ø½À´Ï´Ù';
  KOREAN_esFilesIn = 'ÆÄÀÏ ';
  KOREAN_esFileOpFailed = 'ÆÄÀÏ Ã³¸® ½ÇÆÐ';
  KOREAN_esReadOnly = 'ÀÐ±â Àü¿ë ÆÄÀÏÀÔ´Ï´Ù';
  KOREAN_esNoFileSelected = '¼±ÅÃµÈ ÆÄÀÏÀÌ ¾ø½À´Ï´Ù';
  KOREAN_esSendToFolder = 'Æú´õ·Î º¸³»±â';
  KOREAN_esSendToPath = 'º¸³»±â¸¦ ÇÒ Æú´õ¸¦ ÀÔ·ÂÇØÁÖ¼Å¿ä ';
  KOREAN_esPersistSaveError = '¿¡·¯ - PersistFile.Save ½ÇÆÐ';
  KOREAN_esSetAttr = '¼Ó¼º ¼³Á¤';
  KOREAN_esTreeSize = 'Æ®¸® Å©±â';  //873
  KOREAN_esAllSubDir = '±×¸®°í ¸ðµç ÇÏÀ§ Æú´õ ';  //873
  //>885
//------------------------------------------
//               Polish
//------------------------------------------
  //876<
  //LsFileListview28 Column Caption
  POLISH_ColIdName = 'Nazwa';
  POLISH_ColIdSize = 'Rozmiar';
  POLISH_ColIdType = 'Typ';
  POLISH_ColIdDate = 'Modyfikowany';
  POLISH_ColIdAttr = 'Atrybut';
  POLISH_ColIdHdSize = 'Rozmiar Dysku';
  POLISH_ColIdFSpace = 'Wolne Miejsce';
  //Drive Type
  POLISH_DrvTypeStr0 = 'Nieznany';
  POLISH_DrvTypeStr1 = 'Nie Istnieje';
  POLISH_DrvTypeStr2 = 'Dysk Wymienny';
  POLISH_DrvTypeStr3 = 'Dysk Sta³y';
  POLISH_DrvTypeStr4 = 'Dysk Sieciowy';
  POLISH_DrvTypeStr5 = 'CD-ROM Dysk';
  POLISH_DrvTypeStr6 = 'RAM Dysk';
  //LsFileListView28Popup MenuItem Caption
  POLISH_LvItemID_0 = '&Otwórz';
  POLISH_LvItemID_1 = '&Podgl¹d';
  POLISH_LvItemID_3 = 'Wyœlij &do';
  POLISH_LvItemID_5 = 'Wyt&nij';
  POLISH_LvItemID_6 = '&Kopiuj';
  POLISH_LvItemID_7 = 'Wkl&ej';
  POLISH_LvItemID_9 = '&Zmiana Nazwy';
  POLISH_LvItemID_10 = '&Usuñ';
  POLISH_LvItemID_12 = '&W³aœciwoœci';
  POLISH_LvItemID_14 = 'Nowy &Folder';
  POLISH_LvItemID_30 = 'Nastêpny Folder';
  POLISH_LvItemID_31 = 'Zapisz jako Treœæ';
  POLISH_LvItemID_32 = 'Zapisz jako Nazwa Pliku';
  POLISH_LvItemID_33 = 'Utwórz Skrót na Pulpicie';
  //LsDirTree21Popup MenuItem Caption
  POLISH_TvItemID_0 = 'Nowy &Folder';
  POLISH_TvItemID_1 = 'Zmiana &Nazwy Foldera';
  POLISH_TvItemID_2 = '&Usuñ Folder';
  POLISH_TvItemID_4 = 'Wyt&nij';
  POLISH_TvItemID_5 = '&Kopiuj';
  POLISH_TvItemID_6 = 'Wkl&ej';
  POLISH_TvItemID_8 = 'Poka¿ &Drzewo';
  POLISH_TvItemID_9 = '&Poka¿ Treœæ Foldera';
  POLISH_TvItemID_11 = 'Po³¹cz z Dyskiem Sieciowym';
  POLISH_TvItemID_12 = 'Roz³¹cz Dysk Sieciowy';
  POLISH_TvItemID_14 = '&W³aœciwoœci';
  //Error words
  POLISH_ewDrive = 'Dysk ';
  POLISH_ewFolder = 'Folder "';
  POLISH_ewError = 'B£¥D - ';
  POLISH_ewFile = ' plik ';
  POLISH_ewFrom = 'Z   : ';
  POLISH_ewTo = 'Do   : ';
  POLISH_ewCancel = 'Anuluj';
  POLISH_ewBrowse = 'Przegl¹daj';
  POLISH_ewReadOnly = 'Tylko do Odczytu';
  POLISH_ewArchive = 'Archiwalny';
  POLISH_ewHidden = 'Ukryty';
  POLISH_ewSystem = 'Systemowy';
  //Error strings
  POLISH_esCannot = 'B£¥D - "';
  POLISH_esSpecifyDir = 'Proszê wybraæ miejsce docelowe ';
  POLISH_esInvalidDrvID = 'Nie mogê zidentyfikowaæ dysku, wybierz inny';
  POLISH_esDrvNotReady = 'Ten dysk jest tylko do odczytu';
  POLISH_esExists = 'ju¿ istnieje';
  POLISH_esInvalidDirName = 'B£¥D - ¯le podano œcie¿kê';
  POLISH_esConfirmRename = 'Czy napewno chcesz zmieniæ nazwê wybranego foldera ?';
  POLISH_esCannotAddDrv = 'Nie mogê dodaæ dysku';
  POLISH_esNewFolder = 'Nowy Folder';
  POLISH_esInvalidChars = '¯³a nazwa folderu';
  POLISH_esNotFound = 'nie znalaz³em';
  POLISH_esFilesIn = 'Plików w ';
  POLISH_esFileOpFailed = 'Operacja na pliku nie udana';
  POLISH_esReadOnly = 'Plik " jest tylko do odczytu';
  POLISH_esNoFileSelected = 'Nie wybrano pliku(ów)';
  POLISH_esSendToFolder = 'Wyœlij do nastêpnego foldera ';
  POLISH_esSendToPath = 'Proszê wybraæ œcie¿kê i nacisn¹æ Enter';
  POLISH_esPersistSaveError = 'B³¹d - PersistFile.Zapis nie powiód³ siê';
  POLISH_esSetAttr = 'Dodaj atrybuty';
  POLISH_esTreeSize = 'POKA¯ DRZEWO';
  POLISH_esAllSubDir = 'oraz wszystkie jego Sub-Foldery';
  //>876

//------------------------------------------
//         Brazillian Portuguese
//------------------------------------------
  //873<
  //LsFileListview28 Column Caption
  BRAZ_PORT_ColIdName   = 'Nome';
  BRAZ_PORT_ColIdSize   = 'Tamanho';
  BRAZ_PORT_ColIdType   = 'Tipo';
  BRAZ_PORT_ColIdDate   = 'Modificado';
  BRAZ_PORT_ColIdAttr   = 'Atributos';
  BRAZ_PORT_ColIdHdSize = 'Tamanho total';
  BRAZ_PORT_ColIdFSpace = 'Espaço livre';
  //Drive Type
  BRAZ_PORT_DrvTypeStr0 = 'Desconhecido';
  BRAZ_PORT_DrvTypeStr1 = 'Não existe';
  BRAZ_PORT_DrvTypeStr2 = 'Disco removível';
  BRAZ_PORT_DrvTypeStr3 = 'Disco fixo';
  BRAZ_PORT_DrvTypeStr4 = 'Disco de rede';
  BRAZ_PORT_DrvTypeStr5 = 'CD-ROM';
  BRAZ_PORT_DrvTypeStr6 = 'Disco RAM';
  //LsFileListview28Popup MenuItem Caption
  BRAZ_PORT_LvItemID_0   = '&Abrir';
  BRAZ_PORT_LvItemID_1   = '&Visualizar';
  BRAZ_PORT_LvItemID_3   = 'E&nviar para';
  BRAZ_PORT_LvItemID_5   = 'Cor&tar';
  BRAZ_PORT_LvItemID_6   = '&Copiar';
  BRAZ_PORT_LvItemID_7   = 'Col&ar';
  BRAZ_PORT_LvItemID_9   = '&Renomear';
  BRAZ_PORT_LvItemID_10  = '&Apagar';
  BRAZ_PORT_LvItemID_12  = '&Propriedades';
  BRAZ_PORT_LvItemID_14  = '&Nova pasta ...';
  BRAZ_PORT_LvItemID_30  = 'Para a pasta ...';
  BRAZ_PORT_LvItemID_31  = 'Copiar conteúdo para área de transferência';
  BRAZ_PORT_LvItemID_32  = 'Copiar somente o nome para área de transferência';
  BRAZ_PORT_LvItemID_33  = 'Área de Trabalhao como Atalho';
  //LsDirTree21Popup MenuItem Caption
  BRAZ_PORT_TvItemID_0   = '&Nova pasta';
  BRAZ_PORT_TvItemID_1   = '&Renomear pasta';
  BRAZ_PORT_TvItemID_2   = '&Apagar pasta';
  BRAZ_PORT_TvItemID_4   = 'Cor&tar';
  BRAZ_PORT_TvItemID_5   = '&Copiar';
  BRAZ_PORT_TvItemID_6   = 'Col&ar';
  BRAZ_PORT_TvItemID_8   = 'Ta&manho do diretório';
  BRAZ_PORT_TvItemID_9   = 'C&onteúdo da pasta';
  BRAZ_PORT_TvItemID_11  = 'Con&ectar unidade de rede';
  BRAZ_PORT_TvItemID_12  = 'De&sconectar unidade de rede';
  BRAZ_PORT_TvItemID_14  = '&Propriedades';
  //Error words
  BRAZ_PORT_ewDrive = 'Unidade ';
  BRAZ_PORT_ewFolder = 'Pasta "';
  BRAZ_PORT_ewError = 'ERRO - ';
  BRAZ_PORT_ewFile = ' arquivo  ';
  BRAZ_PORT_ewFrom = 'De  : ';
  BRAZ_PORT_ewTo = 'Para     : ';
  BRAZ_PORT_ewCancel = 'Cancelar';
  BRAZ_PORT_ewBrowse = 'Explorar';
  BRAZ_PORT_ewReadOnly = 'Somente leitura';
  BRAZ_PORT_ewArchive = 'Arquivo';
  BRAZ_PORT_ewHidden = 'Oculto';
  BRAZ_PORT_ewSystem = 'Sistema';
  //Error strings
  BRAZ_PORT_esCannot = 'ERRO - impossível ';
  BRAZ_PORT_esSpecifyDir = 'Por favor especifique uma pasta';
  BRAZ_PORT_esInvalidDrvID = 'Não é uma letra de unidade válida';
  BRAZ_PORT_esDrvNotReady = 'Não há um disco na unidade ' +
                          'ou a unidade não está preparada.';
  BRAZ_PORT_esExists = ' já existe !';
  BRAZ_PORT_esInvalidDirName = 'Erro - Nome da pasta não é válido';
  BRAZ_PORT_esConfirmRename = 'Deseja renomear a pasta selecionada';
  BRAZ_PORT_esCannotAddDrv = 'Não é possível adicionar unidades';
  BRAZ_PORT_esNewFolder = 'Nova pasta';
  BRAZ_PORT_esInvalidChars = 'o nome da pasta contém caracteres inválidos.';
  BRAZ_PORT_esNotFound = 'não encontrado';
  BRAZ_PORT_esFilesIn = 'Arquivos em ';
  BRAZ_PORT_esFileOpFailed = 'Operação com arquivos falhou';
  BRAZ_PORT_esReadOnly = 'Somente para leitura';
  BRAZ_PORT_esNoFileSelected = 'Não há pasta(s) selecionada(s)';
  BRAZ_PORT_esSendToFolder = 'Enviar para a pasta';
  BRAZ_PORT_esSendToPath = 'Por favor digite caminho da pasta de destino';
  BRAZ_PORT_esPersistSaveError = 'Erro - PersistFile.Save falhou';
  BRAZ_PORT_esSetAttr = 'Setar atributos';
  BRAZ_PORT_esTreeSize = 'Tamanho do diretório';
  BRAZ_PORT_esAllSubDir = 'e todas as sub-pastas ';
  //>873
//MPX1
//------------------------------------------
//   Slovenian
//------------------------------------------
  //881<
  //LsFileListview28 Column Caaption
  SLO_ColIdName   = 'Ime';
  SLO_ColIdSize   = 'Velikost';
  SLO_ColIdType   = 'Tip';
  SLO_ColIdDate   = 'Popravljeno';
  SLO_ColIdAttr   = 'Attr';
  SLO_ColIdHdSize = 'Velikost diska';
  SLO_ColIdFSpace = 'Prazen prostor';
  //Drive Type
  SLO_DrvTypeStr0 = 'Neznan';
  SLO_DrvTypeStr1 = 'Ne obstaja';
  SLO_DrvTypeStr2 = 'Odstranljiv disk';
  SLO_DrvTypeStr3 = 'Trdi disk';
  SLO_DrvTypeStr4 = 'Omrežni disk';
  SLO_DrvTypeStr5 = 'CD-ROM';
  SLO_DrvTypeStr6 = 'RAM disk';
  //LsFileListview28Popup MenuItem Caption
  SLO_LvItemID_0  = '&Odpri';
  SLO_LvItemID_1  = '&Poglej';
  SLO_LvItemID_3  = 'Poš&lji na';
  SLO_LvItemID_5  = '&Reži';
  SLO_LvItemID_6  = '&Kopiraj';
  SLO_LvItemID_7  = 'Pr&ilepi';
  SLO_LvItemID_9  = 'Pr&eimenuj';
  SLO_LvItemID_10 = '&Briši';
  SLO_LvItemID_12 = '&Lastnosti';
  SLO_LvItemID_14 = '&Nova mapa...';
  SLO_LvItemID_30 = 'katerakoli mapa ...';
  SLO_LvItemID_31 = 'Odlagališèe kot vsebina';
  SLO_LvItemID_32 = 'Odlagališèe kot ime';
  SLO_LvItemID_33 = 'Namizje kot bljižnica';
  //LsDirTree21Popup MenuItem Caption
  SLO_TvItemID_0  = '&Nova mapa';
  SLO_TvItemID_1  = '&Preimenuj mapo';
  SLO_TvItemID_2  = '&Briši mapo';
  SLO_TvItemID_4  = '&Reži';
  SLO_TvItemID_5  = '&Kopiraj';
  SLO_TvItemID_6  = 'Pr&ilepi';
  SLO_TvItemID_8  = 'Velikost &drevesa';
  SLO_TvItemID_9  = '&Vsebina mape';
  SLO_TvItemID_11 = 'Prikl&juèi omrežni disk ...';
  SLO_TvItemID_12 = '&Odkljuèi omrežni disk ...';
  SLO_TvItemID_14 = '&Lastnosti';
  //Error words
  SLO_ewDrive = 'Disk ';  //873
  SLO_ewFolder = 'mapa "';  //873
  SLO_ewError = 'NAPAKA - ';
  SLO_ewFile = ' datoteka ';
  SLO_ewFrom = 'Iz  : ';
  SLO_ewTo = 'Na     : ';
  SLO_ewCancel = 'Prekini';
  SLO_ewBrowse = 'Prebrskaj';
  SLO_ewReadOnly = 'SamoBranje';
  SLO_ewArchive = 'Arhiva';
  SLO_ewHidden = 'Skrito';
  SLO_ewSystem = 'Sistem';
  //Error strings
  SLO_esCannot = 'NAPAKA - Ne morem ';
  SLO_esSpecifyDir = 'Prosim doloèite mapo';
  SLO_esInvalidDrvID = 'Nepravilen disk';
  SLO_esDrvNotReady = 'Ni diskete ali pa je disk nepripravljen';
  SLO_esExists = 'že obstoja !';
  SLO_esInvalidDirName = 'NAPAKA - napaèno ime';
  SLO_esConfirmRename = 'Ali želite preimenovati izbrano mapo?';
  SLO_esCannotAddDrv = 'Diskov ne morem dodati!';
  SLO_esNewFolder = 'Nova mapa';
  SLO_esInvalidChars = 'Ime vsebuje neveljavne znake';
  SLO_esNotFound = 'ne najdem';
  SLO_esFilesIn = 'Datoteke v ';
  SLO_esFileOpFailed = 'Operacija se ni izvedla';
  SLO_esReadOnly = 'Ozneèena je samo za branje';
  SLO_esNoFileSelected = 'Niste izbrali datotek';
  SLO_esSendToFolder = 'Pošlji v katerokoli mapo';
  SLO_esSendToPath = 'Prosim doloèite pot.';
  SLO_esPersistSaveError = 'NAPAKA - PersistFile.Save failed';
  SLO_esSetAttr = 'Postavi atributr';
  SLO_esTreeSize = 'Velikost drevesa';  //873
  SLO_esAllSubDir = 'in vseh podmapah ';  //873
  //>881
//------------------------------------------
//               Slovak
//------------------------------------------
  //876<
  //LsFileListview28 Column Caption
  SLOVAK_ColIdName   = 'Názov';
  SLOVAK_ColIdSize   = 'Ve¾kos';
  SLOVAK_ColIdType   = 'Typ';
  SLOVAK_ColIdDate   = 'Zmenené';
  SLOVAK_ColIdAttr   = 'Atribúty';
  SLOVAK_ColIdHdSize = 'Kapacita';
  SLOVAK_ColIdFSpace = 'Vo¾né miesto';
  //Drive Type
  SLOVAK_DrvTypeStr0 = 'Neznámy typ';
  SLOVAK_DrvTypeStr1 = 'Neexistuje';
  SLOVAK_DrvTypeStr2 = 'Vymenite¾ný disk';
  SLOVAK_DrvTypeStr3 = 'Pevný disk';
  SLOVAK_DrvTypeStr4 = 'Sieový disk';
  SLOVAK_DrvTypeStr5 = 'CD-ROM';
  SLOVAK_DrvTypeStr6 = 'RAM disk';
  //LsFileListview28Popup MenuItem Caption
  SLOVAK_LvItemID_0  = '&Otvori';
  SLOVAK_LvItemID_1  = '&Zobrazi';
  SLOVAK_LvItemID_3  = 'Odos&la kam';
  SLOVAK_LvItemID_5  = '&Vystrihnú';
  SLOVAK_LvItemID_6  = '&Kopírova';
  SLOVAK_LvItemID_7  = '&Prilepi';
  SLOVAK_LvItemID_9  = 'Pre&menova';
  SLOVAK_LvItemID_10 = 'O&dstráni';
  SLOVAK_LvItemID_12 = 'Vl&astnosti';
  SLOVAK_LvItemID_14 = '&Nový prieèinok...';
  SLOVAK_LvItemID_30 = 'Prieèinok ...';
  SLOVAK_LvItemID_31 = 'Schránka - obsah súboru';
  SLOVAK_LvItemID_32 = 'Schránka - názov súboru';
  SLOVAK_LvItemID_33 = 'Pracovná plocha - vytvori odkaz';
  //LsDirTree21Popup MenuItem Caption
  SLOVAK_TvItemID_0  = '&Nový prieèinok';
  SLOVAK_TvItemID_1  = 'Pre&menova prieèinok';
  SLOVAK_TvItemID_2  = 'O&dstráni prieèinok';
  SLOVAK_TvItemID_4  = '&Vystrihnú';
  SLOVAK_TvItemID_5  = '&Kopírova';
  SLOVAK_TvItemID_6  = '&Prilepi';
  SLOVAK_TvItemID_8  = 'Ve¾kos pod&stromu';
  SLOVAK_TvItemID_9  = '&Obsah prieèinka';
  SLOVAK_TvItemID_11 = 'Pripo&ji sieový disk ...';
  SLOVAK_TvItemID_12 = 'Odpo&ji sieový disk';
  SLOVAK_TvItemID_14 = 'Vl&astnosti';
  //Error words
  SLOVAK_ewDrive = 'Jednotka ';  //873
  SLOVAK_ewFolder = 'Prieèinok "';  //873
  SLOVAK_ewError = 'CHYBA - ';
  SLOVAK_ewFile = ' súbor ';
  SLOVAK_ewFrom = 'Z  : ';
  SLOVAK_ewTo = 'Do     : ';
  SLOVAK_ewCancel = 'Zruši';
  SLOVAK_ewBrowse = 'Preh¾adáva';
  SLOVAK_ewReadOnly = 'Iba na èítanie';
  SLOVAK_ewArchive = 'Archívny';
  SLOVAK_ewHidden = 'Skrytý';
  SLOVAK_ewSystem = 'Systémový';
  //Error strings
  SLOVAK_esCannot = 'CHYBA - Nie je možné ';
  SLOVAK_esSpecifyDir = 'Prosím upresnite adresár';
  SLOVAK_esInvalidDrvID = 'ID jednotky nie je správne';
  SLOVAK_esDrvNotReady = 'Jednotka nie je pripravená, alebo sa v nej nenachádza disk';
  SLOVAK_esExists = 'už existuje !';
  SLOVAK_esInvalidDirName = 'Chyba - nepovolený názov adresára';
  SLOVAK_esConfirmRename = 'Želáte si premenova vybraný adresár';
  SLOVAK_esCannotAddDrv = 'Nie je možné prida jednotky';
  SLOVAK_esNewFolder = 'Nový prieèinok';
  SLOVAK_esInvalidChars = 'Názov prieèinka obsahuje nepovolené znaky';
  SLOVAK_esNotFound = 'nebol nájdený';
  SLOVAK_esFilesIn = 'Súbory v ';
  SLOVAK_esFileOpFailed = 'Operácia so súborom zlyhala';
  SLOVAK_esReadOnly = 'Je to iba na èítanie';
  SLOVAK_esNoFileSelected = 'Nebol vybraný žiadny súbor';
  SLOVAK_esSendToFolder = 'Odosla do prieèinka';
  SLOVAK_esSendToPath = 'Prosím vložte celú cestu k zvolenému prieèinku';
  SLOVAK_esPersistSaveError = 'Chyba - zlyhalo PersistFile.Save';
  SLOVAK_esSetAttr = 'Nastavenie atribútov';
  SLOVAK_esTreeSize = 'VE¼KOS PODSTROMU';  //873
  SLOVAK_esAllSubDir = 'a všetky jeho podadresáre ';  //873
  //>876

//------------------------------------------
//               Spanish
//------------------------------------------
  //872<
  //LsFileListview28 Column Caption
  SPANISH_ColIdName   = 'Nombre';
  SPANISH_ColIdSize   = 'Tamaño';
  SPANISH_ColIdType   = 'Tipo';
  SPANISH_ColIdDate   = 'Modificado';
  SPANISH_ColIdAttr   = 'Attributos';
  SPANISH_ColIdHdSize = 'Tamaño total';
  SPANISH_ColIdFSpace = 'Espacio libre';
  //Drive Type
  SPANISH_DrvTypeStr0 = 'Desconocido';
  SPANISH_DrvTypeStr1 = 'No existe';
  SPANISH_DrvTypeStr2 = 'Disco extraible';
  SPANISH_DrvTypeStr3 = 'Disco fijo';
  SPANISH_DrvTypeStr4 = 'Disco de red';
  SPANISH_DrvTypeStr5 = 'CD-ROM';
  SPANISH_DrvTypeStr6 = 'Disco RAM';
  //LsFileListview28Popup MenuItem Caption
  SPANISH_LvItemID_0   = '&Abrir';
  SPANISH_LvItemID_1   = '&Ver';
  SPANISH_LvItemID_3   = 'E&nviar a';
  SPANISH_LvItemID_5   = 'Cor&tar';
  SPANISH_LvItemID_6   = '&Copiar';
  SPANISH_LvItemID_7   = '&Pegar';
  SPANISH_LvItemID_9   = '&Renombrar';
  SPANISH_LvItemID_10  = '&Borrar';
  SPANISH_LvItemID_12  = '&Propiedades';
  SPANISH_LvItemID_14  = '&Nueva carpeta ...';
  SPANISH_LvItemID_30  = 'Una carpeta ...';
  SPANISH_LvItemID_31  = 'Copiar contenido al portapapeles';
  SPANISH_LvItemID_32  = 'Copiar nombre del fichero al portapapeles';
  SPANISH_LvItemID_33  = 'Acceso directo al escritorio';
  //LsDirTree21Popup MenuItem Caption
  SPANISH_TvItemID_0   = '&Nueva carpeta';
  SPANISH_TvItemID_1   = '&Renombrar carpeta';
  SPANISH_TvItemID_2   = '&Borrar carpeta';
  SPANISH_TvItemID_4   = 'Cor&tar';
  SPANISH_TvItemID_5   = '&Copiar';
  SPANISH_TvItemID_6   = '&Pegar';
  SPANISH_TvItemID_8   = 'Ta&maño del árbol';
  SPANISH_TvItemID_9   = 'C&ontenido de la carpeta';
  SPANISH_TvItemID_11  = 'Con&ectar unidad de red';
  SPANISH_TvItemID_12  = 'De&sconectar unidad de red';
  SPANISH_TvItemID_14  = '&Propiedades';
  //Error words
  SPANISH_ewDrive = 'Unidad ';   //873
  SPANISH_ewFolder = 'Carpeta "';   //873
  SPANISH_ewError = 'ERROR - ';
  SPANISH_ewFile = ' el fichero  ';
  SPANISH_ewFrom = 'De  : ';
  SPANISH_ewTo = 'A     : ';
  SPANISH_ewCancel = 'Cancelar';
  SPANISH_ewBrowse = 'Explorar';
  SPANISH_ewReadOnly = 'Sólo lectura';
  SPANISH_ewArchive = 'Archivo';
  SPANISH_ewHidden = 'Oculto';
  SPANISH_ewSystem = 'Sistema';
  //Error strings
  SPANISH_esCannot = 'ERROR - No se puede ';
  SPANISH_esSpecifyDir = 'Por favor especifique un directorio';
  SPANISH_esInvalidDrvID = 'No es una letra de unidad válida';
  SPANISH_esDrvNotReady = 'No hay disco en la unidad ' +
                          'o la unidad no está preparada.';
  SPANISH_esExists = '" ya existe !';
  SPANISH_esInvalidDirName = 'Error - nombre de carpeta no válido';
  SPANISH_esConfirmRename = 'Quiere renombrar la carpeta seleccionada';
  SPANISH_esCannotAddDrv = 'No se pueden añadir unidades';
  SPANISH_esNewFolder = 'Nueva carpeta';
  SPANISH_esInvalidChars = 'El nombre de la carpeta contiene caracteres inválidos.';
  SPANISH_esNotFound = 'no encontrado';
  SPANISH_esFilesIn = 'Ficheros en ';
  SPANISH_esFileOpFailed = 'La operación con el fichero ha fallado';
  SPANISH_esReadOnly = 'Es de sólo lectura';
  SPANISH_esNoFileSelected = 'No hay fichero(s) seleccionado(s)';
  SPANISH_esSendToFolder = 'Enviar a carpeta';
  SPANISH_esSendToPath = 'Por favor intruduzca la carpeta donde enviar';
  SPANISH_esPersistSaveError = 'Error - PersistFile.Save falló';
  SPANISH_esSetAttr = 'Poner atributos';
  SPANISH_esTreeSize = 'TAMAÑO DEL ÁRBOL';  //873
  SPANISH_esAllSubDir = 'y todas sus subcarpetas ';  //873
  //>872

//------------------------------------------
//               Swedish
//------------------------------------------
  //882<
  //LsFileListview28 Column Caption
  SWEDISH_ColIdName   = 'Namn';
  SWEDISH_ColIdSize   = 'Storlek';
  SWEDISH_ColIdType   = 'Typ';
  SWEDISH_ColIdDate   = 'Modifierad';
  SWEDISH_ColIdAttr   = 'Attr';
  SWEDISH_ColIdHdSize = 'Disk Storlek';
  SWEDISH_ColIdFSpace = 'Fritt utrymma';
  //Drive Type
  SWEDISH_DrvTypeStr0 = 'Okänd';
  SWEDISH_DrvTypeStr1 = 'Finns Ej';
  SWEDISH_DrvTypeStr2 = 'Flyttbar Disk';
  SWEDISH_DrvTypeStr3 = 'Fast Disk';
  SWEDISH_DrvTypeStr4 = 'Nätverks Disk';
  SWEDISH_DrvTypeStr5 = 'CD-ROM Disk';
  SWEDISH_DrvTypeStr6 = 'RAM Disk';
  //LsFileListview28Popup MenuItem Caption
  SWEDISH_LvItemID_0  = 'Ö&ppna';
  SWEDISH_LvItemID_1  = '&Visa';
  SWEDISH_LvItemID_3  = 'Sk&icka Till';
  SWEDISH_LvItemID_5  = 'Klipp U&t';
  SWEDISH_LvItemID_6  = '&Kopiera';
  SWEDISH_LvItemID_7  = 'K&listra In';
  SWEDISH_LvItemID_9  = '&Byt Namn';
  SWEDISH_LvItemID_10 = '&Radera';
  SWEDISH_LvItemID_12 = '&Egenskaper';
  SWEDISH_LvItemID_14 = '&Ny Katalog...';
  SWEDISH_LvItemID_30 = 'Valfri Katalog ...';
  SWEDISH_LvItemID_31 = 'Klipboard som Innehåll';
  SWEDISH_LvItemID_32 = 'Klippbord som FileNamn';
  SWEDISH_LvItemID_33 = 'Skrivbord som Genväg';
  //LsDirTree21Popup MenuItem Caption
  SWEDISH_TvItemID_0  = '&Ny Katalog';
  SWEDISH_TvItemID_1  = '&Döp om Katalog';
  SWEDISH_TvItemID_2  = '&Radera Katalog';
  SWEDISH_TvItemID_4  = 'Klip&p ut';
  SWEDISH_TvItemID_5  = '&Kopiera';
  SWEDISH_TvItemID_6  = '&Klistra IN';
  SWEDISH_TvItemID_8  = 'Träd &Storlek';
  SWEDISH_TvItemID_9  = '&Katalog Innehåll';
  SWEDISH_TvItemID_11 = '&Koppla Nätverks Disk ...';
  SWEDISH_TvItemID_12 = 'Frigör Nätv&erksdisk';
  SWEDISH_TvItemID_14 = '&Egenskaper';
  //Error words
  SWEDISH_ewDrive = 'Enhet ';
  SWEDISH_ewFolder = 'Katalog "';
  SWEDISH_ewError = 'FEL - ';
  SWEDISH_ewFile = ' fil ';
  SWEDISH_ewFrom = 'Från  : ';
  SWEDISH_ewTo = 'Till     : ';
  SWEDISH_ewCancel = 'Avbryt';
  SWEDISH_ewBrowse = 'Sök';
  SWEDISH_ewReadOnly = 'Skrivskyddad';
  SWEDISH_ewArchive = 'Arkiv';
  SWEDISH_ewHidden = 'Dold';
  SWEDISH_ewSystem = 'System';
  //Error strings
  SWEDISH_esCannot = 'FEL - Kan Inte ';
  SWEDISH_esSpecifyDir = 'Ange en Katalog';
  SWEDISH_esInvalidDrvID = 'Inte ett Giltigt Enhets ID';
  SWEDISH_esDrvNotReady = 'Det finns ingen disk i enheten, eller så är inte enheten klar';
  SWEDISH_esExists = 'finns redan !';
  SWEDISH_esInvalidDirName = 'Fel - Ogiltigt Katalog Namn';
  SWEDISH_esConfirmRename = 'Vill du byta namn på den valda katalogen';
  SWEDISH_esCannotAddDrv = 'Kan inte lägga till enheter';
  SWEDISH_esNewFolder = 'Ny Katalog';
  SWEDISH_esInvalidChars = 'Katalognamnet innehåller ogiltiga tecken';
  SWEDISH_esNotFound = 'hittar inte';
  SWEDISH_esFilesIn = 'Filer i ';
  SWEDISH_esFileOpFailed = 'Filåtgärd avbröts';
  SWEDISH_esReadOnly = 'File är Skrivskyddad';
  SWEDISH_esNoFileSelected = 'Inga Fil(er) valda';
  SWEDISH_esSendToFolder = 'Skicka till valfri katalog';
  SWEDISH_esSendToPath = 'ANge sökvägen för Skicka Till';
  SWEDISH_esPersistSaveError = 'Fel - BeståendeFile.Spara kunde inte utföras';
  SWEDISH_esSetAttr = 'Sätt Attribut';
  SWEDISH_esTreeSize = 'TRÄD STORLEK';
  SWEDISH_esAllSubDir = 'och alla underkataloger ';
  //>882
//------------------------------------------
//               Turkish
//------------------------------------------
  //886<
  //LsFileListview28 Column Caption
  TURKISH_ColIdName   = 'Ýsim';
  TURKISH_ColIdSize   = 'Boyut';
  TURKISH_ColIdType   = 'Tip';
  TURKISH_ColIdDate   = 'Deðiþmiþ';
  TURKISH_ColIdAttr   = 'Attr';
  TURKISH_ColIdHdSize = 'Disk Boyutu';
  TURKISH_ColIdFSpace = 'Boþ Alan';
  //Drive Type
  TURKISH_DrvTypeStr0 = 'Bilinmeyen';
  TURKISH_DrvTypeStr1 = 'Yok';
  TURKISH_DrvTypeStr2 = 'Çýkartýlabilir Disk';
  TURKISH_DrvTypeStr3 = 'Sabit Disk';
  TURKISH_DrvTypeStr4 = 'Network Disk';
  TURKISH_DrvTypeStr5 = 'CD-ROM Disk';
  TURKISH_DrvTypeStr6 = 'RAM Disk';
  //LsFileListview28Popup MenuItem Caption
  TURKISH_LvItemID_0  = '&Aç';
  TURKISH_LvItemID_1  = '&Bak';
  TURKISH_LvItemID_3  = '&Gönder';
  TURKISH_LvItemID_5  = 'K&es';
  TURKISH_LvItemID_6  = '&Kopyala';
  TURKISH_LvItemID_7  = '&Yapýþtýr';
  TURKISH_LvItemID_9  = 'Ad &Deðiþtir';
  TURKISH_LvItemID_10 = '&Sil';
  TURKISH_LvItemID_12 = '&Özellikler';
  TURKISH_LvItemID_14 = '&Yeni Dizin...';
  TURKISH_LvItemID_30 = 'Bir Dizine ...';
  TURKISH_LvItemID_31 = 'Not defteri içeriði';
  TURKISH_LvItemID_32 = 'Not defteri dosya adý';
  TURKISH_LvItemID_33 = 'Masaüstüne Kýsayol';
  //LsDirTree21Popup MenuItem Caption
  TURKISH_TvItemID_0  = '&Yeni Dizin';
  TURKISH_TvItemID_1  = '&Dizin Adýný Deðiþtir';
  TURKISH_TvItemID_2  = 'Dizin &Sil';
  TURKISH_TvItemID_4  = 'K&es';
  TURKISH_TvItemID_5  = '&Kopyala';
  TURKISH_TvItemID_6  = '&Yapýþtýr';
  TURKISH_TvItemID_8  = 'Agaç &Boyutu';
  TURKISH_TvItemID_9  = 'Dizin &Ýçeriði';
  TURKISH_TvItemID_11 = '&Að Sürücüsüne Baðlan ...';
  TURKISH_TvItemID_12 = 'Að S&ürücüsü Baðlantýsýný Kes';
  TURKISH_TvItemID_14 = '&Özellikler';
  //Error words
  TURKISH_ewDrive = 'Sürücü ';  //873
  TURKISH_ewFolder = 'Dizin "';  //873
  TURKISH_ewError = 'HATA - ';
  TURKISH_ewFile = ' Dosra ';
  TURKISH_ewFrom = 'From  : ';
  TURKISH_ewTo = 'To     : ';
  TURKISH_ewCancel = 'Vazgeç';
  TURKISH_ewBrowse = 'Listele';
  TURKISH_ewReadOnly = 'Salt Okunur';
  TURKISH_ewArchive = 'Arþiv';
  TURKISH_ewHidden = 'Gizli';
  TURKISH_ewSystem = 'Sistem';
  //Error strings
  TURKISH_esCannot = 'HATA - Yapýlamayan iþlem ';
  TURKISH_esSpecifyDir = 'Lütfen Dizin Belirtin';
  TURKISH_esInvalidDrvID = 'Geçerli bir sürücü harfi deðil';
  TURKISH_esDrvNotReady = 'Disk veya sürücü hazýr deðil';
  TURKISH_esExists = 'Zaten Var !';
  TURKISH_esInvalidDirName = 'Hata - Yanlýþ dizin adý';
  TURKISH_esConfirmRename = 'Seçili dizin adýný deðiþtirmek istiyormusunuz';
  TURKISH_esCannotAddDrv = 'Sürücü eklenemedi';
  TURKISH_esNewFolder = 'Yeni dizin';
  TURKISH_esInvalidChars = 'Diizin adý içerisinde yanlýþ harf(ler) var';
  TURKISH_esNotFound = 'Bulunamadý';
  TURKISH_esFilesIn = 'Dosyalar içerisinde ';
  TURKISH_esFileOpFailed = 'Dosya iþlemi Hatalý';
  TURKISH_esReadOnly = 'Dosya Salt Okunur';
  TURKISH_esNoFileSelected = 'Dosya Seçilmedi';
  TURKISH_esSendToFolder = 'Bir dizin gönder';
  TURKISH_esSendToPath = 'Lütfen gönderilecek dizin adýný girin';
  TURKISH_esPersistSaveError = 'Hata - PersistFile. Kaydetme yapýlamadý';
  TURKISH_esSetAttr = 'Dosya niteliklerini ayarla';
  TURKISH_esTreeSize = 'AÐAÇ BOYUTU';
  TURKISH_esAllSubDir = 've bütün alt dizinler ';
  //>886


implementation

end.

