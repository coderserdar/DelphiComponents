{$I fsdefine.inc}

Unit uconsts;

Interface

Uses
  messages,
  fsllbase,
  fsfuninterp;

Const
  FileTypes: Array[TffFileType] Of String[6] = (//was string[20]
    'Base',
    'Index',
    'BLOB',
    'SYSTEM');

  IndexTypes: Array[TfsIndexType] Of String[4] = (//was string[20]
    'Comp',
    'Exp',
    '',
    '',
    '');
  fsDataCompLevel: Array[TDataCompLevel] Of String[10] = ('BLNONE', 'BLFASTEST', 'BLDEFAULT', 'BLMAX');
  fsDefaultUpdate: Array[TDefaultUpdate] Of String[10] = ('DUNORMAL', 'DUIFNULL', 'DUALWAYS');
  fsRound: Array[TRound] of String[13]=('rNone', 'rMathematical', 'rMatAfter1', 'rMatAfter2', 'rMatAfter3',
    'rMatAfter4', 'rMatAfter5', 'rMatAfter6', 'rMatAfter7', 'rMatAfter8', 'rMatAfter9');
Const
  ffeNetTimeout = 4000;
  ffeRegistrySubKey = '\Explorer';

Const
  ffm_Close = WM_USER + $200;
  { Used to close a form when a failure occurs during FormShow. }

Const
  oeFFEBaseError = 1;
  oeInvalidFieldName = oeFFEBaseError + 0;
  oeDuplicateFieldName = oeFFEBaseError + 1;
  oeMissingFieldName = oeFFEBaseError + 2;
  oeInvalidIndexName = oeFFEBaseError + 3;
  oeDuplicateIndexName = oeFFEBaseError + 4;
  oeMissingIndexName = oeFFEBaseError + 5;
  oeDuplicateFileExtension = oeFFEBaseError + 6;
  oeInvalidFileExtension = oeFFEBaseError + 7;
  oeInvalidFieldUnits = oeFFEBaseError + 8;
  oeInvalidIndexKeyLength = oeFFEBaseError + 9;
  oeMaximumIndexKeyLength = oeFFEBaseError + 10;

  { Help contexts }
Const
  hcMainOutline = 110;
  hcAddDatabaseDlg = 200;
  hcDefineNewTableDlg = 210;
  hcRegisteredServersDlg = 220;
  hcRedefineTableDlg = 230;
  hcViewTableDlg = 240;
  hcImportDataDlg = 250;

Implementation

End.

