unit Res;

interface

resourcestring
  {main}
  sAppName = 'Hex Editor';
  sNeedSaveQuestion = 'File "%s" has been changed. Do you want to save it?';
  sStatusDigit = 'Digit: %d';
  sStatusPosDec = 'Pos: %d';
  sStatusPosHex = 'Pos: %x';
  sModified = 'Modified';
  sOverwrite = 'Overwrite';
  sInsert = 'Insert';
  sNoname = 'new file';
  sReplaceQuestion = 'Replace this occurence of "%s"?';
  sReplace = '&Replace';
  sReplaceAll = 'Replace &all';
  sReplaceSkip = '&Skip';
  sPrintedPageAndCopy = 'Printing page %d from %d. Actual copy: %d from %d.';

  {printsetup}
  sPSAllPages = 'All pages (%d)';

  {errors}
  sErrOpenError = 'Can''t open file "%s".';
  sErrSaveError = 'Can''t save file "%s".';
  sErrTextNotFound = 'The text "%s" was not found.';
  sErrNoDigitsInText = 'The text "%s" can''t be interpreted as hexadecimal digits.';
  sErrIntegerValue = 'Bad integer value.';
  sErrIntegerValueOutOfRange = 'Integer value out of range %d to %d.';
  sErrNoPrinterInstalled = 'No printer is installed on this computer.';

implementation

end.
