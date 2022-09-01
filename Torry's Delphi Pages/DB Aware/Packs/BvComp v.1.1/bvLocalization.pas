unit bvLocalization;

interface

resourcestring
      StrErrorChangeLevel='Error of level change!'+#13+'Database is not defined!';
      StrErrorCannotUnLock='Cannot unlock table';
      StrErrorCannotReadState='cannot read table state!';
      StrErrorCannotLock='Cannot lock table';

      StrErrorTableIsClosed='Table is closed.';
      StrErrorCannotWriteToThisFormat= 'Cannot write for this format';
      StrErrorCannotSaveData='Cannot save data';

      StrErrorNotDefinedGrid='The grid is not defined';
      StrErrorNotDefinedObject='The object, by which the grid will be converted, is not defined.';
      StrErrorNotDefinedTable='The table, by which the grid will be converted, is not defined.';
      StrErrorNotDefinedSource='The source table is not defined!';

      StrErrorForConfirmPasswords='The new and confirm passwords do not match.'+#13+#13
                 +'Try again.';

      StrErrorNotFoundFile='The file is not found';

      StrContinue='continue';

      StrSavingTable='Saving of the table...';
      StrCopyingOfFiles='Copying of files';
      StrCopying='Copying';


      /////////////
           
      StrBookMarks='Bookmarks';

      StrOkOperation='The operation is finished successfully.';

      StrBDEAlias='BDE alias';
      StrTable='Table';

      StrNewPassword='New password';
      StrMaskOfPassword='For protection the password need to be masked';
      StrConfirmPassword='Confirm password';
      StrChangePassword='Change password';
      StrChangePasswordForm='Changing password in tables';

      StrEditorCaption='Grid editor';

      StrDelete='Delete';
      StrInsert='Insert';
      StrReadOnly='Read only';

      StrIncFont='Increment font';
      StrDecFont='Decrement font';

      StrByDefault='By default';
      StrCancel='Cancel';
      StrOptions='Options';
      StrColumns='Columns';
      StrStrippedRows='Stripped rows';
      StrFixedCols='Fixed columns';
      StrTitleMinHeight='Minimal height of the title';
      StrCellRows='Height of the cell';

      StrEnter2Tab='Use ENTER instead TAB';
      StrTitle='Title';
      StrContents='Contents';
      StrColor='Color';
      StrWindowColor='window color';
      StrEach='every';
      StrY='';
      StrAlwaysShowEditor='Always show editor';
      StrAlwaysShowSelection='Always show selection';
      StrCellHint='The hint for cells, in which their contents is not located';
      StrHorzLines='Draw horizontal simple lines';
      StrVertLines='Draw vertical simple lines';
      StrIndicator='Draw indicator';
      StrTabStop='Tab stop';
      StrMultiSelect='Selection of several rows';
      StrSelectRows='Select rows';
      StrTitleHint='Draw title';


resourcestring
      BoolTrueText='Yes';
      BoolFalseText='No';

resourcestring
      StrLeftAlignMent='Left';
      StrCenterAlignMent='Center';
      StrRightAlignMent='Right';

resourcestring
      StrField='Field';
      StrColumn='Column';
      StrWidth='Width';
      StrVisible='Visible';
      StrAlignment='Alignment';
      StrTitleAlignment='Title alignment';
      StrwhatNumber='Invalid number';
      StrFutureSaving='The modifications of  properties will function only after reloading of the form';

resourcestring
      StrConfirm='Confirmation';
      StrConfirmOperation='Confirm operation';
      StrYes='Yes';
      StrNO='No';

resourcestring
      cftPrinter='To send immediately on the printer?';
      cfButtOk='Printer';
      cfButtCancel='Screen';

resourcestring
      StrYesForAll='Yes, for all!';

resourcestring
      StrCopyTab2TabCaption='Copying of the tables  (<--)';

      StrGetVarCaption='Restore fields';
      StrSetVarCaption='Save fields';
      StrConfirmChangesInFieldMap='To save modifications in fields map?';

      StrAccept='Accept';
      StrCompSelCols='To fasten selected columns';
      StrUnCompSelCols='to unfasten selected columns';

      StrClearAll='Clear all';
      StrVar='Variant:';
      StrFieldsInSource='Fields in source';
      StrFieldsInDestination='Fields in destination';
      StrChoiseFields='Choose fields';
      StrFilter='Filter';

resourcestring
      StrErrorReadProperties= 'Error of checkup of table properties';
      StrMessage='Message';
      StrQuickPrint='Quick print';
      StrSaveToFile='Save to file';

resourcestring
      strSeeList='List viewer';

      StrNotFound='Nothing has found';
      StrFinderCaption='Find text';
      StrTextCaption='Text:';
      StrOneColumnCaption='Only on the given column';
      StrWholeWordsOnly='Whole words only';
      StrMatchCase='Match registre';
      StrDirection='Direction';
      StrUp='Up';
      StrDown='Down';
      StrFind='Find';

      StrError='Error';

      StrNewColumn='New column';
      StrChoiceField='choose the field:';

      StrSaveColumns='Save columns:';
      StrSelectAll='Select all';
      StrSelectNone='Select none';
      StrShowResult='Show the result table';
      StrUseBDEDriver='Use BDE Driver';



      StrTitleOfColumn='Title of the column';
      StrRestoreProperties='load properties';
      StrVersion='table version';

      StrOpenedFrom='opened from ';
      StrSavingToExcel='Saving to Excel';

      StrCheckPath='Check up the path!';
      StrPrintFile='Print file';
      StrConfirmReplaceFile='Replace file?';
      StrConfirmClearFile='Clear file?';

      StrSeeDoc='  Document viewer';
      StrFile='File';
      StrClose='Close';
      StrOpen='Open';
      StrClear='Clear';
      StrSave='Save';
      StrPrint='Print';
      StrSeeTable='Table viewer';


const
      ArrNavigatorHints:array[1..10] of string=
       (
        'First record',
        'Last record',
        'Next record',
        'Previous record',
        'Insert record',
        'Delete record',
        'Edit record',
        'Post',
        'Cancel',
        'Refresh'
       );


const
    ArrMonth:array[1..12] of string=(
      'January',
      'February',
      'March',
      'April',
      'May',
      'June',
      'July',
      'August',
      'September',
      'Oktober',
      'November',
      'December'
      );

resourcestring
      StrBadDataTimeFormat='Bad format of date or time!';
      StrBadIntegerFormat='Bad format of integer!';
      StrBadFormatOfNumber='Bad format of number!';
      StrBadFormatOfCurrency='Bad format of currency';

const
      AltChars:set of char=[];
      AltSmallChars:set of char=[];

resourcestring
     StrOneMoment='Please wait';
     StrIsWorkPleaseWait='Work is going on. Please wait!';

     StrFont='Font';

     StrGridEditor='Grid editor';

     StrExit='Exit';

     StrErrorSetupNotIndicated ='Setup is not indicated';

      StrErrorSetupNotFound = 'Setup is not found';

implementation

end.