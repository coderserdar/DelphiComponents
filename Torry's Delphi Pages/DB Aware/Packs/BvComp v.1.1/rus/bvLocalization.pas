unit bvLocalization;

interface

resourcestring
      StrErrorChangeLevel='������ ��� ����� Level!'+#13+'�� ������� ���� ������ ��� �����!';
      StrErrorCannotUnLock='������ ��� ������������� �������';
      StrErrorCannotReadState='�� ���� ������� ��������� �������!';
      StrErrorCannotLock='�� ���� ������������� ������� ';

      StrErrorTableIsClosed='������� �� �������.';
      StrErrorCannotWriteToThisFormat= '�� ���� �������� ��������� � ������� ���������� �������';
      StrErrorCannotSaveData='�� ���� ��������� ������';

      StrErrorNotDefinedGrid='�� ���������� �����.';
      StrErrorNotDefinedObject='�� ��������� ������, � �������� ���������� �����.';
      StrErrorNotDefinedTable='�� ���������� �������, � ������� ���������� �����.';
      StrErrorNotDefinedSource='�� �������, ��� ��������� �������!';

      StrErrorForConfirmPasswords='2 ������ ������ ������ �� ���������.'+#13+#13
                 +'�������� - ����� �� ��� ������������? ;-)))';

      StrErrorNotFoundFile='�� ������ ���� ';

      StrContinue='���������';

      StrSavingTable='���� ���������� �������...';
      StrCopyingOfFiles='���� ����������� ������';
      StrCopying='���� �����������';


      /////////////



      StrBookMarks='��������';

      StrOkOperation='�������� ��������� �������.';

      StrBDEAlias='��������� ������� ������� (BDE)';
      StrTable='�������';

      StrNewPassword='����� ������';
      StrMaskOfPassword='� ����� ������������ ������� ��� �����������';
      StrConfirmPassword='������������� ������';
      StrChangePassword='������� ������';
      StrChangePasswordForm='��������� ������� � ��������';

      StrEditorCaption='�������� �������';

      StrDelete='�������';
      StrInsert='��������';
      StrReadOnly='������ ��� ������';

      StrIncFont='��������� �����';
      StrDecFont='��������� �����';

      StrByDefault='�� ���������';
      StrCancel='��������';
      StrOptions='�����';
      StrColumns='�������';
      StrStrippedRows='������������ ������';
      StrFixedCols='������������ �������';
      StrTitleMinHeight='�����. ������ ���������';
      StrCellRows='������ ������';

      StrEnter2Tab='ENTER -> TAB';
      StrTitle='���������';
      StrContents='����������';
      StrColor='����';
      StrWindowColor='��� ���� ����';
      StrEach='������';
      StrY='-�';
      StrAlwaysShowEditor='������ ���������� ������������� ������';
      StrAlwaysShowSelection='������ ���������� ���������� ������';
      StrCellHint='��������� ��� �����, � ������� �� ���������� �� ����������';
      StrHorzLines='������������� �������������� ������� �����';
      StrVertLines='������������� ������������ ������� �����';
      StrIndicator='���������� ���������';
      StrTabStop='����� ��� ���������';
      StrMultiSelect='��������� ���������� �����';
      StrSelectRows='�������� ������ �������';
      StrTitleHint='���������� ���������';


resourcestring
      BoolTrueText='��';
      BoolFalseText='���';

resourcestring
      StrLeftAlignMent='�����';
      StrCenterAlignMent='�� ������';
      StrRightAlignMent='������';

resourcestring
      StrField='����';
      StrColumn='�������';
      StrWidth='������';
      StrVisible='������';
      StrAlignment='������������';
      StrTitleAlignment='������.���������';
      StrwhatNumber='��� ��� �� �����?';
      StrFutureSaving='������ ��������� ������ � �������� ������ ����� ������������ �����';

resourcestring
      StrConfirm='�������������';
      StrConfirmOperation='����������� ��������';
      StrYes='��';
      StrNO='���';

resourcestring
      cftPrinter='��������� ������ ����� �� �������?';
      cfButtOk='�������';
      cfButtCancel='�����';

resourcestring
      StrYesForAll='��, ��� ����!';

resourcestring
      StrCopyTab2TabCaption='����������� ������  (<--)';

      StrGetVarCaption='������������ ����';
      StrSetVarCaption='��������� ����';
      StrConfirmChangesInFieldMap='��������� ��������� � ���������� �����?';

      StrAccept='�������';
      StrCompSelCols='����������� ���������� �������';
      StrUnCompSelCols='��������� ���������� �������';

      StrClearAll='�������� ���';
      StrVar='�������:';
      StrFieldsInSource='���� � ���������';
      StrFieldsInDestination='���� � ���������';
      StrChoiseFields='����������� ����:';
      StrFilter='������';

resourcestring
      StrErrorReadProperties= '������ ��� �������� �������� ������!';
      StrMessage='���������';

resourcestring
      StrQuickPrint='������� ������';
      StrSaveToFile='��������� � �����';
      strSeeList='�������� ������';

      StrNotFound='������ �� �����';
      StrFinderCaption='����� � �������';
      StrTextCaption='�����:';
      StrOneColumnCaption='������ �� ������ �������';
      StrWholeWordsOnly='������ ����� �������';
      StrMatchCase='� ������ ��������';
      StrDirection='�����������';
      StrUp='�����';
      StrDown='����';
      StrFind='����� (F3)';

      StrError='������';

      StrNewColumn='����� �������';
      StrChoiceField='������� ����:';

      StrSaveColumns='��������� �������:';
      StrSelectAll='�������� ���';
      StrSelectNone='����� ��� �������';
      StrShowResult='�������� �������������� �������';
      StrUseBDEDriver='������������ BDE-�������';



      StrTitleOfColumn='��������� �������';
      StrRestoreProperties='��������� ���������';
      StrVersion='������ �������';

      StrOpenedFrom='������� �� ';
      StrSavingToExcel='���������� � Excel';

      StrCheckPath='��������� ������������ ���������� ����!';
      StrPrintFile='������ �����';
      StrConfirmReplaceFile='�������� ������������ ����?';
      StrConfirmClearFile='�������� ����?';

      StrSeeDoc='  �������� ���������';
      StrFile='����';
      StrClose='�������';
      StrOpen='�������';
      StrClear='��������';
      StrSave='���������';
      StrPrint='������';
      StrSeeTable='�������� �������';


const
      ArrNavigatorHints:array[1..10] of string=
       (
        '������ ������',
        '���������� ������',
        '��������� ������',
        '��������� ������',
        '��������',
        '�������',
        '�������������',
        '����������� ���������',
        '�������� ���������',
        '�������� ����������'
       );


const
     ArrMonth:array[1..12] of string=(
      '������',
      '�������',
      '�����',
      '������',
      '���',
      '����',
      '����',
      '�������',
      '��������',
      '�������',
      '������',
      '�������'
      );

resourcestring
      StrBadDataTimeFormat='������������ ������ ���� ��� ������!';
      StrBadIntegerFormat='������������ ������ ������';
      StrBadFormatOfNumber='������������ ������ �����';
      StrBadFormatOfCurrency='������������ �������� ������';

const
      AltChars:set of char=['�','�','�','�','�','�','�','�','�','�','�',
              '�','�','�','�','�','�','�','�','�','�','�',
              '�','�','�','�','�','�','�','�','�','�'];

      AltSmallChars:set of char=['�','�','�','�','�','�','�','�','�','�','�',
              '�','�','�','�','�','�','�','�','�','�','�',
              '�','�','�','�','�','�','�','�','�','�'];

resourcestring
     StrOneMoment='���� �������';
     StrIsWorkPleaseWait='���� ������. ����������, �����!';

     StrFont='�����';

     StrGridEditor='�������� �������';

     StrExit='�����';

     StrErrorSetupNotIndicated ='�� ������� ���������';

     StrErrorSetupNotFound = '������ ��������� ���� ��� �� ����������'+#13+
                '��� ����� ������� ����� �������� ������ �����';


implementation

end.
