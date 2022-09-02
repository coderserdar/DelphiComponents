{*********************************************************}
{* Create/View/Restructure Table Definition Dialog       *}
{*********************************************************}

{$I fsdefine.inc}

Unit fmstruct;

Interface

Uses
  Db,
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Grids,
  Stdctrls,
  ComCtrls,
  Buttons,
  ExtCtrls,
  fsllgrid,
  fssrbde,
  fsllbase,
  fssrbase,
  fslldict,
  uelement,
  uentity,
  uconfig,
  dgimpdef,
  fsfunInterp,
  ImgList,
  CheckLst;
Const
  dsFSMaxStringSize = 32767;
  dsFSMaxWordArraySize = 32767;
  dsFSMaxIntArraySize = 16383;
  dsFSMaxDoubleArraySize = 8191;

Type
  TffeDialogMode = (dmNeutral, dmViewing, dmCreating, dmRestructuring);
  TffeViewType = (vtViewFields, vtViewIndexes);
  TffeDrawType = (dtNormal, dtGrayed, dtChecked, dtUnchecked, dtWordWrap, dtIgnore);

  TffeCellComboBoxInfo = Packed Record
    Index: Integer; {index into Items list}
    {$IFDEF CBuilder}
    Case Integer Of
      0: (St: Array[0..255] Of char);
      1: (RTItems: TStrings;
        RTSt: Array[0..255] Of char);
      {$ELSE}
    Case Integer Of
      0: (St: ShortString); {string value if Index = -1}
      1: (RTItems: TStrings; {run-time items list}
        RTSt: ShortString); {run-time string value if Index = -1}
      {$ENDIF}
  End;

  TfrmTableStruct = Class(TForm)
    pnlMain: TPanel;
    dlgPrint: TPrintDialog;
    dlgSave: TSaveDialog;
    tabStructure: TPageControl;
    tbsFields: TTabSheet;
    tbsIndexes: TTabSheet;
    tbsExistingData: TTabSheet;
    grpExistingData: TGroupBox;
    tabExistingData: TPageControl;
    tbsFieldMap: TTabSheet;
    tbsOrphanedData: TTabSheet;
    grdOrphanedFields: TfsGrid;
    grdFields: TfsGrid;
    grdFieldMap: TfsGrid;
    cboFieldType: TComboBox;
    pnlFieldDetail: TPanel;
    grpBLOBViewStorage: TGroupBox;
    lblBLOBViewStorage: TLabel;
    btnInsertField: TBitBtn;
    btnDeleteField: TBitBtn;
    btnMoveFieldUp: TBitBtn;
    btnMoveFieldDown: TBitBtn;
    pnlHeader: TPanel;
    lblTableName: TLabel;
    edtTableName: TEdit;
    lblBlockSize: TLabel;
    cboBlockSize: TComboBox;
    pnlDialogButtons: TPanel;
    btnImport: TBitBtn;
    btnCreate: TBitBtn;
    btnRestructure: TBitBtn;
    btnCancel: TBitBtn;
    pnlIndexDetail: TPanel;
    grpCompositeKey: TGroupBox;
    splIndex: TSplitter;
    grdIndexes: TfsGrid;
    cboIndexType: TComboBox;
    cboIndexBlockSize: TComboBox;
    pnlExistingDataHeader: TPanel;
    chkPreserveData: TCheckBox;
    pnlExistingDataButtons: TPanel;
    btnMatchByName: TButton;
    btnMatchByPosition: TButton;
    btnClearAll: TButton;
    cboMapOldField: TComboBox;
    chkEncryptData: TCheckBox;
    pnlCompButtons: TPanel;
    btnAddIndexField: TSpeedButton;
    btnRemoveIndexField: TSpeedButton;
    pnlCompFieldsInIndex: TPanel;
    pnlCompAvailFields: TPanel;
    lblFieldsInIndex: TLabel;
    lstAvailFields: TListBox;
    lblAvailableFields: TLabel;
    chkAvailFieldsSorted: TCheckBox;
    btnMoveIndexFieldUp: TSpeedButton;
    btnMoveIndexFieldDown: TSpeedButton;
    Label1: TLabel;
    edtDescription: TEdit;
    CboBlob: TComboBox;
    CRound: TComboBox;
    Cdefault: TComboBox;
    ChkRange: TCheckBox;
    cboDelete: TComboBox;
    Label2: TLabel;
    grpBLOBEditStorage: TGroupBox;
    lblBLOBExtension: TLabel;
    lblBLOBBlockSize: TLabel;
    lblBLOBFileDesc: TLabel;
    imgMinus: TImage;
    imgPlus: TImage;
    radBLOBInternal: TRadioButton;
    radBLOBExternal: TRadioButton;
    cboBLOBBlockSize: TComboBox;
    edtBlobExtension: TEdit;
    edtBlobFileDesc: TEdit;
    ImageList1: TImageList;
    btnSort: TSpeedButton;
    lstIndexFields: TCheckListBox;
    btnPropField: TSpeedButton;
    btnDeleteIndex: TSpeedButton;
    button1: TBitBtn;
    {=====Form and general events=====}
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure btnCreateClick(Sender: TObject);
    Procedure btnCancelClick(Sender: TObject);
    Procedure btnPrintClick(Sender: TObject);
    Procedure btnImportClick(Sender: TObject);
    Procedure btnRestructureClick(Sender: TObject);
    Procedure btnInsertFieldClick(Sender: TObject);
    Procedure btnDeleteFieldClick(Sender: TObject);
    Procedure btnMoveFieldUpClick(Sender: TObject);
    Procedure btnMoveFieldDownClick(Sender: TObject);
    Procedure radBLOBInternalClick(Sender: TObject);
    Procedure cboFieldTypeChange(Sender: TObject);
    Procedure cboFieldTypeExit(Sender: TObject);
    Procedure grdFieldsEnter(Sender: TObject);
    Procedure grdFieldsSelectCell(Sender: TObject;
      Col, Row: Integer;
      Var CanSelect: Boolean);
    Procedure grdFieldsDrawCell(Sender: TObject;
      ACol, ARow: Integer;
      Rect: TRect;
      State: TGridDrawState);
    Procedure grdFieldsKeyPress(Sender: TObject; Var Key: Char);
    Procedure grdFieldsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    {=====Indexes tab events=====}
    Procedure btnDeleteIndexClick(Sender: TObject);
    Procedure btnAddIndexFieldClick(Sender: TObject);
    Procedure btnRemoveIndexFieldClick(Sender: TObject);
    Procedure btnMoveIndexFieldUpClick(Sender: TObject);
    Procedure btnMoveIndexFieldDownClick(Sender: TObject);
    Procedure lstIndexFieldsDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; Var Accept: Boolean);
    Procedure lstIndexFieldsDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    Procedure lstAvailFieldsDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; Var Accept: Boolean);
    Procedure lstAvailFieldsDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    Procedure cboIndexTypeChange(Sender: TObject);
    Procedure cboIndexTypeExit(Sender: TObject);
    Procedure grdIndexesEnter(Sender: TObject);
    Procedure grdIndexesSelectCell(Sender: TObject; ACol, ARow: Integer;
      Var CanSelect: Boolean);
    Procedure grdIndexesKeyPress(Sender: TObject; Var Key: Char);
    Procedure grdIndexesDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    Procedure grdIndexesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    {=====Existing data tab events=====}
    Procedure tabFieldMapPageChanged(Sender: TObject; Index: Integer);
    Procedure btnMatchByNameClick(Sender: TObject);
    Procedure btnMatchByPositionClick(Sender: TObject);
    Procedure btnClearAllClick(Sender: TObject);
    Procedure chkPreserveDataClick(Sender: TObject);
    Procedure grdFieldMapEnter(Sender: TObject);
    Procedure grdFieldMapActiveCellMoving(Sender: TObject; Command: Word;
      Var RowNum: Longint; Var ColNum: Integer);
    Procedure tcMapOldFieldChange(Sender: TObject);
    Procedure grdFieldsExit(Sender: TObject);
    Procedure grdFieldMapKeyPress(Sender: TObject; Var Key: Char);
    Procedure grdFieldMapSelectCell(Sender: TObject; ACol, ARow: Integer;
      Var CanSelect: Boolean);
    Procedure cboMapOldFieldChange(Sender: TObject);
    Procedure cboMapOldFieldExit(Sender: TObject);
    Procedure tabStructureChange(Sender: TObject);
    Procedure grdIndexesExit(Sender: TObject);
    Procedure FormKeyPress(Sender: TObject; Var Key: Char);
    Procedure lstAvailFieldsDblClick(Sender: TObject);
    Procedure lstIndexFieldsDblClick(Sender: TObject);
    Procedure chkAvailFieldsSortedClick(Sender: TObject);
    Procedure grdIndexesEnterCell(Sender: TfsGrid; aCol,
      aRow: Integer; Const text: String);
    Procedure cboFieldTypeKeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure cboIndexTypeKeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure cboMapOldFieldKeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure tabExistingDataChange(Sender: TObject);
    Procedure edtBlobExtensionExit(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var Action: TCloseAction);
    Procedure CboBlobChange(Sender: TObject);
    Procedure CboBlobExit(Sender: TObject);
    Procedure CboBlobKeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure CRoundChange(Sender: TObject);
    Procedure CRoundExit(Sender: TObject);
    Procedure CRoundKeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure pnlFieldDetailResize(Sender: TObject);
    Procedure Edit1KeyPress(Sender: TObject; Var Key: Char);
    Procedure CdefaultChange(Sender: TObject);
    Procedure CdefaultExit(Sender: TObject);
    Procedure CdefaultKeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure lstIndexFieldsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    Procedure lstIndexFieldsClickCheck(Sender: TObject);
    Procedure btnPropFieldClick(Sender: TObject);
    Procedure Button1Click(Sender: TObject);

  Private
    Procedure CMDialogKey(Var msg: TCMDialogKey); Message CM_DIALOGKEY;
  Protected
    FDialogMode: TffeDialogMode;
    FHasChanged: Boolean;
    { This flag is used to keep track of whether or not the information in the
      dialogs has changed. The approach is simplistic, a better
      approach would be to compare the current dict, and potential dict.
      Perhaps this could be done at a later point. }
    FDatabase: TffeDatabaseItem;
    FOutputDictionary: TFSInfoDict;
    FFieldList: TffeFieldList;
    FIndexList: TffeIndexList;
    FTempElementNumber: Longint;
    FTempStr: TffShStr;
    FTableIndex: Longint;
    FFieldMapComboRec: TffeCellComboBoxInfo;
    FFieldMap: TStringList;
    ReverseFFieldMap: TStringList; {!!.11}
    { to optimize lookup of fieldmappings }
    FInEnterKeyPressed: Boolean; {!!.11}
    FcboMapOldFieldHasBeenFocused: Boolean; {!!.11}
    FFieldMapInShiftTab: Boolean; {!!.11}

    Procedure AddFieldToIndex;
    Procedure RemoveFieldFromIndex;

  Public
    {=====General Routines=====}
    Procedure AlignButtons;
    Procedure PopulateForm(aTableIndex: Longint; aReadOnly: Boolean);
    Procedure DrawCell(Grid: TfsGrid; DrawType: TffeDrawType;
      Rect: TRect; State: TGridDrawState; CellText: String);
    Procedure ShowCellCombo(ComboBox: TCustomComboBox; Grid: TCustomGrid;
      Rect: TRect);

    {=====Dictionary Routines=====}
    Procedure BuildDictionary;
    Procedure LoadDictionary(aTableIndex: Longint);
    Procedure CreateTable(aTableName: TfsTableName);
    Procedure ShowDictionary(aTableIndex: Longint);

    {=====Field Grid Routines=====}
    Procedure InitializeFieldGrid;
    Procedure PopulateFieldGridHeader;
    Procedure InvalidateFieldsTable;
    Procedure InvalidateFieldsRow(Const RowNum: Integer);
    Procedure EnableBLOBControls;
    Procedure EnableFieldControls(aRowNum: Longint);
    Procedure LeavingFieldsCell(Const Col, Row: Longint);

    {=====Index Grid Routines=====}
    Procedure InitializeIndexGrid;
    Procedure PopulateIndexGridHeader;
    Procedure PopulateIndexFieldsLists(aIndex: Longint);
    Procedure InvalidateIndexesTable;
    Procedure InvalidateIndexesRow(Const RowNum: Integer);
    Function CalcKeyLength(aIndex: Integer): Integer;
    Procedure EnableIndexControls(aRowNum: Longint; aName: String);
    Procedure LeavingIndexCell(Const Col, Row: Longint);

    {=====FieldMap Routines=====}
    Procedure InitializeFieldMapGrid;
    Procedure PopulateFieldMapHeader;
    Procedure InvalidateFieldMapTable;
    Procedure InvalidateFieldMapRow(Const RowNum: Integer);
    Procedure RetrieveFieldMapSettings(Const ARow: Integer;
      Var Index: Integer;
      AStrings: TStrings);

    {=====FieldGrid Validation Routines=====}
    Function AllowDefaultField(aRowNum: Integer;
      Var aErrorCode: Word): Boolean;
    Function FieldNameValidation(Const AName: String;
      Var ErrorCode: Word): Boolean;
    Function FieldLengthValidation(Const ALength: String;
      Var ErrorCode: Word): Boolean;
    Function ValidateFieldUnits(aUnits, aFieldNum: Integer): Boolean;
    Function ValidDefaultFieldKey(aUpKey: Char;
      aFieldType: TfsFieldType): Boolean;

    {=====IndexGrid Validation Routines=====}
    Function IndexNameValidation(Const AName: String;
      Var ErrorCode: Word): Boolean;
    Function IndexExtensionValidation(Const AExtension: String;
      Var ErrorCode: Word): Boolean;
    Function IndexKeyLenValidation(Const AKeyLen: Integer;
      Var ErrorCode: Word): Boolean;
    {Misc Validation Routines}
    Function edtBLOBExtensionValidation(Const AExtension: String;
      Var ErrorCode: Word): Boolean;
    Function ValidateRestructure: Boolean;
    Procedure DisplayValidationError(ErrorCode: Word);
    Function ValidateForm: Boolean;
  End;

  {=====Entry-Point routines=====}
Function ShowCreateTableDlg(aDatabase: TffeDatabaseItem;
  Var aTableIndex: Longint;
  DefaultFieldDefs: TFieldDefs): TModalResult; {!!.11}

Function ShowRestructureTableDlg(aDatabase: TffeDatabaseItem;
  aTableIndex: Longint): TModalResult;

Procedure ShowViewTableStructureDlg(aDatabase: TffeDatabaseItem;
  aTableIndex: Longint; aViewType: TffeViewType);

Var
  frmTableStruct: TfrmTableStruct;

Implementation

{$R *.DFM}

Uses
  FsConvFF,
  dgPrintg,
  uBase,
  uConsts,
  FsStDate,
  FsCLConv,
  FsUtil, {!!.06}
  Printers,
  dgpropfieldidx,
  dgmdict;

Const

  {===== Grid column constants =====}
  cnFldNumber = 0;
  cnFldName = 1;
  cnFldType = 2;
  cnFldUnits = 3;
  cnFldDecPl = 4;
  cnFldRequired = 5;
  cnFldDefault = 6;
  cnFldDisplay = 7;
  cnFldBlob = 8;
  cnFldDescription = 9;
  cnFldRound = 10;
  cnFldEmpty = 11;
  cnFldDefaultUpdate = 12;
  cnFldHighest = 12;

  cnIdxNumber = 0;
  cnIdxName = 1;
  //cnIdxType = 2;
  cnIdxKeyLength = 2;
  cnIdxUnique = 3;
  //cnIdxAscending = 5;
  //cnIdxCaseSensitive = 5;
  cnIdxExt = 4;
  cnIdxBlockSize = 5;
  cnIdxDesc = 6;
  cnIdxHighest = 6;

  cnMapFieldName = 0;
  cnMapDatatype = 1;
  cnMapOldField = 2;
  cnMapHighest = 3;

  { Cell margin constants }
  cnTopMargin = 3;
  cnLeftMargin = 3;

  {===== Grid column names =========}
  cnsAscend = 'Ascend';
  cnsBlockSize = 'Block size';
  cnsCaseSens = 'Case';
  cnsDataType = 'Data type';
  cnsDataBlob = 'Blob Compress';
  cnsDescription = 'Description';
  cnsDecPl = 'Decimals';
  cnsDefault = 'Default';
  cnsDisplay = 'Display Label';
  cnsBlob = 'Blob compress';
  cnsRound = 'Round';
  cnsFldEmpty = 'EmptyAsNull';
  cnsFldDefaultUpdate = 'Default Update';

  cnsExt = 'File ext';
  cnsFieldName = 'Field name';
  cnsKeyLen = 'Key size';
  cnsName = 'Name';
  cnsNumber = '#';
  cnsRequired = 'Required';
  cnsType = 'Type';
  cnsUnique = 'Unique';
  cnsUnits = 'Units';

  {=====Entry-Point routines=====}

Function ShowCreateTableDlg(aDatabase: TffeDatabaseItem;
  Var aTableIndex: Longint;
  DefaultFieldDefs: TFieldDefs): TModalResult; {!!.11}
Var
  FieldIdx: Integer;
  OldCursor: TCursor;
  FFType: TfsFieldType;
  FFSize: Longint;

Begin
  Assert(Assigned(aDatabase));
  With TfrmTableStruct.Create(Nil) Do
    Try
      HelpContext := hcDefineNewTableDlg;
      OldCursor := Screen.Cursor;
      Screen.Cursor := crHourglass;
      Try
        tabStructure.ActivePage := tbsFields;
        FDialogMode := dmCreating;
        tbsExistingData.TabVisible := False;
        cboBlockSize.Style := csDropDownList;
        cboBlockSize.Enabled := True;
        cboBlockSize.Color := clWindow;
        cboBlockSize.TabStop := True;

        cboDelete.Style := csDropDownList;
        cboDelete.Enabled := True;
        cboDelete.Color := clWindow;
        cboDelete.TabStop := True;

        FDatabase := aDatabase;

        edtTableName.Enabled := True;
        edtTableName.Color := clWindow;
        edtTableName.Text := '';

        {Begin !!.10}
        edtDescription.Enabled := True;
        edtDescription.Color := clWindow;
        edtDescription.Text := '';
        {End !!.10}

        cboBlockSize.ItemIndex := 0;
        cboDelete.ItemIndex := 0;

        { Set up the fields tab }
        With grdFields Do
          Options := Options + [goEditing] + [goAlwaysShowEditor];

        {Begin !!.11}
        { in order to be able to open the New Table dialog with
          predefined fields, the DefaultFieldDefs parameter and
          this block was added.
        }
        If Assigned(DefaultFieldDefs) Then
          Begin
            grdFields.BeginUpdate;
            Try
              For FieldIdx := 0 To Pred(DefaultFieldDefs.Count) Do
                Begin
                  MapVCLTypeToFF(DefaultFieldDefs[FieldIdx].DataType,
                    DefaultFieldDefs[FieldIdx].Size,
                    FFType,
                    FFSize);
                  FFieldList.Insert(DefaultFieldDefs[FieldIdx].Name,
                    FFType,
                    FFSize,
                    0,
                    False,
                    '',
                    Nil,
                    blNone,
                    '',
                    rNone,
                    False,
                    duNormal);
                End;
              grdFields.RowCount := grdFields.FixedRows + DefaultFieldDefs.Count;
            Finally
              InvalidateFieldsTable;
              grdFields.EndUpdate;
              { moves focus to the grid. this is intentional; if we let focus
                remain on the tablename, then the top left editable cell doesn't
                draw properly. }
              ActiveControl := grdFields;
            End;
          End;
        {End !!.11}

        FFieldList.AddEmpty;
        InvalidateFieldsTable; {!!.11}

        { Show the field editing controls }
        btnInsertField.Visible := True;
        btnDeleteField.Visible := True;
        btnMoveFieldUp.Visible := True;
        btnMoveFieldDown.Visible := True;

        { Set BLOB views }
        grpBLOBViewStorage.Visible := False;
        grpBLOBEditStorage.Visible := True;

        { Adjust the fields grid to smaller space }
        grdFields.Height := btnInsertField.Top - grdFields.Top - 7;

        { Set up the Indexes tab }
        With grdIndexes Do
          Options := Options + [goEditing] + [goAlwaysShowEditor];

        FIndexList.AddEmpty;

        btnImport.Enabled := (FDatabase.TableCount > 0);
        btnImport.Visible := True;
        btnCreate.Visible := True;

        FTableIndex := -1;
        grdFields.Invalidate;
      Finally
        Screen.Cursor := OldCursor;
      End;
      Result := ShowModal;
      If Result = mrOK Then
        aTableIndex := FTableIndex;
    Finally
      Free;
    End;
End;
{--------}

Function ShowRestructureTableDlg(aDatabase: TffeDatabaseItem;
  aTableIndex: Longint): TModalResult;
Var
  OldCursor: TCursor;
Begin
  Assert(Assigned(aDatabase));
  With TfrmTableStruct.Create(Nil) Do
    Try
      cboBlockSize.Style := csDropDownList;
      cboBlockSize.Enabled := True;
      cboBlockSize.Color := clWindow;
      cboBlockSize.TabStop := True;
      cboDelete.Style := csDropDownList;
      cboDelete.Enabled := True;
      cboDelete.Color := clWindow;
      cboDelete.TabStop := True;
      HelpContext := hcRedefineTableDlg;
      OldCursor := Screen.Cursor;
      Screen.Cursor := crHourglass;
      Try
        tabStructure.ActivePage := tbsFields;
        FDialogMode := dmRestructuring;
        FTableIndex := aTableIndex;
        FDatabase := aDatabase;

        With FDatabase.Tables[aTableIndex] Do
          Begin
            Caption := 'Restructure Table: ' + TableName + ' in ' +
              Server.ServerName + '\' + Database.DataBaseName;
          End;

        PopulateForm(aTableIndex, False);

        edtTableName.Text := FDatabase.Tables[FTableIndex].TableName;
        edtTableName.ReadOnly := True;
        edtTableName.ParentColor := True;
        edtTableName.TabStop := False;

        { Set up the fields tab }
        With grdFields Do
          Options := Options + [goEditing] + [goAlwaysShowEditor];

        { Show the field editing controls }
        btnInsertField.Visible := True;
        btnDeleteField.Visible := True;
        btnMoveFieldUp.Visible := True;
        btnMoveFieldDown.Visible := True;

        { Set BLOB views }
        grpBLOBViewStorage.Visible := False;
        grpBLOBEditStorage.Visible := True;

        { Adjust the fields grid to smaller space }
        grdFields.Height := btnInsertField.Top - grdFields.Top - 7;

        { Set up the Indexes tab }
        With grdIndexes Do
          Options := Options + [goEditing] + [goAlwaysShowEditor];

        btnImport.Enabled := (FDatabase.TableCount > 0);
        btnImport.Width := btnRestructure.Width;
        btnImport.Visible := True;
        btnRestructure.Visible := True;
        ActiveControl := grdFields;
      Finally
        Screen.Cursor := OldCursor;
      End;
      Result := ShowModal;
    Finally
      Free;
    End;
End;
{--------}

Procedure ShowViewTableStructureDlg(aDatabase: TffeDatabaseItem;
  aTableIndex: Longint; aViewType: TffeViewType);
Var
  OldCursor: TCursor;
Begin
  Assert(Assigned(aDatabase));
  With TfrmTableStruct.Create(Nil) Do
    Try
      HelpContext := hcViewTableDlg;
      OldCursor := Screen.Cursor;
      Screen.Cursor := crHourglass;
      Try
        FDialogMode := dmViewing;
        FDatabase := aDatabase;
        FTableIndex := aTableIndex;

        tbsExistingData.TabVisible := False;

        With FDatabase.Tables[aTableIndex] Do
          Caption := 'Dictionary table: ' + TableName + ' in ' +
            Server.ServerName + '\' + Database.DataBaseName;

        edtTableName.Text := FDatabase.Tables[FTableIndex].TableName;
        edtTableName.ReadOnly := True;
        edtTableName.ParentColor := True;
        edtTableName.TabStop := False;

        {Begin !!.10}
        edtDescription.ReadOnly := True;
        edtDescription.ParentColor := True;
        edtDescription.TabStop := False;
        {End !!.10}

        cboBlockSize.Style := csSimple;
        cboBlockSize.Enabled := False;
        cboBlockSize.ParentColor := True;
        cboBlockSize.TabStop := False;
        cboDelete.Style := csSimple;
        cboDelete.Enabled := False;
        cboDelete.ParentColor := True;
        cboDelete.TabStop := False;

        chkAvailFieldsSorted.Visible := False;

        With tabStructure Do
          Case aViewType Of
            vtViewFields:
              Begin
                ActivePage := tbsFields;
                ActiveControl := grdFields;
              End;
            vtViewIndexes:
              Begin
                ActivePage := tbsIndexes;
                ActiveControl := grdIndexes;
              End;
          End;

        With grdFields Do
          Begin
            EditorMode := False;
            Options := Options - [goEditing] - [goAlwaysShowEditor];
          End;

        PopulateForm(aTableIndex, True);

        { Set BLOB views after loading the dictionary }
        grpBLOBViewStorage.Visible := True;
        grpBLOBEditStorage.Visible := False;

        With FDatabase.Tables[aTableIndex], Dictionary Do
          Begin
            If BLOBFileNumber = 0 Then
              lblBLOBViewStorage.Caption :=
                'BLOBs are stored in the main data file.'
            Else
              lblBLOBViewStorage.Caption :=
                Format('BLOBs are stored in file %s, block size = %d, description = "%s"',
                [TableName + '.' + FileExt[BLOBFileNumber],
                FileBlockSize[BLOBFileNumber], FileDesc[BLOBFileNumber]]);
          End;

        { Adjust the table encryption group }
        chkEncryptData.Enabled := False;
        chkRange.Enabled := False;
        chkEncryptData.Top := grpBLOBViewStorage.Top + 5;

        { Hide the field editing controls }
        btnInsertField.Visible := False;
        btnDeleteField.Visible := False;
        btnMoveFieldUp.Visible := False;
        btnMoveFieldDown.Visible := False;

        { Adjust the fields grid to larger space }
        grdFields.Height := grpBLOBViewStorage.Top - grdFields.Top - 2;

        { Hide index field editing controls }
        With grdIndexes Do
          Begin
            Options := Options - [goEditing] { - [goAlwaysShowEditor]};
          End;

        btnDeleteIndex.Visible := False;
        btnPropField.Enabled := False;
        btnSort.Enabled := False;
        lstIndexFields.DragMode := dmManual;
        lstAvailFields.DragMode := dmManual;
        btnAddIndexField.Enabled := False;
        btnRemoveIndexField.Enabled := False;
        btnMoveIndexFieldUp.Enabled := False;
        btnMoveIndexFieldDown.Enabled := False;
        button1.Visible := True;
      Finally
        Screen.Cursor := OldCursor;
      End;
      {Begin !!.11}
      {$IFDEF DCC4OrLater}
      Show;
    Finally
    End;
  {$ELSE}
      ShowModal;
    Finally
      Free;
    End;
  {$ENDIF}
  {End !!.11}
End;

{=====Form and general events=====}

Procedure TfrmTableStruct.FormCreate(Sender: TObject);
Begin
  FHasChanged := False;
  FFieldMapComboRec.RTItems := TStringList.Create;
  FFieldMap := TStringList.Create;
  FDialogMode := dmNeutral;
  // btnPrint.Left := btnCreate.Left;

  Left := Application.MainForm.ClientOrigin.X + 100;
  Top := Application.MainForm.ClientOrigin.Y;

  ClientWidth := pnlMain.Width + (pnlMain.Left * 2);
  ClientHeight := pnlMain.Height + (pnlMain.Top * 2);

  FFieldList := TffeFieldList.Create;

  FIndexList := TffeIndexList.Create;

  InitializeFieldGrid;
  InitializeIndexGrid;
  InitializeFieldMapGrid;

  edtBLOBExtension.Text := 'FSB';
  edtBLOBFileDesc.Text := 'BLOB file';

  grpBLOBViewStorage.Left := grpBLOBEditStorage.Left;
  grpBLOBViewStorage.Width := grpBLOBEditStorage.Width;

  grdOrphanedFields.Cells[0, 0] := cnsFieldName;
  grdOrphanedFields.Cells[1, 0] := cnsDataType;
  grdOrphanedFields.Cells[8, 0] := cnsDatablob;

  FInEnterKeyPressed := False; {!!.11}
  FcboMapOldFieldHasBeenFocused := False; {!!.11}
  FFieldMapInShiftTab := False; {!!.11}
End;
{--------}

Procedure TfrmTableStruct.FormDestroy(Sender: TObject);
Begin
  Try
    FFEConfigSaveFormPrefs(ClassName, Self);
    FFEConfigSaveColumnPrefs(ClassName + '.IndexGrid', grdIndexes);
    FFEConfigSaveColumnPrefs(ClassName + '.FieldGrid', grdFields);
    FFEConfigSaveInteger(ClassName, 'IndexSplitterPos', pnlIndexDetail.Height); {!!.11}
  Except
    On E: Exception Do
      ShowMessage('Error writing INI file: ' + E.Message);
  End;

  Assert(Assigned(Config));
  Config.SortAvailIndexFields := chkAvailFieldsSorted.Checked;
  FFieldMap.Free;
  FFieldMap := Nil;
  FFieldMapComboRec.RTItems.Free;
  FFieldMapComboRec.RTItems := Nil;
  FFieldList.Free;
  FFieldList := Nil;
  FIndexList.Free;
  FIndexList := Nil;
End;
{--------}

Procedure TfrmTableStruct.FormShow(Sender: TObject);
Begin
  { Center dialog }
  SetBounds(((Screen.Width - Width) Div 2),
    ((Screen.Height - Height) Div 2),
    Width, Height);

  FFEConfigGetFormPrefs(ClassName, Self);
  pnlIndexDetail.Height := FFEConfigGetInteger(ClassName, 'IndexSplitterPos', pnlIndexDetail.Height); {!!.11}

  AlignButtons;

  If FDialogMode = dmViewing Then
    btnCancel.Caption := 'C&lose'
  Else
    btnCancel.Caption := 'Cancel';

  { If redefining then set focus to first Name field in grid. }
  If FDialogMode <> dmViewing Then
    grdFields.Col := cnFldName;

  { Position to first real index in index grid. }
  If (FDialogMode = dmViewing) And (grdIndexes.RowCount > 2) Then
    grdIndexes.Row := 2;
  edtBlobFileDesc.Width := pnlFieldDetail.Width - edtBlobFileDesc.Left - 10;

End;
{--------}

Procedure TfrmTableStruct.FormCloseQuery(Sender: TObject;
  Var CanClose: Boolean);
Begin
  If Not (ModalResult = mrOK) And {!!.10}
  (FDialogMode <> dmViewing) And
    (FHasChanged) Then
    Begin
      CanClose := (MessageDlg('Are you sure you wish to cancel and lose any changes?',
        mtConfirmation,
        [mbYes, mbNo],
        0) = mrYes);
    End;
End;
{--------}

Procedure TfrmTableStruct.btnCreateClick(Sender: TObject);
Begin
  {Begin !!.11}
  { force typefield validation and saving }
  If grdFields.Col = cnFldType Then
    Begin
      grdFields.Perform(WM_KEYDOWN, VK_TAB, 0);
    End;
  {End !!.11}
  If ValidateForm Then
    Begin
      Try
        BuildDictionary;
        CreateTable(edtTableName.Text);
        FOutputDictionary.Free;
        FOutputDictionary := Nil;
        ModalResult := mrOK;
      Except
        { don't close the form }
        Raise;
      End;
    End;
End;
{--------}

Procedure TfrmTableStruct.btnCancelClick(Sender: TObject);
{Rewritten !!.11}
Begin
  {$IFDEF DCC4OrLater}
  If fsModal In FormState Then
    ModalResult := mrCancel
  Else
    Close;
  {$ELSE}
  ModalResult := mrCancel;
  {$ENDIF}
End;
{--------}

Procedure TfrmTableStruct.btnPrintClick(Sender: TObject);
Begin
End;
{--------}

Procedure TfrmTableStruct.btnImportClick(Sender: TObject);
Var
  ExcludeIndex,
    TableIndex: Longint;
  ImportFromDatabase,
    SaveDatabaseItem: TffeDatabaseItem;
Begin
  ExcludeIndex := -1;
  If btnRestructure.Visible Then
    ExcludeIndex := FTableIndex;
  If ShowImportTableDefDlg(FDatabase, ExcludeIndex, ImportFromDatabase, TableIndex) = mrOK Then
    Begin
      tabStructure.ActivePage := tbsFields; {reset to fields display}

      SaveDatabaseItem := FDatabase;
      FDatabase := ImportFromDatabase;
      Try
        With grdFields Do
          If EditorMode Then
            Begin
              EditorMode := False;
              LoadDictionary(TableIndex);
              EditorMode := True;
            End
          Else
            LoadDictionary(TableIndex);
        {Begin !!.11}
        { if no index in imported table, add an empty entry
          so we have an empty line to start editing in }
        If FIndexList.Count = 0 Then
          FIndexList.AddEmpty;
        {End !!.11}
      Finally
        FDatabase := SaveDatabaseItem;
      End;
    End;
End;
{--------}

Procedure TfrmTableStruct.btnRestructureClick(Sender: TObject);
Begin
  {Begin !!.07}
  { force typefield validation and saving }
  If grdFields.Col = cnFldType Then
    Begin
      grdFields.Perform(WM_KEYDOWN, VK_TAB, 0);
    End;
  {End !!.07}
  If ValidateForm Then
    If ValidateRestructure Then
      Begin
        BuildDictionary;
        With tabStructure Do
          If Not Pages[PageCount - 1].Enabled Or
            Not chkPreserveData.Checked Or
            (FFieldMap.Count = 0) Then
            FDatabase.Tables[FTableIndex].Restructure(FOutputDictionary, Nil, ChkRange.Checked)
          Else
            FDatabase.Tables[FTableIndex].Restructure(FOutputDictionary, FFieldMap, ChkRange.Checked);
        FOutputDictionary.Free;
        FOutputDictionary := Nil;
        ModalResult := mrOK;
      End;
End;

{=====Fields tab events=====}

Procedure TfrmTableStruct.btnInsertFieldClick(Sender: TObject);
Begin
  FHasChanged := True;
  With grdFields Do
    Begin
      Try
        EditorMode := False;
        FFieldList.InsertEmpty(Row - 1);
        Col := cnFldName;
        InvalidateFieldsTable;
      Finally
        EditorMode := True;
      End;
      EnableFieldControls(Row);
    End;
End;
{--------}

Procedure TfrmTableStruct.btnDeleteFieldClick(Sender: TObject);
Var
  I: Integer;
Begin
  FHasChanged := True;
  With grdFields Do
    Begin
      If (Row = RowCount - 1) And (FFieldList.Items[Row - 1].Name = '') Then
        MessageBeep(0)
      Else
        Begin
          With grdFields Do
            Begin
              I := FIndexList.FieldInUse(FFieldList.Items[Row - 1].Name);
              If I <> -1 Then
                Raise Exception.CreateFmt('Field %s is in use by index %d (%s)',
                  [FFieldList.Items[Row - 1].Name,
                  I,
                    FIndexList.Items[I].Name]);
            End;

          BeginUpdate;
          Try
            EditorMode := False;
            FFieldList.DeleteAt(Row - 1);
            InvalidateFieldsTable;
          Finally
            EndUpdate;
            EditorMode := True;
          End;
          EnableFieldControls(Row);
        End;
    End;
End;
{--------}

Procedure TfrmTableStruct.btnMoveFieldUpClick(Sender: TObject);
Begin
  FHasChanged := True;
  With grdFields Do
    Begin
      If Row > 1 Then
        Begin
          FFieldList.Exchange(Row - 1, Row - 2);
          InvalidateFieldsTable;
          Row := Row - 1;
        End;
    End;
End;
{--------}

Procedure TfrmTableStruct.btnMoveFieldDownClick(Sender: TObject);
Begin
  FHasChanged := True;
  With grdFields Do
    Begin
      If Row < pred(RowCount) Then
        Begin
          FFieldList.Exchange(Row, Row - 1);
          InvalidateFieldsTable;
          Row := Row + 1;
        End;
    End;
End;
{--------}

Procedure TfrmTableStruct.radBLOBInternalClick(Sender: TObject);
Begin
  EnableBLOBControls;
End;
{--------}

Procedure TfrmTableStruct.cboFieldTypeChange(Sender: TObject);
Begin
  With grdFields Do
    Begin
      Cells[Col, Row] := cboFieldType.Items[cboFieldType.ItemIndex];
    End;
  grdFields.Invalidate;
End;
{--------}

Procedure TfrmTableStruct.cboFieldTypeExit(Sender: TObject);
Begin
  cboFieldType.Visible := False;
  If Assigned(ActiveControl) And Not (ActiveControl = grdFields) Then
    ActiveControl.SetFocus
  Else
    Begin
      grdFields.SetFocus;
      grdFields.Perform(WM_KEYDOWN, VK_TAB, 0);
    End;
End;
{--------}

Procedure TfrmTableStruct.grdFieldsEnter(Sender: TObject);
Begin
  If FDialogMode <> dmViewing Then
    EnableFieldControls(grdFields.Row);
End;
{--------}

Procedure TfrmTableStruct.grdFieldsSelectCell(Sender: TObject;
  Col, Row: Integer;
  Var CanSelect: Boolean);
Var
  R: TRect;
  ErrorCode: Word;
Begin
  { Validate previously selected cell. If a validation error occurs, stop
    processing and display the error}
  CanSelect := (FDialogMode <> dmViewing);
  If (Not CanSelect) Then
    Exit;

  Case grdFields.Col Of
    cnFldName:
      CanSelect := FieldNameValidation(grdFields.Cells[cnFldName, grdFields.Row], ErrorCode);

    cnFldUnits:
      CanSelect := FieldLengthValidation(grdFields.Cells[cnFldUnits, grdFields.Row], ErrorCode);
  End;
  If Not CanSelect Then
    Begin
      DisplayValidationError(ErrorCode);
      Exit;
    End;

  { Save data to FFieldList, and update the grid if necessary}
  LeavingFieldsCell(grdFields.Col, grdFields.Row);

  { Set any special cell attributes (ComboBoxes, Readonly fields)}
  grdFields.Options := grdFields.Options + [goAlwaysShowEditor, goEditing];
  Case Col Of
    cnFldRequired:
      grdFields.Options := grdFields.Options - [goAlwaysShowEditor, goEditing];

    cnFldType:
      Begin
        R := grdFields.CellRect(Col, Row);
        ShowCellCombo(cboFieldType, grdFields, R);
        cboFieldType.ItemIndex :=
          cboFieldType.Items.IndexOf(grdFields.Cells[Col, Row]);
      End;

    cnFldBlob:
      Begin
        R := grdFields.CellRect(Col, Row);
        ShowCellCombo(cboBlob, grdFields, R);
        cboblob.ItemIndex :=
          cboblob.Items.IndexOf(grdFields.Cells[Col, Row]);
      End;
    cnFldRound:
      Begin
        If Not FFEFieldTypeHasRound(FFieldList.Items[Pred(Row)].FieldType) Then
          grdFields.Options := grdFields.Options - [goAlwaysShowEditor, goEditing]
        Else
          Begin
            R := grdFields.CellRect(Col, Row);
            ShowCellCombo(cRound, grdFields, R);
            cround.ItemIndex :=
              cround.Items.IndexOf(grdFields.Cells[Col, Row]);
          End;

      End;
    cnFldDefaultUpdate:
      Begin
        R := grdFields.CellRect(Col, Row);
        ShowCellCombo(cDefault, grdFields, R);
        cDefault.ItemIndex :=
          cDefault.Items.IndexOf(grdFields.Cells[Col, Row]);
      End;
    cnFldEmpty:
      Begin
        grdFields.Options := grdFields.Options - [goAlwaysShowEditor, goEditing];
      End;

    cnFldUnits:
      If Not FFEFieldTypeHasUnits(FFieldList.Items[Pred(Row)].FieldType) Then
        grdFields.Options := grdFields.Options - [goAlwaysShowEditor, goEditing]
      Else
        grdFields.Options := grdFields.Options + [goAlwaysShowEditor, goEditing];

    cnFldDecPl:
      If Not FFEFieldTypeHasDecPl(FFieldList.Items[Pred(Row)].FieldType) Then
        grdFields.Options := grdFields.Options - [goAlwaysShowEditor, goEditing]
      Else
        grdFields.Options := grdFields.Options + [goAlwaysShowEditor, goEditing];

    cnFldDefault:
      If Not AllowDefaultField(Row, ErrorCode) Then
        grdFields.Options := grdFields.Options - [goAlwaysShowEditor, goEditing]
  End;

  EnableFieldControls(Row);
End;
{--------}

Procedure TfrmTableStruct.grdFieldsDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
Var
  DrawType: TffeDrawType;
  ErrorCode: Word;
Begin
  { Leave fixed portion of the grid alone}
  If gdFixed In State Then
    Exit;

  With grdFields Do
    Begin
      DrawType := dtNormal;
      If ((Not (FDialogMode = dmViewing)) And (FFieldList.Count > ARow)) Or {!!.06}
      ((FDialogMode = dmViewing) And (FFieldList.Count >= ARow)) Then {!!.06}
        Case ACol Of
          cnFldUnits:
            If Not FFEFieldTypeHasUnits(FFieldList.Items[Pred(ARow)].FieldType) Then
              DrawType := dtGrayed;

          cnFldDecPl:
            If Not FFEFieldTypeHasDecPl(FFieldList.Items[Pred(ARow)].FieldType) Then
              DrawType := dtGrayed;
          cnFldRound:
            If Not FFEFieldTypeHasRound(FFieldList.Items[Pred(ARow)].FieldType) Then
              DrawType := dtGrayed;
          cnFldEmpty: If Not
            (FFieldList.Items[Pred(ARow)].FieldType In [fstSingleChar, fstSingleWideChar, fstShortString..High(TfsFieldType)]) Then
              DrawType := dtGrayed
            Else
              Begin
                If FFieldList.Items[Pred(ARow)].fiEmptyAsNull Then
                  DrawType := dtChecked
                Else
                  DrawType := dtUnchecked;
              End;
          cnfldblob:
            If Not FFEFieldTypeHasBlob(FFieldList.Items[Pred(ARow)].FieldType) Then
              DrawType := dtGrayed;
          cnFldRequired:
            If (FFieldList.Items[Pred(ARow)].fiDataType = fstAutoInc32) Or
            (FFieldList.Items[Pred(ARow)].fiDataType = fstAutoInc64) Then {!!.06}
              DrawType := dtGrayed {!!.06}
            Else
              Begin {!!.06}
                If FFieldList.Items[Pred(ARow)].fiRequired Then
                  DrawType := dtChecked
                Else
                  DrawType := dtUnchecked;
              End; {!!.06}

          cnFldDefault, cnFldDefaultUpdate:
            If Not AllowDefaultField(aRow, ErrorCode) Then
              DrawType := dtGrayed;
        End;

      { Now that the DrawType is known, we can manipulate the canvas}
      DrawCell(Sender As TfsGrid, DrawType, Rect, State, Cells[ACol, ARow]);
    End;
End;
{--------}

Procedure TfrmTableStruct.grdFieldsKeyPress(Sender: TObject;
  Var Key: Char);
Const
  valValidNumber = ['0'..'9'];
  valValidAlpha = ['a'..'z', 'A'..'Z'];
Var
  Value: String;
  Ignore: Boolean;
Begin
  If Key = #13 Then
    { Change the selected cell (Enter as tab)}
    With grdFields Do
      If Col < Pred(ColCount) Then
        Col := Col + 1
      Else If Row < Pred(RowCount) Then
        Begin
          Row := Row + 1;
          Col := cnFldName;
        End
      Else
        Begin
          Row := 1;
          Col := cnFldName;
        End
    Else
      Begin
        { Validate data entry as key's are pressed}
        Case grdFields.Col Of
          cnFldName:
            Begin
              Value := grdFields.Cells[cnFldName, grdFields.Row];
              Ignore := Not (Key In [#8, #46]) And (Length(Value) >= 31); {!!.01}
            End;

          cnFldUnits:
            Begin
              Value := grdFields.Cells[cnFldUnits, grdFields.Row];
              If Key In valValidAlpha Then
                Ignore := True
              Else
                Ignore := (Key In valValidNumber) And (Length(Value) >= 5);
            End;

          cnFldDecPl:
            Begin
              Value := grdFields.Cells[cnFldDecPl, grdFields.Row];
              If Key In valValidAlpha Then
                Ignore := True
              Else
                Ignore := (Key In valValidNumber) And (Length(Value) >= 3)
            End;

          cnFldDefault:
            Begin
              {Is the default value <= the units?}
              If (Key <> #8) Then
                Begin
                  {If ((FFEFieldTypeRequiresUnits(FFieldList.Items[pred(grdFields.Row)].FieldType)) Or
                    (StrToInt(grdFields.Cells[cnFldUnits, grdFields.Row]) > 0)) Then
                    Ignore := Length(grdFields.Cells[cnFldDefault, grdFields.Row]) >
                      StrToInt(grdFields.Cells[cnFldUnits, grdFields.Row]) +
                      StrToInt(grdFields.Cells[cnFldDecPl, grdFields.Row])
                  Else  }
                  Ignore := False;
                  // If (Not Ignore) Then
                  //   Ignore := Not ValidDefaultFieldKey(UpCase(Key),
                  //     FFieldList.Items[Pred(grdFields.Row)].FieldType);
                End
              Else
                Ignore := False;
            End;
          cnFldBlob, cnfldround: ignore := False;
          cnFldDisplay:
            Ignore := Not (Key In [#8, #46]) And (Length(Value) >= 31); {!!.01}
          cnFldDescription:
            Ignore := Not (Key In [#8, #46]) And (Length(Value) >= 96); {!!.01}

          cnFldRequired:
            Begin
              Ignore := (Not (Key In [#9, #32]));
              If (Key = ' ') And (Not (FDialogMode = dmViewing)) Then
                With FFieldList.Items[Pred(grdFields.Row)] Do
                  fiRequired := Not fiRequired;
              grdFields.Invalidate;
            End;

          Else
            Ignore := False;
        End;
        If Ignore Then
          Begin
            Key := #0;
            MessageBeep(0);
          End;
      End;
End;
{--------}

Procedure TfrmTableStruct.grdFieldsMouseUp(Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer);
Var
  ACol, ARow: Longint;
  Rect, Dest: TRect;
Begin
  { Manipulate checkbox state in Fields grid}
  If Button <> mbLeft Then
    Exit;
  grdFields.MouseToCell(X, Y, ACol, ARow);
  If ACol = cnFldRequired Then
    Begin
      Rect := grdFields.CellRect(ACol, ARow);
      With imgPlus.Picture Do
        { Retrieve the rect from around the box itself}
        Dest := Bounds(Rect.Left + ((Rect.Right - Bitmap.Width - Rect.Left) Div 2),
          Rect.Top + (grdFields.DefaultRowHeight - Bitmap.Height) Div 2,
          Bitmap.Width,
          Bitmap.Height);

      { Only manipuate the checkbox state if an area on or within the rect was
        clicked}
      If (X >= Dest.Left) And (X <= Dest.Right) And
        (Y >= Dest.Top) And (Y <= Dest.Bottom) And
        (Not (FDialogMode = dmViewing)) Then
        Begin {!!.06}
          With FFieldList.Items[Pred(ARow)] Do
            fiRequired := Not fiRequired;
          grdFields.Invalidate;
        End;
    End;
  If ACol = cnFldEmpty Then
    Begin
      Rect := grdFields.CellRect(ACol, ARow);
      With imgPlus.Picture Do
        { Retrieve the rect from around the box itself}
        Dest := Bounds(Rect.Left + ((Rect.Right - Bitmap.Width - Rect.Left) Div 2),
          Rect.Top + (grdFields.DefaultRowHeight - Bitmap.Height) Div 2,
          Bitmap.Width,
          Bitmap.Height);

      { Only manipuate the checkbox state if an area on or within the rect was
        clicked}
      If (X >= Dest.Left) And (X <= Dest.Right) And
        (Y >= Dest.Top) And (Y <= Dest.Bottom) And
        (Not (FDialogMode = dmViewing)) Then
        Begin {!!.06}
          With FFieldList.Items[Pred(ARow)] Do
            fiEmptyAsNull := Not fiEmptyAsNull;
          grdFields.Invalidate;
        End;
    End;

End;

{=====Indexes tab events=====}

Procedure TfrmTableStruct.btnDeleteIndexClick(Sender: TObject);
Begin
  FHasChanged := True;
  If (grdIndexes.Row = grdIndexes.RowCount - 1) And
    (FIndexList.Items[grdIndexes.Row - 1].Name = '') Then
    MessageBeep(0)
  Else
    Begin
      grdIndexes.BeginUpdate;
      Try
        grdIndexes.EditorMode := False;
        FIndexList.DeleteAt(grdIndexes.Row - 1);
        grdIndexes.RowCount := grdIndexes.RowCount - 1;
        InvalidateIndexesTable;
      Finally
        grdIndexes.EndUpdate;
        grdIndexes.EditorMode := True;
      End;
      EnableIndexControls(grdIndexes.Row, '');
    End;
End;
{--------}

Procedure TfrmTableStruct.AddFieldToIndex;
Var
  Idx: Integer;
  ItemIdx: Integer;
  KeyLength: Integer;
Begin
  FHasChanged := True;
  With lstAvailFields Do
    If SelCount = -1 Then
      Begin
        If ItemIndex <> -1 Then
          Begin
            lstIndexFields.Items.AddObject(Items[ItemIndex], TObject(1));
            With grdIndexes Do
              Begin
                With FIndexList.Items[Row - 1] Do
                  Begin
                    AddField(Items[ItemIndex], 1, 0, 0, 1);
                    KeyLength := CalcKeyLength(Row - 1);
                    If KeyLength > fscl_MaxKeyLength Then
                      Begin
                        DeleteField(Items[ItemIndex]);
                        Raise Exception.CreateFmt('Key length cannot exceed %d', [fscl_MaxKeyLength]);
                      End;
                    iiKeyLen := KeyLength;
                  End;
              End;
            ItemIdx := ItemIndex;
            Items.Delete(ItemIndex);
            If ItemIdx < Items.Count Then
              ItemIndex := ItemIdx
            Else If Items.Count > 0 Then
              ItemIndex := Items.Count - 1;
          End;
      End
    Else
      { The multiselect option is selected for the list}
      For Idx := 0 To Pred(Items.Count) Do
        If Selected[Idx] Then
          Begin
            lstIndexFields.Items.AddObject(Items[Idx], TObject(1));
            With grdIndexes Do
              Begin
                With FIndexList.Items[Row - 1] Do
                  Begin
                    AddField(Items[Idx], 1, 0, 0, 1);
                    KeyLength := CalcKeyLength(Row - 1);
                    If KeyLength > fscl_MaxKeyLength Then
                      Begin
                        DeleteField(Items[Idx]);
                        Raise Exception.CreateFmt('Key length cannot exceed %d', [fscl_MaxKeyLength]);
                      End;
                    iiKeyLen := KeyLength;
                  End;
              End;
            ItemIdx := Idx;
            Items.Delete(Idx);
            If ItemIdx < Items.Count Then
              ItemIndex := ItemIdx
            Else If Items.Count > 0 Then
              ItemIndex := Pred(Items.Count);
          End;
  LeavingIndexCell(grdIndexes.Col, grdIndexes.Row);
  grdIndexes.Invalidate;
End;
{--------}

Procedure TfrmTableStruct.RemoveFieldFromIndex;
Var
  ItemIdx: Integer;
Begin
  FHasChanged := True;
  With lstIndexFields Do
    If ItemIndex <> -1 Then
      Begin
        lstAvailFields.Items.Add(Items[ItemIndex]);
        With grdIndexes Do
          Begin
            BeginUpdate;
            Try
              With FIndexList.Items[Row - 1] Do
                Begin
                  DeleteField(Items[ItemIndex]);
                  iiKeyLen := CalcKeyLength(Row - 1);
                End;
            Finally
              EndUpdate;
            End;
          End;
        ItemIdx := ItemIndex;
        Items.Delete(ItemIndex);
        If ItemIdx < Items.Count Then
          ItemIndex := ItemIdx
        Else If Items.Count > 0 Then
          ItemIndex := Items.Count - 1;
      End;
  LeavingIndexCell(grdIndexes.Col, grdIndexes.Row);
  grdIndexes.Invalidate;
End;
{--------}

Procedure TfrmTableStruct.btnAddIndexFieldClick(Sender: TObject);
Begin
  AddFieldToIndex;
End;
{--------}

Procedure TfrmTableStruct.btnRemoveIndexFieldClick(Sender: TObject);
Begin
  RemoveFieldFromIndex;
End;
{--------}

Procedure TfrmTableStruct.btnMoveIndexFieldUpClick(Sender: TObject);
Var
  NewItemIndex: Integer;
Begin
  FHasChanged := True;
  With lstIndexFields Do
    If ItemIndex > 0 Then
      Begin
        With FIndexList.Items[grdIndexes.Row - 1] Do
          ExchangeFields(Items[ItemIndex], Items[ItemIndex - 1]);
        NewItemIndex := ItemIndex - 1;
        Items.Exchange(ItemIndex, ItemIndex - 1);
        ItemIndex := NewItemIndex;
      End;
End;
{--------}

Procedure TfrmTableStruct.btnMoveIndexFieldDownClick(Sender: TObject);
Var
  NewItemIndex: Integer;
Begin
  FHasChanged := True;
  With lstIndexFields Do
    If (ItemIndex <> -1) And (ItemIndex < Items.Count - 1) Then
      Begin
        With FIndexList.Items[grdIndexes.Row - 1] Do
          ExchangeFields(Items[ItemIndex], Items[ItemIndex + 1]);
        NewItemIndex := ItemIndex + 1;
        Items.Exchange(ItemIndex, ItemIndex + 1);
        ItemIndex := NewItemIndex;
      End;
End;
{--------}

Procedure TfrmTableStruct.lstIndexFieldsDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; Var Accept: Boolean);
Begin
  If Source Is TComponent Then
    Accept := (TComponent(Source).Name = 'lstAvailFields');
End;
{--------}

Procedure TfrmTableStruct.lstIndexFieldsDragDrop(Sender, Source: TObject;
  X, Y: Integer);
Begin
  If FDialogMode <> dmViewing Then
    btnAddIndexFieldClick(Source);
End;
{--------}

Procedure TfrmTableStruct.lstAvailFieldsDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; Var Accept: Boolean);
Begin
  If Source Is TComponent Then
    Accept := (TComponent(Source).Name = 'lstIndexFields');
End;
{--------}

Procedure TfrmTableStruct.lstAvailFieldsDragDrop(Sender, Source: TObject;
  X, Y: Integer);
Begin
  If FDialogMode <> dmViewing Then
    btnRemoveIndexFieldClick(Source);
End;
{--------}

Procedure TfrmTableStruct.cboIndexTypeChange(Sender: TObject);
Begin
  With grdIndexes, TComboBox(Sender) Do {!!.01}
    Cells[Col, Row] := Items[ItemIndex]; {!!.01}

  grdIndexes.Invalidate;
End;
{--------}

Procedure TfrmTableStruct.cboIndexTypeExit(Sender: TObject);
Begin
  TComboBox(Sender).Visible := False;
  If Assigned(ActiveControl) And Not (ActiveControl = grdIndexes) Then
    ActiveControl.SetFocus
  Else
    Begin
      grdIndexes.SetFocus;
      grdIndexes.Perform(WM_KEYDOWN, VK_TAB, 0);
    End;
End;
{--------}

Procedure TfrmTableStruct.grdIndexesEnter(Sender: TObject);
Begin
  If FDialogMode <> dmViewing Then
    EnableIndexControls(grdIndexes.Row, '');
  lstIndexFields.Repaint;
End;
{--------}

Procedure TfrmTableStruct.grdIndexesSelectCell(Sender: TObject; ACol,
  ARow: Integer; Var CanSelect: Boolean);
Var
  Rect: TRect;
  ErrorCode: Word;
Begin
  { Validate previously selected cell. If a validation error occurs, stop
    processing and display the error}
  If FDialogMode = dmViewing Then
    Begin
      CanSelect := grdIndexes.Row <> aRow;
      If CanSelect Then
        PopulateIndexFieldsLists(aRow - 1);
      Exit;
    End;
  Case grdIndexes.Col Of
    cnIdxName:
      CanSelect := IndexNameValidation(grdIndexes.Cells[cnIdxName, grdIndexes.Row], ErrorCode);
    cnIdxExt:
      CanSelect := IndexExtensionValidation(grdIndexes.Cells[cnIdxExt, grdIndexes.Row], ErrorCode);
    cnIdxKeyLength:
      CanSelect := IndexKeyLenValidation(StrToInt('0' + grdIndexes.Cells[cnIdxKeyLength, grdIndexes.Row]), ErrorCode);
  End;
  If Not CanSelect Then
    Begin
      DisplayValidationError(ErrorCode);
      Exit;
    End;

  { Save data to FFieldList, and update the grid if necessary}
  LeavingIndexCell(grdIndexes.Col, grdIndexes.Row);
  PopulateIndexFieldsLists(Pred(aRow));

  {Set any special cell attributes}
  grdIndexes.Options := grdIndexes.Options + [goAlwaysShowEditor, goEditing];
  Case ACol Of
    cnIdxKeyLength:
      // If FIndexList.Items[Pred(ARow)].iiKeyTypeIndex <> ktUserDefined Then
      grdIndexes.Options := grdIndexes.Options - [{goAlwaysShowEditor,} goEditing];

    cnIdxUnique {, cnIdxCaseSensitive}:
      grdIndexes.Options := grdIndexes.Options - [{goAlwaysShowEditor,} goEditing];

    {cnIdxType:
      Begin
        Rect := grdIndexes.CellRect(ACol, ARow);
        ShowCellCombo(cboIndexType, grdIndexes, Rect);
        cboIndexType.ItemIndex :=
          FIndexList.Items[Pred(ARow)].iiKeyTypeIndex;
      End;}

    cnIdxBlockSize:
      Begin
        If FIndexList.Items[Pred(ARow)].iiExtension = '' Then
          grdIndexes.Options := grdIndexes.Options - [{goAlwaysShowEditor,} goEditing]
        Else
          Begin
            Rect := grdIndexes.CellRect(ACol, ARow);
            ShowCellCombo(cboIndexBlockSize, grdIndexes, Rect);
            cboIndexBlockSize.ItemIndex :=
              FIndexList.Items[Pred(ARow)].iiBlockSizeIndex;
          End;
      End;
  End;
  lstIndexFields.Repaint;
End;
{--------}

Procedure TfrmTableStruct.grdIndexesKeyPress(Sender: TObject;
  Var Key: Char);
Const
  valValidNumber = ['0'..'9'];
  valValidAlpha = ['a'..'z', 'A'..'Z'];
Var
  Ignore: Boolean;
Begin
  With grdIndexes Do
    If Key = #13 Then
      If Col < ColCount - 1 Then {next column!}
        Col := Col + 1
      Else If Row < RowCount - 1 Then
        Begin {next Row!}
          Row := Row + 1;
          Col := 1;
        End
      Else
        Begin {End of Grid! - Go to Top again!}
          Row := 1;
          Col := 1;
          {or you can make it move to another Control}
        End
    Else
      Begin
        Case Col Of
          cnIdxName:
            Begin
              Ignore := Not (Key In [#8, #46]) And (Length(Cells[Col, Row]) >= 31); {!!.01}
              EnableIndexControls(Row, Cells[Col, Row] + Key);
            End;

          cnIdxKeyLength:
            If Key In valValidAlpha Then
              Ignore := True
            Else
              Ignore := (Key In valValidNumber) And (Length(Cells[Col, Row]) >= 3);

          cnIdxExt:
            Ignore := Not (Key In [#8, #46]) And (Length(Cells[Col, Row]) >= 3); {!!.01}

          cnIdxDesc:
            Ignore := Not (Key In [#8, #46]) And (Length(Cells[Col, Row]) >= 63) {!!.01}
          Else
            Ignore := False;
        End;
        If Ignore Then
          Begin
            Key := #0;
            MessageBeep(0);
          End;
      End;
End;
{--------}

Procedure TfrmTableStruct.grdIndexesDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
Var
  DrawType: TffeDrawType;
Begin
  If gdFixed In State Then
    Exit;

  With grdIndexes Do
    Begin
      DrawType := dtNormal;
      If (ARow = 0) Then
        DrawType := dtIgnore
      Else
        Case ACol Of
          cnIdxKeyLength:
            //If FIndexList.Items[Pred(ARow)].iiKeyTypeIndex <> ktUserDefined Then
            DrawType := dtGrayed;

          cnIdxBlockSize:
            If FIndexList.Items[Pred(ARow)].iiExtension = '' Then
              DrawType := dtGrayed;

          cnIdxUnique:
            If FIndexList.Items[Pred(ARow)].iiUnique Then
              DrawType := dtChecked
            Else
              DrawType := dtUnchecked;

          { cnIdxAscending:
             If FIndexList.Items[Pred(ARow)].iiAscending Then
               DrawType := dtChecked
             Else
               DrawType := dtUnchecked;}

          {cnIdxCaseSensitive:
            Begin
              If FIndexList.Items[Pred(ARow)].iiKeyTypeIndex <> ktUserDefined Then
                DrawType := dtGrayed
              Else
                Begin
                  If FIndexList.Items[Pred(ARow)].iiCaseSensitive Then
                    DrawType := dtChecked
                  Else
                    DrawType := dtUnchecked;
                End;
            End;}
          Else
            DrawType := dtIgnore;
        End;

      DrawCell(Sender As TfsGrid, DrawType, Rect, State, Cells[ACol, ARow]);
    End;
End;
{--------}

Procedure TfrmTableStruct.grdIndexesMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  ACol, ARow: Longint;
  Rect, Dest: TRect;
Begin
  If Button <> mbLeft Then
    Exit;
  grdIndexes.MouseToCell(X, Y, ACol, ARow);
  If (ARow > 0) And
    (ACol In [cnIdxUnique {cnIdxAscending, cnIdxCaseSensitive}]) Then
    Begin
      Rect := grdIndexes.CellRect(ACol, ARow);
      With imgPlus.Picture Do
        Dest := Bounds(Rect.Left + ((Rect.Right - Bitmap.Width - Rect.Left) Div 2),
          Rect.Top + (grdIndexes.DefaultRowHeight - Bitmap.Height) Div 2,
          Bitmap.Width,
          Bitmap.Height);
      If (X >= Dest.Left) And (X <= Dest.Right) And
        (Y >= Dest.Top) And (Y <= Dest.Bottom) And
        (Not (FDialogMode = dmViewing)) Then
        Begin {!!.06}
          With FIndexList.Items[Pred(ARow)] Do
            Case ACol Of
              cnIdxUnique:
                iiUnique := Not iiUnique;
              //cnIdxAscending:
              //  iiAscending := Not iiAscending;
              //cnIdxCaseSensitive:
                //iiCaseSensitive := Not iiCaseSensitive;
            End;
          grdIndexes.Invalidate;
        End;
    End;
  lstIndexFields.Repaint;
End;

{=====Existing data tab events=====}

Procedure TfrmTableStruct.tabFieldMapPageChanged(Sender: TObject;
  Index: Integer);
Var
  I, J, N: Integer;
  Found: Boolean;
Begin
  Case Index Of
    0:
      Begin
        btnMatchByName.Enabled := True;
        btnMatchByPosition.Enabled := True;
        btnClearAll.Enabled := True;
      End;
    1:
      Begin
        btnMatchByName.Enabled := False;
        btnMatchByPosition.Enabled := False;
        btnClearAll.Enabled := False;

        { Build the orphaned fields list }
        With FDatabase.Tables[FTableIndex].Dictionary Do
          Begin
            N := 0;
            For I := 0 To FieldCount - 1 Do
              Begin
                Found := False;
                For J := 0 To FFieldMap.Count - 1 Do
                  If Pos('=' + FieldName[I] + #255, FFieldMap[J] + #255) <> 0 Then
                    Begin
                      Found := True;
                      Break;
                    End;

                If Not Found Then
                  With grdOrphanedFields Do
                    Begin
                      Cells[0, N + FixedRows] := FieldName[I];
                      If FieldType[I] >= fstArrayUInt8 Then
                        Cells[1, N + FixedRows] := Format('%s[%d]', [FieldDataTypes[FieldType[I]], FieldUnits[I]])
                      Else
                        Cells[1, N + FixedRows] := FieldDataTypes[FieldType[I]];
                      Inc(N);
                    End;
              End;

            With grdOrphanedFields Do
              Begin
                RowCount := N + FixedRows + 1;
                Cells[0, RowCount - 1] := '';
                Cells[1, RowCount - 1] := '';
              End;
          End;
      End;
  End;
End;
{--------}

Procedure TfrmTableStruct.btnMatchByNameClick(Sender: TObject);
Var
  I: Integer;
  NewFieldName: TffDictItemName;
  OldFieldIndex: Integer;
Begin
  With grdFieldMap Do
    Begin
      BeginUpdate;
      ReverseFFieldMap := TStringList.Create; {!!.11}
      Try
        Try
          FFieldMap.Clear;
          For I := 0 To FFieldList.Count - 1 Do
            Begin
              NewFieldName := FFieldList.Items[I].Name;
              With FDatabase.Tables[FTableIndex].Dictionary Do
                Begin
                  OldFieldIndex := GetFieldFromName(NewFieldName);
                  If OldFieldIndex <> -1 Then

                    { Check assignment compatibility }
                    If FSConvertSingleField(
                      Nil,
                      Nil,
                      FieldType[OldFieldIndex],
                      FFieldList.Items[I].fiDatatype,
                      -1,
                      -1,
                      -1,
                      -1,
                      -1,
                      rNone,
                      rNone,
                      ChkRange.Checked) = DBIERR_NONE Then
                      Begin
                        FFieldMap.Values[NewFieldName] := NewFieldName;
                        ReverseFFieldMap.Values[NewFieldName] := NewFieldName; {!!.11}
                      End;
                End;
            End;
        Finally
          InvalidateFieldMapTable;
          EndUpdate;
        End;
        {Begin !!.11}
      Finally
        ReverseFFieldMap.Free;
        ReverseFFieldMap := Nil;
      End;
      {End !!.11}
    End;
End;
{--------}

Procedure TfrmTableStruct.btnMatchByPositionClick(Sender: TObject);
Var
  I: Integer;
  NewFieldName: TffDictItemName;
Begin
  With grdFieldMap Do
    Begin
      BeginUpdate;
      ReverseFFieldMap := TStringList.Create; {!!.11}
      Try
        Try
          FFieldMap.Clear;
          For I := 0 To FFieldList.Count - 1 Do
            Begin
              NewFieldName := FFieldList.Items[I].Name;
              With FDatabase.Tables[FTableIndex].Dictionary Do
                If I < FieldCount Then

                  { Check assignment compatibility }
                  If FSConvertSingleField(
                    Nil,
                    Nil,
                    FieldType[I],
                    FFieldList.Items[I].fiDatatype,
                    -1,
                    -1,
                    -1,
                    -1,
                    -1,
                    rNone,
                    rNone,
                    ChkRange.Checked) = DBIERR_NONE Then
                    Begin
                      FFieldMap.Values[NewFieldName] := FieldName[I];
                      ReverseFFieldMap.Values[FieldName[I]] := NewFieldName;
                    End;
            End;
        Finally
          InvalidateFieldMapTable;
          EndUpdate;
        End;
        {Begin !!.11}
      Finally
        ReverseFFieldMap.Free;
        ReverseFFieldMap := Nil;
      End;
      {End !!.11}
    End;
End;
{--------}

Procedure TfrmTableStruct.btnClearAllClick(Sender: TObject);
Begin
  FFieldMap.Clear;
  InvalidateFieldMapTable;
End;
{--------}

Procedure TfrmTableStruct.chkPreserveDataClick(Sender: TObject);
Begin
  FFEEnableContainer(grpExistingData, chkPreserveData.Checked);
End;
{--------}

Procedure TfrmTableStruct.grdFieldMapEnter(Sender: TObject);
Var
  Dummy: Boolean;
Begin
  { rewritten }
  {Begin !!.11}
  If Not FcboMapOldFieldHasBeenFocused And
    Not FFieldMapInShiftTab Then
    Begin
      grdFieldMap.Col := 2;
      grdFieldMap.OnSelectCell(Self, grdFieldMap.Col, grdFieldMap.Row, Dummy);
    End
  Else If FFieldMapInShiftTab Then
    Begin
      SelectNext(grdFieldMap, False, True);
    End;
  FcboMapOldFieldHasBeenFocused := False;
  FFieldMapInShiftTab := False;
  {End !!.11}
End;
{--------}

Procedure TfrmTableStruct.grdFieldMapActiveCellMoving(Sender: TObject;
  Command: Word; Var RowNum: Longint; Var ColNum: Integer);
Begin
  (*if ColNum < 2 then ColNum := 2;
    with grdFieldMap do
      case Command of
        ccRight: begin
            Inc(RowNum);
            if RowNum >= RowLimit then
              RowNum := LockedRows;
          end;
        ccLeft: begin
            Dec(RowNum);
            if RowNum < LockedRows then
              RowNum := RowLimit - 1;
          end;
      end;*)
End;
{--------}

Procedure TfrmTableStruct.tcMapOldFieldChange(Sender: TObject);
Var
  TCB: TComboBox;
  I: Integer;
  TempStr: TffShStr;
Begin
  TCB := TComboBox(Sender As TCustomComboBox);
  I := TCB.ItemIndex;

  If I < 0 Then
    TempStr := ''
  Else
    TempStr := Copy(TCB.Items[I], 1, Pos(' (', TCB.Items[I]) - 1);

  FFieldMap.Values[FFieldList.Items[grdFieldMap.Row - 1].Name] := TempStr;
End;

{=====General routines=====}
{--------}

Procedure TfrmTableStruct.AlignButtons;
{ Find all the visible buttons on the main panel and center them }
Var
  I: Integer;
  Buttons: TFSNormalList;
  NewLeft: Integer;
  Offset: Integer;
  CurrentIndex: Integer;
  FirstIndex: Integer;
  BaseWidth: Integer;
Begin
  Buttons := TFSNormalList.Create;
  Try
    With pnlDialogButtons Do
      Begin
        For I := 0 To ControlCount - 1 Do
          If Controls[I] Is TBitBtn Then
            If Controls[I].Visible Then

              { We store the control's horizontal position in the 1st word,
                then the control index in the 2nd word. }
              Buttons.Insert(TfsIntListItem.Create(Controls[I].Left * ($FFFF + 1) + I));

        FirstIndex := TfsIntListItem(Buttons[0]).KeyAsInt And $FFFF;
        BaseWidth := Controls[FirstIndex].Width;
        NewLeft := 0;
        For I := 0 To Buttons.Count - 1 Do
          Begin
            CurrentIndex := TfsIntListItem(Buttons[I]).KeyAsInt And $FFFF;
            With Controls[CurrentIndex] Do
              Begin
                Left := NewLeft;
                Width := BaseWidth;
                Inc(NewLeft, Width + 8);
              End;
          End;
        Dec(NewLeft, 8);

        Offset := (pnlMain.Width - NewLeft) Div 2;
        For I := 0 To Buttons.Count - 1 Do
          With Controls[TfsIntListItem(Buttons[I]).KeyAsInt And $FFFF] Do
            Left := Left + Offset;
      End;
  Finally
    Buttons.Free;
  End;
End;
{--------}

Procedure TfrmTableStruct.PopulateForm(aTableIndex: Longint; aReadOnly: Boolean);
Begin
  LoadDictionary(aTableIndex);
  If Not aReadOnly Then
    Begin
      FFieldList.AddEmpty;
      InvalidateFieldsTable;
      FIndexList.AddEmpty;
      InvalidateIndexesTable;
      EnableIndexControls(1, '');
    End;
End;
{--------}

Procedure TfrmTableStruct.DrawCell(Grid: TfsGrid; DrawType: TffeDrawType;
  Rect: TRect; State: TGridDrawState; CellText: String);
Var
  Bitmap: TBitmap;
  Dest, Source: TRect;
  X, Y: Integer;
  WrapText, WrapTemp: String;
  WrapPos: Integer;
Begin
  Case DrawType Of
    dtIgnore: Exit;
    dtNormal, dtGrayed:
      With Grid Do
        Begin
          If DrawType = dtNormal Then
            Canvas.Brush.Color := clWindow
          Else
            Canvas.Brush.Color := clBtnFace;
          Canvas.FillRect(Rect);
          Canvas.TextRect(Rect, Rect.Left + cnLeftMargin, Rect.Top + cnTopMargin,
            CellText);
        End;

    dtChecked, dtUnChecked:
      Begin
        If DrawType = dtChecked Then
          Bitmap := imgPlus.Picture.Bitmap
        Else
          Bitmap := imgMinus.Picture.Bitmap;
        With Grid.Canvas Do
          Begin
            Dest := Bounds(Rect.Left + ((Rect.Right - Bitmap.Width - Rect.Left) Div 2),
              Rect.Top + (grdIndexes.DefaultRowHeight - Bitmap.Height) Div 2,
              Bitmap.Width,
              Bitmap.Height);
            Source := Bounds(0, 0, Bitmap.Width, Bitmap.Height);
            BrushCopy(Dest,
              Bitmap,
              Source,
              Bitmap.TransparentColor);
          End;
      End;
    dtWordWrap:
      Begin
        With Grid.Canvas Do
          Begin
            If gdFixed In State Then
              Begin
                Pen.Color := clBtnText;
                Brush.Color := clBtnFace;
              End
            Else
              Begin
                Pen.Color := clWindowText;
                Brush.Color := clWindow;
              End;
            Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);

            Y := Rect.Top;

            WrapText := CellText;
            Repeat
              WrapPos := Pos(#13, WrapText);
              If WrapPos <= 0 Then
                WrapTemp := WrapText
              Else
                WrapTemp := Copy(WrapText, 1, Pred(WrapPos));
              Delete(WrapText, 1, WrapPos);
              X := Rect.Left + ((Rect.Right - TextWidth(WrapTemp) - Rect.Left) Div 2);
              TextOut(X, Y, WrapTemp);
              Y := Y + TextHeight(WrapTemp);
            Until WrapPos <= 0;
          End;
      End;
  End;
End;
{--------}

Procedure TfrmTableStruct.ShowCellCombo(ComboBox: TCustomComboBox;
  Grid: TCustomGrid; Rect: TRect);
Begin
  Rect.Left := Rect.Left + Grid.Left;
  Rect.Right := Rect.Right + Grid.Left;
  Rect.Top := Rect.Top + Grid.Top;
  Rect.Bottom := Rect.Bottom + Grid.Top;
  ComboBox.Left := Rect.Left + 1;
  ComboBox.Top := Rect.Top + 1;
  ComboBox.Width := (Rect.Right + 1) - Rect.Left;
  ComboBox.Height := (Rect.Bottom + 1) - Rect.Top;

  {Display the combobox}
  ComboBox.Visible := True;
  ComboBox.SetFocus;
End;
{--------}

Procedure TfrmTableStruct.CMDialogKey(Var msg: TCMDialogKey);
Begin
  If (ActiveControl = cboFieldType) Or
    (ActiveControl = cboblob) Or
    (ActiveControl = cround) Or
    (ActiveControl = cboIndexType) Or
    (ActiveControl = cboIndexBlockSize) Then
    Begin
      If (msg.CharCode = VK_TAB) Then
        Begin
          ActiveControl.Visible := False;
          (*    if ActiveControl = cboFieldType then
                  grdFields.SetFocus
                else
                  grdIndexes.SetFocus;*)
          msg.Result := 1;
          Exit;
        End;
    End
  Else
    Begin
    End;
  If (ActiveControl = cboMapOldField) And
    (msg.CharCode = VK_TAB) And
    (GetKeyState(VK_SHIFT) < 0) Then
    Begin
      FFieldMapInShiftTab := True;
    End;
  Inherited;
End;

{=====Dictionary routines=====}

Procedure TfrmTableStruct.BuildDictionary;
Var
  I, J: Integer;
  FileNumber: Integer;
  FieldArray: TffFieldList;
  FieldsAscDesc: TffFieldList;
  aFieldsCase, aFieldsSize, aFieldsFlags, aFieldsNullTop: TffFieldList;
  FieldIHList: TffFieldIHList;
  ExtFound: Boolean;
  bl: TDataCompLevel;
Begin
  FOutputDictionary.Free;
  FOutputDictionary := Nil;

  FOutputDictionary := TFSInfoDict.Create(StrToInt(cboBlockSize.Text));
  Try
    With FOutputDictionary Do
      Begin
        IsEncrypted := chkEncryptData.Checked;
        {Case RadioTable.ItemIndex Of  }
        TableType := ttData;
        { 1: TableType := ttDataList;
         2: TableType := ttUser;
         3: TableType := ttSystem;
       End; }
        FOutputDictionary.EngineDeleteType := TRecoveryEngine(cboDelete.ItemIndex);
        { Add the fields; the field list is assumed to be valid at this point }
        For I := 0 To FFieldList.Count - 1 Do
          With FFieldList.Items[I] Do
            Begin
              bl := fiBlobLevelComp;
              If Name <> '' Then
                AddField(Name, fiDisplay, fiDataType, fiUnits, fiDecPlaces, fiRequired,
                  PffVCheckDescriptor(@fiValCheck),
                  bl,
                  fiDescription,
                  fiRound, fiEmptyAsNull, fiDefaultUpdate);
            End;

        { Check for external BLOB file }
        If radBLOBExternal.Checked Then
          AddFile(edtBLOBFileDesc.Text, edtBLOBExtension.Text,
            StrToInt(cboBLOBBlockSize.Text), ftBlobFile);

        { Add the Indexes }
        For I := 0 To FIndexList.Count - 1 Do
          With FIndexList.Items[I] Do
            If Name <> '' Then
              Begin

                { Determine if this index is to be stored in an external file }
                FileNumber := 0;
                ExtFound := False;
                iiExtension := AnsiUpperCase(iiExtension);
                If iiExtension <> '' Then
                  Begin
                    { note that file descriptions are not supported yet }
                    For J := 0 To FileCount - 1 Do
                      If FFCmpShStrUC(iiExtension, FileExt[J], 255) = 0 Then
                        Begin
                          ExtFound := True;
                          Break;
                        End;
                    If Not ExtFound Then
                      FileNumber := AddFile('', iiExtension, BlockSize, ftIndexFile);
                  End;

                If iiKeyTypeIndex = ktComposite Then
                  Begin

                    { Construct the list of fields that comprise this index }
                    For J := 0 To FieldCount - 1 Do
                      Begin
                        FieldArray[J] := GetFieldFromName(FieldName[J]);
                        If FieldArray[J] = -1 Then
                          Raise Exception.CreateFmt('Index %d (%s) refers to nonexistent field %s', [I + 1, Name, FieldName[J]]);
                        FieldsAscDesc[J] := Integer(Fields.Objects[J]);
                        If Integer(FieldsCase.Objects[J]) = 0 Then
                          aFieldsCase[J] := 1
                        Else
                          aFieldsCase[J] := 0;
                        FieldIHList[J] := '';
                        aFieldsSize[j] := Integer(FieldsSize.Objects[j]);
                        aFieldsFlags[j] := 0;
                        aFieldsNullTop[j] := Integer(FieldsNullTop.Objects[j]);
                      End;

                    AddIndex(Name, iiDescription, FileNumber,
                      FieldCount, FieldArray, FieldsAscDesc, aFieldsCase, aFieldsSize,
                      aFieldsFlags, aFieldsNullTop, FieldIHList, Not iiUnique);
                  End
                Else
                  Begin
                    AddUserIndex(Name, iiDescription, FileNumber,
                      iiKeyLen, Not iiUnique);
                  End;
              End;
        FileDescriptor[0].fdDesc := edtDescription.Text; {!!.10}
        CheckValid;
      End;
  Except
    FOutputDictionary.Free;
    FOutputDictionary := Nil;
    Raise;
  End;
End;
{--------}

Procedure TfrmTableStruct.LoadDictionary(aTableIndex: Longint);
Var
  IndexFields: TStringList;
  I: Integer;
Begin
  With FDatabase.Tables[aTableIndex] Do
    Begin

      { Reload always in case of restructure by another user }
      With Dictionary Do
        Begin
          cboBlockSize.Text := IntToStr(BlockSize);
          cboBlockSize.ItemIndex := FFEBlockSizeIndex(BlockSize);
          cboDelete.ItemIndex := Integer(EngineDeleteType);
          edtDescription.Text := FileDesc[0]; {!!.10}

          { Case TableType Of
             ttData: RadioTable.ItemIndex := 0;
             ttDataList: RadioTable.ItemIndex := 1;
             ttUser: RadioTable.ItemIndex := 2;
             ttSystem: RadioTable.ItemIndex := 3;
           End; }

           { Load the fields }
          grdFields.BeginUpdate;
          Try
            FFieldList.Empty;
            For I := 0 To FieldCount - 1 Do
              Begin
                FFieldList.Insert(FieldName[I],
                  FieldType[I],
                  FieldUnits[I],
                  FieldDecPl[I],
                  FieldRequired[I],
                  FieldDisplay[I],
                  FieldVCheck[I],
                  FieldBlobLevelComp[I],
                  FieldDescription[i],
                  FieldRound[i],
                  FieldEmptyAsNull[i],
                  FieldDefaultUpdate[i]);
              End;
            grdFields.RowCount := grdFields.FixedRows + FieldCount;
          Finally
            InvalidateFieldsTable;
            grdFields.EndUpdate;
          End;

          { Check for BLOB storage }
          edtBLOBExtension.Text := '';
          cboBLOBBlockSize.Text := '';
          edtBLOBFileDesc.Text := '';
          radBLOBInternal.Checked := (BLOBFileNumber = 0);
          radBLOBExternal.Checked := Not radBLOBInternal.Checked;
          EnableBLOBControls;
          If BLOBFileNumber <> 0 Then
            Begin
              edtBLOBExtension.Text := FileExt[BLOBFileNumber];
              cboBLOBBlockSize.Text := IntToStr(FileBlockSize[BLOBFileNumber]);
              edtBLOBFileDesc.Text := FileDesc[BLOBFileNumber];
            End;

          { Load the indexes }
         // IndexFields := TStringList.Create;
          Try
            Try
              FIndexList.LoadFromDict(Dictionary);
              If FDialogMode In [dmCreating, dmRestructuring] Then
                FIndexList.DeleteAt(0);
              grdIndexes.RowCount := grdIndexes.FixedRows + IndexCount;
            Finally
              InvalidateIndexesTable;
            End;
          Finally
            //IndexFields.Free;
          End;

          { Encrypted? }
          chkEncryptData.Checked := IsEncrypted;
        End;
    End;
End;
{--------}

Procedure TfrmTableStruct.CreateTable(aTableName: TfsTableName);
Begin
  With FDatabase Do
    CreateTable(aTableName, FOutputDictionary);

  { Make a new entry for the TableList }
  FTableIndex := FDatabase.AddTable(aTableName);
End;
{--------}

Procedure TfrmTableStruct.ShowDictionary(aTableIndex: Longint);
Var
  I, J: Integer;
  FldName: TffDictItemName;
  t: TffeTableItem;
  def: PffVCheckDescriptor;
  defval: String;

  Function CaseFlag(aNoCase: Boolean): Char;
  Begin
    If aNoCase Then
      Result := 'I'
    Else
      Result := 'S';
  End;

Begin
  If aTableIndex = -1 Then Exit;
  With FDatabase.Tables[aTableIndex], Dictionary Do
    Begin
      With TfrmDict.Create(Nil) Do
        Try
          Memo1.Lines.Clear;
          Memo1.Lines.Add('Table definition for:'); {!!.06}
          Memo1.Lines.Add(Format('  Table:  %s', [TableName])); {!!.06}
          Memo1.Lines.Add(Format('  Alias:  %s', [Database.DataBaseName])); {!!.06}
          Memo1.Lines.Add(Format('  Server: %s', [Server.ServerName])); {!!.06}
          Memo1.Lines.Add('');
          Memo1.Lines.Add(Format('Block Size: %d', [BlockSize]));
          Memo1.Lines.Add(Format('Logical Record Length:  %d', [LogicalRecordLength]));
          Memo1.Lines.Add(Format('Physical Record Length: %d', [RecordLength]));
          Memo1.Lines.Add(Format('Recovery Engine: %s', [StrRecoveryEngine[EngineDeleteType]]));
          If IsEncrypted Then
            Memo1.Lines.Add('Encrypted Table Data:   YES') {!!.06}
          Else
            Memo1.Lines.Add('Encrypted Table Data:   NO'); {!!.06}

          Memo1.Lines.Add('');
          Memo1.Lines.Add('Fields:');
          Memo1.Lines.Add('');
          Memo1.Lines.Add('Num Name                            DisplayName      ' +
            '               Type           Offset Size  Units Dec ' +
            'Req EAsNull CompLevel  DefUpdate  Round         Default         Descript');

          For I := 0 To FieldCount - 1 Do
            Begin
              defval := '';
              def := FieldVCheck[I];
              If def <> Nil Then
                If def.vdHasDefVal Then
                  defval := FSVCheckValToString(def.vdDefVal, FieldType[I]);
              Memo1.Lines.Add(Format('%3.3d %-31s %-31s %-14s %6d %5d %5d' +
                ' %3d  %-2.3s    %-4s %-10s %-9s  %-13s %-15s %-20s',
                [I + 1, FieldName[I], FieldDisplay[I], FieldDataTypes[FieldType[I]],
                FieldOffset[I], FieldLength[I], FieldUnits[I],
                  FieldDecPl[I], FFEBoolToStr(FieldRequired[I]),
                  FFEBoolToStr(FieldEmptyAsNull[I]),
                  fsDataCompLevel[FieldBlobLevelComp[I]],
                  fsDefaultUpdate[FieldDefaultUpdate[I]],
                  fsRound[FieldRound[i]],
                  defval,
                  FieldDescription[i]]));
            End;

          Memo1.Lines.Add('');
          Memo1.Lines.Add('Indexes:');
          Memo1.Lines.Add('');
          Memo1.Lines.Add('Num  Name                Field(s)            File Type Len Uni Case Asc Comp AnullTop Description');
          For I := 0 To IndexCount - 1 Do
            Begin
              With IndexDescriptor[I]^ Do
                Begin
                  FldName := '(n/a)';
                  If idCount > 0 Then
                    FldName := FieldName[idFields[0]];
                  Memo1.Lines.Add(Format('%3d  %-20.20s%-17.17s    %3s %4.4s %3d %2.1s    %s   %s %3d      %s      %s',
                    [idNumber,
                    idName,
                      FldName,
                      FileExt[idFile],
                      IndexTypes[IndexType[I]],
                      idKeyLen,
                      FFEBoolToStr(Not idDups),
                      FFEBoolToStr(Not boolean(idFieldsCase[0])),
                      FFEBoolToStr(boolean(idFieldsAscDesc[0])),
                      idFieldsSize[0],
                      FFEBoolToStr(boolean(idFieldsNullTop[0])),
                      FFShStrTrimR(idDesc)]));
                  J := 1;
                  While J < idCount Do
                    Begin
                      Inc(J);
                      Memo1.Lines.Add(Format('%25.25s%-17.17s                       %s   %s %3d   %s',
                        ['', FieldName[idFields[J - 1]],
                        FFEBoolToStr(Not boolean(idFieldsCase[J - 1])),
                          FFEBoolToStr(boolean(idFieldsAscDesc[J - 1])),
                          idFieldsSize[J - 1],
                          FFEBoolToStr(boolean(idFieldsNullTop[J - 1]))]));
                    End;
                End;
            End;

          Memo1.Lines.Add('');
          Memo1.Lines.Add('Files:');
          Memo1.Lines.Add('Num  File  Block  Type   Description');
          Memo1.Lines.Add('');
          For I := 0 To FileCount - 1 Do
            Memo1.Lines.Add(Format('%3d   %-3.3s %6d  %-5.5s  %s',
              [I, FileExt[I], FileBlockSize[I],
              FileTypes[FileType[I]], FileDesc[I]]));
          Memo1.Lines.Add('');
          Memo1.Lines.Add('FSSQL Explorer ver. ' + FFEVersionStr);
          Memo1.Lines.Add('Printed ' + DateTimeToStr(Now));
          ShowModal;
        Finally
          Free;
        End;
    End;
End;

{=====Field grid routines=====}

Procedure TfrmTableStruct.InitializeFieldGrid;
Var
  T: TfsFieldType;

Begin
  grdFields.ColCount := cnFldHighest + 1;
  grdFields.RowCount := 2;

  grdFields.ColWidths[cnFldNumber] := 25;
  grdFields.ColWidths[cnFldName] := 110;
  grdFields.ColWidths[cnFldType] := 100;
  grdFields.ColWidths[cnFldUnits] := 40;
  grdFields.ColWidths[cnFldDecPl] := 50;
  grdFields.ColWidths[cnFldRequired] := 50;
  grdFields.ColWidths[cnFldDefault] := 110;
  grdFields.ColWidths[cnFldDisplay] := 110;
  grdFields.ColWidths[cnFldBlob] := 85;
  grdFields.ColWidths[cnFldDescription] := 150;
  grdFields.ColWidths[cnFldround] := 85;
  grdFields.ColWidths[cnFldEmpty] := 70;
  grdFields.ColWidths[cnFldDefaultUpdate] := 75;

  grdFields.DefaultRowHeight := cboFieldType.Height;

  FFEConfigGetColumnPrefs(ClassName + '.FieldGrid', grdFields);

  PopulateFieldGridHeader;

  btnInsertField.Enabled := False;
  btnDeleteField.Enabled := False;
  btnMoveFieldUp.Enabled := False;
  btnMoveFieldDown.Enabled := False;
End;
{--------}

Procedure TfrmTableStruct.PopulateFieldGridHeader;
Var
  ColNum: Integer;
Begin
  grdFields.BeginUpdate;
  Try
    For ColNum := 0 To cnFldHighest Do
      Case ColNum Of
        cnFldNumber: grdFields.Cells[ColNum, 0] := cnsNumber;
        cnFldName: grdFields.Cells[ColNum, 0] := cnsName;
        cnFldType: grdFields.Cells[ColNum, 0] := cnsType;
        cnFldUnits: grdFields.Cells[ColNum, 0] := cnsUnits;
        cnFldDecPl: grdFields.Cells[ColNum, 0] := cnsDecPl;
        cnFldRequired: grdFields.Cells[ColNum, 0] := cnsRequired;
        cnFldDefault: grdFields.Cells[ColNum, 0] := cnsDefault;
        cnFldDisplay: grdFields.Cells[ColNum, 0] := cnsDisplay;
        cnFldblob: grdFields.Cells[ColNum, 0] := cnsblob;
        cnFldDescription: grdFields.Cells[ColNum, 0] := cnsDescription;
        cnFldround: grdFields.Cells[ColNum, 0] := cnsround;
        cnFldEmpty: grdFields.Cells[ColNum, 0] := cnsFldEmpty;
        cnFldDefaultUpdate: grdFields.Cells[ColNum, 0] := cnsFldDefaultUpdate;
      End;
  Finally
    grdFields.EndUpdate;
  End;
End;
{--------}

Procedure TfrmTableStruct.InvalidateFieldsTable;
Var
  RowNum: Integer;
Begin
  If FFieldList.Count = 0 Then
    grdFields.RowCount := 2
  Else
    grdFields.RowCount := succ(FFieldList.Count);
  For RowNum := 1 To FFieldList.Count Do
    InvalidateFieldsRow(RowNum);
  For RowNum := 1 To pred(grdFields.RowCount) Do {!!.06}
    grdFields.Cells[0, RowNum] := IntToStr(RowNum - 1); {!!.06}
End;
{--------}

Procedure TfrmTableStruct.InvalidateFieldsRow(Const RowNum: Integer);
Var
  ColNum: Integer;
Begin
  For ColNum := 0 To Pred(grdFields.ColCount) Do
    With FFieldList.Items[Pred(RowNum)] Do
      Case ColNum Of
        cnFldName:
          grdFields.Cells[ColNum, RowNum] := Name;
        cnFldType:
          grdFields.Cells[ColNum, RowNum] := cboFieldType.Items.Strings[cboFieldType.Items.indexof(Trim(FieldDataTypes[fiDataType]))];
        cnFldblob:
          grdFields.Cells[ColNum, RowNum] := cboblob.Items.Strings[ord(fiBlobLevelComp)];
        cnFldround:
          grdFields.Cells[ColNum, RowNum] := cround.Items.Strings[ord(fiRound)];
        cnFldDefaultUpdate:
          grdFields.Cells[ColNum, RowNum] := cDefault.Items.Strings[ord(fiDefaultUpdate)];
        cnFldUnits:
          grdFields.Cells[ColNum, RowNum] := IntToStr(fiUnits);
        cnFldDecPl:
          grdFields.Cells[ColNum, RowNum] := IntToStr(fiDecPlaces);
        cnFldDefault:
          Begin
            If fiValCheck.vdHasDefVal Then
              Begin
                grdFields.Cells[ColNum, RowNum] := FSVCheckValToString(fiValCheck.vdDefVal, fiDataType);
              End
            Else
              grdFields.Cells[ColNum, RowNum] := '';
          End;
        cnFldDisplay:
          grdFields.Cells[ColNum, RowNum] := fiDisplay;
        cnFldDescription:
          grdFields.Cells[ColNum, RowNum] := fiDescription;

      End;
End;
{--------}

Procedure TfrmTableStruct.EnableBLOBControls;
Begin
  lblBLOBExtension.Enabled := radBLOBExternal.Checked;
  edtBLOBExtension.Enabled := radBLOBExternal.Checked;

  lblBLOBBlockSize.Enabled := radBLOBExternal.Checked;
  cboBLOBBlockSize.Enabled := radBLOBExternal.Checked;

  lblBLOBFileDesc.Enabled := radBLOBExternal.Checked;
  edtBLOBFileDesc.Enabled := radBLOBExternal.Checked;
End;
{--------}

Procedure TfrmTableStruct.EnableFieldControls(aRowNum: Longint);
Begin
  If (aRowNum > 0) And (aRowNum <= FFieldList.Count) Then
    Begin
      btnInsertField.Enabled := FFieldList.Items[aRowNum - 1].Name <> '';
      btnDeleteField.Enabled := aRowNum <> grdFields.RowCount - 1;
      btnMoveFieldUp.Enabled := (aRowNum <> grdFields.RowCount - 1) And (aRowNum <> 1);
      btnMoveFieldDown.Enabled := aRowNum < grdFields.RowCount - 2;
    End;
End;
{--------}

Procedure TfrmTableStruct.LeavingFieldsCell(Const Col, Row: Longint);
{ Store new data info FFieldList; Update the interface before the
  Cell is changed}
Var
  i, j: Integer;
  TempStr: String[255];
  TempInt: Longint;
  fd: tfsfieldType;
  bl: TDataCompLevel;
  rd: TRound;
  du: TDefaultUpdate;
  (*  TempExtend  : Extended;
    TempCurrency: Currency;
    TempSingle  : Single;
    TempDouble  : Double;
    TempStDate  : TStDate;
    TempStTime  : TStTime;
    TempDT      : TDateTime;
    TempTS      : TTimeStamp;
    TempComp    : Comp;
    TempWideStr : WideString;*)
Begin
  If FFieldList.Count > (Row - 1) Then
    With FFieldList.Items[Row - 1] Do
      Case Col Of
        cnFldName:
          Begin
            TempStr := Name;
            Name := grdFields.Cells[Col, Row];
            {rename fields in indexes}
            If TempStr <> '' Then
              For I := 0 To Pred(FIndexList.Count) Do
                For J := 0 To Pred(FIndexList.Items[I].FieldCount) Do
                  If FIndexList.Items[I].FieldName[j] = TempStr Then
                    FIndexList.Items[I].FieldName[j] := Name;

            If Row = Pred(grdFields.RowCount) Then
              { If we've added a name in the empty row,
                add a new empty row to the list }
              If (FDialogMode In [dmRestructuring, dmCreating]) And {Start !!.01}
              (Name <> '') Then
                Begin
                  FFieldList.AddEmpty;
                  InvalidateFieldsTable;
                End; {End !!.01}
          End;

        cnFldType:
          Begin
            fd := fiDataType;
            fiDataType := FFEIndexToFieldType(grdFields.Cells[Col, Row]);
            If fd <> fiDataType Then
              Begin
                fiValCheck.vdHasDefVal := False;
                FillChar(fiValCheck.vdDefVal, SizeOf(fiValCheck.vdDefVal), #0);
              End;
          End;
        cnFldblob:
          Begin
            bl := FFEStrToFieldBlob(grdFields.Cells[Col, Row]);
            fiBlobLevelComp := bl;
          End;
        cnFldround:
          Begin
            rd := FFEStrToFieldround(grdFields.Cells[Col, Row]);
            firound := rd;
          End;

        cnFldDefaultUpdate:
          Begin
            du := FFEStrToFieldDefault(grdFields.Cells[Col, Row]);
            fiDefaultUpdate := du;
          End;
        cnFldUnits:
          Begin
            TempInt := fiUnits;
            fiUnits := StrToInt('0' + grdFields.Cells[Col, Row]);
            If (fiUnits < TempInt) Then
              Begin
                fiValCheck.vdHasDefVal := False;
                FillChar(fiValCheck.vdDefVal, SizeOf(fiValCheck.vdDefVal), #0);
              End;
          End;

        cnFldDecPl:
          Begin
            fiDecPlaces := StrToInt(grdFields.Cells[Col, Row]);
            CalcActualValues;
          End;

        cnFldDefault:
          Begin
            If grdFields.Cells[Col, Row] <> '' Then
              Begin
                FSStringToVCheckVal(grdFields.Cells[Col, Row], fiDataType, fiValCheck.vdDefVal);
                fiValCheck.vdHasDefVal := True;
              End
            Else
              fiValCheck.vdHasDefVal := False;
          End;

        cnFldDisplay:
          fiDisplay := grdFields.Cells[Col, Row];
        cnFldDescription:
          fiDescription := grdFields.Cells[Col, Row];

      End;
  InvalidateFieldsRow(grdFields.Row);
  grdFields.Invalidate;
End;

{=====Index grid routines=====}

Procedure TfrmTableStruct.InitializeIndexGrid;
Begin
  grdIndexes.ColCount := cnIdxHighest + 1;
  grdIndexes.RowCount := 2;

  grdIndexes.ColWidths[cnIdxNumber] := 25;
  grdIndexes.ColWidths[cnIdxName] := 110;
  //grdIndexes.ColWidths[cnIdxType] := 50;
  grdIndexes.ColWidths[cnIdxKeyLength] := 50;
  grdIndexes.ColWidths[cnIdxUnique] := 42;
  // grdIndexes.ColWidths[cnIdxAscending] := 42;
  //grdIndexes.ColWidths[cnIdxCaseSensitive] := 38;
  grdIndexes.ColWidths[cnIdxExt] := 40;
  grdIndexes.ColWidths[cnIdxBlockSize] := 60;
  grdIndexes.ColWidths[cnIdxDesc] := 250;

  grdIndexes.DefaultRowHeight := cboIndexType.Height;

  FFEConfigGetColumnPrefs(ClassName + '.IndexGrid', grdIndexes);

  chkAvailFieldsSorted.Checked := Config.SortAvailIndexFields;
  lstAvailFields.Sorted := chkAvailFieldsSorted.Checked;
  PopulateIndexGridHeader;
End;
{--------}

Procedure TfrmTableStruct.PopulateIndexGridHeader;
Var
  ColNum: Integer;
Begin
  grdIndexes.BeginUpdate;
  Try
    For ColNum := 0 To cnIdxHighest Do
      Case ColNum Of
        cnIdxNumber: grdIndexes.Cells[ColNum, 0] := cnsNumber;
        cnIdxName: grdIndexes.Cells[ColNum, 0] := cnsName;
        // cnIdxType: grdIndexes.Cells[ColNum, 0] := cnsType;
        cnIdxKeyLength: grdIndexes.Cells[ColNum, 0] := cnsKeyLen;
        cnIdxUnique: grdIndexes.Cells[ColNum, 0] := cnsUnique;
        // cnIdxAscending: grdIndexes.Cells[ColNum, 0] := cnsAscend;
        //cnIdxCaseSensitive: grdIndexes.Cells[ColNum, 0] := cnsCaseSens;
        cnIdxExt: grdIndexes.Cells[ColNum, 0] := cnsExt;
        cnIdxBlockSize: grdIndexes.Cells[ColNum, 0] := cnsBlockSize;
        cnIdxDesc: grdIndexes.Cells[ColNum, 0] := cnsDescription;
      End;
  Finally
    grdIndexes.EndUpdate;
  End;
End;

Procedure TfrmTableStruct.PopulateIndexFieldsLists(aIndex: Longint);
Var
  I, j: Integer;
  IndexSelected: boolean;
Begin
  If aIndex <= Pred(FIndexList.Count) Then
    Begin
      Case FDialogMode Of
        dmViewing, dmCreating:
          IndexSelected := (aIndex < FIndexList.Count) And (aIndex >= 0);
        Else
          IndexSelected := (aIndex < Pred(FIndexList.Count)) And (aIndex >= 0);
      End;

      With FIndexList.Items[aIndex] Do
        Begin
          If Name = '' Then
            grpCompositeKey.Caption := ' Composite Key '
          Else
            grpCompositeKey.Caption := ' Composite Key (' + Name + ') ';

          { Show fields defined for the current index }
          lstIndexFields.Clear;
          If IndexSelected Then
            Begin
              lstIndexFields.Items.BeginUpdate;
              Try
                For I := 0 To FieldCount - 1 Do
                  Begin
                    j := Integer(FIndexList.Items[aIndex].Fields.Objects[I]);
                    lstIndexFields.Items.AddOBject(FIndexList.Items[aIndex].FieldName[I], TObject(j));
                    j := Integer(FIndexList.Items[aIndex].FieldsCase.Objects[I]);
                    lstIndexFields.Checked[lstIndexFields.Items.Count - 1] := Boolean(j);
                  End;
              Finally
                lstIndexFields.Items.EndUpdate;
              End;
            End;
        End;

      { Show fields remaining in the table eligible to become part of the index }
      With lstAvailFields Do
        Begin
          Items.BeginUpdate;
          Try
            Clear;
            For I := 0 To FFieldList.Count - 1 Do
              With FFieldList.Items[I] Do
                If (Name <> '') And
                  { ByteArray and BLOB type scan't be in keys }
                Not (FieldType In [{fstArrayUInt8, fstArrayUInt16,fstArrayInt32, fstArrayDouble,} fstBLOB..ffcLastBLOBType]) And
                  { Field already in index list }
                (lstIndexFields.Items.IndexOf(Name) = -1) Then
                  Items.Add(Name);
          Finally
            Items.EndUpdate;
          End;
        End;
    End;
End;
{--------}

Procedure TfrmTableStruct.InvalidateIndexesTable;
Var
  RowNum: Integer;
Begin
  If FIndexList.Count = 0 Then
    grdIndexes.RowCount := 2
  Else
    grdIndexes.RowCount := succ(FIndexList.Count);
  For RowNum := 1 To FIndexList.Count Do
    InvalidateIndexesRow(RowNum);
  For RowNum := 1 To Pred(grdIndexes.RowCount) Do {!!.06}
    grdIndexes.Cells[0, RowNum] := IntToStr(RowNum - 1); {!!.06}
End;
{--------}

Procedure TfrmTableStruct.InvalidateIndexesRow(Const RowNum: Integer);
Var
  ColNum: Longint;
Begin
  With grdIndexes Do
    For ColNum := 0 To Pred(ColCount) Do
      With FIndexList.Items[Pred(RowNum)] Do
        Case ColNum Of
          cnIdxName: Cells[ColNum, RowNum] := Name;
          cnIdxKeyLength: Cells[ColNum, RowNum] := IntToStr(iiKeyLen);
          cnIdxExt: Cells[ColNum, RowNum] := iiExtension;
          cnIdxBlockSize: Cells[ColNum, RowNum] := cboBlockSize.Items.Strings[iiBlockSizeIndex];
          cnIdxDesc: Cells[ColNum, RowNum] := iiDescription;
        End;
End;

Function TfrmTableStruct.CalcKeyLength(aIndex: Integer): Integer;
Var
  I, J: Integer;
Begin
  Result := 0;
  With FIndexList.Items[aIndex] Do
    Begin
      For I := 0 To FieldCount - 1 Do
        With FFieldList Do
          Begin
            J := FFieldList.IndexOf(FieldName[I]);
            If J <> -1 Then
              Begin
                If Integer(FIndexList.Items[aIndex].FieldsSize.Objects[i]) > 0 Then
                  Begin
                    If (FFieldList.Items[j].FieldType In [fstShortString,
                      fstVarNullString, fstNullString]) Then
                      Inc(Result, Integer(FIndexList.Items[aIndex].FieldsSize.Objects[i]) + 1)
                    Else If (FFieldList.Items[j].FieldType In [fstWideString, fstVarWideString]) Then
                      Inc(Result, (Integer(FIndexList.Items[aIndex].FieldsSize.Objects[i]) + 1) * sizeof(WideChar));
                  End
                Else
                  Inc(Result, Items[J].fiSize);
                Inc(Result);
              End;
          End;
    End;
End;
{--------}

Procedure TfrmTableStruct.EnableIndexControls(aRowNum: Longint; aName: String);
Var
  Switch: Boolean;
Begin
  If aRowNum = 0 Then
    Exit;

  If (aRowNum > 0) And (aRowNum <= FIndexList.Count) Then
    btnDeleteIndex.Enabled := aRowNum <> grdIndexes.RowCount - 1;

  With FIndexList.Items[aRowNum - 1] Do
    Begin
      { We only enable the key controls when it's a composite key,
        we're in edit mode, and we are focused on a valid index. }
      If aName = '' Then
        aName := Name;
      Switch := (iiKeyTypeIndex = ktComposite) And
        (aName <> '') And
        (FDialogMode In [dmCreating, dmRestructuring]);

      If grpCompositeKey.Enabled <> Switch Then
        FFEEnableContainer(grpCompositeKey, Switch);
    End;
End;

{=====Fieldmap routines=====}

Procedure TfrmTableStruct.InvalidateFieldMapRow(Const RowNum: Integer);
Var
  ThisFieldType: TfsFieldType;
  ColNum: Integer;
Begin
  With FFieldList.Items[Pred(RowNum)] Do
    If Name <> '' Then
      For ColNum := 0 To Pred(cnMapHighest) Do
        Case ColNum Of
          cnMapFieldName: grdFieldMap.Cells[ColNum, RowNum] := Name;
          cnMapDatatype:
            Begin
              ThisFieldType := fiDataType;
              FTempStr := FieldDataTypes[ThisFieldType];
              If ThisFieldType >= fstArrayUInt8 Then
                FTemPStr := Format('%s[%d]', [FTempStr, fiUnits]);
              grdFieldMap.Cells[ColNum, RowNum] := FTempStr;
            End;
          cnMapOldField:
            Begin
              RetrieveFieldMapSettings(RowNum, FFieldMapComboRec.Index, FFieldMapComboRec.RTItems);
              grdFieldMap.Cells[ColNum, RowNum] := FFieldMapComboRec.RTItems[FFieldMapComboRec.Index];
            End;
        End;
End;
{--------}

Procedure TfrmTableStruct.InvalidateFieldMapTable;
Var
  RowNum: Integer;
Begin
  grdFieldMap.RowCount := FFieldList.Count;
  For RowNum := 1 To FFieldList.Count Do
    InvalidateFieldMapRow(RowNum);
End;

{=====Fieldgrid validation routines=====}

Function TfrmTableStruct.FieldNameValidation(Const AName: String;
  Var ErrorCode: Word): Boolean;
Var
  FieldName: TffDictItemName;
  I: Longint;

Begin
  FieldName := FFShStrTrim(AName);
  If FieldName <> '' Then
    Begin
      I := FFieldList.IndexOf(FieldName);
      If (I <> -1) And (I <> grdFields.Row - 1) Then
        Begin
          ErrorCode := oeDuplicateFieldName;
          Result := False;
          Exit;
        End;
    End;

  With grdFields Do
    If (FieldName = '') And (Row <> RowCount - 1) Then
      Begin
        ErrorCode := oeMissingFieldName;
        Result := False;
        Exit;
      End;

  ErrorCode := 0;
  Result := True;
End;
{--------}

Function TfrmTableStruct.FieldLengthValidation(Const ALength: String;
  Var ErrorCode: Word): Boolean;
Begin
  If Not ValidateFieldUnits(StrToInt('0' + ALength), grdFields.Row - 1) Then
    Begin
      ErrorCode := oeInvalidFieldUnits;
      Result := False;
      Exit;
    End;

  ErrorCode := 0;
  Result := True;
End;
{--------}

Function TfrmTableStruct.ValidateFieldUnits(aUnits, aFieldNum: Integer): Boolean;
Begin
  Case FFieldList.Items[aFieldNum].fiDataType Of
    fstShortString:
      Result := (aUnits > 0) And (aUnits < 256);
    fstArrayUInt8: Result := (aUnits > 0) And (aUnits <= (dsfsMaxStringSize * 2) - 50);
    fstArrayInt32: Result := (aUnits > 0) And (aUnits <= dsFSMaxIntArraySize - 50);
    fstArrayDouble: Result := (aUnits > 0) And (aUnits <= dsFSMaxDoubleArraySize - 50);
    fstArrayUInt16: Result := (aUnits > 0) And (aUnits <= dsFSMaxWordArraySize - 50);
    fstNullString,
      fstVarNullString,
      fstWideString, fstVarWideString:
      Result := (aUnits > 0) And (aUnits <= dsfsMaxStringSize);
    Else
      Result := True;
  End;
End;

{=====Indexgrid validation routines=====}

Function TfrmTableStruct.IndexNameValidation(Const AName: String;
  Var ErrorCode: Word): Boolean;
Var
  IndexName: TffDictItemName;
  I: Longint;
Begin
  IndexName := FFShStrTrim(AName);
  If IndexName <> '' Then
    Begin
      I := FIndexList.IndexOf(IndexName);
      If (I <> -1) And (I <> grdIndexes.Row - 1) Then
        Begin
          ErrorCode := oeDuplicateIndexName;
          Result := False;
          Exit;
        End;
    End;

  With grdIndexes Do
    If (IndexName = '') And (Row <> RowCount - 1) Then
      Begin
        ErrorCode := oeMissingIndexName;
        Result := False;
        Exit;
      End;

  ErrorCode := 0;
  Result := True;
End;
{--------}

Function TfrmTableStruct.IndexExtensionValidation(Const AExtension: String;
  Var ErrorCode: Word): Boolean;
Var
  ThisExtension: TffExtension;
  Idx: Integer; {!!.06}
Begin
  ThisExtension := FFShStrTrim(AExtension);
  If ThisExtension <> '' Then
    Begin

      { Can't match the data file }
      If (FFAnsiCompareText(ThisExtension, fsc_ExtForData) = 0) Or {!!.06} {!!.07}
      (FFAnsiCompareText(ThisExtension, fsc_ExtForTrans) = 0) Or {!!.06} {!!.07}
      (FFAnsiCompareText(ThisExtension, fsc_ExtForSQL) = 0) Then
        Begin {!!.06} {!!.07}
          ErrorCode := oeInvalidFileExtension;
          Result := False;
          Exit;
        End;

      { See if there's a conflict with the BLOB extension (if any) }
      If radBLOBExternal.Checked And
        (FFAnsiCompareText(ThisExtension, edtBLOBExtension.Text) = 0) Then
        Begin {!!.06} {!!.07}
          ErrorCode := oeDuplicateFileExtension;
          Result := False;
          Exit;
        End;

      { See if there's a conflict with other index extensions (if any) }{begin !!.06}
      For Idx := 0 To Pred(FIndexList.Count) Do
        Begin
          If Idx = grdIndexes.Row - 1 Then
            continue;
          If FFAnsiCompareText(ThisExtension, FIndexList.Items[Idx].iiExtension) = 0 Then
            Begin {!!.07}
              ErrorCode := oeDuplicateFileExtension;
              Result := False;
              Exit;
            End;
        End; {end !!.06}
    End;

  ErrorCode := 0;
  Result := True;
End;
{--------}

Function TfrmTableStruct.IndexKeyLenValidation(Const AKeyLen: Integer;
  Var ErrorCode: Word): Boolean;
Begin
  ErrorCode := 0;
  Result := True;
End;

{=====Misc validation routines}
{--------}

Function TfrmTableStruct.edtBLOBExtensionValidation(Const AExtension: String;
  Var ErrorCode: Word): Boolean;
Var
  ThisExtension: TffExtension;
  I: Integer;
Begin
  ThisExtension := FFShStrTrim(AExtension);
  If ThisExtension <> '' Then
    Begin

      { Can't match the data file }{begin !!.06, !!.07}
      If (FFAnsiCompareText(ThisExtension, fsc_ExtForData) = 0) Or
        (FFAnsiCompareText(ThisExtension, fsc_ExtForTrans) = 0) Or
        (FFAnsiCompareText(ThisExtension, fsc_ExtForSQL) = 0) Then
        Begin
          ErrorCode := oeInvalidFileExtension;
          Result := False;
          Exit;
        End; {end !!.06, !!.07}

      { See if this extension is being used for any index files }
      For I := 0 To FIndexList.Count - 1 Do
        With FIndexList.Items[I] Do
          Begin
            If (Name <> '') And
              (I <> grdIndexes.Row - 1) And
              (iiExtension = ThisExtension) Then
              Begin
                ErrorCode := oeDuplicateFileExtension;
                Result := False;
                Exit;
              End;
          End;
    End;
  ErrorCode := 0;
  Result := True;
End;
{--------}

Function TfrmTableStruct.ValidateRestructure: Boolean;
Begin
  { Auto-assign field map }
  If tabStructure.Pages[tabStructure.PageCount - 1].Enabled And
    chkPreserveData.Checked And
    (FFieldMap.Count = 0) Then
    Begin
      btnMatchByNameClick(Nil);
      If (FDatabase.Tables[FTableIndex].RecordCount > 0) And {!!.06}
      (FFieldMap.Count <> FDatabase.Tables[FTableIndex].Dictionary.FieldCount) Then
        Begin
          Result := Not (MessageDlg('Some data may be lost.  Would you like to ' +
            'verify the field mappings?', mtWarning,
            [mbYes, mbNo], 0) = mrYes);
          If Not Result Then
            tabStructure.ActivePage := tbsExistingData;
          Exit;
        End;
    End;

  With tabStructure Do
    If (FDatabase.Tables[FTableIndex].RecordCount > 0) And {!!.06}
    (Not chkPreserveData.Checked Or (FFieldMap.Count = 0)) And
      Pages[PageCount - 1].Enabled Then
      Begin
        Result := MessageDlg('Restructure without preserving existing data?', mtWarning, [mbYes, mbNo], 0) = mrYes;
        Exit;
      End;

  Result := True;
End;
{--------}

Procedure TfrmTableStruct.DisplayValidationError(ErrorCode: Word);
Begin
  Case ErrorCode Of
    oeDuplicateFieldName:
      MessageDlg('A field with this name already exists.', mtError, [mbOk], 0);
    oeInvalidFieldName:
      MessageDlg('Invalid field name.', mtError, [mbOk], 0);
    oeMissingFieldName:
      MessageDlg('A field name is required here.', mtError, [mbOk], 0);
    oeDuplicateIndexName:
      MessageDlg('An index with this name already exists.', mtError, [mbOk], 0);
    oeInvalidIndexName:
      MessageDlg('Invalid index name.', mtError, [mbOk], 0);
    oeMissingIndexName:
      MessageDlg('An index name is required here.', mtError, [mbOk], 0);
    oeDuplicateFileExtension:
      MessageDlg('This file extension has already been used.', mtError, [mbOk], 0);
    oeInvalidFileExtension:
      MessageDlg('Invalid file extension.', mtError, [mbOk], 0);
    oeInvalidFieldUnits:
      MessageDlg('Invalid units for this data type', mtError, [mbOK], 0);
    oeInvalidIndexKeyLength:
      MessageDlg('Must supply index key length for user-defined indexes', mtError, [mbOK], 0);
    oeMaximumIndexKeyLength:
      MessageDlg(Format('Index key length cannot exceed %d', [fscl_MaxKeyLength]), mtError, [mbOK], 0);
  End;
End;
{--------}

Function TfrmTableStruct.ValidateForm: Boolean;
Var
  I: Integer;
Begin
  If Not edtTableName.ReadOnly Then
    Begin
      If edtTableName.Text = '' Then
        Begin
          edtTableName.SetFocus;
          Raise Exception.Create('Invalid table name');
        End;
    End;

  { Make sure we have a correct block size }
  If Not FFVerifyBlockSize(StrToInt(cboBlockSize.Text)) Then
    Begin
      cboBlockSize.SetFocus;
      Raise Exception.Create('Invalid block size');
    End;

  { Make sure the field list is valid }
  { needs to be expanded}
  For I := 0 To FFieldList.Count - 1 Do
    With FFieldList.Items[I] Do
      Begin
        If Not ((Name = '') And (I = FFieldList.Count - 1)) Then
          Begin
            If Name = '' Then
              Begin
                With grdFields Do
                  Begin
                    Row := I + FixedRows;
                    Col := cnFldName;
                  End;
                Raise Exception.Create('Invalid field name');
              End;

            If Not ValidateFieldUnits(fiUnits, I) Then
              Begin
                With grdFields Do
                  Begin
                    Row := I + FixedRows;
                    Col := cnFldUnits;
                  End;
                Raise Exception.Create('Invalid units for this data type');
              End;
          End;
      End;

  { make sure the composite indexes have fields }{begin !!.06}
  For I := 0 To Pred(FIndexList.Count) Do
    If (FIndexList.Items[I].Name <> '') And
      (FIndexList.Items[I].iiKeyTypeIndex = ktComposite) And
      (FIndexList.Items[I].FieldCount = 0) Then
      Raise Exception.CreateFmt
        ('No fields defined for composite index: %s',
        [FIndexList.Items[I].Name]); {end !!.06}

  Result := True;
End;
{--------}

Procedure TfrmTableStruct.grdFieldsExit(Sender: TObject);
Begin
  LeavingFieldsCell(grdFields.Col, grdFields.Row);
End;
{--------}

Procedure TfrmTableStruct.InitializeFieldMapGrid;
Begin
  grdFieldMap.ColCount := cnMapHighest;
  grdFieldMap.RowCount := 2;

  grdFieldMap.ColWidths[cnMapFieldName] := 135;
  grdFieldMap.ColWidths[cnMapDatatype] := 120;
  grdFieldMap.ColWidths[cnMapOldField] := 203;

  grdFieldMap.DefaultRowHeight := cboMapOldField.Height;

  PopulateFieldMapHeader;
End;
{--------}

Procedure TfrmTableStruct.PopulateFieldMapHeader;
Var
  ColNum: Integer;
Begin
  With grdFieldMap Do
    Begin
      BeginUpdate;
      Try
        For ColNum := 0 To cnMapHighest Do
          Case ColNum Of
            cnMapFieldName: Cells[ColNum, 0] := 'New Field Name';
            cnMapDatatype: Cells[ColNum, 0] := 'Data Type';
            cnMapOldField: Cells[ColNum, 0] := 'Old Field';
          End;
      Finally
        EndUpdate;
      End;
    End;
End;
{--------}

Procedure TfrmTableStruct.grdFieldMapKeyPress(Sender: TObject;
  Var Key: Char);
Begin
  If Key = #13 Then
    { Change the selected cell (Enter as tab)}
    With grdFieldMap Do
      If Col < Pred(ColCount) Then
        Col := Col + 1
      Else If Row < Pred(RowCount) Then
        Begin
          Row := Row + 1;
          Col := cnFldName;
        End
      Else
        Begin
          Row := 1;
          Col := cnFldName;
        End
End;
{--------}

Procedure TfrmTableStruct.grdFieldMapSelectCell(Sender: TObject; ACol,
  ARow: Integer; Var CanSelect: Boolean);
Var
  R: TRect;
  Idx: Integer;
Begin
  CanSelect := True;

  { Set any special cell attributes (ComboBoxes, Readonly fields)}
  Case ACol Of
    cnMapOldField:
      Begin
        R := grdFieldMap.CellRect(ACol, ARow);
        ShowCellCombo(cboMapOldField, grdFieldMap, R);
        //        Idx := cboMapOldField.ItemIndex; - Idx only used to return value below
        RetrieveFieldMapSettings(ARow, Idx, cboMapOldField.Items);
        cboMapOldField.ItemIndex := Idx;
      End;
  End;
End;
{--------}

Procedure TfrmTableStruct.cboMapOldFieldChange(Sender: TObject);
Begin
  With grdFieldMap Do
    Begin
      Cells[Col, Row] := TComboBox(Sender).Items[TComboBox(Sender).ItemIndex];
    End;
  tcMapOldFieldChange(Sender);
  grdFieldMap.Invalidate;
End;
{--------}

Procedure TfrmTableStruct.cboMapOldFieldExit(Sender: TObject);
Begin
  TComboBox(Sender).Visible := False;
  FcboMapOldFieldHasBeenFocused := ActiveControl = grdFieldMap; {!!.11}
  { only if Enter key was pressed }
  If FInEnterKeyPressed Then {!!.11}
    If Assigned(ActiveControl) And Not (ActiveControl = grdFieldMap) Then
      ActiveControl.SetFocus
    Else
      Begin
        grdFieldMap.SetFocus;
        grdFieldMap.Perform(WM_KEYDOWN, VK_TAB, 0);
      End;
End;
{--------}

Procedure TfrmTableStruct.RetrieveFieldMapSettings(Const ARow: Integer;
  Var Index: Integer;
  AStrings: TStrings);
Var
  I, J: Integer;
  OldFieldName: TffDictItemName;
  CurrentFieldName: TffDictItemName;
  Disqualified: Boolean;
  DisplayDatatype: TffShStr;
  {Begin !!.11}
  CreateReverseFFieldMap: Boolean;
  IndexOfOldFieldName: Integer;

  { "missing" method in TStringList for optimized finding of Name part;
    IndexOfName iterates through the whole stringlist }

  Function StringListFindFirst(Strings: TStringList; Const S: String; Var Index: Integer): Boolean;
  Var
    L, H, I, C: Integer;
  Begin
    Result := False;
    L := 0;
    H := Strings.Count - 1;
    While L <= H Do
      Begin
        I := (L + H) Shr 1;
        C := AnsiStrLIComp(PChar(Strings[I]), PChar(S), Length(S));
        If C < 0 Then
          L := I + 1
        Else
          Begin
            H := I - 1;
            If C = 0 Then
              Begin
                Result := True;
                If Strings.Duplicates <> dupAccept Then
                  L := I;
              End;
          End;
      End;
    Index := L;
  End;
  {End !!.11}

Begin
  With FFieldList.Items[Pred(ARow)] Do
    Begin
      CurrentFieldName := Name; { from FFieldList.Items[x] }

      { Fill the combo box dropdown list with all old fields that are
        a) assignment compatible with the current new field and
        b) not already assigned to another new field. }
      With AStrings Do
        Begin
          Clear;
          BeginUpdate;
          {Begin !!.11}
          CreateReverseFFieldMap := Not Assigned(ReverseFFieldMap);
          If CreateReverseFFieldMap Then
            ReverseFFieldMap := TStringList.Create;
          {End !!.11}

          Try
            {Begin !!.11}
            If CreateReverseFFieldMap Then
              For i := 0 To Pred(FFieldMap.Count) Do
                ReverseFFieldMap.Values[FFieldMap.Values[FFieldMap.Names[i]]] := FFieldMap.Names[i];
            ReverseFFieldMap.Sorted := True;
            {End !!.11}
            Add('<none>');
            With FDatabase.Tables[FTableIndex].Dictionary Do
              Begin
                For I := 0 To FieldCount - 1 Do
                  Begin
                    OldFieldName := FieldName[I];

                    { Check assignment compatability }
                    Disqualified := FSConvertSingleField(
                      Nil,
                      Nil,
                      FieldType[I],
                      fiDatatype,
                      -1,
                      -1,
                      -1,
                      -1,
                      -1,
                      rNone,
                      rNone,
                      ChkRange.Checked) <> DBIERR_NONE;

                    { Already assigned to another new field?
                      (make sure to skip the current field) }
                    If Not Disqualified Then
                      Begin

                        (*            this loop has been optimized away. without the optimization,
                                      entering the "existing data" tab of a table with some
                                      hundred fields would take several minutes.
                                      Instead of potentially looping through the whole fieldmap
                                      list of strings for each row, we now build a list with the
                                      names and values reversed which is used during the entire
                                      populate procedure of the grid. With the added binary-search
                                      enabled lookup function this works out to reduce the time spent
                                      populating from 30 seconds to 1 second for a 200-field table.

                                      for J := 0 to FFieldMap.Count - 1 do
                                        if Pos(#255 + CurrentFieldName + '=', #255 + FFieldMap[J]) = 0 then
                                          if Pos('=' + OldFieldName + #255, FFieldMap[J] + #255) <> 0 then begin
                                         Disqualified := True;
                                            Break;
                                          end;*)
                        If StringListFindFirst(ReverseFFieldMap, OldFieldName + '=', IndexOfOldFieldName) And
                          (ReverseFFieldMap[IndexOfOldFieldName] <> OldFieldName + '=' + CurrentFieldName) Then
                          Disqualified := True;
                      End; { if }

                    If Disqualified Then
                      Continue;

                    { If OK, then add it to the list }
                    If FieldType[I] >= fstArrayUInt8 Then
                      DisplayDatatype := Format('(%s[%d])', [FieldDataTypes[FieldType[I]], FieldUnits[I]])
                    Else
                      DisplayDatatype := Format('(%s)', [FieldDataTypes[FieldType[I]]]);
                    Add(FieldName[I] + ' ' + DisplayDatatype);
                  End; { for }
              End; { with }
          Finally
            EndUpdate;
            {Begin !!.11}
            If CreateReverseFFieldMap Then
              Begin
                ReverseFFieldMap.Free;
                ReverseFFieldMap := Nil;
              End;
            {End !!.11}
          End;
        End;

      { See if we already have an assignment for the current field,
        and if so set the combo box index value accordingly }
      With AStrings Do
        Begin
          Index := 0;
          OldFieldName := FFieldMap.Values[CurrentFieldName];
          If OldFieldName <> '' Then
            Begin
              For J := 0 To Count - 1 Do
                If Pos(AnsiUpperCase(OldFieldName + ' ('), AnsiUpperCase(Strings[J])) <> 0 Then
                  Begin
                    Index := J;
                    Break;
                  End; { if }
            End; { if }
        End; { with }
    End;
End;
{--------}

Procedure TfrmTableStruct.tabStructureChange(Sender: TObject);
Begin
  Case tabStructure.ActivePage.PageIndex Of
    1:
      Begin
        PopulateIndexFieldsLists(grdIndexes.Row - 1);
      End;
    2:
      Begin
        grdFieldMap.RowCount := FFieldList.Count;

        { Auto-assign the field map }
        If FFieldMap.Count = 0 Then
          btnMatchByNameClick(Sender);
      End;
  End;
End;
{--------}

Procedure TfrmTableStruct.LeavingIndexCell(Const Col, Row: Integer);
{ Store new data info FFieldList; Update the interface before the
  Cell is changed}
Begin
  If Row < 1 Then
    Exit;

  With FIndexList.Items[Row - 1] Do
    Case Col Of
      cnIdxName:
        Begin
          FIndexList.Items[Row - 1].Name := grdIndexes.Cells[Col, Row];
          If Row = Pred(grdIndexes.RowCount) Then
            { If we've added a name in the empty row,
              add a new empty row to the list }
            If FDialogMode In [dmRestructuring, dmCreating] Then
              FIndexList.AddEmpty;
          If FIndexList.Items[Row - 1].Name <> '' Then
            Begin
              InvalidateIndexesTable;
            End;
        End;

      cnIdxKeyLength:
        iiKeyLen := StrToInt('0' + grdIndexes.Cells[Col, Row]);

      cnIdxExt:
        iiExtension := grdIndexes.Cells[Col, Row];

      cnIdxBlockSize:
        iiBlockSizeIndex := cboIndexBlockSize.Items.IndexOf(grdIndexes.Cells[Col, Row]);

      cnIdxDesc:
        iiDescription := grdIndexes.Cells[Col, Row];
    End;
  InvalidateIndexesRow(grdIndexes.Row);
  grdIndexes.Invalidate;
End;
{--------}

Procedure TfrmTableStruct.grdIndexesExit(Sender: TObject);
Begin
  LeavingIndexCell(grdIndexes.Col, grdIndexes.Row);
  lstIndexFields.Repaint;
End;
{--------}

Procedure TfrmTableStruct.FormKeyPress(Sender: TObject; Var Key: Char);
Begin
  FHasChanged := True;
End;
{--------}

Procedure TfrmTableStruct.lstAvailFieldsDblClick(Sender: TObject);
Begin
  If FDialogMode <> dmViewing Then
    AddFieldToIndex;
End;
{--------}

Procedure TfrmTableStruct.lstIndexFieldsDblClick(Sender: TObject);
Begin
  If lstIndexFields.ItemIndex > -1 Then
    Begin
      FHasChanged := True;
      If Integer(lstIndexFields.Items.Objects[lstIndexFields.ItemIndex]) = 0 Then
        Begin
          lstIndexFields.Items.Objects[lstIndexFields.ItemIndex] := TObject(1);
          FIndexList.Items[grdIndexes.Row - 1].Fields.Objects[lstIndexFields.ItemIndex] := TObject(1);
        End
      Else
        Begin
          lstIndexFields.Items.Objects[lstIndexFields.ItemIndex] := TObject(0);
          FIndexList.Items[grdIndexes.Row - 1].Fields.Objects[lstIndexFields.ItemIndex] := TObject(0);
        End;
      lstIndexFields.Repaint;
    End;
End;
{--------}

Function TfrmTableStruct.AllowDefaultField(aRowNum: Integer;
  Var aErrorCode: Word): Boolean;
Var
  FieldType: TfsFieldType;
Begin
  Assert(Assigned(FFieldList.Items[pred(aRowNum)]));
  Assert(Assigned(grdFields));
  Assert(grdFields.ColCount > cnFldUnits);
  Assert(grdFields.RowCount > aRowNum);
  Result := False;
  FieldType := FFieldList.Items[pred(aRowNum)].FieldType;
  {This field type must allow default values}
  If FFEFieldAllowedDefault(FieldType) Then
    Begin
      Result := True;
      {if this field type requires units, ensure it's set}
      If ((FFEFieldTypeRequiresUnits(FieldType)) And
        (grdFields.Cells[cnFldUnits, aRowNum] = '0')) Then
        Result := False;
    End;
End;
{--------}

Function TfrmTableStruct.ValidDefaultFieldKey(aUpKey: Char; aFieldType: TfsFieldType): Boolean;
Type
  CharSet = Set Of Char;
Const
  valValidNumber = ['0'..'9'];
  valValidAlpha = ['A'..'Z'];
  valValidBoolean = ['T', 'R', 'U', 'E', 'F', 'A', 'L', 'S'];
  valValidExponent = ['E']; {!!.10}
  valValidNegative = ['-'];
  valValidSpace = [' '];
  valValidAll = [#8, #9];
  valNow = ['N', 'n', 'O', 'o', 'W', 'w'];
Var
  valValidAMPM: Set Of Char;
  valValidDecSep: Set Of Char;
  valValidDateSep: Set Of Char;
  valValidTimeSep: Set Of Char;
  i: Integer;
Begin
  Result := True;
  If Result Then
    Exit;

  valValidAMPM := [];
  For i := 1 To Length(TimeAMString) Do
    Include(valValidAMPM, UpCase(TimeAMString[i]));
  For i := 1 To Length(TimePMString) Do
    Include(valValidAMPM, UpCase(TimePMString[i]));
  valValidDecSep := [];
  valValidDateSep := [];
  valValidTimeSep := [];
  Include(valValidDecSep, UpCase(DecimalSeparator));
  Include(valValidDateSep, UpCase(DateSeparator));
  Include(valValidTimeSep, UpCase(TimeSeparator));

  Case aFieldType Of
    fstBoolean: Result := aUpKey In valValidBoolean;
    fstSingleChar,
      fstSingleWideChar: Result := aUpKey In (valValidNumber + valValidAlpha + valValidSpace);
    fstUInt8,
      fstInt8: Result := True;
    fstInt16,
      fstInt32: Result := True;
    fstUInt16,
      fstUInt32,
      fstInt64: Result := True;
    fstSingle: Result := True;
    fstDouble,
      fstExtended,
      fstCurrency, fstBcd: Result := True;
    fstDate: Result := True;
    fstTime: Result := True;
    fstDateTime: Result := True;
  End;
End;
{--------}

Procedure TfrmTableStruct.chkAvailFieldsSortedClick(Sender: TObject);
Begin
  lstAvailFields.Items.BeginUpdate;
  Try
    lstAvailFields.Sorted := chkAvailFieldsSorted.Checked;
    PopulateIndexFieldsLists(grdIndexes.Row - 1);
  Finally
    lstAvailFields.Items.EndUpdate;
  End;
End;
{--------}

Procedure TfrmTableStruct.grdIndexesEnterCell(Sender: TfsGrid; aCol,
  aRow: Integer; Const text: String);
Begin
  EnableIndexControls(aRow, '');
  lstIndexFields.Repaint;
End;
{--------}

Procedure TfrmTableStruct.cboFieldTypeKeyDown(Sender: TObject;
  Var Key: Word; Shift: TShiftState);
Begin
  If Key = VK_RETURN Then
    Begin
      Key := 0;
      grdFields.SetFocus;
    End;
End;
{--------}

Procedure TfrmTableStruct.cboIndexTypeKeyDown(Sender: TObject;
  Var Key: Word; Shift: TShiftState);
Begin
  If Key = VK_RETURN Then
    Begin
      Key := 0;
      grdIndexes.SetFocus;
    End;
End;
{--------}

Procedure TfrmTableStruct.cboMapOldFieldKeyDown(Sender: TObject;
  Var Key: Word; Shift: TShiftState);
Begin
  If Key = VK_RETURN Then
    Begin
      FInEnterKeyPressed := True; {!!.11}
      Try
        Key := 0;
        grdFieldMap.SetFocus;
      Finally
        FInEnterKeyPressed := False; {!!.11}
      End;
    End;
End;
{--------}

Procedure TfrmTableStruct.tabExistingDataChange(Sender: TObject);
Begin
  tabFieldMapPageChanged(Sender, 2);
End;
{--------}

Procedure TfrmTableStruct.edtBlobExtensionExit(Sender: TObject); {begin !!.06}
Var
  ErrorCode: Word;
Begin
  If Not edtBLOBExtensionValidation(edtBlobExtension.Text, ErrorCode) Then
    Begin
      DisplayValidationError(ErrorCode);
      edtBlobExtension.Text := '';
    End;
End; {end !!.06}
{--------}
{Begin !!.11}

Procedure TfrmTableStruct.FormClose(Sender: TObject;
  Var Action: TCloseAction);
Begin
  {$IFDEF DCC4OrLater}
  Action := caFree;
  {$ENDIF}
End;
{End !!.11}

Procedure TfrmTableStruct.CboBlobChange(Sender: TObject);
Begin
  With grdFields Do
    Begin
      Cells[Col, Row] := cboBlob.Items[cboBlob.ItemIndex];
      LeavingFieldsCell(grdFields.Col, grdFields.Row);
    End;
  grdFields.Invalidate;
End;

Procedure TfrmTableStruct.CboBlobExit(Sender: TObject);
Begin
  cboblob.Visible := False;
  If Assigned(ActiveControl) And Not (ActiveControl = grdFields) Then
    ActiveControl.SetFocus
  Else
    Begin
      grdFields.SetFocus;
      grdFields.Perform(WM_KEYDOWN, VK_TAB, 0);
    End;
End;

Procedure TfrmTableStruct.CboBlobKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If Key = VK_RETURN Then
    Begin
      Key := 0;
      grdFields.SetFocus;
    End;
End;

Procedure TfrmTableStruct.CRoundChange(Sender: TObject);
Begin
  With grdFields Do
    Begin
      Cells[Col, Row] := cround.Items[cround.ItemIndex];
      LeavingFieldsCell(grdFields.Col, grdFields.Row);
    End;
  grdFields.Invalidate;
End;

Procedure TfrmTableStruct.CRoundExit(Sender: TObject);
Begin
  cround.Visible := False;
  If Assigned(ActiveControl) And Not (ActiveControl = grdFields) Then
    ActiveControl.SetFocus
  Else
    Begin
      grdFields.SetFocus;
      grdFields.Perform(WM_KEYDOWN, VK_TAB, 0);
    End;
End;

Procedure TfrmTableStruct.CRoundKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If Key = VK_RETURN Then
    Begin
      Key := 0;
      grdFields.SetFocus;
    End;
End;

Procedure TfrmTableStruct.pnlFieldDetailResize(Sender: TObject);
Begin
  edtBlobFileDesc.Width := pnlFieldDetail.Width - edtBlobFileDesc.Left - 10;
End;

Procedure TfrmTableStruct.Edit1KeyPress(Sender: TObject; Var Key: Char);
Begin
  If Not (Key In ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']) Then key := #0;
End;

Procedure TfrmTableStruct.CdefaultChange(Sender: TObject);
Begin
  With grdFields Do
    Begin
      Cells[Col, Row] := cdefault.Items[cdefault.ItemIndex];
      LeavingFieldsCell(grdFields.Col, grdFields.Row);
    End;
  grdFields.Invalidate;
End;

Procedure TfrmTableStruct.CdefaultExit(Sender: TObject);
Begin
  Cdefault.Visible := False;
  If Assigned(ActiveControl) And Not (ActiveControl = grdFields) Then
    ActiveControl.SetFocus
  Else
    Begin
      grdFields.SetFocus;
      grdFields.Perform(WM_KEYDOWN, VK_TAB, 0);
    End;
End;

Procedure TfrmTableStruct.CdefaultKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If Key = VK_RETURN Then
    Begin
      Key := 0;
      grdFields.SetFocus;
    End;
End;

Procedure TfrmTableStruct.lstIndexFieldsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
Var
  r: TRect;
  i, j, k, l: Integer;
Begin
  Try
    r := Rect;
    r.Right := r.Left + 18;
    r.Bottom := r.Top + 16;
    OffsetRect(r, 2, 0);
    i := Integer(lstIndexFields.Items.Objects[Index]);
    j := 1;
    If FIndexList.Items[grdIndexes.Row - 1].FieldsNullTop.Count - 1 >= Index Then
      j := Integer(FIndexList.Items[grdIndexes.Row - 1].FieldsNullTop.Objects[Index]);
    k := 0;
    If FIndexList.Items[grdIndexes.Row - 1].FieldsSize.Count - 1 >= Index Then
      k := Integer(FIndexList.Items[grdIndexes.Row - 1].FieldsSize.Objects[Index]);
    With lstIndexFields.Canvas Do
      Begin
        FillRect(Rect);
        ImageList1.Draw(lstIndexFields.Canvas, r.Left + 22, r.Top, i);
        ImageList1.Draw(lstIndexFields.Canvas, r.Left + 58, r.Top, j);
        TextOut(Rect.Left + 92, Rect.Top + 1, IntToStr(k));
        TextOut(Rect.Left + 148, Rect.Top + 1, lstIndexFields.Items[Index]);
      End;
  Except
  End;
End;

Procedure TfrmTableStruct.lstIndexFieldsClickCheck(Sender: TObject);
Begin
  If lstIndexFields.ItemIndex > -1 Then
    Begin
      If lstIndexFields.Checked[lstIndexFields.ItemIndex] Then
        FIndexList.Items[grdIndexes.Row - 1].FieldsCase.Objects[lstIndexFields.ItemIndex] := TObject(1)
      Else
        FIndexList.Items[grdIndexes.Row - 1].FieldsCase.Objects[lstIndexFields.ItemIndex] := TObject(0);
    End;
End;

Procedure TfrmTableStruct.btnPropFieldClick(Sender: TObject);
Var
  frmPropFieldIdx: TfrmPropFieldIdx;
  i, j, aSize: Integer;
Begin
  If lstIndexFields.ItemIndex > -1 Then
    Begin
      FHasChanged := True;
      frmPropFieldIdx := TfrmPropFieldIdx.Create(Nil);
      Try
        frmPropFieldIdx.checkbox3.checked := lstIndexFields.Checked[lstIndexFields.ItemIndex];
        i := Integer(lstIndexFields.Items.Objects[lstIndexFields.ItemIndex]);
        frmPropFieldIdx.checkbox1.checked := boolean(i);
        i := Integer(FIndexList.Items[grdIndexes.Row - 1].FieldsNullTop.Objects[lstIndexFields.ItemIndex]);
        frmPropFieldIdx.checkbox2.checked := boolean(i);
        frmPropFieldIdx.SpinEdit1.Value := 0;
        frmPropFieldIdx.SpinEdit1.enabled := False;
        With FIndexList.Items[grdIndexes.Row - 1] Do
          Begin
            J := FFieldList.IndexOf(FieldName[lstIndexFields.ItemIndex]);
            If J <> -1 Then
              Begin
                aSize := FFieldList.Items[J].fiUnits;
                frmPropFieldIdx.SpinEdit1.enabled := (FFieldList.Items[J].FieldType In [fstShortString,
                  fstVarNullString, fstNullString, fstWideString, fstVarWideString]);
                If frmPropFieldIdx.SpinEdit1.enabled Then
                  Begin
                    frmPropFieldIdx.SpinEdit1.MaxValue := aSize;
                    frmPropFieldIdx.SpinEdit1.Value := Integer(FIndexList.Items[grdIndexes.Row - 1].FieldsSize.Objects[lstIndexFields.ItemIndex]);
                  End;
              End;
          End;

        If frmPropFieldIdx.ShowModal = mrOK Then
          Begin
            If lstIndexFields.ItemIndex > -1 Then
              Begin
                lstIndexFields.Checked[lstIndexFields.ItemIndex] := frmPropFieldIdx.checkbox3.checked;
                FIndexList.Items[grdIndexes.Row - 1].FieldsCase.Objects[lstIndexFields.ItemIndex] :=
                  TObject(Byte(frmPropFieldIdx.checkbox3.checked));

                lstIndexFields.Items.Objects[lstIndexFields.ItemIndex] :=
                  TObject(Byte(frmPropFieldIdx.checkbox1.checked));
                FIndexList.Items[grdIndexes.Row - 1].Fields.Objects[lstIndexFields.ItemIndex] :=
                  TObject(Byte(frmPropFieldIdx.checkbox1.checked));

                FIndexList.Items[grdIndexes.Row - 1].FieldsNullTop.Objects[lstIndexFields.ItemIndex] :=
                  TObject(Byte(frmPropFieldIdx.checkbox2.checked));

                If frmPropFieldIdx.SpinEdit1.enabled Then
                  Begin
                    If frmPropFieldIdx.SpinEdit1.Value > aSize Then
                      frmPropFieldIdx.SpinEdit1.Value := aSize;
                    FIndexList.Items[grdIndexes.Row - 1].FieldsSize.Objects[lstIndexFields.ItemIndex] := TObject(frmPropFieldIdx.SpinEdit1.Value);
                  End;
                With FIndexList.Items[grdIndexes.Row - 1] Do
                  Begin
                    iiKeyLen := CalcKeyLength(grdIndexes.Row - 1);
                  End;
                lstIndexFields.repaint;
                LeavingIndexCell(grdIndexes.Col, grdIndexes.Row);
                grdIndexes.Invalidate;
              End;
          End;
      Finally
        frmPropFieldIdx.free;
      End;
    End;
End;

Procedure TfrmTableStruct.Button1Click(Sender: TObject);
Begin
  ShowDictionary(FTableIndex);
End;

End.

