{*********************************************************}
{* Create/View/Restructure Table Definition Dialog       *}
{*********************************************************}

(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower FlashFiler
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{$I ffdefine.inc}

unit fmstruct;

interface

uses
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
  StdCtrls,
  ComCtrls,
  Buttons,
  ExtCtrls,
  ffllgrid,
  ffsrbde,
  ffllbase,
  fflldict,
  uelement,
  uentity,
  uconfig,
  dgimpdef;

type
  TffeDialogMode = (dmNeutral, dmViewing, dmCreating, dmRestructuring);
  TffeViewType   = (vtViewFields, vtViewIndexes);
  TffeDrawType   = (dtNormal, dtGrayed, dtChecked, dtUnchecked, dtWordWrap, dtIgnore);

  TffeCellComboBoxInfo = packed record
    Index : integer;                {index into Items list}
    {$IFDEF CBuilder}
    case integer of
      0 : (St      : array[0..255] of char);
      1 : (RTItems : TStrings;
           RTSt    : array[0..255] of char);
    {$ELSE}
    case integer of
      0 : (St      : ShortString);  {string value if Index = -1}
      1 : (RTItems : TStrings;      {run-time items list}
           RTSt    : ShortString);  {run-time string value if Index = -1}
    {$ENDIF}
  end;

  TfrmTableStruct = class(TForm)
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
    grdOrphanedFields: TffStringGrid;
    grdFields: TffStringGrid;
    grdFieldMap: TffStringGrid;
    cboFieldType: TComboBox;
    pnlFieldDetail: TPanel;
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
    btnPrint: TBitBtn;
    btnRestructure: TBitBtn;
    btnCancel: TBitBtn;
    pnlIndexDetail: TPanel;
    grpCompositeKey: TGroupBox;
    splIndex: TSplitter;
    grdIndexes: TffStringGrid;
    cboIndexType: TComboBox;
    cboIndexBlockSize: TComboBox;
    pnlDeleteIndex: TPanel;
    pnlExistingDataHeader: TPanel;
    chkPreserveData: TCheckBox;
    pnlExistingDataButtons: TPanel;
    btnMatchByName: TButton;
    btnMatchByPosition: TButton;
    btnClearAll: TButton;
    cboMapOldField: TComboBox;
    chkEncryptData: TCheckBox;
    btnDeleteIndex: TButton;
    pnlCompButtons: TPanel;
    btnAddIndexField: TSpeedButton;
    btnRemoveIndexField: TSpeedButton;
    pnlCompFieldsInIndex: TPanel;
    lstIndexFields: TListBox;
    pnlCompAvailFields: TPanel;
    lblFieldsInIndex: TLabel;
    lstAvailFields: TListBox;
    lblAvailableFields: TLabel;
    chkAvailFieldsSorted: TCheckBox;
    btnMoveIndexFieldUp: TSpeedButton;
    btnMoveIndexFieldDown: TSpeedButton;
    Label1: TLabel;
    edtDescription: TEdit;
    {=====Form and general events=====}
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnCreateClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnPrintClick(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure btnRestructureClick(Sender: TObject);
    procedure btnInsertFieldClick(Sender: TObject);
    procedure btnDeleteFieldClick(Sender: TObject);
    procedure btnMoveFieldUpClick(Sender: TObject);
    procedure btnMoveFieldDownClick(Sender: TObject);
    procedure radBLOBInternalClick(Sender: TObject);
    procedure cboFieldTypeChange(Sender: TObject);
    procedure cboFieldTypeExit(Sender: TObject);
    procedure grdFieldsEnter(Sender: TObject);
    procedure grdFieldsSelectCell(Sender    : TObject;
                                  Col, Row  : Integer;
                              var CanSelect : Boolean);
    procedure grdFieldsDrawCell(Sender     : TObject;
                                ACol, ARow : Integer;
                                Rect       : TRect;
                                State      : TGridDrawState);
    procedure grdFieldsKeyPress(Sender: TObject; var Key: Char);
    procedure grdFieldsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);


    {=====Indexes tab events=====}
    procedure btnDeleteIndexClick(Sender: TObject);
    procedure btnAddIndexFieldClick(Sender: TObject);
    procedure btnRemoveIndexFieldClick(Sender: TObject);
    procedure btnMoveIndexFieldUpClick(Sender: TObject);
    procedure btnMoveIndexFieldDownClick(Sender: TObject);
    procedure lstIndexFieldsDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure lstIndexFieldsDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure lstAvailFieldsDragOver(Sender, Source: TObject; X,
      Y: Integer; State: TDragState; var Accept: Boolean);
    procedure lstAvailFieldsDragDrop(Sender, Source: TObject; X,
      Y: Integer);
    procedure cboIndexTypeChange(Sender: TObject);
    procedure cboIndexTypeExit(Sender: TObject);
    procedure grdIndexesEnter(Sender: TObject);
    procedure grdIndexesSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure grdIndexesKeyPress(Sender: TObject; var Key: Char);
    procedure grdIndexesDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure grdIndexesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    {=====Existing data tab events=====}
    procedure tabFieldMapPageChanged(Sender: TObject; Index: Integer);
    procedure btnMatchByNameClick(Sender: TObject);
    procedure btnMatchByPositionClick(Sender: TObject);
    procedure btnClearAllClick(Sender: TObject);
    procedure chkPreserveDataClick(Sender: TObject);
    procedure grdFieldMapEnter(Sender: TObject);
    procedure grdFieldMapActiveCellMoving(Sender: TObject; Command: Word;
      var RowNum: Longint; var ColNum: Integer);
    procedure tcMapOldFieldChange(Sender: TObject);
    procedure grdFieldsExit(Sender: TObject);
    procedure grdFieldMapKeyPress(Sender: TObject; var Key: Char);
    procedure grdFieldMapSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure cboMapOldFieldChange(Sender: TObject);
    procedure cboMapOldFieldExit(Sender: TObject);
    procedure tabStructureChange(Sender: TObject);
    procedure grdIndexesExit(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure lstAvailFieldsDblClick(Sender: TObject);
    procedure lstIndexFieldsDblClick(Sender: TObject);
    procedure chkAvailFieldsSortedClick(Sender: TObject);
    procedure grdIndexesEnterCell(Sender: TffStringGrid; aCol,
      aRow: Integer; const text: String);
    procedure cboFieldTypeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cboIndexTypeKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cboMapOldFieldKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tabExistingDataChange(Sender: TObject);
    procedure edtBlobExtensionExit(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);

  private
    procedure CMDialogKey(var msg: TCMDialogKey); message CM_DIALOGKEY;
  protected
    FDialogMode : TffeDialogMode;
    FHasChanged : Boolean;
      { This flag is used to keep track of whether or not the information in the
        dialogs has changed. The approach is simplistic, a better
        approach would be to compare the current dict, and potential dict.
        Perhaps this could be done at a later point. }
    FDatabase : TffeDatabaseItem;
    FOutputDictionary: TffDataDictionary;
    FFieldList: TffeFieldList;
    FIndexList: TffeIndexList;
    FTempElementNumber: LongInt;
    FTempStr: TffShStr;
    FTableIndex: LongInt;
    FFieldMapComboRec: TffeCellComboBoxInfo;
    FFieldMap: TStringList;
    ReverseFFieldMap: TStringList;                                    {!!.11}
      { to optimize lookup of fieldmappings }
    FInEnterKeyPressed : Boolean;                                     {!!.11}
    FcboMapOldFieldHasBeenFocused: Boolean;                           {!!.11}
    FFieldMapInShiftTab : Boolean;                                    {!!.11}

    procedure AddFieldToIndex;
    procedure RemoveFieldFromIndex;

  public
    {=====General Routines=====}
    procedure AlignButtons;
    procedure PopulateForm(aTableIndex: LongInt; aReadOnly: Boolean);
    procedure DrawCell(Grid : TffStringGrid; DrawType: TffeDrawType;
                       Rect: TRect; State: TGridDrawState; CellText: string);
    procedure ShowCellCombo(ComboBox: TCustomComboBox; Grid: TCustomGrid;
                            Rect: TRect);

    {=====Dictionary Routines=====}
    procedure BuildDictionary;
    procedure LoadDictionary(aTableIndex: LongInt);
    procedure CreateTable(aTableName: TffTableName);
    procedure PrintDictionary(aTableIndex: LongInt; aPrintToFile: Boolean);

    {=====Field Grid Routines=====}
    procedure InitializeFieldGrid;
    procedure PopulateFieldGridHeader;
    procedure InvalidateFieldsTable;
    procedure InvalidateFieldsRow(const RowNum : Integer);
    procedure EnableBLOBControls;
    procedure EnableFieldControls(aRowNum: LongInt);
    procedure LeavingFieldsCell(const Col, Row: LongInt);

    {=====Index Grid Routines=====}
    procedure InitializeIndexGrid;
    procedure PopulateIndexGridHeader;
    procedure PopulateIndexFieldsLists(aIndex: LongInt);
    procedure InvalidateIndexesTable;
    procedure InvalidateIndexesRow(const RowNum: Integer);
    function CalcKeyLength(aIndex: Integer): Integer;
    procedure EnableIndexControls(aRowNum: LongInt; aName: string);
    procedure LeavingIndexCell(const Col, Row: Longint);

    {=====FieldMap Routines=====}
    procedure InitializeFieldMapGrid;
    procedure PopulateFieldMapHeader;
    procedure InvalidateFieldMapTable;
    procedure InvalidateFieldMapRow(const RowNum: Integer);
    procedure RetrieveFieldMapSettings(const ARow : integer;
                                         var Index: Integer;
                                             AStrings: TStrings);

    {=====FieldGrid Validation Routines=====}
    function AllowDefaultField(aRowNum    : Integer;
                           var aErrorCode : Word) : Boolean;
    function FieldNameValidation(const AName     : string;
                                   var ErrorCode : Word) : Boolean;
    function FieldLengthValidation(const ALength   : string;
                                     var ErrorCode : Word): Boolean;
    function ValidateFieldUnits(aUnits, aFieldNum: Integer): Boolean;
    function ValidDefaultFieldKey(aUpKey     : Char;
                                  aFieldType : TffFieldType) : Boolean;

    {=====IndexGrid Validation Routines=====}
    function IndexNameValidation(const AName: string;
                                   var ErrorCode: Word): Boolean;
    function IndexExtensionValidation(const AExtension: string;
                                        var ErrorCode: Word): Boolean;
    function IndexKeyLenValidation(const AKeyLen: Integer;
                                     var ErrorCode: Word): Boolean;
    {Misc Validation Routines}
    function edtBLOBExtensionValidation(const AExtension: string;
                                          var ErrorCode: Word): Boolean;
    function ValidateRestructure: Boolean;
    procedure DisplayValidationError(ErrorCode: Word);
    function ValidateForm: Boolean;
  end;

{=====Entry-Point routines=====}
function ShowCreateTableDlg(aDatabase : TffeDatabaseItem;
                        var aTableIndex: LongInt;
                        DefaultFieldDefs: TFieldDefs): TModalResult;    {!!.11}

function ShowRestructureTableDlg(aDatabase : TffeDatabaseItem;
                                 aTableIndex: LongInt): TModalResult;

procedure ShowViewTableStructureDlg(aDatabase : TffeDatabaseItem;
                                    aTableIndex : longInt; aViewType: TffeViewType);

var
  frmTableStruct: TfrmTableStruct;

implementation

{$R *.DFM}

uses
  FFConvFF,
  dgPrintg,
  uBase,
  uConsts,
  FFStDate,
  FFCLConv,
  FFUtil,                                                             {!!.06}
  Printers;

const

{===== Grid column constants =====}
  cnFldNumber        = 0;
  cnFldName          = 1;
  cnFldType          = 2;
  cnFldUnits         = 3;
  cnFldDecPl         = 4;
  cnFldRequired      = 5;
  cnFldDefault       = 6;
  cnFldDesc          = 7;
  cnFldHighest       = 7;

  cnIdxNumber        = 0;
  cnIdxName          = 1;
  cnIdxType          = 2;
  cnIdxKeyLength     = 3;
  cnIdxUnique        = 4;
  cnIdxAscending     = 5;
  cnIdxCaseSensitive = 6;
  cnIdxExt           = 7;
  cnIdxBlockSize     = 8;
  cnIdxDesc          = 9;
  cnIdxHighest       = 9;

  cnMapFieldName = 0;
  cnMapDatatype  = 1;
  cnMapOldField  = 2;
  cnMapHighest   = 3;

  { Cell margin constants }
  cnTopMargin = 3;
  cnLeftMargin = 3;

{===== Grid column names =========}
cnsAscend = 'Ascend';
cnsBlockSize = 'Block size';
cnsCaseSens = 'Case';
cnsDataType = 'Data type';
cnsDecPl = 'Decimals';
cnsDefault = 'Default';
cnsDesc = 'Description';
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

function ShowCreateTableDlg(aDatabase: TffeDatabaseItem;
                        var aTableIndex: LongInt;
                        DefaultFieldDefs: TFieldDefs): TModalResult;     {!!.11}
var
  FieldIdx : Integer;
  OldCursor: TCursor;
  FFType   : TffFieldType;                   {!!.11}
  FFSize   : word;                           {!!.11}
begin
  Assert(Assigned(aDatabase));
  with TfrmTableStruct.Create(nil) do
  try
    HelpContext := hcDefineNewTableDlg;
    OldCursor := Screen.Cursor;
    Screen.Cursor := crHourglass;
    try
      tabStructure.ActivePage := tbsFields;
      FDialogMode := dmCreating;
      tbsExistingData.TabVisible := False;
      cboBlockSize.Style := csDropDownList;
      cboBlockSize.Enabled := True;
      cboBlockSize.Color := clWindow;
      cboBlockSize.TabStop := True;

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

      { Set up the fields tab }
      with grdFields do
        Options := Options + [goEditing] +  [goAlwaysShowEditor];

      {Begin !!.11}
      { in order to be able to open the New Table dialog with
        predefined fields, the DefaultFieldDefs parameter and
        this block was added. 
      }
      if Assigned(DefaultFieldDefs) then begin
        grdFields.BeginUpdate;
        try
          for FieldIdx := 0 to Pred(DefaultFieldDefs.Count) do begin
            MapVCLTypeToFF(DefaultFieldDefs[FieldIdx].DataType,
                           DefaultFieldDefs[FieldIdx].Size,
                           FFType,
                           FFSize);
            FFieldList.Insert(DefaultFieldDefs[FieldIdx].Name,
                              FFEFieldTypeToIndex(FFType),
                              FFSize,
                              0,
                              False,
                              '',
                              NIL);
          end;
          grdFields.RowCount := grdFields.FixedRows + DefaultFieldDefs.Count;
        finally
          InvalidateFieldsTable;
          grdFields.EndUpdate;
          { moves focus to the grid. this is intentional; if we let focus
            remain on the tablename, then the top left editable cell doesn't
            draw properly. }
          ActiveControl := grdFields;
        end;
      end;
      {End !!.11}

      FFieldList.AddEmpty;
      InvalidateFieldsTable;                         {!!.11}

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
      with grdIndexes do
        Options := Options + [goEditing] + [goAlwaysShowEditor];

      FIndexList.AddEmpty;

      btnImport.Enabled := (FDatabase.TableCount > 0);
      btnImport.Visible := True;
      btnCreate.Visible := True;

      FTableIndex := -1;
      grdFields.Invalidate;
    finally
      Screen.Cursor := OldCursor;
    end;
    Result := ShowModal;
    if Result = mrOK then
      aTableIndex := FTableIndex;
  finally
    Free;
  end;
end;
{--------}
function ShowRestructureTableDlg(aDatabase   : TffeDatabaseItem;
                                 aTableIndex : LongInt): TModalResult;
var
  OldCursor: TCursor;
begin
  Assert(Assigned(aDatabase));
  with TfrmTableStruct.Create(nil) do
  try
    cboBlockSize.Style := csDropDownList;
    cboBlockSize.Enabled := True;
    cboBlockSize.Color := clWindow;
    cboBlockSize.TabStop := True;
    HelpContext := hcRedefineTableDlg;
    OldCursor := Screen.Cursor;
    Screen.Cursor := crHourglass;
    try
      tabStructure.ActivePage := tbsFields;
      FDialogMode := dmRestructuring;
      FTableIndex := aTableIndex;
      FDatabase := aDatabase;

      with FDatabase.Tables[aTableIndex] do begin
        Caption := 'Redefine Table: ' + TableName + ' in ' +
                   Server.ServerName + '\' + Database.DatabaseName;

        { Disable the field map if there is no data }
        if RecordCount = 0 then
          with tabStructure do
            Pages[PageCount - 1].TabVisible := False;
      end;

      PopulateForm(aTableIndex, False);

      edtTableName.Text := FDatabase.Tables[FTableIndex].TableName;
      edtTableName.ReadOnly := True;
      edtTableName.ParentColor := True;
      edtTableName.TabStop := False;

      { Set up the fields tab }
      with grdFields do
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
      with grdIndexes do
        Options := Options + [goEditing] + [goAlwaysShowEditor];

      btnImport.Enabled := (FDatabase.TableCount > 0);
      btnImport.Width := btnRestructure.Width;
      btnImport.Visible := True;
      btnRestructure.Visible := True;
      ActiveControl := grdFields;
    finally
      Screen.Cursor := OldCursor;
    end;
    Result := ShowModal;
  finally
    Free;
  end;
end;
{--------}
procedure ShowViewTableStructureDlg(aDatabase : TffeDatabaseItem;
                                    aTableIndex : longInt; aViewType: TffeViewType);
var
  OldCursor: TCursor;
begin
  Assert(Assigned(aDatabase));
  with TfrmTableStruct.Create(nil) do
  try
    HelpContext := hcViewTableDlg;
    OldCursor := Screen.Cursor;
    Screen.Cursor := crHourglass;
    try
      FDialogMode := dmViewing;
      FDatabase := aDatabase;
      FTableIndex := aTableIndex;

      tbsExistingData.TabVisible := False;

      with FDatabase.Tables[aTableIndex] do
        Caption := 'Table Definition: ' + TableName + ' in ' +
                   Server.ServerName + '\' + Database.DatabaseName;

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

      chkAvailFieldsSorted.Visible := False;

      with tabStructure do
        case aViewType of
          vtViewFields:
            begin
              ActivePage := tbsFields;
              ActiveControl := grdFields;
            end;
          vtViewIndexes:
            begin
              ActivePage := tbsIndexes;
              ActiveControl := grdIndexes;
            end;
        end;

      with grdFields do begin
        EditorMode := False;
        Options := Options - [goEditing] - [goAlwaysShowEditor];
      end;

      PopulateForm(aTableIndex, True);

      { Set BLOB views after loading the dictionary }
      grpBLOBViewStorage.Visible := True;
      grpBLOBEditStorage.Visible := False;

      with FDatabase.Tables[aTableIndex], Dictionary do begin
        if BLOBFileNumber = 0 then
          lblBLOBViewStorage.Caption :=
            'BLOBs are stored in the main data file.'
        else
          lblBLOBViewStorage.Caption :=
            Format('BLOBs are stored in file %s, block size = %d, description = "%s"',
                   [TableName + '.' + FileExt[BLOBFileNumber],
                    FileBlockSize[BLOBFileNumber], FileDesc[BLOBFileNumber]]);
      end;

      { Adjust the table encryption group }
      chkEncryptData.Enabled := False;
      chkEncryptData.Top := grpBLOBViewStorage.Top + 5;

      { Hide the field editing controls }
      btnInsertField.Visible := False;
      btnDeleteField.Visible := False;
      btnMoveFieldUp.Visible := False;
      btnMoveFieldDown.Visible := False;

      { Adjust the fields grid to larger space }
      grdFields.Height := grpBLOBViewStorage.Top - grdFields.Top - 2;

      { Hide index field editing controls }
      with grdIndexes do begin
        Options := Options - [goEditing] - [goAlwaysShowEditor];
      end;

      btnDeleteIndex.Visible := False;
      lstIndexFields.DragMode := dmManual;
      lstAvailFields.DragMode := dmManual;
      btnAddIndexField.Enabled := False;
      btnRemoveIndexField.Enabled := False;
      btnMoveIndexFieldUp.Enabled := False;
      btnMoveIndexFieldDown.Enabled := False;

      btnPrint.Visible := True;
    finally
      Screen.Cursor := OldCursor;
    end;
{Begin !!.11}
{$IFDEF DCC4OrLater}
    Show;
  finally
  end;
{$ELSE}
    ShowModal;
  finally
    Free;
  end;
{$ENDIF}
{End !!.11}
end;

{=====Form and general events=====}

procedure TfrmTableStruct.FormCreate(Sender: TObject);
begin
  FHasChanged := False;
  FFieldMapComboRec.RTItems := TStringList.Create;
  FFieldMap := TStringList.Create;
  FDialogMode := dmNeutral;
  btnPrint.Left := btnCreate.Left;

  Left := Application.MainForm.ClientOrigin.X + 100;
  Top := Application.MainForm.ClientOrigin.Y;

  ClientWidth := pnlMain.Width + (pnlMain.Left * 2);
  ClientHeight := pnlMain.Height + (pnlMain.Top * 2);

  FFieldList := TffeFieldList.Create;

  FIndexList := TffeIndexList.Create;

  InitializeFieldGrid;
  InitializeIndexGrid;
  InitializeFieldMapGrid;

  edtBLOBExtension.Text := 'BLB';
  edtBLOBFileDesc.Text := 'BLOB file';

  grpBLOBViewStorage.Left := grpBLOBEditStorage.Left;
  grpBLOBViewStorage.Width := grpBLOBEditStorage.Width;

  grdOrphanedFields.Cells[0,0] := cnsFieldName;
  grdOrphanedFields.Cells[1,0] := cnsDataType;

  FInEnterKeyPressed := False;                                     {!!.11}
  FcboMapOldFieldHasBeenFocused := False;                          {!!.11}
  FFieldMapInShiftTab := False;                                    {!!.11}
end;
{--------}
procedure TfrmTableStruct.FormDestroy(Sender: TObject);
begin
  try
    FFEConfigSaveFormPrefs(ClassName, Self);
    FFEConfigSaveColumnPrefs(ClassName + '.IndexGrid', grdIndexes);
    FFEConfigSaveColumnPrefs(ClassName + '.FieldGrid', grdFields);
    FFEConfigSaveInteger(ClassName, 'IndexSplitterPos', pnlIndexDetail.Height);  {!!.11}
  except
    on E:Exception do
      ShowMessage('Error writing INI file: '+E.Message);
  end;

  Assert(Assigned(Config));
  Config.SortAvailIndexFields := chkAvailFieldsSorted.Checked;
  FFieldMap.Free;
  FFieldMap := nil;
  FFieldMapComboRec.RTItems.Free;
  FFieldMapComboRec.RTItems := nil;
  FFieldList.Free;
  FFieldList := nil;
  FIndexList.Free;
  FIndexList := nil;
end;
{--------}
procedure TfrmTableStruct.FormShow(Sender: TObject);
begin
  { Center dialog }
  SetBounds(((Screen.Width - Width) div 2),
            ((Screen.Height - Height) div 2),
              Width, Height);

  FFEConfigGetFormPrefs(ClassName, Self);
  pnlIndexDetail.Height := FFEConfigGetInteger(ClassName, 'IndexSplitterPos', pnlIndexDetail.Height);  {!!.11}

  AlignButtons;

  if FDialogMode = dmViewing then
    btnCancel.Caption := 'C&lose'
  else
    btnCancel.Caption := 'Cancel';

  { If redefining then set focus to first Name field in grid. }
  if FDialogMode <> dmViewing then
    grdFields.Col := cnFldName;

  { Position to first real index in index grid. }
  if (FDialogMode = dmViewing) and (grdIndexes.RowCount > 2) then
    grdIndexes.Row := 2;

end;
{--------}
procedure TfrmTableStruct.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if not (ModalResult = mrOK) and                        {!!.10}
     (FDialogMode <> dmViewing) and
     (FHasChanged) then begin
    CanClose := (MessageDlg('Are you sure you wish to cancel and lose any changes?',
                            mtConfirmation,
                            [mbYes, mbNo],
                            0) = mrYes);
  end;
end;
{--------}
procedure TfrmTableStruct.btnCreateClick(Sender: TObject);
begin
  {Begin !!.11}
  { force typefield validation and saving }
  if grdFields.Col=cnFldType then begin
    grdFields.Perform(WM_KEYDOWN, VK_TAB, 0);
  end;
  {End !!.11}
  if ValidateForm then begin
    try
      BuildDictionary;
      CreateTable(edtTableName.Text);
      FOutputDictionary.Free;
      FOutputDictionary := nil;
      ModalResult := mrOK;
    except
      { don't close the form }
      raise;
    end;
  end;
end;
{--------}
procedure TfrmTableStruct.btnCancelClick(Sender: TObject);
{Rewritten !!.11}
begin
{$IFDEF DCC4OrLater}
  if fsModal in FormState then
    ModalResult := mrCancel
  else
    Close;
{$ELSE}
  ModalResult := mrCancel;
{$ENDIF}
end;
{--------}
procedure TfrmTableStruct.btnPrintClick(Sender: TObject);
begin
  if dlgPrint.Execute then
    PrintDictionary(FTableIndex, dlgPrint.PrintToFile);
end;
{--------}
procedure TfrmTableStruct.btnImportClick(Sender: TObject);
var
  ExcludeIndex,
  TableIndex: LongInt;
  ImportFromDatabase,
  SaveDatabaseItem: TffeDatabaseItem;
begin
  ExcludeIndex := -1;
  if btnRestructure.Visible then ExcludeIndex := FTableIndex;
  if ShowImportTableDefDlg(FDatabase, ExcludeIndex, ImportFromDatabase, TableIndex) = mrOK then begin
    tabStructure.ActivePage := tbsFields;  {reset to fields display}

    SaveDatabaseItem := FDatabase;
    FDatabase := ImportFromDatabase;
    try
      with grdFields do
        if EditorMode then begin
          EditorMode := False;
          LoadDictionary(TableIndex);
          EditorMode := True;
        end else
          LoadDictionary(TableIndex);
        {Begin !!.11}
        { if no index in imported table, add an empty entry
          so we have an empty line to start editing in }
        if FIndexList.Count=0 then
          FIndexList.AddEmpty;
        {End !!.11}
    finally
      FDatabase := SaveDatabaseItem;
    end;
  end;
end;
{--------}
procedure TfrmTableStruct.btnRestructureClick(Sender: TObject);
begin
  {Begin !!.07}
  { force typefield validation and saving }
  if grdFields.Col=cnFldType then begin
    grdFields.Perform(WM_KEYDOWN, VK_TAB, 0);
  end;
  {End !!.07}
  if ValidateForm then
    if ValidateRestructure then begin
      BuildDictionary;
      with tabStructure do
        if not Pages[PageCount - 1].Enabled or
           not chkPreserveData.Checked or
           (FFieldMap.Count = 0) then
          FDatabase.Tables[FTableIndex].Restructure(FOutputDictionary, nil)
        else
          FDatabase.Tables[FTableIndex].Restructure(FOutputDictionary, FFieldMap);
      FOutputDictionary.Free;
      FOutputDictionary := nil;
      ModalResult := mrOK;
    end;
end;


{=====Fields tab events=====}
procedure TfrmTableStruct.btnInsertFieldClick(Sender: TObject);
begin
  FHasChanged := True;
  with grdFields do begin
    try
      EditorMode := False;
      FFieldList.InsertEmpty(Row - 1);
      Col := cnFldName;
      InvalidateFieldsTable;
    finally
      EditorMode := True;
    end;
    EnableFieldControls(Row);
  end;
end;
{--------}
procedure TfrmTableStruct.btnDeleteFieldClick(Sender: TObject);
var
  I: Integer;
begin
  FHasChanged := True;
  with grdFields do begin
    if (Row = RowCount - 1) and (FFieldList.Items[Row - 1].Name = '') then
      MessageBeep(0)
    else begin
      with grdFields do begin
        I := FIndexList.FieldInUse(FFieldList.Items[Row - 1].Name);
        if I <> -1 then
          raise Exception.CreateFmt('Field %s is in use by index %d (%s)',
                                     [FFieldList.Items[Row - 1].Name,
                                      I,
                                      FIndexList.Items[I].Name]);
      end;

      BeginUpdate;
      try
        EditorMode := False;
        FFieldList.DeleteAt(Row - 1);
        InvalidateFieldsTable;
      finally
        EndUpdate;
        EditorMode := True;
      end;
      EnableFieldControls(Row);
    end;
  end;
end;
{--------}
procedure TfrmTableStruct.btnMoveFieldUpClick(Sender: TObject);
begin
  FHasChanged := True;
  with grdFields do begin
    if Row > 1 then begin
      FFieldList.Exchange(Row - 1, Row - 2);
      InvalidateFieldsTable;
      Row := Row - 1;
    end;
  end;
end;
{--------}
procedure TfrmTableStruct.btnMoveFieldDownClick(Sender: TObject);
begin
  FHasChanged := True;
  with grdFields do begin
    if Row < pred(RowCount) then begin
      FFieldList.Exchange(Row, Row - 1);
      InvalidateFieldsTable;
      Row := Row + 1;
    end;
  end;
end;
{--------}
procedure TfrmTableStruct.radBLOBInternalClick(Sender: TObject);
begin
  EnableBLOBControls;
end;
{--------}
procedure TfrmTableStruct.cboFieldTypeChange(Sender: TObject);
begin
  with grdFields do begin
    Cells[Col, Row] := cboFieldType.Items[cboFieldType.ItemIndex];
  end;
  grdFields.Invalidate;
end;
{--------}
procedure TfrmTableStruct.cboFieldTypeExit(Sender: TObject);
begin
  cboFieldType.Visible := False;
  if Assigned(ActiveControl) and not(ActiveControl = grdFields) then
    ActiveControl.SetFocus
  else begin
    grdFields.SetFocus;
    grdFields.Perform(WM_KEYDOWN, VK_TAB, 0);
  end;
end;
{--------}
procedure TfrmTableStruct.grdFieldsEnter(Sender: TObject);
begin
  if FDialogMode <> dmViewing then
    EnableFieldControls(grdFields.Row);
end;
{--------}
procedure TfrmTableStruct.grdFieldsSelectCell(Sender    : TObject;
                                              Col, Row  : Integer;
                                          var CanSelect : Boolean);
var
  R         : TRect;
  ErrorCode : Word;
begin
  { Validate previously selected cell. If a validation error occurs, stop
    processing and display the error}
  CanSelect := (FDialogMode <> dmViewing);
  if (not CanSelect) then Exit;

  case grdFields.Col of
    cnFldName :
      CanSelect := FieldNameValidation(grdFields.Cells[cnFldName, grdFields.Row], ErrorCode);

    cnFldUnits :
      CanSelect := FieldLengthValidation(grdFields.Cells[cnFldUnits, grdFields.Row], ErrorCode);
  end;
  if not CanSelect then begin
    DisplayValidationError(ErrorCode);
    Exit;
  end;

  { Save data to FFieldList, and update the grid if necessary}
  LeavingFieldsCell(grdFields.Col, grdFields.Row);                  


  { Set any special cell attributes (ComboBoxes, Readonly fields)}
  grdFields.Options := grdFields.Options + [goAlwaysShowEditor, goEditing];
  case Col of
    cnFldRequired :
      grdFields.Options := grdFields.Options - [goAlwaysShowEditor, goEditing];

    cnFldType :
      begin
        R := grdFields.CellRect(Col, Row);
        ShowCellCombo(cboFieldType, grdFields, R);
        cboFieldType.ItemIndex :=
            cboFieldType.Items.IndexOf(grdFields.Cells[Col, Row]);
      end;

    cnFldUnits :
      if not FFEFieldTypeHasUnits(FFieldList.Items[Pred(Row)].FieldType) then
        grdFields.Options := grdFields.Options - [goAlwaysShowEditor, goEditing]
      else
        grdFields.Options := grdFields.Options + [goAlwaysShowEditor, goEditing];

    cnFldDecPl :
      if not FFEFieldTypeHasDecPl(FFieldList.Items[Pred(Row)].FieldType) then
         grdFields.Options := grdFields.Options - [goAlwaysShowEditor, goEditing]
      else
        grdFields.Options := grdFields.Options + [goAlwaysShowEditor, goEditing];

    cnFldDefault :
      if not AllowDefaultField(Row, ErrorCode) then
         grdFields.Options := grdFields.Options - [goAlwaysShowEditor, goEditing]
  end;

  EnableFieldControls(Row);
end;
{--------}
procedure TfrmTableStruct.grdFieldsDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  DrawType  : TffeDrawType;
  ErrorCode : Word;
begin
  { Leave fixed portion of the grid alone}
  if gdFixed in State then Exit;

  with grdFields do begin
    DrawType := dtNormal;
    if ((not (FDialogMode = dmViewing)) and (FFieldList.Count > ARow)) or       {!!.06}
       ((FDialogMode = dmViewing) and (FFieldList.Count >= ARow)) then          {!!.06}
      case ACol of
        cnFldUnits:
          if not FFEFieldTypeHasUnits(FFieldList.Items[Pred(ARow)].FieldType) then
            DrawType := dtGrayed;

        cnFldDecPl:
          if not FFEFieldTypeHasDecPl(FFieldList.Items[Pred(ARow)].FieldType) then
            DrawType := dtGrayed;

        cnFldRequired:
          if (FFieldList.Items[Pred(ARow)].fiDataTypeIndex = Ord(fftAutoInc)) then {!!.06}
            DrawType := dtGrayed                                                {!!.06}
          else begin                                                            {!!.06}
            if FFieldList.Items[Pred(ARow)].fiRequired then
              DrawType := dtChecked
            else
              DrawType := dtUnchecked;
          end;                                                                  {!!.06}

        cnFldDefault:
          if not AllowDefaultField(aRow, ErrorCode) then
            DrawType := dtGrayed;
      end;

    { Now that the DrawType is known, we can manipulate the canvas}
    DrawCell(Sender as TffStringGrid, DrawType, Rect, State, Cells[ACol, ARow]);
  end;
end;
{--------}
procedure TfrmTableStruct.grdFieldsKeyPress(Sender : TObject;
                                        var Key    : Char);
const
  valValidNumber = ['0'..'9'];
  valValidAlpha = ['a'..'z','A'..'Z'];
var
  Value  : string;
  Ignore : Boolean;
begin
  if Key = #13 then
    { Change the selected cell (Enter as tab)}
    with grdFields do
      if Col < Pred(ColCount) then
        Col := Col + 1
      else if Row < Pred(RowCount) then begin
        Row := Row + 1;
        Col := cnFldName;
      end else begin
        Row := 1;
        Col := cnFldName;
      end
  else begin
    { Validate data entry as key's are pressed}
    case grdFields.Col of
      cnFldName:
        begin
          Value := grdFields.Cells[cnFldName, grdFields.Row];
          Ignore := not(Key in [#8, #46]) and (Length(Value) >= 31); {!!.01}
        end;

      cnFldUnits:
        begin
          Value := grdFields.Cells[cnFldUnits, grdFields.Row];
          if Key in valValidAlpha then
            Ignore := True
          else
            Ignore := (Key in valValidNumber) and (Length(Value) >= 5);
        end;

      cnFldDecPl:
        begin
          Value := grdFields.Cells[cnFldDecPl, grdFields.Row];
          if Key in valValidAlpha then
            Ignore := True
          else
            Ignore := (Key in valValidNumber) and (Length(Value) >= 3)
         end;

      cnFldDefault:
        begin
          {Is the default value <= the units?}
          if (Key <> #8) then begin
            if ((FFEFieldTypeRequiresUnits(FFieldList.Items[pred(grdFields.Row)].FieldType)) or
                (StrToInt(grdFields.Cells[cnFldUnits ,grdFields.Row]) > 0)) then
              Ignore := Length(grdFields.Cells[cnFldDefault ,grdFields.Row]) >=
                      StrToInt(grdFields.Cells[cnFldUnits ,grdFields.Row])
            else
              Ignore := False;
            if (not Ignore) then
              Ignore := not ValidDefaultFieldKey(UpCase(Key),
                                                 FFieldList.Items[Pred(grdFields.Row)].FieldType);
          end else
            Ignore := False;
        end;

      cnFldDesc:
        Ignore := not(Key in [#8, #46]) and (Length(Value) >= 63);       {!!.01}

      cnFldRequired :
        begin
          Ignore := (not (Key in [#9, #32]));
          if (Key = ' ') and (not (FDialogMode = dmViewing)) then
            with FFieldList.Items[Pred(grdFields.Row)] do
              fiRequired := not fiRequired;
          grdFields.Invalidate;
        end;

    else
      Ignore := False;
    end;
    if Ignore then begin
      Key := #0;
      MessageBeep(0);
    end;
  end;
end;
{--------}
procedure TfrmTableStruct.grdFieldsMouseUp(Sender : TObject;
                                           Button : TMouseButton;
                                           Shift  : TShiftState;
                                           X, Y   : Integer);
var
  ACol, ARow: Longint;
  Rect, Dest : TRect;
begin
  { Manipulate checkbox state in Fields grid}
  if Button <> mbLeft then Exit;
    grdFields.MouseToCell(X,Y, ACol, ARow);
    if ACol = cnFldRequired then
    begin
      Rect := grdFields.CellRect(ACol, ARow);
      with imgPlus.Picture do
        { Retrieve the rect from around the box itself}
        Dest := Bounds(Rect.Left + ((Rect.Right - Bitmap.Width  - Rect.Left) div 2),
                       Rect.Top + (grdFields.DefaultRowHeight - Bitmap.Height) div 2,
                       Bitmap.Width,
                       Bitmap.Height);

      { Only manipuate the checkbox state if an area on or within the rect was
        clicked}
      if (X >= Dest.Left) and (X <= Dest.Right) and
         (Y >= Dest.Top) and (Y <= Dest.Bottom) and
         (not (FDialogMode = dmViewing)) then begin                             {!!.06}
        with FFieldList.Items[Pred(ARow)] do
          fiRequired := not fiRequired;
        grdFields.Invalidate;
      end;
    end;
end;


{=====Indexes tab events=====}
procedure TfrmTableStruct.btnDeleteIndexClick(Sender: TObject);
begin
  FHasChanged := True;
  if (grdIndexes.Row = grdIndexes.RowCount - 1) and
     (FIndexList.Items[grdIndexes.Row - 1].Name = '') then
    MessageBeep(0)
  else begin
    grdIndexes.BeginUpdate;
    try
      grdIndexes.EditorMode := False;
      FIndexList.DeleteAt(grdIndexes.Row - 1);
      grdIndexes.RowCount := grdIndexes.RowCount - 1;
      InvalidateIndexesTable;
    finally
      grdIndexes.EndUpdate;
      grdIndexes.EditorMode := True;
    end;
    EnableIndexControls(grdIndexes.Row, '');
  end;
end;
{--------}
procedure TfrmTableStruct.AddFieldToIndex;
var
  Idx       : Integer;
  ItemIdx   : Integer;
  KeyLength : Integer;
begin
  FHasChanged := True;
  with lstAvailFields do
    if SelCount = -1 then begin
      if ItemIndex <> -1 then begin
        lstIndexFields.Items.Add(Items[ItemIndex]);
        with grdIndexes do begin
          BeginUpdate;
          try
            with FIndexList.Items[Row - 1] do begin
              AddField(Items[ItemIndex]);
              KeyLength := CalcKeyLength(Row - 1);
              if KeyLength > ffcl_MaxKeyLength then begin
                DeleteField(Items[ItemIndex]);
                raise Exception.CreateFmt('Key length cannot exceed %d', [ffcl_MaxKeyLength]);
              end;
              iiKeyLen := KeyLength;
            end;
          finally
            EndUpdate;
          end;
        end;
        ItemIdx := ItemIndex;
        Items.Delete(ItemIndex);
        if ItemIdx < Items.Count then
          ItemIndex := ItemIdx
        else if Items.Count > 0 then
          ItemIndex := Items.Count - 1;
      end;
    end else
      { The multiselect option is selected for the list}
      for Idx := 0 to Pred(Items.Count) do
        if Selected[Idx] then begin
          lstIndexFields.Items.Add(Items[Idx]);
          with grdIndexes do begin
            BeginUpdate;
            try
              with FIndexList.Items[Row - 1] do begin
                AddField(Items[Idx]);
                KeyLength := CalcKeyLength(Row - 1);
                if KeyLength > ffcl_MaxKeyLength then begin
                  DeleteField(Items[Idx]);
                  raise Exception.CreateFmt('Key length cannot exceed %d', [ffcl_MaxKeyLength]);
                end;
                iiKeyLen := KeyLength;
              end;
            finally
              EndUpdate;
            end;
          end;
          ItemIdx := Idx;
          Items.Delete(Idx);
          if ItemIdx < Items.Count then
            ItemIndex := ItemIdx
          else if Items.Count > 0 then
            ItemIndex := Pred(Items.Count);
        end;
end;
{--------}
procedure TfrmTableStruct.RemoveFieldFromIndex;
var
  ItemIdx: Integer;
begin
  FHasChanged := True;
  with lstIndexFields do
    if ItemIndex <> -1 then begin
      lstAvailFields.Items.Add(Items[ItemIndex]);
      with grdIndexes do begin
        BeginUpdate;
        try
          with FIndexList.Items[Row - 1] do begin
            DeleteField(Items[ItemIndex]);
            iiKeyLen := CalcKeyLength(Row - 1);
          end;
        finally
          EndUpdate;
        end;
      end;
      ItemIdx := ItemIndex;
      Items.Delete(ItemIndex);
      if ItemIdx < Items.Count then
        ItemIndex := ItemIdx
      else if Items.Count > 0 then
        ItemIndex := Items.Count - 1;
    end;
end;
{--------}
procedure TfrmTableStruct.btnAddIndexFieldClick(Sender: TObject);
begin
  AddFieldToIndex;
end;
{--------}
procedure TfrmTableStruct.btnRemoveIndexFieldClick(Sender: TObject);
begin
  RemoveFieldFromIndex;
end;
{--------}
procedure TfrmTableStruct.btnMoveIndexFieldUpClick(Sender: TObject);
var
  NewItemIndex: Integer;
begin
  FHasChanged := True;
  with lstIndexFields do
    if ItemIndex > 0 then begin
      with FIndexList.Items[grdIndexes.Row - 1] do
        ExchangeFields(Items[ItemIndex], Items[ItemIndex - 1]);
      NewItemIndex := ItemIndex - 1;
      Items.Exchange(ItemIndex, ItemIndex - 1);
      ItemIndex := NewItemIndex;
    end;
end;
{--------}
procedure TfrmTableStruct.btnMoveIndexFieldDownClick(Sender: TObject);
var
  NewItemIndex: Integer;
begin
  FHasChanged := True;
  with lstIndexFields do
    if (ItemIndex <> -1) and (ItemIndex < Items.Count - 1) then begin
      with FIndexList.Items[grdIndexes.Row - 1] do
        ExchangeFields(Items[ItemIndex], Items[ItemIndex + 1]);
      NewItemIndex := ItemIndex + 1;
      Items.Exchange(ItemIndex, ItemIndex + 1);
      ItemIndex := NewItemIndex;
    end;
end;
{--------}
procedure TfrmTableStruct.lstIndexFieldsDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if Source is TComponent then
    Accept := (TComponent(Source).Name = 'lstAvailFields');
end;
{--------}
procedure TfrmTableStruct.lstIndexFieldsDragDrop(Sender, Source: TObject;
  X, Y: Integer);
begin
  if FDialogMode <> dmViewing then
    btnAddIndexFieldClick(Source);
end;
{--------}
procedure TfrmTableStruct.lstAvailFieldsDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if Source is TComponent then
    Accept := (TComponent(Source).Name = 'lstIndexFields');
end;
{--------}
procedure TfrmTableStruct.lstAvailFieldsDragDrop(Sender, Source: TObject;
  X, Y: Integer);
begin
  if FDialogMode <> dmViewing then
    btnRemoveIndexFieldClick(Source);
end;
{--------}
procedure TfrmTableStruct.cboIndexTypeChange(Sender: TObject);
begin
  with grdIndexes, TComboBox(Sender) do                                  {!!.01}
    Cells[Col, Row] := Items[ItemIndex];                                 {!!.01}

  grdIndexes.Invalidate;
end;
{--------}
procedure TfrmTableStruct.cboIndexTypeExit(Sender: TObject);
begin
  TComboBox(Sender).Visible := False;
  if Assigned(ActiveControl) and not(ActiveControl = grdIndexes) then
    ActiveControl.SetFocus
  else begin
    grdIndexes.SetFocus;
    grdIndexes.Perform(WM_KEYDOWN, VK_TAB, 0);
  end;
end;
{--------}
procedure TfrmTableStruct.grdIndexesEnter(Sender: TObject);
begin
  if FDialogMode <> dmViewing then
    EnableIndexControls(grdIndexes.Row, '');
end;
{--------}
procedure TfrmTableStruct.grdIndexesSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  Rect: TRect;
  ErrorCode : Word;
begin
  { Validate previously selected cell. If a validation error occurs, stop
    processing and display the error}
  if FDialogMode = dmViewing then begin
    CanSelect := grdIndexes.Row <> aRow;
    if CanSelect then
      PopulateIndexFieldsLists(aRow - 1);
    Exit;
  end;
  case grdIndexes.Col of
    cnIdxName:
      CanSelect := IndexNameValidation(grdIndexes.Cells[cnIdxName, grdIndexes.Row], ErrorCode);
    cnIdxExt:
      CanSelect := IndexExtensionValidation(grdIndexes.Cells[cnIdxExt, grdIndexes.Row], ErrorCode);
    cnIdxKeyLength:
      CanSelect := IndexKeyLenValidation(StrToInt('0' + grdIndexes.Cells[cnIdxKeyLength, grdIndexes.Row]), ErrorCode);
  end;
  if not CanSelect then begin
    DisplayValidationError(ErrorCode);
    Exit;
  end;

  { Save data to FFieldList, and update the grid if necessary}
  LeavingIndexCell(grdIndexes.Col, grdIndexes.Row);
  PopulateIndexFieldsLists(Pred(aRow));

 {Set any special cell attributes}
  grdIndexes.Options := grdIndexes.Options + [goAlwaysShowEditor, goEditing];
  case ACol of
    cnIdxKeyLength:
      if FIndexList.Items[Pred(ARow)].iiKeyTypeIndex <> ktUserDefined then
        grdIndexes.Options := grdIndexes.Options - [goAlwaysShowEditor, goEditing];

    cnIdxUnique, cnIdxAscending, cnIdxCaseSensitive:
      grdIndexes.Options := grdIndexes.Options - [goAlwaysShowEditor, goEditing];

    cnIdxType:
      begin
        Rect := grdIndexes.CellRect(ACol, ARow);
        ShowCellCombo(cboIndexType, grdIndexes, Rect);
        cboIndexType.ItemIndex :=
          FIndexList.Items[Pred(ARow)].iiKeyTypeIndex;
      end;

    cnIdxBlockSize:
      begin
        if FIndexList.Items[Pred(ARow)].iiExtension = '' then
          grdIndexes.Options := grdIndexes.Options - [goAlwaysShowEditor, goEditing]
        else begin
          Rect := grdIndexes.CellRect(ACol, ARow);
          ShowCellCombo(cboIndexBlockSize, grdIndexes, Rect);
          cboIndexBlockSize.ItemIndex :=
            FIndexList.Items[Pred(ARow)].iiBlockSizeIndex;
        end;
      end;
  end;
end;
{--------}
procedure TfrmTableStruct.grdIndexesKeyPress(Sender: TObject;
  var Key: Char);
const
  valValidNumber = ['0'..'9'];
  valValidAlpha = ['a'..'z','A'..'Z'];
var
  Ignore: Boolean;
begin
  with grdIndexes do
    if Key = #13 then
      if Col < ColCount-1 then {next column!}
        Col := Col + 1
      else if Row < RowCount-1 then begin {next Row!}
        Row := Row + 1;
        Col := 1;
      end else begin {End of Grid! - Go to Top again!}
        Row := 1;
        Col := 1;
        {or you can make it move to another Control}
      end
    else begin
      case Col of
        cnIdxName:
          begin
            Ignore := not(Key in [#8, #46]) and (Length(Cells[Col, Row]) >= 31); {!!.01}
            EnableIndexControls(Row, Cells[Col, Row] + Key);
          end;

        cnIdxKeyLength:
          If Key in valValidAlpha then
            Ignore := True
          else
            Ignore := (Key in valValidNumber) and (Length(Cells[Col, Row]) >= 3);

        cnIdxExt:
          Ignore := not(Key in [#8, #46]) and (Length(Cells[Col, Row]) >= 3); {!!.01}

        cnIdxDesc:
          Ignore := not(Key in [#8, #46]) and (Length(Cells[Col, Row]) >= 63) {!!.01}
      else
        Ignore := False;
      end;
      if Ignore then begin
        Key := #0;
        MessageBeep(0);
      end;
    end;
end;
{--------}
procedure TfrmTableStruct.grdIndexesDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  DrawType: TffeDrawType;
begin
  if gdFixed in State then Exit;

  with grdIndexes do begin
    DrawType := dtNormal;
    if (ARow = 0) then
      DrawType := dtIgnore
    else
      case ACol of
        cnIdxKeyLength:
          if FIndexList.Items[Pred(ARow)].iiKeyTypeIndex <> ktUserDefined then
            DrawType := dtGrayed;

        cnIdxBlockSize:
          if FIndexList.Items[Pred(ARow)].iiExtension = '' then
            DrawType := dtGrayed;

        cnIdxUnique:
          if FIndexList.Items[Pred(ARow)].iiUnique then
            DrawType := dtChecked
          else
            DrawType := dtUnchecked;

        cnIdxAscending:
          if FIndexList.Items[Pred(ARow)].iiAscending then
            DrawType := dtChecked
          else
            DrawType := dtUnchecked;

        cnIdxCaseSensitive:
          if FIndexList.Items[Pred(ARow)].iiCaseSensitive then
            DrawType := dtChecked
          else
            DrawType := dtUnchecked;
      else
        DrawType := dtIgnore;
      end;

    DrawCell(Sender as TffStringGrid, DrawType, Rect, State, Cells[ACol, ARow]);
  end;
end;
{--------}
procedure TfrmTableStruct.grdIndexesMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow: Longint;
  Rect, Dest : TRect;
begin
  if Button <> mbLeft then Exit;
    grdIndexes.MouseToCell(X,Y, ACol, ARow);
    if (ARow > 0) and
       (ACol in [cnIdxUnique, cnIdxAscending, cnIdxCaseSensitive]) then
    begin
      Rect := grdIndexes.CellRect(ACol, ARow);
      with imgPlus.Picture do
        Dest := Bounds(Rect.Left + ((Rect.Right - Bitmap.Width  - Rect.Left) div 2),
                       Rect.Top + (grdIndexes.DefaultRowHeight - Bitmap.Height) div 2,
                       Bitmap.Width,
                       Bitmap.Height);
      if (X >= Dest.Left) and (X <= Dest.Right) and
         (Y >= Dest.Top) and (Y <= Dest.Bottom) and
         (not (FDialogMode = dmViewing)) then begin                             {!!.06}
        with FIndexList.Items[Pred(ARow)] do
          case ACol of
            cnIdxUnique:
              iiUnique := not iiUnique;
            cnIdxAscending:
              iiAscending := not iiAscending;
            cnIdxCaseSensitive:
              iiCaseSensitive := not iiCaseSensitive;
          end;
        grdIndexes.Invalidate;
      end;
    end;
end;


{=====Existing data tab events=====}
procedure TfrmTableStruct.tabFieldMapPageChanged(Sender: TObject;
  Index: Integer);
var
  I, J, N: Integer;
  Found: Boolean;
begin
  case Index of
    0: begin
         btnMatchByName.Enabled := True;
         btnMatchByPosition.Enabled := True;
         btnClearAll.Enabled := True;
       end;
    1: begin
         btnMatchByName.Enabled := False;
         btnMatchByPosition.Enabled := False;
         btnClearAll.Enabled := False;

         { Build the orphaned fields list }
         with FDatabase.Tables[FTableIndex].Dictionary do begin
           N := 0;
           for I := 0 to FieldCount - 1 do begin
             Found := False;
             for J := 0 to FFieldMap.Count - 1 do
               if Pos('=' + FieldName[I] + #255, FFieldMap[J] + #255) <> 0 then begin
                 Found := True;
                 Break;
               end;

             if not Found then
               with grdOrphanedFields do begin
                 Cells[0, N + FixedRows] := FieldName[I];
                 if FieldType[I] >= fftByteArray then
                   Cells[1, N + FixedRows] := Format('%s[%d]', [FieldDataTypes[FieldType[I]], FieldUnits[I]])
                 else
                   Cells[1, N + FixedRows] := FieldDataTypes[FieldType[I]];
                 Inc(N);
               end;
           end;

           with grdOrphanedFields do begin
             RowCount := N + FixedRows + 1;
             Cells[0, RowCount - 1] := '';
             Cells[1, RowCount - 1] := '';
           end;
         end;
       end;
  end;
end;
{--------}
procedure TfrmTableStruct.btnMatchByNameClick(Sender: TObject);
var
  I: Integer;
  NewFieldName: TffDictItemName;
  OldFieldIndex: Integer;
begin
  with grdFieldMap do begin
    BeginUpdate;
    ReverseFFieldMap := TStringList.Create;                           {!!.11}
    try
      try
        FFieldMap.Clear;
        for I := 0 to FFieldList.Count - 1 do begin
          NewFieldName := FFieldList.Items[I].Name;
          with FDatabase.Tables[FTableIndex].Dictionary do begin
            OldFieldIndex := GetFieldFromName(NewFieldName);
            if OldFieldIndex <> -1 then

              { Check assignment compatibility }
              if FFConvertSingleField(
                   nil,
                   nil,
                   FieldType[OldFieldIndex],
                   FFEIndexToFieldType(FFieldList.Items[I].fiDatatypeIndex),
                   -1,
                   -1) = DBIERR_NONE then begin
                FFieldMap.Values[NewFieldName] := NewFieldName;
                ReverseFFieldMap.Values[NewFieldName] := NewFieldName;    {!!.11}
              end;
          end;
        end;
      finally
        InvalidateFieldMapTable;
        EndUpdate;
      end;
    {Begin !!.11}
    finally
      ReverseFFieldMap.Free;
      ReverseFFieldMap := nil;
    end;
    {End !!.11}
  end;
end;
{--------}
procedure TfrmTableStruct.btnMatchByPositionClick(Sender: TObject);
var
  I: Integer;
  NewFieldName: TffDictItemName;
begin
  with grdFieldMap do begin
    BeginUpdate;
    ReverseFFieldMap := TStringList.Create;                           {!!.11}
    try
      try
        FFieldMap.Clear;
        for I := 0 to FFieldList.Count - 1 do begin
          NewFieldName := FFieldList.Items[I].Name;
          with FDatabase.Tables[FTableIndex].Dictionary do
            if I < FieldCount then

              { Check assignment compatibility }
              if FFConvertSingleField(
                   nil,
                   nil,
                   FieldType[I],
                   FFEIndexToFieldType(FFieldList.Items[I].fiDatatypeIndex),
                   -1,
                   -1) = DBIERR_NONE then begin
              FFieldMap.Values[NewFieldName] := FieldName[I];
              ReverseFFieldMap.Values[FieldName[I]] := NewFieldName;
            end;
        end;
      finally
        InvalidateFieldMapTable;
        EndUpdate;
      end;
    {Begin !!.11}
    finally
      ReverseFFieldMap.Free;
      ReverseFFieldMap := nil;
    end;
    {End !!.11}
  end;
end;
{--------}
procedure TfrmTableStruct.btnClearAllClick(Sender: TObject);
begin
  FFieldMap.Clear;
  InvalidateFieldMapTable;
end;
{--------}
procedure TfrmTableStruct.chkPreserveDataClick(Sender: TObject);
begin
  FFEEnableContainer(grpExistingData, chkPreserveData.Checked);
end;
{--------}
procedure TfrmTableStruct.grdFieldMapEnter(Sender: TObject);
var
  Dummy: Boolean;
begin
  { rewritten }
  {Begin !!.11}
  if not FcboMapOldFieldHasBeenFocused and
     not FFieldMapInShiftTab then begin
    grdFieldMap.Col := 2;
    grdFieldMap.OnSelectCell(Self, grdFieldMap.Col, grdFieldMap.Row, Dummy);
  end
  else
  if FFieldMapInShiftTab then begin
    SelectNext(grdFieldMap, False, True);
  end;
  FcboMapOldFieldHasBeenFocused := False;
  FFieldMapInShiftTab := False;
  {End !!.11}
end;
{--------}
procedure TfrmTableStruct.grdFieldMapActiveCellMoving(Sender: TObject;
  Command: Word; var RowNum: Longint; var ColNum: Integer);
begin
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
end;
{--------}
procedure TfrmTableStruct.tcMapOldFieldChange(Sender: TObject);
var
  TCB: TComboBox;
  I: Integer;
  TempStr: TffShStr;
begin
  TCB := TComboBox(Sender as TCustomComboBox);
  I := TCB.ItemIndex;

  if I < 0 then TempStr := ''
  else TempStr := Copy(TCB.Items[I], 1, Pos(' (', TCB.Items[I]) - 1);

  FFieldMap.Values[FFieldList.Items[grdFieldMap.Row - 1].Name] := TempStr;
end;


{=====General routines=====}
{--------}
procedure TfrmTableStruct.AlignButtons;
{ Find all the visible buttons on the main panel and center them }
var
  I: Integer;
  Buttons: TffList;
  NewLeft: Integer;
  Offset: Integer;
  CurrentIndex: Integer;
  FirstIndex: Integer;
  BaseWidth: Integer;
begin
  Buttons := TffList.Create;
  try
    with pnlDialogButtons do begin
      for I := 0 to ControlCount - 1 do
        if Controls[I] is TBitBtn then
          if Controls[I].Visible then

            { We store the control's horizontal position in the 1st word,
              then the control index in the 2nd word. }
            Buttons.Insert(TffIntListItem.Create(Controls[I].Left * ($FFFF + 1) + I));

      FirstIndex := TffIntListItem(Buttons[0]).KeyAsInt and $FFFF;
      BaseWidth := Controls[FirstIndex].Width;
      NewLeft := 0;
      for I := 0 to Buttons.Count - 1 do begin
        CurrentIndex := TffIntListItem(Buttons[I]).KeyAsInt and $FFFF;
        with Controls[CurrentIndex] do begin
          Left := NewLeft;
          Width := BaseWidth;
          Inc(NewLeft, Width + 8);
        end;
      end;
      Dec(NewLeft, 8);

      Offset := (pnlMain.Width - NewLeft) div 2;
      for I := 0 to Buttons.Count - 1 do
        with Controls[TffIntListItem(Buttons[I]).KeyAsInt and $FFFF] do
          Left := Left + Offset;
    end;
  finally
    Buttons.Free;
  end;
end;
{--------}
procedure TfrmTableStruct.PopulateForm(aTableIndex: LongInt; aReadOnly: Boolean);
begin
  LoadDictionary(aTableIndex);
  if not aReadOnly then begin
    FFieldList.AddEmpty;
    InvalidateFieldsTable;
    FIndexList.AddEmpty;
    InvalidateIndexesTable;
    EnableIndexControls(1, '');
  end;
end;
{--------}
procedure TfrmTableStruct.DrawCell(Grid : TffStringGrid; DrawType: TffeDrawType;
                                   Rect: TRect; State: TGridDrawState; CellText: string);
var
  Bitmap: TBitmap;
  Dest, Source: TRect;
  X,Y: Integer;
  WrapText, WrapTemp: string;
  WrapPos: integer;
begin
  case DrawType of
    dtIgnore: Exit;
    dtNormal, dtGrayed:
      with Grid do begin
        if DrawType = dtNormal then
          Canvas.Brush.Color := clWindow
        else
          Canvas.Brush.Color := clBtnFace;
        Canvas.FillRect(Rect);
        Canvas.TextRect(Rect, Rect.Left + cnLeftMargin, Rect.Top + cnTopMargin,
                        CellText);
      end;

    dtChecked, dtUnChecked:
      begin
        if DrawType = dtChecked then
          Bitmap := imgPlus.Picture.Bitmap
        else
          Bitmap := imgMinus.Picture.Bitmap;
        with Grid.Canvas do begin
          Dest := Bounds(Rect.Left + ((Rect.Right - Bitmap.Width  - Rect.Left) div 2),
                         Rect.Top + (grdIndexes.DefaultRowHeight - Bitmap.Height) div 2,
                         Bitmap.Width,
                         Bitmap.Height);
          Source := Bounds(0, 0, Bitmap.Width, Bitmap.Height);
          BrushCopy(Dest,
                    Bitmap,
                    Source,
                    Bitmap.TransparentColor);
        end;
      end;
    dtWordWrap:
      begin
        with Grid.Canvas do begin
          if gdFixed in State then begin
            Pen.Color   := clBtnText;
            Brush.Color := clBtnFace;
          end else begin
            Pen.Color   := clWindowText;
            Brush.Color := clWindow;
          end;
          Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);

          Y := Rect.Top;

          WrapText := CellText;
          repeat
            WrapPos := Pos(#13, WrapText);
            if WrapPos <= 0 then
              WrapTemp := WrapText
            else
              WrapTemp := Copy(WrapText,1,Pred(WrapPos));
            Delete(WrapText, 1, WrapPos);
            X := Rect.Left + ((Rect.Right - TextWidth(WrapTemp)  - Rect.Left) div 2);
            TextOut(X, Y, WrapTemp);
            Y := Y + TextHeight(WrapTemp);
          until WrapPos <= 0;
        end;
      end;
  end;
end;
{--------}
procedure TfrmTableStruct.ShowCellCombo(ComboBox: TCustomComboBox;
  Grid: TCustomGrid; Rect: TRect);
begin
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
end;
{--------}
procedure TfrmTableStruct.CMDialogKey(var msg: TCMDialogKey);
begin
  if (ActiveControl = cboFieldType) or
   (ActiveControl = cboIndexType) or
   (ActiveControl = cboIndexBlockSize) then
  begin
    if (msg.CharCode = VK_TAB) then
    begin
      ActiveControl.Visible := False;
(*    if ActiveControl = cboFieldType then
        grdFields.SetFocus
      else
        grdIndexes.SetFocus;*)
      msg.result := 1;
      Exit;
    end;
  end else begin
  end;
  if (ActiveControl = cboMapOldField) and
     (msg.CharCode = VK_TAB) and
     (GetKeyState(VK_SHIFT)<0) then begin
    FFieldMapInShiftTab := True;
  end;
  inherited;
end;


{=====Dictionary routines=====}
procedure TfrmTableStruct.BuildDictionary;
var
  I, J: Integer;
  FileNumber: Integer;
  FieldArray: TffFieldList;
  FieldIHList : TffFieldIHList;
  ExtFound: Boolean;
begin
  FOutputDictionary.Free;
  FOutputDictionary := nil;

  FOutputDictionary := TffDataDictionary.Create(StrToInt(cboBlockSize.Text));
  try
    with FOutputDictionary do begin
      IsEncrypted := chkEncryptData.Checked;

      { Add the fields; the field list is assumed to be valid at this point }
      for I := 0 to FFieldList.Count - 1 do
        with FFieldList.Items[I] do
          if Name <> '' then
            AddField(Name,
                     fiDescription,
                     FFEIndexToFieldType(fiDataTypeIndex),
                     fiUnits,
                     fiDecPlaces,
                     fiRequired,
                     PffVCheckDescriptor(@fiValCheck));

      { Check for external BLOB file }
      if radBLOBExternal.Checked then
        AddFile(edtBLOBFileDesc.Text, edtBLOBExtension.Text,
                StrToInt(cboBLOBBlockSize.Text), ftBlobFile);

      { Add the Indexes }
      for I := 0 to FIndexList.Count - 1 do
        with FIndexList.Items[I] do
          if Name <> '' then begin

            { Determine if this index is to be stored in an external file }
            FileNumber := 0;
            ExtFound := False;
            iiExtension := ANSIUppercase(iiExtension);
            if iiExtension <> '' then begin
              { note that file descriptions are not supported yet }
              for J := 0 to FileCount - 1 do
                if FFCmpShStrUC(iiExtension, FileExt[J], 255) = 0 then begin
                  ExtFound := True;
                  Break;
                end;
              if not ExtFound then
                FileNumber := AddFile('', iiExtension, BlockSize, ftIndexFile);
            end;

            if iiKeyTypeIndex = ktComposite then begin

              { Construct the list of fields that comprise this index }
              for J := 0 to FieldCount - 1 do begin
                FieldArray[J] := GetFieldFromName(FieldName[J]);
                if FieldArray[J] = -1 then
                  raise Exception.CreateFmt('Index %d (%s) refers to nonexistent field %s', [I + 1, Name, FieldName[J]]);
                FieldIHList[J] := '';
              end;

              AddIndex(Name, iiDescription, FileNumber,
                       FieldCount, FieldArray, FieldIHList, not iiUnique,
                       iiAscending, not iiCaseSensitive);
            end
            else begin
              AddUserIndex(Name, iiDescription, FileNumber,
                           iiKeyLen, not iiUnique, iiAscending, not iiCaseSensitive);
            end;
          end;
      FileDescriptor[0].fdDesc := edtDescription.Text;                          {!!.10}
      CheckValid;
    end;
  except
    FOutputDictionary.Free;
    FOutputDictionary := nil;
    raise;
  end;
end;
{--------}
procedure TfrmTableStruct.LoadDictionary(aTableIndex: LongInt);
var
  IndexFields : TStringList;
  I           : Integer;
begin
  with FDatabase.Tables[aTableIndex] do begin

    { Reload always in case of restructure by another user }
    with Dictionary do begin
      cboBlockSize.Text := IntToStr(BlockSize);
      cboBlockSize.ItemIndex := FFEBlockSizeIndex(BlockSize);

      edtDescription.Text := FileDesc[0];     {!!.10}

      { Load the fields }
      grdFields.BeginUpdate;
      try
        FFieldList.Empty;
        for I := 0 to FieldCount - 1 do begin
          FFieldList.Insert(FieldName[I],
                            FFEFieldTypeToIndex(FieldType[I]),
                            FieldUnits[I],
                            FieldDecPl[I],
                            FieldRequired[I],
                            FieldDesc[I],
                            FieldVCheck[I]);
        end;
        grdFields.RowCount := grdFields.FixedRows + FieldCount;
      finally
        InvalidateFieldsTable;
        grdFields.EndUpdate;
      end;

      { Check for BLOB storage }
      edtBLOBExtension.Text := '';
      cboBLOBBlockSize.Text := '';
      edtBLOBFileDesc.Text := '';
      radBLOBInternal.Checked := (BLOBFileNumber = 0);
      radBLOBExternal.Checked := not radBLOBInternal.Checked;
      EnableBLOBControls;
      if BLOBFileNumber <> 0 then begin
        edtBLOBExtension.Text := FileExt[BLOBFileNumber];
        cboBLOBBlockSize.Text := IntToStr(FileBlockSize[BLOBFileNumber]);
        edtBLOBFileDesc.Text := FileDesc[BLOBFileNumber];
      end;

      { Load the indexes }
      IndexFields := TStringList.Create;
      try
        try
          FIndexList.LoadFromDict(Dictionary);
          if FDialogMode in [dmCreating, dmRestructuring] then
            FIndexList.DeleteAt(0);
          grdIndexes.RowCount := grdIndexes.FixedRows + IndexCount;
        finally
          InvalidateIndexesTable;
        end;
      finally
        IndexFields.Free;
      end;

      { Encrypted? }
      chkEncryptData.Checked := IsEncrypted;
    end;
  end;
end;
{--------}
procedure TfrmTableStruct.CreateTable(aTableName: TffTableName);
begin
  with FDatabase do
    CreateTable(aTableName, FOutputDictionary);

  { Make a new entry for the TableList }
  FTableIndex := FDatabase.AddTable(aTableName);
end;
{--------}
procedure TfrmTableStruct.PrintDictionary(aTableIndex: LongInt; aPrintToFile: Boolean);
var
  F: System.Text;
  I, J: Integer;
  FldName: TffDictItemName;

  procedure BoldOn;
  begin
    if not aPrintToFile then
      with Printer.Canvas.Font do
        Style := Style + [fsBold];
  end;

  procedure BoldOff;
  begin
    if not aPrintToFile then
      with Printer.Canvas.Font do
        Style := Style - [fsBold];
  end;

  function CaseFlag(aNoCase: Boolean): Char;
  begin
    if aNoCase then Result := 'I'
    else Result := 'S';
  end;

begin
  with FDatabase.Tables[aTableIndex], Dictionary do begin
    if aPrintToFile then begin

      { Get filename to save to }
      with dlgSave do begin
        if not Execute then Exit;
        ShowPrintingDlg('Saving structure for ' + TableName);
        AssignFile(F, FileName);
      end;
    end
    else begin
      ShowPrintingDlg('Printing structure for ' + TableName);
      AssignPrn(F);
    end;

    try
      Rewrite(F);
      try
        if not aPrintToFile then
          with Printer.Canvas.Font do begin
            Name := 'Courier New';
            Size := 10;
          end;

        WriteLn(F, 'Table definition for:');                          {!!.06}
        WriteLn(F, Format('  Table:  %s', [TableName]));              {!!.06}
        WriteLn(F, Format('  Alias:  %s', [Database.DatabaseName]));  {!!.06}
        WriteLn(F, Format('  Server: %s', [Server.ServerName]));      {!!.06}
        WriteLn(F);
        WriteLn(F, Format('Block Size: %d', [BlockSize]));
        WriteLn(F, Format('Logical Record Length:  %d', [LogicalRecordLength]));
        WriteLn(F, Format('Physical Record Length: %d', [RecordLength]));
        if IsEncrypted then
          WriteLn(F, 'Encrypted Table Data:   YES')                   {!!.06}
        else
          WriteLn(F, 'Encrypted Table Data:   NO');                   {!!.06}

        WriteLn(F);
        BoldOn;
        WriteLn(F, 'Fields:');
        WriteLn(F);
        WriteLn(F, 'Num  Name                Type              Offset Size Units Dec Req Description');
        BoldOff;
        for I := 0 to FieldCount - 1 do
          WriteLn(F, Format('%3d  %-20.20s%-17.17s %6d %4d %5d %3d %2.1s  %s',
                            [I + 1, FieldName[I], FieldDataTypes[FieldType[I]],
                             FieldOffset[I], FieldLength[I], FieldUnits[I],
                             FieldDecPl[I], FFEBoolToStr(FieldRequired[I]), FieldDesc[I]]));

        WriteLn(F);
        BoldOn;
        WriteLn(F, 'Indexes:');
        WriteLn(F);
        WriteLn(F, 'Num  Name                Field(s)            File Type Len Uni Asc Case Description');
        BoldOff;
        for I := 0 to IndexCount - 1 do begin
          with IndexDescriptor[I]^ do begin
            FldName := '(n/a)';
            if idCount > 0 then
              FldName := FieldName[idFields[0]];
            WriteLn(F, Format('%3d  %-20.20s%-17.17s    %3s %4.4s %3d %2.1s  %2.1s  %3.1s  %s',
                              [idNumber,
                               idName,
                               FldName,
                               FileExt[idFile],
                               IndexTypes[IndexType[I]],
                               idKeyLen,
                               FFEBoolToStr(not idDups),
                               FFEBoolToStr(idAscend),
                               CaseFlag(idNoCase),
                               FFShStrTrimR(idDesc)]));
            J := 1;
            while J < idCount do begin
              Inc(J);
              WriteLn(F, Format('%25.25s%-17.17s', ['', FieldName[idFields[J - 1]]]));
            end;
          end;
        end;

        WriteLn(F);
        BoldOn;
        WriteLn(F, 'Files:');
        WriteLn(F);
        WriteLn(F, 'Num  File  Block  Type   Description');
        BoldOff;
        for I := 0 to FileCount - 1 do
          WriteLn(F, Format('%3d   %-3.3s %6d  %-5.5s  %s',
                            [I, FileExt[I], FileBlockSize[I],
                             FileTypes[FileType[I]], FileDesc[I]]));
        WriteLn(F);
        WriteLn(F);
        WriteLn(F, 'FlashFiler Explorer v' + FFEVersionStr);
        WriteLn(F, 'Printed ', DateTimeToStr(Now));
      finally
        System.Close(F);
      end;
    finally
      HidePrintingDlg;
    end;
  end;
end;


{=====Field grid routines=====}

procedure TfrmTableStruct.InitializeFieldGrid;
var
  T: TffFieldType;

begin
  grdFields.ColCount := cnFldHighest + 1;
  grdFields.RowCount := 2;

  grdFields.ColWidths[cnFldNumber] := 25;
  grdFields.ColWidths[cnFldName] := 110;
  grdFields.ColWidths[cnFldType] := 100;
  grdFields.ColWidths[cnFldUnits] := 40;
  grdFields.ColWidths[cnFldDecPl] := 50;
  grdFields.ColWidths[cnFldRequired] := 50;
  grdFields.ColWidths[cnFldDefault] := 110;
  grdFields.ColWidths[cnFldDesc] := 250;

  grdFields.DefaultRowHeight := cboFieldType.Height;


  FFEConfigGetColumnPrefs(ClassName + '.FieldGrid', grdFields);

  PopulateFieldGridHeader;

  { Load up the datatype combo box }
  for T := Low(T) to High(T) do
    if FFEFieldTypeToIndex(T) <> -1 then
      cboFieldType.Items.Add(FieldDataTypes[T]);

  btnInsertField.Enabled := False;
  btnDeleteField.Enabled := False;
  btnMoveFieldUp.Enabled := False;
  btnMoveFieldDown.Enabled := False;
end;
{--------}
procedure TfrmTableStruct.PopulateFieldGridHeader;
var
  ColNum : Integer;
begin
  grdFields.BeginUpdate;
  try
    for ColNum := 0 to cnFldHighest do
      case ColNum of
        cnFldNumber   : grdFields.Cells[ColNum, 0] := cnsNumber;
        cnFldName     : grdFields.Cells[ColNum, 0] := cnsName;
        cnFldType     : grdFields.Cells[ColNum, 0] := cnsType;
        cnFldUnits    : grdFields.Cells[ColNum, 0] := cnsUnits;
        cnFldDecPl    : grdFields.Cells[ColNum, 0] := cnsDecPl;
        cnFldRequired : grdFields.Cells[ColNum, 0] := cnsRequired;
        cnFldDefault  : grdFields.Cells[ColNum, 0] := cnsDefault;
        cnFldDesc     : grdFields.Cells[ColNum, 0] := cnsDesc;
      end;
  finally
    grdFields.EndUpdate;
  end;
end;
{--------}
procedure TfrmTableStruct.InvalidateFieldsTable;
var
  RowNum : Integer;
begin
  if FFieldList.Count = 0 then
    grdFields.RowCount := 2
  else
    grdFields.RowCount := succ(FFieldList.Count);
  for RowNum := 1 to FFieldList.Count do
    InvalidateFieldsRow(RowNum);
  for RowNum := 1 to pred(grdFields.RowCount) do                      {!!.06}
    grdFields.Cells[0, RowNum] := IntToStr(RowNum-1);                 {!!.06}
end;
{--------}
procedure TfrmTableStruct.InvalidateFieldsRow(const RowNum : Integer);
var
  ColNum      : Integer;
begin
  for ColNum := 0 to Pred(grdFields.ColCount)do
    with FFieldList.Items[Pred(RowNum)] do
      case ColNum of
        cnFldName:
          grdFields.Cells[ColNum,RowNum] := Name;
        cnFldType:
          grdFields.Cells[ColNum,RowNum] := cboFieldType.Items.Strings[fiDataTypeIndex];
        cnFldUnits:
          grdFields.Cells[ColNum,RowNum] := IntToStr(fiUnits);
        cnFldDecPl:
          grdFields.Cells[ColNum,RowNum] := IntToStr(fiDecPlaces);
        cnFldDefault:
          begin
            if fiValCheck.vdHasDefVal then begin
              grdFields.Cells[ColNum, RowNum] :=                      {!!.06}
                FFVCheckValToString(fiValCheck.vdDefVal,
                  FFEIndexToFieldType(fiDataTypeIndex));
            end else
              grdFields.Cells[ColNum,RowNum] := '';
          end;
        cnFldDesc:
          grdFields.Cells[ColNum,RowNum] := fiDescription;
    end;
end;
{--------}
procedure TfrmTableStruct.EnableBLOBControls;
begin
  lblBLOBExtension.Enabled := radBLOBExternal.Checked;
  edtBLOBExtension.Enabled := radBLOBExternal.Checked;

  lblBLOBBlockSize.Enabled := radBLOBExternal.Checked;
  cboBLOBBlockSize.Enabled := radBLOBExternal.Checked;

  lblBLOBFileDesc.Enabled := radBLOBExternal.Checked;
  edtBLOBFileDesc.Enabled := radBLOBExternal.Checked;
end;
{--------}
procedure TfrmTableStruct.EnableFieldControls(aRowNum: LongInt);
begin
  if (aRowNum > 0) and (aRowNum <= FFieldList.Count) then begin
    btnInsertField.Enabled := FFieldList.Items[aRowNum - 1].Name <> '';
    btnDeleteField.Enabled := aRowNum <> grdFields.RowCount - 1;
    btnMoveFieldUp.Enabled := (aRowNum <> grdFields.RowCount - 1) and (aRowNum <> 1);
    btnMoveFieldDown.Enabled := aRowNum < grdFields.RowCount - 2;
  end;
end;
{--------}
procedure TfrmTableStruct.LeavingFieldsCell(const Col, Row: LongInt);
{ Store new data info FFieldList; Update the interface before the
  Cell is changed}
var
  i, j        : Integer;
  TempStr     : string[255];
  TempInt     : Longint;
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
begin
  if FFieldList.Count > (Row - 1) then
    with FFieldList.Items[Row - 1] do
      case Col of
        cnFldName:
          begin
            TempStr := Name;
            Name := grdFields.Cells[Col, Row];
            {rename fields in indexes}
            if TempStr <> '' then
              for I := 0 to Pred(FIndexList.Count) do
                for J := 0 to Pred(FIndexList.Items[I].FieldCount) do
                  if FIndexList.Items[I].FieldName[j] = TempStr then
                    FIndexList.Items[I].FieldName[j] := Name;

            if Row = Pred(grdFields.RowCount) then
              { If we've added a name in the empty row,
                add a new empty row to the list }
              if (FDialogMode in [dmRestructuring, dmCreating]) and   {Start !!.01}
                 (Name <> '') then begin
                FFieldList.AddEmpty;
                InvalidateFieldsTable;
              end;                                                    {End !!.01}

              { Set the default datatype }
            if (fiDataTypeIndex = -1) and (Row > 1) then begin
              fiDataTypeIndex := FFieldList.Items[Row - 2].fiDataTypeIndex;
              if FFEIndexToFieldType(fiDataTypeIndex) >= fftByteArray then
                fiUnits := FFieldList.Items[Row - 2].fiUnits;
            end else
              if (fiDataTypeIndex = -1) then begin
                fiDataTypeIndex := 9;
                if FFEIndexToFieldType(fiDataTypeIndex) >= fftByteArray then
                fiUnits := FFieldList.Items[Row - 2].fiUnits;
              end;
          end;

        cnFldType:
          begin
            TempInt := fiDataTypeIndex;
            fiDataTypeIndex := cboFieldType.Items.IndexOf(grdFields.Cells[Col, Row]);
            if TempInt <> fiDataTypeIndex then begin
              fiValCheck.vdHasDefVal := False;
              FillChar(fiValCheck.vdDefVal, SizeOf(fiValCheck.vdDefVal), #0);
            end;
          end;

        cnFldUnits:
          begin
            TempInt := fiUnits;
            fiUnits := StrToInt('0' + grdFields.Cells[Col, Row]);
            {Clear the default value if it is longer than the new
             Units value.}
  //          Move(fiValCheck, TempStr, ffMaxL(fiUnits, TempInt));
            if (fiUnits < TempInt)  {and
                (Length(AnsiString(TempStr)) > fiUnits))} then begin
              fiValCheck.vdHasDefVal := False;
              FillChar(fiValCheck.vdDefVal, SizeOf(fiValCheck.vdDefVal), #0);
            end;
          end;

        cnFldDecPl:
          begin
            fiDecPlaces := StrToInt('0' + grdFields.Cells[Col, Row]);
            if fiDataTypeIndex <> -1 then
              CalcActualValues;
          end;

        cnFldDefault:
          begin
            if grdFields.Cells[Col, Row] <> '' then begin
              FFStringToVCheckVal(grdFields.Cells[Col, Row],          {!!.06}
                FFEIndexToFieldType(fiDataTypeIndex),
                fiValCheck.vdDefVal);
              fiValCheck.vdHasDefVal := True;
            end else
              fiValCheck.vdHasDefVal := False;
          end;

        cnFldDesc:
          fiDescription := grdFields.Cells[Col, Row];

      end;
  InvalidateFieldsRow(grdFields.Row);
  grdFields.Invalidate;
end;

{=====Index grid routines=====}
procedure TfrmTableStruct.InitializeIndexGrid;
begin
  grdIndexes.ColCount := cnIdxHighest + 1;
  grdIndexes.RowCount := 2;

  grdIndexes.ColWidths[cnIdxNumber] := 25;
  grdIndexes.ColWidths[cnIdxName] := 110;
  grdIndexes.ColWidths[cnIdxType] := 50;
  grdIndexes.ColWidths[cnIdxKeyLength] := 50;
  grdIndexes.ColWidths[cnIdxUnique] := 42;
  grdIndexes.ColWidths[cnIdxAscending] := 42;
  grdIndexes.ColWidths[cnIdxCaseSensitive] := 38;
  grdIndexes.ColWidths[cnIdxExt] := 40;
  grdIndexes.ColWidths[cnIdxBlockSize] := 60;
  grdIndexes.ColWidths[cnIdxDesc] := 250;

  grdIndexes.DefaultRowHeight := cboIndexType.Height;


  FFEConfigGetColumnPrefs(ClassName + '.IndexGrid', grdIndexes);

  chkAvailFieldsSorted.Checked := Config.SortAvailIndexFields;
  lstAvailFields.Sorted := chkAvailFieldsSorted.Checked;
  PopulateIndexGridHeader;
end;
{--------}
procedure TfrmTableStruct.PopulateIndexGridHeader;
var
  ColNum : Integer;
begin
  grdIndexes.BeginUpdate;
  try
    for ColNum := 0 to cnIdxHighest do
      case ColNum of
        cnIdxNumber        : grdIndexes.Cells[ColNum, 0] := cnsNumber;
        cnIdxName          : grdIndexes.Cells[ColNum, 0] := cnsName;
        cnIdxType          : grdIndexes.Cells[ColNum, 0] := cnsType;
        cnIdxKeyLength     : grdIndexes.Cells[ColNum, 0] := cnsKeyLen;
        cnIdxUnique        : grdIndexes.Cells[ColNum, 0] := cnsUnique;
        cnIdxAscending     : grdIndexes.Cells[ColNum, 0] := cnsAscend;
        cnIdxCaseSensitive : grdIndexes.Cells[ColNum, 0] := cnsCaseSens;
        cnIdxExt           : grdIndexes.Cells[ColNum, 0] := cnsExt;
        cnIdxBlockSize     : grdIndexes.Cells[ColNum, 0] := cnsBlockSize;
        cnIdxDesc          : grdIndexes.Cells[ColNum, 0] := cnsDesc;
      end;
  finally
    grdIndexes.EndUpdate;
  end;
end;

procedure TfrmTableStruct.PopulateIndexFieldsLists(aIndex: LongInt);
var
  I: Integer;
  IndexSelected : boolean;
begin
  if aIndex <= Pred(FIndexList.Count) then begin
    case FDialogMode of
      dmViewing, dmCreating :
        IndexSelected := (aIndex < FIndexList.Count) and (aIndex >= 0);
    else
      IndexSelected := (aIndex < Pred(FIndexList.Count)) and (aIndex >= 0);
    end;

    with FIndexList.Items[aIndex] do begin
      if Name = '' then
        grpCompositeKey.Caption := ' Composite Key '
      else
        grpCompositeKey.Caption := ' Composite Key (' + Name + ') ';

      { Show fields defined for the current index }
      lstIndexFields.Clear;
      if IndexSelected then begin
        lstIndexFields.Items.BeginUpdate;
        try
          for I := 0 to FieldCount - 1 do
            lstIndexFields.Items.Add(FieldName[I]);
        finally
          lstIndexFields.Items.EndUpdate;
        end;
      end;
    end;

      { Show fields remaining in the table eligible to become part of the index }
    with lstAvailFields do begin
      Items.BeginUpdate;
      try
        Clear;
        for I := 0 to FFieldList.Count - 1 do
          with FFieldList.Items[I] do
            if (Name <> '') and
               { ByteArray and BLOB type scan't be in keys }
               not (FieldType in [fftByteArray, fftBLOB..ffcLastBLOBType]) and
               { Field already in index list }
               (lstIndexFields.Items.IndexOf(Name) = -1) then
              Items.Add(Name);
      finally
        Items.EndUpdate;
      end;
    end;
  end;
end;
{--------}
procedure TfrmTableStruct.InvalidateIndexesTable;
var
  RowNum: Integer;
begin
  if FIndexList.Count = 0 then
   grdIndexes.RowCount := 2
  else
    grdIndexes.RowCount := succ(FIndexList.Count);
  for RowNum := 1 to FIndexList.Count do
    InvalidateIndexesRow(RowNum);
  for RowNum := 1 to Pred(grdIndexes.RowCount) do                     {!!.06}
    grdIndexes.Cells[0, RowNum] := IntToStr(RowNum-1);                {!!.06}
end;
{--------}
procedure TfrmTableStruct.InvalidateIndexesRow(const RowNum: Integer);
var
  ColNum : LongInt;
begin
(*  if grdIndexes.Row <> RowNum then begin                            {begin !!.06}
    with FIndexList.Items[RowNum - 1] do
      if (Name <> '') and
         (iiKeyTypeIndex = ktComposite) and
         (FieldCount = 0) then
        raise Exception.Create('No fields defined for composite index');
  end; *)                                                             {end !!.06}

  with grdIndexes do
    for ColNum := 0 to Pred(ColCount)do
      with FIndexList.Items[Pred(RowNum)] do
        case ColNum of
          cnIdxName          : Cells[ColNum, RowNum] := Name;
          cnIdxType          : Cells[ColNum, RowNum] := cboIndexType.Items.Strings[iiKeyTypeIndex];
          cnIdxKeyLength     : Cells[ColNum, RowNum] := IntToStr(iiKeyLen);
          cnIdxExt           : Cells[ColNum, RowNum] := iiExtension;
          cnIdxBlockSize     : Cells[ColNum, RowNum] := cboBlockSize.Items.Strings[iiBlockSizeIndex];
          cnIdxDesc          : Cells[ColNum, RowNum] := iiDescription;
      end;
end;

function TfrmTableStruct.CalcKeyLength(aIndex: Integer): Integer;
var
  I, J: Integer;
begin
  Result := 0;
  with FIndexList.Items[aIndex] do begin
    for I := 0 to FieldCount - 1 do
      with FFieldList do begin
        J := IndexOf(FieldName[I]);
        if J <> -1 then begin
          Inc(Result, Items[J].fiSize);
          Inc(Result);
        end;
      end;
  end;
end;
{--------}
procedure TfrmTableStruct.EnableIndexControls(aRowNum: LongInt; aName: string);
var
  Switch: Boolean;
begin
  if aRowNum = 0 then
    Exit;

  if (aRowNum > 0) and (aRowNum <= FIndexList.Count) then
    btnDeleteIndex.Enabled := aRowNum <> grdIndexes.RowCount - 1;

  with FIndexList.Items[aRowNum - 1] do begin
    { We only enable the key controls when it's a composite key,
      we're in edit mode, and we are focused on a valid index. }
    if aName = '' then aName := Name;
    Switch := (iiKeyTypeIndex = ktComposite) and
              (aName <> '') and
              (FDialogMode in [dmCreating, dmRestructuring]);

    if grpCompositeKey.Enabled <> Switch then
      FFEEnableContainer(grpCompositeKey, Switch);
  end;
end;


{=====Fieldmap routines=====}
procedure TfrmTableStruct.InvalidateFieldMapRow(const RowNum: Integer);
var
  ThisFieldType: TffFieldType;
  ColNum: Integer;
begin
  with FFieldList.Items[Pred(RowNum)] do
    if Name <> '' then
      for ColNum := 0 to Pred(cnMapHighest) do
        case ColNum of
          cnMapFieldName: grdFieldMap.Cells[ColNum, RowNum] := Name;
          cnMapDatatype:
            begin
              ThisFieldType := FFEIndexToFieldType(fiDataTypeIndex);
              FTempStr := FieldDataTypes[ThisFieldType];
              if ThisFieldType >= fftByteArray then
                FTemPStr := Format('%s[%d]', [FTempStr, fiUnits]);
              grdFieldMap.Cells[ColNum, RowNum] := FTempStr;
            end;
          cnMapOldField:
            begin
              RetrieveFieldMapSettings(RowNum, FFieldMapComboRec.Index, FFieldMapComboRec.RTItems);
              grdFieldMap.Cells[ColNum, RowNum] := FFieldMapComboRec.RTItems[FFieldMapComboRec.Index];
            end;
        end;
end;
{--------}
procedure TfrmTableStruct.InvalidateFieldMapTable;
var
  RowNum: Integer;
begin
  grdFieldMap.RowCount := FFieldList.Count;
  for RowNum := 1 to FFieldList.Count do
    InvalidateFieldMapRow(RowNum);
end;


{=====Fieldgrid validation routines=====}
function TfrmTableStruct.FieldNameValidation(const AName: string;
                               var ErrorCode: Word): Boolean;
var
  FieldName: TffDictItemName;
  I: LongInt;

begin
  FieldName := FFShStrTrim(AName);
  if FieldName <> '' then begin
    I := FFieldList.IndexOf(FieldName);
    if (I <> -1) and (I <> grdFields.Row - 1) then begin
      ErrorCode := oeDuplicateFieldName;
      Result := False;
      Exit;
    end;
  end;

  with grdFields do
    if (FieldName = '') and (Row <> RowCount - 1) then begin
      ErrorCode := oeMissingFieldName;
      Result := False;
      Exit;
    end;

  ErrorCode := 0;
  Result := True;
end;
{--------}
function TfrmTableStruct.FieldLengthValidation(const ALength: string;
                                 var ErrorCode: Word): Boolean;
begin
  if not ValidateFieldUnits(StrToInt('0' + ALength), grdFields.Row - 1) then begin
    ErrorCode := oeInvalidFieldUnits;
    Result := False;
    Exit;
  end;

  ErrorCode := 0;
  Result := True;
end;
{--------}
function TfrmTableStruct.ValidateFieldUnits(aUnits, aFieldNum: Integer): Boolean;
begin
  case FFEIndexToFieldType(FFieldList.Items[aFieldNum].fiDataTypeIndex) of
    fftShortString,
    fftShortAnsiStr:
      Result := (aUnits > 0) and (aUnits < 256);
    fftByteArray,
    fftNullString,
    fftNullAnsiStr,
    fftWideString:
      Result := (aUnits > 0) and (aUnits <= dsMaxStringSize);         {!!.06}
    else
      Result := True;
  end;
end;


{=====Indexgrid validation routines=====}
function TfrmTableStruct.IndexNameValidation(const AName: string;
                               var ErrorCode: Word): Boolean;
var
  IndexName: TffDictItemName;
  I: LongInt;
begin
  IndexName := FFShStrTrim(AName);
  if IndexName <> '' then begin
    I := FIndexList.IndexOf(IndexName);
    if (I <> -1) and (I <> grdIndexes.Row - 1) then begin
      ErrorCode := oeDuplicateIndexName;
      Result := False;
      Exit;
    end;
  end;

 with grdIndexes do
   if (IndexName = '') and (Row <> RowCount - 1) then begin
     ErrorCode := oeMissingIndexName;
     Result := False;
     Exit;
   end;

  ErrorCode := 0;
  Result := True;
end;
{--------}
function TfrmTableStruct.IndexExtensionValidation(const AExtension: string;
                                    var ErrorCode: Word): Boolean;
var
  ThisExtension: TffExtension;
  Idx : Integer;                                                                {!!.06}
begin
  ThisExtension := FFShStrTrim(AExtension);
  if ThisExtension <> '' then begin

    { Can't match the data file }
    if (FFAnsiCompareText(ThisExtension, ffc_ExtForData)=0) or                    {!!.06}{!!.07}
       (FFAnsiCompareText(ThisExtension, ffc_ExtForTrans)=0) or                   {!!.06}{!!.07}
       (FFAnsiCompareText(ThisExtension, ffc_ExtForSQL)=0) then begin             {!!.06}{!!.07}
      ErrorCode := oeInvalidFileExtension;
      Result := False;
      Exit;
    end;

    { See if there's a conflict with the BLOB extension (if any) }
    if radBLOBExternal.Checked and
       (FFAnsiCompareText(ThisExtension, edtBLOBExtension.Text)=0) then begin     {!!.06}{!!.07}
      ErrorCode := oeDuplicateFileExtension;
      Result := False;
      Exit;
    end;

    { See if there's a conflict with other index extensions (if any) }          {begin !!.06}
    for Idx := 0 to Pred(FIndexList.Count) do begin
      if Idx = grdIndexes.Row - 1 then
        continue;
      if FFAnsiCompareText(ThisExtension, FIndexList.Items[Idx].iiExtension) = 0 then begin {!!.07}
        ErrorCode := oeDuplicateFileExtension;
        Result := False;
        Exit;
      end;
    end;                                                                        {end !!.06}
  end;

  ErrorCode := 0;
  Result := True;
end;
{--------}
function TfrmTableStruct.IndexKeyLenValidation(const AKeyLen: Integer;
                                 var ErrorCode: Word): Boolean;
begin
(*  with grdIndexes do
    case FIndexList.Items[Row - 1].iiKeyTypeIndex of
      ktUserDefined:
        if IntToStr('0' +TOvcNumericField(Sender).AsInteger = 0 then
          ErrorCode := oeInvalidIndexKeyLength;
    end;
  if TOvcNumericField(Sender).AsInteger > ffcl_MaxKeyLength then
    ErrorCode := oeMaximumIndexKeyLength;*)
  ErrorCode := 0;
  Result := True;
end;


{=====Misc validation routines}
{--------}
function TfrmTableStruct.edtBLOBExtensionValidation(const AExtension: string;
                                      var ErrorCode: Word): Boolean;
var
  ThisExtension: TffExtension;
  I: Integer;
begin
  ThisExtension := FFShStrTrim(AExtension);
  if ThisExtension <> '' then begin

    { Can't match the data file }                                               {begin !!.06, !!.07}
    if (FFAnsiCompareText(ThisExtension, ffc_ExtForData)=0) or
       (FFAnsiCompareText(ThisExtension, ffc_ExtForTrans)=0) or
       (FFAnsiCompareText(ThisExtension, ffc_ExtForSQL)=0) then begin
      ErrorCode := oeInvalidFileExtension;
      Result := False;
      Exit;
    end;                                                                        {end !!.06, !!.07}

    { See if this extension is being used for any index files }
    for I := 0 to FIndexList.Count - 1 do
      with FIndexList.Items[I] do begin
        if (Name <> '') and
           (I <> grdIndexes.Row - 1) and
           (iiExtension = ThisExtension) then begin
          ErrorCode := oeDuplicateFileExtension;
          Result := False;
          Exit;
        end;
      end;
  end;
  ErrorCode := 0;
  Result := True;
end;
{--------}
function TfrmTableStruct.ValidateRestructure: Boolean;
begin
  { Auto-assign field map }
  if tabStructure.Pages[tabStructure.PageCount-1].Enabled and
     chkPreserveData.Checked and
     (FFieldMap.Count = 0) then begin
    btnMatchByNameClick(nil);
    if (FDatabase.Tables[FTableIndex].RecordCount > 0) and            {!!.06}
       (FFieldMap.Count <> FDatabase.Tables[FTableIndex].Dictionary.FieldCount) then begin
      Result := not (MessageDlg('Some data may be lost.  Would you like to ' +
                                'verify the field mappings?', mtWarning,
                                [mbYes, mbNo], 0) = mrYes);
      if not Result then
        tabStructure.ActivePage := tbsExistingData;
      Exit;
    end;
  end;

  with tabStructure do
    if (FDatabase.Tables[FTableIndex].RecordCount > 0) and            {!!.06}
       (not chkPreserveData.Checked or (FFieldMap.Count = 0)) and
        Pages[PageCount - 1].Enabled then begin
      Result := MessageDlg('Restructure without preserving existing data?', mtWarning, [mbYes, mbNo], 0) = mrYes;
      Exit;
    end;

  Result := True;
end;
{--------}
procedure TfrmTableStruct.DisplayValidationError(ErrorCode: Word);
begin
  case ErrorCode of
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
      MessageDlg(Format('Index key length cannot exceed %d', [ffcl_MaxKeyLength]), mtError, [mbOK], 0);
  end;
end;
{--------}
function TfrmTableStruct.ValidateForm: Boolean;
var
  I: Integer;
begin
  if not edtTableName.ReadOnly then begin
    if edtTableName.Text = '' then begin
      edtTableName.SetFocus;
      raise Exception.Create('Invalid table name');
    end;
  end;

  { Make sure we have a correct block size }
  if not FFVerifyBlockSize(StrToInt(cboBlockSize.Text)) then begin
    cboBlockSize.SetFocus;
    raise Exception.Create('Invalid block size');
  end;

  { Make sure the field list is valid }
  { needs to be expanded}
  for I := 0 to FFieldList.Count - 1 do
    with FFieldList.Items[I] do begin
      if not ((Name = '') and (I = FFieldList.Count - 1)) then begin
        if Name = '' then begin
          with grdFields do begin
            Row := I + FixedRows;
            Col := cnFldName;
          end;
          raise Exception.Create('Invalid field name');
        end;

        if fiDataTypeIndex = -1 then begin
         with grdFields do begin
           Row := I + FixedRows;
           Col := cnFldType;
         end;
          raise Exception.Create('Invalid data type');
        end;

        if not ValidateFieldUnits(fiUnits, I) then begin
         with grdFields do begin
           Row := I + FixedRows;
           Col := cnFldUnits;
         end;
          raise Exception.Create('Invalid units for this data type');
        end;
      end;
    end;

  { make sure the composite indexes have fields }                     {begin !!.06}
  for I := 0 to Pred(FIndexList.Count) do
    if (FIndexList.Items[I].Name <> '') and
       (FIndexList.Items[I].iiKeyTypeIndex = ktComposite) and
       (FIndexList.Items[I].FieldCount = 0) then
      raise Exception.CreateFmt
        ('No fields defined for composite index: %s',
        [FIndexList.Items[I].Name]);                                  {end !!.06}

  Result := True;
end;
{--------}
procedure TfrmTableStruct.grdFieldsExit(Sender: TObject);
begin
  LeavingFieldsCell(grdFields.Col, grdFields.Row);
end;
{--------}
procedure TfrmTableStruct.InitializeFieldMapGrid;
begin
  grdFieldMap.ColCount := cnMapHighest;
  grdFieldMap.RowCount := 2;

  grdFieldMap.ColWidths[cnMapFieldName] := 135;
  grdFieldMap.ColWidths[cnMapDatatype] := 120;
  grdFieldMap.ColWidths[cnMapOldField] := 203;

  grdFieldMap.DefaultRowHeight := cboMapOldField.Height;

  PopulateFieldMapHeader;
end;
{--------}
procedure TfrmTableStruct.PopulateFieldMapHeader;
var
  ColNum: Integer;
begin
  with grdFieldMap do begin
    BeginUpdate;
    try
      for ColNum := 0 to cnMapHighest do
        case ColNum of
          cnMapFieldName : Cells[ColNum, 0] := 'New Field Name';
          cnMapDatatype  : Cells[ColNum, 0] := 'Data Type';
          cnMapOldField  : Cells[ColNum, 0] := 'Old Field';
        end;
    finally
      EndUpdate;
    end;
  end;
end;
{--------}
procedure TfrmTableStruct.grdFieldMapKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Key = #13 then
    { Change the selected cell (Enter as tab)}
    with grdFieldMap do
      if Col < Pred(ColCount) then
        Col := Col + 1
      else if Row < Pred(RowCount) then begin
        Row := Row + 1;
        Col := cnFldName;
      end else begin
        Row := 1;
        Col := cnFldName;
      end
end;
{--------}
procedure TfrmTableStruct.grdFieldMapSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  R: TRect;
  Idx : Integer;
begin
  CanSelect := True;

  { Set any special cell attributes (ComboBoxes, Readonly fields)}
  case ACol of
    cnMapOldField:
      begin
        R := grdFieldMap.CellRect(ACol, ARow);
        ShowCellCombo(cboMapOldField, grdFieldMap, R);
//        Idx := cboMapOldField.ItemIndex; - Idx only used to return value below
        RetrieveFieldMapSettings(ARow, Idx, cboMapOldField.Items);
        cboMapOldField.ItemIndex := Idx;
      end;
  end;
end;
{--------}
procedure TfrmTableStruct.cboMapOldFieldChange(Sender: TObject);
begin
  with grdFieldMap do begin
    Cells[Col, Row] := TComboBox(Sender).Items[TComboBox(Sender).ItemIndex];
  end;
  tcMapOldFieldChange(Sender);
  grdFieldMap.Invalidate;
end;
{--------}
procedure TfrmTableStruct.cboMapOldFieldExit(Sender: TObject);
begin
  TComboBox(Sender).Visible := False;
  FcboMapOldFieldHasBeenFocused := ActiveControl=grdFieldMap;            {!!.11}
  { only if Enter key was pressed }
  if FInEnterKeyPressed then                                             {!!.11}
  if Assigned(ActiveControl) and not(ActiveControl = grdFieldMap) then
    ActiveControl.SetFocus
  else begin
    grdFieldMap.SetFocus;
    grdFieldMap.Perform(WM_KEYDOWN, VK_TAB, 0);
  end;
end;
{--------}
procedure TfrmTableStruct.RetrieveFieldMapSettings(const ARow : integer;
                                                     var Index: Integer;
                                                         AStrings: TStrings);
var
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
  function StringListFindFirst(Strings: TStringList; const S: string; var Index: Integer): Boolean;
  var
    L, H, I, C: Integer;
  begin
    Result := False;
    L := 0;
    H := Strings.Count - 1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := AnsiStrLIComp(PChar(Strings[I]), PChar(S), Length(S));
      if C < 0 then L := I + 1 else
      begin
        H := I - 1;
        if C = 0 then
        begin
          Result := True;
          if Strings.Duplicates <> dupAccept then L := I;
        end;
      end;
    end;
    Index := L;
  end;
  {End !!.11}

begin
  with FFieldList.Items[Pred(ARow)] do begin
    CurrentFieldName := Name; { from FFieldList.Items[x] }

    { Fill the combo box dropdown list with all old fields that are
      a) assignment compatible with the current new field and
      b) not already assigned to another new field. }
    with AStrings do begin
      Clear;
      BeginUpdate;
      {Begin !!.11}
      CreateReverseFFieldMap := not Assigned(ReverseFFieldMap);
      if CreateReverseFFieldMap then
        ReverseFFieldMap := TStringList.Create;
      {End !!.11}

      try
        {Begin !!.11}
        if CreateReverseFFieldMap then
          for i := 0 to Pred(FFieldMap.Count) do
            ReverseFFieldMap.Values[FFieldMap.Values[FFieldMap.Names[i]]] := FFieldMap.Names[i];
        ReverseFFieldMap.Sorted := True;
        {End !!.11}
        Add('<none>');
        with FDatabase.Tables[FTableIndex].Dictionary do begin
          for I := 0 to FieldCount - 1 do begin
            OldFieldName := FieldName[I];

            { Check assignment compatability }
            Disqualified := FFConvertSingleField(
                              nil,
                              nil,
                              FieldType[I],
                              FFEIndexToFieldType(fiDatatypeIndex),
                              -1,
                              -1) <> DBIERR_NONE;

            { Already assigned to another new field?
              (make sure to skip the current field) }
            if not Disqualified then begin

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
              if StringListFindFirst(ReverseFFieldMap, OldFieldName+'=', IndexOfOldFieldName) and
                 (ReverseFFieldMap[IndexOfOldFieldName]<>OldFieldName+'='+CurrentFieldName) then
                 Disqualified := True;
            end;  { if }

            if Disqualified then Continue;

            { If OK, then add it to the list }
            if FieldType[I] >= fftByteArray then
              DisplayDatatype := Format('(%s[%d])', [FieldDataTypes[FieldType[I]], FieldUnits[I]])
            else
              DisplayDatatype := Format('(%s)', [FieldDataTypes[FieldType[I]]]);
            Add(FieldName[I] + ' ' + DisplayDatatype);
          end;  { for }
        end;  { with }
      finally
        EndUpdate;
        {Begin !!.11}
        if CreateReverseFFieldMap then begin
          ReverseFFieldMap.Free;
          ReverseFFieldMap := nil;
        end;
        {End !!.11}
      end;
    end;

    { See if we already have an assignment for the current field,
      and if so set the combo box index value accordingly }
    with AStrings do begin
      Index := 0;
      OldFieldName := FFieldMap.Values[CurrentFieldName];
      if OldFieldName <> '' then begin
        for J := 0 to Count - 1 do
          if Pos(AnsiUpperCase(OldFieldName + ' ('), AnsiUpperCase(Strings[J])) <> 0 then begin
            Index := J;
            Break;
          end;  { if }
      end;  { if }
    end;  { with }
  end;
end;
{--------}
procedure TfrmTableStruct.tabStructureChange(Sender: TObject);
begin
  case tabStructure.ActivePage.PageIndex of
    1: begin
         PopulateIndexFieldsLists(grdIndexes.Row - 1);
       end;
    2: begin
         grdFieldMap.RowCount := FFieldList.Count;

         { Auto-assign the field map }
         if FFieldMap.Count = 0 then
           btnMatchByNameClick(Sender);
       end;
  end;
end;
{--------}
procedure TfrmTableStruct.LeavingIndexCell(const Col, Row: Integer);
{ Store new data info FFieldList; Update the interface before the
  Cell is changed}
begin
  if Row < 1 then
    Exit;

  with FIndexList.Items[Row - 1] do
    case Col of
      cnIdxName:
        begin
          Name := grdIndexes.Cells[Col, Row];
          if Row = Pred(grdIndexes.RowCount) then
            { If we've added a name in the empty row,
              add a new empty row to the list }
            if FDialogMode in [dmRestructuring, dmCreating] then
              FIndexList.AddEmpty;
            if Name <> '' then begin
              InvalidateIndexesTable;
            end;
        end;

      cnIdxType:
        iiKeyTypeIndex := cboIndexType.Items.IndexOf(grdIndexes.Cells[Col, Row]);

      cnIdxKeyLength:
        iiKeyLen := StrToInt('0' + grdIndexes.Cells[Col, Row]);

      cnIdxExt:
        iiExtension := grdIndexes.Cells[Col, Row];

      cnIdxBlockSize:
        iiBlockSizeIndex := cboIndexBlockSize.Items.IndexOf(grdIndexes.Cells[Col, Row]);

      cnIdxDesc:
        iiDescription := grdIndexes.Cells[Col, Row];
    end;
  InvalidateIndexesRow(grdIndexes.Row);
  grdIndexes.Invalidate;
end;
{--------}
procedure TfrmTableStruct.grdIndexesExit(Sender: TObject);
begin
  LeavingIndexCell(grdIndexes.Col, grdIndexes.Row);
end;
{--------}
procedure TfrmTableStruct.FormKeyPress(Sender: TObject; var Key: Char);
begin
  FHasChanged := True;
end;
{--------}
procedure TfrmTableStruct.lstAvailFieldsDblClick(Sender: TObject);
begin
  if FDialogMode <> dmViewing then
    AddFieldToIndex;
end;
{--------}
procedure TfrmTableStruct.lstIndexFieldsDblClick(Sender: TObject);
begin
  if FDialogMode <> dmViewing then
    RemoveFieldFromIndex;
end;
{--------}
function TfrmTableStruct.AllowDefaultField(aRowNum    : Integer;
                                       var aErrorCode : Word) : Boolean;
var
  FieldType : TffFieldType;
begin
  Assert(Assigned(FFieldList.Items[pred(aRowNum)]));
  Assert(Assigned(grdFields));
  Assert(grdFields.ColCount > cnFldUnits);
  Assert(grdFields.RowCount > aRowNum);
  Result := False;
  FieldType := FFieldList.Items[pred(aRowNum)].FieldType;
  {This field type must allow default values}
  if FFEFieldAllowedDefault(FieldType) then begin
    Result := True;
    {if this field type requires units, ensure it's set}
    if ((FFEFieldTypeRequiresUnits(FieldType)) and
        (grdFields.Cells[cnFldUnits, aRowNum] = '0' )) then
      Result := False;
  end;
end;
{--------}
function TfrmTableStruct.ValidDefaultFieldKey(aUpKey : Char; aFieldType : TffFieldType) : Boolean;
type
  CharSet = set of Char;
const
  valValidNumber   = ['0'..'9'];
  valValidAlpha    = ['A'..'Z'];
  valValidBoolean  = ['T','R','U','E','F','A','L','S'];
  valValidExponent = ['E'];                                            {!!.10}
  valValidNegative = ['-'];
  valValidSpace    = [' '];
  valValidAll      = [#8, #9];
var
  valValidAMPM    : set of Char;
  valValidDecSep  : set of Char;
  valValidDateSep : set of Char;
  valValidTimeSep : set of Char;
  i : Integer;
begin
{Begin !!.10}
  Result := (aUpKey in valValidAll) or
            (aFieldType in [fftShortString, fftShortAnsiStr, fftNullString,
                            fftNullAnsiStr, fftWideString]);
  if Result then
    Exit;
{End !!.10}

  {Add Local Settings to the valValidAMPM set}
  valValidAMPM := [];
  for i := 1 to Length(TimeAMString) do
    Include(valValidAMPM, UpCase(TimeAMString[i]));
  for i := 1 to Length(TimePMString) do
    Include(valValidAMPM, UpCase(TimePMString[i]));
  valValidDecSep := [];
  valValidDateSep := [];
  valValidTimeSep := [];
  Include(valValidDecSep, UpCase(DecimalSeparator));
  Include(valValidDateSep, UpCase(DateSeparator));
  Include(valValidTimeSep, UpCase(TimeSeparator));

  case aFieldType of
    fftBoolean  : Result := aUpKey in valValidBoolean;
    fftChar,
    fftWideChar : Result := aUpKey in (valValidNumber + valValidAlpha + valValidSpace);
    fftByte,
    fftInt8,
    fftInt16,
    fftInt32    : Result := aUpKey in (valValidNumber + valValidNegative);

    fftWord16,
    fftWord32,
    fftComp     : Result := aUpKey in valValidNumber;

    fftSingle,
    fftDouble,
    fftExtended,
    fftCurrency : Result := aUpKey in (valValidNumber + valValidDecSep +     {!!.10}
                                       valValidNegative + valValidExponent); {!!.10}

    fftStDate   : Result := aUpKey in (valValidNumber + valValidDateSep);

    fftStTime   : Result := aUpKey in (valValidNumber + valValidTimeSep + valValidAMPM);

    fftDateTime : Result := aUpKey in (valValidNumber +
                                       valValidTimeSep +
                                       valValidDateSep +               {!!.01}
                                       valValidAMPM +
                                       valValidSpace);
  end;
end;
{--------}
procedure TfrmTableStruct.chkAvailFieldsSortedClick(Sender: TObject);
begin
  lstAvailFields.Items.BeginUpdate;
  try
    lstAvailFields.Sorted := chkAvailFieldsSorted.Checked;
    PopulateIndexFieldsLists(grdIndexes.Row - 1);
  finally
    lstAvailFields.Items.EndUpdate;
  end;
end;
{--------}
procedure TfrmTableStruct.grdIndexesEnterCell(Sender: TffStringGrid; aCol,
  aRow: Integer; const text: String);
begin
  EnableIndexControls(aRow, '');
end;
{--------}
procedure TfrmTableStruct.cboFieldTypeKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then begin
    Key := 0;
    grdFields.SetFocus;
  end;
end;
{--------}
procedure TfrmTableStruct.cboIndexTypeKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then begin
    Key := 0;
    grdIndexes.SetFocus;
  end;
end;
{--------}
procedure TfrmTableStruct.cboMapOldFieldKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then begin
    FInEnterKeyPressed := True;                                  {!!.11}
    try
      Key := 0;
      grdFieldMap.SetFocus;
    finally
      FInEnterKeyPressed := False;                               {!!.11}
    end;
  end;
end;
{--------}
procedure TfrmTableStruct.tabExistingDataChange(Sender: TObject);
begin
  tabFieldMapPageChanged(Sender, 2);
end;
{--------}
procedure TfrmTableStruct.edtBlobExtensionExit(Sender: TObject);                {begin !!.06}
var
  ErrorCode : Word;
begin
  if not edtBLOBExtensionValidation(edtBlobExtension.Text, ErrorCode) then begin
    DisplayValidationError(ErrorCode);
    edtBlobExtension.Text := '';
  end;
end;                                                                            {end !!.06}
{--------}
{Begin !!.11}
procedure TfrmTableStruct.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
{$IFDEF DCC4OrLater}
  Action := caFree;
{$ENDIF}
end;
{End !!.11}

end.

