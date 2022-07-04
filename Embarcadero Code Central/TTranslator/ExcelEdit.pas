{
    This file is part of the TTranslator 

    TTranslator is a Delphi component for localizing String and TStrings 
    properties of components dropped on a form. You can also localize your 
    code strings with TTranslator.
    Copyright (C) 2002 Polycon Ab

    TTranslator is free software; you can redistribute it and/or modify
    it under the terms of the version 2 of the GNU General Public License
    as published by the Free Software Foundation. Any commercial closed 
    source development which use the TTranslator component MUST ACQUIRE A
    COMMERCIAL LICENSE! For more information about licensing, please refer 
    to http://www.polycon.fi/translator/licensing.html

    TTranslator is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with TTranslator; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ $Id: ExcelEdit.pas,v 1.38 2003/04/16 14:02:50 laa Exp $ }

{-----------------------------------------------------------------------------
  ExcelEdit        The logic that import/exports to excel

  What             IExcelUI
                   TExcelEdit
                    + TRowStorageExcelEdit
                       + TAuxTableExcelEdit
                       + TGridEditorExcelEdit
                    + TMultiExcelEdit

  Company          Polycon
  Authors          LAA, MVJ
-----------------------------------------------------------------------------}

unit ExcelEdit;

interface
{$i common.inc}

{$ifdef D5_OR_HIGHER}
uses
  Classes, Dialogs, TranslatorInterfaces,
  DataElements, Storages, Criteria, RowList, CommonLib, ExcelEditErrors,
  OfficeIntf, GridEditor;

type
  TExcelState = (esNone, esExportStarted, esExportFinished, esImportStarted, esImportFinished);
  TExcelImportRule = (eirAdd, eirDelete, eirReplace);
  TExcelImportRuleSet = set of TExcelImportRule;
  TExcelTransferId = (etiFieldName, etiFieldDescription);
  TExcelEdit = class;

  IExcelUI = interface
    ['{506A6ED3-FE1C-4C96-B1AC-55D8CD5DB59E}']
    procedure SetExportEnabled(Value : Boolean);
    procedure SetImportEnabled(Value : Boolean);
    procedure SetImportFileEnabled(Value: Boolean);
    procedure SetExportProgress(RowCount : integer);
    procedure SetImportProgress(RowCount : integer);
    procedure SetPrepareProgress(RowCount : integer);
    function GetImportRule : TExcelImportRule;
    function ShowMe(ExcelEdit : TExcelEdit) : Boolean;
    procedure ForceClose;
  end;

  {/** Abstract base class for all Excel editing. */}
  TExcelEdit = class
  private
    { Private declarations }
    fExcel : TExcelInterface;
    fExcelCreated : Boolean;
    fExcelState : TExcelState;
    fAllowImport : Boolean;
    fExportFields : TFieldList;
    fImported : Boolean;
    fExcelUI : IExcelUI;
    procedure SetExcel(const Value: TExcelInterface);
  protected
    fStringTranslator : IStringTranslator;

    procedure InsertGridToExcel; virtual; abstract;


    function CreateErrorForm : TfrmExcelErrors; virtual;
    function GetExportCaption : string; virtual; abstract;
    function GetTotalRowCount : integer; virtual; abstract;
    function GetAllowedImportRules: TExcelImportRuleSet; virtual; abstract;
    procedure SetAllowedImportRules(const Value: TExcelImportRuleSet); virtual; abstract;

    property AllowImport : Boolean read fAllowImport write fAllowImport;
    property ExportFields : TFieldList read fExportFields;

    function GetString(const AProperty : String) : String; 
    function CustomMessageDlg(const Msg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Longint): Word; virtual;
  public
    constructor Create( ExcelUI : IExcelUI );
    destructor Destroy; override;

    procedure Run;
    function GetMainHeaderCaptionAppend : string; virtual; abstract;
    procedure DoExport;
    function DoImport : Boolean;
    function DoImportFromFile(const AFileName:string) : Boolean;
    procedure DoCancel;
    procedure DoClose;
    procedure FocusExcel;

    property Excel : TExcelInterface read fExcel write SetExcel;
    property TotalRowCount : integer read GetTotalRowCount;
    property Imported : Boolean read fImported write fImported;
    property AllowedImportRules : TExcelImportRuleSet read GetAllowedImportRules write SetAllowedImportRules;
  end;

 {/** Abstract base class for all Excel editing on a row storage basis. */}
  TRowStorageExcelEdit = class(TExcelEdit)
  private
    fExcelTransferId : TExcelTransferId;
    fAllowedImportRules : TExcelImportRuleSet;
    fAllowAddingRows : Boolean;

    function GetFieldIdentifier(AField: TDataField): string;
  protected
    function GetTable : TDataTable; virtual;
    function GetRowStorage : TAbstractRowStorage; virtual; abstract;

    procedure GetExportFields( FieldList : TFieldList ); virtual;
    function GetExportDataRow(iRow : integer) : TAbstractRow; virtual; abstract;


    procedure InsertGridToExcel; override;
    function GetAllowedImportRules: TExcelImportRuleSet; override;
    procedure SetAllowedImportRules(const Value: TExcelImportRuleSet); override;

    function GetExportCaption : string; override;
  public
    constructor Create( ExcelUI : IExcelUI );
    function GetMainHeaderCaptionAppend : string; override;

    property RowStorage : TAbstractRowStorage read GetRowStorage;
    property Table : TDataTable read GetTable;
    property ExcelTransferId : TExcelTransferId read fExcelTransferId write fExcelTransferId;
    property AllowAddingRows : Boolean read fAllowAddingRows write fAllowAddingRows;
  end;




  TAbstractGridEditorExcelEdit = class(TRowStorageExcelEdit)
  private
    fTable : TDataTable;
    fGridEditor : TGridEditor;
    fRows : TDataRowList;
  protected
    procedure GetExportFields( FieldList : TFieldList ); override;
    function GetRowStorage : TAbstractRowStorage; override;
    function GetTotalRowCount : integer; override;
    function GetExportDataRow(iRow : integer) : TAbstractRow; override;


    function DoExportField(AField: TDataField): Boolean; virtual; abstract;
    procedure GetExportRows(Rows: TDataRowList); virtual;
  public
    constructor Create(ExcelUI : IExcelUI; Table : TDataTable; GridEditor:TGridEditor);
    destructor Destroy; override;

    property Rows : TDataRowList read fRows;
    property GridEditor : TGridEditor read fGridEditor;
  end;

  {/** Class for Excel editing of a GridEditor. The exported resembles the
       visible fields in the GridEditor, except
        * fields with DataField.DefaultReadOnky=True are NEVER EVER exported
        * key fields (according to GridEditor.Table.FieldIsKey) are exported
          even when not present in the GridEditor
  */}
  TGridEditorExcelEdit = class(TAbstractGridEditorExcelEdit)
  protected
    procedure GetExportFields( FieldList : TFieldList ); override;
    function DoExportField(AField: TDataField): Boolean; override;
  end;

  {/** Class for Excel editing of a GridEditor. The Excel will contain *excactly*
       the fields that the GridEditor has. This means that the GridEditor must
       have unique key field values -- no duplicates are allowed, otherwise the
       import will fail. Keys that are allowed only one value, that are hidden in
       the GridEditor will be assigned the only legal value possible.

       Also note what we (LAA+MVJ) found out that inserting rows (and thus
       importing with delete) is not possible with this class. Fixing it is
       left "as an excercise for the reader".
       MVJ 11.3.2003: Should be fixed now.
  */}
  TExactGridEditorExcelEdit = class(TAbstractGridEditorExcelEdit)
  private

    fConsiderKeys, fHiddenKeys : TFieldList;
    procedure FillHiddenKeys(AKeyList : TFieldList);
  protected
    function DoExportField(AField: TDataField): Boolean; override;
    procedure CopyHiddenKeys(DestRow: TDataRow);


    property HiddenKeys : TFieldList read FHiddenKeys;
  public
    constructor Create(ExcelUI : IExcelUI; Table : TDataTable; GridEditor:TGridEditor);
    destructor Destroy; override;

    // This is the set of fields that uniquely identifies a row *in the GridEditor*
    // i.e. put here the fields which should be used when matching imported rows
    // with rows in the real TRowStorage   
    property ConsiderKeys : TFieldList read fConsiderKeys;
  end;

  {/** Class that exports/imports multiple ExcelEdit using one visual window,
       practical e.g. for exporting a DataEditor consisting of several
       GridEditors or for expoering many auxiliary tables into on excel workbook.
  */}
  TMultiExcelEdit = class( TExcelEdit )
  private
    fSubEdits : array of TExcelEdit;
  protected
    procedure InsertGridToExcel; override;

    function GetAllowedImportRules: TExcelImportRuleSet; override;
    procedure SetAllowedImportRules(const Value: TExcelImportRuleSet); override;

    function CreateErrorForm : TfrmExcelErrors; override;
    function GetExportCaption : string; override;
    function GetTotalRowCount : integer; override;
  public
    constructor Create(ExcelUI : IExcelUI; const SubEdits : array of TExcelEdit);
    destructor Destroy; override;

    function GetMainHeaderCaptionAppend : string; override;
  end;

{$endif D5_OR_HIGHER}

implementation

{$ifdef D5_OR_HIGHER}
uses
  Controls, SysUtils, Excel97, Quilt,
  DataType, DataTypes, Forms;

type
  TDefaultExcelTranslator = class(TDefaultTranslator)
  public
    constructor Create;
  end;

const
  HelpFileNullIndex = 0;
var
  DefaultExcelTranslator : TDefaultExcelTranslator;

{ TExcelEdit }

constructor TExcelEdit.Create(ExcelUI : IExcelUI);
begin
  inherited Create;
  fStringTranslator := DefaultExcelTranslator;

  fExcel := nil;
  fExcelCreated := True;
  fExcelUI := ExcelUI;
  fExcelState := esNone;
  Imported := False;
  fExportFields := nil;
  fAllowImport := False;
end;

destructor TExcelEdit.Destroy;
begin
  FreeAndNil( fExcel );
  fExportFields.Free;
  inherited Destroy;
end;

procedure TExcelEdit.DoExport;
const
  HelpFileNullIndex = 0;
begin
  fExcelUI.SetExportEnabled( False );
  fExcelUI.SetImportFileEnabled( False );
  fExcel := TExcelInterface.Create;
  fExcelState := esExportStarted;

  if fExcel.Successful then
    try
      InsertGridToExcel;
      fExcelUI.SetExportProgress( TotalRowCount );
      fExcel.Visible := True;
      fExcelUI.SetImportEnabled( AllowImport );

      fExcelState := esExportFinished;
    except
      on E:Exception do
      begin
        fExcelState := esNone;
        fExcel.Free;
        fExcel := nil;
        fExcelUI.SetExportEnabled( True );
        CustomMessageDlg( GetString('EXPORTERROR') + ' :"' + E.Classname + ':' + E.Message + '".', mtError, [mbOK], HelpFileNullIndex);
      end;
    end
  else
    CustomMessageDlg( GetString('NOEXCEL'), mtError, [mbOK], HelpFileNullIndex);
end;

procedure TExcelEdit.DoCancel;
begin
  try
    // This is also debatable, should we close the excel or not?
    if fExcelState = esExportFinished then
    begin
      fExcel.Visible := True;
      fExcel.AutoQuit := False;
    end;
    fExcel.Free;
    fExcel := nil;
  except
  end;
end;

procedure TExcelEdit.DoClose;
begin
  fExcelUI.ForceClose;
end;

procedure TExcelEdit.FocusExcel;
begin
  fExcel.Visible := True;
end;

function TExcelEdit.DoImport : Boolean;

const
  NEW_LINE = #13#10;

begin

  CustomMessageDlg( GetString('IMPEXPTRANS') + NEW_LINE +
              GetString('MOREINFO') + NEW_LINE +
             'http://www.polycon.fi/translator', mtInformation, [mbOK], 0);
  Result := False;

end;

function TExcelEdit.DoImportFromFile(const AFileName: string): Boolean;

const
  NEW_LINE = #13#10;

begin

  CustomMessageDlg( GetString('IMPEXPTRANS') + NEW_LINE +
              GetString('MOREINFO') + NEW_LINE +
             'http://www.polycon.fi/translator', mtInformation, [mbOK], 0);
  Result := False;

end;





procedure TRowStorageExcelEdit.GetExportFields( FieldList : TFieldList );
begin
  FieldList.FieldsToList( Table );
end;

function TExcelEdit.CreateErrorForm : TfrmExcelErrors;
begin
  Result := TfrmExcelErrors.Create( nil );
end;

procedure TExcelEdit.Run;
begin
  fExcelUI.ShowMe( Self );
end;

procedure TExcelEdit.SetExcel(const Value: TExcelInterface);
begin
  if fExcel=nil then
  begin
    fExcel := Value;
    fExcelCreated := False;
  end;
end;

function TExcelEdit.GetString(const AProperty : String) : String;
begin
  result := fStringTranslator.GetString(AProperty);
end;

function TExcelEdit.CustomMessageDlg(const Msg: string;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; HelpCtx: Integer): Word;
begin
  Result := MessageDlg(Msg, DlgType, Buttons, HelpCtx);
end;

{ TRowStorageExcelEdit }

constructor TRowStorageExcelEdit.Create(ExcelUI: IExcelUI);
var
  ARule : TExcelImportRule;
begin
  inherited;
  fExcelTransferId := etiFieldName;
  fAllowAddingRows := True;

  // all rules allowed by default
  fAllowedImportRules := [];
  for ARule := Low(TExcelImportRule) to High(TExcelImportRule) do
    fAllowedImportRules := fAllowedImportRules + [ARule];
end;

function TRowStorageExcelEdit.GetTable : TDataTable;
begin
  result := RowStorage.DataTable;
end;







function TRowStorageExcelEdit.GetExportCaption: string;
begin
  result := GetString('EXCELEDITOF') + ' ' + Table.TableName + ' ' + Table.Description;
end;

function TRowStorageExcelEdit.GetMainHeaderCaptionAppend : string;
begin
  Result := ' ' + Table.TableName
end;

function TRowStorageExcelEdit.GetFieldIdentifier(AField : TDataField) : string;
begin
  case fExcelTransferId of
    etiFieldName        : Result := AField.FieldName;
    etiFieldDescription : Result := AField.LongDescription;
//    else Result := ''; // no other possibility!
  end;
end;



procedure TRowStorageExcelEdit.InsertGridToExcel;
var
{$ifndef D7_OR_HIGHER}
  ARange : Range;
{$else}
  ARange : ExcelRange;
{$endif D7_OR_HIGHER}
  SheetName, ACellFormat : string;
  iField, iRow : integer;
  ARow : TAbstractRow;
  AField : TDataField;
begin
  // set the name of the sheet to something nice, the table name or description
  SheetName := Table.TableName;
  if SheetName='' then
    SheetName := Table.Description;
  if SheetName<>'' then
  try
    fExcel.WorkSheetName[ fExcel.ActiveWorksheetIndex ] := SheetName;
  except
    // this will fail if the same sheet name already existed
  end;

  fExportFields := TFieldList.Create;
  GetExportFields( fExportFields );

  // preparations; headers, cellformat
  for iField := 0 to fExportFields.Count-1 do
    with fExcel do
    begin
      AField := fExportFields[iField];
      CellString[iField, 0] := GetFieldIdentifier( AField );
      CellBold[iField, 0] := True;

      ACellFormat := '';
      ARange := ExcelApplication.Range[CellName(iField,0),CellName(iField, TotalRowCount)];

      try
        if AField.DataType is TStringType then
          ACellFormat := '@'
        else if AField.DataType.IsNumeric then
        begin
          if AField.DataType is TPercentType then
            ACellFormat := '0' + SysUtils.DecimalSeparator +  '0 %'
          else if (AField.DataType is TCurrencyType) or
                  (AField.DataType is TDoubleType) then
            ACellFormat := '0' + SysUtils.DecimalSeparator +  '00';
          ARange.HorizontalAlignment := OleVariant(xlRight);
        end;

        ARange.NumberFormat := ACellFormat;
      except
      end;
    end;

  // insert the data
  for iRow := 0 to TotalRowCount-1 do
  begin
    ARow := GetExportDataRow( iRow );
    for iField := 0 to fExportFields.Count-1 do
      fExcel.CellValue[iField, iRow+1] := ARow[ fExportFields[iField]];

    fExcelUI.SetExportProgress( iRow+1 );
  end;

  // cell widths when ready
  with fExcel do
  begin
    try
      ExcelApplication.Range[ CellName(0,0),CellName(fExportFields.Count-1, TotalRowCount) ].Columns.AutoFit;
      ExcelApplication.Caption := GetExportCaption;
    except
      on E:Exception do
        CustomMessageDlg( GetString('ANERROR') +', "' + E.ClassName + ':' + E.Message + '".', mtError, [mbOK], 0);
    end;

  end;

end;







function TRowStorageExcelEdit.GetAllowedImportRules: TExcelImportRuleSet;
begin
  Result := fAllowedImportRules;
end;

procedure TRowStorageExcelEdit.SetAllowedImportRules(const Value: TExcelImportRuleSet);
begin
  fAllowedImportRules := Value;
end;



{ TAbstractGridEditorExcelEdit }

constructor TAbstractGridEditorExcelEdit.Create(ExcelUI : IExcelUI; Table : TDataTable; GridEditor:TGridEditor);
var
  ACursor : integer;
begin
  inherited Create( ExcelUI );

  fTable := Table;
  fGridEditor := GridEditor;

  // Disable sets cursor to HourGlass, so we have to reset it
  ACursor := Screen.Cursor;
  fGridEditor.Disable;
  Screen.Cursor := ACursor;

  AllowImport := not fGridEditor.ReadOnly;
  fRows := TDataRowList.Create;
  GetExportRows(fRows);
end;

procedure TAbstractGridEditorExcelEdit.GetExportRows(Rows : TDataRowList);
begin
  Rows.Clear;
  RowStorage.GetRows( fRows, nil, gaReference );
end;

destructor TAbstractGridEditorExcelEdit.Destroy;
begin
  inherited Destroy;

  fGridEditor.SortRows;
  fGridEditor.Enable;
  fRows.Free;
end;

procedure TAbstractGridEditorExcelEdit.GetExportFields(FieldList: TFieldList);
var
  iRowView, iField : integer;
  AField : TDataField;
begin
  for iRowView := 0 to fGridEditor.PageView.RowViewCount -1 do
    for iField := 0 to fGridEditor.PageView.RowView[iRowView].FieldCount -1 do
    begin
      AField := fGridEditor.PageView.RowView[iRowView].Field[iField];

      // don't insert duplicates
      if (FieldList.IndexOf( AField ) = -1) and DoExportField( AField ) then
         FieldList.Add( AField );
   end;
end;

function TAbstractGridEditorExcelEdit.GetRowStorage : TAbstractRowStorage;
begin
  Result := fGridEditor.RowStorage;
end;



function TAbstractGridEditorExcelEdit.GetTotalRowCount : integer;
begin
  result := Rows.Count;
end;

function TAbstractGridEditorExcelEdit.GetExportDataRow(iRow : integer) : TAbstractRow;
begin
  result := Rows.AbstractRows[iRow];
end;







{ TGridEditorExcelEdit }

procedure TGridEditorExcelEdit.GetExportFields( FieldList : TFieldList );
begin
  FieldList.KeysToList( Table, False ); // insert non-fictive keys
  inherited GetExportFields( FieldList );
end;

function TGridEditorExcelEdit.DoExportField(AField : TDataField) : Boolean;
begin
  Result := (not AField.DefaultReadOnly) and Table.TableHasField( AField );
end;

{ TExactGridEditorExcelEdit }

constructor TExactGridEditorExcelEdit.Create(ExcelUI: IExcelUI; Table: TDataTable; GridEditor: TGridEditor);
var
  iField : integer;
begin


  inherited ;

  fHiddenKeys := TFieldList.Create;
  FillHiddenKeys(fHiddenKeys);
  fConsiderKeys := TFieldList.Create;
  for iField := 0 to GridEditor.RowStorage.DataTable.KeyCount-1 do
    fConsiderKeys.Add( GridEditor.RowStorage.DataTable.Field[iField] );
end;

destructor TExactGridEditorExcelEdit.Destroy;
begin
  inherited;

  fHiddenKeys.Free;
  fConsiderKeys.Free;
end;

function TExactGridEditorExcelEdit.DoExportField(AField: TDataField): Boolean;
begin
  Result := True;
end;






type
  TGridEditorLink = class(TGridEditor);
procedure TExactGridEditorExcelEdit.FillHiddenKeys(AKeyList : TFieldList);
begin
  TGridEditorLink(GridEditor).FillHiddenKeys(AKeyList);
end;

procedure TExactGridEditorExcelEdit.CopyHiddenKeys(DestRow: TDataRow);
begin
  TGridEditorLink(GridEditor).SetInitialValuesToRow(DestRow);
  TGridEditorLink(GridEditor).SetDefaultValuesToDataRow(DestRow);
end;



{ TMultiExcelEdit }

constructor TMultiExcelEdit.Create(ExcelUI: IExcelUI; const SubEdits: array of TExcelEdit);
var
  iExcelEdit : integer;
begin
  inherited Create( ExcelUI );

  // Copy the array of SubEdits, set AllowImport to false if at least one of
  // the SubEdits has AllowEdit=False
  fAllowImport := True;
  SetLength( fSubEdits, Length(SubEdits));
  for iExcelEdit := Low(SubEdits) to High(SubEdits) do
  begin
    fSubEdits[iExcelEdit] := SubEdits[iExcelEdit];
    fAllowImport := fAllowImport and SubEdits[iExcelEdit].AllowImport
  end;
end;

destructor TMultiExcelEdit.Destroy;
var
  iExcelEdit : integer;
begin
  for iExcelEdit := Low(fSubEdits) to High(fSubEdits) do
  begin
    fSubEdits[iExcelEdit].fExcel := nil;
    fSubEdits[iExcelEdit].Free;
  end;
    
  inherited;
end;

function TMultiExcelEdit.CreateErrorForm: TfrmExcelErrors;
begin
  Result := fSubEdits[0].CreateErrorForm;
end;



function TMultiExcelEdit.GetExportCaption: string;
begin
  Result := fSubEdits[0].GetExportCaption;
end;

function TMultiExcelEdit.GetMainHeaderCaptionAppend: string;
begin
  Result := fSubEdits[0].GetMainHeaderCaptionAppend;
end;

function TMultiExcelEdit.GetTotalRowCount: integer;
var
  iExcelEdit : integer;
begin
  Result := 0;
  for iExcelEdit := Low(fSubEdits) to High(fSubEdits) do
    Result := Result + fSubEdits[iExcelEdit].TotalRowCount;
end;

procedure TMultiExcelEdit.InsertGridToExcel;
var
  iExcelEdit : integer;
begin
  for iExcelEdit := Low(fSubEdits) to High(fSubEdits) do
  begin
    fExcel.SetActiveSheet( iExcelEdit );
    fSubEdits[iExcelEdit].Excel := fExcel;
    fSubEdits[iExcelEdit].InsertGridToExcel;
  end;
  fExcel.SetActiveSheet( 0 );
end;

function TMultiExcelEdit.GetAllowedImportRules: TExcelImportRuleSet;
var
  iExcelEdit : integer;
begin
{  // Solution1: Return the largest subset of allowed rules
  Result := fSubEdits[0].AllowedImportRules;
  for iExcelEdit := Low(fSubEdits)+1 to High(fSubEdits) do
    Result := Result * fSubEdits[iExcelEdit].AllowedImportRules; }

  // Solution2: Return the sum of allowed rules
  Result := [];
  for iExcelEdit := Low(fSubEdits) to High(fSubEdits) do
    Result := Result + fSubEdits[iExcelEdit].AllowedImportRules; 
end;

procedure TMultiExcelEdit.SetAllowedImportRules(const Value: TExcelImportRuleSet);
var
  iExcelEdit : integer;
begin
  // Set the SubEdits to disallow these, in addition to whatever what they disallowed
  // previously.
  for iExcelEdit := Low(fSubEdits) to High(fSubEdits) do
    fSubEdits[iExcelEdit].AllowedImportRules := fSubEdits[iExcelEdit].AllowedImportRules * Value;
end;

{ TDefaultExcelTranslator }

constructor TDefaultExcelTranslator.Create;
const
  StringProperties : array [1..21] of TDoubleStringArray = (
    ('EXPORTERROR', 'Could not export, error'), ('ANERROR', 'An error occured'),
    ('NOEXCEL', 'You don''t seem to have a copy of Excel installed on your machine.'),
    ('VALUEADDTOROWERROR', 'Could not add values in Excel sheet for rows'),
    ('VALUEADDFROMROWERROR', 'An error occurred while copying values from Excel at row'),
    ('FORFIELD', 'for the field'), ('ERROR', 'with error'),
    ('IGNORECONTINUE', 'Ignore error and continue?'),
    ('IMPEXPTRANS', 'The option to import/export the translations to Excel is only available in the commercial version of TTranslator.'),
    ('MOREINFO', 'For more information about licensing refer to the webpage at'),
    ('INSERTROWERROR', 'Could not insert row'),
    ('EXCELEDITOF', 'Excel editing of'),
    ('KEYFIELD', 'The key field'), ('NOTINSHEET', 'is not present in the Excel sheet'),
    ('NOIMPORT', 'No importing can be done.'), ('FIELD', 'The field'),
    ('COLNUM', 'The column number'), ('INVALIDFIELD', 'has an invalid field name'),
    ('CANTIMPORTCOL', 'cannot import that column.'),
    ('TWICE', 'appears twice in the Excel sheet.'),
    ('NOTPRESENT', 'is not present in the destination.')
  );
begin
  inherited Create( StringProperties );
end;

initialization
  DefaultExcelTranslator := TDefaultExcelTranslator.Create;
finalization
  DefaultExcelTranslator.Free;


{$endif D5_OR_HIGHER}

end.


