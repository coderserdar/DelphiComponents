{*********************************************************}
{* Main file                                             *}
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

{Rewritten !!.11}

unit fmmain;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  DB,
  DBTables,
  StdCtrls,
  ExtCtrls,
  Buttons,
  Menus,
  ffclimex,
  ffllbase,
  fflldict,
  ffllprot,
  ffclintf,
  dgimpdo,
  ffdb,
  ffdbbase,
  ComCtrls;

type
  TfrmMain = class(TForm)
    tblSource: TTable;
    btnTransfer: TBitBtn;
    btnExit: TBitBtn;
    imgCheck: TImage;
    btnHelp: TBitBtn;
    mnuMain: TMainMenu;
    mnuOperations: TMenuItem;
    mnuHelp: TMenuItem;
    mnuHelpContents: TMenuItem;
    mnuAbout: TMenuItem;
    tblDest: TffTable;
    dbDest: TffDatabase;
    mnuExit: TMenuItem;
    N1: TMenuItem;
    mnuTransferActiveTable: TMenuItem;
    pgTransfer: TPageControl;
    tabSource: TTabSheet;
    tabOptions: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    lstBDETables: TListBox;
    lstBDEFields: TListBox;
    tabTarget: TTabSheet;
    Label3: TLabel;
    Label5: TLabel;
    edtFFTableName: TEdit;
    lstFFTables: TListBox;
    cmbBDEAliases: TComboBox;
    cmbFFAliases: TComboBox;
    grpStringHandling: TGroupBox;
    chkClearEmptyStrings: TCheckBox;
    chkEmptyStrings: TCheckBox;
    chkOEMAnsi: TCheckBox;
    chkUseANSIFields: TCheckBox;
    chkUseZeroTerminatedStrings: TCheckBox;
    grpMisc: TGroupBox;
    chkSchemaOnly: TCheckBox;
    chkUseSysToolsDates: TCheckBox;
    chkUseSysToolsTimes: TCheckBox;
    grpExistingData: TRadioGroup;
    procedure btnTransferClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure lstBDEFieldsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lstBDEFieldsDblClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure edtBDEAliasNameChange(Sender: TObject);
    procedure edtBDEAliasNameExit(Sender: TObject);
    procedure edtBDEAliasNameKeyPress(Sender: TObject; var Key: Char);
    procedure edtBDETableNameChange(Sender: TObject);
    procedure edtBDETableNameExit(Sender: TObject);
    procedure edtBDETableNameKeyPress(Sender: TObject; var Key: Char);
    procedure edtFFTableNameChange(Sender: TObject);
    procedure edtFFTableNameExit(Sender: TObject);
    procedure edtFFTableNameKeyPress(Sender: TObject; var Key: Char);
    procedure lstFFTablesDblClick(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure cmbBDEAliasesChange(Sender: TObject);
    procedure lstBDETablesClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cmbFFAliasesChange(Sender: TObject);
    procedure chkClearEmptyStringsClick(Sender: TObject);
    procedure chkEmptyStringsClick(Sender: TObject);
  protected
    BDETablesLoaded: Boolean;
    BDETableInited: Boolean;
    FFTablesLoaded: Boolean;
    FFTableInited: Boolean;
    Aborted: Boolean;
    IsSQLServer: Boolean;
    procedure ConvertTable(const BDETableName, FFTableName : TffTableName);
    procedure CreateNewTable(const BDETableName, FFTableName: TffTableName);
    procedure InitBDETable;
    function InitCommsEngine: Boolean;
    procedure InitFFTable;
    procedure LoadAliases;
    procedure LoadBDETables;
    procedure LoadFFTables;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.DFM}

uses
  FFAbout;

const
  FG_UNSELECTED = 0;
  FG_SELECTED = 1;
  FG_UNAVAILABLE = 2;

  csSQLServer = 'SQL Server';

procedure TfrmMain.CreateNewTable(const BDETableName, FFTableName: TffTableName);
var
  Dict: TffDataDictionary;
  I: Integer;
  IdxName: string;
  FFType: TffFieldType;
  FFSize: Longint;
  FFDecPl: Integer;
  FldArray: TffFieldList;
  IHelpers: TffFieldIHList;
  NFields: Integer;

  procedure ParseFieldNames(aFieldNames: TffShStr);
  var
    DoFieldNums: Boolean;
    FieldEntry: TffShStr;
    FieldNo: Integer;
  begin
    DoFieldNums := False; {!!.03 - Start}
    if aFieldNames[1] in ['0'..'9'] then begin
      FieldNo := 2;
      while True do begin
        if aFieldNames[FieldNo] = ';' then begin
          DoFieldNums := True;
          Break;
        end
        else if aFieldNames[FieldNo] in ['0'..'9'] then
          Inc(FieldNo)
        else begin
          DoFieldNums := False;
          Break;
        end;
      end;
    end; {!!.03 - End}
    NFields := 0;
    repeat
      FFShStrSplit(aFieldNames, ';', FieldEntry, aFieldNames);
      if DoFieldNums then
        FldArray[NFields] := StrToInt(FieldEntry) - 1
      else begin
        FieldNo := Dict.GetFieldFromName(FieldEntry);
        if FieldNo = -1 then
          raise Exception.Create('Invalid field in index');
        FldArray[NFields] := FieldNo;
      end;
      Inc(NFields);
      if aFieldNames <> '' then {!!.02}
        IHelpers[NFields] := ''; {!!.02}
    until aFieldNames = '';
  end;

  function DetermineBlockSize: LongInt;
  var
    FFType: TffFieldType;
    FFSize: Longint;
    FFDecPl: Integer;
    BlockSize: LongInt;
    i: Integer;
  begin
   { Build size from source table structure }
    with tblSource do begin
      {Management size}
      BlockSize := 32 + 1;
      { Get the fields }
      FieldDefs.Update;

      if lstBDETables.SelCount > 1 then begin
        for I := 0 to Pred(FieldDefs.Count) do begin
          with FieldDefs[I] do begin
            ConvertBDEDataType(DataType, Size, FFType, FFSize, FFDecPl);
            BlockSize := BlockSize + FFSize;
          end;  { if }
        end;
      end
      else begin
        { Calculate using only the fields selected in the fields list. }
        with lstBDEFields do
          for I := 0 to Items.Count - 1 do
            if (LongInt(Items.Objects[I]) and FG_SELECTED) <> 0 then
              with FieldDefs[I] do begin
                ConvertBDEDataType(DataType, Size, FFType, FFSize, FFDecPl);
                BlockSize := BlockSize + FFSize;
              end;  { if }
      end;  { if }
    end;  { with }
     { Determine the first multiple of 4096 larger then BlockSize }
    Result := (BlockSize div 4096 + 1) * 4096;
  end;

begin
  Dict := TffDataDictionary.Create(DetermineBlockSize);
  try

    { Initialize the FieldArray }
    for I := 0 to pred(ffcl_MaxIndexFlds) do begin
      FldArray[I] := 0;
      IHelpers[I] := '';
    end;

    { Build dictionary from source table structure }
    with tblSource do begin
      { Point to the source table. }
      TableName := BDETableName;
      ReadOnly := True;

      { Get the fields }
      FieldDefs.Update;

      { Obtain the field definitions. }
      if lstBDETables.SelCount > 1 then begin
        { Convert all fields. }
          for I := 0 to Pred(FieldDefs.Count) do begin
            with FieldDefs[I] do begin
              ConvertBDEDataType(DataType, Size, FFType, FFSize, FFDecPl);
              Dict.AddField(Name,
                '', { description }
                FFType,
                FFSize,
                FFDecPl,
                Required,
                nil);
            end;  { with }
          end;  { for }
      end
      else begin
        { Convert only the fields selected in the fields list. }
        with lstBDEFields do
          for I := 0 to Items.Count - 1 do
            if (LongInt(Items.Objects[I]) and FG_SELECTED) <> 0 then
              with FieldDefs[I] do begin
                ConvertBDEDataType(DataType, Size, FFType, FFSize, FFDecPl);
                Dict.AddField(Name,
                  '', { description }
                  FFType,
                  FFSize,
                  FFDecPl,
                  Required,
                  nil);
              end;  { with }
      end;  { if }

      { Obtain the indices. }
      IndexDefs.Update;
      for I := 0 to IndexDefs.Count - 1 do begin
        with IndexDefs[I] do {!!.10}
          if not (ixExpression in Options) then begin {!!.10}
            ParseFieldNames(Fields);
            IdxName := Name;
            if IdxName = '' then
              if ixPrimary in Options then
                IdxName := 'FF$PRIMARY'
              else
                IdxName := 'FF$INDEX' + IntToStr(I + 1);
            Dict.AddIndex(IdxName, { index name }
              '', { description }
              0, { file no }
              NFields, { field count }
              FldArray, { field list }
              IHelpers, { index helper list }
              not (ixUnique in Options), { allow dups }
              not (ixDescending in Options), { ascending }
              ixCaseInsensitive in Options); { case insensitive }
          end; { if } {!!.10}
      end;

      { Create the actual table }
      Check(dbDest.CreateTable(False, FFTableName, Dict))
    end;
  finally
    Dict.Free;
  end;
end;

procedure TfrmMain.InitBDETable;
var
  I: Integer;
  Flag: LongInt;
begin
  if lstBDETables.SelCount > 1 then begin
    lstBDEFields.Clear;
    lstBDEFields.Items.Add('<All fields will be converted for each table>');
    lstBDEFields.Enabled := False;
    lstBDEFields.Color := clBtnFace;
  end
  else begin
    lstBDEFields.Color := clWindow;
    lstBDEFields.Enabled := True;
    with tblSource do begin
      DatabaseName := cmbBDEAliases.Text;
      { Find the selected table. }
      for I := 0 to Pred(lstBDETables.Items.Count) do
        if lstBDETables.Selected[I] then begin
          TableName := lstBDETables.Items[I];
          break;
        end;  { if }
      FieldDefs.Update;
      lstBDEFields.Clear;
      for I := 0 to FieldDefs.Count - 1 do begin
        Flag := FG_SELECTED;
        lstBDEFields.Items.AddObject(FieldDefs[I].Name, Pointer(Flag));
      end;  { for }
    end;  { with }
  end;
  BDETableInited := True;
end;

function TfrmMain.InitCommsEngine: Boolean;
begin
  cmbBDEAliases.Clear;
  cmbFFAliases.Clear;
  Result := True;
  try
    FFDB.Session.Open;
    LoadAliases;
  except
    on E: Exception do begin
      MessageDlg(E.Message, mtError, [mbOk], 0);
      Result := False;
    end;
  end;
end;

procedure TfrmMain.InitFFTable;
begin
  with tblDest do begin
    if Active then Close;
    TableName := edtFFTableName.Text;
  end;
  FFTableInited := True;
end;

procedure TfrmMain.LoadAliases;
var
  Aliases: TStringList;
  I: Integer;
begin
  { Segregate the FlashFiler and native BDE aliases }
  Aliases := TStringList.Create;
  try
    DBTables.Session.GetAliasNames(Aliases);
    with Aliases do begin
      for I := 0 to Count - 1 do
        cmbBDEAliases.Items.Add(Strings[I]);
      cmbBDEAliases.ItemIndex := 0;
      LoadBDETables;
    end;
    Aliases.Clear;
    FFDB.Session.GetAliasNames(Aliases);
    with Aliases do begin
      for I := 0 to Count - 1 do
        cmbFFAliases.Items.Add(Strings[I]);
      cmbFFAliases.ItemIndex := -1;
    end;
  finally
    Aliases.Free;
  end;
end;

procedure TfrmMain.LoadBDETables;
begin
  if cmbBDEAliases.Text <> '' then begin
    try                                                                  {!!.13}
      DBTables.Session.GetTableNames(cmbBDEAliases.Text, '', True, False,
                                     lstBDETables.Items);
    except                                                               {!!.13}
      { ignore all bde exceptions }                                      {!!.13}
    end;                                                                 {!!.13}
    BDETablesLoaded := True;
  end;
end;

procedure TfrmMain.LoadFFTables;
var
  FFTables: TStringList;
  I: Integer;
  TableName: string;
begin
  if cmbFFAliases.Text <> '' then begin
    dbDest.Connected := False;
    dbDest.AliasName := cmbFFAliases.Text;
    dbDest.DatabaseName := 'FF2_' + cmbFFAliases.Text;
    dbDest.Connected := True;

    lstFFTables.Clear;
    FFTables := TStringList.Create;
    try
      FFDB.Session.GetTableNames(cmbFFAliases.Text, '', True, False, FFTables);
      with FFTables do
        for I := 0 to Count - 1 do begin
          TableName := Copy(Strings[I], 1, Pos('.', Strings[I]) - 1);
          lstFFTables.Items.Add(TableName);
        end;
    finally
      FFTables.Free;
    end;
    FFTablesLoaded := True;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  IsSQLServer := False;
  if FileExists(ExtractFilePath(ParamStr(0)) + 'BDE2FF.HLP') then
    Application.HelpFile := ExtractFilePath(ParamStr(0)) + 'BDE2FF.HLP'
  else
    Application.HelpFile := ExtractFilePath(ParamStr(0)) + '..\DOC\BDE2FF.HLP';
  InitCommsEngine;
end;

procedure TfrmMain.lstBDEFieldsDblClick(Sender: TObject);
begin
  with (Sender as TListBox) do
    if (LongInt(Items.Objects[ItemIndex]) and FG_UNAVAILABLE) <> 0 then
      MessageBeep(0)
    else begin
      Items.Objects[ItemIndex] := Pointer((LongInt(Items.Objects[ItemIndex]) + 1) mod 2);
      Invalidate;
    end;
end;

procedure TfrmMain.lstBDEFieldsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  with (Control as TListBox) do begin
    with Canvas do begin
      Font.Assign(Font);

      if (odSelected) in State then begin
        Font.Color := clWindowText;
        Brush.Color := (Control as TListBox).Color;
      end;

      FillRect(Rect);

      if (LongInt(Items.Objects[Index]) and FG_SELECTED) <> 0 then
        with imgCheck.Picture.Bitmap do
          BrushCopy(Bounds(Rect.Left + 2, Rect.Top + 4, Width, Height),
            imgCheck.Picture.Bitmap, Bounds(0, 0, Width, Height),
            TransparentColor);

      if (LongInt(Items.Objects[Index]) and FG_UNAVAILABLE) <> 0 then
        Font.Color := clRed;

      { Draw the item text }
      TextOut(Rect.Left + imgCheck.Picture.Bitmap.Width + 4, Rect.Top, Items[Index]);
    end;
  end;
end;

procedure TfrmMain.ConvertTable(const BDETableName, FFTableName : TffTableName);
var
  I: Integer;
  Msg,
  BDETableNameFinal : string;
  NewTable,
  MultTables : Boolean;
  NumTransferred: LongInt;
  SourceFields: TStringList;
  ZMsg: array[0..255] of Char;
begin

  MultTables := (lstBDETables.SelCount > 1);

  { Init vars }
  Aborted := False;
  NewTable := False;
  NumTransferred := 0;
  tblDest.TableName := FFTableName;


  { If the user selected a table in a SQL Server database then strip the
    leading database name from the table name. }
  BDETableNameFinal := BDETableName;
  if IsSQLServer and (Pos('.', BDETableNameFinal) > 0) then begin
    I := 1;
    while BDETableNameFinal[I] <> '.' do
      inc(I);
    Delete(BDETableNameFinal, 1, I);
  end;  { if }
  tblSource.TableName := BDETableNameFinal;
  tblSource.FieldDefs.Update;

  try
    { Check for schema only import }
    if chkSchemaOnly.Checked then begin
      if (not tblDest.Exists) then begin
        Msg := 'Create new table ' + FFTableName + ' from schema only?';
        NewTable := True;
      end
      else
        Msg := 'Replace table ' + FFTableName + ' from schema only?';

      { If multiple tables being converted or user approves, recreate the
        table. }
      if MultTables or
         (MessageDlg(Msg, mtConfirmation, [mbYes, mbNo], 0) = mrYes) then begin
        if not NewTable then
          tblDest.DeleteTable;
        CreateNewTable(BDETableName, FFTableName);
      end
      else
        Aborted := True;
    end
    else begin
      { Data only or data & schema. }
      case grpExistingData.ItemIndex of
        0 : { Keep existing structure & data }
          if not tblDest.Exists then begin
            if MultTables or
             (MessageDlg('Create new table ' + edtFFTableName.Text + '?',
                         mtConfirmation, [mbYes, mbNo], 0) = mrYes) then begin
              CreateNewTable(BDETableName, FFTableName);
              NewTable := True;
            end;  { if }
          end;
        1 : { Keep existing structure, replace data }
          if tblDest.Exists then
            { Empty the table. }
            tblDest.EmptyTable
          else begin
            CreateNewTable(BDETableName, FFTableName);
            NewTable := True;
          end;
        2 : { Replace structure & data }
          if MultTables or
             (not tblDest.Exists) or
             (MessageDlg('Replace table ' + edtFFTableName.Text + '?',
                         mtConfirmation, [mbYes, mbNo], 0) = mrYes) then begin
            if tblDest.Exists then
              tblDest.DeleteTable;
            CreateNewTable(BDETableName, FFTableName);
            NewTable := True;
          end
          else
            Exit;
      end;  { case }

      { Begin the transfer process }
      Self.Enabled := False;
      try
        try
          SourceFields := TStringList.Create;
          try

            { If more than one table has been selected then convert all
              fields otherwise convert only those selected in the fields list. }
            if (lstBDETables.SelCount > 1) then begin
              for I := 0 to Pred(tblSource.FieldDefs.Count) do
                SourceFields.Add(ANSIUppercase(tblSource.fieldDefs[I].Name));
            end
            else begin
              with lstBDEFields do
                for I := 0 to Items.Count - 1 do
                  if (LongInt(Items.Objects[I]) and FG_SELECTED) <> 0 then
                    SourceFields.Add(ANSIUppercase(Items[I]));
            end;  { if }

            Aborted := not DoImport(tblSource, SourceFields,
                                    tblDest, 100, NumTransferred);
          finally
            SourceFields.Free;
          end;
        except
          Aborted := True;
          raise;
        end;

      finally
        { If we've aborted and we created a new table, get rid of it }
        if Aborted then begin
          if NewTable then begin
            tblDest.DeleteTable;
            NewTable := False;
          end;
        end;

        Application.ProcessMessages;
        Self.Enabled := True;
      end;
    end;
  finally
  end;

  if not Aborted then begin
    if NewTable then LoadFFTables;
    MessageBeep(0);
    StrPCopy(ZMsg, 'Transfer Completed.            ' + #13#13 +
      Format('%d records transferred.', [NumTransferred]));
    if lstBDETables.SelCount = 1 then
      Application.MessageBox(ZMsg, 'BDE Transfer to FlashFiler',
        MB_ICONINFORMATION or MB_OK);
  end;
  if not Aborted then ModalResult := mrOK;
end;

procedure TfrmMain.btnTransferClick(Sender: TObject);
var
  FFTableName : TffTableName;
  Inx : Integer;
begin

  { Check Requirements }
  if (lstBDETables.SelCount = 0) then begin
    ShowMessage('Please select one or more BDE tables for conversion.');
    Exit;
  end;

  if cmbFFAliases.ItemIndex = -1 then begin
    ShowMessage('Please specify a target FlashFiler database.');
    Exit;
  end;

  if (lstBDETables.SelCount = 1) and (edtFFTableName.Text = '') then begin
    ShowMessage('Please specify a destination FlashFiler table.');
    Exit;
  end;

  if tblDest.Active then
    tblDest.Close;

  tblDest.DatabaseName := 'FF2_' + cmbFFAliases.Text;

  for Inx := 0 to Pred(lstBDETables.Items.Count) do begin
    if lstBDETables.Selected[Inx] then begin
      if lstBDETables.SelCount > 1 then
        FFTableName := ChangeFileExt(lstBDETables.Items[Inx], '')
      else
        FFTableName := edtFFTableName.Text;
      ConvertTable(lstBDETables.Items[Inx], FFTableName)
    end;
  end;  { for }

end;

procedure TfrmMain.btnExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.btnHelpClick(Sender: TObject);
begin
  Application.HelpCommand(HELP_CONTENTS, 0);
end;

procedure TfrmMain.edtBDEAliasNameChange(Sender: TObject);
begin
  BDETablesLoaded := False;
  BDETableInited := False;
end;

procedure TfrmMain.edtBDEAliasNameExit(Sender: TObject);
begin
  if not BDETablesLoaded then LoadBDETables;
end;

procedure TfrmMain.edtBDEAliasNameKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then begin
    if not BDETablesLoaded then
      LoadBDETables;
    Key := #0;
  end;
end;

procedure TfrmMain.edtBDETableNameChange(Sender: TObject);
begin
  BDETableInited := False;
end;

procedure TfrmMain.edtBDETableNameExit(Sender: TObject);
begin
  if not BDETableInited then InitBDETable;
end;

procedure TfrmMain.edtBDETableNameKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then begin
    if not BDETableInited then InitBDETable;
    Key := #0;
  end;
end;

procedure TfrmMain.edtFFTableNameChange(Sender: TObject);
begin
  FFTableInited := False;
end;

procedure TfrmMain.edtFFTableNameExit(Sender: TObject);
begin
  if not FFTableInited then InitFFTable;
end;

procedure TfrmMain.edtFFTableNameKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then begin
    if not FFTableInited then InitFFTable;
    Key := #0;
  end;
end;

procedure TfrmMain.lstFFTablesDblClick(Sender: TObject);
begin
  with lstFFTables do
    if ItemIndex <> -1 then begin
      edtFFTableName.Text := Items[ItemIndex];
      InitFFTable;
    end;
end;

procedure TfrmMain.mnuAboutClick(Sender: TObject);
var
  AboutBox: TFFAboutBox;
begin
  AboutBox := TFFAboutBox.Create(Application);
  try
    AboutBox.Caption := 'About FlashFiler Utility';
    AboutBox.ProgramName.Caption := 'FlashFiler BDE2FF Converter';
    AboutBox.ShowModal;
  finally
    AboutBox.Free;
  end;
end;

procedure TfrmMain.cmbBDEAliasesChange(Sender: TObject);
begin
  IsSQLServer := (DBTables.Session.GetAliasDriverName(cmbBDEAliases.Text) = csSQLServer);
  LoadBDETables;
end;

procedure TfrmMain.lstBDETablesClick(Sender: TObject);
var
  Inx : Integer;
begin
  InitBDETable;
  InitFFTable;
  if (lstBDETables.SelCount = 1) then begin
    for Inx := 0 to Pred(lstBDETables.Items.Count) do
      if lstBDETables.Selected[Inx] then begin
        edtFFTableName.Text := ChangeFileExt(lstBDETables.Items[Inx], '');
        Break;
      end;
  end;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  pgTransfer.ActivePage := tabSource;
end;

procedure TfrmMain.cmbFFAliasesChange(Sender: TObject);
begin
  FFTablesLoaded := False;
  FFTableInited := False;
  LoadFFTables;
end;

procedure TfrmMain.chkClearEmptyStringsClick(Sender: TObject);
begin
  chkEmptyStrings.Checked := not chkClearEmptyStrings.Checked;
end;

procedure TfrmMain.chkEmptyStringsClick(Sender: TObject);
begin
  chkClearEmptyStrings.Checked := not chkEmptyStrings.Checked;
end;

end.

