{*******************************************************}
{                                                       }
{     Delphi VCL Extensions (RX) demo program           }
{                                                       }
{     Copyright (c) 1996 AO ROSNO                       }
{     Copyright (c) 1997 Master-Bank                    }
{                                                       }
{*******************************************************}

unit DestTab;

interface

uses WinTypes, WinProcs, SysUtils, Classes, Graphics, Forms, Controls,
  StdCtrls, ExtCtrls, FileCtrl, Placemnt, Mask, ToolEdit, DB, DBTables,
  RXLookup, CurrEdit;

type
  TDestTableDlg = class(TForm)
    OkBtn: TButton;
    CancelBtn: TButton;
    FormStorage: TFormStorage;
    TypeBtn: TRadioGroup;
    TabnameEdit: TFilenameEdit;
    Label1: TLabel;
    RecordCountBox: TGroupBox;
    AllRecsBtn: TRadioButton;
    FirstRecsBtn: TRadioButton;
    Label2: TLabel;
    RecordCntEdit: TCurrencyEdit;
{$IFDEF WIN32}
    procedure TabnameEditAfterDialog(Sender: TObject; var Name: string;
      var Action: Boolean);
{$ELSE}
    procedure TabnameEditAfterDialog(Sender: TObject; var Name: OpenString;
      var Action: Boolean);
{$ENDIF}
    procedure TabnameEditChange(Sender: TObject);
    procedure TypeBtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure RecordCountBtnClick(Sender: TObject);
  private
    { Private declarations }
    function GetTableType: TTableType;
    procedure SetTableType(Value: TTableType);
    function GetTableName: string;
    procedure SetTableName(const Value: string);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
    property TableType: TTableType read GetTableType write SetTableType;
    property TableName: string read GetTableName write SetTableName;
  end;

function GetDestTable(var TabName: string; var TabType: TTableType;
  var RecordCount: Longint): Boolean;

implementation

{$B-}

uses Consts, Dialogs, VCLUtils, FileUtil, Main;

{$R *.DFM}

function GetDestTable(var TabName: string; var TabType: TTableType;
  var RecordCount: Longint): Boolean;
begin
  with TDestTableDlg.Create(Application) do
  try
    FormStorage.RestoreFormPlacement;
    if TabType <> ttDefault then TableType := TabType;
    if TabName <> '' then TableName := TabName;
    if RecordCount <> 0 then begin
      RecordCntEdit.AsInteger := RecordCount;
      FirstRecsBtn.Checked := True;
    end;
    Result := ShowModal = mrOk;
    if Result then begin
      TabName := TableName;
      TabType := TableType;
      RecordCount := 0;
      if FirstRecsBtn.Checked then
        RecordCount := RecordCntEdit.AsInteger;
    end;
  finally
    Free;
  end;
end;

function DefExtension(TableType: TTableType): string;
begin
  case TableType of
    ttParadox: Result := '.DB';
    ttDBase: Result := '.DBF';
    ttASCII: Result := '.TXT';
    else Result := '';
  end;
end;

{ TDestTableDlg }

procedure TDestTableDlg.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if Application.MainForm <> nil then
    Params.WndParent := Application.MainForm.Handle;
end;

function TDestTableDlg.GetTableType: TTableType;
begin
  Result := ttDefault;
  case TypeBtn.ItemIndex of
    0: Result := ttParadox;
    1: Result := ttDBase;
    2: Result := ttASCII;
  end;
end;

procedure TDestTableDlg.SetTableType(Value: TTableType);
begin
  case Value of
    ttParadox: TypeBtn.ItemIndex := 0;
    ttDBase: TypeBtn.ItemIndex := 1;
    ttASCII: TypeBtn.ItemIndex := 2;
    else TypeBtn.ItemIndex := -1;
  end;
end;

function TDestTableDlg.GetTableName: string;
begin
  Result := TabnameEdit.FileName;
end;

procedure TDestTableDlg.SetTableName(const Value: string);
begin
  if Value <> TabnameEdit.FileName then begin
{$IFDEF WIN32}
    TabnameEdit.FileName := NormalDir(TabnameEdit.InitialDir) +
      ChangeFileExt(Value, DefExtension(TableType));
{$ELSE}
    TabnameEdit.FileName := NormalDir(TabnameEdit.InitialDir) +
      ChangeFileExt(Copy(Value, 1, 8), DefExtension(TableType));
{$ENDIF}
  end;
end;

{$IFDEF WIN32}
procedure TDestTableDlg.TabnameEditAfterDialog(Sender: TObject;
  var Name: string; var Action: Boolean);
{$ELSE}
procedure TDestTableDlg.TabnameEditAfterDialog(Sender: TObject;
  var Name: OpenString; var Action: Boolean);
{$ENDIF}
begin
  if (CompareText(ExtractFileExt(Name), '.DB') = 0) then
    TypeBtn.ItemIndex := 0
  else if (CompareText(ExtractFileExt(Name), '.DBF') = 0) then
    TypeBtn.ItemIndex := 1
  else if (CompareText(ExtractFileExt(Name), '.TXT') = 0) then
    TypeBtn.ItemIndex := 2;
end;

procedure TDestTableDlg.TabnameEditChange(Sender: TObject);
begin
  OkBtn.Enabled := TabnameEdit.FileName <> '';
end;

procedure TDestTableDlg.TypeBtnClick(Sender: TObject);
begin
  if TabnameEdit.FileName <> '' then begin
    TabnameEdit.FileName := ChangeFileExt(TabnameEdit.FileName,
      DefExtension(TableType));
  end;
  TabnameEdit.FilterIndex := Integer(TableType);
end;

procedure TDestTableDlg.OkBtnClick(Sender: TObject);
begin
  if (not FileExists(TabnameEdit.FileName)) or
    (MessageDlg(Format('File %s already exists. Do you want to replace it?',
    [TabnameEdit.FileName]), mtWarning, [mbYes, mbNo], 0) = mrYes)
  then
    ModalResult := mrOk;
end;

procedure TDestTableDlg.RecordCountBtnClick(Sender: TObject);
begin
  RecordCntEdit.Enabled := FirstRecsBtn.Checked;
  if RecordCntEdit.Enabled then begin
    RecordCntEdit.Color := clWindow;
    RecordCntEdit.ParentFont := True;
    if TabnameEdit.Text <> '' then ActiveControl := RecordCntEdit
    else ActiveControl := TabnameEdit;
  end
  else begin
    RecordCntEdit.ParentColor := True;
    RecordCntEdit.Font.Color := RecordCntEdit.Color;
  end;
end;

end.
