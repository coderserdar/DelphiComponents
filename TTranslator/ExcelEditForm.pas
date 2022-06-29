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

{ $Id: ExcelEditForm.pas,v 1.8 2002/10/24 11:50:25 laa Exp $ }

{-----------------------------------------------------------------------------
  ExcelEdit       Form for interaction with ExcelEdit classes

  What:           TfrmExcelEdit

  Company:        Polycon Ab
  Authors:        LAA, MVJ
-----------------------------------------------------------------------------}

unit ExcelEditForm;
{$i common.inc}

interface

{$ifdef D5_OR_HIGHER}
uses

  Classes, Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ExcelEdit,
  Buttons;

type
  {/** Abstract base class for all Excel editing. */}
  TfrmExcelEdit = class(TForm, IExcelUI)
    PanelBottom: TPanel;
    PanelMain: TPanel;
    lblCounter: TLabel;
    lblImportFromExcel: TLabel;
    lblExportToExcel2: TLabel;
    lblStepOneA: TLabel;
    lblStepOneB: TLabel;
    lblStepTwo: TLabel;
    lblStepThree: TLabel;
    lblMainHeader: TLabel;
    lblExportToExcel1: TLabel;
    lblProgressCaption: TLabel;
    lblExportToExcel3: TLabel;
    btnExport: TButton;
    btnImport: TButton;
    RadioGroupImportRules: TRadioGroup;
    lblCancel: TLabel;
    btnCancel: TButton;
    lblImportFromFile: TLabel;
    opendialogExcelFile: TOpenDialog;
    btnChooseFile: TSpeedButton;
    txtExcelFile: TEdit;
    procedure btnExportClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnImportClick(Sender: TObject);
    procedure btnChooseFileClick(Sender: TObject);
    procedure OnExcelFileChange(Sender: TObject);
  private
    { Private declarations }
    fExcelEdit : TExcelEdit;
    fIsModal : Boolean;
    fFileListRegKey: string;


    procedure ForceClose;
    procedure SetExportEnabled(Value : Boolean);
    procedure SetImportEnabled(Value : Boolean);
    procedure SetExportProgress(RowCount : integer);
    procedure SetImportProgress(RowCount : integer);
    procedure SetPrepareProgress(RowCount : integer);
    function GetImportRule : TExcelImportRule;
    procedure SetImportReady(Imported: Boolean);
    procedure SetImportFileEnabled(Value: Boolean);
    function GetImportFileName: string;
  protected
    function CustomMessageDlg(const Msg: string; DlgType: TMsgDlgType;
      Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer; virtual;
    function GetString(const AProperty : String) : String; virtual;

    property ExcelEdit : TExcelEdit read fExcelEdit;
  public
    constructor Create( AOwner : TComponent ); override;
    function ShowMe(ExcelEdit : TExcelEdit) : Boolean;

    property IsModal : Boolean read fIsModal write fIsModal;
    property FileListRegKey : string read fFileListRegKey write fFileListRegKey;
  end;

{$endif D5_OR_HIGHER}

implementation

{$R *.DFM}

uses
  SysUtils;

{$ifdef D5_OR_HIGHER}

{ TfrmExcelEdit }

constructor TfrmExcelEdit.Create(AOwner: TComponent);
begin
  inherited Create( AOwner );

  fIsModal := False;
  FileListRegKey := '';

end;

function TfrmExcelEdit.ShowMe(ExcelEdit : TExcelEdit) : Boolean;
begin
  fExcelEdit := ExcelEdit;
  lblMainHeader.Caption := lblMainHeader.Caption + fExcelEdit.GetMainHeaderCaptionAppend;



  if IsModal and (Owner is TWinControl) then
    Result := (ShowModal = mrOK)
  else
  begin
    Parent := TWinControl(Owner);
    BorderIcons := [];
    BorderStyle := bsNone;
    Align := alClient;

    Visible := True;
    Result := True;
  end;
end;

procedure TfrmExcelEdit.btnExportClick(Sender: TObject);
begin
  try
    lblCounter.Caption := GetString('EXCELSTART');
    lblCounter.Refresh;
    fExcelEdit.DoExport;
  except
    on E:Exception do
      CustomMessageDlg( GetString('EXPORTERROR') +' :"' + E.Classname + ':' + E.Message + '".', mtError, [mbOK], 0);
  end;
end;

procedure TfrmExcelEdit.btnCancelClick(Sender: TObject);
begin
  ForceClose;
end;

procedure TfrmExcelEdit.ForceClose;
begin
  fExcelEdit.DoCancel;

  if fIsModal then
    Self.ModalResult := mrCancel
  else
    Self.Close;
end;

function TfrmExcelEdit.GetImportFileName : string;
begin

    Result := txtExcelFile.Text

end;

procedure TfrmExcelEdit.btnImportClick(Sender: TObject);
var
  Imported : Boolean;
  AFileName : string;
begin
  lblCounter.Caption := GetString('PREPAREIMPORT');
  lblCounter.Visible := True;
  lblCounter.Refresh;

  AFileName := GetImportFileName;
  if AFileName<>'' then
  begin
    if FileExists( AFileName ) then
    begin

      Imported := fExcelEdit.DoImportFromFile( AFileName );
      SetImportReady( Imported );
    end
    else
      CustomMessageDlg( GetString('FILENOTEXIST'), mtError, [mbOK], 0);
  end
  else
  begin
    Imported := fExcelEdit.DoImport;
    if Imported then
      SetImportReady( Imported );
  end;
end;

procedure TfrmExcelEdit.SetImportReady(Imported : Boolean);
begin
  if fIsModal then
  begin

    if Imported then
      Self.ModalResult := mrOK
    else
      Self.ModalResult := mrAbort;

  end
  else
    Self.Close;
end;

procedure TfrmExcelEdit.SetExportEnabled(Value : Boolean);
begin
  btnExport.Enabled := Value;
  lblExportToExcel1.Enabled := Value;
//  lblExportToExcel2.Enabled := Value;
//  lblExportToExcel3.Enabled := Value;
end;

procedure TfrmExcelEdit.SetImportEnabled(Value : Boolean);
begin
  btnImport.Enabled := Value;
  lblImportFromExcel.Enabled := Value;
end;

procedure TfrmExcelEdit.SetImportFileEnabled(Value : Boolean);
begin
  lblImportFromFile.Enabled := Value;
  txtExcelFile.Enabled := Value;

  btnChooseFile.Enabled := Value;
end;

procedure TfrmExcelEdit.SetExportProgress(RowCount : integer);
begin
  lblCounter.Caption := IntToStr(RowCount) + ' ' + GetString('ROWSEXPORTED');
  lblCounter.Refresh;
end;

procedure TfrmExcelEdit.SetImportProgress(RowCount : integer);
begin
  lblCounter.Caption := inttostr(RowCount) + ' ' + GetString('ROWSIMPORTED');
  lblCounter.Refresh;
end;

procedure TfrmExcelEdit.SetPrepareProgress(RowCount : integer);
begin
  lblCounter.Caption := GetString('PREPAREIMPORT') + ', ' + GetString('ROW') + ' ' + IntToStr(RowCount);
  lblCounter.Refresh;
  if (RowCount mod 20) = 0 then
    Refresh;
end;

function TfrmExcelEdit.GetImportRule : TExcelImportRule;
begin
  case RadioGroupImportRules.ItemIndex of
   0 : result := eirReplace;
   1 : result := eirAdd;
   2 : result := eirDelete;
   else raise Exception.Create('Unknown import rule');
  end;
end;

function TfrmExcelEdit.CustomMessageDlg(const Msg: string;
  DlgType: TMsgDlgType; Buttons: TMsgDlgButtons;
  HelpCtx: Integer): Integer;
begin
  Result := MessageDlg(Msg, DlgType, Buttons, HelpCtx);
end;

function TfrmExcelEdit.GetString(const AProperty: String): String;
const
  StringProperties : array [1..7, 1..2] of string = (
    ('EXCELSTART', 'Starting Excel'), ('EXPORTERROR', 'Could not export, error'),
    ('PREPAREIMPORT', 'Preparing import'), ('ROWSEXPORTED', 'rows exported'),
    ('ROWSIMPORTED', 'rows imported'), ('ROW', 'row'),
    ('FILENOTEXIST', 'The selected Excel file does not exist.')
  );
var
  iProperty : integer;
begin
  result := AProperty;
  for iProperty := Low(StringProperties) to High(StringProperties) do
    if StringProperties[iProperty,1] = AProperty then
    begin
      result := StringProperties[iProperty,2];
      break;
    end;

end;

procedure TfrmExcelEdit.btnChooseFileClick(Sender: TObject);
begin
  if opendialogExcelFile.Execute then
  begin

    txtExcelFile.Text := opendialogExcelFile.FileName;
  end;
end;

procedure TfrmExcelEdit.OnExcelFileChange(Sender: TObject);
var
  AFileName : string;
begin
  AFileName := GetImportFileName;

  btnExport.Enabled := (AFileName='');
  lblExportToExcel1.Enabled := (AFileName='');
  lblExportToExcel2.Enabled := (AFileName='');
  lblExportToExcel3.Enabled := (AFileName='');
  btnImport.Enabled := (AFileName<>'');
end;


{$endif D5_OR_HIGHER}

end.

