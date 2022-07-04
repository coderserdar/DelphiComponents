unit SqlitePassCreateNewDatabaseDialog;
{$i ..\..\Sources\SqlitePassDbo.inc}

interface

uses
 {$IFDEF FPC}
  LResources,
 {$ELSE}
  Windows,
  Messages,
  FileCtrl,
 {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, SqlitePassConst, SqlitePassDbo;

type
  TSqlitePassCreateNewDatabaseDlg = class(TForm)
    PanelButtons: TPanel;
    ButtonOk: TButton;
    ButtonCancel: TButton;
    Label1: TLabel;
    CbCheckFileExt: TCheckBox;
    TEditDbFileFullPath: TEdit;
    LabelDbFileFullPath: TLabel;
    SbGetDirectory: TSpeedButton;
    CbDbEncoding: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    CbDbPageSize: TComboBox;
    CbDbKind: TComboBox;
    CbDbStorage: TComboBox;
    Label6: TLabel;
    Label7: TLabel;
    CbDbVacuumType: TComboBox;
    CbLegacyFileFormat: TCheckBox;
    Shape6: TShape;
    Shape1: TShape;
    Label2: TLabel;
    Label3: TLabel;
    Shape2: TShape;
    Shape3: TShape;
    Label8: TLabel;
    Label9: TLabel;
    TEditDbFileName: TEdit;
    procedure ButtonOkClick(Sender: TObject);
    procedure CbDbStorageClick(Sender: TObject);
    procedure SbGetDirectoryClick(Sender: TObject);
    procedure TEditDbFileFullPathChange(Sender: TObject);
    procedure CbDbKindChange(Sender: TObject);
    procedure CbCheckFileExtClick(Sender: TObject);
    procedure TEditDbFileNameExit(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
  private
    Db: TSqlitePassDatabase;
    DatabaseType: TSqlitePassDatabaseType;
    Procedure CheckFileExtension;
    function GetFileExt: String;
  public
    DatabaseName: String;
    constructor Create(AOwner: TComponent); reintroduce;
  end;

var
  SqlitePassCreateNewDatabaseDlg: TSqlitePassCreateNewDatabaseDlg;

implementation

{$IFNDEF FPC}
 {$R *.DFM}
{$ENDIF}

constructor TSqlitePassCreateNewDatabaseDlg.Create(AOwner: TComponent);
begin
 Inherited Create(AOwner);
 Db := TSqlitePassDatabase.Create(AOwner);
 DatabaseName := '';
 DatabaseType := dbtUnknown;
 CbDbStorage.ItemIndex := 0;
 CbDbKind.ItemIndex := 5;
 CbDbEncoding.ItemIndex := 0;
 CbDbPageSize.ItemIndex := 3;
 CbDbVacuumType.ItemIndex := 0;
 TEditDbFileName.Text := 'NewDatabase';
 CheckFileExtension;
 TEditDbFileFullPath.Text := GetCurrentDir;
 CbDbStorageClick(Self);
 CbDbKindChange(Self);
end;

procedure TSqlitePassCreateNewDatabaseDlg.ButtonOkClick(Sender: TObject);
var
Encoding: TSqlitePassEncoding;
AutoVacuum, PageSize: Integer;
begin

CheckFileExtension;

Case CbDbStorage.ItemIndex of
     0: DatabaseName := TEditDbFileFullPath.Text+SqlitePassPathSeparator+TEditDbFileName.Text;
     1: DatabaseName := '';
     2: DatabaseName := ':memory:';
     end;

Case CbDbEncoding.ItemIndex of
     0: Encoding := UTF8;
     1: Encoding := UTF16;
     2: Encoding := UTF16le;
     3: Encoding := UTF16be;
     end;

AutoVacuum := CbDbVacuumType.ItemIndex;
PageSize := StrToInt(CbDbPageSize.Text);
if FileExists(DatabaseName)
   then begin
        ShowMessage('The database : '+ DatabaseName +' already exists. Please change the database name and try again.');
        ModalResult := mrNone;
        end
   else try
        Db.CreateDatabase(DatabaseName, DatabaseType, Encoding, PageSize, TSqlitePassAutoVacuumType(AutoVacuum));
        Except
        ShowMessage('Unable to create '+ DatabaseName + '. Please check your database settings and try again.');
        ModalResult := mrNone;
        end;
end;


procedure TSqlitePassCreateNewDatabaseDlg.CbDbStorageClick(
  Sender: TObject);
var
IsStored: Boolean;
begin
IsStored := (CbDbStorage.ItemIndex = 0);
CbCheckFileExt.Enabled   := IsStored;
LabelDbFileFullPath.Enabled  := IsStored;
TEditDbFileFullPath.Enabled  := IsStored;
SbGetDirectory.Enabled   := IsStored;
end;

procedure TSqlitePassCreateNewDatabaseDlg.SbGetDirectoryClick(
  Sender: TObject);
var
ChosenDir: String;
begin
 if SelectDirectory('Select the database storage directory', 'C:\', ChosenDir)
    then TEditDbFileFullPath.Text := ChosenDir;
end;


procedure TSqlitePassCreateNewDatabaseDlg.TEditDbFileFullPathChange(
  Sender: TObject);
begin
 ButtonOk.Enabled :=  (TEditDbFileFullPath.Text <> '');
end;

function TSqlitePassCreateNewDatabaseDlg.GetFileExt: String;
begin
 Case CbDbKind.ItemIndex of
     0: begin
        DatabaseType := dbtUnknown;
        Result := 'db';
        end;
     1: begin
        DatabaseType := dbtKexi;
        Result := 'kexi';
        end;
     2: begin
        DatabaseType := dbtSQLiteAdmin;
        Result := 's3db';
        end;
     3: begin
        DatabaseType := dbtSQLiteExpert;
        Result := 'db3';
        end;
     4: begin
        DatabaseType := dbtSQLite4Fpc;
        Result := 's3fpc';
        end;
     5: begin
        DatabaseType := dbtSQLitePass;
        Result := 'sp3';
        end;
     end;
end;

procedure TSqlitePassCreateNewDatabaseDlg.CbDbKindChange(Sender: TObject);
begin
 CheckFileExtension;
end;

procedure TSqlitePassCreateNewDatabaseDlg.CheckFileExtension;
var
FileName, Ext: String;
begin
if CbCheckFileExt.Enabled and CbCheckFileExt.Checked then
   begin
   FileName := TEditDbFileName.Text;
   Ext := ExtractFileExt(FileName);
   System.Delete(FileName, Length(FileName)-Length(Ext)+1, Length(Ext));
   Ext := GetFileExt;
   if FileName[Length(FileName)] <> '.'
      then Ext := '.' + Ext;
   TEditDbFileName.Text := FileName + Ext;
   end;
end;

procedure TSqlitePassCreateNewDatabaseDlg.CbCheckFileExtClick(
  Sender: TObject);
begin
 CheckFileExtension;
end;

procedure TSqlitePassCreateNewDatabaseDlg.TEditDbFileNameExit(
  Sender: TObject);
begin
 CheckFileExtension;
end;

procedure TSqlitePassCreateNewDatabaseDlg.ButtonCancelClick(
  Sender: TObject);
begin
DatabaseName := '';
DatabaseType := dbtUnknown;
end;

initialization
 {$IFDEF FPC}
  {$I SqlitePassCreateNewDatabaseDialog.lrs}
 {$ENDIF}
end.
