unit AdrMain;

interface

uses
{$IFDEF WIN32}
  Windows,
{$ELSE}
  WinTypes, WinProcs,
{$ENDIF}
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB, DBTables, StdCtrls, Mask, DBCtrls, Menus, Buttons, FileCtrl, ExtCtrls,
  CompLHA, SegLHA;

type
  TMainForm = class(TForm)
    MainMenu: TMainMenu;
    MFile: TMenuItem;
    MBack: TMenuItem;
    MRest: TMenuItem;
    N1: TMenuItem;
    MExit: TMenuItem;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    EName: TDBEdit;
    EAddress: TDBEdit;
    ECity: TDBEdit;
    EPhone: TDBEdit;
    MEdit: TMenuItem;
    MNew: TMenuItem;
    MChange: TMenuItem;
    MDelete: TMenuItem;
    MSearch: TMenuItem;
    MFind: TMenuItem;
    MFirst: TMenuItem;
    MNext: TMenuItem;
    MPrev: TMenuItem;
    MLast: TMenuItem;
    NextBtn: TSpeedButton;
    PrevBtn: TSpeedButton;
    Label5: TLabel;
    EEMail: TDBEdit;
    CanBtn: TBitBtn;
    SaveBtn: TBitBtn;
    DataSource: TDataSource;
    AdrTable: TTable;
    AdrTableName: TStringField;
    AdrTableAddress: TStringField;
    AdrTableCity: TStringField;
    AdrTablePhone: TStringField;
    AdrTableEMail: TStringField;
    MData: TMenuItem;
    MPurge: TMenuItem;
    MsgPanel: TPanel;
    TheQuery: TQuery;
    SegLHA: TSegLHA;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PrevBtnClick(Sender: TObject);
    procedure NextBtnClick(Sender: TObject);
    procedure MExitClick(Sender: TObject);
    procedure MNewClick(Sender: TObject);
    procedure MChangeClick(Sender: TObject);
    procedure MDeleteClick(Sender: TObject);
    procedure CanBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure DoKeyPress(Sender: TObject; var Key: Char);
    procedure MPurgeClick(Sender: TObject);
    procedure MFirstClick(Sender: TObject);
    procedure MNextClick(Sender: TObject);
    procedure MPrevClick(Sender: TObject);
    procedure MLastClick(Sender: TObject);
    procedure MFindClick(Sender: TObject);
    procedure MBackClick(Sender: TObject);
    procedure MRestClick(Sender: TObject);
    {Use the following for Delphi 1.0}
    {procedure SegLHACheckFile(const originalFilePath: String;
      var newFilePath: OpenString; dateTime: Longint;
      mode: TCompLHAProcessMode);}
    procedure SegLHACheckFile(const originalFilePath: string;
      var newFilePath: string; dateTime: Longint;
      mode: TCompLHAProcessMode);
  private
    { Private declarations }
    procedure SetReadOnly( State: Boolean );
    procedure InitOptions( RC: Integer );
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}
var
   DataDir:      String;

{=============================================================}
{=============================================================}
procedure TMainForm.SetReadOnly( State: Boolean );
begin
     EName.ReadOnly := State;
     EAddress.ReadOnly := State;
     ECity.ReadOnly := State;
     EPhone.ReadOnly := State;
     EEMail.ReadOnly := State;
end;

{=============================================================}
{=============================================================}
procedure TMainForm.InitOptions( RC: Integer );
var
   State:     Boolean;
begin
     State := False;
     if (RC > 0) then
       State := True;
     MChange.Enabled := State;
     MDelete.Enabled := State;
     MSearch.Enabled := State;
     MData.Enabled := State;
     State := False;
     if (RC > 1) then
       State := True;
     PrevBtn.Enabled := State;
     NextBtn.Enabled := State;
     MFind.Enabled := State;
     MFirst.Enabled := State;
     MPrev.Enabled := State;
     MNext.Enabled := State;
     MLast.Enabled := State;
     ActiveControl := CanBtn;
end;

{=============================================================}
{=============================================================}
procedure TMainForm.FormCreate(Sender: TObject);
begin
     GetDir(0,DataDir);
     DataDir := ExtractFilePath(Application.ExeName);
     if (DataDir[Length(DataDir)] = '\') then
       DataDir := DataDir + 'Data'
     else
       DataDir := DataDir + '\Data';
     if (DirectoryExists(DataDir) = False) then
       MkDir(DataDir);
     if (FileExists(DataDir+'\Address.db') = False) then
       begin
       DataSource.DataSet := nil;
       TheQuery.DatabaseName := DataDir;
       TheQuery.ExecSQL;
       end;
     DataSource.DataSet := AdrTable;
     AdrTable.DatabaseName := DataDir;
     AdrTable.Open;
     AdrTable.First;
     InitOptions(AdrTable.RecordCount);
     SetReadOnly(True);
     MsgPanel.Caption := '';
     SaveBtn.Enabled := False;
end;

{=============================================================}
{=============================================================}
procedure TMainForm.FormDestroy(Sender: TObject);
begin
     if (AdrTable.Modified = True) then
       begin
       if (MessageDlg('Do you want to save changes to database?',
                       mtConfirmation,[mbYes,mbNo],0) = mrYes) then
         AdrTable.Post
       else
         AdrTable.Cancel;
       end;
     AdrTable.Close;
end;

{=============================================================}
{=============================================================}
procedure TMainForm.PrevBtnClick(Sender: TObject);
begin
     AdrTable.Prior;
end;

{=============================================================}
{=============================================================}
procedure TMainForm.NextBtnClick(Sender: TObject);
begin
     AdrTable.Next;
end;

{=============================================================}
{=============================================================}
procedure TMainForm.MExitClick(Sender: TObject);
begin
     Close;
end;

{=============================================================}
{=============================================================}
procedure TMainForm.MNewClick(Sender: TObject);
begin
     SetReadOnly(False);
     AdrTable.Insert;
     SaveBtn.Enabled := True;
     EName.SetFocus;
end;

{=============================================================}
{=============================================================}
procedure TMainForm.MChangeClick(Sender: TObject);
begin
     SetReadOnly(False);
     EName.ReadOnly := True;
     SaveBtn.Enabled := True;
     EAddress.SetFocus;
end;

{=============================================================}
{=============================================================}
procedure TMainForm.MDeleteClick(Sender: TObject);
begin
     if (MessageDlg('Do you really want to delete this entry?',mtConfirmation,
                        [mbYes,mbNo],0) = mrYes) then
       begin
       AdrTable.Delete;
       InitOptions(AdrTable.RecordCount);
       end;
     SetReadOnly(True);
     SaveBtn.Enabled := False;
     ActiveControl := CanBtn;
end;

{=============================================================}
{=============================================================}
procedure TMainForm.CanBtnClick(Sender: TObject);
begin
     AdrTable.Cancel;
     SetReadOnly(True);
     InitOptions(AdrTable.RecordCount);
     SaveBtn.Enabled := False;
end;

{=============================================================}
{=============================================================}
procedure TMainForm.SaveBtnClick(Sender: TObject);
begin
     AdrTable.Post;
     SetReadOnly(True);
     InitOptions(AdrTable.RecordCount);
     SaveBtn.Enabled := False;
end;

{=============================================================}
{=============================================================}
procedure TMainForm.MPurgeClick(Sender: TObject);
begin
     if (MessageDlg('Do you really want to delete all data?',mtConfirmation,
                        [mbYes,mbNo],0) = mrYes) then
       begin
       while (AdrTable.RecordCount > 0) do
         AdrTable.Delete;
       InitOptions(AdrTable.RecordCount);
       end;
     SetReadOnly(True);
     SaveBtn.Enabled := False;
     ActiveControl := CanBtn;
end;

{=============================================================}
{=============================================================}
procedure TMainForm.MFindClick(Sender: TObject);
var
  Tmp:     String;
begin
     Tmp := '';
     Tmp := InputBox('Find by Name','Name',Tmp);
     AdrTable.SetKey;
     AdrTable.FindNearest([Tmp]);
     ActiveControl := CanBtn;
end;

{=============================================================}
{=============================================================}
procedure TMainForm.MFirstClick(Sender: TObject);
begin
     AdrTable.First;
     ActiveControl := CanBtn;
end;

{=============================================================}
{=============================================================}
procedure TMainForm.MNextClick(Sender: TObject);
begin
     AdrTable.Next;
     ActiveControl := CanBtn;
end;

{=============================================================}
{=============================================================}
procedure TMainForm.MPrevClick(Sender: TObject);
begin
     AdrTable.Prior;
     ActiveControl := CanBtn;
end;

{=============================================================}
{=============================================================}
procedure TMainForm.MLastClick(Sender: TObject);
begin
     AdrTable.Last;
     ActiveControl := CanBtn;
end;

{=============================================================}
{ The following was added to support the backup/restore option}
{ using the TSegEasy component.                               }
{=============================================================}

{=============================================================}
{=============================================================}
procedure TMainForm.DoKeyPress(Sender: TObject; var Key: Char);
begin
     if (Key = #13) then
       begin
       SelectNext(Sender as tWinControl,True,True);
       Key := #0;
       end;
end;

{=============================================================}
{=============================================================}
procedure TMainForm.MBackClick(Sender: TObject);
var
   CDir:     String;
begin
     AdrTable.Close;
     MsgPanel.Caption := '';
     GetDir(0,CDir);
     with SegLHA do
       begin
       FilesToProcess.Clear;
       FilesToProcess.Add(DataDir + '\Address.db');
       FilesToProcess.Add(DataDir + '\Address.px');
       TargetPath := DataDir;
       try
         Compress;
       except
         ShowMessage('Backup failed!');
       end; {try}
       MsgPanel.Caption := ' Backup complete!';
       end;
     AdrTable.Open;
end;

{=============================================================}
{=============================================================}
procedure TMainForm.MRestClick(Sender: TObject);
begin
     AdrTable.Close;
     MsgPanel.Caption := '';
     with SegLHA do
       begin
       FilesToProcess.Clear;
       TargetPath := DataDir;
       try
         Expand;
       except
         ShowMessage('Restore failed!');
       end; {try}
       MsgPanel.Caption := ' Restore complete!';
       end;
     AdrTable.Open;
     AdrTable.First;
     InitOptions(AdrTable.RecordCount);
end;

{=============================================================}
{ Use the following for Delphi 1.0                            }
{=============================================================}
{procedure TMainForm.SegLHACheckFile(const originalFilePath: String;
  var newFilePath: OpenString; dateTime: Longint;
  mode: TCompLHAProcessMode);}

procedure TMainForm.SegLHACheckFile(const originalFilePath: string;
  var newFilePath: string; dateTime: Longint; mode: TCompLHAProcessMode);
begin
     MsgPanel.Caption := ' ' + originalFilePath;
     MsgPanel.Update;
end;

end.
