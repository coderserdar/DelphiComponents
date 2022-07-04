unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, FileCtrl, CopyFile, MyBackup, Buttons, MyArchBackup;

type
  TForm1 = class(TForm)
    MyBackup1: TMyBackup;
    TabControl1: TTabControl;
    lblBackupName: TLabel;
    Label6: TLabel;
    Label1: TLabel;
    edBackupName: TEdit;
    edBackupPath: TEdit;
    edDataPath: TEdit;
    btnStart: TButton;
    btnClose: TButton;
    SpeedButton1: TSpeedButton;
    Button1: TButton;
    Label3: TLabel;
    lbComment: TLabel;
    procedure TabControl1Change(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    procedure edDataPathEnter(Sender: TObject);
    procedure edDataPathExit(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure DoBackup;
    procedure DoRestore;
  end;

var
  Form1: TForm1;

implementation

uses fmBrowseDialog, fmSelectFiles;

{$R *.DFM}

procedure TForm1.TabControl1Change(Sender: TObject);
begin
  if TabControl1.TabIndex = 0 then
  begin
    lblBackupName.Visible := true;
    edBackupName.Visible := true;
    lbComment.Caption := 'Select a data path OR a list of files to backup.';
  end
  else
  begin
    lblBackupName.Visible := false;
    edBackupName.Visible := false;
    lbComment.Caption := 'Select a data path where the files will be restored.';
  end;
end;

procedure TForm1.btnStartClick(Sender: TObject);
begin
  if MyBackup1.FilesToBackup.Count = 0 then
  begin
    if not DirectoryExists(edDataPath.Text) then
    begin
      MessageDlg('Data path not exists!', mtError, [mbOk], 0);
      edDataPath.SetFocus;
      Exit;
    end;
    if not DirectoryExists(edBackupPath.Text) then
    begin
      MessageDlg('Backup path not exists!', mtError, [mbOk], 0);
      edBackupPath.SetFocus;
      Exit;
    end;
  end;
  if TabControl1.TabIndex = 0 then
    DoBackup
  else
    DoRestore;
end;

procedure TForm1.DoBackup;
begin
  Screen.Cursor := crHourGlass;
  Enabled := false;
  with MyBackup1 do
  try
      BackupName := edBackupName.Text;
      DrivePath := edBackupPath.Text;
      FilesPath := edDataPath.Text;
      Backup;
      ShowMessage( 'Backup successfull !' );
  finally
    Enabled := true;
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.DoRestore;
var
  BkName: string;
  SL: TStringList;
begin
  Screen.Cursor := crHourGlass;
  Enabled := false;
  with MyBackup1 do
  try
    DrivePath := edBackupPath.Text;
    FilesPath := edDataPath.Text;
    // get the backup name.
    SL := TStringList.Create;
    try
      BkName := '';
      if GetInfos( SL ) then
        BkName := SL.Values['Backup.Name']
    finally
      SL.Free;
    end;
    Restore;
    lblBackupName.Visible := True;
    edBackupName.Visible := True;
    edBackupName.Text := BkName;
    ShowMessage( 'Restoration successfull !' );
  finally
    Screen.Cursor := crDefault;
    Enabled := true;
  end;
end;


procedure TForm1.edDataPathEnter(Sender: TObject);
begin
  (Sender as TEdit).Color := clTeal;
  (Sender as TEdit).Font.Color := clWhite;
end;

procedure TForm1.edDataPathExit(Sender: TObject);
begin
  (Sender as TEdit).Color := clWindow;
  (Sender as TEdit).Font.Color := clBlack;
end;

procedure TForm1.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  edBackupName.Text := '';
  edDataPath.Text := '';
  edBackupPath.Text := MyBackup1.DrivePath;
  TabControl1Change(TabControl1);
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  if BrowseDialog.ShowModal = mrOk then
    edDataPath.Text := BrowseDialog.edPath.Text;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  SelectFiles.ShowModal;
  MyBackup1.FilesToBackup.Assign( SelectFiles.lbFiles.Items );
end;

end.
