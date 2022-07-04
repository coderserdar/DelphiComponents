unit fmMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Archiver, StdCtrls, ExtCtrls, DBTables, ComCtrls, Buttons, ArchiverRoot,
  CustExtractor, CustArchiver;

type
  TMain = class(TForm)
    Archiver1: TArchiver;
    OpenDialog1: TOpenDialog;
    Notebook1: TNotebook;
    rgDataLoc: TRadioGroup;
    cbAlias: TComboBox;
    edPath: TEdit;
    Button1: TButton;
    GroupBox1: TGroupBox;
    edFileName: TEdit;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    ProgressBar1: TProgressBar;
    lFile: TLabel;
    lTitle: TLabel;
    btnAbort: TBitBtn;
    Button5: TButton;
    btnOptions: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Archiver1StartOperation(Sender: TObject);
    procedure Archiver1FinishOperation(Sender: TObject);
    procedure Archiver1FileProgress(Sender: TObject; Percent: Integer);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure btnAbortClick(Sender: TObject);
    procedure Archiver1WriteUserData(Sender: TObject;
      var UserData: TUserData);
    procedure Archiver1AcceptArchive(Sender: TObject;
      const Header: TArchiveHeader; var Accept: Boolean);
    procedure Button5Click(Sender: TObject);
    procedure btnOptionsClick(Sender: TObject);
    procedure Archiver1DisplayMessage(Sender: TObject; const msg: String);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Déclarations privées }
    function GetPath : String;
  public
    { Déclarations publiques }
  end;

var
  Main: TMain;

implementation
uses FileCtrl, fmInspect, fmOptions;
{$R *.DFM}

procedure TMain.Button1Click(Sender: TObject);
var
  path : String;
begin
  path := '';
  if SelectDirectory( path, [], 0 ) then
    edPath.Text := path;
end;

procedure TMain.Button2Click(Sender: TObject);
begin
  with OpenDialog1 do
    begin
      if Execute then
        edFileName.Text := FileName;
    end;
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  if Assigned(Session) then
    try
      Session.GetAliasNames( cbAlias.Items );
    except
    end;
end;

procedure TMain.Archiver1StartOperation(Sender: TObject);
begin
  with Sender as TArchiver do
    begin
      if Operation = opAdd then
        lTitle.Caption := 'Backup in progress...'
      else if Operation = opExtract then
        lTitle.Caption := 'Restore in progress...'
      else
        lTitle.Caption := '';
    end;
  NoteBook1.PageIndex := 1;
end;

procedure TMain.Archiver1FinishOperation(Sender: TObject);
begin
  NoteBook1.PageIndex := 0;
  Application.ProcessMessages;
end;

procedure TMain.Archiver1FileProgress(Sender: TObject;
  Percent: Integer);
begin
  ProgressBar1.Position := Percent;
  btnAbort.Enabled := Archiver1.CanAbort;
  Application.ProcessMessages;
end;

function TMain.GetPath : String;
var
  L : TStringList;
begin
  if rgDataLoc.ItemIndex = 0 then
    begin
      if cbAlias.Text = '' then
        raise Exception.Create( 'You must select an alias' );
      L := TStringList.Create;
      try
        Session.GetAliasParams( cbAlias.Text, L );
        Result := L.Values['PATH'];
      finally
        L.Free;
      end;
    end
  else if rgDataLoc.ItemIndex = 1 then
    Result := edPath.Text
  else
    Result := '';
  if Result = '' then
    raise Exception.Create( 'There''s no path defined' );
  if not DirectoryExists( Result ) then
    raise Exception.CreateFmt( 'The folder "%s" does not exist', [Result] );
end;

procedure TMain.Button3Click(Sender: TObject);
var
  tmp : String;
begin
  tmp := GetPath;
  with Archiver1 do
    begin
      FileName := edFileName.Text;
      OpenNew;
      try
        Start;
        AddDirectory( tmp );
      finally
        Close;
      end;
    end;
end;

procedure TMain.Button4Click(Sender: TObject);
begin
  if not FileExists(edFileName.Text) then
    begin
      MessageDlg( Format( 'Archive %s does not exist !', [edFileName.Text]), mtError, [mbOk], 0 );
      Exit;
    end;
  with Archiver1 do
    begin
      FileName := edFileName.Text;
      ExtractPath := GetPath;
      Open;
      try
        ExtractFiles;
      finally
        Close;
      end;
    end;
end;

procedure TMain.btnAbortClick(Sender: TObject);
begin
  Archiver1.RequestAbort;
end;

procedure TMain.Archiver1WriteUserData(Sender: TObject;
  var UserData: TUserData);
begin
  with UserData do
    begin
      UserName         := 'John Smith';
      Company          := 'World Company';
      SerialNumber     := '123456789-ABCD';
      BackupName       := 'Demo';
      Date             := Now;
      ProductId        := 1234;
      ProductVersion   := 1;
    end;
end;

procedure TMain.Archiver1AcceptArchive(Sender: TObject;
  const Header: TArchiveHeader; var Accept: Boolean);
begin
  with Header.UserData do
    Accept := (ProductId = 1234) and (ProductVersion >= 1);
end;

procedure TMain.Button5Click(Sender: TObject);
begin
  if not FileExists(edFileName.Text) then
    begin
      MessageDlg( Format( 'Archive %s does not exist !', [edFileName.Text]), mtError, [mbOk], 0 );
      Exit;
    end;
  with Archiver1 do
    begin
      FileName := edFileName.Text;
      Open;
      try
        with Inspect, Header.UserData do
          begin
            lUser.Caption := UserName;
            lCompany.Caption := Company;
            lSerial.Caption := SerialNumber;
            lDate.Caption := DateTimeToStr(Date);
            lName.Caption := BackupName;
            lVersion.Caption := IntToStr(ProductVersion);
            ShowModal;
          end;
      finally
        Close;
      end;
    end;
end;

procedure TMain.btnOptionsClick(Sender: TObject);
begin
  Options.cbLanguage.ItemIndex := Integer(Archiver1.Language);
  Options.cbEncrypt.Checked := oCrypt in Archiver1.Options;
  Options.ShowModal;
  Archiver1.Language := TLanguage(Options.cbLanguage.ItemIndex);
  if Options.cbEncrypt.Checked then
    Archiver1.Options := Archiver1.Options + [oCrypt]
  else
    Archiver1.Options := Archiver1.Options - [oCrypt];
end;

procedure TMain.Archiver1DisplayMessage(Sender: TObject;
  const msg: String);
begin
  LFile.Caption := msg;
end;

procedure TMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not Archiver1.IsBusy;
  if not CanClose then
    MessageDlg( 'There''s an operation in progress. Click on abort before.', mtWarning, [mbOk], 0 );
end;

end.
