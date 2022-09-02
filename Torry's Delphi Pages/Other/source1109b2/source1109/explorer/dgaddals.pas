{$I fsdefine.inc}

unit dgaddals;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  FileCtrl,
  Buttons,
  ExtCtrls,
  fsllbase,
  fsllunc,
  uconsts,
  uentity,
  ubase;

type
  TdlgAddAlias = class(TForm)
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    cboDrives: TDriveComboBox;
    Label3: TLabel;
    Label4: TLabel;
    lstFolders: TDirectoryListBox;
    edtAlias: TEdit;
    edtPath: TEdit;
    cbCheckSpace: TCheckBox;
    procedure btnOKClick(Sender: TObject);
    procedure lstFoldersChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FDatabase : TffeDatabaseItem;
    FServer : TffeServerItem;
  public
  end;

function ShowAddAliasDlg(aServer: TffeServerItem;
                         var aDatabase : TffeDatabaseItem): TModalResult;

var
  dlgAddAlias: TdlgAddAlias;

implementation

{$R *.DFM}

function ShowAddAliasDlg(aServer : TffeServerItem;
                         var aDatabase: TffeDatabaseItem): TModalResult;
begin
  with TdlgAddAlias.Create(nil) do
  try
    edtAlias.MaxLength := fscl_GeneralNameSize;      {!!.10}
    FServer := aServer;
    Result := ShowModal;
    aDatabase := FDatabase;
  finally
    Free;
  end;
end;

procedure TdlgAddAlias.FormCreate(Sender: TObject);
begin
  FDatabase := nil;
  HelpContext := hcAddDatabaseDlg;
end;

procedure TdlgAddAlias.btnOKClick(Sender: TObject);
var
  ExistingAliases: TStringList;
  UNCFilename: TffFullFilename;
begin
  if edtAlias.Text = '' then
    raise Exception.Create('You must enter an alias name.');

  if edtPath.Text = '' then
    raise Exception.Create('You must enter a path.');

  { Check if directory is on local machine }
  UNCFilename := FFExpandUNCFileName(edtPath.Text);
  if Copy(UNCFilename, 2, 1) = ':' then
    if MessageDlg('This path is local to this workstation.  ' +
                  'Are you sure you want to locate a database here?',
                  mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Abort;

  { If directory is not valid, then ask "do you want to create?" }
  if not DirectoryExists(edtPath.Text) then
    if MessageDlg('Directory ' + edtPath.Text + ' does not exist, ' +
                  'do you want to create this directory?', mtConfirmation,
                  [mbYes, mbNo], 0) = mrYes then
      ForceDirectories(edtPath.Text)
    else
      Exit;

  { Go get all the aliases for this server.  Need a fresh list in case any
    were added by other users recently }
  ExistingAliases := TStringList.Create;
  try
    FServer.GetAliases(ExistingAliases);
    if ExistingAliases.IndexOf(edtAlias.Text) <> -1 then
      raise Exception.CreateFmt('The alias "%s" is already defined for this server.', [edtAlias.Text]);
  finally
    ExistingAliases.Free;
  end;

  { Physically add the alias to the server }
  FServer.AddAlias(edtAlias.Text, edtPath.Text, cbCheckSpace.Checked); {!!.11}

  { Now add an entry to our internal list o' databases }
  FDatabase := FServer.AddDatabase(edtAlias.Text);

  ModalResult := mrOK;
end;

procedure TdlgAddAlias.lstFoldersChange(Sender: TObject);
begin
  edtPath.Text := lstFolders.Directory;
end;

end.
