unit UFormReloc;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, ExtCtrls, XPMan;

type
  TFormReloc = class(TForm)
    btnOK: TButton;
    btnCancel: TButton;
    GroupBox1: TGroupBox;
    chkAppData: TRadioButton;
    chkDefault: TRadioButton;
    chkCustom: TRadioButton;
    btnPath: TButton;
    edPath: TEdit;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    chkAllUsers: TCheckBox;
    Bevel1: TBevel;
    Label1: TLabel;
    XPManifest1: TXPManifest;
    procedure btnPathClick(Sender: TObject);
    procedure chkDefaultClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FFolder: AnsiString;
    procedure ReadParams;
    procedure SaveParams;
  public
    { Public declarations }
  end;

var
  FormReloc: TFormReloc;

implementation

uses
  ATxSProc, ATxFProc, ATxRegistry, ATxUtils,
  FileCtrl;

{$R *.DFM}

const
  cRegKey = 'Software\UniversalViewer';
  cRegValue = 'ConfigurationFolder';
  cAppData = '%AppData%\ATViewer';

procedure TFormReloc.ReadParams;
begin
  FFolder:= 
    GetRegKeyStr(HKEY_CURRENT_USER, cRegKey, cRegValue,
    GetRegKeyStr(HKEY_LOCAL_MACHINE, cRegKey, cRegValue, ''));
end;

procedure TFormReloc.SaveParams;
begin
  SetRegKeyStr(HKEY_CURRENT_USER, cRegKey, cRegValue, FFolder);
  if chkAllUsers.Checked then
    SetRegKeyStr(HKEY_LOCAL_MACHINE, cRegKey, cRegValue, FFolder);
end;

procedure TFormReloc.btnPathClick(Sender: TObject);
var
  Path: AnsiString;
begin
  Path:= '';
  if SelectDirectory('', '', Path) then
    edPath.Text:= Path;
end;

procedure TFormReloc.chkDefaultClick(Sender: TObject);
begin
  edPath.Enabled:= chkCustom.Checked;
  btnPath.Enabled:= chkCustom.Checked;
end;

procedure TFormReloc.btnOKClick(Sender: TObject);
begin
  if chkDefault.Checked then FFolder:= '' else
   if chkAppData.Checked then FFolder:= cAppData else
    if chkCustom.Checked then
      begin
      FFolder:= edPath.Text;
      if not IsDirExist(SExpandVars(FFolder)) then
        begin Application.MessageBox('Selected folder does not exist', PChar(Caption), MB_OK or MB_ICONERROR); Exit end;
      end;

  SaveParams;
  Close;
end;

procedure TFormReloc.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormReloc.FormCreate(Sender: TObject);
begin
  ReadParams;
  chkDefault.Checked:= FFolder='';
  chkAppData.Checked:= StrIComp(PChar(FFolder), cAppData)=0;
  chkCustom.Checked:= (not chkDefault.Checked) and (not chkAppData.Checked);
  if chkCustom.Checked then
    edPath.Text:= FFolder;

  chkDefaultClick(Self);
  chkAppData.Enabled:= Pos('%', SExpandVars('%AppData%'))=0;
end;

end.
