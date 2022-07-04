unit ConDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, ComCtrls,
  Buttons;

type
  TfrmConDlg = class(TForm)
    Panel1: TPanel;
    bOK: TBitBtn;
    bCancel: TBitBtn;
    GroupBox1: TGroupBox;
    Label5: TLabel;
    edHost: TEdit;
    Label8: TLabel;
    sedPort: TSpinEdit;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    edUser: TEdit;
    Label6: TLabel;
    edPassword: TEdit;
    procedure bOKClick(Sender: TObject);
    procedure bCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FIniFileName: String;
  public
    { Public declarations }
    procedure Load;
    procedure Save;
    property IniFileName:String read FIniFileName write FIniFileName;
  end;

var
  frmConDlg: TfrmConDlg;

implementation

uses IniFiles;

{$R *.dfm}

procedure TfrmConDlg.Load;
var LIni: TIniFile;
begin
  if FileExists(FIniFileName) then
  begin
    LIni:= TIniFile.Create(IncludeTrailingPathDelimiter(
                           ExtractFilePath(Application.ExeName)) + FIniFileName);
    try
      edHost.Text:= LIni.ReadString('Server','Host','localhost');
      sedPort.Value:= LIni.ReadInteger('Server','Port',0);
      edUser.Text:= LIni.ReadString('Auth','User','');
      edPassword.Text:= LIni.ReadString('Auth','Password','');
    finally
      FreeAndNil(LIni);
    end;
  end;
end;

procedure TfrmConDlg.Save;
var LIni: TIniFile;
begin
  LIni:= TIniFile.Create(IncludeTrailingPathDelimiter(
                         ExtractFilePath(Application.ExeName)) + FIniFileName);
  try
    LIni.WriteString('Server','Host',edHost.Text);
    LIni.WriteInteger('Server','Port',sedPort.Value);
    LIni.WriteString('Auth','User',edUser.Text);
    LIni.WriteString('Auth','Password',edPassword.Text);
    LIni.UpdateFile;
  finally
    FreeAndNil(LIni);
  end;
end;

procedure TfrmConDlg.bOKClick(Sender: TObject);
begin
  Save;
end;

procedure TfrmConDlg.bCancelClick(Sender: TObject);
begin
  Load;
end;

procedure TfrmConDlg.FormShow(Sender: TObject);
begin
  Load;
end;

procedure TfrmConDlg.FormCreate(Sender: TObject);
begin
  FIniFileName:= 'options.ini';
end;

end.
