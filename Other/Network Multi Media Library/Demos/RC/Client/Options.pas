unit Options;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, ComCtrls, Buttons;

type
  TfrmOptions = class(TForm)
    Panel1: TPanel;
    bOK: TBitBtn;
    bCancel: TBitBtn;
    pcParams: TPageControl;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Label9: TLabel;
    Label10: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label7: TLabel;
    edProxyHost: TEdit;
    sedProxyPort: TSpinEdit;
    edProxyUserName: TEdit;
    edProxyPassword: TEdit;
    cbSocksProxyVersion: TComboBox;
    Label2: TLabel;
    edPeriod: TSpinEdit;
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
  frmOptions: TfrmOptions;

implementation

uses IniFiles;

{$R *.dfm}

procedure TfrmOptions.Load;
var LIni: TIniFile;
begin
  if FileExists(FIniFileName) then
  begin
    LIni:= TIniFile.Create(IncludeTrailingPathDelimiter(
                           ExtractFilePath(Application.ExeName)) + FIniFileName);
    try
      edPeriod.Value:= LIni.ReadInteger('Server','Period',0);

      cbSocksProxyVersion.ItemIndex:=
        cbSocksProxyVersion.Items.IndexOf(LIni.ReadString('Proxy','Version',''));
      edProxyHost.Text:= LIni.ReadString('Proxy','Host','localhost');
      sedProxyPort.Value:= LIni.ReadInteger('Proxy','Port',0);
      edProxyUserName.Text:= LIni.ReadString('Proxy','User','');
      edProxyPassword.Text:= LIni.ReadString('Proxy','Password','');
    finally
      FreeAndNil(LIni);
    end;
  end;
end;

procedure TfrmOptions.Save;
var LIni: TIniFile;
begin
  LIni:= TIniFile.Create(IncludeTrailingPathDelimiter(
                         ExtractFilePath(Application.ExeName)) + FIniFileName);
  try
    LIni.WriteInteger('Server','Period',edPeriod.Value);

    LIni.WriteString('Proxy','Version',cbSocksProxyVersion.Text);
    LIni.WriteString('Proxy','Host',edProxyHost.Text);
    LIni.WriteInteger('Proxy','Port',sedProxyPort.Value);
    LIni.WriteString('Proxy','User',edProxyUserName.Text);
    LIni.WriteString('Proxy','Password',edProxyPassword.Text);

    LIni.UpdateFile;
  finally
    FreeAndNil(LIni);
  end;
end;

procedure TfrmOptions.bOKClick(Sender: TObject);
begin
  Save;
end;

procedure TfrmOptions.bCancelClick(Sender: TObject);
begin
  Load;
end;

procedure TfrmOptions.FormShow(Sender: TObject);
begin
  Load;
end;

procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  FIniFileName:= 'options.ini';
end;

end.
