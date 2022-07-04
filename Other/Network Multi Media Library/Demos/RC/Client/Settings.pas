unit Settings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, ComCtrls, frSelectAudio,
  frSelectAudioInput, Buttons;

type
  TfrmSettings = class(TForm)
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
  frmSettings: TfrmSettings;

implementation

uses IniFiles;

{$R *.dfm}

procedure TfrmSettings.Load;
var LIni: TIniFile;
begin
  if FileExists(FIniFileName) then
  begin
    LIni:= TIniFile.Create(IncludeTrailingPathDelimiter(
                           ExtractFilePath(Application.ExeName)) + FIniFileName);
    try
      edHost.Text:= LIni.ReadString('Server','Host','localhost');
      sedPort.Value:= LIni.ReadInteger('Server','Port',0);
      edPeriod.Value:= LIni.ReadInteger('Server','Period',0);
      edUser.Text:= LIni.ReadString('Auth','User','');
      edPassword.Text:= LIni.ReadString('Auth','Password','');

      cbSocksProxyVersion.ItemIndex:=
        cbSocksProxyVersion.Items.IndexOf(LIni.ReadString('Proxy','Version',''));
      edProxyHost.Text:= LIni.ReadString('Proxy','Host','localhost');
      sedProxyPort.Value:= LIni.ReadInteger('Proxy','Port',0);
      edProxyUserName.Text:= LIni.ReadString('Proxy','User','');
      edProxyPassword.Text:= LIni.ReadString('Proxy','Password','');

      SelectAudioInput.DeviceId:= LIni.ReadInteger('Audio','DeviceID',0);
    finally
      FreeAndNil(LIni);
    end;
  end;
end;

procedure TfrmSettings.Save;
var LIni: TIniFile;
begin
  LIni:= TIniFile.Create(IncludeTrailingPathDelimiter(
                         ExtractFilePath(Application.ExeName)) + FIniFileName);
  try
    LIni.WriteString('Server','Host',edHost.Text);
    LIni.WriteInteger('Server','Port',sedPort.Value);
    LIni.WriteInteger('Server','Period',edPeriod.Value);
    LIni.WriteString('Auth','User',edUser.Text);
    LIni.WriteString('Auth','Password',edPassword.Text);

    LIni.WriteString('Proxy','Version',cbSocksProxyVersion.Text);
    LIni.WriteString('Proxy','Host',edProxyHost.Text);
    LIni.WriteInteger('Proxy','Port',sedProxyPort.Value);
    LIni.WriteString('Proxy','User',edProxyUserName.Text);
    LIni.WriteString('Proxy','Password',edProxyPassword.Text);

    LIni.WriteInteger('Audio','DeviceID',SelectAudioInput.DeviceId);
    LIni.UpdateFile;
  finally
    FreeAndNil(LIni);
  end;
end;

procedure TfrmSettings.bOKClick(Sender: TObject);
begin
  Save;
end;

procedure TfrmSettings.bCancelClick(Sender: TObject);
begin
  Load;
end;

procedure TfrmSettings.FormShow(Sender: TObject);
begin
  Load;
end;

end.
