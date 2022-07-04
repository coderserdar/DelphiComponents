unit PhoneSettings;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons;

type
  TfrmPhoneSettings = class(TForm)
    Label2: TLabel;
    edIP: TEdit;
    Label3: TLabel;
    edPort: TEdit;
    Label1: TLabel;
    edUser: TEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function LocalIP: string;
    procedure Load;
    procedure Save;
  end;

var
  frmPhoneSettings: TfrmPhoneSettings;

implementation

uses IniFiles, WinSock,
     NokiaStyle;

{$R *.dfm}

function TfrmPhoneSettings.LocalIP: string;
type
   TaPInAddr = array [0..10] of PInAddr;
   PaPInAddr = ^TaPInAddr;
   PHostEnt= ^hostent;
var
    phe: PHostEnt;
    pptr: PaPInAddr;
    Buffer: array [0..63] of char;
    i: Integer;
    GInitData: TWSADATA;
begin
    WSAStartup($101, GInitData);
    Result := '';
    GetHostName(Buffer, SizeOf(Buffer));
    phe:= Pointer(GetHostByName(buffer));
    if phe = nil then Exit;
    pptr := PaPInAddr(Phe^.h_addr_list);
    i := 0;
    while pptr^[i] <> nil do
    begin
      result:=StrPas(inet_ntoa(pptr^[i]^));
      Inc(i);
    end;
    WSACleanup;
end;

procedure TfrmPhoneSettings.Load;
var LIni: TIniFile;
    LIniName: String;
begin
  LIniName:= IncludeTrailingPathDelimiter(
                           ExtractFilePath(Application.ExeName)) + 'PhoneSettings.ini';
  LIni:= TIniFile.Create(LIniName);
  try
    edPort.Text:= LIni.ReadString('Network','Port',IntToStr(frmNokiaStyle.Server.Port));
    edUser.Text:= LIni.ReadString('Auth','User','Untitled');
  finally
    FreeAndNil(LIni);
  end;
end;

procedure TfrmPhoneSettings.Save;
var LIni: TIniFile;
    LIniName: String;
begin
  LIniName:= IncludeTrailingPathDelimiter(
                           ExtractFilePath(Application.ExeName)) + 'PhoneSettings.ini';
  LIni:= TIniFile.Create(LIniName);
  try
    LIni.WriteString('Network','Port',edPort.Text);
    LIni.WriteString('Auth','User',edUser.Text);
    LIni.UpdateFile;
  finally
    FreeAndNil(LIni);
  end;
end;

procedure TfrmPhoneSettings.BitBtn1Click(Sender: TObject);
begin
  Save;
end;

procedure TfrmPhoneSettings.BitBtn2Click(Sender: TObject);
begin
  Load;
end;

procedure TfrmPhoneSettings.FormCreate(Sender: TObject);
begin
  edIP.Text:= LocalIP;
  Load;
end;

end.
