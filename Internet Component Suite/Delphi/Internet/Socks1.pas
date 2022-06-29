{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Show how to use TWSocket with SOCKS protocol to traverse
              a firewall.
EMail:        francois.piette@pophost.eunet.be    francois.piette@rtfm.be
              http://www.rtfm.be/fpiette
Creation:     November 21, 1998
Version:      1.00
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1998 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Socks1;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, IniFiles, WinSock, WSocket, StdCtrls, ExtCtrls;

type
  TSocksTestForm = class(TForm)
    DisplayMemo: TMemo;
    Panel1: TPanel;
    ConnectButton: TButton;
    DisconnectButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    TargetHostEdit: TEdit;
    TargetPortEdit: TEdit;
    SocksServerEdit: TEdit;
    SocksPortEdit: TEdit;
    WSocket1: TWSocket;
    Label5: TLabel;
    SocksUsercodeEdit: TEdit;
    SocksPasswordEdit: TEdit;
    Label6: TLabel;
    SocksAuthCheckBox: TCheckBox;
    ClearButton: TButton;
    Socks4RadioButton: TRadioButton;
    Socks5RadioButton: TRadioButton;
    procedure ConnectButtonClick(Sender: TObject);
    procedure WSocket1SessionConnected(Sender: TObject; Error: Word);
    procedure WSocket1DataAvailable(Sender: TObject; Error: Word);
    procedure WSocket1SocksError(Sender: TObject; Error: Integer;
      Msg: String);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure WSocket1SessionClosed(Sender: TObject; Error: Word);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure WSocket1SocksAuthState(Sender: TObject;
      AuthState: TSocksAuthState);
    procedure WSocket1SocksConnected(Sender: TObject; Error: Word);
    procedure DisplayMsg(Sender : TObject; var Msg : String);
    procedure ClearButtonClick(Sender: TObject);
  private
    FIniFileName : String;
    FInitialized : Boolean;
    FRcvBuf      : array [0..2047] of char;
    FRcvCnt      : Integer;
  public
    { Déclarations publiques }
  end;

var
  SocksTestForm: TSocksTestForm;

implementation

{$R *.DFM}

const
    SectionWindow      = 'Windows';
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';
    SectionData        = 'Data';
    KeyTargetHost      = 'TargetHost';
    KeyTargetPort      = 'TargetPort';
    KeySocksServer     = 'SocksServer';
    KeySocksPort       = 'SocksPort';
    KeySocksUsercode   = 'SocksUsercode';
    KeySocksPassword   = 'SocksPassword';
    KeySocksAuth       = 'SocksAuthentification';
    KeySocks4          = 'Socks4';
    KeySocks5          = 'Socks5';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
function TrimRight(Str : String) : String;
var
    i : Integer;
begin
    i := Length(Str);
    while (i > 0) and (Str[i] = ' ') do
        i := i - 1;
    Result := Copy(Str, 1, i);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TrimLeft(Str : String) : String;
var
    i : Integer;
begin
    if Str[1] <> ' ' then
        Result := Str
    else begin
        i := 1;
        while (i <= Length(Str)) and (Str[i] = ' ') do
            i := i + 1;
        Result := Copy(Str, i, Length(Str) - i + 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Trim(Str : String) : String;
begin
    Result := TrimLeft(TrimRight(Str));
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSocksTestForm.FormCreate(Sender: TObject);
begin
    FIniFileName := LowerCase(ExtractFileName(Application.ExeName));
    FIniFileName := Copy(FIniFileName, 1, Length(FIniFileName) - 3) + 'ini';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSocksTestForm.FormShow(Sender: TObject);
var
    IniFile : TIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        DisplayMemo.Clear;
        IniFile      := TIniFile.Create(FIniFileName);
        Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                            (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                            (Screen.Width  - Width)  div 2);
        TargetHostEdit.Text    := IniFile.ReadString(SectionData, KeyTargetHost,    '');
        TargetPortEdit.Text    := IniFile.ReadString(SectionData, KeyTargetPort,    '');
        SocksServerEdit.Text   := IniFile.ReadString(SectionData, KeySocksServer,   '');
        SocksPortEdit.Text     := IniFile.ReadString(SectionData, KeySocksPort,     '1080');
        SocksUsercodeEdit.Text := IniFile.ReadString(SectionData, KeySocksUsercode, '');
        SocksPasswordEdit.Text := IniFile.ReadString(SectionData, KeySocksPassword, '');
        SocksAuthCheckBox.Checked := Boolean(IniFile.ReadInteger(SectionData, KeySocksAuth, 0));
        Socks4RadioButton.Checked := Boolean(IniFile.ReadInteger(SectionData, KeySocks4,    0));
        Socks5RadioButton.Checked := Boolean(IniFile.ReadInteger(SectionData, KeySocks5,    1));
        IniFile.Destroy;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSocksTestForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
    IniFile.WriteString(SectionData, KeyTargetHost,    Trim(TargetHostEdit.Text));
    IniFile.WriteString(SectionData, KeyTargetPort,    Trim(TargetPortEdit.Text));
    IniFile.WriteString(SectionData, KeySocksServer,   Trim(SocksServerEdit.Text));
    IniFile.WriteString(SectionData, KeySocksPort,     Trim(SocksPortEdit.Text));
    IniFile.WriteString(SectionData, KeySocksUsercode, Trim(SocksUsercodeEdit.Text));
    IniFile.WriteString(SectionData, KeySocksPassword, Trim(SocksPasswordEdit.Text));
    IniFile.WriteInteger(SectionData, KeySocksAuth, Ord(SocksAuthCheckBox.Checked));
    IniFile.WriteInteger(SectionData, KeySocks5,    Ord(Socks5RadioButton.Checked));
    IniFile.WriteInteger(SectionData, KeySocks4,    Ord(Socks4RadioButton.Checked));
    IniFile.Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSocksTestForm.DisplayMsg(Sender : TObject; var Msg : String);
begin
    DisplayMemo.lines.Add(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSocksTestForm.ConnectButtonClick(Sender: TObject);
const
    AuthMethod : array [Boolean] of TSocksAuthentication =
        (socksNoAuthentication, socksAuthenticateUsercode);
begin
    if Socks5RadioButton.Checked then
        WSocket1.SocksLevel := '5'
    else if Socks4RadioButton.Checked and SocksAuthCheckBox.Checked then
        WSocket1.SocksLevel := '4A'
    else
        WSocket1.SocksLevel := '4';
    DisplayMemo.Lines.Add('Connecting using Socks' + WSocket1.SocksLevel);

    WSocket1.SocksServer         := Trim(SocksServerEdit.Text);
    WSocket1.SocksPort           := Trim(SocksPortEdit.Text);
    WSocket1.SocksUsercode       := Trim(SocksUsercodeEdit.Text);
    WSocket1.SocksPassword       := Trim(SocksPasswordEdit.Text);
    WSocket1.SocksAuthentication := AuthMethod[SocksAuthCheckBox.Checked];
    WSocket1.Proto               := 'tcp';
    WSocket1.Addr                := Trim(TargetHostEdit.Text);
    WSocket1.Port                := Trim(TargetPortEdit.Text);
    WSocket1.OnDisplay           := DisplayMsg;
    WSocket1.Connect;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSocksTestForm.DisconnectButtonClick(Sender: TObject);
begin
    WSocket1.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSocksTestForm.WSocket1SessionConnected(Sender: TObject; Error: Word);
begin
    DisplayMemo.Lines.Add('Session connected to remote host.');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSocksTestForm.WSocket1SocksConnected(Sender: TObject; Error: Word);
begin
    DisplayMemo.Lines.Add('Session connected to socks server.');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSocksTestForm.WSocket1SocksAuthState(Sender: TObject;
  AuthState: TSocksAuthState);
begin
    case AuthState of
    socksAuthStart:
        DisplayMemo.Lines.Add('Socks authentification start.');
    socksAuthSuccess:
        DisplayMemo.Lines.Add('Socks authentification success.');
    socksAuthFailure:
        DisplayMemo.Lines.Add('Socks authentification failure.');
    socksAuthNotRequired:
        DisplayMemo.Lines.Add('Socks authentification not required.');
    else
        DisplayMemo.Lines.Add('Unknown socks authentification state.')
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSocksTestForm.WSocket1SessionClosed(Sender: TObject; Error: Word);
begin
    DisplayMemo.Lines.Add('Session Closed');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSocksTestForm.WSocket1DataAvailable(Sender: TObject; Error: Word);
var
    Len : Integer;
    I   : Integer;
    p   : PChar;
begin
    Len := TWSocket(Sender).Receive(@FRcvBuf[FRcvCnt], Sizeof(FRcvBuf) - FRcvCnt - 1);
    if Len < 0 then
        Exit;
    FRcvCnt := FRcvCnt + Len;
    FRcvBuf[FRcvCnt] := #0;

    while FRcvCnt > 0 do begin
        p := StrScan(FRcvBuf, #10);
        if p = nil then
            Exit;
        I := p - FRcvBuf;

        FRcvBuf[I] := #0;
        if (I > 0) and (FRcvBuf[I - 1] = #13) then
            FRcvBuf[I - 1] := #0;

        DisplayMemo.Lines.Add('Received: ''' + StrPas(FRcvBuf) + '''');
        Move(FRcvBuf[I + 1], FRcvBuf[0], FRcvCnt - I);
        FRcvCnt := FRcvCnt - I - 1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSocksTestForm.WSocket1SocksError(Sender : TObject; Error : Integer; Msg : String);
begin
    DisplayMemo.Lines.Add('Socks error #' + IntToStr(Error) + ' ' + Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSocksTestForm.ClearButtonClick(Sender: TObject);
begin
    DisplayMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

