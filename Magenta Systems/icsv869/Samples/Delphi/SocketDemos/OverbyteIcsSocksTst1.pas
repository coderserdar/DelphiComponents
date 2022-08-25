{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Show how to use TWSocket with SOCKS protocol to traverse
              a firewall.
Creation:     November 21, 1998
Version:      8.66
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 1996-2021 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.

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
Feb 08, 2014 FPiette: Removed use of StrScan
Mar 23, 2021 V8.66 - Added ProxyURL allowing settings using a single URL,
                     Added support for HTTP Tunnel proxy.
                     Added Command to send on connection or by button.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSocksTst1;

{$I Include\OverbyteIcsDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, OverbyteIcsIniFiles, WinSock, OverbyteIcsWSocket, StdCtrls, ExtCtrls,
  OverbyteIcsWndControl, OverbyteIcsUtils;

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
    HttpTunnelRadioButton: TRadioButton;
    Label7: TLabel;
    ProxyUrlEdit: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    CommandEdit: TEdit;
    Label10: TLabel;
    cmdButton: TButton;
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
    procedure cmdButtonClick(Sender: TObject);
    procedure WSocket1HttpTunnelConnected(Sender: TObject; ErrCode: Word);
    procedure WSocket1HttpTunnelError(Sender: TObject; ErrCode: Word; TunnelServerAuthTypes: THttpTunnelServerAuthTypes;
      const Msg: string);
  private
    FIniFileName : String;
    FInitialized : Boolean;
    FRcvBuf      : array [0..2047] of AnsiChar;
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
    KeyHttpTunnel      = 'HttpTunnel';
    KeyProxyUrl        = 'ProxyUrl';
    KeyCommand         = 'Command';

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
    FIniFileName := OverbyteIcsIniFiles.GetIcsIniFileName;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSocksTestForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        DisplayMemo.Clear;
        IniFile      := TIcsIniFile.Create(FIniFileName);
        try
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
            HttpTunnelRadioButton.Checked := Boolean(IniFile.ReadInteger(SectionData, KeyHttpTunnel,  0));
            ProxyUrlEdit.Text := IniFile.ReadString(SectionData, KeyProxyUrl, '');
            CommandEdit.Text := IniFile.ReadString(SectionData, KeyCommand, 'Hello');
        finally
            IniFile.Free;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSocksTestForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    try
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
        IniFile.WriteInteger(SectionData, KeyHttpTunnel,Ord(HttpTunnelRadioButton.Checked));
        IniFile.WriteString(SectionData, KeyProxyUrl,   Trim(ProxyUrlEdit.Text));
        IniFile.WriteString(SectionData, KeyCommand,    Trim(CommandEdit.Text));
        IniFile.UpdateFile;
    finally
        IniFile.Destroy;
    end;
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
    HttpMethod : array [Boolean] of THttpTunnelAuthType =
        (htatNone, htatBasic);
begin
    WSocket1.SetSocks('');           { V8.66 clear socks settings }
    WSocket1.SetHTTPTunnel('');      { V8.66 clear tunnel settings }

  { V8.66 ProxyURL allows us to set everything in one go }
    if ProxyUrlEdit.Text <> '' then begin
         WSocket1.ProxyURL := ProxyUrlEdit.Text;
         DisplayMemo.Lines.Add('Connecting using Proxy URL');
    end
    else begin
        WSocket1.ProxyURL := '';
        if Socks5RadioButton.Checked then
            WSocket1.SocksLevel := '5'
        else if Socks4RadioButton.Checked and SocksAuthCheckBox.Checked then
            WSocket1.SocksLevel := '4A'
        else
            WSocket1.SocksLevel := '4';
     { V8.66 test HTTP Tunnel }
        if HttpTunnelRadioButton.Checked then begin
            DisplayMemo.Lines.Add('Connecting using HTTP Tunnel Proxy');
            WSocket1.HttpTunnelServer         := Trim(SocksServerEdit.Text);
            WSocket1.HttpTunnelPort           := Trim(SocksPortEdit.Text);
            WSocket1.HttpTunnelUsercode       := Trim(SocksUsercodeEdit.Text);
            WSocket1.HttpTunnelPassword       := Trim(SocksPasswordEdit.Text);
            WSocket1.HttpTunnelAuthType       := HttpMethod[SocksAuthCheckBox.Checked];
        end
        else begin
            DisplayMemo.Lines.Add('Connecting using Socks' + WSocket1.SocksLevel + ' Proxy');
            WSocket1.SocksServer         := Trim(SocksServerEdit.Text);
            WSocket1.SocksPort           := Trim(SocksPortEdit.Text);
            WSocket1.SocksUsercode       := Trim(SocksUsercodeEdit.Text);
            WSocket1.SocksPassword       := Trim(SocksPasswordEdit.Text);
            WSocket1.SocksAuthentication := AuthMethod[SocksAuthCheckBox.Checked];
        end;
    end;
    WSocket1.Proto               := 'tcp';
    WSocket1.Addr                := Trim(TargetHostEdit.Text);
    WSocket1.Port                := Trim(TargetPortEdit.Text);
    WSocket1.OnDebugDisplay      := DisplayMsg;
    WSocket1.Connect;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSocksTestForm.DisconnectButtonClick(Sender: TObject);
begin
    WSocket1.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSocksTestForm.cmdButtonClick(Sender: TObject);
begin
    if (CommandEdit.Text <> '') and (WSocket1.State = wsConnected) then
        WSocket1.SendStr(CommandEdit.Text + IcsCRLF);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSocksTestForm.WSocket1SessionConnected(Sender: TObject; Error: Word);
begin
    if Error = 0 then begin
        DisplayMemo.Lines.Add('Session connected okay to remote host.');
        if CommandEdit.Text <> '' then
             WSocket1.SendStr(CommandEdit.Text + IcsCRLF);
    end
    else
        DisplayMemo.Lines.Add('Session failed to connect to remote host.')
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSocksTestForm.WSocket1SocksConnected(Sender: TObject; Error: Word);
begin
    if Error = 0 then
        DisplayMemo.Lines.Add('Session connected okay to socks server.')
     else
        DisplayMemo.Lines.Add('Session failed to connect to socks server.')
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
begin
    Len := TWSocket(Sender).Receive(@FRcvBuf[FRcvCnt], Sizeof(FRcvBuf) - FRcvCnt - 1);
    if Len < 0 then
        Exit;
    FRcvCnt := FRcvCnt + Len;
    FRcvBuf[FRcvCnt] := #0;

    while FRcvCnt > 0 do begin
        // Search for ending LF
        I := 1;
        while (I < FRcvCnt) and (FRcvBuf[I] <> #10) do
            Inc(I);
        if I >= FRcvCnt then
            Exit;
        FRcvBuf[I] := #0;   // Remove LF
        // If CR present, remove it as well
        if (I > 0) and (FRcvBuf[I - 1] = #13) then
            FRcvBuf[I - 1] := #0;

        DisplayMemo.Lines.Add('Received: ''' + String(FRcvBuf) + '''');

        Move(FRcvBuf[I + 1], FRcvBuf[0], FRcvCnt - I);
        FRcvCnt := FRcvCnt - I - 1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSocksTestForm.WSocket1HttpTunnelConnected(Sender: TObject; ErrCode: Word);
begin
    if Error = 0 then
        DisplayMemo.Lines.Add('Session connected okay to HTTP Tunnel server.')
     else
        DisplayMemo.Lines.Add('Session failed to connect to HTTP Tunnel server.')
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSocksTestForm.WSocket1HttpTunnelError(Sender: TObject; ErrCode: Word; TunnelServerAuthTypes: THttpTunnelServerAuthTypes;
  const Msg: string);
begin
    DisplayMemo.Lines.Add('HTTP Tunnel error #' + IntToStr(ErrCode) + ' ' + Msg);
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

