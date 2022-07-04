{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Copyright:    You can use this software freely, at your own risks
Creation:     April 4, 1997
Version:      2.03
Object:       Demo program to show how to use TWSocket object to listen
              UDP messages from the network. Use UDPSend or any other
              program to send UDP messages.
EMail:        francois.piette@pophost.eunet.be    
              francois.piette@rtfm.be             http://www.rtfm.be/fpiette
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1997, 1998 by François PIETTE
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

Updates:
Jul 23, 1997 Adapted for Delphi 1, 2 and 3
Sep 06, 1997 Version 2.01
Sep 27, 1997 Updated for TWSocket changes
             Replace loopback address by real localhost IP addr
Dec 12, 1998 V2.02 Added icomming IP and port number display
Mar 07, 1999 V2.03 Corrected compatibility bug with Delphi 1

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit UdpLstn1;

interface

uses
  WinTypes, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FormPos, StdCtrls, WinSock, WSocket, IniFiles;

type
  TMainForm = class(TForm)
    WSocket: TWSocket;
    StartButton: TButton;
    DataAvailableLabel: TLabel;
    InfoLabel: TLabel;
    StopButton: TButton;
    PortEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    ServerEdit: TEdit;
    AnyServerCheckBox: TCheckBox;
    procedure StartButtonClick(Sender: TObject);
    procedure WSocketDataAvailable(Sender: TObject; Error: Word);
    procedure WSocketSessionConnected(Sender: TObject; Error: Word);
    procedure StopButtonClick(Sender: TObject);
    procedure WSocketSessionClosed(Sender: TObject; Error: Word);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure AnyServerCheckBoxClick(Sender: TObject);
    procedure ServerEditChange(Sender: TObject);
  private
    { Déclarations privées }
    FIniFileName : String;
    FSectionName : String;
    FKeyName     : String;
    FServerAddr  : TInAddr;
  public
    { Déclarations publiques }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF WIN32}
function TrimRight(Str : String) : String;
var
    i : Integer;
begin
    i := Length(Str);
    while (i > 0) and (Str[i] = ' ') do
        i := i - 1;
    Result := Copy(Str, 1, i);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF WIN32}
function TrimLeft(Str : String) : String;
var
    i : Integer;
begin
    if Str[1] <> ' ' then             { Petite optimisation: pas d'espace   }
        Result := Str
    else begin
        i := 1;
        while (i <= Length(Str)) and (Str[i] = ' ') do
            i := i + 1;
        Result := Copy(Str, i, Length(Str) - i + 1);
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF WIN32}
function Trim(Str : String) : String;
begin
    Result := TrimLeft(TrimRight(Str));
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.StartButtonClick(Sender: TObject);
begin
    FServerAddr               := WSocketResolveHost(ServerEdit.Text);
    if FServerAddr.S_addr = htonl(INADDR_LOOPBACK) then begin
        { Replace loopback address by real localhost IP addr }
        FServerAddr           := WSocketResolveHost(LocalHostName);
    end;
    WSocket.Proto             := 'udp';
    WSocket.Addr              := '0.0.0.0';
    WSocket.Port              := PortEdit.Text;
    WSocket.Listen;
    PortEdit.Enabled          := FALSE;
    ServerEdit.Enabled        := FALSE;
    AnyServerCheckBox.Enabled := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function atoi(value : string) : Integer;
var
    i : Integer;
begin
    Result := 0;
    i := 1;
    while (i <= Length(Value)) and (Value[i] = ' ') do
        i := i + 1;
    while (i <= Length(Value)) and (Value[i] in ['0'..'9']) do begin
        Result := Result * 10 + ord(Value[i]) - ord('0');
        i := i + 1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.WSocketDataAvailable(Sender: TObject; Error: Word);
var
    Buffer : array [0..1023] of char;
    Len    : Integer;
    Src    : TSockAddrIn;
    SrcLen : Integer;
begin
    SrcLen := SizeOf(Src);
    Len    := WSocket.ReceiveFrom(@Buffer, SizeOf(Buffer), Src, SrcLen);
    if Len >= 0 then begin
        if (FServerAddr.S_addr = INADDR_ANY) or
           (FServerAddr.S_addr = Src.Sin_addr.S_addr) then begin
            Buffer[Len] := #0;
            DataAvailableLabel.Caption := IntToStr(atoi(DataAvailableLabel.caption) + 1) +
                                          '  ' + StrPas(inet_ntoa(Src.sin_addr)) +
                                          ':'  + IntToStr(ntohs(Src.sin_port)) +
                                          '--> ' + StrPas(Buffer);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.WSocketSessionConnected(Sender: TObject;
  Error: Word);
begin
    StartButton.Enabled := FALSE;
    StopButton.Enabled  := TRUE;
    InfoLabel.Caption   := 'Connected';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.StopButtonClick(Sender: TObject);
begin
    StartButton.Enabled       := TRUE;
    StopButton.Enabled        := FALSE;
    PortEdit.Enabled          := TRUE;
    ServerEdit.Enabled        := TRUE;
    AnyServerCheckBox.Enabled := TRUE;
    WSocket.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.WSocketSessionClosed(Sender: TObject; Error: Word);
begin
    StartButton.Enabled        := TRUE;
    StopButton.Enabled         := FALSE;
    PortEdit.Enabled           := TRUE;
    ServerEdit.Enabled         := TRUE;
    AnyServerCheckBox.Enabled  := TRUE;
    InfoLabel.Caption          := 'Disconnected';
    DataAvailableLabel.Caption := '';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.FormShow(Sender: TObject);
const
    FirstTime : Boolean = TRUE;
var
    IniFile   : TIniFile;
begin
    if FirstTime then begin
        FirstTime := FALSE;
        FIniFileName     := 'UdpLstn';
        FSectionName     := 'Windows';
        FKeyName         := 'MainForm';
        LoadFormPos(Self, FIniFilename, FSectionName, FKeyName);
        DataAvailableLabel.Caption := '';
        InfoLabel.Caption          := 'Click on Start button';
        StartButton.Enabled        := TRUE;
        StopButton.Enabled         := FALSE;
        IniFile                    := TIniFile.Create(FIniFileName);
        PortEdit.Text              := IniFile.ReadString('data', 'port',   '600');
        ServerEdit.Text            := IniFile.ReadString('data', 'server', '0.0.0.0');
        IniFile.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
var
    IniFile   : TIniFile;
begin
    SaveFormPos(Self, FIniFilename, FSectionName, FKeyName);
    IniFile := TIniFile.Create(FIniFileName);
    IniFile.WriteString('data', 'port',   PortEdit.Text);
    IniFile.WriteString('data', 'server', ServerEdit.Text);
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.AnyServerCheckBoxClick(Sender: TObject);
begin
    if AnyServerCheckBox.Checked then
        ServerEdit.Text := '0.0.0.0';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.ServerEditChange(Sender: TObject);
begin
    AnyServerCheckBox.Checked := (Trim(ServerEdit.Text) = '0.0.0.0');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

