{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Program:      TNCLIENT.PAS
Object:       Delphi application which is a basic telnet program demonstrating
              WSocket, TnCnx, TnEmulVT, EmulVT components.
Author:       François PIETTE
Creation:     July 22, 1997
Version:      8.02
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2013 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
              <francois.piette@overbyte.be>

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
Sep 05, 1997  Added display of windows socket version info.
Sep 23, 1997  Added local echo check box
Sep 24, 1997  V2.03 Added TnEmulVT1.RestoreOptions just before connecting
              Added interactive support for telnet echo option.
Sep 25, 1997  V2.04 Port to C++Builder
Dec 10, 1998  V2.05 Added IniFile to save config
Mar 26, 2006  V2.06 Set local echo at the right places: after connect and
              after setting the options.
Mar 26, 2006  V6.00 Started from ICS-V5
Aug 15, 2008  V7.00 Delphi 2009 (Unicode) support. The terminal is not
              unicode, but the component support unicode strings.
May 2012 - V8.00 - this is a Windows only demo, IPv4 only
Apr 16, 2013  V8.01 Angus, now supports IPv6, changed font to Courier New
Jun 4, 2014   V8.02 Angus, set autosize and white font

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsTelnetClient1;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms,
  StdCtrls, OverbyteIcsIniFiles, OverbyteIcsUtils,
  OverbyteIcsWSocket, OverbyteIcsWinsock,
  OverbyteIcsEmulVT,  OverbyteIcsTnEmulVT;

type
  TTelnetForm = class(TForm)
    TnEmulVT1: TTnEmulVT;
    ConnectButton: TButton;
    Label1: TLabel;
    HostNameEdit: TEdit;
    Label2: TLabel;
    PortEdit: TEdit;
    DisconnectButton: TButton;
    StatusLabel: TLabel;
    SendButton: TButton;
    OptionsButton: TButton;
    LocalEchoCheckBox: TCheckBox;
    RequestLocalEchoOnButton: TButton;
    RequestLocalEchoOffButton: TButton;
    procedure ConnectButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure TnEmulVT1SessionConnected(Sender: TObject);
    procedure TnEmulVT1SessionClosed(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SendButtonClick(Sender: TObject);
    procedure OptionsButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LocalEchoCheckBoxClick(Sender: TObject);
    procedure TnEmulVT1LocalEcho(Sender: TObject);
    procedure RequestLocalEchoOnButtonClick(Sender: TObject);
    procedure RequestLocalEchoOffButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FIniFileName : String;
    FInitialized : Boolean;
  end;

var
  TelnetForm: TTelnetForm;

implementation

{$R *.DFM}
const
    SectionWindow = 'Window';
    KeyTop        = 'Top';
    KeyLeft       = 'Left';
    KeyWidth      = 'Width';
    KeyHeight     = 'Height';
    SectionData   = 'Data';
    KeyHostName   = 'HostName';
    KeyPort       = 'Port';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTelnetForm.FormCreate(Sender: TObject);
begin
    FIniFileName := GetIcsIniFileName;
    TnEmulVT1.IniFilename := FIniFileName;
    StatusLabel.Caption := 'Not connected';
    TnEmulVT1.RestoreOptions;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTelnetForm.FormShow(Sender: TObject);
var
    WinsockData : TWSADATA;
    IniFile     : TIcsIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        IniFile := TIcsIniFile.Create(FIniFileName);
        HostNameEdit.Text  := IniFile.ReadString(SectionData, KeyHostName,
                                                 'localhost');
        PortEdit.Text      := IniFile.ReadString(SectionData, KeyPort,
                                                 'telnet');

        Width  := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top    := IniFile.ReadInteger(SectionWindow, KeyTop,    (Screen.Height - Height) div 2);
        Left   := IniFile.ReadInteger(SectionWindow, KeyLeft,   (Screen.Width - Width) div 2);

        IniFile.Free;
        TnEmulVT1.Clear;
        { Set auto-wrap mode. Here is the place to do other settings. }
        TnEmulVT1.WriteStr(#27'[?7h');
        WinsockData := WinsockInfo;
        StatusLabel.Caption := String(IcsStrPas(WinsockData.szDescription));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTelnetForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteString(SectionData, KeyHostName,  HostNameEdit.Text);
    IniFile.WriteString(SectionData, KeyPort,      PortEdit.Text);
    IniFile.WriteInteger(SectionWindow, KeyTop,    Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,   Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,  Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight, Height);
    IniFile.UpdateFile;
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTelnetForm.ConnectButtonClick(Sender: TObject);
begin
    StatusLabel.Caption := 'Connecting';
    Refresh;
    ConnectButton.Enabled := FALSE;
    try
        TnEmulVT1.Disconnect;
        TnEmulVT1.Port     := PortEdit.Text;
        TnEmulVT1.HostName := HostNameEdit.Text;
        TnEmulVT1.RestoreOptions;
        { This can take quite a long time when hostname is unknown and }
        { if DNS feature is enabled (2 or 3 minutes !)                 }
        TnEmulVT1.Connect;
    except
        ConnectButton.Enabled := TRUE;
        raise;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTelnetForm.DisconnectButtonClick(Sender: TObject);
begin
    TnEmulVT1.Disconnect;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTelnetForm.TnEmulVT1SessionConnected(Sender: TObject);
begin
    DisconnectButton.Enabled := TRUE;
    StatusLabel.Caption      := 'Connected';
    TnEmulVT1.LocalEcho      := LocalEchoCheckBox.Checked;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTelnetForm.TnEmulVT1SessionClosed(Sender: TObject);
begin
    DisconnectButton.Enabled := FALSE;
    ConnectButton.Enabled    := TRUE;
    StatusLabel.Caption      := 'Not connected';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTelnetForm.FormResize(Sender: TObject);
begin
    TnEmulVT1.Width  := ClientWidth;
    TnEmulVT1.Height := ClientHeight - TnEmulVT1.Top;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTelnetForm.SendButtonClick(Sender: TObject);
begin
    TnEmulVT1.SendStr('Hello world !' + #13#10);
    ActiveControl := TnEmulVT1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTelnetForm.OptionsButtonClick(Sender: TObject);
begin
    TnEmulVT1.HostName := HostNameEdit.Text;
    TnEmulVT1.EditOptions;
    TnEmulVT1.LocalEcho := LocalEchoCheckBox.Checked;
    ActiveControl       := TnEmulVT1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTelnetForm.TnEmulVT1LocalEcho(Sender: TObject);
begin
    if TnEmulVT1.GetLocalEcho then
        StatusLabel.Caption := 'Remote will not echo'
    else
        StatusLabel.Caption := 'Remote will echo';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTelnetForm.LocalEchoCheckBoxClick(Sender: TObject);
begin
    TnEmulVT1.LocalEcho := LocalEchoCheckBox.Checked;
    ActiveControl       := TnEmulVT1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTelnetForm.RequestLocalEchoOnButtonClick(Sender: TObject);
begin
    TnEmulVT1.RequestLocalEcho(TRUE);
    ActiveControl       := TnEmulVT1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTelnetForm.RequestLocalEchoOffButtonClick(Sender: TObject);
begin
    TnEmulVT1.RequestLocalEcho(FALSE);
    ActiveControl       := TnEmulVT1;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}


end.

