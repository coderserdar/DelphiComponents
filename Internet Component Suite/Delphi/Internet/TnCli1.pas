{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Program:      TNCLIENT.PAS
Object:       Delphi application which is a basic telnet program demonstrating
              WSocket, TnCnx, TnEmulVT, EmulVT components.
Author:       François PIETTE
Creation:     July 22, 1997
Version:      2.05
EMail:        francois.piette@pophost.eunet.be    francois.piette@rtfm.be
              http://www.rtfm.be/fpiette
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1997, 1998, 1999 by François PIETTE
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
Sep 05, 1997  Added display of windows socket version info.
Sep 23, 1997  Added local echo check box
Sep 24, 1997  V2.03 Added TnEmulVT1.RestoreOptions just before connecting
              Added interactive support for telnet echo option.
Sep 25, 1997  V2.04 Port to C++Builder
Dec 10, 1998  V2.05 Added IniFile to save config

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit TnCli1;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IniFiles, EmulVT, TnEmulVT, WSocket, Winsock;

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
  public
    { Déclarations publiques }
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
    FIniFileName := LowerCase(ExtractFileName(Application.ExeName));
    FIniFileName := Copy(FIniFileName, 1, Length(FIniFileName) - 3) + 'ini';
    StatusLabel.Caption := 'Not connected';
    TnEmulVT1.RestoreOptions;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTelnetForm.FormShow(Sender: TObject);
var
    WinsockData : TWSADATA;
    IniFile     : TIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        IniFile := TIniFile.Create(FIniFileName);
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
        WinsockData := WinsockInfo;
        StatusLabel.Caption := StrPas(WinsockData.szDescription);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTelnetForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIniFile;
begin
    IniFile := TIniFile.Create(FIniFileName);
    IniFile.WriteString(SectionData, KeyHostName,  HostNameEdit.Text);
    IniFile.WriteString(SectionData, KeyPort,      PortEdit.Text);
    IniFile.WriteInteger(SectionWindow, KeyTop,    Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,   Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,  Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight, Height);
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

