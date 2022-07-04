{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Copyright:    You can use this software freely, at your own risks
Creation:     April 4, 1997
Version:      2.02
Object:       Demo program to show how to use TWSocket object to broadcast
              UDP messages on the network. Use UDPLstn to listen to those
              UDP messages, or other UDP messages.
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
Sep 06, 1997 Version 2.01
Dec 12, 1998 V2.02 Added LocalPort editbox

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit UdpSend1;

interface

uses
  WinTypes, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  FormPos, StdCtrls, WSocket, IniFiles;

type
  TMainForm = class(TForm)
    WSocket: TWSocket;
    SendButton: TButton;
    MessageEdit: TEdit;
    PortEdit: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    LocalPortEdit: TEdit;
    AnyPortCheckBox: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SendButtonClick(Sender: TObject);
    procedure AnyPortCheckBoxClick(Sender: TObject);
    procedure LocalPortEditChange(Sender: TObject);
  private
    FIniFileName : String;
    FSectionName : String;
    FKeyName     : String;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.FormShow(Sender: TObject);
const
    FirstTime : Boolean = TRUE;
var
    IniFile   : TIniFile;
begin
    if FirstTime then begin
        FirstTime          := FALSE;
        FIniFileName       := 'UdpSend';
        FSectionName       := 'Windows';
        FKeyName           := 'MainForm';
        LoadFormPos(Self, FIniFilename, FSectionName, FKeyName);
        IniFile            := TIniFile.Create(FIniFileName);
        PortEdit.Text      := IniFile.ReadString('data', 'Port',      '600');
        LocalPortEdit.Text := IniFile.ReadString('data', 'LocalPort', '0');
        MessageEdit.Text   := IniFile.ReadString('data', 'Message',   '');
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
    IniFile.WriteString('data', 'Port',      PortEdit.Text);
    IniFile.WriteString('data', 'LocalPort', LocalPortEdit.Text);
    IniFile.WriteString('data', 'Message',   MessageEdit.Text);
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.SendButtonClick(Sender: TObject);
begin
    WSocket.Proto      := 'udp';
    WSocket.Addr       := '255.255.255.255';
    WSocket.Port       := PortEdit.Text;
    WSocket.LocalPort  := LocalPortEdit.Text;
    WSocket.Connect;
    WSocket.SendStr(MessageEdit.Text);
    WSocket.Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.AnyPortCheckBoxClick(Sender: TObject);
begin
     if AnyPortCheckBox.Checked then
         LocalPortEdit.Text := '0';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMainForm.LocalPortEditChange(Sender: TObject);
begin
     AnyPortCheckBox.Checked := (LocalPortEdit.Text = '0');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

