{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Demonstration for Server program using TWSocket.
Creation:     8 december 1997
Version:      1.02
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2010 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
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
Dec 09, 1997 V1.01 Made it compatible with Delphi 1
Dec 28, 1998 V1.02 Use line mode to make it simpler.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSrvDemo1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, OverbyteIcsIniFiles, OverbyteIcsWSocket, OverbyteIcsSrvDemo2, Db, 
  DBTables, ExtCtrls, OverbyteIcsWndControl;

const
  IniFileName = 'SrvDemo.ini';

type
  TSrvForm = class(TForm)
    SrvSocket: TWSocket;
    ClientListBox: TListBox;
    DataTable: TTable;
    Panel1: TPanel;
    Label1: TLabel;
    PortEdit: TEdit;
    PortButton: TButton;
    procedure FormShow(Sender: TObject);
    procedure SrvSocketSessionAvailable(Sender: TObject; Error: Word);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure PortButtonClick(Sender: TObject);
  private
    Initialized  : Boolean;
    ClientNumber : Integer;
    procedure   WMUser(var msg: TMessage); message WM_USER;
    procedure   StartServer;
  end;

var
  SrvForm: TSrvForm;

implementation

{$R *.DFM}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSrvForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
    Buffer  : String;
begin
    if not Initialized then begin
        Initialized     := TRUE;
        IniFile         := TIcsIniFile.Create(IniFileName);
        Top             := IniFile.ReadInteger('Window', 'Top',    Top);
        Left            := IniFile.ReadInteger('Window', 'Left',   Left);
        Width           := IniFile.ReadInteger('Window', 'Width',  Width);
        Height          := IniFile.ReadInteger('Window', 'Height', Height);
        PortEdit.Text   := IniFile.ReadString('Data',    'Port',   'telnet');
        IniFile.Free;

        DataTable.DataBaseName := ExtractFilePath(Application.ExeName);
        try
            DataTable.Open;
        except
            Buffer := 'Unable to open ' + DataTable.DataBaseName +
                      DataTable.TableName + #0;
            Application.MessageBox(@Buffer[1], 'Error', MB_OK);
            Application.Terminate;
            Exit;
        end;
        StartServer;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSrvForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(IniFileName);
    IniFile.WriteInteger('Window', 'Top',    Top);
    IniFile.WriteInteger('Window', 'Left',   Left);
    IniFile.WriteInteger('Window', 'Width',  Width);
    IniFile.WriteInteger('Window', 'Height', Height);
    IniFile.WriteString('Data', 'Port',    PortEdit.Text);
    IniFile.Free;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSrvForm.PortButtonClick(Sender: TObject);
begin
    StartServer;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSrvForm.StartServer;
begin
    SrvSocket.Close;
    SrvSocket.Addr  := '0.0.0.0';
    SrvSocket.Port  := PortEdit.Text;
    SrvSocket.Proto := 'tcp';
    SrvSocket.Listen;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSrvForm.SrvSocketSessionAvailable(Sender: TObject; Error: Word);
var
    Form    : TCliForm;
begin
    Inc(ClientNumber);
    Form := TCliForm.Create(self);
    { Add the form address as an identifier in our client list }
    ClientListBox.Items.Add(IntToStr(LongInt(Form)));
    { We request line mode, to receive only complete line. }
    { TWSocket does all the job for us...                  }
    Form.CliSocket.LineMode := TRUE;
    Form.CliSocket.LineEnd  := #13#10;
    { Now accept the new client connection }
    Form.CliSocket.HSocket := SrvSocket.Accept;
    Form.DataTable         := DataTable;
    Form.Caption           := 'Client ' + IntToStr(ClientNumber);
    { Showing the form is not mandatory. In a real server, this can be      }
    { annoying to have a form displayed for each client. In some situation, }
    { it may be handy to have a user interface for each connected client.   }
    Form.Show;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSrvForm.WMUser(var msg: TMessage);
var
    Form : TCliForm;
    I    : Integer;
begin
    Form := TCliForm(Msg.lParam);
    Form.Release;
    for I := 0 to ClientListBox.Items.Count - 1 do begin
        if ClientListBox.Items[I] = IntToStr(LongInt(Form)) then begin
            ClientListBox.Items.Delete(I);
            break;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

