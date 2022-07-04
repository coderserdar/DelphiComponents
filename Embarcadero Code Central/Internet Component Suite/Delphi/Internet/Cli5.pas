{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Object:       Simple client application demonstrating TWSocket object in action.
EMail:        francois.piette@pophost.eunet.be    
              francois.piette@rtfm.be             http://www.rtfm.be/fpiette
Creation:     September 21, 1996
Version:      2.06
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1996, 1997, 1998, 1999 by François PIETTE
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
Sep 06, 1997 Beautified
Nov 09, 1997 Added a button to display the list of IP addresses for the
             local computer (you can have two IP addresses if you are connected
             to a LAN and to your ISP).
Nov 11, 1997 V2.03 Added a ReadLine button to show how to read a single line
             synchronously.
Nov 18, 1997 V2.04 Show how to use ReceiveStr
Dec 05, 1998 V2.05 Don't use TWait component anymore
Aug 20, 1999 V2.06 Introduced FError to disply connection errors correctly.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit Cli5;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, Winsock, WSocket, StdCtrls;

const
  Client5Version        = 206;

type
  TClientForm = class(TForm)
    ConnectButton: TButton;
    CliSocket: TWSocket;
    InfoLabel: TLabel;
    DisconnectButton: TButton;
    DataLabel: TLabel;
    IPButton: TButton;
    ReadLineButton: TButton;
    procedure ConnectButtonClick(Sender: TObject);
    procedure DisconnectButtonClick(Sender: TObject);
    procedure CliSocketDataAvailable(Sender: TObject; Error: Word);
    procedure CliSocketSessionConnected(Sender: TObject; Error: Word);
    procedure CliSocketSessionClosed(Sender: TObject; Error: Word);
    procedure IPButtonClick(Sender: TObject);
    procedure ReadLineButtonClick(Sender: TObject);
  public
    FError : Word;
  end;

var
  ClientForm: TClientForm;

implementation

{$R *.DFM}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* This event handler gets called when we connected the server             *}
procedure TClientForm.CliSocketSessionConnected(Sender: TObject; Error: Word);
begin
    FError := Error; { Remember error code for SessionClosed event }
    if Error <> 0 then
        InfoLabel.Caption    := 'Connection failed, error #' + IntToStr(Error)
    else
        InfoLabel.Caption    := 'Connected';
    DisconnectButton.Enabled := TRUE;
    ConnectButton.Enabled    := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* This event handler gets called when the server's connection is broken   *}
{* Either by us or by the server.                                          *}
procedure TClientForm.CliSocketSessionClosed(Sender: TObject; Error: Word);
begin
    DataLabel.Caption        := '';
    if FError = 0 then begin
        { FError = 0 means we connected succesfully }
        if Error <> 0 then
            InfoLabel.Caption := 'Disconnected. Error #' + IntToStr(Error)
        else
            InfoLabel.Caption := 'Disconnected';
    end;
    DisconnectButton.Enabled := FALSE;
    ConnectButton.Enabled    := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* The user has clicked on the connect button...                           *}
procedure TClientForm.ConnectButtonClick(Sender: TObject);
begin
    CliSocket.Addr   := 'localhost';        { Server host name              }
    CliSocket.Proto  := 'tcp';              { Protocol we wants to use      }
    CliSocket.Port   := 'telnet';           { The port we wants to connect  }
    CliSocket.Connect;                      { Let's connect !               }
    { Connect is just a request, it returns immediately. We eventually gets }
    { gets connected later. At that time we will receive the event          }
    { SessionConnected. If you need a timeout, you have to start a TTimer.  }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{* The user has clicked the disconnect button...                           *}
procedure TClientForm.DisconnectButtonClick(Sender: TObject);
begin
    CliSocket.Close;                     { This will close the connection   }
    { When the connection will be effectively closed, we will receive the   }
    { SessionClosed even.                                                   }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.CliSocketDataAvailable(Sender: TObject; Error: Word);
begin
    DataLabel.Caption := CliSocket.ReceiveStr;
end;

{$IFDEF NEVER}
{ The same procedure using Receive: a little bit more complicated but }
{ more efficient because data is less copied from here to there.      }
procedure TClientForm.CliSocketDataAvailable(Sender: TObject; Error: Word);
var
    Buffer : String[200];
    Count  : Integer;
begin
    Count             := CliSocket.Receive(@Buffer[1], High(Buffer));
    Buffer[0]         := chr(Count);
    DataLabel.Caption := Buffer;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.IPButtonClick(Sender: TObject);
var
    IPList : TStrings;
    I      : Integer;
begin
    IPList := WSocket.LocalIPList;
    InfoLabel.Caption := '';
    for I := 0 to IPList.Count - 1 do
        InfoLabel.Caption := InfoLabel.Caption + '   ' + IPList.Strings[I];
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.ReadLineButtonClick(Sender: TObject);
var
    Buf : String;
begin
    ReadLineButton.Enabled := FALSE;
    try
        CliSocket.ReadLine(30, Buf);
        DataLabel.Caption      := 'Line: ''' + Buf + '''';
    finally
        ReadLineButton.Enabled := TRUE;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

