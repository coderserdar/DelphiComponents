{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François Piette
Creation:     Aug 29, 1999
Version:      1.03
Description:  Basic TCP server showing how to use TWSocketServer and
              TWSocketClient components.
EMail:        francois.piette@pophost.eunet.be    francois.piette@rtfm.be
              francois.piette@swing.be            http://www.rtfm.be/fpiette
Support:      Use the mailing list twsocket@rtfm.be See website for details.
Legal issues: Copyright (C) 1999-2000 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@pophost.eunet.be><francois.piette@swing.be>

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

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.
History:
Sep 05, 1999 V1.01 Adapted for Delphi 1
Oct 15, 2000 V1.02 Display remote and local socket binding when a client
                   connect.
Nov 11, 2000 V1.03 Implemented OnLineLimitExceeded event

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit TcpSrv1;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, IniFiles, StdCtrls, ExtCtrls, WSocket, WSocketS;

const
  TcpSrvVersion = 103;
  CopyRight     = ' TcpSrv (c) 1999-2000 by François PIETTE. V1.03';
  WM_APPSTARTUP = WM_USER + 1;

type
  { TTcpSrvClient is the class which will be instanciated by server component }
  { for each new client. N simultaneous clients means N TTcpSrvClient will be }
  { instanciated. Each being used to handle only a single client.             }
  { We can add any data that has to be private for each client, such as       }
  { receive buffer or any other data needed for processing.                   }
  TTcpSrvClient = class(TWSocketClient)
  public
    RcvdLine    : String;
    ConnectTime : TDateTime;
  end;

  TTcpSrvForm = class(TForm)
    ToolPanel: TPanel;
    DisplayMemo: TMemo;
    WSocketServer1: TWSocketServer;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure WSocketServer1ClientConnect(Sender: TObject;
      Client: TWSocketClient; Error: Word);
    procedure WSocketServer1ClientDisconnect(Sender: TObject;
      Client: TWSocketClient; Error: Word);
    procedure WSocketServer1BgException(Sender: TObject; E: Exception;
      var CanClose: Boolean);
  private
    FIniFileName : String;
    FInitialized : Boolean;
    procedure Display(Msg : String);
    procedure WMAppStartup(var Msg: TMessage); message WM_APPSTARTUP;
    procedure ClientDataAvailable(Sender: TObject; Error: Word);
    procedure ProcessData(Client : TTcpSrvClient);
    procedure ClientBgException(Sender       : TObject;
                                E            : Exception;
                                var CanClose : Boolean);
    procedure ClientLineLimitExceeded(Sender        : TObject;
                                      Cnt           : LongInt;
                                      var ClearData : Boolean);
  public
    property IniFileName : String read FIniFileName write FIniFileName;
  end;

var
  TcpSrvForm: TTcpSrvForm;

implementation

{$R *.DFM}

const
    SectionWindow      = 'WindowTcpSrv';
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTcpSrvForm.FormCreate(Sender: TObject);
begin
    { Compute INI file name based on exe file name. Remove path to make it  }
    { go to windows directory.                                              }
    FIniFileName := LowerCase(ExtractFileName(Application.ExeName));
    FIniFileName := Copy(FIniFileName, 1, Length(FIniFileName) - 3) + 'ini';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTcpSrvForm.FormShow(Sender: TObject);
var
    IniFile : TIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

        { Fetch persistent parameters from INI file }
        IniFile      := TIniFile.Create(FIniFileName);
        Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                            (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                            (Screen.Width  - Width)  div 2);
        IniFile.Destroy;
        DisplayMemo.Clear;
        { Delay startup code until our UI is ready and visible }
        PostMessage(Handle, WM_APPSTARTUP, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTcpSrvForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIniFile;
begin
    { Save persistent data to INI file }
    IniFile := TIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
    IniFile.Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Display a message in our display memo. Delete lines to be sure to not     }
{ overflow the memo which may have a limited capacity.                      }
procedure TTcpSrvForm.Display(Msg : String);
var
    I : Integer;
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 200 then begin
            for I := 1 to 50 do
                DisplayMemo.Lines.Delete(0);
        end;
        DisplayMemo.Lines.Add(Msg);
    finally
        DisplayMemo.Lines.EndUpdate;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This is our custom message handler. We posted a WM_APPSTARTUP message     }
{ from FormShow event handler. Now UI is ready and visible.                 }
procedure TTcpSrvForm.WMAppStartup(var Msg: TMessage);
begin
    Display(CopyRight);
    Display(WSocket.Copyright);
    Display(WSockets.CopyRight);
    WSocketServer1.Proto       := 'tcp';         { Use TCP protocol  }
    WSocketServer1.Port        := 'telnet';      { Use telnet port   }
    WSocketServer1.Addr        := '0.0.0.0';     { Use any interface }
    WSocketServer1.ClientClass := TTcpSrvClient; { Use our component }
    WSocketServer1.Listen;                       { Start litening    }
    Display('Waiting for clients...');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTcpSrvForm.WSocketServer1ClientConnect(
    Sender : TObject;
    Client : TWSocketClient;
    Error  : Word);
begin
    with Client as TTcpSrvClient do begin
        Display('Client connected.' +
                ' Remote: ' + PeerAddr + '/' + PeerPort +
                ' Local: '  + GetXAddr + '/' + GetXPort);
        LineMode            := TRUE;
        LineEdit            := TRUE;
        LineLimit           := 80; { Do not accept long lines }
        OnDataAvailable     := ClientDataAvailable;
        OnLineLimitExceeded := ClientLineLimitExceeded;
        OnBgException       := ClientBgException;
        ConnectTime         := Now;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTcpSrvForm.WSocketServer1ClientDisconnect(
    Sender : TObject;
    Client : TWSocketClient;
    Error  : Word);
begin
    with Client as TTcpSrvClient do begin
        Display('Client disconnecting: ' + PeerAddr + '   ' +
                'Duration: ' + FormatDateTime('hh:nn:ss',
                Now - ConnectTime));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTcpSrvForm.ClientLineLimitExceeded(
    Sender        : TObject;
    Cnt           : LongInt;
    var ClearData : Boolean);
begin
    with Sender as TTcpSrvClient do begin
        Display('Line limit exceeded from ' + GetPeerAddr + '. Closing.');
        ClearData := TRUE;
        Close;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTcpSrvForm.ClientDataAvailable(
    Sender : TObject;
    Error  : Word);
begin
    with Sender as TTcpSrvClient do begin
        { We use line mode. We will receive complete lines }
        RcvdLine := ReceiveStr;
        { Remove trailing CR/LF }
        while (Length(RcvdLine) > 0) and
              (RcvdLine[Length(RcvdLine)] in [#13, #10]) do
            RcvdLine := Copy(RcvdLine, 1, Length(RcvdLine) - 1);
        Display('Received from ' + GetPeerAddr + ': ''' + RcvdLine + '''');
        ProcessData(Sender as TTcpSrvClient);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTcpSrvForm.ProcessData(Client : TTcpSrvClient);
var
    I       : Integer;
    AClient : TTcpSrvClient;
begin
    { We could replace all those CompareText with a table lookup }
    if CompareText(Client.RcvdLine, 'help') = 0 then
        Client.SendStr('Commands are:' + #10#13 +
                       '  exit' + #10#13 +
                       '  who' + #10#13 +
                       '  time' + #10#13 +
                       '  exception' + #10#13)
    else if CompareText(Client.RcvdLine, 'exit') = 0 then
        { We can't call Client.Close here because we will immediately }
        { reenter DataAvailable event handler with same line because  }
        { a line is removed from buffer AFTER it has been processed.  }
        { Using CloseDelayed will delay Close until we are out of     }
        { current event handler.                                      }
        Client.CloseDelayed
    else if CompareText(Client.RcvdLine, 'time') = 0 then
        { Send server date and time to client }
        Client.SendStr(DateTimeToStr(Now) + #13#10)
    else if CompareText(Client.RcvdLine, 'who') = 0 then begin
        { Send client list to client }
        Client.SendStr('There are ' + IntToStr(WSocketServer1.ClientCount) +
                       ' connected users:' + #13#10);
        for I := WSocketServer1.ClientCount - 1 downto 0 do begin
            AClient := TTcpSrvClient(WSocketServer1.Client[I]);
            Client.SendStr(AClient.PeerAddr + ':' + AClient.GetPeerPort + ' ' +
                           DateTimeToStr(AClient.ConnectTime) + #13#10);
        end;
    end
    else if CompareText(Client.RcvdLine, 'exception') = 0 then
        { This will trigger a background exception for client }
        PostMessage(Client.Handle, WM_TRIGGER_EXCEPTION, 0, 0)
    else
        if Client.State = wsConnected then
            Client.SendStr('Unknown command: ''' + Client.RcvdLine + '''' + #13#10);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when listening (server) socket experienced   }
{ a background exception. Should normally never occurs.                     }
procedure TTcpSrvForm.WSocketServer1BgException(
    Sender       : TObject;
    E            : Exception;
    var CanClose : Boolean);
begin
    Display('Server exception occured: ' + E.ClassName + ': ' + E.Message);
    CanClose := FALSE;  { Hoping that server will still work ! }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when a client socket experience a background }
{ exception. It is likely to occurs when client aborted connection and data }
{ has not been sent yet.                                                    }
procedure TTcpSrvForm.ClientBgException(
    Sender       : TObject;
    E            : Exception;
    var CanClose : Boolean);
begin
    Display('Client exception occured: ' + E.ClassName + ': ' + E.Message);
    CanClose := TRUE;   { Goodbye client ! }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

