{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François Piette
Creation:     Aug 29, 1999
Version:      7.01
Description:  Basic TCP server showing how to use TWSocketServer and
              TWSocketClient components and how to send binary data
              which requires OverbyteIcsBinCliDemo as client application.
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1999-2010 by François PIETTE
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

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.
History:
Sep 05, 1999 V1.01 Adapted for Delphi 1
Oct 15, 2000 V1.02 Display remote and local socket binding when a client
                   connect.
Nov 11, 2000 V1.03 Implemented OnLineLimitExceeded event
Dec 15, 2001 V1.03 In command help changed #10#13 to the correct value #13#10.
Jul 19, 2008 V6.00 F.Piette made some changes for Unicode
Nov 28, 2008 V7.01 A.Garrels added command binary, requires OverbyteIcsBinCliDemo.
Dec 20, 2008 V7.02 F.Piette removed an implicit string conversion warning in
                   WMAppStartup (Hostname).
May 2012 - V8.00 - this is a Windows only demo, IPv4 only

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsTcpSrv1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  OverbyteIcsIniFiles, StdCtrls, ExtCtrls,
  OverbyteIcsWSocket, OverbyteIcsWSocketS, OverbyteIcsWndControl;

const
  TcpSrvVersion = 800;
  CopyRight     = ' TcpSrv (c) 1999-2012 by François PIETTE. V8.00';
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

  { This record is prepended to binary data }
  PHdrRec = ^THdrRec;
  THdrRec = record
    case Integer of
      0: (
        ID1     : Byte;
        ID2     : Byte;
        ID3     : Byte;
        ID4     : Byte;
        SizeLo  : Word;
        SizeHi  : Word);
      1: (
        ID    : Longint;
        Size  : Longint);
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
    FIniFileName := GetIcsIniFileName;
{$IF COMPILERVERSION > 17}
    // BDS2006 has built-in memory leak detection and display
    ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
{$IFEND}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTcpSrvForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

        { Fetch persistent parameters from INI file }
        IniFile      := TIcsIniFile.Create(FIniFileName);
        Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
        Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                            (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                            (Screen.Width  - Width)  div 2);
        IniFile.Free;
        DisplayMemo.Clear;
        { Delay startup code until our UI is ready and visible }
        PostMessage(Handle, WM_APPSTARTUP, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTcpSrvForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    { Save persistent data to INI file }
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
    IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
    IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
    IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
    IniFile.UpdateFile;
    IniFile.Free;
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
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This is our custom message handler. We posted a WM_APPSTARTUP message     }
{ from FormShow event handler. Now UI is ready and visible.                 }
procedure TTcpSrvForm.WMAppStartup(var Msg: TMessage);
var
    MyHostName : AnsiString;
begin
    Display(CopyRight);
    Display(OverbyteIcsWSocket.Copyright);
    Display(OverbyteIcsWSocketS.CopyRight);
    WSocket_gethostname(MyHostName);
    Display(' I am "' + String(MyHostName) + '"');
    Display(' IP: ' + LocalIPList.Text);
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
        Display('There is now ' +
                IntToStr(TWSocketServer(Sender).ClientCount) +
                ' clients connected.');
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
        Display('There is now ' +
                IntToStr(TWSocketServer(Sender).ClientCount - 1) +
                ' clients connected.');
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
              ((RcvdLine[Length(RcvdLine)] = #13) or
               (RcvdLine[Length(RcvdLine)] = #10)) do
            RcvdLine := Copy(RcvdLine, 1, Length(RcvdLine) - 1);
        Display('Received from ' + GetPeerAddr + ': ''' + RcvdLine + '''');
        ProcessData(Sender as TTcpSrvClient);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTcpSrvForm.ProcessData(Client : TTcpSrvClient);
var
    I       : Integer;
    P       : Pointer; 
    AClient : TTcpSrvClient;
begin
    { We could replace all those CompareText with a table lookup }
    if CompareText(Client.RcvdLine, 'help') = 0 then
        Client.SendStr('Commands are:' + #13#10 +
                       '  exit' + #13#10 +
                       '  who' + #13#10 +
                       '  time' + #13#10 +
                       '  exception' + #13#10 +
                       '  binary [size]' + #13#10)
    else if CompareText(Copy(Client.RcvdLine, 1, 6), 'binary') = 0 then
    begin
        I := StrToIntDef(Copy(Client.RcvdLine, 7, MaxInt), 0);
        if I <= 0 then
            Client.SendStr('500 Error binary size not spezified'#13#10)
        else begin
            if I > MaxWord then
            begin
                Client.SendStr('500 Error binary size limited to ' +
                               IntToStr(MaxWord) + ' bytes'#13#10);
                Exit;
            end
            else
                Client.SendStr('200 OK Binary ' + IntToStr(I) +
                           ' bytes requested'#13#10);
            Inc(I, SizeOf(THdrRec));
            GetMem(P, I);
            try
                FillChar(P^, I, '1');
                PHdrRec(P)^.ID      := 0; // any value < 32 marks = valid binary data.
                PHdrRec(P)^.Size    := I - SizeOf(THdrRec);
                PAnsiChar(P)[I - 1] := 'E';
                Client.Send(P, I);
            finally
                FreeMem(P);
            end;    
        end;
    end                   
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
        PostMessage(Client.Handle, Client.FMsg_WM_TRIGGER_EXCEPTION, 0, 0)
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

