{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François Piette
Creation:     Aug 29, 1999
Version:      8.36
Description:  Basic TCP server showing how to use TWSocketServer and
              TWSocketClient components and how to send binary data
              which requires OverbyteIcsBinCliDemo as client application.
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1999-2016 by François PIETTE
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
Jun 15, 2010 V8.00 A.Garrels changed to demonstrate IPv6 and listening on
                   multiple interfaces.
Oct 26, 2016 V8.36 Angus added WSocketServer1Exception event which is triggered
                      instead of raising an exception with more details 

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsTcpSrv1IPv6;

interface

uses
  WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  OverbyteIcsIniFiles, StdCtrls, ExtCtrls,
  OverbyteIcsWSocket, OverbyteIcsWSocketS, OverbyteIcsWndControl,
  OverbyteIcsWinsock;

const
  TcpSrvVersion = 800;
  CopyRight     = ' TcpSrv (c) 1999-2014 by François PIETTE. V8.00';
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
    ListeningSocketsPortNum : Integer;
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
    procedure WSocketServer1SessionClosed(Sender: TObject; ErrCode: Word);
    procedure WSocketServer1SessionAvailable(Sender: TObject; ErrCode: Word);
    procedure WSocketServer1Exception(Sender: TObject;
      SocExcept: ESocketException);
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
{$IFDEF DELPHI10_UP}
    // BDS2006 has built-in memory leak detection and display
    ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
{$ENDIF}
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
const
    LMsg = '%s index #%d IP/port [%s]:%d State: %s';
var
    MyHostName : AnsiString;
    I: Integer;
    L: TWSocketMultiListenItem;
begin
    Display(CopyRight);
    Display(OverbyteIcsWSocket.Copyright);
    Display(OverbyteIcsWSocketS.CopyRight);
    WSocket_gethostname(MyHostName);
    Display(' I am "' + String(MyHostName) + '"');
    Display(' IP: ' + LocalIPList(sfAny).Text);
    WSocketServer1.Addr        := '0.0.0.0';
    WSocketServer1.Port        := 'telnet';
    WSocketServer1.ClientClass := TTcpSrvClient;

    WSocketServer1.Listen;

    WSocketServer1.MultiListenSockets.Clear;
    { Let's create some additional listening sockets }
    if OverbyteIcsWinsock.IsIPv6Available then begin
        with WSocketServer1.MultiListenSockets.Add do begin
            Addr := '::'; // IPv6 unspecified address, same as IPv4 "0.0.0.0"
            Port := 'telnet';
        end;
    end;
    with WSocketServer1.MultiListenSockets.Add do begin
        Addr := 'LocalHost';
        Port := '24';
     //   SocketFamily := sfAny; // OS preference of either IPv4 or IPv6.
        SocketFamily := sfIPv4; // OS preference of either IPv4 or IPv6.
    end;
    { Oct 2016 a duplicate to raise an exception } 
    with WSocketServer1.MultiListenSockets.Add do begin
        Addr := 'LocalHost';
        Port := '24';
        SocketFamily := sfIPv4;
    end;

    try
        { MultiListen attempts to listen on all listeners not yet listening  }
        { including main socket. Note that both Listen and MultiListen raise }
        { an exception if they fail, most likely this happens because        }
        { IP:Port is already in use. A real world application must handle    }
        { the exception, in this demo we just display it.                    }
        WSocketServer1.MultiListen;
    except
        on E: Exception do begin
            Display(E.ClassName + ' ' + E.Message);
        end;
    end;

    { Display status of all listeners. }
    Display(Format(LMsg, ['Main        ', -1, WSocketServer1.Addr,
                          WSocketServer1.PortNum,
                          SocketStateNames[WSocketServer1.State]]));
    for I := 0 to WSocketServer1.MultiListenSockets.Count -1 do begin
        L := WSocketServer1.MultiListenSockets[I];
        Display(Format(LMsg, ['Multi-listen', I, L.AddrResolved, L.PortNum,
                              SocketStateNames[L.State]]));
    end;

    { testing.. }
    {if (WSocketServer1.MultiListenSockets.Count > 1) then
        if WSocketServer1.MultiListenSockets[1].State = wsListening then
            WSocketServer1.MultiListenSockets[1].Close;}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTcpSrvForm.WSocketServer1ClientConnect(
    Sender : TObject;
    Client : TWSocketClient;
    Error  : Word);
begin
    with Client as TTcpSrvClient do begin
        Display('Client connected.' +
                ' Remote: [' + PeerAddr + ']:' + PeerPort +
                ' Local: ['  + GetXAddr + ']:' + GetXPort);
        Display('There is now ' +
                IntToStr(TWSocketServer(Sender).ClientCount) +
                ' clients connected.');
        LineMode            := TRUE;
        LineEdit            := TRUE;
        LineLimit           := 80; { Do not accept long lines }
        { We may want to check for the listening socket that  }
        { accepted this client and tag the client.            }
        if WSocketServer1.MultiListenIndex = -1 then
            { it's our main server's socket that accepted this client}
            ListeningSocketsPortNum := WSocketServer1.PortNum
        else
            { it's one of our additional sockets that accepted this client}
            ListeningSocketsPortNum := WSocketServer1.MultiListenSockets[
                WSocketServer1.MultiListenIndex].PortNum;
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
{ this event is triggered instead of raising an exception }
procedure TTcpSrvForm.WSocketServer1Exception(Sender: TObject;
  SocExcept: ESocketException);
begin
    Display('Server socket exception): ' + SocExcept.Message);
    Display('Server socket exception (friendly): ' + SocExcept.FriendlyMsg);
    Display('Exception details - Error: ' + SocExcept.ErrorMessage +
       ', Errnr: ' + IntToStr (SocExcept.ErrorCode) + ', Function: ' +
        SocExcept.Func + ', IP: ' + SocExcept.IPStr + ', Port: ' +
                   SocExcept.PortStr + ', Proto: ' + SocExcept.ProtoStr) ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event is triggered before a connection is accepted and before a      }
{ client object is instantiated.}
procedure TTcpSrvForm.WSocketServer1SessionAvailable(Sender: TObject;
  ErrCode: Word);
var
    L : TWSocketMultiListenItem;
begin
    with Sender as TWSocketServer do begin
        if MultiListenIndex = -1 then
            Display('Main socket received a connection request on [' + Addr +
                    ']:' + IntToStr(PortNum) +  ', error #' + IntToStr(ErrCode))
        else begin
           L := MultiListenSockets[MultiListenIndex];
           Display('Multi-listen socket index #' + IntToStr(MultiListenIndex) +
                   ' received a connection request on [' + L.AddrResolved +
                   ']:' + IntToStr(L.PortNum) + ', error #' + IntToStr(ErrCode));
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTcpSrvForm.WSocketServer1SessionClosed(
    Sender  : TObject;
    ErrCode : Word);
var
    L : TWSocketMultiListenItem;
begin
    with Sender as TWSocketServer do begin
        if MultiListenIndex = -1 then
            Display('Main session closed [' + Addr + ']:' + IntToStr(PortNum) +
                    ', error #' + IntToStr(ErrCode))
        else begin
           L := MultiListenSockets[MultiListenIndex];
           Display('Multi-listen session closed [' + L.AddrResolved + ']:' +
                   IntToStr(L.PortNum) + ', error #' + IntToStr(ErrCode));
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTcpSrvForm.ClientLineLimitExceeded(
    Sender        : TObject;
    Cnt           : LongInt;
    var ClearData : Boolean);
begin
    with Sender as TTcpSrvClient do begin
        Display('Line limit exceeded from [' + GetPeerAddr + ']. Closing.');
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
        Display('Received from [' + GetPeerAddr + '] : ''' + RcvdLine + '''');
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
                       //'  exception' + #13#10 +
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
        Client.SendStr('There are ' + IntToStr(TWSocketServer(Client.Server).ClientCount) +
                       ' connected users:' + #13#10);
        for I := TWSocketServer(Client.Server).ClientCount - 1 downto 0 do begin
            AClient := TTcpSrvClient(TWSocketServer(Client.Server).Client[I]);
            Client.SendStr(AClient.PeerAddr + ':' + AClient.GetPeerPort + ' ' +
                           DateTimeToStr(AClient.ConnectTime) + #13#10);
        end;
    end
//  else if CompareText(Client.RcvdLine, 'exception') = 0 then
        { This will trigger a background exception for client }
//      PostMessage(Client.Handle, Client.FMsg_WM_TRIGGER_EXCEPTION, 0, 0)
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

