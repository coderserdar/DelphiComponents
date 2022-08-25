{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François Piette
Creation:     Nov 05, 2005
Version:      3.00
Description:  Basic TCP server showing how to use TWSocketThrdServer
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2005-2010 by François PIETTE
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

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.
History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsThrdSrvV3_1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, OverbyteIcsIniFiles, StdCtrls, ExtCtrls, OverbyteIcsWSocket, OverbyteIcsWSocketS,
  OverbyteIcsWSocketTS, OverbyteIcsWndControl;

const
  TcpSrvVersion  = 300;
  CopyRight      = ' TcpSrv (c) 2005-2010 by François PIETTE. V3.00';
  WM_APPSTARTUP  = WM_USER + 1;
  WM_LOG_MESSAGE = WM_USER + 2;
var
  LockDisplay : TRtlCriticalSection;

type

  TMyClient = class(TWSocketThrdClient)
  public
    RcvdLine    : String;
    ConnectTime : TDateTime;
  end;

  TThrdSrvForm = class(TForm)
    ToolPanel: TPanel;
    DisplayMemo: TMemo;
    ClientsPerThreadEdit: TEdit;
    Label1: TLabel;
    DisconnectAllButton: TButton;
    ClearMemoButton: TButton;
    WSocketThrdServer1: TWSocketThrdServer;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure WSocketThrdServer1ClientConnect(Sender: TObject;
      Client: TWSocketClient; Error: Word);
    procedure WSocketThrdServer1ClientDisconnect(Sender: TObject;
      Client: TWSocketClient; Error: Word);
    procedure WSocketThrdServer1BgException(Sender: TObject; E: Exception;
      var CanClose: Boolean); 
    procedure FormDestroy(Sender: TObject);
    procedure WSocketThrdServer1ClientCreate(Sender: TObject;
      Client: TWSocketClient);
    procedure ClientsPerThreadEditChange(Sender: TObject);
    procedure DisconnectAllButtonClick(Sender: TObject);
    procedure ClearMemoButtonClick(Sender: TObject);
    procedure WSocketThrdServer1ThreadException(Sender: TObject;
      AThread: TWsClientThread; const AErrMsg: String);
  private
    FIniFileName : String;
    FInitialized : Boolean;
    FLogList  : TStringList;
    procedure Display(const Msg : String);
    procedure WmLogMessage(var Msg: TMessage); message WM_LOG_MESSAGE;
    procedure WMAppStartup(var Msg: TMessage); message WM_APPSTARTUP;
    procedure ClientDataAvailable(Sender: TObject; Error: Word);
    procedure ProcessData(Client : TMyClient);
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
  ThrdSrvForm : TThrdSrvForm;

implementation

{$R *.DFM}
uses
    OverbyteIcsUtils;

const
    SectionWindow      = 'WindowTcpSrv';
    SectionMain        = 'Main';
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';
    KeyCliPerThrd      = 'ClientsPerThread';


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TThrdSrvForm.FormCreate(Sender: TObject);
begin
    FIniFileName := OverbyteIcsIniFiles.GetIcsIniFileName;
    FLogList     := nil;
    FLogList     := TStringList.Create;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TThrdSrvForm.FormDestroy(Sender: TObject);
begin
    EnterCriticalSection(LockDisplay);
    try
        if Assigned(FLogList) then
            FLogList.Free;
    finally
        LeaveCriticalSection(LockDisplay);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TThrdSrvForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        { Fetch persistent parameters from INI file }
        IniFile      := TIcsIniFile.Create(FIniFileName);
        try
            Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
            Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
            Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                               (Screen.Height - Height) div 2);
            Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                               (Screen.Width  - Width)  div 2);
            ClientsPerThreadEdit.Text :=  IniFile.ReadString(SectionMain,
                                                           KeyCliPerThrd, '2');
        finally
            IniFile.Free;
        end;
        DisplayMemo.Clear;
        { Delay startup code until our UI is ready and visible }
        PostMessage(Handle, WM_APPSTARTUP, 0, 0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TThrdSrvForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    { Save persistent data to INI file }
    IniFile := TIcsIniFile.Create(FIniFileName);
    try
        IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
        IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
        IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
        IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
        IniFile.WriteString(SectionMain,    KeyCliPerThrd,  ClientsPerThreadEdit.Text);
        IniFile.UpdateFile;
    finally
        IniFile.Free;
    end;
end;

{ Display a message in our display memo. Delete lines to be sure to not     }
{ overflow the memo which may have a limited capacity.                      }
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TThrdSrvForm.WmLogMessage(var Msg: TMessage);
var
    I : Integer;
begin
    DisplayMemo.Lines.BeginUpdate;
    try
        if DisplayMemo.Lines.Count > 200 then begin
            for I := 1 to 50 do
                DisplayMemo.Lines.Delete(0);
        end;
        EnterCriticalSection(LockDisplay);
        try
            DisplayMemo.Lines.AddStrings(FLogList);
            FLogList.Clear;
        finally
            LeaveCriticalSection(LockDisplay);
        end;
    finally
        DisplayMemo.Lines.EndUpdate;
        DisplayMemo.Perform(EM_SCROLLCARET, 0, 0);
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TThrdSrvForm.Display(const Msg : String);
begin
    EnterCriticalSection(LockDisplay);
    try
        FLogList.Add(Msg);
        PostMessage(Handle, WM_LOG_MESSAGE, 0, 0);
    finally
        LeaveCriticalSection(LockDisplay);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This is our custom message handler. We posted a WM_APPSTARTUP message     }
{ from FormShow event handler. Now UI is ready and visible.                 }
procedure TThrdSrvForm.WMAppStartup(var Msg: TMessage);
begin
    Display(CopyRight);
    Display(OverbyteIcsWSocket.Copyright);
    Display(OverbyteIcsWSockets.CopyRight);
    WSocketThrdServer1.Proto       := 'tcp';                { Use TCP protocol  }
    WSocketThrdServer1.Port        := 'telnet';             { Use telnet port   }
    WSocketThrdServer1.Addr        := '0.0.0.0';            { Use any interface }
    WSocketThrdServer1.ClientClass := TMyClient;            { Use our component }
    WSocketThrdServer1.Listen;                              { Start listening    }
    Display('Waiting for clients...');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TThrdSrvForm.WSocketThrdServer1ClientCreate(Sender: TObject;
  Client: TWSocketClient);
var
    Cli : TMyClient;
begin
    Cli := Client as  TMyClient;
    Cli.LineMode            := TRUE;
    Cli.LineEdit            := TRUE;
    Cli.LineLimit           := 255; { Do not accept long lines }
    Cli.OnDataAvailable     := ClientDataAvailable;
    Cli.OnLineLimitExceeded := ClientLineLimitExceeded;
    Cli.OnBgException       := ClientBgException;
    //TMyClientThread(Cli.ClientThread).OnDisplay := Display;
    Cli.ConnectTime         := Now;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TThrdSrvForm.WSocketThrdServer1ClientConnect(
    Sender : TObject;
    Client : TWSocketClient;
    Error  : Word);
begin
    with Client as TMyClient do begin
        Display('Client connected.' +
                ' Remote: ' + PeerAddr + '/' + PeerPort +
                ' Local: '  + GetXAddr + '/' + GetXPort +
                ' ThrdID : $' + IntToStr(ClientThread.ThreadID) +
                ' ThrdCnt: #' + IntToStr(WSocketThrdServer1.ThreadCount) + #13#10 +
                'There is now ' +
                IntToStr(TWSocketThrdServer(Sender).ClientCount) +
                ' clients connected.');

        Client.LineMode            := TRUE;
        Client.LineEdit            := TRUE;
        Client.LineLimit           := 255; { Do not accept long lines }
        Client.OnDataAvailable     := ClientDataAvailable;
        Client.OnLineLimitExceeded := ClientLineLimitExceeded;
        Client.OnBgException       := ClientBgException;
        TMyClient(Client).ConnectTime  := Now;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TThrdSrvForm.WSocketThrdServer1ClientDisconnect(
    Sender : TObject;
    Client : TWSocketClient;
    Error  : Word);
var
    MyClient       : TMyClient;
    ClientThreadID : Integer;
begin
    MyClient := Client as TMyClient;
    if Assigned(MyClient.ClientThread) then
        ClientThreadID := MyClient.ClientThread.ThreadID
    else
        ClientThreadID := -1;
    Display('Client disconnecting: ' + MyClient.PeerAddr + '   ' +
            'Duration: ' + FormatDateTime('hh:nn:ss',
            Now - MyClient.ConnectTime) + ' Error: ' + IntTostr(Error) +
            ' ThrdID: $' + IntToStr(ClientThreadID) +
            ' ThrdCnt: #' + IntToStr(TWSocketThrdServer(Sender).ThreadCount) + #13#10 +
            'There is now ' +
            IntToStr(TWSocketThrdServer(Sender).ClientCount - 1) +
            ' clients connected.');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TThrdSrvForm.ClientLineLimitExceeded(
    Sender        : TObject;
    Cnt           : LongInt;
    var ClearData : Boolean);
begin
    with Sender as TMyClient do begin
        Display('Line limit exceeded from ' + GetPeerAddr + '. Closing.');
        ClearData := TRUE;
        Close;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TThrdSrvForm.ClientDataAvailable(
    Sender : TObject;
    Error  : Word);
var
    Cli : TMyClient;
begin
    Cli := Sender as TMyClient;
    { We use line mode. We will receive complete lines }
    Cli.RcvdLine := Cli.ReceiveStr;
        { Remove trailing CR/LF }
    while (Length(Cli.RcvdLine) > 0) and
              IsCharInSysCharSet(Cli.RcvdLine[Length(Cli.RcvdLine)], [#13, #10]) do
            Cli.RcvdLine := Copy(Cli.RcvdLine, 1, Length(Cli.RcvdLine) - 1);
    Display('Received from ' + Cli.GetPeerAddr + ': ''' + Cli.RcvdLine + '''');
    ProcessData(Cli);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TThrdSrvForm.ProcessData(Client : TMyClient);
var
    I       : Integer;
    AClient : TMyClient;
begin
    { We could replace all those CompareText with a table lookup }
    if CompareText(Client.RcvdLine, 'help') = 0 then
        Client.SendStr('Commands are:' + #13#10 +
                       '  exit' + #13#10 +
                       '  who' + #13#10 +
                       '  time' + #13#10 +
                       '  threadexception' + #13#10 )
                       //'  exception' + #13#10 ) 
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
        Client.SendStr('There are ' + IntToStr(WSocketThrdServer1.ClientCount) +
                       ' connected users:' + #13#10);
        for I := WSocketThrdServer1.ClientCount - 1 downto 0 do begin
            AClient := TMyClient(WSocketThrdServer1.Client[I]);
            Client.SendStr(AClient.PeerAddr + ':' + AClient.GetPeerPort + ' ' +
                           DateTimeToStr(AClient.ConnectTime) + #13#10);
        end;
    end
    (*
    else if CompareText(Client.RcvdLine, 'exception') = 0 then
        { This will trigger a background exception for client }
        PostMessage(Client.Handle, WM_TRIGGER_EXCEPTION, 0, 0)
    *)
    else if CompareText(Client.RcvdLine, 'threadexception') = 0 then
        { This will trigger a background exception for client }
        PostThreadMessage(GetCurrentThreadID, WM_THREAD_EXCEPTION_TEST, 0, 0)
    else
        if Client.State = wsConnected then
            Client.SendStr('Unknown command: ''' + Client.RcvdLine + '''' + #13#10);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when listening (server) socket experienced   }
{ a background exception. Should normally never occurs.                     }
procedure TThrdSrvForm.WSocketThrdServer1BgException(
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
procedure TThrdSrvForm.ClientBgException(
    Sender       : TObject;
    E            : Exception;
    var CanClose : Boolean);
begin
    with Sender as TMyClient do begin
        Display('Client exception occured: ' + E.ClassName + ': ' + E.Message);
        CanClose := TRUE;   { Goodbye client ! }
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TThrdSrvForm.ClientsPerThreadEditChange(Sender: TObject);
var
    Num : Integer;
begin
    try
        Num := StrToInt((Sender as TEdit).Text);
    except
        Num := 1;
    end;
    WSocketThrdServer1.ClientsPerThread := Num;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TThrdSrvForm.DisconnectAllButtonClick(Sender: TObject);
begin
    WSocketThrdServer1.DisconnectAll;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TThrdSrvForm.ClearMemoButtonClick(Sender: TObject);
begin
    DisplayMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TThrdSrvForm.WSocketThrdServer1ThreadException(
    Sender        : TObject;
    AThread       : TWsClientThread;
    const AErrMsg : String);
begin
    Display(AErrMsg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
initialization
    InitializeCriticalSection(LockDisplay);


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
finalization
    DeleteCriticalSection(LockDisplay);


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.

