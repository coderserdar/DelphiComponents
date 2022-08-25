{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François Piette
Creation:     Sep 02, 2001
Version:      1.03
Description:  Basic TCP server showing how to use TWSocketServer and
              TWSocketClient components with threads.
              This demo is mostly the same as TcpSrv demo but use a thread to
              run client code. This is needed if client operation is lengthy
              and blocking (such as a long database query) but otherwise will
              consume more CPU cycles in task switching and makes thing much
              more complexe because multithreading requires synchronization.
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2001-2010 by François PIETTE
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
Feb 24, 2002 V1.01 Wilfried Mestdagh <wilfried@mestdagh.biz> added ThreadDetach
Jun 20, 2004 V1.02 Fixed BannerToBusy error (BannerTooBusy).
Feb 28, 2010 V1.03 Arno Garrels fixed a deadlock.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsThrdSrv1;

interface

{$IFDEF VER80}
  'This sample program use threads and hence is not compatible with Delphi 1';
{$ENDIF}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, OverbyteIcsIniFiles, StdCtrls, ExtCtrls, OverbyteIcsWSocket, 
  OverbyteIcsWSocketS, OverbyteIcsWndControl;

const
  ThrdSrvVersion = 102;
  CopyRight      = ' ThrdSrv (c) 2001-2010 by François PIETTE. V1.03';
  WM_APPSTARTUP  = WM_USER + 1;
  WM_LOGMSG      = WM_USER + 2;

type
  TDisplayProc  = procedure (const Msg : String) of object;

  { TClientThread is our worker thread class. Each time a client connect, a }
  { new TClientThread is instanciated and client socket attached to it so   }
  { events are processed in the thread's context.                           }
  { Remember that multithreading requires synchronization, specially when   }
  { updating GUI or accessing shared data.                                  }
  {$M+}       { Needed for Published to take effect }
  TClientThread = class(TThread)
  private
      FWSocket        : TWSocket;             { Reference to client socket  }
      FOnDisplay      : TDisplayProc;         { Event variable              }
      FThreadAttached : Boolean;              { TRUE once socket attached   }
  public
      procedure Execute; override;            { Main method                 }
      procedure Display(const Msg : String);  { Takes care of synchroniz.   }
  published
      property WSocket   :      TWSocket     read  FWSocket
                                             write FWSocket;
      property ThreadAttached : Boolean      read  FThreadAttached
                                             write FThreadAttached;
      property OnDisplay :      TDisplayProc read  FOnDisplay
                                             write FOnDisplay;
  end;

  { TThrdSrvClient is the class which will be instanciated by server        }
  { component for each new client. N simultaneous clients means N           }
  { TThrdSrvClient will be instanciated. Each being used to handle only a   }
  { single client.                                                          }
  { We can add any data that has to be private for each client, such as     }
  { receive buffer or any other data needed for processing.                 }
  TThrdSrvClient = class(TWSocketClient)
  public
    ClientThread : TClientThread;
    RcvdLine     : String;
    ConnectTime  : TDateTime;
  end;

  { Application main from                                                   }
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
    procedure WSocketServer1ClientCreate(Sender: TObject;
      Client: TWSocketClient);
  protected
    procedure WmLogMessage(var Msg: TMessage); message WM_LOGMSG;
  private
    FIniFileName  : String;
    FInitialized  : Boolean;
    procedure Display(const Msg : String);
    procedure WMAppStartup(var Msg: TMessage); message WM_APPSTARTUP;
    procedure ClientDataAvailable(Sender: TObject; Error: Word);
    procedure ProcessData(Client : TThrdSrvClient);
    procedure ClientBgException(Sender       : TObject;
                                E            : Exception;
                                var CanClose : Boolean);
    procedure ClientLineLimitExceeded(Sender        : TObject;
                                      Cnt           : LongInt;
                                      var ClearData : Boolean);
{$IFDEF VER140}
    { Delphi 6 changed the rules about synchronization... }
    procedure WakeMainThread(Sender: TObject);
{$ENDIF}
  public
    property IniFileName : String read FIniFileName write FIniFileName;
  end;

var
  TcpSrvForm: TTcpSrvForm;

implementation

{$R *.DFM}
uses
    OverbyteIcsUtils;

const
    SectionWindow      = 'WindowTcpSrv';
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    KeyWidth           = 'Width';
    KeyHeight          = 'Height';

var
    GLogList     : TStringList;
    LogCritSect  : TRTLCriticalSection;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTcpSrvForm.FormCreate(Sender: TObject);
begin
    FIniFileName := OverbyteIcsIniFiles.GetIcsIniFileName;

{$IFDEF VER140}
    { With Delphi 6, we need to waken mainthread ourself !                  }
    Classes.WakeMainThread := Self.WakeMainThread;
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
        try
            Width        := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
            Height       := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
            Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                               (Screen.Height - Height) div 2);
            Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                               (Screen.Width  - Width)  div 2);
        finally
            IniFile.Free;
        end;
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
    try
        IniFile.WriteInteger(SectionWindow, KeyTop,         Top);
        IniFile.WriteInteger(SectionWindow, KeyLeft,        Left);
        IniFile.WriteInteger(SectionWindow, KeyWidth,       Width);
        IniFile.WriteInteger(SectionWindow, KeyHeight,      Height);
        IniFile.UpdateFile;
    finally
        IniFile.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Display a message in our display memo. Delete lines to be sure to not     }
{ overflow the memo which may have a limited capacity.                      }
procedure TTcpSrvForm.Display(const Msg : String);
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
        {$IFNDEF VER80}
        { Scroll to makes caret visible }
        SendMessage(DisplayMemo.Handle, EM_SCROLLCARET, 0, 0);
        {$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This is our custom message handler. We posted a WM_APPSTARTUP message     }
{ from FormShow event handler. Now UI is ready and visible.                 }
procedure TTcpSrvForm.WMAppStartup(var Msg: TMessage);
begin
    Display(Trim(CopyRight));                     { This demo version       }
    Display(Trim(OverbyteIcsWSocket.Copyright));             { TWSocket version        }
    Display(Trim(OverbyteIcsWSockets.CopyRight));            { TWSocketServer version  }
    Display('');
    Display('MainThreadID : $' + IntToHex(GetCurrentThreadID, 8));
    WSocketServer1.Proto       := 'tcp';          { Use TCP protocol        }
    WSocketServer1.Port        := 'telnet';       { Use telnet port         }
    WSocketServer1.Addr        := '0.0.0.0';      { Use any interface       }
    WSocketServer1.ClientClass := TThrdSrvClient; { Use our component       }
    WSocketServer1.Listen;                        { Start litening          }
    Display('Waiting for clients on port ''' + WSocketServer1.Port + '''...');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER140}
{ Delphi 6 requires a little help in order for TThread.Synchronize to work. }
{ We just post a do-nothing message to the form which will waken up the     }
{ maine thread and execute waiting synchronized procedures.                 }
procedure TTcpSrvForm.WakeMainThread(Sender: TObject);
begin
    PostMessage(Handle, WM_NULL, 0, 0);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TTcpSrvForm.WmLogMessage(var Msg: TMessage);
var
    I : Integer;
begin
    EnterCriticalSection(LogCritSect);
    try
        if GLogList.Count > 0 then begin
            for I := 0 to GLogList.Count -1 do
                Display(GLogList[I]);
            GLogList.Clear;
        end;    
    finally
        LeaveCriticalSection(LogCritSect);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Called in main thread context.                                            }
procedure TTcpSrvForm.WSocketServer1ClientCreate(
   Sender : TObject;
   Client : TWSocketClient);
begin
    with Client as TThrdSrvClient do begin
        Client.ThreadDetach;
        Client.MultiThreaded         := TRUE;
        ClientThread                 := TClientThread.Create(TRUE);
        ClientThread.FreeOnTerminate := TRUE;
        ClientThread.WSocket         := Client;
        ClientThread.OnDisplay       := Display;
        ClientThread.Suspended       := FALSE;
        { Wait until thread is started and has attached client socket to    }
        { his own context.                                                  }
        while not ClientThread.ThreadAttached do
            Sleep(0);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event is called each time a new client is connecting.                }
{ Called in main thread context.                                            }
procedure TTcpSrvForm.WSocketServer1ClientConnect(
    Sender : TObject;
    Client : TWSocketClient;
    Error  : Word);
begin
    with Client as TThrdSrvClient do begin
        Display('Client connected.' +
                ' Remote: '     + PeerAddr + '/' + PeerPort +
                ' Local: '      + GetXAddr + '/' + GetXPort +
                ' ThreadID : $' + IntToHex(ClientThread.ThreadID, 8));
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
{ This event handler is called each time a client disconnect.               }
{ This procedure is called in main thread context.                          }
procedure TTcpSrvForm.WSocketServer1ClientDisconnect(
    Sender : TObject;
    Client : TWSocketClient;
    Error  : Word);
begin
    with Client as TThrdSrvClient do begin
        Display('Client disconnecting: ' + PeerAddr + '   ' +
                'Duration: ' + FormatDateTime('hh:nn:ss',
                Now - ConnectTime) +
                ' ThreadID : $' + IntToHex(GetCurrentThreadID, 8));

        { Clear WSocket reference in worker thread }
        { ClientThread.WSocket := nil;             }
        { Break message pump within worker thread  }
        PostThreadMessage(ClientThread.ThreadID, WM_QUIT, 0, 0);
        { Allow up to 10 second for thread termination }
        WaitForSingleObject(ClientThread.Handle, 10000);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is called in client thread context. So be aware about GUI  }
{ update: you must use synchronize like ClientThread.Display does.          }
procedure TTcpSrvForm.ClientLineLimitExceeded(
    Sender        : TObject;
    Cnt           : LongInt;
    var ClearData : Boolean);
begin
    with Sender as TThrdSrvClient do begin
        ClientThread.Display('Line limit exceeded from ' + GetPeerAddr + '. Closing.');
        ClearData := TRUE;
        Close;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Each time a client has datavailable triggers this event handler.          }
{ We receive data (line mode) and execute "commands".                       }
{ This procedure is called in client thread context. So be aware about GUI  }
{ update: you must use synchronize like ClientThread.Display does.          }
procedure TTcpSrvForm.ClientDataAvailable(
    Sender : TObject;
    Error  : Word);
begin
    with Sender as TThrdSrvClient do begin
        { We use line mode. We will receive complete lines }
        RcvdLine := ReceiveStr;
        { Remove trailing CR/LF }
        while (Length(RcvdLine) > 0) and
              IsCharInSysCharset(RcvdLine[Length(RcvdLine)], [#13, #10]) do
            RcvdLine := Copy(RcvdLine, 1, Length(RcvdLine) - 1);
        ClientThread.Display('Received from ' + GetPeerAddr + ': ''' +
                             RcvdLine + ''' ' +
                             'ThreadID: $' + IntToHex(GetCurrentThreadID, 8));
        ProcessData(Sender as TThrdSrvClient);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is called in client thread context. So be aware about GUI  }
{ update: you must use synchronize.                                         }
procedure TTcpSrvForm.ProcessData(Client : TThrdSrvClient);
var
    I       : Integer;
    AClient : TThrdSrvClient;
begin
    { We could replace all those CompareText with a table lookup }
    if CompareText(Client.RcvdLine, 'help') = 0 then
        Client.SendStr('Commands are:' + #13#10 +
                       '  exit'        + #13#10 +
                       '  who'         + #13#10 +
                       '  sleep'       + #13#10 +
                       '  time'        + #13#10 )
                       //'  exception'   + #13#10)
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
            AClient := TThrdSrvClient(WSocketServer1.Client[I]);
            Client.SendStr(AClient.PeerAddr + ':' + AClient.GetPeerPort + ' ' +
                           DateTimeToStr(AClient.ConnectTime) + #13#10);
        end;
    end
    else if CompareText(Client.RcvdLine, 'Sleep') = 0 then begin
        Client.SendStr('Now sleeping for 15"...' + #13#10);
        Sleep(15000);
        Client.SendStr('Wakeup !' + #13#10);
    end
    (*
    else if CompareText(Client.RcvdLine, 'exception') = 0 then
        { This will trigger a background exception for client }
        PostMessage(Client.Handle, WM_TRIGGER_EXCEPTION, 0, 0)
    *)
    else
        if Client.State = wsConnected then
            Client.SendStr('Unknown command: ''' + Client.RcvdLine + '''' + #13#10);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when listening (server) socket experienced   }
{ a background exception. Should normally never occurs.                     }
{ This procedure is called in main thread context.                          }
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
{ Warning: This procedure is executed in worker thread context.             }
procedure TTcpSrvForm.ClientBgException(
    Sender       : TObject;
    E            : Exception;
    var CanClose : Boolean);
begin
    with Sender as TThrdSrvClient do begin
        ClientThread.Display('Client exception occured: ' +
                             E.ClassName + ': ' + E.Message);
    end;
    CanClose := TRUE;   { Goodbye client ! }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{*                                                                         *}
{*                          TClientThread                                  *}
{*                                                                         *}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This is our client worker thread main procedure. It is thread's code.     }
{ We have to attach client socket to this thread's context and then         }
{ process messages so that TWSocket events works.                           }
procedure TClientThread.Execute;
begin
    if not Assigned(WSocket) then
        Exit;

    { Attach client socket to this thread                                   }
    WSocket.ThreadAttach;
    { Signal main thread that we've attached socket to this thread          }
    ThreadAttached := TRUE;
    { Now let main thread continue starting the connection.                 }
    { This little avoid race condition.                                     }
    Sleep(0);
    { Then process messages until WM_QUIT message is posted.                }
    { TWSocket is event-driven. So even when used within a thread, we       }
    { have to have a "message pump". Any message pump will do and there     }
    { is one built in TWSocket, so use it !                                 }
    WSocket.MessageLoop;
    { Be sure to have main thread waiting for termination before terminating}
    Sleep(0);
    { Detach the hidden window from within the thread                       }
    WSocket.ThreadDetach;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is called from client thread and must display a message    }
{ on the GUI (main application form). As we are in a thread, we can't       }
{ simply call something that act on the GUI, we add the message to a        }
{ TStringList instance and notify the main thread by posting a custom       }
{ message.                                                                  }
procedure TClientThread.Display(const Msg: String);
begin
    { We protect access to the list using a critical section }
    EnterCriticalSection(LogCritSect);
    try
        GLogList.Add(Msg);
    finally
        LeaveCriticalSection(LogCritSect);
    end;
    PostMessage(TcpSrvForm.Handle, WM_LOGMSG, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
initialization
  InitializeCriticalSection(LogCritSect);
  GLogList := TStringList.Create;

finalization
  GLogList.Free;
  DeleteCriticalSection(LogCritSect);
  
end.

