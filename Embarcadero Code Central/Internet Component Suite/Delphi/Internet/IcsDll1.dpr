{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     April 03, 2000
Description:  This is a demo showing how to use a TWSocket component in a DLL.
              This demo must be used with ICS TcpSrv demo program as a server.
              The DLL is a client which connect to the server and send "time"
              command, then wait for the reply and return it in the buffer
              passed to the DLL.
              There is only one function exported from the DLL: IcsDllDemo.
              It takes four arguments: a pointer to the hostname to connect to,
              a pointer to the port, a pointer to a buffer and a pointer for
              buffer size. On entry buffer size must be initialised with the
              size of the actual buffer. On exit, it is filled with the
              actual reply size. The function's return value is the error code
              such as 10061 when the server is not running.
Version:      1.01
EMail:        francois.piette@pophost.eunet.be    francois.piette@swing.be
              francois.piette@rtfm.be             http://www.rtfm.be/fpiette
Support:      Unsupported code.
Legal issues: Copyright (C) 2000 by François PIETTE
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

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:
Apr 29, 2000 V1.01 Use WSocketForceLoadWinsock.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
library IcsDll1;

{$DEFINE NOFORMS}   // This will avoid forms unit and reduce DLL size
// You should add NOFORMS in the project options to be sure to have all units
// compiled with this option.

uses
  Windows, Messages, SysUtils, Classes, WSocket;

function IcsDllDemo(HostName : PChar;
                    Port     : PChar;
                    Buffer   : PChar;
                    BufSize  : PInteger): Integer; stdcall; forward;
procedure StrToBuffer(Buffer : PChar; BufSize : PInteger; Msg : String); forward;

exports
    IcsDllDemo;

type
  // We use a workerthread to do the job.
  // This will allows the DLL to be called by several processes simultaneously
  TClientThread = class(TThread)
  private
    FClientSocket   : TWSocket;
    FBannerReceived : Boolean;
    FErrorCode      : PInteger;
    FBuffer         : PChar;
    FBufSize        : PInteger;
    FHostName       : PChar;
    FPort           : PChar;
    procedure ClientWSocketDataAvailable(Sender: TObject; Error: Word);
    procedure ClientWSocketSessionConnected(Sender: TObject; Error: Word);
    procedure ClientWSocketSessionClosed(Sender: TObject; Error: Word);
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor  Destroy; override;

    property ClientWSocket : TWSocket read FClientSocket write FClientSocket;
    property Buffer        : PChar    read FBuffer       write FBuffer;
    property BufSize       : PInteger read FBufSize      write FBufSize;
    property ErrorCode     : PInteger read FErrorCode    write FErrorCode;
    property HostName      : PChar    read FHostName     write FHostName;
    property Port          : PChar    read FPort         write FPort;
  end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Create a new thread in the blocked state. This allow the user to register }
{ the client thread before it actually start working.                       }
constructor TClientThread.Create;
begin
    FreeOnTerminate := TRUE;
    inherited Create(TRUE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Destroy the thread. Destroy the ClientWSocket if needed.                  }
destructor TClientThread.Destroy;
begin
    if Assigned(FClientSocket) then begin
         FClientSocket.Destroy;
         FClientSocket := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This is the main thread routine. There is not much to do because TWSocket }
{ is event driven. So everythong to do is done inside an event handler,     }
{ mostly the OnDataAvailable event handler which is triggered each time     }
{ something is received.						    }
procedure TClientThread.Execute;
begin
    try
        { Create the client TWSocket. It is important to create it inside the }
        { Execute method because it *must* be created by the thread.          }
        { Otherwise the messages sent by winsock would be processed in the    }
        { main thread context, effectively disabling multi-threading.         }
        FClientSocket                    := TWSocket.Create(nil);
        FClientSocket.OnDataAvailable    := ClientWSocketDataAvailable;
        FClientSocket.OnSessionConnected := ClientWSocketSessionConnected;
        FClientSocket.OnSessionClosed    := ClientWSocketSessionClosed;
        FClientSocket.LineMode           := TRUE;
        FClientSocket.Addr               := FHostName;
        FClientSocket.Port               := FPort;
        FClientSocket.Proto              := 'tcp';
        FClientSocket.Connect;

        { Message loop to handle TWSocket messages                          }
        { The loop is exited when WM_QUIT message is received               }
        FClientSocket.MessageLoop;
    except
        on E:Exception do begin
            FErrorCode^ := -3;
            StrToBuffer(Buffer, BufSize, E.ClassName + ':' + E.Message);
        end;
    end;

    { Returning from the Execute function effectively terminate the thread  }
    ReturnValue := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when the client connection is established.   }
procedure TClientThread.ClientWSocketSessionConnected(
    Sender: TObject; Error: Word);
begin
    if Error <> 0 then begin
        FErrorCode^ := Error;
        StrToBuffer(Buffer, BufSize, 'Connect failed');
        PostMessage(FClientSocket.Handle, WM_QUIT, 0, 0);
        Exit;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when the client connection is closed.        }
procedure TClientThread.ClientWSocketSessionClosed(
    Sender: TObject; Error: Word);
begin
    PostMessage(FClientSocket.Handle, WM_QUIT, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when data has been received from server.     }
{ Since this sample program use line mode, we comes here only when a        }
{ complete line has been received.                                          }
procedure TClientThread.ClientWSocketDataAvailable(
    Sender: TObject;
    Error: Word);
var
    RcvBuffer : String;
begin
    // Received the line
    RcvBuffer := FClientSocket.ReceiveStr;
    // Check if we already received the banner (message sent by server
    // as soon as we are connected.
    if not FBannerReceived then begin
        // We are just receiving the banner. Flag as received
        FBannerReceived := TRUE;
        // Then send the command to the server
        FClientSocket.SendStr('time' + #13 + #10);
    end
    else begin
        // We already received then banner. So this must be the answer
        // to our command. Copy to the buffer, without trailling CR/LF
        // and without overflowing the given buffer
        if Length(RcvBuffer) < BufSize^ then
            BufSize^ := Length(RcvBuffer) - 2;  // Remove CR/LF
        if BufSize^ > 0 then
            Move(RcvBuffer[1], Buffer^, BufSize^);
        // Then just close the communication
        FClientSocket.CloseDelayed;
        FErrorCode^ := 0;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Copy a string to a buffer with overflow check.                            }
procedure StrToBuffer(Buffer : PChar; BufSize : PInteger; Msg : String);
begin
    if Length(Msg) < BufSize^ then
        BufSize^ := Length(Msg);
    if BufSize^ > 0 then
        Move(Msg[1], Buffer^, BufSize^);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsDllDemo(
    HostName : PChar;
    Port     : PChar;
    Buffer   : PChar;
    BufSize  : PInteger): integer; stdcall;
var
    WorkerThread : TClientThread;
begin
    try
        Result := -1;
        // Create a new thread. It is created in sleeping state
        WorkerThread           := TClientThread.Create;
        // Then pass all parameters
        WorkerThread.Buffer    := Buffer;
        WorkerThread.BufSize   := BufSize;
        WorkerThread.ErrorCode := @Result;
        WorkerThread.HostName  := HostName;
        WorkerThread.Port      := Port;
        // Then let thread start his work
        WorkerThread.Resume;
        // And wait until it finishes
        WaitForSingleObject(WorkerThread.Handle, INFINITE);
    except
        on E:Exception do begin
            Result := -2;
            StrToBuffer(Buffer, BufSize, E.ClassName + ': ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure DLLHandler(Reason: Integer);
begin
    if Reason = DLL_PROCESS_DETACH then begin
//      MessageBox(0, PChar('Reason = ' + IntToStr(Reason)), 'DLLHandler', MB_OK);
        WSocketCancelForceLoadWinsock;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
begin
//  MessageBox(0, PChar('DLL Init ' + IntToStr(WSocketGCount)), 'DLL', MB_OK);
    WSocketForceLoadWinsock;
    DLLProc := @DLLHandler;
end.
