{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     December 12, 2004
Description:  This is a demo showing how to use a THttpCli component in a DLL.
              The DLL is a HTTP client which get an URL and returns the document
              in the supplied buffer.
              There is only one function exported from the DLL: IcsDllDemo.
              It takes 3 arguments: a pointer to the URL to get (nul
              terminated string), a pointer to the document buffer and a
              pointer for buffer size. On entry buffer size must be initialised
              with the size of the actual document buffer. On exit, it is
              filled with the actual bytes in the document. If the supplied
              buffer is too short, then it will conatins partial document
              and buffer size will return a negative number which is the
              required size.
              The function's return value is the error code such as 404 when
              the document pointed by the URL is not found.
              To debug the DLL, enter DllTst1.exe as a host application into
              the run parameters.
Version:      1.02
EMail:        http://www.overbyte.be        francois.piette@overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2004-2017 by François PIETTE
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
Jan 29, 2012 V1.01 Arno fixed it.
Apr 15, 2017 V1.02 FPiette - Removed compiler warning


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
library OverbyteIcsDll2;

{$DEFINE NOFORMS}   // This will avoid forms unit and reduce DLL size
// You should add NOFORMS in the project options to be sure to have all units
// compiled with this option, specially wsocket.pas.

{$I OVERBYTEICSDEFS.INC}
{$IFDEF DELPHI25_UP}
{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  OverbyteIcsWinsock,
  OverbyteIcsHttpProt,
  OverbyteIcsWSocket;

const
  IcsDll2Version            = 101;
  CopyRight    : String     = ' IcsDll2 (c) 2012 Francois Piette V1.01 ';

// If you use strings or other dynamically allocated data between the DLL and
// the main program, then you _must_ use ShareMem unit as explained in Delphi
// documentation.
// Here we use only basic data types so that our DLL is usable with any language
// able to call a DLL.
function IcsDllDemo(URL      : PAnsiChar;
                    Buffer   : PAnsiChar;
                    BufSize  : PInteger): Integer; stdcall; forward;
procedure StrToBuffer(Buffer : PAnsiChar; BufSize : PInteger; Msg : AnsiString); forward;

exports
    IcsDllDemo;

type
  // We use a workerthread to do the job.
  // This will allows the DLL to be called by several processes simultaneously
  TClientThread = class(TThread)
  private
    FHttpCli        : THttpCli;
    FUrl            : PAnsiChar;
    FErrorCode      : PInteger;
    FBuffer         : PAnsiChar;
    FBufSize        : PInteger;
    FReady          : Boolean;
    procedure HttpCliRequestDone(Sender: TObject; RqType: THttpRequest;
                                 ErrCode: Word);
  protected
    procedure Execute; override;
  public
    constructor Create;
    property HttpCli       : THttpCli  read FHttpCli      write FHttpCli;
    property Url           : PAnsiChar read FUrl          write FUrl;
    property Buffer        : PAnsiChar read FBuffer       write FBuffer;
    property BufSize       : PInteger  read FBufSize      write FBufSize;
    property ErrorCode     : PInteger  read FErrorCode    write FErrorCode;
  end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Create a new thread in the blocked state. This allow the user to register }
{ the client thread before it actually start working.                       }
constructor TClientThread.Create;
begin
    inherited Create(TRUE);
    FreeOnTerminate := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This is the main thread routine. There is not much to do because THttpCli }
{ is event driven. So everythong to do is done inside an event handler.     }
procedure TClientThread.Execute;
begin
    FReady := True;
    try
        { Create the HTTP component. It is important to create it inside the  }
        { Execute method because it *must* be created by the thread.          }
        { Otherwise the messages sent by winsock would be processed in the    }
        { main thread context, effectively disabling multi-threading.         }
        FHttpCli                    := THttpCli.Create(nil);
        try
            FHttpCli.Url                := String(StrPas(FUrl));
            FHttpCli.OnRequestDone      := HttpCliRequestDone;
            FHttpCli.RcvdStream         := TMemoryStream.Create;
            FHttpCli.GetAsync;

            { Message loop to handle all messages                               }
            { The loop is exited when WM_QUIT message is received               }
            FHttpCli.CtrlSocket.MessageLoop;
        finally
            FHttpCli.Free;
        end;
    except
        on E:Exception do begin
            FErrorCode^ := -3;
            StrToBuffer(Buffer, BufSize, AnsiString(E.ClassName + ':' + E.Message));
        end;
    end;

    { Returning from the Execute function effectively terminate the thread  }
    ReturnValue := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when the client connection is established.   }
procedure TClientThread.HttpCliRequestDone(
    Sender  : TObject;
    RqType  : THttpRequest;
    ErrCode : Word);
var
    N : Integer;
begin
    if ErrCode <> 0 then begin
        // Failure
        FErrorCode^ := ErrCode;
        StrToBuffer(Buffer, BufSize, AnsiString(FHttpCli.ReasonPhrase));
    end
    else if FHttpCli.StatusCode <> 200 then begin
        FErrorCode^ := FHttpCli.StatusCode;
        StrToBuffer(Buffer, BufSize, AnsiString(FHttpCli.ReasonPhrase));
    end
    else begin
        // Success
        FErrorCode^ := FHttpCli.StatusCode;
        N := FHttpCli.RcvdStream.Size;
        if N > FBufSize^ then begin
            // Supplied buffer is too small
            N         := FBufSize^;                 // Truncate length to copy
            FBufSize^ := -FHttpCli.RcvdStream.Size; // Return negative size
        end
        else
            FBufSize^ := N;                         // Return document size
        // Copy data to buffer
        Move(TMemoryStream(FHttpCli.RcvdStream).Memory^, Buffer^, N);
    end;
    // Free receive stream
    FHttpCli.RcvdStream.Free;
    FHttpCli.RcvdStream := nil;
    PostMessage(FHttpCli.CtrlSocket.Handle, WM_QUIT, 0, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Copy a string to a buffer with overflow check.                            }
procedure StrToBuffer(Buffer : PAnsiChar; BufSize : PInteger; Msg : AnsiString);
begin
    if Length(Msg) < BufSize^ then
        BufSize^ := Length(Msg);
    if BufSize^ > 0 then
        Move(Msg[1], Buffer^, BufSize^);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IcsDllDemo(
    Url      : PAnsiChar;
    Buffer   : PAnsiChar;
    BufSize  : PInteger): integer; stdcall;
var
    WorkerThread : TClientThread;
begin
    try
        Result := -1;
        WSocketForceLoadWinsock;
        // Create a new thread. It is created in sleeping state
        WorkerThread           := TClientThread.Create;
        // Then pass all parameters
        WorkerThread.Buffer    := Buffer;
        WorkerThread.BufSize   := BufSize;
        WorkerThread.ErrorCode := @Result;
        WorkerThread.Url       := Url;
        // Then let thread start his work
    {$if RTLVersion >= 21}
        WorkerThread.Start;
    {$else}
        WorkerThread.Resume;
    {$ifend}
        // And wait until it finishes
        while not WorkerThread.FReady do
            Sleep(10);
        WorkerThread.Free;
    except
        on E:Exception do begin
            Result := -2;
            StrToBuffer(Buffer, BufSize, AnsiString(E.ClassName + ': ' + E.Message));
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure DLLHandler(Reason: Integer);
begin
    if Reason = DLL_PROCESS_DETACH then begin
//      MessageBox(0, PChar('Reason = ' + IntToStr(Reason)), 'DLLHandler', MB_OK);
        OverbyteIcsWinsock.CancelForceLoadWinsock;
        //WSocketUnregisterClass;  // 27/04/2002
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
begin
//  MessageBox(0, PChar('DLL Init ' + IntToStr(WSocketGCount)), 'DLL', MB_OK);
    DLLProc := @DLLHandler;
end.
