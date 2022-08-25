{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE (From a work done by Ed Hochman <ed@mbhsys.com>)
Creation:     Jan 13, 1998
Version:      1.02
Description:  HttpThrd is a demo program showing how to use THttpCli component
              in a multi-threaded program.
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2011 by François PIETTE
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

Updates:
Jun 19 2011 V1.01 Arno - Make use of an event object rather than
                  TThread.Suspend/Resume, both are deprecated since D2010.
Jun 20 2011 V1.02 Arno reworked it, was needed.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsHttpThr2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OverbyteIcsHttpProt;

type
  TThreadState = (tsInvalid, tsReady, tsBusy);
  THTTPThread = class(TThread)
  private
    FEvent        : THandle;
    FState        : TThreadState;
    FURL          : String;
    FProxy        : String;
    FThreadNumber : Integer;
    FHttpCli      : THTTPCli;
    FSuccess      : Boolean;
    FDataStream   : TMemoryStream;
    FLogList      : TStringList;
    FCritSect     : TRTLCriticalSection;
    procedure Progress(const Msg : String);
    procedure DocBegin(Sender : TObject);
    procedure DocData(Sender : TObject; Buffer : Pointer; Len : Integer);
    procedure DocEnd(Sender : TObject);
  protected
    procedure Execute; override;
  public
    constructor Create(AThreadNumber: Integer);
    destructor Destroy; override;
    procedure Wakeup;
    function LockLogList: TStringList;
    procedure UnlockLogList;
    property State: TThreadState read FState write FState;

    { None thread-safe properties, they have to be accessed only in State tsReady }
    property URL: String read FURL write FURL;
    property Proxy: String read FProxy write FProxy;
    property ThreadNumber: Integer read FThreadNumber write FThreadNumber;
    property Success: Boolean read FSuccess;
    property DataStream: TMemoryStream read FDataStream;
  end;

implementation

uses
    OverbyteIcsHttpThr1,
    OverbyteIcsUtils;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor THTTPThread.Create(AThreadNumber: Integer);
begin
    inherited Create(TRUE);
    InitializeCriticalSection(FCritSect);
    FDataStream   := TMemoryStream.Create;
    FLogList      := TStringList.Create;
    FThreadNumber := AThreadNumber;
    FEvent        := CreateEvent(nil, False, False, nil);
    if FEvent = 0 then
        RaiseLastOSError;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor THTTPThread.Destroy;
begin
    if FEvent <> 0 then begin
        Terminate;
        SetEvent(FEvent); { Wake it up otherwise inherited Destroy won't return }
    end;
    inherited Destroy;
    { The thread is down and THttpCli instance is released now }
    FDataStream.Free;
    FLogList.Free;
    DeleteCriticalSection(FCritSect);
    if FEvent <> 0 then
        CloseHandle(FEvent);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THTTPThread.Wakeup;
begin
    SetEvent(FEvent);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THTTPThread.LockLogList;
begin
    EnterCriticalSection(FCritSect);
    Result := FLogList;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THTTPThread.UnlockLogList;
begin
    LeaveCriticalSection(FCritSect);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THTTPThread.Progress(const Msg : String);
begin
    EnterCriticalSection(FCritSect);
    try
        FLogList.Add(Msg);
        if FLogList.Count = 1 then
            PostMessage(HttpThreadForm.Handle, WM_LOG, WParam(Self), 0);
    finally
        LeaveCriticalSection(FCritSect);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THTTPThread.Execute;
begin
    OverbyteIcsUtils.IcsNameThreadForDebugging(AnsiString(ClassName + ' ' +
                                               IntToStr(FThreadNumber)));
    FHttpCli := THTTPCli.Create(Nil);
    try
        FHttpCli.MultiThreaded := TRUE;
        FHttpCli.RcvdStream    := FDataStream;
        FHttpCli.OnDocBegin    := DocBegin;
        FHttpCli.OnDocEnd      := DocEnd;
        FHttpCli.OnDocData     := DocData;
        while not Terminated do begin
            WaitForSingleObject(FEvent, INFINITE);
            if Terminated then
                Break;
            Progress(IntToStr(FThreadNumber) + ' Start get');
            FDataStream.Clear;
            with FHttpCli do begin
                URL   := FURL;
                Proxy := FProxy;
                try
                    Get;   // Get page from internet
                    FSuccess := TRUE;
                except
                    FSuccess := FALSE;
                end;
            end;

            if not Terminated then
                PostMessage(HttpThreadForm.Handle, WM_THREAD_RESULT,
                            WParam(Self), Ord(Success));
        end;
    finally
        FHttpCli.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THTTPThread.DocBegin(Sender : TObject);
begin
    Progress(IntToStr(FThreadNumber) + ' Doc begin');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THTTPThread.DocData(Sender : TObject; Buffer : Pointer; Len : Integer);
begin
    Progress(IntToStr(FThreadNumber) + ' Doc data');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THTTPThread.DocEnd(Sender : TObject);
begin
    Progress(IntToStr(FThreadNumber) + ' Doc end');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

