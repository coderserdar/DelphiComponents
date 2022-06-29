{$INCLUDE ..\cDefines.inc}
unit cSocketHostLookup;

{                                                                              }
{                         WinSock Lookup functions 3.04                        }
{                                                                              }
{       This unit is copyright © 2001-2003 by David Butler (david@e.co.za)     }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{               Its original file name is cSocketHostLookup.pas                }
{       The latest version is available from the Fundamentals home page        }
{                     http://fundementals.sourceforge.net/                     }
{                                                                              }
{                I invite you to use this unit, free of charge.                }
{        I invite you to distibute this unit, but it must be for free.         }
{             I also invite you to contribute to its development,              }
{             but do not distribute a modified copy of this file.              }
{                                                                              }
{          A forum is available on SourceForge for general discussion          }
{             http://sourceforge.net/forum/forum.php?forum_id=2117             }
{                                                                              }
{                                                                              }
{ Description:                                                                 }
{   Host lookup functions.                                                     }
{                                                                              }
{ Revision history:                                                            }
{   11/12/2001  0.01  Spawned from cSockets.                                   }
{   05/01/2002  0.02  Added Reverse Lookup functionality.                      }
{   01/07/2002  3.03  Refactored for Fundamentals 3.                           }
{   08/10/2002  3.04  Added OnLookup, State and Active properties.             }
{                                                                              }

interface

uses
  { Delphi }
  Classes,
  WinSock,
  SysUtils,

  { Fundamentals }
  cWinSock,
  cWindows;



{                                                                              }
{ TSocketHostLookup                                                            }
{   Class to do host lookups using WinSock.                                    }
{                                                                              }
{   Three methods are available: Block, Asynchronous and Thread.               }
{   i)   Block: Uses WinSock blocking mode. The current thread blocks (and     }
{        don't process messages) until the function returns.                   }
{   ii)  Asynchronous: Uses WinSock asynchronous mode. The running thread      }
{        continues and a message is posted when the result is available        }
{   iii) Thread: Uses a seperate thread (that uses WinSock blocking mode) to   }
{        get the result. OnComplete is called synchronized with the main       }
{        thread.                                                               }
{                                                                              }
type
  TSocketHostLookupMethod = (lmBlock, lmAsync, lmThread);
  TSocketHostLookup = class;
  TSocketHostLookupEvent = procedure (Sender: TSocketHostLookup) of object;
  TSocketHostLookupState = (lsInit, lsBusy, lsComplete, lsCancelled);
  TSocketHostLookup = class(TComponent)
  protected
    FHost        : String;
    FMethod      : TSocketHostLookupMethod;
    FErrorCode   : Integer;
    FState       : TSocketHostLookupState;
    FActive      : Boolean;
    FAddresses   : TInAddrArray;
    FOnLookup    : TSocketHostLookupEvent;
    FOnComplete  : TSocketHostLookupEvent;
    FThread      : TThread;
    FAsync       : TWindowHandle;

    procedure Init; virtual;
    procedure RaiseError(const Msg: String);
    procedure CheckNotBusy;
    procedure CheckHostAssigned;
    procedure SetComplete;
    procedure Clear;
    function  GetThread: TThread;
    procedure FreeThread;

    procedure BlockingLookup;
    procedure ThreadLookup;
    procedure AsyncLookup;
    procedure BlockingReverseLookup;
    procedure ThreadReverseLookup;
    procedure AsyncReverseLookup;

    function  GetAddressCount: Integer;
    function  GetAddress(const Idx: Integer): TInAddr;
    function  GetAddressStr(const Idx: Integer): String;
    function  GetComplete: Boolean;
    function  GetSuccess: Boolean;
    function  GetErrorMessage: String;

    procedure SetHost(const Host: String);
    procedure SetMethod(const Method: TSocketHostLookupMethod);
    function  GetActive: Boolean;
    procedure SetActive(const Active: Boolean);
    procedure Loaded; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Resolve; overload;
    procedure Resolve(const Host: String; const Method: TSocketHostLookupMethod = lmThread); overload;
    procedure Lookup; overload;
    procedure Lookup(const Host: String; const Method: TSocketHostLookupMethod = lmThread); overload;
    procedure ReverseLookup(const Address: TInAddr; const Method: TSocketHostLookupMethod = lmThread);
    procedure Cancel;

    property  Active: Boolean read GetActive write SetActive;
    property  Host: String read FHost write SetHost;
    property  Method: TSocketHostLookupMethod read FMethod write SetMethod default lmThread;
    property  OnLookup: TSocketHostLookupEvent read FOnLookup write FOnLookup;
    property  OnComplete: TSocketHostLookupEvent read FOnComplete write FOnComplete;

    property  State: TSocketHostLookupState read FState;
    property  Complete: Boolean read GetComplete;
    property  Success: Boolean read GetSuccess;
    property  ErrorCode: Integer read FErrorCode;
    property  ErrorMessage: String read GetErrorMessage;
    property  Addresses: TInAddrArray read FAddresses;
    property  AddressCount: Integer read GetAddressCount;
    property  Address[const Idx: Integer]: TInAddr read GetAddress;
    property  AddressStr[const Idx: Integer]: String read GetAddressStr;
  end;
  ESocketHostLookup = class (Exception);
  TSocketHostLookupArray = Array of TSocketHostLookup;

  { TfndSocketHostLookup                                                       }
  TfndSocketHostLookup = class(TSocketHostLookup)
  published
    property  Active;
    property  Host;
    property  Method;
    property  OnLookup;
    property  OnComplete;
  end;



{                                                                              }
{ Helper functions                                                             }
{   If SocketHostLookup can resolve Host immediately, the address is returned  }
{   in Addr and the function returns nil. Otherwise, an active instance of     }
{   TSocketHostLookup is returned.                                             }
{                                                                              }
function  SocketHostLookup(const Host: String; var Addr: TInAddr;
          const Method: TSocketHostLookupMethod = lmThread;
          const OnComplete: TSocketHostLookupEvent = nil): TSocketHostLookup;



implementation

uses
  // Delphi
  Messages,
  WinProcs;



{                                                                              }
{ TThreadHostLookupThread                                                      }
{                                                                              }
type
  { TThreadHostLookupThread                                                    }
  TThreadHostLookupThreadTask = (ttLookup, ttReverseLookup);
  TThreadHostLookupThread = class(TThread)
  protected
    FTask       : TThreadHostLookupThreadTask;
    FHost       : String;
    FErrorCode  : Integer;
    FAddresses  : TInAddrArray;
    FLookup     : TSocketHostLookup;
    FPending    : Boolean;

    procedure SetComplete;
    procedure Execute; override;

  public
    constructor Create(const Lookup: TSocketHostLookup);
    destructor Destroy; override;

    procedure Lookup(const Host: String);
    procedure ReverseLookup(const Address: TInAddr);
  end;

{ TThreadHostLookupThread                                                      }
constructor TThreadHostLookupThread.Create(const Lookup: TSocketHostLookup);
begin
  Assert(Assigned(Lookup), 'Assigned(Lookup)');
  inherited Create(True);
  FLookup := Lookup;
  FreeOnTerminate := True;
end;

destructor TThreadHostLookupThread.Destroy;
begin
  if Assigned(FLookup) and (FLookup.FThread = self) then
    FLookup.FThread := nil;
  FLookup := nil;
  inherited Destroy;
end;

procedure TThreadHostLookupThread.Lookup(const Host: String);
begin
  FHost := Host;
  FAddresses := nil;
  FTask := ttLookup;
  if not Suspended then
    FPending := True else
    Resume;
end;

procedure TThreadHostLookupThread.ReverseLookup(const Address: TInAddr);
begin
  FHost := '';
  SetLength(FAddresses, 1);
  FAddresses[0] := Address;
  FTask := ttReverseLookup;
  if not Suspended then
    FPending := True else
    Resume;
end;

procedure TThreadHostLookupThread.SetComplete;
begin
  if not Assigned(FLookup) or (FLookup.FThread <> self) then
    exit;
  FLookup.FHost := FHost;
  FLookup.FAddresses := FAddresses;
  FLookup.FErrorCode := FErrorCode;
  FLookup.SetComplete;
end;

procedure TThreadHostLookupThread.Execute;

  function IsTerminated: Boolean;
  begin
    Result := Terminated or not Assigned(FLookup) or (FLookup.FThread <> self);
  end;

var HostEnt : PHostEnt;
    Task    : TThreadHostLookupThreadTask;
begin
  While not IsTerminated do
    begin
      // Do Task
      FPending := False;
      FErrorCode := 0;
      Task := FTask;
      if Task = ttLookup then
        HostEnt := WinSock.GetHostByName(PChar(FHost)) else
        HostEnt := WinSock.GetHostByAddr(@FAddresses[0], Sizeof(TInAddr), AF_INET);
      if not Assigned(HostEnt) then
        // Failure
        FErrorCode := WinSock.WSAGetLastError else
        // Success
        if Task = ttLookup then
          FAddresses := HostEntAddresses(HostEnt) else
          FHost := HostEntName(HostEnt);
      if IsTerminated then
        exit;
      // Notify completion
      Synchronize(SetComplete);
      if IsTerminated then
        exit;
      // Wait next task
      if not FPending then
        begin
          if IsTerminated then
            exit;
          Suspend;
        end;
    end;
end;



{                                                                              }
{ TAsyncHostLookup                                                             }
{                                                                              }
const
  WM_LOOKUP        = WM_USER + 550;
  WM_REVERSELOOKUP = WM_USER + 551;
  WM_LOOKUPCANCEL  = WM_USER + 552;

type
  TAsyncHostLookup = class(TWindowHandle)
  protected
    FLookup         : TSocketHostLookup;
    FHost           : String;
    FAddresses      : TInAddrArray;
    FLookupHandle   : THandle;
    FResult         : Array[0..MAXGETHOSTSTRUCT] of Char;
    FErrorCode      : Integer;
    FSuccessPending : Boolean;

    procedure WMLookup(const Msg: Cardinal; const Handle: THandle; const Error: Integer);
    function  HandleWM(const Msg: Cardinal; const wParam, lParam: Integer): Integer; override;

    procedure SetSuccess;
    procedure SetLookupSuccess;
    procedure SetReverseLookupSuccess;
    procedure SetFailure(const Error: Integer);

  public
    constructor Create(const Lookup: TSocketHostLookup); reintroduce; overload;

    procedure Lookup(const Host: String);
    procedure ReverseLookup(const Address: TInAddr);
    procedure Cancel;
  end;

constructor TAsyncHostLookup.Create(const Lookup: TSocketHostLookup);
begin
  Assert(Assigned(Lookup), 'Assigned(Lookup)');
  inherited Create(nil);
  FLookup := Lookup;
end;

procedure TAsyncHostLookup.Cancel;
var H : THandle;
begin
  H := FLookupHandle;
  if H <> 0 then
    begin
      FLookupHandle := 0;
      WinSock.WSACancelAsyncRequest(H);
      if FWindowHandle <> 0 then
        PostMessage(FWindowHandle, WM_LOOKUPCANCEL, H, 0);
    end;
end;

procedure TAsyncHostLookup.Lookup(const Host: String);
begin
  Cancel;
  FHost := Host;
  FAddresses := nil;
  FLookupHandle := 0;
  FSuccessPending := False;
  FLookupHandle := WinSock.WSAAsyncGetHostByName(GetWindowHandle, WM_LOOKUP,
      PChar(FHost), @FResult, Sizeof(FResult));
  if FLookupHandle = 0 then
    SetFailure(WinSock.WSAGetLastError) else
    if FSuccessPending then
      SetLookupSuccess;
end;

procedure TAsyncHostLookup.ReverseLookup(const Address: TInAddr);
begin
  Cancel;
  SetLength(FAddresses, 1);
  FAddresses[0] := Address;
  FHost := '';
  FLookupHandle := 0;
  FSuccessPending := False;
  FLookupHandle := WinSock.WSAAsyncGetHostByAddr(GetWindowHandle, WM_REVERSELOOKUP,
      @FAddresses[0], Sizeof(TInAddr), AF_INET, @FResult, Sizeof(FResult));
  if FLookupHandle = 0 then
    SetFailure(WinSock.WSAGetLastError) else
    if FSuccessPending then
      SetReverseLookupSuccess;
end;

procedure TAsyncHostLookup.SetSuccess;
begin
  FLookupHandle := 0;
  if not Assigned(FLookup) or (FLookup.FAsync <> self) then
    exit;
  FLookup.FAddresses := FAddresses;
  FLookup.FHost := FHost;
  FLookup.FErrorCode := 0;
  FLookup.SetComplete;
end;

procedure TAsyncHostLookup.SetLookupSuccess;
begin
  FAddresses := HostEntAddresses(@FResult);
  SetSuccess;
end;

procedure TAsyncHostLookup.SetReverseLookupSuccess;
begin
  FHost := HostEntName(@FResult);
  SetSuccess;
end;

procedure TAsyncHostLookup.SetFailure(const Error: Integer);
begin
  FLookupHandle := 0;
  FLookup.FErrorCode := Error;
  FLookup.SetComplete;
end;

function TAsyncHostLookup.HandleWM(const Msg: Cardinal; const wParam, lParam: Integer): Integer;
begin
  if (Msg = WM_LOOKUP) or (Msg = WM_REVERSELOOKUP) then
    begin
      WMLookup(Msg, THandle(wParam), HiWord(LongWord(lParam)));
      Result := 0;
    end else
    Result := inherited HandleWM(Msg, wParam, lParam);
end;

procedure TAsyncHostLookup.WMLookup(const Msg: Cardinal; const Handle: THandle; const Error: Integer);
begin
  if (FLookupHandle <> 0) and (Handle <> FLookupHandle) then
    exit;
  if Error = 0 then
    // Success
    if FLookupHandle = 0 then // Message received before WSAAsync call returned
      FSuccessPending := True else
      if Msg = WM_LOOKUP then
        SetLookupSuccess else
        SetReverseLookupSuccess
  else
    // Failure
    SetFailure(Error);
end;



{                                                                              }
{ TSocketHostLookup                                                            }
{                                                                              }
constructor TSocketHostLookup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Init;
end;

procedure TSocketHostLookup.Init;
begin
  FMethod := lmThread;
  FState := lsInit;
end;

destructor TSocketHostLookup.Destroy;
begin
  FreeThread;
  FreeAndNil(FAsync);
  inherited Destroy;
end;

procedure TSocketHostLookup.RaiseError(const Msg: String);
begin
  raise ESocketHostLookup.Create(Msg);
end;

procedure TSocketHostLookup.CheckNotBusy;
begin
  if FState = lsBusy then
    RaiseError('Lookup in progress');
end;

procedure TSocketHostLookup.CheckHostAssigned;
begin
  if FHost = '' then
    RaiseError('Host not assigned');
end;

function TSocketHostLookup.GetAddressCount: Integer;
begin
  Result := Length(FAddresses);
end;

function TSocketHostLookup.GetAddress(const Idx: Integer): TInAddr;
begin
  Result := FAddresses[Idx];
end;

function TSocketHostLookup.GetAddressStr(const Idx: Integer): String;
begin
  Result := IPAddressStr(GetAddress(Idx));
end;

function TSocketHostLookup.GetComplete: Boolean;
begin
  Result := FState in [lsComplete, lsCancelled];
end;

function TSocketHostLookup.GetSuccess: Boolean;
begin
  Result := (FState = lsComplete) and (FErrorCode = 0);
end;

function TSocketHostLookup.GetErrorMessage: String;
begin
  if FState <> lsComplete then
    Result := '' else
    Result := WinSockErrorAsString(FErrorCode);
end;

procedure TSocketHostLookup.SetHost(const Host: String);
begin
  if FHost = Host then
    exit;
  CheckNotBusy;
  FHost := Host;
end;

procedure TSocketHostLookup.SetMethod(const Method: TSocketHostLookupMethod);
begin
  if FMethod = Method then
    exit;
  CheckNotBusy;
  FMethod := Method;
end;

function TSocketHostLookup.GetThread: TThread;
begin
  if not Assigned(FThread) then
    FThread := TThreadHostLookupThread.Create(self);
  Result := FThread;
end;

procedure TSocketHostLookup.FreeThread;
var T : TThread;
begin
  T := FThread;
  if T = nil then
    exit;
  FThread := nil;
  T.Terminate;
end;

procedure TSocketHostLookup.SetComplete;
begin
  if FState = lsCancelled then
    exit;
  FState := lsComplete;
  if Assigned(FOnComplete) then
    FOnComplete(self);
end;

procedure TSocketHostLookup.BlockingLookup;
var HostEnt : PHostEnt;
begin
  HostEnt := WinSock.GetHostByName(PChar(FHost));
  if FState = lsCancelled then
    exit;
  if not Assigned(HostEnt) then
    FErrorCode := WinSock.WSAGetLastError else
    begin
      FAddresses := HostEntAddresses(HostEnt);
      FErrorCode := 0;
    end;
  SetComplete;
end;

procedure TSocketHostLookup.BlockingReverseLookup;
var HostEnt : PHostEnt;
begin
  HostEnt := WinSock.GetHostByAddr(@FAddresses[0], Sizeof(TInAddr), AF_INET);
  if FState = lsCancelled then
    exit;
  if not Assigned(HostEnt) then
    FErrorCode := WinSock.WSAGetLastError else
    begin
      FErrorCode := 0;
      FHost := HostEntName(HostEnt);
    end;
  SetComplete;
end;

procedure TSocketHostLookup.ThreadLookup;
begin
  TThreadHostLookupThread(GetThread).Lookup(FHost);
end;

procedure TSocketHostLookup.ThreadReverseLookup;
begin
  TThreadHostLookupThread(GetThread).ReverseLookup(FAddresses[0]);
end;

procedure TSocketHostLookup.AsyncLookup;
begin
  if not Assigned(FAsync) then
    FAsync := TAsyncHostLookup.Create(self);
  TAsyncHostLookup(FAsync).Lookup(FHost);
end;

procedure TSocketHostLookup.AsyncReverseLookup;
begin
  if not Assigned(FAsync) then
    FAsync := TAsyncHostLookup.Create(self);
  TAsyncHostLookup(FAsync).ReverseLookup(FAddresses[0]);
end;

procedure TSocketHostLookup.Clear;
begin
  FHost := '';
  FAddresses := nil;
  FErrorCode := 0;
  FState := lsInit;
end;

procedure TSocketHostLookup.ReverseLookup(const Address: TInAddr; const Method: TSocketHostLookupMethod);
begin
  CheckNotBusy;
  Clear;
  SetLength(FAddresses, 1);
  FAddresses[0] := Address;
  FMethod := Method;
  FState := lsBusy;
  WinSockStartup;
  Case FMethod of
    lmBlock  : BlockingReverseLookup;
    lmThread : ThreadReverseLookup;
    lmAsync  : AsyncReverseLookup;
  end;
end;

procedure TSocketHostLookup.Lookup;
begin
  CheckNotBusy;
  CheckHostAssigned;
  FAddresses := nil;
  FErrorCode := 0;
  FState := lsBusy;
  WinSockStartup;
  if Assigned(FOnLookup) then
    FOnLookup(self);
  Case FMethod of
    lmBlock  : BlockingLookup;
    lmThread : ThreadLookup;
    lmAsync  : AsyncLookup;
  end;
end;

procedure TSocketHostLookup.Lookup(const Host: String; const Method: TSocketHostLookupMethod);
begin
  CheckNotBusy;
  Clear;
  FHost := Host;
  FMethod := Method;
  Lookup;
end;

procedure TSocketHostLookup.Resolve;
var Addr : TInAddr;
begin
  CheckNotBusy;
  CheckHostAssigned;
  if IsIPAddress(FHost, Addr) then
    begin
      SetLength(FAddresses, 1);
      FAddresses[0] := Addr;
      SetComplete;
      exit;
    end;
  Lookup;
end;

procedure TSocketHostLookup.Resolve(const Host: String; const Method: TSocketHostLookupMethod);
begin
  CheckNotBusy;
  Clear;
  FHost := Host;
  FMethod := Method;
  Resolve;
end;

procedure TSocketHostLookup.Cancel;
begin
  if FState <> lsBusy then
    exit;
  FState := lsCancelled;
  Case FMethod of
    lmThread :
      FreeThread;
    lmAsync  :
      if Assigned(FAsync) then
        TAsyncHostLookup(FAsync).Cancel;
  end;
end;

function TSocketHostLookup.GetActive: Boolean;
begin
  if [csDesigning, csLoading] * ComponentState <> [] then
    Result := FActive else
    Result := FState = lsBusy;
end;

procedure TSocketHostLookup.SetActive(const Active: Boolean);
begin
  if [csDesigning, csLoading] * ComponentState <> [] then
    FActive := Active else
    if Active then
      begin
        if FState = lsBusy then
          exit;
        Lookup;
      end else
      begin
        if FState <> lsBusy then
          exit;
        Cancel;
      end;
end;

procedure TSocketHostLookup.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    if FActive then
      Lookup;
end;



{                                                                              }
{ Helper functions                                                             }
{                                                                              }
function SocketHostLookup(const Host: String; var Addr: TInAddr;
    const Method: TSocketHostLookupMethod;
    const OnComplete: TSocketHostLookupEvent): TSocketHostLookup;
begin
  if IsIPAddress(Host, Addr) then
    begin
      Result := nil;
      exit;
    end;
  Result := TSocketHostLookup.Create(nil);
  try
    Result.OnComplete := OnComplete;
    Result.Lookup(Host, Method);
  except
    FreeAndNil(Result);
    raise;
  end;
end;



end.

