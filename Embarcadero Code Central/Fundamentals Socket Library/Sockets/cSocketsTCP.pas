{$INCLUDE ..\cDefines.inc}
unit cSocketsTCP;

{                                                                              }
{                           TCP socket class v3.04                             }
{                                                                              }
{       This unit is copyright © 2001-2003 by David Butler (david@e.co.za)     }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                  Its original file name is cSocketsTCP.pas                   }
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
{ Revision history:                                                            }
{   11/12/2001  2.01  Initial version of AClientSocket, TClientSocket.         }
{   24/08/2002  3.02  Created cSocketsTCP from cSocketsTCPClient.              }
{   18/12/2002  3.03  Changed ReadStr to also read directly from the Windows   }
{                     socket buffer. This is more efficient when ReadStr is    }
{                     called from the OnDataAvailable handler.                 }
{   06/06/2003  3.04  Fixed bug in Peek function.                              }
{                                                                              }

interface

uses
  { Delphi }
  WinSock,

  { Fundamentals }
  cUtils,
  cSockets;



{                                                                              }
{ ATCPClientSocket                                                             }
{   Base class for TCP client sockets.                                         }
{   Implements read, write and close functionality.                            }
{                                                                              }
type
  ATCPClientSocket = class;
  ATCPClientSocketEvent = procedure (Sender: ATCPClientSocket) of object;
  ATCPClientSocket = class(ASocketExSendBuffer)
  protected
    FInBuffer          : String;
    FInBufferSize      : Integer;
    FInBufferPos       : Integer;
    FThrottleRead      : Boolean;
    FReadThrottleRate  : Integer;
    FReadTimerStart    : Integer;
    FReadTimerCount    : Integer;
    FReadRate          : Integer;
    FGotRemoteHostName : Boolean;
    FRemoteHostName    : String;
    FOnDataAvailable   : ATCPClientSocketEvent;
    FOutBufferSize     : Integer;
    FClosePending      : Boolean;
    FOnClose           : ATCPClientSocketEvent;

    procedure Init; override;
    procedure Init(const SocketHandle: TSocket); override;

    procedure WMSocket(const Events, lWordHi: Word); override;
    function  GetAsynchronousEvents: LongInt; override;

    function  ThrottleTimerActive: Boolean; override;
    procedure TriggerTimer; override;
    procedure TriggerSendBufferEmpty; override;

    procedure HandleReadEvent;
    procedure HandleCloseEvent(const Error: Word);

    procedure PackInBuffer;
    procedure AdvanceInBuffer(const Count: Integer);
    procedure SetThrottleRead(const ThrottleRead: Boolean);
    function  SocketAvailableToRead: Integer;
    function  SocketRead(var Buf; const Size: Integer; var Error: Integer): Integer;
    function  ReadFromBuffer(var Buf; const Size: Integer; const Peek: Boolean): Integer;
    function  SkipBuffered(const Count: Integer): Integer;
    function  BufferInData(const Count: Integer; var Error: Integer): Integer;
    procedure TriggerDataAvailable; virtual;

    function  SocketSend(const Buf; const Size: Integer; var Error: Integer): Integer;
    function  SendData(const Buf; const Size: Integer; var Error: Integer): Integer;
    procedure SendBufferedData; override;

    procedure TriggerClose; virtual;
    procedure SetClosed;

  public
    procedure Close(const Wait: Boolean);

    function  Connected: Boolean;

    function  GetRemoteAddress: TInAddr; virtual;
    function  GetRemoteAddressStr: String; virtual;
    function  GetRemoteHostName: String; virtual;

    function  Read(var Buf; const Size: Integer): Integer;
    function  ReadStr(const Size: Integer): String;
    function  ReadAvailable: String;
    function  AvailableToRead: Integer;
    property  InBufferSize: Integer read FInBufferSize;

    function  GetReadRate: Integer;
    property  ThrottleRead: Boolean read FThrottleRead write SetThrottleRead;
    property  ReadThrottleRate: Integer read FReadThrottleRate write FReadThrottleRate;

    function  Send(const Buf; const Size: Integer): Integer;
    function  SendStr(const Buf: String): Integer;
    property  OutBufferSize: Integer read FOutBufferSize;

    function  Peek(var Buf; const Size: Integer): Integer;
    function  PeekStr(const Size: Integer): String;
    function  PeekAvailable: String;
    function  Skip(const Count: Integer): Integer;

    function  LocateChar(const Ch: Char; const LocateNonMatch: Boolean = False): Integer; overload;
    function  LocateChar(const C: CharSet; const LocateNonMatch: Boolean = False): Integer; overload;
    function  LocateSequence(const Sequence: String; const CaseSensitive: Boolean = True): Integer;
    function  MatchSequence(const Sequence: String; const CaseSensitive: Boolean = True): Boolean;
    function  MatchChar(const Ch: Char; const MatchNonMatch: Boolean = False;
              const SkipOnMatch: Boolean = False): Boolean;

    function  IsLineAvailable: Integer;
    function  ReadLine(var Line: String): Boolean;

    property  OnDataAvailable: ATCPClientSocketEvent read FOnDataAvailable write FOnDataAvailable;
    property  OnClose: ATCPClientSocketEvent read FOnClose write FOnClose;
  end;



implementation

uses
  { Delphi }
  Windows,

  { Fundamentals }
  cStrings,
  cLinkedLists,
  cWinSock;



{                                                                              }
{ ATCPClientSocket                                                             }
{                                                                              }
type
  TTCPSendBufferItem = class(TDoublyLinkedString);

procedure ATCPClientSocket.Init;
begin
  inherited Init;
  FProtocol := spTCP;
end;

procedure ATCPClientSocket.Init(const SocketHandle: TSocket);
var L : Integer;
begin
  inherited Init(SocketHandle);
  if WinSock.ioctlsocket(SocketHandle, FIONREAD, L) >= 0 then
    begin
      FState := ssConnected;
      SetSocketAsynchronous;
      FReadTimerStart := GetTickCount;
      FReadTimerCount := 0;
    end;
end;

procedure ATCPClientSocket.WMSocket(const Events, lWordHi: Word);
begin
  if Events and FD_CLOSE <> 0 then
    HandleCloseEvent(lWordHi);
  if Events and FD_READ <> 0 then
    HandleReadEvent;
  inherited WMSocket(Events, lWordHi);
end;

function ATCPClientSocket.GetAsynchronousEvents: LongInt;
begin
  Result := FD_READ or FD_WRITE or FD_CLOSE or FD_CONNECT;
end;

function ATCPClientSocket.Connected: Boolean;
begin
  Result := FState = ssConnected;
end;

function ATCPClientSocket.GetReadRate: Integer;
var Elapsed : Integer;
begin
  AdvanceTimer(FReadTimerStart, FReadTimerCount, GetTickCount, Elapsed, FReadRate);
  Result := FReadRate;
end;

{ Throttle                                                                     }
procedure ATCPClientSocket.SetThrottleRead(const ThrottleRead: Boolean);
begin
  if ThrottleRead = FThrottleRead then
    exit;
  FThrottleRead := ThrottleRead;
  RefreshThrottleTimer;
  if not ThrottleRead and (FState = ssConnected) then
    PostMessage(FWindowHandle, WM_SOCKET, FSocketHandle, FD_READ); // Re-start unthrottled handling
end;

function ATCPClientSocket.ThrottleTimerActive: Boolean;
begin
  Result := (FThrottleRead or FThrottleWrite) and (FState = ssConnected);
end;

procedure ATCPClientSocket.TriggerTimer;
begin
  inherited TriggerTimer;
  if FThrottleRead then
    HandleReadEvent;
end;



{ Close                                                                        }
procedure ATCPClientSocket.TriggerClose;
begin
  if Assigned(FOnClose) then
    FOnClose(self);
end;

procedure ATCPClientSocket.SetClosed;
begin
  SetState(ssClosed);
  TriggerClose;
  DestroySocketHandle;
  ClearSendBuffer;
end;

procedure ATCPClientSocket.HandleCloseEvent(const Error: Word);
begin
  HandleReadEvent; // Read unfired data from socket buffers before closing
  SetError(Error, '');
  SetClosed;
end;

procedure ATCPClientSocket.Close(const Wait: Boolean);
begin
  if (FState = ssClosed) or (FSocketHandle = INVALID_SOCKET) then
    exit;
  WinSock.shutdown(FSocketHandle, SD_SEND);
  if Wait and not FOutBuffer.IsEmpty then
    FClosePending := True
  else
    SetClosed;
end;

function ATCPClientSocket.GetRemoteAddress: TInAddr;
var Addr : TSockAddrIn;
    Len  : Integer;
begin
  CheckSocketConnected;
  Len := Sizeof(Addr);
  if WinSock.getpeername(FSocketHandle, Addr, Len) <> 0 then
    Result.S_addr := INADDR_NONE else
    Result := Addr.sin_addr;
end;

function ATCPClientSocket.GetRemoteAddressStr: String;
var Address : TInAddr;
begin
  Address := GetRemoteAddress;
  if (Address.S_addr = INADDR_NONE) or (Address.S_addr = INADDR_ANY) then
    Result := '' else
    Result := IPAddressStr(Address);
end;

function ATCPClientSocket.GetRemoteHostName: String;
var Address : TInAddr;
begin
  if not FGotRemoteHostName then
    begin
      Address := GetRemoteAddress;
      if (Address.S_addr = INADDR_NONE) or (Address.S_addr = INADDR_ANY) then
        FRemoteHostName := '' else
        begin
          FRemoteHostName := cWinSock.GetRemoteHostName(Address);
          if FRemoteHostName = '' then
            FRemoteHostName := IPAddressStr(Address);
        end;
      FGotRemoteHostName := True;
    end;
  Result := FRemoteHostName;
end;



{ Read                                                                         }
function ATCPClientSocket.SocketAvailableToRead: Integer;
begin
  if FSocketHandle = INVALID_SOCKET then
    Result := 0 else
    if WinSock.ioctlsocket(FSocketHandle, FIONREAD, Result) <> 0 then
      Result := 0 else
      if FThrottleRead then
        Result := ThrottledSize(FReadTimerStart, FReadTimerCount, Result,
            GetTickCount, FReadThrottleRate, FReadRate);
end;

procedure ATCPClientSocket.TriggerDataAvailable;
begin
  if Assigned(FOnDataAvailable) then
    FOnDataAvailable(self);
end;

function ATCPClientSocket.SocketRead(var Buf; const Size: Integer; var Error: Integer): Integer;
var L : Integer;
begin
  if Size <= 0 then
    begin
      Error := 0;
      Result := 0;
      exit;
    end;
  if FThrottleRead and (FState = ssConnected) then
    begin
      L := ThrottledSize(FReadTimerStart, FReadTimerCount, Size,
          GetTickCount, FReadThrottleRate, FReadRate);
      if L = 0 then
        begin
          Error := 0;
          Result := 0;
          exit;
        end;
    end else
    L := Size;
  Result := WinSock.recv(FSocketHandle, Buf, L, 0);
  if Result < 0 then
    begin
      Error := WinSock.WSAGetLastError;
      Result := 0;
    end else
    begin
      Error := 0;
      Inc(FReadTimerCount, Result);
    end;
end;

procedure ATCPClientSocket.PackInBuffer;
var L : Integer;
    P : PChar;
begin
  L := Length(FInBuffer);
  // prevent large empty buffers from hanging around
  // prevent unused area from growing out of control
  if (L >= 16384) and ((FInBufferSize < 256) or (FInBufferPos > L div 2)) then
    begin
      if FInBufferSize > 0 then
        begin
          P := Pointer(FInBuffer);
          Inc(P, FInBufferPos);
          Move(P^, Pointer(FInBuffer)^, FInBufferSize);
        end;
      FInBufferPos := 0;
      SetLength(FInBuffer, MaxI(16384, FInBufferSize));
    end;
end;

procedure ATCPClientSocket.AdvanceInBuffer(const Count: Integer);
begin
  if Count <= 0 then
    exit;

  Assert(FInBufferSize >= Count, 'AdvanceBuffer: Invalid Count parameter');
  Assert(FInBufferPos + FInBufferSize <= Length(FInBuffer), 'AdvanceBuffer: Invalid Buffer state');

  Inc(FInBufferPos, Count);
  Dec(FInBufferSize, Count);

  PackInBuffer;
end;

function ATCPClientSocket.ReadFromBuffer(var Buf; const Size: Integer; const Peek: Boolean): Integer;
var L : Integer;
    P : PChar;
begin
  Result := 0;
  L := MinI(Size, FInBufferSize);
  if L <= 0 then
    exit;
  P := Pointer(FInBuffer);
  Inc(P, FInBufferPos);
  Move(P^, Buf, L);
  AdvanceInBuffer(L);
  Result := L;
end;

function ATCPClientSocket.SkipBuffered(const Count: Integer): Integer;
var L : Integer;
begin
  Result := 0;
  L := MinI(Count, FInBufferSize);
  if L <= 0 then
    exit;
  AdvanceInBuffer(L);
  Result := L;
end;

function ATCPClientSocket.BufferInData(const Count: Integer; var Error: Integer): Integer;
var L : Integer;
    P : PChar;
begin
  if Count <= 0 then
    begin
      Error := 0;
      Result := 0;
      exit;
    end;
  L := FInBufferPos + FInBufferSize;
  if Length(FInBuffer) - L < Count then
    SetLength(FInBuffer, L + Count);
  P := Pointer(FInBuffer);
  Inc(P, L);
  Result := SocketRead(P^, Count, Error);
  if Result <= 0 then
    exit;
  Inc(FInBufferSize, Result);
end;

procedure ATCPClientSocket.HandleReadEvent;
var L, Error : Integer;
begin
  // Give event handler a chance to read data first
  L := SocketAvailableToRead;
  if L > 0 then
    TriggerDataAvailable;

  // Read and buffer as much as possible from socket
  Repeat
    if L = 0 then
      L := 1460; // Try anyway
    if BufferInData(L, Error) <= 0 then
      break; // No more data to read
    TriggerDataAvailable;
    L := SocketAvailableToRead;
  Until False;
end;

// Reading of already received (buffered) data is allowed when State <> ssConnected.
function ATCPClientSocket.Read(var Buf; const Size: Integer): Integer;
var M, T, Error : Integer;
    P : PChar;
begin
  Result := 0;
  if Size <= 0 then
    exit;

  // Read buffered data
  P := @Buf;
  T := Size;
  M := ReadFromBuffer(P^, T, False);
  if M > 0 then
    begin
      Inc(P, M);
      Inc(Result, M);
      Dec(T, M);
      if T <= 0 then
        exit;
    end;

  // Read from socket
  Repeat
    M := SocketRead(P^, T, Error);
    if M <= 0 then
      exit else
      begin
        Inc(P, M);
        Inc(Result, M);
        Dec(T, M);
      end;
  Until T <= 0;
end;

function ATCPClientSocket.ReadStr(const Size: Integer): String;
var L, M : Integer;
begin
  Result := '';
  M := Size;
  if M <= 0 then
    exit;
  if (FInBufferSize > 0) and (M >= FInBufferSize) then
    begin
      Result := ReadAvailable;
      exit;
    end;
  SetLength(Result, M);
  L := Read(Pointer(Result)^, M);
  if L = M then
    exit;
  SetLength(Result, L);
end;

function ATCPClientSocket.Peek(var Buf; const Size: Integer): Integer;
var L : Integer;
    P : PChar;
begin
  Result := 0;
  L := MinI(Size, FInBufferSize);
  if L <= 0 then
    exit;
  P := Pointer(FInBuffer);
  Inc(P, FInBufferPos);
  Move(P^, Buf, L);
  Result := L;
end;

function ATCPClientSocket.PeekStr(const Size: Integer): String;
var L, M : Integer;
begin
  Result := '';
  M := MaxI(0, Size);
  if M = 0 then
    exit;
  if M >= FInBufferSize then
    begin
      Result := FInBuffer;
      exit;
    end;
  SetLength(Result, M);
  L := Peek(Pointer(Result)^, M);
  if L = M then
    exit;
  SetLength(Result, L);
end;

function ATCPClientSocket.Skip(const Count: Integer): Integer;
begin
  Result := SkipBuffered(Count);
end;

function ATCPClientSocket.AvailableToRead: Integer;
begin
  Result := FInBufferSize;
end;

function ATCPClientSocket.ReadAvailable: String;
begin
  if FInBufferSize = 0 then
    begin
      Result := '';
      exit;
    end;
  if FInBufferPos > 0 then
    Delete(FInBuffer, 1, FInBufferPos);
  if FInBufferSize < Length(FInBuffer) then
    SetLength(FInBuffer, FInBufferSize);
  Result := FInBuffer;
  FInBufferPos := 0;
  FInBufferSize := 0;
  FInBuffer := '';
end;

function ATCPClientSocket.PeekAvailable: String;
begin
  if FInBufferSize = 0 then
    begin
      Result := '';
      exit;
    end;
  if FInBufferPos > 0 then
    Delete(FInBuffer, 1, FInBufferPos);
  if FInBufferSize < Length(FInBuffer) then
    SetLength(FInBuffer, FInBufferSize);
  Result := FInBuffer;
  FInBufferPos := 0;
end;

function ATCPClientSocket.LocateChar(const Ch: Char; const LocateNonMatch: Boolean): Integer;
var I : Integer;
begin
  if FInBufferSize = 0 then
    begin
      Result := -1;
      exit;
    end;
  if LocateNonMatch then
    I := PosNotChar(Ch, FInBuffer, FInBufferPos + 1) else
    I := PosChar(Ch, FInBuffer, FInBufferPos + 1);
  if I > FInBufferPos + FInBufferSize then
    I := 0;
  if I >= 1 then
    Result := I - FInBufferPos - 1 else
    Result := -1;
end;

function ATCPClientSocket.LocateChar(const C: CharSet; const LocateNonMatch: Boolean): Integer;
var I : Integer;
begin
  if FInBufferSize = 0 then
    begin
      Result := -1;
      exit;
    end;
  if LocateNonMatch then
    I := PosNotChar(C, FInBuffer, FInBufferPos + 1) else
    I := PosChar(C, FInBuffer, FInBufferPos + 1);
  if I > FInBufferPos + FInBufferSize then
    I := 0;
  if I >= 1 then
    Result := I - FInBufferPos - 1 else
    Result := -1;
end;

function ATCPClientSocket.LocateSequence(const Sequence: String; const CaseSensitive: Boolean): Integer;
var I : Integer;
begin
  if (Sequence = '') or (FInBufferSize = 0) then
    begin
      Result := -1;
      exit;
    end;
  I := PosStr(Sequence, FInBuffer, FInBufferPos + 1, CaseSensitive);
  if I > FInBufferPos + FInBufferSize then
    I := 0;
  if I >= 1 then
    Result := I - FInBufferPos - 1 else
    Result := -1;
end;

function ATCPClientSocket.MatchSequence(const Sequence: String; const CaseSensitive: Boolean): Boolean;
var L : Integer;
begin
  L := Length(Sequence);
  if (L = 0) or (FInBufferSize < L) then
    begin
      Result := False;
      exit;
    end;
  if CaseSensitive then
    Result := StrMatch(Sequence, FInBuffer, FInBufferPos + 1) else
    Result := StrMatchNoCase(Sequence, FInBuffer, FInBufferPos + 1);
end;

function ATCPClientSocket.MatchChar(const Ch: Char; const MatchNonMatch: Boolean; const SkipOnMatch: Boolean): Boolean;
begin
  if FInBufferSize < 1 then
    begin
      Result := False;
      exit;
    end;
  Result := (FInBuffer[FInBufferPos + 1] = Ch) xor MatchNonMatch;
  if Result and SkipOnMatch then
    Skip(1);
end;

function ATCPClientSocket.IsLineAvailable: Integer;
begin
  Result := LocateChar([#13, #10], False);
  if Result < 0 then
    exit;
  if FInBufferSize < Result + 2 then
    Result := -1;
end;

function ATCPClientSocket.ReadLine(var Line: String): Boolean;
var I    : Integer;
    C, D : Char;
begin
  I := IsLineAvailable;
  Result := I >= 0;
  if not Result then
    begin
      Line := '';
      exit;
    end;
  Line := ReadStr(I);
  ReadFromBuffer(C, 1, False);
  if Peek(D, 1) < 1 then
    exit;
  if ((C = #13) and (D = #10)) or ((C = #10) and (D = #13)) then
    Skip(1);
end;



{ Send                                                                         }
function ATCPClientSocket.SocketSend(const Buf; const Size: Integer; var Error: Integer): Integer;
var L : Integer;
    P : Pointer;
begin
  L := GetThrottledWriteSize(Size);
  if L = 0 then
    begin
      Result := 0;
      Error := 0;
      exit;
    end;
  P := @Buf;
  Result := WinSock.send(FSocketHandle, P^, L, 0);
  ActSocketSent(Result, Size, Error, Result);
end;

function ATCPClientSocket.SendData(const Buf; const Size: Integer; var Error: Integer): Integer;
var P : PChar;
    T, M : Integer;
begin
  Result := 0;
  Error := 0;
  P := @Buf;
  T := Size;
  While T > 0 do
    begin
      M := SocketSend(P^, T, Error);
      if M = 0 then
        break;
      Inc(Result, M);
      Inc(P, M);
      Dec(T, M);
    end;
end;

procedure ATCPClientSocket.SendBufferedData;
var P : TTCPSendBufferItem;
    L, Sent, Error : Integer;
    Q : PChar;
begin
  P := TTCPSendBufferItem(FOutBuffer.First);
  While Assigned(P) do
    begin
      L := Length(P.Value);
      Q := Pointer(P.Value);
      Sent := SendData(Q^, L, Error);
      if Sent = 0 then
        exit;
      Dec(FOutBufferSize, Sent);
      if Sent < L then
        begin
          Inc(Q, Sent);
          Move(Q^, Pointer(P.Value)^, L - Sent);
          SetLength(P.Value, L - Sent);
        end else
        begin
          FOutBuffer.RemoveFirst.Free;
          P := TTCPSendBufferItem(FOutBuffer.First);
        end;
    end;
end;

procedure ATCPClientSocket.TriggerSendBufferEmpty;
begin
  if FClosePending then
    SetClosed
  else
    inherited TriggerSendBufferEmpty;
end;

function ATCPClientSocket.Send(const Buf; const Size: Integer): Integer;
var S : String;
    L, Sent, Error : Integer;
    P : PChar;
begin
  Result := 0;
  L := Size;
  if L = 0 then
    exit;
  P := @Buf;
  CheckSocketConnected;

  // Send as much as possible and buffer the rest
  if FReadyToSend and FOutBuffer.IsEmpty then
    begin
      Sent := SendData(Buf, L, Error);
      Result := Sent;
      if Sent = L then
        exit;
      Inc(P, Sent);
      Dec(L, Sent);
    end;

  if not FUseSendBuffer then
    exit;

  SetLength(S, L);
  Move(P^, Pointer(S)^, L);
  FOutBuffer.Append(TTCPSendBufferItem.Create(S));
  Inc(FOutBufferSize, L);
  Result := Size;
end;

function ATCPClientSocket.SendStr(const Buf: String): Integer;
var S : String;
    L, Sent, Error : Integer;
    P : PChar;
begin
  Result := 0;
  L := Length(Buf);
  if L = 0 then
    exit;
  CheckSocketConnected;

  // Send as much as possible and buffer the rest
  if FReadyToSend and FOutBuffer.IsEmpty then
    begin
      P := Pointer(Buf);
      Sent := SendData(P^, L, Error);
      Result := L;
      if Sent = L then
        exit;
      Inc(P, Sent);
      Dec(L, Sent);
      SetLength(S, L);
      Move(P^, Pointer(S)^, L);
      FOutBuffer.Append(TTCPSendBufferItem.Create(S));
      Inc(FOutBufferSize, L);
      exit;
    end;

  if not FUseSendBuffer then
    exit;

  FOutBuffer.Append(TTCPSendBufferItem.Create(Buf));
  Inc(FOutBufferSize, L);
  Result := L;
end;



end.

