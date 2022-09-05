{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2020 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnUDP;
{* |<PRE>
================================================================================
* ������ƣ�����ͨѶ�����
* ��Ԫ���ƣ�UDP ͨѶ��Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ��    ע�������� TCnUDP��ʹ�÷�������ʽ���� UDP ͨѶ��֧�ֹ㲥
* ����ƽ̨��PWin2000Pro + Delphi 5.01
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2008.11.28 V1.1
*                ����һ���ƽ��ջ�������С������
*           2003.11.21 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, Classes, SysUtils, WinSock, Forms, contnrs;

const
  csDefRecvBuffSize = 4096;
  csDefUDPSendBuffSize = 256 * 1024;
  csDefUDPRecvBuffSize = 256 * 1024;

type

//==============================================================================
// UDP ͨѶ��
//==============================================================================

{ TCnUDP }

  TOnReceive = procedure(Sender: TComponent; Buffer: Pointer; Len: Integer;
    FromIP: string; Port: Integer) of object;
  {* ���յ������¼�
   |<PRE>
     Sender     - TCnUDP ����
     Buffer     - ���ݻ�����
     Len        - ���ݻ���������
     FromIP     - ������Դ IP
     Port       - ������Դ�˿ں�
   |</PRE>}

  TCnUDP = class(TComponent)
  {* ʹ�÷�������ʽ���� UDP ͨѶ���ࡣ֧�ֹ㲥�����ݶ��еȡ�}
  private
    FRemoteHost: string;
    FRemotePort: Integer;
    FLocalPort: Integer;
    FSocketWindow: HWND;
    FOnDataReceived: TOnReceive;
    FListening: Boolean;
    Wait_Flag: Boolean;
    RemoteAddress: TSockAddr;
    RemoteHostS: PHostEnt;
    Succeed: Boolean;
    Procing: Boolean;
    EventHandle: THandle;
    ThisSocket: TSocket;
    Queue: TQueue;
    FLastError: Integer;
    FRecvBufSize: Cardinal;
    FRecvBuf: Pointer;
    FBindAddr: string;
    FSockCount: Integer;
    FUDPSendBufSize: Cardinal;
    FUDPRecvBufSize: Cardinal;
    procedure WndProc(var Message: TMessage);
    function ResolveRemoteHost(ARemoteHost: string): Boolean;
    procedure SetLocalPort(NewLocalPort: Integer);
    procedure ProcessIncomingdata;
    procedure ProcessQueue;
    procedure FreeQueueItem(P: Pointer);
    function GetQueueCount: Integer;
    procedure SetupLastError;
    function GetLocalHost: string;
    procedure SetRecvBufSize(const Value: Cardinal);
    procedure SetBindAddr(const Value: string);
    function SockStartup: Boolean;
    procedure SockCleanup;
    procedure SetUDPRecvBufSize(const Value: Cardinal);
    procedure SetUDPSendBufSize(const Value: Cardinal);
  protected
    procedure Wait;
    procedure UpdateBinding;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function SendStream(DataStream: TStream; BroadCast: Boolean = False): Boolean;
    {* ����һ������������� BroadCase Ϊ�棬ִ�� UDP �㲥�����������ݵ�
       RomoteHost �Ļ����ϵ� RemotePort �˿�}
    function SendBuffer(Buff: Pointer; Length: Integer; BroadCast:
      Boolean = False): Boolean;
    {* ����һ�����ݿ顣��� BroadCase Ϊ�棬ִ�� UDP �㲥�����������ݵ�
       RomoteHost �Ļ����ϵ� RemotePort �˿�}
    procedure ClearQueue;
    {* ������ݶ��С�����û�������������յ������ݣ������������ݰ��ŵ�����
       �����У����ø÷�����������ݶ���}
    function ProcessRecv: Boolean;
    {* ����� UDP �ӿڵĽ������ݡ����� CnUDP ����� OnDataReceived �������߳�
       ��Ϣ�����е��õģ�������̴߳�����Ҫ�ȴ� UDP ���ն���ϣ������������Ϣ��
       ���Ե��øú�����}

    property LastError: Integer read FLastError;
    {* ���һ�δ���Ĵ���ţ�ֻ������}
    property Listening: Boolean read FListening;
    {* ��ʾ��ǰ�Ƿ����ڼ������ض˿ڣ�ֻ������}
    property QueueCount: Integer read GetQueueCount;
    {* ��ǰ���ݶ��еĳ��ȣ�ֻ������}
    property BindAddr: string read FBindAddr write SetBindAddr;
    {* �󶨱��ص�ַ}
  published
    property RemoteHost: string read FRemoteHost write FRemoteHost;
    {* Ҫ���� UDP ���ݵ�Ŀ��������ַ}
    property RemotePort: Integer read FRemotePort write FRemotePort;
    {* Ҫ���� UDP ���ݵ�Ŀ�������˿ں�}
    property LocalHost: string read GetLocalHost;
    {* ���ر��� IP ��ַ��ֻ������}
    property LocalPort: Integer read FLocalPort write SetLocalPort;
    {* ���ؼ����Ķ˿ں�}
    property RecvBufSize: Cardinal read FRecvBufSize write SetRecvBufSize default csDefRecvBuffSize;
    {* ���յ����ݻ�������С}
    property UDPSendBufSize: Cardinal read FUDPSendBufSize write SetUDPSendBufSize default csDefUDPSendBuffSize;
    {* UDP ���͵����ݻ�������С}
    property UDPRecvBufSize: Cardinal read FUDPRecvBufSize write SetUDPRecvBufSize default csDefUDPRecvBuffSize;
    {* UDP ���յ����ݻ�������С}
    property OnDataReceived: TOnReceive read FOnDataReceived write
      FOnDataReceived;
    {* ���յ� UDP ���ݰ��¼�}
  end;

// ȡ�㲥��ַ
procedure GetBroadCastAddress(sInt: TStrings);

// ȡ���� IP ��ַ
procedure GetLocalIPAddress(sInt: TStrings);

implementation

{$R-}

//==============================================================================
// ��������
//==============================================================================

// ��Winsock 2.0���뺯��WSAIOCtl
function WSAIoctl(s: TSocket; cmd: DWORD; lpInBuffer: PCHAR; dwInBufferLen:
  DWORD;
  lpOutBuffer: PCHAR; dwOutBufferLen: DWORD;
  lpdwOutBytesReturned: LPDWORD;
  lpOverLapped: POINTER;
  lpOverLappedRoutine: POINTER): Integer; stdcall; external 'WS2_32.DLL';

const
  SIO_GET_INTERFACE_LIST = $4004747F;
  IFF_UP = $00000001;
  IFF_BROADCAST = $00000002;
  IFF_LOOPBACK = $00000004;
  IFF_POINTTOPOINT = $00000008;
  IFF_MULTICAST = $00000010;

type
  sockaddr_gen = packed record
    AddressIn: sockaddr_in;
    filler: packed array[0..7] of AnsiChar;
  end;

  INTERFACE_INFO = packed record
    iiFlags: u_long;                    // Interface flags
    iiAddress: sockaddr_gen;            // Interface address
    iiBroadcastAddress: sockaddr_gen;   // Broadcast address
    iiNetmask: sockaddr_gen;            // Network mask
  end;

// ȡ�㲥��ַ
procedure DoGetIPAddress(sInt: TStrings; IsBroadCast: Boolean);
var
  s: TSocket;
  wsaD: WSADATA;
  NumInterfaces: Integer;
  BytesReturned, SetFlags: u_long;
  pAddr, pMask, pCast: TInAddr;
  pAddrStr: string;
  PtrA: pointer;
  Buffer: array[0..20] of INTERFACE_INFO;
  i: Integer;
begin
  WSAStartup($0101, wsaD);              // Start WinSock
  s := Socket(AF_INET, SOCK_STREAM, 0); // Open a socket
  if (s = INVALID_SOCKET) then
    exit;

  try                                   // Call WSAIoCtl
    PtrA := @bytesReturned;
    if (WSAIoCtl(s, SIO_GET_INTERFACE_LIST, nil, 0, @Buffer, 1024, PtrA, nil,
      nil) <> SOCKET_ERROR) then
    begin                               // If ok, find out how
      // many interfaces exist
      NumInterfaces := BytesReturned div SizeOf(INTERFACE_INFO);
      sInt.Clear;
      for i := 0 to NumInterfaces - 1 do // For every interface
      begin
        SetFlags := Buffer[i].iiFlags;
        if (SetFlags and IFF_BROADCAST = IFF_BROADCAST) and not
          (SetFlags and IFF_LOOPBACK = IFF_LOOPBACK) then
        begin
          pAddr := Buffer[i].iiAddress.AddressIn.sin_addr;
          pMask := Buffer[i].iiNetmask.AddressIn.sin_addr;
          if IsBroadCast then
          begin
            pCast.S_addr := pAddr.S_addr or not pMask.S_addr;
            pAddrStr := string(inet_ntoa(pCast));
          end
          else
          begin
            pAddrStr := string(inet_ntoa(pAddr));
          end;
            
          if sInt.IndexOf(pAddrStr) < 0 then
            sInt.Add(pAddrStr);
        end;
      end;
    end;
  except
    ;
  end;
  CloseSocket(s);
  WSACleanUp;
end;

// ȡ���� IP ��ַ
procedure GetLocalIPAddress(sInt: TStrings);
begin
  DoGetIPAddress(sInt, False);
end;  

// ȡ�㲥��ַ
procedure GetBroadCastAddress(sInt: TStrings);
begin
  DoGetIPAddress(sInt, True);
end;

//==============================================================================
// UDP ͨѶ��
//==============================================================================

{ TCnUDP }

const
  WM_ASYNCHRONOUSPROCESS = WM_USER + 101;
  Const_cmd_true = 'TRUE';

type
  PRecvDataRec = ^TRecvDataRec;
  TRecvDataRec = record
    FromIP: string[128];
    FromPort: u_short;
    Buff: Pointer;
    BuffSize: Integer;
  end;

constructor TCnUDP.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Queue := TQueue.Create;
  FListening := False;
  Procing := False;
  FRecvBufSize := csDefRecvBuffSize;
  FUDPSendBufSize := csDefUDPSendBuffSize;
  FUDPRecvBufSize := csDefUDPRecvBuffSize;
  FBindAddr := '0.0.0.0';

  GetMem(RemoteHostS, MAXGETHOSTSTRUCT);
  FSocketWindow := AllocateHWND(WndProc);
  EventHandle := CreateEvent(nil, True, False, '');
  if SockStartup then
  begin
    ThisSocket := Socket(AF_INET, SOCK_DGRAM, 0);
    if ThisSocket = TSocket(INVALID_SOCKET) then
    begin
      SetupLastError;
      SockCleanup;
      Exit;
    end;
    setsockopt(ThisSocket, SOL_SOCKET, SO_DONTLINGER, Const_cmd_true, 4);
    setsockopt(ThisSocket, SOL_SOCKET, SO_BROADCAST, Const_cmd_true, 4);
    FListening := True;
  end;
end;

destructor TCnUDP.Destroy;
begin
  if FRecvBuf <> nil then
  begin
    FreeMem(FRecvBuf);
    FRecvBuf := nil;
  end;

  ClearQueue;
  Queue.Free;
  FreeMem(RemoteHostS, MAXGETHOSTSTRUCT);
  DeallocateHWND(FSocketWindow);
  CloseHandle(EventHandle);
  if ThisSocket <> 0 then
    closesocket(ThisSocket);
  if FListening then
    SockCleanup;
  inherited Destroy;
end;

procedure TCnUDP.UpdateBinding;
var
  Data: DWORD;
  Addr: TSockAddr;
begin
  if not (csDesigning in ComponentState) then
  begin
    FListening := False;

    if ThisSocket <> 0 then
    begin
      closesocket(ThisSocket);
      SockCleanup;
    end;

    if SockStartup then
    begin
      ThisSocket := Socket(AF_INET, SOCK_DGRAM, 0);
      if ThisSocket = TSocket(INVALID_SOCKET) then
      begin
        SockCleanup;
        SetupLastError;
        Exit;
      end;
    end;

    FillChar(Addr, SizeOf(Addr), 0);
    Addr.sin_addr.S_addr := Inet_Addr(PAnsiChar(AnsiString(FBindAddr)));
    Addr.sin_family := AF_INET;
    Addr.sin_port := htons(FLocalPort);
    Wait_Flag := False;
    if WinSock.Bind(ThisSocket, Addr, SizeOf(Addr)) =
      SOCKET_ERROR then
    begin
      SetupLastError;
      SockCleanup;
      Exit;
    end;

    // Allow to send to 255.255.255.255
    Data := 1;
    WinSock.setsockopt(ThisSocket, SOL_SOCKET, SO_BROADCAST,
      PAnsiChar(@Data), SizeOf(Data));
    Data := FUDPSendBufSize;
    WinSock.setsockopt(ThisSocket, SOL_SOCKET, SO_SNDBUF,
      PAnsiChar(@Data), SizeOf(Data));
    Data := FUDPRecvBufSize;
    WinSock.setsockopt(ThisSocket, SOL_SOCKET, SO_RCVBUF,
      PAnsiChar(@Data), SizeOf(Data));
    WSAAsyncSelect(ThisSocket, FSocketWindow, WM_ASYNCHRONOUSPROCESS, FD_READ);
    FListening := True;
  end;
end;

procedure TCnUDP.Loaded;
begin
  inherited;
  UpdateBinding;
end;

procedure TCnUDP.SetBindAddr(const Value: string);
begin
  if Value <> FBindAddr then
  begin
    FBindAddr := Value;
    UpdateBinding;
  end;
end;

procedure TCnUDP.SetLocalPort(NewLocalPort: Integer);
begin
  if NewLocalPort <> FLocalPort then
  begin
    FLocalPort := NewLocalPort;
    UpdateBinding;
  end;
end;

function TCnUDP.ResolveRemoteHost(ARemoteHost: string): Boolean;
var
  Buf: array[0..127] of AnsiChar;
begin
  Result := False;
  if not FListening then Exit;
  try
    RemoteAddress.sin_addr.S_addr := Inet_Addr(PAnsiChar(StrPCopy(Buf, {$IFDEF UNICODE}AnsiString{$ENDIF}(ARemoteHost))));
    if RemoteAddress.sin_addr.S_addr = SOCKET_ERROR then
    begin
      Wait_Flag := False;
      WSAAsyncGetHostByName(FSocketWindow, WM_ASYNCHRONOUSPROCESS, Buf,
        PAnsiChar(RemoteHostS), MAXGETHOSTSTRUCT);
      repeat
        Wait;
      until Wait_Flag;
      if Succeed then
      begin
        with RemoteAddress.sin_addr.S_un_b do
        begin
          s_b1 := remotehostS.h_addr_list^[0];
          s_b2 := remotehostS.h_addr_list^[1];
          s_b3 := remotehostS.h_addr_list^[2];
          s_b4 := remotehostS.h_addr_list^[3];
        end;
      end;
    end;
  except
    ;
  end;
  if RemoteAddress.sin_addr.S_addr <> 0 then
    Result := True;
  if not Result then
    SetupLastError;
end;

function TCnUDP.SendStream(DataStream: TStream; BroadCast: Boolean): Boolean;
var
  Buff: Pointer;
begin
  GetMem(Buff, DataStream.Size);
  try
    DataStream.Position := 0;
    DataStream.Read(Buff^, DataStream.Size);
    Result := SendBuffer(Buff, DataStream.Size, BroadCast);
  finally
    FreeMem(Buff);
  end;
end;

function TCnUDP.SendBuffer(Buff: Pointer; Length: Integer;
  BroadCast: Boolean): Boolean;
var
  Hosts: TStrings;
  i: Integer;
  
  function DoSendBuffer(Buff: Pointer; Length: Integer; Host: string): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    try
      if not ResolveRemoteHost(Host) then
        Exit;
      RemoteAddress.sin_family := AF_INET;
      RemoteAddress.sin_port := htons(FRemotePort);
      i := SizeOf(RemoteAddress);
      if WinSock.sendto(ThisSocket, Buff^, Length, 0, RemoteAddress, i)
        <> SOCKET_ERROR then
        Result := True
      else
        SetupLastError;
    except
      SetupLastError;
    end;
  end;
begin
  if BroadCast then
  begin
    Result := False;
    Hosts := TStringList.Create;
    try
      GetBroadCastAddress(Hosts);
      for i := 0 to Hosts.Count - 1 do
        if DoSendBuffer(Buff, Length, Hosts[i]) then
          Result := True;
    finally
      Hosts.Free;
    end;
  end
  else
    Result := DoSendBuffer(Buff, Length, FRemoteHost);
end;

function TCnUDP.GetQueueCount: Integer;
begin
  Result := Queue.Count;
end;

procedure TCnUDP.FreeQueueItem(P: Pointer);
var
  Rec: PRecvDataRec;
begin
  Rec := PRecvDataRec(P);
  Rec.FromIP := '';
  FreeMem(Rec.Buff);
  FreeMem(Rec);
end;

procedure TCnUDP.ClearQueue;
var
  Rec: PRecvDataRec;
begin
  while Queue.Count > 0 do
  begin
    Rec := Queue.Pop;
    FreeQueueItem(Rec);
  end;
end;

procedure TCnUDP.ProcessQueue;
var
  Rec: PRecvDataRec;
begin
  if Procing then Exit;
  Procing := True;
  try
    while Queue.Count > 0 do
    begin
      Rec := Queue.Pop;
      if Assigned(FOnDataReceived) then
        FOnDataReceived(Self, Rec.Buff, Rec.BuffSize, string(Rec.FromIP), Rec.FromPort);
      FreeQueueItem(Rec);
    end;
  finally
    Procing := False;
  end;
end;

function TCnUDP.ProcessRecv: Boolean;
var
  Unicode: Boolean;
  MsgExists: Boolean;
  Msg: TMsg;
begin
  Unicode := IsWindowUnicode(FSocketWindow);
  if Unicode then
    MsgExists := PeekMessageW(Msg, FSocketWindow, 0, 0, PM_REMOVE)
  else
    MsgExists := PeekMessageA(Msg, FSocketWindow, 0, 0, PM_REMOVE);

  if MsgExists then
  begin
    if Msg.Message <> WM_QUIT then
    begin
      TranslateMessage(Msg);
      if Unicode then
        DispatchMessageW(Msg)
      else
        DispatchMessageA(Msg);
    end;
  end;
  Result := MsgExists;
end;

procedure TCnUDP.WndProc(var Message: TMessage);
begin
  if FListening then
  begin
    with Message do
    begin
      if Msg = WM_ASYNCHRONOUSPROCESS then
      begin
        if LParamLo = FD_READ then
        begin
          ProcessIncomingdata;
          if not Procing then
            ProcessQueue;
        end
        else
        begin
          Wait_Flag := True;
          if LParamHi > 0 then
            Succeed := False
          else
            Succeed := True;
        end;
        SetEvent(EventHandle);
      end
      else
        Result := DefWindowProc(FSocketWindow, Msg, WParam, LParam);
    end;
  end;
end;

procedure TCnUDP.ProcessIncomingdata;
var
  from: TSockAddr;
  i: Integer;
  Rec: PRecvDataRec;
  IBuffSize: Integer;
begin
  i := SizeOf(from);
  if FRecvBuf = nil then
    GetMem(FRecvBuf, FRecvBufSize);

  IBuffSize := WinSock.recvfrom(ThisSocket, FRecvBuf^, FRecvBufSize, 0, from, i);
  if (IBuffSize > 0) and Assigned(FOnDataReceived) then
  begin
    GetMem(Rec, SizeOf(TRecvDataRec));
    ZeroMemory(Rec, SizeOf(TRecvDataRec));
    Rec.FromIP := ShortString(Format('%d.%d.%d.%d', [Ord(from.sin_addr.S_un_b.S_b1),
      Ord(from.sin_addr.S_un_b.S_b2), Ord(from.sin_addr.S_un_b.S_b3),
        Ord(from.sin_addr.S_un_b.S_b4)]));
    Rec.FromPort := ntohs(from.sin_port);
    GetMem(Rec.Buff, IBuffSize);
    Rec.BuffSize := IBuffSize;
    CopyMemory(Rec.Buff, FRecvBuf, IBuffSize);
    Queue.Push(Rec);
  end;
end;

procedure WaitforSync(Handle: THandle);
begin
  repeat
    if MsgWaitForMultipleObjects(1, Handle, False, INFINITE, QS_ALLINPUT)
      = WAIT_OBJECT_0 + 1 then
      Application.ProcessMessages
    else
      Break;
  until False;
end;

procedure TCnUDP.Wait;
begin
  WaitforSync(EventHandle);
  ResetEvent(EventHandle);
end;

procedure TCnUDP.SetupLastError;
begin
  FLastError := WSAGetLastError;
end;

procedure TCnUDP.SockCleanup;
begin
  if FSockCount > 0 then
  begin
    Dec(FSockCount);
    if FSockCount = 0 then
      WSACleanup;
  end;
end;

function TCnUDP.SockStartup: Boolean;
var
  wsaData: TWSAData;
begin
  if FSockCount = 0 then
  begin
    Result := WSAStartup($0101, wsaData) = 0;
    if not Result then
      Exit;
  end;
  Inc(FSockCount);
  Result := True;
end;

function TCnUDP.GetLocalHost: string;
var
  p: PHostEnt;
  s: array[0..256] of AnsiChar;
begin
  SockStartup;
  try
    GetHostName(@s, 256);
    p := GetHostByName(@s);
    Result := string(inet_ntoa(PInAddr(p^.h_addr_list^)^));
  finally
    SockCleanup;
  end;
end;

procedure TCnUDP.SetRecvBufSize(const Value: Cardinal);
begin
  if FRecvBufSize <> Value then
  begin
    FRecvBufSize := Value;
    if FRecvBuf <> nil then
    begin
      // �ͷţ��ȴ��´���Ҫʱ���·���
      FreeMem(FRecvBuf);
      FRecvBuf := nil;
    end;  
  end;
end;

procedure TCnUDP.SetUDPRecvBufSize(const Value: Cardinal);
var
  Data: DWORD;
begin
  FUDPRecvBufSize := Value;
  if FListening then
  begin
    Data := FUDPRecvBufSize;
    WinSock.setsockopt(ThisSocket, SOL_SOCKET, SO_RCVBUF,
      PAnsiChar(@Data), SizeOf(Data));
  end;
end;

procedure TCnUDP.SetUDPSendBufSize(const Value: Cardinal);
var
  Data: DWORD;
begin
  FUDPSendBufSize := Value;
  if FListening then
  begin
    Data := FUDPSendBufSize;
    WinSock.setsockopt(ThisSocket, SOL_SOCKET, SO_SNDBUF,
      PAnsiChar(@Data), SizeOf(Data));
  end;
end;

end.

