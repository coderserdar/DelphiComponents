{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2007 CnPack ������                       }
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

unit CnIP;
{* |<PRE>
================================================================================
* ������ƣ�����ͨѶ�����
* ��Ԫ���ƣ�IP ��������ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�������Sesame (sesamehch@163.com)
* ��    ע��
*           �ռ�����������ʹ��IPʱ������ʵ�ֺ���,����IP��ַ���㹦��
* ����ƽ̨��PWin2000Pro + Delphi 5.01
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2019.03.03 V1.3
*                �����ֺ�����Ϊ class ���ⲿҲ�ɵ���
*           2011.05.15 V1.2
*                ������127.0.0.1��ΪĬ�ϵ�ַ������
*           2009.08.14 V1.1
*                ���Ӷ� D2009 ��֧��
*           2008.04.14 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes, Controls, Winsock, StdCtrls, //Sockets,
  CnClasses, CnConsts, CnNetConsts;

const
  MAXIPNOTE = 255;
  IPJOIN = '.';
  IPADDRFORMAT = '%0:D.%1:D.%2:D.%3:D';

  SIO_GET_INTERFACE_LIST = $4004747F;
  IFF_UP = $00000001;
  IFF_BROADCAST = $00000002;
  IFF_LOOPBACK = $00000004;
  IFF_POINTTOPOINT = $00000008;
  IFF_MULTICAST = $00000010;

  IPNOTE1 = $FF000000;
  IPNOTE2 = $00FF0000;
  IPNOTE3 = $0000FF00;
  IPNOTE4 = $000000FF;

type
  TIPNotes = array[1..4] of Byte;
  {* IP��ַ�ĸ��ӽڵ�,��192.168.20.102,����Note[1]=192 ... Note[4]=102}

  TIP_NetType = (iptNone, iptANet, iptBNet, iptCNet, iptDNet, iptENet,
    iptBroadCast, iptKeepAddr);
  {* IP��ַ����, ����IP��ַ, A���ַ, B���ַ, C���ַ, D���ַ, E���ַ,
    �㲥��ַ, ������ַ(��127��)}

  TIP_Info = packed record
    IPAddress: Cardinal;                 // IP��ַ,�˴������δ洢
    SubnetMask: Cardinal;                // ��������,�˴������δ洢
    BroadCast: Cardinal;                 // �㲥��ַ,�˴������δ洢
    HostName: array[0..255] of AnsiChar; // ������
    NetType: TIP_NetType;                // IP��ַ����������
    Notes: TIPNotes;                     // IP��ַ�ĸ��ӽڵ�
    UpState: Boolean;                    // ����״̬
    Loopback: Boolean;                   // �Ƿ񻷻ص�ַ
    SupportBroadcast: Boolean;           // �Ƿ�֧�ֹ㲥
  end;
  TIPGroup = array of TIP_Info; //IP��ַ��

  sockaddr_gen = packed record
    AddressIn: sockaddr_in;
    filler: packed array[0..7] of AnsiChar;
  end;

  TINTERFACE_INFO = packed record
    iiFlags: u_long; // Interface flags
    iiAddress: sockaddr_gen; // Interface address
    iiBroadcastAddress: sockaddr_gen; // Broadcast address
    iiNetmask: sockaddr_gen; // Network mask
  end;

  { TCnIp }

  TCnIp = class(TCnComponent)
  private
    FIP: TIP_Info;
    FLocalIPs: TIPGroup;
    FNotes: TIPNotes;
    FWSAData: TWSAData;

    function GetIPAddress: string;
    procedure SetIPAddress(const Value: string);
    function GetBroadCastIP: string;
    function GetSubnetMask: string;
    procedure SetSubnetMask(const Value: string);
    function GetHosts: Cardinal;
    class function GetIPNotes(const aIP: string; var aResult: TIPNotes): Boolean;
    {* �ֽ�IP��ַ�����,IP����ʱ���׳�������Ϣ}
    function GetLocalIPCount: Integer;
    function GetComputerName: string;
    function GetMacAddress: string;
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string);
      override;
    function EnumLocalIP(var aLocalIP: TIPGroup): Integer;
    {* ö�ٱ�������IP�������������,����ֵΪIP��ַ��}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property LocalIPGroup: TIPGroup read FLocalIPs;
    {* ����IP��ַ�����Ϣ, ��������ʵ��IP��127.0.0.1}
    property LocalIPCount: Integer read GetLocalIPCount;
    {* ����IP��ַ��,�Ѿ��ų�127.0.0.1}
    class function IPTypeCheck(const aIP: string): TIP_NetType;
    {* ���IP��ַ�����Լ��Ƿ�Ϸ�}
    class function IPToInt(const aIP: string): Cardinal;
    {* ת��IP��ַΪ����}
    class function IntToIP(const aIP: Cardinal): string;
    {* ת������ΪIP��ַ}
    function NextIP(const aIP: string): string;
    {* ȡ��һ��IP��ַ}
    function PrevIP(const aIP: string): string;
    {* ȡǰһ��IP��ַ}
    function GetIpNum(const aStartIP, aEndIP: string): Integer;
    {* ��������IP��ַ֮���IP��}
    property Hosts: Cardinal read GetHosts;
    {* ����ָ����IP��ַ����������ʱ������}
    function GetIPByName(var aIp: string; const aName: string = ''): Boolean;
    {* ͨ���������Ƶõ�IP, aName=''��ʾȡ��������}
    function GetNameByIP(var aName: string; const aIP: string = ''): Boolean;
    {* ͨ��IP�õ���������, aIpAddr=''��ʾȡ����IP}
  published
    property IPAddress: string read GetIPAddress write SetIPAddress;
    {* IP��ַ�ַ�����ʽ,Ĭ��Ϊ���� IP ��ַ}
    property SubnetMask: string read GetSubnetMask write SetSubnetMask;
    {* IP��ַ����������}
    property ComputerName: string read GetComputerName;
    {* ��������}
    property MacAddress: string read GetMacAddress;
    {* ����Mac��ַ}
    property BroadCastIP: string read GetBroadCastIP;
    {* �㲥��ַ}
  end;

implementation

{$R-}

const
  WS2_32DLL = 'WS2_32.DLL';

type
  { ��Winsock 2.0���뺯��WSAIOCtl -- ��Win98/ME/2K/Xp and 95 OSR2, NT srv pack #3�²���Winsock 2 }
  TWSAIoctl = function (s: TSocket; cmd: DWORD; lpInBuffer: PByte; dwInBufferLen:
                        DWORD; lpOutBuffer: PByte; dwOutBufferLen: DWORD;
                        lpdwOutBytesReturned: LPDWORD; lpOverLapped: POINTER;
                        lpOverLappedRoutine: POINTER): Integer; stdcall;

var
  WSAIoctl: TWSAIoctl = nil;
  WS2_32DllHandle: THandle = 0;

procedure InitWSAIoctl;
begin
  WS2_32DllHandle := LoadLibrary(WS2_32DLL);
  if WS2_32DllHandle <> 0 then
  begin
    @WSAIoctl := GetProcAddress(WS2_32DllHandle, 'WSAIoctl');
  end;
end;

procedure FreeWSAIoctl;
begin
  if WS2_32DllHandle <> 0 then
    FreeLibrary(WS2_32DllHandle);
end;  

{ TCnIp }

constructor TCnIp.Create(AOwner: TComponent);
var
  IPs, I: Integer;
begin
  inherited Create(AOwner);
  IPs := EnumLocalIP(FLocalIPs);
  if IPs = 1 then // Only ONE IP address
  begin
    FIP.IPAddress := FLocalIPs[0].IPAddress;
    FIP.SubnetMask := FLocalIPs[0].SubnetMask;
  end
  else if IPs > 1 then // IF more than one, do not use 127.0.0.1 as default
  begin
    for I := 0 to IPs - 1 do
    begin
      if IntToIP(FLocalIPs[I].IPAddress) <> '127.0.0.1' then
      begin
        FIP.IPAddress := FLocalIPs[I].IPAddress;
        FIP.SubnetMask := FLocalIPs[I].SubnetMask;
        Break;
      end;
      if FIP.IPAddress = 0 then
      begin
        FIP.IPAddress := FLocalIPs[0].IPAddress;
        FIP.SubnetMask := FLocalIPs[0].SubnetMask;
      end;
    end;
  end;
end;

destructor TCnIp.Destroy;
begin

  inherited;
end;

procedure TCnIp.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnIPName;
  Author := SCnPack_Sesame;
  Email := SCnPack_SesameEmail;
  Comment := SCnIPComment;
end;

function TCnIp.GetLocalIPCount: Integer;
begin
  Result := Length(FLocalIPs) - 1;
end;

function TCnIp.GetIPAddress: string;
begin
  Result := IntToIP(FIP.IPAddress);
end;

function TCnIp.GetIpNum(const aStartIP, aEndIP: string): Integer;
begin
  Result := IPToInt(aEndIP) - IPToInt(aStartIP);
end;

class function TCnIp.GetIPNotes(const aIP: string; var aResult: TIPNotes): Boolean;
var
  iPos, iNote: Integer;
  sIP: string;

  function CheckIpNote(aNote: string): Byte;
  begin
    iNote := StrToInt(aNote);
    if (iNote < 0) or (iNote > MAXIPNOTE) then
      raise Exception.Create(aNote + SCnErrorAddrRang);
    Result := iNote;
  end;
begin
  iPos := Pos(IPJOIN, aIP);
  aResult[1] := CheckIpNote(Copy(aIP, 1, iPos - 1));
  sIP := Copy(aIP, iPos + 1, 20);
  iPos := Pos(IPJOIN, sIP);
  aResult[2] := CheckIpNote(Copy(sIP, 1, iPos - 1));
  sIP := Copy(sIP, iPos + 1, 20);
  iPos := Pos(IPJOIN, sIP);
  aResult[3] := CheckIpNote(Copy(sIP, 1, iPos - 1));
  aResult[4] := CheckIpNote(Copy(sIP, iPos + 1, 20));
  Result := aResult[1] > 0;
end;

class function TCnIp.IntToIP(const aIP: Cardinal): string;
var
  Notes: TIPNotes;
begin
  Notes[1] := aIP and IPNOTE1 shr 24;
  Notes[2] := aIP and IPNOTE2 shr 16;
  Notes[3] := aIP and IPNOTE3 shr 8;
  Notes[4] := aIP and IPNOTE4;
  Result := Format(IPADDRFORMAT, [Notes[1], Notes[2], Notes[3], Notes[4]]);
end;

class function TCnIp.IPToInt(const aIP: string): Cardinal;
var
  Notes: TIPNotes;
begin
  Result := 0;
  if IPTypeCheck(aIP) = iptNone then
  begin
    //raise Exception.Create(SCnErrorAddress);
    Exit;
  end;
  if GetIPNotes(aIP, Notes) then
  begin
    Result := Result or Notes[1] shl 24 or Notes[2] shl 16 or Notes[3] shl 8
      or Notes[4];
  end;
end;

class function TCnIp.IPTypeCheck(const aIP: string): TIP_NetType;
var
  Notes: TIPNotes;
begin
  Result := iptNone;
  if GetIPNotes(aIP, Notes) then
  begin
    case Notes[1] of
      1..126: Result := iptANet;
      127: Result := iptKeepAddr;
      128..191: Result := iptBNet;
      192..223: Result := iptCNet;
      224..239: Result := iptDNet;
      240..255: Result := iptENet;
      else
        Result := iptNone;
    end;
  end;
end;

function TCnIp.NextIP(const aIP: string): string;
begin
  Result := IntToIP(IPToInt(aIP) + 1);
end;

function TCnIp.PrevIP(const aIP: string): string;
begin
  Result := IntToIP(IPToInt(aIP) - 1);
end;

procedure TCnIp.SetIPAddress(const Value: string);
begin
  FIP.IPAddress := IPToInt(Value);
end;

function TCnIp.GetBroadCastIP: string;
var
  IpNote, MaskNote: TIPNotes;
begin
  Result := '255.255.255.255';
  if GetIPNotes(SubnetMask, MaskNote) and GetIPNotes(IPAddress, IpNote) then
  begin
    MaskNote[1] := not MaskNote[1];
    MaskNote[2] := not MaskNote[2];
    MaskNote[3] := not MaskNote[3];
    MaskNote[4] := not MaskNote[4];
    MaskNote[1] := MaskNote[1] or IpNote[1];
    MaskNote[2] := MaskNote[2] or IpNote[2];
    MaskNote[3] := MaskNote[3] or IpNote[3];
    MaskNote[4] := MaskNote[4] or IpNote[4];
    FIP.BroadCast := MaskNote[1] shl 24 or MaskNote[2] shl 16 or MaskNote[3]
      shl 8 or MaskNote[4];
    Result := IntToIP(FIP.BroadCast);
  end;
end;

function TCnIp.GetSubnetMask: string;
begin
  Result := IntToIP(FIP.SubnetMask);
end;

procedure TCnIp.SetSubnetMask(const Value: string);
begin
  FIP.SubnetMask := IPToInt(Value);
end;

function TCnIp.GetHosts: Cardinal;
var
  iHost: Int64;
begin
  Result := 0;
  if GetIPNotes(SubnetMask, FNotes) then
  begin
    FNotes[1] := not FNotes[1];
    FNotes[2] := not FNotes[2];
    FNotes[3] := not FNotes[3];
    FNotes[4] := not FNotes[4];
    iHost := FNotes[1] shl 24 or FNotes[2] shl 16 or FNotes[3] shl 8 or FNotes[4]
      - 2;
    if iHost > 0 then
      Result := iHost;
  end;
end;

function TCnIp.GetComputerName: string;
var
  sName: array[0..255] of AnsiChar;
begin
  WSAStartup(2, FWSAData);
  try
    GetHostName(@sName, SizeOf(sName));
    Result := {$IFDEF UNICODE}String{$ENDIF}(sName);
  finally
    WSACleanup;
  end;
end;

function TCnIp.GetMacAddress: string;
var
  Lib: Cardinal;
  Func: function(GUID: PGUID): Longint; stdcall;
  GUID1, GUID2: TGUID;
begin
  Result := '';
  Lib := LoadLibrary('rpcrt4.dll');
  if Lib <> 0 then
  try
    if Win32Platform <> VER_PLATFORM_WIN32_NT then
      @Func := GetProcAddress(Lib, 'UuidCreate')
    else
      @Func := GetProcAddress(Lib, 'UuidCreateSequential');
    if Assigned(Func) then
    begin
      if (Func(@GUID1) = 0) and
        (Func(@GUID2) = 0) and
        (GUID1.D4[2] = GUID2.D4[2]) and
        (GUID1.D4[3] = GUID2.D4[3]) and
        (GUID1.D4[4] = GUID2.D4[4]) and
        (GUID1.D4[5] = GUID2.D4[5]) and
        (GUID1.D4[6] = GUID2.D4[6]) and
        (GUID1.D4[7] = GUID2.D4[7]) then
      begin
        Result :=
          IntToHex(GUID1.D4[2], 2) + '-' +
          IntToHex(GUID1.D4[3], 2) + '-' +
          IntToHex(GUID1.D4[4], 2) + '-' +
          IntToHex(GUID1.D4[5], 2) + '-' +
          IntToHex(GUID1.D4[6], 2) + '-' +
          IntToHex(GUID1.D4[7], 2);
      end;
    end;
  finally
    FreeLibrary(Lib);
  end;
end;

function TCnIp.GetIPByName(var aIP: string; const aName: string): Boolean;
var
  pHost: PHostEnt;
  sName: array[0..256] of Char;
begin
  StrPCopy(sName, aName);
  WSAStartup($101, FWSAData);
  try
    if sName = '' then
      GetHostName(PAnsiChar({$IFDEF UNICODE}AnsiString{$ELSE}string{$ENDIF}(sName)), SizeOf(sName));
    pHost := GetHostByName(@sName);
    Result := pHost <> nil;
    if Result then
      aIP := {$IFDEF UNICODE}String{$ENDIF}(inet_ntoa(PInAddr(pHost^.h_addr_list^)^));
  finally
    WSACleanup;
  end;
end;

function TCnIp.GetNameByIP(var aName: string; const aIP: string): Boolean;
var
  HostEnt: PHostEnt;
  InetAddr: dword;
  sIP: string;
begin
  Result := False;
  sIP := aIP;
  aName := '';
  if sIP = '' then
    Exit;
  WSAStartup(2, FWSAData);
  try
    InetAddr := inet_addr(PAnsiChar({$IFDEF UNICODE}AnsiString{$ENDIF}(sIP)));
    HostEnt := GetHostByAddr(@InetAddr, Length(sIP), PF_Inet);
    Result := HostEnt <> nil;
    if Result then
      aName := {$IFDEF UNICODE}String{$ENDIF}(StrPas(Hostent^.h_name));
  finally
    WSACleanup;
  end;
end;

{-------------------------------------------------------------------
1. ����һ��Socket
2. ����WSAIOCtl��ȡ��������
3. ��ÿ�����ӣ���ȡ����IP�����롢�㲥��ַ��״̬
4. ����Ϣ��䵽IP������
5. �����ر�Socket
--------------------------------------------------------------------}
function TCnIp.EnumLocalIP(var aLocalIP: TIPGroup): Integer;
var
  skLocal: TSocket;
  iIP: Integer;
  PtrA: pointer;
  BytesReturned, SetFlags: u_long;
  pAddrInet: Sockaddr_IN;
  Buffer: array[0..20] of TINTERFACE_INFO;
begin
  Result := 0;

  WSAStartup($101, FWSAData);
  try
    skLocal := Socket(AF_INET, SOCK_STREAM, 0); // Open a socket
    if (skLocal = INVALID_SOCKET) then
      Exit;

    try // Call WSAIoCtl
      PtrA := @bytesReturned;
      if (WSAIoCtl(skLocal, SIO_GET_INTERFACE_LIST, nil, 0, @Buffer, 1024, PtrA,
        nil, nil) <> SOCKET_ERROR) then
      begin // If ok, find out how
        Result := BytesReturned div SizeOf(TINTERFACE_INFO);
        SetLength(aLocalIP, Result);
        for iIP := 0 to Result - 1 do // For every interface
        begin
          pAddrInet := Buffer[iIP].iiAddress.AddressIn;
          aLocalIP[iIP].IPAddress := IPToInt({$IFDEF UNICODE}String{$ENDIF}(inet_ntoa(pAddrInet.sin_addr)));
          pAddrInet := Buffer[iIP].iiNetMask.AddressIn;
          aLocalIP[iIP].SubnetMask := IPToInt({$IFDEF UNICODE}String{$ENDIF}(inet_ntoa(pAddrInet.sin_addr)));
          pAddrInet := Buffer[iIP].iiBroadCastAddress.AddressIn;
          aLocalIP[iIP].BroadCast := IPToInt({$IFDEF UNICODE}String{$ENDIF}(inet_ntoa(pAddrInet.sin_addr)));
          SetFlags := Buffer[iIP].iiFlags;
          aLocalIP[iIP].UpState := (SetFlags and IFF_UP) = IFF_UP;
          aLocalIP[iIP].Loopback := (SetFlags and IFF_LOOPBACK) = IFF_LOOPBACK;
          aLocalIP[iIP].SupportBroadcast := (SetFlags and IFF_BROADCAST) =
            IFF_BROADCAST;
        end;
      end;
    except
      ;
    end;
    CloseSocket(skLocal);
  finally
    WSACleanUp;
  end;
end;

initialization
  InitWSAIoctl;

finalization
  FreeWSAIoctl;

end.

