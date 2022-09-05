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

unit CnPing;
{* |<PRE>
================================================================================
* ������ƣ�����ͨѶ�����
* ��Ԫ���ƣ�Ping ͨѶ��Ԫ
* ��Ԫ���ߣ�������Sesame (sesamehch@163.com)
* ��    ע�������� TCnPing
* ����ƽ̨��PWin2000Pro + Delphi 5.01
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2008.04.04 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, SysUtils, Classes, Controls, Winsock, StdCtrls, //Sockets,
  CnClasses, CnConsts, CnNetConsts;

type

  PCnIPOptionInformation = ^TCnIPOptionInformation;
  TCnIPOptionInformation = packed record
    TTL: Byte; // Time To Live (used for traceroute)
    TOS: Byte; // Type Of Service (usually 0)
    Flags: Byte; // IP header flags (usually 0)
    OptionsSize: Byte; // Size of options data (usually 0, max 40)
    OptionsData: PAnsiChar; // Options data buffer
  end;

  PCnIcmpEchoReply = ^TCnIcmpEchoReply;
  TCnIcmpEchoReply = packed record
    Address: DWORD; // replying address
    Status: DWORD; // IP status value (see below)
    RTT: DWORD; // Round Trip Time in milliseconds
    DataSize: Word; // reply data size
    Reserved: Word;
    Data: Pointer; // pointer to reply data buffer
    Options: TCnIPOptionInformation; // reply options
  end;

  TIpInfo = record
    Address: Int64;
    IP: string;
    Host: string;
  end;

  TOnReceive = procedure(Sender: TComponent; IPAddr, HostName: string;
    TTL, TOS: Byte) of object;

  TOnError = procedure(Sender: TComponent; IPAddr, HostName: string;
    TTL, TOS: Byte; ErrorMsg: string) of object;

//==============================================================================
// Ping ͨѶ��
//==============================================================================

  { TCnPing }

  TCnPing = class(TCnComponent)
  {* ͨ������ICMP.DLL���еĺ�����ʵ��Ping���ܡ�}
  private
    FHICMP: THandle;
    FRemoteHost: string;
    FRemoteIP: string;
    FIPAddress: Int64;
    FTTL: Byte;
    FTimeOut: DWord;
    FPingCount: Integer;
    FDelay: Integer;
    FOnError: TOnError;
    FOnReceived: TOnReceive;
    FDataString: string;
    FWSAData: TWSAData;
    FIP: TIpInfo;

    procedure SetPingCount(const Value: Integer);
    procedure SetRemoteHost(const Value: string);
    procedure SetTimeOut(const Value: DWord);
    procedure SetTTL(const Value: Byte);
    procedure SetDataString(const Value: string);
    procedure SetRemoteIP(const Value: string);
    function PingIP_Host(const aIP: TIpInfo; const Data; Count: Cardinal;
      var aReply: string): Integer;
    {* ���趨������Data(�����ͻ�����)Pingһ�β����ؽ����Count��ʾ���ݳ��� }
    function GetReplyString(aResult: Integer; aIP: TIpInfo;
      pIPE: PCnIcmpEchoReply): string;
    {* ���ؽ���ַ�����}
    function GetDataString: string;
    function GetIPByName(const aName: string; var aIP: string): Boolean;
    {* ͨ���������ƻ�ȡIP��ַ}
    function SetIP(aIPAddr, aHost: string; var aIP: TIpInfo): Boolean;
    {* ͨ���������ƻ�IP��ַ�������IP��Ϣ}
  protected
    procedure GetComponentInfo(var AName, Author, Email, Comment: string);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Ping(var aReply: string): Boolean;
    {* ����ѭ��Ping,ѭ��������PingCount������ָ����}
    function PingOnce(var aReply: string): Boolean; overload;
    {* ���趨������Pingһ�β����ؽ����}
    function PingOnce(const aIP: string; var aReply: string): Boolean; overload;
    {* ��ָ��IP����һ��Ping�����ؽ����}
    function PingFromBuffer(var Buffer; Count: Longint; var aReply: string):
      Boolean;
    {* �Բ���Buffer������Pingһ�β���ȡ���ؽ����}
  published
    property RemoteIP: string read FRemoteIP write SetRemoteIP;
    {* ҪPing��Ŀ��������ַ��ֻ֧��ip}
    property RemoteHost: string read FRemoteHost write SetRemoteHost;
    {* Ҫping��Ŀ����������������������ʱ�Ḳ�� RemoteIP ������}
    property PingCount: Integer read FPingCount write SetPingCount default 4;
    {* ����Ping����ʱ���ж��ٴ����ݷ��ͣ�Ĭ����4�Ρ�}
    property Delay: Integer read FDelay write FDelay default 0;
    {* �������� Ping ���ʱ��������λ���룬Ĭ�� 0 Ҳ���ǲ���ʱ}
    property TTL: Byte read FTTL write SetTTL;
    {* ���õ�TTLֵ��Time to Live}
    property TimeOut: DWord read FTimeOut write SetTimeOut;
    {* ���õĳ�ʱֵ}
    property DataString: string read GetDataString write SetDataString;
    {* �����͵����ݣ����ַ�����ʽ��ʾ��Ĭ��Ϊ"CnPack Ping"��}
    property OnReceived: TOnReceive read FOnReceived write FOnReceived;
    {* Pingһ�γɹ�ʱ�����������������¼�}
    property OnError: TOnError read FOnError write FOnError;
    {* Ping����ʱ���ص����ݺ���Ϣ������Ŀ��δ֪�����ɴ��ʱ�ȡ�}
  end;

implementation

{$R-}

const
  SCnPingData = 'CnPack Ping.';
  ICMPDLL = 'icmp.dll';

type

//==============================================================================
// ��������  ��icmp.dll����ĺ���
//==============================================================================

  TIcmpCreateFile = function (): THandle; stdcall;

  TIcmpCloseHandle = function (IcmpHandle: THandle): Boolean; stdcall;

  TIcmpSendEcho = function (IcmpHandle: THandle;
                            DestAddress: DWORD;
                            RequestData: Pointer;
                            RequestSize: Word;
                            RequestOptions: PCnIPOptionInformation;
                            ReplyBuffer: Pointer;
                            ReplySize: DWord;
                            TimeOut: DWord): DWord; stdcall;

var
  IcmpCreateFile: TIcmpCreateFile = nil;
  IcmpCloseHandle: TIcmpCloseHandle = nil;
  IcmpSendEcho: TIcmpSendEcho = nil;

  IcmpDllHandle: THandle = 0;

procedure InitIcmpFunctions;
begin
  IcmpDllHandle := LoadLibrary(ICMPDLL);
  if IcmpDllHandle <> 0 then
  begin
    @IcmpCreateFile := GetProcAddress(IcmpDllHandle, 'IcmpCreateFile');
    @IcmpCloseHandle := GetProcAddress(IcmpDllHandle, 'IcmpCloseHandle');
    @IcmpSendEcho := GetProcAddress(IcmpDllHandle, 'IcmpSendEcho');
  end;  
end;

procedure FreeIcmpFunctions;
begin
  if IcmpDllHandle <> 0 then
    FreeLibrary(IcmpDllHandle);
end;

//==============================================================================
// Ping ͨѶ��
//==============================================================================

{ TCnPing }

constructor TCnPing.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if IcmpDllHandle = 0 then
    InitIcmpFunctions;

  FRemoteIP := '127.0.0.1';
  FTTL := 64;
  FPingCount := 4;
  FDelay := 0;
  FTimeOut := 10;
  FDataString := SCnPingData;

  FHICMP := IcmpCreateFile(); // ȡ��DLL���
  if FHICMP = INVALID_HANDLE_VALUE then
    raise Exception.Create(SICMPRunError);
end;

destructor TCnPing.Destroy;
begin
  if FHICMP <> INVALID_HANDLE_VALUE then
    IcmpCloseHandle(FHICMP);
  inherited Destroy;
end;

procedure TCnPing.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnPingName;
  Author := SCnPack_Sesame;
  Email := SCnPack_SesameEmail;
  Comment := SCnPingComment;
end;

procedure TCnPing.SetPingCount(const Value: Integer);
begin
  if Value > 0 then
    FPingCount := Value;
end;

procedure TCnPing.SetRemoteIP(const Value: string);
begin
  if FRemoteIP <> Value then
  begin
    FRemoteIP := Value;
    if SetIP(FRemoteIP, '', FIP) then
    begin
      FRemoteHost := FIP.Host;
      FIPAddress := FIP.Address;
    end;
  end;
end;

procedure TCnPing.SetRemoteHost(const Value: string);
begin
  if FRemoteHost <> Value then
  begin
    // RemoteHost ���ĵĻ���RemoteIP �Զ����
    FRemoteHost := Value;
    if SetIP('', FRemoteHost, FIP) then
    begin
      FRemoteIP := FIP.IP;
      FIPAddress := FIP.Address;
    end;
  end;
end;

procedure TCnPing.SetTimeOut(const Value: DWord);
begin
  FTimeOut := Value;
end;

procedure TCnPing.SetTTL(const Value: Byte);
begin
  FTTL := Value;
end;

procedure TCnPing.SetDataString(const Value: string);
begin
  FDataString := Value;
end;

function TCnPing.GetDataString: string;
begin
  if FDataString = '' then
    FDataString := SCnPingData;
  Result := FDataString;
end;

function TCnPing.Ping(var aReply: string): Boolean;
var
  iCount, iResult: Integer;
  sReply: string;
begin
  aReply := '';
  iResult := 0;
  try
    SetIP(RemoteIP, RemoteHost, FIP);
    for iCount := 1 to PingCount do
    begin
      iResult := PingIP_Host(FIP, Pointer(FDataString)^, Length(DataString) * SizeOf(Char),
        sReply);
      aReply := aReply + #13#10 + sReply;
      if iResult < 0 then
        Break;

      if FDelay > 0 then
        Sleep(FDelay);
    end;
  finally
    Result := iResult >= 0;
  end;
end;

function TCnPing.PingOnce(var aReply: string): Boolean;
begin
  SetIP(RemoteIP, RemoteHost, FIP);
  Result := PingIP_Host(FIP, Pointer(FDataString)^, Length(DataString) * SizeOf(Char),
    aReply) >= 0;
end;

function TCnPing.PingOnce(const aIP: string; var aReply: string): Boolean;
begin
  SetIP(aIP, aIP, FIP);
  Result := PingIP_Host(FIP, Pointer(FDataString)^, Length(DataString) * SizeOf(Char),
    aReply) >= 0;
end;

function TCnPing.PingFromBuffer(var Buffer; Count: Integer;
  var aReply: string): Boolean;
begin
  SetIP(RemoteIP, RemoteHost, FIP);
  Result := PingIP_Host(FIP, Buffer, Count, aReply) >= 0;
end;

function TCnPing.PingIP_Host(const aIP: TIpInfo; const Data;
  Count: Cardinal; var aReply: string): Integer;
var
  IPOpt: TCnIPOptionInformation; // �������ݽṹ
  pReqData, pRevData: PAnsiChar;
  pCIER: PCnIcmpEchoReply;
begin
  Result := -100;
  pReqData := nil;
  
  if Count <= 0 then
  begin
    aReply := GetReplyString(Result, aIP, nil);
    Exit;
  end;
  if aIP.Address = INADDR_NONE then
  begin
    Result := -1;
    aReply := GetReplyString(Result, aIP, nil);
    Exit;
  end;

  GetMem(pCIER, SizeOf(TCnICMPEchoReply) + Count);
  GetMem(pRevData, Count);
  try
    FillChar(pCIER^, SizeOf(TCnICMPEchoReply) + Count, 0); // ��ʼ���������ݽṹ
    pCIER^.Data := pRevData;
    GetMem(pReqData, Count);
    Move(Data, pReqData^, Count); // ׼�����͵�����
    FillChar(IPOpt, Sizeof(IPOpt), 0); // ��ʼ���������ݽṹ
    IPOpt.TTL := FTTL;

    try //Ping��ʼ
      if WSAStartup(MAKEWORD(2, 0), FWSAData) <> 0 then
        raise Exception.Create(SInitFailed);
      if IcmpSendEcho(FHICMP, //dll handle
        aIP.Address, //target
        pReqData, //data
        Count, //data length
        @IPOpt, //addree of ping option
        pCIER,
        SizeOf(TCnICMPEchoReply) + Count, //pack size
        FTimeOut //timeout value
        ) <> 0 then
      begin
        Result := 0; // Ping��������
        if Assigned(FOnReceived) then
          FOnReceived(Self, aIP.IP, aIP.Host, IPOpt.TTL, IPOpt.TOS);
      end
      else
      begin
        Result := -2; // û����Ӧ
        if Assigned(FOnError) then
          FOnError(Self, aIP.IP, aIP.Host, IPOpt.TTL, IPOpt.TOS, SNoResponse);
      end;
    except
      on E: Exception do
      begin
        Result := -3; // ��������
        if Assigned(FOnError) then
          FOnError(Self, aIP.IP, aIP.Host, IPOpt.TTL, IPOpt.TOS, E.Message);
      end;
    end;
  finally
    WSACleanUP;
    aReply := GetReplyString(Result, aIP, pCIER);
    if pRevData <> nil then
    begin
      FreeMem(pRevData); // �ͷ��ڴ�
      pCIER.Data := nil;
    end;
    if pReqData <> nil then
      FreeMem(pReqData); //�ͷ��ڴ�
    FreeMem(pCIER); //�ͷ��ڴ�
  end;
end;

function TCnPing.GetReplyString(aResult: Integer; aIP: TIpInfo;
  pIPE: PCnIcmpEchoReply): string;
var
  sHost: string;
begin
  Result := SInvalidAddr;
  case aResult of
    -100: Result := SICMPRunError;
    -1: Result := SInvalidAddr;
    -2: Result := Format(SNoResponse, [RemoteHost]);
    else
      if pIPE <> nil then
      begin
        sHost := aIP.IP;
        if aIP.Host <> '' then
          sHost := aIP.Host + ': ' + sHost;
        Result := (Format(SPingResultString, [sHost, pIPE^.DataSize, pIPE^.RTT,
          pIPE^.Options.TTL]));
      end;
  end;
end;

function TCnPing.GetIPByName(const aName: string;
  var aIP: string): Boolean;
var
  pHost: PHostEnt;
  FWSAData: TWSAData;
  sName: array[0..255] of AnsiChar;
begin
  Result := False;
  StrPCopy(sName, {$IFDEF UNICODE}AnsiString{$ENDIF}(aName));
  aIP := '';
  if aName = '' then
    Exit;

  WSAStartup($101, FWSAData);
  try
    pHost := GetHostByName(@sName);
    Result := pHost <> nil;
    if Result then
      aIP := {$IFDEF UNICODE}String{$ENDIF}(inet_ntoa(PInAddr(pHost^.h_addr_list^)^));
  finally
    WSACleanup;
  end;
end;

function TCnPing.SetIP(aIPAddr, aHost: string; var aIP: TIpInfo): Boolean;
var
  pIPAddr: PAnsiChar;
begin
  Result := False;
  aIP.Address := INADDR_NONE;
  aIP.IP := aIPAddr;
  aIP.Host := aHost;
  if aIP.IP = '' then
  begin
    if (aIP.Host = '') or (not GetIPByName(aIP.Host, aIP.IP)) then
      Exit;
  end;

  GetMem(pIPAddr, Length(aIP.IP) + 1);
  try
    ZeroMemory(pIPAddr, Length(aIP.IP) + 1);
    StrPCopy(pIPAddr, {$IFDEF UNICODE}AnsiString{$ENDIF}(aIP.IP));
    aIP.Address := inet_addr(PAnsiChar(pIPAddr)); // IPת�����޵�����
  finally
    FreeMem(pIPAddr); // �ͷ�����Ķ�̬�ڴ�
  end;
  Result := aIP.Address <> INADDR_NONE;
end;

initialization

finalization
  FreeIcmpFunctions;

end.
