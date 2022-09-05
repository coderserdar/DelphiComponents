unit UnitNetDecl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, WinSock;

type
  TFormNetDecl = class(TForm)
    pgcNetDecl: TPageControl;
    tsIP: TTabSheet;
    cbbIP: TComboBox;
    lblLocal: TLabel;
    btnSniff: TButton;
    rbTCP: TRadioButton;
    rbUDP: TRadioButton;
    rbICMP: TRadioButton;
    mmoIPSniffer: TMemo;
    lblIPCount: TLabel;
    rbAll: TRadioButton;
    btnIPManual: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnSniffClick(Sender: TObject);
    procedure btnIPManualClick(Sender: TObject);
  private
    { Private declarations }
    FRecving: Boolean;
    FRecCount: Integer;
    FSnifSock: TSocket;
    procedure UpdateButtonState;
    procedure StartSniff;
    procedure StopSniff;
    procedure ParsingPacket(Buf: Pointer; DataLen: Integer);
  public
    { Public declarations }
  end;

  TIpInfo = record
    Address: Int64;
    IP: string;
    Host: string;
  end;

  TIP_NetType = (iptNone, iptANet, iptBNet, iptCNet, iptDNet, iptENet,
    iptBroadCast, iptKeepAddr);

  TIPNotes = array[1..4] of Byte;

  TIP_Info = packed record
    IPAddress: Cardinal;                 // IP��ַ,�˴������δ洢
    SubnetMask: Cardinal;                // ��������,�˴������δ洢
    Broadcast: Cardinal;                 // �㲥��ַ,�˴������δ洢
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

  TWSAIoctl = function(s: TSocket; cmd: DWORD; lpInBuffer: PByte; dwInBufferLen:
    DWORD; lpOutBuffer: PByte; dwOutBufferLen: DWORD; lpdwOutBytesReturned:
    LPDWORD; lpOverLapped: POINTER; lpOverLappedRoutine: POINTER): Integer; stdcall;

var
  FormNetDecl: TFormNetDecl;

implementation

uses
  CnNetDecls;

{$R *.DFM}

const
  WS2_32DLL = 'WS2_32.DLL';
  MAXIPNOTE = 255;
  IPJOIN = '.';
  IPADDRFORMAT = '%0:D.%1:D.%2:D.%3:D';
  SIO_GET_INTERFACE_LIST = $4004747F;
  IOC_VENDOR = $18000000;
  SIO_RCVALL = IOC_IN or IOC_VENDOR or 1;
  IFF_UP = $00000001;
  IFF_BROADCAST = $00000002;
  IFF_LOOPBACK = $00000004;
  IFF_POINTTOPOINT = $00000008;
  IFF_MULTICAST = $00000010;
  IPNOTE1 = $FF000000;
  IPNOTE2 = $00FF0000;
  IPNOTE3 = $0000FF00;
  IPNOTE4 = $000000FF;

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

function SetIP(aIPAddr: string; var aIP: TIpInfo): Boolean;
var
  pIPAddr: PAnsiChar;
begin
  Result := False;
  aIP.Address := INADDR_NONE;
  aIP.IP := aIPAddr;
  aIP.Host := '';
  if aIP.IP = '' then
    Exit;

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

function GetIPNotes(const aIP: string; var aResult: TIPNotes): Boolean;
var
  iPos, iNote: Integer;
  sIP: string;

  function CheckIpNote(aNote: string): Byte;
  begin
    iNote := StrToInt(aNote);
    if (iNote < 0) or (iNote > MAXIPNOTE) then
      raise Exception.Create(aNote + ' Error Range.');
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

function IPTypeCheck(const aIP: string): TIP_NetType;
var
  FNotes: TIPNotes;
begin
  Result := iptNone;
  if GetIPNotes(aIP, FNotes) then
  begin
    case FNotes[1] of
      1..126:
        Result := iptANet;
      127:
        Result := iptKeepAddr;
      128..191:
        Result := iptBNet;
      192..223:
        Result := iptCNet;
      224..239:
        Result := iptDNet;
      240..255:
        Result := iptENet;
    else
      Result := iptNone;
    end;
  end;
end;

function IPToInt(const aIP: string): Cardinal;
var
  FNotes: TIPNotes;
begin
  Result := 0;
  if IPTypeCheck(aIP) = iptNone then
  begin
    //raise Exception.Create(SCnErrorAddress);
    Exit;
  end;
  if GetIPNotes(aIP, FNotes) then
  begin
    Result := Result or FNotes[1] shl 24 or FNotes[2] shl 16 or FNotes[3] shl 8
      or FNotes[4];
  end;
end;

function IntToIP(const aIP: Cardinal): string;
var
  FNotes: TIPNotes;
begin
  FNotes[1] := aIP and IPNOTE1 shr 24;
  FNotes[2] := aIP and IPNOTE2 shr 16;
  FNotes[3] := aIP and IPNOTE3 shr 8;
  FNotes[4] := aIP and IPNOTE4;
  Result := Format(IPADDRFORMAT, [FNotes[1], FNotes[2], FNotes[3], FNotes[4]]);
end;

function EnumLocalIP(var aLocalIP: TIPGroup): Integer;
var
  skLocal: TSocket;
  iIP: Integer;
  PtrA: pointer;
  BytesReturned, SetFlags: u_long;
  pAddrInet: Sockaddr_IN;
  Buffer: array[0..20] of TINTERFACE_INFO;
  FWSAData: TWSAData;
begin
  Result := 0;

  WSAStartup($101, FWSAData);
  try
    skLocal := socket(AF_INET, SOCK_STREAM, 0); // Open a socket
    if (skLocal = INVALID_SOCKET) then
      Exit;

    try // Call WSAIoCtl
      PtrA := @BytesReturned;
      if (WSAIoctl(skLocal, SIO_GET_INTERFACE_LIST, nil, 0, @Buffer, 1024, PtrA,
        nil, nil) <> SOCKET_ERROR) then
      begin // If ok, find out how
        Result := BytesReturned div SizeOf(TINTERFACE_INFO);
        SetLength(aLocalIP, Result);
        for iIP := 0 to Result - 1 do // For every interface
        begin
          pAddrInet := Buffer[iIP].iiAddress.AddressIn;
          aLocalIP[iIP].IPAddress := IPToInt({$IFDEF UNICODE}string{$ENDIF}(inet_ntoa
            (pAddrInet.sin_addr)));
          pAddrInet := Buffer[iIP].iiNetMask.AddressIn;
          aLocalIP[iIP].SubnetMask := IPToInt({$IFDEF UNICODE}string{$ENDIF}(inet_ntoa
            (pAddrInet.sin_addr)));
          pAddrInet := Buffer[iIP].iiBroadCastAddress.AddressIn;
          aLocalIP[iIP].Broadcast := IPToInt({$IFDEF UNICODE}string{$ENDIF}(inet_ntoa
            (pAddrInet.sin_addr)));
          SetFlags := Buffer[iIP].iiFlags;
          aLocalIP[iIP].UpState := (SetFlags and IFF_UP) = IFF_UP;
          aLocalIP[iIP].Loopback := (SetFlags and IFF_LOOPBACK) = IFF_LOOPBACK;
          aLocalIP[iIP].SupportBroadcast := (SetFlags and IFF_BROADCAST) = IFF_BROADCAST;
        end;
      end;
    except
      ;
    end;
    closesocket(skLocal);
  finally
    WSACleanup;
  end;
end;

procedure TFormNetDecl.FormCreate(Sender: TObject);
var
  Wsa: TWSAData;
  aLocalIP: TIPGroup;
  I: Integer;
begin
  if (WSAStartup(MakeWord(2, 2), Wsa) <> 0) then
    raise Exception.Create('WSAStartup');
  InitWSAIoctl;

  EnumLocalIP(aLocalIP);
  if Length(aLocalIP) > 0 then
  begin
    for I := 0 to Length(aLocalIP) - 1 do
      cbbIP.Items.Add(IntToIP(aLocalIP[I].IPAddress));
    cbbIP.ItemIndex := 0;
  end;
end;

procedure TFormNetDecl.btnSniffClick(Sender: TObject);
begin
  if FRecving then
  begin
    // Stop
    StopSniff;
    UpdateButtonState;
  end
  else
  begin
    // Start
    StartSniff;
    UpdateButtonState;
  end;
end;

procedure TFormNetDecl.UpdateButtonState;
begin
  if FRecving then
  begin
    btnSniff.Caption := 'Stop';
  end
  else
  begin
    btnSniff.Caption := 'Sniff';
  end;
end;

procedure TFormNetDecl.StartSniff;
var
  RecvTimeout: Integer;
  Sa: TSockAddr;
  Ret: Integer;
  Buf: array[0..65535] of AnsiChar;
  BytesRet, InBufLen: Cardinal;
  CtlBuf: array[0..1023] of AnsiChar;
  DataLen: Integer;
  ListeningIP: u_long;
begin
  if FRecving then
    Exit;

  FRecCount := 0;
  mmoIPSniffer.Clear;

  FSnifSock := socket(AF_INET, SOCK_RAW, IPPROTO_IP);
  if FSnifSock = INVALID_SOCKET then
    raise Exception.Create('Create Socket Fail.');

  RecvTimeout := 10;
  if (setsockopt(FSnifSock, SOL_SOCKET, SO_RCVTIMEO, @RecvTimeout, SizeOf(RecvTimeout))
    = SOCKET_ERROR) then
  begin
    Ret := WSAGetLastError;
    closesocket(FSnifSock);
    raise Exception.CreateFmt('Set Socketopt Fail. %d', [Ret]);
  end;

  ListeningIP := inet_addr(PAnsiChar(cbbIP.Text));

  Sa.sin_family := AF_INET;
  Sa.sin_port := htons(0);
  Sa.sin_addr.s_addr := ListeningIP;
  if bind(FSnifSock, Sa, SizeOf(Sa)) = SOCKET_ERROR then
  begin
    Ret := WSAGetLastError;
    closesocket(FSnifSock);
    raise Exception.CreateFmt('Bind IP Fail. %d', [Ret]);
  end;

  InBufLen := 1;
  if WSAIoctl(FSnifSock, SIO_RCVALL, @InBufLen, SizeOf(InBufLen), @CtlBuf[0],
    SizeOf(CtlBuf), @BytesRet, nil, nil) = SOCKET_ERROR then
  begin
    Ret := WSAGetLastError;
    closesocket(FSnifSock);
    raise Exception.CreateFmt('WSAIoctl Fail. %d', [Ret]);
  end;

  FillChar(Buf[0], SizeOf(Buf), 0);
  FRecving := True;
  UpdateButtonState;

  Application.ProcessMessages;
  try
    while FRecving do
    begin
      DataLen := recv(FSnifSock, Buf[0], SizeOf(Buf), 0);
      if (DataLen <> SOCKET_ERROR) and (DataLen > 0) then
      begin
        Inc(FRecCount);
        lblIPCount.Caption := IntToStr(FRecCount);

        // ���� IP ��
        ParsingPacket(@Buf[0], DataLen);
      end
      else if DataLen = SOCKET_ERROR then
      begin
        Ret := WSAGetLastError;
        if Ret <> 10060 then // 10060 �ǳ�ʱ�����ݣ���������
          raise Exception.CreateFmt('Recv Fail. %d', [Ret]);
      end;

      Application.ProcessMessages;
    end;
  finally
    closesocket(FSnifSock);
    UpdateButtonState;
  end;
end;

procedure TFormNetDecl.StopSniff;
begin
  if not FRecving then
    Exit;
  FRecving := False;
end;

procedure TFormNetDecl.ParsingPacket(Buf: Pointer; DataLen: Integer);
var
  PIP: PCnIPHeader;
  PTCP: PCnTCPHeader;
  PUDP: PCnUDPHeader;
  PICMP: PCnICMPHeader;
begin
  PIP := Buf;
  if not rbAll.Checked then
  begin
    if rbTCP.Checked and (PIP^.Protocol <> CN_IP_PROTOCOL_TCP) then
      Exit;
    if rbUDP.Checked and (PIP^.Protocol <> CN_IP_PROTOCOL_UDP) then
      Exit;
    if rbICMP.Checked and (PIP^.Protocol <> CN_IP_PROTOCOL_ICMP) then
      Exit;
  end;

  mmoIPSniffer.Lines.Add('=== Got an IP Packet. Length: ' + IntToStr(DataLen));
  mmoIPSniffer.Lines.Add('IP Version: ' + IntToStr(CnGetIPVersion(PIP)));
  mmoIPSniffer.Lines.Add('IP Header Length(Bytes): ' + IntToStr(SizeOf(DWORD) *
    CnGetIPHeaderLength(PIP)));
  mmoIPSniffer.Lines.Add('IP Type Of Service Precedence: ' + IntToStr(CnGetIPTypeOfServicePrecedence
    (PIP)));
  mmoIPSniffer.Lines.Add('IP Type Of Service Delay: ' + IntToStr(Integer(CnGetIPTypeOfServiceDelay
    (PIP))));
  mmoIPSniffer.Lines.Add('IP Type Of Service Throughput: ' + IntToStr(Integer(CnGetIPTypeOfServiceThroughput
    (PIP))));
  mmoIPSniffer.Lines.Add('IP Type Of Service Relibility: ' + IntToStr(Integer(CnGetIPTypeOfServiceRelibility
    (PIP))));
  mmoIPSniffer.Lines.Add('IP TotalLength(Bytes): ' + IntToStr(CnGetIPTotalLength(PIP)));
  mmoIPSniffer.Lines.Add(Format('IP Identification: $%4.4x', [CnGetIPIdentification
    (PIP)]));
  mmoIPSniffer.Lines.Add('IP Fragment Offset: ' + IntToStr(CnGetIPFragmentOffset(PIP)));
  mmoIPSniffer.Lines.Add('IP Fragment Dont Flag: ' + IntToStr(Integer(CnGetIPFlagDontFragment
    (PIP))));
  mmoIPSniffer.Lines.Add('IP Fragment More Flag: ' + IntToStr(Integer(CnGetIPFlagMoreFragment
    (PIP))));
  mmoIPSniffer.Lines.Add('IP TTL: ' + IntToStr(PIP^.TTL));
  mmoIPSniffer.Lines.Add('IP Protocol: ' + IntToStr(PIP^.Protocol));
  mmoIPSniffer.Lines.Add('IP Checksum: ' + IntToStr(CnGetIPCheckSum(PIP)));
  mmoIPSniffer.Lines.Add('IP Source: ' + IntToIP(CnGetIPSourceIP(PIP)));
  mmoIPSniffer.Lines.Add('IP Destination: ' + IntToIP(CnGetIPDestIP(PIP)));

  if PIP^.Protocol = CN_IP_PROTOCOL_TCP then
  begin
    PTCP := PCnTCPHeader(Integer(PIP) + SizeOf(DWORD) * CnGetIPHeaderLength(PIP));
    mmoIPSniffer.Lines.Add('  TCP Source Port: ' + IntToStr(CnGetTCPSourcePort(PTCP)));
    mmoIPSniffer.Lines.Add('  TCP Destination Port: ' + IntToStr(CnGetTCPDestPort(PTCP)));
    mmoIPSniffer.Lines.Add('  TCP Seq Num: ' + IntToStr(CnGetTCPSequenceNumber(PTCP)));
    mmoIPSniffer.Lines.Add('  TCP Ack Num: ' + IntToStr(CnGetTCPAcknowledgementNumber
      (PTCP)));
    mmoIPSniffer.Lines.Add('  TCP Offset(Bytes): ' + IntToStr(SizeOf(DWORD) *
      CnGetTCPOffset(PTCP)));
    mmoIPSniffer.Lines.Add('  TCP Flag URG: ' + IntToStr(Integer(CnGetTCPFlagURG(PTCP))));
    mmoIPSniffer.Lines.Add('  TCP Flag ACK: ' + IntToStr(Integer(CnGetTCPFlagACK(PTCP))));
    mmoIPSniffer.Lines.Add('  TCP Flag PSH: ' + IntToStr(Integer(CnGetTCPFlagPSH(PTCP))));
    mmoIPSniffer.Lines.Add('  TCP Flag RST: ' + IntToStr(Integer(CnGetTCPFlagRST(PTCP))));
    mmoIPSniffer.Lines.Add('  TCP Flag SYN: ' + IntToStr(Integer(CnGetTCPFlagSYN(PTCP))));
    mmoIPSniffer.Lines.Add('  TCP Flag FIN: ' + IntToStr(Integer(CnGetTCPFlagFIN(PTCP))));
    mmoIPSniffer.Lines.Add('  TCP Window: ' + IntToStr(Integer(CnGetTCPWindow(PTCP))));
    mmoIPSniffer.Lines.Add('  TCP CheckSum: ' + IntToStr(Integer(CnGetTCPCheckSum
      (PTCP))));
    mmoIPSniffer.Lines.Add('  TCP UrgentPointer: ' + IntToStr(Integer(CnGetTCPUrgentPointer
      (PTCP))));
  end
  else if PIP^.Protocol = CN_IP_PROTOCOL_UDP then
  begin
    PUDP := PCnUDPHeader(Integer(PIP) + SizeOf(DWORD) * CnGetIPHeaderLength(PIP));
    mmoIPSniffer.Lines.Add('  UDP Source Port: ' + IntToStr(CnGetUDPSourcePort(PUDP)));
    mmoIPSniffer.Lines.Add('  UDP Destination Port: ' + IntToStr(CnGetUDPDestPort(PUDP)));
    mmoIPSniffer.Lines.Add('  UDP Length: ' + IntToStr(CnGetUDPLength(PUDP)));
    mmoIPSniffer.Lines.Add('  UDP CheckSum: ' + IntToStr(Integer(CnGetUDPCheckSum(PUDP))));
  end
  else if PIP^.Protocol = CN_IP_PROTOCOL_ICMP then
  begin
    PICMP := PCnICMPHeader(Integer(PIP) + SizeOf(DWORD) * CnGetIPHeaderLength(PIP));
    mmoIPSniffer.Lines.Add('  ICMP Type: ' + IntToStr(CnGetICMPType(PICMP)));
    mmoIPSniffer.Lines.Add('  ICMP Code: ' + IntToStr(CnGetICMPCode(PICMP)));
  end;
end;

procedure TFormNetDecl.btnIPManualClick(Sender: TObject);
const
  IP: array[0..47] of Byte = ($45, $00, $00, $30, $00, $00, $40, $00, $35,
    $06, $24, $98, $DC, $B5, $7C, $32, $AC, $14, $1C, $34, $00, $50, $C3, $F9,
    $5E, $56, $55, $60, $DD, $24, $1A, $2B, $70, $12, $39, $08, $B9, $8E, $00,
    $00, $02, $04, $05, $AC, $01, $01, $04, $02);
begin
  mmoIPSniffer.Clear;
  ParsingPacket(@IP[0], SizeOf(IP));
end;

end.

