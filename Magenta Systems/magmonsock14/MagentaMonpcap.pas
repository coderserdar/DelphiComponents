unit MagentaMonpcap;

{ Magenta Systems Internet Packet Monitoring Components

Magenta Systems Monitor WinPCAP Component.
Updated by Angus Robertson, Magenta Systems Ltd, England, v1.4 26th November 2018
delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

This module requires the WinPcap device driver or Npcap NDIS driver to be
installed.  WinPCap is no longer developed, although it still installs and works
 on Windows 10 Win64. Npcap is a new project being actively developed,
 https://nmap.org/npcap/, which uses newer NDIS 6 light weight filter monitoring
 APIs for Windows 7 and later which is faster and less overhead than WinPCap,
 and is fully supported on Windows 10.  Npcap should be installed with the
 WinPCap compatible box ticked, this component does not yet support any advanced
 features of Npcap.

Npcap may be installed so that administrator program rights are required by
applications for improved security.

The Delphi conversion for packet.dll in MagentaPcap.pas and MagentaPacket32 is
by Lars Peter Christiansen, http://www.nzlab.dk, but modified by Magenta Systems
 from static linkage to dynamic DLL loading to allow the application to load
 without the DLL and to fix problems reading the adaptor list.

8 Aug 2008 - 1.2 - updated to support ICS V6 and V7, and Delphi 2009

26 Nov 2018 - 1.4 - no changes, testing with ICS V8, add Npcap support.

}

interface

uses
  Windows, Messages, Classes, SysUtils, Winsock,
  MagentaPackhdrs, MagentaPcap, MagentaPacket32, MagentaBpf,
  MagClasses ;

type

  TPcapThread = class ;  // forward declaration 

  TMonitorPcap = class(TComponent)
  protected
      FLastError: string ;
      FAddr: string ;
      FAddrMask: string ;
      FIgnoreIPList: TFindList ;
      FInAddr: TInAddr ;
      FInAddrMask: TInAddr ;
      FIgnoreData: boolean ;
      FIgnoreLAN: boolean ;
      FIgnoreNonIP: boolean ;
      FPromiscuous: boolean ;
      FTotRecvBytes: int64 ;
      FTotSendBytes: int64 ;
      FTotRecvPackets: integer ;
      FTotSendPackets: integer ;
      FDriverVersion: string ;
      FPacketVersion: string ;
      FOnPacketEvent: TPacketEvent ;
//      FPcapHandle: PPCAP ;        // control record handle
      FPcapThread: TPcapThread ;  // read packet thread
      FAdapterNameList: TStringList;  // ethernet adapters internal names
      FAdapterDescList: TStringList;  // ethernet adapters descriptions
      FMonAdapter: AnsiString ;       // adapter to monitor
      FConnected: boolean ;       // are we connected to PCap driver
      FAdapterMac: TMacAddr ;
      FLocalBiasUTC: TDateTime ;
      function GetAdapters: boolean ;
      procedure ThreadTerminate (Sender: TObject);
      procedure MonDataAvailable (const Header: Ppcap_pkthdr ; const PackPtr: PAnsiChar) ;
  public
      FPcapHandle: PPCAP ;        // control record handle
      constructor Create(AOwner: TComponent); override;
      destructor  Destroy; override;
      procedure StartMonitor;
      procedure StopMonitor;
      procedure SetIgnoreIP (IPAddr: string) ;
      procedure ClearIgnoreIP ;
      function GetIPAddresses (AdapterName: AnsiString ;  // 8 Aug 2010
                                IPList, MaskList, BcastList: TStringList): integer ;
  published
      property AdapterNameList: TStringList read FAdapterNameList ;
      property AdapterDescList: TStringList read FAdapterDescList ;
      property DriverVersion: string    read FDriverVersion ;
      property PacketVersion: string    read FPacketVersion ;
      property MonAdapter: AnsiString   read FMonAdapter
                                        write FMonAdapter ;
      property Connected: boolean       read FConnected ;
      property LastError: string        read FLastError ;
      property Addr: string             read FAddr
                                        write FAddr ;
      property AddrMask: string         read FAddrMask
                                        write FAddrMask ;
      property IgnoreData: boolean      read FIgnoreData
                                        write FIgnoreData ;
      property IgnoreLAN: boolean       read FIgnoreLAN
                                        write FIgnoreLAN ;
      property IgnoreNonIP: boolean     read FIgnoreNonIP
                                        write FIgnoreNonIP ;
      property Promiscuous: boolean     read FPromiscuous
                                        write FPromiscuous ;
      property TotRecvBytes: int64      read FTotRecvBytes ;
      property TotSendBytes: int64      read FTotSendBytes ;
      property TotRecvPackets: integer  read FTotRecvPackets ;
      property TotSendPackets: integer  read FTotSendPackets ;
      property OnPacketEvent: TPacketEvent read  FOnPacketEvent
                                           write FOnPacketEvent;
  end;

  TPcapThread = class(TThread)
  private
      FMonitorPcap: TMonitorPcap ;
      procedure GetPackets ;
  public
      procedure Execute; override;
  end;

implementation

procedure Register;
begin
    RegisterComponents('FPiette', [TMonitorPcap]) ;
end ;

function GetLocalBiasUTC: Integer;
var
    tzInfo : TTimeZoneInformation;
begin
    case GetTimeZoneInformation(tzInfo) of
    TIME_ZONE_ID_STANDARD: Result := tzInfo.Bias + tzInfo.StandardBias;
    TIME_ZONE_ID_DAYLIGHT: Result := tzInfo.Bias + tzInfo.DaylightBias;
    else
        Result := tzInfo.Bias;
    end;
end;

procedure CaptureCallBack (User: Pointer; const Header: Ppcap_pkthdr ; const PackPtr: PAnsiChar) ;
begin
    TPcapThread (User).FMonitorPcap.MonDataAvailable (Header, PackPtr) ;
end ;

procedure TPcapThread.GetPackets ;
begin
    Pcap_Read (FMonitorPcap.FPcapHandle, 0, CaptureCallBack, Pointer (Self)) ;
end ;

procedure TPcapThread.Execute;
begin
    if NOT Assigned (FMonitorPcap) then exit ;
    if FMonitorPcap.FPcapHandle = Nil then exit ;
    PacketSetReadTimeout (FMonitorPcap.FPcapHandle.Adapter, 100) ;
    while NOT Terminated do
    begin
        GetPackets ;
    end;
end ;

procedure TMonitorPcap.ThreadTerminate (Sender: tobject);
begin
    FConnected := false ;
    Pcap_Close (FPcapHandle) ;
    FPcapHandle := Nil ;
end;

constructor TMonitorPcap.Create(AOwner: TComponent);
begin
    FIgnoreData := false ;
    FIgnoreIPList := TFindList.Create ;
    FIgnoreIPList.Sorted := true ;
    FAdapterDescList := TStringList.Create ;
    FAdapterNameList := TStringList.Create ;
    FLastError := '' ;
    FPcapHandle := Nil ;
    FConnected := false ;
    FDriverVersion := Pcap_GetDriverVersion ;
    FPacketVersion := Pcap_GetPacketVersion ;
    GetAdapters ;
    if FAdapterNameList.Count <> 0 then
          FMonAdapter := AnsiString (FAdapterNameList [0]) ;  // 8 Aug 2010
    inherited Create(AOwner);
end ;

destructor TMonitorPcap.Destroy;
begin
    if FConnected then StopMonitor ;
    FreeAndNil (FIgnoreIPList) ;
    FreeAndNil (FAdapterNameList) ;
    FreeAndNil (FAdapterDescList) ;
    inherited Destroy;
end ;

function TMonitorPcap.GetAdapters: boolean ;
var
    total: integer ;
begin
    result := false;
    if NOT Assigned (FAdapterNameList) then exit ;
    FAdapterNameList.Clear ;
    FAdapterDescList.Clear ;
    total := Pcap_GetAdapterNamesEx (FAdapterNameList, FAdapterDescList, FLastError) ;
    if total = 0 then exit ;
    result := true;
end ;

function TMonitorPcap.GetIPAddresses (AdapterName: AnsiString ;    // 8 Aug 2010
                                    IPList, MaskList, BcastList: TStringList): integer ;
var
    IPArray, MaskArray, BcastArray: IPAddrArray ;
    I: integer ;
begin
    IPList.Clear ;
    MaskList.Clear ;
    BcastList.Clear ;
    result := Pcap_GetIPAddresses (AdapterName, IPArray, MaskArray, BcastArray, FLastError) ;
    if result = 0 then exit ;
    for I := 0 to Pred (result) do
    begin
        IPList.Add (IPToStr (IPArray [I])) ;
        MaskList.Add (IPToStr (MaskArray [I])) ;
        BcastList.Add (IPToStr (BcastArray [I])) ;
    end ;
end ;

// called by TFindList for sort and find comparison of file records

function CompareFNext (Item1, Item2: Pointer): Integer;
// Compare returns < 0 if Item1 is less than Item2, 0 if they are equal
// and > 0 if Item1 is greater than Item2.
begin
    result := 0 ;
    if longword (Item1) > longword (Item2) then result := 1 ;
    if longword (Item1) < longword (Item2) then result := -1 ;
end ;

procedure TMonitorPcap.SetIgnoreIP (IPAddr: string) ;
var
    InIPAddr: TInAddr ;
begin
    if NOT Str2IP (IPAddr, InIPAddr) then exit ;
    FIgnoreIPList.AddSorted (Pointer (InIPAddr.S_addr), @CompareFNext) ; 
end ;

procedure TMonitorPcap.ClearIgnoreIP ;
begin
    FIgnoreIPList.Clear ;
end ;

// convert seconds since 1 Jan 1970 (UNIX time stamp) to proper Delphi stuff
// and micro seconds 

function UnixStamptoDT (stamp: TunixTimeVal): TDateTime ;
begin
    result := ((stamp.tv_Sec / SecsPerDay) + 25569) +
                                    ((stamp.tv_uSec / 1000000) / SecsPerDay) ;
end ;

procedure TMonitorPcap.MonDataAvailable (const Header: Ppcap_pkthdr ; const PackPtr: PAnsiChar) ;
var
    hdrlen, iploc: integer ;
    ethernethdr: PHdrEthernet ;
    iphdr: PHdrIP;
    tcphdr: PHdrTCP;
    udphdr: PHdrUDP;
    PacketInfo: TPacketInfo ;  // the data we return in the event

    procedure GetDataByOffset (offset: integer) ;
    var
        datastart: PAnsiChar ;
    begin
        datastart := PAnsiChar (PAnsiChar (iphdr) + offset) ;
        with PacketInfo do
        begin
            if ntohs (iphdr.tot_len) < (Header.Len - OFFSET_IP) then
                DataLen := ntohs (iphdr.tot_len) - offset
            else
                DataLen := Header.Len - OFFSET_IP - offset;
            if DataLen = 0 then exit ;
            if FIgnoreData then exit ;
            SetLength (DataBuf, DataLen) ;  // ANSI
            Move (datastart^, DataBuf [1], DataLen) ;
        end ;
    end;

begin
    FillChar (PacketInfo, Sizeof(PacketInfo), 0) ;
    with PacketInfo do
    begin
        PacketLen := Header.Len ;
        if PacketLen <= 0 then exit ;
        ethernethdr := PHdrEthernet (PackPtr) ;
        EtherProto := ntohs (ethernethdr.protocol) ;
        EtherSrc := ethernethdr.smac ;
        EtherDest := ethernethdr.dmac ;
        SendFlag := CompareMem (@EtherSrc, @FAdapterMac, SizeOf (TMacAddr)) ;
        PacketDT := UnixStamptoDT (Header.ts) + FLocalBiasUTC ; // Unix time stamp correct to local time

     // internet layer IP, lots to check
        if EtherProto = PROTO_IP then
        begin
            iphdr := PHdrIP (PAnsiChar (PackPtr) + OFFSET_IP) ;  // IP header is past ethernet header
            AddrSrc := iphdr.saddr ;        // 32-bit IP addresses
            AddrDest := iphdr.daddr ;
     //     SendFlag := (FInAddr.S_addr = AddrSrc.S_addr) ;  // did we sent this packet
            ProtoType := iphdr.protocol ;   // TCP, UDP, ICMP

          // check if either IP is in the ignore list
            if FIgnoreIPList.Count > 0 then
            begin
                iploc := -1 ;
                if FIgnoreIPList.Find (Pointer (AddrSrc.S_addr), @CompareFNext, iploc) then exit ;
                if FIgnoreIPList.Find (Pointer (AddrDest.S_addr), @CompareFNext, iploc) then exit ;
            end ;

         // check if both IP on the same subnet as the LAN mask, if so ignore
            if (FInAddrMask.S_addr <> 0) and FIgnoreLAN then
            begin
                if (AddrSrc.S_addr AND FInAddrMask.S_addr) =
                                (AddrDest.S_addr AND FInAddrMask.S_addr) then exit ;
                if AddrDest.S_addr = 0 then exit ;
            end ;

         // increment global traffic counters
            if SendFlag then
            begin
                inc (FTotSendBytes, packetlen) ;
                inc (FTotSendPackets) ;
            end
            else
            begin
                inc (FTotRecvBytes, packetlen) ;
                inc (FTotRecvPackets) ;
            end ;

        // check protocol and find ports and data
            if Assigned (FOnPacketEvent) then
            begin
                DataBuf := '' ;
                hdrlen := GetIHlen (iphdr^) ;
                if ProtoType = IPPROTO_ICMP then
                begin
                    IcmpType := PByte (PAnsiChar (iphdr) + hdrlen)^ ;
                    GetDataByOffset (hdrlen) ;
                end
                else
                begin
                    if ProtoType = IPPROTO_TCP then
                    begin
                        tcphdr := PHdrTCP (PAnsiChar (iphdr) + hdrlen) ;
                        PortSrc := ntohs (tcphdr.source) ;
                        PortDest := ntohs (tcphdr.dest) ;
                        TcpFlags := ntohs (tcphdr.flags) ;
                        GetDataByOffset (hdrlen + GetTHdoff (tcphdr^)) ;
                    end;
                    if ProtoType = IPPROTO_UDP then
                    begin
                        udphdr := PHdrUDP (PAnsiChar (iphdr) + hdrlen) ;
                        PortSrc := ntohs (udphdr.src_port) ;
                        PortDest := ntohs (udphdr.dst_port) ;
                        GetDataByOffset (hdrlen + Sizeof (THdrUDP));
                    end;
                end;
                FOnPacketEvent (Self, PacketInfo) ;
            end ;
        end
        else

     // otherwise ARP or something more obscure
        begin
         //   if FIgnoreLAN then exit ;
            if FIgnoreNonIP then exit ;
            if SendFlag then
            begin
                inc (FTotSendBytes, packetlen) ;
                inc (FTotSendPackets) ;
            end
            else
            begin
                inc (FTotRecvBytes, packetlen) ;
                inc (FTotRecvPackets) ;
            end ;
            if Assigned (FOnPacketEvent) then
            begin
                DataLen := PacketLen - OFFSET_IP ;
                if DataLen <= 0 then exit ;
                SetLength (DataBuf, DataLen) ;
                Move (PAnsiChar (PAnsiChar (PackPtr) + OFFSET_IP)^, DataBuf [1], DataLen) ;
                FOnPacketEvent (Self, PacketInfo) ;
            end ;
        end ;

    end ;
end ;

procedure TMonitorPcap.StartMonitor;
var
    snaplen, mins: integer ;
begin
    if (FAdapterNameList.Count = 0) or (FMonAdapter = '') then
    begin
        FLastError := 'No Adaptors Found to Monitor' ;
        exit;
    end;
    if FConnected or (FPcapHandle <> nil) then
    begin
        FLastError := 'PCap Driver Already Running' ;
        exit;
    end;
    FAddr := Trim (FAddr) ;
    FInAddr := StrToIP (FAddr) ;  // keep 32-bit listen IP address
    FAddrMask := Trim (FAddrMask) ;
    if Length(FAddrMask) = 0 then
        FInAddrMask.S_addr := 0
    else
        FInAddrMask := StrToIP (FAddrMask) ; // and IP mask
    FTotRecvBytes := 0 ;
    FTotSendBytes := 0 ;
    FTotRecvPackets := 0 ;
    FTotSendPackets := 0 ;
    mins := GetLocalBiasUTC ;
    if mins < 0 then          // reverse minutes, -60 for GMT summer time 
        mins := Abs (mins)
    else
        mins := 0 - mins ;
    FLocalBiasUTC := mins  / (60.0 * 24.0) ;  // keep local time bias

  // open winpcap driver for specific adaptor
    FConnected := false ;
    if FIgnoreData then
        snaplen := DEFAULT_SNAPLEN
    else
        snaplen := 2000 ;
    FPcapHandle := pcap_open_live (PAnsiChar (FMonAdapter), snaplen,
                                                        FPromiscuous, 100, FLastError) ;
    if FPcapHandle = nil then exit;
//    Pcap_SetMinToCopy (FPcapHandle, 20000) ;  not sure if this is beneficial
    FAdapterMac := Pcap_GetMacAddress (FPcapHandle, FLastError) ;

  // Start Snoop Read Thread
    FPcapThread := TPcapThread.Create (true) ;
    FPcapThread.FMonitorPcap := Self ;
    FPcapThread.OnTerminate := ThreadTerminate ;
    FPcapThread.FreeOnTerminate := false;
    FPcapThread.Resume ;  // 9 Aug 2010 should use Start for Delphi XE
    FConnected := true;
end ;

procedure  TMonitorPcap.StopMonitor;
begin
    FConnected := false ;

  // stop thread
    if Assigned (FPcapThread) then
    begin
        FPcapThread.Terminate ;
        FPcapThread.WaitFor ;
        FPcapThread.Free ;
        FPcapThread := nil ;
    end ;
    if Assigned (FPcapHandle) then
    begin
        Pcap_Close (FPcapHandle) ;
        FPcapHandle := Nil ;
    end ;
end ;


end.
