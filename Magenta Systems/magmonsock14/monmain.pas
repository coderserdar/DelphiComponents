unit monmain;

{ Magenta Systems Internet Packet Monitoring Components

Demo Application - Display Packets using Raw Sockets and WinPcap
6thUpdated by Angus Robertson, Magenta Systems Ltd, England, v1.4 26th November 2018
delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

TMonitorSocket needs WSocket from François PIETTE internet component suite
V8 from  http://www.overbyte.eu/
TMonitorPcap needs WinPcap or Npcap, but this application will run without
it being installed

This application displays ethernet packets captured using two different techniques:

1 - Raw sockets (W2K and later) using ICS, does not any other software installed,
but may not capture send packets on W2K and XP, only W2K3, and ignores non-IP

2 - WinPcap device driver or Npcap NDIS driver needs to be installed.  WinPCap
is no longer developed, although it still installs and works on Windows 10 Win64.
Npcap is a new project being actively developed, https://nmap.org/npcap/, which
uses newer NDIS 6 light weight filter monitoring APIs for Windows 7 and later
which is faster and less overhead than WinPCap, and is fully supported on Windows
10.  Npcap should be installed with the WinPCap compatible box ticked, this
component does not yet support any advanced features of Npcap.

Npcap may be installed so that administrator program rights are required by
applications for improved security.

This application is only designed to demonstrate the two components, it's not
intended as a full network analyser.

Baseline v1.1 29th October 2005

8 Aug 2008  - 1.2  - updated to support ICS V6 and V7, and Delphi 2009
                     when stopping capture ignore any buffered data so it stops faster
9 Aug 2010  -  1.3 - fixed various cast warnings with Delphi 2009 and later
                     added Clear button
26 Nov 2018 - 1.4 -  comsmetics only for Npcap support, tested with ICS V8.
                     added program Admin rights check and force Wincap if missing
                     since socket monitoring won't work
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Winsock,
  OverbyteIcsWsocket,
  MagentaMonsock, MagentaMonpcap, MagentaPacket32, MagentaPcap,
  MagentaPackhdrs, Magsubs1, MagClasses ;

const
  sPacketLine = '%-12s %-4s %4d  %-20s > %-20s %-12s %4d %s' ;
//               01:02:03:004 UDP   109  192.168.1.201:161    > 192.168.1.109:1040   snmp           81 [0O    ]
  sHeaderLine = 'Time         Prot Plen  Source IP:Port         Dest IP:Port         Service      Dlen Packet Data' ;


type
  TMonForm = class(TForm)
    LogWin: TMemo;
    Panel1: TPanel;
    MonIpList: TListBox;
    doMonitor: TButton;
    doExit: TButton;
    Timer: TTimer;
    LabelTraffic: TLabel;
    IgnoreLAN: TCheckBox;
    IgnoreData: TCheckBox;
    IpMask: TEdit;
    FullData: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    IgnoreIPs: TMemo;
    Label3: TLabel;
    Label4: TLabel;
    UseWinPCap: TCheckBox;
    AdapterList: TListBox;
    IgnoreNonIp: TCheckBox;
    Promiscuous: TCheckBox;
    doClear: TButton;
    LabelAdmin: TLabel;
    procedure doExitClick(Sender: TObject);
    procedure doMonitorClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure AdapterListClick(Sender: TObject);
    procedure UseWinPCapClick(Sender: TObject);
    procedure doClearClick(Sender: TObject);
  private
    { Private declarations }
    procedure PacketEvent (Sender: TObject; PacketInfo: TPacketInfo) ;
  public
    { Public declarations }
  end;

var
  MonForm: TMonForm;
  MonitorSocket: TMonitorSocket ;
  MonitorPcap: TMonitorPcap ;
  MonLive: boolean ;
  AdapterIPList: TStringList ;
  AdapterMaskList: TStringList ;
  AdapterBcastList: TStringList ;

implementation

{$R *.dfm}

procedure TMonForm.PacketEvent (Sender: TObject; PacketInfo: TPacketInfo) ;
var
    srcip, destip, S, S2: string ;
begin
    if NOT MonLive then exit ;   // 8 Aug 2008 ignore data once Stopped
    with PacketInfo do
    begin
        if (NOT FullData.Checked) and (DataLen > 96) then SetLength (DataBuf, 96) ;
        S2 := '[' + String (DataBuf) + ']' ;
        StringRemCntls (S2) ;
        if EtherProto = PROTO_IP then
        begin
            srcip := IPToStr (AddrSrc) + ':' + IntToStr (PortSrc);   // convert 32-bit IP address into dotted ASCII
            destip := IPToStr (AddrDest) + ':' + IntToStr (PortDest) ;
            if ProtoType = IPPROTO_ICMP then
                S := Format (sPacketLine, [TimeToZStr (PacketDT),
                    GetIPProtoName (ProtoType), PacketLen,
                        srcip , destip, Lowercase (GetICMPType (IcmpType)), DataLen, S2])
            else
            begin
                if DataLen = 0 then S2 := GetFlags (TcpFlags) ;
                S := Format (sPacketLine, [TimeToZStr (PacketDT),
                    GetIPProtoName (ProtoType), PacketLen, srcip, destip,
                        Lowercase (GetServiceNameEx (PortSrc, PortDest)), DataLen, S2]) ;
            end ;
        end
        else
        begin
            S := Format (sPacketLine, [TimeToZStr (PacketDT),
                GetEtherProtoName (EtherProto), PacketLen,
                           MacToStr (EtherSrc), MacToStr (EtherDest), '', DataLen, S2]) ;
        end ;
        LogWin.Lines.Add (S) ;
    end ;
end ;

procedure TMonForm.doClearClick(Sender: TObject);
begin
  LogWin.Lines.Clear ;  // 8 Aug 2010
end;

procedure TMonForm.doExitClick(Sender: TObject);
begin
    if MonLive then doMonitorClick (self) ;
    Close ;
end;

procedure TMonForm.doMonitorClick(Sender: TObject);
var
    I: integer ;
begin
    if MonIpList.ItemIndex < 0 then exit ;
    if MonLive then
    begin
        MonLive := false ;
        LogWin.Lines.Add ('Capture Stopped' + CRLF) ;
        if UseWinPCap.Checked then
            MonitorPcap.StopMonitor
        else
            MonitorSocket.StopMonitor ;
        doMonitor.Caption := 'Start Monitor' ;
    end
    else
    begin
        try
            if UseWinPCap.Checked then
            begin
                MonitorPcap.MonAdapter := AnsiString (MonitorPcap.AdapterNameList [AdapterList.ItemIndex]) ;  // 8 Aug 2010
                I := MonitorPcap.GetIPAddresses (MonitorPcap.MonAdapter, AdapterIPList,
                                                    AdapterMaskList, AdapterBcastList) ;
//                LogWin.Lines.Add (AdapterIPList.CommaText) ;  // temp
//                LogWin.Lines.Add (AdapterMaskList.CommaText) ;  // temp
//                LogWin.Lines.Add (AdapterBcastList.CommaText) ;  // temp
                if I > 0 then
                begin
                    MonitorPcap.Addr := AdapterIPList [0] ;
                    MonitorPcap.AddrMask := AdapterMaskList [0] ;
                end
                else
                begin
                    MonitorPcap.Addr := MonIpList.Items [MonIpList.ItemIndex] ;
                    MonitorPcap.AddrMask := IpMask.Text ;
                end ;
                MonitorPcap.IgnoreData := IgnoreData.Checked ;
                MonitorPcap.IgnoreLAN := IgnoreLAN.Checked ;
                MonitorPcap.IgnoreNonIP := IgnoreNonIP.Checked ;
                MonitorPcap.Promiscuous := Promiscuous.Checked ;
                MonitorPcap.ClearIgnoreIP ;
                if IgnoreIPs.Lines.Count <> 0 then
                begin
                    for I := 0 to Pred (IgnoreIPs.Lines.Count) do
                            MonitorPcap.SetIgnoreIP (IgnoreIPs.Lines [I]) ;
                end ;
                MonitorPcap.StartMonitor ;
                if NOT MonitorPcap.Connected then
                begin
                    LogWin.Lines.Add (MonitorPcap.LastError) ;
                    exit ;
                end ;
                LogWin.Lines.Add ('Capture Started - ' + AdapterList.Items
                         [AdapterList.ItemIndex] + ' on ' + MonitorPcap.Addr) ;
            end
            else
            begin
                MonitorSocket.Addr := MonIpList.Items [MonIpList.ItemIndex] ;
                MonitorSocket.AddrMask := IpMask.Text ;
                MonitorSocket.IgnoreData := IgnoreData.Checked ;
                MonitorSocket.IgnoreLAN := IgnoreLAN.Checked ;
                MonitorSocket.ClearIgnoreIP ;
                if IgnoreIPs.Lines.Count <> 0 then
                begin
                    for I := 0 to Pred (IgnoreIPs.Lines.Count) do
                            MonitorSocket.SetIgnoreIP (IgnoreIPs.Lines [I]) ;
                end ;
                MonitorSocket.StartMonitor ;
                LogWin.Lines.Add ('Capture Started - Raw Sockets on ' +
                                                        MonitorSocket.Addr) ;
            end ;
            MonLive := true ;
            doMonitor.Caption := 'Stop Monitor' ;
            LogWin.Lines.Add (CRLF_ + sHeaderLine + CRLF_);
        except
            LogWin.Lines.Add ('Failed to Start Monitor - ' +
                                            GetExceptMess (ExceptObject)) ;
        end ;
    end ;
end;

procedure TMonForm.FormCreate(Sender: TObject);
var
    I: integer ;
begin

// raw sockets monitoring
    MonitorSocket := TMonitorSocket.Create (self) ;
    MonitorSocket.onPacketEvent := PacketEvent ;
    MonIpList.Items := LocalIPList ;
    if MonIpList.Items.Count > 0 then MonIpList.ItemIndex := 0 ;

// winpcap monitoring, needs packet.dll and drivers installed
    if LoadPacketDll then
    begin
        MonitorPcap := TMonitorPcap.Create (self) ;
        MonitorPcap.onPacketEvent := PacketEvent ;
        LogWin.Lines.Add ('WinPCap/Npcap version: ' + Pcap_GetPacketVersion) ; // 8 Aug 2010
        AdapterList.Items.Assign (MonitorPcap.AdapterDescList) ;
        if AdapterList.Items.Count <> 0 then
        begin
            AdapterList.ItemIndex := 0 ;
            if AdapterList.Items.Count >= 2 then  // skip dialup adaptors
            begin
                for I := 0 to AdapterList.Items.Count - 2 do
                begin
                    if Pos ('dial', LowerCase (AdapterList.Items [I])) = 0 then break ;
                    AdapterList.ItemIndex := I + 1 ;
                end ;
            end;
            AdapterList.Enabled := true ;
            UseWinPCap.Enabled := true ;
            Promiscuous.Enabled := true ;
            IgnoreNonIp.Enabled := true ;
            AdapterIPList := TStringList.Create ;
            AdapterMaskList := TStringList.Create ;
            AdapterBcastList := TStringList.Create ;
        end ;
    end ;

    // Nov 2018 socket monitoring needs admin rights
    if IsProgAdmin then
        LabelAdmin.Caption := 'Program has Administrator Rights'
    else
    begin
        LabelAdmin.Caption := 'Program does not have Administrator Rights, no socket monitoring';
        UseWinPCap.Checked := true ;
    end;

    MonLive := false ;
end;

procedure TMonForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    if MonLive then doMonitorClick (self) ;
end;

procedure TMonForm.TimerTimer(Sender: TObject);
begin
    if NOT MonLive then exit ;
    if UseWinPCap.Checked then
    begin
        with MonitorPcap do
            LabelTraffic.Caption := 'Traffic: Sent ' + IntToKbyte (TotSendBytes) +
                            ', Received ' + IntToKbyte (TotRecvBytes) + CRLF_ +
                            'Packets Sent ' + IntToCStr (TotSendPackets) +
                            ', Received ' + IntToCStr (TotRecvPackets) ;
    end
    else
    begin
        with MonitorSocket do
            LabelTraffic.Caption := 'Traffic: Sent ' + IntToKbyte (TotSendBytes) +
                            ', Received ' + IntToKbyte (TotRecvBytes) + CRLF_ +
                            'Packets Sent ' + IntToCStr (TotSendPackets) +
                            ', Received ' + IntToCStr (TotRecvPackets) ;
    end ;
end;

procedure TMonForm.FormDestroy(Sender: TObject);
begin
    FreeAndNil (MonitorSocket) ;
    FreeAndNil (MonitorPcap) ;
end;

procedure TMonForm.AdapterListClick(Sender: TObject);
var
    I: integer ;
begin
    I := MonitorPcap.GetIPAddresses (AnsiString (MonitorPcap.AdapterNameList [AdapterList.ItemIndex]),
                                        AdapterIPList, AdapterMaskList, AdapterBcastList) ;
    if I = 0 then exit ;
    MonIpList.Items.Assign (AdapterIPList) ;
    if MonIpList.Items.Count > 0 then MonIpList.ItemIndex := 0 ;
    IpMask.Text := AdapterMaskList [0] ;
end;

procedure TMonForm.UseWinPCapClick(Sender: TObject);
begin
    if UseWinPCap.Checked then
        AdapterListClick(self)
    else
    begin
        MonIpList.Items := LocalIPList ;
        if MonIpList.Items.Count > 0 then MonIpList.ItemIndex := 0 ;
    end ;
end;


end.

