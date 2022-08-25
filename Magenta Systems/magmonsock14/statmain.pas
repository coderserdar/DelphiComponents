unit statmain;

{ Magenta Systems Internet Packet Monitoring Components

Demo Application - Traffic Monitor using Raw Sockets and WinPcap
Updated by Angus Robertson, Magenta Systems Ltd, England, v1.4 26th November 2018
delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

TMonitorSocket needs WSocket from François PIETTE internet component suite
V8 from http://www.overbyte.eu/
TMonitorPcap needs WinPcap or Npcap, but this application will run without it
being installed

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

Baseline v1.1 29th October 2005

8 Aug 2008  - 1.2 - updated to support ICS V6 and V7, and Delphi 2009
                   when stopping capture ignore any buffered data so it stops faster
9 Aug 2010  - 1.3 - fixed various cast warnings with Delphi 2009 and later
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
  sTrafficLine = '%-15s %-15s %-12s %14s %14s ' ;
//               192.168.1.109   192.168.1.108   microsoft-ds 1.19M [4.47K]  1.87M [4.77K]
  sHeaderLine = 'Local IP        Remote IP       Service      Sent [packets] Received [packets]' ;

type
  TStatForm = class(TForm)
    Panel1: TPanel;
    LabelTraffic: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MonIpList: TListBox;
    doMonitor: TButton;
    doExit: TButton;
    IgnoreLAN: TCheckBox;
    IpMask: TEdit;
    IgnoreIPs: TMemo;
    UseWinPCap: TCheckBox;
    AdapterList: TListBox;
    IgnoreNonIp: TCheckBox;
    Promiscuous: TCheckBox;
    LogDestinations: TMemo;
    Timer: TTimer;
    doReport: TButton;
    AutoDisplay: TCheckBox;
    LabelAdmin: TLabel;
    procedure doExitClick(Sender: TObject);
    procedure doMonitorClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure AdapterListClick(Sender: TObject);
    procedure UseWinPCapClick(Sender: TObject);
    procedure doReportClick(Sender: TObject);
  private
    { Private declarations }
    procedure PacketEvent (Sender: TObject; PacketInfo: TPacketInfo) ;
  public
    { Public declarations }
  end;

var
  StatForm: TStatForm;
  MonitorSocket: TMonitorSocket ;
  MonitorPcap: TMonitorPcap ;
  MonLive: boolean ;
  UpdateTrafficCounter: integer ;
  AdapterIPList: TStringList ;
  AdapterMaskList: TStringList ;
  AdapterBcastList: TStringList ;
  TrafficClass: TTrafficClass ;

implementation

{$R *.dfm}


procedure TStatForm.PacketEvent (Sender: TObject; PacketInfo: TPacketInfo) ;
begin
    if NOT MonLive then exit ;   // 8 Aug 2008 ignore data once Stopped
    TrafficClass.Add (PacketInfo) ;
end ;

procedure TStatForm.doExitClick(Sender: TObject);
begin
    if MonLive then doMonitorClick (self) ;
    Close ;
end;

procedure TStatForm.doMonitorClick(Sender: TObject);
var
    I: integer ;
begin
    if MonIpList.ItemIndex < 0 then exit ;
    if MonLive then
    begin
        MonLive := false ;
        LogDestinations.Lines.Add ('Capture Stopped' + CRLF) ;
        if UseWinPCap.Checked then
            MonitorPcap.StopMonitor
        else
            MonitorSocket.StopMonitor ;
        doMonitor.Caption := 'Start Monitor' ;
    end
    else
    begin
        TrafficClass.Clear ;
        try
            if UseWinPCap.Checked then
            begin
                MonitorPcap.MonAdapter := AnsiString (MonitorPcap.AdapterNameList [AdapterList.ItemIndex]);  // 9 Aug 2010
                I := MonitorPcap.GetIPAddresses (MonitorPcap.MonAdapter, AdapterIPList,
                                                    AdapterMaskList, AdapterBcastList) ;
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
                MonitorPcap.IgnoreData := true ;  // we never want data
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
                    LogDestinations.Lines.Add (MonitorPcap.LastError) ;
                    exit ;
                end ;
                LogDestinations.Lines.Add ('Capture Started - ' + AdapterList.Items
                         [AdapterList.ItemIndex] + ' on ' + MonitorPcap.Addr) ;
            end
            else
            begin
                MonitorSocket.Addr := MonIpList.Items [MonIpList.ItemIndex] ;
                MonitorSocket.AddrMask := IpMask.Text ;
                MonitorSocket.IgnoreData := true ;  // we never want data
                MonitorSocket.IgnoreLAN := IgnoreLAN.Checked ;
                MonitorSocket.ClearIgnoreIP ;
                if IgnoreIPs.Lines.Count <> 0 then
                begin
                    for I := 0 to Pred (IgnoreIPs.Lines.Count) do
                            MonitorSocket.SetIgnoreIP (IgnoreIPs.Lines [I]) ;
                end ;
                MonitorSocket.StartMonitor ;
                LogDestinations.Lines.Add ('Capture Started - Raw Sockets on ' +
                                                        MonitorSocket.Addr) ;
            end ;
            MonLive := true ;
            doMonitor.Caption := 'Stop Monitor' ;
        //    LogDestinations.Lines.Add (CRLF + sHeaderLine + CRLF) ;
        except
            LogDestinations.Lines.Add ('Failed to Start Monitor - ' + GetExceptMess (ExceptObject)) ;
        end ;
    end ;
end;

procedure TStatForm.FormCreate(Sender: TObject);
var
    I: integer ;
begin

// traffic records
    TrafficClass := TTrafficClass.Create (self) ;

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
        LogDestinations.Lines.Add ('WinPCap/Npcap version: ' + Pcap_GetPacketVersion) ; // Nov 2018
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

procedure TStatForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    if MonLive then doMonitorClick (self) ;
end;

procedure TStatForm.TimerTimer(Sender: TObject);
var
    TotalTraffic: TServiceInfo ;
begin
    if NOT MonLive then exit ;
{    if UseWinPCap.Checked then
    begin
        with MonitorPcap do
            LabelTraffic.Caption := 'Traffic: Sent ' + IntToKbyte (TotSendBytes) +
                            ', Received ' + IntToKbyte (TotRecvBytes) + CRLF +
                            'Packets Sent ' + IntToCStr (TotSendPackets) +
                            ', Received ' + IntToCStr (TotRecvPackets) ;
    end
    else
    begin
        with MonitorSocket do
            LabelTraffic.Caption := 'Traffic: Sent ' + IntToKbyte (TotSendBytes) +
                            ', Received ' + IntToKbyte (TotRecvBytes) + CRLF +
                            'Packets Sent ' + IntToCStr (TotSendPackets) +
                            ', Received ' + IntToCStr (TotRecvPackets) ;
    end ;   }

    TotalTraffic := TrafficClass.GetTotals ;
    with TotalTraffic do
            LabelTraffic.Caption := 'Traffic: Sent ' + IntToKbyte (BytesSent) +
                            ', Received ' + IntToKbyte (BytesRecv) + CRLF_ +
                            'Packets Sent ' + IntToCStr (PacksSent) +
                            ', Received ' + IntToCStr (PacksRecv) ;

    if NOT AutoDisplay.Checked then exit ;
    inc (UpdateTrafficCounter) ;
    if UpdateTrafficCounter > 10 then  // update traffic list every 10 seconds
    begin
        UpdateTrafficCounter := 0 ;
        doReportClick (Self) ;
    end ;
end;

procedure TStatForm.doReportClick(Sender: TObject);
var
    I: integer ;
    S: string ;
begin
    LogDestinations.Lines.Clear ;
    LogDestinations.Lines.Add (sTrafficHdr) ;
    if TrafficClass.TotTraffic = 0 then exit ;
    TrafficClass.UpdateService ;
    for I := 0 to Pred (TrafficClass.TotTraffic) do
    begin
        S := TrafficClass.GetFmtTrafStr (I) ;
        if S = '' then continue ;  // sanity check
        LogDestinations.Lines.Add (S) ;
    end ;
    LogDestinations.Lines.Add (CRLF + sServiceHdr) ;
    if TrafficClass.TotService = 0 then exit ;
    for I := 0 to Pred (TrafficClass.TotService) do
    begin
        S := TrafficClass.GetFmtServStr (I) ;
        if S = '' then continue ;  // sanity check
        LogDestinations.Lines.Add (S) ;
    end ;
end;

procedure TStatForm.FormDestroy(Sender: TObject);
begin
    FreeAndNil (TrafficClass) ;
    FreeAndNil (MonitorSocket) ;
    FreeAndNil (MonitorPcap) ;
end;

procedure TStatForm.AdapterListClick(Sender: TObject);
var
    I: integer ;
begin
    I := MonitorPcap.GetIPAddresses (AnsiString (MonitorPcap.AdapterNameList [AdapterList.ItemIndex]),
                                        AdapterIPList, AdapterMaskList, AdapterBcastList) ;   // 9 Aug 2010?
    if I = 0 then exit ;
    MonIpList.Items.Assign (AdapterIPList) ;
    if MonIpList.Items.Count > 0 then MonIpList.ItemIndex := 0 ;
    IpMask.Text := AdapterMaskList [0] ;
end;

procedure TStatForm.UseWinPCapClick(Sender: TObject);
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
