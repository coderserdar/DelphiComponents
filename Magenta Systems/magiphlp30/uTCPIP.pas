unit uTCPIP;

// Magenta Systems Internet Protocol Helper TCP/IP Monitor demo application
// 26th November 2018 - Release 4.0 (C) Magenta Systems Ltd, 2018
// based on work by by Dirk Claessens

// Copyright by Angus Robertson, Magenta Systems Ltd, England
// delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/

(*

  Developed on: D4.03
  Tested on   :  WIN-NT4/SP6, WIN98se, WIN95/OSR1

  ================================================================
                    This software is FREEWARE
                    -------------------------
  If this software works, it was surely written by Dirk Claessens
                http://users.pandora.be/dirk.claessens2/
                  Dirk Claessens <dirkcl@pandora.be>
  (If it doesn't, I don't know anything about it.)
  ================================================================

v1.3 - 18th September 2001
----
  Angus Robertson, Magenta Systems Ltd, England
     delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
  Dynamic load DLL, show error if not available
  Re-arranged windows slightly

v1.4 - 28th February 2002 - Angus
  Re-arranged windows again so usable on 800x600 screen

v1.5 - 26th July 2002 - Angus
  Added DNS Server for adaptors

v1.6 - 19th August 2002 - Angus
  Improved connections list adding process and EXE XP only (using undocumented APIs)

v1.7 - 14th October 2003 - Angus - bug fixes in IPHelper

v1.8 - 25th October 2005 - Angus

v1.9 - 8th August 2006 - Angus
  Made main window resizable, needs minimum screen 1024x768

v2.0 - 25th February 2007 - Angus
Tested OK on Vista

v2.1 - 5th August 2008 - Angus
Updated to be compatible with Delphi 2009

v2.2 - 16th January 2009 - Angus
Better compatibility with Vista and later, tested on Windows 7 beta

v2.3 - 3rd August 2009
Changed ULONGLONG to LONGLONG for Delphi 7 compatability

v2.4 - 8th August 2010
    Fixed various cast warning for Delphi 2009 and later

v2.5 - 12th August 2011
    Fixes for 64-bit

v3.0 - 26th November 2018
   Added IPv6 support, numerous new structures and functions, Vista and later
   Only supporting XP SP3 and later, so remove code for earlier OSs
   Still runs on XP SP3, but TCP and UDP connection lists not supported and
     some other functions return limited info, IP addresses in particular
   Note XP has not been tested for several years since it's out of support.
   Added notification functions for interface changes
   Larger windows, some columns wider



Pending - IPv6 not yet supported for ARP or IP Routing table, sorry

*)



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, jpeg, IPHelper, IpHlpApi ;

const
   version = 'Magenta v3.0 beta - 26th November 2018' ;
type
  TIPForm = class( TForm )
    Timer1: TTimer;
    PageControl1: TPageControl;
    ARPSheet: TTabSheet;
    ConnSheet: TTabSheet;
    IP1Sheet: TTabSheet;
    ARPMemo: TMemo;
    StaticText1: TStaticText;
    TCPMemo: TMemo;
    StaticText2: TStaticText;
    IPAddrMemo: TMemo;
    IP2Sheet: TTabSheet;
    IPForwMemo: TMemo;
    StaticText8: TStaticText;
    AdaptSheet: TTabSheet;
    AdaptMemo: TMemo;
    SpeedButton1: TSpeedButton;
    cbTimer: TCheckBox;
    NwMemo: TMemo;
    StaticText10: TStaticText;
    StatsSheet: TTabSheet;
    ICMPInMemo: TMemo;
    ICMPOutMemo: TMemo;
    StaticText12: TStaticText;
    btRTTI: TSpeedButton;
    cbRecentIPs: TComboBox;
    edtRTTI: TEdit;
    StaticText14: TStaticText;
    IfMemo: TMemo;
    TCPStatMemo: TMemo;
    StaticText7: TStaticText;
    UDPStatsMemo: TMemo;
    StaticText4: TStaticText;
    IPStatsMemo: TMemo;
    StaticText5: TStaticText;
    UDPMemo: TMemo;
    StaticText3: TStaticText;
    Image1: TImage;
    Panel1: TPanel;
    TabSheet2: TTabSheet;
    ShowProgPath: TCheckBox;
    ChangesSheet: TTabSheet;
    ChangesLog: TMemo;
    procedure Timer1Timer( Sender: TObject );
    procedure FormCreate( Sender: TObject );
    procedure SpeedButton1Click( Sender: TObject );
    procedure cbTimerClick( Sender: TObject );
    procedure btRTTIClick( Sender: TObject );
    procedure cbRecentIPsClick( Sender: TObject );
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    procedure DOIpStuff;
    procedure DoIpChangesEvent (IpAddrInfo: TIpAddrInfo; CallerContext: Pointer;
                                                NotificationType: TMibNoticationType);
  public
    { Public declarations }
  end;

var
  IPForm        : TIPForm;
//  DebugList: TStringList ;

implementation

{$R *.DFM}

//------------------------------------------------------------------------------
procedure TIPForm.FormCreate( Sender: TObject );
var
    Error: Integer;
begin
//  DebugList := TStringList.Create ;  // test memory leak reporting
  PageControl1.ActivePage := ConnSheet;
  Caption := Caption + ' ' + Version;
  if LoadIpHlp then
  begin
      DOIpStuff;
      ChangesLog.Lines.Clear ;
      fIpChangesEvent := DoIpChangesEvent;  // Nov 2014 notification event
      Error := IpChangesStart (AF_UNSPEC, Nil);
      if Error <> NO_ERROR then ChangesLog.Lines.Add (SysErrorMessage (Error));
      Timer1.Enabled := true;
  end
  else
      ShowMessage( 'Internet Helper DLL Not Available or Not Supported') ;
end;

procedure TIPForm.FormDestroy(Sender: TObject);
begin
    IpChangesStop;  // Nov 2014
end;

procedure TIPForm.FormResize(Sender: TObject);
var
   Half: integer;
begin
    Half := AdaptSheet.Height div 2 ;
    AdaptMemo.Top := 0 ;
    AdaptMemo.Height := Half;
    IfMemo.Top := Half;
    IfMemo.Height := Half;
end;

//------------------------------------------------------------------------------
procedure TIPForm.DOIpStuff;
begin
  IpHelper.ShowExePath := ShowProgPath.Checked ;   // 15 Jan 2009
  Get_NetworkParams( NwMemo.Lines );
  Get_ARPTable( ARPMemo.Lines );
  Get_TCPTable( TCPMemo.Lines );
  Get_TCPStatistics( TCPStatMemo.Lines );
  Get_UDPTable( UDPMemo.Lines );
  Get_IPStatistics( IPStatsMemo.Lines );
  Get_IPAddrTable( IPAddrMemo.Lines );
  Get_IPForwardTable( IPForwMemo.Lines );
  Get_UDPStatistics( UDPStatsMemo.Lines );
  Get_AdaptersInfo( AdaptMemo.Lines );
  Get_IfTable2( IfMemo.Lines );   // Nov 2014
  Get_ICMPStats( ICMPInMemo.Lines, ICMPOutMemo.Lines );
  Get_RecentDestIPs( cbRecentIPs.Items );
end;

//------------------------------------------------------------------------------
procedure TIPForm.cbTimerClick( Sender: TObject );
begin
  Timer1.Enabled := (cbTimer.State = cbCHECKED) ;
end;

//------------------------------------------------------------------------------
procedure TIPForm.Timer1Timer( Sender: TObject );
begin
  if cbTimer.State = cbCHECKED then
  begin
    Timer1.Enabled := false;
    DoIPStuff;
    Timer1.Enabled := true;
  end;
end;

//------------------------------------------------------------------------------
procedure TIPForm.SpeedButton1Click( Sender: TObject );
begin
  Speedbutton1.Enabled := false;
  DoIPStuff;
  Speedbutton1.Enabled := true;
end;

//------------------------------------------------------------------------------
procedure TIPForm.btRTTIClick( Sender: TObject );
var
  IPadr         : dword;
  Rtt, HopCount : longint;
  Res           : integer;
begin
  btRTTI.Enabled := false;
  Screen.Cursor := crHOURGLASS;
  IPadr := Str2IPAddr( edtRTTI.Text );
  Res := Get_RTTAndHopCount( IPadr, 128, RTT, HopCount );
  if Res = NO_ERROR then
    ShowMessage( ' Round Trip Time '
      + inttostr( rtt ) + ' ms, '
      + inttostr( HopCount )
      + ' hops to : ' + edtRTTI.Text
      )
  else
    ShowMessage( 'Error occurred:' + #13
                 + ICMPErr2Str( Res ) ) ;
  btRTTI.Enabled := true;
  Screen.Cursor := crDEFAULT;
end;

//------------------------------------------------------------------------------
procedure TIPForm.cbRecentIPsClick( Sender: TObject );
begin
  edtRTTI.Text := cbRecentIPs.Items[cbRecentIPs.ItemIndex];
end;

//------------------------------------------------------------------------------
procedure TIPForm.DoIpChangesEvent (IpAddrInfo: TIpAddrInfo; CallerContext: Pointer;
                                                NotificationType: TMibNoticationType);
begin
    if NotificationType = MibInitialNotification then
        ChangesLog.Lines.Add ('Waiting for IP address notification change events')
    else if  NotificationType = MibAddInstance then
        ChangesLog.Lines.Add ('New IP address added: ' + IpAddrInfo.IpAddress + ' on ' + IpAddrInfo.Description)
    else if  NotificationType = MibDeleteInstance then
        ChangesLog.Lines.Add ('Old IP address removed: ' + IpAddrInfo.IpAddress + ' on ' + IpAddrInfo.Description);
end;

end.
