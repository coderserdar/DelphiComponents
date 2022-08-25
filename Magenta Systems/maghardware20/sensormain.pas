unit sensormain;

{ Magenta Location and GPS component sample application
Updated by Angus Robertson, Magenta Systems Ltd, England, 1st February 2022
delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

The TMagGpsLoc component is designed to process GPS location data from
various sources with an event triggered when movement is detected.

Sources supported are:
1 - Windows Location API, available on most Windows tablets and higher end
laptops with a GPS sensor.
2 - NMEA 0183 sentences commonly generated by GPS receivers, these are
text lines starting with $. NMEA 0183 sentences processed are: GGA, GSA,
GSV, RMC, GLL and VTG, others are ignored.
3 - GT02 GPS Tracker Protocol, used by Concox TR02 vehicle trackers that
combine GPS, GPRS and GSM is a small 12V driven package designed for mounting
in vehicles.
4 - TK102/103 Tracker Protocol, essentially the NMEA RMC sentence, preceded by
date/time and mobile number, followed by useful stuff from other NMEA sentences
like satellite count, mobile IMEI and cell station stuff.
5 - WondeX/TK5000 Tracker Protocol used by VT-10, VT300 and other devices is a
simple format with IMEI, time, co-ordinates, speed and direction.

Some of these GPS protocols are also generated by Android and iOS mobile
apps for location tracking.

Most testing was with a GlobalSat BU-353-S4 USB GPS Receiver a two inch diameter
device with a roof magnet that presents as a Prolific serial port (a version with
a real serial connector is also available), a DIYmalls VK-162 USB GPS/Glonass Dongle
using an eighth generation U-Blox module, and the Concox TR02 vehicle tracker.
Also tested were a battery operated GlobalSat BT-359 Bluetooth CoPilot GPS device
(but Bluetooth serial ports are not always very reliable) and NMEA 0183 streaming
from Android tablets and phones.  GlobalSat, DIYmalls and TK102/103 devices are
available from Amazon.  Android GPSd Forwarder is available in the Play store and
sends raw NMEA packets to a UDP server (no IMEI).

This sample requires the Async Pro serial RS232 library to access GPS devices
with serial output https://github.com/TurboPack/AsyncPro, and the Internet
Component Suite library for TCP devices http://wiki.overbyte.eu/wiki/index.php/Main_Page

This sample includes a Google maps display for tracking GPS location movement
using the TWebBrowser component which uses the Internet Explorer engine.
Unfortunately Microsoft has removed Internet Explorer from Windows 11 so map
display is more problematic, currently it still works but Google displays a
warning about using non-supported browser and plans to remove support for
MSIE in August 2022. Delphi 10.4 and later has a new TEdgeBrowser component
that is used by the sensoredgemap.pas unit which should be used instead of
sensormap.pas, it is easier to use than TWebBrowser. Change uses below and
the DPR.
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ActiveX, OleServer, ExtCtrls, Spin, OoMisc, IniFiles,
  magsubs1,
  magsubs4,
  MagGpsLoc,
  MagGpsConv,
  LocationApiLib_TLB,
  AdPort, AdPacket,
  OverbyteIcsLogger,
  OverbyteIcsUtils,
  OverbyteIcsIpStreamLog,
  OverbyteIcsWndControl ;


type
  TFormMain = class(TForm)
  // saved components 
    SetLocEvents: TCheckBox;
    RawData: TCheckBox;
    MinInterval: TSpinEdit;
    MaxInterval: TSpinEdit;
    NmeaComPort: TEdit;
    NmeaIpAddr: TEdit;
    NmeaIpPort: TSpinEdit;
    Gt02ServIP: TEdit;
    Gt02ServPort: TSpinEdit;
    Gt02Cmd: TEdit;
    Tk102IpAddr: TEdit;
    Tk102IpPort: TSpinEdit;
    WondeXIpAddr: TEdit;
    WondeXIpPort: TSpinEdit;
// unsaved components
    Log: TMemo;
    PaneRight: TPanel;
    ApdComPort: TApdComPort;
    ApdDataPacket: TApdDataPacket;
    BoxLocation: TGroupBox;
    doLocation: TButton;
    doLocStart: TButton;
    doLocEnd: TButton;
    BoxNmea: TGroupBox;
    LabelNmea: TLabel;
    doNMEACom: TButton;
    doNmeaTCP: TButton;
    doNmeaStop: TButton;
    BoxGT02: TGroupBox;
    LabelGt02Packets: TLabel;
    doGT02ServStart: TButton;
    doGt02ServStop: TButton;
    ButtonBox: TPanel;
    doClose: TButton;
    doGetOS: TButton;
    doMap: TButton;
    CaptureData: TMemo;
    doLatLong: TButton;
    doSensorInfo: TButton;
    Label1: TLabel;
    Label2: TLabel;
    doGT02ServCmd: TButton;
    BoxTk102: TGroupBox;
    LabelTk102: TLabel;
    doTK102Start: TButton;
    doTK102Stop: TButton;
    BoxWondex: TGroupBox;
    LabelWondeX: TLabel;
    doWondeXStart: TButton;
    doWondeXStop: TButton;
    MagIpLog: TIcsIpStrmLog;
    LabelPosition: TLabel;
    doNmeaTCPSrv: TButton;
    doNmeaUDP: TButton;
    procedure doLocationClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure doLocEndClick(Sender: TObject);
    procedure doLocStartClick(Sender: TObject);
    procedure doCloseClick(Sender: TObject);
    procedure doLatLongClick(Sender: TObject);
    procedure doGetOSClick(Sender: TObject);
    procedure MagIpLogRecvEvent(Sender: TObject; Socnr: Integer; const Line: string);
    procedure doNMEAComClick(Sender: TObject);
    procedure doNmeaTCPClick(Sender: TObject);
    procedure doNmeaStopClick(Sender: TObject);
    procedure MagIpLogLogProgEvent(Sender: TObject; Socnr: Integer; LogOption: TLogOption; const Msg: string);
    procedure doMapClick(Sender: TObject);
{$IFDEF UNICODE}
    procedure ApdDataPacketStringPacket(Sender: TObject; Data: AnsiString);
{$ELSE}
    procedure ApdDataPacketStringPacket(Sender: TObject; Data: String);
{$ENDIF}
    procedure doGT02ServStartClick(Sender: TObject);
    procedure doSensorInfoClick(Sender: TObject);
    procedure doGt02ServStopClick(Sender: TObject);
    procedure doGT02ServCmdClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MagIpLogLogChangeEvent(Sender: TObject; Socnr: Integer; LogState: TStrmLogState);
    procedure doTK102StartClick(Sender: TObject);
    procedure doTK102StopClick(Sender: TObject);
    procedure doWondeXStartClick(Sender: TObject);
    procedure doWondeXStopClick(Sender: TObject);
    procedure doNmeaUDPClick(Sender: TObject);
    procedure doNmeaTCPSrvClick(Sender: TObject);
  private
    { Private declarations }
    procedure LocationChange(Sender: TObject);
    procedure StatusChange(Sender: TObject);
    procedure SetButtons (start: boolean; GpsType: TGpsType) ;
    procedure SendDataEvent (Sender: TObject; const Data: string);
    procedure LogEvent (Sender: TObject; const Data: string);
  public
    { Public declarations }
  end;

const
  RegEmulMSIEKey = 'Software\Microsoft\Internet Explorer\Main\FeatureControl\FEATURE_BROWSER_EMULATION' ;
  EmulMSIE11E = 11001 ;  // Windows 10 Edge mode or MSIE 11
  EmulMSIE11D = 11000 ;  // MSIE 11 default
  EmulMSIE10S = 10001 ;  // MSIE 10 standards mode
  EmulMSIE10D = 10000 ;  // MSIE 10 default           MAPS earliest
  EmulMSIE9S  = 9999 ;   // MSIE 9 standards mode
  EmulMSIE9D  = 9000 ;   // MSIE 9 default
  EmulMSIE8S  = 8888 ;   // MSIE 8 standards mode
  EmulMSIE8D  = 8000 ;   // MSIE 8 default


var
  FormMain: TFormMain;
  MagGpsLoc: TMagGpsLoc ;
  MapOpenFlag: boolean ;
  TotSentences: integer ;
  LastClientNr: integer ;
  FIniFileName: string;

implementation

{$R *.dfm}

Uses sensormap ;
// Uses sensoredgemap ;  // Delphi 10.4 and later - also change DPR

procedure TFormMain.SetButtons (start: boolean; GpsType: TGpsType) ;
begin
    doLocStart.Enabled := false ;
    doLocEnd.Enabled := false ;
    doLocation.Enabled := false ;
    doSensorInfo.Enabled := false ;
    doNMEACom.Enabled := false ;
    doNmeaTCP.Enabled := false ;
    doNmeaTCPSrv.Enabled := false ;
    doNmeaUDP.Enabled := false ;
    doNmeaStop.Enabled := false ;
    doGT02ServStart.Enabled := false ;
    doGt02ServStop.Enabled := false ;
    doGT02ServCmd.Enabled := false ;
    doTK102Start.Enabled := false ;
    doTK102Stop.Enabled := false ;
    doWondeXStart.Enabled := false ;
    doWondeXStop.Enabled := false ;
    if start then
    begin
        doSensorInfo.Enabled := true ;
        if GpsType = GpsTypeNMEA then
        begin
            doNmeaStop.Enabled := true ;
        end
        else if GpsType = GpsTypeGT02 then
        begin
            doGt02ServStop.Enabled := true ;
            doGT02ServCmd.Enabled := true ;
        end
        else if GpsType = GpsTypeTK10X then
        begin
            doTK102Stop.Enabled := true ;
        end
        else if GpsType = GpsTypeWondeX then
        begin
            doWondeXStop.Enabled := true ;
        end
        else if GpsType = GpsTypeLocApi then
        begin
            doLocEnd.Enabled := true ;
            doLocation.Enabled := true ;
        end;
    end
    else
    begin
        doGT02ServStart.Enabled := true ;
        doNMEACom.Enabled := true ;
        doNmeaTCP.Enabled := true ;
        doNmeaTCPSrv.Enabled := true ;
        doNmeaUDP.Enabled := true ;
        doLocStart.Enabled := MagGpsLoc.LocApiOK ;
        doTK102Start.Enabled := true ;
        doWondeXStart.Enabled := true ;
    end;
end;


procedure TFormMain.FormCreate(Sender: TObject);
var
    IniFile : TMemIniFile;
    SectionData: string ;
begin
// get old settings
    FIniFileName := ChangeFileExt (ParamStr (0), '.ini') ;
    IniFile := TMemIniFile.Create(FIniFileName);
    with IniFile do
    begin
        SectionData := 'Main' ;
        if ReadString (SectionData, 'SetLocEvents_Checked', 'False') = 'True' then SetLocEvents.Checked := true else SetLocEvents.Checked := false ;
        if ReadString (SectionData, 'RawData_Checked', 'False') = 'True' then RawData.Checked := true else RawData.Checked := false ;
        MinInterval.Value := ReadInteger (SectionData, 'MinInterval_Value', MinInterval.Value) ;
        MaxInterval.Value := ReadInteger (SectionData, 'MaxInterval_Value', MaxInterval.Value) ;
        NmeaComPort.Text := ReadString (SectionData, 'NmeaComPort_Text', NmeaComPort.Text) ;
        NmeaIpAddr.Text := ReadString (SectionData, 'NmeaIpAddr_Text', NmeaIpAddr.Text) ;
        NmeaIpPort.Value := ReadInteger (SectionData, 'NmeaIpPort_Value', NmeaIpPort.Value) ;
        Gt02ServIP.Text := ReadString (SectionData, 'Gt02ServIP_Text', Gt02ServIP.Text) ;
        Gt02ServPort.Value := ReadInteger (SectionData, 'Gt02ServPort_Value', Gt02ServPort.Value) ;
        Gt02Cmd.Text := ReadString (SectionData, 'Gt02Cmd_Text', Gt02Cmd.Text) ;
        Tk102IpAddr.Text := ReadString (SectionData, 'Tk102IpAddr_Text', Tk102IpAddr.Text) ;
        Tk102IpPort.Value := ReadInteger (SectionData, 'Tk102IpPort_Value', Tk102IpPort.Value) ;
        WondeXIpAddr.Text := ReadString (SectionData, 'WondeXIpAddr_Text', WondeXIpAddr.Text) ;
        WondeXIpPort.Value := ReadInteger (SectionData, 'WondeXIpPort_Value', WondeXIpPort.Value) ;
        Top := ReadInteger ('Window', 'Top', (Screen.Height - Height) div 2);
        Left := ReadInteger ('Window', 'Left', (Screen.Width - Width) div 2);
        Width := ReadInteger ('Window', 'Width', Width);
        Height := ReadInteger ('Window', 'Height', Height);
    end;
    IniFile.Free;

    CoInitializeEx(nil, COINIT_MULTITHREADED OR COINIT_DISABLE_OLE1DDE);
    MagGpsLoc := TMagGpsLoc.Create (self) ;
    MinInterval.Value := MagGpsLoc.MinInterval ;
    MaxInterval.Value := MagGpsLoc.MaxInterval ;
    SetButtons (false, GpsTypeNone) ;
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TMemIniFile;
    SectionData, temp: string ;
begin
    SetButtons (false, GpsTypeNone) ;
    if MagGpsLoc.Active then MagGpsLoc.Active := false ;
    MagIpLog.StopLogging ;
    Action := caFree ;
    IniFile := TMemIniFile.Create(FIniFileName);
    with IniFile do
    begin
        SectionData := 'Main' ;
        if SetLocEvents.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'SetLocEvents_Checked', temp) ;
        if RawData.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'RawData_Checked', temp) ;
        WriteInteger (SectionData, 'MinInterval_Value', MinInterval.Value) ;
        WriteInteger (SectionData, 'MaxInterval_Value', MaxInterval.Value) ;
        WriteString (SectionData, 'NmeaComPort_Text', NmeaComPort.Text) ;
        WriteString (SectionData, 'NmeaIpAddr_Text', NmeaIpAddr.Text) ;
        WriteInteger (SectionData, 'NmeaIpPort_Value', NmeaIpPort.Value) ;
        WriteString (SectionData, 'Gt02ServIP_Text', Gt02ServIP.Text) ;
        WriteInteger (SectionData, 'Gt02ServPort_Value', Gt02ServPort.Value) ;
        WriteString (SectionData, 'Gt02Cmd_Text', Gt02Cmd.Text) ;
        WriteString (SectionData, 'Tk102IpAddr_Text', Tk102IpAddr.Text) ;
        WriteInteger (SectionData, 'Tk102IpPort_Value', Tk102IpPort.Value) ;
        WriteString (SectionData, 'WondeXIpAddr_Text', WondeXIpAddr.Text) ;
        WriteInteger (SectionData, 'WondeXIpPort_Value', WondeXIpPort.Value) ;
        WriteInteger ('Window', 'Top', Top);
        WriteInteger ('Window', 'Left', Left);
        WriteInteger ('Window', 'Width', Width);
        WriteInteger ('Window', 'Height', Height);
    end ;
    IniFile.UpdateFile;
    IniFile.Free;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
    MagGpsLoc.Free ;
    CoUnInitialize; // <-- free memory
end;

procedure TFormMain.doSensorInfoClick(Sender: TObject);
var
    S: String;
begin
    S := MagGpsLoc.GetNmeaInfo;
    if S = '' then    
        S := MagGpsLoc.LastErrorStr;
    Log.Lines.Add (S) ;
end;

// simple latlong function using COM object, not MagGpsLoc component

procedure TFormMain.doLatLongClick(Sender: TObject);
var
    LatLongReportFactory: TLatLongReportFactory;
    MyReport: IDispLatLongReport ;
begin
    try
        LatLongReportFactory := TLatLongReportFactory.Create(Nil);
        LatLongReportFactory.Connect ;
        Log.Lines.Add ('LatLong COM Connected') ;
        MyReport := LatLongReportFactory.LatLongReport ;
        Log.Lines.Add ('Lat=' + Double2EStr (MyReport.Latitude, 4) +
                 ', Long=' + Double2EStr (MyReport.Longitude, 4) +
                 ', at ' + DateTimetoStr (MyReport.Timestamp))  ;
        LatLongReportFactory.Disconnect;
        LatLongReportFactory.Destroy;
        Log.Lines.Add ('LatLong COM Disconnected') ;
    except
        Log.Lines.Add ('LatLong COM Error - ' + GetExceptMess (ExceptObject)) ;
     end;
end;

procedure TFormMain.doGetOSClick(Sender: TObject);
var
    ProductType: longword ;
begin
    Log.Lines.Add ('GetOSVersion = ' + GetOSVersion) ;

    Log.Lines.Add ('Raw = ' + IntToStr(OsInfoRaw.dwMajorVersion) +  '.' +
         IntToStr(OsInfoRaw.dwMinorVersion) + '.' + IntToStr(LOWORD(OsInfoRaw.dwBuildNumber)) +
         ', wProduct = ' + IntToStr(OsInfoRaw.wProductType) +  ', wSuite = x' + IntToHex(OsInfoRaw.wSuiteMask, 4));

    if GetProductInfo (OsInfo.dwMajorVersion, OsInfo.dwMinorVersion,
                      OsInfo.wServicePackMajor, OsInfo.wServicePackMinor, ProductType) then
         Log.Lines.Add ('Product Type = x' + IntToHex (ProductType, 2) + ' - ' + IntToStr (ProductType )) ;

// July 2021 stuff from registry          
    Log.Lines.Add ('Product Name = ' + MagGetRegHlmStr (CVNTKey, 'ProductName')) ;
    Log.Lines.Add ('Display Version = ' + MagGetRegHlmStr (CVNTKey, 'DisplayVersion')) ;
    Log.Lines.Add ('Release ID = ' + MagGetRegHlmStr (CVNTKey, 'ReleaseId')) ;
    Log.Lines.Add ('Current Build = ' + MagGetRegHlmStr (CVNTKey, 'CurrentBuild')) ;

end;


procedure TFormMain.doCloseClick(Sender: TObject);
begin
    Close ;
end;

procedure TFormMain.doLocStartClick(Sender: TObject);
begin
    if MagGpsLoc.Active then exit ;
    MagGpsLoc.OnLocationChange := Nil ;
    MagGpsLoc.OnStatusChange := Nil ;
    if SetLocEvents.Checked then
    begin
        MagGpsLoc.OnLocationChange := LocationChange ;
        MagGpsLoc.OnStatusChange := StatusChange ;
    end;
    MagGpsLoc.RequestPermission := true ;
    MagGpsLoc.MinInterval := MinInterval.Value ;
    MagGpsLoc.MaxInterval := MaxInterval.Value ;
    MagGpsLoc.GpsType := GpsTypeLocApi ;
    MagGpsLoc.Active := true ;
    if MagGpsLoc.Active then
    begin
        Log.Lines.Add ('Location Started') ;
        SetButtons (true, GpsTypeLocApi) ;
    end
    else
        Log.Lines.Add (MagGpsLoc.LastErrorStr) ;
end;

procedure TFormMain.doMapClick(Sender: TObject);
var
    fname: string ;
begin
    if MapOpenFlag then exit ;
    fname := ExtractFileName (ParamStr(0)) ;
 // Dec 2016 force browser into MSIE 11 mode to avoid script errors
    regWriteInteger (HKEY_CURRENT_USER, RegEmulMSIEKey + '\' +  fname, EmulMSIE11E) ;
    FormMap := TFormMap.Create (Self) ;
    FormMap.Show ;
    MapOpenFlag := true ;
   // may not be ready to plot yet
    if FormMap.CheckRealTime.Checked then FormMap.PlotLatLong ;
end;

procedure TFormMain.LocationChange(Sender: TObject);
var
    S: string ;
    Alt, Eastings, Northings: integer;
//    Lat, Long: double ;
begin
    try
        with Sender as TMagGpsLoc do
        begin
            S := GetLocText ;
            if Distance >= 5 then S := S + ', Distance ' + IntToStr (Distance) + 'm' ;
            Log.Lines.Add (S) ;
            LabelPosition.Caption := S;
            Alt := 0 ;
            if WGS84LatLongToNGREx (Latitude, Longitude, Alt, Eastings, Northings) then
            begin
                Log.Lines.Add ('GB Eastings ' + IntToCStr (Eastings) + ', GB Northings ' + IntToCStr (Northings)) ;
             //   NGRToWGS84LatLong (Eastings, Northings, Lat, Long, Alt) ;  // testing, convert it back
             //   Log.Lines.Add ('Converted Latitude ' + Double2EStr (Lat, 5) + ', Longitude ' + Double2EStr (Long, 5)) ;
            end;
            if MapOpenFlag then
            begin
                if FormMap.CheckRealTime.Checked then
                begin
                    FormMap.LabelUpdate.Caption := 'Last Plot Update: ' + TimeToStr (Time) ;
                  {  if (Distance >= 5) then } FormMap.PlotLatLong ;
                end;
            end;
        end ;
    except
    end;
end;

procedure TFormMain.StatusChange(Sender: TObject);
begin
    Log.Lines.Add ('Status Changed: ' + MagGpsLoc.StatusStr) ;
end;

procedure TFormMain.LogEvent (Sender: TObject; const Data: string);
begin
    Log.Lines.Add (Data) ;
end;

procedure TFormMain.doLocationClick(Sender: TObject);
begin
    if MagGpsLoc.GetLocation then
        LocationChange (MagGpsLoc)
    else
        Log.Lines.Add (MagGpsLoc.LastErrorStr) ;
end;

procedure TFormMain.doLocEndClick(Sender: TObject);
begin
    SetButtons (false, GpsTypeNone) ;
    if NOT MagGpsLoc.Active then exit ;
    MagGpsLoc.Active := false ;
    Log.Lines.Add ('Location Stopped')
end;

procedure TFormMain.doNMEAComClick(Sender: TObject);
begin
    if MagGpsLoc.Active then exit ;
    TotSentences := 0 ;
    LabelPosition.Caption := 'No position data yet';
    LabelNmea.Caption := 'Nmea Sentences:' ;
    ApdComPort.ComNumber := StrToInt(Copy(NmeaComPort.Text, 4, 3)) ;
// following line needs patched version of AdPort.pas to allow AsyncPro to open named serial ports with ComPortName
//  ApdComPort.ComPortName := NmeaComPort.Text ;
    MagGpsLoc.MinInterval := MinInterval.Value ;
    MagGpsLoc.MaxInterval := MaxInterval.Value ;
    MagGpsLoc.GpsType := GpsTypeNMEA ;
    MagGpsLoc.ExtendedRep := true ;
    MagGpsLoc.OnLocationChange := LocationChange ;
    MagGpsLoc.OnStatusChange := StatusChange ;
    MagGpsLoc.Active := true ;
    if MagGpsLoc.Active then
    begin
        SetButtons (true, GpsTypeNMEA) ;
        try
            ApdComPort.Open := true ;
        except
        end;
        if NOT ApdComPort.Open then
        begin
            Log.Lines.Add ('Failed to Open COM Port') ;
            MagGpsLoc.Active := false ;
            SetButtons (false, GpsTypeNone) ;
            exit ;
        end;
        ApdComPort.RTS := true ;
        ApdComPort.DTR := true ;
        Log.Lines.Add ('NMEA COM Port Processing Started') ;
    end
    else
        Log.Lines.Add (MagGpsLoc.LastErrorStr) ;
end;

procedure TFormMain.doNmeaTCPClick(Sender: TObject);
begin
    if MagGpsLoc.Active then exit ;
    TotSentences := 0 ;
    LabelPosition.Caption := 'No position data yet';
    LabelNmea.Caption := 'Nmea Sentences:' ;
    MagIpLog.RemoteHost := NmeaIpAddr.Text ;
    MagIpLog.RemoteIpPort := NmeaIpPort.Text ;
    MagIpLog.LogProtocol := logprotTcpClient ;
    MagIpLog.LineEndType := lineendCRLF ;
    MagIpLog.MaxLineLen := 500 ;
    MagIpLog.RetryAttempts := 6;
    MagIpLog.RetryWaitSecs := 10;
    MagGpsLoc.MinInterval := MinInterval.Value ;
    MagGpsLoc.MaxInterval := MaxInterval.Value ;
    MagGpsLoc.GpsType := GpsTypeNMEA ;
    MagGpsLoc.ExtendedRep := true ;
    MagGpsLoc.OnLocationChange := LocationChange ;
    MagGpsLoc.OnStatusChange := StatusChange ;
    MagGpsLoc.Active := true ;
    if MagGpsLoc.Active then
    begin
        SetButtons (true, GpsTypeNMEA) ;
        if NOT MagIpLog.StartLogging then
        begin
            Log.Lines.Add ('Failed to Start TCP/IP Client') ;
            MagGpsLoc.Active := false ;
            SetButtons (false, GpsTypeNone) ;
            exit ;
        end;
        Log.Lines.Add ('NMEA TCP/IP Client Connecting') ;
      // note does not mean TCP is connected yet  !!!!
    end
    else
        Log.Lines.Add (MagGpsLoc.LastErrorStr) ;

end;


procedure TFormMain.doNmeaUDPClick(Sender: TObject);
begin
    if MagGpsLoc.Active then exit ;
    TotSentences := 0 ;
    LabelPosition.Caption := 'No position data yet';
    LabelNmea.Caption := 'Nmea Sentences:' ;
    MagIpLog.LocalIpAddr := NmeaIpAddr.Text ;
    MagIpLog.LocalIpPort := NmeaIpPort.Text ;
    MagIpLog.LogProtocol := logprotUdpServer ;
    MagIpLog.LineEndType := lineendPacket ;
    MagIpLog.MaxLineLen := 500 ;
    MagGpsLoc.MinInterval := MinInterval.Value ;
    MagGpsLoc.MaxInterval := MaxInterval.Value ;
    MagGpsLoc.GpsType := GpsTypeNMEA ;
    MagGpsLoc.ExtendedRep := true ;
    MagGpsLoc.OnLocationChange := LocationChange ;
    MagGpsLoc.OnStatusChange := StatusChange ;
    MagGpsLoc.Active := true ;
    if MagGpsLoc.Active then
    begin
        SetButtons (true, GpsTypeNMEA) ;
        if NOT MagIpLog.StartLogging then
        begin
            Log.Lines.Add ('Failed to Start UDP/IP Server') ;
            MagGpsLoc.Active := false ;
            SetButtons (false, GpsTypeNone) ;
            exit ;
        end;
        Log.Lines.Add ('NMEA UDP/IP Server Listening') ;
    end
    else
        Log.Lines.Add (MagGpsLoc.LastErrorStr) ;
end;

procedure TFormMain.doNmeaTCPSrvClick(Sender: TObject);
begin
    if MagGpsLoc.Active then exit ;
    TotSentences := 0 ;
    LabelPosition.Caption := 'No position data yet';
    LabelNmea.Caption := 'Nmea Sentences:' ;
    MagIpLog.SrvIcsHosts[0].BindIpAddr := NmeaIpAddr.Text ;
    MagIpLog.SrvIcsHosts[0].BindNonPort := atoi(NmeaIpPort.Text) ;
    MagIpLog.LogProtocol := logprotTcpServer ;
    MagIpLog.LineEndType := lineendCRLF ;
    MagIpLog.MaxLineLen := 500 ;
    MagGpsLoc.MinInterval := MinInterval.Value ;
    MagGpsLoc.MaxInterval := MaxInterval.Value ;
    MagGpsLoc.GpsType := GpsTypeNMEA ;
    MagGpsLoc.ExtendedRep := true ;
    MagGpsLoc.OnLocationChange := LocationChange ;
    MagGpsLoc.OnStatusChange := StatusChange ;
    MagGpsLoc.Active := true ;
    if MagGpsLoc.Active then
    begin
        SetButtons (true, GpsTypeNMEA) ;
        if NOT MagIpLog.StartLogging then
        begin
            Log.Lines.Add ('Failed to Start TCP/IP Server') ;
            MagGpsLoc.Active := false ;
            SetButtons (false, GpsTypeNone) ;
            exit ;
        end;
        Log.Lines.Add ('NMEA TCP/IP Server Listening') ;
    end
    else
        Log.Lines.Add (MagGpsLoc.LastErrorStr) ;

end;

procedure TFormMain.MagIpLogLogChangeEvent(Sender: TObject; Socnr: Integer; LogState: TStrmLogState);
begin
 //   MagGpsLoc.Active := (LogState = logstateOK) ;
end;

procedure TFormMain.MagIpLogLogProgEvent(Sender: TObject; Socnr: Integer; LogOption: TLogOption; const Msg: string);
begin
    Log.Lines.Add (Msg) ;
end;

procedure TFormMain.MagIpLogRecvEvent(Sender: TObject; Socnr: Integer; const Line: string);
begin
    LastClientNr := Socnr ;
    if Line = '' then Exit;
    if RawData.Checked then
    begin
        if MagGpsLoc.GpsType = GpsTypeGT02 then
            CaptureData.Lines.Add (ConvHexQuads (Line))
         else
            CaptureData.Lines.Add (Line) ;
    end;
    if NOT MagGpsLoc.SetRecvData (Line) then
    begin
        if MagGpsLoc.LastErrorStr <> '' then Log.Lines.Add (MagGpsLoc.LastErrorStr) ;
    end;
    inc (TotSentences) ;
    if MagGpsLoc.GpsType = GpsTypeGT02 then
        LabelGt02Packets.Caption := 'GT02 Packets: ' + IntToCStr (TotSentences)
    else if MagGpsLoc.GpsType = GpsTypeNMEA then
        LabelNmea.Caption := 'Nmea Sentences: ' + IntToCStr (TotSentences)
    else if MagGpsLoc.GpsType = GpsTypeTK10X then
        LabelTk102.Caption := 'TK102 Packets:' + IntToCStr (TotSentences)
    else if MagGpsLoc.GpsType = GpsTypeWondeX then
        LabelWondeX.Caption := 'WondeX Packets:' + IntToCStr (TotSentences) ;
end;


{$IFDEF UNICODE}
procedure TFormMain.ApdDataPacketStringPacket(Sender: TObject; Data: AnsiString);
{$ELSE}
procedure TFormMain.ApdDataPacketStringPacket(Sender: TObject; Data: String);
{$ENDIF}
begin
    if RawData.Checked then CaptureData.Lines.Add (String(Data)) ;
    MagGpsLoc.SetRecvData (String(Data)) ;
    inc (TotSentences) ;
    LabelNmea.Caption := 'Nmea Sentences: ' + IntToCStr (TotSentences) ;

end;

procedure TFormMain.doNmeaStopClick(Sender: TObject);
begin
    if NOT MagGpsLoc.Active then exit ;
    SetButtons (false, GpsTypeNone) ;
    MagIpLog.StopLogging ;
    if ApdComPort.Open then ApdComPort.Open := false ;
    MagGpsLoc.Active := false ;
    Log.Lines.Add ('NMEA Processing Stopped')
end;

procedure TFormMain.doGT02ServCmdClick(Sender: TObject);
begin
    if NOT MagGpsLoc.Active then exit ;
    MagGpsLoc.SendGT02Cmd (Gt02Cmd.Text) ;
end;

procedure TFormMain.doGT02ServStartClick(Sender: TObject);
begin
    if MagGpsLoc.Active then exit ;
    TotSentences := 0 ;
    LabelPosition.Caption := 'No position data yet';
    LabelGt02Packets.Caption := 'GT02 Packets:' ;
    MagIpLog.SrvIcsHosts[0].BindIpAddr := Gt02ServIP.Text ;
    MagIpLog.SrvIcsHosts[0].BindNonPort := atoi(Gt02ServPort.Text) ;
    MagIpLog.LogProtocol := logprotTcpServer ;
    MagIpLog.LineEndType := lineendCRLF ;
    MagIpLog.MaxLineLen := 500 ;
    MagIpLog.RawData := true ;
    MagGpsLoc.GpsType := GpsTypeGT02 ;
    MagGpsLoc.ExtendedRep := true ;
    MagGpsLoc.OnLocationChange := LocationChange ;
    MagGpsLoc.OnStatusChange := StatusChange ;
    MagGpsLoc.OnGpsSendData := SendDataEvent ;
    MagGpsLoc.OnGpsLog := LogEvent ;
    MagGpsLoc.Active := true ;
    if MagGpsLoc.Active then
    begin
        SetButtons (true, GpsTypeGT02) ;
        if NOT MagIpLog.StartLogging then
        begin
            Log.Lines.Add ('Failed to Start TCP/IP Server') ;
            MagGpsLoc.Active := false ;
            SetButtons (false, GpsTypeNone) ;
            exit ;
        end;
        Log.Lines.Add ('GT02 TCP/IP Server Started') ;
    end
    else
        Log.Lines.Add (MagGpsLoc.LastErrorStr) ;
end;

procedure TFormMain.SendDataEvent (Sender: TObject; const Data: string);
begin
    MagIpLog.SendLogLine(AnsiString(Data));
    if RawData.Checked then CaptureData.Lines.Add ('> ' + ConvHexQuads (Data)) ;
end;

procedure TFormMain.doGt02ServStopClick(Sender: TObject);
begin
    MagIpLog.StopLogging ;
    if NOT MagGpsLoc.Active then exit ;
    SetButtons (false, GpsTypeNone) ;
    MagGpsLoc.Active := false ;
    Log.Lines.Add ('GT02 Server Stopped')
end;

procedure TFormMain.doTK102StartClick(Sender: TObject);
begin
    if MagGpsLoc.Active then exit ;
    TotSentences := 0 ;
    LabelPosition.Caption := 'No position data yet';
    LabelTk102.Caption := 'TK102 Packets:' ;
    MagIpLog.SrvIcsHosts[0].BindIpAddr := Tk102IpAddr.Text ;
    MagIpLog.SrvIcsHosts[0].BindNonPort := atoi(Tk102IpPort.Text) ;
    MagIpLog.LogProtocol := logprotTcpServer ;
    MagIpLog.LineEndType := lineendCRLF ;
    MagIpLog.MaxLineLen := 500 ;
    MagIpLog.RawData := true ;
    MagGpsLoc.GpsType := GpsTypeTK10X ;
    MagGpsLoc.ExtendedRep := true ;
    MagGpsLoc.OnLocationChange := LocationChange ;
    MagGpsLoc.OnStatusChange := StatusChange ;
    MagGpsLoc.OnGpsSendData := SendDataEvent ;
    MagGpsLoc.OnGpsLog := LogEvent ;
    MagGpsLoc.Active := true ;
    if MagGpsLoc.Active then
    begin
        SetButtons (true, GpsTypeTK10X) ;
        if NOT MagIpLog.StartLogging then
        begin
            Log.Lines.Add ('Failed to Start TCP/IP Server') ;
            MagGpsLoc.Active := false ;
            SetButtons (false, GpsTypeNone) ;
            exit ;
        end;
        Log.Lines.Add ('TK102/3 TCP/IP Server Started') ;
    end
    else
        Log.Lines.Add (MagGpsLoc.LastErrorStr) ;
end;

procedure TFormMain.doTK102StopClick(Sender: TObject);
begin
    MagIpLog.StopLogging ;
    if NOT MagGpsLoc.Active then exit ;
    SetButtons (false, GpsTypeNone) ;
    MagGpsLoc.Active := false ;
    Log.Lines.Add ('TK102/3 Server Stopped')
end;

procedure TFormMain.doWondeXStartClick(Sender: TObject);
begin
    if MagGpsLoc.Active then exit ;
    TotSentences := 0 ;
    LabelPosition.Caption := 'No position data yet';
    LabelWondeX.Caption := 'WondeX Packets:' ;
    MagIpLog.SrvIcsHosts[0].BindIpAddr := WondeXIpAddr.Text ;
    MagIpLog.SrvIcsHosts[0].BindNonPort := atoi(WondeXIpPort.Text) ;
    MagIpLog.LogProtocol := logprotTcpServer ;
    MagIpLog.LineEndType := lineendCRLF ;
    MagIpLog.MaxLineLen := 500 ;
    MagIpLog.RawData := true ;
    MagGpsLoc.GpsType := GpsTypeWondeX ;
    MagGpsLoc.ExtendedRep := true ;
    MagGpsLoc.OnLocationChange := LocationChange ;
    MagGpsLoc.OnStatusChange := StatusChange ;
    MagGpsLoc.OnGpsSendData := SendDataEvent ;
    MagGpsLoc.OnGpsLog := LogEvent ;
    MagGpsLoc.Active := true ;
    if MagGpsLoc.Active then
    begin
        SetButtons (true, GpsTypeWondeX) ;
        if NOT MagIpLog.StartLogging then
        begin
            Log.Lines.Add ('Failed to Start TCP/IP Server') ;
            MagGpsLoc.Active := false ;
            SetButtons (false, GpsTypeNone) ;
            exit ;
        end;
        Log.Lines.Add ('WondeX TCP/IP Server Started') ;
    end
    else
        Log.Lines.Add (MagGpsLoc.LastErrorStr) ;
end;

procedure TFormMain.doWondeXStopClick(Sender: TObject);
begin
    MagIpLog.StopLogging ;
    if NOT MagGpsLoc.Active then exit ;
    SetButtons (false, GpsTypeNone) ;
    MagGpsLoc.Active := false ;
    Log.Lines.Add ('WondeX Server Stopped')
end;

end.