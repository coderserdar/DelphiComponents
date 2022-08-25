unit signmain;
{
Magenta Serial Port Detection Component
Updated by Angus Robertson, Magenta Systems Ltd, England, 1st February 2022

delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/

This application tests three different components:
1 - Magenta Serial Port Detection Component which contains serial COM port
enumeration functions, using several methods which can identify different
ports depending on how they are installed, all are combined and a sorted
array returned with friendly names and install information.  
2 - Magenta Hardware Events Component that listens for Windows
hardware event messages and calls events handlers for device changes such as
serial ports, disk volume changes, low disk space events and, power events.
3 - Directory Changes Monitoring Component, that notifies changes in a
directory such as file or directory Create/Delete/Modify/Rename.



History
4 March 2003 - Async Pro 4
6 Dec 2007 try emumerating serial ports
7 Dec 2007 use port name not number - patched Async Pro so ComPortName instead
Oct 2014 - 2.0 - new serial detection and reporting, hardware monitoring
Jan 2022 - 3.0 - keep settings in INI file, DirChange created in code.


Note - this sample requires Async Pro to be installed to open and monitor
serial ports, but Async Pro is not required to list list serial ports.
Also there is a modified AdPort component that allows opening of named
ports using ComPortName method, the original component only allows
numbered ports, note the modified AdPort component is not currently
compatible with unicode versions of Async Pro from https://github.com/TurboPack/AsyncPro
}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, IniFiles,
  AdStatLt, OoMisc, AdPort, AdSelCom,
  magsubs1,
  MagSerPorts,
  MagHardwareEvents,
  MagDirChange;

type
  TForm1 = class(TForm)
    ApdComPort: TApdComPort;
    ApdSLController: TApdSLController;
    Panel1: TPanel;
    StatusCTS: TApdStatusLight;
    StatusDSR: TApdStatusLight;
    StatusDCD: TApdStatusLight;
    StatusRI: TApdStatusLight;
    StatusER: TApdStatusLight;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    StatusRX: TApdStatusLight;
    StatusTX: TApdStatusLight;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    doCom: TButton;
    SetRTS: TCheckBox;
    SetDTR: TCheckBox;
    doStop: TButton;
    doExt: TButton;
    Log: TMemo;
    Timer: TTimer;
    Panel2: TPanel;
    PortList: TListView;
    doListPorts: TButton;
    doMonDirs: TCheckBox;
    procedure ApdComPortTriggerStatus(CP: TObject; TriggerHandle: Word);
    procedure doComClick(Sender: TObject);
    procedure SetDTRClick(Sender: TObject);
    procedure doStopClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SetRTSClick(Sender: TObject);
    procedure doExtClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure HardwareEventsComPortEvent(Sender: TObject; const PortName: string; Arrived: Boolean);
    procedure HardwareEventsLogEvent(Sender: TObject; const Line: string);
    procedure HardwareEventsLowSpaceEvent(Sender: TObject; const VolName: string; Full: Boolean);
    procedure HardwareEventsPowerEvent(Sender: TObject; PowerEvent: Integer; const Desc: string);
    procedure HardwareEventsRawEvent(Sender: TObject; var Msg: TMessage);
    procedure HardwareEventsVolumeEvent(Sender: TObject; const VolName: string; Arrived: Boolean);
    procedure PortListDblClick(Sender: TObject);
    procedure doListPortsClick(Sender: TObject);
    procedure DirChangeCreated(Sender: TObject; FileName: string);
    procedure DirChangeDeleted(Sender: TObject; FileName: string);
    procedure DirChangeModified(Sender: TObject; FileName: string);
    procedure DirChangeRenamed(Sender: TObject; fromFileName, toFileName: string);
    procedure doMonDirsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure RefreshSerialPorts;
  end;

var
  Form1: TForm1;
  TrigMS: Word;
  HardwareEvents: THardwareEvents ;
  SerialPorts: TSerialPorts;
  TotPorts: integer ;
  FIniFileName: string;
  DirChange: TDirChange;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
    IniFile : TMemIniFile;
    section: string ;
begin
// get old settings
    FIniFileName := ChangeFileExt (ParamStr (0), '.ini') ;
    IniFile := TMemIniFile.Create(FIniFileName);
    with IniFile do
    begin
        section := 'Main' ;
        Top := ReadInteger ('Window', 'Top', (Screen.Height - Height) div 2);
        Left := ReadInteger ('Window', 'Left', (Screen.Width - Width) div 2);
        Width := ReadInteger ('Window', 'Width', Width);
        Height := ReadInteger ('Window', 'Height', Height);
    end;
    IniFile.Free;

    doStop.Enabled := false ;
    doListPortsClick (Self) ;
    HardwareEvents := THardwareEvents.Create (Self) ;
    with HardwareEvents do
    begin
        onComPortEvent := HardwareEventsComPortEvent ;
        onLogEvent := HardwareEventsLogEvent ;
        onLowSpaceEvent := HardwareEventsLowSpaceEvent ;
        onPowerEvent := HardwareEventsPowerEvent ;
        onRawEvent := HardwareEventsRawEvent ;
        onVolumeEvent := HardwareEventsVolumeEvent ;
        Log.Lines.Add ('Hardware Monitoring Started');
    end;
    DirChange := TDirChange.Create(Self);
    with DirChange do
    begin
        Path := 'c:\';
        OnCreated := DirChangeCreated;
        OnDeleted := DirChangeDeleted;
        OnModified := DirChangeModified;
        OnRenamed := DirChangeRenamed;
        WatchSubtree := True;
        WatchFilters := [nfFILE_NAME, nfDIR_NAME, nfLAST_WRITE, nfCREATION];
    end;                                                                    
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TMemIniFile;
    section: string ;
begin
    doStopClick(Self) ;
    DirChange.Active := False;
    if Assigned (HardwareEvents) then HardwareEvents.Free ;
    IniFile := TMemIniFile.Create(FIniFileName);
    with IniFile do
    begin
        section := 'Main' ;
        WriteInteger ('Window', 'Top', Top);
        WriteInteger ('Window', 'Left', Left);
        WriteInteger ('Window', 'Width', Width);
        WriteInteger ('Window', 'Height', Height);
    end ;
    IniFile.UpdateFile;
    IniFile.Free;
end;
procedure TForm1.doMonDirsClick(Sender: TObject);
begin
    if doMonDirs.Checked then
        DirChange.Active := True
     else
         DirChange.Active := False;
end;

procedure TForm1.DirChangeCreated(Sender: TObject; FileName: string);
begin
    Log.Lines.Add ('DirChanged: Create: ' + FileName);
end;

procedure TForm1.DirChangeDeleted(Sender: TObject; FileName: string);
begin
    Log.Lines.Add ('DirChanged: Deleted: ' + FileName);
end;

procedure TForm1.DirChangeModified(Sender: TObject; FileName: string);
begin
    Log.Lines.Add ('DirChanged: Modified: ' + FileName);
end;

procedure TForm1.DirChangeRenamed(Sender: TObject; fromFileName, toFileName: string);
begin
    Log.Lines.Add ('DirChanged: Renamed: ' + fromFileName + ' > ' + toFileName);
end;

{
// following function needs patched version of AdPort.pas to allow AsyncPro to open named serial ports with ComPortName
procedure TForm1.doComClick(Sender: TObject);
begin
    try
        if PortList.ItemIndex < 0 then Exit;
        doCom.Enabled := false ;
        doStop.Enabled := true ;
        ApdComPort.ComPortName := PortList.Items [PortList.ItemIndex].Caption ;
        if ApdComPort.Open then ApdComPort.Open := false ;
        ApdComPort.Open := true ;
        if ApdComPort.Open then
        begin
            TrigMS := ApdComPort.AddStatusTrigger (stModem) ;
            ApdComPort.SetStatusTrigger (TrigMS, msCTSDelta or msDCDDelta or
                                             msRingDelta or msDSRDelta, True) ;
            ApdSLController.Monitoring := true ;
            ApdComPort.DTR := SetDTR.Checked ;
            ApdComPort.RTS := SetRTS.Checked ;
            Log.Lines.Add (TimeToStr (Time) + ' Monitor ' + ApdComPort.ComPortName) ;
        end ;
    except
        Log.Lines.Add ('Unable to Open ' + ApdComPort.ComPortName +
                                               ' - ' + GetExceptMess (ExceptObject)) ;
        doStopClick(Self) ;
    end ;
end;  }

procedure TForm1.doComClick(Sender: TObject);
begin
    try
        if PortList.ItemIndex < 0 then Exit;
        doCom.Enabled := false ;
        doStop.Enabled := true ;
        ApdComPort.ComNumber := StrToInt(Copy(PortList.Items [PortList.ItemIndex].Caption, 4, 3)) ;
        if ApdComPort.Open then ApdComPort.Open := false ;
        ApdComPort.Open := true ;
        if ApdComPort.Open then
        begin
            TrigMS := ApdComPort.AddStatusTrigger (stModem) ;
            ApdComPort.SetStatusTrigger (TrigMS, msCTSDelta or msDCDDelta or
                                             msRingDelta or msDSRDelta, True) ;
            ApdSLController.Monitoring := true ;
            ApdComPort.DTR := SetDTR.Checked ;
            ApdComPort.RTS := SetRTS.Checked ;
            Log.Lines.Add (TimeToStr (Time) + ' Monitor COM' + IntToStr(ApdComPort.ComNumber)) ;
        end ;
    except
        Log.Lines.Add ('Unable to Open COM' + IntToStr(ApdComPort.ComNumber) +
                                               ' - ' + GetExceptMess (ExceptObject)) ;
        doStopClick(Self) ;
    end ;
end;

procedure TForm1.SetDTRClick(Sender: TObject);
begin
    if ApdComPort.Open then ApdComPort.DTR := SetDTR.Checked ;
end;

procedure TForm1.doStopClick(Sender: TObject);
begin
    try
        if ApdComPort.Open then
        begin
            Log.Lines.Add (TimeToStr (Time) + ' Stop Monitor') ;
            ApdComPort.SetStatusTrigger (TrigMS, 0, false) ;
            ApdSLController.Monitoring := false ;
            ApdComPort.Open := false ;
        end ;
    except
    end ;
    doStop.Enabled := false ;
    doCom.Enabled := true ;
end;

procedure TForm1.SetRTSClick(Sender: TObject);
begin
    if ApdComPort.Open then ApdComPort.RTS := SetRTS.Checked ;
end;

procedure TForm1.doExtClick(Sender: TObject);
begin
    Close ;
end;

procedure TForm1.ApdComPortTriggerStatus(CP: TObject; TriggerHandle: Word);
var
    info: string ;
begin
    info := '' ;
    if TriggerHandle = TrigMS then
    begin
        if ApdComPort.DeltaCTS then
        begin
            if ApdComPort.CTS then
                info := info + 'CTS Raised, '
            else
                info := info + 'CTS Dropped, ' ;
        end ;
        if ApdComPort.DeltaDSR then
        begin
            if ApdComPort.DSR then
                info := info + 'DSR Raised, '
            else
                info := info + 'DSR Dropped, ' ;
        end ;
        if ApdComPort.DeltaDCD then
        begin
            if ApdComPort.DCD then
                info := info + 'DCD Raised, '
            else
                info := info + 'DCD Dropped, ' ;
        end ;
        if ApdComPort.DeltaRI then
        begin
            if ApdComPort.RI then
                info := info + 'Ring Raised'
            else
                info := info + 'Ring Dropped' ;
        end ;
        ApdComPort.SetStatusTrigger (TrigMS, msCTSDelta or msDCDDelta or
                                            msRingDelta or msDSRDelta, True) ;
    end;
    if info <> '' then Log.Lines.Add (TimeToStr (Time) + ' ' + info) ;
end;

procedure TForm1.RefreshSerialPorts;
var
    I: integer ;
begin
    PortList.Items.Clear;
    TotPorts := EnumSerialPortsEx (SerialPorts, true) ;
    If TotPorts = 0 then
    begin
        Log.Lines.Add ('Total Serial Ports: None');
        exit ;
    end;
    for I := 0 to Pred (TotPorts) do
    begin
        with PortList.Items.Add do
        begin
            Caption := SerialPorts [I].ComName;
            SubItems.Add (GetYN (SerialPorts [I].Enabled));
            SubItems.Add (SerialPorts [I].FriendlyName);
            SubItems.Add (SerialPorts [I].IntName);
            SubItems.Add (SerialPorts [I].NumPort);
            SubItems.Add (SerialPorts [I].Desc);
            SubItems.Add (SerialPorts [I].Manufacturer);
            SubItems.Add (SerialPorts [I].HardwareId);
            SubItems.Add (SerialPorts [I].Location);
            SubItems.Add (SerialPorts [I].StatInfo);
        end;
    end;
    Log.Lines.Add ('Total Serial Ports: ' + IntToStr(TotPorts));
end;

procedure TForm1.doListPortsClick(Sender: TObject);
begin
    RefreshSerialPorts;
    Log.Lines.Add (ReportSerialPorts (SerialPorts));
end;


procedure TForm1.HardwareEventsComPortEvent(Sender: TObject; const PortName: string; Arrived: Boolean);
begin
    if Arrived then
        Log.Lines.Add ('New Serial Port Arrived: ' + PortName)
    else
        Log.Lines.Add ('Old Serial Port Removed: ' + PortName) ;
    doListPortsClick (Self) ;
end;

procedure TForm1.HardwareEventsLogEvent(Sender: TObject; const Line: string);
begin
    Log.Lines.Add ('Device Hardware Event: ' + Line); // both power and device events
end;

procedure TForm1.HardwareEventsLowSpaceEvent(Sender: TObject; const VolName: string; Full: Boolean);
begin
    if Full then
        Log.Lines.Add ('Disk Space Exhausted for Volume ' + VolName)
    else
        Log.Lines.Add ('Disk Space low for Volume ' + VolName) ;
end;

procedure TForm1.HardwareEventsPowerEvent(Sender: TObject; PowerEvent: Integer; const Desc: string);
begin
    Log.Lines.Add ('Power event: ' + Desc);

 // ports may have been added or removed while suspended
    if PowerEvent = PBT_APMRESUMESUSPEND then doListPortsClick (Self) ;
end;

procedure TForm1.HardwareEventsRawEvent(Sender: TObject; var Msg: TMessage);
begin
    Log.Lines.Add ('Device Change Message, event=0x' + IntToHex (Msg.WPARAM, 4) +
                                                            ', data=0x' + IntToHex (Msg.LPARAM, 8)) ;
end;

procedure TForm1.HardwareEventsVolumeEvent(Sender: TObject; const VolName: string; Arrived: Boolean);
begin
    if Arrived then
        Log.Lines.Add ('New Disk Arrived, Volume: ' + VolName)
    else
        Log.Lines.Add ('Old Disk Removed, Volume: ' + VolName) ;
end;

procedure TForm1.PortListDblClick(Sender: TObject);
begin
    if NOT doCom.Enabled then Exit; 
    doComClick(Self);
end;

procedure TForm1.TimerTimer(Sender: TObject);
begin
    if ApdComPort.DeltaRI or ApdComPort.RI then
                    Log.Lines.Add (TimeToStr (Time) + ' Ring') ;
end;


end.
