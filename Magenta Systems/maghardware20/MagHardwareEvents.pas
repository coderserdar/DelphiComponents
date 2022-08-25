unit MagHardwareEvents;

// Delphi 7 and later

{$IFNDEF VER140}
  {$WARN UNSAFE_TYPE off}
  {$WARN UNSAFE_CAST off}
  {$WARN UNSAFE_CODE off}
{$ENDIF}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}

{
Magenta Hardware Events Component.
Updated by Angus Robertson, Magenta Systems Ltd, England, 1st February 2022
delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

This unit contains the THardwareEvents component that listens for Windows 
hardware event messages and calls events handlers for device changes such 
as serial ports, disk volume changes, low disk space events and, power events.

THardwareEvents is installable, and has events onLogEvent (Device Hardware
Event below), onComPortEvent, onVolumeEvent, onLowSpaceEvent, onPowerEvent
and onRawEvent (Device Change Message below). Power events return PBT_xx
values for suspend, resume, battery low, etc.  Note the component starts
monitoring as soon as it is created.

Device Change Message, event=0x8004, data=0x007A1828
Device Hardware Event:  devicetype=5
Device Change Message, event=0x8004, data=0x007AC960
Old Serial Port Removed: COM5
Device Hardware Event: Old Serial Port Removed: COM5
Device Change Message, event=0x0007, data=0x00000000

Device Change Message, event=0x8000, data=0x0078EF00
Device Hardware Event:  devicetype=5
Device Change Message, event=0x8000, data=0x0078EC68
New Serial Port Arrived: COM5
Device Hardware Event: New Serial Port Installed: COM5
Device Change Message, event=0x0007, data=0x00000000

Device Change Message, event=0x8000, data=0x0079E260
Device Hardware Event:  devicetype=5
Device Change Message, event=0x0007, data=0x00000000
Device Change Message, event=0x8000, data=0x0009E3C0
New Disk Arrived, Volume: I
Device Hardware Event: Disk Change - New Disk Volume Installed: I
Device Change Message, event=0x0007, data=0x00000000


}

interface

uses
  SysUtils, Classes, Windows, Messages, Controls,
  MagDBT;

// power management stuff from pbt.h
const
    PBT_APMQUERYSUSPEND         =   $0000 ;
    PBT_APMQUERYSTANDBY         =   $0001 ;
    PBT_APMQUERYSUSPENDFAILED   =   $0002 ;
    PBT_APMQUERYSTANDBYFAILED   =   $0003 ;
    PBT_APMSUSPEND              =   $0004 ;
    PBT_APMSTANDBY              =   $0005 ;
    PBT_APMRESUMECRITICAL       =   $0006 ;
    PBT_APMRESUMESUSPEND        =   $0007 ;
    PBT_APMRESUMESTANDBY        =   $0008 ;
    PBTF_APMRESUMEFROMFAILURE   =   $00000001 ;
    PBT_APMBATTERYLOW           =   $0009 ;
    PBT_APMPOWERSTATUSCHANGE    =   $000A ;
    PBT_APMOEMEVENT             =   $000B ;
    PBT_APMRESUMEAUTOMATIC      =   $0012 ;

    GUID_DEVINTERFACE_USB_DEVICE: TGUID = '{A5DCBF10-6530-11D2-901F-00C04FB951ED}';

type
    PWMPOWERBROADCAST = ^TWMPOWERBROADCAST;
    TWMPOWERBROADCAST = record
        Msg: Cardinal;
        PowerEvent: DWORD;
        Data: DWORD;
    end ;

    TLogEvent = procedure (Sender: TObject; const Line: string) of object;
    TComPortEvent = procedure (Sender: TObject; const PortName: string; Arrived: Boolean) of object;
    TVolumeEvent = procedure (Sender: TObject; const VolName: string; Arrived: Boolean) of object;
    TLowSpaceEvent = procedure (Sender: TObject; const VolName: string; Full: Boolean) of object;
    TPowerEvent = procedure (Sender: TObject; PowerEvent: Integer; const Desc: string) of object;
    TRawEvent = procedure (Sender: TObject; var Msg: TMessage) of object;


type
  THardwareEvents = class(TComponent)
  private
    { Private declarations }
  protected
    { Protected declarations }
    FWindowHandle: HWND;
    FLogEvent: TLogEvent;
    FComPortEvent: TComPortEvent;
    FVolumeEvent: TVolumeEvent;
    FLowSpaceEvent: TLowSpaceEvent;
    FPowerEvent: TPowerEvent;
    FRawEvent: TRawEvent;
    procedure WMPowerBroadcast (var Msg : TMessage);
    procedure WMDeviceChange (var Msg : TMessage);
    procedure WndProc(var Msg: TMessage);
  public
    { Public declarations }
    DeviceHandle : Pointer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RegisterEventHandler (AHandle: THandle);
    procedure UnRegisterEventHandler;
  published
    { Published declarations }
    property onLogEvent: TLogEvent            read FLogEvent write FLogEvent;
    property onComPortEvent: TComPortEvent    read FComPortEvent write FComPortEvent;
    property onVolumeEvent: TVolumeEvent      read FVolumeEvent write FVolumeEvent;
    property onLowSpaceEvent: TLowSpaceEvent  read FLowSpaceEvent write FLowSpaceEvent;
    property onPowerEvent: TPowerEvent        read FPowerEvent write FPowerEvent;
    property onRawEvent: TRawEvent            read FRawEvent write FRawEvent;
  end;

procedure Register;

implementation

procedure Register;
begin
    RegisterComponents('Magenta Systems', [THardwareEvents]);
end;

constructor THardwareEvents.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FWindowHandle := AllocateHWnd (WndProc);
    RegisterEventHandler (FWindowHandle);
end;

destructor THardwareEvents.Destroy;
begin
    UnRegisterEventHandler;
    DeallocateHWnd (FWindowHandle);
    inherited Destroy;
end;

procedure THardwareEvents.WMPowerBroadcast (var Msg : TMessage);
var
    PowerBroadcast: PWMPOWERBROADCAST;
    mess: string ;
begin
    PowerBroadcast := @Msg ;

// find out what's happening
    mess := 'Unknown Event ' + IntToStr (PowerBroadcast.PowerEvent) ;
    case PowerBroadcast.PowerEvent of
        PBT_APMBATTERYLOW: mess := 'Battery power is low' ;
        PBT_APMOEMEVENT: mess := 'OEM-defined event occurred' ;
        PBT_APMPOWERSTATUSCHANGE: mess := 'Power status has changed' ;
        PBT_APMQUERYSUSPEND: mess := 'Request for permission to suspend' ;
        PBT_APMSUSPEND: mess := 'System is suspending operation' ;
        PBT_APMQUERYSUSPENDFAILED: mess := 'Suspension request denied' ;
        PBT_APMRESUMECRITICAL: mess := 'Operation resuming after critical suspension' ;
        PBT_APMRESUMESUSPEND: mess := 'Operation resuming after suspension' ;
        PBT_APMRESUMEAUTOMATIC: mess := 'Operation resuming automatically after event' ;
    end ;
    if Assigned (FPowerEvent) then
       FPowerEvent (Self, PowerBroadcast.PowerEvent, mess) ;
    if Assigned (FLogEvent) then
      FLogEvent (Self, mess) ;
end ;

// check device change events, new modems, RAS up/down???

procedure THardwareEvents.WMDeviceChange (var Msg : TMessage);
var
    DEVBROADCAST: PDEV_BROADCAST_HDR ;
//  DEVBROADCASTDEVNODE: PDEV_BROADCAST_DEVNODE ;
{$IFDEF UNICODE}
    DEVBROADCASTPORT: PDEV_BROADCAST_PORT_W ;
{$ELSE}
    DEVBROADCASTPORT: PDEV_BROADCAST_PORT_A ;
{$ENDIF}
    DEVBROADCASTVOLUME: PDEV_BROADCAST_VOLUME;
    info, portname, volname: string ;
    I, mask: longword ;
begin
    try

 // !!!! TCP/IP changes needs new IP Helper stuff for NotifyAddrChange, DBT_DEVTYP_NET not used

    // send when configmg finished a process tree batch. Some devnodes
    //  may have been added or removed. This is used by ring3 people which
    //  need to be refreshed whenever any devnode changed occur (like device manager).
     {   info := 'Hardware Device Changed Detected: ' ;  // 10 Apri 2014
        if (Msg.WPARAM = DBT_DEVNODES_CHANGED) then
        begin
            info := '' ;
        end ;       }

    // pass everything
       if Assigned (FRawEvent) then
            FRawEvent (Self, Msg) ;

    // debug
        info := '' ;

    // main parameters
        if (Msg.LPARAM < 0) or (Msg.LPARAM > 10000) then
        begin
            Pointer (DEVBROADCAST) := PChar (Msg.LPARAM) ;
            info := info + ' devicetype=' + IntToStr (DEVBROADCAST.dbch_devicetype) ;
            if DEVBROADCAST.dbch_devicetype = DBT_DEVTYP_DEVNODE then
            begin
             //   Pointer (DEVBROADCASTDEVNODE) := PChar (Msg.LPARAM) ;
             //   info := '' ;   // too many times
            end ;
            if DEVBROADCAST.dbch_devicetype = DBT_DEVTYP_PORT then
            begin
                Pointer (DEVBROADCASTPORT) := PChar (Msg.LPARAM) ;
                info := info + ' port=' + String(DEVBROADCASTPORT.dbcp_name) ;
            end ;
            if DEVBROADCAST.dbch_devicetype = DBT_DEVTYP_VOLUME then
            begin
                Pointer (DEVBROADCASTVOLUME) := PChar (Msg.LPARAM) ;
                info := info + ' unitmask=' + IntToHex (DEVBROADCASTVOLUME.dbcv_unitmask, 8) ;
            end ;
            if DEVBROADCAST.dbch_devicetype = DBT_DEVTYP_NET then
            begin
                info := info + 'NET' ;
            end;
        end ;

    // disk space warning
        if (Msg.WPARAM = DBT_NO_DISK_SPACE) then
        begin
            volname := Char (Msg.LPARAM + 64) ;
            info := 'Zero Disk Space Warning for: ' + volname ;
            if Assigned (FLowSpaceEvent) then
                        FLowSpaceEvent (Self, volname, True) ;
        end ;
        if (Msg.WPARAM = DBT_LOW_DISK_SPACE) then
        begin
            volname := Char (Msg.LPARAM + 64) ;
            info := 'Low Disk Space Warning for: ' + volname ;
            if Assigned (FLowSpaceEvent) then
                        FLowSpaceEvent (Self, volname, False) ;
        end ;

    // new hardware added or removed
        if (Msg.WPARAM = DBT_DEVICEARRIVAL) or (Msg.WPARAM = DBT_DEVICEREMOVEPENDING) or
                                                (Msg.WPARAM = DBT_DEVICEREMOVECOMPLETE) then
        begin
            Pointer (DEVBROADCASTPORT) := PChar (Msg.LPARAM) ;
            Pointer (DEVBROADCASTVOLUME) := PChar (Msg.LPARAM) ;
            if DEVBROADCASTPORT.dbch_devicetype = DBT_DEVTYP_VOLUME then
            begin
                mask := 1 ;
                volname := '' ;  // may be several volume letters
                for I := 0 to 25 do
                begin
                    if (DEVBROADCASTVOLUME.dbcv_unitmask AND mask) = mask then
                                        volname := volname + Char (I + 65) ;
                    mask := mask * 2 ;
                end ;
                if Msg.WPARAM = DBT_DEVICEARRIVAL then
                begin
                    info := 'Disk Change - New Disk Volume Installed: ' + volname ;
                    if Assigned (FVolumeEvent) then
                        FVolumeEvent (Self, volname, True) ;
                end
                else if Msg.WPARAM = DBT_DEVICEREMOVEPENDING then
                begin
                    info := 'Disk Change - Disk Volume Being Removed: ' + volname ;
                    if Assigned (FVolumeEvent) then
                        FVolumeEvent (Self, volname, False) ;
                end
                else if Msg.WPARAM = DBT_DEVICEREMOVECOMPLETE then
                begin
                    info := 'Disk Change - Disk Volume Now Removed: ' + volname ;
                    if Assigned (FVolumeEvent) then
                        FVolumeEvent (Self, volname, False) ;
                end
                else
                     info := 'Disk Change - Unexpected Event: ' + volname ;
            end ;
            if (DEVBROADCASTPORT.dbch_devicetype = DBT_DEVTYP_PORT) then
            begin
                portname := String(DEVBROADCASTPORT.dbcp_name) ;
                if Msg.WPARAM = DBT_DEVICEARRIVAL then
                begin
                    info := 'New Serial Port Installed: ' + portname ;
                    if Assigned (FComPortEvent) then
                        FComPortEvent (Self, portname, True) ;
                end
                else
                begin
                    info := 'Old Serial Port Removed: ' + portname ;
                    if Assigned (FComPortEvent) then
                        FComPortEvent (Self, portname, False) ;
                end;
           end ;
        end ;
        if (info <> '') and Assigned (FLogEvent) then
                                    FLogEvent (Self, info) ;
    except
        on E:Exception do begin
            if Assigned (FLogEvent) then
                FLogEvent (Self, 'Error Processing Windows Device Change Message: ' +
                                                                info + ' - ' + E.Message) ;
        end;
    end ;
end ;


procedure THardwareEvents.WndProc(var Msg: TMessage);
begin
 {   case Msg of
      WM_DISPLAYCHANGE:
        WMDisplayChange(TWMDisplayChange(Message));
      WM_DEVMODECHANGE:
        DoDevModeChange(TWMDevModeChange(Message).Device);
      WM_SETTINGCHANGE:
        with TWMSettingChange(Message) do
          DoSettingChange(Flag, Section);
      WM_COMPACTING:
        DoCompacting(wParam);
      WM_USERCHANGED:
        DoUserChanged;
      WM_TIMECHANGE:
        DoTimeChange;
      WM_FONTCHANGE:
        DoFontChange;
      WM_SYSCOLORCHANGE:
        DoSysColorChange;
      WM_SPOOLERSTATUS:
        with TWMSpoolerStatus(Message) do
          DoSpoolerStatus(JobStatus, JobsLeft);
      WM_PALETTEISCHANGING:
        with TWmPaletteIsChanging(Message) do
          DoPaletteChanging(Realize);
      WM_PALETTECHANGED:
        with TWmPaletteChanged(Message) do
          DoPaletteChanged(PalChg);  }

    if (Msg.Msg = WM_DEVICECHANGE) then
        WMDeviceChange (Msg)
    else if (Msg.Msg = WM_POWERBROADCAST) or (Msg.Msg = WM_POWER) then
        WMPowerBroadcast (Msg)
    else
        Msg.Result := DefWindowProc (FWindowHandle, Msg.Msg, Msg.wParam, Msg.lParam);
end;

// not needed for DBT_DEVTYP_PORT or DBT_DEVTYP_VOLUME.which are broadcast to all top level windows

procedure THardwareEvents.RegisterEventHandler (AHandle: THandle);
var
{$IFDEF UNICODE}
    Dbi : DEV_BROADCAST_DEVICEINTERFACE_W ;
{$ELSE}
    Dbi : DEV_BROADCAST_DEVICEINTERFACE_A ;
{$ENDIF}
    iSize : integer ;
begin
    iSize := SizeOf(Dbi) ;
    ZeroMemory (@Dbi, iSize) ;
    Dbi.dbch_size := iSize ;
    Dbi.dbch_devicetype := DBT_DEVTYP_DEVICEINTERFACE ;
    Dbi.dbcc_classguid := GUID_DEVINTERFACE_USB_DEVICE ;
    DeviceHandle := RegisterDeviceNotification (AHandle, @Dbi, DEVICE_NOTIFY_WINDOW_HANDLE) ;
end;

procedure THardwareEvents.UnRegisterEventHandler;
begin
    if DeviceHandle <> nil then
    begin
        UnregisterDeviceNotification (DeviceHandle);
        DeviceHandle := nil;
    end;
end;

end.
