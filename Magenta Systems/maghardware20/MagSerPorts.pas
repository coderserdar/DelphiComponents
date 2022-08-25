unit MagSerPorts;

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
Magenta Serial Port Detection Component.
Updated by Angus Robertson, Magenta Systems Ltd, England, 1st February 2022

delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

This unit contains serial COM port enumeration functions, using several
methods which can identify different ports depending on how they are
installed, all are combined and a sorted array returned with friendly
names and install information.    EnumSerialPortsEx does the following, in order:

1 - Enumerate HLM\HARDWARE\DEVICEMAP\SERIALCOMM registry
2 - Enumerate Device Installation Class 'Ports' which finds most serial ports including USB
3 - Optional add Disabled 'Ports' (hardware not currently installed)
4 - Enumerate Device Installation Class 'CNCPorts' (used by com0com serial port emulator)
5 - Enumerate Device Installation Class 'Modem' which finds USB and internal modems

The array contains the COM port name, friendly Windows desciption, manufacturer name,
internal name from registry, hardware ID and location.  Note this information may be
filled in different ways by different hardware vendors for different classes, for
example, on the development PC:

COM1, Enabled=Y, Communications Port (COM1), (Standard port types), Serial0, ACPI\PNP0501,
COM3, Enabled=N, Prolific USB-to-Serial Comm Port (COM3), Prolific, , USB\VID_067B&PID_2303&REV_0400, Port_#0001.Hub_#0003
COM4, Enabled=Y, HHD Software Virtual Serial Port (COM4), HHD Software Ltd., 0000005e, HHDVIRT\HHDVySer,
COM5, Enabled=Y, HHD Software Virtual Serial Port (COM5), HHD Software Ltd., 0000005f, HHDVIRT\HHDVySer,
COM6, Enabled=Y, HHD Software Virtual Serial Port (COM6), HHD Software Ltd., 00000060, HHDVIRT\HHDVySer,
COM11, Enabled=Y, D-Link DU-562M External Modem, CXT, Winachsf0, USB\VID_0572&PID_1300&REV_0100, Port_#0007.Hub_#0004
COM12, Enabled=Y, Enhanced Communication Port (COM12), Oxford Semiconductor, OXMF0, OXMF\*PNP0501, oxmf bus, port 0
COM13, Enabled=Y, Enhanced Communication Port (COM13), Oxford Semiconductor, OXMF3, OXMF\*PNP0501, oxmf bus, port 3
COM14, Enabled=Y, Enhanced Communication Port (COM14), Oxford Semiconductor, OXMF2, OXMF\*PNP0501, oxmf bus, port 2
COM15, Enabled=Y, Enhanced Communication Port (COM15), Oxford Semiconductor, OXMF1, OXMF\*PNP0501, oxmf bus, port 1
COM16, Enabled=Y, Enhanced Communication Port (COM16), Oxford Semiconductor, OXMF6, OXMF\*PNP0501, oxmf bus, port 2
COM17, Enabled=Y, Enhanced Communication Port (COM17), Oxford Semiconductor, OXMF5, OXMF\*PNP0501, oxmf bus, port 1
COM18, Enabled=Y, Enhanced Communication Port (COM18), Oxford Semiconductor, OXMF4, OXMF\*PNP0501, oxmf bus, port 0
COM19, Enabled=Y, Enhanced Communication Port (COM19), Oxford Semiconductor, OXMF7, OXMF\*PNP0501, oxmf bus, port 3
COM20, Enabled=Y, Prolific USB-to-Serial Comm Port (COM20), Prolific, ProlificSerial1, USB\VID_067B&PID_2303&REV_0400, Port_#0002.Hub_#0003
COM24, Enabled=Y, Standard Serial over Bluetooth link (COM24), Microsoft, BthModem0, BTHENUM\00001101-0000-1000-8000-00805f9b34fb_VID&0001000f_PID&1200,
COM26, Enabled=Y, USB Serial Port (COM26), FTDI, VCP0, FTDIBUS\COMPORT&VID_0403&PID_6001,
COM27, Enabled=N, USB-SERIAL CH340 (COM27), wch.cn, , USB\VID_1A86&PID_7523&REV_0254, Port_#0001.Hub_#0003
COM31, Enabled=N, Prolific USB-to-Serial Comm Port (COM31), Prolific, , USB\VID_067B&PID_2303&REV_0400, Port_#0003.Hub_#0006
COM32, Enabled=Y, Prolific USB-to-Serial Comm Port (COM32), Prolific, ProlificSerial0, USB\VID_067B&PID_2303&REV_0400, Port_#0003.Hub_#0005
CNCA0, Enabled=Y, com0com - serial port emulator (CNCA0), Vyacheslav Frolov, com0com10, com0com\port, CNCA0
CNCB0, Enabled=Y, com0com - serial port emulator (CNCB0), Vyacheslav Frolov, com0com20, com0com\port, CNCB0

Under Windows 11 (note needs Prolific WDF WHQL Driver: v3.9.1.0):
COM1, Enabled=Y, Communications Port (COM1), (Standard port types), Serial0, ACPI\VEN_PNP&DEV_0501, 
COM2, Enabled=Y, PCIe to High Speed Serial Port (COM2), ASIX Electronics Corporation, StnSerial0, MCS9950MF\STN_CASCADE_COM, 
COM3, Enabled=Y, PCIe to High Speed Serial Port (COM3), ASIX Electronics Corporation, StnSerial1, MCS9950MF\STN_CASCADE_COM, 
COM4, Enabled=Y, Prolific USB-to-Serial Comm Port (COM4), Prolific, ProlificSerial0, USB\VID_067B&PID_2303&REV_0400, Port_#0004.Hub_#0007
COM5, Enabled=Y, Prolific USB-to-Serial Comm Port (COM5), Prolific, ProlificSerial1, USB\VID_067B&PID_2303&REV_0400, Port_#0001.Hub_#0007
COM6, Enabled=Y, Conexant USB CX93010 ACF Modem, Conexant, USBSER000, USB\VID_0572&PID_1329&REV_0100, Port_#0007.Hub_#0001
COM7, Enabled=Y, USB Serial Device (COM7), Microsoft, USBSER000, USB\VID_1546&PID_01A8&REV_0201, Port_#0002.Hub_#0007


Note some USB ports are unlugged, but have fixed COM numbers if replugged into the same USB port.
On this PC, COM3 to COM10 were originally another 8-way serial card since removed which is the
reason for the missing numbers. For the 8-way card, the internal name correctly describes the
labelled ports (0 to 7) which the location fails to do.

WARNING - Delphi 7 and later only, supports Delphi 2009 and later with Unicode

Note this unit needs three Jedi jwapi header files: setupapi, Cfg, CfgMgr32

}

interface
uses
  Windows, Messages, SysUtils, Classes, Registry, Magsubs1 ;

type

// serial COM ports  7 Dec 2007, 7 Apr 2014 more elements
 TSerialPort = record
    ComName: string ;
    NumPort: string ;
    IntName: string ;
    FriendlyName: string ;
    Desc: string ;
    Manufacturer: string ;
    HardwareId: string ;
    Location: string ;
    StatInfo: string ;
    Enabled: boolean ;  // USB ports may be configured but hidden and disabled
 end;
 TSerialPorts = array of TSerialPort ;

function EnumSerialPorts (var SerialPorts: TSerialPorts): integer;
function EnumSerialPortsEx (var SerialPorts: TSerialPorts; ShowDisabled: boolean = false): integer;
function ReportSerialPorts (SerialPorts: TSerialPorts): string;

implementation

uses setupapi, Cfg, CfgMgr32;  { Jedi jwapi headers }

var
  LoadAPIs: Boolean = False;

function RegPropertyStr (PnPHandle: HDEVINFO; const DevData: TSPDevInfoData; Prop: DWORD): String;
var
    BytesReturned: DWORD;
    RegDataType: DWORD;
    Buffer: array [0..1023] of Char;
begin
    BytesReturned := 0;
    RegDataType := 0;
    Buffer[0] := #0;
    SetupDiGetDeviceRegistryProperty (PnPHandle, DevData, Prop, RegDataType,
                                        PByte(@Buffer[0]), SizeOf(Buffer), BytesReturned);
    Result := Buffer;
end;

// simple serial port enumeration using registry only, may miss USB devices and modems

function EnumSerialPorts (var SerialPorts: TSerialPorts): integer;
var
    I, J, PortLen: Integer;
    SortStr, PortName: string;
    SortList, ValueList: TStringList;
    aRegistry : TRegistry;
begin
    result := 0 ;
    SetLength (SerialPorts, 0) ;
    aRegistry := TRegistry.Create;
    ValueList := TStringList.Create;
    SortList := TStringList.Create;
    try
    // 16 Feb 2014, total rewrite using registry functions since old version was
    // having trouble reading registry keys reliably causing some corrupted names
        try
            with aRegistry do
            begin
                RootKey := HKEY_LOCAL_MACHINE;
                Access := KEY_QUERY_VALUE ;
                OpenKey ('HARDWARE\DEVICEMAP\SERIALCOMM', False);
                GetValueNames (ValueList);
                if ValueList.Count = 0 then exit ;
                for I := 0 to ValueList.Count - 1 do
                begin
                    if ValueExists (ValueList [I]) then   // avoid exception if no value
                    begin
                        PortName := ReadString (ValueList [I]);
                        PortLen := Length (PortName) ;
                        if Pos ('COM', PortName) = 1 then
                        begin
                            SortStr := Copy (PortName, 4, 9) ;
                            if PortLen = 4 then
                                SortStr := '00' + SortStr
                            else if PortLen = 5 then
                                SortStr := '0' + SortStr ;
                        end
                        else
                            SortStr := 'xxx' ;
                        SortStr := SortStr + PortName + '|' ;
                        if Pos ('\Device\', ValueList [I]) = 1 then     // \Device\OXMF0, \Device\BthModem0
                            SortStr := SortStr + Copy (ValueList [I], 9, 99)
                        else
                            SortStr := SortStr + ValueList [I] ;       // Winachsf0
                        SortList.Add (SortStr) ;
                      end;
                end;
                CloseKey ;
            end;
            result := SortList.Count ;
            if result = 0 then exit ;
            SortList.Sort ;
            SetLength (SerialPorts, result) ;
            for I := 0 to (Pred (result)) do
            begin
                SortStr := SortList [I] ;
                J := Pos ('|', SortStr) ;
                serialPorts [I].ComName := Copy (SortStr, 4, J - 4) ;
                serialPorts [I].NumPort := Copy (SortStr, 1, 3) ;
                SerialPorts [I].IntName := Copy (SortStr, Succ (J), 99) ;
                SerialPorts [I].Desc := SerialPorts [I].IntName ;  // may get better name later
                SerialPorts [I].Enabled := true ;
            end;
        except
        end ;
    finally
        SortList.Free;
        ValueList.Free;
        aRegistry.Free;
  end;

end;

// extended erial port enumeration using registry and various Device Installation Class
// optionally includes ports that are disabled due the hardware not currently being available

function EnumSerialPortsEx (var SerialPorts: TSerialPorts; ShowDisabled: boolean = false): integer;
var
    NewPort: TSerialPort;

    procedure ArrayInsert (var S: TSerialPorts; Index: integer; T: TSerialPort;
                                                                    var Total: integer) ;
    var
        I: integer ;
    begin
        if Total > Length (S) then Total := Length (S) ;
        if Length (S) <= Total then SetLength (S, Succ (Total)) ;
        if index > Total then index := Total ;  // add at end if index too large
        if (index < Total) and (Total <> 0) then
        begin
            for I := Total downto Succ (index) do S [I] := S [Pred(I)] ;
        end ;
        S [index] := T ;
        inc (Total) ;
    end ;

    // find string in sorted array, returns position to insert if not found

    function ArrayFindSorted (const S: TSerialPorts; T: TSerialPort; var Index: longint;
                                                                Total: integer): Boolean;
    var
        I, res: integer ;
    begin
        result := false ;
        Index := 0 ;
        if Total > Length (S) then Total := Length (S) ;
        if Total = 0 then exit ;
      // pending - use binary chop sort for speed
        for I := 0 to pred (Total) do
        begin
            res := CompareStr (T.NumPort, S [I].NumPort) ;
            if res = 0 then
            begin
                result := true ;  // found OK
                break ;
            end ;
            if res < 0 then break ; // passed it
        end ;
        Index := I ;
    end ;

    function ArrayAddSorted (var S: TSerialPorts; T: TSerialPort; var Total: integer): boolean ;
    var
        Index: integer ;
    begin
        result := ArrayFindSorted (S, T, Index, Total) ;
        if result then exit ;
        ArrayInsert (S, Index, T, Total) ;
    end ;

   procedure GetDeviceClass (const ClassName: String; Disabled: boolean) ;
    var
        RequiredSize, MemberIndex, Flags, ulStatus, ulProblemNumber, Ret: Cardinal;
        Guid: TGUID;
        GUIDSize, Regtyp: DWORD;
        pnr: integer ;
        DevInfoHandle: HDEVINFO;
        DeviceInfoData: TSPDevInfoData;
        Key: Hkey;
        RegBuffer: array [0..33] of Char;
        cname: string ;

        function FindPort (const cname: string): integer;
        begin
            for result := 0 to Pred (Length (SerialPorts)) do
            begin
                if cname = SerialPorts [result].ComName then exit ;
            end;
            result := -1 ;
        end;

    begin
        GUIDSize := 1;    // Guid structure contains a single GUID
        if NOT SetupDiClassGuidsFromName (PChar (ClassName), @Guid, GUIDSize, RequiredSize) then exit;

    //get object handle of class to interate devices
        if Disabled then
            Flags := DIGCF_ALLCLASSES   // warning, does not get all present ports for some reason
        else
            Flags := DIGCF_PRESENT ;
        DevInfoHandle := SetupDiGetClassDevs (@Guid, Nil, 0, Flags);
        if Cardinal (DevInfoHandle) = Invalid_Handle_Value then exit;
        try
            MemberIndex := 0;
         //iterate device list
            repeat
                FillChar (DeviceInfoData, SizeOf (DeviceInfoData), 0);
                DeviceInfoData.cbSize := SizeOf (DeviceInfoData);
            //get device info that corresponds to the next memberindex
                if NOT SetupDiEnumDeviceInfo (DevInfoHandle, MemberIndex, DeviceInfoData) then break;  // done
                Key := SetupDiOpenDevRegKey (DevInfoHandle, DeviceInfoData,
                                                    DICS_FLAG_GLOBAL, 0, DIREG_DEV, KEY_READ);
                if Key <> INValid_Handle_Value then
                begin
               //query the real port name from the registry value 'PortName'
                    RequiredSize := 32 ;
                    Regtyp := REG_SZ;
                    if RegQueryValueEx (Key, 'PortName', Nil, @Regtyp, @RegBuffer[0] ,@RequiredSize) = Error_Success then
                    begin
                        cname := RegBuffer ;
                        if (Pos ('USB', cname) <> 1) and (Pos ('LPT', cname) <> 1) then  // skip printers
                        begin
                            pnr := FindPort (cname) ;
                            if pnr < 0 then  // found a port not in the registry list, add it here
                            begin
                                NewPort.ComName :=  cname;
                                if Pos ('COM', cname) = 1 then
                                begin
                                    NewPort.NumPort := Copy (cname, 4, 9) ;
                                    if Length (cname) = 4 then
                                        NewPort.NumPort := '00' + NewPort.NumPort
                                    else if Length (cname) = 5 then
                                        NewPort.NumPort := '0' + NewPort.NumPort ;
                                end
                                else
                                    NewPort.NumPort := 'xxx' ;
                                NewPort.IntName := '';
                                NewPort.Enabled := false;
                                ArrayAddSorted (SerialPorts, NewPort, result) ;
                                pnr := FindPort (cname) ;
                                if pnr < 0 then break ;
                            end;
                            if SerialPorts [pnr].FriendlyName = '' then
                            begin
                                SerialPorts [pnr].FriendlyName := RegPropertyStr
                                        (DevInfoHandle, DeviceInfoData, SPDRP_FRIENDLYNAME); // ie Communications Port (COM1)
                                SerialPorts [pnr].Desc := RegPropertyStr
                                        (DevInfoHandle, DeviceInfoData, SPDRP_DEVICEDESC); // ie Communications Port
                                SerialPorts [pnr].Manufacturer := RegPropertyStr
                                         (DevInfoHandle, DeviceInfoData, SPDRP_MFG); // ie Oxford Semiconductor
                                SerialPorts [pnr].HardwareId := RegPropertyStr
                                         (DevInfoHandle, DeviceInfoData, SPDRP_HARDWAREID); // multiple strings, ie USB\VID_0572&PID_1300&REV_0100
                                SerialPorts [pnr].Location := RegPropertyStr
                                         (DevInfoHandle, DeviceInfoData, SPDRP_LOCATION_INFORMATION ); // multiple strings
                            end ;
                            if (SerialPorts [pnr].FriendlyName = '') then
                            begin
                                if (SerialPorts [pnr].Desc  <> '') then  // if missing, try other names
                                   SerialPorts [pnr].FriendlyName := SerialPorts [pnr].Desc
                                else
                                    SerialPorts [pnr].FriendlyName := SerialPorts [pnr].IntName ;
                                SerialPorts [pnr].FriendlyName := SerialPorts [pnr].FriendlyName + ' (' +
                                                                                    SerialPorts [pnr].ComName + ')' ;
                            end;
                            Ret := CM_Get_DevNode_Status (ulStatus, ulProblemNumber, DeviceInfoData.DevInst, 0);
                            if Ret = CR_SUCCESS then
                            begin
                                SerialPorts [pnr].StatInfo := 'Flags=' + IntToHex (ulStatus, 8) +
                                                                 ' Problem=' + IntToStr (ulProblemNumber) ;
                                SerialPorts [pnr].Enabled := (DN_STARTED = (DN_STARTED and ulStatus));
                            end
                            else
                            begin
                                SerialPorts [pnr].StatInfo := 'Error=' + IntToStr (Ret) ;
                                SerialPorts [pnr].Enabled := false;
                            end;
                        end;
                    end;
                    RegCloseKey (key);
                end;
                inc (MemberIndex);
            until False;
        finally
            SetupDiDestroyDeviceInfoList (DevInfoHandle);
       end;
    end;

begin
    SetLength (SerialPorts, 0) ;
    result := EnumSerialPorts (SerialPorts);  // get ports from registry first, sorted
    GetDeviceClass ('Ports', false);     // most serial ports
    if ShowDisabled then GetDeviceClass ('Ports', true);  // may not get as many ports
    GetDeviceClass ('CNCPorts', false);  // com0com - serial port emulator
    GetDeviceClass ('Modem', false);     // USB and internal modems with ports
    SetLength (SerialPorts, result);
end;

// return multiline textual listing of serial ports

function ReportSerialPorts (SerialPorts: TSerialPorts): string;
var
    I, newtot: integer;
begin
    result := '' ;
    newtot := Length (SerialPorts) ;
    If newtot > 0 then
    begin
        for I := 0 to Pred (newtot) do
        begin
            result := result + SerialPorts [I].ComName + ', Enabled=' + GetYN (SerialPorts [I].Enabled) +
                 ', ' + SerialPorts [I].FriendlyName + ', ' + SerialPorts [I].Manufacturer + ', ' +
                 SerialPorts [I].IntName + ', ' + SerialPorts [I].HardwareId + ', ' +
                 SerialPorts [I].Location + CRLF_ ;
        end ;
    end
    else
        result := 'None' ;
end;


initialization

if not LoadAPIs then
begin
    LoadSetupApi;
    LoadConfigManagerApi;
    LoadAPIs := true;
end;

finalization

if LoadAPIs then
begin
    UnloadSetupApi;
    UnloadConfigManagerApi;
end;

end.
