unit magwmi;
{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}

{
Magenta Systems WMI and SMART Component v5.5
Updated by Angus Robertson, Magenta Systems Ltd, England, 26th November 2018
delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/
Copyright 2018, Magenta Systems Ltd

partly based on articles in Delphi Developers Magazine by Craig Murphy and work by
Denis Blondeau (both needed the same bug fixes to read variant arrays properly)

note - WbemScripting_TLB was created by importing the Microsoft WMI Scripting type library

This component contains WMI, SMART and SCSI Pass Through functions, of particular use
for getting hard disk information and configuring network adaptors, but also for many
other general uses.

24 Nov 2003 - Release 4.93
26 Nov 2003 - removed some duplicate subs
1 Dec 2003  - added MagWmiGetOSInfo and MagWmiGetProcInfo
19 Dec 2003 - added MagWmiNetSetDHCP
2 Jan 2004  - using common subs and types from magsubs1
10 Jan 2004 - added SMART stuff to read hard disk serial which WMI only does for XP and later
15 Jul 2004 - use SMART stuff to get disk failure info and attributes
14 Oct 2004 - added MagWmiScsiDiskInfo seems to work with firewire drives SMART dislikes (but not USB)
21 Oct 2004 - more error handling in MagWmiSmartDiskFail looking for range error, AttrRawValue now Int64
9  Jan 2005 - added MagWmiCloseWin to close down windows
14 Jan 2005 - added MagWmiGetMemory
29 Jul 2008 - 5.1 - removed widestrings for better compability with unicode in Delphi 2009
            - using PAnsiChars and Bytes where necessary
5 Mar 2009  - 5.2 - fixed memory leaks with OleVariants, thanks to Andy Whittles and Luke Painter
              added MagWmiGetInfoEx which returns exception error string as well as -1
9 Aug 2010  - 5.3 - fixed some string cast warnings for Delphi 2009 and later
23 Jan 2013 - 5.4 - old SMART APIs were designed for PCs with only four IDE PATA drives and
                    drives less than 128 gig, whereas SATA PCs may have six or more drives
              removed support for Win95/98, ignore bIDEDeviceMap with maximum four drives
              support more than four drives in MagWmiSmartDiskInfo and MagWmiSmartDiskFail
              all drive info returned in TDriveResult structure
              added MagWmiMapDrives which returns details of all disk drives in array of  TDriveResult structures
              removed MagWmiScsiDiskInfo now part of MagWmiSmartDiskInfo
              added MagWmiSmartScsiBus which returns details of all devices on SCSI buses, which may be
                 disks, DVD-ROMs, etc, including all ATA devices, not currently supporting SMART on these devices
5 Aug 2013  - 5.5 - fix another WMI memory leak with OleVariants, thanks to Ekkehard Domning and anon



pending - get SMART data via Intel RAID controller using CSMI commands
pending - use SCSI passthrough commands for SMART data
}


{ an alternate name space has other stuff:

root\wmi
MSNdis_HardwareStatus
MSNdis_80211_TransmitPowerLevel
MSNdis_80211_ReceivedSignalStrength
MSNdis_MediaConnectStatus
MSTapeDriveParam
MSRedbook_DriverInformation
MSSerial_PortName
MSStorageDriver_FailurePredictStatus
MSStorageDriver_ATAPISmartData
}

interface

uses
  Windows, Messages, SysUtils, Classes, WbemScripting_TLB,
  magsubs1, smartapi {, csmisas} ;


const
    RootNameSpace = 'root\CIMV2' ;
    MaxSmartAttr = NUM_ATTRIBUTE_STRUCTS + 1 ;

type
    TDriveResult = Record
        DriveNum: integer ;
        DeviceId: string ;
        BusTypeDisp: string ; // ATA. SCSI, USB, CSMI
        DevTypeDisp: string ; // disk, cd-rom, etc,
        ErrInfo: string ;
        VendorId: string ;
        ProductId: string ;
        ProductRev: string ;
        BusType: integer ;
        DeviceType: integer ;
        MediaType: integer ;
        VendorUnique: array [1..4] of USHORT ;
        SerialNumber: string ;
        FirmwareRev: string ;
        ModelNumber: string ;
        RemoveMedia: boolean ;
        SmartSupport: boolean ;
        SmartEnabled: boolean ;
        LBA48Support: boolean ;
        CapacityNum: int64 ;
        SectorNum: int64 ;
        MajorRev: integer ;    // ATA versions
        MinorRev: integer ;    // minor ATA versions
        AtaVersion: string ;
        SataVersion: string ; 
        SecSizeLogical: integer ;
        SecSizePhysical: integer ;
        IdentifyDevice: TIdentifyDeviceData ;
    end ;
    TDriveResults = array of TDriveResult ;

    TSmartResult = Record
        DriveNum: integer ;
        Temperature: integer ;
        TempWorst: integer ;
        TempLow: integer ;
        HoursRunning: integer ;
        ReallocSector: integer ;
        PowerCycles: integer ;
        SmartFailTot: integer ;
        SmartWarnTot: integer ;
        TotalAttrs: integer ;
        AttrNum: array [0..MaxSmartAttr] of integer ;
        AttrName: array [0..MaxSmartAttr] of string ;
        AttrPreFail: array [0..MaxSmartAttr] of boolean ;
        AttrEvents: array [0..MaxSmartAttr] of boolean ;
        AttrErrorRate: array [0..MaxSmartAttr] of boolean ;
        AttrCurValue: array [0..MaxSmartAttr] of integer ;
        AttrWorstVal: array [0..MaxSmartAttr] of integer ;
        AttrThreshold: array [0..MaxSmartAttr] of integer ;
        AttrRawValue: array [0..MaxSmartAttr] of Int64 ;   // 21 Oct 2004, was int
        AttrState: array [0..MaxSmartAttr] of string ;
    end ;

    TWmiMemoryRec = Record
        FreePhysicalMemory: Int64 ;
        FreeSpaceInPagingFiles: Int64 ;
        FreeVirtualMemory: Int64 ;
        SizeStoredInPagingFiles: Int64 ;
        TotalSwapSpaceSize: Int64 ;
        TotalVirtualMemorySize: Int64 ;
        TotalVisibleMemorySize: Int64 ;
    end ;

    function MagWmiDate2DT (S: string; var UtcOffset: integer): TDateTime ;
    function MagWmiGetPropStr (wmiProp: ISWbemProperty): string ;
    function MagWmiGetInfoEx (const Comp, NameSpace, User, Pass, Arg: string ;
       var WmiResults: T2DimStrArray; var instances: integer; var errinfo: string): integer ;
    function MagWmiGetInfo (const Comp, NameSpace, User, Pass, Arg: string ;
                       var WmiResults: T2DimStrArray; var instances: integer): integer ;
    function MagWmiGetOneG (const Arg, Prop: string ; var ResStr: string): integer ;
    function MagWmiGetOneQ (const Arg, Prop: string ; var ResStr: string): integer ;
    function MagWmiSearchIdx (const WmiResults: T2DimStrArray; const Prop: string): integer ;
    function MagWmiSearch1 (const WmiResults: T2DimStrArray; const Prop: string): string ;
    function MagWmiGetBaseBoard: string ;
    function MagWmiGetSMBIOS: string ;
    function MagWmiGetLastBootDT: TDateTime ;
    function MagWmiGetDiskSerial (drive: integer): string ;
    function MagWmiGetDiskModel (drive: integer): string ;
    function MagWmiFindAdaptor (var AdapterName: string): integer ;
    function MagWmiNetSetIPAddr (const AdapNum: integer; const IPAddresses,
                                                SubnetMasks: StringArray): integer ;
    function MagWmiNetSetGateway (const AdapNum: integer; const IPGateways: StringArray;
                                           const GatewayCosts: TIntegerArray): integer ;
    function MagWmiNetSetDHCP (const AdapNum: integer): integer ;
    function MagWmiRenameComp (const NewName, UserName, Password: string): integer ;
    function MagWmiGetOSInfo (item: string): string ;
    function MagWmiGetProcInfo (item: string): string ;
    function MagWmiSmartDiskInfo (drivenr: integer; const deviceid: string; var DriveResult: TDriveResult): boolean ;
    function MagWmiSmartDiskFail (drivenr: integer; var DriveResult: TDriveResult;
                                                        var SmartResult: TSmartResult): boolean ;
    function MagWmiCloseWin (const Comp, User, Pass: string ; reboot: boolean;
                                                var errinfo: string): integer ;
    function MagWmiGetMemory: TWmiMemoryRec ;
    function MagWmiMapDrives (MaxNum: integer; var DriveResults: TDriveResults): integer ;
    function MagWmiSmartScsiBus (MaxNum: integer; var DriveResults: TDriveResults): integer ;


implementation

uses ActiveX, ComObj, Variants;

(* ************************************************************************* *)

function MagWmiDate2DT (S: string; var UtcOffset: integer): TDateTime ;
// yyyymmddhhnnss.zzzzzzsUUU  +60 means 60 mins of UTC time
// 20030709091030.686000+060
// 1234567890123456789012345
var
    yy, mm, dd, hh, nn, ss, zz: integer ;
    timeDT: TDateTime ;

    function GetNum (offset, len: integer): integer ;
    var
        E: Integer;
    begin
        Val (copy (S, offset, len), result, E) ;
    end ;

begin
    result := 0 ;
    UtcOffset := 0 ;
    if length (S) <> 25 then exit ;   // fixed length
    yy := GetNum (1, 4) ;
    mm := GetNum (5, 2) ;
    if (mm = 0) or (mm > 12) then exit ;
    dd := GetNum (7, 2) ;
    if (dd = 0) or (dd > 31) then exit ;
    if NOT TryEncodeDate (yy, mm, dd, result) then     // D6 and later
    begin
        result := -1 ;
        exit ;
    end ;
  { try
        result := EncodeDate (yy, mm, dd) ;
    except
        result := -1 ;
        exit ;
    end ;    }
    hh := GetNum (9, 2) ;
    nn := GetNum (11, 2) ;
    ss := GetNum (13, 2) ;
    zz := 0 ;
    if Length (S) >= 18 then zz := GetNum (16, 3) ;
    if NOT TryEncodeTime (hh, nn, ss, zz, timeDT) then exit ;   // D6 and later
    result := result + timeDT ;
    UtcOffset := GetNum (22, 4) ; // including sign
 {   try
        result := result + EncodeTime (hh, nn, ss, zz) ;
    except
        result := -1 ;
        exit ;
    end ; }
end ;

(* ************************************************************************* *)

function MagWmiGetPropStr (wmiProp: ISWbemProperty): string ;
var
    I: integer ;
begin
    result := '';
    if VarIsNull(wmiProp.Get_Value) then
        result := 'NULL'
    else
    begin
        case wmiProp.CIMType of
            wbemCimtypeSint8, wbemCimtypeUint8, wbemCimtypeSint16,
            wbemCimtypeUint16, wbemCimtypeSint32, wbemCimtypeUint32,
            wbemCimtypeSint64:
                if VarIsArray(wmiProp.Get_Value) then
                begin
                    for I := 0 to VarArrayHighBound (wmiProp.Get_Value, 1) do
                    begin
                        if I > 0 then result := result + '|' ;
                        result := result + IntToStr (wmiProp.Get_Value [I]) ;
                    end ;
                end
                else
                    result := IntToStr (wmiProp.Get_Value);

            wbemCimtypeReal32, wbemCimtypeReal64:
                result := FloatToStr (wmiProp.Get_Value);

            wbemCimtypeBoolean:
                if wmiProp.Get_Value then result := 'True' else result := 'False';

            wbemCimtypeString, wbemCimtypeUint64:
                if VarIsArray(wmiProp.Get_Value) then
                begin
                    for I := 0 to VarArrayHighBound (wmiProp.Get_Value, 1) do
                    begin
                        if I > 0 then result := result + '|' ;
                        result := result + wmiProp.Get_Value [I] ;
                    end ;
                end
                else
                    result := wmiProp.Get_Value;

            wbemCimtypeDatetime:
                result := wmiProp.Get_Value;

            wbemCimtypeReference:
            begin
                result := wmiProp.Get_Value ;
            // Services.Get(result, 0, nil).GetObjectText_(0));  another query
            end;

            wbemCimtypeChar16:
                result := '<16-bit character>';

            wbemCimtypeObject:
                result := '<CIM Object>';
        end ;
    end;
end ;

(* ************************************************************************* *)

// Comp may be blank for local computer, user and pass optional
// results returned in two dimensioned array, properties as instance/column 0
// Instances is 1 or greater, result is number of rows, 0 for none, -1 for error
// The size of the dynamic array may also be checked with Low/High
// 5 March 2009 - Ex version returns exception message

function MagWmiGetInfoEx (const Comp, NameSpace, User, Pass, Arg: string ;
      var WmiResults: T2DimStrArray; var instances: integer; var errinfo: string): integer ;
var
    wmiLocator: TSWbemLocator;
    wmiServices: ISWbemServices;
    wmiObjectSet: ISWbemObjectSet;
    wmiObject: ISWbemObject;
    propSet: ISWbemPropertySet;
    wmiProp: ISWbemProperty;
    propEnum, Enum: IEnumVariant;
    ovVar1, ovVar2: OleVariant;  // 5.2
    lwValue: LongWord;
    sValue: String;
    inst, row: integer ;
    dimmed: boolean ;
begin
    result := 0 ;
    errinfo := '' ;
    Instances := 0 ;
    SetLength (WmiResults, 0, 0) ;
    dimmed := false ;
    VarClear (ovVar1) ;   // 5.2
    VarClear (ovVar2) ;   // 5.2
    wmiLocator := TSWbemLocator.Create (Nil) ;
    try
    try
        wmiServices := wmiLocator.ConnectServer (Comp, Namespace, User, Pass,
                                                                        '', '', 0, nil) ;
        if Pos ('SELECT', Arg) = 1 then
            wmiObjectSet := wmiServices.ExecQuery (Arg, 'WQL', wbemFlagReturnImmediately, nil)
        else
            wmiObjectSet := wmiServices.InstancesOf (Arg, wbemFlagReturnImmediately or
                                                             wbemQueryFlagShallow, nil) ;
        Instances := wmiObjectSet.Count ;
        if Instances = 0 then exit ;

        // Replicate VBScript's "for each" construct
        Enum := (wmiObjectSet._NewEnum) as IEnumVariant;
        inst := 0 ;
        while (Enum.Next (1, ovVar1, lwValue) = S_OK) do    // 5.2
        begin
            wmiObject := IUnknown(ovVar1) as SWBemObject;   // 5.2
            propSet := wmiObject.Properties_;
            result := propSet.Count ;
            if NOT dimmed then
            begin
                SetLength (WmiResults, Instances + 1, result + 1) ;
                WmiResults [0, 0] := 'Instance' ;
                dimmed := true ;
            end ;
            propEnum := (propSet._NewEnum) as IEnumVariant;
            inc (inst) ;
            row := 1 ;
            WmiResults [inst, 0] := IntToStr (inst) ;

       // Replicate VBScript's "for each" construct
            while (propEnum.Next (1, ovVar2, lwValue) = S_OK) do  // 5.2
            begin
                wmiProp := IUnknown(ovVar2) as SWBemProperty;     // 5.2
                sValue := MagWmiGetPropStr (wmiProp) ;
                if inst = 1 then WmiResults [0, row] := wmiProp.Name ;
                WmiResults [inst, row] := sValue ;
                inc (row) ;
                VarClear (ovVar2); // 5.2 whomp them mem leaks
            end;
            VarClear (ovVar1); // 5.5 clear for each interation, not just once
        end;
    except
        VarClear (ovVar1) ;   // 5.2
        VarClear (ovVar2) ;   // 5.2
        result := -1 ;
        errinfo := GetExceptMess (ExceptObject) ;   // 5.2
    end ;
    finally
        wmiLocator.Free;
    end;
end;

(* ************************************************************************* *)

// Comp may be blank for local computer, user and pass optional
// results returned in two dimensioned array, properties as instance/column 0
// Instances is 1 or greater, result is number of rows, 0 for none, -1 for error
// The size of the dynamic array may also be checked with Low/High

function MagWmiGetInfo (const Comp, NameSpace, User, Pass, Arg: string ;
                   var WmiResults: T2DimStrArray; var Instances: integer): integer ;
var
    errinfo: string ;
begin
    result := MagWmiGetInfoEx (Comp, NameSpace, User, Pass, Arg,
                                                WmiResults, instances, errinfo) ;
end ;

(* ************************************************************************* *)

// get a single property, argument is Get which seems very unprectictable

function MagWmiGetOneG (const Arg, Prop: string ; var ResStr: string): integer ;
var
    wmiLocator: TSWbemLocator;
    wmiServices: ISWbemServices;
    wmiObject: ISWbemObject;
    wmiProp: ISWbemProperty;
begin
    result := 0 ;
    ResStr := '' ;
    wmiLocator := TSWbemLocator.Create (Nil);
    try
    try
        wmiServices := wmiLocator.ConnectServer ('', RootNameSpace, '', '',
                                                                        '', '', 0, nil);
        wmiObject := wmiServices.Get (Arg, 0, Nil) ;
        wmiProp := wmiObject.Properties_.Item (Prop, 0) ;
        if wmiProp.Name <> Prop then exit ;
        ResStr := MagWmiGetPropStr (wmiProp) ;
        if ResStr <> 'NULL' then result := 1 ;
    except
        result := -1 ;
    end ;
    finally
        wmiLocator.Free;
    end;
end ;

(* ************************************************************************* *)

// get a single property, argument is a WQL query, lengthy, but reliable
// fails if more than one instance returned by query

function MagWmiGetOneQ (const Arg, Prop: string ; var ResStr: string): integer ;
var
    wmiLocator: TSWbemLocator;
    wmiServices: ISWbemServices;
    wmiObjectSet: ISWbemObjectSet;
    wmiObject: ISWbemObject;
    wmiProp: ISWbemProperty;
    ovVar: OleVariant;
    lwValue: LongWord;
    Enum: IEnumVariant;
begin
    ResStr := '' ;
    VarClear (ovVar); // 5.2
    wmiLocator := TSWbemLocator.Create (Nil);
    try
    try
        wmiServices := wmiLocator.ConnectServer ('', RootNameSpace, '', '',
                                                                      '', '', 0, nil);
        wmiObjectSet := wmiServices.ExecQuery (Arg, 'WQL',
                                                     wbemFlagReturnImmediately, nil) ;
        result := wmiObjectSet.Count ;
        if (result <> 1) then exit ;  // can only handle a single instance
        Enum := (wmiObjectSet._NewEnum) as IEnumVariant;
        while (Enum.Next (1, ovVar, lwValue) = S_OK) do
        begin
            wmiObject := IUnknown(ovVar) as SWBemObject;
            wmiProp := wmiObject.Properties_.Item (Prop, 0) ;
            if wmiProp.Name = Prop then
            begin
                ResStr := MagWmiGetPropStr (wmiProp) ;
                if ResStr <> 'NULL' then result := 1 ;
            end ;
            VarClear (ovVar); // 5.2
        end ;
    except
        result := -1 ;
        VarClear (ovVar); // 5.2
    end ;
    finally
        wmiLocator.Free;
    end;
end ;

(* ************************************************************************* *)

function MagWmiSearchIdx (const WmiResults: T2DimStrArray; const Prop: string): integer ;
var
    I: integer ;
begin
    result := 0 ;
    for I := 1 to High (WmiResults [0]) do
    begin
        if WmiResults [0, I] = Prop then
        begin
            result := I ;
            exit ;
        end ;
    end ;
end ;

(* ************************************************************************* *)

function MagWmiSearch1 (const WmiResults: T2DimStrArray; const Prop: string): string ;
var
    I: integer ;
begin
    result := '' ;
    I := MagWmiSearchIdx (WmiResults, Prop) ;
    if I >= 1 then result := WmiResults [1, I] ;
end ;

(* ************************************************************************* *)

function MagWmiGetBaseBoard: string ;
var
    rows, instances: integer ;
    WmiResults: T2DimStrArray ;
begin
    result := '' ;
    try
        rows := MagWmiGetInfo ('', RootNameSpace, '', '',
            'SELECT Manufacturer, Product FROM Win32_BaseBoard', WmiResults, instances) ;
        if (instances = 1) and (rows > 1) then
            result := MagWmiSearch1 (WmiResults, 'Manufacturer') + ' ' +
                                                MagWmiSearch1 (WmiResults, 'Product') ;
    finally
        WmiResults := Nil ;
    end ;
end ;

(* ************************************************************************* *)

function MagWmiGetSMBIOS: string ;
var
    rows, instances: integer ;
    WmiResults: T2DimStrArray ;
begin
    result := '' ;
    try
        rows := MagWmiGetInfo ('', RootNameSpace, '', '', 'Win32_BIOS',
                                                                 WmiResults, instances) ;
        if (instances = 1) and (rows > 1) then
            result := MagWmiSearch1 (WmiResults, 'SMBIOSBIOSVersion') + ' v' +
                        MagWmiSearch1 (WmiResults, 'SMBIOSMajorVersion') + '.' +
                               MagWmiSearch1 (WmiResults, 'SMBIOSMinorVersion') ;
    finally
        WmiResults := Nil ;
    end ;
end ;

(* ************************************************************************* *)

function MagWmiGetLastBootDT: TDateTime ;
var
    rawdate: string ;
    utcoffset: integer ;
begin
    result := 0 ;
    if MagWmiGetOneQ ('SELECT LastBootUpTime FROM Win32_OperatingSystem',
                                         'LastBootUpTime', rawdate) <> 1 then exit ;
    result := MagWmiDate2DT (rawdate, utcoffset) ;
end ;

(* ************************************************************************* *)

// XP and W2K3 only !!!

function MagWmiGetDiskSerial (drive: integer): string ;
begin
    result := '' ;
    MagWmiGetOneQ ('SELECT SerialNumber FROM Win32_PhysicalMedia WHERE ' +
       'Tag = "\\\\.\\PHYSICALDRIVE' + IntToStr (drive) + '"', 'SerialNumber', Result) ;
end ;

(* ************************************************************************* *)

function MagWmiGetDiskModel (drive: integer): string ;
begin
    result := '' ;
    MagWmiGetOneQ ('SELECT Model FROM Win32_DiskDrive WHERE ' +
       'Name = "\\\\.\\PHYSICALDRIVE' + IntToStr (drive) + '"', 'Model', Result) ;
end ;

(* ************************************************************************* *)

function MagWmiGetProcInfo (item: string): string ;
begin
    result := '' ;
    MagWmiGetOneQ ('SELECT ' + item + ' FROM Win32_Processor', item, Result) ;
end ;

(* ************************************************************************* *)

function MagWmiGetOSInfo (item: string): string ;
begin
    result := '' ;
    MagWmiGetOneQ ('SELECT ' + item + ' FROM Win32_OperatingSystem', item, Result) ;
end ;

(* ************************************************************************* *)

// find the name and index of a unique Ethernet 802.3 adaptor (except 1394 Net Adapter)
// there may be other hidden adapters but they have null AdapterTypes (luckily)

function MagWmiFindAdaptor (var AdapterName: string): integer ;
var
    I, rows, instances: integer ;
    WmiResults: T2DimStrArray ;
begin
    result := -1 ;
    AdapterName := '' ;
    try
        rows := MagWmiGetInfo ('', RootNameSpace, '', '',
            'SELECT Name, Index FROM Win32_NetworkAdapter ' +
               'WHERE AdapterType = "Ethernet 802.3" AND Name <> "1394 Net Adapter"',
                                                                 WmiResults, instances) ;
        if (instances = 1) and (rows > 1) then
        begin
            I := MagWmiSearchIdx (WmiResults, 'Name') ;
            if I >= 1 then AdapterName := WmiResults [1, I] ;
            I := MagWmiSearchIdx (WmiResults, 'Index') ;
            if I >= 1 then result := AscToInt (WmiResults [1, I]) ;
        end ;
    finally
        WmiResults := Nil ;
    end ;
end ;

(* ************************************************************************* *)

// change network adaptor static IP addresses and masks
// this disables DHCP, multiple IP addresses may be supplied, with matching masks

function MagWmiNetSetIPAddr (const AdapNum: integer; const IPAddresses,
                                                SubnetMasks: StringArray): integer ;
var
    wmiLocator: TSWbemLocator;
    wmiServices: ISWbemServices;
    wmiObjectSet: ISWbemObjectSet;
    wmiObject, wmiInst, wmiInParams, wmiOutParams: ISWbemObject;
    wmiProp: ISWbemProperty;
    ovVar, propValue: OleVariant;
    Enum: IEnumVariant;
    wmiMethod: SWbemMethod;
    wmiProperty: SWbemProperty;
    Arg: string ;
    lwValue: LongWord;
    ArrayValue: Variant;
begin
    Result := -1 ;
    VarClear (ovVar); // 5.2
    wmiLocator := TSWbemLocator.Create (Nil);
    try
    try
        wmiServices := wmiLocator.ConnectServer ('', RootNameSpace, '', '',
                                                                      '', '', 0, nil);
        Arg := 'SELECT * FROM Win32_NetworkAdapterConfiguration WHERE Index = "' +
                                                             IntToStr (AdapNum) + '"' ;
        wmiObjectSet := wmiServices.ExecQuery (Arg, 'WQL',
                                                     wbemFlagReturnImmediately, nil) ;
        if (wmiObjectSet.Count <> 1) then exit ;  // can only handle a single instance
        Enum := (wmiObjectSet._NewEnum) as IEnumVariant;
        while (Enum.Next (1, ovVar, lwValue) = S_OK) do
        begin
            wmiObject := IUnknown(ovVar) as SWBemObject;
            wmiMethod := wmiObject.Methods_.Item ('EnableStatic', 0) ;
            wmiInParams := wmiMethod.InParameters ;
            wmiInst := wmiInParams.SpawnInstance_(0) ;
            wmiProperty := wmiInst.Properties_.Add ('IPAddress', wbemCimtypeString, True, 0) ;
            DynArrayToVariant (ArrayValue, IPAddresses, TypeInfo (StringArray)) ;
            propValue := ArrayValue ;
            wmiProperty.Set_Value (propValue);
            wmiProperty := wmiInst.Properties_.Add ('SubnetMask', wbemCimtypeString, True, 0) ;
            DynArrayToVariant (ArrayValue, SubnetMasks, TypeInfo (StringArray)) ;
            propValue := ArrayValue ;
            wmiProperty.Set_Value (propValue);
            wmiOutParams := wmiObject.ExecMethod_ ('EnableStatic', wmiInst, 0, nil) ;
            wmiProp := wmiOutParams.Properties_.Item ('ReturnValue', 0) ;
            VarClear (ovVar); // 5.2
            VarClear (propValue); // 5.2
            result := wmiProp.Get_Value ;
         // 0=OK no reboot, 1=ok reboot, 64 to 100 various errors, see MSDN
         // 68 = bad input parameter, 84=IP not enabled (ie wrong adaptor)
            exit ;
        end ;
    except
        result := -1 ;
        VarClear (ovVar); // 5.2
        VarClear (propValue); // 5.2
    end ;
    finally
        wmiLocator.Free;
    end;
end ;

(* ************************************************************************* *)

// change network adaptor static gateway IP addresses and metric
// this only works if DHCP is disabled, multiple IP addresses may be supplied

function MagWmiNetSetGateway (const AdapNum: integer; const IPGateways: StringArray;
                                          const GatewayCosts: TIntegerArray): integer ;
var
    wmiLocator: TSWbemLocator;
    wmiServices: ISWbemServices;
    wmiObjectSet: ISWbemObjectSet;
    wmiObject, wmiInst, wmiInParams, wmiOutParams: ISWbemObject;
    wmiProp: ISWbemProperty;
    ovVar, propValue: OleVariant;
    Enum: IEnumVariant;
    wmiMethod: SWbemMethod;
    wmiProperty: SWbemProperty;
    Arg: string ;
    lwValue: LongWord;
    ArrayValue: Variant;
begin
    Result := -1 ;
    wmiLocator := TSWbemLocator.Create (Nil);
    try
    try
        wmiServices := wmiLocator.ConnectServer ('', RootNameSpace, '', '',
                                                                      '', '', 0, nil);
        Arg := 'SELECT * FROM Win32_NetworkAdapterConfiguration WHERE Index = "' +
                                                             IntToStr (AdapNum) + '"' ;
        wmiObjectSet := wmiServices.ExecQuery (Arg, 'WQL',
                                                     wbemFlagReturnImmediately, nil) ;
        if (wmiObjectSet.Count <> 1) then exit ;  // can only handle a single instance
        Enum := (wmiObjectSet._NewEnum) as IEnumVariant;
        while (Enum.Next (1, ovVar, lwValue) = S_OK) do
        begin
            wmiObject := IUnknown(ovVar) as SWBemObject;
            wmiMethod := wmiObject.Methods_.Item ('SetGateways', 0) ;
            wmiInParams := wmiMethod.InParameters ;
            wmiInst := wmiInParams.SpawnInstance_(0) ;
            wmiProperty := wmiInst.Properties_.Add ('DefaultIPGateway', wbemCimtypeString, True, 0) ;
            DynArrayToVariant (ArrayValue, IPGateways, TypeInfo (StringArray)) ;
            propValue := ArrayValue ;
            wmiProperty.Set_Value (propValue);
            wmiProperty := wmiInst.Properties_.Add ('GatewayCostMetric', wbemCimtypeUint16, True, 0) ;
            DynArrayToVariant (ArrayValue, GatewayCosts, TypeInfo (TIntegerArray)) ;
            propValue := ArrayValue ;
            wmiProperty.Set_Value (propValue);
            wmiOutParams := wmiObject.ExecMethod_ ('SetGateways', wmiInst, 0, nil) ;
            wmiProp := wmiOutParams.Properties_.Item ('ReturnValue', 0) ;
            VarClear (ovVar); // 5.2
            VarClear (propValue); // 5.2
            result := wmiProp.Get_Value ;
         // 0=OK no reboot, 1=ok reboot, 64 to 100 various errors, see MSDN
         // 68 = bad input parameter, 84=IP not enabled (ie wrong adaptor)
            exit ;
        end ;
    except
        result := -1 ;
        VarClear (ovVar); // 5.2
        VarClear (propValue); // 5.2
    end ;
    finally
        wmiLocator.Free;
    end;
end ;

(* ************************************************************************* *)

// change network adaptor to enable DHCP

function MagWmiNetSetDHCP (const AdapNum: integer): integer ;
var
    wmiLocator: TSWbemLocator;
    wmiServices: ISWbemServices;
    wmiObjectSet: ISWbemObjectSet;
    wmiObject, wmiOutParams: ISWbemObject;
    wmiProp: ISWbemProperty;
    ovVar: OleVariant;
    Enum: IEnumVariant;
    wmiMethod: SWbemMethod;
    Arg: string ;
    lwValue: LongWord;
begin
    Result := -1 ;
    wmiLocator := TSWbemLocator.Create (Nil);
    try
    try
        wmiServices := wmiLocator.ConnectServer ('', RootNameSpace, '', '',
                                                                      '', '', 0, nil);
        Arg := 'SELECT * FROM Win32_NetworkAdapterConfiguration WHERE Index = "' +
                                                             IntToStr (AdapNum) + '"' ;
        wmiObjectSet := wmiServices.ExecQuery (Arg, 'WQL',
                                                     wbemFlagReturnImmediately, nil) ;
        if (wmiObjectSet.Count <> 1) then exit ;  // can only handle a single instance
        Enum := (wmiObjectSet._NewEnum) as IEnumVariant;
        while (Enum.Next (1, ovVar, lwValue) = S_OK) do
        begin
            wmiObject := IUnknown(ovVar) as SWBemObject;
            wmiMethod := wmiObject.Methods_.Item ('EnableDHCP', 0) ;
            wmiOutParams := wmiObject.ExecMethod_ ('EnableDHCP', Nil, 0, nil) ;
            wmiProp := wmiOutParams.Properties_.Item ('ReturnValue', 0) ;
            VarClear (ovVar); // 5.2
            result := wmiProp.Get_Value ;
         // 0=OK no reboot, 1=ok reboot, 64 to 100 various errors, see MSDN
         // 68 = bad input parameter, 84=IP not enabled (ie wrong adaptor)
            exit ;
        end ;
    except
        result := -1 ;
        VarClear (ovVar); // 5.2
    end ;
    finally
        wmiLocator.Free;
    end;
end ;

(* ************************************************************************* *)

// rename Computer Name, always needs reboot - account info is for domain controller
// XP and W2K3 only

function MagWmiRenameComp (const NewName, UserName, Password: string): integer ;
var
    wmiLocator: TSWbemLocator;
    wmiServices: ISWbemServices;
    wmiObjectSet: ISWbemObjectSet;
    wmiObject, wmiInst, wmiInParams, wmiOutParams: ISWbemObject;
    wmiProp: ISWbemProperty;
    ovVar, propValue: OleVariant;
    Enum: IEnumVariant;
    wmiMethod: SWbemMethod;
    wmiProperty: SWbemProperty;
    lwValue: LongWord;
begin
    Result := -1 ;
    if NewName = '' then exit ;
 // should validate new name for no control chars, spaces, / \ [ ]
    wmiLocator := TSWbemLocator.Create (Nil);
    try
    try
        wmiServices := wmiLocator.ConnectServer ('', RootNameSpace, '', '',
                                                                      '', '', 0, nil);
        wmiObjectSet := wmiServices.InstancesOf ('Win32_ComputerSystem',
                             wbemFlagReturnImmediately or wbemQueryFlagShallow, nil) ;
        if (wmiObjectSet.Count <> 1) then exit ;  // can only handle a single instance
        Enum := (wmiObjectSet._NewEnum) as IEnumVariant;
        while (Enum.Next (1, ovVar, lwValue) = S_OK) do
        begin
            wmiObject := IUnknown(ovVar) as SWBemObject;
            wmiMethod := wmiObject.Methods_.Item ('Rename', 0) ;
            wmiInParams := wmiMethod.InParameters ;
            wmiInst := wmiInParams.SpawnInstance_(0) ;
            wmiProperty := wmiInst.Properties_.Add ('Name', wbemCimtypeString, False, 0) ;
            propValue := NewName ;
            wmiProperty.Set_Value (propValue);
            if UserName <> '' then
            begin
                wmiProperty := wmiInst.Properties_.Add ('UserName', wbemCimtypeString, False, 0) ;
                propValue := UserName ;
                wmiProperty.Set_Value (propValue);
                wmiProperty := wmiInst.Properties_.Add ('Password', wbemCimtypeString, False, 0) ;
                propValue := Password ;
                wmiProperty.Set_Value (propValue);
            end ;
            wmiOutParams := wmiObject.ExecMethod_ ('Rename', wmiInst, 0, nil) ;
            wmiProp := wmiOutParams.Properties_.Item ('ReturnValue', 0) ;
            VarClear (ovVar); // 5.2
            VarClear (propValue); // 5.2
            result := wmiProp.Get_Value ;
         // 0=OK reboot needed, non 0 various errors, see MSDN
            exit ;
        end ;
    except
        result := -1 ;
        VarClear (ovVar); // 5.2
        VarClear (propValue); // 5.2
    end ;
    finally
        wmiLocator.Free;
    end;
end ;

(* ************************************************************************* *)

// reboot or close down PC

function MagWmiCloseWin (const Comp, User, Pass: string ; reboot: boolean;
                                                var errinfo: string): integer ;
var
    wmiLocator: TSWbemLocator;
    wmiServices: ISWbemServices;
    wmiObjectSet: ISWbemObjectSet;
    wmiObject, wmiInst, wmiInParams, wmiOutParams: ISWbemObject;
    wmiProp: ISWbemProperty;
    wmiProperty: SWbemProperty;
    wmiMethod: SWbemMethod;
    ovVar, propValue: OleVariant;
    Enum: IEnumVariant;
    Arg: string ;
    lwValue, flags: LongWord;
begin
    errinfo := '' ;
    Result := -1 ;
    wmiLocator := TSWbemLocator.Create (Nil);
    try
    try
        wmiServices := wmiLocator.ConnectServer (Comp, RootNameSpace, User, Pass,
                                                                      '', '', 0, nil);
        wmiServices.Security_.Privileges.Add(wbemPrivilegeShutdown, True);
        Arg := 'SELECT * FROM Win32_OperatingSystem WHERE Primary=True' ;
        wmiObjectSet := wmiServices.ExecQuery (Arg, 'WQL',
                                                     wbemFlagReturnImmediately, nil) ;
        if (wmiObjectSet.Count < 1) then exit ;  // expect a single instance
        Enum := (wmiObjectSet._NewEnum) as IEnumVariant;
        while (Enum.Next (1, ovVar, lwValue) = S_OK) do
        begin
            wmiObject := IUnknown(ovVar) as SWBemObject;
            wmiMethod := wmiObject.Methods_.Item ('Win32Shutdown', 0) ;
            wmiInParams := wmiMethod.InParameters ;
            wmiInst := wmiInParams.SpawnInstance_(0) ;
            flags := EWX_SHUTDOWN + EWX_FORCE ;  // forced shutdown
            if reboot then flags := EWX_REBOOT + EWX_FORCE ;  // forced reboot
            wmiProperty := wmiInst.Properties_.Add ('Flags', wbemCimtypeSint32, False, 0) ;
            propValue := flags ;
            wmiProperty.Set_Value (propValue);
            wmiOutParams := wmiObject.ExecMethod_ ('Win32Shutdown', wmiInst, 0, nil);
            wmiProp := wmiOutParams.Properties_.Item ('ReturnValue', 0) ;
            result := wmiProp.Get_Value ;
            VarClear (ovVar); // 5.2
            VarClear (propValue); // 5.2
            if result <> 0 then errinfo := 'PC Close Down Failed: ' +
                         SysErrorMessage (result) + ' [' + IntToCStr (result) + ']' ;
            exit ;
        end ;
    except
        result := -1 ;
        errinfo := 'Exception in MagWmiCloseWin' ;
        VarClear (ovVar); // 5.2
        VarClear (propValue); // 5.2
    end ;
    finally
        wmiLocator.Free;
    end;
end ;

(* ************************************************************************* *)

function MagWmiGetMemory: TWmiMemoryRec ;
var
    rows, instances: integer ;
    WmiResults: T2DimStrArray ;

    function GetResInt64 (Info: string): Int64 ;
    begin
        result := AscToInt64 (MagWmiSearch1 (WmiResults, Info)) * KBYTE ;
    end ;

begin
    with result do
    begin
        FreePhysicalMemory := 0 ;
        FreeSpaceInPagingFiles := 0 ;
        FreeVirtualMemory := 0 ;
        SizeStoredInPagingFiles := 0 ;
        TotalSwapSpaceSize := 0 ;
        TotalVirtualMemorySize := 0 ;
        TotalVisibleMemorySize := 0 ;
    end ;
    try
        rows := MagWmiGetInfo ('', RootNameSpace, '', '',
          'SELECT FreePhysicalMemory, FreeSpaceInPagingFiles, FreeVirtualMemory, ' +
          'SizeStoredInPagingFiles, TotalSwapSpaceSize, TotalVirtualMemorySize, ' +
          'TotalVisibleMemorySize FROM Win32_OperatingSystem', WmiResults, instances) ;
        if (instances = 1) and (rows > 1) then
        begin
            with result do
            begin
                FreePhysicalMemory := GetResInt64 ('FreePhysicalMemory') ;
                FreeSpaceInPagingFiles := GetResInt64 ('FreeSpaceInPagingFiles') ;
                FreeVirtualMemory := GetResInt64 ('FreeVirtualMemory') ;
                SizeStoredInPagingFiles := GetResInt64 ('SizeStoredInPagingFiles') ;
                TotalSwapSpaceSize := GetResInt64 ('TotalSwapSpaceSize') ;
                TotalVirtualMemorySize := GetResInt64 ('TotalVirtualMemorySize') ;
                TotalVisibleMemorySize := GetResInt64 ('TotalVisibleMemorySize') ;
            end ;
        end ;
    finally
        WmiResults := Nil ;
    end ;
end ;


(* ************************************************************************* *)

function ChangeByteOrders (P: Pointer; Len: Integer): String  ;
var
    I: integer ;
    C: PAnsiChar ;
    S: AnsiString ;
begin
    SetLength (S, Len) ;
    C := PAnsiChar (P) ;
    I := 1 ;
    while I < Len do
    begin
        S [I + 1] := C^ ;
        inc (C) ;
        S [I] :=  C^ ;
        inc (I, 2) ;
        inc (C) ;
    end ;
    result := trim (String (S)) ; // 9 Aug 2010
end ;

// convert hex bytes string to binary

function HexStrToBinStr (const HexStr: AnsiString): AnsiString ;
var
    I, len: integer ;
    S: AnsiString ;
begin
    len := Length (HexStr) div 2 ;
    SetLength (result, len) ;
    for I := 1 to len do
    begin
        S := AnsiChar ('$') + HexStr [(I * 2) - 1] + HexStr [I * 2];
        result [I] := AnsiChar (AscToIntAnsi(S));
    end;
end;

// find Most Significant Bit

function FindMSB (value: USHORT): integer ;
var
    mask: USHORT ;
begin
    mask := $8000;
    for result := 15 downto 0 do
    begin
        if value AND mask = mask then exit ;
        mask := mask div 2 ;
    end;
    result := 0 ;
end ;

function GetATAMajorVersion (value: USHORT): string ;
var
    I: integer ;
begin
    I := FindMSB (value) ;
    case I of
        9: result := 'ACS-2';
        8: result := 'ATA8-ACS';
        7: result := 'ATA/ATAPI-7';
        6: result := 'ATA/ATAPI-6';
        5: result := 'ATA/ATAPI-5';
        4: result := 'ATA/ATAPI-4';
        3: result := 'ATA-3';
        2: result := 'ATA-2';
        1: result := 'ATA-1';
    else
        result := 'Unknown';
    end;
end ;

function GetSATAVersion (value: USHORT): string ;
var
    I: integer ;
begin
    result := '' ;
    if (value AND $F000) <> $1000 then exit ;
    I := FindMSB (value AND $0FFF) ;
    case I of
        6: result := 'SATA 3.1';
        5: result := 'SATA 3.0';
        4: result := 'SATA 2.6';
        3: result := 'SATA 2.5';
        2: result := 'SATA II Ext';
        1: result := '"SATA 1.0a';
        0: result := 'ATA8-AST';
    end;
end;

function GetSATASpeed (value: USHORT): string ;
var
    I: integer ;
begin
    result := '' ;
    I := FindMSB (value AND $0FE) ;
    case I of
        3: result := '6.0 Gb/s';
        2: result := '3.0 Gb/s';
        1: result := '1.5 Gb/s';
    end;
end;

(* ************************************************************************* *)

// get as much disk drive information as we can using various techniques

function MagWmiSmartDiskInfo (drivenr: integer; const deviceid: string;
                                                var DriveResult: TDriveResult): boolean ;
var
    hSMARTIOCTL: THandle;
    cbBytesReturned: DWORD ;
    scip: TSendCmdInParams;
    IdOutCmd: PSendCmdOutParams ;
    IdentifyDeviceData: PIdentifyDeviceData ;
    StoragePropertyQuery: TStoragePropertyQuery ;
    StorageDeviceDescr: TStorageDeviceDescr ;
    DiskGeometryEx: TDiskGeometryEx ;
    ScsiInquiryData: PInquiryData ;
    S: String ;
    I: integer ;
    status: boolean ;
    StrPtr: PAnsiChar ;
    S1: AnsiString ; // unicode
    Buffer: Array [0..SizeOf (TScsiPassThroughWithBuffers) +
                            SizeOf (TScsiPassThrough) -1 ] of Byte;
    sptwb: TScsiPassThroughWithBuffers absolute Buffer;

    function ExtractFromBuf (offset, len: integer): Ansistring ;  // SCSI buffer
    var
        I: integer ;
    begin
        result := '' ;
        I := offset ;
        while len <> 0 do
        begin
            if sptwb.bDataBuf [I] = 0 then exit ;
            result := result + AnsiChar (sptwb.bDataBuf [I]) ;
            dec (len) ;
            inc (I) ;
        end;
    end ;

begin
    result := false;
    with DriveResult do
    begin
        DriveNum := drivenr ;
        ErrInfo := '' ;
        VendorId := '' ;
        ProductId := '' ;
        BusType := 0 ;
        DeviceType := 0 ;
        MediaType := 0 ;
        for I := 1 to 4 do VendorUnique [I] := 0 ;
        SerialNumber := '' ;
        FirmwareRev := '' ;
        ModelNumber := '' ;
        RemoveMedia := false ;
        SmartSupport := false ;
        SmartEnabled := false ;
        LBA48Support := false ;
        MajorRev := 0 ;
        MinorRev := 0 ;
        AtaVersion := '' ;
        SataVersion := '' ;
        CapacityNum := 0 ;
        SectorNum := 0 ;
        SecSizeLogical := 0 ;
        SecSizePhysical := 0 ;
        FillChar (IdentifyDevice, SizeOf (IdentifyDevice), 0) ;
    end;
    if deviceId = '' then
        S := '\\.\PhysicalDrive' + IntToStr (drivenr)
    else
        S := deviceid ;
    DriveResult.DeviceId := Copy (S, 5, 99) ;

 // NT+ only returns info for a specified single physical drive
    hSMARTIOCTL := CreateFile(PChar (S), Generic_Read or Generic_Write,
                    File_Share_Read or File_Share_Write, nil, Open_Existing, 0, 0) ;
    if hSMARTIOCTL = INVALID_HANDLE_VALUE then
    begin
        DriveResult.ErrInfo := 'Unable to open: ' + S + ': ' + SysErrorMessage (GetLastError) ;
        exit ;
    end ;
    result := true ;

    GetMem (IdOutCmd, IdOutLen) ;
    try  // protect handle and memory

     // get IOCTL_STORAGE_QUERY_PROPERTY
        FillChar (StoragePropertyQuery, SizeOf (TStoragePropertyQuery), 0) ;
        FillChar (StorageDeviceDescr, SizeOf (TStorageDeviceDescr), 0) ;
        StoragePropertyQuery.PropertyId := Ord (StorageDeviceProperty) ;
        StoragePropertyQuery.QueryType := Ord (PropertyStandardQuery) ;
        status := DeviceIoControl (hSMARTIOCTL, IOCTL_STORAGE_QUERY_PROPERTY,
                    @StoragePropertyQuery, SizeOf (TStoragePropertyQuery),
                    @StorageDeviceDescr, SizeOf (TStorageDeviceDescr), cbBytesReturned, nil) ;
        if NOT status then
        begin
            DriveResult.ErrInfo := 'IOCTL_STORAGE_QUERY_PROPERTY failed: ' + SysErrorMessage (GetLastError) ;
            exit ;
        end ;
        DriveResult.BusType := StorageDeviceDescr.BusType ;
        DriveResult.DeviceType := StorageDeviceDescr.DeviceType ;
        DriveResult.RemoveMedia := StorageDeviceDescr.RemovableMedia ;
        if StorageDeviceDescr.VendorIdOffset <> 0 then
        begin
            StrPtr := @StorageDeviceDescr ;
            StrPtr := StrPtr + StorageDeviceDescr.VendorIdOffset ;
            DriveResult.VendorId := String (StrPtr) ;
        end;
        if StorageDeviceDescr.ProductIdOffset <> 0 then
        begin
            StrPtr := @StorageDeviceDescr ;
            StrPtr := StrPtr + StorageDeviceDescr.ProductIdOffset ;
            DriveResult.ProductId := String (StrPtr) ;
        end;
        if StorageDeviceDescr.ProductRevisionOffset <> 0 then
        begin
            StrPtr := @StorageDeviceDescr ;
            StrPtr := StrPtr + StorageDeviceDescr.ProductRevisionOffset ;
            DriveResult.ProductRev := String (StrPtr) ;
        end;
        if StorageDeviceDescr.SerialNumberOffset <> 0 then
        begin
            StrPtr := @StorageDeviceDescr ;
            StrPtr := StrPtr + StorageDeviceDescr.SerialNumberOffset ;
            S1 := StrPtr ;
            if DriveResult.BusType = BusTypeAta then
            begin
                S1 := HexStrToBinStr (S1) ;
                DriveResult.SerialNumber := Trim (String  (ChangeByteOrders (@S1 [1], Length (S1)))) ;
            end
            else
                DriveResult.SerialNumber := Trim (String ((S1))) ;
        end;

     // try and handle non-RAID drives on Intel chipset RAID controller
        if DriveResult.BusType = BusTypeRaid then
        begin
            if (DriveResult.VendorId <> 'Intel') and (Pos ('Raid', DriveResult.ProductId) <> 1) then
                                                                        DriveResult.BusType := BusTypeAta ;
        end;

     // displayable types
        DriveResult.BusTypeDisp := BusTypeNames [DriveResult.BusType] ;
        if DriveResult.DeviceType <= DEVICE_TYPE_MAX then
                           DriveResult.DevTypeDisp := DeviceTypeStr [DriveResult.DeviceType] ;

    // SCART stuff only works on ATA drives with these functions - Sata does not work
        if DriveResult.BusType IN [BusTypeAta, BusTypeAtapi {, BusTypeSata}] then
        begin

         // enable SMART for the drive, so we can read stuff from it
            FillChar (scip, sizeof (scip), 0) ;
            SCIP.cBufferSize := 0 ;
            SCIP.irDriveRegs.bFeaturesReg := SMART_ENABLE_SMART_OPERATIONS;
            SCIP.irDriveRegs.bSectorCountReg := 1;
            SCIP.irDriveRegs.bSectorNumberReg := 1;
            SCIP.irDriveRegs.bCylLowReg := SMART_CYL_LOW;
            SCIP.irDriveRegs.bCylHighReg := SMART_CYL_HI;
        // Compute the drive number.
            SCIP.irDriveRegs.bDriveHeadReg := $A0 OR ((drivenr AND 1) shl 4) ;
            SCIP.irDriveRegs.bCommandReg := IDE_EXECUTE_SMART_FUNCTION;
            SCIP.bDriveNumber := drivenr ;
            FillChar (IdOutCmd^, IdOutLen, 0) ;
            cbBytesReturned := 0 ;
            status := DeviceIoControl (hSMARTIOCTL, SMART_SEND_DRIVE_COMMAND, @SCIP,
                            sizeof (SCIP) - 1, IdOutCmd, IdOutLen - 1, cbBytesReturned, Nil );
            if NOT status then
            begin
                DriveResult.ErrInfo := 'SMART Enable Command Failed, Drive: ' +
                        IntToStr (drivenr) + ' : ' + SysErrorMessage (GetLastError) ;
                exit ;
            end;

        // get the IDENTIFY_DEVICE_DATA sector
            FillChar (scip, sizeof (scip), 0) ;
            SCIP.cBufferSize := IDENTIFY_BUFFER_SIZE ;
            SCIP.irDriveRegs.bFeaturesReg := 0;
            SCIP.irDriveRegs.bSectorCountReg := 1;
            SCIP.irDriveRegs.bSectorNumberReg := 1;
            SCIP.irDriveRegs.bCylLowReg := 0;
            SCIP.irDriveRegs.bCylHighReg := 0;
            SCIP.irDriveRegs.bDriveHeadReg := $A0 OR ((drivenr AND 1) shl 4) ;
            if DriveResult.BusType = BusTypeAtapi then
                SCIP.irDriveRegs.bCommandReg := CAP_IDE_ATAPI_ID
            else
                SCIP.irDriveRegs.bCommandReg := IDE_ID_FUNCTION ;
            SCIP.bDriveNumber := drivenr ;
            FillChar (IdOutCmd^, IdOutLen, 0) ;
            IdentifyDeviceData := @IdOutCmd.bBuffer ;
            IdOutCmd^.cBufferSize := IDENTIFY_BUFFER_SIZE ;
            cbBytesReturned := 0 ;
            status := DeviceIoControl (hSMARTIOCTL, SMART_RECEIVE_DRIVE_DATA, @SCIP,
                                        sizeof (SCIP) - 1, IdOutCmd, IdOutLen, cbBytesReturned, Nil) ;
            if status then
            begin
                with IdentifyDeviceData^ do
                begin
                    DriveResult.ModelNumber := ChangeByteOrders (@sModelNumber, Length (sModelNumber)) ;
                    if DriveResult.SerialNumber = '' then
                        DriveResult.SerialNumber := ChangeByteOrders (@sSerialNumber, Length (sSerialNumber)) ;
                    DriveResult.FirmwareRev := ChangeByteOrders (@sFirmwareRev, Length (sFirmwareRev)) ;
                    DriveResult.SectorNum := Int64 (ulTotalAddressableSectors) ;  // LBA28 size, max 128 gigs
                    DriveResult.SecSizeLogical := 512 ;
                    DriveResult.SecSizePhysical := DriveResult.SecSizeLogical ;
                    for I := 1 to 3 do
                            DriveResult.VendorUnique [I] := wVendorUnique [I] ;
                    DriveResult.VendorUnique [4] := sVendorUnique2 ;
                    DriveResult.SmartSupport := (wCommandSetSupport1 AND CmdSet1SmartCommands) = CmdSet1SmartCommands ;
                    DriveResult.SmartEnabled := (wCommandSetActive1 AND CmdSet1SmartCommands) = CmdSet1SmartCommands ;
                    DriveResult.LBA48Support := (wCommandSetSupport2 AND CmdSet2BigLba) = CmdSet2BigLba ;
                    DriveResult.MajorRev := wMajorRevision ;
                    DriveResult.MinorRev := wMinorRevision ;
                    DriveResult.AtaVersion :=  GetATAMajorVersion (DriveResult.MajorRev) ;
                    if wSataVersion <> 0 then
                        DriveResult.SataVersion :=  GetSATAVersion (wSataVersion) + ', max ' +
                                        GetSATASpeed (wSataMaxSpeed) + ', cur ' + GetSATASpeed (wSataCurSpeed) ;
                    if DriveResult.LBA48Support then
                    begin
                        DriveResult.SectorNum := ullMax48BitLBA ;   // LBA48 size, larger than 128 gigs
                        if (wPhysicalLogicalSectorSize AND PhysLogSectSizeLogicalSectorLongerThan256Words) =
                                                                PhysLogSectSizeLogicalSectorLongerThan256Words then
                        begin
                            DriveResult.SecSizeLogical := ulWordsPerLogicalSector ;
                        end;
                        I := wPhysicalLogicalSectorSize AND PhysLogSectSizeLogicalSectorsPerPhysicalSector ;
                        if (wPhysicalLogicalSectorSize AND PhysLogSectSizeMultipleLogicalSectorsPerPhysicalSector) =
                                                           PhysLogSectSizeMultipleLogicalSectorsPerPhysicalSector then
                        begin
                             DriveResult.SecSizePhysical := DriveResult.SecSizeLogical * I ;
                        end;
                    end;
                    DriveResult.CapacityNum := DriveResult.SectorNum * DriveResult.SecSizeLogical ;
                 end ;
                 DriveResult.IdentifyDevice := IdentifyDeviceData^ ;
            end
            else
                DriveResult.ErrInfo := 'Identify Command Failed on Drive: ' +  DriverErrorStr [IdOutCmd.DriverStatus.bDriverError] +
                    ' bIDEStatus=' + IntToHex (IdOutCmd.DriverStatus.bIDEStatus, 4) + ' : ' + SysErrorMessage (GetLastError) ;
        end
        else
        begin

        // get drive size
            FillChar (DiskGeometryEx, SizeOf (DiskGeometryEx), 0) ;
            status := DeviceIoControl (hSMARTIOCTL, IOCTL_DISK_GET_DRIVE_GEOMETRY_EX, nil, 0,
                                @DiskGeometryEx, SizeOf (DiskGeometryEx), cbBytesReturned, nil) ;
            if NOT status then
            begin
                DriveResult.ErrInfo := 'IOCTL_DISK_GET_DRIVE_GEOMETRY_EX failed: ' + SysErrorMessage (GetLastError) ;
                exit ;
            end ;
            with DiskGeometryEx do
            begin
                DriveResult.MediaType := Geometry.MediaType ;
                DriveResult.CapacityNum := DiskSize ;  // LBA48 size, more than 128 gigs
                DriveResult.SecSizeLogical := Geometry.BytesPerSector ;
                DriveResult.SecSizePhysical := DriveResult.SecSizeLogical ;
                if Geometry.BytesPerSector > 0 then
                    DriveResult.SectorNum := DiskSize div Geometry.BytesPerSector ;
            end;

        // get more information using SCSI commands
            try
          // the SCSI buffer is documented at http://en.wikipedia.org/wiki/SCSI_Inquiry_Command
                if (DriveResult.BusType IN [BusTypeUSB]) or
                             (DriveResult.VendorId = '') or (DriveResult.ProductId = '') then
                begin
                 // first standard enquiry to get general stuff
                    FillChar (Buffer, SizeOf(Buffer), #0) ;
                    with sptwb.spt do
                    begin
                        Length := SizeOf (TScsiPassThrough) ;
                        CdbLength := CDB6GENERIC_LENGTH ;
                        SenseInfoLength := 24 ;
                        DataIn := SCSI_IOCTL_DATA_IN ;
                        DataTransferLength := 192 ;
                        TimeOutValue := 2 ;
                        DataBufferOffset := PAnsiChar (@sptwb.bDataBuf) - PAnsiChar (@sptwb) ;
                        SenseInfoOffset := PAnsiChar (@sptwb.bSenseBuf) - PAnsiChar (@sptwb) ;
                        Cdb[0] := SCSIOP_INQUIRY; // operation code
                        Cdb[1] := 0 ; //  Flags none, standard query
                        Cdb[2] := 0 ;
                        Cdb[4] := DataTransferLength ; // AllocationLength
                    end;
                    status := DeviceIoControl (hSMARTIOCTL, IOCTL_SCSI_PASS_THROUGH,
                                    @sptwb, SizeOf(TScsiPassThrough), @sptwb,
                                    sptwb.spt.DataBufferOffset + sptwb.spt.DataTransferLength, cbBytesReturned, nil) ;
                    if NOT status then
                    begin
                         DriveResult.ErrInfo := 'IOCTL_SCSI_PASS_THROUGH Command Failed: ' + SysErrorMessage (GetLastError) ;
                         exit ;
                    end ;
                    ScsiInquiryData := @sptwb.bDataBuf ;
                    with DriveResult do
                    begin
                        SetString (VendorId, PAnsiChar (@ScsiInquiryData.sVendorId), 8) ;
                        SetString (ProductId, PAnsiChar (@ScsiInquiryData.sProductId), 16) ;
                        SetString (ProductRev, PAnsiChar (@ScsiInquiryData.sProductRevisionLevel), 4) ;
                    end;

                {    DriveResult.VendorId := Trim (ExtractFromBuf (8, 8)) ;
                    DriveResult.ProductId := Trim (ExtractFromBuf (16, 16)) ;
                    DriveResult.FirmwareRev := Trim (ExtractFromBuf (32, 4)) ; }
                end;

             // now query vital product information - unit serial number
                if DriveResult.SerialNumber = '' then
                begin
                    FillChar (Buffer, SizeOf(Buffer), #0) ;
                    with sptwb.spt do
                    begin
                        Length := SizeOf (TScsiPassThrough) ;
                        CdbLength := CDB6GENERIC_LENGTH ;
                        SenseInfoLength := 24 ;
                        DataIn := SCSI_IOCTL_DATA_IN ;
                        DataTransferLength := 192 ;
                        TimeOutValue := 2 ;
                        DataBufferOffset := PAnsiChar (@sptwb.bDataBuf) - PAnsiChar (@sptwb) ;
                        SenseInfoOffset := PAnsiChar (@sptwb.bSenseBuf) - PAnsiChar (@sptwb) ;
                        Cdb[0] := SCSIOP_INQUIRY; // operation code
                        Cdb[1] := $01 ; //  Flags := CDB_INQUIRY_EVPD;  Vital product data
                        Cdb[2] := $80 ; //  PageCode            Unit serial number
                        Cdb[4] := DataTransferLength ; // AllocationLength
                    end ;
                    status := DeviceIoControl (hSMARTIOCTL, IOCTL_SCSI_PASS_THROUGH,
                                    @sptwb, SizeOf(TScsiPassThrough), @sptwb,
                                    sptwb.spt.DataBufferOffset + sptwb.spt.DataTransferLength, cbBytesReturned, nil) ;
                    if NOT status then
                    begin
                         DriveResult.ErrInfo := 'IOCTL_SCSI_PASS_THROUGH Command Failed: ' + SysErrorMessage (GetLastError) ;
                         exit ;
                    end ;
                    I := Ord (sptwb.bDataBuf [3]) ;  // length of data
                    if (I > 0) and (sptwb.bDataBuf [1] = $80) then  // same as page code command, got a serial number
                    begin
                        S1 := ExtractFromBuf (4, I) ;
                        DriveResult.SerialNumber := Trim (String (ChangeByteOrders (@S1 [1], Length (S1)))) ;   // 8 Jan 2013 correct byte order
                    end;
                end;
            except
                DriveResult.ErrInfo := 'Exception Getting SCSI Drive Inquiry Information' ;
            end ;
        end;
    finally
        Freemem (IdOutCmd) ;
        CloseHandle (hSMARTIOCTL) ;
    end ;
end ;

(* ************************************************************************* *)

// map list of physical disk drives, returns total number found

function MagWmiMapDrives (MaxNum: integer; var DriveResults: TDriveResults): integer ;
var
    I: integer ;
begin
    result := 0 ;
    SetLength (DriveResults, 0) ;
    SetLength (DriveResults, MaxNum) ;
    for I := 0 to MaxNum - 1 do
    begin
        if MagWmiSmartDiskInfo (I, '\\.\PhysicalDrive' + IntToStr (I),
                                                         DriveResults [result]) then
        begin
            inc (result) ;
        end ;
    end;
end;

(* ************************************************************************* *)

// locally connected ATA drives only, no SCSI, USB or firewire

function MagWmiSmartDiskFail (drivenr: integer; var DriveResult: TDriveResult;
                                                        var SmartResult: TSmartResult): boolean ;
var
    hSMARTIOCTL: THandle;
    cbBytesReturned: DWORD ;
    scip: TSendCmdInParams;
    I, J, totattrs, basevalue: integer ;
    ThreshOutCmd: PSendCmdOutParams ;
    SAttrThreshold: PAttrThreshold ;
    AttrOutCmd: PSendCmdOutParams ;
    SDriveAttribute: PDriveAttribute ;
    S: String ;    // unicode
    Attr: byte ;
    BufPtr: PAnsiChar ;    // unicode
    Raw64: int64 ;
begin
    result := false;
    try
    with SmartResult do
    begin
        DriveNum := drivenr ;
        Temperature := 0 ;
        TempWorst := 0 ;
        TempLow := 0 ;
        HoursRunning := 0 ;
        ReallocSector := 0 ;
        PowerCycles := 0 ;
        SmartFailTot := 0 ;
        SmartWarnTot := 0 ;
        TotalAttrs := 0 ;
        for I := 0 to MaxSmartAttr do AttrNum [I] := 0 ;
        for I := 0 to MaxSmartAttr do AttrName [I] := '' ;
        for I := 0 to MaxSmartAttr do AttrPreFail [I] := false ;
        for I := 0 to MaxSmartAttr do AttrEvents [I] := false ;
        for I := 0 to MaxSmartAttr do AttrErrorRate [I] := false ;
        for I := 0 to MaxSmartAttr do AttrCurValue [I] := 0 ;
        for I := 0 to MaxSmartAttr do AttrWorstVal [I] := 0 ;
        for I := 0 to MaxSmartAttr do AttrThreshold [I] := 0 ;
        for I := 0 to MaxSmartAttr do AttrRawValue [I] := 0 ;
        for I := 0 to MaxSmartAttr do AttrState [I] := '' ;
    end ;

// get all disk info
    if NOT MagWmiSmartDiskInfo (drivenr, '\\.\PhysicalDrive' + IntToStr (drivenr), DriveResult) then exit ;
    if NOT DriveResult.SmartEnabled then
    begin
        DriveResult.ErrInfo := 'SMART Not Supported' ;
        exit ;
    end ;

 // NT+ only returns info for a specified single physical drive
    S := '\\.\PhysicalDrive' + IntToStr (drivenr) ;
    hSMARTIOCTL := CreateFile(PChar (S), Generic_Read or Generic_Write,
                        File_Share_Read or File_Share_Write, nil, Open_Existing, 0, 0) ;
    if hSMARTIOCTL = INVALID_HANDLE_VALUE then
    begin
        DriveResult.ErrInfo := 'Unable to open physical drive: ' + SysErrorMessage (GetLastError) ;
        exit ;
    end ;

    GetMem (AttrOutCmd, AttrOutLen) ;
    GetMem (ThreshOutCmd, ThreshOutLen) ;
    try  // protect handle and memory

    // read SMART attributes
        FillChar (scip, sizeof (scip), 0) ;
        SCIP.cBufferSize := READ_ATTRIBUTE_BUFFER_SIZE ;
        SCIP.irDriveRegs.bFeaturesReg := SMART_READ_ATTRIBUTE_VALUES;
        SCIP.irDriveRegs.bSectorCountReg := 1;
        SCIP.irDriveRegs.bSectorNumberReg := 1;
        SCIP.irDriveRegs.bCylLowReg := SMART_CYL_LOW;
        SCIP.irDriveRegs.bCylHighReg := SMART_CYL_HI;
// Compute the drive number.
        SCIP.irDriveRegs.bDriveHeadReg := $A0 OR ((drivenr AND 1) shl 4) ;
        SCIP.irDriveRegs.bCommandReg := IDE_EXECUTE_SMART_FUNCTION;
        SCIP.bDriveNumber := drivenr ;
        FillChar (AttrOutCmd^, AttrOutLen, 0) ;
        cbBytesReturned := 0 ;
        result := DeviceIoControl (hSMARTIOCTL, SMART_RECEIVE_DRIVE_DATA, @SCIP,
                        sizeof (SCIP) - 1, AttrOutCmd, AttrOutLen - 1, cbBytesReturned, Nil );
        if NOT result then
        begin
            DriveResult.ErrInfo:= 'SMART Read Attr Command Failed: ' +
                        SysErrorMessage (GetLastError) + '; DriverStatus: ' +
                       DriverErrorStr [AttrOutCmd.DriverStatus.bDriverError] +
                       '; bIDEStatus=' + IntToHex (AttrOutCmd.DriverStatus.bIDEStatus, 4) ;
            exit ;  // no more
        end ;
        FillChar (scip, sizeof (scip), 0) ;
        SCIP.cBufferSize := READ_THRESHOLD_BUFFER_SIZE ;
        SCIP.irDriveRegs.bFeaturesReg := SMART_READ_ATTRIBUTE_THRESHOLDS;
        SCIP.irDriveRegs.bSectorCountReg := 1;
        SCIP.irDriveRegs.bSectorNumberReg := 1;
        SCIP.irDriveRegs.bCylLowReg := SMART_CYL_LOW;
        SCIP.irDriveRegs.bCylHighReg := SMART_CYL_HI;
// Compute the drive number.
        SCIP.irDriveRegs.bDriveHeadReg := $A0 OR ((drivenr AND 1) shl 4) ;
        SCIP.irDriveRegs.bCommandReg := IDE_EXECUTE_SMART_FUNCTION;
        SCIP.bDriveNumber := drivenr ;
        FillChar (ThreshOutCmd^, ThreshOutLen, 0) ;
        cbBytesReturned := 0 ;
        result := DeviceIoControl (hSMARTIOCTL, SMART_RECEIVE_DRIVE_DATA, @SCIP,
                    sizeof (SCIP) - 1, ThreshOutCmd, ThreshOutLen - 1, cbBytesReturned, Nil );
        if NOT result then
        begin
            DriveResult.ErrInfo := 'SMART Read Threshold Command Failed: ' +
                        SysErrorMessage (GetLastError) + '; DriverStatus: ' +
                       DriverErrorStr [AttrOutCmd.DriverStatus.bDriverError] +
                       '; bIDEStatus=' + IntToHex (AttrOutCmd.DriverStatus.bIDEStatus, 4) ;
            exit ;
        end  ;

// loop through the structures, getting attributes
        BufPtr := @ThreshOutCmd.bBuffer ;
        Pointer (SAttrThreshold) := BufPtr + 2 ;
        BufPtr := @AttrOutCmd.bBuffer ;
        Pointer (SDriveAttribute) := BufPtr + 2 ;
        totattrs := 0 ;
        basevalue := 100 ;
        for J := 0 to Pred (NUM_ATTRIBUTE_STRUCTS) do
        begin
            with SmartResult do
            begin
                Raw64 := 0 ;
                Move (SDriveAttribute^.bRawValue [1], Raw64, 6) ;
                Attr := SDriveAttribute^.bAttrID ;
                if Attr = 0 then continue ;
                AttrName [totattrs] := 'Unknown' ;
                if Attr <= ATTR_POWER_CYCLE_COUNT then
                    AttrName [totattrs] := pAttrNames [Attr]
                else if (Attr >= Attr_Emergency_Retract_Cycle) and (Attr <= Attr_Offline_Seek_Perf) then
                    AttrName [totattrs] := pAttrNames2 [Attr - Attr_Emergency_Retract_Cycle] ;
                AttrNum [totattrs] := SDriveAttribute^.bAttrID ;
                AttrPreFail [totattrs] := (SDriveAttribute^.wStatusFlags AND
                                                    PRE_FAILURE_WARRANTY) = PRE_FAILURE_WARRANTY ;
                AttrEvents [totattrs] := (SDriveAttribute^.wStatusFlags AND
                                                    EVENT_COUNT_ATTRIBUTE) = EVENT_COUNT_ATTRIBUTE ;
                AttrErrorRate [totattrs] := (SDriveAttribute^.wStatusFlags AND
                                                    ERROR_RATE_ATTRIBUTE) = ERROR_RATE_ATTRIBUTE ;
                AttrCurValue [totattrs] := SDriveAttribute^.bAttrValue ;
                AttrWorstVal [totattrs] := SDriveAttribute^.bWorstValue ;
                AttrThreshold [totattrs] := SAttrThreshold^.bWarrantyThreshold ;
                AttrRawValue [totattrs] := Raw64 ;

             // keep some more interesting raw values
                if (Raw64 > 0) and (Raw64 < MaxLongInt) then
                begin
                    if Attr = ATTR_REALLOC_SECTOR_COUNT then ReallocSector := Raw64 ;
                    if Attr = ATTR_POWER_ON_COUNT then HoursRunning := (Raw64 div 60) + 1 ;  // assume minutes
                    if Attr = ATTR_POWER_CYCLE_COUNT then PowerCycles := Raw64 ;
                end ;

             // try and set base or highest value for attribute
                if ((Attr = ATTR_SPIN_UP_TIME) or (Attr = ATTR_START_STOP_COUNT)) and
                                           (AttrCurValue [totattrs] > 230) then basevalue := 253 ;

             // check if drive is OK, failing or warn
                AttrState [totattrs] := '-' ; // 'Aging' ;
                if AttrPreFail [totattrs] and (AttrThreshold [totattrs] <> 0) then
                begin
                    AttrState [totattrs] := 'OK' ;
                    if AttrCurValue [totattrs] < AttrThreshold [totattrs] then
                    begin
                        inc (SmartFailTot) ;
                        AttrState [totattrs] := 'Failing Now' ;
                    end
                    else if AttrWorstVal [totattrs] < AttrThreshold [totattrs] then
                    begin
                        inc (SmartFailTot) ;
                        AttrState [totattrs] := 'Failed In Past' ;
                    end
                    else
                    begin
                      // increase threshold for warning
                        I := ((basevalue - AttrThreshold [totattrs]) div 2) + AttrThreshold [totattrs] ;
                        if (AttrCurValue [totattrs] < I) OR
                                               ((Attr = ATTR_REALLOC_SECTOR_COUNT) and (Raw64 > 500)) then
                        begin
                            inc (SmartWarnTot) ;
                            AttrState [totattrs] := 'Warning' ;
                        end ;
                    end ;
                end ;
                if Attr = Attr_Temperature_Celcius then
                begin
                     Temperature := SDriveAttribute^.bRawValue [1] ;
                     TempLow := SDriveAttribute^.bRawValue [3] ;   // IBM and Fujitsu
                     TempWorst := SDriveAttribute^.bRawValue [5] ; // IBM and Fujitsu
                   // Seagate drives store worst temperature
                     if Temperature = SDriveAttribute^.bAttrValue then
                                             TempWorst := SDriveAttribute^.bWorstValue ;
                end ;
            end ;
            inc (totattrs) ;
            if totattrs >= MaxSmartAttr then break ;
            Inc (SAttrThreshold) ;   // next record
            Inc (SDriveAttribute) ;
        end ;
        SmartResult.TotalAttrs := totattrs ;
        result := true ;
    finally
        Freemem (AttrOutCmd) ;
        Freemem (ThreshOutCmd) ;
        CloseHandle (hSMARTIOCTL) ;
    end ;
    except
        DriveResult.ErrInfo := 'SMART Failed - ' + GetExceptMess (ExceptObject) ;
    end ;
end ;

(* ************************************************************************* *)

// build a list of SCSI devices by checking up to MaxNum adaptors, then
// parsing each bus on each adaptor for devices, including disks, DVD-ROMs
// note NT sees all devices as SCSI, including all ATA devices, not currently supporting SMART on these devices

function MagWmiSmartScsiBus (MaxNum: integer; var DriveResults: TDriveResults): integer ;
const
    DataLength = sizeof(TSendCmdInParams)-1+IDENTIFY_BUFFER_SIZE;
    BufferLength = SizeOf(SRB_IO_CONTROL)+DataLength;
var
    drivenr, portnr, I, J: integer ;
    ScsiPort: string ;
    StrPtr: PAnsiChar ;
    hSMARTIOCTL: THandle;
    SendCmdInParams: PSendCmdInParams;
    cbBytesReturned, offset: DWORD ;
    status: boolean ;
    ScsiAdapterBusInfo: TScsiAdapterBusInfo ;
    ScsiInquiryData: PScsiInquiryData ;
    SrbBuffer : array[0..BufferLength] of Byte;
    SrbControl : TSrbIoControl absolute SrbBuffer;
    IdentifyDeviceData: PIdentifyDeviceData ;
begin
    result := 0 ;
    SetLength (DriveResults, 0) ;
    SetLength (DriveResults, MaxNum) ;
    for portnr := 0 to Pred (MaxNum) do
    begin
        with DriveResults [result] do
        begin
            DriveNum := 0 ;
            ErrInfo := '' ;
            VendorId := '' ;
            ProductId := '' ;
            BusType := 0 ;
            for I := 1 to 4 do VendorUnique [I] := 0 ;
            SerialNumber := '' ;
            FirmwareRev := '' ;
            ModelNumber := '' ;
            RemoveMedia := false ;
            SmartSupport := false ;
            SmartEnabled := false ;
            LBA48Support := false ;
            MajorRev := 0 ;
            MinorRev := 0 ;
            AtaVersion := '' ;
            SataVersion := '' ;
            CapacityNum := 0 ;
            SectorNum := 0 ;
            SecSizeLogical := 0 ;
            SecSizePhysical := 0 ;
            FillChar (IdentifyDevice, SizeOf (IdentifyDevice), 0) ;
        end;
        ScsiPort := '\\.\SCSI' + IntToStr (portnr) + ':' ;
        DriveResults [result].DeviceId := Copy (ScsiPort, 5, 99) ;
        hSMARTIOCTL := CreateFile(PChar (ScsiPort), Generic_Read or Generic_Write,
                    File_Share_Read or File_Share_Write, nil, Open_Existing, 0, 0) ;
        if hSMARTIOCTL = INVALID_HANDLE_VALUE then
        begin
            DriveResults [result].ErrInfo := 'Unable to open SCSI bus: ' + SysErrorMessage (GetLastError) ;
            continue ;
        end ;
        try
            cbBytesReturned := SizeOf (TScsiAdapterBusInfo) ;
            FillChar (ScsiAdapterBusInfo, SizeOf (TScsiAdapterBusInfo), 0);
            status:= DeviceIoControl (hSMARTIOCTL, IOCTL_SCSI_GET_INQUIRY_DATA, nil,
                                                 0, @ScsiAdapterBusInfo, cbBytesReturned, cbBytesReturned, nil );
            if NOT status then
            begin
                DriveResults [result].ErrInfo := 'IOCTL_SCSI_GET_INQUIRY_DATA Failed: ' +
                                                                SysErrorMessage (GetLastError) ;
            end
            else
            begin

            // returns one TScsiBusData record for each bus on the adaptor
                for I := 0 to Pred (ScsiAdapterBusInfo.NumberOfBuses) do
                begin

                // now scan for logical devices on the bus - ScsiAdapterBusInfo.BusData [I].NumberOfLogicalUnits
                // not necessarily consecutive
                    offset := ScsiAdapterBusInfo.BusData [I].InquiryDataOffset ;
                    while offset <> 0 do
                    begin
                        ScsiInquiryData := PScsiInquiryData (PAnsiChar (@ScsiAdapterBusInfo) + offset) ;
                        offset := ScsiInquiryData^.NextInquiryDataOffset ;
                        drivenr := ScsiInquiryData^.TargetId ;
                        with DriveResults [result] do
                        begin
                            DriveNum := drivenr ;
                            DeviceId := Copy (ScsiPort, 5, 99) ;
                            BusType := BusTypeScsi ;
                            DeviceType := ScsiInquiryData^.InquiryData.DeviceType ;
                            if DeviceType <= DEVICE_TYPE_MAX then
                                    DevTypeDisp := DeviceTypeStr [DeviceType] ;
                            BusTypeDisp := BusTypeNames [BusType] ;
                            SetString (VendorId, PAnsiChar (@ScsiInquiryData^.InquiryData.sVendorId), 8) ;
                            SetString (ProductId, PAnsiChar (@ScsiInquiryData^.InquiryData.sProductId), 16) ;
                            SetString (ProductRev, PAnsiChar (@ScsiInquiryData^.InquiryData.sProductRevisionLevel), 4) ;
                        end;

                    // get the IDENTIFY_DEVICE_DATA sector - note this seems to give an error for many devices
                    // and the returned data is dubious even if there is no error - don't know why
                        FillChar (SrbControl, BufferLength,#0) ;
                        SrbControl.HeaderLength := SizeOf(SRB_IO_CONTROL) ;
                        System.Move ('SCSIDISK', SrbControl.Signature, 8) ;      // atapi.sys   ('3ware>' for 3ware controller)
                        SrbControl.Timeout      := 2;
                        SrbControl.Length       := DataLength;
                        SrbControl.ControlCode  := IOCTL_SCSI_MINIPORT_IDENTIFY;
                        SendCmdInParams := PSendCmdInParams (PAnsiChar (@SrbBuffer) + SizeOf (SRB_IO_CONTROL));
                        with SendCmdInParams^ do
                        begin
                            cBufferSize  := IDENTIFY_BUFFER_SIZE;
                            bDriveNumber := drivenr;
                            with irDriveRegs do
                            begin
                                bFeaturesReg     := 0;
                                bSectorCountReg  := 1;
                                bSectorNumberReg := 1;
                                bCylLowReg       := 0;
                                bCylHighReg      := 0;
                                bDriveHeadReg := $A0 OR ((drivenr and 1) shl 4);
                                bCommandReg      := IDE_ID_FUNCTION ;
                            end;
                        end;
                        IdentifyDeviceData := @SendCmdInParams^.bBuffer ;
                        cbBytesReturned := 0 ;
                        status := DeviceIoControl (hSMARTIOCTL, IOCTL_SCSI_MINIPORT, @SrbControl,
                                                     BufferLength, @SrbControl, BufferLength, cbBytesReturned, Nil) ;
                        if status and (SrbControl.ReturnCode = 0) then
                        begin
                            with DriveResults [result], IdentifyDeviceData^ do
                            begin
                                DriveNum := drivenr ;
                                ModelNumber := ChangeByteOrders (@sModelNumber, Length (sModelNumber)) ;
                                SerialNumber := ChangeByteOrders (@sSerialNumber, Length (sSerialNumber)) ;
                                FirmwareRev := ChangeByteOrders (@sFirmwareRev, Length (sFirmwareRev)) ;
                                SectorNum := Int64 (ulTotalAddressableSectors) ;  // LBA28 size, max 128 gigs
                                SecSizeLogical := 512 ;
                                SecSizePhysical := SecSizeLogical ;
                                for J := 1 to 3 do VendorUnique [J] := wVendorUnique [J] ;
                                VendorUnique [4] := sVendorUnique2 ;
                                SmartSupport := (wCommandSetSupport1 AND CmdSet1SmartCommands) = CmdSet1SmartCommands ;
                                SmartEnabled := (wCommandSetActive1 AND CmdSet1SmartCommands) = CmdSet1SmartCommands ;
                                LBA48Support := (wCommandSetSupport2 AND CmdSet2BigLba) = CmdSet2BigLba ;
                                MajorRev := wMajorRevision ;
                                MinorRev := wMinorRevision ;
                                AtaVersion :=  GetATAMajorVersion (MajorRev) ;
                                if wSataVersion <> 0 then
                                    SataVersion :=  GetSATAVersion (wSataVersion) + ', max ' +
                                                    GetSATASpeed (wSataMaxSpeed) + ', cur ' + GetSATASpeed (wSataCurSpeed) ;
                                if LBA48Support then
                                begin
                                    SectorNum := ullMax48BitLBA ;   // LBA48 size, larger than 128 gigs
                                    if (wPhysicalLogicalSectorSize AND PhysLogSectSizeLogicalSectorLongerThan256Words) =
                                                                            PhysLogSectSizeLogicalSectorLongerThan256Words then
                                    begin
                                        SecSizeLogical := ulWordsPerLogicalSector ;
                                    end;
                                    J := wPhysicalLogicalSectorSize AND PhysLogSectSizeLogicalSectorsPerPhysicalSector ;
                                    if (wPhysicalLogicalSectorSize AND PhysLogSectSizeMultipleLogicalSectorsPerPhysicalSector) =
                                                                       PhysLogSectSizeMultipleLogicalSectorsPerPhysicalSector then
                                    begin
                                         SecSizePhysical := SecSizeLogical * J ;
                                    end;
                                end;
                                CapacityNum := SectorNum * SecSizeLogical ;
                                IdentifyDevice := IdentifyDeviceData^ ;
                             end ;
                        end
                        else
                            DriveResults [result].ErrInfo := 'IOCTL_SCSI_MINIPORT Command Failed: ' +
                                                                                 SysErrorMessage (GetLastError) ;
                        inc (result) ;
                    end;
                end;
            end;
        finally
            CloseHandle (hSMARTIOCTL) ;
        end;
    end;
end ;

(* ************************************************************************* *)


end.
