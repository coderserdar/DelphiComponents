unit MagTapiW;

{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}

interface
// Include the resource with the bitmap. Note bitmap
// must have same name as the class
{$R MAGTAPI.RES}

uses
  Windows, SysUtils, Classes, Controls, Registry,
  MagTapiApiW, MagTapiStr, MagSubs1 ;

const
  MagVersion = 'TMagTapi, 6th June 2017 - Release 6.40, Copyright 2017, Magenta Systems Ltd' ;

{
Magenta TAPI Functions
Copyright by Angus Robertson, Magenta Systems Ltd, England
delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/

Changes in 1.2 ===
ignore most VPN modems, since Windows 2000 has 1,000 of them
use Device properly since array elements are now missing
added GetCallStatus and DeallocateCall properties
!!! GONE  process some CallInfo messages to check for end of call (some ISDN stuff
  do not send an idle callstate)
TTapiStatusEvent now has call handle to allow deallocation on error

Changes in 1.3
renamed API file

Changes in 1.4 - 3rd Oct 1999
some integers now DWORD to keep compiler happy
on Magenta Systems property tab

Changes in 1.5 - 11th Nov 1999
allow disconnection detail again
added GetModemCaps, mainly for diagnostics on strange device
W2K does not need special ISDN handling, but names still do not match RAS
added DevType similar to RAS devicetype, ie modem, isdn, vnp
get comport from Device Class (classname not returned for ISDN...)

Changes in 4.4 - 13th April 2000
None really, but version made common with TMagRas stuff

Changes in 4.42 - 1st October 2000
NT some ISDN had channel names starting from 0 not 1, so now allocate
ports sequentially, 1/2/3/4 Win9x/NT, 2/1/4/3 W2K
minor change to ensure call not returned if deallocated
increased VPN modems to 30
OnTapiStatus now sets tinfo2 to disconnect reason

Changes in 4.50
Literals moved to resource strings for translation in magtapistr.pas

Changes in 4.51
Only de-allocate calls in idle event, not disconnected

Changes in 4.60
Moved some common functions to MagSubs1
Added extra translations to W2K VPN and Parallel device names/ports match RAS devices
Makes sure PortStr is always upper case
No special handling for W2K ports, removed W2K extra VPN checking since MS removed them

Changes in 4.93
Better error if TAPI monitoring fails

Changes in 5.4
Allow COM ports high than 9, added numeric port to diag callback
Report error if Get Capabilities fails
Don't ignore device if failed
Fixed lineNegotiateApiVersion failed returned wrong error string
Made compatible with Delphi 2009, but still using ANSI RAS functions, not Unicode

Changes in 6.00
Using unicode W structures and functions to support Delphi 2009

Changes in 6.20
Fixed cast warnings for Delphi 2009 and later

Changes in 6.30
RegisterComponent now in magrasregw

}

type
    TModemObject = record
       Name: String ;
       Line: TLine;
       Media: DWORD ;
       RegKey: String ;
       PortStr: String ;
       PortNr: integer ;
       Device: DWORD ;
       TAPIName: String ;
       Provider: String ;
       ApiVersion: DWORD ;
       DevType: String ;
    end ;
    PTModemObject = ^TModemObject;

{TAPI status event}
    TTAPIStatusEvent = procedure (state, devid, callh: DWORD;
                                mess, tinfo1, tinfo2: string) of object;

{TAPI diag event when locating modems}
    TTAPIDiagEvent = procedure (mess: string) of object;

var
    ObjTAPI: TObject ;   // public variable to make TMagTAPI available to callback

const
    MaxVPNModems = 30 ;

type

  TMagTAPI = class(TComponent)
  private
    { Private declarations }
    fLineApp: TLineApp ;    // TAPI application handle
    fRunning: boolean ;     // is TAPI running
    fModemInst: DWORD ;   // configured devices, installed
    ModemList: TList ;      // list of modem objects
    fCount: Integer ;       // number of modem objects
    fTapiStatus: TTapiStatusEvent ;
    fTapiDiag: TTapiDiagEvent ;
    fVersion: string ;

  protected
    { Protected declarations }
    Function GetAModem (Index: Integer) : TModemObject;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearModemList ;
    function  ConfigDialog (parh: HWND; devid: integer) : Boolean;
    procedure GetModemList ;
    function  GetModemDevice (NewName: String): integer ;
    function  InitCalls: integer;
    function  MonitorOne (num: integer): boolean ;
    procedure StartMonitor ;
    function  StartTAPI: integer ;
    procedure StopMonitor ;
    procedure StopTAPI ;
    function  TranslateAddr (devid: integer; inputnum: String;
                                var disnum, dialnum: String) : Boolean;
    function  TranslateDialog (parh: HWND; devid:
                                        integer; addr : String) : Boolean;
    property  Count : Integer read fCount;
    property  InstCount : DWORD read FModemInst ;
    property  Modems [index : Integer]: TModemObject read GetAModem;
    property  Running: boolean read fRunning ;
    function  CheckModems: integer ;
    function  GetCallStatus (hCall: TCall): integer ;
    procedure DeallocateCall (hCall: TCall) ;
    function GetModemCaps (devnr: integer; var LineCaps: TLineDevCaps): boolean ;

  published
    { Published declarations }
    Property Version: string read fVersion write fversion stored False ;
    property OnTAPIStatus: TTapiStatusEvent read fTapiStatus write fTapiStatus ;
    property OnTAPIDiag: TTapiDiagEvent read fTapiDiag write fTapiDiag ;
  end;

//procedure Register;

implementation


// main callback whenever a TAPI event happens on an open line

procedure lineCallback (hDevice, dwMsg, dwCallbackInstance,
                            dwParam1, dwParam2, dwParam3: DWORD); stdcall;
var
    callstr, tinfo1, tinfo2 : string;
    devid, callstate: DWORD ;
    hCall, origCall: TCall;
    callinfo: TLineCallInfo ;
    callstatus: TLineCallStatus ;
    TAPIOBJ: TMagTAPI absolute ObjTAPI ; // cludge because this is a callback ;

    procedure docallinfo ;
    begin
        if hcall = 0 then exit ;
        callinfo.dwTotalSize := sizeof (callinfo.data) ;
        if lineGetCallInfoW (hCall, callinfo) = 0 then
        with callinfo do
        begin

          // get application that started connects, RASAPI32, RASCOM, AMEOL2, etc
            if dwAppNameSize > 1 then
            begin
                tinfo1 := Trim (FixedToPasStrW (@callinfo.Data [dwAppNameOffset],
                                                             dwAppNameSize div 2)) ;
            end ;

        // get connection speed, ie 56000 - only valid once connected
            tinfo2 := IntToStr (callinfo.dwRate) ;
        end;
    end ;

    procedure ProcCallState ;
    begin
        case callstate of
            LINECALLSTATE_IDLE:     // 1 (01) call terminated
                if hcall <> 0 then
                begin
                    lineDeallocateCall (hCall) ; // deallocate the monitored call
                    hCall := 0 ;
                    callstr := STapiCsIdle ;
                end;
            LINECALLSTATE_OFFERING:     // 2 (02)  Offering incoming call
                callstr := STapiCsOffering ;
            LINECALLSTATE_ACCEPTED:     // 4  04) Accepted incoming call
                callstr := STapiCsAccepted ;
            LINECALLSTATE_DIALTONE:     // 8 (08) only some modems
                callstr := STapiCsDialtone ;
            LINECALLSTATE_DIALING:      // 16 (10) call dialing
                begin
                    callstr := STapiCsDialing ;
                    docallinfo ;    // get application
                end ;
            LINECALLSTATE_RINGBACK:     // 32 (20) ringback
                callstr := STapiCsRingback ;
            LINECALLSTATE_BUSY:         // 64 (40) busy - param2 has more info
                callstr := STapiCsBusy ;
            LINECALLSTATE_SPECIALINFO:  // 128 (80) special info - failed
                callstr := STapiCsSpecialInfo ;
            LINECALLSTATE_CONNECTED:    // 256 (100) Service connected
                begin
                    callstr := STapiCsConnected ;
                    docallinfo ;    // get application and speed
                end;
            LINECALLSTATE_PROCEEDING:      // 512 (200) call proceeding (dialing)
                begin
                    callstr := STapiCsProceeding ;
                    docallinfo ;    // get application and speed
                end;
            LINECALLSTATE_ONHOLD:           // 1024 (400) hold
                callstr := STapiCsOnHold ;
            LINECALLSTATE_CONFERENCED:      // 2048 (800)
                callstr := STapiCsConferenced ;
            LINECALLSTATE_ONHOLDPENDCONF:   // 4096 (1000)
                callstr := STapiCsOnHoldPendConf ;
            LINECALLSTATE_ONHOLDPENDTRANSFER:  // 8192 (2000)
                callstr := STapiCsOnHoldPendTransfer ;
            LINECALLSTATE_DISCONNECTED:    // 16384 (4000) Disconnected }
                begin
               {     if hcall <> 0 then
                    begin
                        lineDeallocateCall (hCall) ; // deallocate the monitored call
                        hCall := 0 ;
                    end;            }
                    callstr := STapiCsDisconnected ;
                    case dwParam2 of
                        LINEDISCONNECTMODE_NORMAL:  callstr := STapiDmNormal ;
                        LINEDISCONNECTMODE_UNKNOWN:  callstr := STapiDmUnknown ;
                        LINEDISCONNECTMODE_REJECT:  callstr := STapiDmReject ;
                        LINEDISCONNECTMODE_PICKUP:  callstr := STapiDmPickup ;
                        LINEDISCONNECTMODE_FORWARDED:  callstr := STapiDmForwarded ;
                        LINEDISCONNECTMODE_BUSY:  callstr := STapiDmBusy ;
                        LINEDISCONNECTMODE_NOANSWER:  callstr := STapiDmNoAnswer ;
                        LINEDISCONNECTMODE_NODIALTONE:  callstr := STapiDmNoDialTone ;
                        LINEDISCONNECTMODE_BADADDRESS:  callstr := STapiDmBadAddress ;
                        LINEDISCONNECTMODE_UNREACHABLE:  callstr := STapiDmUureachable ;
                        LINEDISCONNECTMODE_CONGESTION:  callstr := STapiDmConjestion ;
                        LINEDISCONNECTMODE_INCOMPATIBLE:  callstr := STapiDmImcompatible ;
                        LINEDISCONNECTMODE_UNAVAIL:  callstr := STapiDmUnavail ;
                    end ;
                    tinfo2 := IntToStr (dwParam2) ;  // keep reason for event handle - 4.42
                end ;
            LINECALLSTATE_UNKNOWN:  // 32K (8000) unknown - seems to indicate TAPI about to dial
            begin
                callstr :=STapiCsUnknown ;
                docallinfo ;    // get application
            end ;
        end;
    end ;

begin
    callstate := 0 ;
    callstr := '' ;
    tinfo1 := '' ;
    tinfo2 := '' ;
    devid := dwCallBackInstance ;    // must be set when line is opened
    hCall := TCall (hDevice);        // handle to call
    origCall := hCall ;

// change in call info  - probably call owners changed so check state
    if dwMsg = LINE_CALLINFO then
    begin
        callstatus.dwTotalSize := sizeof (callstatus.data) ;
        if lineGetCallStatus (hCall, callstatus) = 0 then
        begin
            callstate := callstatus.dwCallState ;
            ProcCallState ;
        end
        else
        begin
            callstr := STapiGenCallEnded ;  // Call Ended
            callstate := 0 ;
        end ;
        if Assigned (TAPIOBJ.fTapiDiag) then
            TAPIOBJ.fTapiDiag (Format (STapiGenCIDiag, [devid, origCall,
                    dwParam1, callstate, dwParam2, callstr])) ;
        {   TAPIOBJ.fTapiDiag ('TAPI Line_CallInfo, device ' +
                InttoStr (devid) + ' handle ' + InttoStr (origCall) + ', Parms ' +
                InttoHex (dwParam1, 4) + 'h, CallState = ' +
                InttoHex (callstate, 4) + 'h, ' + InttoHex (dwParam2, 4) +
                    'h = ' + tinfo1 + ' ' + tinfo2 + ' (' + callstr + ')') ;  }

    // suppress events for active calls
        if (dwParam1 = LINECALLINFOSTATE_NUMOWNERDECR) then
        begin
            if (callstate >= LINECALLSTATE_OFFERING) and
                 (callstate < LINECALLSTATE_DISCONNECTED) then callstr := '' ;
        end ;
        callstr := '' ;   // !!!! STOP ALL CALLINFO EVENTS !!!!
    end ;

// change in line state
    if dwMsg = LINE_CALLSTATE then
    begin
        callstate := dwParam1 ;
        ProcCallState ;
        if Assigned (TAPIOBJ.fTapiDiag) then
            TAPIOBJ.fTapiDiag (Format (STapiGenCSDiag, [devid, origCall,
                   callstate, dwParam2, dwParam3, tinfo1, tinfo2, callstr])) ;
         {   TAPIOBJ.fTapiDiag ('TAPI Line_CallState, device ' +
                InttoStr (devid) + ' handle ' + InttoStr (origCall) +
               ', Parms ' + InttoHex (callstate, 4) +
                 'h (' + InttoHex (dwParam2, 4) + 'h = ' +
                    InttoStr (dwParam3) + ') ' + tinfo1 + ' ' + tinfo2 +
                                                    ' (' + callstr + ')') ;  }
    end;
    if callstr <> '' then
    begin
        if Assigned (TAPIOBJ.fTapiStatus) then
                    TAPIOBJ.fTapiStatus (callstate, devid, hCall,
                                                callstr, tinfo1, tinfo2) ;
    end ;
end;

function  strLastCh(const S: String): Char;
begin
    result := #0 ;
    if length (S) <> 0 then Result := S [Length (S)];
end;

function regReadString(aKey: HKEY; const Path: String): String;
var
    aRegistry : TRegistry;
    aPath     : String;
    aValue    : String;
begin
    aRegistry:=TRegistry.Create;
    try
        with aRegistry do
        begin
            RootKey:=aKey;
            aPath:=Path;
            aValue:='';
            while (Length(aPath)>0) and (strLastCh(aPath)<>'\') do
            begin
                aValue:=strLastCh(aPath)+aValue;
                Delete(aPath,Length(aPath),1);
            end;
            OpenKey(aPath,True);
            Result:=ReadString(aValue);
        end;
    finally
        aRegistry.Free;
    end;
end;

constructor TMagTAPI.Create(AOwner: TComponent);
begin
    inherited;
    ObjTAPI := self ;    // keep component OBJ for use by callback

    if NOT LoadTAPI then exit ;
    ModemList := TList.Create;
    fCount := 0;
    fRunning := false ;
    fVersion := MagVersion ;
end ;

destructor TMagTAPI.Destroy;
begin
    StopTAPI ;
    ClearModemList ;
    ModemList.Free ;
    inherited;
end;

//  Initialize TAPI, returning number of modems installed

function TMagTAPI.StartTAPI: integer ;
var
    APIVersion: DWORD ;
    LineInitializeExParams: TLineInitializeExParams ;
begin
    result := 0 ;
    fRunning := false ;
    if fLineApp = 0 then
    begin
//        if lineInitialize (fLineApp, HInstance, lineCallback,
//                                       nil, fModemInst) <> 0 then  // 0 is OK
        APIVersion := TAPI_CURRENT_VERSION; 
        LineInitializeExParams.dwTotalSize := SizeOf (TLineInitializeExParams) ;
        LineInitializeExParams.dwNeededSize := SizeOf (TLineInitializeExParams) ;
        LineInitializeExParams.dwUsedSize := SizeOf (TLineInitializeExParams) ;
        LineInitializeExParams.dwOptions := LINEINITIALIZEEXOPTION_USEHIDDENWINDOW; 
        if lineInitializeExW (fLineApp, HInstance, lineCallback,
                          nil, fModemInst, APIVersion, LineInitializeExParams) <> 0 then  // 0 is OK  6.40
        begin
            if Assigned (fTapiDiag) then fTapiDiag (STapiGenInitFail) ;   // TAPI Initialise Failed
            fLineApp := 0 ;
            exit ;
        end
        else
        begin
            if fModemInst = 0 then      // no devices
            begin
                lineShutDown (flineApp);
                if Assigned (fTapiDiag) then fTapiDiag (STapiGenNoModems) ;  // TAPI No Modems
                fLineApp := 0;
                exit ;
            end ;
        end ;
    end ;
    if Assigned (fTapiDiag) then
        fTapiDiag (Format (STapiGenModemTot, [fModemInst])) ;   // TAPI Modems Total
    fRunning := true ;
    result := fModemInst ;
end ;

// terminate TAPI

procedure TMagTAPI.StopTAPI ;
begin
    if fLineApp <> 0 then
    begin
        StopMonitor ;
        lineShutDown (flineApp);   // TAPI shutdown
        flineApp := 0 ;
        fRunning := false ;
    end ;
end ;

// clear modem list

procedure TMagTAPI.ClearModemList ;
var
    I: integer ;
begin
    if ModemList.Count > 0 then
    begin
        for I := 0 to (ModemList.Count - 1) do
                        Dispose (PTModemObject (ModemList [I]));
    end;
    ModemList.Clear ;
    fCount := 0;
end;

// stop monitoring all active lines

procedure TMagTAPI.StopMonitor ;
var
    I, line: integer ;
begin
    if fCount <> 0 then
    begin
        for I := 0 to (fCount - 1) do
        begin
            line := Modems [I].Line ;
            begin
                if line <> 0 then lineClose (line);
                PTModemObject (ModemList [I])^.Line := 0 ;
            end ;
        end ;
    end ;
end;

// get object for specified modem in list

function TMagTAPI.GetAModem (Index: Integer): TModemObject;
begin
    Result := PTModemObject (ModemList[Index])^;
end;

// get list of modems used by TAPI

procedure TMagTAPI.GetModemList ;
var
    I, Devportnr, J, isdnbase, retval: integer;
    LineCaps: TLineDevCaps;
    LineExt: TLineExtensionID;
    DeviceID: TVarString ;
    DevName, DevDispName, DevProvider, DevSpecific: String ;
    DevComPort, DeviceType, DevCommName, DevRegKey: String ;
    DeviceClass: AnsiString ;
    DevApiVersion, DevMedia, DevLine, retcode: DWORD ;
    bKey: hKey;
    ModemObj: PTModemObject;
begin

// stop monitoring, in case added a new modem, clear old modem list
    if fModemInst = 0 then exit ;   // no installed modems
    StopMonitor ;
    ClearModemList ;
    isdnbase := 1 ;

// Enumerate all line devices and build a list of names
    for I := 0 to (fModemInst - 1) do
    begin

      {Negotiate the API version to use for this device}
        retval := lineNegotiateApiVersion (fLineApp, I,  TAPI_MIN_VERSION,
                                            TAPI_MAX_VERSION, DevApiVersion, LineExt) ;
        if retval <> 0 then
        begin
             if Assigned (fTapiDiag) then fTapiDiag (Format (STapiGenFailedAPI,
                     [I, SysErrorMessage (retval), retval])) ;  // Device x Failed API Negotiation
        // now add modem object to list
            New (ModemObj) ;
            with ModemObj^ do
            begin
                Line := 0 ;
                Name := '(Unavailable)' ;
                portnr := -1 ;
                Device := I ;
            end ;
            ModemList.Add (ModemObj) ;
        end
        else
        begin
            {Get the device capabilities}
            DevName := '' ;
            DevDispName := '' ;
            DevProvider := '?' ;
            DevSpecific := '?' ;
            DevPortnr := -1 ;
            DevCommName := '' ;
            DevComPort := '' ;
            DeviceType := RASDT_Modem ;  // most common
            LineCaps.dwTotalSize := SizeOf (LineCaps) ;
            retval := lineGetDevCapsW (fLineApp, I, DevApiVersion, 0, LineCaps) ;
            if retval <> 0 then
            begin
                if Assigned (fTapiDiag) then fTapiDiag (Format
                         (STapiGenFailedGetCaps, [I, SysErrorMessage (retval), retval])) ;  // 12 June 2008 Device %d Failed Get Capabilities: %s'
            // now add modem object to list
                New (ModemObj) ;
                with ModemObj^ do
                begin
                    Line := 0 ;
                    Name := '(Unavailable)' ;
                    portnr := -1 ;
                    Device := I ;
                end ;
                ModemList.Add (ModemObj) ;
                 continue;
            end ;
            J := 0 ; 
            with LineCaps do
            begin
                if dwStringFormat = STRINGFORMAT_UNICODE then
                begin

                // Extract the device name
                    DevName := Trim (FixedToPasStrW (@LineCaps.Data
                                        [dwLineNameOffset], dwLineNameSize div 2)) ;
                    DevDispName := DevName ;

                // Extract the provider information
                    DevProvider := Trim (FixedToPasStrW (@LineCaps.Data
                                    [dwProviderInfoOffset], dwProviderInfoSize div 2));
                    J := pos (#0, DevProvider) ;
                    if J > 1 then DevProvider [J] := ' ' ;

                  // Extract the registry key - undocumented - Ansi, not Unicode!!!!
                    DevSpecific := String (FixedToPasStr (@LineCaps.Data
                                            [dwDevSpecificOffset], dwDevSpecificSize)); // 11 Aug 2010
                end ;
                DevRegKey := trim (copy (DevSpecific, 9, 99)) ;

                DevMedia := LineCaps.dwMediaModes ;  // 14h=modem, 100h or 102h=isdn

            // check for ISDN card - $100 or $102
                if ((DevMedia and LINEMEDIAMODE_DIGITALDATA) =
                                LINEMEDIAMODE_DIGITALDATA) and
                                    (Copy (DevProvider, 1, 4) = 'ISDN') then
                begin
                    DeviceType := RASDT_Isdn ;
            // see if handling modem name specially, ISDN cards on NT4 only
                    if (Win32Platform = VER_PLATFORM_WIN32_NT) and
                            (Win32MajorVersion = 4) and (J <> 0) then
                                   DevDispName := Copy (DevProvider, J+1, 20) ;

                // get NDIS port number, sequentially
                // NT seems to reverse ordered
                    if isdnbase = 0 then isdnbase := 4 ;  // allow for second card
                    DevComport := 'ISDN' +  IntToStr (isdnbase) ;
                    inc (isdnbase) ;
                end ;

            // check for VPN
                if (Copy (DevProvider, 1, 3) = 'VPN') then
                begin
                    DeviceType := RASDT_Vpn ;
            // see if handling name specially on NT4 only
                    if (Win32Platform = VER_PLATFORM_WIN32_NT) and
                            (Win32MajorVersion = 4) and (J <> 0) then
                                   DevDispName := Copy (DevProvider, J+1, 20) ;
                    DevComport := 'VPN1' ;
                end ;
                if DevName = '' then Devname := STapiGenUnnamed ;  // (Unnamed Device)
                if DevDispName = '' then DevDispName := DevName ;
            end;

        // open device to get class names
            DevLine := 0 ;
            if lineOpen (fLineApp, I, DevLine, DevApiVersion, 0, 0,
               LINECALLPRIVILEGE_NONE, LINEMEDIAMODE_DATAMODEM, Nil) <> 0 then
            begin
                if Assigned (fTapiDiag) then
                        fTapiDiag (Format (STapiGenModemTot, [I])) ; // Device x Failed to Open
            end
            else
            begin

              // get COM port device name - use ANSI version
                FillChar (DeviceID, SizeOf (TVarString), 0);
                DeviceID.dwTotalSize := SizeOf (TVarString);
                DeviceClass := ClassCommModem ;
//                if DeviceType = RASDT_Isdn then DeviceClass := ClassNDIS ;  // temp
                retcode := lineGetIDA (DevLine, 0, 0, LINECALLSELECT_LINE,
                                              DeviceID, PAnsiChar(DeviceClass)) ;
                if retcode = 0 then
                begin
                    with DeviceId do
                    begin
                        if dwStringSize > 4 then // returns STRINGFORMAT_BINARY data
                        begin
                            DevCommName := Trim (String (FixedToPasStr
                                    (@DeviceId.Data [dwStringOffset + 4], (dwStringSize - 4)))) ; // 11 Aug 2010
                        end ;
                    end ;
                end ;

              // get com port - use ANSI version
                if DeviceType <> RASDT_Isdn then
                begin
                    FillChar (DeviceID, SizeOf (TVarString), 0);
                    DeviceID.dwTotalSize := SizeOf (TVarString);
                    DeviceClass := ClassCommModemPort ;
                    retcode := lineGetIDA (DevLine, 0, 0, LINECALLSELECT_LINE,
                                              DeviceID, PAnsiChar(DeviceClass)) ;
                    if retcode = 0 then
                    begin
                        with DeviceId do
                        begin
                            if dwStringSize > 0 then  // returns STRINGFORMAT_ASCII
                            begin
                                DevComPort := Uppercase (Trim (String (FixedToPasStr
                                    (@DeviceId.Data [dwStringOffset], dwStringSize)))) ; // 11 Aug 2010
                            end ;
                        end ;
                    end ;
                end ;
            end ;
            if DevLine <> 0 then lineClose (DevLine);

        // try and get com port from registry - if not got it yet
            if ((DevMedia AND LINEMEDIAMODE_DATAMODEM) <> 0) and
                                                        (DevComPort = '') then
            begin
                if RegOpenKey (HKEY_LOCAL_MACHINE, PChar (DevRegKey),   // deliberately PChar
                                                bKey) = ERROR_SUCCESS then
                begin
                    DevComPort := Uppercase (regReadString (bKey, 'AttachedTo')) ;
                    RegCloseKey (bKey);
                end;
            end ;
            if (copy (DevComport, 1, 3) = 'COM') and (length (DevComport) >= 4) then // 12 June 2008 COM1 to COM999
                                      DevPortnr := strToInt (copy (DevComport, 4, 3)) ;
            if (DevComport = '') and ((DevMedia and LINEMEDIAMODE_DATAMODEM) =
                                        LINEMEDIAMODE_DATAMODEM) then  // $10
            begin
                DevPortnr := 0 ;  // plug and play no port yet
                DevComport := 'PnP' ;
            end ;

        // check for special devices on Windows 2000, RAS uses very strange names
            if (Win32Platform = VER_PLATFORM_WIN32_NT) and
                                        (Win32MajorVersion = 5) then
            begin
                if Pos ('VPN', DevProvider) > 0 then
                begin
                    if DevProvider = 'VPN RASPPTP' then     // Angus 4.60
                                        DevDispName := 'WAN Miniport (PPTP)' ;
                    DeviceType := RASDT_Vpn ;
                    DevComport := 'VPN1' ;
                end ;
                if DevProvider = 'VPN RASPPPOE' then     // Angus 4.60
                begin
                    DevDispName := 'WAN Miniport (PPPOE)' ;   // Windows XP
                    DeviceType := RASDT_PPPoE ;
                    DevComport := 'PPPoE1' ;
                end ;
                if Pos ('L2TP', DevDispName) > 0 then            // Angus 4.60
                begin
                    DeviceType := RASDT_Vpn ;
                    DevComport := 'VPN1' ;
                end ;
                if Pos ('LPT1', DevDispName) > 0 then            // Angus 4.60
                begin
                    DevDispName := 'Direct Parallel' ;
                    DeviceType := RASDT_Parallel ;
                    DevComport := 'LPT1' ;
                end ;
                if DevProvider = 'ISDN AVMWAN' then             // Angus 4.60
                                  DevDispName := 'AVM NDIS WAN CAPI Driver' ;
            end ;

        // still no port, set to (None)
            if (DevComport = '') then DevComport := STapiGenNone ; // (None)

        // now add modem object to list
            New (ModemObj) ;
            with ModemObj^ do
            begin
                Line := 0 ;
                Name := trim (DevDispName) ;
                TAPIName := DevName ;
                Provider := DevProvider ;
                ApiVersion := DevApiVersion ;
                Media := DevMedia ;
                RegKey := DevRegKey ;
                portnr := DevPortnr ;
                PortStr := DevComPort ;
                Device := I ;
                DevType := DeviceType ;
            end ;
            ModemList.Add (ModemObj) ;

        // tell user what we found
            if Assigned (fTapiDiag) then
             begin
                fTapiDiag (Format (STapiGenDevDiag, [I, DevName, DevDispName,
                           DeviceType, DevProvider, DevCommName, DevMedia,
                                    DevComPort, DevPortnr, DevApiVersion, DevRegKey])) ;
            end ;
        end;
    end;
    fCount := ModemList.Count ;
    if Assigned (fTapiDiag) then
    begin
        DevName := Format (STapiGenModemListed, [fCount]) ;  // TAPI Modems Listed x
        fTapiDiag (DevName) ;
    end ;
end ;

// start monitoring a single modem

function TMagTAPI.MonitorOne (num: integer): boolean ;
var
    line: DWORD ;
    I: integer ;
begin
    I := Modems [num].Device ;
    if lineOpen (fLineApp, I, line, Modems [num].ApiVersion, 0, I,
           LINECALLPRIVILEGE_MONITOR, LINEMEDIAMODE_DATAMODEM, Nil) <> 0 then
    begin
        line := 0 ;
        if Assigned (fTapiDiag) then
                    fTapiDiag (Format (STapiGenMonFailed, [I])) ; // Device x Failed to Monitor
        result := false ;
    end
    else
    begin
        if Assigned (fTapiDiag) then
            fTapiDiag (Format (STapiGenMonStart, [I, line])) ; // Device x Started Monitoring, Line y
        result := true ;
    end ;
    PTModemObject(ModemList [num])^.Line := line ;
//  Modems [num].Line : = line ;  // better, but not implemented yet
end ;

// start monitoring all modems

procedure TMagTAPI.StartMonitor ;
var
    num: integer ;
begin
    if ModemList.Count > 0 then
    begin
        for num := 0 to (ModemList.Count - 1) do MonitorOne (num) ;
    end;
end ;

// get the device for named modem

function TMagTAPI.GetModemDevice (NewName: String): integer ;
var
    num: integer ;
begin
    result := -1 ;
    if NewName = '' then exit ;
    if fCount = 0 then exit ;
    for num := 0 to (fCount - 1) do
    begin
        if (Modems [num].Name = NewName) then
        begin
            result := Modems [num].Device ;
            exit ;
        end ;
    end ;
end ;

// create events for any existing calls

function TMagTAPI.InitCalls: integer;
var
    num, calls, ret: integer ;
    xline: TLine ;
    calllist: TLineCallList ;
    callstatus: TLineCallStatus ;
    hcall: TCall ;
begin
    result := 0 ;
    calls := 0 ;
    if fCount = 0 then exit ;

// loop each modem, checking for calls
    for num := 0 to (fCount - 1) do
    begin
        xline := Modems [num].Line ;
        if xline <> 0 then
        begin
            calllist.dwTotalSize := sizeof (calllist.data) ;
            hcall := 0 ;
            ret := lineGetNewCalls (xline, 0, LINECALLSELECT_LINE, calllist) ;
            if ret = 0 then
            with calllist do
            begin
                if dwCallsNumEntries <> 0 then
                begin
                    Move (calllist.Data [dwCallsOffset], hcall, sizeof (hcall));
                end ;
            end
            else
            begin
                if Assigned (fTapiDiag) then fTapiDiag (Format (STapiGenMonFailed, // Device x Failed Check For New Calls
                            [Modems [num].Device]) + ': ' + Modems [num].TAPIName +
                                                   ' - Err $' + IntToHex (ret, 8)) ;
            end ;

        // get status of new call, create event for it
            if hcall <> 0 then
            begin
                inc (calls) ;
                callstatus.dwTotalSize := sizeof (callstatus.data) ;
                if lineGetCallStatus (hCall, callstatus) = 0 then
                with callstatus do
                begin
                    lineCallback (hCall, LINE_CALLSTATE, Modems [num].Device,
                                            dwCallState, dwCallStateMode, 99);
                end ;
            end;
        end ;
    end ;
    result := calls ;
end;

// check each modem for calls, mainly looking for errors if line handle is invalid

function TMagTAPI.CheckModems: integer ;
var
    num, ret: integer ;
    xline: TLine ;
    calllist: TLineCallList ;
    hcall: TCall ;
begin
    result := 0 ;
    if fCount = 0 then exit ;

// loop each modem, checking for calls - stop on new call
    for num := 0 to (fCount - 1) do
    begin
        xline := Modems [num].Line ;
        if xline <> 0 then
        begin
            calllist.dwTotalSize := sizeof (calllist.data) ;
            hcall := 0 ;
            ret := lineGetNewCalls (xline, 0, LINECALLSELECT_LINE, calllist) ;
            if ret = 0 then
            begin
                with calllist do
                begin

          // not really expecting a new call, since we should have seen event
                    if dwCallsNumEntries <> 0 then
                    begin
                        Move (calllist.Data [dwCallsOffset],
                                                    hcall, sizeof (hcall));
                        if hcall <> 0 then
                            lineDeallocateCall (hCall) ; // deallocate the monitored call
                        result := 1 ;
                        exit ;
                    end ;
                end ;
            end
            else
            begin
                if Assigned (fTapiDiag) then fTapiDiag (Format (STapiGenMonFailed, // Device x Failed Check For New Calls
                            [Modems [num].Device]) + ': ' + Modems [num].TAPIName +
                                                   ' - Err $' + IntToHex (ret, 8)) ;
                result := -1 ;
                exit ;
            end ;
        end ;
    end ;
end;

// display the Translate dialog box

function TMagTAPI.TranslateDialog (parh: HWND; devid:
                                    integer; addr : String) : Boolean;
var
    addrw: WideString ;
begin
    addrw := addr ;
    Result := (lineTranslateDialogW (fLineApp, devid, TAPI_CURRENT_VERSION,
                                                     parh, PWideChar(Addrw)) = 0);
end;

// display the modem configure dialog box

function TMagTAPI.ConfigDialog (parh: HWND; devid: integer) : Boolean;
begin
    Result := (lineConfigDialogW (devid, parh, nil) = 0);
end;

// translate a canonical number into displayable and dialling formats

function TMagTAPI.TranslateAddr (devid: integer; inputnum: String;
                                var disnum, dialnum: String) : Boolean;
var
    TransOutput: TLineTranslateOutput ;
    inputnumw: WideString ;
begin
    result := false ;
    disnum := '' ;
    dialnum := '' ;
    if inputnum = '' then exit ;
    TransOutput.dwTotalSize := sizeof (TransOutput.data) ;
    try
        inputnumw := inputnum ;
        if lineTranslateAddressW (fLineApp, devid, TAPI_CURRENT_VERSION,
                                  PWideChar(inputnumw), 0, 0, TransOutput) = 0 then
        begin
        with TransOutput do
        begin
            disnum := Trim (FixedToPasStrW (@TransOutput.Data
                    [dwDisplayableStringOffset], dwDisplayableStringSize div 2));
            if length (disnum) > 4 then     // change 0 845 xxx into 0845 xxx
            begin
                if copy (disnum, 1, 2) = '0 ' then delete (disnum, 2, 1) ;
            end ;
            dialnum := Trim (FixedToPasStrW (@TransOutput.Data
                            [dwDialableStringOffset], dwDialableStringSize div 2));
            result := true ;
        end;
    end ;
    except
    end ;
end;

// state of current active call, negative is error
// require handle to call (not device)

function TMagTAPI.GetCallStatus (hCall: TCall): integer ;
var
    callstatus: TLineCallStatus ;
begin
    callstatus.dwTotalSize := sizeof (callstatus.data) ;
    result := lineGetCallStatus (hCall, callstatus) ;
    if result = 0 then result := callstatus.dwCallState ;
end ;

// deallocate the monitored call

procedure TMagTAPI.DeallocateCall (hCall: TCall) ;
begin
    if hcall = 0 then exit ;
    lineDeallocateCall (hCall) ;
end ;

// get device capabilites for single modem - for diagnostic purposes

function TMagTAPI.GetModemCaps (devnr: integer; var LineCaps: TLineDevCaps): boolean ;
var
    DevApiVersion: DWORD ;
    LineExt: TLineExtensionID;
begin
    result := false ;
    if lineNegotiateApiVersion (fLineApp, devnr, TAPI_MIN_VERSION,
                     TAPI_MAX_VERSION, DevApiVersion, LineExt) <> 0 then exit ;
    LineCaps.dwTotalSize := SizeOf (LineCaps) ;
    result := (lineGetDevCapsW (fLineApp, devnr, DevApiVersion, 0, LineCaps) = 0) ;
end ;

//procedure Register;
//begin
//  RegisterComponents('Magenta Systems', [TMagTAPI]);
//end;

end.
