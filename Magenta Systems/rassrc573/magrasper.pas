unit magrasper;
{$IFNDEF VER140}
  {$WARN UNSAFE_TYPE off}
  {$WARN UNSAFE_CAST off}
  {$WARN UNSAFE_CODE off}
{$ENDIF}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}

// this component includes comments formatted for creation of a help file
// using F1HELP.  // is ignored, { } is used for help, (* *) is ignored

{-----------------------------------------------------
DELPHI RAS COMPONENT - Performance Statistics - Part 2 of 3
(C) 2013 Magenta Systems Ltd

Created by Angus Robertson, Magenta Systems Ltd, England
in 1998, delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

Compatible with Delphi 7, 2007, 2009, 2010, XE, XE2, XE3, XE4,
supports Windows XP, 2003, Vista, 2008, 7, 8 and 2012,
32-bit and 64-bit editions. Win32 and Win64 binaries only.

TMagRas is a set of installable Delphi non-visual components,
supplied with a demo program, for accessing Dial Up Networking
or Remote Access Services functions.  This is a major update
of Daniel Polistchuck's and Mike Armstrong's earlier TRAS component
(little of which remains).  It allows Delphi developers to add full
RAS functionality to their applications, including dialling and
monitoring multiple connections, creating and editing phonebooks
(without using Windows dialogs), and getting performance information for
connections.  TMagRas supports Windows XP and later.

TMagRas is supplied as three separate Delphi components, TMagRasCon
which includes dialling and monitoring and limited connection editing,
TMagRasPer with performance statistics, and TMagRasEdt to create and
edit connections/phonebooks.  A demo program illustrates use of the
components, including monitoring multiple connections, creating a
simple new connection with minimal properties, and editing detailed
connection properties (seven tabs of settings) including Windows 2000
extensions.  Seven example programs illustrate various aspects of the
components and may be copied to quickly add specific RAS functionality
to applications.

TMagRas is copyrighted software, but the compiled component DCUs may be
used without cost in applications for use by the developer or within
their company but which are not otherwise distributed (including
freeware and shareware).  The component source code may be licensed for
a cost of £100 (UKP) (£117.50 with tax), which is about $200 or 147 euro
less tax.  Developers that license the source may distribute applications
using TMagRas.

Since TMagRasCon does need design time properties, it need not really be
installed on the component palette, but may be created in the application
as needed, as shown in the demo program.


Known Problems
--------------

Please note that getting performance statistics from Windows 95/98 can
sometimes be difficult.  This component now searches the registry for any
Dial-Up Adaptors and will default to the first found.  If there are two or
more adaptors installed, I'm not currently sure how to determine which is
being used, but a list is returned and a property may be changed to select
one. To get performance statistics running, make sure that Connection
Properties, Server Types, Record a Log File is ticked, and in Modem
Properties, Options, Display Modem Status is ticked.  You may need to
reboot after these changes and then make a connection, at which point the
correct registry keys should be created.  Users have reported difficulties
geting performance statistics from DUN version earlier than 1.2, so this
is recommended.

(*
Changes in 4.00
Statistics are now handled for separate devices on NT4 and W2K
A single call gets statistics for all devices, which are read from
   the arrays.  Element 0 is info for all devices.

Changes in 4.10
Reset clears all arrays so stats restart correctly on NT4
Two adaptors supported on Win9x, allocated sequentially
KeyDUNAdap property removed since always read from registry

Changes in 4.31
Added ErrInfo property that displays more error information if i
initialisation fails

Changes in 4.40
Conditional compile as TComponent again to allow use as NT service

Changes in 4.41
Graphics and Control units now conditional as well to ease linking

Changes in 4.42
Optionally using PDH.DLL for NT stats - when registry versions fails
Tests show that only the RAS Total object returns any data, so the RAS
Port object code is currently disabled

Changes in 4.50
Literals moved to resource strings for translation in magrasstr.pas

Changes in 4.51
Minor change for Delphi 3 compatibility

Changes in 4.62
Allow for Win9x DWORD counters wrapping after 4 gigs of total data since a
reboot, but maximum data for a session is 4 gigs.  NT4/W2K should have been
4 gigs already, but that assumes the Microsoft APIs wrap correctly.
Yes, 4 gigs on a modem is a lot, but ADSL devices also use RAS and can
do that in 24 hours.
Supporting Windows XP, exactly the same as W2K

Changes in 4.91
Replaced MaxDevices with MaxDevs since not really locked to devices from W2K and better

Changes in 5.10
Supporting Windows Vista (and maybe Longhorn)

Changes in 5.30
None, but note received and sent counters stop when they reach 4 gigs,
and there may be range overflow if the totals for multiple connections
also totals 4 gigs.  Ideally int64s need to be used with a flag to keep
track of when the DWORDs revert to zero, but this is all very hard to
test, so it's easier to leave a 4 gig restriction for the moment

Changes in 5.40
Made compatible with Delphi 2009, but still using ANSI RAS functions, not Unicode
Many functions deliberately use AnsiString for compatability with PAnsiChar APIs

Changes in 5.60
Fixed cast warnings for Delphi 2009 and later

Changes in 5.70
RegisterComponent now in magrasreg

Changes in 5.71
MagRasOSVersion returns OS7 and OS8 but check >= OSVista


------------------------------------------------------------------------ *)

// following define changes TMagRas from being descended from TComponent
// to TCustomControl, allowing use in ActiveX (but stopping use in NT Service)
{.$DEFINE CUSTCNTL}

interface

//Include the resource with the bitmap. Note bitmap
//must have same name as the class
{$R MAGRASPER.RES}

uses
  SysUtils, Windows, Messages, Classes,
 {$IFDEF CUSTCNTL} Graphics, Controls, {$ENDIF}
  MagRasApi, WinPerf, MagPdhApi, MagRasStr, magsubs1 ;

const
  MagVersion = 'TMagRasPer, 12th August 2013 - Release 5.72, Copyright 2013, Magenta Systems Ltd' ;
  MaxLongWord: DWord = $FFFFFFFF;  // this will probably die on Delphi 3!!!
  MaxDevs = 30 ;   // how many devices supported for stats

type

{$IFDEF CUSTCNTL}
  TMagRasPer = class(TCustomControl)
{$ELSE}
  TMagRasPer = class(TComponent)
{$ENDIF}
  private
    { Private declarations }
{$IFDEF CUSTCNTL}
    fDesignBitmap : TBitmap;
{$ENDIF}
    fDAXCtrl : Boolean;
    fVersion: string ;

    fStatsXmitTot: array [1..4] of DWord ;    // Win9x - value read from registry
    fStatsXmitCon: array [1..4] of DWord ;    // Win9x - value at start of connection
    fStatsRecvTot: array [1..4] of DWord ;
    fStatsRecvCon: array [1..4] of DWord ;
    fKeyDUNConn: AnsiString ;
    fKeyDUNXmit: AnsiString ;
    fKeyDUNRecv: AnsiString ;
    fLastError: integer ;
    fTotPorts: integer ;    // NT4 and W9x, total ports for which data returned
    fTotAdaptors: integer ; // Win9x, total adaptors (from fDialUpAdaptors)
    fDialUpAdaptors: TStringList ;
    fErrInfo: string ;    // more useful error for user

// arrays with performance data, 0 = all, 1 > for separate devices - max 30
    fPerfRasConn: array [0..MaxDevs] of HRasConn ;   // handle for W2K a
    fPerfSubEnt: array [0..MaxDevs] of integer ;     // subentry for W2K
    fPerfXmitCur: array [0..MaxDevs] of DWORD ;
    fPerfRecvCur: array [0..MaxDevs] of DWORD ;
    fPerfConnSpd: array [0..MaxDevs] of DWORD ;      // speed Win9x only
    fPerfPortNam: array [0..MaxDevs] of AnsiString ;     // NT4 com port, W2K device number, W9x connection name

    fPDHXmitHand: array [0..MaxDevs] of HCOUNTER ;  // PDH handle, xmit
    fPDHRecvHand: array [0..MaxDevs] of HCOUNTER ;  // PDH handle, recv

// NT PDH.DLL support - 4.42
    fUsePDHFlag: boolean ;
    fQueryHandle: HQUERY;

{$IFDEF CUSTCNTL}
    procedure WindowPosChanging (var msg : TWMWindowPosChanging); message
     WM_WINDOWPOSCHANGING;
{$ENDIF}

    function GetPerfRasConn (Index: Integer): HRasConn ;
    function GetPerfSubEnt (Index: Integer): integer ;
    function GetPerfXmitCur (Index: Integer): DWORD ;
    function GetPerfRecvCur (Index: Integer): DWORD ;
    function GetPerfConnSpd (Index: Integer): DWORD ;
    function GetPerfPortNam (Index: Integer): AnsiString ;
    procedure PutPerfRasConn (Index: Integer; Value: HRasConn) ;
    procedure PutPerfSubEnt (Index: Integer; Value: integer) ;
    procedure PutPerfPortNam (Index: Integer; Value: AnsiString) ;
    procedure SetPDHFlag (value: boolean) ;    // 4.42
    function GetPerfXmitCur0: DWORD ;  // 5.40 
    function GetPerfRecvCur0: DWORD ;
    function GetPerfConnSpd0: DWORD ;

  protected
//   Protected declarations
  public
//   Public declarations


{ Initialises various lists and functions in TMagRasPer.  }

    CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;

{ Destroys TMagRasPer, closing registry keys. }

    DESTRUCTOR Destroy; override;
{$IFDEF CUSTCNTL}
    procedure Paint; override;
{$ENDIF}

{ Resets statistics for all ports, channels or connections.
  May be done before or after a connection, but EnablePerfStats
  must have been called first.

  Calls ResetPerfStat. }
    procedure ResetPerfStats ;

{ Resets statistics for a single port, channel or connection.
  May be done before or after a connection, but EnablePerfStats
  must have been called first.

  Item depends on the platform.

  Calls GetPerfStats. }
    procedure ResetPerfStat (Item: integer) ;

{ Enables collection of performance statistics for dial-up networking
  connections.   This is done before checking for any connections.

  On Win9x stats are accessed from one or more Dial-Up Adaptors defined
  as registry keys.  These keys may be translated for non-English languages
  so setting Search to will cause searching of (a small part) of the
  registry for the required keys, which are available as the DialUpAdaptors
  property.  The first connection uses the first Dial Up Adaptor, the
  second (consectutive) connection the second.  It is not possible to
  confirm the connection name being used on a dial up adaptor.  Separate
  statistics are not available for separate multilink channels.

  On NT4, stats are accessed from performance objects in the registry, one
  for all RAS connections, and then also separately for each port (COM1,
  ISDN1, etc) so the application needs to know the port being used by a
  specific connection to get stats. Separate statistics are not available
  for separate multilink channels.  The device connection speed is not
  available either.

  On Windows 2000, stats are accessed using proper RAS functions, based
  on the connection handle and sub entry, so separate stats are available
  for separate multilink channels.

  The Start parameter specifies that GetPerfStats is called to get the
  initial perf stats (even if offline). 

  Result is true if perf stats were successfully returned, otherwise the
  ErrInfo may report a specific problem.

  Calls SearchDUA, GetPerfStats and ResetPerfStats. }
    function EnablePerfStats (Start, Search: boolean): boolean ;

{ Get the current performance statistics for all dial-up networking
  connections.  EnablePerfStats must have been called before this method.
  Makes available stats for all combined connections, and for separate
  connections.

  Before calling this method with Windows 2000, the handles and sub entries
  for any active connections must to be specified as the PerfRasConn and
  PerfSubEnt properties.  If PerfSubEnt is set to zero, combined stats for
  any multlink channels will be returned.

  Result is true if perf stats were successfully returned, otherwise the
  ErrInfo may report a specific problem.

  Properties returned are StatsXmit, StatsRecv, StatsConn, PerfXmitCur,
  PerfRecvCur, PerfConnSpd, and for NT4 only PerfPortNam.  The StatsXXXX
  and element 0 of the PerfXXXX properties reflect the combined stats for
  all connection, elements 1 to 30 of the PerfXXXX properties reflect stats
  for separate connections.

  On Win9x, the application should assume that stats for the first active
  connection are at element 1 and the second (assuming two or more than
  modems or ISDN adaptors are available) on element 2.  But none of this
  is documented by Microsoft (it just appears to be what happens).  Also
  note that with Win95 stats are only reliably returned with DUN 1.2 and
  later (which is not installed with OSR2).

  On NT4, stats are returned for each separate device port, so for a new
  active connection the application needs to check which device name is
  being used, look-up the device port (with GetEntryProperties) and then
  check through each element of PerfPortNam to find a match (do a case
  insensitive compare).  The ports do not change until a reboot, so this
  search only needs to be done once.

  On W2K, stats are returned according to the connection handles specified
  before calling GetPerfStats. }
    function GetPerfStats: boolean ;

{ An internal function used to search for Win9x dial-up adapters. }
    function SearchDUA: boolean ;

{ Encapsulates the RasGetErrorString function, to convert a RAS error code
  to a message.  If the error code is not from RAS (ie 600 to 782), a windows
  system error message is returned instead.  }
    function GetErrorString(ErrorCode: LongInt): String;

// runtime stuff

{ Specifies the Win9x Dial Up Adaptors located by SearchDUA and
  EnablePerfStats.  }
    PROPERTY DialUpAdaptors: TStringList read fDialUpAdaptors ;

{ The total number of Win9x Dial Up Adaptors located by SearchDUA and
  EnablePerfStats.  }
    PROPERTY TotAdaptors: integer read fTotAdaptors ;

// these three for old compatibility, return all stats

{ The number of bytes transmitted by all RAS connections, since
  ResetPerfStats was used. }
//    PROPERTY StatsXmit: DWORD     read fPerfXmitCur [0] ;
    PROPERTY StatsXmit: DWORD     read GetPerfXmitCur0 ;

{ The number of bytes received by all RAS connections, since ResetPerfStats
  was used. }
//    PROPERTY StatsRecv: DWORD     read fPerfRecvCur [0] ;
    PROPERTY StatsRecv: DWORD     read GetPerfRecvCur0 ;

{ The combined connection speed for all RAS connections, as reported by the
  modem or ISDN adapter at connection, but sometimes the COM port speed. W9x and
  W2K only, not NT4.  }
//    PROPERTY StatsConn: DWORD     read fPerfConnSpd [0] ;
    PROPERTY StatsConn: DWORD     read GetPerfConnSpd0 ;

{ The handle for a Windows 2000 RAS connection (base 1), must be specified
  after ResetPerfStats, ingored for W9x/NT4.  }
    PROPERTY PerfRasConn [Index: Integer]: HRasConn read GetPerfRasConn write PutPerfRasConn;

{ The sub entry for a Windows 2000 RAS connection (base 1), must be specified
  after ResetPerfStats, ingored for W9x/NT4.  }
    PROPERTY PerfSubEnt [Index: Integer]: integer read GetPerfSubEnt write PutPerfSubEnt ;

{ The number of bytes transmitted by a single RAS connection or channel
 (base 1), since  ResetPerfStats was used. }
    PROPERTY PerfXmitCur [Index: Integer]: DWORD read GetPerfXmitCur ;

{ The number of bytes received by a single RAS connection or channel
 (base 1), since ResetPerfStats was used. }
    PROPERTY PerfRecvCur [Index: Integer]: DWORD read GetPerfRecvCur ;

{ The connection speed for the RAS connections, as reported by the modem or
  ISDN adapter at connection, but sometimes the COM port speed, since
  ResetPerfStats was used  (base 1).  W9x and W2K only, not NT4.  }
    PROPERTY PerfConnSpd [Index: Integer]: DWORD read GetPerfConnSpd ;

{ The COM port name for which performance statistics are being returned on
  NT4, ie COM1, ISDN1 (base 1).  This property will be constant until a
  reboot to install or remove devices, so it's only necessary to check it
  once after EnablePerfStats.  }
    PROPERTY PerfPortNam [Index: Integer]: AnsiString read GetPerfPortNam write PutPerfPortNam ;

  PUBLISHED
// Published declarations

{ Used for ActiveX only }
    Property DAXCtrl : Boolean    read FDAXCtrl write FDAXCtrl default False;

{ The version of TMagRasPer - note is only only made writable so it displays
  in the object inspector }
    Property Version: string      read fVersion write fversion stored False;
//ROPERTY KeyDUNAdap: String      read fKeyDUNAdap write fKeyDUNAdap ;

{ The Win9x registry key for reading connection speed data, it's
  conceivable that this may get translated to a new language. }
    PROPERTY KeyDUNConn: AnsiString   read fKeyDUNConn write fKeyDUNConn ;

{ The Win9x registry key for reading transmitted data, it's
  conceivable that this may get translated to a new language. }
    PROPERTY KeyDUNXmit: AnsiString   read fKeyDUNXmit write fKeyDUNXmit ;

{ The Win9x registry key for reading received speed data, it's
  conceivable that this may get translated to a new language. }
    PROPERTY KeyDUNRecv: AnsiString   read fKeyDUNRecv write fKeyDUNRecv ;

{ May give more detailed error information if EnablePerfStats or
  GetPerfStats fails. }
    PROPERTY ErrInfo: String      read fErrInfo write fErrInfo ;

{ On NT4 only, specifies if the Performance Data Helpher library (PDH.DLL)
  should be used instead of the normal registry interface for performance
  statistics.  Sometimes the registry interface fails.  }
    PROPERTY UsePDH: boolean      read fUsePDHFlag write SetPDHFlag ;

{ The number of ports for which stats are being returned. }
    PROPERTY TotPorts: integer    read fTotPorts ;

  end;

// procedure Register;

implementation

var
  datasize: integer = 0 ;   // performance data buffer size

const

  TOTALBYTES =  8192 ;    // initial buffer size for NT performance data
  BYTEINCREMENT = 1024 ;  // make it bigger

// NT4 performance counter identifiers, assume they are fixed data
  Pdata_RAS_Port   = '870' ;
  Pdata_RAS_Total   = '906' ;
  Pdata_Bytes_Xmit  = 872 ;
  Pdata_Bytes_Recv  = 874 ;
// connect speed is not available on NT4, get it from TAPI instead

// NT4 performance data helper counter paths - 4.42
  PathPDHRasTotRecv = '\RAS Total\Bytes Received' ;
  PathPDHRasTotXmit = '\RAS Total\Bytes Transmitted' ;
  PDHRasPortEnum = 'RAS Port' ;
  PathPDHRasPortRecv = '\RAS Port(#)\Bytes Received' ;
  PathPDHRasPortXmit = '\RAS Port(#)\Bytes Transmitted' ;
  PathPDHPortOffset = 11 ;

// Win2K does not RAS performance objects, but has APIs instead

// keys and names for Win9x performance statistics under HKEY_DYN_DATA
  Reg_PerfStatStart     = 'PerfStats\StartStat';
  Reg_PerfStatData      = 'PerfStats\StatData';
  Reg_PerfStatStop      = 'PerfStats\StopStat';
  Reg_PerfAdap          = 'Dial-Up Adapter' ;
  Reg_PerfAdap2         = 'Dial-Up Adapter #2' ;
  Reg_PerfXmit          = 'TotalBytesXmit' ;
  Reg_PerfRecv          = 'TotalBytesRecvd' ;
  Reg_PerfConn          = 'ConnectSpeed' ;
  Reg_PerfStatEmum   = 'System\CurrentControlSet\Control\PerfStats\Enum' ;

{ other keys... Win9x only
Dial-Up Adapter #2\
"Dial-Up Adapter\Buffer"
"Dial-Up Adapter\Framing"
"Dial-Up Adapter\Overrun "
"Dial-Up Adapter\Alignment"
"Dial-Up Adapter\Timeout"
"Dial-Up Adapter\CRC"
"Dial-Up Adapter\Runts"
"Dial-Up Adapter\FramesXmit"
"Dial-Up Adapter\FramesRecvd"
"Dial-Up Adapter\BytesXmit" these are the same as Total
"Dial-Up Adapter\BytesRecvd"    }


//procedure Register;
//begin
//    RegisterComponents('Magenta Systems', [TMagRasPer]);
//end;


{ ********************************************************************* }
{   TMagRasPer - RAS Performance Statistics   }
{ ********************************************************************* }

CONSTRUCTOR TMagRasPer.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    ResetPerfStats ;  // clear performance statistics
//  fKeyDUNAdap := Reg_PerfAdap ;
    fKeyDUNConn := Reg_PerfConn ;
    fKeyDUNXmit := Reg_PerfXmit ;
    fKeyDUNRecv := Reg_PerfRecv ;
    fDialUpAdaptors := TStringList.Create;
    fVersion := MagVersion ;
    fErrInfo := '' ;
    fUsePDHFlag := false ;
    fQueryHandle := 0 ;

  // Create a bitmap which is used to show the Components
  // in both Delphi and non Delphi environments.
{$IFDEF CUSTCNTL}
    fDesignBitmap := TBitmap.Create;
    fDesignBitmap.LoadFromResourceName (hInstance, ClassType.ClassName);
    Width := fDesignBitmap.Width;
    Height := fDesignBitmap.Height;
  //Note you can rely on this working in a non Delphi environment.
    if Not(csDesigning In ComponentState) then Visible := False;
{$ENDIF}
end;

destructor TMagRasPer.Destroy;
begin
    fDialUpAdaptors.Free ;

// must close key to allow other applications to be deleted/accessed
    if MagRasOSVersion = OSNT4 then RegCloseKey (HKEY_PERFORMANCE_DATA);

// optional PDH
    if MagPdhAPI_Loaded and (fQueryHandle <> 0) then
                                        pdhCloseQuery (fQueryHandle) ;
    fQueryHandle := 0 ;

{$IFDEF CUSTCNTL}
    fDesignBitmap.Free;
{$ENDIF}
    inherited Destroy;
end;

{$IFDEF CUSTCNTL}
procedure TMagRasPer.WindowPosChanging (var msg :
  TWMWindowPosChanging);
begin
  //Don't let the user resize the component
  //at design time.
  msg.WindowPos.cx := Width;
  msg.WindowPos.cy := Height
end;

procedure TMagRasPer.Paint;
begin
  inherited Paint;
  //Draw the bitmap if we are in a Delphi form design state
  //or the component is being used as an Active X control.
  if (csDesigning in ComponentState)or (DAXCtrl) then
  begin
    Width := FDesignBitmap.Width;
    Height := FDesignBitmap.Height;
    Canvas.Draw(0, 0, FDesignBitmap);
  end;
end;
{$ENDIF}

function TMagRasPer.GetErrorString(ErrorCode: LongInt): String;
var
    szErrorString: Array[0..256] of AnsiChar;
begin
    if (NOT MagLoadRasApi) then
    begin
        fLastError := ERROR_DLL_NOT_FOUND ;
        result := RASAPI_DLL + #32 +  SRasGenNotAvailable ; // Not Available
        exit ;
    end ;
    Result := '';
    FillChar (szErrorString, SizeOf (szErrorString), #0);
    RasGetErrorString (ErrorCode, szErrorString, 256);
    If szErrorString[0] <> #0 THEN
        Result := String (szErrorString) // 9 Aug 2010
    Else
        Result := SysErrorMessage (ErrorCode) ;  // Angus, try a windows error
end;

procedure TMagRasPer.ResetPerfStat (item: integer) ;
begin
// tot counters are from IPL, Win9x only
    if (item > 0) and (item <= 4) then
    begin
        fStatsXmitCon [item] := fStatsXmitTot [item] ;
        fStatsRecvCon [item] := fStatsRecvTot [item] ;
    end ;

// now stuff reported to user for cconnections
    if item > MaxDevs then exit ;
    fPerfXmitCur [item] := 0 ;
    fPerfRecvCur [item] := 0 ;
    fPerfConnSpd [item] := 0 ;

// windows 2000 and XP, aka nt5 - no attempt to clear separate links
    if MagRasOSVersion >= OSW2K then
    begin
        fLastError := ERROR_DLL_NOT_FOUND ;
        if (NOT MagLoadRasApi) then exit ;
        if NOT Assigned (RasClearConnectionStatistics) then exit ;
        if fPerfRasConn [item] <> 0 then
            fLastError := RasClearConnectionStatistics (fPerfRasConn [item]) ;
    end ;
end ;

procedure TMagRasPer.ResetPerfStats ;
var
    I: integer ;
begin
    for I := 0 to MaxDevs do   // most counters zero all, then each device
    begin
        ResetPerfStat (I) ;
        fPerfRasConn [I] := 0 ;  // W2K handle and subentry
        fPerfSubEnt [I] := 0 ;
        if (fQueryHandle = 0) then fPerfPortNam [I] := '' ;  // not if PDH running
    end ;
    GetPerfStats ;   // get NT portnames which we just killed
 end ;

function TMagRasPer.SearchDUA: boolean ;
var
    TempKey, Temp2Key: HKey;
    keyname, lockey: AnsiString ;
    NumSubKeys, NumValues, count: integer ;
    dwType, dwSize, Len: DWORD ;
begin
    result := false ;
    fTotAdaptors := 0 ;
    if MagRasOSVersion <> OSW9x then exit ;
    fDialUpAdaptors.Clear ;
    TempKey := 0;
    Temp2Key := 0;
    result := RegOpenKeyExA (HKEY_LOCAL_MACHINE, PAnsiChar(Reg_PerfStatEmum),
                                   0, KEY_READ, TempKey) = ERROR_SUCCESS ;
    if result then
        result := RegOpenKeyExA (HKEY_DYN_DATA, PAnsiChar(Reg_PerfStatStart),
                                   0, KEY_READ, Temp2Key) = ERROR_SUCCESS ;
    if result then
    begin
        NumSubKeys := 0 ;
        NumValues := 0 ;
        RegQueryInfoKey (TempKey, nil, nil, nil, @NumSubKeys,
                               nil, nil, @NumValues, nil, nil, nil, nil) ;
        if NumSubKeys <> 0 then
        begin
            SetString (lockey, nil, 33);
            for count := 0 to NumSubKeys - 1 do
            begin
                Len := 33 ;
                RegEnumKeyExA (TempKey, count, PAnsiChar(lockey), Len,
                                                    nil, nil, nil, nil);
                keyname := lockey + '\' + fKeyDUNConn ;  // 5.40
                if RegQueryValueExA (Temp2Key, PAnsiChar(keyname), nil,
                        @dwType, nil, @dwSize) = ERROR_SUCCESS then
                                         fDialUpAdaptors.Add (String (lockey)) ; // 9 Aug 2010
            end ;
        end ;
    end ;
    if TempKey <> 0 then RegCloseKey (TempKey) ;
    if Temp2Key <> 0 then RegCloseKey (Temp2Key) ;
    fTotAdaptors := fDialUpAdaptors.Count ;
    if fTotAdaptors <> 0 then fDialUpAdaptors.Sort ;
end;

procedure TMagRasPer.SetPDHFlag (value: boolean) ;
begin
    if value then
    begin
        fUsePDHFlag := value ;
    end
    else
    begin
        fUsePDHFlag := value ;
        if fQueryHandle <> 0 then pdhCloseQuery (fQueryHandle) ;
        fQueryHandle := 0 ;
    end ;
end ;

function TMagRasPer.EnablePerfStats (Start, Search: boolean): boolean ;
var
    TempKey: HKey;
    keyname: AnsiString ;
    dwType, dwSize: DWORD ;
    TempData: Pointer ;
    I, errcode: integer ;
//  path: string ;
//  countlen, instlen: DWORD ;
//  dataptr, instbuff: PAnsiChar ;

    function InitData (I: integer; ValueName: AnsiString): boolean ;
    begin
        result := false ;
        ValueName := AnsiString (fDialUpAdaptors [I - 1]) + '\' + ValueName ; // 9 Aug 2010
        if RegQueryValueExA (TempKey, PAnsiChar(ValueName), nil,
                            @dwType, nil, @dwSize) = ERROR_SUCCESS then
        begin
        try     // read data but ignore it
            GetMem (TempData, dwSize) ;
            Result := RegQueryValueExA (TempKey, PAnsiChar(ValueName), nil,
                            @dwType, TempData, @dwSize) = ERROR_SUCCESS ;
        finally
            FreeMem (TempData) ;
            end ;
        end ;
    end ;

    procedure AddCounter (portnr: integer; path: AnsiString; var handle: HCOUNTER) ;
//    var
//        counterinfo: PPDH_COUNTER_INFO;  // variable sized, data after structure
//        infosize: DWORD ;
    begin
        handle := 0 ;
        errcode := PdhAddCounter (fQueryHandle, PAnsiChar (path),
                                                        DWORD (portnr), handle);
    {    if errcode <> Error_Success then exit ;
        infosize := 65536;
        GetMem (counterinfo, infoSize) ;
        errcode := pdhGetCounterInfo (handle, true, infoSize, counterinfo) ;
        if errcode = Error_Success then   // nothing useful here...
        begin
            SetLength (fPDHCInfo [portnr], infosize) ;
            move (counterinfo^, fPDHCInfo [portnr] [1], infosize) ;   // temporary !!!!
        end ;
        FreeMem (counterinfo) ;   }
    end ;

begin
    result := false ;
    fErrInfo := '' ;
    if Win32Platform = VER_PLATFORM_WIN32s then exit ;
    result := true ;
    case MagRasOSVersion of     // Angus 3.2
    OSW9x:    // Windows 95 and Windows 98, requires DUN 1.2 or later
    begin
        if search then
        begin
           SearchDUA ;
           if fTotAdaptors = 0 then
           begin
                fErrInfo := SRasErrNoDUA ; // Unable to Locate Win9x Dial-Up Adapter
                result := false ;
                exit ;
           end ;
        end ;
        TempKey := 0;
        if start then
            keyname := Reg_PerfStatStart
        else
            keyname := Reg_PerfStatStop ;
        errcode := RegOpenKeyExA (HKEY_DYN_DATA, PAnsiChar(keyname), 0,
                                                      KEY_ALL_ACCESS, TempKey) ;
        result := (errcode = ERROR_SUCCESS) ;
        if result then
        begin
            for I := 1 to fTotAdaptors do
            begin
                result := InitData (I, fKeyDUNXmit) ;
                if result then result := InitData (I, fKeyDUNRecv) ;
                if result then result := InitData (I, fKeyDUNConn) ;
                if NOT result then
                begin
                    fErrInfo := Format (SRasErrPerKey, [keyname, fKeyDUNConn]) ;  // Unable to Read Registry Key
                    break ;
                end ;
            end ;
            RegCloseKey (TempKey) ;
        end
        else
            fErrInfo := Format (SRasErrPerKey, [keyname, GetErrorString (fLastError)]) ;  // Unable to Read Registry Key
    end ;

    OSNT4:    // NT4, optional PDH.DLL (nothing for registry
    begin
        fTotPorts := 0 ;
        for I := 0 to 8 do
        begin
            fPDHRecvHand [I] := 0 ;
            fPDHXmitHand [I] := 0 ;
        end ;
        if fUsePDHFlag and (fQueryHandle = 0) then
        begin
            fLastError := ERROR_DLL_NOT_FOUND ;
            if (NOT MagLoadPdhApi) then
            begin
                fErrInfo := SRasErrNoPDH ; // Unable to Find Performance Data Helper Library (PDH.DLL)' ;
                result := false ;
                exit ;
            end ;

        // create new query and add total counters
            errcode := pdhOpenQuery (nil, 0, fQueryHandle) ;
            if errcode = ERROR_SUCCESS then
                       AddCounter (0, PathPDHRasTotRecv, fPDHRecvHand [0]) ;
            if errcode = ERROR_SUCCESS then
                       AddCounter (0, PathPDHRasTotXmit, fPDHXmitHand [0]) ;

            if errcode = ERROR_SUCCESS then
            begin

            // now looks for ports
           {     instlen := 0 ;
                countlen := 0 ;
                errcode := PdhEnumObjectItems (Nil, Nil, PAnsiChar (PDHRasPortEnum),
                             Nil, countlen, Nil, instlen, PERF_DETAIL_STANDARD, 0) ;
                GetMem (instbuff, instlen) ;
                try
                    countlen := 0 ;  // ignore counter names, we know them already
                    errcode := PdhEnumObjectItems (Nil, Nil, PAnsiChar (PDHRasPortEnum),
                          Nil, countlen, instbuff, instlen, PERF_DETAIL_STANDARD, 0) ;
                    dataptr := instbuff;
                    I := 1 ;
                    if Assigned (dataptr) then
                    while dataptr^ <> #0 do
                    begin
                    // Warning - returns 0,1,2 instead of COM1, ISDN1, etc,
                        fPerfPortNam [I] := 'PORT' + dataptr ;
                        Inc (dataptr, lstrlen (dataptr) + 1) ;  // next string
                        path := PathPDHRasPortRecv ;
                        path [PathPDHPortOffset] := Chr (I + 47) ;  // instance, base '0'
                        AddCounter (I, path, fPDHRecvHand [I]) ;
                        if errcode <> ERROR_SUCCESS then break ;
                        path := PathPDHRasPortXmit ;
                        path [PathPDHPortOffset] := Chr (I + 47) ;
                        AddCounter (I, path, fPDHXmitHand [I]) ;
                        if errcode <> ERROR_SUCCESS then break ;
                        inc (I) ;
                        inc (fTotPorts) ;
                    end;
                finally
                    FreeMem (instbuff) ;
                end ;   }
            end
            else
            begin
                fErrInfo := Format (SRasErrBadPDH, [errcode]) ; // Unable to Initialise Performance Data Helper Library (PDH.DLL), ErrCode
                result := false ;
            end ;
        end ;
    end ;

  // nothing for XP and better, nice simple API
    end ;

// get starting counters
    if result then
    begin
        if start then result := GetPerfStats ;
        ResetPerfStats ;
    end ;
end;

function TMagRasPer.GetPerfStats: boolean ;
var
    TempKey: HKey;
    dwType,
    dwSize,
    connspd, countertype: DWORD ;
    Ras_Stats: TRas_Stats;   // W2K only
    portnr, I, errcode: integer ;
    rawcounter: TPDH_RAW_COUNTER ;

// get Win9x registry data - simple binary key
    function GetWin9xData (I: integer; ValueName: AnsiString;
                                                var Info: DWORD): boolean ;
    begin
        ValueName := AnsiString (fDialUpAdaptors [I -1 ]) + '\' + ValueName ; // 9 Aug 2010
        dwSize := 4 ;   // data is four bytes of binary, aka a DWORD
        Result := RegQueryValueExA (TempKey, PAnsiChar(ValueName), nil,
                            @dwType, @Info, @dwSize) = ERROR_SUCCESS;
    end ;


    function GetNT4Data (perfobjname: AnsiString; portstart: integer): boolean ;
    var
        perfdata: PPERF_DATA_BLOCK ;
        perfobj: PPERF_OBJECT_TYPE ;
        perfcdef: PPERF_COUNTER_DEFINITION ;
        perfmcdef: array [1..50] of PPERF_COUNTER_DEFINITION ;
        perfinst: PPERF_INSTANCE_DEFINITION ;
        perfcblk: PPERF_COUNTER_BLOCK ;
        regbuff,
        objptr,
        defptr,
        countptr: PAnsiChar ;
        actualsize,
        DataType: Integer;
        objnr,
        instnr,
        countnr: integer ;
        datvalue: ^DWORD ;
        loopflag: boolean ;
    begin
        DataType := REG_NONE;
        if datasize = 0 then datasize := TOTALBYTES ;
        portnr := portstart ;
        result := false ;
        GetMem (regbuff, datasize) ;

    // start with small buffer, it will be increased in size if necessary the
    // first time, to that required for the returned performance data
        try
            actualsize := datasize ;
            while RegQueryValueExA (HKEY_PERFORMANCE_DATA,
                pAnsiChar(perfobjname), nil, @DataType, PByte(regbuff),
                                        @actualsize) = ERROR_MORE_DATA do
            begin
                Freemem (regbuff) ;
                inc (datasize,  BYTEINCREMENT) ;  // increase buffers size by 1K
                GetMem (regbuff, datasize) ;
                actualsize := datasize ;
            end ;

        // get performance data block
            if actualsize < 100 then
            begin
                fErrInfo := Format (SRasErrPerData, [perfobjname ]) ; // No Performance Data Returned for Object
                exit ;     // forget it
            end ;
            pointer (perfdata) := regbuff ;   // PERF_DATA_BLOCK

        // get performance object type blocks
            if perfdata.numobjecttypes = 0 then
            begin
                fErrInfo := Format (SRasErrPerBlock, [perfobjname ]) ; // No Performance Object Blocks Found for
                exit ;   // no objects to process
            end ;
            objptr := regbuff + perfdata.HeaderLength ;
            for objnr := 1 to perfdata.numobjecttypes do
            begin
                pointer (perfobj) := objptr ;  // PERF_OBJECT_TYPE
            //  perfobj.ObjectNameTitleIndex   // not needed
                defptr := objptr + perfobj.HeaderLength ;

            // get performance counter definitions
                if perfobj.numcounters > 0 then
                begin

            // read through definitions, really looking for length
                for countnr := 1 to perfobj.numcounters do
                    begin
                        pointer (perfmcdef [countnr]) := defptr ;  // keep each definitition
                        pointer (perfcdef) := defptr ;  // PERF_COUNTER_DEFINITION
                        inc (defptr, perfcdef.bytelength) ;
                        if countnr > 50 then
                        begin
                            fErrInfo := Format (SRasErrPerBlock, [perfobjname ]) ; // Unable to Find Counter for Object -
                            exit ;
                        end ;
                    end ;

            // now get counter data, perhaps from multiple instances for different ports
                    loopflag := true ;
                    instnr := 1 ;
                    while loopflag do
                    begin
                        if perfobj.numinstances >= 1 then
                        begin
                            pointer (perfinst) := defptr ;  // PERF_INSTANCE_DEFINITON
                            fPerfPortNam [portnr] := AnsiString (Lowercase (WideCharToString
                                    (PWideChar(defptr + perfinst.nameoffset)))) ;  // 9 Aug 2010
                            inc (defptr, perfinst.bytelength) ;
                        end
                        else
                            fPerfPortNam [portnr] := 'all' ;

                    // get counter block, then read actual data values
                        countptr := defptr ;  // after reading through blocks
                        pointer (perfcblk) := countptr ;  // PERF_COUNTER_BLOCK

                    // get counter data, currently only doublewords
                        for countnr := 1 to perfobj.numcounters do
                        begin
                            if perfmcdef [countnr].CounterNameTitleIndex =
                                                        Pdata_Bytes_Xmit then
                            begin
                                pointer (datvalue) := countptr +
                                    perfmcdef [countnr].counteroffset ;
                                if Datvalue^ > fPerfXmitCur [portnr] then
                                         fPerfXmitCur [portnr] := Datvalue^ ;
                            end ;
                            if perfmcdef [countnr].CounterNameTitleIndex =
                                                        Pdata_Bytes_Recv then
                            begin
                                pointer (datvalue) := countptr +
                                    perfmcdef [countnr].counteroffset ;
                                if Datvalue^ > fPerfRecvCur [portnr] then
                                         fPerfRecvCur [portnr] := Datvalue^ ;
                            end ;
                        end ;
                        inc (defptr, perfcblk.bytelength) ;

                    // check for more instances of these counters
                        if perfobj.numinstances >= 1 then
                        begin
                            inc (fTotPorts) ;
                            inc (portnr) ;
                            inc (instnr) ;
                            if instnr > perfobj.numinstances then loopflag := false ;
                            if portnr > MaxDevs then loopflag := false ;
                        end
                        else
                            loopflag := false ;
                    end ;
                end ;
                objptr := objptr + perfobj.totalbytelength ;
            end ;
            result := true ;
        finally
            if regbuff <> nil then Freemem (regbuff) ;
        end ;
    end ;

    procedure GetPDHData (portnr: integer) ;
    begin
        if fPDHXmitHand [portnr] = 0 then exit ;
        errcode := PdhGetRawCounterValue (fPDHXmitHand [portnr],
                                                    countertype, rawcounter) ;
        if errcode <> Error_Success then exit ;
        if (rawcounter.CStatus <> PDH_CSTATUS_VALID_DATA) and
                (rawcounter.CStatus <> PDH_CSTATUS_NEW_DATA) then exit ;
        if rawcounter.FirstValue > fPerfXmitCur [portnr] then
{$IFDEF VER100} { Delphi 3 did not have Int64 but used Comp }
                      fPerfXmitCur [portnr] := Trunc (rawcounter.FirstValue) ;
{$ELSE}
                              fPerfXmitCur [portnr] := rawcounter.FirstValue ;
{$ENDIF}
        errcode := PdhGetRawCounterValue (fPDHRecvHand [portnr],
                                                    countertype, rawcounter) ;
        if errcode <> Error_Success then exit ;
        if (rawcounter.CStatus <> PDH_CSTATUS_VALID_DATA) and
                (rawcounter.CStatus <> PDH_CSTATUS_NEW_DATA) then exit ;
        if rawcounter.FirstValue > fPerfRecvCur [portnr] then
{$IFDEF VER100} { Delphi 3 did not have Int64 but used Comp }
                      fPerfRecvCur [portnr] := Trunc (rawcounter.FirstValue) ;
{$ELSE}
                              fPerfRecvCur [portnr] := rawcounter.FirstValue ;
{$ENDIF}
    end ;

begin
    result := false ;
    if Win32Platform = VER_PLATFORM_WIN32s then exit ;

// windows 95 and 98 - simply registry keys for each item of data
    if MagRasOSVersion = OSW9x then    // Windows 95 and Windows 98, requires DUN 1.2 or later
    begin
        fTotPorts := 0 ;
        TempKey := 0;
        fPerfConnSpd [0] := 0 ;
        fPerfXmitCur [0] := 0 ;
        fPerfRecvCur [0] := 0 ;
        fPerfPortNam [0] := 'all' ;
        result := RegOpenKeyExA (HKEY_DYN_DATA, PAnsiChar(Reg_PerfStatData),
                                        0, KEY_READ, TempKey) = ERROR_SUCCESS ;
        if result then
        begin
            for I := 1 to fTotAdaptors do
            begin
                result := GetWin9xData (I, fKeyDUNXmit, fStatsXmitTot [I]) ;
                if result then
                    result := GetWin9xData (I, fKeyDUNRecv, fStatsRecvTot [I]) ;
                if result then
                        result := GetWin9xData (I, fKeyDUNConn, connspd) ;
                if result then
                begin
               //     if fStatsXmitTot [I] < fStatsXmitCon [I] then ResetPerfStat (I) ;  4.62
               //     if fStatsRecvTot [I] < fStatsRecvCon [I] then ResetPerfStat (I) ;
                    fPerfConnSpd [I] := connspd ;
                    if fStatsXmitTot [I] >= fStatsXmitCon [I] then               // 4.62
                        fPerfXmitCur [I] := fStatsXmitTot [I] - fStatsXmitCon [I]
                    else
                        fPerfXmitCur [I] := (MaxLongWord -
                                    fStatsXmitCon [I]) + fStatsXmitTot [I] ;
                    if fStatsRecvTot [I] >= fStatsRecvCon [I] then               // 4.62
                        fPerfRecvCur [I] := fStatsRecvTot [I] - fStatsRecvCon [I]
                    else
                        fPerfRecvCur [I] := (MaxLongWord -
                                        fStatsRecvCon [I]) + fStatsRecvTot [I] ;
                //  fPerfPortNam [I] !! do not change this, it's used to store the stats connection
                // keep combined session totals
                    if fStatsRecvTot [I] <> 0 then  // only if some data sent
                        fPerfConnSpd [0] := fPerfConnSpd [0] + fPerfConnSpd [I] ;
                    fPerfXmitCur [0] := fPerfXmitCur [0] + fPerfXmitCur [I] ;
                    fPerfRecvCur [0] := fPerfRecvCur [0] + fPerfRecvCur [I] ;
                    inc (fTotPorts) ;
                end
                else
                    fErrInfo := Format (SRasErrPerKey,
                                     [Reg_PerfStatData, fKeyDUNConn]) ; // Unable to Read Registry Key
            end ;
            RegCloseKey (TempKey) ;
        end
        else
            fErrInfo := Format (SRasErrOpKey, [Reg_PerfStatData]) ; // Unable to Open Registry Key
    end

// Windows NT4 performance data
    else if MagRasOSVersion = OSNT4 then
    begin
        if fUsePDHFlag and (fQueryHandle <> 0) then
        begin
            if NOT MagPdhAPI_Loaded then exit ;

        // do not clear port names, we found them earlier (but with wrong names)
            errcode := pdhCollectQueryData (fQueryHandle) ;
            if errcode = Error_Success then
            begin
                GetPDHData (0) ;
                result := (errcode = Error_Success) ;
                if fTotPorts <> 0 then
                begin
//                  for I := 1 to fTotPorts do GetPDHData (I) ;
                end ;
            end ;
        end
        else
        begin
        // clear port names, these are completed as stats are found
        // if blank, all statistics are ignored
            for I := 0 to MaxDevs do fPerfPortNam [I] := '' ;
            result := GetNT4Data (Pdata_RAS_Total, 0) ;  // combined data for all ports
            fTotPorts := 0 ;
            if result then result := GetNT4Data (Pdata_RAS_Port, 1) ; // and separate ports
        end ;
    end

// Windows 2000 and better have real APIs
    else
    begin
        for I := 0 to MaxDevs do fPerfPortNam [I] := '' ;
        fPerfConnSpd [0] := 0 ;
        fPerfXmitCur [0] := 0 ;
        fPerfRecvCur [0] := 0 ;
        fPerfPortNam [0] := 'all' ;
        fLastError := ERROR_DLL_NOT_FOUND ;
        if (NOT MagLoadRasApi) then exit ;
        if NOT Assigned (RasClearConnectionStatistics) then exit ;
        for portnr := 1 to MaxDevs do
        begin
            if fPerfRasConn [portnr] <> 0 then
            begin
                FillChar (Ras_Stats, SizeOf(Ras_Stats), #0);
                Ras_Stats.dwSize := Sizeof (Ras_Stats) ;
                if fPerfSubEnt [portnr] <> 0 then
                    fLastError := RasGetLinkStatistics (fPerfRasConn [portnr],
                                              fPerfSubEnt [portnr], @Ras_Stats)
                else
                    fLastError := RasGetConnectionStatistics
                                        (fPerfRasConn [portnr], @Ras_Stats) ;
                If fLastError = 0 THEN
                begin
                    with Ras_Stats do
                    begin
                        if dwBps > fPerfConnSpd [portnr] then
                                                fPerfConnSpd [portnr] := dwBps ;
                        if dwBytesXmited > fPerfXmitCur [portnr] then
                                        fPerfXmitCur [portnr] := dwBytesXmited ;
                        if dwBytesRcved > fPerfRecvCur [portnr] then
                                        fPerfRecvCur [portnr] := dwBytesRcved ;
                        fPerfPortNam [portnr] := 'x' + AnsiString (IntToStr (portnr)) ; // 9 Aug 2010
                        if portnr > fTotPorts then  fTotPorts := portnr ;
                    end ;
                // keep combined session totals
                    fPerfConnSpd [0] := fPerfConnSpd [0] + fPerfConnSpd [portnr] ;
                    fPerfXmitCur [0] := fPerfXmitCur [0] + fPerfXmitCur [portnr] ;
                    fPerfRecvCur [0] := fPerfRecvCur [0] + fPerfRecvCur [portnr] ;
                end
                else
                begin
                    fErrInfo := GetErrorString (fLastError) ;
                end ;
            end ;
        end ;
        result := true ;
    end ;
end;

function TMagRasPer.GetPerfRasConn (Index: Integer): HRasConn ;
begin
    result := fPerfRasConn [Index] ;
end ;

function TMagRasPer.GetPerfSubEnt (Index: Integer): integer ;
begin
    result := fPerfSubEnt [Index] ;
end ;

function TMagRasPer.GetPerfXmitCur (Index: Integer): DWORD ;
begin
    result := fPerfXmitCur [Index] ;
end ;

function TMagRasPer.GetPerfRecvCur (Index: Integer): DWORD ;
begin
    result := fPerfRecvCur [Index] ;
end ;

function TMagRasPer.GetPerfConnSpd (Index: Integer): DWORD ;
begin
    result := fPerfConnSpd [Index] ;
end ;

function TMagRasPer.GetPerfXmitCur0: DWORD ;
begin
    result := fPerfXmitCur [0] ;
end ;

function TMagRasPer.GetPerfRecvCur0: DWORD ;
begin
    result := fPerfRecvCur [0] ;
end ;

function TMagRasPer.GetPerfConnSpd0: DWORD ;
begin
    result := fPerfConnSpd [0] ;
end ;

function TMagRasPer.GetPerfPortNam (Index: Integer): AnsiString ;
begin
    result := fPerfPortNam [Index] ;
end ;

procedure TMagRasPer.PutPerfRasConn (Index: Integer; Value: HRasConn) ;
begin
    if value <> fPerfRasConn [Index] then
                       fPerfRasConn [Index] := value ;
end ;

procedure TMagRasPer.PutPerfSubEnt (Index: Integer; Value: integer) ;
begin
    if value <> fPerfSubEnt [Index] then
                       fPerfSubEnt [Index] := value ;
end ;

procedure TMagRasPer.PutPerfPortNam (Index: Integer; Value: AnsiString) ;
begin
    if value <> fPerfPortNam [Index] then
                       fPerfPortNam [Index] := value ;
end ;
Initialization
finalization

end.
