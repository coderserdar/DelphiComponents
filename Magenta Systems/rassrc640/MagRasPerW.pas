unit MagRasPerW;
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
(C) 2017 Magenta Systems Ltd

Updated by Angus Robertson, Magenta Systems Ltd, England
in 2017, delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

Compatible with Delphi 7, 2007, XEx to D10.x, supports Windows Vista,
2008, 7, 8, 2012, 10 and 2016, both 32-bit and 64-bit editions.
Windows XP may still work but is no longer supported.

TMagRas is a set of installable Delphi non-visual components,
supplied with a demo program, for accessing Dial Up Networking
or Remote Access Services functions.  This is a major update
of Daniel Polistchuck's and Mike Armstrong's earlier TRAS component
(little of which remains).  It allows Delphi developers to add full
RAS functionality to their applications, including dialling and
monitoring multiple connections, creating and editing phonebooks
(without using Windows dialogs), and getting performance information for
connections.  TMagRas supports Windows 95, 98, NT 4.0, Windows 2000
and later, allowing an application to support all the various RAS
extensions in NT and W2K, including sub entries for multi-channel ISDN
connections. Performance statistics are returned separately for each
connection in NT4 and each channel in W2K and later.

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

Changes in 6.00
Support RAS WideChar APIs and Unicode for Delphi 2009 with new MagRasxxxW units
Removed support for Win9x and NT4, both 10 years old now
The units may be compiled under Delphi 7 to 2009, but only support
   Unicode properties under Delphi 2009 where String=UnicodeString

Changes in 6.30
RegisterComponent now in magrasregw

Changes in 6.31
Clean up removing old Win9x and NT4 code (a lot)
MagRasOSVersion returns OS7 and OS8 but check >= OSVista


------------------------------------------------------------------------ }

interface

//Include the resource with the bitmap. Note bitmap
//must have same name as the class
{$R MAGRASPER.RES}

uses
  SysUtils, Windows, Messages, Classes,
  MagRasApiW, MagRasStr, magsubs1 ;

const
  MagVersion = 'TMagRasPer, 6th June 2017 - Release 6.40, Copyright 2017, Magenta Systems Ltd' ;
  MaxLongWord: DWord = $FFFFFFFF;  // this will probably die on Delphi 3!!!
  MaxDevs = 30 ;   // how many devices supported for stats

type

  TMagRasPer = class(TComponent)
  private
    { Private declarations }
    fDAXCtrl : Boolean;
    fVersion: string ;
    fLastError: integer ;
    fTotPorts: integer ;    //  total ports for which data returned
    fErrInfo: string ;    // more useful error for user

// arrays with performance data, 0 = all, 1 > for separate devices - max 30
    fPerfRasConn: array [0..MaxDevs] of HRasConn ;   // handle for W2K a
    fPerfSubEnt: array [0..MaxDevs] of integer ;     // subentry for W2K
    fPerfXmitCur: array [0..MaxDevs] of DWORD ;
    fPerfRecvCur: array [0..MaxDevs] of DWORD ;
    fPerfConnSpd: array [0..MaxDevs] of DWORD ;      // speed
    fPerfPortNam: array [0..MaxDevs] of string ;     // device number

    function GetPerfRasConn (Index: Integer): HRasConn ;
    function GetPerfSubEnt (Index: Integer): integer ;
    function GetPerfXmitCur (Index: Integer): DWORD ;
    function GetPerfRecvCur (Index: Integer): DWORD ;
    function GetPerfConnSpd (Index: Integer): DWORD ;
    function GetPerfPortNam (Index: Integer): string ;
    procedure PutPerfRasConn (Index: Integer; Value: HRasConn) ;
    procedure PutPerfSubEnt (Index: Integer; Value: integer) ;
    procedure PutPerfPortNam (Index: Integer; Value: string) ;

  protected
//   Protected declarations
  public
//   Public declarations


{ Initialises various lists and functions in TMagRasPer.  }

    CONSTRUCTOR Create(AOwner: TComponent); OVERRIDE;

{ Destroys TMagRasPer, closing registry keys. }

    DESTRUCTOR Destroy; override;

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

  Stats are accessed using the connection handle and sub entry, so
  separate stats are available for separate multilink channels.

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

  Before calling this method, the handles and sub entries
  for any active connections must to be specified as the PerfRasConn and
  PerfSubEnt properties.  If PerfSubEnt is set to zero, combined stats for
  any multlink channels will be returned.

  Result is true if perf stats were successfully returned, otherwise the
  ErrInfo may report a specific problem.

  Properties returned are StatsXmit, StatsRecv, StatsConn, PerfXmitCur,
  PerfRecvCur, and PerfPortNam.  The StatsXXXX
  and element 0 of the PerfXXXX properties reflect the combined stats for
  all connection, elements 1 to 30 of the PerfXXXX properties reflect stats
  for separate connections.

  Stats are returned according to the connection handles specified
  before calling GetPerfStats. }
    function GetPerfStats: boolean ;

{ An internal function used to search for Win9x dial-up adapters. }
//    function SearchDUA: boolean ;

{ Encapsulates the RasGetErrorString function, to convert a RAS error code
  to a message.  If the error code is not from RAS (ie 600 to 782), a windows
  system error message is returned instead.  }
    function GetErrorString(ErrorCode: LongInt): String;

// runtime stuff

{ The number of bytes transmitted by all RAS connections, since
  ResetPerfStats was used. }
    PROPERTY StatsXmit: DWORD     read fPerfXmitCur [0] ;

{ The number of bytes received by all RAS connections, since ResetPerfStats
  was used. }
    PROPERTY StatsRecv: DWORD     read fPerfRecvCur [0] ;

{ The combined connection speed for all RAS connections, as reported by the
  modem or ISDN adapter at connection }
    PROPERTY StatsConn: DWORD     read fPerfConnSpd [0] ;

{ The handle for the RAS connection (base 1), must be specified after ResetPerfStats.  }
    PROPERTY PerfRasConn [Index: Integer]: HRasConn read GetPerfRasConn write PutPerfRasConn;

{ The sub entry for a RAS connection (base 1), must be specified after ResetPerfStats  }
    PROPERTY PerfSubEnt [Index: Integer]: integer read GetPerfSubEnt write PutPerfSubEnt ;

{ The number of bytes transmitted by a single RAS connection or channel
 (base 1), since  ResetPerfStats was used. }
    PROPERTY PerfXmitCur [Index: Integer]: DWORD read GetPerfXmitCur ;

{ The number of bytes received by a single RAS connection or channel
 (base 1), since ResetPerfStats was used. }
    PROPERTY PerfRecvCur [Index: Integer]: DWORD read GetPerfRecvCur ;

{ The connection speed for the RAS connections, as reported by the modem or
  ISDN adapter at connection, but sometimes the COM port speed, since
  ResetPerfStats was used  (base 1).  }
    PROPERTY PerfConnSpd [Index: Integer]: DWORD read GetPerfConnSpd ;

{ The device name for which performance statistics are being returned on
  NT4, ie COM1, ISDN1 (base 1).  This property will be constant until a
  reboot to install or remove devices, so it's only necessary to check it
  once after EnablePerfStats.  }
    PROPERTY PerfPortNam [Index: Integer]: string read GetPerfPortNam write PutPerfPortNam ;

  PUBLISHED
// Published declarations

{ Used for ActiveX only }
    Property DAXCtrl : Boolean    read FDAXCtrl write FDAXCtrl default False;

{ The version of TMagRasPer - note is only only made writable so it displays
  in the object inspector }
    Property Version: string      read fVersion write fversion stored False;

{ May give more detailed error information if EnablePerfStats or
  GetPerfStats fails. }
    PROPERTY ErrInfo: String      read fErrInfo write fErrInfo ;

{ On NT4 only, specifies if the Performance Data Helpher library (PDH.DLL)
  should be used instead of the normal registry interface for performance
  statistics.  Sometimes the registry interface fails.  }
//    PROPERTY UsePDH: boolean      read fUsePDHFlag write SetPDHFlag ;

{ The number of ports for which stats are being returned. }
    PROPERTY TotPorts: integer    read fTotPorts ;

  end;

//procedure Register;

implementation

var
  datasize: integer = 0 ;   // performance data buffer size

const

  TOTALBYTES =  8192 ;    // initial buffer size for NT performance data
  BYTEINCREMENT = 1024 ;  // make it bigger

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
    fVersion := MagVersion ;
    fErrInfo := '' ;
end;

destructor TMagRasPer.Destroy;
begin
    inherited Destroy;
end;

function TMagRasPer.GetErrorString(ErrorCode: LongInt): String;
var
    szErrorString: Array[0..256] of WideChar;
begin
    if (NOT MagLoadRasApi) then
    begin
        fLastError := ERROR_DLL_NOT_FOUND ;
        result := RASAPI_DLL + #32 +  SRasGenNotAvailable ; // Not Available
        exit ;
    end ;
    Result := '';
    FillChar (szErrorString, SizeOf (szErrorString), #0);
    RasGetErrorStringW (ErrorCode, szErrorString, 256);
    If szErrorString[0] <> #0 THEN
        Result := szErrorString
    Else
        Result := SysErrorMessage (ErrorCode) ;  // Angus, try a windows error
end;

procedure TMagRasPer.ResetPerfStat (item: integer) ;
begin

// now stuff reported to user for connections
    if item > MaxDevs then exit ;
    fPerfXmitCur [item] := 0 ;
    fPerfRecvCur [item] := 0 ;
    fPerfConnSpd [item] := 0 ;

// windows 2000 and XP, aka nt5 - no attempt to clear separate links
    fLastError := ERROR_DLL_NOT_FOUND ;
    if (NOT MagLoadRasApi) then exit ;
    if NOT Assigned (RasClearConnectionStatistics) then exit ;
    if fPerfRasConn [item] <> 0 then
        fLastError := RasClearConnectionStatistics (fPerfRasConn [item]) ;
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
    end ;
    GetPerfStats ;   // get NT portnames which we just killed
 end ;

function TMagRasPer.EnablePerfStats (Start, Search: boolean): boolean ;
begin
    result := false ;
    fErrInfo := '' ;
    if Win32Platform <> VER_PLATFORM_WIN32_NT then exit ;
    result := true ;

// get starting counters
    if result then
    begin
        if start then result := GetPerfStats ;
        ResetPerfStats ;
    end ;
end;

function TMagRasPer.GetPerfStats: boolean ;
var
    Ras_Stats: TRas_Stats;   // W2K only
    portnr, I: integer ;
begin
    result := false ;
// Windows 2000 and XP have real APIs
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
                    fPerfPortNam [portnr] := 'x' + IntToStr (portnr) ;
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

function TMagRasPer.GetPerfPortNam (Index: Integer): string ;
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

procedure TMagRasPer.PutPerfPortNam (Index: Integer; Value: string) ;
begin
    if value <> fPerfPortNam [Index] then
                       fPerfPortNam [Index] := value ;
end ;
Initialization
finalization

end.
