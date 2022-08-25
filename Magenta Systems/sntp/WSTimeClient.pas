// TWSTimeClient by Chris Barber, Lucas Controls, Inc.
// This component is based on works by Francois Piette, whose tireless efforts
// are greatly appreciated.

// The "raw time" to TDateTime conversion was provided by M.W. Workman.

// NOTE: This decendant of TWSocketExt requires that the PreserveSettings
// property be set to true at all times. For this reason the property is initialized
// as such upon creation and loading. Do not change the value to false unless you
// know what you are doing!


// Revisions:
//    1.0   initial. Provides two methods: GetTime and GetDayTime, and two events:
//          OnTime and OnDayTime. Call GetTime to request a 'time' packet from a time server.
//          When you receive a response you will get an OnTime event which gives you
//          the server's time as a TDateTime value. Call GetDayTime to request a
//          'daytime' string from a time server. This is a pre-formatted string containing
//          the time and date provided by the server. When you receive a response
//          you will get an OnDayTime event which gives you the string.
//
//    Notes: This thing theoretically works with both TCP and UDP time servers,
//    but I have only successfully tested with TCP servers. It may be that I do
//    not have a valid address for a UDP-based time server, or I may have a mistake
//    in the component.


// modified by angus@magsys.co.uk, http://www.magsys.co.uk - 27 March 1999
// UDP time now works (wsocket will not send empty string)
// supporting SNTP protocol (but ignoring fractional seconds and roundtrip time)
// new Protocol property, which determines what to do (time/tcp, time/upd, sntp)
// added some helper routines for UTC time,

// 12 Oct 1999 - turn off range checking to avoid error in ConvTimeStamp

// 25 Sept 2001 - SNTP to host without time server seems to return year 2036,
//                assume that's an error

// 24 July 2002 - support SNTP fractional seconds and round trip time
//                corrected bug in ConvTimeStamp that caused two second error for SNTP
//                added public vars DTRoundtrip, MSecsRoundtrip, DTDifference,
//                    SecsDifference and SecsDiffDisp
//                added functions DateTimeZZtoStr, GetUTCNtpTime, Ntp2TDateTime,
//                    TimeToFSecs, GetNewTime, ChangeSystemTime

// Angus 30 Nov 2005 register to FPiette not Internet


unit WSTimeClient;

interface

uses
  Windows, Messages, SysUtils, DateUtils, Classes, WSocket, WSocketExt ;

{$R-}  // disable range checking


type

//  longword = cardinal ;  // best we can manage for a 32-bit unsigned until Delphi 4

  TTimeEvent = procedure(Sender:TObject; DateTime:TDateTime) of object;
  TDayTimeEvent = procedure(Sender:TObject; DateTime:string) of object;

  TNtpTime = packed record
    Seconds: DWORD ;    // seconds since 1st January 1900
    FracSecs: DWORD ;   // fraction of second
  end;


  SNTPheader = packed record
  	LIvnMmode: 		byte ;    	// 01=leap indicator, 234=version (3), 567=mode (3=client)
    Statum: 		byte ;    	// 0=unspecified, 1=primary, >2 secondary
    Poll: 			byte ;    	// 4 (16 secs), 5 (32 secs), 6 (64 secs), etc
    Precision:		shortint ;  // -6 mains freq clocks, to -20 microsecond accuracy
    RootDelay:  	integer ;   // delay to primary source
    RootDispersion:	integer ;   // error relative to primary source
    RefIdent:   	DWORD ; //  array [0..3] of char ;  // LOCL, PSS, OMEG, GEOS, GPS, etc
    RefTimeStamp: 	TNtpTime ;  // time local clock was set/corrected
    OrigTimeStamp:  TNtpTime ;  // time request left client
    RecvTimeStamp:  TNtpTime ;  // time request arrived at server
    XmitTimeStamp:	TNtpTime ;  // time request left server
  end ;

  TWSTimeClient = class(TWSocketExt)
  private
    { Private declarations }
    FProtocol: string ;
    FOnTime:TTimeEvent;
    FOnDayTime:TDayTimeEvent;
  protected
    { Protected declarations }
    procedure DoReceiveBuffer(Buf:pointer; Count:integer; PeerAddr:string; PeerPort:word);override;
    procedure Loaded;override;
  public
    { Public declarations }
    SNTP: SNTPheader ;
	DTRequest: TDateTime ;
	DTOriginate: TDateTime ;
	DTReceive: TDateTime ;
	DTTransmit: TDateTime ;
	DTDestination: TDateTime ;
    DTRoundtrip: TDateTime ;
    MSecsRoundtrip: integer ;
    DTDifference: TDateTime ;
    SecsDifference: Double ;
    SecsDiffDisp: string ;
    Tag2: integer ;
    ServLI: integer ;
    ServVN: integer ;
    ServMode: integer ;
    constructor Create(AComponent:TComponent);override;
    procedure GetTime;
    procedure GetDayTime;
  published
    { Published declarations }
	property Protocol:  String	  read fProtocol write fProtocol;  // Angus
    property OnTime:TTimeEvent read FOnTime write FOnTime;
    property OnDayTime:TDayTimeEvent read FOnDayTime write FOnDayTime;

  end;


// helper functions - PC can only set UTC time which is basically GMT
  function DateTimeZZtoStr (D: TDateTime): string ;
  function GetUTCTime: TDateTime;
  function GetUTCNtpTime: TNtpTime;
  function SetUTCTime (DateTime: TDateTime): boolean ;
  function ConvTimeStamp (timestamp: LongWord): TDateTime ;
  procedure ByteSwaps (DataPtr: Pointer; NoBytes: integer) ;
  function Ntp2TDateTime (NtpTime: TNtpTime): TDateTime ;
  function TimeToFSecs (T: TDateTime): double ;   // returns seconds with fractions
  function GetNewTime (DateTime, Difference: TDateTime): TDateTime ;
  function ChangeSystemTime (Difference: TDateTime): boolean ;

const
   wsTimePort:word = 37;
   wsDayTimePort:word = 13;
   wsSNTPPort:word = 123 ;
   LongTimeMask = 'hh:nn:ss:zzz' ;

procedure Register;

implementation

// swap any number of bytes, integer, double, extended, anything
// ByteSwaps (@value, sizeof (value)) ;

procedure ByteSwaps(DataPtr : Pointer;NoBytes : integer);
var
  	i : integer;
  	dp : PChar;
  	tmp : char;
begin
  // Perform a sanity check to make sure that the function was called properly
  	if (NoBytes > 1) then
  	begin
    	Dec(NoBytes);
    	dp := PChar(DataPtr);
    // we are now safe to perform the byte swapping
    	for i := NoBytes downto (NoBytes div 2 + 1) do
    	begin
      		tmp := Pchar(Integer(dp)+i)^;
      		Pchar(Integer(dp)+i)^ := Pchar(Integer(dp)+NoBytes-i)^;
      		Pchar(Integer(dp)+NoBytes-i)^ := tmp;
    	end;
  	end;
end;

// convert date/time to tring with milliseconds - 31/12/2002 01:02:03:456

function DateTimeZZtoStr (D: TDateTime): string ;
begin
    result := DateToStr (D) + ' ' + FormatDateTime (LongTimeMask, D) ;
end ;

// get system date and time as UTC/GMT into Delphi time

function GetUTCTime: TDateTime;
var
	SystemTime: TSystemTime;
begin
	GetSystemTime(SystemTime);
	with SystemTime do
	begin
		Result := EncodeTime (wHour, wMinute, wSecond, wMilliSeconds) +
                                                EncodeDate (wYear, wMonth, wDay);
	end ;
end;

// get system date and time as UTC/GMT into NTP Time

function GetUTCNtpTime: TNtpTime;
var
	SystemTime: TSystemTime;
begin
	GetSystemTime(SystemTime);
	with SystemTime do
	begin
		Result.Seconds := Round ((EncodeTime (wHour, wMinute, wSecond, 0) +
                               EncodeDate (wYear, wMonth, wDay) - 2) * SecsPerDay) ;
		Result.FracSecs := Round ($FFFFFFFF * (wMilliseconds / 1000)) ;
	end ;
    ByteSwaps (@Result.Seconds, 4);
    ByteSwaps (@Result.FracSecs, 4);
end;

// set system date and time as UTC/GMT

function SetUTCTime (DateTime: TDateTime): boolean ;
var
	SystemTime: TSystemTime;
begin
  	with SystemTime do DecodeDateTime (DateTime, wYear, wMonth,
                                 wDay, wHour, wMinute, wSecond, wMilliSeconds) ;
	result := SetSystemTime (SystemTime) ;
end ;

function TimeToFSecs (T: TDateTime): double ;   // returns seconds to 3dp
begin
  	try
	  	Result := (MSecsPerday * Frac (T)) / 1000 ;      // time
	  	Result := Result + (Trunc (T) * SecsPerDay) ;    // date
  	except
	  	Result := 0 ;
  	end ;
end;

// convert timestamp into Delphi date and time

// 32-bit wrap at 6h 28m 16s UTC on 7 February 2036.
// after that time assumed to be after 1968 when hi-bit was set

function ConvTimeStamp (timestamp: LongWord): TDateTime ;
begin
    result := 0 ;
    if timestamp = 0 then exit ;
	try
     //	timestamp := timestamp + $7fffffff ;
	//	result := (timestamp / SecsPerDay) + 24855.13480324 + 2.0 ;  2 second error 
        result := (timestamp / SecsPerDay) + 2.0 ;  // correct for 30 Dec 1899 to 1 Jan 1900
	except
    	result := 0 ;
	end ;
end ;

function Ntp2TDateTime (NtpTime: TNtpTime): TDateTime ;
begin
    ByteSwaps (@NtpTime.Seconds, 4);
    ByteSwaps (@NtpTime.FracSecs, 4);
    result := ConvTimeStamp (NtpTime.Seconds) +
                            ((NtpTime.FracSecs / $FFFFFFFF) / SecsPerDay) ;
end ;

constructor TWSTimeClient.Create(AComponent:TComponent);
begin
   	inherited Create(AComponent);
	FOnTime := nil;
   	FOnDayTime := nil;
   	PreserveSettings := true;
   	FProtocol := 'sntp' ;
end;

procedure TWSTimeClient.Loaded;
begin
   	inherited;
   	PreserveSettings := true;
end;

procedure TWSTimeClient.DoReceiveBuffer(Buf:pointer; Count:integer;
											PeerAddr:string; PeerPort:word);
var
   	strbuf: Pchar;
   	len: integer ;
begin
   	inherited DoReceiveBuffer (Buf, Count, PeerAddr, PeerPort);
    DTDestination := GetUTCTime ;   // when time was received here
   	if ((PortNum = wsTimePort) and Assigned (FOnTime) and (Count >= 4)) then
   	begin
      	ByteSwaps (buf, 4);
      	DTTransmit := ConvTimeStamp (longword(Buf^)) ;
        DTReceive := DTTransmit ;
        DTTransmit := DTTransmit ;
        DTOriginate := DTRequest ;

     // calculate round trip time and correction needed, in various formats
        DTRoundtrip := (DTDestination - DTOriginate) ;
        MsecsRoundtrip := Trunc (TimeToFSecs (DTRoundtrip) * 1000) ;
        DTDifference := ((DTTransmit - DTOriginate) + (DTTransmit - DTDestination)) / 2;
        SecsDifference := TimeToFSecs (DTDifference) ;
        SecsDiffDisp := Format ('%.3f', [SecsDifference]) ;

      // tell user what happened
      	FOnTime (self, DTTransmit) ;
      	Close ;    // byebye
   	end
   	else if ((PortNum = wsDayTimePort) and
                                    Assigned (FOnDayTime) and (Count > 0)) then
   	begin
      // the buffer contains an ascii string of the date and time
      	strbuf := StrAlloc (Count + 1) ;
      	try
         	strlcopy (strbuf, pchar(Buf), Count) ;
         	strbuf [Count] := chr(0);
         	FOnDayTime (self, string (strbuf)) ;
      	finally
         	StrDispose(strbuf);
         	Close ;
      	end;
        exit ;
   	end
   	else if ((PortNum = wsSNTPPort) and
					    	Assigned (FOnTime) and (Count >= SizeOf (SNTP))) then
   	begin

    // get record for easier handling
   	  	FillChar (SNTP, SizeOf (SNTP), #0);
        len := count ;
        if len > SizeOf (SNTP) then len := SizeOf (SNTP) ;
        Move (Buf^, SNTP, len) ;
        ServLI := (SNTP.LIvnMmode and $C0) shr 6 ;  // leap indicator
        ServVN := (SNTP.LIvnMmode and $38) shr 3 ;  // version (most seem to 6)
        ServMode := SNTP.LIvnMmode and $07 ;        // 3=client, 4=server, 5=broadcast

     // ensure that we found an SNTP server meeting RFC 2030
        if (ServMode <> 4) or (SNTP.Statum = 0) or
                 (SNTP.Statum > 15) or (SNTP.XmitTimeStamp.Seconds = 0) then
        begin
            Close ;
	        FOnTime(self, 0) ;  // error
            exit ;
        end ;

     // convert NTP time stamps into Delphi dates
        DTTransmit := Ntp2TDateTime (SNTP.XmitTimeStamp) ;
		DTOriginate := Ntp2TDateTime (SNTP.OrigTimeStamp) ;
        if DTOriginate = 0 then DTOriginate := DTRequest ;  // may come back as zero
		DTReceive := Ntp2TDateTime (SNTP.RecvTimeStamp) ;

     // calculate round trip time and correction needed, in various formats
        DTRoundtrip := (DTDestination - DTOriginate) - (DTReceive - DTTransmit);
        MsecsRoundtrip := Trunc (TimeToFSecs (DTRoundtrip) * 1000) ;
        DTDifference := ((DTReceive - DTOriginate) + (DTTransmit - DTDestination)) / 2;
        SecsDifference := TimeToFSecs (DTDifference) ;
        SecsDiffDisp := Format ('%.3f', [SecsDifference]) ;

      // tell user what happened
	    FOnTime (self, DTTransmit) ;
        Close ;
        exit ;
    end
    else
    begin
    // ignore result
        Close ;
	    FOnTime(self, 0) ;  // error
    end ;
end;

procedure TWSTimeClient.GetTime;
begin
    DTRoundtrip := 0 ;
    MSecsRoundtrip := 0 ;
    DTDifference := 0 ;
    SecsDifference := 0 ;
    SecsDiffDisp := '' ;
    DTDestination := 0 ;
    DTTransmit := 0 ;
    DTOriginate := 0 ;
	DTReceive := 0 ;

   	if State <> wsClosed then Abort ;

   	if (lowercase (FProtocol) = 'time/tcp') and (State = wsClosed) then
   	begin
	  	Proto := 'tcp' ;
      	Port := 'time' ;
//    	PortNumber := wsTimePort;
      	Connect;
        DTRequest := GetUTCTime ;    // in case OrigTime gets lost
   	end
   	else
   	if (lowercase (FProtocol) = 'time/udp') then
   	begin
      	if State = wsClosed then
      	begin
         	Proto := 'udp' ;
//  	 	Port := 'time' ;
         	PortNumber := wsTimePort;
         	Connect;
      	end;
        DTRequest := GetUTCTime ;    // in case OrigTime gets lost
      	SendStr(' ');  // Angus, send space instead of nil
   	end
   	else
   	if (lowercase (FProtocol) = 'sntp') then
   	begin
      	begin
   	   	  	Abort ;
	   	  	FillChar (SNTP, SizeOf (SNTP), #0);
		  	SNTP.LIvnMmode := $33 ; //  Li-0, version=3, mode=3 server
   	  	    if State = wsClosed then
    	  	begin
	    	    Proto := 'udp' ;
//       		Port := 'ntp' ;
  		        PortNumber := wsSNTPPort;
         		Connect;
	         	if State <> wsConnected then
                       RaiseException('Not connected to time server');
    	  	end;
            DTRequest := GetUTCTime ;    // in case OrigTime gets lost
          	SNTP.OrigTimeStamp := GetUTCNtpTime ;
	      	Send (@SNTP, SizeOf (SNTP)) ;
 	  	end ;
   	end;
end;


procedure TWSTimeClient.GetDayTime;
begin
   	if (Proto = 'tcp') and (State = wsClosed)then
   	begin
      	PortNumber := wsDayTimePort;
      	Connect;
   	end
   	else
   	if (Proto = 'udp')  then
   	begin
      	if State = wsClosed then
      	begin
         	PortNumber := wsDayTimePort;
         	Connect;
      	end;
      	SendStr(' ');     // Angus, send space instead of nil
   	end;
end;

// get time adjusted by a difference, ie WSTimeClient.DTDifference

function GetNewTime (DateTime, Difference: TDateTime): TDateTime ;
begin
    result := DateTime + Difference ;
end ;

// change PC system time by a difference, ie WSTimeClient.DTDifference

function ChangeSystemTime (Difference: TDateTime): boolean ;
var
    NewUTCTime: TDateTime ;
begin
    NewUTCTime := GetUTCTime + Difference ;
    result := SetUTCTime (NewUTCTime)
end ;

procedure Register;
begin
  RegisterComponents('FPiette', [TWSTimeClient]);
end;

end.
