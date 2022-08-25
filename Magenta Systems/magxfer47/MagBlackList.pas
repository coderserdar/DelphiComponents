unit MagBlackList;

{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}

{
Updated by Angus Robertson, Magenta Systems Ltd, England, 14 Oct 2016
delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

MagBlackList is a component designed to support black listing of remote
IP addresses that attempt to repeated failed logon to TCP/IP servers.
It maintains a list of IP addresses or Values that have previously exceeded a
specific number of failed login attempts, against which new attempts may
be checked.

BlockAttempts - maximum attempts allowed within BlockAfterMins period
BlockAfterMins - period in minutes in which BlockAttempts allowed
BlockForMins - minutes for which further attempts blocked after max reached 

Note: IP Mask, and White List not implemented yet

Baseline - March 2009
2 June 2010 - better attempt to clear old data when removed IPs from blacklist
18 July 2012 - added GetCountAll and GetCountBlock
14 Oct 2016  - added SaveAscii to save strings instead of IP addresses
               added ListName propertry for events
               better check for old saved duplicates 
}

interface

uses
  SysUtils, Classes, Windows, ExtCtrls, MagClasses, MagSubs1, MagSubs4,
  OverbyteIcsTicks64 ;

const
    OneMask = '255.255.255.255' ;
    MaxAttempts = 9999 ;  // 1 Sept 2009, was 100

type

  TBlackRec = record
    Addr: LongWord ;       // 32-bit IP address
    Mask: LongWord ;
    Value: string ;      // Oct 2016
    Ascii: boolean ;     // Oct 2016
    FirstUtime: int64 ;  // Unix time, seconds since 1970
    LastUtime: int64 ;
    Attempts: integer ;
    ReasonId: integer ;
  end ;
  PTBlackRec = ^TBlackRec ;

  TWhiteRec = record
    Addr: LongWord ;
    Mask: LongWord ;
    Value: string ;      // Oct 2016
  end ;

  TBlackLogEvent = procedure (const info: string) of object ;

  TMagBlackList = class(TComponent)
  private
    { Private declarations }
  protected
    { Protected declarations }
    fBlackRecs: array of TBlackRec ;
    fBlackIdx: TFindList ;
    fTotBlackRecs: integer ;
    fWhiteRecs: array of TWhiteRec ;
    fWhiteIdx: TFindList ;
    fTotWhiteRecs: integer ;
    fBlackFile: string ;
    fWhiteFile: string ;
    fTimer: TTimer ;
    fChangedFlag: boolean ;
    fFlushMins: integer ;
    fCleanMins: integer ;
    fFlushTrg: int64 ;
    fCleanTrg: int64 ;
    fBlockAfterMins: integer ;
    fBlockAttempts: integer ;
    fBlockForMins: integer ;
    fSaveAscii: boolean ;  // Oct 2016
    fListName: string ;    // Oct 2016
    fBlackLogEvent: TBlackLogEvent ;
    procedure SetFlushMins (Value: integer) ;
    procedure SetCleanMins (Value: integer) ;
    procedure SetBlackFile (Value: string) ;
    procedure OnTimer (Sender: TObject);
    function ResizeArray (NewLen: integer): integer ;
  public
    { Public declarations }
    constructor Create(Aowner:TComponent); override;
    destructor Destroy; override;
    procedure doLogEvent (const info: string) ;
    procedure FlushToFiles ;
    procedure LoadFromFiles ;
    procedure ClearList ;
    procedure CleanList ;
    procedure RebuildList;
    procedure AddBlackList (const IpAddr, IpMask: string; ReasonId: integer) ;
    procedure AddWhiteList (const IpAddr, IpMask: string) ;
    procedure RemBlackList (const IpAddr: string) ;
    procedure RemWhiteList (const IpAddr: string) ;
    function CheckBlackList (const IpAddr: string): boolean ;
    function GetFullBlackList(const IpAddr: string): TBlackRec ;
    function FailedBlackList (const IpAddr: string; ReasonId: integer): boolean ;
    function InternalAddBlack (const IpAddr, IpMask: string;
                                  MoreAttempts: boolean; ReasonId: integer): TBlackRec ;
    function ReportBlackList (All: boolean): string ;
    function GetCountAll: integer ;    // 18 July 2012
    function GetCountBlock: integer ;   // 18 July 2012
  published
    { Published declarations }
    property SaveAscii: boolean         read fSaveAscii      write fSaveAscii;  // Oct 2016
    property ListName: string           read fListName       write fListName;
    property BlackFile: string          read fBlackFile      write SetBlackFile;
    property WhiteFile: string          read fWhiteFile      write fWhiteFile;
    property FlushMins: integer         read fFlushMins      write SetFlushMins;
    property CleanMins: integer         read fCleanMins      write SetCleanMins;
    property BlockAfterMins: integer    read fBlockAfterMins write fBlockAfterMins;
    property BlockAttempts: integer     read fBlockAttempts  write fBlockAttempts;
    property BlockForMins: integer      read fBlockForMins   write fBlockForMins;
    property BlackLogEvent: TBlackLogEvent read fBlackLogEvent write fBlackLogEvent;

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Magenta Systems', [TMagBlackList]);
end;

{ TMagBlackList }

// must turn off range checking off
{$R-}
{$Q-}


constructor TMagBlackList.Create(Aowner: TComponent);
begin
    inherited;
    SetLength (fBlackRecs, 0) ;
    fBlackIdx := TFindList.Create ;
    fBlackIdx.Sorted := true ;
    fTotBlackRecs := 0 ;
    SetLength (fWhiteRecs, 0) ;
    fWhiteIdx := TFindList.Create ;
    fWhiteIdx.Sorted := true ;
    fTotWhiteRecs := 0 ;
    fBlackFile := '' ;
    fWhiteFile := '' ;
    fChangedFlag := false ;
    SetFlushMins (5) ;
    SetCleanMins (2) ;
    fBlockAfterMins := 5 ;
    fBlockAttempts := 5 ;
    fBlockForMins := 120 ;
    fSaveAscii := false ;  // Oct 2016
    fListName := 'Blacklist' ;  // Oct 2016
    fTimer := TTimer.Create (self) ;
    fTimer.OnTimer := OnTimer ;
    fTimer.Interval := 30 * TicksPerSecond ;
    fTimer.Enabled := true ;
end;

destructor TMagBlackList.Destroy;
begin
    try
        if fChangedFlag then FlushToFiles ;
    except
    end ;
    SetLength (fBlackRecs, 0) ;
    FreeAndNil (fBlackIdx) ;
    SetLength (fWhiteRecs, 0) ;
    FreeAndNil (fWhiteIdx) ;
    FreeAndNil (fTimer) ;
    inherited;
end;

// called by TFindList for sort and find comparison
// swap endian of IP addresses so they sort nicely

function CompareBlackRec (Item1, Item2: Pointer): Integer;
// Compare returns < 0 if Item1 is less than Item2, 0 if they are equal
// and > 0 if Item1 is greater than Item2.
begin
    if PTBlackRec (Item1).Ascii then
    begin
        result := CompareText (PTBlackRec (Item1).Value, PTBlackRec (Item2).Value) ;
    end
    else
    begin
        if PTBlackRec (Item1).Addr = PTBlackRec (Item2).Addr then
            result := 0
        else if EndianLong (PTBlackRec (Item1).Addr) < EndianLong (PTBlackRec (Item2).Addr) then
            result := -1
        else
            result := 1 ;
    end;
end ;

// resize array and list for new records

function TMagBlackList.ResizeArray (NewLen: integer): integer ;
var
    OldLen, K: integer ;
begin
    OldLen := Length (fBlackRecs) ;
    if NewLen < 16 then NewLen := 16 ;
    SetLength (fBlackRecs, NewLen) ;
    for K := OldLen to Pred (NewLen) do
    begin
        with fBlackRecs [K] do
        begin
            Addr := 0 ;  // clear new addresses
            Value := '' ;
            Ascii := false ;
            Attempts := 0 ;  // 2 June 2010 clear record
            FirstUtime := 0 ;
            LastUtime := 0 ;
        end ;
    end;
    fBlackIdx.Capacity := NewLen ;
    result := NewLen ;
    RebuildList;  // 20 May 2009 rebuild index
    doLogEvent (fListName + ': Increased Empty List Size to ' + IntToStr (NewLen)) ;
end ;

// internal add IP address to blacklist, incremented attempts if done already
// information is saved in a dynamic array, with a sorted list pointing to the
// each record used to search actual IP address (saved as 32-bits).  If an IP
// is removed, the IP for the record is set to zero, and the record then re-used.
// The array is increased in size if too short.

function TMagBlackList.InternalAddBlack (const IpAddr, IpMask: string;
                                   MoreAttempts: boolean; ReasonId: integer): TBlackRec ;
var
    BlackRec: TBlackRec ;
    Index, OldLen, K: integer ;
begin
    result.Addr := 0 ;
    result.Value := '' ;
    result.Attempts := 0 ;  // 2 June 2010
    result.ascii := fSaveAscii ;     // Oct 2016
    BlackRec.ascii := fSaveAscii ;   // Oct 2016
    if fSaveAscii then
    begin
        if IpAddr = '' then exit ;
         BlackRec.Value := IpAddr ;  // Oct 2016
    end
    else
    begin
        if NOT Str2IP (IpAddr, BlackRec.Addr) then exit ;  // sanity check
    end ;
    if fBlackIdx.Find (@BlackRec, CompareBlackRec, Index) then
    begin
     // IP already listed, update record and increment attempts
        PTBlackRec (fBlackIdx [Index]).LastUtime := GetUnixTime ;
        if MoreAttempts then
        begin
            if PTBlackRec (fBlackIdx [Index]).Attempts < MaxAttempts then
            begin
                inc (PTBlackRec (fBlackIdx [Index]).Attempts) ;
                fChangedFlag := true ;
            end ;
        end
        else
        begin
            PTBlackRec (fBlackIdx [Index]).Attempts := MaxAttempts ;
            fChangedFlag := true ;
        end ;
        result := PTBlackRec (fBlackIdx [Index])^ ;
    end
    else
    begin
    // create new record for this IP
        Str2IP (IpMask, BlackRec.Mask) ;
        BlackRec.ReasonId := ReasonId ;
        BlackRec.FirstUtime := GetUnixTime ;
        BlackRec.LastUtime := BlackRec.FirstUtime ;
        if MoreAttempts then
            BlackRec.Attempts := 1
        else
            BlackRec.Attempts := MaxAttempts ;
        result := BlackRec ;

       // see if allocating more memory in array
        OldLen := Length (fBlackRecs) ;
        if OldLen <= fTotBlackRecs then OldLen := ResizeArray (OldLen * 2) ;

     // find first free record in array
        Index := -1 ;
        for K := 0 to Pred (OldLen) do
        begin
            if fBlackRecs [K].ascii then
            begin
                if fBlackRecs [K].Value = '' then
                begin
                    Index := K ;
                    break ;
                end ;
            end
            else
            begin
                if fBlackRecs [K].Addr = 0 then
                begin
                    Index := K ;
                    break ;
                end ;
            end;
        end ;
        if Index < 0 then
        begin
            doLogEvent (fListName + ': Internal Error, No Blank Records') ;
            exit ;
        end ;

      // add record to array and sorted list
        fBlackRecs [Index] := BlackRec ;
        fBlackIdx.AddSorted (@fBlackRecs [Index], CompareBlackRec) ;
        inc (fTotBlackRecs) ;
        fChangedFlag := true ;
        doLogEvent (fListName + ': Added New Item: ' + IpAddr +
                                    ', Total Items ' + IntToStr (fTotBlackRecs)) ;
    end ;
end;

// unconditional add IP address to blacklist, no more attempts allowed

procedure TMagBlackList.AddBlackList(const IpAddr, IpMask: string;
                                                      ReasonId: integer);
begin
    InternalAddBlack (IpAddr, IpMask, false, ReasonId) ;
end;

// add IP address and mask to white list

procedure TMagBlackList.AddWhiteList(const IpAddr, IpMask: string);
begin

end;

// get black list record for specific IP address

function TMagBlackList.GetFullBlackList(const IpAddr: string): TBlackRec ;
var
    BlackRec: TBlackRec ;
    Index: integer ;
begin
    result.Addr := 0 ;
    result.Value := '' ;
    result.Attempts := 0 ;  // 2 June 2010
    result.Ascii := fSaveAscii ;   // Oct 2016
    BlackRec.ascii := fSaveAscii ;   // Oct 2016
    if fSaveAscii then
    begin
        if IpAddr = '' then exit ;
         BlackRec.Value := IpAddr ;  // Oct 2016
    end
    else
    begin
        if NOT Str2IP (IpAddr, BlackRec.Addr) then exit ;  // sanity check
    end ;
    if (fTotBlackRecs = 0) then exit ;
    if fBlackIdx.Find (@BlackRec, CompareBlackRec, Index) then
                            result := PTBlackRec (fBlackIdx [Index])^ ;
end;

// check if IP address in black list and exceeded maximum attempts
// return true if blocked

function TMagBlackList.CheckBlackList(const IpAddr: string): boolean;
var
    BlackRec: TBlackRec ;
begin
    result := false ;
    BlackRec := GetFullBlackList (IpAddr) ;
    if (BlackRec.Attempts = 0) then exit ;  // not found
    if (BlackRec.Attempts >= fBlockAttempts) then result := true ;
end;

// notify a failed login attempt, add IP address to black list if not already
// done otherwise increment attempts counter, return true if now blocked

function TMagBlackList.FailedBlackList(const IpAddr: string;
                                            ReasonId: integer): boolean;
var
    BlackRec: TBlackRec ;
begin
    result := false ;
    BlackRec := InternalAddBlack (IpAddr, OneMask, true, ReasonId) ;
    if (BlackRec.Attempts = 0) then exit ;  // not found
    if (BlackRec.Attempts >= fBlockAttempts) then result := true ;
end;

// remove an IP address from black list

procedure TMagBlackList.RemBlackList(const IpAddr: string);
var
    BlackRec: TBlackRec ;
    Index: integer ;
begin
    Blackrec.ascii := fSaveAscii ;   // Oct 2016
    if fSaveAscii then
    begin
        if IpAddr = '' then exit ;
         BlackRec.Value := IpAddr ;  // Oct 2016
    end
    else
    begin
        if NOT Str2IP (IpAddr, BlackRec.Addr) then exit ;  // sanity check
    end ;
    if (fTotBlackRecs = 0) then exit ;
    if fBlackIdx.Find (@BlackRec, CompareBlackRec, Index) then
    begin
        PTBlackRec (fBlackIdx [Index]).Addr := 0 ;
        PTBlackRec (fBlackIdx [Index]).Value := '' ;
        PTBlackRec (fBlackIdx [Index]).Attempts := 0 ;
        PTBlackRec (fBlackIdx [Index]).Ascii := false ;
        PTBlackRec (fBlackIdx [Index]).FirstUtime := 0 ;
        PTBlackRec (fBlackIdx [Index]).LastUtime := 0 ;
        fBlackIdx.Delete (Index) ;
        dec (fTotBlackRecs) ;
        fChangedFlag := true ;
    end ;
end;

// remove an IP address from white list

procedure TMagBlackList.RemWhiteList(const IpAddr: string);
begin
    //
end;

// rebuild black list sorted index to array ignoring blank records
// pending - could also compact list periodically if too many blanks

procedure TMagBlackList.RebuildList;
var
    I: integer ;
begin
    fBlackIdx.Clear ;
    if fBlackIdx.Capacity < (Length (fBlackRecs) * 2) then
                        fBlackIdx.Capacity := Length (fBlackRecs) * 2 ;
    for I := 0 to Pred (Length (fBlackRecs)) do
    begin
        if (fBlackRecs [I].Attempts <> 0) then
                fBlackIdx.AddSorted (@fBlackRecs [I], CompareBlackRec) ;
    end ;
    fTotBlackRecs := fBlackIdx.Count ;
    doLogEvent (fListName + ': Rebuilt List, Total Items ' + IntToStr (fTotBlackRecs)) ;
    fChangedFlag := true ;
end ;

// clean black list by removing any IP addresses that have been blocked but
// whose blocked time has now expired, and IPs not blocked because they did not
// reach the maximum within the allowed period

procedure TMagBlackList.CleanList;
var
    I: integer ;
    rebuild: boolean ;
    utime: int64 ;
begin
    if (fTotBlackRecs = 0) then exit ;
    rebuild := false ;
    utime := GetUnixTime ;
    for I := 0 to Pred (Length (fBlackRecs)) do
    begin
        with fBlackRecs [I] do
        begin
            if (Attempts > 0) then
            begin
                if (Attempts >= fBlockAttempts) then
                begin
                    if (((utime - LastUtime) div 60) >= fBlockForMins) or
                                                        (LastUtime > utime) then
                    begin
                        if Ascii then
                            doLogEvent (fListName + ': Removed Blocked Item: ' + Value)
                        else
                            doLogEvent (fListName + ': Removed Blocked Item: ' + IpToStr (Addr)) ;
                        Addr := 0 ;
                        Ascii := false ;
                        Value := '' ;
                        Attempts := 0 ;  // 2 June 2010 clear record
                        FirstUtime := 0 ;
                        LastUtime := 0 ;
                        rebuild := true ;
                    end ;
                end
                else
                begin
                    if (((utime - FirstUtime) div 60) >= fBlockAfterMins) or
                                                        (FirstUtime > utime) then
                    begin
                        if Ascii then
                            doLogEvent (fListName + ': Removed Old Item: ' + Value)
                        else
                            doLogEvent (fListName + ': Removed Old Item: ' + IpToStr (Addr)) ;
                        Addr := 0 ;
                        Value := '' ;
                        Ascii := false ;
                        Attempts := 0 ;  // 2 June 2010 clear record
                        FirstUtime := 0 ;
                        LastUtime := 0 ;
                        rebuild := true ;
                    end ;
                end ;
            end ;
        end ;
    end ;
    if rebuild then RebuildList;
end;

// clear black list of all IP addresses

procedure TMagBlackList.ClearList;
begin
    fTotBlackRecs := 0 ;
    SetLength (fBlackRecs, 0) ;
    fBlackIdx.Clear ;
    fChangedFlag := true ;
end;

// log event

procedure TMagBlackList.doLogEvent (const info: string) ;
begin
    if Assigned (fBlackLogEvent) then fBlackLogEvent (info) ;
end;

// flush black and white lists to comma separated ASCII files
// this is done automatically every two minutes and when the component
// is destroyed

procedure TMagBlackList.FlushToFiles;
var
    FileLines: TStringList ;
    I: integer ;
    S: string ;
begin
    fChangedFlag := false ;
    if fBlackFile = '' then exit ;  // no file name means nothing to save
    if NOT Assigned (fBlackIdx) then exit ;  // sanity check
    FileLines := TStringList.Create ;
    try
        if (fTotBlackRecs > 0) then
        begin
            for I := 0 to Pred (Length (fBlackRecs)) do
            begin
                with fBlackRecs [I] do
                begin
                    if (Attempts = 0) or (FirstUtime = 0) then continue ;
                    if Ascii then
                    begin
                        S := Value ;
                        if S = '' then continue ;
                    end
                    else
                    begin
                        if Addr = 0 then continue ;
                        S := IpToStr (Addr) ;
                    end;
                    FileLines.Add (S + comma + IpToStr (Mask) + comma +
                        IntToStr (FirstUtime) + comma + IntToStr (LastUtime) + comma +
                                     IntToStr (Attempts) + comma + IntToStr (ReasonId) ) ;
                end ;
            end ;
        end ;
        try
            if FileExists (fBlackFile) then SysUtils.DeleteFile (fBlackFile) ;
            FileLines.SaveToFile (fBlackFile) ;
            doLogEvent (fListName + ': Saved to File: ' + fBlackFile) ;
        except
            doLogEvent (fListName + ': Failed File Save: ' + fBlackFile +
                                         ' - ' + GetExceptMess (ExceptObject)) ;
        end ;
    finally
        FileLines.Free ;
        fChangedFlag := false ;
    end ;
end;

// load black and white lists from comma separated ASCII files, then Clean
// any IP addresses that have expired and flush back to disk
// this is done automatically when the BlackFile property is assigned a file name

procedure TMagBlackList.LoadFromFiles;
var
    FileLines, FileRecord: TStringList ;
    I, BadNr, Index: integer ;
begin
    fChangedFlag := false ;
    if fBlackFile = '' then exit ;  // no file name means nothing to read
    if NOT Assigned (fBlackIdx) then exit ;  // sanity check
    FileLines := TStringList.Create ;
    FileRecord := TStringList.Create ;
    try
        try
            if NOT FileExists (fBlackFile) then
            begin
                doLogEvent (fListName + ': File Not Found: ' + fBlackFile) ;
                exit ;
            end ;
            FileLines.LoadFromFile (fBlackFile) ;
        except
            doLogEvent (fListName + ': Failed to File Load: ' + fBlackFile +
                                         ' - ' + GetExceptMess (ExceptObject)) ;
            exit ;
        end ;
        ClearList ;
         if FileLines.Count > 0 then
        begin
            BadNr := 0 ;
            ResizeArray (FileLines.Count * 2) ;
            for I := 0 to Pred (FileLines.Count) do
            begin
                FileRecord.CommaText := FileLines [I] ;
                if FileRecord.Count >= 6 then
                begin
                    with fBlackRecs [fTotBlackRecs] do
                    begin
                        Ascii := FSaveAscii ;
                        if FSaveAscii then
                            Value := FileRecord [0]
                        else
                            Str2IP (FileRecord [0], Addr) ;
                        if ((Value <> '') or (Addr <> 0)) and (AscToInt64 (FileRecord [2]) > 0) and
                                                                    (AscToInt (FileRecord [4]) > 0) then
                        begin
                            if NOT fBlackIdx.Find (@fBlackRecs [fTotBlackRecs], CompareBlackRec, Index) then
                            begin
                                Str2IP (FileRecord [1], Mask) ;
                                FirstUtime := AscToInt64 (FileRecord [2]) ;
                                LastUtime := AscToInt64 (FileRecord [3]) ;
                                Attempts := AscToInt (FileRecord [4]) ;
                                ReasonId := AscToInt (FileRecord [5]) ;
                                fBlackIdx.AddSorted (@fBlackRecs [fTotBlackRecs],
                                                                     CompareBlackRec) ;
                                inc (fTotBlackRecs) ;
                            end
                            else
                            begin
                                 Addr := 0 ;
                                 Value := '' ;
                                 doLogEvent (fListName + ': Ignored old saved duplicate') ;
                                 inc (BadNr) ;
                            end;
                        end
                        else
                            inc (BadNr) ;
                    end ;
                end
                else
                    inc (BadNr) ;
            end ;
            doLogEvent (fListName + ': Loaded from File: ' + fBlackFile +
                               ', Total Items ' + IntToStr (fTotBlackRecs) +
                                            ', Bad Records ' + IntToStr (BadNr)) ;
            fChangedFlag := false ;
            CleanList;
            if fChangedFlag then FlushToFiles ;
        end
        else
            doLogEvent (fListName + ': File Empty: ' + fBlackFile) ;

    finally
        FileLines.Free ;
        FileRecord.Free ;
        fChangedFlag := false ;
    end ;
end;

// timer called every 30 seconds to Clean and Flush lists

procedure TMagBlackList.OnTimer(Sender: TObject);
begin
    fTimer.Enabled := false ;
    try
        if IcsTestTrgTick64 (fCleanTrg) then
        begin
            fCleanTrg := IcsGetTrgMins64 (fCleanMins) ;
            CleanList ;
            if fChangedFlag then FlushToFiles ;
        end ;
        if IcsTestTrgTick64 (fFlushTrg) then
        begin
            fFlushTrg := IcsGetTrgMins64 (fFlushMins) ;
            if fChangedFlag then FlushToFiles ;
        end ;

    finally
        fTimer.Enabled := true ;
    end ;
end;

function TMagBlackList.ReportBlackList (All: boolean): string ;
var
    I: integer ;
begin
    result := '' ;
    if (fTotBlackRecs = 0) then exit ;
    for I := 0 to Pred (fBlackIdx.Count) do
    begin
        with PTBlackRec (fBlackIdx [I])^ do
        begin
            if (Attempts >= fBlockAttempts) or All then
            begin
                if Ascii then
                begin
                    if Value = '' then continue ;
                    result := result + Value ;
                end
                else
                begin
                    if Addr = 0 then continue ;
                    result := result + IpToStr (Addr) ;
                end;
                result := result + ' attempts ' + IntToCStr (Attempts) + ', first at ' +
                    TimeToStr (TStamptoDT (FirstUtime)) + ', last at ' + TimeToStr (TStamptoDT (LastUtime)) ;
                if Attempts >= fBlockAttempts then result := result + ' BLOCKED' ;
                result := result + CRLF ;
            end;
        end;
    end;
end;

function TMagBlackList.GetCountAll: integer ;    // 18 July 2012
var
    I: integer ;
begin
    result := 0 ;
    if (fTotBlackRecs = 0) then exit ;
    for I := 0 to Pred (fBlackIdx.Count) do
    begin
        with PTBlackRec (fBlackIdx [I])^ do
        begin
           if (Attempts = 0) then continue ;
           inc (result) ;
        end;
    end;
end;

function TMagBlackList.GetCountBlock: integer ;   // 18 July 2012
var
    I: integer ;
begin
    result := 0 ;
    if (fTotBlackRecs = 0) then exit ;
    for I := 0 to Pred (fBlackIdx.Count) do
    begin
        with PTBlackRec (fBlackIdx [I])^ do
        begin
            if (Attempts = 0) then continue ;
            if (Attempts >= fBlockAttempts) then inc (result) ;
        end;
    end;
end;

// property setters

procedure TMagBlackList.SetCleanMins(Value: integer);
begin
    fCleanMins := Value ;
    if fCleanMins = 0 then fCleanMins := 2 ;
    fCleanTrg := IcsGetTrgMins64 (fCleanMins) ;
end;

procedure TMagBlackList.SetFlushMins(Value: integer);
begin
    fFlushMins := Value ;
    if fFlushMins = 0 then fFlushMins := 2 ;
    fFlushTrg := IcsGetTrgMins64 (fFlushMins) ;
end;

procedure TMagBlackList.SetBlackFile(Value: string);
begin
    if Value <> fBlackFile then
    begin
        fBlackFile := Value ;
        if fTotBlackRecs = 0 then LoadFromFiles;  // don't kill existing black list
    end ;
end;


end.
