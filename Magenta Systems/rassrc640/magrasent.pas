Unit magrasent;
{$IFNDEF VER140}
  {$WARN UNSAFE_TYPE off}
  {$WARN UNSAFE_CAST off}
  {$WARN UNSAFE_CODE off}
{$ENDIF}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}

Interface

Uses SysUtils, Windows, Classes, MagRasApi, MagRasStr, MagSubs1 ;

{ DELPHI RAS COMPONENT - Entry List
Created by Angus Robertson, Magenta Systems Ltd, England
in 2001, delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd, 2013   }

const
  MagVersion = 'TMagRasEnt, 12th August 2013 - Release 5.72, Copyright 2013, Magenta Systems Ltd' ;

{
Changes in 4.60
Added MagRasGetEntryList which fills MagRasEntryRecs with sorted entry names
  and phonebook paths (W2K only)
Added MagRasGetEntryRecs similar to MagRasGetEntryList but fills phone number,
   devices and ports, 100 times faster on NT4/W2K than using RAS APIs
Added MagRasGetPhoneBookFile function that loads MagRasPhoneBookInf StringList
   from rasphone.pbk files
Added MagRasIsPhoneBookNew to check if rasphone.pbk files have changed since
   last read by MagRasGetPhoneBookFile
Added MagRasGetEntryKey function to return keyed data for an entry in
   MagRasPhoneBookInf

Changes in 4.61
Fixed possible range error in MagRasGetEntryList if no installed entries

Changes in 4.70
Supporting Windows XP
Corrected MagRasGetEntryRecs to convert ISDN11-0 to ISDN1 on Windows XP

Changes in 4.90
MagRasEntryRecs is now a dynamic array (Delphi 4 and later) to save space
Removed 100 maximum number of supported phonebooks/entryrecs, now unlimited

Changes in 5.10
Supporting Windows Vista (and maybe Longhorn)

Changes in 5.20
Changed Flags to PBLocation in TEntryRec to make it's use more obvious

Changes in 5.21
Always use correct phonebook for entry properties

Changes in 5.40
Made compatible with Delphi 2009, but still using ANSI RAS functions, not Unicode
Many functions deliberately use AnsiString for compatability with PAnsiChar APIs
rasphone.pbk has UTF-8 encoding, so decode it to ANSI

Changes in 5.60
Fixed cast warnings for Delphi 2009 and later

}

type

{ Connection entry record built by MagRasGetEntryList and MagRasGetEntryRecs.
  It includes the EntryName, PBLocation (W2K and later, Phonebook Location
  REN_Allusers or REN_User), Phonebook filename (W2K and later),
  CanonNum is the canonical telephone number,
  DevName1/DevType1/DevPort1 are the first device name, type and port, while
  multilink sets the 2 suffic versions.  Note that some translation occurs
  to make the device type and port match the dialogs (ie ISDN6-0 becomes
  ISDN1). RpbkLine is the entry top line number, see MagRasGetEntryKey.  }
  TEntryRec = record
    EntryName: AnsiString ;
    PBLocation: integer ;  // 5.20 was Flags, but make purpose more obvious
    Phonebook: AnsiString ;
    CanonNum: AnsiString ;
    DevName1: AnsiString ;
    DevType1: AnsiString ;
    DevPort1: AnsiString ;
    DevName2: AnsiString ;
    DevType2: AnsiString ;
    DevPort2: AnsiString ;
    RpbkLine: integer ;
  end ;

var
{ Dynamic array of connection entry records, MagRasGetEntryList and
  MagRasGetEntryRecs, check MagRasNumEntryRec for size. }
  MagRasEntryRecs: array of TEntryRec ;   // dynamic array to save space, 4.90

{ Number of records in EntryRecs, set by MagRasGetEntryList and
  MagRasGetEntryRecs. }
  MagRasNumEntryRec: integer = 0 ;

{ Clear entry list MagRasEntryRecs array.  Called by MagRasGetEntryList. }
  procedure MagRasClearEntryList ;

{ Build connection entry list as a data array.  This is the miminal
  information version using RasEnumEntries to get one MagRasEntryRecs TEntryRec
  per entry, filling EntryName, PBLocation and Phonebook (last two W2K and
  later).  The list is sorted by EntryName.  PhoneBookPath is only needed
  for NT4, it's ignored by Win9x, W2K and later.

  Do not call this function after MagRasGetEntryRecs because the extra
  entry and device data will be lost.

  Returns 0 if successful, or an error code.

  Properties set are MagRasEntryRecs data array and MagRasNumEntryRec.}
  function MagRasGetEntryList (const PhoneBookPath: AnsiString): integer;

{ Build connection entry list as a data array.  This is the full information
  version that uses MagRasGetEntryList to get one MagRasEntryRecs TEntryRec
  per entry, and then completes the remainder of the record with canonical
  telephone number number, device name, type and ports.  The data array will
  then contain sufficient information about each connection entry to display
  a menu similar to the Explorer Dial-Up Connections window.  Will return an
  unlimited number of entries, tested with over 250 when it does slow down.
  PhoneBookPath is only needed for NT4, it's ignored by Win9x, W2K and later.

  On NT4/W2K reads the physical phonebook INI file since much faster than
  using API calls. On W9x, uses RasGetEntryProperties API calls (but the
  registry reads faster than INI file).  Calls MagRasIsPhoneBookNew,
  MagRasGetPhoneBookFiles, MagRasGetEntryKey and MagRasGetEntryList.

  Returns 0 if successful, or an error code.

  Properties set are MagRasEntryRecs data array and MagRasNumEntryRec.}
  function MagRasGetEntryRecs (const PhoneBookPath: AnsiString;
                                        const UseAPI: boolean): integer ;

{ Splits a canonical phone number into CountryCode, AreaCode,
  LocalPhoneNumber and UseCountryAndAreaCodes.

  The canonical number forrmat is '+44 (845) 123456' or
  '+country code (area code) local number'. }
  procedure MagRasUnpackCanonical (const PhoneCanonical: AnsiString;
      var UseCountryAndAreaCodes: boolean; var CountryCode: integer;
                                var AreaCode, LocalPhoneNumber: AnsiString) ;

{ Creates a canonical phone number from CountryCode, AreaCode and
  LocalPhoneNumber, if UseCountryAndAreaCodes is true.

  The canonical number forrmat is '+44 (845) 123456' or
  '+country code (area code) local number'.  A canonical number may be
  missing the + in which case it will not be processed by dialling properties.
  The canonical number is passed to TranslateAddr or TransAddr to translate
  the number in a dialable number with optional calling card, etc

  Returns the canonical number. }
  function MagRasGetCanonical (const UseCountryAndAreaCodes: boolean; const
        CountryCode: DWORD; const AreaCode, LocalPhoneNumber: AnsiString): AnsiString ;

{ Loads the contents of the system phonebook INI files (one on NT4, two on
  W2K) into a string list allowing keyed data to be examined.  Note the
  entries are not in any sorted order, and may have duplicate keys for
  multilink.  The actual file names loaded are specified in the array
  MagRasPhoneFiles which is set by MagLoadRasApi when the component is
  first loaded to the system default phonebooks.

  Returns the number of phonebook entries found, or zero if none.

  Properties set are PhoneBookInf.  Not supported on Win9x. }
  function MagRasGetPhoneBookFiles: integer ;

{ Checks if any of the MagRasPhoneFiles have been updated since they was
  loaded by MagRasGetPhoneBookFiles, NT4/W2K only.

  Returns true if phonebooks have changed, in which case MagRasGetEntryRecs
  should be called to get updated connection entry information.  }
  function MagRasIsPhoneBookNew: boolean ;

{ Reads phonebook entry data from the system phonebook files, NT4/W2K only.
  This is an alternative means of getting entry data to using the normal
  RasGetEntryProperties API call.  The two advantages over the API are pure
  speed (NT4/W2K seems to open the file afresh for each access), and to
  access data that is not returned by the APIs, in particular the TAPI
  port.

  It is beyond this help to detail all the keys in the INI files,
  but examining the raw files and comparing the keys against the API
  documentation and property dialogs is straightforward.  Note that this
  component does not currently support updating the phonebook files, which
  could potentially be dangerous since Microsoft regularly changes the
  keys.

  EntryName is the connection entry, Line is starting line in PhoneBookInf
  for the entry or zero if the whole file is to be searched, Key is the
  element to locate, Instance is 0 or 1 for first instance of Key or the
  specific instance, return value is the data found, or space if key not
  found.

  If Key is blank and Line is -1, result is the line number of entry
  in ASCII.  This may be saved as a numeric, and used in subsequent calls
  to MagRasGetEntryKey to speed up locating the entry.  The Line is also
  available from the TEntryRec RpbkLine element set by MagRasGetEntryRecs. }
  function MagRasGetEntryKey (const EntryName: AnsiString; const Line: integer;
                         const Key: AnsiString; const Instance: integer): AnsiString ;

implementation

// clear entry list

procedure MagRasClearEntryList ;
//var
//    I: integer ;
begin
    MagRasNumEntryRec := 0 ;
    SetLength (MagRasEntryRecs, 0) ;
 {   for I := 0 to Pred (MaxPhonebooks) do
    begin
        with MagRasEntryRecs [I] do
        begin
            EntryName := '' ;
            CanonNum := '' ;
            DevType1 := '' ;
            DevName1 := '' ;
            DevPort1 := '' ;
            DevType2 := '' ;
            DevName2 := '' ;
            DevPort2 := '' ;
            RpbkLine := 0 ;
            Flags := 0 ;
            Phonebook := '' ;
        end ;
    end ;   }
end ;

// alternate way of showing phone number

function MagRasGetCanonical (const UseCountryAndAreaCodes: boolean; const
        CountryCode: DWORD; const AreaCode, LocalPhoneNumber: AnsiString): AnsiString ;
begin
    if UseCountryAndAreaCodes then
    begin
        result := '+' + IntToStrAnsi (CountryCode) + ' ' ;
        if (AreaCode <> '') then result := result + '(' + AreaCode + ') ' ;
        result := result + LocalPhoneNumber ;
    end
    else
        result := LocalPhoneNumber ;
end ;

procedure MagRasUnpackCanonical (const PhoneCanonical: AnsiString;
    var UseCountryAndAreaCodes: boolean; var CountryCode: integer;
                                var AreaCode, LocalPhoneNumber: AnsiString) ;
var
    temp: AnsiString ;
    loc: integer ;
begin
    UseCountryAndAreaCodes := false ;
    LocalPhoneNumber := '' ;
    CountryCode := 0 ;
    AreaCode := '' ;
    temp := TrimAnsi (PhoneCanonical) ;
    if Length (temp) < 2 then exit ;
    if temp [1] = '+' then
    begin
        UseCountryAndAreaCodes := true ;

    // find country code, digits following + sign and strip from temp number
        loc := CharPos (' ', temp) ;
        if loc > 2 then CountryCode := AscToIntAnsi (Copy (temp, 2, loc - 2)) ;  // 4.60
        if loc = 0 then loc := 1 ;
        temp := TrimAnsi (Copy (temp, loc + 1, 99)) ;  // remove country code

    // !!! should really lookup country ID from country code table

    // see if area code specified, in brackets, it is optional (ie Hong Kong)
        loc := CharPos (')', temp) ;
        if loc > 2 then
        begin
            AreaCode := Copy (temp, 2, loc - 2) ;
            temp := Copy (temp, loc + 2, 99) ;  // remove area code
        end
        else
            AreaCode := ' ' ;  // blank with space
    end ;

// keep full phone number (non canonical) or local phone number (canonical)
    LocalPhoneNumber := temp ;
end ;

// check if physical rasphone.pbk files (NT4/W2K) have been updated since being loaded

function MagRasIsPhoneBookNew: boolean ;
var
    newage, I: integer ;
begin
    result := false ;
    if MagRasOSVersion = OSW9x then
    begin
        result := true ;
        exit ;
    end ;
    for I := 0 to Pred (MaxPhoneBkFiles) do
    begin
        if MagRasPhoneFiles [I] <> '' then
        begin
            newage := FileAge (String (MagRasPhoneFiles [I])) ;  // 9 Aug 2010
            if (newage >= 0) and (MagRasPhoneAges [I] <> newage) then
                                                            result := true ;
        end ;
    end ;
end ;

// read physical rasphone.pbk (NT4/W2K) files, return number of entries
// note that two or more files may be loaded
// file is basically an INI file, but may have repeated keys for multilink
// Warning - W2K may read any *.pbk files it finds!!!
// note: rasphone.pbk has UTF-8 encoding!!!!

function MagRasGetPhoneBookFiles: integer ;
var
    I, J: integer ;
    templist: TStringList ;
begin
    result := 0 ;
    MagRasPhoneBookInf.Clear ;
    if MagRasOSVersion = OSW9x then exit ;
    templist := TStringList.Create ;
    try
        for I := 0 to Pred (MaxPhoneBkFiles) do
        begin
            if MagRasPhoneFiles [I] <> '' then
            begin
                if FileExists (String (MagRasPhoneFiles [I])) then  // 9 Aug 2010
                begin
                   try
                        templist.Clear ;
                        templist.LoadFromFile (String (MagRasPhoneFiles [I])) ;  // 9 Aug 2010
                        templist.Text := Utf8ToAnsi (RawByteString (templist.Text)) ; // 11 Aug 2008 // 9 Aug 2010
                        MagRasPhoneAges [I] := FileAge (String (MagRasPhoneFiles [I])) ; // 9 Aug 2010
                        if templist.Count = 0 then exit ;
                        for J := 0 to Pred (templist.Count) do
                        begin
                            if length (templist [J]) >= 3 then
                            begin
                                if templist [J] [1] = '[' then inc (result) ;
                            end ;
                        end ;
                        MagRasPhoneBookInf.AddStrings (templist) ;
                    except
                    end ;
                end ;
            end ;
        end ;
    finally
        templist.Free ;
    end ;
end ;

// get data for specified key in specified connection entry from rasphone.pbk
// if line is >=0, it saves searching whole file
// instance is 0 or 1 for first instance of key, or > 1

function MagRasGetEntryKey (const EntryName: AnsiString; const Line: integer;
                          const Key: AnsiString; const Instance: integer): AnsiString ;
var
    I, curline, curinst: integer ;
    locentry, lockey: AnsiString ;
begin
    result := '' ;
    if MagRasPhoneBookInf.Count = 0 then exit ;
    locentry := '[' + EntryName + ']' ;
    curline := Line ;
    curinst := Instance ;

// check line number in INI file matches entry name
    if (curline >= 0) and (line < MagRasPhoneBookInf.Count) then
    begin
        if MagRasPhoneBookInf [curline] <> String (locentry) then curline := -1 ;  // 9 Aug 2010
    end ;

// otherwise find start of entry
    if (curline < 0) or (curline >= MagRasPhoneBookInf.Count) then
    begin
        for I := 0 to Pred (MagRasPhoneBookInf.Count) do
        begin
            if MagRasPhoneBookInf [I] = String (locentry) then    // 9 Aug 2010
            begin
                curline := I ;
                break ;
            end ;
        end ;
    end ;
    if curline < 0 then exit ;

// see if searching for top entry line only
    if key = '' then
    begin
        result := IntToStrAnsi (curline) ;
        exit ;
    end ;
    inc (curline) ;
    if curinst >= 1 then dec (curinst) ;

// then look for specific key in this section
    lockey := key + '=' ;
    for I := curline to Pred (MagRasPhoneBookInf.Count) do
    begin
        if length (MagRasPhoneBookInf [I]) >= 3 then
        begin
            if Pos (lockey, AnsiString (MagRasPhoneBookInf [I])) = 1 then // 9 Aug 2010
            begin
                if curinst = 0 then
                begin
                    result := Copy (AnsiString (MagRasPhoneBookInf [I]),
                                                Succ (length (lockey)), 99) ;   // 9 Aug 2010
                    exit ;
                end ;
                dec (curinst) ;
            end ;
            if MagRasPhoneBookInf [I] [1] = '[' then exit ; // next section
        end ;
    end ;
end ;

// get sorted list of defined phonebook entries (aka DUN connections) into dynamic array
// this is called by MagRasCon.GetPhoneBookEntries and MagRasGetEntryRecs

function MagRasGetEntryList (const PhoneBookPath: AnsiString): integer;
var
    RasEntryNameAll: TRasEntryNameW2K ;
    I, startoff, endoff, rlen, totents: integer ;
    BufSize, Entries: DWord;
    EntBuf, curEntry: PAnsiChar ;    // 4.90 dynamic instead of fixed memory
    SortList: TStringList ;
    temp: AnsiString ;
    PBPtr: Pointer ;
begin
    MagRasClearEntryList ;
    if (NOT MagLoadRasApi) then
    begin
        result := ERROR_DLL_NOT_FOUND ;
        exit ;
    end ;
    Entries := 0 ;
    PBPtr := Nil ;
 // 5.20 always get all users and current user for W2K and later
    if (PhoneBookPath <> '') and (MagRasOSVersion = OSNT4) then
                                        PBPtr := PAnsiChar (PhoneBookPath) ;
    rlen := SizeOf (TRasEntryName) ;
    if MagRasOSVersion >= OSW2K then rlen := SizeOf (TRasEntryNameW2K) ;
    BufSize := rlen ;   // 4.90 get one entry to start, to find how much memory needed
    GetMem (EntBuf, BufSize) ;     // 4.90 dynamic instead of fixed memory
    SortList := TStringList.Create ;
    try
        FillChar (EntBuf^, BufSize, 0) ;
        PRasEntryNameW2K (EntBuf).dwSize := rlen ;
        result := RasEnumEntries (Nil, PBPtr, PRasEntryNameW2K (EntBuf),
                                                 BufSize, Entries) ;  // one entry only
        if (result <> 0) and (result <> ERROR_BUFFER_TOO_SMALL) then exit ;
        if (Entries = 0) then exit ;
        ReallocMem (EntBuf, BufSize) ;  // 4.90 allocate memory for all entries
        FillChar (EntBuf^, BufSize, 0) ;
        PRasEntryNameW2K (EntBuf).dwSize := rlen ;
        result := RasEnumEntries (Nil, PBPtr, PRasEntryNameW2K (EntBuf),
                                                BufSize, Entries) ;   // get all entries
        if (result <> 0) then exit ;
        if (Entries = 0) then exit ;  // 4.61, stop now if nothing
        SetLength (MagRasEntryRecs, Entries) ;  // 4.90 allocate space for entries

// get entries from array, elements are longer for W2K
// combine as text into stringlist to enable sorting
        curEntry := EntBuf ; // @EntriesBuff ;
        totents := 0 ;
        for I := 0 to Pred (Entries) do
        begin
            FillChar (RasEntryNameAll, SizeOf (RasEntryNameAll), 0);
            Move (curEntry^, RasEntryNameAll, rlen) ;
            with RasEntryNameAll do
            begin
                if szEntryName [0] <> #0 then
                begin
                    SortList.Add (String (szEntryName) + #9 +
                        IntToStr (dwFlags) + #9 + String (szPhonebookPath)) ;  // 9 Aug 2010
                    inc (totents) ;
                end ;
            end ;
            inc (curEntry, rlen) ;
        end ;
        SortList.Sort ;

    // extract from string list into array
        for I := 0 to Pred (totents) do
        begin
            temp := AnsiString (SortList [I]) ; // 9 Aug 2010
            with MagRasEntryRecs [I] do
            begin
                endoff := Pos (#9, String (temp)) ;  // 9 Aug 2010
                EntryName := Copy (temp, 1, pred (endoff)) ;  // keep name
                startoff := succ (endoff) ;
                endoff := Pos (#9, Copy (String (temp), startoff, 999)) ;  // 9 Aug 2010
                PBLocation := AscToIntAnsi (Copy (temp, startoff, pred (endoff))) ; // keep phonebook location 5.20 17 May 2007
                startoff := startoff + endoff ;
                Phonebook := Copy (temp, startoff, 999) ;     // keep phonebook path
            end ;
        end ;
        MagRasNumEntryRec := totents ;  // set last in case of errors
    finally
        SortList.Free ;
        FreeMem (EntBuf) ;
    end ;
end;

// build connection entry list, depends on OS
// on NT4/W2K use physical rasphone.pbk since much faster than API calls
// on W9x, meed to use proper API calls (but registry reads faster than INI file)
// On a PIII/800 with W2K, refreshing 32 entries using the APIs takes 1.61 secs
// (5.43 sec with Delphi debugger), but 10-20ms reading rasphone.pbk
// On a PII/350 with Win98, refreshing 20 entries takes 22-25ms using RAS APIs.

function MagRasGetEntryRecs (const PhoneBookPath: AnsiString;
                                        const UseAPI: boolean): integer ;
var
    EntryInfo: array [0..1200] of DWORD ;
    RasEntry: TRasEntry ;
    EntrySize, tempsize: DWORD ;
    I, line, res: integer ;
    newentry, temp: AnsiString ;
    UseCountryAndAreaCodes: boolean ;
    CountryCode: integer;
    AreaCode, LocalPhoneNumber: AnsiString ;
    PBPtr: Pointer ;

    function GetCurKey (const key: AnsiString; const instance: integer): AnsiString ;
    begin
        result := MagRasGetEntryKey (newentry, line, key, instance) ;
    end ;

  // W2K ports in phone book look like ISDN6-0, ISDN2-1, ISDN11-0, VPN2-0, PPPoE7-0, etc

    function TranslatePort (const ntport: AnsiString): AnsiString ;
    var
        sep, port: integer ;
    begin
        result := UppercaseAnsi (ntport) ;
        if length (ntport) < 6 then exit ;
        sep := CharPos ('-', ntport) ;
        if sep < 4 then exit ;
        port := Succ (AscToIntAnsi (copy (ntport, Succ (sep), 9))) ;  // get port number, add one
        if (copy (ntport, 1, 4) = 'ISDN') then
                                    result := 'ISDN' + IntToStrAnsi (port) ;
        if (copy (ntport, 1, 3) = 'VPN') then
                                    result := 'VPN1' + IntToStrAnsi (port) ;
        if copy (result, 1, 5) = 'PPPOE' then result := 'PPPoE1' ;   // Windows XP broadband
    end ;

begin
    result := MagRasGetEntryList (PhoneBookPath) ;
    if (Result <> 0) then exit ;
    if (MagRasNumEntryRec = 0) then exit ;

// use RAS API
    if (NOT UseAPI) and (MagRasOSVersion <> OSW9x) then
    begin

    // use physical rasphone.pbk, checking file timestamp to reread
        if MagRasIsPhoneBookNew then MagRasGetPhoneBookFiles ;
        if MagRasPhoneBookInf.Count <> 0 then
        begin
            for I := 0 to Pred (MagRasNumEntryRec) do
            begin
                with MagRasEntryRecs [I] do
                begin
                    newentry := EntryName ;
                // find and keep entry first line in INI file, saves searching for each key
                    line := -1 ;
                    temp := MagRasGetEntryKey (newentry, line, '', 1) ;
                    if temp <> '-1' then
                    begin
                       line := AscToIntAnsi (temp) ;
                       RpbkLine := line ;
                    end ;

               // get various keys from entry
                    temp := GetCurKey ('UseDialingRules', 1) ;  // NT4 may not have this key
                    if temp = '' then temp := GetCurKey ('UseCountryAndAreaCodes', 1) ;
                    UseCountryAndAreaCodes := (temp = '1') ;
                    CountryCode := AscToIntAnsi (GetCurKey ('CountryCode', 1)) ;
                    AreaCode := GetCurKey ('AreaCode', 1) ;
                    LocalPhoneNumber := GetCurKey ('PhoneNumber', 1) ;
                    CanonNum := MagRasGetCanonical (UseCountryAndAreaCodes,
                                  CountryCode, AreaCode, LocalPhoneNumber) ;
                    DevType1 := LowerCaseAnsi (GetCurKey ('DEVICE', 1)) ;
                    DevName1 := GetCurKey ('Device', 1) ;
                    DevPort1 := TranslatePort (GetCurKey ('Port', 1)) ;
                    DevType2 := LowerCaseAnsi (GetCurKey ('DEVICE', 2)) ;
                    DevName2 := GetCurKey ('Device', 2) ;
                    DevPort2 := TranslatePort (GetCurKey ('Port', 2)) ;

                // fix bad W2K device types, first device only
                    if DevType1 = 'rastapi' then
                    begin
                        if Pos ('VPN', String (DevPort1)) >= 1 then  // 9 Aug 2010
                                                    DevType1 := RASDT_Vpn ;
                        if Pos ('LPT', String (DevPort1)) >= 1 then  // 9 Aug 2010
                                                DevType1 := RASDT_Parallel ;
                        if Pos ('PPPoE', String (DevPort1)) >= 1 then  // 9 Aug 2010
                                                DevType1 := RASDT_PPPoE ;   // Windows XP broadband
                    end ;
                end ;
            end ;
        end ;
    end ;

// now check for any elements not yet filled and use API - generally W9x only
  for I := 0 to Pred (MagRasNumEntryRec) do
   begin
        with MagRasEntryRecs [I] do
        begin
            if DevType1 = '' then
            begin
                EntrySize := Sizeof (EntryInfo) ;
                FillChar (EntryInfo, EntrySize, 0);
                EntryInfo [0] := Sizeof (TRasEntry) ;
                PBPtr := Nil ;
             // 5.21 set correct phonebook
                if (PhoneBook <> '') then PBPtr := PAnsiChar (PhoneBook) ;
                res := RasGetEntryProperties (PBPtr, PAnsiChar (EntryName),
                                         @EntryInfo, EntrySize, Nil, tempsize) ;
                if res = 0 then
                begin
                    Move (EntryInfo, RasEntry, Sizeof (TRasEntry)) ;
                    with RASEntry do   // get properties in same order as RASENTRY
                    begin
                        CanonNum := MagRasGetCanonical (dwfOptions And
                                RASEO_UseCountryAndAreaCodes =
                                  RASEO_UseCountryAndAreaCodes, dwCountryCode,
                                            szAreaCode, szLocalPhoneNumber) ;
                        DevType1 := LowerCaseAnsi (szDeviceType) ;
                        DevName1 := szDeviceName ;
                      // ignore sub entries
                    end ;
                end ;
            end ;
        end ;
    end ;

end ;

initialization
    MagRasClearEntryList ;
finalization
    MagRasClearEntryList ;
end.


