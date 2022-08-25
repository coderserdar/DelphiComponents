Unit MagRasEntW;
{$IFNDEF VER140}
  {$WARN UNSAFE_TYPE off}
  {$WARN UNSAFE_CAST off}
  {$WARN UNSAFE_CODE off}
{$ENDIF}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}

Interface

Uses SysUtils, Windows, Classes,
    MagRasApiW, MagRasStr, MagSubs1 ;

{ DELPHI RAS COMPONENT - Entry List
Created by Angus Robertson, Magenta Systems Ltd, England
in 2001, delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd, 2012   }

const
  MagVersion = 'TMagRasEnt, 6th June 2017 - Release 6.40, Copyright 2017, Magenta Systems Ltd' ;

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
rasphone.pbk has UTF-8 encoding, so decode it to ANSI

Changes in 6.00
Support RAS WideChar APIs and Unicode for Delphi 2009 with new MagRasxxxW units
Removed support for Win9x and NT4, both 10 years old now
The units may be compiled under Delphi 7 to 2009, but only supports
   Unicode properties under Delphi 2009 where String=UnicodeString
Added DialParamsUID to TEntryRec to index RAS credential in LSA store

Changes in 6.20
Ensure phonebook is read as UTF8, fixed cast warning for Delphi 2009 and later

Changes in 6.31
Clean up removing old Win9x and NT4 code
MagRasOSVersion returns OS7 and OS8 but check >= OSVista

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
    EntryName: string ;
    PBLocation: integer ;  // 5.20 was Flags, but make purpose more obvious
    Phonebook: string ;
    CanonNum: string ;
    DevName1: string ;
    DevType1: string ;
    DevPort1: string ;
    DevName2: string ;
    DevType2: string ;
    DevPort2: string ;
    RpbkLine: integer ;
    DialParamsUID: string ; // 6.00
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
  function MagRasGetEntryList (const PhoneBookPath: string): integer;

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
  function MagRasGetEntryRecs (const PhoneBookPath: string;
                                        const UseAPI: boolean): integer ;

{ Splits a canonical phone number into CountryCode, AreaCode,
  LocalPhoneNumber and UseCountryAndAreaCodes.

  The canonical number forrmat is '+44 (845) 123456' or
  '+country code (area code) local number'. }
  procedure MagRasUnpackCanonical (const PhoneCanonical: string;
      var UseCountryAndAreaCodes: boolean; var CountryCode: integer;
                                var AreaCode, LocalPhoneNumber: string) ;

{ Creates a canonical phone number from CountryCode, AreaCode and
  LocalPhoneNumber, if UseCountryAndAreaCodes is true.

  The canonical number forrmat is '+44 (845) 123456' or
  '+country code (area code) local number'.  A canonical number may be
  missing the + in which case it will not be processed by dialling properties.
  The canonical number is passed to TranslateAddr or TransAddr to translate
  the number in a dialable number with optional calling card, etc

  Returns the canonical number. }
  function MagRasGetCanonical (const UseCountryAndAreaCodes: boolean; const
        CountryCode: DWORD; const AreaCode, LocalPhoneNumber: string): string ;

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
  function MagRasGetEntryKey (const EntryName: string; const Line: integer;
                         const Key: string; const Instance: integer): string ;


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
        CountryCode: DWORD; const AreaCode, LocalPhoneNumber: string): string ;
begin
    if UseCountryAndAreaCodes then
    begin
        result := '+' + IntToStr (CountryCode) + ' ' ;
        if (AreaCode <> '') then result := result + '(' + AreaCode + ') ' ;
        result := result + LocalPhoneNumber ;
    end
    else
        result := LocalPhoneNumber ;
end ;

procedure MagRasUnpackCanonical (const PhoneCanonical: string;
    var UseCountryAndAreaCodes: boolean; var CountryCode: integer;
                                var AreaCode, LocalPhoneNumber: string) ;
var
    temp: string ;
    loc: integer ;
begin
    UseCountryAndAreaCodes := false ;
    LocalPhoneNumber := '' ;
    CountryCode := 0 ;
    AreaCode := '' ;
    temp := trim (PhoneCanonical) ;
    if length (temp) < 2 then exit ;
    if temp [1] = '+' then
    begin
        UseCountryAndAreaCodes := true ;

    // find country code, digits following + sign and strip from temp number
        loc := pos (' ', temp) ;
        if loc > 2 then CountryCode := AscToInt (copy (temp, 2, loc - 2)) ;  // 4.60
        if loc = 0 then loc := 1 ;
        temp := trim (copy (temp, loc + 1, 99)) ;  // remove country code

    // !!! should really lookup country ID from country code table

    // see if area code specified, in brackets, it is optional (ie Hong Kong)
        loc := pos (')', temp) ;
        if loc > 2 then
        begin
            AreaCode := copy (temp, 2, loc - 2) ;
            temp := copy (temp, loc + 2, 99) ;  // remove area code
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
    for I := 0 to Pred (MaxPhoneBkFiles) do
    begin
        if MagRasPhoneFiles [I] <> '' then
        begin
            newage := FileAge (MagRasPhoneFiles [I]) ;
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
    templist := TStringList.Create ;
    try
        for I := 0 to Pred (MaxPhoneBkFiles) do
        begin
            if MagRasPhoneFiles [I] <> '' then
            begin
                if FileExists (MagRasPhoneFiles [I]) then
                begin
                   try
                        templist.Clear ;
// 11 Aug 2008 - Delphi 2009 supports UnicodeString so complete decode, else just ANSI
{$IFDEF UNICODE}
                        templist.LoadFromFile (MagRasPhoneFiles [I], TEncoding.UTF8) ;  // 11 Aug 2010
                        templist.Text := Utf8Decode (RawByteString (templist.Text)) ;   // 11 Aug 2010
{$ELSE}
                        templist.LoadFromFile (MagRasPhoneFiles [I]) ;
                        templist.Text := Utf8ToAnsi (templist.Text) ; // 11 Aug 2008
{$ENDIF}
                        MagRasPhoneAges [I] := FileAge (MagRasPhoneFiles [I]) ;
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

function MagRasGetEntryKey (const EntryName: string; const Line: integer;
                          const Key: string; const Instance: integer): string ;
var
    I, curline, curinst: integer ;
    locentry, lockey: string ;
begin
    result := '' ;
    if MagRasPhoneBookInf.Count = 0 then exit ;
    locentry := '[' + EntryName + ']' ;
    curline := Line ;
    curinst := Instance ;

// check line number in INI file matches entry name
    if (curline >= 0) and (line < MagRasPhoneBookInf.Count) then
    begin
        if MagRasPhoneBookInf [curline] <> locentry then curline := -1 ;
    end ;

// otherwise find start of entry
    if (curline < 0) or (curline >= MagRasPhoneBookInf.Count) then
    begin
        for I := 0 to Pred (MagRasPhoneBookInf.Count) do
        begin
            if MagRasPhoneBookInf [I] = locentry then
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
        result := IntToStr (curline) ;
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
            if Pos (lockey, MagRasPhoneBookInf [I]) = 1 then
            begin
                if curinst = 0 then
                begin
                    result := copy (MagRasPhoneBookInf [I],
                                                succ (length (lockey)), 99) ;
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

function MagRasGetEntryList (const PhoneBookPath: string): integer;
var
    RasEntryNameAll: TRasEntryNameW2KW ;
    I, startoff, endoff, rlen, totents: integer ;
    BufSize, Entries: DWord;
    EntBuf, curEntry: PAnsiChar ;    // 4.90 dynamic instead of fixed memory
    SortList: TStringList ;
    temp: string ;
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
    rlen := SizeOf (TRasEntryNameW2KW) ;
    BufSize := rlen ;   // 4.90 get one entry to start, to find how much memory needed
    GetMem (EntBuf, BufSize) ;     // 4.90 dynamic instead of fixed memory
    SortList := TStringList.Create ;
    try
        FillChar (EntBuf^, BufSize, 0) ;
        PRasEntryNameW2KW (EntBuf).dwSize := rlen ;
        result := RasEnumEntriesW (Nil, PBPtr, PRasEntryNameW2KW (EntBuf),
                                                 BufSize, Entries) ;  // one entry only
        if (result <> 0) and (result <> ERROR_BUFFER_TOO_SMALL) then exit ;
        if (Entries = 0) then exit ;
        ReallocMem (EntBuf, BufSize) ;  // 4.90 allocate memory for all entries
        FillChar (EntBuf^, BufSize, 0) ;
        PRasEntryNameW2KW (EntBuf).dwSize := rlen ;
        result := RasEnumEntriesW (Nil, PBPtr, PRasEntryNameW2KW (EntBuf),
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
                        IntToStr (dwFlags) + #9 + String (szPhonebookPath)) ;
                    inc (totents) ;
                end ;
            end ;
            inc (curEntry, rlen) ;
        end ;
        SortList.Sort ;

    // extract from string list into array
        for I := 0 to Pred (totents) do
        begin
            temp := SortList [I] ;
            with MagRasEntryRecs [I] do
            begin
                endoff := Pos (#9, temp) ;
                EntryName := Copy (temp, 1, pred (endoff)) ;  // keep name
                startoff := succ (endoff) ;
                endoff := Pos (#9, copy (temp, startoff, 999)) ;
                PBLocation := AscToInt (Copy (temp, startoff, pred (endoff))) ; // keep phonebook location 5.20 17 May 2007
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

function MagRasGetEntryRecs (const PhoneBookPath: string;
                                        const UseAPI: boolean): integer ;
var
    EntryInfo: array [0..1200] of DWORD ;
    RasEntry: TRasEntryW ;
    EntrySize, DevSize: DWORD ;
    I, line, res: integer ;
    newentry, temp: string ;
    UseCountryAndAreaCodes: boolean ;
    CountryCode: integer;
    AreaCode, LocalPhoneNumber: string ;
    PBPtr: Pointer ;
    WideName: WideString ;

    function GetCurKey (const key: string; const instance: integer): string ;
    begin
        result := MagRasGetEntryKey (newentry, line, key, instance) ;
    end ;

  // W2K ports in phone book look like ISDN6-0, ISDN2-1, ISDN11-0, VPN2-0, PPPoE7-0, etc

    function TranslatePort (const ntport: string): string ;
    var
        sep, port: integer ;
    begin
        result := Uppercase (ntport) ;
        if length (ntport) < 6 then exit ;
        sep := Pos ('-', ntport) ;
        if sep < 4 then exit ;
        port := Succ (AscToInt (copy (ntport, Succ (sep), 9))) ;  // get port number, add one
        if (copy (ntport, 1, 4) = 'ISDN') then
                                    result := 'ISDN' + IntToStr (port) ;
        if (copy (ntport, 1, 3) = 'VPN') then
                                    result := 'VPN1' + IntToStr (port) ;
        if copy (result, 1, 5) = 'PPPOE' then result := 'PPPoE1' ;   // Windows XP broadband
    end ;

begin
    result := MagRasGetEntryList (PhoneBookPath) ;
    if (Result <> 0) then exit ;
    if (MagRasNumEntryRec = 0) then exit ;

// use RAS API
    if (NOT UseAPI) then
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
                       line := AscToInt (temp) ;
                       RpbkLine := line ;
                    end ;

               // get various keys from entry
                    temp := GetCurKey ('UseDialingRules', 1) ;  // NT4 may not have this key
                    if temp = '' then temp := GetCurKey ('UseCountryAndAreaCodes', 1) ;
                    UseCountryAndAreaCodes := (temp = '1') ;
                    CountryCode := AscToInt (GetCurKey ('CountryCode', 1)) ;
                    AreaCode := GetCurKey ('AreaCode', 1) ;
                    LocalPhoneNumber := GetCurKey ('PhoneNumber', 1) ;
                    CanonNum := MagRasGetCanonical (UseCountryAndAreaCodes,
                                  CountryCode, AreaCode, LocalPhoneNumber) ;
                    DevType1 := lowercase (GetCurKey ('DEVICE', 1)) ;
                    DevName1 := GetCurKey ('Device', 1) ;
                    DevPort1 := TranslatePort (GetCurKey ('Port', 1)) ;
                    DevType2 := lowercase (GetCurKey ('DEVICE', 2)) ;
                    DevName2 := GetCurKey ('Device', 2) ;
                    DevPort2 := TranslatePort (GetCurKey ('Port', 2)) ;
                    DialParamsUID := GetCurKey ('DialParamsUID', 1) ; // 6.00

                // fix bad W2K device types, first device only
                    if DevType1 = 'rastapi' then
                    begin
                        if Pos ('VPN', DevPort1) >= 1 then
                                                    DevType1 := RASDT_Vpn ;
                        if Pos ('LPT', DevPort1) >= 1 then
                                                DevType1 := RASDT_Parallel ;
                        if Pos ('PPPoE', DevPort1) >= 1 then
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
                EntryInfo [0] := Sizeof (TRasEntryW) ;
                PBPtr := Nil ;
             // 5.21 set correct phonebook
                if (PhoneBook <> '') then PBPtr := PWideChar (WideString (PhoneBook)) ;
                WideName := EntryName ;
                res := RasGetEntryPropertiesW (PBPtr, PWideChar (WideName),
                                             @EntryInfo, EntrySize, Nil, DevSize) ;  // 6.0 use vars
                if res = 0 then
                begin
                    Move (EntryInfo, RasEntry, Sizeof (TRasEntryW)) ;
                    with RASEntry do   // get properties in same order as RASENTRY
                    begin
                        AreaCode := szAreaCode ;
                        LocalPhoneNumber := szLocalPhoneNumber ;
                        CanonNum := MagRasGetCanonical (dwfOptions And
                                RASEO_UseCountryAndAreaCodes =
                                  RASEO_UseCountryAndAreaCodes, dwCountryCode,
                                                    AreaCode, LocalPhoneNumber) ;
                        DevType1 := lowercase (szDeviceType) ;
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


