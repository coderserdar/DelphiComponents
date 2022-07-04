unit ICQDb {v 1.19};
{(C) Alex Demchenko(alex@ritlabs.com)}
{$R-}

interface
uses
  Windows, Messages, ICQWorks, SysUtils, Classes, ICQLang;

const
  {Database versions}
  DB_99A = 10;   {99a}
  DB_99B = 14;   {99b}
  DB_2000a = 17; {2000a}
  DB_2000b = 18; {2000b}
  DB_2001a = 19; {2001a, 2001b, 2002a, 2003a}
  DB_MIRANDA121 = $00000700; {Miranda 1.2.1, 1.2.0}

const
  {Miranda-icq signatures}
  DBHEADER_SIGNATURE: array[0..15] of Char = ('M', 'i', 'r', 'a', 'n', 'd', 'a', ' ',   'I', 'C', 'Q', ' ', 'D', 'B', #$00, #$1a);
  DBCONTACT_SIGNATURE: LongWord         = $43DECADE;
  DBMODULENAME_SIGNATURE: LongWord      = $4DDECADE;
  DBCONTACTSETTINGS_SIGNATURE: LongWord = $53DECADE;
  DBEVENT_SIGNATURE: LongWord           = $45DECADE;

  {Miranda-icq data types}
  DBVT_DELETED = 0;    //this setting just got deleted, no other values are valid
  DBVT_BYTE    = 1;    //bVal and cVal are valid
  DBVT_WORD    = 2;    //wVal and sVal are valid
  DBVT_DWORD   = 4;    //dVal and lVal are valid
  DBVT_ASCIIZ  = 255;  //pszVal is valid
  DBVT_BLOB    = 254;  //cpbVal and pbVal are valid
  DBVTF_VARIABLELENGTH = $80;

  {Miranda-icq database flags}
  DBEF_FIRST   = 1;    //this is the first event in the chain;
  DBEF_SENT    = 2;    //this event was sent by the user. If not set this
  DBEF_READ    = 4;    //event has been read by the user. It does not need

  {Miranda-icq event types}
  EVENTTYPE_MESSAGE     = 0;           //Message
  EVENTTYPE_URL         = 1;           //URL
  EVENTTYPE_ADDED       = 1000;        //v0.1.1.0+: these used to be module-
  EVENTTYPE_AUTHREQUEST = 1001;        //specific codes, hence the module-
  EVENTTYPE_FILE        = 1002;        //specific limit has been raised to 2000


type
  TOnErrorEvent = procedure(Sender: TObject; Reason: Word; ReasonStr: String) of object;
  TOnProgress = procedure(Sender: TObject; Progress: Byte) of object;
  TOnContact = procedure(Sender: TObject; UIN: LongWord; NickName, FirstName, LastName,
    Email: String; Age, Gender: Byte; LastUpdate: String; LastUpdateStamp: LongWord) of object;
  TOnSelfInfo = procedure(Sender: TObject; UIN: LongWord; NickName, FirstName, LastName,
    Email, Password: String; Age, Gender: Byte; LastUpdate: String; LastUpdateStamp: LongWord) of object;
  TOnMessage = procedure(Sender: TObject; UIN: LongWord; Incoming: Boolean; Msg, RecvTime: String; RecvTimeStamp: LongWord) of object;
  TOnUrl = procedure(Sender: TObject; UIN: LongWord; Incoming: Boolean; Description, URL, RecvTime: String; RecvTimeStamp: LongWord) of object;
  TOnAdvMessage = procedure(Sender: TObject; UIN: LongWord; Incoming: Boolean; PlainText, RichText, UTF8Text, RecvTime: String; RecvTimeStamp: LongWord) of object;

  {Index record}
  TIdxRec = record
    Code,                                       //If entry is valid the it's set to -2
    Number,                                     //DAT entry number
    Next,                                       //Next IdxRec offset
    Prev,                                       //Previous IdxRec offset
    DatPos: LongInt;                            //Offset in .dat file
  end;

  {Dat header record}
  TDatRec = record
    Length,
    FillType,
    Number: LongInt;
    Command: Byte;
    Signature: array[0..14] of Byte;
  end;

  {Miranda .dat header}
  TMirandaHdr = record
    Signature: array[0..15] of Byte;
    Version: LongWord;
    ofsFileEnd: LongWord;
    slackSpace: LongWord;
    contactCount: LongWord;
    ofsFirstContact: LongWord;
    ofsUser: LongWord;
    ofsFirstModuleName: LongWord;
  end;

  {Miranda's contact entry}
  TMirandaContact = record
    Signature: DWord;
    ofsNext: DWord;
    ofsFirstSettings: DWord;
    eventCount: DWord;
    ofsFirstEvent, ofsLastEvent: DWord;
    ofsFirstUnreadEvent: DWord;
    timestampFirstUnread: DWord;
  end;

  {Miranda's contact settings}
  TDBContactSettings = record
    Signature: LongWord;
    ofsNext: LongWord;
    ofsModuleName: LongWord;
    cbBlob: LongWord
  end;

  {Miranda's event}
  TDBEvent = packed record
    Signature: LongWord;
    ofsPrev: LongWord;
    ofsNext: LongWord;
    ofsModuleName: LongWord;
    Timestamp: LongWord;
    Flags: LongWord;
    eventType: Word;
    cbBlob: LongWord;
  end;

  {Component}
  TICQDb = class(TComponent)
  private
    FIdxFile, FDatFile: String;
    FHandle: THandle;                           //Main .idx file handle
    FDHandle: THandle;                          //Main .dat file handle
    FIdxRoot: LongWord;                         //Root .idx entry
    FIdxEntries: LongWord;                      //Count of idx entries
    FDbVersion: LongWord;                       //Database version extracted from .idx file
    FMirandaHdr: TMirandaHdr;
    {-=-=-=-=-}
    FOnError: TOnErrorEvent;
    FOnParsingStarted: TNotifyEvent;
    FOnParsingFinished: TNotifyEvent;
    FOnProgress: TOnProgress;
    FOnContact: TOnContact;
    FOnSelfInfo: TOnSelfInfo;
    FOnMessage: TOnMessage;
    FOnURL: TOnUrl;
    FOnAdvMessage: TOnAdvMessage;
    FDbType: TDbType;
    FErrLang: TICQLangType;
    function ReadInt(Handle: THandle; Len: ShortInt): LongWord;
    function ReadBuf(Handle: THandle; Len: LongWord; var Buf): LongWord;
    function ReadStr(Handle: THandle; Len: LongWord): String;
    function ReadLNTS(Handle: THandle): String;
    procedure Skip(Handle: THandle; Len: LongWord);
    function Seek(Handle: THandle; Pos: LongWord): Boolean;
    function GetPos(Handle: THandle): LongWord;
    function OpenIdx(const FileName: String): Boolean;
    procedure CloseIdx;
    function OpenDat(const FileName: String): Boolean;
    procedure CloseDat;
    function IsMiranda: Boolean;
    function IsICQ: Boolean;
    function ReadHeader: Boolean;
    function ReadIdxChunk(var IdxRec: TIdxRec): Boolean;
    procedure ParseIndexes;
    procedure ParseDatEntry;
    procedure ParseMirandaDatFile;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure StartParsing;
    property DbType: TDbType read FDbType;
  published
    property DatFile: String read FDatFile write FDatFile;
    property OnError: TOnErrorEvent read FOnError write FOnError;
    property DbVersion: LongWord read FDbVersion;
    property OnParsingStarted: TNotifyEvent read FOnParsingStarted write FOnParsingStarted;
    property OnParsingFinished: TNotifyEvent read FOnParsingFinished write FOnParsingFinished;
    property OnProgress: TOnProgress read FOnProgress write FOnProgress;
    property OnContactFound: TOnContact read FOnContact write FOnContact;
    property OnSelfInfoFound: TOnSelfInfo read FOnSelfInfo write FOnSelfInfo;
    property OnMessageFound: TOnMessage read FOnMessage write FOnMessage;
    property OnURLFound: TOnUrl read FOnUrl write FOnUrl;
    property OnAdvMessageFound: TOnAdvMessage read FOnAdvMessage write FOnAdvMessage;
    property ErrorLanguage: TICQLangType read FErrLang write FErrLang default LANG_EN;
  end;

procedure Register;

implementation
function TimeStamp2Str(Timestamp: LongWord): String;
var
  DelphiTime: Double;
begin
  DelphiTime := EncodeDate(1970, 1, 1) + (TimeStamp / 86400);
  Result := DateTimeToStr(DelphiTime);
end;

constructor TICQDb.Create;
begin
  inherited;
  FHandle := INVALID_HANDLE_VALUE;
  FDHandle := INVALID_HANDLE_VALUE;
end;

destructor TICQDb.Destroy;
begin
  CloseIdx; CloseDat;
  inherited;
end;

function TICQDb.IsMiranda: Boolean;
begin
  FDbType := DB_MIRANDA;
  Result := OpenDat(FDatFile);
  if (Result) and (not ReadHeader) then begin
    Result := False;
    CloseDat;
  end;
end;

function TICQDb.IsICQ: Boolean;
begin
  FDbType := DB_ICQ;
  Result := OpenIdx(FIdxFile) and OpenDat(FDatFile);
  if (Result) and (not ReadHeader) then begin
    Result := False; 
    CloseDat; CloseIdx;
    Exit;
  end;
end;

procedure TICQDb.StartParsing;
begin
  FIdxFile := Copy(FDatFile, 0, Pos('.', FDatFile) - 1) + '.idx';
  if (not IsICQ) and (not IsMiranda) then begin
    if Assigned(OnError) then
      FOnError(Self, IMSG_EDB_EFILEOPEN, ICQLanguages[FErrLang].Translate(IMSG_EDB_EFILEOPEN));
    Exit;
  end;
  if (FDbVersion <> DB_2001a) and (FDbVersion <> DB_2000a) and
     (FDbVersion <> DB_2000b) and (FDbVersion <> DB_MIRANDA121)
  then begin
    CloseDat; CloseIdx;
    if Assigned(OnError) then
      FOnError(Self, IMSG_EDB_EDBVERNOTSUPPORTED, ICQLanguages[FErrLang].Translate(IMSG_EDB_EDBVERNOTSUPPORTED));
     Exit;
  end;
  if FDbType = DB_ICQ then ParseIndexes else ParseMirandaDatFile;
end;

function TICQDb.ReadInt(Handle: THandle; Len: ShortInt): LongWord;
var
  buf: array[0..3] of Byte;
  read: LongWord;
begin
  Result := 0;
  if (Len < 0) or (Len > 4) then
    Exit;
  FillChar(buf, SizeOf(buf), 0);
  ReadFile(Handle, buf, Len, read, nil);
  if read < 1 then Exit;
  Result := PLongWord(@buf)^;
end;

function TICQDb.ReadBuf(Handle: THandle; Len: LongWord; var Buf): LongWord;
begin
  if Len = 0 then Exit;
  ReadFile(Handle, Buf, Len, Result, nil);
end;

function TICQDb.ReadStr(Handle: THandle; Len: LongWord): String;
var
  buf: Pointer;
  read: LongWord;
begin
  Result := '';
  GetMem(buf, Len);
  if Len = 0 then Exit;
  ReadFile(Handle, buf^, Len, read, nil);
  if read < 1 then begin
    FreeMem(buf);
    Exit;
  end;
  Result := Copy(PChar(buf), 0, Len);
  FreeMem(buf);
end;

function TICQDb.ReadLNTS(Handle: THandle): String;
begin
  Result := ReadStr(Handle, ReadInt(Handle, 2));
end;

procedure TICQDb.Skip(Handle: THandle; Len: LongWord);
begin
  SetFilePointer(Handle, SetFilePointer(Handle, 0, nil, 1) + Len, nil, 0)
end;

function TICQDb.Seek(Handle: THandle; Pos: LongWord): Boolean;
begin
  Result := SetFilePointer(Handle, Pos, nil, 0) <> LongWord(-1);
end;

function TICQDb.GetPos(Handle: THandle): LongWord;
begin
  Result := SetFilePointer(Handle, 0, nil, 1);
end;

function TICQDb.OpenIdx(const FileName: String): Boolean;
begin
  Result := False;
  CloseIdx;
  FHandle := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  if FHandle = INVALID_HANDLE_VALUE then Exit;
  if SetFilePointer(FHandle, 0, nil, 0) = LongWord(-1) then begin
    CloseIdx;
    Exit;
  end;
  Result := True;
end;

procedure TICQDb.CloseIdx;
begin
  if FHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FHandle);
  FHandle := INVALID_HANDLE_VALUE;
end;

function TICQDb.OpenDat(const FileName: String): Boolean;
begin
  Result := False;
  CloseDat;
  FDHandle := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
  if FDHandle = INVALID_HANDLE_VALUE then Exit;
  if SetFilePointer(FDHandle, 0, nil, 0) = LongWord(-1) then begin
    CloseDat;
    Exit;
  end;
  Result := True;
end;

procedure TICQDb.CloseDat;
begin
  if FDHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FDHandle);
  FDHandle := INVALID_HANDLE_VALUE;
end;

function TICQDb.ReadHeader: Boolean;
var
  Size: LongWord;
begin
  Result := False;
  if DbType = DB_ICQ then begin
    Size := FileSizeFromStr(FIdxFile);
    if Size <> INVALID_FILE_SIZE then
      FIdxEntries := (Size - 20) div (SizeOf(TIdxRec) shl 4)
    else
      Exit;
    if FHandle = INVALID_HANDLE_VALUE then Exit;
    if (ReadInt(FHandle, 4) <> 4) or (ReadInt(FHandle, 4) <> 20) or
       (ReadInt(FHandle, 4) <> 8) then
      Exit;
    FIdxRoot := ReadInt(FHandle, 4);
    FDbVersion := ReadInt(FHandle, 4);
  end else begin
    Size := FileSizeFromStr(FDatFile);
    if Size = INVALID_FILE_SIZE then Exit;
    if ReadBuf(FDHandle, SizeOf(TMirandaHdr), FMirandaHdr) <> SizeOf(TMirandaHdr) then Exit;
    FDbVersion := FMirandaHdr.Version;
    if not CompareMem(@FMirandaHdr.Signature, @DBHEADER_SIGNATURE, 16) then
      Exit;
  end;
  Result := True;
end;

function TICQDb.ReadIdxChunk(var IdxRec: TIdxRec): Boolean;
begin
  Result := False;
  if FHandle = INVALID_HANDLE_VALUE then Exit;
  if IdxRec.Next = -1 then Exit;
  if SetFilePointer(FHandle, IdxRec.Next, nil, 0) = LongWord(-1) then
    Exit;
  if FHandle = INVALID_HANDLE_VALUE then Exit;
  if ReadBuf(FHandle, SizeOf(TIdxRec), IdxRec) <> SizeOf(TIdxRec) then
    Exit;
  Result := True;
end;

procedure TICQDb.ParseIndexes;
var
  idx: TIdxRec;
  i: LongWord;
begin
  if Assigned(OnParsingStarted) then
    FOnParsingStarted(Self);
  idx.Next := FIdxRoot;
  i := 0;
  while ReadIdxChunk(idx) do begin
    if idx.Code = -2 then
    begin
      if idx.DatPos <> -1 then                    {if it's not a root entry}
        if not Seek(FDhandle, idx.DatPos) then
          Break
        else
          ParseDatEntry;
    end;
    Inc(i);
    if (Assigned(OnProgress)) and (FIdxEntries > 0) then
      FOnProgress(Self, Round((i / FIdxEntries) * 100));
  end;
  CloseIdx; CloseDat;
  if Assigned(OnProgress) then
    FOnProgress(Self, 100);
  if Assigned(OnParsingFinished) then
    FOnParsingFinished(Self);
end;

procedure TICQDb.ParseDatEntry;
function Read64h: Char;
begin
  Result := Chr(ReadInt(FDHandle, 1));
end;

function Read65h: Byte;
begin
  Result := ReadInt(FDHandle, 1);
end;

function Read66h: Word;
begin
  Result := ReadInt(FDHandle, 2);
end;

function Read67h: Integer;
begin
  Result := ReadInt(FDHandle, 2);
end;

function Read68h: LongWord;
begin
  Result := ReadInt(FDHandle, 4);
end;

function Read69h: LongInt;
begin
  Result := ReadInt(FDHandle, 4);
end;

function Read6bh: String;
begin
  Result := ReadStr(FDHandle, ReadInt(FDHandle, 2));
end;

{Global variables in ParseDatEntry procedure}
var
  FNickName: String;
  FFirstName: String;
  FLastName: String;
  FEmail: String;
  FLastUpdate: String;
  FAge, FGender: Byte;
  FUIN: LongWord;
  FMsg, FMsg2, FMsg3: String;
  FFlag: LongWord;
  FSeparator: Word;
  FSubType: Word;
  FTStamp: LongWord;

  FPassword: String;
  FCryptIV: LongWord;

procedure ReadProperty;
var
  Len: Word;
  AName: String;
  Num, PropNum, i, n: LongWord;
  CType: Byte;
  Cmd: Byte;
begin
  Len := ReadInt(FDHandle, 2);
  AName := ReadStr(FDHandle, Len);
  Cmd := ReadInt(FDHandle, 1);
  case Cmd of
    $64: {Char}
      Read64h;
    $65: {Byte}
      if AName = 'Age' then
        FAge := Read65h
      else if AName = 'Gender' then
        FGender := Read65h
      else
        Read65h;
    $66: {Word}
      Read66h;
    $67: {Integer}
      Read67h;
    $68: {DWord}
      if AName = '99BCryptIV' then
        FCryptIV := Read68h
      else
        Read68h;
    $69: {LongInt}
      if AName = 'UIN' then
        FUIN := Read69h
      else
        Read69h;
    $6b: {LNTS}
      if AName = 'NickName' then
        FNickName := Read6bh
      else if AName = 'FirstName' then
        FFirstName := Read6bh
      else if AName = 'LastName' then
        FLastName := Read6bh
      else if AName = 'PrimaryEmail' then
        FEmail := Read6bh
      else if AName = 'Password' then begin
        if FPassword = '' then                  //For some unknown reasons, password is stored many times with null value
          FPassword := Read6bh
        else
          Read6bh
      end else
        Read6bh;
    $6d: {Sublist}
    begin
      Num := ReadInt(FDHandle, 4);
      CType := ReadInt(FDHandle, 1);
      if Num > 0 then
        for i := 0 to Num - 1 do
          case CType of
            $6b:
              Skip(FDHandle, ReadInt(FDHandle, 2));
            $6e:
            begin
              Skip(FDHandle, 2);                //Separator value
              PropNum := ReadInt(FDHandle, 4);  //Number of properties
              if PropNum > 0 then
                for n := 0 to PropNum - 1 do
                  ReadProperty;                 //Parse each property (call recursively)
            end;
          end;
    end;
    $6f: {DWORD (length) + BYTE array}
      Skip(FDHandle, ReadInt(FDHandle, 4));
  end;
end;

procedure ReadPropertyBlock;
var
  Num, i: LongWord;
begin
  Skip(FDHandle, 2);                            //Separator value
  Num := ReadInt(FDHandle, 4);                  //Number of user properties
  if Num > 0 then
    for i := 0 to Num - 1 do
      ReadProperty;
end;

procedure ReadWavEntry;
begin
  Skip(FDHandle, 2);                            //Separator value
  Skip(FDHandle, 4);                            //User event for which Wav will be played
  Skip(FDHandle, 4);                            //0: play default WAV, 1: play the user-specified WAV
  ReadLNTS(FDHandle);                           //Full path and file name of WAV
end;

procedure ReadWavBlock;
var
  Num, i: LongWord;
begin
  Num := ReadInt(FDHandle, 4);            //Number of user event WAV entries
  if Num > 0 then
    for i := 0 to Num - 1 do
      ReadWavEntry;
  Skip(FDHandle, 2);                      //Separator value
end;

var
  Dat: TDatRec;
  Num: LongWord;
  i: LongWord;
  FURL, FDesc: String;
begin
  if FDHandle = INVALID_HANDLE_VALUE then Exit;
  if ReadBuf(FDHandle, SizeOf(Dat), Dat) <> SizeOf(Dat) then Exit;
  case Dat.Command of
    $e0, $a0: {Short Message & URL Format (ICQ 99a-2002a)}
    begin
      Skip(FDHandle, 2);                        //Separator
      Skip(FDHandle, 4);                        //Filing flags
      FSubType := ReadInt(FDHandle, 2);         //Entry sub type: 1: Message; 4: URL; 19: Contacts
      if (FSubType <> 1) and (FSubType <> 4) then
        Exit;
      FUIN := ReadInt(FDHandle, 4);             //UIN of sender/receiver
      FMsg := ReadStr(FDHandle, ReadInt(FDHandle, 2));
      Skip(FDHandle, 4);                        //Status of receiving user
      FFlag := ReadInt(FDHandle, 4);            //Sent or received: 0: Received, 1: Sent
      Skip(FDHandle, 2);                        //Separator value
      FTStamp := ReadInt(FDHandle, 4);          //Timestamp, time of last update
      FLastUpdate := TimeStamp2Str(FTStamp);
      if FSubType = 1 then begin
        if Assigned(OnMessageFound) then
          FOnMessage(Self, FUIN, FFlag = 0, FMsg, FLastUpdate, FTStamp);
      end else
      if FSubType = 4 then begin
        FDesc := Copy(FMsg, 0, Pos(#$fe, FMsg) - 1);
        FURL := Copy(FMsg, Pos(#$fe, FMsg) + 1, Length(FMsg) - Pos(#$fe, FMsg));
        if Assigned(OnUrlFound) then
          FOnUrl(Self, FUIN, FFlag = 0, FDesc, FURL, FLastUpdate, FTStamp);
      end;
    end;
    $e4: {My details}
    begin
      if Dat.Number <> 1005 then Exit;
      FNickName := ''; FFirstName := ''; FLastName := '';  FEmail := '';
      FPassword := ''; FAge := 0; FGender := 0; FUIN := 0;
      FSeparator := ReadInt(FDHandle, 2);       //Separator
      if ReadStr(FDHandle, 4) <> 'RESU' then    //Label   = 55534552h ('USER')
        Exit;
      if ReadInt(FDHandle, 4) <> 6 then Exit;   //User entry status: 6 = "My Details"
      Skip(FDHandle, 4);                        //0 (Unknown, most likely an unused group entry)
      Skip(FDHandle, 2);                        //Separator value
      {Some modifications in ICQ2000x}
      if (FDbVersion = DB_2000a) or (FDbVersion = DB_2000b) then
        ReadWavBlock;
      {Some modifications in ICQ2002a}
      if (FSeparator >= 533) and (FDbVersion = DB_2001a) then begin
        Skip(FDHandle, 4);                      //0 (Unknown, if this can be longer than a long it will most likely crash the importer
        Skip(FDHandle, 2);                      //Separator value
      end;
      Num := ReadInt(FDHandle, 4);              //Number of property blocks
      if Num > 0 then
        for i := 0 to Num - 1 do
          ReadPropertyBlock;
      Skip(FDHandle, 2);                        //Separator value
      FTStamp := ReadInt(FDHandle, 4);          //Timestamp, time of last update
      FLastUpdate := TimeStamp2Str(FTStamp);
      FPassword := Decrypt99bPassword(FUIN, FCryptIV, FPassword);
      if Assigned(OnSelfInfoFound) then
        FOnSelfInfo(Self, FUIN, FNickName, FFirstName, FLastName, FEmail, FPassword, FAge, FGender, FLastUpdate, FTStamp);
    end;
    $e5: {Contact entry}
    begin
      FNickName := ''; FFirstName := ''; FLastName := '';  FEmail := '';
      FAge := 0; FGender := 0; FUIN := 0;
      FSeparator := ReadInt(FDHandle, 2);       //Separator
      if ReadStr(FDHandle, 4) <> 'RESU' then    //Label   = 55534552h ('USER')
        Exit;
      ReadInt(FDHandle, 4);                     //User entry status
      ReadInt(FDHandle, 4);                     //GroupID of contact group containing user
      Skip(FDHandle, 2);                        //Separator value
      {Some modifications in ICQ2000x}
      if (FDbVersion = DB_2000a) or (FDbVersion = DB_2000b) then
        ReadWavBlock;
      {Some modifications in ICQ2002a}
      if (FSeparator >= 533) and (FDbVersion = DB_2001a) then begin
        Skip(FDHandle, 4);                      //Unknown, 0
        Skip(FDHandle, 2);                      //Separator value
      end;
      Num := ReadInt(FDHandle, 4);              //Number of property blocks
      if Num > 0 then
        for i := 0 to Num - 1 do
          ReadPropertyBlock;
      Skip(FDHandle, 2);                        //Separator value
      FTStamp := ReadInt(FDHandle, 4);          //Timestamp, time of last update
      FLastUpdate := TimeStamp2Str(FTStamp);
      if Assigned(OnContactFound) then
        FOnContact(Self, FUIN, FNickName, FFirstName, FLastName, FEmail, FAge, FGender, FLastUpdate, FTStamp);
    end;
    $50: {Long Message Format (ICQ 99a-2002a)}
    begin
      Skip(FDHandle, 2);                        //Separator
      Skip(FDHandle, 4);                        //Filing flags
      Skip(FDHandle, 2);                        //Entry sub type
      FUIN := ReadInt(FDHandle, 4);             //UIN of sender/receiver
      FMsg := ReadLNTS(FDHandle);               //ANSI text
      Skip(FDHandle, 4);                        //Status of receiving user
      FFlag := ReadInt(FDHandle, 4);            //Sent or received: 0: Received, 1: Sent
      Skip(FDHandle, 2);                        //Separator value
      FTStamp := ReadInt(FDHandle, 4);          //Timestamp, time of last update
      FLastUpdate := TimeStamp2Str(FTStamp);
      Skip(FDHandle, 19);                       //Zeroes
      FMsg2 := ReadLNTS(FDHandle);              //Rich Text
      FMsg3 := ReadLNTS(FDHandle);              //UTF-8 Text
      if Assigned(OnAdvMessageFound) then
        FOnAdvMessage(Self, FUIN, FFlag = 0, FMsg, FMsg2, FMsg3, FLastUpdate, FTStamp);
    end;
  end;
end;

procedure TICQDb.ParseMirandaDatFile;
{Global variables in ParseMirandaDatFile procedure}
var
  FNickName: String;
  FFirstName: String;
  FLastName: String;
  FEmail: String;
  FLastUpdate: String;
  FAge, FGender: Byte;
  FUIN: LongWord;
  FMsg: String;
  FPassword: String;


function GetModuleName(Ofs: LongWord): String;
type
  TDBModuleName = record
    Signature: LongWord;
    ofsNext: LongWord;
    cbName: Byte;
  end;
var
  FMod: TDbModuleName;
  FCurrOff: LongWord;
begin
  Result := '';
  FCurrOff := GetPos(FDHandle);
  if not Seek(FDHandle, Ofs) then Exit;
  if ReadBuf(FDHandle, SizeOf(FMod), FMod) <> SizeOf(FMod) then Exit;
  Result := ReadStr(FDHandle, FMod.cbName);
  Seek(FDHandle, FCurrOff);
end;

function ReadContactSettings(Ofs: LongWord): Boolean;
function ReadByte: Byte;
begin
  Result := ReadInt(FDHandle, 1);
end;

function ReadWord: Word;
begin
  Result := ReadInt(FDHandle, 2);
end;

function ReadDWord: LongWord;
begin
  Result := ReadInt(FDHandle, 4);
end;

function ReadASCIIZ: String;
begin
  Result := ReadStr(FDHandle, ReadWord);
end;

procedure ReadParams(Len: LongWord);
var
  FName: String;
  __pos: LongWord;
begin
  __pos := GetPos(FDHandle);
  while True do begin
    FName := ReadStr(FDHandle, ReadByte);
    if FName = '' then Break;                   //We acheived end of property list
    case ReadByte of
      DBVT_DELETED: Exit;                       //This setting just got deleted, no other values are valid
      DBVT_BYTE:
        if FName = 'Gender' then begin
          FGender := ReadByte;
          if Chr(FGender) = 'M' then
            FGender := GEN_MALE
          else if Chr(FGender) = 'F' then
            FGender := GEN_FEMALE
          else
            FGender := 0;
        end else
          ReadByte;
      DBVT_WORD:
        if FName = 'age' then
          FAge := ReadWord
        else
          ReadWord;
      DBVT_DWORD:
        if FName = 'UIN' then
          FUIN := ReadDWord
        else
          ReadDWord;
      DBVT_ASCIIZ:
        if FName = 'Nick' then
          FNickName :=  ReadASCIIZ
        else if FName = 'FirstName' then
          FFirstName := ReadASCIIZ
        else if FName = 'LastName' then
          FLastName := ReadASCIIZ
        else if FName = 'e-mail' then
          FEmail := ReadASCIIZ
        else if FName = 'Password' then
          FPassword := DecryptMirandaPassword(ReadASCIIZ)
        else
          ReadASCIIZ;
      DBVT_BLOB:
        Skip(FDHandle, ReadDWord);
      DBVTF_VARIABLELENGTH:
        Exit;
    else
      Exit;
    end;
    if GetPos(FDHandle) >= __pos + Len then Break;
  end;
end;
var
  FDbset: TDBContactSettings;
  FModName: String;
begin
  FNickName := ''; FFirstName := ''; FLastName := '';
  FEmail := ''; FLastUpdate := ''; FAge := 0;
  FGender := 0; FUIN := 0; FMsg := ''; Result := False;
  if not Seek(FDHandle, Ofs) then Exit;
  while True do begin
    if ReadBuf(FDHandle, SizeOf(FDbSet), FDbSet) <> SizeOf(FDbSet) then Break;
    FModName := GetModuleName(FDbSet.ofsModuleName);
    //if FModName = '' then                       //Do not parse any module settings
      ReadParams(FDbSet.cbBlob);                //Parse contact params
    if FDbSet.ofsNext = 0 then Break;
    if not Seek(FDHandle, FDbSet.ofsNext) then Break;
  end;
  Result := True;
end;

procedure ReadEvents(Ofs: LongWord);
var
  FDbEvent: TDbEvent;
  FDesc: String;
  FURL: String;
begin
  if not Seek(FDHandle, Ofs) then Exit;
  while True do begin
    if ReadBuf(FDHandle, SizeOf(TDbEvent), FDbEvent) <> SizeOf(TDbEvent) then Break;
    if FDbEvent.Signature <> DBEVENT_SIGNATURE then Break;
    //if GetModuleName(FDbEvent.ofsModuleName) = '' then  //Parse only miranda's events
      if (FDbEvent.eventType = EVENTTYPE_MESSAGE) or
         (FDbEvent.eventType = EVENTTYPE_URL) then
      begin
        FMsg := ReadStr(FDHandle, FDbEvent.cbBlob);
        if FDbEvent.eventType = EVENTTYPE_MESSAGE then begin
          if Assigned(OnMessageFound) then
            FOnMessage(Self, FUIN, FDbEvent.flags and DBEF_SENT <> DBEF_SENT, FMsg, TimeStamp2Str(FDbEvent.Timestamp), FDbEvent.Timestamp);
        end else begin
          FDesc := Copy(FMsg, 0, Pos(#$fe, FMsg) - 1);
          FURL := Copy(FMsg, Pos(#$fe, FMsg) + 1, Length(FMsg) - Pos(#$fe, FMsg));
          if Assigned(OnUrlFound) then
            FOnUrl(Self, FUIN, FDbEvent.flags and DBEF_SENT <> DBEF_SENT, FDesc, FURL, TimeStamp2Str(FDbEvent.Timestamp), FDbEvent.Timestamp);
        end;
      end;
    if FDbEvent.ofsNext = 0 then Break;
    if not Seek(FDHandle, FDbEvent.ofsNext) then Break;
  end;
end;

var
  FContact: TMirandaContact;
  i: Word;
begin
  if Assigned(OnParsingStarted) then
    FOnParsingStarted(Self);
  if Assigned(OnProgress) then
    FOnProgress(Self, 0);
  i := 1;
  if not Seek(FDHandle, FMirandaHdr.ofsFirstContact) then Exit;
  if FMirandaHdr.contactCount > 0 then
    while True do begin
      if ReadBuf(FDHandle, SizeOf(FContact), FContact) <> SizeOf(FContact) then Break;
      if ReadContactSettings(FContact.ofsFirstSettings) then
        if Assigned(OnContactFound) then                  //It's called here because of same property reader for the self info
          FOnContact(Self, FUIN, FNickName, FFirstName, FLastName, FEmail, FAge, FGender, '', 0);
      ReadEvents(FContact.ofsFirstEvent);
      if FContact.ofsNext = 0 then Break;
      if not Seek(FDhandle, FContact.ofsNext) then Break;
      if Assigned(OnProgress) then
        FOnProgress(Self, Round((i / FMirandaHdr.contactCount) * 100));
      Inc(i);
    end;
  if (FMirandaHdr.ofsUser = 0) or (not Seek(FDHandle, FMirandaHdr.ofsUser)) then Exit;
  if ReadBuf(FDHandle, SizeOf(FContact), FContact) <> SizeOf(FContact) then Exit;
  FPassword := '';
  if ReadContactSettings(FContact.ofsFirstSettings) then
    if Assigned(OnSelfInfoFound) then
      FOnSelfInfo(Self, FUIN, FNickName, FFirstName, FLastName, FEmail, FPassword, FAge, FGender, '', 0);
  CloseDat;     
  if Assigned(OnProgress) then
    FOnProgress(Self, 100);
  if Assigned(OnParsingFinished) then
    FOnParsingFinished(Self);
end;

procedure Register;
begin
  RegisterComponents('Standard', [TICQDb]);
end;

end.
