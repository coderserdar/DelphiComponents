// ----------------------------------------
//
//  (C) Alex Demchenko (coban2k@mail.ru)
//          http://www.cobans.net
//
// ----------------------------------------
{$IFNDEF VER140}
  {$WARN UNSAFE_TYPE off}
  {$WARN UNSAFE_CAST off}
  {$WARN UNSAFE_CODE off}
{$ENDIF}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{

DELPHI RAS COMPONENT - LSA Usernames and Passwords

Updated by Angus Robertson, Magenta Systems Ltd, England
in 2011, delphi@magsys.co.uk, http://www.magsys.co.uk/delphi/
Copyright Magenta Systems Ltd

The Local Security Authority (LSA) is a protected subsystem of Windows that maintains
information about all aspects of local security on a system, collectively known as
the local security policy of the system.   The information is saved in the registry
under the key:

HKEY_LOCAL_MACHINE\SECURITY\Policy\Secrets\

to which the administrator account is normally denied access.  Various applications
write password information under subkeys, the RAS credentials for entries are at:

HKEY_LOCAL_MACHINE\SECURITY\Policy\Secrets\L$_RasDefaultCredentials#0\CurrVal

while those for a specific user include the local SID for the user:

HKEY_LOCAL_MACHINE\SECURITY\Policy\Secrets\RasDialParams!S-1-5-21-1993962763-527237240-725345543-1003#0\CurrVal

All the data is unicode and encrypted, the LSA functions allow it to be read
and written.

This unit is based on the work of Alex Demchenko, but has been improved to
allow return all the RAS credential, not just the password.

Currently, this unit only reads the RAS credentials.  To write them, it would be
necessary to build the entire list as multiple null terminated strings, and write
it back with SetLsaData. You can not easily update just one logon.

Changes in 6.30
Updated for 64-bit compiler

}

unit MagLsaPw;

interface
uses
  Windows, Messages, SysUtils,
  MagRasApiW, MagSubs1;

const
  MagVersion = 'TLSARasPw, 6th June 2017 - Release 6.40, Copyright 2017, Magenta Systems Ltd' ;

type
  TLsaRasCred = record
    DialParamsUID: string ;
    Unknown2: string ;
    Unknown3: string ;
    Unknown4: string ;
    Unknown5: string ;
    UserName: string ;
    Password: string ;
    Unknown8: string ;
    Unknown9: string ;
    PBLocation: integer ;  // REN_AllUsers or REN_Users
  end ;

const
  POLICY_VIEW_LOCAL_INFORMATION = 1;
  POLICY_VIEW_AUDIT_INFORMATION = 2;
  POLICY_GET_PRIVATE_INFORMATION = 4;
  POLICY_TRUST_ADMIN = 8;
  POLICY_CREATE_ACCOUNT = 16;
  POLICY_CREATE_SECRET = 32;
  POLICY_CREATE_PRIVILEGE = 64;
  POLICY_SET_DEFAULT_QUOTA_LIMITS = 128;
  POLICY_SET_AUDIT_REQUIREMENTS = 256;
  POLICY_AUDIT_LOG_ADMIN = 512;
  POLICY_SERVER_ADMIN = 1024;
  POLICY_LOOKUP_NAMES = 2048;
  POLICY_NOTIFICATION = 4096;
  POLICY_ALL_ACCESS = (STANDARD_RIGHTS_REQUIRED or POLICY_VIEW_LOCAL_INFORMATION or POLICY_VIEW_AUDIT_INFORMATION or POLICY_GET_PRIVATE_INFORMATION or POLICY_TRUST_ADMIN or POLICY_CREATE_ACCOUNT or POLICY_CREATE_SECRET or POLICY_CREATE_PRIVILEGE or POLICY_SET_DEFAULT_QUOTA_LIMITS or POLICY_SET_AUDIT_REQUIREMENTS or POLICY_AUDIT_LOG_ADMIN or POLICY_SERVER_ADMIN or POLICY_LOOKUP_NAMES);
  POLICY_READ = (STANDARD_RIGHTS_READ or POLICY_VIEW_AUDIT_INFORMATION or POLICY_GET_PRIVATE_INFORMATION);
  POLICY_WRITE = (STANDARD_RIGHTS_WRITE or POLICY_TRUST_ADMIN or POLICY_CREATE_ACCOUNT or POLICY_CREATE_SECRET or POLICY_CREATE_PRIVILEGE or POLICY_SET_DEFAULT_QUOTA_LIMITS or POLICY_SET_AUDIT_REQUIREMENTS or POLICY_AUDIT_LOG_ADMIN or POLICY_SERVER_ADMIN);
  POLICY_EXECUTE = (STANDARD_RIGHTS_EXECUTE or POLICY_VIEW_LOCAL_INFORMATION or POLICY_LOOKUP_NAMES);

type
  PLSA_UNICODE_STRING = ^LSA_UNICODE_STRING;
  LSA_UNICODE_STRING = record
    Length: WORD;
    MaximumLength: WORD;
    Buffer: PWCHAR;
  end;

  PLSA_OBJECT_ATTRIBUTES = ^LSA_OBJECT_ATTRIBUTES;
  LSA_OBJECT_ATTRIBUTES = record
    Length: LongWord;
    RootDirectory: THandle;
    ObjectName: PLSA_UNICODE_STRING;
    Attributes: LongWord;
    SecurityDescriptor: Pointer;
    SecurityQualityOfService: Pointer;
  end;

  TConvertSidToStringSid = function(sid: Pointer; var StringSid: PAnsiChar): BOOL; stdcall;
  TLsaOpenPolicy = function(SystemName: PLSA_UNICODE_STRING; ObjectAttributes: PLSA_OBJECT_ATTRIBUTES; DesiredAccess: LongWord; PolicyHandle: PLongWord): LongWord; stdcall;
  TLsaRetrievePrivateData = function(LSA_HANDLE: LongWord; KeyName: PLSA_UNICODE_STRING; PrivateData: PLSA_UNICODE_STRING): LongWord; stdcall;
  TLsaStorePrivateData = function(LSA_HANDLE: LongWord; KeyName: PLSA_UNICODE_STRING; var PrivateData: LSA_UNICODE_STRING): LongWord; stdcall;
  TLsaClose = function(ObjectHandle: LongWord): LongWord; stdcall;
  TLsaFreeMemory = function(Buffer: Pointer): LongWord; stdcall;
  TLsaNtStatusToWinError = function(NTSTATUS: integer): LongWord; stdcall;

  TLSALocal = class(TObject)
  private
    hLibrary: THandle;
    ConvertSidToStringSid: TConvertSidToStringSid;
    LsaOpenPolicy: TLsaOpenPolicy;
    LsaRetrievePrivateData: TLsaRetrievePrivateData;
    LsaClose: TLsaClose;
    LsaFreeMemory: TLsaFreeMemory;
    LsaStorePrivateData: TLsaStorePrivateData;
    function LoadLSAFunctions: Boolean;
    function UnLoadLSAFunctions: Boolean;
    function GetInitialized: Boolean;
  public
    LsaNtStatus: integer ;
    LsaNtStatusToWinError:  TLsaNtStatusToWinError ;
    constructor Create;
    destructor Destroy; override;
    procedure AnsiStringToLsaStr(const AValue: String; var LStr: LSA_UNICODE_STRING);
    function GetLocalSid: String;
    function GetLsaData(Policy: LongWord; const KeyName: String; var OutData: PLSA_UNICODE_STRING): Boolean;
    function SetLsaData(Policy: LongWord; const KeyName: String; var InData: LSA_UNICODE_STRING): Boolean;
    function LsaFree(Buffer: Pointer): LongWord;
    property Initialized: Boolean read GetInitialized;
  end;

  TLSARasPw = class(TObject)
  private
  public
    LsaRasCreds: array of TLsaRasCred ;
    constructor Create;
    destructor Destroy; override;
    procedure ProcessLSABuffer(Buffer: PWideChar; BufLen, Location: Integer);
    procedure GetLSAPasswords;
    function FindDialParamsUID (const Id: string): integer ;
  end;

implementation

{ TLSALocal }

constructor TLSALocal.Create;
begin
  LoadLSAFunctions;
end;

destructor TLSALocal.Destroy;
begin
  UnLoadLSAFunctions;
  inherited;
end;

function TLSALocal.LoadLSAFunctions;
begin
  hLibrary := LoadLibrary('advapi32.dll');
  if hLibrary <> 0 then begin
    ConvertSidToStringSid := GetProcAddress(hLibrary, 'ConvertSidToStringSidA');
    LsaOpenPolicy := GetProcAddress(hLibrary, 'LsaOpenPolicy');
    LsaRetrievePrivateData := GetProcAddress(hLibrary, 'LsaRetrievePrivateData');
    LsaClose := GetProcAddress(hLibrary, 'LsaClose');
    LsaFreeMemory := GetProcAddress(hLibrary, 'LsaFreeMemory');
    LsaStorePrivateData := GetProcAddress(hLibrary, 'LsaStorePrivateData');
    LsaNtStatusToWinError := GetProcAddress(hLibrary, 'LsaNtStatusToWinError');

    Result := (@ConvertSidToStringSid <> nil) and (@LsaOpenPolicy <> nil) and (@LsaRetrievePrivateData <> nil) and
              (@LsaClose <> nil) and (@LsaFreeMemory <> nil) and (@LsaStorePrivateData <> nil);

    if not Result then
      UnLoadLSAFunctions;
  end else
    Result := False;
end;

function TLSALocal.UnLoadLSAFunctions;
begin
  if hLibrary <> 0 then begin
    FreeLibrary(hLibrary);
    hLibrary := 0;
  end;
  Result := True;
end;

function TLSALocal.GetInitialized: Boolean;
begin
  Result := hLibrary <> 0;
end;

procedure TLSALocal.AnsiStringToLsaStr(const AValue: String; var LStr: LSA_UNICODE_STRING);
begin
  LStr.Length := Length(AValue) shl 1;
  LStr.MaximumLength := LStr.Length+2;
  GetMem(LStr.Buffer, LStr.MaximumLength);
  StringToWideChar(AValue, LStr.Buffer, LStr.MaximumLength);
end;

function TLSALocal.GetLocalSid: String;
var
  UserName: String;
  UserNameSize, SidSize, DomainSize: Cardinal;
  sid, domain: array[0..255] of AnsiChar;
  snu: SID_NAME_USE;
  pSid: PAnsiChar;
begin
  Result := '';

  { Local User Name }
  SetLength(UserName, 256);
  UserNameSize := 255;
  if not GetUserNameA(@UserName[1], UserNameSize) then Exit;

  { Find a security identificator (sid) for local user }
  SidSize := 255;
  DomainSize := 255;
  if not LookupAccountNameA(nil, @UserName[1], @sid, SidSize, @domain, DomainSize, snu) then Exit;
  if not IsValidSid(@sid) then Exit;

  { Convert sid to string }
  ConvertSidToStringSid(@sid, pSid);
  Result := String (AnsiString(pSid)); // 11 Aug 2010
  GlobalFree(Cardinal(pSid));
end;

function TLSALocal.GetLsaData(Policy: LongWord; const KeyName: String; var OutData: PLSA_UNICODE_STRING): Boolean;
var
  LsaObjectAttribs: LSA_OBJECT_ATTRIBUTES;
  LsaHandle: LongWord;
  LsaKeyName: LSA_UNICODE_STRING;
begin
  Result := False;
  FillChar(LsaObjectAttribs, SizeOf(LsaObjectAttribs), 0);
  LsaNtStatus := LsaOpenPolicy(nil, @LsaObjectAttribs, Policy, @LsaHandle) ;
  if LsaNtStatus > 0 then Exit;
  AnsiStringToLsaStr(KeyName, LsaKeyName);
  LsaNtStatus := LsaRetrievePrivateData(LsaHandle, @LsaKeyName, @OutData) ;
  Result := (LsaNtStatus = 0) ;
  FreeMem(LsaKeyName.Buffer);
  LsaClose(LsaHandle);
end;

function TLSALocal.SetLsaData(Policy: LongWord; const KeyName: String; var InData: LSA_UNICODE_STRING): Boolean;
var
  LsaObjectAttribs: LSA_OBJECT_ATTRIBUTES;
  LsaHandle: LongWord;
  LsaKeyName: LSA_UNICODE_STRING;
begin
  Result := False;
  FillChar(LsaObjectAttribs, SizeOf(LsaObjectAttribs), 0);
  if LsaOpenPolicy(nil, @LsaObjectAttribs, Policy, @LsaHandle) > 0 then
    Exit;
  AnsiStringToLsaStr(KeyName, LsaKeyName);
  Result := LsaStorePrivateData(LsaHandle, @LsaKeyName, InData) = 0;
  FreeMem(LsaKeyName.Buffer);
  LsaClose(LsaHandle);
end;

function TLSALocal.LsaFree(Buffer: Pointer): LongWord;
begin
  Result := LsaFreeMemory(Buffer);
end;

constructor TLSARasPw.Create;
begin
  SetLength (LsaRasCreds, 0) ;
  GetLSAPasswords;
end;

destructor TLSARasPw.Destroy;
begin
  SetLength (LsaRasCreds, 0) ;
  inherited;
end;

procedure TLSARasPw.ProcessLSABuffer(Buffer: PWideChar; BufLen, Location: Integer);
var
    I, J, totstr, totent, curent, curfield: integer ;
    P: PWideChar ;
begin
    totstr := 0 ;
    if BufLen = 0 then exit ; // length is bytes in buffer, not characters
    P := Buffer;
    for I := 1 to (BufLen div 2) do
    begin
        if P^ = #0 then inc (totstr) ;  // count strings in buffer
        inc (P) ;
    end ;
    totent := totstr div 9 ;
    if totent = 0 then exit ;
    curent := Length (LsaRasCreds) ;
    SetLength (LsaRasCreds, curent + totent) ;
    P := Buffer ;
    I := 1 ;
    curfield := 1 ;
    while (I < (BufLen div 2)) do  // 28 Aug 2008 allow for empty strings, except last null
    begin
        with LsaRasCreds [curent] do
        begin
            case curfield of
                1: DialParamsUID := P;
                2: Unknown2 := p;
                3: Unknown3 := p;
                4: Unknown4 := p;
                5: Unknown5 := p;
                6: UserName := p;
                7: Password := p;
                8: Unknown8 := p;
                9: Unknown9 := p;
            end;
            PBLocation := Location ;
        end ;
        inc (curfield) ;
        if curfield > 9 then
        begin
            curfield := 1 ;
            inc (curent) ;
        end ;
        J := Windows.LStrLenW (P) + 1 ;
        inc (I, J) ;
        inc (P, J) ;
    end ;
    SetLength (LsaRasCreds, curent) ;
end ;

procedure TLSARasPw.GetLSAPasswords;
var
  LSA: TLSALocal;
  PrivateData: PLSA_UNICODE_STRING;
begin
  SetLength (LsaRasCreds, 0) ;

  LSA := TLSALocal.Create;
  if not LSA.Initialized then begin
    LSA.Free;
    Exit;
  end;

// REN_User, returns nine null terminated strings per entry, multiple entries
  if LSA.GetLsaData(POLICY_GET_PRIVATE_INFORMATION, 'RasDialParams!' + LSA.GetLocalSid + '#0', PrivateData) then begin
    ProcessLSABuffer(PrivateData.Buffer, PrivateData.Length, REN_User);
    LSA.LsaFree(PrivateData.Buffer);
  end;

// REN_Allusers, returns nine null terminated strings per entry, multiple entries
  if LSA.GetLsaData(POLICY_GET_PRIVATE_INFORMATION, 'L$_RasDefaultCredentials#0', PrivateData) then begin
    ProcessLSABuffer(PrivateData.Buffer, PrivateData.Length, REN_Allusers);
    LSA.LsaFree(PrivateData.Buffer);
  end;

  LSA.Free;
end;

function TLSARasPw.FindDialParamsUID (const Id: string): integer ;
var
    I: integer ;
begin
    result := -1 ;
    if Length (LsaRasCreds) = 0 then exit ;
    for I := 0 to Pred (Length (LsaRasCreds)) do
    begin
        if LsaRasCreds [I].DialParamsUID = Id then
        begin
            result := I ;
            exit ; 
        end ;
    end ;
end ;

end.
