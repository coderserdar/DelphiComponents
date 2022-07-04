unit NetUtilsUnit;

interface

uses
  Windows, LMAccess, LMCons, LMAPIBUF, LMERR;


type
  TInfoLevel = (ilLevel0, ilLevel1, ilLevel2, ilLevel3, ilLevel10, ilLevel11,
                ilLevel12);
  TEnumAccountsFilter = (afTempDuplicate, afNormal, afInterdomainTrust,
                         afWorkstationTrust, afServerTrust);
  TEnumAccountsFilters = set of TEnumAccountsFilter;

function EnumFilterToInteger(const Filters: TEnumAccountsFilters): Integer;
function InfoLevelToInteger(const InfoLevel: TInfoLevel): Integer;
function PrivilegeLevelToString(const PrivilegeLevel: DWord): string;
function UserFlagsToString(const Flags: DWord): string;
procedure CheckNet(const Value: Integer);

implementation

uses
  SysUtils;

function EnumFilterToInteger(const Filters: TEnumAccountsFilters): Integer;
begin
  Result := 0;
  if afTempDuplicate in Filters then
    Result := Result or FILTER_TEMP_DUPLICATE_ACCOUNT;
  if afNormal in Filters then
    Result := Result or FILTER_NORMAL_ACCOUNT;
  if afInterdomainTrust in Filters then
    Result := Result or FILTER_INTERDOMAIN_TRUST_ACCOUNT ;
  if afWorkstationTrust in Filters then
    Result := Result or FILTER_WORKSTATION_TRUST_ACCOUNT ;
  if afServerTrust in Filters then
    Result := Result or FILTER_SERVER_TRUST_ACCOUNT;
end;

function InfoLevelToInteger(const InfoLevel: TInfoLevel): Integer;
begin
  case InfoLevel of
    ilLevel0:   Result := 0;
    ilLevel1:   Result := 1;
    ilLevel2:   Result := 2;
    ilLevel3:   Result := 3;
    ilLevel10:  Result := 10;
    ilLevel11:  Result := 11;
    ilLevel12:  Result := 12;
    else        raise Exception.Create('Unrecognized information level');
  end;
end;

function PrivilegeLevelToString(const PrivilegeLevel: DWord): string;
begin
  case PrivilegeLevel of
    USER_PRIV_GUEST:  Result := 'Guest';
    USER_PRIV_USER:   Result := 'User';
    USER_PRIV_ADMIN:  Result := 'Administrator';
    else              Result := 'Unknown';
  end;
end;

function UserFlagsToString(const Flags: DWord): string;
var IsFirst: Boolean;
  procedure OnFlag(var S: string; const Mask: DWord; const Text: string);
  begin
    if (Mask and Flags) <> 0 then begin
      if IsFirst then
        S := Text
      else
        S := S + ';' + Text;
      IsFirst := False;
    end;
  end;
begin
  IsFirst := True;
  Result := '';
  OnFlag(Result, UF_SCRIPT, 'The logon script executed');
  OnFlag(Result, UF_ACCOUNTDISABLE, 'The user''s account is disabled');
  OnFlag(Result, UF_HOMEDIR_REQUIRED, 'The home directory is required');
  OnFlag(Result, UF_PASSWD_NOTREQD, 'No password is required');
  OnFlag(Result, UF_PASSWD_CANT_CHANGE , 'The user cannot change the password');
  OnFlag(Result, UF_LOCKOUT, 'The account is currently locked out');
  OnFlag(Result, UF_DONT_EXPIRE_PASSWD, 'Represents the password, which should never expire on the account.');
  OnFlag(Result, UF_ENCRYPTED_TEXT_PASSWORD_ALLOWED, 'The user''s password is stored under reversible encryption in the Active Directory.');
  OnFlag(Result, UF_NOT_DELEGATED, 'Marks the account as "sensitive"; other users cannot act as delegates of this user account.');
  OnFlag(Result, UF_SMARTCARD_REQUIRED, 'Requires the user to log on to the user account with a smart card.');
  { These are used in Windows 2000 and I do not have the correct header. }
  {
  OnFlag(Result, UF_USE_DES_KEY_ONLY, 'Restrict this principal to use only Data Encryption Standard (DES) encryption types for keys.');
  OnFlag(Result, UF_DONT_REQUIRE_PREAUTH, 'This account does not require Kerberos preauthentication for logon.');
  OnFlag(Result, UF_TRUSTED_FOR_DELEGATION, 'The account is enabled for delegation. This is a security-sensitive setting - accounts with this option enabled should be tightly controlled. This setting allows a service running under the account to assume a client''s identity and authenticate as that user to other remote servers on the network.');
  }
end;

procedure CheckNet(const Value: Integer);
var s: string;
begin
  s := '';
  case Value of
    NERR_SUCCESS: exit;
    ERROR_ACCESS_DENIED:  s := 'The user does not have access to the requested information';
    NERR_InvalidComputer: s := 'The computer name is invalid';
    ERROR_MORE_DATA:      s := 'More entries are available with subsequent calls';
    NERR_DCNotFound:      s := 'Could not find the domain controller for the domain';
    ERROR_INVALID_NAME:   s := 'The name could not be found';
    {ERROR_ACCESS_DENIED:  s := 'The user does not have access to the requested information';}
    ERROR_INVALID_LEVEL:  s := 'The Level parameter specifies an invalid value';
    else                  s := 'Unknown error code ' + IntToStr(Value) + ' ($' + IntToHex(Value, 4) + ')';
  end;
  if s <> '' then
    raise Exception.Create(s);
end;

end.
