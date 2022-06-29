unit ImpersonatorUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TLogonType = (ltBatch, ltInteractive, ltService, ltNetwork);
  TImpersonator = class(TComponent)
  private
    FActive: Boolean;
    FUserName: string;
    FPassword: string;
    FLogonType: TLogonType;
    FDomain: string;
    FToken :THandle;
    procedure SetActive(const Value: Boolean);
    function GetToken: THandle;
    { Private declarations }
  protected
    { Protected declarations }
    procedure Loaded; override;
    procedure CheckPrivilege(const Privilege :PChar);
    procedure CheckPrimaryPrivilege(const Privilege :PChar);
  public
    { Public declarations }
    constructor Create(AOwner :TComponent); override;
    destructor Destroy; override;
    procedure LaunchProcess(const ProcessName: string);
    property Token :THandle read GetToken;
  published
    { Published declarations }
    property Active :Boolean read FActive write SetActive default False;
    property UserName :string read FUserName write FUserName;
    property Password :string read FPassword write FPassword;
    property Domain :string read FDomain write FDomain;
    property LogonType :TLogonType read FLogonType write FLogonType default ltInteractive;
  end;

procedure Register;

function LogonTypeToWinLogonType(const LogonType :TLogonType) :Cardinal;

implementation

uses
  SysConst;

const
  { Public Const SE_TCB_NAME = "SeTcbPrivilege" }
  SE_TCB_NAME = 'SeTcbPrivilege';

procedure Register;
begin
  RegisterComponents('MLR Sysco', [TImpersonator]);
end;

function LogonTypeToWinLogonType(const LogonType :TLogonType) :Cardinal;
begin
  case LogonType of
    ltBatch:        Result := LOGON32_LOGON_BATCH;
    ltInteractive:  Result := LOGON32_LOGON_INTERACTIVE;
    ltService:      Result := LOGON32_LOGON_SERVICE;
    ltNetwork:      Result := LOGON32_LOGON_NETWORK;
    else            Result := 0;
  end;
end;

procedure RaiseLastWin32ErrorDescription(const Description :string);
var
  LastError: DWORD;
  Error: EWin32Error;
  s :string;
begin
  LastError := GetLastError;
  if LastError <> ERROR_SUCCESS then
    s := Format(SWin32Error, [LastError, SysErrorMessage(LastError)]) else
    s := SUnkWin32Error;
  Error := EWin32Error.Create(s + #13 + Description);
  Error.ErrorCode := LastError;
  raise Error;
end;

{ TImpersonator }

constructor TImpersonator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLogonType := ltInteractive;
end;

destructor TImpersonator.Destroy;
begin
  Active := False;
  inherited Destroy;
end;

function TImpersonator.GetToken: THandle;
begin
  if FToken = 0 then Active := True;
  Result := FToken;
end;

procedure TImpersonator.Loaded;
begin
  inherited Loaded;
  if FActive then begin
    FActive := False;
    Active := True;
  end;
end;

procedure ReportUser(const Message :string);
begin
  MessageBox(0, PChar(Message), nil, MB_SERVICE_NOTIFICATION or MB_TOPMOST);
end;

function GetPrivilegeDisplayName(const Privilege :PChar) :string;
var
  DisplayNameLength :Cardinal;
  s :string;
  LanguageId :Cardinal;
begin
  DisplayNameLength := 500;
  SetLength(s, DisplayNameLength);
  LanguageId := GetUserDefaultLangID;
  if not LookupPrivilegeDisplayName(nil, Privilege, PChar(s), DisplayNameLength,
    LanguageId) then
    Result := Privilege else
    Result := Copy(s, 1, StrLen(PChar(s)));
end;

procedure TImpersonator.CheckPrivilege(const Privilege :PChar);
var
  LUID :Int64;          { ID of privilege. }
  AToken :THandle;      { Token to check. }
  pfResult :LongBool;   { Result of check. }
  RequiredPrivileges :PRIVILEGE_SET;
  s :string;
begin
  if not LookupPrivilegeValue(nil, Privilege, LUID) then
    RaiseLastWin32ErrorDescription('Cannot lookup privilege value.');
  if FToken = 0 then
    AToken := 0 else
    AToken := FToken;
  RequiredPrivileges.PrivilegeCount := 1;
  RequiredPrivileges.Control := PRIVILEGE_SET_ALL_NECESSARY;
  RequiredPrivileges.Privilege[0].Luid := LUID;
  RequiredPrivileges.Privilege[0].Attributes := 0;
  if not PrivilegeCheck(AToken, RequiredPrivileges, pfResult) then
    RaiseLastWin32ErrorDescription('Cannot check privilege.');
  if not pfResult then begin
    s := GetPrivilegeDisplayName(Privilege);
    raise Exception.Create('Privilege ' + s + ' not granted');
  end;
end;

procedure TImpersonator.CheckPrimaryPrivilege(const Privilege :PChar);
var
  LUID :Int64;          { ID of privilege. }
  AToken :THandle;      { Token to check. }
  pfResult :LongBool;   { Result of check. }
  RequiredPrivileges :PRIVILEGE_SET;
  s :string;
begin
  if not LookupPrivilegeValue(nil, Privilege, LUID) then
    RaiseLastWin32ErrorDescription('Cannot lookup privilege value.');
  if not OpenProcessToken(GetCurrentProcess, TOKEN_READ, AToken) then
    RaiseLastWin32ErrorDescription('Cannot open process token');
  try
    RequiredPrivileges.PrivilegeCount := 1;
    RequiredPrivileges.Control := PRIVILEGE_SET_ALL_NECESSARY;
    RequiredPrivileges.Privilege[0].Luid := LUID;
    RequiredPrivileges.Privilege[0].Attributes := 0;
    if not PrivilegeCheck(AToken, RequiredPrivileges, pfResult) then
      RaiseLastWin32ErrorDescription('Cannot check privilege.');
    if not pfResult then begin
      s := GetPrivilegeDisplayName(Privilege);
      raise Exception.Create('Privilege ' + s + ' not granted');
    end;
  finally
    CloseHandle(AToken);
  end;
end;

procedure TImpersonator.SetActive(const Value: Boolean);
begin
  if FActive = Value then exit;
  if (csDesigning in ComponentState) or (csReading in ComponentState) then begin
    FActive := Value;
    exit;
  end;
  if Value then begin
    CheckPrimaryPrivilege(SE_TCB_NAME);
    ReportUser('Will log user ' + FUserName +
        ' with password ' + FPassword + ' on domain ' + FDomain);
    if not LogonUser(PChar(FUserName), PChar(FDomain), PChar(FPassword),
      LogonTypeToWinLogonType(FLogonType), LOGON32_PROVIDER_DEFAULT, FToken) then
      RaiseLastWin32ErrorDescription('Cannot log user ' + FUserName +
        ' with password ' + FPassword + ' on domain ' + FDomain);
    ReportUser('User logged on.');
    try
      {AdjustTokenPrivileges(FToken, False,}
      if not ImpersonateLoggedOnUser(FToken) then
        RaiseLastWin32ErrorDescription('Cannot impersonate user.');
      ReportUser('User impersonated.');
    except
      CloseHandle(FToken);
      FToken := 0;
      raise;
    end;
  end else begin
    RevertToSelf;
    CloseHandle(FToken);
    FToken := 0;
  end;
  FActive := Value;
end;

procedure TImpersonator.LaunchProcess(const ProcessName: string);
var
  OldActive: Boolean;
  si: StartupInfo;
  pi: TProcessInformation;
begin
  OldActive := True;
  Active := True;
  si.cb := sizeof(si);
  FillChar(si, sizeof(si), #0);
  if CreateProcessAsUser(FToken, PChar(ProcessName), nil, nil, nil, False, 0,
    nil, nil, si, pi) then begin
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);
  end else
    raise Exception.Create('Could not create process ' + ProcessName);
end;

end.

