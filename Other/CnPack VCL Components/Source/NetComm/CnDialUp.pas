{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2007 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��http://www.cnpack.org                                   }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnDialUp;
{* |<PRE>
================================================================================
* ������ƣ�����ͨѶ�����
* ��Ԫ���ƣ��������ʵ�ֵ�Ԫ
* ��Ԫ���ߣ�����
* ��    ֲ��Childe Ng
* ��    ע��
* ����ƽ̨��PWin2000Pro + Delphi 5.01
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* �޸ļ�¼��2008.06.03 V1.0
*                ������Ԫ
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, WinInet;

const
  DNLEN = 15;
  UNLEN = 256;
  PWLEN = 256;

  RAS_MaxEntryName = 256;
  RAS_MaxDeviceType = 16;
  RAS_MaxDeviceName = 128;
  RAS_MaxPhoneNumber = 128;
  RAS_MaxCallbackNumber = RAS_MaxPhoneNumber;

  RASCS_PAUSED = $1000;
  RASCS_DONE = $2000;

  RASCS_OpenPort = 0;
  RASCS_PortOpened = 1;
  RASCS_ConnectDevice = 2;
  RASCS_DeviceConnected = 3;
  RASCS_AllDevicesConnected = 4;
  RASCS_Authenticate = 5;
  RASCS_AuthNotify = 6;
  RASCS_AuthRetry = 7;
  RASCS_AuthCallback = 8;
  RASCS_AuthChangePassword = 9;
  RASCS_AuthProject = 10;
  RASCS_AuthLinkSpeed = 11;
  RASCS_AuthAck = 12;
  RASCS_ReAuthenticate = 13;
  RASCS_Authenticated = 14;
  RASCS_PrepareForCallback = 15;
  RASCS_WaitForModemReset = 16;
  RASCS_WaitForCallback = 17;
  RASCS_Projected = 18;
  RASCS_StartAuthentication = 19;
  RASCS_CallbackComplete = 20;
  RASCS_LogonNetwork = 21;
  RASCS_Interactive = RASCS_PAUSED;
  RASCS_RetryAuthentication = RASCS_PAUSED + 1;
  RASCS_CallbackSetByCaller = RASCS_PAUSED + 2;
  RASCS_PasswordExpired = RASCS_PAUSED + 3;
  RASCS_Connected = RASCS_DONE;
  RASCS_Disconnected = RASCS_DONE + 1;

type
  THRasConn = Longint;

  LPRasConnA = ^TRasConnA;
  TRasConnA = record
    dwSize: Longint;
    hrasconn: THRasConn;
    szEntryName: array[0..RAS_MaxEntryName] of AnsiChar;
    szDeviceType: array[0..RAS_MaxDeviceType] of AnsiChar;
    szDeviceName: array[0..RAS_MaxDeviceName] of AnsiChar;
  end;

  LPRasConn = ^TRasConn;
  TRasConn = TRasConnA;

  LPRasConnState = ^TRasConnState;
  TRasConnState = Integer;

  LPRasConnStatusA = ^TRasConnStatusA;
  TRasConnStatusA = record
    dwSize: Longint;
    rasconnstate: TRasConnState;
    dwError: Longint;
    szDeviceType: array[0..RAS_MaxDeviceType] of AnsiChar;
    szDeviceName: array[0..RAS_MaxDeviceName] of AnsiChar;
  end;

  LPRasConnStatus = ^TRasConnStatus;
  TRasConnStatus = TRasConnStatusA;

  LPRasEntryNameA = ^TRasEntryNameA;
  TRasEntryNameA = record
    dwSize: Longint;
    szEntryName: array[0..RAS_MaxEntryName] of AnsiChar;
  end;

  LPRasEntryName = ^TRasEntryName;
  TRasEntryName = TRasEntryNameA;

  LPRasDialParamsA = ^TRasDialParamsA;
  TRasDialParamsA = record
    dwSize: Longint;
    szEntryName: array[0..RAS_MaxEntryName] of AnsiChar;
    szPhoneNumber: array[0..RAS_MaxPhoneNumber] of AnsiChar;
    szCallbackNumber: array[0..RAS_MaxCallbackNumber] of AnsiChar;
    szUserName: array[0..UNLEN] of AnsiChar;
    szPassword: array[0..PWLEN] of AnsiChar;
    szDomain: array[0..DNLEN] of AnsiChar;
  end;

  LPRasDialParams = ^TRasDialParams;
  TRasDialParams = TRasDialParamsA;

  LPRasDialExtensions = ^TRasDialExtensions;
  TRasDialExtensions = record
    dwSize: Longint;
    dwfOptions: Longint;
    hwndParent: HWnd;
    reserved: Longint;
  end;

type
  TOnStatusEvent = procedure(Sender: TObject; MessageText: string; Error: Boolean) of object;

  TCnDialUp = class(TComponent)
  private
    FTimer: TTimer;
    FPassword: string;
    FUsername: string;
    FConnectTo: string;
    hRasDLL: THandle;
    StatusStr: string;
    ErrorStat: Boolean;
    AsyncStatus: Boolean;
    FLangStrList: TStringList;
    FPossibleConnections: TStringList;
    FOnStatusEvent: TOnStatusEvent;
    function StatusString(State: TRasConnState; Error: Integer; var ES: Boolean): string;
    function GetActiveConnection: string;
    procedure SetLangStrList(Value: TStringList);
    function GetCurrentConnection: string;
    function GetPossibleConnections: TStringList;
    procedure GetConnections(var SL: TStringList);
    function GetRasInstalled: Boolean;
    function GetOnlineStatus: Boolean;
  protected
    procedure Timer(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GoOnline: Boolean;
    procedure GoOffline;
  published
    property IsOnline: Boolean read GetOnlineStatus;
    {* ����Ƿ�����������}
    property Password: string read FPassword write FPassword;
    {* �������ӵ�����}
    property Username: string read FUsername write FUsername;
    {* �������ӵ��û���}
    property CurrentConnection: string read GetCurrentConnection;
    {* ��ǰ������������}
    property ConnectTo: string read FConnectTo write FConnectTo;
    {* ��Ҫ���ӵ�����������}
    property PossibleConnections: TStringList read GetPossibleConnections;
    {* ���п��õĲ�������}
    property LangStrList: TStringList read FLangStrList write SetLangStrList;
    {* �û��������棬�ɶ��ﻯ����}
    property OnStatusEvent: TOnStatusEvent read FOnStatusEvent write FOnStatusEvent;
    {* �������ӻ�Ͽ�����ʱ�������¼�}
    property RasInstalled: Boolean read GetRasInstalled;
    {* ������л���}
  end;

implementation

var
  xSelf: Pointer;

  RasHangUp: function(hConn: THRasConn): Longint; stdcall;
  RasEnumConnections: function(RasConnArray: LPRasConn; var lpcb: Longint; var lpcConnections: Longint): Longint; stdcall;
  RasGetConnectStatus: function(hConn: THRasConn; var lpStatus: TRasConnStatus): Longint; stdcall;
  RasEnumEntries: function(reserved: PAnsiChar; lpszPhoneBook: PAnsiChar; EntryNamesArray: LPRasEntryNameA; var lpcb: Longint; var lpcEntries: Longint): Longint; stdcall;
  RasGetEntryDialParams: function(lpszPhoneBook: PAnsiChar; var lpDialParams: TRasDialParams; var lpfPassword: LongBool): Longint; stdcall;
  RasGetErrorString: function(ErrorValue: Integer; ErrorString: PAnsiChar; cBufSize: Longint): Longint; stdcall;
  RasDial: function(lpRasDialExt: LPRasDialExtensions; lpszPhoneBook: PAnsiChar; var Params: TRasDialParams; dwNotifierType: Longint; lpNotifier: Pointer; var RasConn: THRasConn): Longint; stdcall;
  RasSetEntryDialParams: function(lpszPhoneBook: PAnsiChar; var lpDialParams: TRasDialParams; fRemovePassword: LongBool): Longint; stdcall;

procedure TCnDialUp.Timer(Sender: TObject);
begin
  FTimer.Enabled := False;
  if AsyncStatus = False then Exit;
  if Assigned(FOnStatusEvent) then
    FOnStatusEvent(TCnDialUp(xSelf), StatusStr, ErrorStat);
  AsyncStatus := False;
end;

procedure RasCallback(Msg: Integer; State: TRasConnState; Error: Integer); stdcall;
begin
  while TCnDialUp(xSelf).AsyncStatus = True do ;
  TCnDialUp(xSelf).AsyncStatus := True;
  TCnDialUp(xSelf).FTimer.Enabled := True;
  TCnDialUp(xSelf).StatusStr := TCnDialUp(xSelf).StatusString(State, Error, TCnDialUp(xSelf).ErrorStat);
end;

constructor TCnDialUp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AsyncStatus := False;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 1;
  FTimer.OnTimer := Timer;
  FPossibleConnections := TStringList.Create;
  FLangStrList := TStringList.Create;
  FLangStrList.Add('Connecting to %s...');
  FLangStrList.Add('Verifying username and password...');
  FLangStrList.Add('An error occured while trying to connect to %s.');

  // Attempt to load the RASAPI32 DLL.  If the DLL loads, hRasDLL will
  //   be non-zero.  Otherwise, hRasDLL will be zero.

  hRasDLL := LoadLibrary('RASAPI32.DLL');

  // Assign function pointers for the RAS functions.
  if hRasDLL < 1 then Exit;
  @RasEnumConnections := GetProcAddress(hRasDLL, 'RasEnumConnectionsA');
  @RasHangUp := GetProcAddress(hRasDLL, 'RasHangUpA');
  @RasGetConnectStatus := GetProcAddress(hRasDLL, 'RasGetConnectStatusA');
  @RasEnumEntries := GetProcAddress(hRasDLL, 'RasEnumEntriesA');
  @RasGetEntryDialParams := GetProcAddress(hRasDLL, 'RasGetEntryDialParamsA');
  @RasGetErrorString := GetProcAddress(hRasDLL, 'RasGetErrorStringA');
  @RasDial := GetProcAddress(hRasDLL, 'RasDialA');
  @RasSetEntryDialParams := GetProcAddress(hRasDLL, 'RasSetEntryDialParamsA');
end;

destructor TCnDialUp.Destroy;
begin
  // If the RASAPI32 DLL was loaded, then free it.
  if RasInstalled then
    FreeLibrary(hRasDLL);

  FLangStrList.Free;
  FPossibleConnections.Free;
  FTimer.Free;
  inherited Destroy;
end;

function TCnDialUp.GetRasInstalled: Boolean;
// Determines if RAS has been installed by checking for DLL handle.  If RAS
//   has not been installed, hRasDLL is zero.

begin
  Result := hRasDLL <> 0;
end;

function TCnDialUp.GetCurrentConnection: string;
begin
  Result := GetActiveConnection;
end;

function TCnDialUp.GetPossibleConnections: TStringList;
begin
  FPossibleConnections.Clear;
  GetConnections(FPossibleConnections);
  Result := FPossibleConnections;
end;

procedure TCnDialUp.SetLangStrList(Value: TStringList);
begin
  FLangStrList.Assign(Value);
end;

function TCnDialUp.GoOnline: Boolean;
var
  hRAS: THRasConn;
  B: LongBool;
  R: Integer;
  C: array[0..100] of Char;
  DialParams: TRasDialParams;
begin
  Result := False;

  if not RasInstalled then Exit;

  try
    GoOffline;
    FillChar(DialParams, SizeOf(TRasDialParams), 0);
    DialParams.dwSize := SizeOf(TRasDialParams);
    StrPCopy(DialParams.szEntryName, {$IFDEF UNICODE}AnsiString{$ENDIF}(FConnectTo));
    B := False;
    R := RasGetEntryDialParams(nil, DialParams, B);
    if R <> 0 then
    begin
      Result := False;
      GoOffline;
      if Assigned(FOnStatusEvent) then
        FOnStatusEvent(Self, FLangStrList[2], True);
      Exit;
    end;
    DialParams.dwSize := SizeOf(TRasDialParams);
    StrPCopy(DialParams.szUserName, {$IFDEF UNICODE}AnsiString{$ENDIF}(FUsername));
    StrPCopy(DialParams.szPassword, {$IFDEF UNICODE}AnsiString{$ENDIF}(FPassword));
    R := RasSetEntryDialParams(nil, DialParams, False);
    if R <> 0 then
    begin
      Result := False;
      GoOffline;
      if Assigned(FOnStatusEvent) then
        FOnStatusEvent(Self, FLangStrList[2], True);
      Exit;
    end;
    xSelf := Self;
    AsyncStatus := False;
    hRAS := 0;
    R := RasDial(nil, nil, DialParams, 0, @RasCallback, hRAS);
    if R <> 0 then
    begin
      Result := False;
      RasGetErrorString(R, PAnsiChar({$IFDEF UNICODE}AnsiString{$ELSE}string{$ENDIF}(C)), 100);
      GoOffline;
      if Assigned(FOnStatusEvent) then
        FOnStatusEvent(Self, C, True);
      Exit;
    end;
    Result := True;
  except
    on E: Exception do
    begin
      GoOffline;
      if Assigned(FOnStatusEvent) then
        FOnStatusEvent(Self, E.Message, True);
    end;
  end;
end;

procedure TCnDialUp.GetConnections(var SL: TStringList);
var
  BuffSize, Entries, R, I: Integer;
  Entry: array[1..100] of TRasEntryName;
begin
  if not RasInstalled then Exit;

  SL.Clear;
  Entry[1].dwSize := SizeOf(TRasEntryName);
  BuffSize := SizeOf(TRasEntryName) * 100;
  R := RasEnumEntries(nil, nil, @Entry[1], BuffSize, Entries);
  if (R = 0) and (Entries > 0) then
    for I := 1 to Entries do SL.Add({$IFDEF UNICODE}String{$ENDIF}(Entry[I].szEntryName));
end;

function TCnDialUp.GetActiveConnection: string;
var
  BufSize, NumEntries, I, R: Integer;
  Entries: array[1..100] of TRasConn;
  Stat: TRasConnStatus;
begin
  Result := '';

  if not RasInstalled then Exit;

  Entries[1].dwSize := SizeOf(TRasConn);
  BufSize := SizeOf(TRasConn) * 100;
  FillChar(Stat, SizeOf(TRasConnStatus), 0);
  Stat.dwSize := SizeOf(TRasConnStatus);
  R := RasEnumConnections(@Entries[1], BufSize, NumEntries);
  if R = 0 then
    if NumEntries > 0 then
      for I := 1 to NumEntries do begin
        RasGetConnectStatus(Entries[I].hrasconn, Stat);
        if Stat.rasconnstate = RASCS_Connected then
          Result := Entries[I].szEntryName + ' (' + {$IFDEF UNICODE}string{$ENDIF}(Entries[I].szDeviceName) + ')'
      end;
end;

procedure TCnDialUp.GoOffline;
var
  Entries: array[1..100] of TRasConn;
  BufSize, NumEntries, R, I, E: Integer;
begin

  if not RasInstalled then Exit;

  for E := 0 to 6 do begin
    Entries[1].dwSize := SizeOf(TRasConn);
    R := RasEnumConnections(@Entries[1], BufSize, NumEntries);
    if R = 0 then begin
      if NumEntries > 0 then
        for I := 1 to NumEntries do RasHangUp(Entries[I].hrasconn);
    end;
    Application.ProcessMessages;
  end;
end;

function TCnDialUp.StatusString(State: TRasConnState; Error: Integer; var ES: Boolean): string;
var
  C: array[0..100] of Char;
  S: string;
begin
  S := 'Something went wrong...';
  ES := False;

  if not RasInstalled then Exit;

  if Error <> 0 then
  begin
    RasGetErrorString(Error, PAnsiChar({$IFDEF UNICODE}AnsiString{$ELSE}string{$ENDIF}(C)), 100);
    ES := True;
    S := C;
  end
  else
  begin
    case State of
      //connecting
      RASCS_OpenPort, RASCS_PortOpened, RASCS_ConnectDevice, RASCS_DeviceConnected,
        RASCS_AllDevicesConnected, RASCS_PrepareForCallback, RASCS_WaitForModemReset,
        RASCS_WaitForCallback, RASCS_Projected, RASCS_CallbackComplete, RASCS_LogonNetwork,
        RASCS_Interactive, RASCS_CallbackSetByCaller, RASCS_Connected: S := Format(FLangStrList[0], [FConnectTo]);
      //authenticateing
      RASCS_Authenticate, RASCS_StartAuthentication, RASCS_Authenticated: S := FLangStrList[1];
      //error
      RASCS_AuthNotify, RASCS_AuthRetry, RASCS_AuthCallback, RASCS_AuthChangePassword,
        RASCS_AuthProject, RASCS_AuthLinkSpeed, RASCS_AuthAck, RASCS_ReAuthenticate,
        RASCS_RetryAuthentication, RASCS_Disconnected, RASCS_PasswordExpired: S := Format(FLangStrList[2], [FConnectTo]);
    end;
  end;
  Result := S;
end;

function TCnDialUp.GetOnlineStatus: Boolean;
var
  Types: Integer;
begin
  Types := INTERNET_CONNECTION_MODEM +
    INTERNET_CONNECTION_LAN + INTERNET_CONNECTION_PROXY;
  Result := InternetGetConnectedState(@Types, 0);
end;

end.

