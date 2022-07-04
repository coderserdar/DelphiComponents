unit kmNetUpdate;

interface

uses
  Windows,SysUtils,ComCtrls,Classes,Graphics,Forms,
  kmTypes,kmUtils,kmWinInet,kmWizard;

type
  TkmLaunchUpdater = procedure(Sender:TObject; var Launch:Boolean) of object;
  TkmSetRegistryValues = procedure(Sender:TObject; var newVerDate,newVerNum:string) of object;

  TkmNetUpdate = class(TComponent)
  private
    FOnCheck: TNotifyEvent;
    FOnCompleted: TNotifyEvent;
    FOnCheckCompleted: TNotifyEvent;
    FOnLaunchUpdater: TkmLaunchUpdater;
    FOnSetRegistry: TkmSetRegistryValues;

    FAccessType: TnuAccessType;
    FActive: boolean;
    FAutoCheckDelay: integer;
    FCacheOptions: TnuCacheOptions;
    FCheckDay: TnuDay;
    FCheckTimeStr: string;
    FCloseEXE: boolean;
    FCreateBackup: boolean;
    FFTPPassive: boolean;
    FHideFileLocation: boolean;
    FInternetOptions: TnuInternetOptions;
    FLaunchApp: boolean;
    FLaunchParams: string;
    FLogFile: boolean;
    FLogin: boolean;
    FMsgStrs: TStrings;
    FPassword: string;
    FPort: integer;
    FProxyBypass: String;
    FProxyPassword: String;
    FProxyPort: Integer;
    FProxyServer: String;
    FProxyUsername: String;
    FRunMode: TnuRunMode;
    FSchedule: TnuSchedule;
    FShowMessages: TnuShowMessages;
    FStayOnTop: Boolean;
    FThreadPriority: TThreadPriority;
    FURLFile: string;
    FURLPath: string;
    FURLProtocol: TnuProtocol;
    FUseRegistry: Boolean;
    FUserName: string;
    FVersionControl: TnuVersionControl;
    FVersionDate: string;
    FVersionNumber: string;
    FWarnOnCancel: boolean;
    FWarnOnRestart: boolean;
    FWizard: TfmWizard;
    procedure SetActive(Value:boolean);
    procedure SetAutoCheckDelay(Value:Integer);
    procedure SetCheckTime(Value:string);
    procedure SetURLProtocol(Value:TnuProtocol);
    procedure SetURLPath(Value:string);
    procedure SetVersionDate(Value:string);
    procedure SetMessages();
    procedure SetMessageStrs(Value: TStrings);
  public
    FCheckTime: TDateTime;
    FURLPrefix: string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Check(); overload;
    procedure Check(noMsg:boolean); overload;
  published
    property AccessType: TnuAccessType read FAccessType write FAccessType default atPreconfig;
    property Active: boolean read FActive write SetActive default true;
    property AutoCheckDelay: integer read FAutoCheckDelay write SetAutoCheckDelay;
    property CacheOptions: TnuCacheOptions read FCacheOptions write FCacheOptions;
    property CheckDay: TnuDay read FCheckDay write FCheckDay;
    property CheckTime: string read FCheckTimeStr write SetCheckTime;
    property CloseEXE: boolean read FCloseEXE write FCloseEXE default true;
    property CreateBackup: boolean read FCreateBackup write FCreateBackup;
    property HideFileLocation: boolean read FHideFileLocation write FHideFileLocation;
    property InternetOptions: TnuInternetOptions read FInternetOptions write FInternetOptions;
    property LaunchApp: boolean read FLaunchApp write FLaunchApp default true;
    property LaunchParams: string read FLaunchParams write FLaunchParams;
    property LogFile: boolean read FLogFile write FLogFile default true;
    property MsgStrs: TStrings read FMsgStrs write SetMessageStrs;
    property ProxyBypass: String read FProxyBypass write FProxyBypass;
    property ProxyPassword: String read FProxyPassword write FProxyPassword;
    property ProxyPort: Integer read FProxyPort write FProxyPort;
    property ProxyServer: String read FProxyServer write FProxyServer;
    property ProxyUsername: String read FProxyUsername write FProxyUsername;
    property RunMode: TnuRunMode read FRunMode write FRunMode;
    property Schedule: TnuSchedule read FSchedule write FSchedule;
    property ShowMessages: TnuShowMessages read FShowMessages write FShowMessages default [mAskUpgrade, mConnLost, mPasswordRequest];
    property StayOnTop: boolean read FStayOnTop write FStayOnTop default true;
    property ThreadPriority: TThreadPriority read FThreadPriority write FThreadPriority;
    property WarnOnCancel: boolean read FWarnOnCancel write FWarnOnCancel default true;
    property WarnOnRestart: boolean read FWarnOnRestart write FWarnOnRestart default true;
    property FTPPassive: boolean read FFTPPassive write FFTPPassive;
    property Login: boolean read FLogin write FLogin;
    property Password: string read FPassword write FPassword;
    property Port: integer read FPort write FPort;
    property URLFile: string read FURLFile write FURLFile;
    property URLPath: string read FURLPath write SetURLPath;
    property URLProtocol: TnuProtocol read FURLProtocol write SetURLProtocol;
    property UseRegistry: boolean read FUseRegistry write FUseRegistry default true;
    property UserName : string read FUserName write FUserName;
    property VersionControl: TnuVersionControl read FVersionControl write FVersionControl;
    property VersionDate: string read FVersionDate write SetVersionDate;
    property VersionNumber: string read FVersionNumber write FVersionNumber;

    property OnCheck: TNotifyEvent read FOnCheck write FOnCheck;
    property OnCompleted: TNotifyEvent read FOnCompleted write FOnCompleted;
    property OnCheckCompleted: TNotifyEvent read FOnCheckCompleted write FOnCheckCompleted;
    property OnLaunchUpdater: TkmLaunchUpdater read FOnLaunchUpdater write FOnLaunchUpdater;
    property OnSetRegistry: TkmSetRegistryValues read FOnSetRegistry write FOnSetRegistry;
  end;

var nuClient1: TkmNetUpdate;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Misc', [TkmNetUpdate]);
end; {Register}

{ ----------------------------------------------------------------- }

{TnuClient}
constructor TkmNetUpdate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMsgStrs := TStringList.Create;
  FActive := true;
  FAutoCheckDelay := 2000;
  FCacheOptions := [coAlwaysReload,coPragmaNoCache];
  FCloseEXE := true;
  FInternetOptions := [ioKeepConnection];
  FLaunchApp := true;
  FLogFile := true;
  FProxyBypass := '127.0.0.1,';
  FProxyPort := 8080;
  FShowMessages := [mAskUpgrade,mPromptCancel,mNoUpdateAvailable,mPasswordRequest];
  FStayOnTop := true;
  FThreadPriority := tpNormal;
  FTPPassive := true;
  FUseRegistry := true;
  FVersionNumber := '1.00.00';
  FWarnOnCancel := true;
  FWarnOnRestart := true;
  SetCheckTime('2:00:00 AM');
  SetMessages();
  SetURLProtocol(pHTTP);
  SetVersionDate('');
  URLFile := 'update_01.inf';
  URLPath := 'www.kidmoses.com/netupdater/update/';
  VersionControl := byVersion;
  nuClient1 := self;

  FWizard := TfmWizard.Create(Self);
  FWizard.Visible := False;
  fmWizard := FWizard;
end; {Create}
{ ----------------------------------------------------------------- }

destructor TkmNetUpdate.Destroy;
begin
  FMsgStrs.Clear;
  FMsgStrs.Free;
  nuClient1 := nil;
  fmWizard := nil;
  FWizard.Free;
  inherited Destroy;
end; {Destroy}

{ ----------------------------------------------------------------- }

procedure TkmNetUpdate.Check();
begin
  FWizard.ProcessAgent();
end; {CheckUpdate}

{ ----------------------------------------------------------------- }

procedure TkmNetUpdate.Check(noMsg:boolean);
begin
  if noMsg then
    nuClient1.ShowMessages := nuClient1.ShowMessages + [mNoUpdateAvailable]
  else nuClient1.ShowMessages := nuClient1.ShowMessages - [mNoUpdateAvailable];
  FWizard.ProcessAgent();
end; {CheckUpdate}

{ ----------------------------------------------------------------- }

procedure TkmNetUpdate.SetActive(Value:boolean);
begin
  FActive := Value;
  if csDesigning in ComponentState then exit;
  if Value then FWizard.OnCheckTimer(Self);
end; {SetActive}

{ ----------------------------------------------------------------- }

procedure TkmNetUpdate.SetAutoCheckDelay(Value:integer);
begin
  if Value < 1000 then
    FAutoCheckDelay := 1000
  else FAutoCheckDelay := Value;
end; {SetAutoCheckDelay}

{ ----------------------------------------------------------------- }

procedure TkmNetUpdate.SetURLProtocol(Value:TnuProtocol);
begin
  FURLProtocol := Value;
  case Value of
    pHTTP:  begin FURLPrefix := 'http://'; FPort := 80; end;
    pHTTPS: begin FURLPrefix := 'https://'; FPort := 443; end;
    pFTP:   begin FURLPrefix := 'ftp://'; FPort := 21; end;
    pFILE:  begin FURLPrefix := 'file://'; FPort := 0; end;
    end;
end; {SetURLProtocol}

{ ----------------------------------------------------------------- }

procedure TkmNetUpdate.SetURLPath(Value:string);
begin
  if Value <> '' then begin
    if FURLProtocol = pFile then begin
      if Value[Length(Value)] <> '\' then
        Value := Value + '\';
      end
    else begin
      if Value[Length(Value)] <> '/' then
        Value := Value + '/';
      end;
    end;
  FURLPath := Value;
end; {SetURLPath}

{ ----------------------------------------------------------------- }

procedure TkmNetUpdate.SetCheckTime(Value:string);
var dt: TDateTime;
begin
  if Value <> '' then begin
    try
      dt := StrToTime(Value);
      except
      dt := StrToTime('2:00:00 AM');
      end;
    end
  else dt := StrToTime('2:00:00 AM');
  FCheckTime := dt;
  FCheckTimeStr := TimeToStr(dt);
end; {SetCheckTime}

{ ----------------------------------------------------------------- }

procedure TkmNetUpdate.SetVersionDate(Value:string);
begin
  if Value = '' then
    FVersionDate := FormatDateTime('yyyy-mm-dd hh:nn:ss',now())
  else FVersionDate := Value;
end; {SetVersionDate}

{ ----------------------------------------------------------------- }

procedure TkmNetUpdate.SetMessageStrs(Value: TStrings);
begin
  FMsgStrs.Assign(Value);
end; {SetMessageStrs}

{ ----------------------------------------------------------------- }

procedure TkmNetUpdate.SetMessages();
var
  i:Integer;
begin
  FMsgStrs.Clear ;
  for i := 0 to 34 do FMsgStrs.Add('');

  // Captions for forms, panels and labels
  FMsgStrs[0]  := 'Net Update';
  FMsgStrs[1]  := 'Checking for updates...'; { not used }
  FMsgStrs[2]  := 'Downloading files...'; { not used }
  FMsgStrs[3]  := 'Update completed!'; { not used }
  FMsgStrs[4]  := 'Current file:'; { not used }
  FMsgStrs[5]  := 'Destination: %s';
  FMsgStrs[6]  := 'Source: %s';
  FMsgStrs[7]  := 'File: %s';
  FMsgStrs[8]  := 'Retreiving file information... please wait.';
  FMsgStrs[9]  := 'Elapsed: %d.%.2d min, remaining: %d.%.2d min  (%.2n of %.2n Kb)';
  FMsgStrs[10] := 'Downloading file %d of %d';
  FMsgStrs[11] := 'Installing file %d of %d';
  FMsgStrs[12] := 'The update downloaded successfully. Press OK to complete installation.';
  FMsgStrs[13] := 'The application, %s, is open. Close the application and press OK to continue.';
  FMsgStrs[14] := 'Application is still open. Do you want Net Update to close the application.';

  // Buttons
  FMsgStrs[15] := '&Yes'; { not used }
  FMsgStrs[16] := '&No'; { not used }
  FMsgStrs[17] := '&OK'; { not used }
  FMsgStrs[18] := '&Cancel';
  FMsgStrs[19] := '&Next >'; { not used }
  FMsgStrs[20] := '&Later'; { not used }
  FMsgStrs[21] := '&Finish'; { not used }

  // Error messages
  FMsgStrs[22] := 'Warning'; { not used }
  FMsgStrs[23] := 'Error'; { not used }
  FMsgStrs[24] := 'Information'; { not used }
  FMsgStrs[25] := 'A newer version of this software was released.';
  FMsgStrs[26] := 'Do you want to update your version with the latest one?';
  FMsgStrs[27] := 'A newer version of this software was not found.';
  FMsgStrs[28] := 'Update cancelled. File not found: %s';
  FMsgStrs[29] := 'Update cancelled. File lost: %s';
  FMsgStrs[30] := 'Are you sure you want to exit?';
  FMsgStrs[31] := 'Host is unreachable. Please check your Internet connection.';
  FMsgStrs[32] := 'The update is not complete. Are you sure you want to cancel?';
  FMsgStrs[33] := 'Error: 404 - File not found or inaccessible: ';
  FMsgStrs[34] := 'Connection with remote server lost. Restore connection and try again.';
end; { SetMessages }

end.
