{*******************************************************}
{                                                       }
{         Delphi VCL Extensions (RX)                    }
{                                                       }
{         Copyright (c) 1998 Master-Bank                }
{                                                       }
{*******************************************************}

unit RxNotify;

interface

{$I RX.INC}

uses
  Windows, SysUtils, Classes, Messages, ExtCtrls;

type
  TFileChangeFilter = (fnFileName, fnDirName, fnAttributes, fnSize,
    fnLastWrite, fnLastAccess, fnCreation, fnSecurity);
  TFileChangeFilters = set of TFileChangeFilter;
  TNotifyThread = class;

{ TRxFolderMonitor }

  TRxFolderMonitor = class(TComponent)
  private
    FNotifyThread: TNotifyThread;
    FFilter: TFileChangeFilters;
    FDelayTimer: TTimer;
    FDelayTime: Cardinal;
    FMonitorSubtree: Boolean;
    FFolderName: string;
    FStreamedActive: Boolean;
    FOnChange: TNotifyEvent;
    function GetActive: Boolean;
    function GetDelayTime: Cardinal;
    procedure SetActive(Value: Boolean);
    procedure SetFilter(Value: TFileChangeFilters);
    procedure SetMonitorSubtree(Value: Boolean);
    procedure SetFolderName(const Value: string);
    procedure SetDelayTime(Value: Cardinal);
    procedure Timer(Sender: TObject);
    procedure ThreadNotification(Sender: TObject);
  protected
    procedure Loaded; override;
    procedure Changed; dynamic;
    procedure FreeNotifyThread;
    procedure ResetNotifyThread(Activate: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read GetActive write SetActive default False;
    property DelayTime: Cardinal read GetDelayTime write SetDelayTime default 0;
    property Filter: TFileChangeFilters read FFilter write SetFilter
      default [fnFileName, fnDirName, fnSize, fnLastWrite];
    property FolderName: string read FFolderName write SetFolderName;
    property MonitorSubtree: Boolean read FMonitorSubtree write SetMonitorSubtree
      default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{ TNotifyThread }

  TNotifyThread = class(TThread)
  private
    FNotifyHandle: THandle;
    FEvent: THandle;
    FOnChange: TNotifyEvent;
    FFinished: Boolean;
    FLastError: DWORD;
    procedure CallOnChange;
    procedure StopWaiting;
  protected
    procedure DoChange; virtual;
    procedure DoTerminate; override;
    procedure Execute; override;
  public
    constructor Create(const FolderName: string; WatchSubtree: Boolean;
      Filter: TFileChangeFilters);
    destructor Destroy; override;
    procedure Terminate;
    property Terminated;
    property Finished: Boolean read FFinished;
    property LastError: DWORD read FLastError;
    property NotifyHandle: THandle read FNotifyHandle;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

function CreateNotifyThread(const FolderName: string; WatchSubtree: Boolean;
  Filter: TFileChangeFilters): TNotifyThread;

implementation

uses
  Forms,
  rxVCLUtils, rxFileUtil;

{$IFNDEF RX_D3}
const
  FILE_NOTIFY_CHANGE_LAST_ACCESS  = $00000020;
  FILE_NOTIFY_CHANGE_CREATION     = $00000040;
{$ENDIF}

{ TNotifyThread }

constructor TNotifyThread.Create(const FolderName: string;
  WatchSubtree: Boolean; Filter: TFileChangeFilters);
const
  NotifyFilters: array[TFileChangeFilter] of DWORD = (
    FILE_NOTIFY_CHANGE_FILE_NAME, FILE_NOTIFY_CHANGE_DIR_NAME,
    FILE_NOTIFY_CHANGE_ATTRIBUTES, FILE_NOTIFY_CHANGE_SIZE,
    FILE_NOTIFY_CHANGE_LAST_WRITE, FILE_NOTIFY_CHANGE_LAST_ACCESS,
    FILE_NOTIFY_CHANGE_CREATION, FILE_NOTIFY_CHANGE_SECURITY);
var
  Filters: DWORD;
  I: TFileChangeFilter;
  Subtree: Integer;
begin
  FLastError := ERROR_SUCCESS;
  Filters := 0;
  for I := Low(TFileChangeFilter) to High(TFileChangeFilter) do
    if I in Filter then
      Filters := Filters or NotifyFilters[I];
  if WatchSubtree then
    Subtree := 1
  else
    Subtree := 0;
  FNotifyHandle := FindFirstChangeNotification(PChar(FolderName),
    Bool(Subtree), Filters);
  if FNotifyHandle <> INVALID_HANDLE_VALUE then
    FEvent := CreateEvent(nil, BOOL(1), BOOL(0), nil)
  else
    FLastError := GetLastError;
  inherited Create(False);
end;

destructor TNotifyThread.Destroy;
begin
  FOnChange := nil;
  StopWaiting;
  inherited Destroy;
end;

procedure TNotifyThread.Terminate;
begin
  inherited Terminate;
  StopWaiting;
end;

procedure TNotifyThread.CallOnChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TNotifyThread.DoChange;
begin
  if Assigned(FOnChange) then
    Synchronize(CallOnChange);
end;

procedure TNotifyThread.DoTerminate;
begin
  if FNotifyHandle <> INVALID_HANDLE_VALUE then
    FindCloseChangeNotification(FNotifyHandle);
  FNotifyHandle := INVALID_HANDLE_VALUE;
  if FEvent <> 0 then
    CloseHandle(FEvent);
  FEvent := 0;
  inherited DoTerminate;
end;

procedure TNotifyThread.Execute;
var
  Handles: array[0..1] of THandle;
begin
  while not Terminated and (FNotifyHandle <> INVALID_HANDLE_VALUE) do
  begin
    Handles[0] := FNotifyHandle;
    Handles[1] := FEvent;
    case WaitForMultipleObjects(2, PWOHandleArray(@Handles), False, INFINITE) of
      WAIT_OBJECT_0: { notification }
        if not Terminated then
        begin
          DoChange;
          if not FindNextChangeNotification(FNotifyHandle) then
          begin
            FLastError := GetLastError;
            Break;
          end;
        end;
      WAIT_OBJECT_0 + 1: { event is signaled }
        Break;
      WAIT_FAILED:
        begin
          FLastError := GetLastError;
          Break;
        end;
    end;
  end;
  FFinished := True;
end;

procedure TNotifyThread.StopWaiting;
begin
  if FEvent <> 0 then
    SetEvent(FEvent);
end;

function CreateNotifyThread(const FolderName: string; WatchSubtree: Boolean;
  Filter: TFileChangeFilters): TNotifyThread;
begin
  Result := TNotifyThread.Create(FolderName, WatchSubtree, Filter);
  try
    if Result.LastError <> ERROR_SUCCESS then
      RaiseWin32Error(Result.LastError);
  except
    Result.Free;
    raise;
  end;
end;

{ TRxFolderMonitor }

constructor TRxFolderMonitor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFilter := [fnFileName, fnDirName, fnSize, fnLastWrite];
  FMonitorSubtree := True;
end;

destructor TRxFolderMonitor.Destroy;
begin
  if FDelayTimer <> nil then
    FDelayTimer.OnTimer := nil;
  FreeNotifyThread;
  FDelayTimer.Free;
  inherited Destroy;
end;

procedure TRxFolderMonitor.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedActive then
      Active := True;
  except
    if csDesigning in ComponentState then
      Application.HandleException(Self)
    else raise;
  end;
end;

function TRxFolderMonitor.GetActive: Boolean;
begin
  Result := FNotifyThread <> nil;
end;

procedure TRxFolderMonitor.SetActive(Value: Boolean);
begin
  if (csReading in ComponentState) then
  begin
    if Value then
      FStreamedActive := True;
  end
  else
  if Active <> Value then
    ResetNotifyThread(Value);
end;

procedure TRxFolderMonitor.SetFilter(Value: TFileChangeFilters);
var
  SaveFilter: TFileChangeFilters;
  IsActive: Boolean;
begin
  if FFilter <> Value then
  begin
    SaveFilter := FFilter;
    IsActive := Active;
    FFilter := Value;
    try
      ResetNotifyThread(IsActive);
    except
      FFilter := SaveFilter;
      if IsActive then
      try
        ResetNotifyThread(True);
      except
      end;
      raise;
    end;
  end;
end;

procedure TRxFolderMonitor.SetMonitorSubtree(Value: Boolean);
begin
  if FMonitorSubtree <> Value then
  begin
    FMonitorSubtree := Value;
    ResetNotifyThread(Active);
  end;
end;

procedure TRxFolderMonitor.SetFolderName(const Value: string);
begin
  if FFolderName <> Value then
  begin
    FFolderName := Value;
    ResetNotifyThread(Active);
  end;
end;

procedure TRxFolderMonitor.FreeNotifyThread;
begin
  if FNotifyThread <> nil then
    with FNotifyThread do
    begin
      OnChange := nil;
      if FFinished then
        Free
      else
      begin
        FreeOnTerminate := True;
        Terminate;
      end;
    end;
  FNotifyThread := nil;
end;

procedure TRxFolderMonitor.ResetNotifyThread(Activate: Boolean);
begin
  FreeNotifyThread;
  if Activate and DirExists(FFolderName) then
  begin
    FNotifyThread := CreateNotifyThread(FolderName, MonitorSubtree, Filter);
    FNotifyThread.OnChange := ThreadNotification;
  end;
end;

function TRxFolderMonitor.GetDelayTime: Cardinal;
begin
  if FDelayTimer <> nil then
    Result := FDelayTimer.Interval
  else
    Result := FDelayTime;
end;

procedure TRxFolderMonitor.SetDelayTime(Value: Cardinal);
begin
  if (FDelayTimer <> nil) then
  begin
    if Value > 0 then
      FDelayTimer.Interval := Value
    else
    begin
      FDelayTimer.OnTimer := nil;
      FDelayTimer.Free;
      FDelayTimer := nil;
    end;
  end;
  FDelayTime := Value;
end;

procedure TRxFolderMonitor.ThreadNotification(Sender: TObject);
begin
  if FDelayTime <= 0 then
    Changed
  else
  if FDelayTimer = nil then
  begin
    FDelayTimer := TTimer.Create(Self);
    with FDelayTimer do
    begin
      Interval := FDelayTime;
      OnTimer := Timer;
      Enabled := True;
    end;
  end;
end;

procedure TRxFolderMonitor.Timer(Sender: TObject);
begin
  FDelayTimer.Free;
  FDelayTimer := nil;
  Changed;
end;

procedure TRxFolderMonitor.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

end.
