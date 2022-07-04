{ Copyright (C) 1998-2006, written by Shkolnik Mike, Scalabium Software
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com
}
unit FileNotify;

interface

uses
  Windows, SysUtils, Classes;

type
  EFileNotificationError = class(Exception);

  TFileNotificationOption = (foFile, foFolder, foAttributes, foSize, foTime, foWatchSubFolders);
  TFileNotificationOptions = set of TFileNotificationOption;

  TFileNotifyEvent = procedure(Sender: TObject; Action: TFileNotificationOption) of object;

  TSMFileNotificator = class;

  TNotificationThread = class(TThread)
  private
    FOwner: TSMFileNotificator;
    FHandles: array[0..7] of THandle;
    FHandleFilters: array[0..7] of TFileNotificationOption;
    FHandleCount: Integer;
    FActiveFilter: TFileNotificationOption;

    procedure ReleaseHandles;
    procedure AllocateHandles;
  protected
    procedure Execute; override;
    procedure Notify;
  public
    constructor Create(Owner: TSMFileNotificator);
    destructor Destroy; override;
    procedure Reset;
  end;

  TSMFileNotificator = class(TComponent)
  private
    FThread: TNotificationThread;
    FFolder: string;
    FOptions: TFileNotificationOptions;
    FOnFileNotification: TFileNotifyEvent;

    procedure SetOptions(Value: TFileNotificationOptions);
    procedure SetFolder(Value: string);
  protected
    procedure Notify(Action: TFileNotificationOption); virtual;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TCOmponent); override;
    destructor Destroy; override;
  published
    property Folder: string read FFolder write SetFolder;
    property Options: TFileNotificationOptions read FOptions write SetOptions;
    property OnFileNotification: TFileNotifyEvent read FOnFileNotification write FOnFileNotification;
  end;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SMComponents', [TSMFileNotificator]);
end;


const
  SErrInstall = 'Could not install notification callback';
  SErrAllocHandle = 'Could not allocate notification object';


{ TNotificationThread }
const
  HANDLE_FILE = 0;
  HANDLE_DIR  = 1;
  HANDLE_ATTR = 2;
  HANDLE_SIZE = 3;
  HANDLE_TIME = 4;

constructor TNotificationThread.Create(Owner: TSMFileNotificator);
begin
  inherited Create(True);

  FOwner := Owner;
  FreeOnTerminate := False;
  Reset;
  Resume;
end;

destructor TNotificationThread.Destroy;
begin
  ReleaseHandles;

  inherited Destroy;
end;

procedure TNotificationThread.AllocateHandles;

  procedure SetNotification(FileNotificationOption: TFileNotificationOption);
  const
    ANotifyFilter: array[foFile..foTime] of Integer =
      (FILE_NOTIFY_CHANGE_FILE_NAME,
       FILE_NOTIFY_CHANGE_DIR_NAME,
       FILE_NOTIFY_CHANGE_ATTRIBUTES,
       FILE_NOTIFY_CHANGE_SIZE,
       FILE_NOTIFY_CHANGE_LAST_WRITE);
  begin
    FHandleFilters[FHandleCount] := FileNotificationOption;
    FHandles[FHandleCount] := FindFirstChangeNotification(PChar(FOwner.Folder), (foWatchSubFolders in FOwner.FOptions), ANotifyFilter[FileNotificationOption]);
    Inc(FHandleCount);
    if FHandles[FHandleCount-1] = INVALID_HANDLE_VALUE then
      raise EFileNotificationError.Create(SErrAllocHandle);
  end;

begin
  if (FOwner <> nil) and (FOwner.FOptions <> []) then
    try
      FillChar(FHandles, SizeOf(FHandles), 0);
      FHandleCount := 0;
      if (foFile in FOwner.FOptions) then
        SetNotification(foFile);
      if (foFolder in FOwner.FOptions) then
        SetNotification(foFolder);
      if (foAttributes in FOwner.FOptions) then
        SetNotification(foAttributes);
      if (foSize in FOwner.FOptions) then
        SetNotification(foSize);
      if (foTime in FOwner.FOptions) then
        SetNotification(foTime);
    except
      ReleaseHandles;
      raise;
    end;
end;

procedure TNotificationThread.ReleaseHandles;
var i: Integer;
begin
  for i := 0 to FHandleCount-1 do
    if FHandles[i] <> 0 then
      FindCloseChangeNotification(FHandles[i]);
  FillChar(FHandles, SizeOf(FHandles), 0);
  FillChar(FHandleFilters, Sizeof(FHandleFilters), 0);
  FHandleCount := 0;
end;

procedure TNotificationThread.Reset;
begin
  ReleaseHandles;
  AllocateHandles;
end;

procedure TNotificationThread.Execute;
var i, j: Integer;
begin
  while not Terminated do
  begin
    if FHandleCount > 0 then
    begin
      j := WaitForMultipleObjects(FHandleCount, @FHandles, False, 200);
      for i := 0 to FHandleCount-1 do
        if j = (WAIT_OBJECT_0 + i) then
        begin
          FActiveFilter := FHandleFilters[i];
          Synchronize(Notify);
          FindNextChangeNotification(FHandles[i]);
          break;
        end;
    end;
//    else
//      Sleep(0);
    Sleep(1);
  end
end;

procedure TNotificationThread.Notify;
begin
  FOwner.Notify(FActiveFilter);
end;


{ TSMFileNotificator }
constructor TSMFileNotificator.Create(AOwner: TCOmponent);
begin
  inherited Create(AOwner);

  FOptions := [foFile, foFolder];
end;

destructor TSMFileNotificator.Destroy;
begin
  if Assigned(FThread) then
  begin
    FThread.Free;
    FThread := nil;
  end;

  inherited Destroy;
end;

procedure TSMFileNotificator.Loaded;
begin
  inherited Loaded;

  if not (csDesigning in ComponentState) then
    FThread := TNotificationThread.Create(Self);
end;

procedure TSMFileNotificator.Notify(Action: TFileNotificationOption);
begin
  if Assigned(OnFileNotification) then
    OnFileNotification(Self, Action);
end;

procedure TSMFileNotificator.SetOptions(Value: TFileNotificationOptions);
begin
  if Value <> Options then
  begin
    FOptions := Value;
    if Assigned(FThread) then
      FThread.Reset;
  end;
end;

procedure TSMFileNotificator.SetFolder(Value: string);
begin
  if (Value <> FFolder) then
  begin
    FFolder := Value;
    if Assigned(FThread) then
      FThread.Reset;
  end;
end;

end.
