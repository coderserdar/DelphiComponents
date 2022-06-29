unit BroadcasterUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TScope = (bsLocal, bsRemote, bsDomain, bsCurrentDomain);

  TBroadcaster = class; { Forward declaration. }

  TListenerThread = class(TThread)
  private
    FBroadcaster: TBroadcaster;
    FMailHandle: THandle;
    procedure SetMailHandle(const Value: THandle);
  protected
    procedure Execute; override;
    procedure NotifyBroadcaster;
  public
    property Broadcaster :TBroadcaster read FBroadcaster write FBroadcaster;
    property MailHandle :THandle read FMailHandle write SetMailHandle;
  end;

  TBroadcaster = class(TComponent)
  private
    { Private declarations }
    FTimeOut: Boolean;
    FActive: Boolean;
    FMaxMessageSize: Integer;
    FTimeOutMs: Longint;
    FDomainName: string;
    FComputerName: string;
    FScope: TScope;
    FMailslot :THandle;
    FFile :THandle;
    FSlotID: string;
    FSendOut: Boolean;
    FListenerThread :TListenerThread;
    FOnReceived: TNotifyEvent;
    procedure SetActive(const Value: Boolean);
    function GetMailslotName: string;
    function GetLocalMailslotName: string;
  protected
    { Protected declarations }
    procedure Loaded; override;
    procedure MessageReceived;
  public
    { Public declarations }
    constructor Create(AOwner :TComponent); override;
    destructor Destroy; override;
    procedure WriteText(const Text :string);
    function ReadText :string;
    property MailslotName :string read GetMailslotName;
    property Text :string read ReadText write WriteText stored False;
  published
    { Published declarations }
    property MaxMessageSize :Integer read FMaxMessageSize write FMaxMessageSize default 0;
    property TimeOut :Boolean read FTimeOut write FTimeOut default False;
    property TimeOutMs :Longint read FTimeOutMs write FTimeOutMs default 0;
    property Active :Boolean read FActive write SetActive default False;
    property ComputerName :string read FComputerName write FComputerName;
    property DomainName :string read FDomainName write FDomainName;
    property SlotID :string read FSlotID write FSlotID;
    property Scope :TScope read FScope write FScope;
    property SendOut :Boolean read FSendOut write FSendOut default True;
    property OnReceived :TNotifyEvent read FOnReceived write FOnReceived;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('MLR Sysco', [TBroadcaster]);
end;

{ TBroadcaster }

constructor TBroadcaster.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := False;
  FTimeOutMs := 0;
  FSendOut := True;
end;

destructor TBroadcaster.Destroy;
begin
  Active := False;
  inherited Destroy;
end;

function TBroadcaster.GetLocalMailslotName: string;
begin
  Result := '\\.\mailslot\' + FSlotID;
end;

function TBroadcaster.GetMailslotName: string;
begin
  Result := '\\';
  case FScope of
    bsLocal:          Result := Result + '.\';
    bsRemote:         Result := Result + FComputerName + '\';
    bsDomain:         Result := Result + FDomainName + '\';
    bsCurrentDomain:  Result := Result + '*\';
  end;
  Result := Result + 'mailslot\' + FSlotID;
end;

procedure TBroadcaster.Loaded;
begin
  if FActive then begin
    FActive := False;
    Active := True;
  end;
end;

procedure TBroadcaster.MessageReceived;
begin
  if Assigned(FOnReceived) then
    FOnReceived(Self);
end;

function TBroadcaster.ReadText: string;
var
  NextSize :Cardinal;
  MessageCount :Cardinal;
  ReadTimeout :Pointer;
begin
  Result := '';
  if not FActive then exit;
  if FTimeOut then ReadTimeout := @FTimeOutMs else ReadTimeout := nil;
  GetMailslotInfo(FMailslot, nil, NextSize, @MessageCount, ReadTimeout);
  if MessageCount = 0 then exit;
  SetLength(Result, NextSize);
  if not ReadFile(FMailslot, PChar(Result)^, NextSize, NextSize, nil) then begin
    if NextSize = 0 then begin
      SetFilePointer(FFile, 0,  nil, FILE_BEGIN);
      if not ReadFile(FFile, PChar(Result)^, NextSize, NextSize, nil) then
        RaiseLastWin32Error;
    end else
      RaiseLastWin32Error;
  end;
end;

procedure TBroadcaster.SetActive(const Value: Boolean);
var ReadTimeout :Cardinal;
begin
  if FActive = Value then exit;
  if csReading in ComponentState then exit;
  if Value then begin
    if FTimeOut then ReadTimeOut := FTimeOutMs
      else ReadTimeOut := MAILSLOT_WAIT_FOREVER;
    if not FSendOut then begin
      FMailslot := CreateMailslot(PChar(GetLocalMailslotName),
        FMaxMessageSize, ReadTimeout, nil);
      if FMailslot = INVALID_HANDLE_VALUE then
        RaiseLastWin32Error;
      FListenerThread := TListenerThread.Create(True);
      FListenerThread.MailHandle := FMailslot;
      FListenerThread.Broadcaster := Self;
      FListenerThread.Priority := tpLower;
      FListenerThread.FreeOnTerminate := True;
      FListenerThread.Resume;
    end else begin
      FFile := CreateFile(PChar(GetMailslotName), GENERIC_WRITE, FILE_SHARE_READ,
        nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0)
    end;
    if FFile = INVALID_HANDLE_VALUE then RaiseLastWin32Error;
  end else begin
    if Assigned(FListenerThread) then begin
      FListenerThread.Terminate;
      FListenerThread := nil;
    end;
    if FFile <> 0 then begin
      CloseHandle(FFile);
      FFile := 0;
    end;
    if FMailslot <> 0 then begin
      CloseHandle(FMailslot);
      FMailslot := 0;
    end;
  end;
  FActive := Value;
end;

procedure TBroadcaster.WriteText(const Text: string);
var BytesWritten :Cardinal; Len :Integer;
begin
  if not FActive then
    raise Exception.Create('Cannot write to an closed broadcaster');
  Len := Length(Text);
  WriteFile(FFile, PChar(Text)^, Len, BytesWritten, nil);
  if Cardinal(Len) <> BytesWritten then
    RaiseLastWin32Error;
end;

{ TListenerThread }

procedure TListenerThread.Execute;
var NextSize, ReadTimeout :Cardinal;
begin
  ReadTimeout := 2 * 100;
  repeat
    { WaitForSingleObject(FMailHandle, 2 * 100); }
    { Get mailslot information. }
    if GetMailslotInfo(FMailHandle, nil, NextSize, nil, @ReadTimeout) and
       (NextSize <> MAILSLOT_NO_MESSAGE) then begin
      Synchronize(NotifyBroadcaster);
    end;
  until Terminated;
  CloseHandle(FMailHandle);
end;

procedure TListenerThread.NotifyBroadcaster;
begin
  if Assigned(FBroadcaster) then
    FBroadcaster.MessageReceived;
end;

procedure TListenerThread.SetMailHandle(const Value: THandle);
begin
  DuplicateHandle(GetCurrentProcess, Value, GetCurrentProcess,
    @FMailHandle, 0, False, DUPLICATE_SAME_ACCESS);
end;

end.
