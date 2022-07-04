{
Professional Screen Camera Component (Delphi 7 to above)
Developed 2008 by Mohammad Reza Hanifeh Pour (MRH Software Co.)
Author E-Mail: mrh.info2007@gmail.com
Centeral Office Tel: +98(21)(7764)(4130).   Everyday 9AM ~ 4PM.
Office Address: F2 29 Rezai St. Namjo Av. Tehran-Iran.
................................................................................
Version history:

v4.7.1.0: Updated 01/01/2009
    New features:
      1) Add unit hightimar.pas for a threaded timer in preview or recording.
      2) Add canvas.trylock and canvas.unlock for all parts of image processing.
      3) Included all necessary units of Wave Audio Package and TEffect in my component. 
    Modify features:
      1) Fixed some routines in function PowerDeleteFile, Because long time waiting for deleting a file.
    Remove features:
      No thing
 
v4.4.1.1: Updated 12/11/2008
    New features:
      1) Screen Camera Unit converted to component packege (Delphi 7 to above)
      2) Add info frame rate to preview routine
    Modify features:
      1) Replaced PreviewScreenFrame routine with CaptureScreenFrame routine in preview mode
    Remove features:
      1) Delete PreviewScreenFrame routine, Because between record and preview
         eventuate to memory stack overflow

v4.2.2.1: Updated 12/03/2008
    New features:
      1) Add recording from multi monitor
      2) Add Noise effect to image effects
    Modify features:
      1) Fixed some errors
      2) Fixed memory overflow in low frame rate
    Remove features:
      1) Remove solarize filter effect from image effects

v4.0.1.0: Updated 11/18/2008
    New features:
      1) Add grayscale drawing (Capture And Preview)
      2) Add some image effects (Rotation, Brightness, Contrast, Color Adjusting, Saturation, Solarize)
    Modify features:
      1) Fixed some errors
    Remove features:
      No thing

v3.8.2.0: Updated 04/03/2008
    New features:
      No thing
    Modify features:
      1) Fixed error on selecting audio input.
    Remove features:
      No thing

v3.8.1.0: Updated 03/18/2008
    New features:
      1) Add overlay event for draw objects, picture, text and more over image.
      2) Add deleting event.
      3) Add correct frame rate info.
    Modify features:
      1) correction elapsed timer.
    Remove features:
      No thing

v3.5.3.2: Updated 03/07/2008
    New features:
      No thing
    Modify features:
      1) Canceling select region from object and windows on start record, that correct.
      2) Not synchronized record time with play time in full auto mode, that correct.
      3) Corrected some internal errors.
    Remove features:
      1) Remove capture timer and elapsed timer and add into record routin.
      2) Remove sleep timer on record (For full motion).

v3.5.0.1: Updated 02/28/2008
    New features:
      1) Upper interval TTimer (Because, sometimes system error).
      2) Lower sleep on upper frame rate during record (Softer motion).
      3) Not delete already temp audio/video files from temp directory, But can now.
      4) Add freehand window for free select region.
      5) Add select object window for select region from object or
          windows under mouse pointer.
    Modify features:
      No thing
    Remove features:
      1) Remove recompressing after record (Because, Some codecs, more the size of file).

v3.0.0.0: Released 11/20/2007
    First release.
................................................................................
}

unit HighTimer;

interface

uses
  Windows,
  Classes,
  Messages,
  MMSystem;

type
  THighTimer = class;
  TSCTimerInterval = 1..High(Word);
  {------------------------------- TSCThreadedTimer ---------------------------}
  TSCThreadedTimer = class(TThread)
  private
    FTimer: THighTimer;
    FInterval: TSCTimerInterval;
  protected
    procedure Execute; override;
  public
    property Interval: TSCTimerInterval read FInterval write FInterval;
    constructor Create(aOwner: THighTimer);
  end;

  THighTimer = class(TComponent)
  private
    FSynchronize,
    FUseThread,
    FEnabled,
    FInternalFlag   : Boolean;
    FInterval,
    FTimerID        : UINT;
    FTimerCaps      : TTimeCaps;
    FOnTimer        : TNotifyEvent;
    FWindowHandle   : HWND;
    FThread         : TSCThreadedTimer;
    FThreadPriority : TThreadPriority;
    procedure SetThreadPriority(aValue: TThreadPriority);
    procedure SetUseThread(aValue: Boolean);
    procedure SetEnabled(aValue: Boolean);
    procedure SetInterval(aValue: UINT);
    procedure SetOnTimer(aValue: TNotifyEvent);
    procedure WndProc(var Msg: TMessage);
    procedure UpdateTimer;
    function GetCaps: TTimeCaps;
    function GetInteger(index: Integer):Integer;
  protected
    procedure Timer; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateExt(aOwner: TComponent; Interval: TSCTimerInterval);
    destructor Destroy; override;
    property MaxInterval    : Integer index 0 read GetInteger;
    property MinInterval    : Integer index 1 read GetInteger;
  published
    property Enabled        : Boolean         read FEnabled        write SetEnabled default False;
    property Interval       : UINT            read FInterval       write SetInterval default 1000;
    property OnTimer        : TNotifyEvent    read FOnTimer        write SetOnTimer;
    property Synchronize    : Boolean         read FSynchronize    write FSynchronize default True;
    property UseThread      : Boolean         read FUseThread      write SetUseThread default True;
    property ThreadPriority : TThreadPriority read FThreadPriority write SetThreadPriority default tpNormal;
  end;


implementation

uses
  Forms,
  SysUtils,
  Consts;

{------------------------------------------------------------------------------}
procedure TimeCallBack(uTimerID, uMessage: UINT; dwUser, dw1, dw2: DWORD); stdcall;
begin
  PostMessage(HWND(dwUser), WM_TIMER, uTimerID, 0);
end;

{------------------------------------------------------------------------------}
procedure TSCThreadedTimer.Execute;

  function ThreadClosed: Boolean;
  begin
    Result := Terminated or Application.Terminated or (FTimer = nil);
  end;

begin
  repeat
    if not ThreadClosed then
      if SleepEx(FInterval, False) = 0 then
        begin
          if not ThreadClosed and FTimer.FEnabled then
            with FTimer do
              if FSynchronize then
                Self.Synchronize(Timer)
              else
                Timer;
      end;
  until Terminated;
end;

{------------------------------------------------------------------------------}
constructor TSCThreadedTimer.Create(aOwner: THighTimer);
begin
  inherited Create(True);
  FTimer    := aOwner;
  FInterval := 1000;
  // free Thread after Termination
  FreeOnTerminate := True;
end;

{*************************** Class THighTimer**********************************}
{----------------------------  Private ----------------------------------------}
procedure THighTimer.SetEnabled(aValue: Boolean);
begin
 if aValue <> FEnabled then
   begin
     FEnabled := aValue;
     UpdateTimer;
   end;
end;

{------------------------------------------------------------------------------}
procedure THighTimer.SetInterval(aValue: UINT);
begin
 if aValue <> FInterval then
   begin
     if (aValue < FTimerCaps.wPeriodMin) then
       FInterval := FTimerCaps.wPeriodMin
     else
       if (aValue > FTimerCaps.wPeriodMax) then
         FInterval := FTimerCaps.wPeriodMax
       else
         FInterval := aValue;
     UpdateTimer;
   end;
end;

{------------------------------------------------------------------------------}
procedure THighTimer.SetOnTimer(aValue: TNotifyEvent);
begin
  FOnTimer := aValue;
  UpdateTimer;
end;

{------------------------------------------------------------------------------}
procedure THighTimer.SetUseThread(aValue: Boolean);
begin
  if aValue <> FUseThread then
    begin
      FUseThread := aValue;
      UpdateTimer;
    end;
end;

{------------------------------------------------------------------------------}
procedure THighTimer.SetThreadPriority(aValue: TThreadPriority);
begin
  if aValue <> FThreadPriority then
    begin
      FThreadPriority := aValue;
      if FUseThread then UpdateTimer;
    end;
end;

{------------------------------------------------------------------------------}
function THighTimer.GetCaps: TTimeCaps;
var
  Temp: TTimeCaps;
begin
  TimeGetDevCaps(@Temp, 2 * SizeOf(UINT));
  Result := Temp;
end;

{------------------------------------------------------------------------------}
function THighTimer.GetInteger(index: Integer): Integer;
begin
  Result := 0;
  case Index of
    0: Result := FTimerCaps.wPeriodMax;
    1: Result := FTimerCaps.wPeriodMin;
  end;
end;

{------------------------------------------------------------------------------}
procedure THighTimer.UpdateTimer;
begin
  if csDesigning in ComponentState then Exit;
  if FThread <> nil then
    FThread.Terminate;
  {if one is active - delete it!}
  if FTimerID <> 0 then TimeKillEvent(FTimerID);
  if csDestroying in ComponentState then exit;
  if FUseThread then
    begin
      if FWindowHandle <> 0 then
        begin
          DeallocateHWnd(FWindowHandle);
          FWindowHandle := 0;
        end;
      if FEnabled and Assigned(FOnTimer) then
        begin
          FThread := TSCThreadedTimer.Create(Self);
          FThread.FInterval := FInterval;
          FThread.Priority  := FThreadPriority;
          while FThread.Suspended do
            FThread.Resume;
        end;
    end
  else
    begin
      if FWindowHandle = 0 then
        FWindowHandle := AllocateHWnd(WndProc);
      if (FInterval <> 0) and FEnabled and Assigned(FOnTimer) then
        begin
          FTimerID := TimeSetEvent(FInterval,
                                   FInterval,
                                   TimeCallBack,
                                   FWindowHandle,
                                   TIME_PERIODIC);
          FInternalFlag := False;
          If FTimerId = 0 then
            raise EOutOfResources.Create(SNoTimers);
        end;
    end;
end;

{------------------------------------------------------------------------------}
procedure THighTimer.WndProc(var Msg: TMessage);
begin
  with Msg do
    if (Msg = WM_TIMER) and (DWORD(WPARAM) = FTimerID) then
      try
        if FInternalFlag then exit;
        FInternalFlag := True;
        Timer;
        FInternalFlag := False;
        Result := 0;
      except
        Application.HandleException(Self);
      end
    else
      Result := DefWindowProc(FWindowHandle, Msg, wParam, lParam);
end;

{--------------------- Protected ----------------------------------------------}
procedure THighTimer.Timer;
begin
  if Assigned(FOnTimer) then FOnTimer(Self);
end;

{---------------------Public---------------------------------------------------}
constructor THighTimer.Create(aOwner: TComponent);
begin
  CreateExt(aOwner, 1000);
end;

{------------------------------------------------------------------------------}
constructor THighTimer.CreateExt(aOwner: TComponent; Interval: TSCTimerInterval);
begin
  inherited Create(aOwner);
  FEnabled        := False;
  FInternalFlag   := False;
  FTimerCaps      := GetCaps;
  TimeBeginPeriod(FTimerCaps.wPeriodMin);
  FInterval       := Interval;
  FWindowHandle   := 0;
  FSynchronize    := True;
  FUseThread      := True;
  FThreadPriority := tpNormal;
  FThread         := nil;
end;

{------------------------------------------------------------------------------}
Destructor THighTimer.Destroy;
begin
  FOnTimer := nil;
  if FThread <> nil then
    UpdateTimer;
  TimeEndPeriod(FTimerCaps.wPeriodMin);
  FEnabled := False;
  if FWindowHandle <> 0 then
    DeallocateHWnd(FWindowHandle);
  inherited Destroy;
end;

end.
