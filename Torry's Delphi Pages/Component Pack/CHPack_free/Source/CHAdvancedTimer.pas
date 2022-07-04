unit CHAdvancedTimer;

{ ##############################################################################
  TCHAdvancedTimer

  Version   		:   1.1.1
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)

  History:
  1.0.0 - 01.11.2002    - First Release
  1.1.0 - 10.01.2003    - better error-handling
  1.1.1 - 09.03.2003    - reorganize "uses" for more performance and less memory needed


  ############################################################################ }

interface

uses
  Windows, Forms, SysUtils, Classes;


type
  TTimerResolution = (trSecond, trMillisecond, trMicrosecond);
  TTimerMode = (timHighSolution, timCompatible);

  TCHAdvancedTimer = class(TComponent)
  private
    FEnabled: Boolean;
    FInterval: Cardinal;
    FOnTimer: TNotifyEvent;
    FFreq : Int64;
    FCountStart : Int64;
    FCountStop: TLargeInteger;
    FOnStopTimer: TNotifyEvent;
    FTimerMode: TTimerMode;
    FTimerResolution: TTimerResolution;
    procedure InitCounter;
    function GetPerformanceCounter : Cardinal;
    procedure StartTimer(Interval : Cardinal);
    procedure DoTimer;
    procedure StopTimer;
    procedure SetEnabled(const Value: Boolean);
    procedure SetInterval(const Value: Cardinal);
    procedure SetTimerResolution(const Value: TTimerResolution);
    procedure SetTimerMode(const Value: TTimerMode);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnTimer : TNotifyEvent read FOnTimer write FOnTimer;
    property OnStopTimer : TNotifyEvent read FOnStopTimer write FOnStopTimer;
    property Enabled : Boolean read FEnabled Write SetEnabled;
    property Resolution : TTimerResolution read FTimerResolution Write SetTimerResolution;
    property Interval : Cardinal read FInterval Write SetInterval;
    property Mode : TTimerMode read FTimerMode Write SetTimerMode;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHAdvancedTimer]);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHAdvancedTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := False;
  FInterval := 1000000;
  FTimerResolution := trMicrosecond;
  FTimerMode := timHighSolution;
  InitCounter;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHAdvancedTimer.Destroy;
begin
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHAdvancedTimer.DoTimer;
begin
  try
    FOnTimer(Self);
  except
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHAdvancedTimer.StopTimer;
begin
  if Assigned(FOnStopTimer) then
    FOnStopTimer(self);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHAdvancedTimer.InitCounter;
begin
  QueryPerformanceFrequency(FFreq);
  QueryPerformanceCounter(FCountStart);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHAdvancedTimer.StartTimer(Interval: Cardinal);
var
  nCountUpd : Cardinal;
begin
  nCountUpd := GetPerformanceCounter;
  while FEnabled do
  begin
    if (nCountUpd + Interval > GetPerformanceCounter) then
    begin
      if FTimerMode = timCompatible then
        Sleep(1);
      Application.ProcessMessages;
    end
    else
    begin
      DoTimer;
      nCountUpd := GetPerformanceCounter;
    end;
  end;
  StopTimer;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
function TCHAdvancedTimer.GetPerformanceCounter: Cardinal;
begin
  QueryPerformanceCounter(FCountStop);
  if FTimerResolution = trMicrosecond then
    Result := Trunc((FCountStop - FCountStart) / FFreq * 1000000)
  else if FTimerResolution = trMillisecond then
    Result := Trunc((FCountStop - FCountStart) / FFreq * 1000)
  else
    Result := Trunc((FCountStop - FCountStart) / FFreq);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHAdvancedTimer.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
  begin
    FEnabled := Value;
    if FEnabled and Assigned(FOnTimer) and not (csDestroying in ComponentState) then
    begin
      StartTimer(FInterval);
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHAdvancedTimer.SetInterval(const Value: Cardinal);
begin
  if FInterval <> Value then
    FInterval := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHAdvancedTimer.SetTimerMode(const Value: TTimerMode);
begin
  if FTimerMode <> Value then
  begin
    FTimerMode := Value;
    if FTimerMode = timCompatible then
    begin
      if FTimerResolution = trMicrosecond then
      begin
        FInterval := FInterval div 1000;
        if FInterval < 1 then
          FInterval := 1;
        FTimerResolution := trMillisecond;
      end;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHAdvancedTimer.SetTimerResolution( const Value: TTimerResolution);
begin
  if FTimerResolution <> Value then
    FTimerResolution := Value;
end;

end.
