{*******************************************************}
{                                                       }
{       Borland Delphi Visual Component Library         }
{                                                       }
{       Copyright (c) 1997,99 Inprise Corporation       }
{                                                       }
{*******************************************************}

unit FxPBar;

{ the progress dialog with a cancel button }

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, FxCommon;

type
  EUserCanceled = class(Exception);

  TFxProgressDialog = class(TForm)
    ProgressBar: TProgressBar;
    CancelButton: TButton;
    labStatus: TStaticText;
    labCount: TLabel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FCanceled: Boolean;
    FInterval: Integer;
    FCount: Integer;
    FRealMax: Integer;
    FBuilding: Boolean;
    BuildDone: Boolean;
    FExceptObj:TObject;
    FOnPerformBuild: TNotifyEvent;
    function GetMax: Integer;
    function StepProgress: Boolean;
    procedure SetMax(Value: Integer);
    procedure SetInterval(Value: Integer);
    procedure StartBuild(var Message); message WM_USER;
  public
    function UpdateCounter(const Value:Integer):Boolean;
    function UpdateProgress: Boolean;
    procedure Reset;
    procedure ShowStatus(Msg: string);
    procedure StartCounter(const AStatus:string);
    property Max: Integer read GetMax write SetMax;
    property Canceled: Boolean read FCanceled write FCanceled;
    property Interval: Integer read FInterval write SetInterval;
    property OnPerformBuild: TNotifyEvent read FOnPerformBuild write FOnPerformBuild;
  end;

  TFxProgress=class(TInterfacedObject,IFxProgress)
  private
    FDialog:TFxProgressDialog;
    function GetCancel:Boolean;
    procedure SetText(const Value:WideString);
    procedure StartProgress(Max:Integer);
    procedure FinishProgress;
    procedure UpdateProgress;
  public
    constructor Create;
    destructor Destroy;override;
  end;


var
  ProgressDlg: TFxProgressDialog = nil;

procedure ExecuteProgressDialog(V:TNotifyEvent);
procedure StartProgress(const ACaption:string; Count:Integer);
procedure UpdateProgress;
procedure StartCounter(const AStatus:string);
procedure UpdateCounter(const Value:Integer);

implementation

uses FxConsts,FxStore;

{$R *.dfm}

procedure ExecuteProgressDialog(V:TNotifyEvent);
begin
  ProgressDlg:=TFxProgressDialog.Create(Application);
  with ProgressDlg do begin
    try
      Caption:=sBuildingDataStore;
      OnPerformBuild:=V;
      ShowModal;
      if FExceptObj<>nil then
         raise (FExceptObj as Exception);
    finally
      Free;
      ProgressDlg:=nil;
    end;
  end;
end;

procedure StartProgress(const ACaption:string; Count:Integer);
begin
  if Assigned(ProgressDlg) then begin
    ProgressDlg.ProgressBar.Visible:=True;
    ProgressDlg.labCount.Visible:=False;
    ProgressDlg.ShowStatus(ACaption);
    ProgressDlg.Max    := Count;
  end;
end;

procedure UpdateProgress;
begin
  if Assigned(ProgressDlg) then begin
    if not ProgressDlg.UpdateProgress then
      raise EUserCanceled.Create(sUserCanceled);
  end;
end;

procedure StartCounter(const AStatus:string);
begin
  if Assigned(ProgressDlg)then
    ProgressDlg.StartCounter(AStatus);
end;

procedure UpdateCounter(const Value:Integer);
begin
  if Assigned(ProgressDlg)then
    if not ProgressDlg.UpdateCounter(Value)then
      raise EUserCanceled.Create(sUserCanceled);
end;

procedure TFxProgressDialog.StartBuild(var Message);
begin
  try
    FBuilding := True;
    if Assigned(FOnPerformBuild) then begin
      try
        FOnPerformBuild(Self);
      except
        FExceptObj:=AcquireExceptionObject;
      end;
      ModalResult := mrCancel;
    end;
  finally
    Self.Visible := False;
    BuildDone := True;
    FBuilding := False;
    Canceled  := True;
  end;
end;

function TFxProgressDialog.UpdateProgress: Boolean;
begin
  StepProgress;
  Application.ProcessMessages;
  Result:= not Canceled;
end;

function TFxProgressDialog.GetMax: Integer;
begin
  Result := FRealMax;
end;

procedure TFxProgressDialog.SetMax(Value: Integer);
begin
  if Value<>FRealMax then begin
    FRealMax := Value;
    ProgressBar.Max := Value;
    ProgressBar.Position := 1;
    if Value>10000 then
      Interval := Integer(Trunc(Value * 0.05))
    else if (Value > 100) then
      Interval := Integer(Trunc(Value * 0.10))
    else
      Interval := 1;
  end;
end;

function TFxProgressDialog.StepProgress: Boolean;
begin
  Result := False;
  if (FCount = FInterval) and (ProgressBar.Max > 0) then
  begin
    if not Visible then Visible := True;
    ProgressBar.StepIt;
    FCount := 0;
    Result := True;
  end;
  Inc(FCount);
end;

procedure TFxProgressDialog.ShowStatus(Msg: string);
begin
  labStatus.Visible:=True;
  labStatus.Caption:=msg;
  Application.ProcessMessages;
end;

procedure TFxProgressDialog.CancelButtonClick(Sender: TObject);
begin
  Canceled := True;
  if BuildDone and (CancelButton.ModalResult <> mrOK) then
    ModalResult := mrCancel;
end;

procedure TFxProgressDialog.SetInterval(Value: Integer);
begin
  if (Value <> FInterval) then
  begin
    { set the new max based on the interval }
    FInterval := Value;
    FCount := 1;
    ProgressBar.Step := Value;
  end;
end;

procedure TFxProgressDialog.Reset;
begin
  ProgressBar.Position := 1;
end;

procedure TFxProgressDialog.FormActivate(Sender: TObject);
begin
  if not FBuilding then
    PostMessage(Handle, WM_USER, 0, 0);
end;

procedure TFxProgressDialog.FormCreate(Sender: TObject);
begin
  SetBounds((Screen.Width - Width) div 2,
           (GetSystemMetrics(SM_CYSCREEN) - Height) div 3,
           Width, Height);
end;

{ TFxProgress }

constructor TFxProgress.Create;
begin
  inherited Create;
  FDialog:=TFxProgressDialog.Create(Application);
end;

destructor TFxProgress.Destroy;
begin
  FDialog.Free;
  inherited;
end;

procedure TFxProgress.FinishProgress;
begin

end;

function TFxProgress.GetCancel: Boolean;
begin
  Result:=FDialog.FCanceled;
end;

procedure TFxProgress.SetText(const Value: WideString);
begin
  FDialog.labStatus.Caption:=Value;
end;

procedure TFxProgress.StartProgress(Max: Integer);
begin
  FDialog.ProgressBar.Max:=Max;
  FDialog.ShowModal;
end;

procedure TFxProgress.UpdateProgress;
begin
  FDialog.ProgressBar.StepIt;
end;

procedure TFxProgressDialog.StartCounter(const AStatus:string);
begin
  ProgressBar.Visible:=False;
  labStatus.Visible:=True;
  labStatus.Caption:=AStatus;
  labCount.Visible:=True;
  labCount.Caption:='0';
  Application.ProcessMessages;
end;

function TFxProgressDialog.UpdateCounter(const Value: Integer): Boolean;
begin
  labCount.Caption:=IntToStr(Value);
  Application.ProcessMessages;
  Result:= not Canceled;
end;

end.
