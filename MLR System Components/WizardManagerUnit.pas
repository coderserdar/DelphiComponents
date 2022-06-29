unit WizardManagerUnit;

interface

uses
  ExtCtrls, StdCtrls, Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs;

type
  TWizardStage = class;
  TWizardPlan = class;
  TWizardManager = class;
  TWizardStageName = string;
  TMovingDirection = (mdNone, mdPrevious, mdNext);

  TWizardStage = class(TCollectionItem)
  private
    FCaption: string;
    FOnLeave: TNotifyEvent;
    FOnEnter: TNotifyEvent;
    FWinControl: TWinControl;
    FNextStage: TWizardStageName;
    FPreviousStage: TWizardStageName;
  protected
    function GetDisplayName: string; override;
    procedure Enter;
    procedure Leave;
  published
    property Caption :string read FCaption write FCaption;
    property WinControl :TWinControl read FWinControl write FWinControl;
    property PreviousStage :TWizardStageName read FPreviousStage write FPreviousStage;
    property NextStage :TWizardStageName read FNextStage write FNextStage;
    property OnEnter :TNotifyEvent read FOnEnter write FOnEnter;
    property OnLeave :TNotifyEvent read FOnLeave write FOnLeave;
  end;

  TWizardPlan = class(TCollection)
  private
    FWizardManager: TWizardManager;
    function GetItem(Index: Integer): TWizardStage;
    procedure SetItem(Index: Integer; Value: TWizardStage);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(WizardManager: TWizardManager);
    function Add: TWizardStage;
    function FindByName(const WizardStageName :string) :TWizardStage;
    property Items[Index: Integer]: TWizardStage read GetItem write SetItem; default;
  end;

  TWizardManager = class(TComponent)
  private
    FWizardPlan: TWizardPlan;
    FCurrentStage: TWizardStage;
    FOnFinish: TNotifyEvent;
    FPreviousButton: TButton;
    FNextButton: TButton;
    FFinishCaption: string;
    FPreviousCaption: string;
    FCancelCaption: string;
    FNextCaption: string;
    FExecuting: Boolean;
    FCancelButton: TButton;
    FFirstStageName: string;
    FOnCancel: TNotifyEvent;
    FFreezeLevel :Integer;
    FBaseCaption: string;
    FMovingTo: TWizardStage;
    FMovingFrom: TWizardStage;
    FMovingDirection: TMovingDirection;
    FCloseOnFinish: Boolean;
    FLabel: TLabel;
    FImage: TImage;
    FLayout: Boolean;
    procedure SetWizardPlan(const Value: TWizardPlan);
    procedure SetLayout(const Value: Boolean);
    { Private declarations }
  protected
    { Protected declarations }
    procedure UpdateButtons;
    procedure UpdateCaption;
  public
    { Public declarations }
    constructor Create(AOwner :TComponent); override;
    destructor Destroy; override;
    procedure Execute;
    procedure Next;
    procedure Previous;
    procedure Cancel;
    procedure FreezeUserInterface;
    procedure UnFreezeUserInterface;
    procedure Update;
    property CurrentStage :TWizardStage read FCurrentStage;
    property Executing :Boolean read FExecuting write FExecuting;
    property MovingTo :TWizardStage read FMovingTo write FMovingTo;
    property MovingFrom :TWizardStage read FMovingFrom;
    property MovingDirection :TMovingDirection read FMovingDirection;
    property Layout: Boolean read FLayout write SetLayout;
  published
    { Published declarations }
    property NextButton :TButton read FNextButton write FNextButton;
    property PreviousButton :TButton read FPreviousButton write FPreviousButton;
    property CancelButton :TButton read FCancelButton write FCancelButton;
    property FinishCaption :string read FFinishCaption write FFinishCaption;
    property PreviousCaption :string read FPreviousCaption write FPreviousCaption;
    property NextCaption :string read FNextCaption write FNextCaption;
    property CancelCaption :string read FCancelCaption write FCancelCaption;
    property WizardPlan :TWizardPlan read FWizardPlan write SetWizardPlan;
    property FirstStageName :TWizardStageName read FFirstStageName write FFirstStageName;
    property BaseCaption :string read FBaseCaption write FBaseCaption;
    property CloseOnFinish :Boolean read FCloseOnFinish write FCloseOnFinish default True;
    property TopLabel :TLabel read FLabel write FLabel;
    property Image :TImage read FImage write FImage;
    property OnFinish :TNotifyEvent read FOnFinish write FOnFinish;
    property OnCancel :TNotifyEvent read FOnCancel write FOnCancel;
  end;

procedure Register;

implementation

uses
  DsgnIntf;

type
  TWizardStageNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure Register;
begin
  RegisterComponents('MLR Sysco', [TWizardManager]);
  RegisterPropertyEditor(TypeInfo(TWizardStageName), TWizardStage,
    'PreviousStage', TWizardStageNameProperty);
  RegisterPropertyEditor(TypeInfo(TWizardStageName), TWizardStage,
    'NextStage', TWizardStageNameProperty);
  RegisterPropertyEditor(TypeInfo(TWizardStageName), TWizardManager,
    'FirstStageName', TWizardStageNameProperty);
end;

{ TWizardManager }

procedure TWizardManager.Cancel;
begin
  FExecuting := False;
  UpdateButtons;
  if Assigned(FOnCancel) then FOnCancel(Self);
end;

constructor TWizardManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWizardPlan := TWizardPlan.Create(Self);
  FCloseOnFinish := True;
end;

destructor TWizardManager.Destroy;
begin
  FWizardPlan.Free;
  inherited Destroy;
end;

procedure TWizardManager.Execute;
begin
  Layout := True;
  FExecuting := True;
  FCurrentStage := FWizardPlan.FindByName(FFirstStageName);
  UpdateButtons;
  UpdateCaption;
  if Assigned(FCurrentStage) and Assigned(FCurrentStage.FWinControl) then
    FCurrentStage.FWinControl.Show;
end;

procedure TWizardManager.FreezeUserInterface;
begin
  Inc(FFreezeLevel);
  if FFreezeLevel = 1 then begin
    if Assigned(FNextButton) then
      FNextButton.Enabled := False;
    if Assigned(FCancelButton) then
      FCancelButton.Enabled := False;
    if Assigned(FPreviousButton) then
      FPreviousButton.Enabled := False;
  end;
end;

procedure TWizardManager.Next;
begin
  FMovingDirection := mdNext;
  FMovingFrom := FCurrentStage;
  if FCurrentStage.NextStage <> '' then begin
    FMovingTo := FWizardPlan.FindByName(FCurrentStage.NextStage);
    FCurrentStage.Leave;
    FCurrentStage := FMovingTo;
    FCurrentStage.Enter;
  end else begin
    FCurrentStage.Leave;
    if Assigned(FOnFinish) then FOnFinish(Self);
    if FCloseOnFinish and (Owner is TCustomForm) then begin
      TCustomForm(Owner).Close;
      exit;
    end;
  end;
  UpdateButtons;
  UpdateCaption;
  FMovingDirection := mdNone;
end;

procedure TWizardManager.Previous;
begin
  if FCurrentStage.PreviousStage = '' then exit;
  FMovingDirection := mdPrevious;
  FMovingFrom := FCurrentStage;
  FMovingTo := FWizardPlan.FindByName(FCurrentStage.PreviousStage);
  FCurrentStage.Leave;
  FCurrentStage := FMovingTo;
  FCurrentStage.Enter;
  UpdateButtons;
  UpdateCaption;
  FMovingDirection := mdNone;
end;

procedure TWizardManager.SetLayout(const Value: Boolean);
var
  i :Integer;
  MaxWidth :Integer;
  MaxHeight :Integer;
  ControlMaxHeight :Integer;
  LastX :Integer;
  FStartTop :Integer;
  FStartLeft :Integer;
begin
  if FLayout = Value then exit;
  if not Value then begin
    FLayout := False;
    exit;
  end;
  if Assigned(FImage) then begin
    FImage.Align := alLeft;
    FStartLeft := FImage.Width;
  end else
    FStartLeft := 0;
  if Assigned(FLabel) then begin
    FLabel.AutoSize := True;
    FLabel.Top := 0;
    FLabel.Left := FStartLeft;
    FLabel.Font.Style := [fsBold];
    FStartTop := FLabel.Height + 8;
  end else
    FStartTop := 0;
  MaxWidth := 0;
  MaxHeight := 0;
  for i := 0 to FWizardPlan.Count - 1 do
    if Assigned(FWizardPlan[i].FWinControl) then begin
      FWizardPlan[i].FWinControl.Visible := False;
      FWizardPlan[i].FWinControl.Left := FStartLeft;
      FWizardPlan[i].FWinControl.Top := FStartTop;
      if MaxWidth < FWizardPlan[i].FWinControl.Width then
        MaxWidth := FWizardPlan[i].FWinControl.Width;
      if MaxHeight < FWizardPlan[i].FWinControl.Height then
        MaxHeight := FWizardPlan[i].FWinControl.Height;
    end;
  for i := 0 to FWizardPlan.Count - 1 do
    if Assigned(FWizardPlan[i].FWinControl) then begin
      FWizardPlan[i].FWinControl.Width := MaxWidth;
      FWizardPlan[i].FWinControl.Height := MaxHeight;
    end;
  Inc(MaxHeight, FStartTop);
  Inc(MaxWidth, FStartLeft);
  LastX := MaxWidth;
  ControlMaxHeight := MaxHeight;
  if Assigned(FCancelButton) then begin
    FCancelButton.Top := MaxHeight + 8;
    FCancelButton.Left := LastX - (FCancelButton.Width + 8);
    LastX := FCancelButton.Left;
    ControlMaxHeight := FCancelButton.Top + FCancelButton.Height + 8;
  end;
  if Assigned(FNextButton) then begin
    FNextButton.Top := MaxHeight + 8;
    FNextButton.Left := LastX - (FNextButton.Width + 8);
    LastX := FNextButton.Left;
    ControlMaxHeight := FNextButton.Top + FNextButton.Height + 8;
  end;
  if Assigned(FPreviousButton) then begin
    FPreviousButton.Top := MaxHeight + 8;
    FPreviousButton.Left := LastX - (FPreviousButton.Width + 8);
    ControlMaxHeight := FPreviousButton.Top + FPreviousButton.Height + 8;
  end;
  if Owner is TCustomForm then begin
    TCustomForm(Owner).ClientWidth := MaxWidth;
    TCustomForm(Owner).ClientHeight := ControlMaxHeight;
    TCustomForm(Owner).Left := (Screen.Width - TCustomForm(Owner).Width) div 2;
    TCustomForm(Owner).Top := (Screen.Height - TCustomForm(Owner).Height) div 2;
  end;
  FLayout := True;
end;

procedure TWizardManager.SetWizardPlan(const Value: TWizardPlan);
begin
  FWizardPlan.Assign(Value);
end;

procedure TWizardManager.UnFreezeUserInterface;
begin
  Dec(FFreezeLevel);
  if FFreezeLevel = 0 then begin
    if Assigned(FNextButton) then
      FNextButton.Enabled := False;
    if Assigned(FCancelButton) then
      FCancelButton.Enabled := False;
    if Assigned(FPreviousButton) then
      FPreviousButton.Enabled := False;
  end;
end;

procedure TWizardManager.Update;
begin
  UpdateButtons;
  UpdateCaption;
end;

procedure TWizardManager.UpdateButtons;
begin
  if FExecuting then begin
    if Assigned(FNextButton) then begin
      if FCurrentStage.NextStage <> '' then
        FNextButton.Caption := FNextCaption
      else
        FNextButton.Caption := FFinishCaption;
    end;
    if Assigned(FPreviousButton) then begin
      FPreviousButton.Caption := FPreviousCaption;
      FPreviousButton.Visible := FCurrentStage.PreviousStage <> '';
    end;
    if Assigned(FCancelButton) then
      FCancelButton.Caption := FCancelCaption;
  end else begin
    if Assigned(FNextButton) then
      FNextButton.Hide;
    if Assigned(FPreviousButton) then
      FPreviousButton.Hide;
    if Assigned(FCancelButton) then
      FCancelButton.Hide;
  end;
end;

procedure TWizardManager.UpdateCaption;
begin
  if Assigned(FLabel) and Assigned(FCurrentStage) then
    FLabel.Caption := '  ' + FCurrentStage.Caption;
  if not (Owner is TCustomForm) then exit;
  if not Assigned(FCurrentStage) then exit;
  TCustomForm(Owner).Caption := FBaseCaption + ' - ' + FCurrentStage.Caption;
end;

{ TWizardPlan }

function TWizardPlan.Add: TWizardStage;
begin
  Result := TWizardStage.Create(Self);
end;

constructor TWizardPlan.Create(WizardManager: TWizardManager);
begin
  inherited Create(TWizardStage);
  FWizardManager := WizardManager;
end;

function TWizardPlan.FindByName(
  const WizardStageName: string): TWizardStage;
var i :Integer;
begin
  for i := 0 to Count - 1 do
    if Items[i].FCaption = WizardStageName then begin
      Result := Items[i];
      exit;
    end;
  Result := nil;
end;

function TWizardPlan.GetItem(Index: Integer): TWizardStage;
begin
  Result := TWizardStage(inherited GetItem(Index));
end;

function TWizardPlan.GetOwner: TPersistent;
begin
  Result := FWizardManager;
end;

procedure TWizardPlan.SetItem(Index: Integer; Value: TWizardStage);
begin
  inherited SetItem(Index, Value);
end;

{ TWizardStage }

procedure TWizardStage.Enter;
begin
  if Assigned(FOnEnter) then FOnEnter(Self);
  FWinControl.Visible := True;
end;

function TWizardStage.GetDisplayName: string;
begin
  Result := FCaption;
  if Result = '' then Result := inherited GetDisplayName;
end;

procedure TWizardStage.Leave;
begin
  if Assigned(FOnLeave) then FOnLeave(Self);
  FWinControl.Visible := False;
end;

{ TWizardStageNameProperty }

function TWizardStageNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TWizardStageNameProperty.GetValues(Proc: TGetStrProc);
var
  Stage :TWizardStage;
  Plan :TWizardPlan;
  Manager :TWizardManager;
  Persistent :TPersistent;
  i :Integer;
begin
  Plan := nil;
  Persistent := GetComponent(0);
  if Persistent is TWizardStage then begin
    Stage := TWizardStage(Persistent);
    Plan := TWizardPlan(Stage.Collection);
  end else if Persistent is TWizardManager then begin
    Manager := TWizardManager(Persistent);
    Plan := Manager.FWizardPlan;
  end;
  if Assigned(Plan) then begin
    for i := 0 to Plan.Count - 1 do
      Proc(Plan[i].FCaption);
  end;
end;

end.
