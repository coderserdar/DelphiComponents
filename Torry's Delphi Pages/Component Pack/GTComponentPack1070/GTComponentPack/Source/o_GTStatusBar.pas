{*******************************************************}
{                                                       }
{       GT Delphi Components                            }
{       GT StatusBar                                    }
{                                                       }
{       Copyright (c) GT Delphi Components              }
{       http://www.gtdelphicomponents.gr                }
{                                                       }
{                                                       }
{*******************************************************}
unit o_GTStatusBar;

interface
uses
   Classes
  ,Controls
  ,ComCtrls
  ,StdCtrls
  ,Windows
  ,ExtCtrls
  ,Types
  ,Graphics
  ;
type
  TgtStatusPanel = class;
  TgtStatusBar   = class;
{------------------------------------------------------------------------------}
  TgtStatusPanelKind =
                      (
                        spkNone
                       ,spkCustom 
                       ,spkText
                       ,spkProgress
                       ,spkDateTime
                       ,spkImage
                       ,spkButton
                       ,spkComboBox
                       ,spkCheckBox
                       ,spkColorBox
                       )
                       ;
{------------------------------------------------------------------------------}
  TgtPanelExtraSettings = class(TPersistent)
  private
    FComboItems: TStrings;
    function GetButtonCaption: string;
    function GetCheckBoxCaption: string;
    function GetCheckBoxChecked: Boolean;
    function GetComboDropDownCount: Integer;
    function GetDate: TDate;
    function GetDateChecked: Boolean;
    function GetDateFormat: TDTDateFormat;
    function GetDateMode: TDTDateMode;
    function GetFormat: string;
    function GetImageFileName: string;
    function GetProgressMax: Integer;
    function GetProgressPos: Integer;
    function GetShowCheckBox: Boolean;
    function GetTime: TTime;
    procedure SetButtonCaption(const Value: string);
    procedure SetCheckBoxCaption(const Value: string);
    procedure SetCheckBoxChecked(const Value: Boolean);
    procedure SetComboDropDownCount(const Value: Integer);
    procedure SetComboItems(const Value: TStrings);
    procedure SetDate(const Value: TDate);
    procedure SetDateChecked(const Value: Boolean);
    procedure SetDateFormat(const Value: TDTDateFormat);
    procedure SetDateMode(const Value: TDTDateMode);
    procedure SetFormat(const Value: string);
    procedure SetImageFileName(const Value: string);
    procedure SetProgressBax(const Value: Integer);
    procedure SetProgressPos(const Value: Integer);
    procedure SetShowCheckBox(const Value: Boolean);
    procedure SetTime(const Value: TTime);
    function GetImageAutoSize: Boolean;
    function GetImageCenter: Boolean;
    function GetImageStretch: Boolean;
    procedure SetImageAutoSize(const Value: Boolean);
    procedure SetImageCenter(const Value: Boolean);
    procedure SetImageStretch(const Value: Boolean);
    function GetColorSelected: TColor;
    function GetColorStyle: TColorBoxStyle;
    procedure SetColorSelected(const Value: TColor);
    procedure SetColorStyle(const Value: TColorBoxStyle);
    function GetPicture: TPicture;
    procedure SetPicture(const Value: TPicture);
    { Private declarations }
  protected
    { Protected declarations }
    FPanel         : TgtStatusPanel;
    FImageFileName : string;
  public
    { Public declarations }
    constructor Create(APanel:TgtStatusPanel);
    destructor  Destroy;override;
    procedure   Assign(Source : TPersistent);override;
  published
    { Published declarations}
      property ProgressMax         : Integer         read GetProgressMax         write SetProgressBax;
      property ProgressPos         : Integer         read GetProgressPos         write SetProgressPos;
      property DT_Date             : TDate           read GetDate                write SetDate;
      property DT_Time             : TTime           read GetTime                write SetTime;
      property DT_DateFormat       : TDTDateFormat   read GetDateFormat          write SetDateFormat;
      property DT_DateMode         : TDTDateMode     read GetDateMode            write SetDateMode;
      property DT_Format           : string          read GetFormat              write SetFormat;
      property DT_ShowChekBox      : Boolean         read GetShowCheckBox        write SetShowCheckBox;
      property DT_Checked          : Boolean         read GetDateChecked         write SetDateChecked;
      property ImageFileName       : string          read GetImageFileName       write SetImageFileName;
      property ImageAutoSize       : Boolean         read GetImageAutoSize       write SetImageAutoSize;
      property ImageStretch        : Boolean         read GetImageStretch        write SetImageStretch;
      property ImageCenter         : Boolean         read GetImageCenter         write SetImageCenter;
      property Image               : TPicture        read GetPicture             write SetPicture;
      property ButtonCaption       : string          read GetButtonCaption       write SetButtonCaption;
      property ComboItems          : TStrings        read FComboItems            write SetComboItems;
      property ComboDropDownCount  : Integer         read GetComboDropDownCount  write SetComboDropDownCount;
      property CheckBoxCaption     : string          read GetCheckBoxCaption     write SetCheckBoxCaption;
      property CheckBoxChecked     : Boolean         read GetCheckBoxChecked     write SetCheckBoxChecked;
      property ColorSelected       : TColor          read GetColorSelected       write SetColorSelected;
      property ColorStyle          : TColorBoxStyle  read GetColorStyle          write SetColorStyle;
  end;
{------------------------------------------------------------------------------}
  TgtStatusPanel = class(TStatusPanel)
  private
    FKind             : TgtStatusPanelKind;
    FExtraIsDrawn     : Boolean;
    FPanelProgressBar : TProgressBar;
    FDateTimePicker   : TDateTimePicker;
    FImage            : TImage;
    FCheckBox         : TCheckBox;
    FButton           : TButton;
    FComboBox         : TComboBox;
    FColorBox         : TColorBox;
    FExtraSettings    : TgtPanelExtraSettings;
    FControl          : TControl;
    procedure SetKind(const Value: TgtStatusPanelKind);
    function GetStatusBar: TgtStatusBar;
    procedure SetExtraSettings(const Value: TgtPanelExtraSettings);
    procedure SetCustomControl(const Value: TControl);
    { Private declarations }
  protected
    { Protected declarations }
    property ExtraIsDrawn   : Boolean             read FExtraIsDrawn     write FExtraIsDrawn;
    property StatusBar      : TgtStatusBar        read GetStatusBar;
    {property ProgressBar    : TProgressBar        read FPanelProgressBar;
    property DateTimePicker : TDateTimePicker     read FDateTimePicker  ;}
  public
    { Public declarations }
    constructor Create(Collection: TCollection); override;
    destructor Destroy;override;
  published
    { Published declarations}
    property Kind             : TgtStatusPanelKind    read FKind             write SetKind default spkText;
    property ExtraSettings    : TgtPanelExtraSettings read FExtraSettings    write SetExtraSettings;
    property CustomControl    : TControl              read FControl          write SetCustomControl;
  end;
{------------------------------------------------------------------------------}
  TgtStatusPanels = class(TCollection)
  private
    FStatusBar : TgtStatusBar;
    function GetItem(Index: Integer): TgtStatusPanel;
    procedure SetItem(Index: Integer; const Value: TgtStatusPanel);
    { Private declarations }
  protected
    { Protected declarations }
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    { Public declarations }
    constructor Create(StatusBar: TgtStatusBar);
    function Add: TgtStatusPanel;
    function AddItem(Item: TgtStatusPanel; Index: Integer): TgtStatusPanel;
    function Insert(Index: Integer): TgtStatusPanel;
    property Items[Index: Integer]: TgtStatusPanel read GetItem write SetItem; default;
  published
    { Published declarations}
  end;
{------------------------------------------------------------------------------}
  TgtStatusBarPanelControlNotifyEvent = procedure (Sender : TObject ; PanelIndex : Integer) of Object;
{------------------------------------------------------------------------------}
  TgtStatusBar = class(TStatusBar)
  private
    FOnPanelButtonClick: TgtStatusBarPanelControlNotifyEvent;
    FOnPanelCheckBoxClick: TgtStatusBarPanelControlNotifyEvent;
    FOnPanelComboSelect: TgtStatusBarPanelControlNotifyEvent;
    FOnPanelDateTimeChange: TgtStatusBarPanelControlNotifyEvent;
    { Private declarations }
    function GetPanels: TgtStatusPanels;
    procedure SetPanels(const Value: TgtStatusPanels);
    procedure DrawImagePanel(APanel: TgtStatusPanel; PanelRect: TRect);
    procedure DrawButtonPanel(APanel: TgtStatusPanel; PanelRect: TRect);
    procedure DrawCheckBoxPanel(APanel: TgtStatusPanel; PanelRect: TRect);
    procedure DrawComboBoxPanel(APanel: TgtStatusPanel; PanelRect: TRect);
    procedure DrawCustomControlPanel(APanel: TgtStatusPanel;PanelRect: TRect);
    procedure DrawColorBoxPanel(APanel: TgtStatusPanel; PanelRect: TRect);
  protected
    { Protected declarations }
    procedure InternalOnPanelButtonClick   (Sender : TObject);
    procedure InternalOnPanelCheckBoxClick (Sender : TObject);
    procedure InternalOnPanelComboSelect   (Sender : TObject);
    procedure InternalOnPanelDateTimeChange(Sender : TObject);
    function  GetPanelClass: TStatusPanelClass; override;
    procedure DrawPanel(Panel: TStatusPanel; const Rect: TRect);override;
    function  CreatePanel: TStatusPanel; override;
    function  CreatePanels: TStatusPanels; override;
  protected
    procedure DrawProgressBarPanel(APanel :TgtStatusPanel;PanelRect : TRect);
    procedure DrawDateTimePanel   (APanel :TgtStatusPanel;PanelRect : TRect);
    procedure DrawControlForPanel (APanel :TgtStatusPanel;PanelRect : TRect);
    procedure CreateWnd; override;
    procedure Notification(AComponent: TComponent;Operation: TOperation);override;
  public
    { Public declarations }
    constructor Create(AOwner:TComponent);override;
    destructor  Destroy;override;
  public
    property GTPanels : TgtStatusPanels read GetPanels write SetPanels;
  published
    { Published declarations}
    property OnPanelButtonClick   : TgtStatusBarPanelControlNotifyEvent read FOnPanelButtonClick    write FOnPanelButtonClick;
    property OnPanelCheckBoxClick : TgtStatusBarPanelControlNotifyEvent read FOnPanelCheckBoxClick  write FOnPanelCheckBoxClick;
    property OnPanelComboSelect   : TgtStatusBarPanelControlNotifyEvent read FOnPanelComboSelect    write FOnPanelComboSelect;
    property OnPanelDateTimeChange: TgtStatusBarPanelControlNotifyEvent read FOnPanelDateTimeChange write FOnPanelDateTimeChange;
  end;
{------------------------------------------------------------------------------}


implementation

{ TgtStatusPanel }
{------------------------------------------------------------------------------}
constructor TgtStatusPanel.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FExtraIsDrawn  := False;
  FExtraSettings := TgtPanelExtraSettings.Create(Self);
end;
{------------------------------------------------------------------------------}
destructor TgtStatusPanel.Destroy;
begin
  FExtraSettings.Free;
  inherited;
end;
{------------------------------------------------------------------------------}
function TgtStatusPanel.GetStatusBar: TgtStatusBar;
begin
  Result :=  TgtStatusBar(TgtStatusPanels(Self.GetOwner).GetOwner);
end;
{------------------------------------------------------------------------------}
procedure TgtStatusPanel.SetCustomControl(const Value: TControl);
begin
  if Assigned(FControl) then
    FControl.RemoveFreeNotification(Self.StatusBar);

  FControl := Value;

  if Assigned(FControl) then
    FControl.FreeNotification(Self.StatusBar);
end;
{------------------------------------------------------------------------------}
procedure TgtStatusPanel.SetExtraSettings(const Value: TgtPanelExtraSettings);
begin
  FExtraSettings.Assign(Value);
end;
{------------------------------------------------------------------------------}
procedure TgtStatusPanel.SetKind(const Value: TgtStatusPanelKind);
begin
  if FKind <> Value then
  begin
    FKind := Value;
    case FKind of
      spkText     : ;
      spkProgress : FPanelProgressBar  := TProgressBar.Create(Self.StatusBar);
      spkDateTime : FDateTimePicker    := TDateTimePicker.Create(Self.StatusBar);
      spkImage    : FImage             := TImage.Create(Self.StatusBar);
      spkButton   : FButton            := TButton.Create(Self.StatusBar);
      spkComboBox : FComboBox          := TComboBox.Create(Self.StatusBar);
      spkCheckBox : FCheckBox          := TCheckBox.Create(Self.StatusBar);
      spkColorBox : FColorBox          := TColorBox.Create(Self.StatusBar);
    end;
    Self.StatusBar.RecreateWnd;
  end;
end;
{------------------------------------------------------------------------------}


{ TgtStatusPanels }
{------------------------------------------------------------------------------}
constructor TgtStatusPanels.Create(StatusBar: TgtStatusBar);
begin
  if StatusBar <> nil then
    inherited Create(StatusBar.GetPanelClass)
  else
    inherited Create(TStatusPanel);
  FStatusBar := StatusBar;
end;
{------------------------------------------------------------------------------}
function TgtStatusPanels.Add: TgtStatusPanel;
begin
  Result := TgtStatusPanel(inherited Add);
end;
{------------------------------------------------------------------------------}
function TgtStatusPanels.AddItem(Item: TgtStatusPanel;Index: Integer): TgtStatusPanel;
begin
  if Item = nil then
    Result := TgtStatusPanel(FStatusBar.CreatePanel)
  else
    Result := Item;
  if Assigned(Result) then
  begin
    Result.Collection := Self;
    if Index < 0 then
      Index := Count - 1;
    Result.Index := Index;
  end;
end;
{------------------------------------------------------------------------------}
function TgtStatusPanels.GetItem(Index: Integer): TgtStatusPanel;
begin
  Result := TgtStatusPanel(inherited GetItem(Index));
end;
{------------------------------------------------------------------------------}
function TgtStatusPanels.GetOwner: TPersistent;
begin
  Result := FStatusBar;
end;
{------------------------------------------------------------------------------}
function TgtStatusPanels.Insert(Index: Integer): TgtStatusPanel;
begin
  Result := AddItem(nil, Index);
end;
{------------------------------------------------------------------------------}
procedure TgtStatusPanels.Update(Item: TCollectionItem);
begin
  inherited;
  {if Item <> nil then
    FStatusBar.UpdatePanel(Item.Index, False)
  else
    FStatusBar.UpdatePanels(True, False);}
end;
{------------------------------------------------------------------------------}
procedure TgtStatusPanels.SetItem(Index: Integer; const Value: TgtStatusPanel);
begin
  inherited SetItem(Index, Value);
end;
{------------------------------------------------------------------------------}


{ TgtStatusBar }
{------------------------------------------------------------------------------}
constructor TgtStatusBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Height := 25;
end;
{------------------------------------------------------------------------------}
destructor TgtStatusBar.Destroy;
begin
  Self.Panels.Clear;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtStatusBar.Notification(AComponent: TComponent; Operation: TOperation);
var
  i : integer;
begin
  if Operation = opRemove then
  begin
    for i:=0 to Pred(Self.GTPanels.Count) do
    begin
      if AComponent = Self.GTPanels[i].CustomControl then
        Self.GTPanels[i].CustomControl := nil;
    end;
  end;
  inherited Notification(AComponent,Operation);
end;
{------------------------------------------------------------------------------}
procedure TgtStatusBar.CreateWnd;
var
 i : Integer;
begin
  inherited CreateWnd;
  for i:= 0 to Pred(Self.Panels.Count) do
    if TgtStatusPanel(Self.Panels[i]).Kind <> spkText then
      TgtStatusPanel(Self.Panels[i]).Style := psOwnerDraw;
end;
{------------------------------------------------------------------------------}
function TgtStatusBar.CreatePanel: TStatusPanel;
begin
  Result       := inherited CreatePanel;
  Result.Style := psOwnerDraw;
  Result.Width := 50;
end;
{------------------------------------------------------------------------------}
function TgtStatusBar.CreatePanels: TStatusPanels;
begin
  Result := TStatusPanels(TgtStatusPanels.Create(Self));
end;
{------------------------------------------------------------------------------}
function TgtStatusBar.GetPanelClass: TStatusPanelClass;
begin
  Result := TgtStatusPanel;
end;
{------------------------------------------------------------------------------}
procedure TgtStatusBar.DrawPanel(Panel: TStatusPanel; const Rect: TRect);
begin
  if TgtStatusPanel(Panel).Kind <> spkNone then
    DrawControlForPanel(TgtStatusPanel(Panel),Rect)
  else
    inherited DrawPanel(Panel,Rect);
end;
{------------------------------------------------------------------------------}
procedure TgtStatusBar.DrawControlForPanel(APanel: TgtStatusPanel;PanelRect: TRect);
begin
  case APanel.Kind of
    spkNone         : Exit;
    spkProgress     : DrawProgressBarPanel(APanel,PanelRect);
    spkDateTime     : DrawDateTimePanel(APanel,PanelRect);
    spkImage        : DrawImagePanel(APanel,PanelRect);
    spkButton       : DrawButtonPanel(APanel,PanelRect);
    spkComboBox     : DrawComboBoxPanel(APanel,PanelRect);
    spkCheckBox     : DrawCheckBoxPanel(APanel,PanelRect);
    spkColorBox     : DrawColorBoxPanel(APanel,PanelRect);
    spkCustom       : DrawCustomControlPanel(APanel,PanelRect);
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtStatusBar.DrawProgressBarPanel(APanel: TgtStatusPanel;PanelRect: TRect);
begin
  if not Assigned(APanel.FPanelProgressBar) then
    APanel.FPanelProgressBar      := TProgressBar.Create(Self);

  APanel.FPanelProgressBar.Parent     := Self;
  APanel.FPanelProgressBar.BoundsRect := PanelRect;
  APanel.FPanelProgressBar.Tag        := APanel.Index;
  APanel.ExtraIsDrawn                 := True;
end;
{------------------------------------------------------------------------------}
procedure TgtStatusBar.DrawDateTimePanel(APanel: TgtStatusPanel;PanelRect: TRect);
begin
  if not Assigned(APanel.FDateTimePicker) then
    APanel.FDateTimePicker      := TDateTimePicker.Create(Self);

  APanel.FDateTimePicker.Parent     := Self;
  APanel.FDateTimePicker.BoundsRect := PanelRect;
  APanel.FDateTimePicker.Tag        := APanel.Index;
  APanel.FDateTimePicker.OnChange   := InternalOnPanelDateTimeChange;
  APanel.ExtraIsDrawn           := True;
end;
{------------------------------------------------------------------------------}
procedure TgtStatusBar.DrawImagePanel(APanel: TgtStatusPanel;PanelRect: TRect);
begin
  if not Assigned(APanel.FImage) then
    APanel.FImage      := TImage.Create(Self);

  APanel.FImage.Parent     := Self;
  APanel.FImage.BoundsRect := PanelRect;
  APanel.FImage.Tag        := APanel.Index;
  APanel.ExtraIsDrawn      := True;
end;
{------------------------------------------------------------------------------}
procedure TgtStatusBar.DrawButtonPanel(APanel: TgtStatusPanel;PanelRect: TRect);
begin
  if not Assigned(APanel.FButton) then
    APanel.FButton          := TButton.Create(Self);

  APanel.FButton.Tag        := APanel.Index;  
  APanel.FButton.Parent     := Self;
  APanel.FButton.BoundsRect := PanelRect;
  APanel.FButton.OnClick    := InternalOnPanelButtonClick;
  APanel.ExtraIsDrawn       := True;
end;
{------------------------------------------------------------------------------}
procedure TgtStatusBar.DrawComboBoxPanel(APanel: TgtStatusPanel;PanelRect: TRect);
begin
  if not Assigned(APanel.FComboBox) then
    APanel.FComboBox          := TComboBox.Create(Self);

  APanel.FComboBox.Tag        := APanel.Index;
  APanel.FComboBox.Parent     := Self;
  APanel.FComboBox.BoundsRect := PanelRect;
  APanel.FComboBox.Items.Assign(APanel.ExtraSettings.ComboItems);
  APanel.FComboBox.OnSelect   := InternalOnPanelComboSelect;
  APanel.ExtraIsDrawn         := True;
end;
{------------------------------------------------------------------------------}
procedure TgtStatusBar.DrawColorBoxPanel(APanel: TgtStatusPanel;PanelRect: TRect);
begin
  if not Assigned(APanel.FColorBox) then
    APanel.FColorBox          := TColorBox.Create(Self);

  APanel.FColorBox.Tag        := APanel.Index;
  APanel.FColorBox.Parent     := Self;
  APanel.FColorBox.BoundsRect := PanelRect;
  APanel.FColorBox.OnSelect   := InternalOnPanelComboSelect;
  APanel.ExtraIsDrawn         := True;
end;
{------------------------------------------------------------------------------}
procedure TgtStatusBar.DrawCheckBoxPanel(APanel: TgtStatusPanel;PanelRect: TRect);
begin
  if not Assigned(APanel.FCheckBox) then
    APanel.FCheckBox          := TCheckBox.Create(Self);

  APanel.FCheckBox.Parent     := Self;
  APanel.FCheckBox.BoundsRect := PanelRect;
  APanel.FCheckBox.OnClick    := InternalOnPanelCheckBoxClick;
  APanel.FCheckBox.Tag        := APanel.Index;
  APanel.ExtraIsDrawn         := True;
end;
{------------------------------------------------------------------------------}
procedure TgtStatusBar.DrawCustomControlPanel(APanel: TgtStatusPanel;PanelRect: TRect);
begin
  if Assigned(APanel.CustomControl) then
  begin
    APanel.CustomControl.Parent     := Self;
    APanel.CustomControl.BoundsRect := PanelRect;
    APanel.ExtraIsDrawn             := True;
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtStatusBar.InternalOnPanelButtonClick(Sender: TObject);
begin
  if Assigned(FOnPanelButtonClick) then
    FOnPanelButtonClick(Sender,TComponent(Sender).Tag);
end;
{------------------------------------------------------------------------------}
procedure TgtStatusBar.InternalOnPanelCheckBoxClick(Sender: TObject);
begin
  if Assigned(FOnPanelCheckBoxClick) then
    FOnPanelCheckBoxClick(Sender,TComponent(Sender).Tag);
end;
{------------------------------------------------------------------------------}
procedure TgtStatusBar.InternalOnPanelComboSelect(Sender: TObject);
begin
  if Assigned(FOnPanelComboSelect) then
    FOnPanelComboSelect(Sender,TComponent(Sender).Tag);
end;
{------------------------------------------------------------------------------}
procedure TgtStatusBar.InternalOnPanelDateTimeChange(Sender: TObject);
begin
  if Assigned(FOnPanelDateTimeChange) then
    FOnPanelDateTimeChange(Sender,TComponent(Sender).Tag);
end;
{------------------------------------------------------------------------------}


{------------------------------------------------------------------------------}
function TgtStatusBar.GetPanels: TgtStatusPanels;
begin
  Result := TgtStatusPanels(Panels);
end;
{------------------------------------------------------------------------------}
procedure TgtStatusBar.SetPanels(const Value: TgtStatusPanels);
begin
  Panels.Assign(Value);
end;
{------------------------------------------------------------------------------}








{ TgtPanelExtraSettings }
{------------------------------------------------------------------------------}
constructor TgtPanelExtraSettings.Create(APanel: TgtStatusPanel);
begin
  inherited Create;
  FPanel := APanel;
  FComboItems := TStringList.Create;
end;
{------------------------------------------------------------------------------}
destructor TgtPanelExtraSettings.Destroy;
begin
  FComboItems.Free;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TgtPanelExtraSettings.Assign(Source: TPersistent);
begin
  if Source is TgtPanelExtraSettings then
  begin
    Self.ProgressMax     := TgtPanelExtraSettings(Source).ProgressMax;
    Self.ProgressPos     := TgtPanelExtraSettings(Source).ProgressPos;
    Self.DT_Date         := TgtPanelExtraSettings(Source).DT_Date;
    Self.DT_Time         := TgtPanelExtraSettings(Source).DT_Time;
    Self.DT_DateFormat   := TgtPanelExtraSettings(Source).DT_DateFormat;
    Self.DT_DateMode     := TgtPanelExtraSettings(Source).DT_DateMode;
    Self.DT_Format       := TgtPanelExtraSettings(Source).DT_Format;
    Self.DT_ShowChekBox  := TgtPanelExtraSettings(Source).DT_ShowChekBox;
    Self.DT_Checked      := TgtPanelExtraSettings(Source).DT_Checked;
    Self.ImageFileName   := TgtPanelExtraSettings(Source).ImageFileName;
    Self.ButtonCaption   := TgtPanelExtraSettings(Source).ButtonCaption;
    Self.CheckBoxCaption := TgtPanelExtraSettings(Source).CheckBoxCaption;
    Self.CheckBoxChecked := TgtPanelExtraSettings(Source).CheckBoxChecked;
  end
  else
  inherited Assign(Source);
end;
{------------------------------------------------------------------------------}
function TgtPanelExtraSettings.GetButtonCaption: string;
begin
  Result := '';
  if FPanel.Kind = spkButton then
    Result := FPanel.FButton.Caption;
end;
{------------------------------------------------------------------------------}
function TgtPanelExtraSettings.GetCheckBoxCaption: string;
begin
  Result := '';
  if FPanel.Kind = spkCheckBox then
    Result := FPanel.FCheckBox.Caption;
end;
{------------------------------------------------------------------------------}
function TgtPanelExtraSettings.GetCheckBoxChecked: Boolean;
begin
  Result := False;
  if FPanel.Kind = spkCheckBox then
    Result := FPanel.FCheckBox.Checked;
end;
{------------------------------------------------------------------------------}
function TgtPanelExtraSettings.GetComboDropDownCount: Integer;
begin
  Result := 0;
  if FPanel.Kind = spkComboBox then
    Result := FPanel.FComboBox.DropDownCount;
end;
{------------------------------------------------------------------------------}
function TgtPanelExtraSettings.GetDate: TDate;
begin
  Result := 0;
  if FPanel.Kind = spkDateTime then
    Result := FPanel.FDateTimePicker.Date;
end;
{------------------------------------------------------------------------------}
function TgtPanelExtraSettings.GetDateChecked: Boolean;
begin
  Result := False;
  if FPanel.Kind = spkDateTime then
    Result := FPanel.FDateTimePicker.Checked;
end;
{------------------------------------------------------------------------------}
function TgtPanelExtraSettings.GetDateFormat: TDTDateFormat;
begin
  Result := dfShort;
  if FPanel.Kind = spkDateTime then
    Result := FPanel.FDateTimePicker.DateFormat;
end;
{------------------------------------------------------------------------------}
function TgtPanelExtraSettings.GetDateMode: TDTDateMode;
begin
  Result := dmComboBox;
  if FPanel.Kind = spkDateTime then
    Result := FPanel.FDateTimePicker.DateMode
end;
{------------------------------------------------------------------------------}
function TgtPanelExtraSettings.GetFormat: string;
begin
  Result := '';
  if FPanel.Kind = spkDateTime then
    Result := FPanel.FDateTimePicker.Format;
end;
{------------------------------------------------------------------------------}
function TgtPanelExtraSettings.GetImageFileName: string;
begin
  Result := FImageFileName;
end;
{------------------------------------------------------------------------------}
function TgtPanelExtraSettings.GetImageAutoSize: Boolean;
begin
  Result := False;
  if FPanel.Kind = spkImage then
    Result := FPanel.FImage.AutoSize;
end;
{------------------------------------------------------------------------------}
function TgtPanelExtraSettings.GetImageCenter: Boolean;
begin
  Result := False;
  if FPanel.Kind = spkImage then
    Result := FPanel.FImage.Center;
end;
{------------------------------------------------------------------------------}
function TgtPanelExtraSettings.GetImageStretch: Boolean;
begin
  Result := False;
  if FPanel.Kind = spkImage then
    Result := FPanel.FImage.Stretch;
end;
{------------------------------------------------------------------------------}
function TgtPanelExtraSettings.GetPicture: TPicture;
begin
  Result := nil;
  if FPanel.Kind = spkImage then
    Result := FPanel.FImage.Picture;
end;
{------------------------------------------------------------------------------}
function TgtPanelExtraSettings.GetProgressMax: Integer;
begin
  Result := 0;
  if FPanel.Kind = spkProgress then
    Result := FPanel.FPanelProgressBar.Max;
end;
{------------------------------------------------------------------------------}
function TgtPanelExtraSettings.GetProgressPos: Integer;
begin
  Result := 0;
  if FPanel.Kind = spkProgress then
    Result := FPanel.FPanelProgressBar.Position;
end;
{------------------------------------------------------------------------------}
function TgtPanelExtraSettings.GetShowCheckBox: Boolean;
begin
  Result := False;
  if FPanel.Kind = spkDateTime then
    Result := FPanel.FDateTimePicker.ShowCheckbox;
end;
{------------------------------------------------------------------------------}
function TgtPanelExtraSettings.GetTime: TTime;
begin
  Result := 0;
  if FPanel.Kind = spkDateTime then
    Result := FPanel.FDateTimePicker.Time;
end;
{------------------------------------------------------------------------------}
function TgtPanelExtraSettings.GetColorSelected: TColor;
begin
  Result := 0;
  if FPanel.Kind = spkColorBox then
    Result := FPanel.FColorBox.Selected;
end;
{------------------------------------------------------------------------------}
function TgtPanelExtraSettings.GetColorStyle: TColorBoxStyle;
begin
  Result := [];
  if FPanel.Kind = spkColorBox then
    Result := FPanel.FColorBox.Style;
end;
{------------------------------------------------------------------------------}



{------------------------------------------------------------------------------}
procedure TgtPanelExtraSettings.SetButtonCaption(const Value: string);
begin
  if FPanel.Kind = spkButton then
    FPanel.FButton.Caption := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtPanelExtraSettings.SetCheckBoxCaption(const Value: string);
begin
  if FPanel.Kind = spkCheckBox then
    FPanel.FCheckBox.Caption := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtPanelExtraSettings.SetCheckBoxChecked(const Value: Boolean);
begin
  if FPanel.Kind = spkCheckBox then
    FPanel.FCheckBox.Checked := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtPanelExtraSettings.SetComboDropDownCount(const Value: Integer);
begin
  if FPanel.Kind = spkComboBox then
    FPanel.FComboBox.DropDownCount := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtPanelExtraSettings.SetComboItems(const Value: TStrings);
begin
  if FPanel.Kind = spkComboBox then
  begin
    FComboItems.Assign(Value);
    if Assigned(FPanel.FComboBox.Parent) then
      FPanel.FComboBox.Items.Assign(FComboItems);
  end;
end;
{------------------------------------------------------------------------------}
procedure TgtPanelExtraSettings.SetDate(const Value: TDate);
begin
  if FPanel.Kind = spkDateTime then
    FPanel.FDateTimePicker.Date := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtPanelExtraSettings.SetDateChecked(const Value: Boolean);
begin
  if FPanel.Kind = spkDateTime then
    FPanel.FDateTimePicker.Checked := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtPanelExtraSettings.SetDateFormat(const Value: TDTDateFormat);
begin
  if FPanel.Kind = spkDateTime then
    FPanel.FDateTimePicker.DateFormat := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtPanelExtraSettings.SetDateMode(const Value: TDTDateMode);
begin
  if FPanel.Kind = spkDateTime then
    FPanel.FDateTimePicker.DateMode := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtPanelExtraSettings.SetFormat(const Value: string);
begin
  if FPanel.Kind = spkDateTime then
    FPanel.FDateTimePicker.Format := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtPanelExtraSettings.SetImageFileName(const Value: string);
begin
  if FPanel.Kind = spkImage then
    FPanel.FImage.Picture.LoadFromFile(Value);
end;
{------------------------------------------------------------------------------}
procedure TgtPanelExtraSettings.SetImageAutoSize(const Value: Boolean);
begin
  if FPanel.Kind = spkImage then
    FPanel.FImage.AutoSize := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtPanelExtraSettings.SetImageCenter(const Value: Boolean);
begin
  if FPanel.Kind = spkImage then
    FPanel.FImage.Center := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtPanelExtraSettings.SetImageStretch(const Value: Boolean);
begin
  if FPanel.Kind = spkImage then
    FPanel.FImage.Stretch := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtPanelExtraSettings.SetPicture(const Value: TPicture);
begin
    if FPanel.Kind = spkImage then
      FPanel.FImage.Picture := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtPanelExtraSettings.SetProgressBax(const Value: Integer);
begin
  if FPanel.Kind = spkProgress then
    FPanel.FPanelProgressBar.Max := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtPanelExtraSettings.SetProgressPos(const Value: Integer);
begin
  if FPanel.Kind = spkProgress then
    FPanel.FPanelProgressBar.Position := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtPanelExtraSettings.SetShowCheckBox(const Value: Boolean);
begin
  if FPanel.Kind = spkDateTime then
    FPanel.FDateTimePicker.ShowCheckBox := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtPanelExtraSettings.SetTime(const Value: TTime);
begin
  if FPanel.Kind = spkDateTime then
    FPanel.FDateTimePicker.Time := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtPanelExtraSettings.SetColorSelected(const Value: TColor);
begin
  if FPanel.Kind = spkColorBox then
    FPanel.FColorBox.Selected := Value;
end;
{------------------------------------------------------------------------------}
procedure TgtPanelExtraSettings.SetColorStyle(const Value: TColorBoxStyle);
begin
  if FPanel.Kind = spkColorBox then
    FPanel.FColorBox.Style := Value;
end;
{------------------------------------------------------------------------------}









end.
