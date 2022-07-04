unit CHForm;

{ ##############################################################################
  TCHForm

  Version   		:   1.1.1
  Delphi    		:   5, 6, 7
  Author     		:   Christian Hämmerle
  eMail     		:   chaemmerle@Blue-Xplosion.de
  Internet  		:   http://www.Blue-Xplosion.de (German/English)

  History:
  1.0.0 - 21.07.2002    - First Release
  1.1.0 - 29.08.2002    - NEW: FormLook (Elliptic, RoundRect, Triangle, Ring)
  1.1.1 - 09.03.2003    - reorganize "uses" for more performance and less memory needed

  ############################################################################ }

interface

uses
  Windows, Forms, Messages, Classes, Controls, ExtCtrls;

type
  TFormMoveMode = (foOnlyCaption, foAll, foNone);
  TOnTopMode = (otNormalOnTop, otForceOnTop);
  TFormLook = (flNormal, flElliptic, flRoundRect, flTriangle, flRing);

  TCHForm = class;

  TCHTitle = class(TPersistent)
  private
    FOwner : TCHForm;
    FTitleBlink: Boolean;
    FBlinkInterval: Cardinal;
    FTimer : TTimer;
    FShowTitlebar: Boolean;
    FCaption : string;
    FShowCaption : Boolean;

    procedure SetBlinkInterval(const Value: Cardinal);
    procedure SetTitleBlink(const Value: Boolean);
    procedure SetShowTitlebar(const Value: Boolean);
  public
    constructor Create(AOwner: TCHForm); virtual;
    destructor Destroy; override;
  published
    property Blink : Boolean read FTitleBlink write SetTitleBlink;
    property Interval : Cardinal read FBlinkInterval Write SetBlinkInterval;
    property ShowTitlebar : Boolean read FShowTitlebar Write SetShowTitlebar;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHFormLook = class(TPersistent)
  private
    FOwner : TCHForm;
    FFormLook: TFormLook;
    FRoundRectY: Word;
    FInnerRing: Word;
    FRoundRectX: Word;
    procedure SetFormLook(const Value: TFormLook);
    procedure SetInnerRing(const Value: Word);
    procedure SetRoundRectX(const Value: Word);
    procedure SetRoundRectY(const Value: Word);

  public
    constructor Create(AOwner: TCHForm); virtual;
  published
    property Look : TFormLook read FFormLook Write SetFormLook;
    property RoundRectX : Word read FRoundRectX Write SetRoundRectX;
    property RoundRectY : Word read FRoundRectY Write SetRoundRectY;
    property InnerRing : Word read FInnerRing Write SetInnerRing;
  end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
  TCHForm = class(TComponent)
  private
    FForm : TForm;
    FOrigWndProc : TWndMethod;
    FTitleClass : TCHTitle;
    FFormLookClass : TCHFormLook;
    FFormMoveMode : TFormMoveMode;
    FOnTopMode : TOnTopMode;
    FAllwaysOnTop: Boolean;
    FTimer : TTimer;

    FFullscreen: Boolean;
    FFormLeft: Integer;
    FFormTop: Integer;
    FFormBorderStyle : TFormBorderStyle;
    FFormWidth : Integer;
    FFormHeight : Integer;

    procedure DoBlink(Sender : TObject);
    procedure DoFormLook(Value : TFormLook);
    procedure DoOnTopMode(Sender : TObject);
    procedure SetAllwaysOnTop(const Value: Boolean);
    procedure SetFormMoveMode(const Value: TFormMoveMode);
    procedure SetOnTopMode(const Value: TOnTopMode);

    procedure WndProc(var Message: TMessage);
    procedure WMNCHitTest(var Msg: TMessage);
    procedure SetFullscreen(const Value: Boolean);
  protected

  public
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
    destructor Destroy; override;

    procedure HideTitlebar(Form : TForm);
    procedure ShowTitlebar(Form : TForm);
    procedure AlwaysOnTop(Form : TForm; OnTop : Boolean);
  published
    property AllwaysOnTop : Boolean read FAllwaysOnTop Write SetAllwaysOnTop;
    property Fullscreen : Boolean read FFullscreen Write SetFullscreen;
    property FormLook : TCHFormLook read FFormLookClass Write FFormLookClass;
    property MoveMode : TFormMoveMode read FFormMoveMode Write SetFormMoveMode;
    property OnTopMode : TOnTopMode read FOnTopMode Write SetOnTopMode;
    property Title : TCHTitle read FTitleClass Write FTitleClass;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('CH Pack', [TCHForm]);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FForm := TForm(GetParentForm(TControl(AOwner)));
  FTitleClass := TCHTitle.Create(self);
  FFormLookClass := TCHFormLook.Create(Self);

  FOrigWndProc := FForm.WindowProc;

  FOnTopMode := otNormalOnTop;
  FFormMoveMode := foOnlyCaption;

  FTimer := TTimer.Create(Self);
  FTimer.Interval := 10;
  FTimer.Enabled := False;
  FTimer.OnTimer := DoOnTopMode;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHForm.Destroy;
begin
  FTitleClass.Free;
  FFormLookClass.Free;
  FTimer.Free;
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHForm.Loaded;
begin
  inherited Loaded;

  if not (csDesigning in ComponentState) then
  begin
    // OnTop
    if FAllwaysOnTop then
      AlwaysOnTop(FForm, True);

    // ShowTitle
    if FTitleClass.FShowTitlebar = False then
      HideTitlebar(FForm);

    DoFormLook(FFormLookClass.FFormLook);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHForm.AlwaysOnTop(Form: TForm; OnTop: Boolean);
begin
  if OnTop then
  begin
    SetWindowPos(Form.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOACTIVATE or
      SWP_SHOWWINDOW or SWP_NOMOVE or SWP_NOSIZE);
    BringWindowToTop(Form.Handle);
  end
  else
    SetWindowPos(Form.Handle, HWND_NOTOPMOST, 0, 0, 0,0, SWP_NOACTIVATE or
      SWP_SHOWWINDOW or SWP_NOMOVE or SWP_NOSIZE);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHForm.ShowTitlebar(Form: TForm);
var 
  Style : longint; 
begin 
  if Form.BorderStyle = bsNone then
    Exit;
  Style := GetWindowLong(Form.Handle, GWL_STYLE);
  if (Style and WS_CAPTION) <> WS_CAPTION then
  begin 
    case Form.BorderStyle of
      bsSingle, bsSizeable : SetWindowLong(Form.Handle, GWL_STYLE, Style or
        WS_CAPTION or WS_BORDER);
      bsDialog : SetWindowLong(Form.Handle, GWL_STYLE, Style or WS_CAPTION or
        DS_MODALFRAME or WS_DLGFRAME);
    end; 
    Form.Height := Form.Height + GetSystemMetrics(SM_CYCAPTION);
    Form.Refresh;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHForm.HideTitlebar(Form: TForm);
var
  Style : longint; 
begin
  if Form.BorderStyle = bsNone then
    Exit;
  Style := GetWindowLong(Form.Handle, GWL_STYLE);
  if (Style and WS_CAPTION) = WS_CAPTION then
  begin 
    case Form.BorderStyle of
      bsSingle, bsSizeable: SetWindowLong(Form.Handle, GWL_STYLE, Style and
        (not (WS_CAPTION)) or WS_BORDER);
      bsDialog: SetWindowLong(Form.Handle, GWL_STYLE, Style and
        (not (WS_CAPTION)) or DS_MODALFRAME or WS_DLGFRAME);
    end;
    Form.Height := Form.Height - GetSystemMetrics(SM_CYCAPTION);
    Form.Refresh;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHForm.SetAllwaysOnTop(const Value: Boolean);
begin
  if FAllwaysOnTop <> Value then
  begin
    FAllwaysOnTop := Value;
    if not (csDesigning in ComponentState) then
    begin
      if FAllwaysOnTop then
      begin
        AlwaysOnTop(FForm, True);
        if FOnTopMode = otNormalOnTop then
          FTimer.Enabled := False
        else
          FTimer.Enabled := True;
      end
      else
      begin
        FTimer.Enabled := False;
        AlwaysOnTop(FForm, False);
      end;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHForm.SetFormMoveMode(const Value: TFormMoveMode);
begin
  if FFormMoveMode <> Value then
  begin
    FFormMoveMode := Value;
    if (not (csDesigning in ComponentState)) then
    begin
      if (FFormMoveMode = foOnlyCaption) then
        FForm.WindowProc := FOrigWndProc
      else
        FForm.WindowProc := WndProc;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHForm.SetOnTopMode(const Value: TOnTopMode);
begin
  if FOnTopMode <> Value then
  begin
    FOnTopMode := Value;
    if not (csDesigning in ComponentState) and (FAllwaysOnTop) then
    begin
      if FOnTopMode = otNormalOnTop then
        FTimer.Enabled := False
      else
        FTimer.Enabled := True;
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHForm.DoBlink(Sender: TObject);
begin
  if FTitleClass.FShowCaption then
  begin
    FForm.Caption := FTitleClass.FCaption;
    FTitleClass.FShowCaption := False;
  end
  else
  begin
    FTitleClass.FCaption := FForm.Caption;
    FForm.Caption := '';
    FTitleClass.FShowCaption := True;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHForm.DoOnTopMode(Sender: TObject);
begin
  AlwaysOnTop(FForm, True);
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHForm.WndProc(var Message: TMessage);
begin
  if not (csDesigning in ComponentState) then
  begin
    case Message.Msg of
      WM_NCHITTEST : WMNCHitTest(Message)
    else
      FOrigWndProc(Message);
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHForm.WMNCHitTest(var Msg: TMessage);
begin
  inherited;
  FOrigWndProc(Msg);

  with Msg do
  begin
    if FFormMoveMode = foNone then
    begin
      if (Result = HTCAPTION) or (Result = HTCLIENT) then
        Result := HTNOWHERE;
    end
    else if FFormMoveMode = foOnlyCaption then
    begin
      Result := HTCAPTION;
    end
    else
    begin
      if Result = HTCLIENT then
        Result := HTCAPTION;
    end;
  end;
end;



{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ TCHTitle }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHTitle.Create(AOwner: TCHForm);
begin
  inherited Create;
  FOwner := AOwner;
  FBlinkInterval := 50;
  FTitleBlink := False;
  FShowCaption := True;
  FShowTitlebar := True;
  FCaption := FOwner.FForm.Caption;

  FTimer := TTimer.Create(AOwner);
  FTimer.Interval := FBlinkInterval;
  FTimer.Enabled := FTitleBlink;
  FTimer.OnTimer := FOwner.DoBlink;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
destructor TCHTitle.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHTitle.SetBlinkInterval(const Value: Cardinal);
begin
  if FBlinkInterval <> Value then
  begin
    FBlinkInterval := Value;
  end;

  FTimer.Interval := FBlinkInterval;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHTitle.SetShowTitlebar(const Value: Boolean);
begin
  if FShowTitlebar <> Value then
  begin
    FShowTitlebar := Value;
    if not (csDesigning in FOwner.ComponentState) then
    begin
      if FShowTitlebar then
        FOwner.ShowTitlebar(FOwner.FForm)
      else
        FOwner.HideTitlebar(FOwner.FForm);
    end;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHTitle.SetTitleBlink(const Value: Boolean);
begin
  if FTitleBlink <> Value then
  begin
    FTitleBlink := Value;
  end;

  if FTitleBlink then
    FTimer.Enabled := True
  else
  begin
    FTimer.Enabled := False;
    FOwner.FForm.Caption := FCaption;
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHForm.SetFullscreen(const Value: Boolean);
begin
  if Fullscreen <> Value then
  begin
    FFullscreen := Value;
    if not (csDesigning in ComponentState) then
    begin
      if FFullscreen then
      begin
        FFormLeft := FForm.Left;
        FFormTop := FForm.Top;
        FFormBorderStyle := FForm.BorderStyle;
        FFormWidth := FForm.Width;
        FFormHeight := FForm.Height;

        FForm.Left := 0;
        FForm.Top := 0;
        FForm.BorderStyle := bsNone;
        FForm.Width := Screen.Width;
        FForm.Height := Screen.Height;
      end
      else
      begin
        FForm.Left := FFormLeft;
        FForm.Top := FFormTop;
        FForm.BorderStyle := FFormBorderStyle;
        FForm.Width := FFormWidth;
        FForm.Height := FFormHeight;
      end;
    end;
  end;
end;



{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
{ TCHFormLook }
{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
constructor TCHFormLook.Create(AOwner: TCHForm);
begin
  inherited Create;
  FOwner := AOwner;

  FRoundRectX := 20;
  FRoundRectY := 20;
  FInnerRing := 100;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFormLook.SetFormLook(const Value: TFormLook);
begin
  if FFormLook <> Value then
  begin
    FFormLook := Value;
    FOwner.DoFormLook(Value);
  end;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFormLook.SetInnerRing(const Value: Word);
begin
  if FInnerRing <> Value then
    FInnerRing := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFormLook.SetRoundRectX(const Value: Word);
begin
  if FRoundRectX <> Value then
    FRoundRectX := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHFormLook.SetRoundRectY(const Value: Word);
begin
  if FRoundRectY <> Value then
    FRoundRectY := Value;
end;

{ ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ }
procedure TCHForm.DoFormLook(Value: TFormLook);
var
  nFormWidth, nFormHeight : Integer;
  HR, HR2 : HRgn;
  FPtTriangle : array[0..2] of TPoint;
begin
  if not (csDesigning in ComponentState) and (Value <> flNormal) then
  begin
    nFormWidth := FForm.Width;
    nFormHeight := FForm.Height;
    // Elliptic
    if Value = flElliptic then
    begin
      HR := CreateEllipticRgn(0, 0, nFormWidth, nFormHeight);
      SetWindowRgn(FForm.Handle, HR, True);
    end
    // RoundRect
    else if Value = flRoundRect then
    begin
      HR := CreateRoundRectRgn(0, 0, nFormWidth, nFormHeight,
        FFormLookClass.FRoundRectX, FFormLookClass.FRoundRectY);
      SetWindowRgn(FForm.Handle, HR, True);
    end
    // Triangle
    else if Value = flTriangle then
    begin
      FPtTriangle[0] := Point(0,0);
      FPtTriangle[1] := Point(nFormWidth,0);
      FPtTriangle[2] := Point(nFormWidth div 2, nFormHeight);
      HR := CreatePolygonRgn(FPtTriangle ,3 ,WINDING);
      SetWindowRgn(FForm.Handle, HR, True);
    end
    // Ring
    else if Value = flRing then
    begin
      HR := CreateEllipticRgn(0, 0, nFormWidth, nFormHeight);
      HR2 := CreateEllipticRgn(FFormLookClass.FInnerRing, FFormLookClass.FInnerRing,
        nFormWidth - FFormLookClass.FInnerRing, nFormHeight - FFormLookClass.FInnerRing);
      CombineRgn(HR2, HR, HR2, RGN_XOR);
      SetWindowRgn(FForm.Handle, HR2, True);
    end;
  end;
end;

end.
