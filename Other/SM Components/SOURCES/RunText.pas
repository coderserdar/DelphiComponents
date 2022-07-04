{ Copyright (C) 1998-2003, written by Shkolnik Mike, Scalabium
  E-Mail:  mshkolnik@scalabium.com
           mshkolnik@yahoo.com
  WEB: http://www.scalabium.com
       http://www.geocities.com/mshkolnik
  tel: 380-/44/-552-10-29

  Эта компонента - это аналог TLabel, но с возможностью
  отображения не статического текста, а прокручиваемого.
}
unit RunText;

interface

uses
  Windows, Classes, Graphics, Controls, ExtCtrls;

type
  TDirect = (diLeftToRight, diRightToLeft,
             diTopToBottom, diBottomToTop);
  TStyleLabel = (slNormal, slLowered, slRaised);
  TTextLayout = (tlTop, tlCenter, tlBottom);

  TRunningText = class(TCustomPanel) //TGraphicControl)
  private
    { Private declarations }
    FTimer: TTimer;
    FCurCycle: Integer;
    FActive: Boolean;
    FDirect: TDirect;
    FStyleLabel: TStyleLabel;
    FLayout: TTextLayout;
    FSpeed: Integer;
    FNumRepeat: Word; //number of repeat cycles
    FContinuous: Boolean; //same that FNumRepeat = infinity
    FSteps: Integer;            { steps}
    CurrentStep: Integer;       { step number when running}

    procedure SetActive(Value: Boolean);
    procedure SetDirect(Value: TDirect);
    procedure SetStyleLabel(Value: TStyleLabel);
    procedure SetSteps(Value: Integer);
    procedure SetSpeed(Value: Integer);
    function GetTransparent: Boolean;
    procedure SetTransparent(Value: Boolean);
    procedure SetLayout(Value: TTextLayout);

    procedure DoTextOut(ACanvas: TCanvas; X, Y: Integer; AText: string);
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure TimerExpired(Sender: TObject);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Active: Boolean read FActive write SetActive;
    property Direct: TDirect read FDirect write SetDirect;
    property StyleLabel: TStyleLabel read FStyleLabel write SetStyleLabel;
    property NumRepeat: Word read FNumRepeat write FNumRepeat;
    property Continuous: Boolean read FContinuous write FContinuous;

    property Steps: Integer read FSteps write SetSteps;
    property Speed: Integer read FSpeed write SetSpeed;
//    property Transparent: Boolean read GetTransparent write SetTransparent default False;
    property Layout: TTextLayout read FLayout write SetLayout default tlTop;

    property Align;
    property Alignment;
//    property BevelInner;
//    property BevelOuter;
//    property BevelWidth;
//    property BorderWidth;
//    property BorderStyle;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Caption;
    property Color;
    property Ctl3D;
    property Font;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('SMComponents', [TRunningText]);
end;

constructor TRunningText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSteps := 100;
  CurrentStep := 0;
  FSpeed := 100;
  FTimer := TTimer.Create(Self);
  with FTimer do
  begin
    Enabled := False;
    OnTimer := TimerExpired;
    Interval := FSpeed;
  end;
end;

destructor TRunningText.Destroy;
begin
  FTimer.Free;

  inherited Destroy;
end;

procedure TRunningText.TimerExpired(Sender: TObject);
begin
  if not FTimer.Enabled then Exit;

  if (FCurCycle < FNumRepeat) or FContinuous then
  begin
    Inc(CurrentStep);
    Paint;

    if CurrentStep = FSteps then
    begin
      CurrentStep := 0;
      if not FContinuous then
        Inc(FCurCycle);
    end;
  end
  else
    Active := False;
end;

procedure TRunningText.DoTextOut(ACanvas: TCanvas; X, Y: Integer; AText: string);
begin
  with ACanvas do
  begin
    Font := Self.Font;
    Brush.Style := bsClear;
    { Draw text}
    case FStyleLabel of
      slRaised: begin
                  Font.Color := clBtnShadow;
                  TextOut(X + 2, Y + 2, AText);
                end;
      slLowered: begin
                   Font.Color := clBtnHighlight;
                   TextOut(X + 2, Y + 2, AText);
                 end;
    end;
    Font.Color := Self.Font.Color;
    TextOut(X, Y, AText);
  end;
end;

procedure TRunningText.Paint;
const
  Alignments: array[TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  Layouts: array[TTextLayout] of Word = (DT_TOP, DT_VCENTER, DT_BOTTOM);
var
  TmpBmp: TBitmap;
  StX, StY: Integer;
  intWidth, intHeight: Integer;
  CnX, CnY: Integer;
  PctDone: Double;

begin
  TmpBmp := TBitmap.Create;
  try
    TmpBmp.Width := Width;
    TmpBmp.Height := Height;
    with TmpBmp.Canvas do
    begin
      Font := Self.Font;
      intWidth := TextWidth(Caption) + 2;
      intHeight := TextHeight(Caption) + 2;
      Brush.Color := Self.Color;
      Brush.Style := bsSolid;
      FillRect(ClipRect);
    end;

    {Calculate start points}
    case Alignment of
      taRightJustify: CnX := Width - intWidth;
      taCenter: CnX := (Width - intWidth) div 2;
    else // taLeftJustify
      CnX := 0;
    end;
    case Layout of
      tlBottom: CnY := Height - intHeight;
      tlCenter: CnY := (Height - intHeight) div 2;
    else // tlTop
      CnY := 0;
    end;

    {Calculate percentages & starting points}
    PctDone := CurrentStep/FSteps;

    {set a static label parameters}
    StX := CnX;
    StY := CnY;
    if Active then
      case FDirect of
        diRightToLeft: begin
                         StY := CnY;
                         StX := -intWidth + Round((intWidth + Width)*(1 - PctDone));
                       end;
        diLeftToRight: begin
                         StY := CnY;
                         StX := -intWidth + Round((intWidth + Width)*(PctDone));
                       end;
        diTopToBottom: begin
                         StX := CnX;
                         StY := -intHeight + Round((intHeight + Height)*(PctDone));
                       end;
        diBottomToTop: begin
                         StX := CnX;
                         StY := -intHeight + Round((intHeight + Height)*(1 - PctDone));
                       end;
      end;
    DoTextOut(TmpBmp.Canvas, StX, StY, Caption);
    Canvas.Draw(0, 0, TmpBmp);
  finally
    TmpBmp.Free;
  end
end;

procedure TRunningText.SetActive(Value: Boolean);
begin
  if (FActive <> Value) then
  begin
    FActive := Value;
    FCurCycle := 0;
    CurrentStep := 0;
    FTimer.Enabled := Value;
    if not Value then
      Invalidate
  end;
end;

procedure TRunningText.SetDirect;
begin
  if (FDirect <> Value) then
  begin
    FDirect := Value;
    if FActive then
      Invalidate;
  end;
end;

procedure TRunningText.SetSteps(Value: Integer);
begin
  if FSteps <> Value then
  begin
    FSteps := Value;
    if (csDesigning in ComponentState) then
    begin
      Invalidate;
    end;
  end;
end;

procedure TRunningText.SetStyleLabel(Value: TStyleLabel);
begin
  if FStyleLabel <> Value then
  begin
    FStyleLabel := Value;
    Invalidate;
  end;
end;

procedure TRunningText.SetSpeed(Value: Integer);
begin
  if FSpeed <> Value then
  begin
    FSpeed := Value;
    if Value > 1000 then FSpeed := 1000;
    if Value < 1 then FSpeed := 1;
    {Change the timer interval}
    if FTimer <> nil then
      FTimer.Interval := FSpeed;
  end;
end;

function TRunningText.GetTransparent: Boolean;
begin
  Result := not (csOpaque in ControlStyle);
end;

procedure TRunningText.SetTransparent(Value: Boolean);
begin
//  if Transparent <> Value then
  begin
    if Value then
      ControlStyle := ControlStyle - [csOpaque]
    else
      ControlStyle := ControlStyle + [csOpaque];
    Invalidate;
  end;
end;

procedure TRunningText.SetLayout(Value: TTextLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

end.
